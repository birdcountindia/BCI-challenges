
# basic eligible list filter

data0 <- data_yc %>% 
  filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  filter(!any(OBSERVATION.COUNT == "X")) %>% 
  ungroup()

# prolific eBirder (>=500 eligible lists) ---------------------------------

data1 <- data0 %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(NO.LISTS >= 500) %>%
  ungroup()

prolific_r <- data1 %>% 
  arrange(desc(NO.LISTS)) %>% 
  left_join(eBird_users, by = "OBSERVER.ID") %>% 
  anti_join(filtGA, by = "OBSERVER.ID")

# random selection 
a <- prolific_r %>% 
  filter(!is.na(FULL.NAME)) # removes NAs
set.seed(2)
prolific_w <- a %>% slice_sample(n = 1) %>% select(FULL.NAME)

prolific_w_ann <- glue("Prolific challenge winner is {prolific_w}")


# consistent eBirder (>=1 eligible list each day) -------------------------

data1 <- data0 %>% 
  mutate(DAY.Y = yday(OBSERVATION.DATE)) %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(NO.DAYS = n_distinct(DAY.Y)) %>% 
  filter(NO.DAYS >= 365)

consistent_r <- data1 %>% 
  left_join(eBird_users, by = "OBSERVER.ID") %>% 
  anti_join(filtGA, by = "OBSERVER.ID")

# random selection 
a <- consistent_r %>% 
  filter(!is.na(FULL.NAME)) # removes NAs
set.seed(5)
consistent_w <- a %>% slice_sample(n = 1) %>% select(FULL.NAME)

consistent_w_ann <- glue("Consistent challenge winner is {consistent_w}")


# adventurous eBirder (>=2 eligible lists from >=15 districts) ------------

data1 <- data0 %>% 
  group_by(OBSERVER.ID, COUNTY.CODE) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(NO.LISTS >= 2) %>% 
  summarise(NO.DIST = n_distinct(COUNTY.CODE)) %>% 
  filter(NO.DIST >= 15)

adventurous_r <- data1 %>% 
  arrange(desc(NO.DIST)) %>% 
  left_join(eBird_users, by = "OBSERVER.ID") %>% 
  anti_join(filtGA, by = "OBSERVER.ID")

# random selection 
a <- adventurous_r %>% 
  filter(!is.na(FULL.NAME)) # removes NAs
set.seed(34)
adventurous_w <- a %>% slice_sample(n = 1) %>% select(FULL.NAME)

adventurous_w_ann <- glue("Adventurous challenge winner is {adventurous_w}")


# faithful eBirder (>=150 eligible lists from a single location) ----------

data1 <- data0 %>% 
  group_by(OBSERVER.ID, LOCALITY.ID) %>% 
  summarise(LOC.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(LOC.LISTS >= 150) %>% 
  ungroup()

faithful_r <- data1 %>% 
  arrange(desc(LOC.LISTS)) %>% 
  left_join(eBird_users, by = "OBSERVER.ID") %>% 
  anti_join(filtGA, by = "OBSERVER.ID")

# random selection 
a <- faithful_r %>% 
  filter(!is.na(FULL.NAME)) # removes NAs
set.seed(4)
faithful_w <- a %>% slice_sample(n = 1) %>% select(FULL.NAME)

faithful_w_ann <- glue("Faithful challenge winner is {faithful_w}")


# dedicated eBirder (>=500 hours of birding) ------------------------------

data1 <- data_yc %>% 
  filter(ALL.SPECIES.REPORTED == 1, !is.na(DURATION.MINUTES)) %>%
  distinct(OBSERVER.ID, SAMPLING.EVENT.IDENTIFIER, DURATION.MINUTES)

data2 <- data1 %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(B.TIME.M = sum(DURATION.MINUTES),
            B.TIME.H = round(B.TIME.M/60, 1),
            B.TIME.D = round(B.TIME.H/24, 1)) %>% 
  filter(B.TIME.H >= 500)

dedicated_r <- data2 %>% 
  arrange(desc(B.TIME.H)) %>% 
  left_join(eBird_users, by = "OBSERVER.ID") %>% 
  anti_join(filtGA, by = "OBSERVER.ID")

# random selection 
a <- dedicated_r %>% 
  filter(!is.na(FULL.NAME)) # removes NAs
set.seed(5)
dedicated_w <- a %>% slice_sample(n = 1) %>% select(FULL.NAME)

dedicated_w_ann <- glue("Dedicated challenge winner is {dedicated_w}")


# Exploratory eBirder (>=5 eligible lists from >=5 districts with <1000) ------------------------------

datax <- read_xlsx("../dark-loci/outputs/2023/concern_classification_rel-202312.xlsx") %>% 
  filter(YEAR == 2023, MONTH == 12)

dists_to_explore <- datax %>% 
  filter(LISTS.DIST < 1000) %>% 
  arrange(LISTS.DIST) %>% 
  dplyr::select(COUNTY.CODE, STATE, COUNTY, LISTS.DIST, CONCERN.COARSE)


data1 <- data0 %>% 
  filter(COUNTY.CODE %in% dists_to_explore$COUNTY.CODE) %>%
  group_by(OBSERVER.ID, COUNTY.CODE) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(NO.LISTS >= 5) %>% 
  summarise(NO.DIST = n_distinct(COUNTY.CODE)) %>% 
  filter(NO.DIST >= 5)

exploratory_r <- data1 %>% 
  arrange(desc(NO.DIST)) %>% 
  left_join(eBird_users, by = "OBSERVER.ID") %>% 
  anti_join(filtGA, by = "OBSERVER.ID")

# random selection 
a <- exploratory_r %>% 
  filter(!is.na(FULL.NAME)) # removes NAs
set.seed(34)
exploratory_w <- a %>% slice_sample(n = 1) %>% select(FULL.NAME)

exploratory_w_ann <- glue("Exploratory challenge winner is {exploratory_w}")