###### media data ###

media_csv_names <- list.files(path = glue("{currel_year}/MC_{currel_year}_{str_pad(currel_month_num, width=2, pad='0')}_media/"), 
                              pattern = "*.csv", full.names = T) %>%
  lapply(read.csv) %>% 
  bind_rows() %>% 
  dplyr::select(contains("ML.Catalog.Number"), 
                Recordist, eBird.Checklist.ID, Number.of.Ratings) %>% 
  magrittr::set_colnames(c("ML.ID", "FULL.NAME", "SAMPLING.EVENT.IDENTIFIER", "RATINGS")) %>% 
  distinct(ML.ID, FULL.NAME, SAMPLING.EVENT.IDENTIFIER, RATINGS)


###### monthly challenge winners/results ###

# basic eligible list filter
data0 <- data_mc %>% 
  filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  filter(!any(OBSERVATION.COUNT == "X")) %>% 
  ungroup()

# at least 15 media with ratings
data1 <- data0 %>%
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  slice(1) %>% 
  ungroup() %>% 
  inner_join(media_csv_names, by = "SAMPLING.EVENT.IDENTIFIER") %>% 
  filter(YEAR == currel_year, MONTH == currel_month_num, 
         RATINGS >= 1) %>% # rated
  group_by(OBSERVER.ID, FULL.NAME) %>% 
  summarise(NO.MEDIA = n_distinct(ML.ID)) %>% 
  ungroup() %>% 
  filter(NO.MEDIA >= 15)



results <- data1 %>% 
  # left_join(eBird_users, by = "OBSERVER.ID") %>% 
  anti_join(filtGA, by = "OBSERVER.ID")


# random selection 
a <- results %>% 
  filter(FULL.NAME != "MetalClicks Ajay Ashok") # removes NAs too
set.seed(1)
winner <- a %>% slice_sample(n = 1) %>% select(FULL.NAME)

winner_mc_announcement <- glue("Monthly challenge winner is {winner}")
