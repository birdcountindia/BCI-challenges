###### monthly challenge winners/results ###

# basic eligible list filter
data0 <- data_mc %>% 
  filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  filter(!any(OBSERVATION.COUNT == "X")) %>% 
  ungroup()

# at least 30 lists
data1 <- data0 %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(NO.LISTS >= 30)

# at least 4 lists shared with other eBirder(s)
data2 <- data0 %>% 
  group_by(GROUP.ID) %>% 
  mutate(SHARED = case_when(n_distinct(SAMPLING.EVENT.IDENTIFIER) > 1 ~ TRUE,
                            TRUE ~ FALSE)) %>% 
  group_by(OBSERVER.ID) %>% 
  filter(SHARED == TRUE) %>% 
  summarise(S.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(S.LISTS >= 4)


results <- data1 %>% 
  inner_join(data2, by = "OBSERVER.ID") %>% 
  left_join(eBird_users, by = "OBSERVER.ID") %>% 
  anti_join(filtGA, by = "OBSERVER.ID")


# random selection 
a <- results %>% 
  filter(FULL.NAME != "MetalClicks Ajay Ashok") # removes NAs too
set.seed(1)
winner <- a %>% slice_sample(n = 1) %>% select(FULL.NAME)

winner_mc_announcement <- glue("Monthly challenge winner is {winner}")
