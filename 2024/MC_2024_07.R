###### monthly challenge winners/results ###

# basic eligible list filter
data0 <- data_mc %>% 
  filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  filter(!any(OBSERVATION.COUNT == "X")) %>% 
  ungroup()

# at least 1 list every day
data1 <- data0 %>% 
  distinct(OBSERVER.ID, DAY.M, SAMPLING.EVENT.IDENTIFIER) %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(NO.DAYS = n_distinct(DAY.M)) %>% 
  filter(NO.DAYS >= 31)


results <- data1 %>% 
  left_join(eBird_users, by = "OBSERVER.ID") %>% 
  anti_join(filtGA, by = "OBSERVER.ID")


# random selection 
winner <- sel_random_winner(results, seed = 2)
winner_mc_announcement <- glue("Monthly challenge winner is {winner}")
