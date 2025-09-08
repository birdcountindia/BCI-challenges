###### monthly challenge winners/results for May 2025 ###

# basic eligible list filter
data0 <- data_mc %>% 
  filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  filter(!any(OBSERVATION.COUNT == "X")) %>% 
  ungroup()

# at least 20 lists from same location
data1 <- data0 %>% 
  group_by(OBSERVER.ID, LOCALITY.ID) %>% 
  reframe(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(NO.LISTS >= 20)
# observers with more than one location get advantage in random selection

results <- data1 %>% 
  left_join(eBird_users, by = "OBSERVER.ID") %>% 
  anti_join(filtGA, by = "OBSERVER.ID")

# random selection
winner <- sel_random_winner(results, seed = 1)
winner_mc_announcement <- glue("Monthly challenge winner is {winner}")