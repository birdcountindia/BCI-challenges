###### monthly challenge winners/results for December 2024 ###

# basic eligible list filter
data0 <- data_mc %>% 
  filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  filter(!any(OBSERVATION.COUNT == "X")) %>% 
  ungroup()

# At least 30 lists in October
data1 <- data0 %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(NO.LISTS >= 30)

# hotspots
hotspots <- data0 %>% 
  group_by(OBSERVER.ID) %>% 
  filter(LOCALITY.TYPE == "H") %>% 
  distinct(LOCALITY.ID, LOCALITY) %>% 
  ungroup()

# At least 4 lists from the same hotspot
data2 <- data0 %>% 
  inner_join(hotspots, by = c("LOCALITY", "LOCALITY.ID", "OBSERVER.ID")) %>% 
  group_by(OBSERVER.ID, LOCALITY.ID) %>% 
  summarise(HOT.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(HOT.LISTS >= 4) %>% 
  summarise(NO.HOT = n_distinct(LOCALITY.ID)) %>% 
  filter(NO.HOT >= 1)

results <- data1 %>% 
  inner_join(data2, by = "OBSERVER.ID") %>% 
  left_join(eBird_users, by = "OBSERVER.ID") %>% 
  anti_join(filtGA, by = "OBSERVER.ID")

# random selection
winner <- sel_random_winner(results, seed = 123)
winner_mc_announcement <- glue("Monthly challenge winner is {winner}")