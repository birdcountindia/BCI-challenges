###### Monthly challenge winners/results for September 2025 ###
###### 20 or more eligible checklists of which 4 should be shared with another eBirder

# basic eligible list filter
data0 <- data_mc %>% 
  filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  filter(!any(OBSERVATION.COUNT == "X")) %>% 
  ungroup()

# at least 20 lists from same location
data1 <- data0 %>% 
  group_by(OBSERVER.ID) %>% 
  reframe(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(NO.LISTS >= 20)

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
winner <- sel_random_winner(results, seed = 671)
winner_mc_announcement <- glue("Monthly challenge winner is {winner}")