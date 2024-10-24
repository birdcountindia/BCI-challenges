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


# bonus: Big Butterfly Month
bonus <- data0 %>% 
  # mention Big Butterfly Month 2024 in comments
  filter(str_detect(TRIP.COMMENTS, 
                    coll("Big Butterfly Month 2024", ignore_case = TRUE)),
         # also link to observation
         (str_detect(TRIP.COMMENTS, "inaturalist.org/") | 
            str_detect(TRIP.COMMENTS, "indiabiodiversity.org/") |
            str_detect(TRIP.COMMENTS, "ifoundbutterflies.org/"))) %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(LISTS.WITH.BUTT.LINK = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  left_join(eBird_users, by = "OBSERVER.ID") %>% 
  anti_join(filtGA, by = "OBSERVER.ID")



# random selection 
winner <- sel_random_winner(results, seed = 4)
winner_mc_announcement <- glue("Monthly challenge winner is {winner}")
