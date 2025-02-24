###### monthly challenge winners/results for December 2024 ###

# basic eligible list filter
data0 <- data_mc %>% 
  filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  filter(!any(OBSERVATION.COUNT == "X")) %>% 
  ungroup()

# keywords
wetlands <- c("pond","Pond","lake","Lake","beach","Beach","river","River"," Dam",
              "reservoir","Reservoir","canal","Canal","jheel","Jheel","kere","Kere",
              "wetland","Wetland","mangrove","Mangrove","creek","Creek","jetty",
              "Jetty","marsh","Marsh", "beel", "Beel", "bheel", "Bheel")

awc <- c("AWC", "awc", "Asian Waterbird Census", "ensus", "aterbird")

# at least 30 lists
data1 <- data0 %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(NO.LISTS >= 31)

# two different wetland locations
data2 <- data0 %>% 
  # shared with AWC
  group_by(GROUP.ID) %>% 
  filter(any(str_detect(LOCALITY, paste(wetlands, collapse = "|"))) | 
           any(str_detect(TRIP.COMMENTS, paste(wetlands, collapse = "|"))) |
           any(str_detect(TRIP.COMMENTS, paste(awc, collapse = "|")))) %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(NO.WETLAND = n_distinct(LOCALITY)) %>% 
  filter(NO.WETLAND >= 2)


results <- data1 %>% 
  inner_join(data2, by = "OBSERVER.ID") %>% 
  left_join(eBird_users, by = "OBSERVER.ID") %>% 
  anti_join(filtGA, by = "OBSERVER.ID")

# random selection
winner <- sel_random_winner(results, seed = 123)
winner_mc_announcement <- glue("Monthly challenge winner is {winner}")