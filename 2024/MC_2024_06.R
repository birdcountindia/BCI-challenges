###### monthly challenge winners/results ###

cuckoos <- c("Asian Koel", "Pied Cuckoo", "Chestnut-winged Cuckoo", "Large Hawk-Cuckoo",
             "Common Hawk-Cuckoo", "Hodgson's Hawk-Cuckoo", "Indian Cuckoo", "Common Cuckoo",
             "Himalayan Cuckoo", "Lesser Cuckoo", "Banded Bay Cuckoo", "Plaintive Cuckoo", 
             "Grey-bellied Cuckoo", "Asian Emerald Cuckoo", "Violet Cuckoo", 
             "Square-tailed Drongo-Cuckoo", "Fork-tailed Drongo-Cuckoo")

# basic eligible list filter
data0 <- data_mc %>% 
  filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  filter(!any(OBSERVATION.COUNT == "X")) %>% 
  ungroup()

# at least 25 lists in month
data1 <- data0 %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(NO.LISTS >= 25)

# at least 3 with brood parasitic cuckoo
data2 <- data0 %>% 
  group_by(OBSERVER.ID, SAMPLING.EVENT.IDENTIFIER) %>% 
  filter(any(COMMON.NAME %in% cuckoos)) %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(NO.LISTS >= 3)


results <- data1 %>% 
  inner_join(data2, by = "OBSERVER.ID") %>% 
  left_join(eBird_users, by = "OBSERVER.ID") %>% 
  anti_join(filtGA, by = "OBSERVER.ID")


# random selection 
a <- results %>% 
  filter(FULL.NAME != "MetalClicks Ajay Ashok") # removes NAs too
set.seed(2)
winner <- a %>% slice_sample(n = 1) %>% select(FULL.NAME)

winner_mc_announcement <- glue("Monthly challenge winner is {winner}")
