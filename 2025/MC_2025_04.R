###### media data ###

media_csv_names <- list.files(path = glue("{currel_year}/MC_{currel_year}_{str_pad(currel_month_num, width=2, pad='0')}_media/"), 
                              pattern = "*.csv", full.names = T) %>%
  lapply(read.csv) %>% 
  bind_rows() %>% 
  dplyr::select(contains("ML.Catalog.Number"), 
                Recordist, eBird.Checklist.ID, Number.of.Ratings, Common.Name) %>% 
  magrittr::set_colnames(c("ML.ID", "FULL.NAME", "SAMPLING.EVENT.IDENTIFIER", "RATINGS", "COMMON.NAME"))

###### monthly challenge winners/results ###

# basic eligible list filter
data0 <- data_mc %>% 
  filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  filter(!any(OBSERVATION.COUNT == "X")) %>% 
  ungroup()

# observers who submitted at least 20 checklists
data1 <- data0 %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(NO.LISTS >= 20)

# qualifying checklists with 10+ rated media covering 5+ species
data2 <- data0 %>%
  inner_join(media_csv_names, by = c("SAMPLING.EVENT.IDENTIFIER", "COMMON.NAME"), relationship = "many-to-many") %>%
  group_by(OBSERVER.ID) %>%
  mutate(SPECIES.MEDIA.COUNT = n_distinct(COMMON.NAME),
         TOTAL.MEDIA = n_distinct(ML.ID)) %>%
  ungroup() %>%
  filter(TOTAL.MEDIA >= 10, SPECIES.MEDIA.COUNT >= 5) %>%
  distinct(OBSERVER.ID, FULL.NAME)


results <- data1 %>% 
  inner_join(data2, by = "OBSERVER.ID") %>% 
  select(-FULL.NAME) %>%  # drop conflicting FULL.NAME before final join
  left_join(eBird_users, by = "OBSERVER.ID") %>% 
  anti_join(filtGA, by = "OBSERVER.ID")

# random selection
winner <- sel_random_winner(results, seed = 6)
winner_mc_announcement <- glue("Monthly challenge winner is {winner}")