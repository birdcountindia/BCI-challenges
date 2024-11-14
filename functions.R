# randomly select winner --------------------------------------------------

sel_random_winner <- function(data_res, seed) {
  
  data_res <- data_res %>% 
    filter(!is.na(FULL.NAME))
  
  set.seed(seed)
  
  random_winner <- data_res %>% slice_sample(n = 1) %>% select(FULL.NAME)
  return(random_winner)
  
}

