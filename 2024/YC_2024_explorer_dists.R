library(tidyverse)
library(lubridate)
library(magrittr)
library(glue)
library(writexl) # to save results
library(readxl)

data0 <- read_xlsx("../dark-loci/outputs/2023/concern_classification_rel-202311.xlsx") %>% 
  filter(YEAR == 2023, MONTH == 11)

dists_to_explore <- data0 %>% 
  filter(LISTS.DIST < 1000) %>% 
  arrange(LISTS.DIST) %>% 
  dplyr::select(COUNTY.CODE, STATE, COUNTY, LISTS.DIST, CONCERN.COARSE)

write_xlsx(dists_to_explore, "2024/YC_2024_explorer_dists.xlsx")
