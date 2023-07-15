
# export data
library(here)
library(tidyverse)
NEM_regions_2022 %>% write.csv(here("data", "NEM_regions_2022.csv"))
