

# Objective: create stacked column chart of price buckets by month
# Author: Grant Coble-Neal

library(here)
library(tidyverse)
df.21_22 <- readRDS(here("data", "NEM_regions_2021.rds")) %>% 
  mutate(
    Month = lubridate::month(SETTLEMENTDATE)
  ) %>% 
  group_by(REGION) %>% 
  mutate(
    Price.Buckets = case_when(
      RRP <= -500 ~ "<= 500",
      RRP > -500 & RRP <= 0 ~ "<= 0",
      RRP > 0 & RRP <= 50 ~ "<= 50",
      RRP > 50 & RRP <= 100 ~ "<= 100",
      RRP > 100 & RRP <= 200 ~"<= 200",
      RRP > 200 & RRP <= 500 ~ "<= 500",
      RRP > 500 & RRP <= 1000 ~ "<= 1000",
      RRP > 1000 & RRP <= 2000 ~ "<= 2000",
      RRP > 2000 & RRP <= 5000 ~ "<= 5000",
      RRP > 5000 & RRP <= 10000 ~ "<= 10000",
      RRP > 10000 
    )
  )
  
  