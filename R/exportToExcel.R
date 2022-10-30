
# Objective: export compiled regional data
# Author: Grant Coble-Neal
# Dependency: getNEMdataMap2.R

library(here)
library(tidyverse)
library(tsibble)
library(writexl)

NEM_regions <- readRDS(here("data", "NEM_regions.rds"))


# Create a DateTime index
# Filter the data down to one day

NEM_regions.1 <- NEM_regions %>% 
  mutate(
    Year = lubridate::year(SETTLEMENTDATE),
    Month = lubridate::month(SETTLEMENTDATE),
    Day = lubridate::day(SETTLEMENTDATE),
    Hour = lubridate::hour(SETTLEMENTDATE),
    Minutes = lubridate::minute(SETTLEMENTDATE)
  ) %>% 
  mutate(
    DateTime = lubridate::ymd_hm(paste(sep="-", Year, Month, Day, Hour, Minutes))
  ) %>% 
  select(REGION, DateTime, TOTALDEMAND, RRP) %>% 
  tsibble(index = DateTime, key = REGION) %>% 
  filter_index("2022-01-15") %>%
  group_by(REGION) %>% 
  mutate(
    DateTime = seq(1:length(DateTime)) - 1
  ) 

desination.path <- "C:\\Users\\User\\OneDrive\\Ventity models\\NEM intraday simulation\\Data\\NEM_regions.xlsx"

writexl::write_xlsx(x = NEM_regions.1, path = desination.path)
