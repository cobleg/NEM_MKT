
# Objective: create event log for the NEM
# Author: Grant Coble-Neal
# Output: NEM event log tibble

# Types of events:
#               (1) Price cap events
#               (2) High price events

library(here)
library(padr)

eventLog <- NEM_regions_2022 <- readRDS(here("data", "NEM_regions_2022.rds")) %>% 
  group_by(REGION) %>% 
  mutate(
    DateTime = lubridate::ymd_hms(SETTLEMENTDATE)
  ) %>% 
  padr::thicken(interval = "30 min") %>% 
  group_by(REGION, DateTime) %>% 
  summarise(
    RRP = mean(RRP),
    Demand = sum(TOTALDEMAND)/12
  ) %>% 
  mutate(
    Weekday = lubridate::wday(DateTime, label = TRUE),
    Month = lubridate::month(DateTime, label = TRUE),
    Week.Number = lubridate::week(DateTime)
  ) %>% mutate(
    Season = case_when(
      Month %in% c("Jan", "Feb", "Dec") ~ "Summer",
      Month %in% c("Mar", "Apr", "May") ~ "Autumn",
      Month %in% c("Jun", "Jul", "Aug") ~ "Winter",
      Month %in% c("Sep", "Oct", "Nov") ~ "Spring"
    )) %>% 
  select(REGION, DateTime, Demand, RRP, Weekday, Month, Week.Number, Season) %>% 
  mutate(
    Event.HighPrice = case_when(
      RRP > 5000 ~ 1,
      TRUE ~ 0
    )
  )
