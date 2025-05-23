#
#
# Objective: create a quantile model of NEM intra-day demand data
# Author: Grant Coble-Neal
# Dependencies: getNEMdataMap2.R

library(here)
library(tidyverse)
library(quantreg)
library(TSstudio)
library(tsibble)

NEM_regions_2022 <- readRDS("~/R/NEM_MKT/data/NEM_regions_2022.rds")

NEM_NSW <- NEM_regions_2022 %>% filter(
  REGION == "NSW1"
) %>% mutate(
  DateTime = lubridate::ymd_hms(paste(Date, Time))
)  %>%
  mutate(
    hour = lubridate::hour(DateTime)
    ) %>% group_by(
      Date, hour
    ) %>% 
  summarise(
    Demand = mean(TOTALDEMAND)
  ) %>% mutate(
    DateTime = lubridate::ymd_hm(paste(Date, hour, "-0"))
  ) %>%  ungroup() %>% select(
    DateTime, Demand
  )

# Plotting the quantile of the UKgrid dataset
# No period subset
ts_quantile(NEM_NSW, 
            period = NULL, 
            title = "NEM NSW 2022 - Quantile Plot")

# Plotting the quantile of the UKgrid dataset
# Using a weekday subset
ts_quantile(NEM_NSW, 
            period = "weekdays",
            title = "NEM NSW 2022 - by Weekdays")

# Spacing the plots by setting the 
# number of rows of the plot to 2
ts_quantile(NEM_NSW, 
            period = "weekdays",
            title = "NEM NSW 2022 Grid Net Demand for Electricity - by Weekdays",
            n = 2)

ts_quantile(NEM_NSW, 
            period = "monthly",
            title = "NEM NSW 2022 Grid Net Demand for Electricity - by month",
            n = 2)
