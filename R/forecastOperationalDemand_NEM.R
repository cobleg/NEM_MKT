

# Objective: create time series forecast model of NEM operational demand
# Author: Grant Coble-Neal
# References:
#            (1) Reto Marek (2021). Energy Data Analysis with R; https://hslu-ige-laes.github.io/edar/
#            (2) Hyndman, R.J., & Athanasopoulos, G. (2021) Forecasting: principles and practice, 3rd edition, OTexts: Melbourne, Australia. OTexts.com/fpp3. 
#                     Accessed on 6 November 2022; https://otexts.com/fpp3/
# Dependencies: worldMet.R
# Output: seasonal forecast

# Forecast inputs: 
#                (1) Typical seasonal profile (i.e. an intraday shape)
#                (2) Temperature (NEM_weather.5mins)
#                (3) Solar irradiance

# Source data sets:
#                 (1) NEM_regions_2021.rds (train), NEM_regions_2022 (test)
#                 (2) NEM_weather.5mins for temperature
#                 (3) Need solar irradiance data

library(here)
library(lubridate)
library(feasts)
library(tidyverse)
NEM_regions_2022 <- readRDS(here("data", "NEM_regions_2022.rds")) %>% 
  group_by(REGION) %>% 
  mutate(
    DateTime = lubridate::ymd_hms(SETTLEMENTDATE)
  ) %>% 
  mutate(
    Weekday = lubridate::wday(DateTime, label = TRUE),
    Month = lubridate::month(DateTime, label = TRUE),
    Week.Number = lubridate::week(DateTime)
  ) %>% 
  mutate(
    Season = case_when(
      Month %in% c("Jan", "Feb", "Dec") ~ "Summer",
      Month %in% c("Mar", "Apr", "May") ~ "Autumn",
      Month %in% c("Jun", "Jul", "Aug") ~ "Winter",
      Month %in% c("Sep", "Oct", "Nov") ~ "Spring"
    )) %>% 
  select(REGION, DateTime, TOTALDEMAND, RRP, Weekday, Month, Week.Number, Season)

df.ts <- tsibble::tsibble(NEM_regions_2022, key = c(REGION, Season), index = DateTime)

df.OpDemand.decompose <- df.ts %>% 
  model(
    feasts::STL( TOTALDEMAND)
  ) %>% 
  components(df.OpDemand.decompose) %>% 
  select(!c(.model))

df.OpDemand.decompose %>% 
  autoplot(TOTALDEMAND, colour = "grey") +
  geom_line(aes(y=trend), colour = "black") +
  facet_grid(vars(REGION, Season), scales = "free_y") +
  ggtitle("Operational Demand by region and season, 2022") +
  ylab("MWh")

NEM_regions_2022 %>% 
  group_by(REGION, Weekday, Month, Week.Number) %>% 
  summarise(
    WeekDay.Demand = mean(TOTALDEMAND)
  ) %>% 
  filter(Month == "Jan") %>% 
  ggplot() +
  geom_line(aes(x = Weekday, y = WeekDay.Demand )) +
  facet_wrap(~REGION) +
  ggtitle("Daily Average Operational Demand, January 2022") +
  ylab("MWh")

 NEM_regions_2022 %>% 
   group_by(REGION, Weekday, Month, Week.Number) %>%
  filter(REGION == "NSW1") %>%
  summarise(
    WeekDay.Demand = mean(TOTALDEMAND)
  ) %>% 
  ggplot() +
  geom_line(aes(x = Month, y = WeekDay.Demand) ) +
  ggtitle("NSW Monthly Average Operational Demand, 2022") +
  ylab("MWh")
 