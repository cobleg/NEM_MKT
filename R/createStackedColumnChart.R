

# Objective: create stacked column chart of price buckets by month
# Author: Grant Coble-Neal

library(here)
library(tidyverse)

source(here("R", "convertIntervals2.R"))

Price.Bands_Monthly <- df.21_22 %>% 
  mutate(
    Month = lubridate::month(SETTLEMENTDATE),
    Year = lubridate::year(SETTLEMENTDATE)
  ) %>% 
  mutate(
    Date = lubridate::make_date(year = Year, month = Month, day = 1)
  ) %>% 
  group_by(REGION, Date) %>% 
  mutate(
    Price.Buckets = case_when(
      AveragePrice <= -500 ~ "<= 500",
      AveragePrice > -500 & AveragePrice <= 0 ~ "<= 0",
      AveragePrice > 0 & AveragePrice <= 50 ~ "<= 50",
      AveragePrice > 50 & AveragePrice <= 100 ~ "<= 100",
      AveragePrice > 100 & AveragePrice <= 200 ~"<= 200",
      AveragePrice > 200 & AveragePrice <= 500 ~ "<= 500",
      AveragePrice > 500 & AveragePrice <= 1000 ~ "<= 1000",
      AveragePrice > 1000 & AveragePrice <= 2000 ~ "<= 2000",
      AveragePrice > 2000 & AveragePrice <= 5000 ~ "<= 5000",
      AveragePrice > 5000 & AveragePrice <= 10000 ~ "<= 10000",
      AveragePrice > 10000 ~ "> 10000"
    )
  ) %>% 
  group_by(REGION, Date, Price.Buckets) %>% 
  summarise(
    Count = n()
  ) 

Price.Bands_Monthly %>% 
  ggplot(
    aes(fill = forcats::fct_rev(Price.Buckets), y = Count, x = Date) 
  ) +
  geom_bar(position = "stack", stat = "identity") +
  facet_grid(cols = vars(REGION)) +
  labs( fill = "Price bucket")
  
