

# Objective: create time series plots of NEM data
# Author: Grant Coble-Neal
# Dependency: convertIntervals2.R

library(fpp3)
library(tidyverse)

df.21_22$Date = lubridate::ymd_h(paste(sep="-", df.21_22$year, df.21_22$month, df.21_22$day, df.21_22$hour))

NEM_ts <- tsibble(df.21_22, index = Date, key = REGION) %>% 
  filter(REGION == "NSW1", year == 2022) 

NEM_hourly_ts <- NEM_ts %>%
  pivot_longer(
    cols = c("AveragePrice", "TOTALDEMAND"),
    names_to = "Names",
    values_to = "Values") %>%
  select(!c(REGION))
  mutate(Names = as.factor(Names))

panel_labels <- c("1" = "$/MWh", "2" = "MWh")
  
autoplot(NEM_hourly_ts, Values ) + 
  facet_wrap( ~ Names, scales = "free_y", 
      labeller = as_labeller(c(AveragePrice = "$/MWh", TOTALDEMAND = "MWh")),
      strip.position = "left") +
  labs(y = NULL) +
  theme(strip.background = element_blank(), strip.placement = "outside")

price.chart <- ggplot(NEM_hourly_ts[NEM_hourly_ts$Names == "AveragePrice", ], aes(x=Date, y=Values)) +
  geom_line() +
  facet_wrap( ~ Names, scales = "free_y", 
              labeller = as_labeller(c(Price = "$/MWh")),
              strip.position = "left") +
  labs(y = NULL) +
  coord_cartesian(ylim=c(0, max(NEM_hourly_ts[NEM_hourly_ts$Names == "AveragePrice", "Values"])))

quantity.chart <- ggplot(NEM_hourly_ts[NEM_hourly_ts$Names == "TOTALDEMAND", ], aes(x=Date, y=Values)) +
  geom_line() +
  facet_wrap( ~ Names, scales = "free_y", 
              labeller = as_labeller(c(Quantity = "MWh")),
              strip.position = "left") +
  labs(y = NULL) +
  coord_cartesian(ylim=c(0, max(NEM_hourly_ts[NEM_hourly_ts$Names == "TOTALDEMAND", "Values"])))

gridExtra::grid.arrange(price.chart, quantity.chart, ncol=2)
