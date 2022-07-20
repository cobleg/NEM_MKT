
# Objective: create panel of intra-day plots by region
# Author: Grant Coble-Neal

library(dplyr)
library(ggplot2)
library(tsibble)
library(lubridate)

NEM_regions_ts1 <- df.21_22 %>% 
  group_by(REGION, year, hour) %>% 
  filter(month == 4 | month == 5 | month == 6) %>% 
  summarise(
            Q25_MW = quantile(TOTALDEMAND, 0.25),
            Q50_MW = quantile(TOTALDEMAND, 0.50),
            Q75_MW = quantile(TOTALDEMAND, 0.95),
            Q95_MW = quantile(TOTALDEMAND, 0.95)) 
  
p <-  ggplot(data = NEM_regions_ts1) + geom_point(aes(x = hour, y = Q95_MW, group = REGION, color = year)) + 
  facet_wrap(~REGION, ncol = 2, scales = "free_y") + 
  geom_smooth(aes(x = hour, y = Q95_MW), method = "lm", formula = y ~ splines::bs(x,7), se = T)
p


           