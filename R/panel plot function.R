
# Objective: create panel of intra-day plots by region
# Author: Grant Coble-Neal

library(dplyr)
library(ggplot2)
library(tsibble)
library(lubridate)

panelPlot <- function(data = NEM_regions_ts1, xAxis = "hour", yAxis = "Q95_MW", group = "REGION", color = "year", splines = 7){
  library(rlang)
  library(ggplot2)
  p <-  ggplot(data = data) + geom_point(aes(x = !!as.name(xAxis), y = !!as.name(yAxis), group = !!as.name(group), color = !!as.name(color))) + 
    facet_wrap(~ data$REGION, ncol = 2, scales = "free_y") + 
    geom_smooth(aes(x = !!as.name(xAxis), y = !!as.name(yAxis)), method = "lm", formula = y ~ splines::bs(x,splines), se = T)
  
  return(p)
  
}

NEM_regions_ts1 <- df.21_22 %>% 
  group_by(REGION, year, hour) %>% 
  filter(month == 4 | month == 5 | month == 6) %>% 
  summarise(
            P25_MW = quantile(TOTALDEMAND, 0.25),
            P50_MW = quantile(TOTALDEMAND, 0.50),
            P75_MW = quantile(TOTALDEMAND, 0.95),
            P95_MW = quantile(TOTALDEMAND, 0.95),
            P25_RRP = quantile(AveragePrice), 0.25,
            P50_RRP = quantile(AveragePrice), 0.50,
            P75_RRP = quantile(AveragePrice), 0.75,
            P95_RRP = quantile(AveragePrice), 0.95
            )


panelPlot(yAxis = "P95_MW")
panelPlot(yAxis = "P95_RRP")
