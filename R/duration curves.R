
# Objective: Create duration curves for quantity and price
# Author: Grant Coble-Neal

library(dplyr)
library(ggplot2)

Duration_Qty <- NEM_regions_data %>% 
  filter(REGION == "QLD1")
  arrange(desc(TOTALDEMAND)) %>% 
  mutate(INDEX = seq(1:length(TOTALDEMAND)))

ggplot(data = Duration_Qty, aes(INDEX, TOTALDEMAND), ) + 
  geom_line() +
  ggtitle("Load Duration Curve (NSW)") + 
  xlab("Market Interval (5 minutes)") + 
  ylab("MWh")
  
Duration_Price <- NEM_NSW_2022 %>% 
  arrange(desc(RRP)) %>% 
  mutate(INDEX = seq(1:length(RRP)))

ggplot(data = Duration_Price, aes(INDEX, RRP), ) + 
  geom_line() +
  ggtitle("Price Duration Curve (NSW)") + 
  xlab("Market Interval (5 minutes)") + 
  ylab("$/MWh")
