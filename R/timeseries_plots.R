

# Objective: create time series plots of NEM data
# Author: Grant Coble-Neal

library(fpp3)

Half_hour_ts <- tsibble(Half_hour) # dependency on convertintervals.R

Half_hour_ts <- Half_hour_ts %>% 
  pivot_longer(
    cols = c("Price", "Quantity"),
    names_to = "Names",
    values_to = "Values")
autoplot(Half_hour_ts, Values ) + facet_wrap( ~ Names, scales = "free_y", 
                                              labeller = as_labeller(c(Price = "$/MWh", Quantity = "MWh")),
                                              strip.position = "left") +
  labs(y = NULL) +
  theme(strip.background = element_blank(), strip.placement = "outside")
