
# Objective: Create a function to produce a load duration curve
# Author: Grant Coble-Neal
# Dependency: getNEMdataMap2.R

library(here)

source(here("R", "getNEMdataMap2.R"))

PDC <- function(data = NEM_regions_data, region = "NSW", x_variable = "INDEX" , y_variable = "RRP"){
  library(dplyr)
  library(ggplot2)
  Duration_Price <- data %>% 
    filter(REGION == paste0(region, "1")) %>% 
    arrange(desc(!!as.symbol(y_variable)) ) %>% 
    group_by(REGION) %>% 
    mutate(INDEX = seq(1:length(SETTLEMENTDATE)))
  
  return(Duration_Price)
}
regions <- c("NSW", "QLD", "VIC", "SA", "TAS")
purrr::map(regions, ~PDC(NEM_regions_data, .x, y_variable = "RRP", category = NULL))

price.duration <- purrr::map(regions, ~PDC(df.21_22, .x, y_variable = "AveragePrice") ) 

price.duration <- do.call("rbind", price.duration)

ggplot(data = price.duration, aes(x = INDEX, y = AveragePrice, col = year) ) + 
  geom_line() +
  facet_wrap(~ REGION, ncol = 2, scales = "free_y") +
  ggtitle( paste0("Price Duration Curve by region")) +  
  xlab("Market Interval (5 minutes)") + 
  ylab("$/MWh")
