
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
  
  pdc <- ggplot(data = Duration_Price, aes_string(x = x_variable, y = y_variable), ) + 
      geom_line() +
      ggtitle( paste0("Price Duration Curve (", region, ")")) +  
      xlab("Market Interval (5 minutes)") + 
      ylab("$/MWh")
  return(pdc)
}
regions <- c("NSW", "QLD", "VIC", "SA", "TAS")
purrr::map(regions, ~PDC(NEM_regions_data, .x, y_variable = "RRP"))
