
# Objective: Create a function to produce a load duration curve
# Author: Grant Coble-Neal

LDC <- function(data, region = "NSW"){
  library(dplyr)
  library(ggplot2)
  Duration_Qty <- data %>% 
    filter(REGION == paste0(region, "1")) %>% 
    arrange(desc(TOTALDEMAND)) %>% 
    mutate(INDEX = seq(1:length(TOTALDEMAND)))
  
  ldc <- ggplot(data = Duration_Qty, aes(INDEX, TOTALDEMAND), ) + 
      geom_line() +
      ggtitle( paste0("Load Duration Curve (", region, ")")) + 
      xlab("Market Interval (5 minutes)") + 
      ylab("MWh")
  return(ldc)
}

regions <- c("NSW", "QLD", "VIC", "SA", "TAS")
map(regions, ~LDC(NEM_regions_data, .x))
