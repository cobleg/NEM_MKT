
# Objective: Create a function to produce a load duration curve
# Author: Grant Coble-Neal
# Dependency: getNEMdataMap2.R

PDC <- function(data, region = "NSW1"){
  library(dplyr)
  library(ggplot2)
  Duration_Price <- data %>% 
    filter(REGION == paste0(region, "1")) %>% 
    arrange(desc(RRP)) %>% 
    mutate(INDEX = seq(1:length(RRP)))
  
  pdc <- ggplot(data = Duration_Price, aes(INDEX, RRP), ) + 
      geom_line() +
      ggtitle( paste0("Price Duration Curve (", region, ")")) +  
      xlab("Market Interval (5 minutes)") + 
      ylab("$/MWh")
  return(pdc)
}
regions <- c("NSW", "QLD", "VIC", "SA", "TAS")
purrr::map(regions, ~PDC(NEM_regions_data, .x))
