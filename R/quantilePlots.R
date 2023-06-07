#
#
# Objective: create quantile plots of Regional Reference Price
# Author: Grant Coble-Neal
# Dependencies: 

library(here)
library(tidyverse)
library(TSstudio)

NEM_regions_2022 <- readRDS("~/R/NEM_MKT/data/NEM_regions_2022.rds")

regional.data <- function(df = NEM_regions_2022, variable = RRP, Region = "NSW"){
  
  df.0 <- df %>% filter(
    REGION == paste0(Region, 1)
    
  ) %>% mutate(
    DateTime = lubridate::ymd_hms(paste(Date, Time))
  )  %>%
    mutate(
      hour = lubridate::hour(DateTime)
    ) %>% group_by(
      Date, hour
    ) %>% 
    summarise(
      Variable = mean({{variable}})
    ) %>% mutate(
      DateTime = lubridate::ymd_hm(paste(Date, hour, "-0"))
    ) %>%  ungroup() %>% select(
      DateTime, Variable
    )
  return(df.0)
}

# Plotting the quantile of the NEM dataset

ts_quantile(regional.data(variable = RRP, Region = "NSW"),
            upper = 0.9,
            lower = 0,
            period = NULL, 
            title = "NEM NSW 2022 - RRP Quantile Plot",
            Ytitle = "Regional Reference Price ($/MWh)")

ts_quantile(regional.data(variable = RRP, Region = "NSW"),
            upper = 0.9,
            lower = 0,
            period = "monthly", 
            title = "NEM NSW 2022 - RRP Quantile Plot",
            Ytitle = "Regional Reference Price ($/MW)")

ts_quantile(regional.data(variable = RRP, Region = "QLD"),
            upper = 0.9,
            lower = 0,
            period = NULL, 
            title = "NEM QLD 2022 - RRP Quantile Plot",
            Ytitle = "Regional Reference Price ($/MWh)")

ts_quantile(regional.data(variable = RRP, Region = "QLD"),
            upper = 0.9,
            lower = 0,
            period = "monthly", 
            title = "NEM QLD 2022 - RRP Quantile Plot",
            Ytitle = "Regional Reference Price ($/MWh)")

ts_quantile(regional.data(variable = RRP, Region = "VIC"),
            upper = 0.9,
            lower = 0,
            period = NULL, 
            title = "NEM VIC 2022 - RRP Quantile Plot",
            Ytitle = "Regional Reference Price ($/MWh)")

ts_quantile(regional.data(variable = RRP, Region = "VIC"),
            upper = 0.9,
            lower = 0,
            period = "monthly", 
            title = "NEM VIC 2022 - RRP Quantile Plot",
            Ytitle = "Regional Reference Price ($/MWh)")

ts_quantile(regional.data(variable = RRP, Region = "SA"),
            upper = 0.9,
            lower = 0,
            period = NULL, 
            title = "NEM SA 2022 - RRP Quantile Plot",
            Ytitle = "Regional Reference Price ($/MWh)")

ts_quantile(regional.data(variable = RRP, Region = "SA"),
            upper = 0.9,
            lower = 0,
            period = "monthly", 
            title = "NEM SA 2022 - RRP Quantile Plot",
            Ytitle = "Regional Reference Price ($/MWh)")

ts_quantile(regional.data(variable = RRP, Region = "SA"),
            upper = 0.9,
            lower = 0,
            period = "weekdays", 
            title = "NEM SA 2022 - RRP Quantile Plot",
            Ytitle = "Regional Reference Price ($/MWh)")
