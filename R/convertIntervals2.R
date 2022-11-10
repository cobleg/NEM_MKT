

# Objective: convert 5 minute interval data to 60 minute intervals
# Author: Grant Coble-Neal

loadData <- function(fileName = "NEM_regions_2022.rds"){
  # load the  data 
  file <- c(fileName)
  wd <- getwd()
  subdirectory <- "data"
  myPath <- file.path(wd, subdirectory, file)
  df <- readRDS(myPath)
  return(df)
}

convertInterval <- function(data = "NEM_regions_2022", intervalsPerHour = 12){
  library(dplyr)
  library(lubridate)
  
  df <- data %>% 
    mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE),
           date = round_date(SETTLEMENTDATE, "day"),
           year = year(date),
           month = month(SETTLEMENTDATE),
           day = day(SETTLEMENTDATE),
           hour = hour(SETTLEMENTDATE),
    ) %>% 
    group_by(REGION, year, month, day, hour) %>% 
    summarise(TOTALDEMAND = sum(TOTALDEMAND)/intervalsPerHour,
              AveragePrice = mean(RRP))
  return(df)
}

library(dplyr)
# load the 2021 data NEM_NSW_2022
file <- c("NEM_regions_2021.rds")
df.2021 <-loadData(file) %>% convertInterval(intervalsPerHour=2)

# load the 2022 data NEM_NSW_2022
file <- c("NEM_regions_2022.rds")
df.2022 <-loadData(file) %>% convertInterval(intervalsPerHour=12)

df.21_22 <- rbind(df.2021, df.2022) %>% 
  mutate(
    SETTLEMENTDATE = lubridate::make_datetime(year = year, month = month, day = day, hour = hour, min = 0)
  )
