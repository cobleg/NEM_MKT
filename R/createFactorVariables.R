
# Objective: create time of year analytics by season, month, week, day factor variables
# Author: Grant Coble-Neal


wd <- getwd()
subdirectory <- c("data")
file <- c("NSW_Met_2021.rds")
myPath <- file.path(wd, subdirectory, file)
df <- readRDS(myPath)

df <-cutData(df, type = "season", hemisphere = "southern")
df$weekday <- weekdays(df$date)
df$month <- format(df$date, "%m")
df$day <- format(df$date, "%d")
df$month_name <- format(df$date, "%h")
df$hour <- format(df$date, "%H")

library(openair)
calendarPlot(df, pollutant = "RRP", year = 2021) # month view of price variation
calendarPlot(df, pollutant = "TOTALDEMAND", year = 2021) # month view of demand variation
calendarPlot(df, pollutant = "air_temp", year = 2021) # month view of demand variation

# export result to CSV
file <- c("NEM_NSW_Analytical.CSV")
myPath <- file.path(wd, subdirectory, file)

write.csv(df, myPath)
