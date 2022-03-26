
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

library(Cairo)
library(openair)
file <- c("NEM_NSW_price_2021.png")
subdirectory <- c("output")
myPath <- file.path(wd, subdirectory, file)

Cairo(file = myPath, type = 'png', units = "px", width = 862, height = 556) # Save the calendar plot
calendarPlot(df, pollutant = "Price", year = 2021, main = "Price variation ($/MWh)") # month view of price variation
dev.off()

file <- c("NEM_NSW_MWh_2021.png")
myPath <- file.path(wd, subdirectory, file)
Cairo(file = myPath, type = 'png', units = "px", width = 862, height = 556) # Save the calendar plot

calendarPlot(df, pollutant = "Quantity", year = 2021, main = "Quantity variation (MWh)") # month view of quantity variation

dev.off()


file <- c("NEM_NSW_Price_2022.png")
myPath <- file.path(wd, subdirectory, file)
Cairo(file = myPath, type = 'png', units = "px", width = 862, height = 556) # Save the calendar plot

calendarPlot(df, pollutant = "Price", year = 2022, main = "Price variation ($/MWh)") # month view of price variation

dev.off()

file <- c("NEM_NSW_MWh_2022.png")
myPath <- file.path(wd, subdirectory, file)
Cairo(file = myPath, type = 'png', units = "px", width = 862, height = 556) # Save the calendar plot

calendarPlot(df, pollutant = "Quantity", year = 2022, main = "Quantity variation (MWh)") # month view of quantity variation

dev.off()


calendarPlot(df, pollutant = "air_temp", year = 2021, main = "Ambient temperature variation (\u00B0C)") # month view of demand variation

calendarPlot(df, pollutant = "air_temp", year = 2022, main = "Ambient temperature variation (\u00B0C)") # month view of demand variation


# export result to CSV
file <- c("NEM_NSW_Analytical.CSV")
subdirectory <- c("data")
myPath <- file.path(wd, subdirectory, file)

write.csv(df, myPath)

# save file
file <- c("NEM_NSW_Analytical.rds")

myPath <- file.path(wd, subdirectory, file)

saveRDS(df, myPath)
