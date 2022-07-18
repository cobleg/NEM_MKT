

# Objective: explore the worldmet package. Does it have meteorological data for Australia?

library(worldmet)
getMeta(lat = -31.95224, lon = 115.8614)
getMeta(lat = -33.86785, lon = 151.20732)

perth_met <- importNOAA(code = "946080-99999", year = 2022)
sydney_met <- importNOAA(code = "947660-99999", year = 2021)
sydney_met.2 <- importNOAA(code = "947660-99999", year = 2022)

# truncate rows after 28 February 2022

library(lubridate)
sydney_met.2$date <- ymd_hms(sydney_met.2$date)
sydney_met.2 <- sydney_met.2[which(sydney_met.2$date <= ymd_hms(c("2022-02-28 23:00:00"))),]

sydney_met.3 <- rbind(sydney_met, sydney_met.2)

# convert to 30-minute interval

sydney_met_half_hour <- timeAverage(sydney_met.3, avg.time = "30 min", fill = TRUE)

wd <- getwd()
subdirectory <- c("data")
file <- c("NEM_half_hour_NSW_20210101_20220228.rds")
myPath <- file.path(wd, subdirectory, file)
df <- readRDS(myPath)

head(df)
tail(df)

# merge the NEM NSW data and the Sydney meteorological data
library(tidyverse)
df_met <- left_join(df, sydney_met_half_hour, by = 'date')
tail(df_met)

library(Cairo)
file <- c("NSW_scatter_air_temp_vs_Price.png")
subdirectory <- c("output")
myPath <- file.path(wd, subdirectory, file)
Cairo(file = myPath, type = 'png', units = "px", width = 862, height = 556) # Save the calendar plot

scatterPlot(df_met, x = "air_temp", y = "Price", ylab = "$/MWh", xlab = "Celsius", main = "NEM NSW region 2021 - February 2022")
dev.off()

# save file
file <- c("NSW_Met_2021.rds")
subdirectory <- c("data")
myPath <- file.path(wd, subdirectory, file)

saveRDS(df_met, myPath)
