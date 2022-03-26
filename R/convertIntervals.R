

# Objective: convert 5-minute interval data to 30-minute interval data
# Author: Grant Coble-Neal

library(lubridate)
library(openair)

# NEM_NSW_2021OCT_DEC
wd <- getwd()
subdirectory <- c("data")
file <- c("NEM_NSW_2021OCT_DEC.rds")
myPath <- file.path(wd, subdirectory, file)
df.1 <- readRDS(myPath)

df.1$date_time <- ymd_hms(df.1$SETTLEMENTDATE)

df.2 <- data.frame(date = df.1$date_time, Price = df.1$RRP, Quantity = df.1$TOTALDEMAND)


# load the 2022 data NEM_NSW_2022
file <- c("NEM_NSW_2022.rds")
myPath <- file.path(wd, subdirectory, file)
df.3 <- readRDS(myPath)

df.3$date_time <- ymd_hms(df.3$SETTLEMENTDATE)

df.4 <- data.frame(date = df.3$date_time, Price = df.3$RRP, Quantity = df.3$TOTALDEMAND)

df.5 <- rbind(df.2, df.4)

Half_hour <- timeAverage(df.5, avg.time = "30 min", start.date = ymd_hms("2021-10-01 00:00:00"))

head(Half_hour)
tail(Half_hour)

# delete the last row of the data
Half_hour <- head(Half_hour, -1)

wd <- getwd()
subdirectory <- c("data")
region <- c("NSW")
filePath2 <- file.path(wd, subdirectory, paste0("NEM_half_hour_", region, ".rds"))

# save file
saveRDS(Half_hour, filePath2)
