

# Objective: convert 5-minute interval data to 30-minute interval data
# Author: Grant Coble-Neal

library(lubridate)
library(openair)

df3$date_time <- ymd_hms(df3$SETTLEMENTDATE)

df4 <- data.frame(date = df3$date_time, Price = df3$RRP)

Half_hour <- timeAverage(df4, avg.time = "30 min", start.date = ymd_hms("2021-10-01 00:00:00"))




wd <- getwd()
subdirectory <- c("data")
region <- c("NSW")
filePath2 <- file.path(wd, subdirectory, paste0("NEM_half_hour_", region, ".rds"))

# save file
saveRDS(Half_hour, filePath2)
