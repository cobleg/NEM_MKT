

# Objective: prepare data set to compare NSW settlement prices before and after the introduction of 5-minute settlement
#            which occurred on 1 October 2021
# Author: Grant Coble-Neal

# load the data for 2021 for the period 1 January 2021 to 30 September 2021

wd <- getwd()
subdirectory <- c("data")
file <- c("NEM_NSW_2021.rds")
myPath <- file.path(wd, subdirectory, file)
df <- readRDS(myPath)

# inspect the data
head(df)
tail(df)

# convert the time series from class 'character' to class POSIXct
library(lubridate)
df.1 <- data.frame(date = ymd_hms(df$SETTLEMENTDATE),  Price = df$RRP)

# inspect the data
head(df.1)
tail(df.1)

# delete the last row of the data
df.1 <- head(df.1, -1)

# load the data for 2021 for the period 1 October 2021 to 30 September 2022
# note that this has been aggregated to 30-minute intervals

file <- c("NEM_half_hour_NSW.rds")
myPath <- file.path(wd, subdirectory, file)
df.2 <- readRDS(myPath)

# inspect the data
head(df.2)
tail(df.2)

# combine the two data frames
df.3 <- rbind(df.1, df.2)

# inspect the data
head(df.3)
tail(df.3)


# delete the last row of the data
df.3 <- head(df.3, -1)

# Add an indicator variable (before, after) to allow subsetting
library(dplyr)
df.3 <- df.3 %>% mutate(Indicator = case_when(date <  ymd_hms("2021-10-01 00:00:00") ~ "Before",
                                      date >= ymd_hms("2021-10-01 00:00:00") ~ "After"))
head(df.3)
tail(df.3)

wd <- getwd()
subdirectory <- c("data")
region <- c("NSW")
dateRange <- c("_20210101_20220228")
filePath2 <- file.path(wd, subdirectory, paste0("NEM_half_hour_", region, dateRange, ".rds"))

# save file
saveRDS(df.3, filePath2)
