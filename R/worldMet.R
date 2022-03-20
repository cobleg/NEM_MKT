

# Objective: explore the worldmet package. Does it have meteorological data for Australia?

library(worldmet)
getMeta(lat = -31.95224, lon = 115.8614)
getMeta(lat = -33.86785, lon = 151.20732)

perth_met <- importNOAA(code = "946080-99999", year = 2021)
sydney_met <- importNOAA(code = "947660-99999", year = 2021)

# convert to 30-minute interval
sydney_met_half_hour <- timeAverage(sydney_met, avg.time = "30 min", fill = TRUE)

wd <- getwd()
subdirectory <- c("data")
file <- c("NEM_NSW_2021.rds")
myPath <- file.path(wd, subdirectory, file)
df <- readRDS(myPath)

head(df)
tail(df)
library(lubridate)

df$date <- ymd_hms(df$SETTLEMENTDATE)

# merge the NEM NSW data and the Sydney meteorological data
df_met <- left_join(df, sydney_met_half_hour, by = 'date')
tail(df_met)
df_met <- tail(df_met, -1)
