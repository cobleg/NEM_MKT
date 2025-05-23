

# Objective: explore the worldmet package. Does it have meteorological data for Australia?
# Author: Grant Coble-Neal
# Dependencies: none
# Outputs: 
#         (1) NEM_weather (an hourly time series of temperature, relative humidity etc by weather station)
#         (2) NEM_weather.5mins (a five minute time series of temperature and relative humidity by weather station)
#         (3) sydney_met_half_hour (a half-hour weather temperature data frame for Sydney, NSW)

library(here)
library(tidyverse)
library(worldmet)
getMeta(lat = -31.95224, lon = 115.8614) # Perth
getMeta(lat = -33.86785, lon = 151.20732) # Sydney
getMeta(lat = -37.814, lon = 144.96) # Melbourne
getMeta(lat = -27.4694, lon = 153.02809) # Brisbane
getMeta(lat = -34.906101, lon = 138.5393903) # Adelaide
getMeta(lat = -42.87936, lon = 147.32941) # Hobart

# WEM region
perth_met <- importNOAA(code = "946080-99999", year = 2022)

# NEM regions
brisbane_met <- importNOAA(code = "945780-99999", year = 2021)
sydney_met <- importNOAA(code = "947660-99999", year = 2021)
sydney_met.2 <- importNOAA(code = "947660-99999", year = 2022)
melbourne_met <- importNOAA(code = "959360-99999", year = 2021)
hobart_met <- importNOAA(code = "949700-99999", year = 2021)
adelaide_met <- importNOAA(code = "946720-99999", year = 2021)

NEM_weather <- dplyr::bind_rows(brisbane_met, sydney_met, melbourne_met, hobart_met, adelaide_met)

library(lubridate)
sydney_met.2$date <- lubridate::ymd_hms(sydney_met.2$date)
# sydney_met.2 <- sydney_met.2[which(sydney_met.2$date <= ymd_hms(c("2022-02-28 23:00:00"))),]

sydney_met.3 <- rbind(sydney_met, sydney_met.2)

NEM_weather$DateTime <- lubridate::ymd_hms(NEM_weather$date)

# To Do: convert to 5 minute data to match NEM operational demand timeseries
library(padr)

NEM_weather <- NEM_weather %>% 
  pad(by = "DateTime", group = "station") %>% 
  fill_by_function(fun = mean)

start.date <- min(NEM_weather$DateTime)
end.date <- max(NEM_weather$DateTime)
group.names <- unique(NEM_weather$station)

NEM_weather.5mins <- tibble(
  DateTime = rep(seq(from = start.date, to = end.date, by = "5 min"), each = length(group.names)),
) %>% 
  mutate(
    station = rep(rep(group.names), length.out = length(DateTime))
  )

NEM_weather.5mins <- left_join(NEM_weather.5mins, NEM_weather, by = c("DateTime", "station")) %>% 
  select(DateTime, station, air_temp, dew_point, RH) %>% 
  pad(by = "DateTime", group = "station") %>% 
  fill_by_function(fun = mean)

# save the NEM_weather.5mins
saveRDS(NEM_weather.5mins, here("data", "NEM_weather_5M_2021.rds"))
  
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
