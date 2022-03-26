
# Objective: add solar irradiance data to the analytical data set
# Source: https://power.larc.nasa.gov/data-access-viewer/
# Author: Grant Coble-Neal

# NASA/POWER CERES/MERRA2 Native Resolution Hourly Data 							
# Dates (month/day/year): 01/01/2021 through 02/28/2022 							
# Location: Latitude  -33.8678   Longitude 151.2073 							
# Elevation from MERRA-2: Average for 0.5 x 0.625 degree lat/lon region = 31.72 meters							
# The value for missing source data that cannot be computed or is outside of the sources availability range: -999 							
# Parameter(s): 							
# T2M                     MERRA-2 Temperature at 2 Meters (C) 							
# T2MDEW                  MERRA-2 Dew/Frost Point at 2 Meters (C) 							
# T2MWET                  MERRA-2 Wet Bulb Temperature at 2 Meters (C) 							
# ALLSKY_SFC_SW_DWN       CERES SYN1deg All Sky Surface Shortwave Downward Irradiance (Wh/m^2) 							
# CLRSKY_SFC_SW_DWN       CERES SYN1deg Clear Sky Surface Shortwave Downward Irradiance (Wh/m^2) 							
# ALLSKY_KT               CERES SYN1deg All Sky Insolation Clearness Index (dimensionless) 							
# ALLSKY_SRF_ALB          CERES SYN1deg All Sky Surface Albedo (dimensionless) 							
# SZA                     CERES SYN1deg Solar Zenith Angle (Degrees) 							
# ALLSKY_SFC_PAR_TOT      CERES SYN1deg All Sky Surface PAR Total (W/m^2) 							
# CLRSKY_SFC_PAR_TOT      CERES SYN1deg Clear Sky Surface PAR Total (W/m^2) 							
# ALLSKY_SFC_UVA          CERES SYN1deg All Sky Surface UVA Irradiance (W/m^2) 							
# ALLSKY_SFC_UVB          CERES SYN1deg All Sky Surface UVB Irradiance (W/m^2) 							
# ALLSKY_SFC_UV_INDEX     CERES SYN1deg All Sky Surface UV Index (dimensionless) 							


wd <- getwd()
subdirectory <- c("data")
file <- c("POWER_Point_Hourly_20210101_20220228_033d8678S_151d2073E_LST.csv")
myPath <- file.path(wd, subdirectory, file)
df <- read.csv(myPath, header = T, na.strings = -999)

# add date-time index
df$date <- as.Date(paste0(df$YEAR,"/", df$MO,"/", df$DY,"/"), format = "%Y/%m/%d")

library(chron)
df$time <- chron(time = paste0(df$HR, ":00:00"))

library(lubridate)
df$date <- ymd_hms(paste0(df$date, df$time))

# convert to half-hour
library(openair)
df_solar_half_hour <- timeAverage(df, avg.time = "30 min", fill = TRUE)

# import the prepared analytical data set 
wd <- getwd()
subdirectory <- c("data")
file <- c("NEM_NSW_Analytical.rds")
myPath <- file.path(wd, subdirectory, file)
df <- readRDS(myPath)

# merge the NEM NSW analytical data and the NASA solar irradiance data
library(tidyverse)
df_solar <- left_join(df_solar_half_hour, df, by = 'date')
head(df_solar)
tail(df_solar)

# save file
file <- c("NEM_NSW_solar_2021_22.rds")
subdirectory <- c("data")
myPath <- file.path(wd, subdirectory, file)

saveRDS(df_solar, myPath)

# export result to CSV
file <- c("NEM_NSW_solar_2021_22.CSV")
subdirectory <- c("data")
myPath <- file.path(wd, subdirectory, file)
write.csv(df_solar, myPath)
