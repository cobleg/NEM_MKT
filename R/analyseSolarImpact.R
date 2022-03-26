
# Objective: analyse the impact of solar irradiance on market price & quantity
# Author: Grant Coble-Neal

wd <- getwd()
file <- c("NEM_NSW_solar_2021_22.rds")
subdirectory <- c("data")
myPath <- file.path(wd, subdirectory, file)
df <- readRDS(myPath)

library(psych)
describe(df)

## Use percentiles to examine the outliers
lower_bound <- quantile(df$Price, 0.025, na.rm=TRUE)
upper_bound <- quantile(df$Price, 0.975, na.rm=TRUE)
library(tidyverse)
df <- df %>% 
  mutate( Outlier = case_when( Price < lower_bound | Price > upper_bound ~ 'outlier',
                               Price >= lower_bound & Price <= upper_bound ~ 'normal'))
df <- df %>% 
  mutate( Outlier_H_L = case_when( Price < lower_bound ~ 'low outlier',
                                   Price > upper_bound ~ 'high outlier',
                                   Price >= lower_bound & Price <= upper_bound ~ 'normal'))

library(openair)
normal <-df[which(df$Outlier == "normal"), ]
outliers <-df[which(df$Outlier == "outlier"), ]

file <- c("NEM_NSW_solar_normal.png")
subdirectory <- c("output")
myPath <- file.path(wd, subdirectory, file)
Cairo(file = myPath, type = 'png', units = "px", width = 862, height = 556) # Save the calendar plot

scatterPlot(normal, x = "CLRSKY_SFC_SW_DWN", y = "Price", xlab = expression("Solar irradiance (Wh/m"^2*")"), ylab="MWh", 
            method = "hexbin", cols = "jet",
            main = "Solar irradiance versus quantity during normal market intervals", 
            sub = "Data source: Prediction of Worldwide Energy Resource (POWER) Project & AEMO")

dev.off()

file <- c("NEM_NSW_solar_price_spikes.png")
subdirectory <- c("output")
myPath <- file.path(wd, subdirectory, file)
Cairo(file = myPath, type = 'png', units = "px", width = 862, height = 556) # Save the calendar plot

scatterPlot(outliers, x = "CLRSKY_SFC_SW_DWN", y = "Price", xlab = expression("Solar irradiance (Wh/m"^2*")"), 
            ylab="MWh", main = "Solar irradiance versus quantity during abnormal spot price intervals",
            sub = "Data source: Prediction of Worldwide Energy Resource (POWER) Project & AEMO",
            method = "hexbin", cols = "jet", type = "air_temp")
dev.off()

# Assess impact by season
file <- c("NEM_NSW_solar_normal_season.png")
subdirectory <- c("output")
myPath <- file.path(wd, subdirectory, file)

library(Cairo)
Cairo(file = myPath, type = 'png', units = "px", width = 862, height = 556) # Save the calendar plot

scatterPlot(normal, x = "ALLSKY_SFC_SW_DWN", y = "Price", xlab = expression("Solar irradiance (Wh/m"^2*")"), ylab="MWh", 
            main = "Solar irradiance versus quantity during normal market intervals", 
            sub = "Data source: Prediction of Worldwide Energy Resource (POWER) Project & AEMO",
            type = "season", layout = c(2,2), linear = T)
dev.off()

file <- c("NEM_NSW_solar_outliers_season.png")
subdirectory <- c("output")
myPath <- file.path(wd, subdirectory, file)

Cairo(file = myPath, type = 'png', units = "px", width = 862, height = 556) # Save the calendar plot

scatterPlot(df[which(df$Outlier_H_L == "low outlier"), ], x = "ALLSKY_SFC_SW_DWN", y = "Price", xlab = expression("Solar irradiance (Wh/m"^2*")"), ylab="MWh", 
            main = "Solar irradiance versus quantity during normal market intervals", 
            sub = "Data source: Prediction of Worldwide Energy Resource (POWER) Project & AEMO",
            type = "season", layout = c(2,2), linear = T, ylim =c(-150, 100))
dev.off()

scatterPlot(df, x = "T2M", y = "Quantity", type = "season", layout = c(2,2), linear = T,
            main = "Effect of air temperature on electricity demand (NSW, 2021-22)",
            xlab = "Air temperature (\u00B0C)",
            ylab = "MWh",
            sub = "Data source: Prediction of Worldwide Energy Resource (POWER) Project & AEMO")
