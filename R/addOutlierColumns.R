

# Objective: add factor variables to classify market intervals by outlier (low, high) and normal
# Author: Grant Coble-Neal

wd <- c("C:\\Users\\User\\Google Drive\\R\\NEM_MKT")
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

file <- c("NEM_NSW_solar_2021_22_A.rds")
subdirectory <- c("data")
myPath <- file.path(wd, subdirectory, file)

write_rds(df, myPath)
