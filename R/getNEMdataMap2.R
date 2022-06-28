
# Objective: get and compile NEM market data across all regions using map2()
# Author: Grant Coble-Neal

library(purrr)
library(stringr)
years <- c(2022)

prefix <- c("PRICE_AND_DEMAND_")
regions <- c("NSW", "QLD", "VIC", "SA", "TAS")

filePath <- c("https://aemo.com.au/aemo/data/nem/priceanddemand/")

subdirectory <- c("data")
wd <- getwd()
filePath2 <- map2(years, regions, ~file.path(wd, subdirectory, paste0("NEM_", .y, "_" ,.x, ".rds")))
