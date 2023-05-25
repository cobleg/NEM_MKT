
# Objective: get and compile NEM market data across all regions using map2()
# Author: Grant Coble-Neal
# Reference: Function programming with purrr, https://rpubs.com/cliex159/867722

library(here)
library(purrr)
library(stringr)
library(tidyr)
library(tidyverse)

# set up script parameters
prefix <- c("PRICE_AND_DEMAND_")
regions <- c("NSW", "QLD", "VIC", "SA", "TAS")
years <- c(2022)
months <-  str_pad(seq(1:12), 2, "left", pad="0")
subdirectory <- c("data")
wd <- getwd()
filePath <- c("https://aemo.com.au/aemo/data/nem/priceanddemand/")

# construct lists

filePath2 <- map2(years, regions, ~file.path(wd, subdirectory, paste0("NEM_", .y, "_" ,.x, ".rds"))) # path to save data files
map(filePath2,~head(.x))

files <- map2(years, months,  ~c(paste0(prefix, .x, .y )) )   # name of target files on AEMO web site
files <-  map(regions, ~as.list(paste0(files, "_",.x,"1.csv" )) )
files <- do.call(c, unlist(files, recursive = FALSE))

myURLs <- map( files, ~paste0(filePath, .x) )
# df.list <- map( myURLs, ~read.csv(.x) )
df.list <- map( myURLs, ~readr::read_csv(.x) )
map(files,~head(.x))
walk(files, print)

# append files
NEM_regions_data = do.call(rbind, df.list)

# modify the dataframe
NEM_regions_data %>% as_tibble() %>% 
  mutate(
    DateTime = lubridate::as_datetime(SETTLEMENTDATE),
    Date = lubridate::as_date(SETTLEMENTDATE),
    Time = format(as.POSIXct(SETTLEMENTDATE), format = "%H:%M:%S")
  ) %>% select(
    REGION, Date, Time, TOTALDEMAND, RRP, PERIODTYPE
  )  -> NEM_regions_data.tb 

# save files
fileName <- paste0("NEM_regions_", years, ".rds" )
saveRDS(NEM_regions_data, file.path(wd, subdirectory, fileName))

map( filePath2, ~saveRDS(NEM_regions_data, .x) )

# export to Excel
library(openxlsx)
write.xlsx(NEM_regions_data.tb, file = here("data", "NEM_Data.xlsx"), sheetName = 'Data', rowNames=FALSE, asTable = TRUE)
