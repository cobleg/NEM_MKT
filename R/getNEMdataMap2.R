
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
  ) %>% 
  mutate(
    Year = lubridate::year(Date),
    Month = lubridate::month(Date)
  ) %>% 
  mutate(
    Season = case_when(
      Month %in% c(1,2,12) ~ "Summer",
      Month %in% c(3,4,5) ~ "Autumn",
      Month %in% c(6,7,8) ~ "Winter",
      Month %in% c(9,10,11) ~ "Spring",
      TRUE ~ "NA"
    )
  ) %>% select(
    REGION, Season, Date, Year, Month, Time, TOTALDEMAND, RRP
  )  -> NEM_regions_data.tb 

# create meta data file
Meta.Data <- data.frame(
  "Variable" = c("REGION", "Season", "Date", "Year", "Month", "Time",  "TOTALDEMAND", "RRP"),
  "UoM" = c(rep(NA,3), "years", "months",  "Hours:Minutes:Seconds", "MW", "$/MW")
)

# Create summary statistics
NEM_regions_data.tb %>% pivot_longer(
  !c(REGION, Season, Date, Year, Month, Time),
  names_to = "Series",
  values_to = "Values"
) %>% group_by(
  REGION, Season, Year, Month, Time, Series
) %>% summarise(
  quantile.values = quantile(Values, c(0, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975, 0.99, 1)),
  quantiles = c(0, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975, 0.99, 1)
) -> NEM_summary.stats
  
# save files
fileName <- paste0("NEM_regions_", years, ".rds" )
saveRDS(NEM_regions_data, file.path(wd, subdirectory, fileName))

map( filePath2, ~saveRDS(NEM_regions_data, .x) )

# export to Excel
library(openxlsx)
wb <- createWorkbook()
addWorksheet(wb, "Data")
addWorksheet(wb, "Meta Data")

writeData(wb, "Data", NEM_regions_data.tb)
writeData(wb, "Meta Data", Meta.Data)

saveWorkbook(wb, file = here("data", "NEM_Data.xlsx"), overwrite = TRUE )

# export summary stats to Excel
NEM_summary.stats %>% group_by(REGION, Series) %>% 
  group_split() -> NEM_Summary_List

# Reference: http://optimumsportsperformance.com/blog/r-tips-tricks-write-data-to-separate-excel-sheet-tabs/
blank_excel <- createWorkbook()

Map(function(df, tab_number)
  {
  addWorksheet(blank_excel, paste0("Sheet",tab_number))
  writeData(blank_excel, paste0("Sheet",tab_number), df)
  },
  NEM_Summary_List, 1:length(NEM_Summary_List)
)

saveWorkbook(blank_excel, file = here("data", "Summary_Stats.xlsx"), overwrite = TRUE)

