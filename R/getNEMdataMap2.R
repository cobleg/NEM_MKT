# Objective: get and compile NEM market data across all regions for a specified year using map2().
# The year is provided as a command-line argument.
# Author: Grant Coble-Neal
# Reference: Function programming with purrr, https://rpubs.com/cliex159/867722
#
# Example Usage:
# Rscript getNEMdataMap2.R 2025

library(here)
library(purrr)
library(stringr)
library(tidyr)
library(tidyverse)

# Get command line arguments
args <- commandArgs(trailingOnly = TRUE)

# Check if the correct number of arguments is provided
if (length(args) != 1) {
  stop("Usage: Rscript getNEMdataMap2.R <year>\nExample: Rscript getNEMdataMap2.R 2023", call. = FALSE)
}

year_arg <- as.integer(args[1])

# Validate year input
if (is.na(year_arg)) {
  stop("Invalid year provided. Year must be an integer.", call. = FALSE)
}

print(paste("Selected year:", year_arg))

# Determine months to download based on the year_arg
current_sys_year <- as.integer(format(Sys.Date(), "%Y"))
current_sys_month <- as.integer(format(Sys.Date(), "%m"))
months_sequence <- integer(0) # Default to empty sequence

if (year_arg == current_sys_year) {
  if (current_sys_month > 1) {
    months_sequence <- 1:(current_sys_month - 1)
  }
  print(paste("Current year detected. Attempting to download months:", paste(months_sequence, collapse=", ")))
} else if (year_arg < current_sys_year) {
  months_sequence <- 1:12
  print(paste("Past year detected. Attempting to download all 12 months."))
} else { # year_arg > current_sys_year
  print(paste("Future year detected. No data will be downloaded for year:", year_arg))
}

if (length(months_sequence) > 0) {
  months <- str_pad(months_sequence, 2, "left", pad = "0")
} else {
  months <- character(0) # Ensure months is an empty character vector if no months to process
}

# set up script parameters
prefix <- c("PRICE_AND_DEMAND_")
regions <- c("NSW", "QLD", "VIC", "SA", "TAS")
years <- year_arg # Use the command-line argument for years
subdirectory <- c("data")
wd <- getwd()
filePath <- c("https://aemo.com.au/aemo/data/nem/priceanddemand/")

# Only proceed if there are months to process
if (length(months) > 0) {
  # construct lists
  files <- map2(years, months,  ~c(paste0(prefix, .x, .y )) )   # name of target files on AEMO web site
  files <-  map(regions, ~as.list(paste0(files, "_",.x,"1.csv" )) )
  files <- do.call(c, unlist(files, recursive = FALSE))

  myURLs <- map( files, ~paste0(filePath, .x) )
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
} else {
  print(paste("No months to process for year", year_arg, ". Script will not download or process data."))
}

