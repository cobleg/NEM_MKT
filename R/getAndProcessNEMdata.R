#
# Objective: This script downloads, processes, and restructures National Electricity Market (NEM) data.
# It fetches data from the AEMO website, cleans it, calculates summary statistics,
# and saves the processed data as RDS files and an Excel file.
#
# Author: Grant Coble-Neal
# Dependencies: here, purrr, stringr, tidyr, tidyverse, readr, lubridate, openxlsx
# Ensure these packages are installed before running the script.
# Example: install.packages(c("here", "purrr", "stringr", "tidyr", "tidyverse", "readr", "lubridate", "openxlsx"))

# Load required libraries
library(here)
library(purrr)
library(stringr)
library(tidyr)
library(tidyverse)
library(readr)
library(lubridate)
library(openxlsx)

# Define script parameters
script_parameters <- list(
  prefix = "PRICE_AND_DEMAND_",
  regions = c("NSW", "QLD", "VIC", "SA", "TAS"),
  years = rep(2022, length(months)), # Modify the length of years to match months
  months = str_pad(seq(1:12), 2, "left", pad = "0"),
  subdirectory = "data",
  filePath = "https://aemo.com.au/aemo/data/nem/priceanddemand/"
)

# Define helper functions
construct_file_path <- function(directory, region, year) {
  file.path(directory, paste0("NEM_", region, "_", year, ".rds"))
}

construct_file_name <- function(prefix, year, month, region) {
  paste0(prefix, year, month, "_", region, "1.csv")
}

get_data <- function(url) {
  read_csv(url)
}

clean_data <- function(data) {
  data %>%
    mutate(
      DateTime = as_datetime(SETTLEMENTDATE),
      Date = as_date(SETTLEMENTDATE),
      Time = format(as.POSIXct(SETTLEMENTDATE), format = "%H:%M:%S"),
      Year = year(Date),
      Month = month(Date),
      Season = case_when(
        Month %in% c(1, 2, 12) ~ "Summer",
        Month %in% c(3, 4, 5) ~ "Autumn",
        Month %in% c(6, 7, 8) ~ "Winter",
        Month %in% c(9, 10, 11) ~ "Spring",
        TRUE ~ "NA"
      )
    ) %>%
    select(
      REGION, Season, Date, Year, Month, Time, TOTALDEMAND, RRP
    )
}

create_meta_data <- function() {
  data.frame(
    "Variable" = c("REGION", "Season", "Date", "Year", "Month", "Time", "TOTALDEMAND", "RRP"),
    "UoM" = c(rep(NA, 3), "years", "months", "Hours:Minutes:Seconds", "MW", "$/MW")
  )
}

create_summary_stats <- function(data) {
  data %>%
    pivot_longer(
      !c(REGION, Season, Date, Year, Month, Time),
      names_to = "Series",
      values_to = "Values"
    ) %>%
    group_by(REGION, Season, Year, Month, Time, Series) %>%
    summarise(
      quantile.values = quantile(Values, c(0, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975, 0.99, 1)),
      quantiles = c(0, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975, 0.99, 1)
    )
}

save_data <- function(data, file_path) {
  saveRDS(data, file_path)
}

save_summary_stats <- function(df, directory, fileName = "Summary_Stats.xlsx") {
  df %>% group_by(REGION, Series) %>%
    group_split() -> NEM_Summary_List
  
  blank_excel <- createWorkbook()
  
  Map(function(df, tab_number)
  {
    addWorksheet(blank_excel, paste0("Sheet",tab_number))
    writeData(blank_excel, paste0("Sheet",tab_number), df)
  },
  NEM_Summary_List, 1:length(NEM_Summary_List)
  )
  
  saveWorkbook(blank_excel, file = here("data", fileName), overwrite = TRUE)
}

# Main script
main <- function() {
  params <- script_parameters
  
  # Create subdirectory if it doesn't exist
  dir.create(params$subdirectory, showWarnings = FALSE)
  
  # Construct file paths and URLs
  filePath2 <- map2(params$years, params$regions, construct_file_path, directory = params$subdirectory)
  filePath3 <- map2(params$years, params$months, construct_file_path, directory = params$subdirectory)
  fileNames <- map2(params$years, params$months, construct_file_name, prefix = params$prefix, region = params$regions)
  myURLs <- map(fileNames, ~ paste0(params$filePath, .x))
  
  # Read and clean data
  df.list <- map(myURLs, get_data)
  NEM_regions_data <- bind_rows(df.list) %>%
    clean_data()
  
  # Save data
  save_data(NEM_regions_data, file.path(params$subdirectory, paste0("NEM_regions_", params$years, ".rds")))
  walk2(df.list, filePath3, save_data)
  
  # Create metadata
  Meta.Data <- create_meta_data()
  
  # Create summary statistics
  NEM_summary.stats <- create_summary_stats(NEM_regions_data)
  
  # Save data and summary statistics to Excel
  wb <- createWorkbook()
  addWorksheet(wb, "Data")
  addWorksheet(wb, "Meta Data")
  
  writeData(wb, "Data", NEM_regions_data)
  writeData(wb, "Meta Data", Meta.Data)
  
  saveWorkbook(wb, file = here(params$subdirectory, "NEM_Data.xlsx"), overwrite = TRUE)
  
  save_summary_stats(df = NEM_summary.stats, directory = params$subdirectory)
}

# Run the main script
main()
