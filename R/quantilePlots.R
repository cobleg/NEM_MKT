#
# Objective: Create quantile plots of Regional Reference Price (RRP) for a specified year and NEM region.
# This script takes two command-line arguments:
#   1. Year (e.g., 2022)
#   2. NEM Region (e.g., NSW, QLD, VIC, SA, TAS)
#
# Example usage from the PowerShell terminal:
# If current directory is C:\\Users\\User\\R\\NEM_MKT:
#   Rscript R\\quantilePlots.R 2022 NSW
# If current directory is C:\\Users\\User\\R\\NEM_MKT\\R:
#   Rscript quantilePlots.R 2022 NSW
#
# Author: Grant Coble-Neal
# Dependencies: here, tidyverse, TSstudio, httpgd, plotly
# Make sure these packages are installed before running the script.

message("quantilePlots.R script started") # DIAGNOSTIC
message(paste("Initial R working directory:", getwd())) # DIAGNOSTIC

library(here)
message("SUCCESS: library(here) loaded") # DIAGNOSTIC
library(tidyverse)
message("SUCCESS: library(tidyverse) loaded") # DIAGNOSTIC
library(TSstudio)
message("SUCCESS: library(TSstudio) loaded") # DIAGNOSTIC
library(plotly)
message("SUCCESS: library(plotly) loaded") # DIAGNOSTIC
library("httpgd")
message("SUCCESS: library(httpgd) loaded. All libraries loaded.") # DIAGNOSTIC

# Get command line arguments
args <- commandArgs(trailingOnly = TRUE)
message(paste("DEBUG: Raw commandArgs received:", paste(args, collapse=" "))) # DIAGNOSTIC

# Check if the correct number of arguments is provided
if (length(args) != 2) {
  message("ERROR: Incorrect number of arguments provided. Stopping.") # DIAGNOSTIC
  stop(paste("Please provide two arguments: year and region (e.g., Rscript quantilePlots.R 2022 NSW). Received:", length(args), "arguments."), call. = FALSE)
}

year_arg <- args[1]
region_arg <- toupper(args[2]) # Ensure region is uppercase, e.g., NSW
message(paste("INFO: Arguments parsed: year_arg =", year_arg, ", region_arg =", region_arg)) # DIAGNOSTIC

# Construct the file path dynamically
# First, let's see what 'here' thinks is the project root
project_root_path <- tryCatch({
    here::here()
}, error = function(e) {
    paste("Error calling here::here():", e$message)
})
message(paste("DEBUG: Project root according to here::here():", project_root_path)) # DIAGNOSTIC

file_path <- tryCatch({
    here("data", paste0("NEM_regions_", year_arg, ".rds"))
}, error = function(e) {
    message(paste("ERROR: Failed to construct file_path using here():", e$message)) # DIAGNOSTIC
    return(NA) # Return NA or some indicator of failure
})

if (is.na(file_path)) {
    stop("Stopping script due to error in file path construction.", call. = FALSE)
}

# Print the path to the source data
message(paste("INFO: Attempting to load data from resolved file_path:", file_path)) # DIAGNOSTIC

# Check if the RDS file exists
if (!file.exists(file_path)) {
  message(paste("ERROR: RDS file not found at the specified path:", file_path, ". Stopping.")) # DIAGNOSTIC
  stop(paste("RDS file not found:", file_path, "\nPlease ensure the file exists or run the appropriate script to generate it."), call. = FALSE)
}
message(paste("INFO: RDS file confirmed to exist at:", file_path)) # DIAGNOSTIC

NEM_data_all_regions <- readRDS(file_path)
message("INFO: RDS file loaded successfully.") # DIAGNOSTIC

# Ensure output directory exists
output_dir <- here("output")
if (!dir.exists(output_dir)) {
  message(paste("INFO: Output directory not found. Creating directory:", output_dir)) # DIAGNOSTIC
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
} else {
  message(paste("INFO: Output directory already exists:", output_dir)) # DIAGNOSTIC
}

regional.data <- function(df = NEM_data_all_regions, variable = RRP, Region_input = region_arg){
  message(paste("DEBUG: regional.data function called with Region_input:", Region_input)) # DIAGNOSTIC
  
  df.0 <- df %>% filter(
    REGION == paste0(Region_input, 1)
  ) %>% mutate(
    DateTime = lubridate::ymd_hms(paste(Date, Time))
  )  %>%
    mutate(
      hour = lubridate::hour(DateTime)
    ) %>% group_by(
      Date, hour
    ) %>% 
    summarise(
      Variable = mean({{variable}}), .groups = 'drop' # Ensure .groups = 'drop' is applied
    ) %>% mutate(
      DateTime = lubridate::ymd_hm(paste(Date, hour, "-0"))
    ) %>%  ungroup() %>% select(
      DateTime, Variable
    )
  message("INFO: regional.data function executed successfully.") # DIAGNOSTIC
  return(df.0)
}

# Prepare data for plotting
message("INFO: Preparing data for all plots...") # DIAGNOSTIC
processed_data <- regional.data(variable = RRP, Region_input = region_arg)

message("DEBUG: Structure of processed_data (output of regional.data):")
message(paste(capture.output(str(processed_data)), collapse = "\n"))
message("DEBUG: Head of processed_data (output of regional.data):")
message(paste(capture.output(head(processed_data)), collapse = "\n"))

if (is.null(processed_data) || nrow(processed_data) == 0) {
  message(paste("ERROR: No data returned by regional.data for region:", region_arg, "and year:", year_arg, ". Cannot generate plots. Stopping.")) # DIAGNOSTIC
  stop("Stopping script: Data preparation resulted in empty or NULL dataset.", call. = FALSE)
} else {
  message("INFO: Data preparation successful. Proceeding to plotting.") # DIAGNOSTIC
}

# Plotting the quantile of the NEM dataset
message("INFO: Starting plotting sequence...") # DIAGNOSTIC

plot_title_null <- paste("NEM", region_arg, year_arg, "- RRP Quantile Plot")
plot_title_monthly <- paste("NEM", region_arg, year_arg, "- RRP Monthly Quantile Plot")
plot_title_weekdays <- paste("NEM", region_arg, year_arg, "- RRP Weekdays Quantile Plot")

# Helper function to save plots or placeholders
save_plot_or_placeholder <- function(plot_object, filepath, plot_title) {
  if (!is.null(plot_object) && (inherits(plot_object, "plotly") || inherits(plot_object, "htmlwidget"))) {
    message(paste("INFO: ts_quantile returned a plotly object for", plot_title, ". Attempting plotly::save_image to:", filepath))
    tryCatch({
      plotly::save_image(p = plot_object, file = filepath, width = 800, height = 600)
      message(paste("SUCCESS: Plot saved via plotly::save_image to:", filepath))
    }, error = function(e_save_image) {
      message(paste("ERROR: plotly::save_image failed for", plot_title, ":", e_save_image$message))
      message("       INFO: This might be because the 'kaleido' package is not installed or properly configured.")
      message("       INFO: Please ensure you have run: install.packages(\"kaleido\") in your R console.")
      # Fallback to placeholder
      png(filename = filepath, width = 800, height = 600)
      plot.new()
      text(0.5, 0.5, paste("Placeholder: plotly::save_image failed for\n", plot_title, "\nCheck Kaleido installation."), cex = 0.9, col = "red")
      dev.off()
      message(paste("INFO: Placeholder image saved to:", filepath))
    })
  } else {
    if(is.null(plot_object)) {
      message(paste("WARNING: ts_quantile returned NULL for", plot_title, ". Saving placeholder image to:", filepath))
    } else {
      message(paste("WARNING: ts_quantile for", plot_title, "returned an unexpected object type (class:", class(plot_object)[1], "). Saving placeholder image to:", filepath))
    }
    png(filename = filepath, width = 800, height = 600)
    plot.new()
    text(0.5, 0.5, paste("Placeholder: Plot generation issue for\n", plot_title), cex = 1, col = "red")
    dev.off()
    message(paste("INFO: Placeholder image saved to:", filepath))
  }
}

# Null period plot
filename_null <- paste0("NEM_", region_arg, "_", year_arg, "_RRP_Quantile_NULL.png")
filepath_null <- here("output", filename_null)
plot_obj_null <- ts_quantile(processed_data,
                             upper = 0.9, lower = 0, period = NULL,
                             title = plot_title_null, Ytitle = "Regional Reference Price ($/MWh)")
save_plot_or_placeholder(plot_obj_null, filepath_null, plot_title_null)

# Monthly plot
filename_monthly <- paste0("NEM_", region_arg, "_", year_arg, "_RRP_Quantile_Monthly.png")
filepath_monthly <- here("output", filename_monthly)
plot_obj_monthly <- ts_quantile(processed_data,
                                upper = 0.9, lower = 0, period = "monthly",
                                title = plot_title_monthly, Ytitle = "Regional Reference Price ($/MWh)")
save_plot_or_placeholder(plot_obj_monthly, filepath_monthly, plot_title_monthly)

# Weekdays plot
filename_weekdays <- paste0("NEM_", region_arg, "_", year_arg, "_RRP_Quantile_Weekdays.png")
filepath_weekdays <- here("output", filename_weekdays)
plot_obj_weekdays <- ts_quantile(processed_data,
                                 upper = 0.9, lower = 0, period = "weekdays",
                                 title = plot_title_weekdays, Ytitle = "Regional Reference Price ($/MWh)")
save_plot_or_placeholder(plot_obj_weekdays, filepath_weekdays, plot_title_weekdays)

# Indicate script completion and plot display
message("DEBUG: Reached end of script logic before final print statement.") # DIAGNOSTIC
print(paste("Script completed successfully. Quantile plots for", region_arg, year_arg, "have been saved as PNG files in the 'output' directory."))
if (exists("hgd") && !is.null(hgd())) {
  message(paste("httpgd graphics device is also active. Plots might have also been sent there."))
}
message("INFO: Script execution completed.") # DIAGNOSTIC
