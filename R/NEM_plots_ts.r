# SimplePlot.R
#
# This script performs the following actions:
# 1. Loads National Electricity Market (NEM) data for all regions from an RDS file.
# 2. Identifies all unique NEM regions present in the loaded data.
# 3. Defines a function `regional.data` to process the data for a specified region:
#    a. Filters the raw data for the given NEM region.
#    b. Parses the `SETTLEMENTDATE` (character) into a full POSIXct `FullDateTime` object.
#    c. Determines an appropriate timezone from the `FullDateTime` data, defaulting to UTC if needed.
#    d. Aggregates RRP (Regional Reference Price) and TOTALDEMAND 
#       to an hourly frequency by summing values within each hour.
#    e. Constructs a final `DateTime` column representing the start of each hour.
#    f. Returns a tibble containing the hourly `DateTime`, `RRP_Value`, and `TOTALDEMAND_Value`.
# 4. Iterates through each unique NEM region:
#    a. Calls the `regional.data` function to get the processed hourly data for the current region.
#    b. Extracts the year from the processed data (if available) for use in plot titles/subtitles.
#    c. Generates a base R line plot showing RRP (left y-axis, $/MWh) and TOTALDEMAND (right y-axis, MWh).
#       - The plot title includes the region and year.
#       - The plot is saved as a PNG file with a dynamic name (e.g., "NEM_plot_REGION_YEAR.png") into an 'output' directory.
#    d. Generates a ggplot2 line plot using facets for RRP and TOTALDEMAND.
#       - Each facet will have its own y-axis scale, implicitly representing $/MWh for RRP and MWh for TOTALDEMAND.
#       - The plot title and subtitle dynamically include the region and year.
#       - The x-axis is formatted to show "Year-Month".
#       - The plot is saved as a PNG file with a dynamic name (e.g., "NEM_ggplot_REGION_YEAR.png") into an 'output' directory.
#    e. Includes error handling for plotting: if data is insufficient or invalid,
#       placeholder PNG images are generated and saved.
#
# Libraries used: here, tidyverse, TSstudio, plotly, lubridate.
# Input: data/NEM_regions_2025.rds (or as specified by rds_file_path)
# Output: Multiple PNG plot files (e.g., output/NEM_plot_NSW1.png, output/NEM_ggplot_NSW1.png).
#         Diagnostic messages to the console.

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

# attached the NEM_data_all_regions data
rds_file_path <- here("data", "NEM_regions_2025.rds")
NEM_data_all_regions <- readRDS(rds_file_path)
message(paste("SUCCESS: NEM_data_all_regions loaded from", rds_file_path)) # DIAGNOSTIC
print(head(NEM_data_all_regions))

# Get all unique regions
all_regions <- unique(NEM_data_all_regions$REGION)
message(paste("INFO: Found regions:", paste(all_regions, collapse = ", ")))

# Ensure the output directory exists
dir.create(here("output"), showWarnings = FALSE, recursive = TRUE)

regional.data <- function(df = NEM_data_all_regions, Region_input = "NSW1") {
  message(paste("DEBUG: regional.data function called with Region_input:", Region_input))

  # Filter data for the specified region
  df_filtered <- df %>% filter(REGION == Region_input)

  # Check if filtering resulted in empty data
  if (nrow(df_filtered) == 0) {
    message(paste("WARNING: No data found for Region_input:", Region_input, ". Returning empty tibble."))
    # Return an empty tibble with the expected column structure
    return(tibble(DateTime = as.POSIXct(character()), RRP_Value = numeric(), TOTALDEMAND_Value = numeric()))
  }

  # Create FullDateTime and determine timezone from it
  df_with_fulldatetime <- df_filtered %>% 
    mutate(
      FullDateTime = lubridate::ymd_hms(SETTLEMENTDATE) # Parse full date-time
    )

  # Determine target timezone from the first valid FullDateTime
  # Default to UTC if no valid timezone can be extracted or if all are NA
  target_tz <- "UTC" 
  if (nrow(df_with_fulldatetime) > 0 && "FullDateTime" %in% names(df_with_fulldatetime)) {
    # Get the first non-NA POSIXct value from FullDateTime
    first_valid_dt_value <- first(stats::na.omit(df_with_fulldatetime$FullDateTime))
    
    # Check if first_valid_dt_value is a valid POSIXct object
    if (inherits(first_valid_dt_value, "POSIXct") && !is.na(first_valid_dt_value)) {
      extracted_tz <- lubridate::tz(first_valid_dt_value)
      # Ensure the extracted timezone string is valid and not empty
      if (length(extracted_tz) > 0 && !is.na(extracted_tz) && nzchar(extracted_tz)) {
        target_tz <- extracted_tz
      }
    }
  }
  message(paste("DEBUG: Using timezone for make_datetime:", target_tz))

  df.0 <- df_with_fulldatetime %>% # Continue pipe from df_with_fulldatetime
    mutate(
      DateOnly = as.Date(FullDateTime),          # Extract Date part
      HourOnly = lubridate::hour(FullDateTime)   # Extract Hour part
    ) %>%
    group_by(
      DateOnly, HourOnly                         # Group by Date and Hour
    ) %>%
    summarise(
      RRP_Value = mean(RRP, na.rm = TRUE), # Use mean for RRP
      TOTALDEMAND_Value = sum(TOTALDEMAND, na.rm = TRUE), # Use sum for TOTALDEMAND
      .groups = 'drop'                           # Drop grouping structure after summarise
    ) %>%
    mutate(
      # Create the final DateTime column at the start of the hour
      DateTime = lubridate::make_datetime(
        year = lubridate::year(DateOnly),
        month = lubridate::month(DateOnly),
        day = lubridate::day(DateOnly),
        hour = HourOnly,
        min = 0,
        sec = 0,
        tz = target_tz # Use the determined timezone
      )
    ) %>%
    select(DateTime, RRP_Value, TOTALDEMAND_Value) # Select final columns

  message("INFO: regional.data function executed successfully.")
  return(df.0)
}

# Loop through each region to generate and save plots
for (current_region in all_regions) {
  message(paste("\\nPROCESSING REGION:", current_region)) # DIAGNOSTIC

  year_arg <- NA # Initialize year_arg for the current region

  regional_data_df <- regional.data(Region_input = current_region)

  # Extract year from data if available
  if (nrow(regional_data_df) > 0 && "DateTime" %in% names(regional_data_df)) {
    first_valid_datetime <- first(stats::na.omit(regional_data_df$DateTime))
    if (inherits(first_valid_datetime, "POSIXct") && !is.na(first_valid_datetime)) {
      year_arg <- lubridate::year(first_valid_datetime)
    }
  }
  message(paste("DEBUG: Year for", current_region, ":", year_arg)) # DIAGNOSTIC

  # Skip saving the CSV for now, focus on plots per region
  # write.csv(regional_data_df, file = here(paste0("NEM_data_", current_region, ".csv")), row.names = FALSE)

  # Save a single image
  if (nrow(regional_data_df) > 0 && 
      sum(!is.na(regional_data_df$DateTime)) > 0 && 
      sum(!is.na(regional_data_df$RRP_Value)) > 0 && # Check RRP_Value
      sum(!is.na(regional_data_df$TOTALDEMAND_Value)) > 0) { # Check TOTALDEMAND_Value

    # Plot the data    
    ## Check for finite range in y-values as well
    finite_y_range_RRP <- range(regional_data_df$RRP_Value, na.rm = TRUE)
    finite_y_range_TD <- range(regional_data_df$TOTALDEMAND_Value, na.rm = TRUE)
    
    colour_palette_macquarie <- c("#000000", "#FFFFFF","#D6D2C4", "#E6E4DC", "#A6192E", "#C6007E", "#80225F")
    
    if (all(is.finite(finite_y_range_RRP)) && all(is.finite(finite_y_range_TD))) {
      # Base R plot
      base_plot_filename <- paste0("NEM_plot_", current_region, ".png")
      png(here("output", base_plot_filename), width = 1000, height = 700) # Increased width for two axes
      
      par(mar = c(5, 4, 4, 5) + 0.1) # Increase right margin for secondary axis

      # Plot RRP
      plot(
        x = regional_data_df$DateTime,
        y = regional_data_df$RRP_Value,
        type = "l",
        col = colour_palette_macquarie[5], # A6192E for RRP
        xlab = "DateTime (Year-Month)",
        ylab = "RRP ($/MWh)",
        main = paste("NEM Data -", current_region, ifelse(!is.na(year_arg), paste("-", year_arg), "")),
        xaxt = "n",
        ylim = finite_y_range_RRP # Set ylim for RRP
      )
      axis_ticks <- pretty(regional_data_df$DateTime)
      axis.POSIXct(1, at = axis_ticks, format = "%Y-%m")

      par(new = TRUE) # Allow new plot on top

      # Plot TOTALDEMAND
      plot(
        x = regional_data_df$DateTime,
        y = regional_data_df$TOTALDEMAND_Value,
        type = "l",
        col = colour_palette_macquarie[1], # Black for TOTALDEMAND
        xaxt = "n", yaxt = "n", # No new axes
        xlab = "", ylab = "",
        ylim = finite_y_range_TD # Set ylim for TOTALDEMAND
      )
      axis(4, pretty(finite_y_range_TD)) # Secondary y-axis on the right
      mtext("TOTALDEMAND (MWh)", side = 4, line = 3)

      legend("topright", legend = c("RRP", "TOTALDEMAND"), 
             col = c(colour_palette_macquarie[5], colour_palette_macquarie[1]), lty = 1, cex=0.8, bg="white")
      
      dev.off()
      message(paste("INFO: Base R plot for", current_region, "saved as", base_plot_filename, "in", here("output")))

      # ggplot2 plot
      # Reshape data to long format for ggplot
      regional_data_long <- regional_data_df %>%
        select(DateTime, RRP_Value, TOTALDEMAND_Value) %>%
        pivot_longer(cols = c(RRP_Value, TOTALDEMAND_Value), names_to = "Metric", values_to = "Value") %>%
        mutate(Metric = factor(Metric, levels = c("RRP_Value", "TOTALDEMAND_Value"), labels = c("RRP", "TOTALDEMAND")))

      # Create facet labels with units
      facet_labels <- c(
        RRP = "RRP ($/MWh)",
        TOTALDEMAND = "TOTALDEMAND (MWh)"
      )

      plot_title <- sprintf("NEM Region: %s", current_region)
      plot_subtitle <- if (!is.na(year_arg)) sprintf("Year: %d", year_arg) else sprintf("Year: N/A (Data for %s)", current_region)

      gg <- ggplot(regional_data_long, aes(x = DateTime, y = Value, color = Metric)) +
        geom_line() +
        scale_x_datetime(date_labels = "%Y-%m", date_breaks = "1 month") +
        scale_color_manual(values = c("RRP" = colour_palette_macquarie[5], "TOTALDEMAND" = colour_palette_macquarie[1])) +
        facet_wrap(~Metric, scales = "free_y", ncol = 1, labeller = labeller(Metric = facet_labels)) +
        labs(
          title = plot_title,
          subtitle = plot_subtitle,
          x = "Year-Month",
          y = NULL, # Y-axis label handled by facet strips
          color = "Metric" 
        ) +
        theme_classic() +
        theme(strip.text.y = element_text(angle = 0)) # Ensure facet labels are horizontal if they become y-strips

      ggplot_file_name <- paste0("NEM_ggplot_", current_region, ".png")
      ggplot_file_path <- here("output",ggplot_file_name)
      tryCatch({
        ggsave(filename = ggplot_file_path, plot = gg, width = 10, height = 6, units = "in", dpi = 100)
        message(paste("INFO: ggplot for", current_region, "saved as", ggplot_file_name, "in", here()))
      }, error = function(e) {
        message(paste("ERROR: Failed to save ggplot for", current_region, ":", e$message))
        # Fallback to saving a placeholder if ggsave fails
        png(ggplot_file_path, width = 800, height = 600)
        plot.new()
        text(0.5, 0.5, paste("ggplot saving failed for", current_region, ":\\n", e$message), col = "red")
        dev.off()
      })

    } else {
      message(paste("WARNING: Variable data for", current_region, "has non-finite range after removing NAs. Cannot generate plots."))
      placeholder_plot_filename <- paste0("NEM_plot_", current_region, "_placeholder.png")
      png(here("output", placeholder_plot_filename), width = 800, height = 600) # Ensure placeholder is in output
      plot.new()
      text(0.5, 0.5, paste("Plotting failed for", current_region, ": Non-finite Y-axis range."), col = "red")
      dev.off()
      message(paste("INFO: Placeholder plot (non-finite Y-range) for", current_region, "saved as", placeholder_plot_filename, "in", here()))
    }
  } else {
    message(paste("WARNING: No valid data available in regional_data_df for", current_region, "to plot. Placeholder plot will be saved."))
    placeholder_plot_filename <- paste0("NEM_plot_", current_region, "_placeholder.png")
    png(here("output", placeholder_plot_filename), width = 800, height = 600) # Ensure placeholder is in output
    plot.new() # Create a new plot frame
    text(0.5, 0.5, paste("No valid data available for plotting for region:", current_region), col = "red")
    dev.off()
    message(paste("INFO: Placeholder plot (no valid data) for", current_region, "saved as", placeholder_plot_filename, "in", here()))
  }
} # End of loop for all_regions