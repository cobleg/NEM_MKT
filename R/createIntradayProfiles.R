# Objective: Create intraday profiles of total demand and RRP for a specified year and NEM region(s).
# This script takes command-line arguments:
#   1. Year (e.g., 2022) - Mandatory
#   2. NEM Region (e.g., NSW1, QLD1) or "ALL" - Optional (defaults to "ALL")

library(here)
message("SUCCESS: library(here) loaded") # DIAGNOSTIC
library(tidyverse)
message("SUCCESS: library(tidyverse) loaded") # DIAGNOSTIC
library(gt)
message("SUCCESS: library(gt) loaded. All essential libraries loaded.") # DIAGNOSTIC

# attached the NEM_data_all_regions data
rds_file_path <- here("data", "NEM_regions_2025.rds")
NEM_data_all_regions <- readRDS(rds_file_path)
message(paste("SUCCESS: NEM_data_all_regions loaded from", rds_file_path)) # DIAGNOSTIC

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

  # Parse SETTLEMENTDATE to POSIXct. lubridate::ymd_hms defaults to UTC if no timezone in string.
  # Assign original RRP and TOTALDEMAND to RRP_Value and TOTALDEMAND_Value respectively.
  df.0 <- df_filtered %>%
    mutate(
      DateTime = lubridate::ymd_hms(SETTLEMENTDATE, quiet = TRUE), # MODIFIED: Added quiet = TRUE
      RRP_Value = RRP,
      TOTALDEMAND_Value = TOTALDEMAND
    ) %>%
    # Filter out rows where SETTLEMENTDATE was unparseable (resulting in NA DateTime)
    filter(!is.na(DateTime)) %>%
    select(DateTime, RRP_Value, TOTALDEMAND_Value) 

  # Check if data remains after NA DateTime filter
  if (nrow(df.0) == 0) {
     message(paste("WARNING: No valid DateTime entries after parsing for Region_input:", Region_input, ". Returning empty tibble."))
     return(tibble(DateTime = as.POSIXct(character()), RRP_Value = numeric(), TOTALDEMAND_Value = numeric()))
  }
  
  message("INFO: regional.data function executed successfully (raw data prepared).") # MODIFIED message
  return(df.0)
}

# --- Command Line Argument Parsing --- START
args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  stop("Usage: Rscript createIntradayProfiles.R <Year> [NEM_Region_Code_or_ALL]", call. = FALSE)
}
if (length(args) > 2) {
  stop("Usage: Rscript createIntradayProfiles.R <Year> [NEM_Region_Code_or_ALL]. Too many arguments.", call. = FALSE)
}

year_arg <- NA
if (length(args) >= 1) {
  year_input_str <- args[1]
  # Check if the input string is purely numeric and can be an integer
  if (grepl("^[0-9]+$", year_input_str) && !is.na(suppressWarnings(as.integer(year_input_str)))) {
    year_arg <- as.integer(year_input_str)
  } else {
    stop(paste0("Error: Year argument '", year_input_str, "' must be a valid integer (e.g., 2022)."), call. = FALSE)
  }
}

if (is.na(year_arg)) { # Should be caught by the above, but as a safeguard
  stop("Error: Year argument must be provided as a valid integer.", call. = FALSE)
}

regions_to_process <- c()
if (length(args) == 2 && toupper(args[2]) != "ALL") {
  region_arg <- toupper(args[2])
  if (!(region_arg %in% all_regions)) {
    stop(paste("Error: Specified region '", region_arg, "' not found in the data. Available regions: ", paste(all_regions, collapse = ", ")), call. = FALSE)
  }
  regions_to_process <- c(region_arg)
  message(paste("INFO: Script initiated for Year:", year_arg, "and Region:", region_arg))
} else {
  regions_to_process <- all_regions
  if (length(args) == 1) {
     message(paste("INFO: Script initiated for Year:", year_arg, "for ALL regions (region argument not provided)."))
  } else { # Implies args[2] was "ALL"
     message(paste("INFO: Script initiated for Year:", year_arg, "for ALL regions (region argument was 'ALL')."))
  }
}
# --- Command Line Argument Parsing --- END

# --- Load lubridate for date/time functions ---
if (!requireNamespace("lubridate", quietly = TRUE)) {
  message("ERROR: lubridate package not found. Please install it by running: install.packages(\"lubridate\") in your R console.")
  stop("lubridate package is required but not installed.", call. = FALSE)
}
library(lubridate)
message("SUCCESS: library(lubridate) loaded")
# --- End lubridate loading ---

# --- Main execution block to generate intraday profiles ---
if (exists("regions_to_process") && length(regions_to_process) > 0) {
  
  all_hourly_profiles_list <- list() 

  for (current_region in regions_to_process) {
    message(paste0("\\n--- Processing Intraday Profiles for Region: ", current_region, ", Year: ", year_arg, " ---"))
    
    region_raw_data <- regional.data(df = NEM_data_all_regions, Region_input = current_region)

    if (nrow(region_raw_data) == 0) {
      message(paste("INFO: No raw data for region", current_region, "from regional.data. Skipping."))
      next
    }

    region_yearly_data <- region_raw_data %>%
      mutate(Year = year(DateTime), Hour = hour(DateTime)) %>%
      filter(Year == year_arg)

    if (nrow(region_yearly_data) == 0) {
      message(paste("INFO: No data for region", current_region, "in year", year_arg, ". Skipping."))
      next
    }

    hourly_summary <- region_yearly_data %>%
      filter(!is.na(RRP_Value) & !is.na(TOTALDEMAND_Value)) %>%
      group_by(Hour) %>%
      summarise(
        RRP_Q25 = quantile(RRP_Value, probs = 0.25, na.rm = TRUE),
        RRP_Q50 = quantile(RRP_Value, probs = 0.50, na.rm = TRUE),
        RRP_Q75 = quantile(RRP_Value, probs = 0.75, na.rm = TRUE),
        TOTALDEMAND_Q25 = quantile(TOTALDEMAND_Value, probs = 0.25, na.rm = TRUE),
        TOTALDEMAND_Q50 = quantile(TOTALDEMAND_Value, probs = 0.50, na.rm = TRUE),
        TOTALDEMAND_Q75 = quantile(TOTALDEMAND_Value, probs = 0.75, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      arrange(Hour)

    if (nrow(hourly_summary) == 0 || all(is.na(hourly_summary$RRP_Q50) & is.na(hourly_summary$TOTALDEMAND_Q50))) {
      message(paste("INFO: Could not generate valid hourly summary for", current_region, "in year", year_arg, "(empty or all NA values). Skipping table generation."))
      next
    }
    
    all_hourly_profiles_list[[paste0(current_region, "_", year_arg)]] <- hourly_summary
    message(paste("SUCCESS: Generated hourly profile for", current_region, "for year", year_arg))

    profile_gt_table <- hourly_summary %>%
      gt() %>% 
      tab_header(
        title = md(paste0("**Intraday Profile for ", current_region, "**")),
        subtitle = md(paste0("*Hourly Quantiles (25th, 50th, 75th) for Year ", year_arg, "*"))
      ) %>%
      cols_label(
        Hour = md("**Hour**"),
        RRP_Q25 = "Q25", RRP_Q50 = "Q50", RRP_Q75 = "Q75",
        TOTALDEMAND_Q25 = "Q25", TOTALDEMAND_Q50 = "Q50", TOTALDEMAND_Q75 = "Q75"
      ) %>%
      fmt_number(
        columns = where(is.numeric),
        decimals = 2
      ) %>%
      tab_spanner(
        label = md("**RRP ($/MWh)**"),
        columns = c(RRP_Q25, RRP_Q50, RRP_Q75) 
      ) %>%
      tab_spanner(
        label = md("**Total Demand (MW)**"),
        columns = c(TOTALDEMAND_Q25, TOTALDEMAND_Q50, TOTALDEMAND_Q75)
      ) %>%
      cols_align(
        align = "center",
        columns = where(is.numeric)
      ) %>%
      cols_width(
        vars(Hour) ~ px(80),
        everything() ~ px(100)
      ) %>%
      opt_table_font(font = list(google_font("Roboto"), default_fonts())) %>%
      tab_options(
        table.border.top.style = "solid", table.border.top.width = px(2), table.border.top.color = "black",
        table.border.bottom.style = "solid", table.border.bottom.width = px(2), table.border.bottom.color = "black",
        heading.border.bottom.style = "solid", heading.border.bottom.width = px(2), heading.border.bottom.color = "black",
        column_labels.border.bottom.style = "solid", column_labels.border.bottom.width = px(1), column_labels.border.bottom.color = "grey",
        stub.border.style = "solid", stub.border.width = px(1), stub.border.color = "grey",
        table_body.hlines.color = "lightgrey",
        data_row.padding = px(5)
      )

    profile_table_filename <- here("output", paste0("intraday_profile_", current_region, "_", year_arg, ".png"))
    gtsave(profile_gt_table, filename = profile_table_filename, vwidth = 900, vheight = 700, zoom = 1.5)
    message(paste0("Intraday profile table for ", current_region, " (", year_arg, ") saved to: ", profile_table_filename))

    # --- GGPLOT GENERATION --- START
    # Reshape data for ggplot
    rrp_profile_long <- hourly_summary %>%
      select(Hour, RRP_Q25, RRP_Q50, RRP_Q75) %>%
      pivot_longer(cols = starts_with("RRP_"),
                   names_to = "Quantile_Type",
                   names_prefix = "RRP_",
                   values_to = "RRP_Value") %>%
      mutate(Quantile_Type = factor(Quantile_Type, levels = c("Q25", "Q50", "Q75"), labels = c("25th Pctl", "Median (50th)", "75th Pctl")))

    demand_profile_long <- hourly_summary %>%
      select(Hour, TOTALDEMAND_Q25, TOTALDEMAND_Q50, TOTALDEMAND_Q75) %>%
      pivot_longer(cols = starts_with("TOTALDEMAND_"),
                   names_to = "Quantile_Type",
                   names_prefix = "TOTALDEMAND_",
                   values_to = "TOTALDEMAND_Value") %>%
      mutate(Quantile_Type = factor(Quantile_Type, levels = c("Q25", "Q50", "Q75"), labels = c("25th Pctl", "Median (50th)", "75th Pctl")))

    # Define color mapping for quantiles from the provided palette
    # Palette: "#000000", "#FFFFFF","#D6D2C4", "#E6E4DC", "#A6192E", "#C6007E", "#80225F"
    quantile_colors <- c("25th Pctl" = "#A6192E", "Median (50th)" = "#000000", "75th Pctl" = "#80225F")

    # Create and save RRP plot
    if(nrow(rrp_profile_long) > 0 && sum(!is.na(rrp_profile_long$RRP_Value)) > 0) {
      plot_rrp <- ggplot(rrp_profile_long, aes(x = Hour, y = RRP_Value, color = Quantile_Type, group = Quantile_Type)) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        scale_color_manual(values = quantile_colors, name = "Quantile") +
        scale_x_continuous(breaks = seq(0, 23, by = 2), limits = c(0, 23)) +
        labs(
          title = paste("Intraday RRP Profile for", current_region),
          subtitle = paste("Year:", year_arg, "- Hourly 25th, 50th (Median), 75th Percentiles"),
          x = "Hour of Day",
          y = "RRP ($/MWh)"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          plot.subtitle = element_text(hjust = 0.5, size = 12),
          legend.position = "top",
          panel.grid.minor.x = element_blank() # Remove minor vertical grid lines
        )
      
      rrp_plot_filename <- here("output", paste0("intraday_profile_RRP_", current_region, "_", year_arg, ".png"))
      ggsave(filename = rrp_plot_filename, plot = plot_rrp, width = 11, height = 7, dpi = 300)
      message(paste0("RRP intraday profile plot saved to: ", rrp_plot_filename))
    } else {
      message(paste0("INFO: No valid RRP data to plot for ", current_region, " in year ", year_arg))
    }

    # Create and save TOTALDEMAND plot
    if(nrow(demand_profile_long) > 0 && sum(!is.na(demand_profile_long$TOTALDEMAND_Value)) > 0) {
      plot_demand <- ggplot(demand_profile_long, aes(x = Hour, y = TOTALDEMAND_Value, color = Quantile_Type, group = Quantile_Type)) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        scale_color_manual(values = quantile_colors, name = "Quantile") +
        scale_x_continuous(breaks = seq(0, 23, by = 2), limits = c(0,23)) +
        labs(
          title = paste("Intraday Total Demand Profile for", current_region),
          subtitle = paste("Year:", year_arg, "- Hourly 25th, 50th (Median), 75th Percentiles"),
          x = "Hour of Day",
          y = "Total Demand (MW)"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          plot.subtitle = element_text(hjust = 0.5, size = 12),
          legend.position = "top",
          panel.grid.minor.x = element_blank() # Remove minor vertical grid lines
        )

      demand_plot_filename <- here("output", paste0("intraday_profile_TOTALDEMAND_", current_region, "_", year_arg, ".png"))
      ggsave(filename = demand_plot_filename, plot = plot_demand, width = 11, height = 7, dpi = 300)
      message(paste0("Total Demand intraday profile plot saved to: ", demand_plot_filename))
    } else {
      message(paste0("INFO: No valid TOTALDEMAND data to plot for ", current_region, " in year ", year_arg))
    }
    # --- GGPLOT GENERATION --- END

  }

  if (length(all_hourly_profiles_list) == 0) {
    message("INFO: No hourly profiles were generated for any valid region/year combination.")
  }

} else {
  message("No regions to process based on arguments. Exiting.")
}
# --- End main execution block ---

