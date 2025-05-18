# Objective: calculate summary statistics for NEM data

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
print(head(NEM_data_all_regions))

# Get all unique regions
all_regions <- unique(NEM_data_all_regions$REGION)
message(paste("INFO: Found regions:", paste(all_regions, collapse = ", ")))

# --- Determine global date range for subtitles --- START
if (nrow(NEM_data_all_regions) > 0 && "SETTLEMENTDATE" %in% names(NEM_data_all_regions)) {
  # Parse all settlement dates. Using quiet = TRUE to suppress individual parsing warnings.
  # Assuming UTC for consistency, though ymd_hms can infer if tz info is in the string.
  all_dates <- lubridate::ymd_hms(NEM_data_all_regions$SETTLEMENTDATE, tz = "UTC", quiet = TRUE)
  all_dates_valid <- all_dates[!is.na(all_dates)]

  if (length(all_dates_valid) > 0) {
    min_date <- min(all_dates_valid)
    max_date <- max(all_dates_valid)
    formatted_min_date <- format(min_date, "%Y-%m-%d")
    formatted_max_date <- format(max_date, "%Y-%m-%d")
    date_range_subtitle <- paste0("Data Range: ", formatted_min_date, " to ", formatted_max_date)
  } else {
    date_range_subtitle <- "Data range: Not available (no valid dates found in SETTLEMENTDATE)"
    message("WARNING: Could not determine a valid date range from SETTLEMENTDATE column.")
  }
} else {
  date_range_subtitle <- "Data range: Not available (input data or SETTLEMENTDATE column missing)"
  message("WARNING: NEM_data_all_regions is empty or SETTLEMENTDATE column is missing, cannot determine date range.")
}
# --- Determine global date range for subtitles --- END

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
      DateTime = lubridate::ymd_hms(SETTLEMENTDATE), 
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
  
  message("INFO: regional.data function executed successfully (no hourly aggregation).")
  return(df.0)
}

# Main execution block to calculate and display summary statistics
if (exists("all_regions") && length(all_regions) > 0) {
  stat_names <- c("Min", "Q01", "Q05", "Q10", "Q25", "Median", "Q75", "Q90", "Q95", "Q99", "Max", "Mean", "SD")
  rrp_summary_data <- tibble(Statistic = stat_names)
  demand_summary_data <- tibble(Statistic = stat_names)

  # --- Setup for Correlation Data ---
  # Store individual region data (DateTime, RRP_Value, TOTALDEMAND_Value)
  all_regions_processed_data_list <- list()

  for (current_region in all_regions) {
    message(paste0("\n--- Processing Statistics for Region: ", current_region, " ---"))
    processed_data <- regional.data(df = NEM_data_all_regions, Region_input = current_region)

    if (nrow(processed_data) > 0 && sum(!is.na(processed_data$RRP_Value)) > 0 && sum(!is.na(processed_data$TOTALDEMAND_Value)) > 0) {
      regional_rrp_stats_vector <- c(
        min(processed_data$RRP_Value, na.rm = TRUE),
        quantile(processed_data$RRP_Value, probs = 0.01, na.rm = TRUE),
        quantile(processed_data$RRP_Value, probs = 0.05, na.rm = TRUE),
        quantile(processed_data$RRP_Value, probs = 0.10, na.rm = TRUE),
        quantile(processed_data$RRP_Value, probs = 0.25, na.rm = TRUE),
        median(processed_data$RRP_Value, na.rm = TRUE),
        quantile(processed_data$RRP_Value, probs = 0.75, na.rm = TRUE),
        quantile(processed_data$RRP_Value, probs = 0.90, na.rm = TRUE),
        quantile(processed_data$RRP_Value, probs = 0.95, na.rm = TRUE),
        quantile(processed_data$RRP_Value, probs = 0.99, na.rm = TRUE),
        max(processed_data$RRP_Value, na.rm = TRUE),
        mean(processed_data$RRP_Value, na.rm = TRUE),
        sd(processed_data$RRP_Value, na.rm = TRUE)
      )
      rrp_summary_data <- rrp_summary_data %>% mutate(!!sym(current_region) := regional_rrp_stats_vector)

      regional_demand_stats_vector <- c(
        min(processed_data$TOTALDEMAND_Value, na.rm = TRUE),
        quantile(processed_data$TOTALDEMAND_Value, probs = 0.01, na.rm = TRUE),
        quantile(processed_data$TOTALDEMAND_Value, probs = 0.05, na.rm = TRUE),
        quantile(processed_data$TOTALDEMAND_Value, probs = 0.10, na.rm = TRUE),
        quantile(processed_data$TOTALDEMAND_Value, probs = 0.25, na.rm = TRUE),
        median(processed_data$TOTALDEMAND_Value, na.rm = TRUE),
        quantile(processed_data$TOTALDEMAND_Value, probs = 0.75, na.rm = TRUE),
        quantile(processed_data$TOTALDEMAND_Value, probs = 0.90, na.rm = TRUE),
        quantile(processed_data$TOTALDEMAND_Value, probs = 0.95, na.rm = TRUE),
        quantile(processed_data$TOTALDEMAND_Value, probs = 0.99, na.rm = TRUE),
        max(processed_data$TOTALDEMAND_Value, na.rm = TRUE),
        mean(processed_data$TOTALDEMAND_Value, na.rm = TRUE),
        sd(processed_data$TOTALDEMAND_Value, na.rm = TRUE)
      )
      demand_summary_data <- demand_summary_data %>% mutate(!!sym(current_region) := regional_demand_stats_vector)
      
      # Store processed data for correlation analysis
      all_regions_processed_data_list[[current_region]] <- processed_data # Add to list

    } else {
      message(paste0("No valid data or insufficient data returned from regional.data for ", current_region, ". Adding NA column for this region."))
      rrp_summary_data <- rrp_summary_data %>% mutate(!!sym(current_region) := rep(NA_real_, length(stat_names)))
      demand_summary_data <- demand_summary_data %>% mutate(!!sym(current_region) := rep(NA_real_, length(stat_names)))
    }
  }

  # Create and save tables if data was processed
  if (ncol(rrp_summary_data) > 1) { # Check if any region columns were added
    rrp_gt_table <- rrp_summary_data %>% 
      gt(rowname_col = "Statistic") %>% 
      tab_header(
        title = "Summary Statistics for RRP",
        subtitle = paste0("Statistics by Region, \n", date_range_subtitle) # MODIFIED
      ) %>% 
      fmt_number(
        columns = where(is.numeric), # Applies to all region columns
        decimals = 2
      ) %>%
      tab_stubhead(label = "Statistic") # Label for the stub column head
    
    rrp_table_filename <- here("output", "summary_stats_RRP_regions_as_cols.png")
    gtsave(rrp_gt_table, filename = rrp_table_filename)
    message(paste0("RRP summary table (regions as columns) saved to: ", rrp_table_filename))

    demand_gt_table <- demand_summary_data %>% 
      gt(rowname_col = "Statistic") %>% 
      tab_header(
        title = "Summary Statistics for TOTALDEMAND",
        subtitle = paste0("Statistics by Region, \n", date_range_subtitle) # MODIFIED
      ) %>% 
      fmt_number(
        columns = where(is.numeric), # Applies to all region columns
        decimals = 2
      ) %>%
      tab_stubhead(label = "Statistic") # Label for the stub column head

    demand_table_filename <- here("output", "summary_stats_TOTALDEMAND_regions_as_cols.png")
    gtsave(demand_gt_table, filename = demand_table_filename)
    message(paste0("TOTALDEMAND summary table (regions as columns) saved to: ", demand_table_filename))

    # --- Correlation Analysis --- START
    if (length(all_regions_processed_data_list) > 1) {
      # Prepare data for RRP correlation
      rrp_for_corr <- all_regions_processed_data_list %>% 
        map(~select(.x, DateTime, RRP_Value)) %>% 
        reduce(full_join, by = "DateTime")
      # Rename columns to just REGION for clarity
      colnames(rrp_for_corr) <- c("DateTime", names(all_regions_processed_data_list)) # MODIFIED
      
      # Calculate RRP correlation matrix (excluding DateTime column)
      rrp_corr_matrix <- cor(rrp_for_corr[,-1], use = "pairwise.complete.obs")
      
      # Prepare data for TOTALDEMAND correlation
      demand_for_corr <- all_regions_processed_data_list %>% 
        map(~select(.x, DateTime, TOTALDEMAND_Value)) %>% 
        reduce(full_join, by = "DateTime")
      # Rename columns to just REGION for clarity
      colnames(demand_for_corr) <- c("DateTime", names(all_regions_processed_data_list)) # MODIFIED
      
      # Calculate TOTALDEMAND correlation matrix (excluding DateTime column)
      demand_corr_matrix <- cor(demand_for_corr[,-1], use = "pairwise.complete.obs")
      
      # Convert correlation matrices to tibbles for gt
      rrp_corr_tibble <- as_tibble(rrp_corr_matrix, rownames = "Region1")
      demand_corr_tibble <- as_tibble(demand_corr_matrix, rownames = "Region1")
      
      # Create and save RRP correlation table
      rrp_corr_gt <- rrp_corr_tibble %>% 
        gt(rowname_col = "Region1") %>% 
        tab_header(
          title = "RRP Correlation Matrix Across Regions",
          subtitle = date_range_subtitle
        ) %>% 
        fmt_number(columns = where(is.numeric), decimals = 2) %>% 
        tab_stubhead(label = "Region")

      # Apply conditional highlighting for cells with correlation > 0.7
      numeric_cols_rrp <- colnames(rrp_corr_matrix)
      for (col_name in numeric_cols_rrp) {
        rrp_corr_gt <- rrp_corr_gt %>% 
          tab_style(
            style = list(cell_fill(color = "lightgreen")),
            locations = cells_body(
              columns = c(!!sym(col_name)),
              rows = !!sym(col_name) > 0.7 & !!sym(col_name) < 1 # MODIFIED
            )
          )
      }
      
      rrp_corr_filename <- here("output", "correlation_RRP_regions.png")
      gtsave(rrp_corr_gt, filename = rrp_corr_filename)
      message(paste0("RRP correlation table saved to: ", rrp_corr_filename))
      
      # Create and save TOTALDEMAND correlation table
      demand_corr_gt <- demand_corr_tibble %>% 
        gt(rowname_col = "Region1") %>% 
        tab_header(
          title = "Total Demand Correlation Matrix Across Regions",
          subtitle = date_range_subtitle
        ) %>% 
        fmt_number(columns = where(is.numeric), decimals = 2) %>% 
        tab_stubhead(label = "Region")

      # Apply conditional highlighting for cells with correlation > 0.7
      numeric_cols_demand <- colnames(demand_corr_matrix)
      for (col_name in numeric_cols_demand) {
        demand_corr_gt <- demand_corr_gt %>% 
          tab_style(
            style = list(cell_fill(color = "lightgreen")),
            locations = cells_body(
              columns = c(!!sym(col_name)),
              rows = !!sym(col_name) > 0.7 & !!sym(col_name) < 1 # MODIFIED
            )
          )
      }
        
      demand_corr_filename <- here("output", "correlation_TOTALDEMAND_regions.png")
      gtsave(demand_corr_gt, filename = demand_corr_filename)
      message(paste0("TOTALDEMAND correlation table saved to: ", demand_corr_filename))
      
    } else {
      message("Insufficient number of regions with data (<2) to calculate correlations.")
    }
    # --- Correlation Analysis --- END

  } else {
    message("No regional statistics were calculated. Summary tables cannot be generated.")
  }

} else {
  message("No regions found in NEM_data_all_regions or 'all_regions' variable not defined. Cannot calculate statistics.")
}

