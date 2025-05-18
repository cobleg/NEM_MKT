# Objective: get and compile NEM market data for specified regions and year from command line.
# Author: Grant Coble-Neal
#
# Example Usage in PowerShell:
# Rscript getNEMdata.R 2025 QLD VIC SA TAS
# Rscript getNEMdata.R 2022 SA

library(stringr)

# Get command line arguments
args <- commandArgs(trailingOnly = TRUE)

# Check if enough arguments are provided
if (length(args) < 2) {
  stop("Usage: Rscript getNEMdata.R <year> <region1> [region2 ... regionN]\nExample: Rscript getNEMdata.R 2023 NSW QLD VIC", call. = FALSE)
}

year <- as.integer(args[1])
selected_regions <- args[-1]

# Validate year input
if (is.na(year)) {
  stop("Invalid year provided. Year must be an integer.", call. = FALSE)
}

print(paste("Selected year:", year))
print(paste("Selected regions:", paste(selected_regions, collapse=", ")))

prefix <- c("PRICE_AND_DEMAND_")
filePath <- c("https://aemo.com.au/aemo/data/nem/priceanddemand/")

subdirectory <- c("data")
wd <- getwd()

# Loop through each selected region from command line arguments
for (current_region in selected_regions) {
  print(paste("Processing region:", current_region))
  
  filePath2 <- file.path(wd, subdirectory, paste0("NEM_", current_region, "_", year, ".rds"))

  # Check if the RDS file already exists for the current region
  if (file.exists(filePath2)) {
    print(paste("File already exists for", current_region, year, ":", filePath2))
    print("Skipping download and processing.")
  } else {
    print(paste("File for", current_region, year, "not found. Proceeding with download and creation:", filePath2))

    # Ensure the output directory exists
    output_dir <- dirname(filePath2)
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
      if (dir.exists(output_dir)) {
        print(paste("Created directory:", output_dir))
      } else {
        # If directory creation fails, stop the script as saving will fail.
        stop(paste("Failed to create directory:", output_dir, "- cannot proceed."))
      }
    }

    datalist = list()
    successful_downloads = 0
    
    # Determine months to download based on the year
    current_sys_year <- as.integer(format(Sys.Date(), "%Y"))
    current_sys_month <- as.integer(format(Sys.Date(), "%m"))
    
    if (year == current_sys_year) {
      # If it's the current year, download months up to the previous month
      months_to_try = 1:(current_sys_month - 1)
      if (current_sys_month == 1) { # Edge case: If it's January, download no months for current year
          months_to_try = integer(0)
      }
      print(paste("Current year detected. Attempting to download months:", paste(months_to_try, collapse=", ")))
    } else if (year > current_sys_year) {
      # If it's a future year, download no months
      months_to_try = integer(0)
      print(paste("Future year detected. No data will be downloaded for year:", year))
    } else {
      # For past years, download all 12 months
      months_to_try = 1:12
      print(paste("Past year detected. Attempting to download months:", paste(months_to_try, collapse=", ")))
    }

    if (length(months_to_try) > 0) {
      for (myMonth_num in months_to_try)
      {
        month_str <- str_pad(myMonth_num, 2, "left", pad="0")
        file <- paste0(prefix, year, month_str, "_", current_region, "1.csv") 
        print(paste("Attempting to download:", file, "for region", current_region))
        myURL <- paste0(filePath, file)
        
        current_df <- tryCatch({
          read.csv(myURL)
        }, error = function(e) {
          warning(paste("Failed to download or read CSV from:", myURL, "for region", current_region, "\nError:", e$message))
          return(NULL) # Return NULL if the CSV can't be read
        })
        
        if (!is.null(current_df) && nrow(current_df) > 0) {
          # Store data frame in the list, using month number as index
          datalist[[as.character(myMonth_num)]] <- current_df 
          successful_downloads <- successful_downloads + 1
        } else {
          if (!is.null(current_df) && nrow(current_df) == 0) {
            warning(paste("CSV file was empty:", myURL, "for region", current_region))
          }
          datalist[[as.character(myMonth_num)]] <- NULL 
        }
      }
    } else {
      print(paste("No months to download for year", year, "and region", current_region))
    }

    if (successful_downloads > 0) {
      # append files
      # do.call(rbind, datalist) will correctly skip NULL elements
      NEM_data = do.call(rbind, datalist)
      
      # save file
      saveRDS(NEM_data, filePath2)
      print(paste("Data for", current_region, year, "successfully processed and saved to:", filePath2))
      if (successful_downloads < length(months_to_try)) {
          print(paste("Note: For region", current_region, ", data for", length(months_to_try) - successful_downloads, "month(s) could not be retrieved or was empty."))
      }
    } else {
      print(paste("No data was successfully downloaded for any month for", year, current_region, ". RDS file not created:", filePath2))
    }
  } # End of if/else for file existence
  print(paste("Finished processing region:", current_region))
  print("-----------------------------------------------------")
} # End of loop for regions

print("All regions processed.")

