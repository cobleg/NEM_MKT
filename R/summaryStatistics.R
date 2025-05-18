# ----------------------------------------------------------------------------------
# Script: summaryStatistics.R
#
# Description:
# This script performs a detailed statistical analysis of National Electricity Market (NEM)
# data, with a primary focus on the Regional Reference Price (RRP).
# Key analyses include:
#   - Loading and merging historical NEM data for NSW.
#   - Calculating and displaying summary statistics for RRP and TOTALDEMAND.
#   - Exploring the distribution of RRP:
#     - Attempting to fit a Generalized Hyperbolic Distribution (GHD), specifically
#       the Normal Inverse Gaussian (NIG) variant, to the raw RRP data (which can
#       include negative values).
#     - Filtering for positive RRP values for further analysis.
#     - Visualizing positive RRP with boxplots (raw and log-transformed) and histograms.
#     - Using `plotdist` and `descdist` (Cullen and Frey graph) to assess potential
#       distributions for positive RRP.
#     - Attempting to fit a Gamma distribution to positive RRP.
#     - Generating Q-Q plots.
#   - Identifying and analyzing outliers in the positive RRP data using IQR and
#     percentile methods.
#   - Re-analyzing and attempting to fit distributions (Normal, Log-normal) to
#     positive RRP data after removing outliers identified by the boxplot method.
#
# Inputs:
#   - Reads data from two .rds files located in the "data" subdirectory:
#     - "NEM_NSW_2021OCT_DEC.rds"
#     - "NEM_NSW_2022.rds"
#   - These datasets are expected to contain at least "RRP" and "TOTALDEMAND" columns.
#
# Console Outputs:
#   - Summary statistics for TOTALDEMAND and RRP (using `summary()`).
#   - Detailed descriptive statistics for RRP (using `psych::describe()`).
#   - Messages regarding the GHD/NIG fitting process, including success or error messages.
#   - Printed summary of the fitted NIG distribution object, if successful.
#   - Printed summary of the fitted Gamma distribution object (`fit_gamma`), if successful.
#   - Warnings or messages if certain conditions are not met for distribution fitting
#     (e.g., no positive RRP values, insufficient data after outlier removal).
#   - Summary of outliers identified by the boxplot method.
#   - Descriptive statistics of the outlier values themselves.
#   - Printed summaries of fitted Normal (`norm.f`) and Log-normal (`lnorm.f`)
#     distributions to data with outliers removed, if successful.
#
# Saved Plot Files (in the "output" directory):
#   - `logPrice_boxplot.png`: Boxplot of log-transformed positive RRP.
#   - `positive_RRP_histogram.png`: Histogram of positive RRP values.
#   - `positive_RRP_plotdist.png`: Diagnostic plot (histogram, empirical density/CDF)
#     for positive RRP.
#   - `positive_RRP_cullen_frey.png`: Cullen and Frey graph for positive RRP.
#   - `positive_RRP_qqnorm.png`: Normal Q-Q plot for positive RRP.
#   - `x_no_outliers_boxplot.png`: Boxplot of positive RRP after boxplot-defined
#     outliers are removed.
#   - `x_no_outliers_histogram.png`: Histogram of positive RRP after boxplot-defined
#     outliers are removed.
#   - `x_no_outliers_cullen_frey.png`: Cullen and Frey graph for positive RRP
#     after boxplot-defined outliers are removed.
#   - `x_no_outliers_fit_comparison_plots.png`: A 2x2 panel comparing density and
#     Q-Q plots for Normal and Log-normal distributions fitted to positive RRP
#     data after outlier removal.
# ----------------------------------------------------------------------------------

# Objective: Summary statistics and distribution fitting for NEM RRP data.
#
# This script performs the following actions and produces the specified outputs:
#
# Inputs:
# - Reads NEM data for NSW from two .rds files:
#   - data/NEM_NSW_2021OCT_DEC.rds
#   - data/NEM_NSW_2022.rds
#
# Processing Steps:
# 1. Loads necessary libraries (Hmisc, fitdistrplus, stats4, MASS, survival, actuar, distrMod, psych, ghyp).
# 2. Creates an 'output' directory if it doesn't exist.
# 3. Reads and combines the two input RDS files into a single dataframe `df3`.
# 4. Prints initial summary statistics for TOTALDEMAND and RRP using `summary()` and `psych::describe()`.
# 5. Attempts to fit a Generalized Hyperbolic Distribution (GHD), specifically the Normal Inverse Gaussian (NIG) case,
#    to the raw RRP data (after removing NAs/Infs) and prints the fit summary to the console.
# 6. Filters RRP data to include only positive values (`positive_RRP`).
# 7. Performs analysis on `positive_RRP`:
#    - Calculates and plots a boxplot of log-transformed `positive_RRP` (saved as output/logPrice_boxplot.png).
#    - Plots a histogram of `positive_RRP` (saved as output/positive_RRP_histogram.png).
#    - Generates a `plotdist` diagnostic plot (saved as output/positive_RRP_plotdist.png).
#    - Generates a Cullen and Frey graph using `descdist` (saved as output/positive_RRP_cullen_frey.png).
#    - Attempts to fit a Gamma distribution and prints the fit summary to the console.
#    - Generates a Q-Q plot against the normal distribution (saved as output/positive_RRP_qqnorm.png).
# 8. Explores outliers in `positive_RRP`:
#    - Identifies outliers using the boxplot method.
#    - Creates a dataset `x_no_outliers` by removing these outliers from `positive_RRP`.
#    - Prints summary statistics and a Cullen and Frey graph for the identified outlier values themselves.
#    - Identifies outliers using a percentile-based method (0.025 and 0.975 quantiles).
# 9. Performs analysis on `x_no_outliers` (positive RRP data with boxplot outliers removed):
#    - Plots a boxplot (saved as output/x_no_outliers_boxplot.png).
#    - Attempts to fit a log-normal distribution.
#    - Plots a histogram (saved as output/x_no_outliers_histogram.png).
#    - Generates a Cullen and Frey graph (saved as output/x_no_outliers_cullen_frey.png).
#    - Fits Normal and Log-normal distributions.
#    - Generates and saves comparison plots (density and Q-Q) for these fits
#      (saved as output/x_no_outliers_fit_comparison_plots.png).
#
# Console Output:
# - Summary statistics for RRP and TOTALDEMAND.
# - Detailed descriptive statistics for RRP.
# - Printed results of NIG distribution fit (if successful).
# - Printed results of Gamma distribution fit to positive RRP (if successful).
# - Summary of outlier values.
# - Messages indicating progress, warnings, or errors (e.g., if data is insufficient for fitting).
#
# Saved Plot Files (in the 'output' directory):
# - output/logPrice_boxplot.png
# - output/positive_RRP_histogram.png
# - output/positive_RRP_plotdist.png
# - output/positive_RRP_cullen_frey.png
# - output/positive_RRP_qqnorm.png
# - output/x_no_outliers_boxplot.png
# - output/x_no_outliers_histogram.png
# - output/x_no_outliers_cullen_frey.png
# - output/x_no_outliers_fit_comparison_plots.png

library(Hmisc)
library(fitdistrplus)
library(stats4)
library(MASS)
# for other necessary test or graphical tools
library(survival)
library(actuar)
library(distrMod)
library(psych)
library(ghyp) # Add ghyp library
library(here)
# Ensure output directory exists
dir.create(here("output"), showWarnings = FALSE, recursive = TRUE)

wd <- getwd()
subdirectory <- c("data")
file <- c("NEM_NSW_2021OCT_DEC.rds")
myPath <- file.path(wd, subdirectory, file)
df <- readRDS(myPath)

file <- c("NEM_NSW_2022.rds")
myPath <- file.path(wd, subdirectory, file)
df2 <- readRDS(myPath)

df3 <- rbind(df, df2)

summary(df3[, names(df3) %in% c("TOTALDEMAND", "RRP")])
describe(df3$RRP)

# --- Consider fitting GHD to the original RRP data (df3$RRP) ---
# --- as it can handle negative values. ---

# Remove NA/NaN/Inf values if any, as ghyp might be sensitive
rrp_for_ghd <- df3$RRP[!is.na(df3$RRP) & is.finite(df3$RRP)]

if (length(rrp_for_ghd) > 1) { # Need at least 2 data points
  message("Attempting to fit Generalized Hyperbolic Distribution (GHD) to RRP data...")
  
  # Fit the GHD. This can be computationally intensive.
  # You might need to adjust parameters or try different fitting methods if it fails or takes too long.
  # Common methods include 'fit.ghypuv' (univariate) or 'fit.NIGuv' for the Normal Inverse Gaussian special case.
  
  # Example: Fitting a NIG distribution (a special case of GHD)
  nig.fit <- tryCatch({
    fit.NIGuv(data = rrp_for_ghd, silent = TRUE)
  }, error = function(e) {
    message(paste("Error fitting NIG distribution:", e$message))
    return(NULL)
  })
  
  if (!is.null(nig.fit)) {
    print("NIG Distribution Fit:")
    print(nig.fit)
    # summary(nig.fit)
    # plot(nig.fit) # This would plot density, QQ-plot etc.
  }

  # Example: Fitting a general GHD (can be slower)
  # ghyp.fit <- tryCatch({
  #   fit.ghypuv(data = rrp_for_ghd, silent = TRUE)
  # }, error = function(e) {
  #   message(paste("Error fitting GHD distribution:", e$message))
  #   return(NULL)
  # })
  # 
  # if (!is.null(ghyp.fit)) {
  #   print("Generalized Hyperbolic Distribution Fit:")
  #   print(ghyp.fit)
  #   # summary(ghyp.fit)
  #   # plot(ghyp.fit)
  # }
  
} else {
  message("Not enough valid data points in RRP to fit GHD.")
}

# Filter RRP for positive values before log transformation and plotting
positive_RRP <- df3$RRP[df3$RRP > 0]

# Check if there are any positive RRP values before proceeding
if (length(positive_RRP) == 0) {
  stop("Error: No positive RRP values found in the data. Cannot proceed with log transformation or gamma distribution fitting.")
}

logPrice <- log(positive_RRP) # Use filtered positive_RRP
png(here("output", "logPrice_boxplot.png"))
boxplot(logPrice)
dev.off()

png(here("output", "positive_RRP_histogram.png"))
hist(positive_RRP, col="blue", main = "Spot price histogram (NSW - 5 minute settlement)", xlab = "$/MWh") # Use filtered positive_RRP
dev.off()

png(here("output", "positive_RRP_plotdist.png"))
plotdist(positive_RRP, histo=TRUE, demp = TRUE) # Use filtered positive_RRP
dev.off()

png(here("output", "positive_RRP_cullen_frey.png"))
descdist(positive_RRP, boot=1000) # Use filtered positive_RRP
dev.off()

# Attempt to fit gamma distribution only if there are positive values
if(any(positive_RRP <= 0)){
 message("Warning: RRP data contains non-positive values. Gamma distribution cannot be fitted to the original RRP data.")
 fit_gamma <- NULL # or handle as appropriate
} else {
  fit_gamma <- fitdist(positive_RRP, "gamma")
  print(fit_gamma)
}

png(here("output", "positive_RRP_qqnorm.png"))
qqnorm(positive_RRP) # Use filtered positive_RRP
dev.off()

### Exploring the outliers

# Inter-quartile range - use positive_RRP for consistency if outliers are price-based
Q <- quantile(positive_RRP, probs = c(0.25, 0.75), na.rm = TRUE) # Added na.rm = TRUE
iqr <- IQR(positive_RRP, na.rm = TRUE) # Added na.rm = TRUE

up <- Q[2]+1.5*iqr  # Upper range
low <- Q[1]-1.5*iqr #Lower range
# The following line for 'eliminated' seems to have a logical error in the original script, 
# it should likely use positive_RRP and the condition was inverted.
# For now, focusing on fixing the fitdist error. The outlier logic might need further review.
eliminated <- subset(positive_RRP, positive_RRP > low & positive_RRP < up)

outliers_from_boxplot <- boxplot(positive_RRP, plot=FALSE)$out # Use positive_RRP
x_no_outliers <- positive_RRP[!positive_RRP %in% outliers_from_boxplot] # Create a new variable for RRP without boxplot outliers

summary(outliers_from_boxplot)
descdist(outliers_from_boxplot) # This will describe the distribution of the outlier values themselves
# describe(outliers_from_boxplot) # psych::describe

## Use percentiles to examine the outliers - on positive_RRP
lower_bound <- quantile(positive_RRP, 0.025, na.rm = TRUE)
upper_bound <- quantile(positive_RRP, 0.975, na.rm = TRUE)
outlier_ind_percentile <- which(positive_RRP < lower_bound | positive_RRP > upper_bound)

# describe(outlier_ind_percentile) # This describes indices, not values

# hist(outlier_ind_percentile) # Histogram of indices is not usually what's needed

png(here("output", "x_no_outliers_boxplot.png"))
boxplot(x_no_outliers) # Plot data with boxplot outliers removed
dev.off()

if(length(x_no_outliers) > 0 && !any(x_no_outliers <=0)){
  fitdist(x_no_outliers, "lnorm")
} else {
  message("Not enough positive data after outlier removal to fit lnorm, or non-positive values still present.")
}
png(here("output", "x_no_outliers_histogram.png"))
hist(x_no_outliers, col="blue", main = "Spot price histogram (NSW - 5 min settlement) outliers removed", xlab = "$/MWh")
dev.off()

png(here("output", "x_no_outliers_cullen_frey.png"))
descdist(x_no_outliers)
dev.off()

if(length(x_no_outliers) > 1 && !any(x_no_outliers <=0)) { # Ensure there's enough data and it's positive
  norm.f <- fitdist(x_no_outliers, "norm") # fit the normal distribution
  lnorm.f <- fitdist(x_no_outliers, "lnorm") # fit the log-normal distribution

  png(here("output", "x_no_outliers_fit_comparison_plots.png"))
  par(mfrow = c(2, 2))
  plot.legend <- c("Normal","Log-normal")
  denscomp(list(norm.f,lnorm.f), legendtext = plot.legend, xlab="$/MWh")
  qqcomp(list(norm.f,lnorm.f), legendtext = plot.legend)
  dev.off()
  par(mfrow = c(1, 1)) # Reset plotting layout
} else {
  message("Skipping normal and log-normal distribution fitting due to insufficient or non-positive data after outlier removal.")
}

