

# Objective: analyse the NEM data to identify patterns and causal influences on price and quantity
# Author: Grant Coble-Neal

wd <- getwd()
subdirectory <- c("data")
file <- c("NEM_NSW_Analytical.rds")
myPath <- file.path(wd, subdirectory, file)
df <- readRDS(myPath)

library(tidyverse)

df %>% group_by(season) %>%
  summarise(mean_price = mean(Price, na.rm=TRUE),
            sd_price = sd(Price, na.rm=TRUE),
            iqr_price = IQR(Price, na.rm=TRUE),
            mad_price = mad(Price, na.rm=TRUE))

df %>% group_by(season) %>%
  summarise(Q_025 = quantile(Price, probs = 0.025, na.rm=TRUE),
            Q_10 = quantile(Price, probs = 0.1, na.rm=TRUE),
            Q_50 = quantile(Price, probs = 0.5, na.rm=TRUE),
            Q_90 = quantile(Price, probs = 0.9, na.rm=TRUE),
            Q_975 = quantile(Price, probs = 0.975, na.rm=TRUE))

## Use percentiles to examine the outliers
lower_bound <- quantile(df$Price, 0.025, na.rm=TRUE)
upper_bound <- quantile(df$Price, 0.975, na.rm=TRUE)
df <- df %>% 
  mutate( Outlier = case_when( Price < lower_bound | Price > upper_bound ~ 'outlier',
                               Price >= lower_bound & Price <= upper_bound ~ 'normal'))

df.1 <- df[, c("Price", "Quantity", "air_temp", "Outlier")]

library(tangram)
df.1 %>% group_by(Outlier) %>%
  summarise(mean_price = mean(Price, na.rm=T),
            sd_price = sd(Price, na.rm=T),
            mean_quantity = mean(Price, na.rm = T),
            sd_quantity = sd(Quantity, na.rm = T),
            mean_temperature = mean(air_temp, na.rm=T)) %>%
  tangram()


library(psych)
describe(df)
describe(df[which(df$Outlier == "outlier"), c("Price", "Quantity") ]) # summary statistics of outliers recorded from 1 January 2021 to 28 February 2022

file <- c("NEM_NSW_Price_Outlier_Risk_2021_22.png")
subdirectory <- c("output")
myPath <- file.path(wd, subdirectory, file)
Cairo(file = myPath, type = 'png', units = "px", width = 862, height = 556) # Save the calendar plot

hist(df[which(df$Outlier == "outlier"), c("Price")], xlab = "$/MWh", main = "Outlier risk profile, NEM NSW 2021-22")

dev.off()

library(openair)
scatterPlot(df[which(df$Outlier == "outlier"), c("Price", "Quantity")], x = "Quantity", y = "Price", main = "Price versus quantity during price spikes")

# Examine normal market intervals
describe(df[which(df$Outlier == "normal"), c("Price", "Quantity") ]) # summary statistics of outliers recorded from 1 January 2021 to 28 February 2022

file <- c("NEM_NSW_Price_2021_22_normal_hist.png")
subdirectory <- c("output")
myPath <- file.path(wd, subdirectory, file)

Cairo(file = myPath, type = 'png', units = "px", width = 862, height = 556) # Save the calendar plot

hist(df[which(df$Outlier == "normal"), c("Price")], xlab = "$/MWh", main = "Normal risk profile, NEM NSW 2021-22")

dev.off()

x <- df[which(df$Outlier == "normal"), c("Price")]

boxplot(x, main = "NEM NSW 2021-22 spot price during normal market intervals")

library(fitdistrplus)
fitdist(x, "lnorm")
hist(x, col="blue", main = "Spot price histogram (NSW 2021-22: 30 minute average settlement interval) outliers removed", xlab = "$/MWh")
descdist(x)
gamma.f <- fitdist(x, "gamma") # fit the normal distribution
lnorm.f <- fitdist(x, "lnorm") # fit the log-normal distribution

file <- c("NEM_NSW_Price_2021_22_PDF_QQ.png")
subdirectory <- c("output")
myPath <- file.path(wd, subdirectory, file)
Cairo(file = myPath, type = 'png', units = "px", width = 862, height = 556) # Save the calendar plot

par(mfrow = c(1, 2))
plot.legend <- c("Gamma", "Lognormal")
denscomp(list(gamma.f, lnorm.f), legendtext = plot.legend, xlab="$/MWh", main = "Price PDF")
qqcomp(list(gamma.f,lnorm.f), legendtext = plot.legend)
mtext("Spot price (NSW 2021-22: 30 minute average settlement interval)", side = 1, line = -1, outer = T)

dev.off()

outliers <-df[which(df$Outlier == "outlier"), ]

# Calendar plot of outliers

file <- c("NEM_NSW_Price_2021_cal_out.png")
myPath <- file.path(wd, subdirectory, file)
Cairo(file = myPath, type = 'png', units = "px", width = 862, height = 556) # Save the calendar plot

calendarPlot(outliers, pollutant = "Price", year = 2021, main = "Price variation ($/MWh)") # month view of price variation

dev.off()

file <- c("NEM_NSW_Price_2022_cal_out.png")
myPath <- file.path(wd, subdirectory, file)
Cairo(file = myPath, type = 'png', units = "px", width = 862, height = 556) # Save the calendar plot

calendarPlot(outliers, pollutant = "Price", year = 2022, main = "Price variation ($/MWh)") # month view of price variation

dev.off()

# calendar plot of normal spot prices
normal <-df[which(df$Outlier == "normal"), ]

file <- c("NEM_NSW_Price_2021_cal_normal.png")
myPath <- file.path(wd, subdirectory, file)
Cairo(file = myPath, type = 'png', units = "px", width = 862, height = 556) # Save the calendar plot

calendarPlot(normal, pollutant = "Price", year = 2021, main = "Price variation during normal intervals ($/MWh)") # month view of price variation

dev.off()

file <- c("NEM_NSW_Price_2022_cal_normal.png")
myPath <- file.path(wd, subdirectory, file)
Cairo(file = myPath, type = 'png', units = "px", width = 862, height = 556) # Save the calendar plot

calendarPlot(normal, pollutant = "Price", year = 2022, main = "Price variation during normal intervals ($/MWh)") # month view of price variation

dev.off()

# scatter plot of price and quantity
file <- c("NEM_NSW_Price_2022_scatter_normal.png")
myPath <- file.path(wd, subdirectory, file)
Cairo(file = myPath, type = 'png', units = "px", width = 862, height = 556) # Save the calendar plot

scatterPlot(normal, x = "Quantity", y = "Price", xlab= "MWh", ylab = "$/MWh", main = "Price versus quantity during normal market intervals", method = "hexbin", cols = "jet")

dev.off()

# scatter plot of air temperature and quantity
file <- c("NEM_NSW_Price_2021_normal_scatter_temp_qty.png")
myPath <- file.path(wd, subdirectory, file)
Cairo(file = myPath, type = 'png', units = "px", width = 862, height = 556) # Save the calendar plot

scatterPlot(normal, x = "air_temp", y = "Quantity", xlab="Air temperature", ylab="MWh", main = "Air temperature versus quantity during normal market intervals", method = "hexbin", cols = "jet")

dev.off()