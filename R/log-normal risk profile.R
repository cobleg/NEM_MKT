
# Objective: calculate the log-normal distribution of percent change in price
# Author: Grant Coble-Neal

library(dplyr)

logNormalReturns <- df.21_22 %>% 
  group_by(REGION, year) %>% 
  filter(year == 2021) %>% 
  mutate(
    LogReturns = log(AveragePrice / lag(AveragePrice))
  )
  
logNormalReturns

SummaryStatistics <- logNormalReturns %>% 
  group_by(REGION, year) %>% 
  filter(year == 2021) %>% 
  summarise(
    mu = mean(LogReturns, na.rm = T),
    sigma = sd(LogReturns, na.rm = T)
  )
SummaryStatistics

hist(logNormalReturns$LogReturns, xlab = "(% change)", col = "grey",
     main = "NSW 2021 Spot Price (% change)", cex.lab = 1.5, cex.axis = 1.5)

initialPrice = 60
mu = as.numeric(SummaryStatistics[SummaryStatistics$REGION == "NSW1", "mu"])
sigma <- as.numeric(SummaryStatistics[SummaryStatistics$REGION == "NSW1", "sigma"])
hours <- 8760
Log_mean <- log(initialPrice) + (mu - sigma^2 / 2)
Log_sigma <- sigma * sqrt(1)
LogSpotPrice_simulated <- ( rnorm(n = 10000, mean = Log_mean, sd = Log_sigma) ) # normally distributed prices
SpotPrice_simulated <- exp(LogSpotPrice_simulated)
hist(SpotPrice_simulated, xlab = "Price Range", col = "grey",
     main = "NSW 2021 VaR (Simulated Spot Price)", cex.lab = 1.5, cex.axis = 1.5)

quantile(SpotPrice_simulated, 0.01)
quantile(SpotPrice_simulated, 0.05)
