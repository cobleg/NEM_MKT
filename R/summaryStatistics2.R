

# Objective: calculate summary statistics by region over a 12-month period
# Author: Grant Coble-Neal

library(dplyr)

SummaryStatistics <- df.21_22 %>% 
  group_by(REGION, year) %>% 
  filter(year == 2021) %>% 
  summarise(
    PriceMean = mean(AveragePrice),
    PriceVol = sd(AveragePrice)
  )

sd <- as.numeric(SummaryStatistics[SummaryStatistics$REGION == "QLD1", "PriceVol"])

rand_norms_10000 <- rnorm(n = 10000, mean = 60, sd = sd) # normally distributed prices

hist(rand_norms_10000, xlab = "Random value ($/MWh)", col = "grey",
     main = "Simulated spot price volatility ", cex.lab = 1.5, cex.axis = 1.5)

actualSpotPrices <- df.21_22 %>% 
  group_by(REGION, year) %>% 
  filter(year == 2021, REGION == "QLD1") %>% 
  select( AveragePrice )

hist(actualSpotPrices$AveragePrice, xlab = "$/MWh", col = "grey",
     main = "Actual Spot Price for QLD 2021", cex.lab = 1.5, cex.axis = 1.5, breaks = "Scott")

library(ggplot2)
ggplot(actualSpotPrices, aes(AveragePrice)) + geom_histogram() + 
  facet_wrap(~(AveragePrice > 300), scale = 'free') +
  ggtitle('QLD Spot Price 2021 ($/MWh)') + 
  xlab('Price ($/MWh)')
