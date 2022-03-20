
# Objective: Summary statistics 
library(Hmisc)
library(fitdistrplus)
library(stats4)
library(MASS)
# for other necessary test or graphical tools
library(survival)
library(actuar)
library(distrMod)
library(psych)

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

logPrice <- log(df3[, "RRP"])
boxplot(logPrice)

hist(df3[,"RRP"], col="blue", main = "Spot price histogram (NSW - 5 minute settlement)", xlab = "$/MWh")

plotdist(df3$RRP, histo=TRUE, demp = TRUE)

descdist(df3$RRP, boot=1000)

fitdist(df3$RRP, "gamma")

qqnorm(df3$RRP)

### Exploring the outliers

# Iner-quartile range
Q <- quantile(df3$RRP, probs = c(0.25, 0.75), na.rm = FALSE)
iqr <- IQR(df3$RRP)

up <- Q[2]+1.5*iqr  # Upper range
low <- Q[1]-1.5*iqr #Lower range
eliminated <- subset(df3$RRP, df3$RRP > ((Q[1] - 1.5*iqr) & (df3$RRP < Q[2] + 1.5*iqr)))

outliers <- boxplot(df3$RRP, plot=FALSE)$out
x <- df3$RRP
x <- x[-which(df3$RRP %in% outliers)]

summary(outliers)
descdist(outliers)
describe(outliers)

## Use percentiles to examine the outliers
lower_bound <- quantile(df3$RRP, 0.025)
upper_bound <- quantile(df3$RRP, 0.975)
outlier_ind <- which(df3$RRP < lower_bound | df3$RRP > upper_bound)

describe(outlier_ind)

hist(outlier_ind)

boxplot(x)
fitdist(x, "lnorm")
hist(x, col="blue", main = "Spot price histogram (NSW - 5 minute settlement) outliers removed", xlab = "$/MWh")
descdist(x)
norm.f <- fitdist(x, "norm") # fit the normal distribution
lnorm.f <- fitdist(x, "lnorm") # fit the log-normal distribution

par(mfrow = c(2, 2))
plot.legend <- c("Normal","Log-normal")
denscomp(list(norm.f,lnorm.f), legendtext = plot.legend, xlab="$/MWh")
qqcomp(list(norm.f,lnorm.f), legendtext = plot.legend)

