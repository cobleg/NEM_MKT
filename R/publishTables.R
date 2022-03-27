

# Objective: create publication ready summary statistics tables
# Author: Grant Coble-Neal

wd <- getwd()
file <- c("NEM_NSW_solar_2021_22_A.rds")
subdirectory <- c("data")
myPath <- file.path(wd, subdirectory, file)
df <- readRDS(myPath)

library(psych)
describe(df)

library(tidyverse)

table.1 <- df %>%
  select(Price, Quantity, Outlier_H_L) %>%
  group_by(Outlier_H_L) %>%
  summarise_all(funs(mean, sd))

table.1 <- head(table.1, -1)

library(kableExtra)
kable(table.1) %>%
  kable_styling(font_size = 10)
