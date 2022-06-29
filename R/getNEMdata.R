
# Objective: get and compile NEM market data
# Author: Grant Coble-Neal
library(stringr)
year <- 2022

prefix <- c("PRICE_AND_DEMAND_")
region <- c("NSW")

filePath <- c("https://aemo.com.au/aemo/data/nem/priceanddemand/")

subdirectory <- c("data")
wd <- getwd()
filePath2 <- file.path(wd, subdirectory, paste0("NEM_", region, "_" ,year, ".rds"))
datalist = list()
myMonth = 1
for (myMonth in 1:6)
{
  month <- str_pad(myMonth, 2, "left", pad="0")
  file <- c(paste0(prefix, year, month, "_", region,"1.csv" ))
  print(file)
  myURL <- paste0(filePath, file)
  
  df <- read.csv(myURL)
  datalist[[myMonth]] <- df # add to list
}

# append files
NEM_data = do.call(rbind, datalist)

# save file
saveRDS(NEM_data, filePath2)

