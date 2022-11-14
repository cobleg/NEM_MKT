
# Objective: get data via an API
# Author: Grant Coble-Neal
# References:
#           (1) https://www.dataquest.io/blog/r-api-tutorial/

library(httr)
library(jsonlite)

res = GET("https://api.opennem.org.au/stats/power/station/NEM/ANGAST1")
