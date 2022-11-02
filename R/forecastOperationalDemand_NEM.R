

# Objective: create time series forecast model of NEM operational demand
# Author: Grant Coble-Neal
# References:
#            Reto Marek (2021). Energy Data Analysis with R; https://hslu-ige-laes.github.io/edar/

# Forecast inputs: 
#                (1) Typical seasonal profile (i.e. an intraday shape)
#                (2) Temperature (NEM_weather.5mins)
#                (3) Solar irradiance

# Source data sets:
#                 (1) NEM_regions_2021.rds (train), NEM_regions_2022 (test)
#                 (2) NEM_weather.5mins
#                 (3) Need solar irradiance

