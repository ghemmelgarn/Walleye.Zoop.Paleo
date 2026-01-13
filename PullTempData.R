#This script pulls daily surface temperature data max y min for my lake centroid coordinates from Daymet

#I compared this to nasa data and it is comparable
#1 km grid resolution

#as of 1/13/2025 only has data through 2024


library(tidyverse)
library(dplyr)
library(daymetr) #allows access to the data
library(purrr)

coords <- read.csv("Data/Input/Contemp_Lake_Centroid_Coords.csv")

#Write a function that pulls 1989-2025 data for one point
get_daymet_data <- function(id, latitude, longitude) {

  temp_data <- download_daymet(
    site = id,
    lat = latitude,
    lon = longitude,
    start = 1989,
    end = 2025,
    internal = TRUE
  )$data
  
  # Clean up and add the site name - other data gets downloaded but this just selects the temp data
  temp_data %>%
    mutate(
      site = id,
      date = as.Date(paste(year, yday, sep = "-"), format = "%Y-%j"),
      tmax = tmax..deg.c.,
      tmin = tmin..deg.c.
    ) %>%
    select(site, date, tmax, tmin)
}

#run a loop that gets this data for my list of coordinates
#pmap_dfr runs the function I made for each row and "binds" the results together
temp_all <- pmap_dfr(list(coords$parentdow, coords$Latitude, coords$Longitude), get_daymet_data)

temp_final <- temp_all %>% 
  rename(parentdow = site)

#save output as a csv
write.csv(temp_final, "Data/Output/Contemp_Lake_Daily_Air_Temps_1989_2024.csv", row.names = FALSE)















