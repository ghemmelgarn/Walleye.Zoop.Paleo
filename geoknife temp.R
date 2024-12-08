#temperature data for degree days with geoknife
#help and instructions at https://doi-usgs.github.io/geoknife/

#THIS DOESN'T WORK BECAUSE THE DATA SERVER WAS RETIRED IN APRIL 2024 :(

library(reshape2)
library(dplyr)
library(readr)
library(devtools)

#can install geoknife from CRAN or from github
# I'd install the new geoknife - it will give you a progress bar for the job when you are using `wait` = TRUE
devtools::install_github('usgs-r/geoknife')
library(geoknife)


#gconfig(wps.url='https://cida-test.er.usgs.gov/gdp/process/WebProcessingService')
#sites <- read.csv('data/NHD_summ/nhd_centroids.csv', stringsAsFactors = FALSE)
#read in your lat/longs here, this is just an example.


sites=data.frame("id"="test", "lat"=44.95795655975761, "lon"=-93.22345822899838)

#create a dataframe called geom where each site is a column and there is are two rows: lat and long
for (i in 1:length(sites$id)){
  df <- data.frame(c(sites$lon[i], sites$lat[i]))
  names(df) <- sites$id[i]
  if (i == 1)
    geom <- df
  else 
    geom <- cbind(geom, df)
}

#loop through all years
years=seq(2020, 2021, 1)
all.temps=data.frame("DateTime"=NA, "DOW"=NA, "value"=NA)

for(i in 1:length(years))
  
{
  data.name=paste('http://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/NARR/Dailies/monolevel/air.2m.',years[i],'.nc', sep="")
  # This will give you all time for year of interest, because leaving times out is times = c(NA, NA), which means "give me all times". 
  fabric <- webdata(url = data.name, variables = 'air')
  print(years[i])
  
  job <- geoknife(geom, fabric, wait = TRUE)
  
  airtemp_data <- result(job, with.units = TRUE)
  airtemp_data <- airtemp_data %>% mutate_if(is.numeric, funs(. -273.15))
  airlong=melt(select(airtemp_data, -variable, -statistic, -units), id.vars = "DateTime", variable.name = "DOW")
  all.temps=rbind(all.temps, airlong)
}



#remove NA row
all.temps=all.temps[2:nrow(all.temps),]
all.temps$DateTime=as.POSIXct(all.temps$DateTime, tz="UTC", origin = "1970-01-01")

#rename value as mean.daily.temp
all.temps=rename(all.temps, mean.daily.temp=value)


#write file


write.csv(all.temps,"test_air_temps.csv", row.names=F )