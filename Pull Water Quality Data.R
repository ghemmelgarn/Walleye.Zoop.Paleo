#this script downloads water quality data from USGS Water Quality Portal
#based on Denver's water quality pull for his zebra mussel project

library(dataRetrieval)
library(tidyverse)


#parameters to get: secchi, water temp C (NWIS parm code 00010), air temp C (NWIS parm code 00020)
#https://www.waterqualitydata.us/public_srsnames/ - this is the link to the list of NWIS parameters

#check data available for one station in one of my lakes
Long <- readWQPdata(statecode = "MN", siteid = "MNPCA-03-0383-00-202")
unique(Long$CharacteristicName)
#looks like temp for MPCA reported as "Temperature, water"


# #ONLY RUN THIS TO UPDATE DATA AND IF YOU HAVE 2-3 HOURS TO WAIT FOR IT
# #the many things secchi data may be called plus two temp thingies
# parameter.names <- c("Depth, Secchi disk depth",
#                   "Depth, Secchi disk depth (choice list)",
#                   "Secchi Reading Condition (choice list)",
#                   "Water transparency, Secchi disc",
#                   "Temperature, water",
#                   "Temperature, air")
# #you want all of the parameters with the above names for the state of Minnesota
# args <- list(statecode = "MN",
#              characteristicName = parameter.names)
# #Pulls those data from the WQ database
# WQparms <- readWQPdata(args)
# #save the output as csv so I don't have to wait 8 million hours to pull it from the portal except when I want to update the data
# write_csv(WQparms, "Data/Output/WQ_Secchi_Temp_12-6-24.csv")
# #see how many observations of each type I have
# count(WQparms$CharacteristicName)
# #it's mostly temp data...
# #this file is too big and my computer is struggling
# #I can't get degree days from this temperature data anyway, so filtering out only the rows with secchi data
# WQsecchi <- filter(WQparms, CharacteristicName == "Depth, Secchi disk depth" | CharacteristicName ==  "Depth, Secchi disk depth (choice list)" | CharacteristicName == "Secchi Reading Condition (choice list)" | CharacteristicName ==  "Water transparency, Secchi disc")
# #save secchi data as a .csv
# write_csv(WQsecchi, "Data/Output/WQ_Secchi_12-6-24.csv")

#import WQsecchi file from saved .csv
WQSecchi <- read.csv("Data/Input/WQ_Secchi_12-6-24.csv")

#look at structure of data
str(WQsecchi)

#look at the organization that collected the data
unique(WQsecchi$OrganizationFormalName)
MPCA.secchi <- filter(WQsecchi, OrganizationFormalName == "Minnesota Pollution Control Agency - Ambient Surface Water")
#MCPA contributed the majority of this data, this is what I will use

#create parentdow column from monitoring station IDs, also removes hyphen to match data format I have for other datasets
MPCA.secchi <- MPCA.secchi %>%
  mutate(parentdow = substr(MonitoringLocationIdentifier, 7, 13)) %>%
  mutate(parentdow = gsub("-", "", parentdow))


#set secchi as numberic, sometimes reported as a text description, these just become NA, then filter out these NA values
MPCA.secchi <- MPCA.secchi %>%
  mutate(ResultMeasureValue = as.numeric(ResultMeasureValue)) %>%
  filter(!is.na(ResultMeasureValue))

#check for consistent method
unique(MPCA.secchi$ResultAnalyticalMethod.MethodName)
#we have the regular method and a transparency tube, filter out only regular method
MPCA.secchi <- filter(MPCA.secchi, ResultAnalyticalMethod.MethodName == "Field measurement/observation, generic method")

#check secchi units and make sure all consistent
unique(MPCA.secchi$ResultMeasure.MeasureUnitCode)
#we have meters and feet. Need to convert feet to meters
MPCA.secchi <- MPCA.secchi %>%
  mutate(secchi_meters = case_when(ResultMeasure.MeasureUnitCode == "ft"~ ResultMeasureValue*0.3048, ResultMeasure.MeasureUnitCode == "m"~ ResultMeasureValue))
  

#need to think about temporal aspect of secchi - lets take mean of June/July/Aug but need to make sure all three months have data for all the lakes
#combine all the inidivual station data for each lake when I take this mean
