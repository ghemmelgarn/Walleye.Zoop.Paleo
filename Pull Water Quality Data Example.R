#this script downloads water quality data from USGS Water Quality Portal
#based on Denver's water quality pull for his zebra mussel project

library(dataRetrieval)
library(tidyverse)


#https://www.waterqualitydata.us/public_srsnames/ - this is the link to the list of NWIS parameters (characteristicNames)

#check data available for one station in one lake
Long <- readWQPdata(statecode = "MN", siteid = "MNPCA-03-0383-00-202", service = "Result")
unique(Long$CharacteristicName)
#This is a good way to quickly test if you can connect with the portal



#ONLY RUN THIS TO UPDATE MPCA DATA IF YOU HAVE 2-3 HOURS TO WAIT FOR IT
#the many things secchi data may be called - add all your desired characteristicNames here (see link above)
parameter.names <- c("Depth, Secchi disk depth",
                  "Depth, Secchi disk depth (choice list)",
                  "Secchi Reading Condition (choice list)",
                  "Water transparency, Secchi disc"
                  )
#I want all of the parameters with the above names for the state of Minnesota
args1 <- list(statecode = "MN",
             characteristicName = parameter.names,
             startDateLo = "1998-01-01", #date range makes this smaller so the server doesn't time out and give me "HTTP 500 Internal Server Error"
             startDateHi = "2010-12-31") #need to do two smaller date ranges and then rowbind
args2 <- list(statecode = "MN",
             characteristicName = parameter.names,
             startDateLo = "2011-01-01",
             startDateHi = "2025-11-25") #goes up to the day I am running this
#Pulls those data from the WQ database - THIS IS THE STEP THAT TAKES TIME
WQparms1 <- readWQPdata(args1, service = "Result",  dataProfile = "resultPhysChem")
WQparms2 <- readWQPdata(args2, service = "Result",  dataProfile = "resultPhysChem")

#Rowbind
WQparms_all <- rbind(WQparms1, WQparms2)

#save the output as csv so I don't have to wait 8 million hours to pull it from the portal except when I want to update the data
write_csv(WQparms_all, "Data/Output/WQP_1998-2025_Secchi_20251124.csv") #UPDATE THIS DATE WHEN YOU SAVE IT


#create DOW column from MPCA monitoring station IDs, also removes hyphen to match data format I have for other datasets
WQP.DOW <- WQparms_all %>%
  mutate(DOW = substr(MonitoringLocationIdentifier, 7, 16)) %>%
  mutate(DOW = gsub("-", "", parentdow))
#NOTE THAT DATA THAT IS NOT FROM MPCA DOES NOT ALWAYS HAVE STATION IDS THAT WORK WITH THIS CODE TO GET DOWS
  

#Units may not be consistent - check this and correct as needed
#Can investigate which organization reported the data and filter by that if desired


