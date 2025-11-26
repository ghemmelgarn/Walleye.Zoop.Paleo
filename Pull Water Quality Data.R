#this script downloads water quality data from USGS Water Quality Portal and some DNR data
#based on Denver's water quality pull for his zebra mussel project

library(dataRetrieval)
library(tidyverse)


#parameters to get: secchi, water temp C (NWIS parm code 00010), air temp C (NWIS parm code 00020)
#https://www.waterqualitydata.us/public_srsnames/ - this is the link to the list of NWIS parameters

# #check data available for one station in one of my lakes
# Long <- readWQPdata(statecode = "MN", siteid = "MNPCA-03-0383-00-202", service = "Result")
# unique(Long$CharacteristicName)
# #looks like temp for MPCA reported as "Temperature, water"
# 
# 
# #ONLY RUN THIS TO UPDATE MPCA DATA AND IF YOU HAVE 2-3 HOURS TO WAIT FOR IT
# #the many things secchi data may be called plus two temp thingies
# parameter.names <- c("Depth, Secchi disk depth",
#                   "Depth, Secchi disk depth (choice list)",
#                   "Secchi Reading Condition (choice list)",
#                   "Water transparency, Secchi disc"
#                   )
# #you want all of the parameters with the above names for the state of Minnesota
# args1 <- list(statecode = "MN",
#              characteristicName = parameter.names,
#              startDateLo = "1998-01-01", #date range makes this smaller so the server doesn't time out and give me "HTTP 500 Internal Server Error"
#              startDateHi = "2010-12-31") #need to do two smaller date ranges and then rowbind
# args2 <- list(statecode = "MN",
#              characteristicName = parameter.names,
#              startDateLo = "2011-01-01", 
#              startDateHi = "2025-11-25") #goes up to the day I am running this
# #Pulls those data from the WQ database
# WQparms1 <- readWQPdata(args1, service = "Result",  dataProfile = "resultPhysChem")
# WQparms2 <- readWQPdata(args2, service = "Result",  dataProfile = "resultPhysChem")
# 
# WQparms_all <- rbind(WQparms1, WQparms2)
# 
# #save the output as csv so I don't have to wait 8 million hours to pull it from the portal except when I want to update the data
# write_csv(WQparms_all, "Data/Output/WQP_1998-2025_Secchi_20251124.csv") #UPDATE THIS DATE WHEN YOU SAVE IT
# #see how many observations of each type I have
# table(WQparms_all$CharacteristicName)


#import WQsecchi file from saved .csv
WQsecchi <- read.csv("Data/Input/WQ_Secchi_12-6-24.csv")

#look at structure of data
str(WQsecchi)

#look at the organization that collected the data
unique(WQsecchi$OrganizationFormalName)
#using all of them

#create parentdow column from monitoring station IDs, also removes hyphen to match data format I have for other datasets
WQP.secchi <- WQsecchi %>%
  mutate(parentdow = substr(MonitoringLocationIdentifier, 7, 13)) %>%
  mutate(parentdow = gsub("-", "", parentdow))

#set secchi as numeric, sometimes reported as a text description, these just become NA, then filter out these NA values
WQP.secchi <- WQP.secchi %>%
  mutate(ResultMeasureValue = as.numeric(ResultMeasureValue)) %>%
  filter(!is.na(ResultMeasureValue))

#check for consistent method
unique(WQP.secchi$ResultAnalyticalMethod.MethodName)
#we have various methods, let's filter out the ones that look like a transparency tube, keep NA
WQP.secchi <- filter(WQP.secchi, ResultAnalyticalMethod.MethodName != "Secchi Transparency Tube, 100 cm" | is.na(ResultAnalyticalMethod.MethodName))

#check secchi units and make sure all consistent
unique(WQP.secchi$ResultMeasure.MeasureUnitCode)
#we have meters, feet, cm, inches, and "none". Need to convert all to meters and filter out measurements without a unit
WQP.secchi <- WQP.secchi %>%
  filter(ResultMeasure.MeasureUnitCode != "None") %>%
  mutate(secchi_meters = case_when(ResultMeasure.MeasureUnitCode == "ft"~ ResultMeasureValue*0.3048, ResultMeasure.MeasureUnitCode == "m"~ ResultMeasureValue, ResultMeasure.MeasureUnitCode == "cm"~ ResultMeasureValue/100, ResultMeasure.MeasureUnitCode == "in"~ ResultMeasureValue*0.0254))
  
#create a smaller dataset with only columns I care about
WQP.secchi.simple <- WQP.secchi %>%
  select(parentdow,
         secchi_meters,
         OrganizationIdentifier,
         ActivityStartDate,
         MonitoringLocationIdentifier,
         CharacteristicName,
         ResultMeasureValue,
         ResultMeasure.MeasureUnitCode
         )

#create year and month columns
WQP.secchi.simple <- WQP.secchi.simple %>%
  mutate(year = substr(ActivityStartDate, 1, 4)) %>%
  mutate(month = substr(ActivityStartDate, 6, 7))

#make parentdow and year numeric instead of character
#some of the monitoring location ID numbers do not follow the formula with the dow inside them, so they end up having parentdows with letters and get filtered out here because they become NA when changed to numeric
WQP.secchi.simple <- WQP.secchi.simple %>%
  mutate(parentdow = as.numeric(parentdow), year = as.numeric(year)) %>%
  filter(!is.na(parentdow))


#remove more columns I don't need anymore to join to DNR data
WQP.secchi.join <- WQP.secchi.simple %>%
  select(parentdow,
         secchi_meters,
         OrganizationIdentifier,
         year,
         month,
         MonitoringLocationIdentifier,
  )

#THIS CODE TO ADD THE DNR DATA TO THE WQP DATA IS FROM DENVER AND I MODIFIED IT SLIGHTLY TO GET DOW INSTEAD OF NHDHR.ID FOR EACH LAKE
#linking to dnr secchi data

#read in DNR secchi data
dnr_secchi <- read_csv("Data/Input/dnr_all_secchi_1decimal.csv")

#change format to match MPCA.secchi.join format
dnr.secchi.join <- dnr_secchi %>%
  mutate(parentdow = substr(DOW, 1, 6),
         secchi_meters = SECCHI_DISC_READING_FEET*0.3048,
         OrganizationIdentifier = "MNDNR",
         ActivityStartDate = as.Date(SAMPLE_DATE, format = "%m/%d/%Y"),
         year = substr(ActivityStartDate, 1, 4),
         month = substr(ActivityStartDate, 6, 7),
         MonitoringLocationIdentifier = paste0("mndow_", DOW)) %>%
  select(parentdow,
         secchi_meters,
         OrganizationIdentifier,
         year,
         month,
         MonitoringLocationIdentifier)

# #why are there so many 0s?
# dnr.secchi.join %>%
#   summarise(zero = sum(secchi_meters == 0),
#             non_zero = sum(secchi_meters != 0))

# #returns number of observations per location
# dnr.secchi.join %>%
#   group_by(MonitoringLocationIdentifier) %>%
#   count() %>%
#   print(n= nrow(.))

#remove rows with 0 for secchi meters - this doesn't make sense, even a low secchi should be at least 0.0000001
#an email from Corey Geving : "I’m not sure if this is because field crews are skipping over the column and just putting in zero for some reason, or if there’s something else going on. Jon Hansen might have some insight into why there are so many zeroes."
dnr.secchi.join.filter <- dnr.secchi.join %>%
  filter(secchi_meters > 0)

#fix some data structure issues to be able to bind rows
dnr.secchi.join.filter <- dnr.secchi.join.filter %>%
  mutate(parentdow = as.numeric(parentdow), 
         year = as.numeric(year)
         )
#bind the rows of the WQP and DNR secchi data into one data frame
all.secchi <- bind_rows(WQP.secchi.join, dnr.secchi.join.filter)

# #save all this data as a csv for later reference
# write.csv(all.secchi, file = paste0("Data/Output/All_Secchi_Data_WQP_DNR", format(Sys.Date(), "%Y%m%d") ,".csv"))








#DO NOT USE BELOW THIS LINE - THIS IS OLD
#----------------------------------------------------------------------------------------------------------------------------------------------------------------


# #BELOW FILTERS OUT THE SECCHI DATA I WANT TO ACTAULLY USE
# 
# #join the combined DNR and WQ data to the inclusion table
# WQ.join <- all.secchi %>%
#   right_join(incl.table.WQ, by = c("parentdow", "year"))
# 
# #remove the one rows with a secchi_meters value of 0
# #the only one from the WQP was taken in April so maybe ice?
# #DNR zeroes should have been removed above
# #this does not take out any NA values for lakes with secchi data
# WQ.join.clean <- filter(WQ.join, secchi_meters != 0 | is.na(secchi_meters))
# 
# 
# #need to think about temporal aspect of secchi - lets take mean of June/July/Aug but need to make sure all three months have data for all the lakes
# #combine all the inidivual station data for each lake when I take this mean
# #as long as I do this consistently and all the lakes are well represented across the time period I should be able to compare between lakes
# 
# #filter out only June, July, August samples
# WQ.summer <- WQ.join.clean %>%
#   filter(month == "06" | month == "07" | month == "08")
# #check that all the lakes have data from all three months - summarize with the number of months for each lake/year
# WQ.summer.months <- WQ.summer %>%
#   group_by(parentdow, year) %>%
#   summarize(secchi.month.count = length(unique(month)), .groups = 'drop')
# #create an inclusion table with only the lakes that have enough monthly secchi data representation to include
# WQ.good.lakes <- filter(WQ.summer.months, secchi.month.count == 3)
# #join the summer data back to this to only include the observations for the desired lakes
# WQ.good.summer.secchi <- WQ.summer %>%
#   right_join(WQ.good.lakes, by = c("parentdow", "year"))
# 
# 
# # #write a csv file with all of your selected good secchi data
# # write.csv(WQ.good.summer.secchi, file = "Data/Output/Selected_Secchi_Data.csv")
# 
# 
# # #DON'T NEED TO DO THIS, WILL DO IN CREATION OF PRELIM DATASET
# # #summarize the mean of the selected secchi data for each lake/year
# # WQ.secchi.mean <- WQ.good.summer.secchi %>%
# #   group_by(parentdow, year) %>%
# #   summarize(mean.summer.secchi.meters = mean(secchi_meters), .groups = 'drop')



