#this script downloads water quality data from USGS Water Quality Portal and some DNR data
#based on Denver's water quality pull for his zebra mussel project

library(dataRetrieval)
library(tidyverse)


#parameters to get: secchi, water temp C (NWIS parm code 00010), air temp C (NWIS parm code 00020)
#https://www.waterqualitydata.us/public_srsnames/ - this is the link to the list of NWIS parameters

# #check data available for one station in one of my lakes
# Long <- readWQPdata(statecode = "MN", siteid = "MNPCA-03-0383-00-202")
# unique(Long$CharacteristicName)
# #looks like temp for MPCA reported as "Temperature, water"


# #ONLY RUN THIS TO UPDATE MPCA DATA AND IF YOU HAVE 2-3 HOURS TO WAIT FOR IT
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
WQsecchi <- read.csv("Data/Input/WQ_Secchi_12-6-24.csv")

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
  
#create a smaller dataset with only columns I care about
MPCA.secchi.simple <- MPCA.secchi %>%
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
MPCA.secchi.simple <- MPCA.secchi.simple %>%
  mutate(year = substr(ActivityStartDate, 1, 4)) %>%
  mutate(month = substr(ActivityStartDate, 6, 7))

#make parentdow nad year numeric instead of character
#some of the monitoring location ID numbers do not follow the formula with the dow inside them, so they end up having parentdows with letters and get filtered out here because they become NA when changed to numeric
MPCA.secchi.simple <- MPCA.secchi.simple %>%
  mutate(parentdow = as.numeric(parentdow), year = as.numeric(year)) %>%
  filter(!is.na(parentdow))



# #SEE HOW MUCH USEFUL DATA THIS ACTUALLY GAVE ME
# #import inclusion table created/cleaned in fish download R script
# incl.table.WQ <- read.csv("Data/Input/Inclusion.Table.Clean.Exact.csv")
# 
# #do a right join to pull out all the water quality portal data that matches our lakes and years
# wpq.join <- MPCA.secchi.simple %>%
#   right_join(incl.table.WQ, by = c("parentdow", "year"))
# 
# #find the NA values - these are the lakes that don't have secchi data from the portal
# wpq.na <- filter(wpq.join, is.na(secchi_meters))
# #there are 67 of them, so I will need to include DNR data that Denver has
# 
# #remove these temporary joins so they don't confuse me later
# rm(wpq.join)
# rm(wpq.na)




# #THIS CODE TO ADD THE DNR DATA TO THE WQP DATA IS FROM DENVER AND I MODIFIED IT SLIGHTLY TO GET DOW INSTEAD OF NHDHR.ID FOR EACH LAKE
# #linking to dnr secchi data
# dnr_secchi <- read_csv("Data/dnr_all_secchi.csv") %>% 
#   mutate(ResultMeasureValue = SECCHI_DISC_READING_FEET*0.3048,
#          ResultMeasure.MeasureUnitCode = "m",
#          ActivityStartDate = as.Date(SAMPLE_DATE, format = "%m/%d/%Y")) %>%
#   local_to_nhdhr(from_colname = "DOW", states = "mn") %>% 
#   rename(site_id = nhdhr.id) %>% 
#   mutate(MonitoringLocationIdentifier = paste0("mndow_", DOW),
#          OrganizationIdentifier = "MNDNR") %>% 
#   select(site_id,
#          MonitoringLocationIdentifier,
#          OrganizationIdentifier,
#          ActivityStartDate,
#          ResultMeasureValue,
#          ResultMeasure.MeasureUnitCode,
#          SECCHI_DISC_READING_FEET)
# #why are there so many 0s?
# dnr_secchi %>% 
#   summarise(zero = sum(SECCHI_DISC_READING_FEET == 0),
#             non_zero = sum(SECCHI_DISC_READING_FEET != 0))
# 
# dnr_secchi %>% 
#   filter(is.na(site_id)) %>% 
#   group_by(MonitoringLocationIdentifier) %>% 
#   count() %>% 
#   print(n= nrow(.))
# #DOWS that do not get an nhdid appear to be either very small or riverine - im going to get rid of them
# #~5,000 samples from the pools of the Mississippi river 
# #get rid of non-nhdid water bodies and 
# dnr_secchi <- dnr_secchi %>% 
#   filter(!is.na(site_id))
# secchi <- bind_rows(dnr_secchi, secchi)
# rm(dnr_secchi)
# #saving just the combined secchi data
# secchi.save <- secchi %>% 
#   select(site_id,
#          MonitoringLocationIdentifier,
#          lake_namelagos,
#          OrganizationIdentifier,
#          ActivityStartDate,
#          ResultMeasureValue,
#          ResultMeasure.MeasureUnitCode,
#          SECCHI_DISC_READING_FEET,
#          CharacteristicName,
#          nhdhr_area_sqkm
#   )
# glimpse(secchi.save)
# write_csv(secchi.save, "dnr_wqp_secchi.csv")
# rm(secchi.save)

#call your combined file all.secchi so the code below works:

#save all this data as a csv for later reference
# write.csv(all.secchi, file = "Data/Output/All_Secchi_Data_MPCA_DNR.csv"


#BELOW FILTERS OUT THE SECCHI DATA I WANT TO ACTAULLY USE 

#join the combined DNR and WQ data to the inclusion table
WQ.join <- all.secchi %>%
  right_join(incl.table.WQ, by = c("parentdow", "year"))

#remove the one rows with a secchi_meters value of 0
#the only one from the WQP was taken in April so maybe ice?
#DNR zeroes should have been removed above
#this does not take out any NA values for lakes with secchi data
WQ.join.clean <- filter(WQ.join, secchi_meters != 0 | is.na(secchi_meters))


#need to think about temporal aspect of secchi - lets take mean of June/July/Aug but need to make sure all three months have data for all the lakes
#combine all the inidivual station data for each lake when I take this mean
#as long as I do this consistently and all the lakes are well represented across the time period I should be able to compare between lakes

#filter out only June, July, August samples
WQ.summer <- WQ.join.clean %>%
  filter(month == "06" | month == "07" | month == "08")
#check that all the lakes have data from all three months - summarize with the number of months for each lake/year
WQ.summer.months <- WQ.summer %>%
  group_by(parentdow, year) %>%
  summarize(month.count = length(unique(month)), .groups = 'drop')
#create an inclusion table with only the lakes that have enough monthly secchi data representation to include
WQ.good.lakes <- filter(WQ.summer.months, month.count == 3)
#join the summer data back to this to only include the observations for the desired lakes
WQ.good.summer.secchi <- WQ.summer %>%
  right_join(WQ.good.lakes, by = c("parentdow", "year"))
#summarize the mean of the selected secchi data for each lake/year
WQ.secchi.mean <- WQ.good.summer.secchi %>%
  group_by(parentdow, year) %>%
  summarize(mean.summer.secchi.meters = mean(secchi_meters), .groups = 'drop')

# #write a csv file with all of your selected good secchi data
# write.csv(WQ.good.summer.secchi, file = "Data/Output/Selected_Secchi_Data.csv"







