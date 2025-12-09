#Code copied from "Create Preliminary Multivariate Analysis Dataset" and modified
#USE THIS FROM NOW ON TO UPDATE THE DATASET - this data will be used for the contemporary fish/zoop community analysis (Master's chapter 1)

#Fish and zoop datasets will be formatted and finalized in another script before being added in here at the end
#This formats covariates and then joins in the ready-to-go fish/zoop datasets


#___________________________________________________________________________________

#Starts with inclusion table created in "Create Fish-Zoop Pelagic Model Inclusion Table" 
  #Includes lakes with exact match for fish/zoop sampling years, with surveys that passed my quality checks
#Formats and joins all covariates as detailed in the outline of thesis methods document

#packages
library(tidyverse)
library(readxl)
library(ggplot2)
library(cowplot)
library(scales)
library(readr)
library(dplyr)
library(vegan)
library(tidyr)
library(sf) #to read in .gpkg files
library(foreign) #to read in .dbf files
library(stringr)
library(gridExtra) #to export multiple plots together as .tiff files

#ADD IDENTIFIERS TO INCLUSION TABLE------------------------------------------


#Import inclusion table
Incl.Table <- read.csv("Data/Input/LakeYear_Pelagic_Inclusion_Table.csv")

#make separate year and dow columns in the inclusion table
Incl.Table.DOW <- Incl.Table %>% 
  mutate(Year = str_sub(parentdow.year, -4, -1)) %>% 
  mutate(parentdow = str_sub(parentdow.year, 1, -6))


#I need the crosswalk to also get nhdid values in the inclusion table to join the LAGOS data
#read in crosswalk that Denver made from fish database
crosswalk <- read.csv("Data/Input/dow_nhdhr_fish_lakes.csv")

#create a parentdow column in crosswalk to match my incluson table (no leading zeroes included here)
cw.parentdow <- crosswalk %>%
  mutate(parentdow = case_when(
    (crosswalk$lake_id == "1014202" | crosswalk$lake_id == "1014201" | crosswalk$lake_id == "4003502" | crosswalk$lake_id == "4003501") ~ substr(crosswalk$lake_id, 1, 7),   #takes care of North and Red lakes (7 characters)
    (crosswalk$lake_id == "69037802" | crosswalk$lake_id == "69037801") ~ substr(crosswalk$lake_id, 1, 8),  #takes care of Vermilion (different because 8 characters)
    (nchar(crosswalk$lake_id) == 7 & (crosswalk$lake_id != "01014202" & crosswalk$lake_id != "01014201" & crosswalk$lake_id != "04003502" & crosswalk$lake_id != "04003501" & crosswalk$lake_id != "69037802" & crosswalk$lake_id != "69037801")) ~ substr(crosswalk$lake_id, 1, 5), #this gets 5 digits from the DOWs that have 7 characters and are not those identified before
    (nchar(crosswalk$lake_id) == 8 & (crosswalk$lake_id != "01014202" & crosswalk$lake_id != "01014201" & crosswalk$lake_id != "04003502" & crosswalk$lake_id != "04003501" & crosswalk$lake_id != "69037802" & crosswalk$lake_id != "69037801")) ~ substr(crosswalk$lake_id, 1, 6) #this gets 6 digits from the DOWs that have 8 characters and are not those identified before
  ))

#Join it to the inclusion table
Incl.Table.cross <- left_join(Incl.Table.DOW, cw.parentdow, by = "parentdow")
#get rid of "nhdhr_" in front of the nhdid's
Incl.Table.cross2 <- Incl.Table.cross %>% 
  mutate(nhdhr_id = str_sub(Incl.Table.cross$nhdhr_id, 7))

#Need to find and input nhdids that split Hill and Vermilion, and need to find Lower Red
#lets check lagos lake link, directly filter down to just MN lakes to make this easier
lake_link <- read.csv("Data/Input/lake_link.csv") %>% 
  filter(lake_centroidstate == "MN")

#search for the lakes I want
lake_link_verm <- lake_link %>% 
  filter(str_detect(lake_namelagos, "Vermilion"))
lake_link_hill <- lake_link %>% 
  filter(str_detect(lake_namelagos, "Hill"))
lake_link_red <- lake_link %>% 
  filter(str_detect(lake_namelagos, "Red"))

#Refine further after hand-identifying the right ones
lake_link_verm <- lake_link_verm %>% 
  filter(lagoslakeid == 2554) #Vermilion is split east-west with the wqp ID but not with nhdid or lagos lake id in lagos... this will requires some finagling
lake_link_hill <- lake_link_hill %>% 
  filter(lagoslakeid == 168 | lagoslakeid == 425)
lake_link_red <- lake_link_red %>% 
  filter(lagoslakeid == 39213 | lagoslakeid == 34986)
#I checked all the coordinates on google maps and they are the correct lakes
#Extract and save these nhd values
Verm <- lake_link_verm$lake_nhdid[1]
NHill <- lake_link_hill$lake_nhdid[4]
SHill <- lake_link_hill$lake_nhdid[1]
URed <- lake_link_red$lake_nhdid[29]
LRed <- lake_link_red$lake_nhdid[1]
Crane <- lake_link$lake_nhdid[5830] #Crane lake has the wrong nhdid in the fish database so correcting that too

#add the correct nhdhr ID's to the inclusion table
Incl.Table.nhd <- Incl.Table.cross2 %>% 
  mutate(nhdhr_id = ifelse(Incl.Table.cross2$lake_name == "Hill north", NHill,
                           ifelse(Incl.Table.cross2$lake_name == "Hill south", SHill,
                                  ifelse(Incl.Table.cross2$lake_name == "Red (Lower Red)", LRed,
                                         ifelse(Incl.Table.cross2$lake_name == "Red (Upper Red)", URed,
                                                ifelse(Incl.Table.cross2$lake_name == "Crane", Crane,
                                                      ifelse(Incl.Table.cross2$lake_name == "East Vermilion" | Incl.Table.cross2$lake_name == "West Vermilion", Verm, Incl.Table.cross2$nhdhr_id))))))
          )


#Let's add the lagos lake id to make the inclusion of lagos data easier
#also bring in lagos names to check my work
lake_link_simple <- lake_link %>%  #simplify the lake_link file for joining
  select(lake_nhdid, lagoslakeid, lake_namelagos) %>% #selec the columns I want
  rename(nhdhr_id = lake_nhdid) #rename the nhd column to match for the join

lake_link_unique <- unique(lake_link_simple) #remove duplicate rows

Incl.Table.lagos <- left_join(Incl.Table.nhd, lake_link_unique, by = "nhdhr_id")

#remove unneeded columns
Incl.Table.Final <- Incl.Table.lagos %>% 
  select(-lake_id, -lake_namelagos)


#REMEMBER THAT EAST/WEST VERMILION HAVE THE SAME LAGOS AND NHDIDs... BUT with lagos data I can separate with the mpca station identifiers


#keep environment clean
rm(crosswalk, cw.parentdow, Incl.Table, Incl.Table.cross, Incl.Table.cross2, Incl.Table.DOW,
   Incl.Table.lagos, Incl.Table.nhd, lake_link, lake_link_hill, lake_link_red, lake_link_simple,
   lake_link_unique, lake_link_verm, Crane, LRed, NHill, SHill, URed, Verm)












#REMOTE SENSED DATA FROM LAKEBROWSER (Clarity, Chl-a, CDOM) --------------------------------------------------------------------------------------------


#read in data (2017-2024)
RS <- st_read("Data/Input/Minnesota Lake Water Quality 2017-2024.gpkg")
#I checked that there is only one layer so don't need to specify

#remove geometry aspects of this file
RS.nogeom <- st_drop_geometry(RS)
#now it is a regular dataframe


#remove unnecessary identifiers and lake information, rename DOW and LakeName columns, and remove summarized data we don't want to deal with (the other time frames)
#also get rid of any lakes without a DOW number - these won't have matchable fish surveys anyways
RS.clean <- RS.nogeom %>% 
  select(-MN_LK_NUM, -unmlknum, -umnlknum_o, -PWI_CLASS, -AREA_BASIN, -WETTYPE, -X_UTM, -Y_UTM, -PolyAcres, -US_L3CODE, -US_L3NAME, -Unnamed..73) %>% 
  rename(DOW = dowlknum_1,
        LakeName = RNAME_1
        ) %>% 
  select(-ends_with("0726_0824")) %>% 
  #also fix a naming issue for all metrics in june, july, august of 2022
  rename(
    SD_202206 = SD_202SD_206,
    SD_202207 = SD_202SD_207,
    SD_202208 = SD_202SD_208,
    CL_202206 = CL_202CL_206,
    CL_202207 = CL_202CL_207,
    CL_202208 = CL_202CL_208,
    a440_202206 = a440_202a440_206,
    a440_202207 = a440_202a440_207,
    a440_202208 = a440_202a440_208
  ) %>% 
  filter(!is.na(DOW))




#I need to get this into rows for each lake-year
#step 1 = pivot longer
RS.long <- RS.clean %>% 
  pivot_longer(
    cols = c(-DOW, -LakeName),
    names_to = "Data",
    values_to = "Value"
  )


#Step 2 = Create separate columns for datatype, year, and month,
RS.long <- RS.long %>% 
  #separate data type
  separate(
    Data,
    into = c("DataType", "Date"),
    sep = "_", #split on underscore
    extra = "merge" #gets everything to the right of first underscore, including other underscores
  ) %>% 
  #separate year and month
  separate(
    Date,
    into = c("Year", "Month"),
    sep = "(?<=^.{4})",   # split after the first 4 characters
    fill = "right" #puts everything else into the other column
  )
  

#Step 3 = now pivot it back wider so we have a row for each lake-year-datatype
RS.year <- RS.long %>% 
  pivot_wider(
    names_from = Month,
    values_from = Value,
    values_fn = mean #when there are multiple observations for a single dow/lakename combo, this takes the mean of them (they were just sampled at a sub-basin scale)
  )


#separate out the secchi data so I can filter it differently than the Chl-a and CDOM data
SD.year <- RS.year %>%
    filter(DataType == "SD")

CL.CDOM.year <- RS.year %>%
    filter(DataType == "CL" | DataType == "a440")



#Remove Chl-a and CDOM data that calculates a summer average based only on a June sample (based on exploration) - removes 4839 rows
#remove all the monthly averages and the late summer average
#rename the summer average column to "value"
#also rename the data types with the column names you want in the final dataset
CL.CDOM.avg <- CL.CDOM.year %>% 
  filter(!(!is.na(`06`) & is.na(`07`) & is.na(`08`) & is.na(`09`))) %>% 
  select(-`05`, -`06`, -`07`, -`08`, -`09`, -`10`, -`0720_0920`) %>% 
  rename(value = `0601_0930`) %>% 
  mutate(DataType = ifelse(DataType == "CL", "remote.chla", 
                       ifelse(DataType == "a440", "remote.CDOM", "error")))

#For secchi data, remove all the monthly averages and the summer average (June - Sept)
#rename the late summer average column to "value"
#also rename the data types with the column names you want in the final dataset
SD.avg <- SD.year %>% 
  select(-`05`, -`06`, -`07`, -`08`, -`09`, -`10`, -`0601_0930`) %>% 
  rename(value = `0720_0920`) %>% 
  mutate(DataType = ifelse(DataType == "SD", "remote.secchi", "error"))

#now put the 3 data types back together
RS.summer <- rbind(SD.avg, CL.CDOM.avg)


#Convert this to wide for each lake-year
RS.final.new <- pivot_wider(RS.summer,
                        names_from = DataType,
                        values_from = value)

#NOW GET THE SECCHI DATA FROM THE OLDER YEARS

#read in data (1975-2015)
RS.old <- st_read("Data/Input/Minnesota_Lake_Water_Quality_1975_2015.shp")
#this data ONLY provides the late summer secchi average, which is July 20 - Sept 20

#remove geometry aspects of this file
RS.old.nogeom <- st_drop_geometry(RS.old)
#now it is a regular dataframe

#remove 1975 data (metadata says it uses a different method and is unreliable)
RS.no75 <- RS.old.nogeom %>% 
  select(-SDM_1975)


#remove unnecessary identifiers and lake information, rename DOW and LakeName columns
#also get rid of any lakes without a DOW number - these won't have matchable fish surveys anyways
RS.clean.old <- RS.no75 %>% 
  select(-MN_LK_NUM, -unmlknum, -umnlknum_o, -PWI_CLASS, -AREA_BASIN, -WETTYPE, -X_UTM, -Y_UTM) %>% 
  rename(DOW = dowlknum_1,
         LakeName = RNAME_1
  ) %>% 
  filter(!is.na(DOW))

#I need to get this into rows for each lake-year
#step 1 = pivot longer
RS.long.old <- RS.clean.old %>% 
  pivot_longer(
    cols = c(-DOW, -LakeName),
    names_to = "Year",
    values_to = "remote.secchi"
  )


#get rid of prefix in the year column - this is all secchi so no need to retain data type
RS.long.old$Year <- substr(RS.long.old$Year, 5, 8)

#make empty columns so it matches the newer dataset and I can just add the rows on to it
RS.long.old$remote.chla <- NA
RS.long.old$remote.CDOM <- NA

#add the older secchi data to the newer remote sensed data
RS.all <- rbind(RS.final.new, RS.long.old)

#remove rows (lake years) with NA values for all 3 remote sensed variables
RS.filter <- RS.all %>% 
  filter(!is.na(remote.secchi) | !is.na(remote.chla) | !is.na(remote.CDOM))


#Create parentdow.year column that matches the inclusion table
#this is sightly different from other parentdow creation code because we have some leading 0's we need to get rid of to match other data
RS.parentdow <- RS.filter %>%
  mutate(parentdow = case_when(
    (RS.filter$DOW == "01014202" | RS.filter$DOW == "01014201" | RS.filter$DOW == "04003502" | RS.filter$DOW == "04003501") ~ substr(RS.filter$DOW, 2, 8),   #takes care of North and Red lakes
    (RS.filter$DOW == "69037802" | RS.filter$DOW == "69037801") ~ substr(RS.filter$DOW, 1, 8),  #takes care of Vermilion (different because no leading 0),
    (str_detect(RS.filter$DOW, "^0") & (RS.filter$DOW != "01014202" & RS.filter$DOW != "01014201" & RS.filter$DOW != "04003502" & RS.filter$DOW != "04003501" & RS.filter$DOW != "69037802" & RS.filter$DOW != "69037801")) ~ substr(RS.filter$DOW, 2, 6), #this gets 5 digits from the DOWs that start with zero and are not those identified before
    (!str_detect(RS.filter$DOW, "^0") & (RS.filter$DOW != "01014202" & RS.filter$DOW != "01014201" & RS.filter$DOW != "04003502" & RS.filter$DOW != "04003501" & RS.filter$DOW != "69037802" & RS.filter$DOW != "69037801")) ~ substr(RS.filter$DOW, 1, 6) #this gets 6 digits from the DOWs that don't start with zero and are not those identified before
  ))

RS.parentdow$parentdow.year = paste(RS.parentdow$parentdow, RS.parentdow$Year)

#When there are multiple sub-basins that I don't want to separate, average them together
#also rename secchi column
RS.mean <- RS.parentdow %>%
  group_by(parentdow.year) %>%
  summarize(LakeName = first(LakeName),
            remote.secchi = mean(remote.secchi), 
            remote.chla = mean(remote.chla ),
            remote.CDOM = mean(remote.CDOM),
            .groups = 'drop')

#Rename lake name to show where it came from to check matches later
#also rename secchi to show this column is for exact year matches
RS.final <- RS.mean %>%
  rename(LakeNameRS = LakeName) %>% 
  rename(remote.secchi.exact.year = remote.secchi)

#Join to inclusion table
Join1 <- left_join(Incl.Table.Final, RS.final, by = "parentdow.year")

#now, since we don't have data for every year from the older remote sensed secchi data, we will make another column for the closest secchi year
#first isolate the secchi data
RS.secchi <- RS.mean %>% 
  select(-remote.chla, -remote.CDOM)

#separate parentdows and years in a copy of the Join1 data
secchi.help <- Join1 %>% 
  #filter(is.na(remote.secchi.exact.year)) %>%     #UNCOMMENT THIS LINE IF YOU JUST WANT THE LAKE-YEARS WITHOUT EXACT SECCHI YEARS
  mutate(Year = str_sub(parentdow.year, -4, -1)) %>% 
  mutate(parentdow = str_sub(parentdow.year, 1, -6))
         
#create a column with the closest remote sensed data year
secchi.help$best.secchi.year <- ifelse((secchi.help$Year == 1999 | secchi.help$Year == 2001 | secchi.help$Year == 2002), 2000,
                                       ifelse((secchi.help$Year == 2003 | secchi.help$Year == 2004 | secchi.help$Year == 2006), 2005,
                                              ifelse((secchi.help$Year == 2007 | secchi.help$Year == 2009), 2008, 
                                                     ifelse((secchi.help$Year == 2010 | secchi.help$Year == 2012 | secchi.help$Year == 2013), 2011,
                                                            ifelse((secchi.help$Year == 2014 | secchi.help$Year == 2016), 2015, 
                                                                   ifelse(secchi.help$parentdow.year == "410089 2018", 2019,
                                                                          ifelse(secchi.help$parentdow.year == "410089 2021", 2022,
                                                                                 ifelse(secchi.help$parentdow.year == "130027 2023", 2015, secchi.help$Year)))))))) 


#create a parentdow.secchi.year to join in the closest year data
secchi.help$parentdow.secchi.year <- paste(secchi.help$parentdow, secchi.help$best.secchi.year)

#clean up the secchi help data frame to only columns you need
secchi.help.clean <- secchi.help %>% 
  select(parentdow.year, parentdow.secchi.year)

#join this secchi year back to the Join1
Join2 <- left_join(Join1, secchi.help.clean, by = "parentdow.year")

#change the secchi mean dataset to be able to join just the closest secchi years
secchi.close <- RS.mean %>% 
  select(parentdow.year, LakeName, remote.secchi) %>% 
  rename(parentdow.secchi.year = parentdow.year) %>% 
  rename(LakeNameRS.close = LakeName) %>% 
  rename(remote.secchi.closest.year = remote.secchi)

#Now join to dataset
Join3 <- left_join(Join2, secchi.close, by = "parentdow.secchi.year")

#I visually checked names to make sure things joined well - all looks good

#some cleanup in the Join dataframe
Join4 <- Join3 %>% 
  mutate(remote.secchi.year = str_sub(parentdow.secchi.year, -4, -1)) %>% #this records the true year the remote secchi data came from
  select(parentdow.year, lake_name, Year, parentdow, nhdhr_id, lagoslakeid, remote.chla, remote.CDOM, remote.secchi.exact.year, remote.secchi.closest.year, remote.secchi.year) #keep only the columns you want in the order you want them


#remove previous dataframes to keep environment clean
rm(RS, RS.clean, RS.long, RS.nogeom, RS.summer, RS.year, CL.CDOM.avg, CL.CDOM.year,
   RS.all, RS.clean.old, RS.final.new, RS.long.old, RS.no75, RS.old.nogeom, RS.year.old,
   SD.avg, SD.year, RS.old, RS.parentdow, RS.filter, RS.mean, RS.secchi, secchi.help, secchi.help.clean, secchi.close)


#COMMENTED CODE BELOW EXPLORES WHICH REMOTE SENSED SECCHI DATA I CAN USE (HOW CLOSE THE YEAR HAS TO BE)

# #run a few correlations to see how much using secchi data one year and two years off makes a difference to decide if you can use the closest match
# #how far off is my closest secchi data?
# secchi.error <- abs(as.numeric(Join4$year) - as.numeric(Join4$remote.secchi.year))
# table(secchi.error)
# #So I should investigate the one that is 8 years off and other than that, I have either one or two years off
# 
# RS.test <- RS.final %>% 
#   select(parentdow, Year, remote.secchi.exact.year)
# 
# RS.wide <- pivot_wider(data = RS.test, names_from = Year, values_from = remote.secchi.exact.year)
# 
# #2021 vs. 2022
# cor(RS.wide$`2021`, RS.wide$`2022`, "pairwise.complete.obs")
# RS_2021_v_2022 <- ggplot(data = RS.wide, aes(x = `2021`, y = `2022`))+
#   geom_point()+
#   geom_smooth(method = "lm")
# RS_2021_v_2022
# 
# #2022 vs. 2023
# cor(RS.wide$`2022`, RS.wide$`2023`, "pairwise.complete.obs")
# RS_2022_v_2023 <- ggplot(data = RS.wide, aes(x = `2022`, y = `2023`))+
#   geom_point()+
#   geom_smooth(method = "lm")
# RS_2022_v_2023
# 
# #2017 vs. 2018
# cor(RS.wide$`2017`, RS.wide$`2018`, "pairwise.complete.obs")
# RS_2017_v_2018 <- ggplot(data = RS.wide, aes(x = `2017`, y = `2018`))+
#   geom_point()+
#   geom_smooth(method = "lm")
# RS_2017_v_2018
# 
# #CONCLUSION: One year off is okay - we consistently have a correlation of 0.93 and the dataset did this sometimes anyways without telling us
# 
# #2021 vs. 2023
# cor(RS.wide$`2021`, RS.wide$`2023`, "pairwise.complete.obs")
# RS_2021_v_2023 <- ggplot(data = RS.wide, aes(x = `2021`, y = `2023`))+
#   geom_point()+
#   geom_smooth(method = "lm")
# RS_2021_v_2023
# 
# #2022 vs. 2024
# cor(RS.wide$`2022`, RS.wide$`2024`, "pairwise.complete.obs")
# RS_2022_v_2024 <- ggplot(data = RS.wide, aes(x = `2022`, y = `2024`))+
#   geom_point()+
#   geom_smooth(method = "lm")
# RS_2022_v_2024
# 
# #2017 vs. 2019
# cor(RS.wide$`2017`, RS.wide$`2019`, "pairwise.complete.obs")
# RS_2017_v_2019 <- ggplot(data = RS.wide, aes(x = `2017`, y = `2019`))+
#   geom_point()+
#   geom_smooth(method = "lm")
# RS_2017_v_2019
# 
# #2019 vs. 2021
# cor(RS.wide$`2019`, RS.wide$`2021`, "pairwise.complete.obs")
# RS_2019_v_2021 <- ggplot(data = RS.wide, aes(x = `2019`, y = `2021`))+
#   geom_point()+
#   geom_smooth(method = "lm")
# RS_2019_v_2021
# 
# #I AM ALSO GETTING AT LEAST A 0.89 OR HIGHER CORRELATION FOR 2 YEARS OFF - THIS SEEMS OK
# 
# #2017 vs. 2024
# cor(RS.wide$`2017`, RS.wide$`2024`, "pairwise.complete.obs")
# RS_2017_v_2024 <- ggplot(data = RS.wide, aes(x = `2017`, y = `2024`))+
#   geom_point()+
#   geom_smooth(method = "lm")
# RS_2017_v_2024
# 
# #This wide jump seems ok - but I want to look into the trend of the lake that is 8 years off
# #It is south center lake parentdow = 130027
# SouthCenter <- RS.wide %>% 
#   filter(parentdow == "130027") %>% 
#   pivot_longer(cols = "1985":"2024",names_to = "year", values_to = "secchi")
# 
# SouthCenterPlot <- ggplot(data = SouthCenter, aes(x = year, y = secchi)) +
#   geom_point()+
#   ylim(0, 5)
# SouthCenterPlot
# #It hovers fairly consistently around 1... so I am going to use it
# #ALSO I looked in lake finder and in 2023 (the year I have fish/zoop data for), they describe a clarity of ~3 feet
# 
# #clean up environment
# rm(RS_2017_v_2018, RS_2017_v_2019, RS_2017_v_2024, RS_2019_v_2019, RS_2019_v_2021, RS_2021_v_2022, RS_2021_v_2023,
#    RS_2022_v_2023, RS_2022_v_2024, secchi.error, SouthCenterPlot, SouthCenter, RS.test, RS.wide)







#SECCHI DATA FROM MPCA ------------------------------------------------------------------------------------------------------
#Including to compare to coverage and quality from remote sensed data

#import filtered and formatted water quality data pulled from WQP on 11-24-2025
#Created in the "Pull Water Quality Data" script

secchi.data <- read.csv("Data/Input/WQP_1998-2025_Secchi_20251124_FILTERED_FORMATTED.csv")

#make parentdow.year column
secchi.data$parentdow.year <- paste(secchi.data$parentdow, secchi.data$year)

#join the MPCA secchi data to the inclusion table, preserving all secchi rows
WQ.join <- secchi.data %>%
  right_join(Incl.Table, by = "parentdow.year")

#remove any rows with a secchi_meters value of 0
#this does not take out any NA values - preserves lakes that don't have secchi data in this dataset
WQ.join.clean <- filter(WQ.join, secchi_meters != 0 | is.na(secchi_meters))


#need to think about temporal aspect of secchi - lets take mean of June/July/Aug but need to make sure all three months have data for all the lakes
#combine all the individual station data for each lake when I take this mean
#as long as I do this consistently and all the lakes are well represented across the time period I should be able to compare between lakes

#filter out only June, July, August samples
WQ.summer <- WQ.join.clean %>%
  filter(month == "6" | month == "7" | month == "8")
#check that all the lakes have data from all three months - summarize with the number of months for each lake/year
WQ.summer.months <- WQ.summer %>%
  group_by(parentdow, year) %>%
  summarize(secchi.month.count = length(unique(month)), .groups = 'drop')
#create an inclusion table with only the lakes that have enough monthly secchi data representation to include
WQ.good.lakes <- filter(WQ.summer.months, secchi.month.count == 3)
#join the summer data back to this to only include the observations for the desired lakes
WQ.good.summer.secchi <- WQ.summer %>%
  right_join(WQ.good.lakes, by = c("parentdow", "year"))

#summarize the mean of the selected secchi data for each lake/year
secchi.mean <- WQ.good.summer.secchi %>%
  group_by(parentdow.year) %>%
  summarize(secchi.meters.MPCA.Jun.to.Aug = mean(secchi_meters), .groups = 'drop')
#note that some lake/years are missing because not enough data for them (or none at all)

#join to the rest of the preliminary data
Join5 <- left_join(Join4, secchi.mean, by = "parentdow.year")


#Make another MPCA column with July to September mean, not restrictive on how many months present (more equivalent to remote sensed data)
#filter out only July, August, Sept samples
WQ.late.summer <- WQ.join.clean %>%
  filter(month == "7" | month == "8" | month == "9")
#summarize the mean of the selected secchi data for each lake/year
secchi.mean2 <- WQ.late.summer %>%
  group_by(parentdow.year) %>%
  summarize(secchi.meters.MPCA.Jul.to.Sept = mean(secchi_meters), .groups = 'drop')
#note that some lake/years are missing because not enough data for them (or none at all)

#join to the rest of the preliminary data
Join6 <- left_join(Join5, secchi.mean2, by = "parentdow.year")

#remove unneeded intermediate data frames to keep environment clean
rm(secchi.data,
   secchi.mean,
   WQ.good.lakes,
   WQ.good.summer.secchi,
   WQ.join,
   WQ.join.clean,
   WQ.summer,
   WQ.summer.months,
   WQ.late.summer,
   secchi.mean2
)









#LAGOS LOCUS DATA: Lake surface area, centroid coordinates, elevation, shoreline development factor, drainage connectivity ------------------------------------------------
#these data summarized to lake level, but not lake-year because they should be consistent over the years of my study


#read in both .csv files
Locus.info <- read.csv("Data/Input/lake_information.csv")
Locus.char <- read.csv("Data/Input/lake_characteristics.csv")
#good they are the same length

#filter the info file to only lakes that are at least partially in Minnesota to make it smaller, then delete the behemoth file
Locus.info.MN <- Locus.info %>% 
  filter(str_detect(lake_states, "MN"))

rm(Locus.info)

#select only the columns you want from both dataframes
#also keeping a bunch of flags (for lakes that cross an international border or have a shape that shows they may be riverine)
Locus.info.select <- Locus.info.MN %>% 
  select(lagoslakeid,
         lake_nhdid,
         lake_namelagos,
         lake_onlandborder,
         lake_shapeflag,
         lake_lat_decdeg,
         lake_lon_decdeg,
         lake_elevation_m
         )
                  
Locus.char.select <- Locus.char %>% 
  select(lagoslakeid,
         lake_waterarea_ha,
         lake_shorelinedevfactor, #This is "calculated as lake_perimeter_m divided by the product of 2 times the square root of pi times lake_waterarea_ha"
         lake_connectivity_class #This includes both perennial and intermittent-ephemeral flow
         )

#Join the characteristics data to the info data so it only preserves lakes in the info dataset, which means it will only have Minnesota lakes
Locus.all <- left_join(Locus.info.select, Locus.char.select, by = "lagoslakeid")


                  
                  
#remove unneeded intermediate data frames to keep environment clean
rm(Locus.char,
   Locus.char.select,
   Locus.info.MN,
   Locus.info.select
   )




#LAGOS DEPTH DATA: Max depth and Mean depth----------------------------------------------------------------------------------------------
#mean depth not available for all, but worth seeing how many lakes have it

#read in data
Depth <- read.csv("Data/Input/lake_depth.csv")

#filter to just MN lakes
Depth.MN <- Depth %>% 
  filter(str_detect(lake_states, "MN"))

#select just the columns you want
Depth.select <- Depth.MN %>% 
  select(lagoslakeid,
         lake_maxdepth_m,
         lake_meandepth_m)

#rename to specify I got these depths from lagos
Depth.select.named <- Depth.select %>% 
  rename(lagos_lake_maxdepth_m = lake_maxdepth_m,
         lagos_lake_meandepth_m = lake_meandepth_m)
  

#join to Locus data
Locus.Depth <- full_join(Locus.all, Depth.select.named, by = "lagoslakeid")

#remove intermediary dataframes
rm(Depth.MN, Depth.select, Depth, Depth.select.named)


#LAGOS LIMNO DATA: Conductivity--------------------------------------------------------------------------------

#read in data
Limno <- read.csv("Data/Input/chemistry_limno.csv")

#filter to just conductivity data
Cond <- Limno %>% 
  filter(parameter_name == "spcond_uscm")

#filter to just MN lakes
Cond.MN <- Cond %>% 
  filter(str_detect(sample_id, "MN"))

#where is this data coming from?
table(Cond.MN$source_id)
#mostly MPCA, Red Lake, and USGS

#CHECK FOR VARIOUS FLAGS:

          # #0 values
          # zero.test <- Cond.MN %>% 
          #   filter(parameter_value == 0)
          # 
          # LE6.test <- Cond.MN %>% 
          #   filter(censorcode == "LE6")
          # #so all these 0 values have the same censor code of LE6 which means "parameter_value equals 0; detection limit value missing and neither qualifier nor comments provided"
          # #let's remove these values
          # 
          # 
          # #negative values
          # neg.test <- Cond.MN %>% 
          #   filter(parameter_value < 0)
          # #no negative values present
          # 
          # 
          # #censor code flags related to analytical detection limit
          # table(Cond.MN$censorcode)
          # #already dealt with LE6
          # #LE5 = "parameter_value is missing; detection limit present" - REMOVE THIS ONE OBSERVATION
          # #NC2 = "parameter_value greater than the parameter_detectionlimit_value, detection limit value provided; neither qualifier or comments provided"
          #     #this is because detection limits are almost never specified and when they are specified they are ridiculously low
          #     #IGNORE THIS FLAG
          # #NC4 = "parameter_detectionlimit_value is missing; neither qualifier or comments provided"
          #     #this is the majority of my observations
          #     #IGNORE THIS FLAG
          # 
          # 
          # #not worried about depth flags because depth isn't super important for conductivity - everything is specified anyways

#filter out the data you decided you can't include
Cond.clean <- Cond.MN %>% 
  filter(censorcode != "LE6" & censorcode != "LE5")

#first need to isolate year
Cond.clean$year <- substr(Cond.clean$sample_date, 1, 4)

#take a mean conductivity for each year
Cond.mean <- Cond.clean %>%
  group_by(lagoslakeid, year) %>%
  summarize(cond_uscm = mean(parameter_value), .groups = 'drop')

#remvoe intermediate dataframes
rm(Cond, Cond.clean, Cond.MN)


#this is now formatted as lake-year
#Need to match lagoslakeid to parent DOW, but other than that it should be ready to join


#FISH CPUE JOIN - ALL THE FISH and nhdhr-id :))))) ----------------------------------------------------------------

#read in fish metrics that I downloaded from fish database, quality checked, and calculated the metrics in other scripts
fish <- read.csv("Data/Input/All_Fish_CPUE.csv")


#make parentdow column
fish_parentdow <- fish %>%
  mutate(parentdow = case_when(
    nchar(fish$lake_id) == 7 ~ substr(lake_id, 1, 5),
    nchar(fish$lake_id) == 8 ~ substr(lake_id, 1, 6)
  ))

#make parentdow.fish.year column to join to master datasheet
fish_parentdow$parentdow.fish.year = paste(fish_parentdow$parentdow, fish_parentdow$year)

#filter just the fish columns to join
fish.join <- fish_parentdow %>%
  select(-year.x, -lake_name, -lake_id, -X, -parentdow)

#join fish CPUE to dataset 
#full_join will retain all rows in both initial tables and create NA values when the other table doesn't have info for that lake/year
Data_d <- left_join(Data_c, fish.join, by = "parentdow.fish.year")


#remove unneeded intermediate data frames to keep environment clean
rm(fish,
   fish_parentdow,
   fish.join
)

#not all the rows in the inclusion table have fish data - some of these are core surveys with fish data too recent to be in the BMFD and some are not good enough quality surveys/not enough sampling effort






#TEMP DATA ---------------------------------------------------------------------

#read in degree days for water temp that Gretchen already calculated and connected to DOWs
temp.data <- read.csv("Data/Input/MN_lake_Gdds_all_years.csv")

#create parentdow column
temp_parentdow <- temp.data %>%
  mutate(parentdow = case_when(
    nchar(temp.data$DOW) == 7 ~ substr(DOW, 1, 5),
    nchar(temp.data$DOW) == 8 ~ substr(DOW, 1, 6)
  ))

#check the most recent year we have temp data on
max(temp_parentdow$year)
#looks like 2021 - we will need to fill in this gap eventually


#create parentdow.year for join
temp_parentdow$parentdow.year = paste(temp_parentdow$parentdow, temp_parentdow$year)

#there are sub-basins for some lakes in this temp data, so we get multiple parentdow rows when joining is attempted
#need to condense into one row per parentdow
#are the degree days different between subbasins of the same lake in the same year? check number of unique values:
temp.subbasin.check <- temp_parentdow %>%
  group_by(parentdow.year) %>%
  summarise(num_unique_values = n_distinct(gdd_wtr_5c))
#there are some lake-years with different values in subbasins
#join this to the original inclusion table to see if this is an issue for my lakes
temp.check.2 <- left_join(Incl.Table, temp.subbasin.check, by = "parentdow.year")
unique(temp.check.2$num_unique_values)

#many NA values: these are boundary waters or too recent to be included (2022 or later)

#it is an issue, but only for 4 lakes, corrected here:

#Minnetonka has 4 unique values - this makes sense, it is a huge lake with many sub-basins - fish data came from the non-divided 27013300 DOW so restrict temp to this
temp.dow.correction <- temp_parentdow %>%
  filter(DOW != 27013301 & 
           DOW != 27013302 & 
           DOW != 27013303 & 
           DOW != 27013304 &
           DOW != 27013305 &
           DOW != 27013309 &
           DOW != 27013310 &
           DOW != 27013311 &
           DOW != 27013312 &
           DOW != 27013313 &
           DOW != 27013314 &
           DOW != 27013315 
  )
#Red lake has upper and lower, fish data here came from upper basin (4003501), so restrict to this
temp.dow.correction <- temp.dow.correction %>%
  filter(DOW != 4003500 & 
           DOW != 4003502
  )
#Hill lake fish data came from lake without subbasin: 1014200, restrict to this
temp.dow.correction <- temp.dow.correction %>%
  filter(DOW != 1014201 & 
           DOW != 1014202
  )

#Cut Foot Sioux lake fish data came from lake without subbasin: 31085700, restrict to this
temp.dow.correction <- temp.dow.correction %>%
  filter(DOW != 31085701 & 
           DOW != 31085702 &
           DOW != 31085703
  )

# #check that this correction worked:
# temp.subbasin.check2 <- temp.dow.correction %>%
#   group_by(parentdow.fish.year) %>%
#   summarise(num_unique_values = n_distinct(gdd_wtr_5c))
# temp.check.3 <- left_join(Incl.Table, temp.subbasin.check2, by = "parentdow.fish.year")
# unique(temp.check.3$num_unique_values)
# #it worked

#there are some lakes without temp data but that's ok for now, some of these are boundary waters or after this was calculated up to 2021
#summarize to get only one observation per parentdow.fish.year when there are multiple equivalent degree day values (all the differing ones have already been dealt with individually)
#use this to also select just the columns you want to join
temp.join <- temp.dow.correction %>%
  group_by(parentdow.fish.year) %>%
  summarize(gdd_wtr_5c = mean(gdd_wtr_5c), .groups = 'drop')


#join to the inclusion table
Data_a <- left_join(Incl.Table, temp.join, by = "parentdow.fish.year")

#remove unneeded intermediate data frames to keep environment clean
rm(temp_parentdow,
   temp.check.2,
   temp.check.3,
   temp.subbasin.check,
   temp.subbasin.check2,
   temp.data,
   temp.dow.correction,
   temp.join,
   test
)
#you may get an error message if you didn't run the commented out code to check my work here - this is okay






#ZOOP METRIC JOIN -------------------------------------------------------------------------

#zooplankton metrics calculated here:
#biomass of each species in each lake-year (average of all the tows in a day, then in a month and then in a year) - prevents 


#read data
zoop <- read.csv("Data/Input/ZoopDB_data_20250204.csv")
#this is the zoop data Kylie sent me on Feb 4, 2025 - it is up to date with what she had processed at that time

#create columns for month, day, and year separately
zoop_months <- zoop %>%
  mutate(year = substr(sample_date, 1, 4)) %>% 
  mutate(month = substr(sample_date, 6, 7)) %>%
  mutate(day = substr(sample_date, 9, 10))

#make parentdow column
zoop_parentdow <- zoop_months %>%
  mutate(parentdow = case_when(
    nchar(zoop$dowlknum) == 7 ~ substr(dowlknum, 1, 5),
    nchar(zoop$dowlknum) == 8 ~ substr(dowlknum, 1, 6)
  ))

#make parentdow.zoop.year column to join to master datasheet
zoop_parentdow$parentdow.zoop.year = paste(zoop_parentdow$parentdow, zoop_parentdow$year)

#remove bythotrephes and leptodora from the zoop data -  - discussed with Kylie and Heidi and we don't have the data resolution to include them
zoop_parentdow <- filter(zoop_parentdow, species != "Bythotrephes longimanus" & species != "Leptodora kindti")

# #are there the same number of tows in each month within each lake-year?
# zoop_month_tows <- zoop_parentdow %>%
#   group_by(parentdow.zoop.year, month) %>%
#   summarize(tow.count = n_distinct(sample_id), .groups = 'drop')
# #NO THERE ARE NOT - need to average within months first so timing of sampling effort does not affect final calculations

#isolate just summer months (May - October)
zoop_summer <- zoop_parentdow %>%
  filter(month == "05" | month == "06" | month == "07" | month == "08" | month == "09" | month == "10")

#calculate number of unique summer months for each parentdow.zoop.year
zoop_summer_month_count <- zoop_summer %>%
  group_by(parentdow.zoop.year) %>%
  summarize(Zoop.Month.Count = n_distinct(month), .groups = 'drop')

#need to know which months each lake has - held as separate data frame to investigate effect of requiring/dropping October later
#this makes each month a column, and the data is the day it was sampled or NA for no sample that month
zoop_wide <- pivot_wider(data = zoop_summer, names_from = month, values_from = day)
#some lake/years have more than one sampling date per month

#rename columns to make them easier to deal with and R doesn't think they are numeric
#Here I am calling them by their column number in the data frame, not the month number
zoop_wide <- zoop_wide %>% 
  rename(May = 27, June = 28, July = 29, Aug = 26, Sept = 30, Oct = 31)

#collapse into one column per lake/year - each month column has the count of days in that month where zoops were sampled
zoop_wide_short <- zoop_wide %>%
  group_by(parentdow.zoop.year) %>%
  summarize(May = length(unique(May[!is.na(May)])), June = length(unique(June[!is.na(June)])), July = length(unique(July[!is.na(July)])), Aug = length(unique(Aug[!is.na(Aug)])), Sept = length(unique(Sept[!is.na(Sept)])), Oct = length(unique(Oct[!is.na(Oct)])), .groups = 'drop')

#join the count of zoop months to this dataframe
zoop_summer_month_all <- left_join(zoop_wide_short, zoop_summer_month_count, by = "parentdow.zoop.year")
#KEEP THIS FOR FUTURE USE

#filter to only lake-years with at least 5 summer zoop months (this is the decision... for now)
#first join the month info to the summer zoop data
zoop_summer_month_combo <- left_join(zoop_summer,  zoop_summer_month_all, by = "parentdow.zoop.year")
#now filter to only lake-years with sufficient effort
zoop_good_effort <- zoop_summer_month_combo %>% 
  filter(Zoop.Month.Count >= 5)

#example 

#clean up zooplankton taxonomy
#check species present
unique(zoop_good_effort$species)
#this dataset already has all the littoral samples removed
# 1. rename taxa that need it based on conversation with Heidi and Kylie
#targeted just the Belle lake Daphnia based on Jodie's notes from when she IDed them
#I know I can run these together but I was getting an error I didn't have time to deal with when I tried that
zoop_clean_taxa <- zoop_good_effort %>%
  mutate(species = ifelse(species == "Chydorus sp." | species == "Chydoridae" | species == "Chydorus bicornutus", "Chydorus sphaericus", species)) 
zoop_clean_taxa <- zoop_clean_taxa %>%
  mutate(species = ifelse(species == "Bosmina longirostris" | species == "Bosmina sp.", "Bosminidae", species))
#Bosminid taxonomy here is what we decided with Heidi
zoop_clean_taxa <- zoop_clean_taxa %>%
  mutate(species = ifelse(species == "Alona setulosa" | species == "Alona quadrangularis" , "Alona sp.", species))
zoop_clean_taxa <- zoop_clean_taxa %>%  
  mutate(species = ifelse(species == "Ceriodaphnia reticulata" | species == "Ceriodaphnia quadrangula" | species == "Ceriodaphnia lacustris", "Ceriodaphnia sp.", species))
zoop_clean_taxa <- zoop_clean_taxa %>%
  mutate(species = ifelse(species == "Daphnia pulex", "Daphnia pulicaria", species))
#below I am targeting just the Belle lake Daphnia based on Jodie's notes from when she IDed them
zoop_clean_taxa <- zoop_clean_taxa %>%
  mutate(species = ifelse(species == "Daphnia sp." & parentdow.zoop.year == "470049 2008", "Daphnia rosea", species))
# 2. remove the Daphnia sp. observations without species - counts are all low
zoop_clean_taxa <- zoop_clean_taxa %>%
  filter(species != "Daphnia sp.")
# 3. remove other problematic taxonomic resolutions with low counts
zoop_clean_taxa <- zoop_clean_taxa %>%
  filter(species != "Pleuroxus sp." & species != "Harpacticoida")
#Harpacticoida is another order of copepods (like cyclopoids and calanoids) but only 6 individuals in entire dataset and most not measured
# 4. rename copepod "species" to the taxonomic level we consistently have for them
zoop_clean_taxa <- zoop_clean_taxa %>%
  mutate(species = ifelse(grp2 == "calanoids", "calanoids", 
                          ifelse(grp2 == "cyclopoids", "cyclopoids", species)))

# #check that it worked
sort(unique(zoop_clean_taxa$species))
# #yay!

# #Here I need to check for the "wonky" samples that Heidi warned me about
#     #I AM JOINING THIS TO AN INCLUSION TABLE FROM THE END RESULT OF THIS ENTIRE SCRIPT SO I AM ONLY LOOKING AT DB ISSUES THAT AFFECT THE DATA I WANT TO USE
#     
#     #read in end result of this script (already saved as .csv file)
#     incl.tab <- read.csv("Data/output/PrelimMultivarData.csv")
#     #pull out only the columns you want for the inclusion table
#     incl.tab2 <- incl.tab %>% 
#       select(LakeName, lake_id, parentdow.fish.year) %>% 
#       rename(parentdow.zoop.year = parentdow.fish.year)
#    
#     
#      #I want a count of how many sample_IDs we have for each lake-year
#     zoop_sample_id_check <- zoop_clean_taxa %>%
#       group_by(parentdow.zoop.year, sample_date, site_number, haul_depth_m) %>%
#       summarize(sample.id.Count = n_distinct(sample_id), .groups = 'drop')
#     #join this to the inclusion table to only get the ones that matter
#     zoop_sample_id_check_relevant <- left_join(incl.tab2, zoop_sample_id_check, by = "parentdow.zoop.year")
# 
#     #Here I need to check for species with multiple rows within a sample ID
#     #I want a count of how many sample_IDs we have for each lake-year
#     zoop_species_row_check <- zoop_clean_taxa %>%
#       group_by(parentdow.zoop.year, sample_date, site_number, sample_id, species) %>%
#       summarize(sample.id.Count = n_distinct(count), .groups = 'drop')
#     #join this to the inclusion table to only get the ones that matter
#     zoop_species_row_check_relevant <- left_join(incl.tab2, zoop_species_row_check, by = "parentdow.zoop.year")
#     #remove cyclopoids and calanoids because I created that here
#     zoop_species_row_check_relevant_filter <- zoop_species_row_check_relevant %>% 
#       filter(species != "calanoids" & species != "cyclopoids")
#     #There are a few problematic surveys: Mille Lacs 2010-9-14, Mille Lacs 2010-6-29, Ten Mile 2010-5-19 and Ten Mile 2010-6-21
# 
#     #does this also happen before I rename? - try with pre-taxonomic fix zoop data
#     zoop_species_row_check2 <- zoop_good_effort %>%
#       group_by(parentdow.zoop.year, sample_date, site_number, sample_id, species) %>%
#       summarize(sample.id.Count = n_distinct(count), .groups = 'drop')
#     #YES IT DOES
#     #DO I get the same list here as when I checked this after renaming? - YES I DO - end up with same list of surveys
#     #join this to the inclusion table to only get the ones that matter
#     zoop_species_row_check_relevant2 <- left_join(incl.tab2, zoop_species_row_check2, by = "parentdow.zoop.year")
# 
# #TAKEAWAY: WE HAVE PROBLEMS HERE- CURRENTLY WORKING THIS OUT WITH HEIDI, KYLIE, JAKE
# #FOR NOW (4-10-25) proceeding with analyses for stats class without addressing this - circle back to this.


#need to make sure all species have a row for all tows - even if the biomass value is 0 so that my means calculate correctly
#create a unique identifier for each tow
zoop_clean_taxa$tow.id <- paste(zoop_clean_taxa$parentdow.zoop.year, zoop_clean_taxa$sample_id)
#make all the empty rows you need, preserve the groups you need to average, and fill the data values with 0 for the new rows
zoop_complete <- complete(data = zoop_clean_taxa, nesting(parentdow.zoop.year, month, sample_date, sample_id), species, fill = list(density = 0, biomass = 0, number_pct = 0, weight_pct = 0, mean_weight = 0, mean_length = 0, count = 0), explicit = FALSE)


#first need to average tow biomasses on each sampling date in each lake-year for each species
zoop_biom_day_mean <- zoop_complete %>%
  group_by(parentdow.zoop.year, month, sample_date, species) %>%
  summarize(biomass = mean(biomass), .groups = 'drop')    

#then average daily biomass in each month in each lake-year for each species
zoop_biom_month_mean <- zoop_biom_day_mean %>%
  group_by(parentdow.zoop.year, month, species) %>%
  summarize(biomass = mean(biomass), .groups = 'drop') 

#finally average monthly biomasses to get average biomass in each lake-year for each species
zoop_biom_year_mean <- zoop_biom_month_mean %>%
  group_by(parentdow.zoop.year, species) %>%
  summarize(biomass = mean(biomass), .groups = 'drop')

#convert to wide so there is a column for each species
zoop_wide <- pivot_wider(data = zoop_biom_year_mean, names_from = species, values_from = biomass)
#add the zoop sampling month info back in so it makes it to master dataset
zoop_wide_month <- left_join(zoop_wide, zoop_summer_month_all, by = "parentdow.zoop.year")
#rename parentdow.zoop.year so I can join it
zoop_wide_month <- zoop_wide_month %>% 
  rename(parentdow.fish.year = parentdow.zoop.year)



#join the zoop biomass metrics to the rest of the data
Data_e <- left_join(Data_d, zoop_wide_month, by = "parentdow.fish.year")

#clean up the environment
rm(zoop_month_tows,
   zoop_months,
   zoop_parentdow,
   zoop,
   zoop_wide,
   zoop_wide_short,
   zoop_wide_month,
   zoop_summer_month_count,
   zoop_complete,
   zoop_biom_day_mean,
   zoop_biom_month_mean,
   zoop_biom_year_mean,
   zoop_clean_taxa,
   zoop_good_effort,
   zoop_summer_month_combo,
   zoop_summer_month_all,
   zoop_summer
)












#INFESTED WATERS DATA DOWNLOAD AND MERGE ------------------------------------------------------------------------------

# Data----
## Infested Waters List (UPDATE FILE PATHS, THEN RUN SECTION TO UPDATE INFESTED WATERS DATA) ----
# The code below will automatically download the newest infested-waters.xlsx document from our webpage
# as long as the url for the download hasn't changed. The code will also get the data into 
# a workable/summarized formats for the visualizations below. 

# Click the down arrow by the line number next to "Data ..." to hide/show
# the code for downloading and updating the infested waters data used here.


#ONLY RUN THIS THE FIRST TIME YOU NEED TO GET THE INFESTED WATERS DATA (OR UPDATE IT)
# Infested Waters URL
#url_iw <- "https://files.dnr.state.mn.us/eco/invasives/infested-waters.xlsx"

### UPDATE FILE PATH FOR IW DOWNLOAD----
# Path to where I want data stored
#data_path <- "G:/My Drive/Thesis/Data/R Working Directories/Walleye.Zoop.Paleo/"

# Date data was downloaded
# This will make it so that a new file is generated in my Data folder every time I run this code
#date_downloaded <- Sys.Date()

# Name I want to save infested waters under
#data_name <- paste("infested-waters_", date_downloaded, ".xlsx", sep="")

# Paste together data_path and data_name to tell R where to save downloaded infested waters list
#destfile <- paste(data_path, data_name, sep="")

# Download infested waters list
#download.file(url_iw, destfile, mode='wb')

# Read in infested waters list
#run this if downloading data for the first time
#iw <- read_excel(path=destfile, skip=1)
#RUN THIS IF JUST READING THE INFESTED WATERS SPREADSHEET ALREADY SAVED IN WORKING DIRECTORY
iw <- read_excel("Data/Input/Copy of infested-waters_2024-10-09.xlsx")

#ALWAYS RUN THIS
iw <- iw[, -dim(iw)[2]]

# Change column names
# I normally don't do this, but the default are pretty bulky to code with
colnames(iw) <- c("waterbody_name", "county", "species", "year", "year_conf", "dowlknum")

#Creates a new column that specifies if a water body is connected or confirmed
iw$connected <- factor(ifelse(grepl("connect", iw$year_conf) | grepl("Connect", iw$year_conf) | grepl("conect", iw$year_conf),
                              "connected", "confirmed"))

iw$species[iw$species=="Eurasian Watermilfoil"] <- "Eurasian watermilfoil"

# Fixing some dowlknum entries, hyphenating these really helps here
# This code fixes dowlknum issues up to 9/11/2024
# CC-LLLL-BB, where C=county, L=lake or parent dow, B=basin, is a nice convention to standardize to
unique(iw$dowlknum[!grepl("-", iw$dowlknum)])

iw$dowlknum[iw$dowlknum=="NA"] <- NA
iw$dowlknum[iw$dowlknum=="na"] <- NA
iw$dowlknum[iw$dowlknum=="none"] <- NA
iw$dowlknum[iw$dowlknum=="NONE"] <- NA

iw$dowlknum[iw$dowlknum=="none, part of Winnibigoshish"] <- "11-0147"

iw$dowlknum[iw$dowlknum=="18002900" ] <- "18-0029"

# There are some dowlknum that use hyphens, but not according to the convention above
# These all look good: CC-LLLL
unique(iw$dowlknum[grepl("-", iw$dowlknum) & nchar(iw$dowlknum)==7])
# None here:
unique(iw$dowlknum[grepl("-", iw$dowlknum) & nchar(iw$dowlknum)==8])
# Here's a mistake where there isn't a second hyphen
unique(iw$dowlknum[grepl("-", iw$dowlknum) & nchar(iw$dowlknum)==9])
# fix second hyphen
iw$dowlknum[iw$dowlknum=="18-012601"] <- "18-0126-01"
# These all look good; CC-LLLL-BB
unique(iw$dowlknum[grepl("-", iw$dowlknum) & nchar(iw$dowlknum)==10])

#Make column for parent dow - no sub-basin
iw$parentdow <- substr(iw$dowlknum, 1, 7)
#leading zeros not a problem because stored as character data


#INFESTED WATERS JOIN

#Combining infested water data with the matched fish/zoop data
#Column to match: DOW - but needs to be reformatted in match data

#filter iw to only include confirmed invasive species
iw_confirmed <-  filter(iw, connected=="confirmed")

#remove "-" from infested waters parent dow and remove leading zeroes to match other data
iw_confirmed_format <- iw_confirmed %>%
  mutate(parentdow=str_remove_all(parentdow, "-")) %>%
  mutate(parentdow=ifelse(substr(parentdow, 1, 1)=="0", substr(parentdow, 2, 6), parentdow))


#now the parentdow columns in the two datasets are the same format and can be used to match

#filter out just zebra mussels and spiny water fleas
zm_swf <- iw_confirmed_format %>% 
  filter(species == "zebra mussel" | species == "spiny waterflea")
#need to summarize infested water data into one row per lake - change from long to wide format
zm_swf_wide <- spread(zm_swf, species, year)
#combine the rows if multiple for each lake and drop unnecessary rows - only keep parentdow for match
zm_swf_wide_format <- zm_swf_wide %>%
  group_by(parentdow) %>%
  summarize(SpinyWaterflea = max(`spiny waterflea`, na.rm = TRUE), ZebraMussel = max(`zebra mussel`, na.rm = TRUE))



# #USE THIS CODE IF YOU WANT ALL THE SPECIES
# #need to summarize infested water data into one row per lake - change from long to wide format
# #make a column for each invasive species and fill it with the year it was found
# iw_wide <- spread(iw_confirmed_format, species, year)
# #combine the rows if multiple for each lake and drop unnecessary rows - only keep parentdow for match
# iw_wide_format <- iw_wide %>%
#   group_by(parentdow) %>%
#   summarize(BigheadCarp = max(`bighead carp`, na.rm = TRUE), BrittleNaiad = max(`brittle naiad`, na.rm = TRUE), 
#             EurasianWatermilfoil = max(`Eurasian watermilfoil`, na.rm = TRUE), FaucetSnail = max(`faucet snail`, na.rm = TRUE), 
#             FloweringRush = max(`flowering rush`, na.rm = TRUE), GrassCarp = max(`grass carp`, na.rm = TRUE), 
#             NZMudSnail = max(`New Zealand mud snail`, na.rm = TRUE), RedSwampCrayfish = max(`red swamp crayfish`, na.rm = TRUE), 
#             RoundGoby = max(`round goby`, na.rm = TRUE), Ruffe = max(ruffe, na.rm = TRUE), SilverCarp = max(`silver carp`, na.rm = TRUE), 
#             SpinyWaterflea = max(`spiny waterflea`, na.rm = TRUE), StarryStonewart = max(`starry stonewort`, na.rm = TRUE), 
#             VHS = max(VHS, na.rm = TRUE), WhitePerch = max(`white perch`, na.rm = TRUE), ZebraMussel = max(`zebra mussel`, na.rm = TRUE))

zm_swf_wide_format$parentdow <- as.numeric(zm_swf_wide_format$parentdow)

Data_all <- left_join(Data_e, zm_swf_wide_format, by = "parentdow")

#remove unneeded intermediate data frames to keep environment clean
rm(iw,
   iw_confirmed,
   iw_confirmed_format,
   zm_swf,
   zm_swf_wide,
   zm_swf_wide_format
)

#calculate if lake-years are invaded or not chronologically
Data_all$SpinyWaterflea <- as.numeric(Data_all$SpinyWaterflea)
Data_all$ZebraMussel <- as.numeric(Data_all$ZebraMussel)

Data_all$SpinyWaterflea <- ifelse(is.na(Data_all$SpinyWaterflea), "no", 
                                  ifelse(Data_all$SpinyWaterflea - Data_all$year <= 0, "yes", "no"))

Data_all$ZebraMussel <- ifelse(is.na(Data_all$ZebraMussel), "no", 
                               ifelse(Data_all$ZebraMussel - Data_all$year <= 0, "yes", "no"))







#FILTER out to select the lakes with all the data I can actually use. ------------------------------------------------------------------------

#remove extra columns I never used or don't need like the empty invasive species and the walleye count from the old spreadsheet
Data_all <- Data_all %>%
  select(-Chosen.for.analysis.)

#Requirements:
#conditions filtered above in this script:
#lake/years with exact matches of fish/zoop data
#zoops: at least 5 summer months for each lake/year, removed Bytho and Lepto

#conditions filtered as I generated data in other scripts:
#fish: acceptable survey and gear types, sufficient effort, removed surveys with gear issues
#productivity: secchi data must have samples from June, July, and August in the lake/year, no secchi values of 0, all secchi data from MPCA and DNR 

#need to filter to lake-years that have all the data: fish survey, zoop survey, temp, clarity, area AND require May and June zoop samples
Data_complete <- Data_all %>%    
  filter(!is.na(calanoids) &
           !is.na(WAE.CPUE) &
           !is.na(gdd_wtr_5c) &
           !is.na(mean.summer.secchi.meters) &
           !is.na(lake.area.acres)&
           May != 0 &
           June != 0
  )
#sample size = 110 lake-years (it's 119 if I don't require any specific zoop months)
unique(Data_complete$LakeName)
#31 different lakes represented

#INVEESTIGATE EFFECT OF EXCLUDING MONTHS WITHOUT OCTOBER
Data_req_Oct <- Data_all %>%    
  filter(!is.na(calanoids) &
           !is.na(WAE.CPUE) &
           !is.na(gdd_wtr_5c) &
           !is.na(mean.summer.secchi.meters) &
           !is.na(lake.area.acres)&
           May != 0 &
           June != 0 &
           Oct != 0
  )

#requiring october brings the sample size down to 82
unique(Data_req_Oct$LakeName)
#29 different lakes represented - lose Freeborn and Garfield

#WHAT IF I REQUIRE MAY-SEPTEMBER 
Data_req_M_S <- Data_all %>%    
  filter(!is.na(calanoids) &
           !is.na(WAE.CPUE) &
           !is.na(gdd_wtr_5c) &
           !is.na(mean.summer.secchi.meters) &
           !is.na(lake.area.acres)&
           May != 0 &
           June != 0 &
           July != 0 &
           Aug != 0 &
           Sept != 0
  )

#This gets us a sample size of 106. I think this is the best option.
unique(Data_req_M_S$LakeName)
#31 different lakes represented - SO WE DON'T LOSE ANY LAKES - THIS IS GOOD
table(Data_req_M_S$LakeName)

#Save this output!
#write.csv(Data_req_M_S, file = "Data/Output/PrelimMultivarData.csv")

# #What is the effect of the potential confounding variable lack of data?
# 
# #what is our sample size if we don't consider, temp, area, productivity data coverage but keep the May-Sept zoop restriction?
# test <- Data_all %>%    
#       filter(!is.na(calanoids) &
#          !is.na(WAE.CPUE) &
#          May != 0 &
#          June != 0 &
#          July != 0 &
#          Aug != 0 &
#          Sept != 0
# )
# unique(test$LakeName)
# #168 observations on 38 lakes
# #so these potential confounding variables take 62 observations and 7 lakes from us...
# 
# #How many of which observations are we missing?
# test2 <- Data_all %>%    
#   filter(!is.na(calanoids) &
#            !is.na(WAE.CPUE) &
#            is.na(gdd_wtr_5c) &
#            May != 0 &
#            June != 0 &
#            July != 0 &
#            Aug != 0 &
#            Sept != 0
#   )
# unique(test2$LakeName)
# #29 lake-years on 6 lakes missing temp data
# 
# test3 <- Data_all %>%    
#   filter(!is.na(calanoids) &
#            !is.na(WAE.CPUE) &
#            is.na(mean.summer.secchi.meters) &
#            May != 0 &
#            June != 0 &
#            July != 0 &
#            Aug != 0 &
#            Sept != 0
#   )
# unique(test3$LakeName)
# #37 lake-years on 11 lakes missing secchi data
# 
# test4 <- Data_all %>%    
#   filter(!is.na(calanoids) &
#            !is.na(WAE.CPUE) &
#            is.na(lake.area.acres) &
#            May != 0 &
#            June != 0 &
#            July != 0 &
#            Aug != 0 &
#            Sept != 0
#   )
# unique(test4$LakeName)
# #None are missing lake area!
# 
# #How many are missing BOTH temp and secchi?
# test5 <- Data_all %>%    
#   filter(!is.na(calanoids) &
#            !is.na(WAE.CPUE) &
#            is.na(mean.summer.secchi.meters) &
#            is.na(gdd_wtr_5c) &
#            May != 0 &
#            June != 0 &
#            July != 0 &
#            Aug != 0 &
#            Sept != 0
#   )
# unique(test5$LakeName)
# #only 4 observations on two lakes: Namakan and Winnie
# 
# #lakes missing only secchi
# test6 <- Data_all %>%
#   filter(!is.na(calanoids) &
#            !is.na(WAE.CPUE) &
#            is.na(mean.summer.secchi.meters) &
#            !is.na(gdd_wtr_5c) &
#            May != 0 &
#            June != 0 &
#            July != 0 &
#            Aug != 0 &
#            Sept != 0
#   )
# table(test6$LakeName)
# 
# #lakes missing only temp
# test7 <- Data_all %>%
#   filter(!is.na(calanoids) &
#            !is.na(WAE.CPUE) &
#            !is.na(mean.summer.secchi.meters) &
#            is.na(gdd_wtr_5c) &
#            May != 0 &
#            June != 0 &
#            July != 0 &
#            Aug != 0 &
#            Sept != 0
#   )
# table(test7$LakeName)


