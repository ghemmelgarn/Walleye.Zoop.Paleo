#Code copied from "Create Preliminary Multivariate Analysis Dataset" and modified
#USE THIS FROM NOW ON TO UPDATE THE DATASET - this data will be used for the conteporary fish/zoop community analysis (Master's chapter 1)


#___________________________________________________________________________________

#Starts with inclusion table created in "1a Read in Fish Data" - includes lakes with exact match for fish/zoop sampling years AND all core lakes
#Formats and joins all covariates as detailed in the outline of thesis methods document

#Adds fish data for all species
#Downloads and joins invasive species information (zebra mussels and Bythotrephes only) from DNR infested waters list
#Filters out to just complete and best data - see description at end


#corrects zooplankton taxonomy issues in the selected data and calculates an accurate Shannon Diversity Index
#cleans zoop dataset based on Kylie's notes and known issues with DNR zooplankton dataset
#calculates zoop metrics (Bytho and Lepto excluded) and joins them



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
library(stringr)
library(gridExtra) #to export multiple plots together as .tiff files

#Starting inclusion table known issues:
#Inclusive to all potential data that may or may not be used due to various issues
#Only includes fish/zoop years with EXACT matches PLUS all the core lakes, year listed for core lakes is fish survey
#at some point check if some core lakes have 2025 fish surveys we didn't know about
#as of 3-12-25, the 2025 sampling season is still in the future so we won't get all the WQ data yet

#Import inclusion table
Incl.Table <- read.csv("Data/Input/Inclusion.Table.Clean.Exact.csv")


#remove Hill lake south as a row from the dataset - the zoops will get summarized appropriately in "Hill (north)" by parentdow and the fish will too
Incl.Table <- filter(Incl.Table, LakeName != "Hill (south)")








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


#create parentdow.fish.year for join
temp_parentdow$parentdow.fish.year = paste(temp_parentdow$parentdow, temp_parentdow$year)

# #there are sub-basins for some lakes in this temp data, so we get multiple parentdow rows when joining is attempted
# #need to condense into one row per parentdow
# #are the degree days different between subbasins of the same lake in the same year? check number of unique values:
# temp.subbasin.check <- temp_parentdow %>%
#   group_by(parentdow.fish.year) %>%
#   summarise(num_unique_values = n_distinct(gdd_wtr_5c))
# #there are some lake-years with different values in subbasins
# #join this to the MatchData original inclusion table to see if this is an issue for my lakes
# temp.check.2 <- left_join(Incl.Table, temp.subbasin.check, by = "parentdow.fish.year")
# unique(temp.check.2$num_unique_values)

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








#SECCHI DATA FROM MPCA ------------------------------------------------------------------------------------------------------

#import all water quality data pulled from WQP in December 2024 plus the .csv file the DNR gave Denver (idk when exactly)
secchi.data <- read.csv("Data/Input/All_Secchi_Data_WQP_DNR.csv")

#join the combined DNR and WQ data to the inclusion table, preserving all secchi rows
WQ.join <- secchi.data %>%
  right_join(Incl.Table, by = c("parentdow", "year"))

#remove the one rows with a secchi_meters value of 0
#the only one from the WQP was taken in April so maybe ice?
#DNR zeroes should have been removed when creating this data
#this does not take out any NA values for lakes with secchi data
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
  group_by(parentdow, year) %>%
  summarize(mean.summer.secchi.meters = mean(secchi_meters), .groups = 'drop')
#note that some lake/years are missing because not enough data for them (or none at all)

#create parentdow.fish.year for join
secchi.mean$parentdow.fish.year = paste(secchi.mean$parentdow, secchi.mean$year)

#remove the parentdow and year columns so they don't get confusing and duplicated
secchi.mean <- secchi.mean %>%
  select(parentdow.fish.year, mean.summer.secchi.meters)

#join to the rest of the preliminary data
Data_b <- left_join(Data_a, secchi.mean, by = "parentdow.fish.year")

#remove unneeded intermediate data frames to keep environment clean
rm(secchi.data,
   secchi.mean,
   WQ.good.lakes,
   WQ.good.summer.secchi,
   WQ.join,
   WQ.join.clean,
   WQ.summer,
   WQ.summer.months
)




#READ IN ALL REMOTE SENSED DATA FROM LAKEBROWSER--------------------------------------------------------------------------------------------

#read in data
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
  select(-ends_with("0720_0920"), -ends_with("0726_0824")) %>% 
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


#Create separate columns for datatype, year, and month,
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
  

#now pivot it back wider so we have a row for each lake-year-datatype
RS.year <- RS.long %>% 
  pivot_wider(
    names_from = Month,
    values_from = Value,
    values_fn = mean #when there are multiple observations for a single dow/lakename combo, this takes the mean of them (they were just sampled at a sub-basin scale)
  )

# #see if I can calculate an equivalent mean 
# RS.year$JunSeptTest <- (RS.year$`06`+ RS.year$`07`+ RS.year$`08`+ RS.year$`09`)/4
# #THESE ARE NOT THE SAME - BECAUSE IM SURE SAMPLE SIZE WITHIN EACH MONTH IS NOT CONSISTENT
# #ALSO THERE ARE MANY MONTHS THAT ARE MISSING DATA - AND THEY REPORT THE SUMMER MEAN ANYWAYS EVEN IF ONLY ONE MONTH OF DATA
# #I need to look into how much data is enough to predict the total summer average
# #START HERE DOING CORRELATION TO TEST THIS OUT FOR LAKE-YEARS WITH ALL THE DATA
# 
# #I WILL USE THE SUMMER AVERAGE THEY REPORT, BUT I WILL RESTRICT TO LAKE-YEARS WITH WHAT I DETERMINE TO BE SUFFICIENT DATA

#filter out only lake-years with all data in all 4 summer months and remove May and October data
RS.test.data <- RS.year %>% 
  filter(!is.na(`06`) & !is.na(`07`) & !is.na(`08`) & !is.na(`09`)) %>% 
  select(-`05`, -`10`)

#create separate data frames for SD, CL, and CDOM
SD.test.data <- RS.test.data %>% 
  filter(DataType == "SD")

CL.test.data <- RS.test.data %>% 
  filter(DataType == "CL")

CDOM.test.data <- RS.test.data %>% 
  filter(DataType == "a440")

#do correlations of the individual months vs. summer mean for secchi data:
SD_June_cor <- cor(SD.test.data$`06`, SD.test.data$`0601_0930`)

SD_June <- ggplot(data = SD.test.data, aes(x = `06`, y = `0601_0930`)) +
  geom_point()+
  annotate("text", x = 2, y = 7.5, label = paste("r^2 = ",round(SD_June_cor,3)), size = 5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "Secchi June vs. Summer Summary", y = "Summer Summary", x = "June") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  ) 


SD_July_cor <- cor(SD.test.data$`07`, SD.test.data$`0601_0930`)

SD_July <- ggplot(data = SD.test.data, aes(x = `07`, y = `0601_0930`)) +
  geom_point()+
  annotate("text", x = 2, y = 7.5, label = paste("r^2 = ",round(SD_July_cor,3)), size = 5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "Secchi July vs. Summer Summary", y = "Summer Summary", x = "July") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  ) 


SD_Aug_cor <- cor(SD.test.data$`08`, SD.test.data$`0601_0930`)

SD_Aug <- ggplot(data = SD.test.data, aes(x = `08`, y = `0601_0930`)) +
  geom_point()+
  annotate("text", x = 2, y = 7.5, label = paste("r^2 = ",round(SD_Aug_cor,3)), size = 5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "Secchi August vs. Summer Summary", y = "Summer Summary", x = "August") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  ) 


SD_Sept_cor <- cor(SD.test.data$`09`, SD.test.data$`0601_0930`)

SD_Sept <- ggplot(data = SD.test.data, aes(x = `09`, y = `0601_0930`)) +
  geom_point()+
  annotate("text", x = 2, y = 7.5, label = paste("r^2 = ",round(SD_Sept_cor,3)), size = 5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "Secchi September vs. Summer Summary", y = "Summer Summary", x = "September") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  ) 



#do correlations of the individual months vs. summer mean for chl-a data:

CL_June_cor <- cor(CL.test.data$`06`, CL.test.data$`0601_0930`)

CL_June <- ggplot(data = CL.test.data, aes(x = `06`, y = `0601_0930`)) +
  geom_point()+
  annotate("text", x = 50, y = 450, label = paste("r^2 = ",round(CL_June_cor,3)), size = 5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "Chlorophyll-a June vs. Summer Summary", y = "Summer Summary", x = "June") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  ) 



CL_July_cor <- cor(CL.test.data$`07`, CL.test.data$`0601_0930`)

CL_July <- ggplot(data = CL.test.data, aes(x = `07`, y = `0601_0930`)) +
  geom_point()+
  annotate("text", x = 50, y = 450, label = paste("r^2 = ",round(CL_July_cor,3)), size = 5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "Chlorophyll-a July vs. Summer Summary", y = "Summer Summary", x = "July") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  ) 



CL_Aug_cor <- cor(CL.test.data$`08`, CL.test.data$`0601_0930`)

CL_Aug <- ggplot(data = CL.test.data, aes(x = `08`, y = `0601_0930`)) +
  geom_point()+
  annotate("text", x = 100, y = 450, label = paste("r^2 = ",round(CL_Aug_cor,3)), size = 5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "Chlorophyll-a August vs. Summer Summary", y = "Summer Summary", x = "August") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  ) 



CL_Sept_cor <- cor(CL.test.data$`09`, CL.test.data$`0601_0930`)

CL_Sept <- ggplot(data = CL.test.data, aes(x = `09`, y = `0601_0930`)) +
  geom_point()+
  annotate("text", x = 100, y = 450, label = paste("r^2 = ",round(CL_Sept_cor,3)), size = 5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "Chlorophyll-a September vs. Summer Summary", y = "Summer Summary", x = "September") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  ) 



#do correlations of the individual months vs. summer mean for CDOM data:

CDOM_June_cor <- cor(CDOM.test.data$`06`, CDOM.test.data$`0601_0930`)

CDOM_June <- ggplot(data = CDOM.test.data, aes(x = `06`, y = `0601_0930`)) +
  geom_point()+
  annotate("text", x = 5, y = 27, label = paste("r^2 = ",round(CDOM_June_cor,3)), size = 5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "CDOM June vs. Summer Summary", y = "Summer Summary", x = "June") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  ) 



CDOM_July_cor <- cor(CDOM.test.data$`07`, CDOM.test.data$`0601_0930`)

CDOM_July <- ggplot(data = CDOM.test.data, aes(x = `07`, y = `0601_0930`)) +
  geom_point()+
  annotate("text", x = 5, y = 27, label = paste("r^2 = ",round(CDOM_July_cor,3)), size = 5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "CDOM July vs. Summer Summary", y = "Summer Summary", x = "July") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  ) 


CDOM_Aug_cor <- cor(CDOM.test.data$`08`, CDOM.test.data$`0601_0930`)

CDOM_Aug <- ggplot(data = CDOM.test.data, aes(x = `08`, y = `0601_0930`)) +
  geom_point()+
  annotate("text", x = 5, y = 27, label = paste("r^2 = ",round(CDOM_Aug_cor,3)), size = 5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "CDOM August vs. Summer Summary", y = "Summer Summary", x = "August") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  ) 


CDOM_Sept_cor <- cor(CDOM.test.data$`09`, CDOM.test.data$`0601_0930`)

CDOM_Sept <- ggplot(data = CDOM.test.data, aes(x = `09`, y = `0601_0930`)) +
  geom_point()+
  annotate("text", x = 5, y = 27, label = paste("r^2 = ",round(CDOM_Sept_cor,3)), size = 5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "CDOM September vs. Summer Summary", y = "Summer Summary", x = "September") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  ) 



#let's make a summary figure for each variable that I can export and easily view:

#tiff("Remote_Sensed_Clarity_Month_vs_Avg.tiff", width = 10, height = 10, units = "in", res = 300)
grid.arrange(SD_June,
             SD_July,
             SD_Aug,
             SD_Sept,
             ncol=2, nrow=2
)
#dev.off()

#tiff("Remote_Sensed_Chla_Month_vs_Avg.tiff", width = 10, height = 10, units = "in", res = 300)
grid.arrange(CL_June,
             CL_July,
             CL_Aug,
             CL_Sept,
             ncol=2, nrow=2
)
#dev.off()

#tiff("Remote_Sensed_CDOM_Month_vs_Avg.tiff", width = 10, height = 10, units = "in", res = 300)
grid.arrange(CDOM_June,
             CDOM_July,
             CDOM_Aug,
             CDOM_Sept,
             ncol=2, nrow=2
)
#dev.off()
  

#KEEPING THIS CODE IN CASE I NEED IT LATER:
#mash datatype and month back together
mutate(DataMonth = paste(DataType, Month, sep = "_"))



#REMOTE SENSED CHLOROPYLL-A DATA ------------------------------------------------------------------------------------------






#LAKE AREA DATA - also brings coordinates  ----------------------------------------------------------

#first need to import lake area data because they took it out of the fish database :(
lake.area <- read.csv("Data/Input/Copy of Copy of mn_lake_list.csv")

#isolate and rename columns from area data
lake.area.select <- lake.area %>% 
  select(LAKE_NAME, DOW_NBR_PRIMARY, LAKE_AREA_GIS_ACRES, LAKE_CENTER_LAT_DD5, LAKE_CENTER_LONG_DD5) %>% 
  rename(parentdow = DOW_NBR_PRIMARY, lake.area.acres = LAKE_AREA_GIS_ACRES, lake.center.lat.dds = LAKE_CENTER_LAT_DD5, lake.center.long.dds = LAKE_CENTER_LONG_DD5)


#join to the rest of the preliminary data
Data_c <- left_join(Data_b, lake.area.select, by = "parentdow")

#remove some duplicates created with the parentdows
Data_c <- Data_c %>% 
  filter(LAKE_NAME != "Piepenburg Park Pond" & LAKE_NAME != "Lake of the Woods(4 Mi B)")

#remove redundant lake name column
Data_c <- Data_c %>% 
  select(-LAKE_NAME)


#remove unneeded intermediate data frames to keep environment clean
rm(lake.area,
   lake.area.select
)








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


