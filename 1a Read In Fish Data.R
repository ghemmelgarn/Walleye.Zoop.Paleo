#this script downloads fish data and creates a "raw" fish data csv file called FishData.csv
#determines which surveys have gear and sufficient effort to use the fish data
#this also pulls out the nhdid, coordinates, and area for all the exact match lakes - at end of script


library(lubridate)
library(arrow)
library(data.table)
library(tidyverse)

#this finds the minnesota arrow file that contains all the Minnesota fish data - update when it's time for the real analysis
all_data <- open_dataset("G:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Fish Survey Data/Parquet files/hive_update", partitioning = c("state"))
#partitioning tells arrow that the parquet are separated by state

#the elf returns with info on the data similar to a str function
glimpse(all_data)

#filter out just Minnesota data
mn_data <- all_data %>%
  filter(state == "Minnesota")
glimpse(mn_data)

#compute function within arrow tells you how big the subset is and what it would be returning - good idea to use first to see if it will crash compu
#collect function actually brings the data into R 

#remove all_data file to keep environment clean
rm(all_data)

#filtering join takes a filter table and tells R to filter out the records that match the table
#I want to only include the lake/years that I have matching fish/zoop data for - my filter table will contain these years

#DATA STRUCTURE NOTES
#an effort is unit of sampling that includes all sampling done with one sampling method on one lake within a year- this only applies to MN
#Minnesota doesn't report subefforts

#for dates, year column is sufficient for this project

#species are reported as common names, all lowercase, with underscores for spaces

#pay attention to preserving both a metric and it's corresponding unit column

#flag column tells you if there is some kind of error or concern with the data
#notes could give you more information 

#ind_fish_ident is agency reported
#obs_id gives each row(usually a fish but could be nothing caught) a number within this dataset by state

#unless grouped, n is number of fish that the criteria applies to

# Minnesota ---------------------------------------------------------------

#within each sampling method, this will a return a count of the number of fish caught
#includes the sampling methods at the three differnt levels of detail
#collect brings the information from the count into R
#arranges puts it in order by a certain group
#print here tells it to give me all the rows instead of just the first 10
mn_data %>% 
  group_by(sampling_method_simple, sampling_method, sampling_method_2) %>%
  count() %>% collect() %>% arrange(sampling_method_simple) %>% print(n=nrow(.))

#same as above, but will return how many times a sampling method was used instead of number of fish it caught
mn_data %>% 
  distinct(sampling_method_simple, total_effort_ident) %>%
  group_by(sampling_method_simple) %>% 
  count() %>% collect() %>% arrange(sampling_method_simple) %>% print(n=nrow(.))

#looking at all MN survey types
mn_data %>% 
  distinct(survey_type, survey_type_2) %>%
  collect() %>% arrange(survey_type) %>% print(n=nrow(.))

#remove unnecessary columns
incl.table <- read.csv("Data/Input/Fish Inclusion Table.csv")%>%
  select(-ZoopYear)%>%
  select(-ZoopTows)%>%
  select(-ZoopMonths)%>%
  select(-FishGear)%>%
  #select(-Chosen.for.analysis.)%>%
  select(-Notes)%>%
  select(-parentdow.zoop.year)

#filter only rows with exact fish/zoop year match
incl.table <-incl.table %>% 
  filter(Match == "Exact")

#rename dow and year columns to match fish database
#leading zeros not in either data frame so that should be okay
incl.table <- incl.table %>%   
  rename(lake_id = DOW)%>%
  rename(year = FishYear)
  
incl.table <- incl.table %>% 
  mutate(lake_id = as.character(lake_id))%>% 
  mutate(year = as.double(year))

# #save this inclusion table for use with water quality data
# #this title means it is cleaned in R and contains only exact fish/zoop matches
# write_csv(incl.table, "Data/Output/Inclusion.Table.Clean.Exact.csv")

#adjust some DOW / lake_id values in inclusion table to make it match fish database
#fix Belle lake_id: It is 47004900 in the inclusion table but 47004901 in the fish database
incl.table$lake_id <- replace(incl.table$lake_id, incl.table$lake_id == 47004900, 47004901)
#remove duplicate rows for Hill lake (north and south are combined for fish)
incl.table <- filter(incl.table, LakeName != "Hill (south)")
#fix hill lake_id: zoops separated by basin, fish are not
incl.table$lake_id <- replace(incl.table$lake_id, incl.table$lake_id == 1014201, 1014200)
#fix red lake - need to specify data is from upper red basin
incl.table$lake_id <- replace(incl.table$lake_id, incl.table$lake_id == 4003500, 4003501)

#THIS NEEDS MORE WORK TO GET IT TO MATCH MY LAKE-FINDER SEARCH RESULTS

#filter out just the data that matches my selected lake/years, represented by the inclusion table

#first choose the surveys that I want, We will go back for the fish later
#filter out just the gear I want: standard, shallow, and deep gillnets
#included one special assessment for White Iron lake - I vetted this and ok to use, was a standard assessment combined with other things too I am just using standard assessment part
#targeted surveys are acceptable for the specific lakes I specify below that I individually investigated
#other specific lake things to clean up:
  #47004600 had a targeted survey in 2020 that I want, a standard survey in 2019 that I want, and a targeted survey in 2019 that I don't want, so I specified the year I want targeted surveys from this lake
#right join - will preserve everything in inclusion table but only matching rows from the mn_data
#joining by lake Id and year - ALL ROWS THAT ARE IN BOTH TABLES NEED TO BE LISTED HERE OR YOU WILL GET .x and .y columns
#takes the distinct columns from the mn_data
#glimpse at end shows me if it did what I wanted
good.surveys <- mn_data %>% 
  filter((sampling_method == "gill_net_standard" | 
            sampling_method == "gill_net_stratified_deep" | 
            sampling_method =="gill_net_stratified_shallow" ) & 
           (survey_type == "Standard Survey" | 
              survey_type == "HISTORICAL"| 
              survey_type == "Population Assessment"| 
              survey_type == "Re-Survey"| 
              survey_type == "Large Lake Survey"| 
              survey_type == "Initial Survey" | 
              (survey_type == "Special Assessment" & lake_id == 69000400) | 
              (survey_type == "Special Assessment" & lake_id == 69025400) | 
              (survey_type == "Targeted Survey" & lake_id == 3057600) | 
              (survey_type == "Targeted Survey" & lake_id == 11041500) | 
              (survey_type == "Targeted Survey" & lake_id == 13002700)| 
              (survey_type == "Targeted Survey" & lake_id == 18030800) | 
              (survey_type == "Targeted Survey" & lake_id == 38039300) | 
              (survey_type == "Targeted Survey" & lake_id == 47004600 & year == 2020) | 
              (survey_type == "Targeted Survey" & lake_id == 47006800) | 
              (survey_type == "Targeted Survey" & lake_id == 69025400)))  %>% 
  right_join(incl.table, by = c("lake_id", "year")) %>% 
  distinct(lake_id, 
           year, 
           total_effort_ident, 
           total_effort_1,  
           sampling_method_simple, 
           sampling_method, 
           survey_type, flag, 
           lakesize, 
           lakesize_units, 
           nhdhr_id, 
           latitude_lake_centroid, 
           longitude_lake_centroid) %>% 
  collect()
#collect actually brings data into R
#end up with more rows than the inclusion table because different gillnet types are separated
#not collecting flag anymore because it confuses things with the join to fish data later
#note that this DOES NOT INCLUDE 2023 DATA


# #use and modify this code to troubleshoot why certain lakes are and are not included
# test <- mn_data %>%
#   filter(sampling_method_simple == "gill_net" & lake_id == "69025400" & year == 2008)
# glimpse(test)
# test %>%
#   distinct(survey_type, year, total_effort_ident) %>%
#   collect() %>% arrange(survey_type) %>% print(n=nrow(.))


#surveys with gear issue have two rows, one that says gear issue and one that doesn't, so need to combine those
good.surveys.flag.summarize <- good.surveys %>%
  group_by(lake_id, year, total_effort_ident, total_effort_1, sampling_method_simple, sampling_method, survey_type, lakesize, lakesize_units) %>%
  summarize(flag = ifelse(any(flag == "gear issue", na.rm = TRUE), "gear issue", NA), .groups = 'drop')
#remove NA total effort rows and gear issue flagged rows
good.surveys.filter <- filter(good.surveys.flag.summarize, !is.na(total_effort_1) & is.na(flag))
#not doing this because there are no na values for total effort and I want to pulled out flagged nets but not entire surveys

#check that effort is sufficient for lake size, based on DNR sampling manual
#fix area for upper red, says 0 here but should be 119295 based on lakefinder
good.surveys.filter$lakesize <- replace(good.surveys.filter$lakesize, good.surveys.filter$lakesize == 0, 119295)
#fix area for Belle, says 1 here but should be 864 based on lakefinder
good.surveys.filter$lakesize <- replace(good.surveys.filter$lakesize, good.surveys.filter$lakesize == 1, 864)
#no other areas are too small to be believable, checked the next 2 smallest and they are accurate

#create lake size bin column
good.surveys.filter <- good.surveys.filter %>%
  mutate(size.bin = ifelse(lakesize < 100, "<100", ifelse(lakesize >= 100 & lakesize < 300, "100-300", ifelse(lakesize >= 300 & lakesize < 600, "300-600", ifelse(lakesize >= 600 & lakesize < 1500, "600-1500", ">1500")))))
#create column for minimum net-night effort based on size bin
good.surveys.filter <- good.surveys.filter %>%
  mutate(min.effort = ifelse(size.bin == "<100", 0, ifelse(size.bin == "100-300", 6, ifelse(size.bin == "300-600", 9, ifelse(size.bin == "600-1500", 12, 15)))))
#add effort from shallow and deep stratified surveys
combined.stratified.effort <- good.surveys.filter %>%
  group_by(lake_id, year) %>%
  summarize(total_effort_cse = sum(total_effort_1), .groups = 'drop')
#join this combined effort back to the good surveys filter, keep all good.surveys.filter rows and add in the matching cse effort
good.surveys.filter.cse <- good.surveys.filter %>% 
  left_join(combined.stratified.effort, by = c("lake_id", "year"))
#create a column that subtracts total effort from minimum required effort
good.surveys.filter.cse$effort.test <- good.surveys.filter.cse$total_effort_cse - good.surveys.filter.cse$min.effort
#create a column with the effort conclusion
good.surveys.filter.cse <- good.surveys.filter.cse %>%
  mutate(fish.effort.sufficient = ifelse (effort.test >= 0, "yes", "no"))

# #save this survey table as a .csv - this includes all surveys regardless of effort but with sufficient effort noted
# write_csv(good.surveys.filter.cse, "Data/Output/Usable_Fish_Surveys.csv")

#filter out only the surveys with good enough effort
good.surveys.effort <- filter(good.surveys.filter.cse, fish.effort.sufficient == "yes")

#trying out what happens if I allow 3 nets less than minimum to improve my total sample size
good.surveys.effort.inclusive <- filter(good.surveys.filter.cse, effort.test >= -3)

#at the end of this, I should feel confident that all of these surveys are good quality and ok to use - will filter for gear issues later

#GO BACK FOR THE FISH
fish <- mn_data %>% 
  right_join(good.surveys.effort, by = c("total_effort_ident", "total_effort_1", "lake_id")) %>% 
  collect()

#sample size testing
fish.inclusive <- mn_data %>% 
  right_join(good.surveys.effort.inclusive, by = c("total_effort_ident", "total_effort_1", "lake_id")) %>% 
  collect()

#CAN'T DO THIS BECAUSE CAN'T ID WHICH NETS ARE BAD, REMOVED ENTIRE FLAGGED SURVEYS ABOVE
# #need to filter out nets flagged with gear issues
# #isolate the flagged fish
# flagged.fish <- filter(fish, flag == "gear issue")
# #then need to adjust total effort for nets removed from analysis
# #remove any rows from the fish data flagged with a gear issue - this removes bad sub-efforts (individual nets) but keeps the rest of the survey
# #gear issue is the only flag with my data


# #save this fish table as a .csv
# write_csv(fish, "Data/Output/FishData.csv")

# #save this fish table as a .csv
# write_csv(fish.inclusive, "Data/Output/FishData.inclusive.csv")






#PULL JUST THE NHDID, COORDINATES, AND AREA DATA I WANT FOR MY LAKES, YEAR DOESN'T MATTER FOR THIS ------------------------------
nhdid.lat.long.area <- mn_data %>%
  right_join(incl.table, by = c("lake_id")) %>% 
  distinct(lake_id, nhdhr_id, latitude_lake_centroid, longitude_lake_centroid, lakesize, lakesize_units) %>% 
  collect()
#fix area for upper red, says 0 here but should be 119295 based on lakefinder
nhdid.lat.long.area$lakesize <- replace(nhdid.lat.long.area$lakesize, nhdid.lat.long.area$lakesize == 0, 119295)
#fix area for Belle, says 1 here but should be 864 based on lakefinder
nhdid.lat.long.area$lakesize <- replace(nhdid.lat.long.area$lakesize, nhdid.lat.long.area$lakesize == 1, 864)
#join back to inclusion table to match this data with all the lakes and years
lakes.lat.long.area <- left_join(incl.table, nhdid.lat.long.area, by = "lake_id")

# #write this as a .csv so I can use it to get temp data and join it to the preliminary data
# write_csv(lakes.lat.long.area, "Data/Output/Selected_Lakes_Location_Area.csv")




