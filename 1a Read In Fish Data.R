#this script downloads fish data and creates a "raw" fish data csv file called FishData.csv

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

#CLEAN UP HOW YOU BRING IN INCLUSION TABLE
#remove unnecessary columns
incl.table <- read.csv("Data/Input/Fish Inclusion Table.csv")%>%
  select(-ZoopYear)%>%
  select(-ZoopTows)%>%
  select(-ZoopMonths)%>%
  select(-FishGear)%>%
  select(-Match)%>%
  select(-Notes)%>%
  select(-parentdow.zoop.year)

#filter only rows chosen for analysis, get rid of maybe lakes
incl.table <-incl.table %>% 
  filter(Chosen.for.analysis. == "Yes")

#rename dow and year columns to match fish database
#leading zeros not in either data frame so that should be okay
incl.table <- incl.table %>%   
  rename(lake_id = DOW)%>%
  rename(year = FishYear)
  
incl.table <- incl.table %>% 
  mutate(lake_id = as.character(lake_id))%>% 
  mutate(year = as.double(year))

#THIS NEEDS MORE WORK TO GET IT TO MATCH MY LAKE-FINDER SEARCH RESULTS

#filter out just the data that matches my selected lake/years, represented by the inclusion table

#first choose the surveys that I want, We will go back for the fish later
#filter out just the gear I want
#right join - will preserve everything in inclusion table but only matching rows from the mn_data
#joining by lake Id and year - ALL ROWS THAT ARE IN BOTH TABLES NEED TO BE LISTED HERE OR YOU WILL GET .x and .y columns
#takes the distinct columns from the mn_data
#glimpse at end shows me if it did what I wanted
good.surveys <- mn_data %>% 
  filter(sampling_method_simple == "gill_net" & (survey_type == "Standard Survey" | survey_type == "Targeted Survey" | survey_type == "HISTORICAL"| survey_type == "Population Assessment"| survey_type == "Re-Survey"| survey_type == "Large Lake Survey"| survey_type == "Initial Survey")) %>% 
  right_join(incl.table, by = c("lake_id", "year")) %>% 
  distinct(lake_id, year, total_effort_ident, total_effort_1, flag, sampling_method_simple, survey_type) %>% 
  collect()
#collect actually brings data into R

#use this to troubleshoot why certain lakes are and are not included
test <- mn_data %>% 
  filter(sampling_method_simple == "gill_net" & lake_id == "6000200")
glimpse(test)
test %>% 
  distinct(survey_type, year) %>%
  collect() %>% arrange(survey_type) %>% print(n=nrow(.))

#remove NA rows and flagged rows
good.surveys.f <- filter(good.surveys, !is.na(total_effort_1) & is.na(flag))

#at the end of this, I should feel confident that all of these surveys are good quality and ok to use

#GO BACK FOR THE FISH
fish <- mn_data %>% 
  right_join(good.surveys.f, by = c("total_effort_ident", "total_effort_1", "lake_id")) %>% 
  collect()

  
#save this fish table as a cvs
write_csv(fish, "Data/Output/FishData.csv")

















