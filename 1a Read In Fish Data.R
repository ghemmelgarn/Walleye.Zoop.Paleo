#this script downloads fish data that match the inclusion table for pelagic analysis and creates a "raw" fish data csv file called Pelagic_Fish_Data.csv



library(lubridate)
library(arrow)
library(data.table)
library(tidyverse)
library(here)

#Find the parquet file and glimpse it--------------------------------------------------------------------------------

#this finds the minnesota arrow file that contains all the Minnesota fish data - this is what Denver updated in Nov 2025, only has MN data
mn_data <- open_dataset("E:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Fish Survey Data/Parquet files/mn_update/part-0.parquet")

#the elf returns with info on the data similar to a str function
glimpse(mn_data)



#Data structure notes---------------------------------------------------------------------------

#compute function within arrow tells you how big the subset is and what it would be returning - good idea to use first to see if it will crash compu
#collect function actually brings the data into R 

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





# Minnesota data exploration---------------------------------------------------------------

#within each sampling method, this will a return a count of the number of fish caught
#includes the sampling methods at the three different levels of detail
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
  distinct(survey_type) %>%
  collect() %>% arrange(survey_type) %>% print(n=nrow(.))






#GO GET THE FISH-----------------------------------------------------------------------------------------------

#read in inclusion table
inc.table <- read.csv("Data/Input/Fish_Survey_Pelagic_Inclusion_Table.csv")
#select just the total effort ident column
inc.table.tei <- inc.table %>% 
  select(total_effort_ident)

inc.table.tei$total_effort_ident <- as.character(inc.table.tei$total_effort_ident)

fish <- mn_data %>% 
  right_join(inc.table.tei, by = c("total_effort_ident")) %>% 
  collect()


# #save this fish table as a .csv
# write.csv(fish, "Data/Output/Pelagic_Fish_Data.csv")








