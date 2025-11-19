library(lubridate)
library(arrow)
library(data.table)
library(tidyverse)


#SETP 1: Get a list of fish surveys from the fish database:

#this finds the minnesota arrow file that contains all the Minnesota fish data - this is what Denver updated in Nov 2025, only has MN data
mn_data <- open_dataset("E:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Fish Survey Data/Parquet files/mn_update/part-0.parquet")

#the elf returns with info on the data similar to a str function
glimpse(mn_data)

#NOTES ABOUT THE FISH DATABASE:
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


#first find the surveys that are good
#filter out just the gear I want: standard, shallow, and deep gillnets
#included one special assessment for White Iron lake - I vetted this and ok to use, was a standard assessment combined with other things too I am just using standard assessment part
#targeted surveys are acceptable for the specific lakes I specify below that I individually investigated
#other specific lake things to clean up:
#47004600 had a targeted survey in 2020 that I want, a standard survey in 2019 that I want, and a targeted survey in 2019 that I don't want, so I specified the year I want targeted surveys from this lake
#takes the distinct columns from the mn_data
#glimpse at end shows me if it did what I wanted

good.fish.surveys <- mn_data %>%
  filter((sampling_method == "gill_net_standard" |
            sampling_method == "gill_net_stratified_deep" |
            sampling_method =="gill_net_stratified_shallow" ) &
           (survey_type == "Standard Survey" |
              survey_type == "HISTORICAL"|
              survey_type == "Population Assessment"|
              survey_type == "Re-Survey"|
              survey_type == "Large Lake Survey"|
              survey_type == "Initial Survey" |
              survey_type == "Special Assessment" | #will individually investigate if I can use these special assessments IF they match to zoop data
              survey_type == "Targeted Survey")) %>% #will individually investigate if I can use these targeted surveys IF they match to zoop data
  distinct(lake_id,
           lake_name,
           year,
           total_effort_ident,
           total_effort_1,
           sampling_method_simple,
           sampling_method,
           survey_type,
           nhdhr_id,
           flag) %>%
  collect()
#collect actually brings data into R
#here I am creating a list of surveys but if you specify in "distinct" the unique fish IDs, species, and whatever info you want about them it will bring in fish-level data

#fix Crane lake nhdhr_id (wrong in fish database - Denver is fixing it but I will do this for now) - need this to join to LAGOS data
good.fish.surveys <- good.fish.surveys %>% 
  mutate(nhdhr_id = ifelse(nhdhr_id == "nhdhr_{E940A362-4076-4895-A23F-1B8CCC905DEE}", "nhdhr_105953135", nhdhr_id))

#let's dig into the flags: ACTUALLY ASK DENVER FOR HELP WITH THIS TOMORROW
fish.flags <- data.frame(unique(good.fish.surveys$flag))