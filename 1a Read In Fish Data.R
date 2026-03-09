#this script downloads fish data that match the inclusion table for pelagic analysis and creates a "raw" fish data csv file called Pelagic_Fish_Data.csv



library(lubridate)
library(arrow)
library(data.table)
library(tidyverse)
library(here)

#Find the parquet file and glimpse it--------------------------------------------------------------------------------

#this finds the minnesota arrow file that contains all the Minnesota fish data - this is what Denver updated in Nov 2025, only has MN data
mn_data <- open_dataset("G:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Fish Survey Data/Parquet files/mn_update/part-0.parquet")

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







#GET WALLEYE YOY WITH CODE FROM CHRIS ROUNDS------------------------------------------------------------------------------------------
#Grace slightly modified this to split east and west vermilion and to relabel Hill as North Hill

# Collect Fall electrofishing for walleye surveys
surveys_target <- mn_data %>% 
  filter(sampling_method == "boat_electrofishing_targeted_night") %>%
  filter(target_species == "walleye") %>%
  mutate(lubridate_date_clean = lubridate::as_date(date_total_effort_ident),
         year = year(lubridate_date_clean), month = lubridate::month(lubridate_date_clean)) %>%
  dplyr::filter(total_effort_1 > 0.5) %>%
  dplyr::filter(is.na(flag)) %>%
  collect()


#split east and west vermilion by site number - This is different pre-2011 so split into two date ranges here
surveys_target_verm <- surveys_target %>% 
  mutate(lake_name = ifelse(lake_name == "Vermilion" & year >= 2011 & (site_id == "EW8" | site_id == "EW9" | site_id == "EW12"), "West Vermilion", 
                            ifelse(lake_name == "Vermilion" & year >= 2011 & (site_id == "EW1" | site_id == "EW10" | site_id == "EW3" | site_id == "EW4" | site_id == "EW11" | site_id == "EW6"), "East Vermilion", 
                                   ifelse(lake_name == "Vermilion" & year < 2011 & (site_id == "EW7" | site_id == "EW8" | site_id == "EW9"), "West Vermilion",
                                          ifelse(lake_name == "Vermilion" & year < 2011 & (site_id == "EW1" | site_id == "EW2" | site_id == "EW3" | site_id == "EW4" | site_id == "EW5" | site_id == "EW6"), "East Vermilion", lake_name)))),
         lake_id = ifelse(lake_name == "West Vermilion", 69037802, 
                                 ifelse(lake_name == "East Vermilion", 69037801, lake_id)))

surveys_target_verm %>% group_by(flag) %>% count()

non_zero <- surveys_target_verm %>%
  dplyr::filter(species_1 == "walleye") %>%
  dplyr::filter(age_class == "young_of_year") %>%
  group_by(lake_id, lake_name, nhdhr_id, total_effort_ident, water_temp,
           date_total_effort_ident, 
           year, total_effort_1) %>%
  summarize(catch = n()) %>% mutate(CPUE = catch/total_effort_1)

zero_surveys = surveys_target_verm %>%
  dplyr::filter(!total_effort_ident %in% non_zero$total_effort_ident) %>%
  group_by(lake_id, lake_name, nhdhr_id, total_effort_ident, water_temp,
           date_total_effort_ident, 
           year, total_effort_1) %>%
  summarize(catch = 0) %>% mutate(CPUE = 0) 

new_surveys_temp <- rbind(non_zero, zero_surveys) %>% 
  ungroup() %>%
  mutate(julian_day_survey = yday(date_total_effort_ident))

notes <- surveys_target_verm %>% 
  filter(total_effort_ident %in% new_surveys_temp$total_effort_ident) %>% 
  select(total_effort_ident, gear_data_notes) %>% group_by(total_effort_ident) %>%
  slice(1) %>% ungroup()

new_surveys_ew <- merge(new_surveys_temp, notes, by = "total_effort_ident")



# Collect Standard electrofishing surveys
# Collect good surveys
surveys <- mn_data %>% filter(state == "Minnesota") %>% 
  filter(sampling_method == "boat_electrofishing_special_night" | 
           sampling_method == "boat_electrofishing_standard_night") %>%
  filter(target_species == "walleye") %>%
  mutate(lubridate_date_clean = lubridate::as_date(date_total_effort_ident),
         year = year(lubridate_date_clean), month = lubridate::month(lubridate_date_clean)) %>%
  dplyr::filter(total_effort_1 > 0.5) %>%
  dplyr::filter(is.na(flag)) %>%
  collect()

#No vermilion surveys here

# Get the surveys where fish were caught (not zeroes)
non_zero <- surveys %>%
  dplyr::filter(species_1 == "walleye") %>%
  dplyr::filter(age_class == "young_of_year") %>%
  group_by(lake_id, lake_name, nhdhr_id, total_effort_ident, water_temp,
           date_total_effort_ident, 
           year, total_effort_1) %>%
  summarize(catch = n()) %>% mutate(CPUE = catch/total_effort_1)

# Get the surveys where no fish were caught (only zeroes)
zero_surveys = surveys %>%
  dplyr::filter(!total_effort_ident %in% non_zero$total_effort_ident) %>%
  group_by(lake_id, lake_name, nhdhr_id, total_effort_ident, water_temp,
           date_total_effort_ident, year, total_effort_1) %>%
  summarize(catch = 0) %>% mutate(CPUE = 0) 

new_surveys_temp <- rbind(non_zero, zero_surveys) %>% 
  ungroup() %>%
  mutate(julian_day_survey = yday(date_total_effort_ident))

notes <- surveys %>% 
  filter(total_effort_ident %in% new_surveys_temp$total_effort_ident) %>% 
  select(total_effort_ident, gear_data_notes) %>% group_by(total_effort_ident) %>%
  slice(1) %>% ungroup()

new_surveys_sef_ef <- merge(new_surveys_temp, notes, by = "total_effort_ident")


# Combine all three
new_surveys <- rbind(new_surveys_ew, new_surveys_sef_ef) %>%
  # Survey has to be later than Aug - 1st
  filter(julian_day_survey > 213) %>%
  # Survey has to be done at night
  filter(!str_detect(gear_data_notes, "DAYLIGHT_SAMPLING:Y")) 

# A couple surveys sampled twice a year
new_surveys %>% group_by(lake_id, year) %>% count() %>% filter(n >1)

#GRACE's NOTE: The only lake I need that has an issue here is Vermilion which was sampled twice in 2016

# could maybe filter more of these out
all_weird <- new_surveys %>% group_by(lake_id, year) %>% count() %>% filter(n >1)

example <- surveys_target_verm %>% filter(lake_id == "31053800") %>% filter(year == 1996)
unique(example$total_effort_ident)

#Grace investigates Vermilion 2016
verm <- surveys_target_verm %>% filter(lake_id == "69037801" | lake_id == "69037802") %>% filter(year == 2016)
unique(verm$total_effort_ident)
#All the walleye in 38232 are not YOY, so I can just get rid of that survey

bad_efforts <- c("21882", "21883", "21884", "21885", "21886", "38232")

new_surveys2 <- new_surveys %>% 
  filter(!total_effort_ident %in% bad_efforts) %>% 
  #I know from Hill lake report from Rich Bruesewitz that fall electrofishing is only in North Hill - so just need to rename this and update DOW
  mutate(lake_name = ifelse(lake_name == "Hill", "Hill (north)", lake_name),
         lake_id = ifelse(lake_id == 1014200, 1014201, lake_id))

#save as csv
#write.csv(new_surveys2, file = "Data/Output/Walleye_YOY_Fall_Electrofishing_2026_03_09.csv", row.names = FALSE)





