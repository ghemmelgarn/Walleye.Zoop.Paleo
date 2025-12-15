#this script downloads fish data and calculates walleye CPUE for our long core lakes to send to Erin


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


#get just the good survey types with gill nets only, including historical surveys but no special or targeted
#only getting surveys for our long core lakes (excluding Red Lake here - I have separate data for that)
core.fish.surveys <- mn_data %>%
  filter((sampling_method == "gill_net_standard" |
            sampling_method == "gill_net_stratified_deep" |
            sampling_method =="gill_net_stratified_shallow" ) &
           (survey_type == "Standard Survey" |
              survey_type == "Population Assessment"|
              survey_type == "Re-Survey"|
              survey_type == "Large Lake Survey"|
              survey_type == "Initial Survey" |
              survey_type == "HISTORICAL") &
           (lake_id == "21005700" | 
              lake_id == "10000600" | 
              lake_id == "11041300" | 
              lake_id == "82016700" | 
              lake_id == "34007900" | 
              lake_id == "11020300" | 
              lake_id == "4003000" | 
              lake_id == "69081000" | 
              lake_id == "48000200" | 
              lake_id == "11014700" | 
              lake_id == "18030800" | 
              lake_id == "69025400" | 
              lake_id == "62005700" | 
              lake_id == "69037800" )) %>% 
  distinct(lake_id,
           lake_name,
           year,
           month, #this is generated from "date_total_effort_ident" so it gives me the correct month that the fishing actually started with my specified gear 
           total_effort_ident,
           total_effort_1,
           sampling_method_simple,
           sampling_method,
           survey_type,
           nhdhr_id,
           flag) %>%
  collect()


#Check that I got the lakes I want
table(core.fish.surveys$lake_name)
#I did

#Check for flags
table(core.fish.surveys$flag)

  
#mark rows/surveys that have survey-level problems (flags) that mean I can't use any of the data (got this code from Denver)
fish.surveys.marked <- core.fish.surveys %>%
  mutate(cpue_invalid = case_when(str_detect(flag, "Do not use in CPUE") |
                                    str_detect(flag, "Not representative sampling") |
                                    str_detect(flag, "Questionable total effort") |
                                    str_detect(flag, "database issue") |
                                    str_detect(flag, "effort value error") |
                                    str_detect(flag, "gear issue") |
                                    str_detect(flag, "high effort") |
                                    str_detect(flag, "likely") |
                                    str_detect(flag, "very high catch") ~ "y",
                                  TRUE ~ "n"))
#filter out these bad surveys
fish.surveys.clean <- fish.surveys.marked %>% 
  filter(cpue_invalid == "n")

#select just the effort ident for the join
fish.surveys.join <- fish.surveys.clean %>% 
  select(total_effort_ident)

#go back for the fish
fish <- mn_data %>% 
  right_join(fish.surveys.join, by = c("total_effort_ident")) %>% 
  collect()

#separate east and west vermilion
#"Gillnets 1-12 are in East Vermilion (see attached map) and 13-20 are in West Vermilion. The same gill net numbers are used every year."
Verm <- fish %>% 
  filter(lake_name == "Vermilion")
table(Verm$site_id, Verm$survey_type, useNA = "ifany")
table(Verm$site_id, Verm$year, useNA = "ifany")
#Looks like in the historical surveys you can't split east/west

#Identifies the nets I can split up and also adjusts the total effort
fish.split.verm <- fish %>% 
  mutate(lake_name = ifelse(lake_name == "Vermilion" & is.na(site_id), "Vermilion",
                            ifelse(lake_name == "Vermilion" & (site_id == "GN1" | site_id == "GN2" | site_id == "GN3" | site_id == "GN4" | 
                                                          site_id == "GN5" | site_id == "GN6" | site_id == "GN7" | site_id == "GN8" | 
                                                          site_id == "GN9" | site_id == "GN10" | site_id == "GN11" | site_id == "GN12"),
                                    "East Vermilion",
                                          ifelse(lake_name == "Vermilion" & (site_id == "GN13" | site_id == "GN14" | site_id == "GN15" | site_id == "GN16" | 
                                                                 site_id == "GN17" | site_id == "GN18" | site_id == "GN19" | site_id == "GN20"),
                                           "West Vermilion", 
                                            fish$lake_name)))) %>% 
  mutate(total_effort_1 = ifelse(lake_name == "Vermilion" & is.na(site_id), fish$total_effort_1,
                            ifelse(lake_name == "East Vermilion", 12,
                                   ifelse(lake_name == "West Vermilion", 8, fish$total_effort_1))))
  
# #test if it worked
# table(fish.split.verm$lake_name)
# 
# Verm2 <- fish.split.verm %>% 
#   filter(lake_name == "Vermilion" | lake_name == "East Vermilion" | lake_name == "West Vermilion")
# table(Verm2$total_effort_1, Verm2$year)
# #looks good

#calculate walleye CPUE
DNR.WAE.cpue <- fish.split.verm %>% 
  group_by(lake_name, total_effort_ident, year, total_effort_1) %>% 
  summarize(WAE.count = sum(species_1 == "walleye"), 
            WAE.CPUE = WAE.count/total_effort_1,
            .groups = 'drop') %>% 
  unique()



#need to add Red Lake fish data (a csv that I digitized from a pdf of their fisheries report)
#Using this red lake data even for upper red instead of DNR data because this combines tribal + DNR surveys
Red.WAE.CPUE<- read.csv("Data/Input/Red_Lake_Walleye_CPUE.csv")
#Note that this data is a little wonky because I calculated the counts and they calculated CPUE and it usually matches up but not always
#Just going with their CPUE calcs

#add the red rows
all.WAE.CPUE <- rbind(DNR.WAE.cpue, Red.WAE.CPUE)

#export csv
write_csv(all.WAE.CPUE, "Data/Output/Core_Lakes_Walleye_CPUE.csv")


#--------------------------------------------------------------------------------------------------------------


#OLD CODE: TRIED AND FAILED TO CALCULATE RED LAKE CPUE HERE BUT CAN'T BECAUSE NO EFFORT DATA
#SAVED CODE BELOW IN CASE IT IS HELPFUL WHEN DEALING WITH THIS DATA IN THE FUTURE


# Red.fish <- read.csv("Data/Input/Red Lakes walleye gill net data.csv")
# 
# #Clean up lake names
# Red.fish.names <- Red.fish %>% 
#   mutate(LakeBasin = ifelse(Red.fish$LakeBasin == "lower red" | Red.fish$LakeBasin == "Lower Red" | Red.fish$LakeBasin == "Lower_Red", "Red (Lower Red)",
#                             ifelse(Red.fish$LakeBasin == "upper red" | Red.fish$LakeBasin == "Upper Red" | Red.fish$LakeBasin == "Upper_Red", "Red (Upper Red) combo", Red.fish$LakeBasin))
#   )
# 
# #calculate effort per lake-year
# #each year and NetIndex combo should be one net - I emailed to confirm this on 12/15/25
# #Count the walleye per net
# fish.count <- Red.fish.names %>% 
#   group_by(Year, LakeBasin, NetIndex) %>% 
#   tally(name = "Fish.Count") %>% 
#   ungroup()
# 
# #Count the nets per lake-year
# net.count <- fish.count %>% 
#   group_by(Year, LakeBasin) %>% 
#   tally(name = "Net.Count") %>% 
#   ungroup()
# 
# #Count the walleye per lake-year
# WAE.red.lakeyear <- fish.count %>% 
#   group_by(Year, LakeBasin) %>% 
#   summarize(WAE.count = sum(Fish.Count)) %>%  
#   ungroup()
# 
# #join the net and fish counts
# Red.all.counts <- full_join(WAE.red.lakeyear, net.count, by = c("Year", "LakeBasin"))
# 
# #Calculate Red Lake CPUE
# Red.CPUE <- Red.all.counts %>% 
#   group_by(Year, LakeBasin, WAE.count, Net.Count) %>%
#   summarize(WAE.CPUE = WAE.count/Net.Count) %>% 
#   ungroup()
# 
# 
# 
# #Quick plot to make sure the red lake data make sense
# Red.plot <- ggplot(data = Red.CPUE, aes(y = WAE.CPUE, x = Year, color = LakeBasin))+
#   geom_line()
# Red.plot





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



















