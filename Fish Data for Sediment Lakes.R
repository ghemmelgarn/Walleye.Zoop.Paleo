#this script downloads fish data and calculates walleye CPUE for our long core lakes AND surface sediment lakes


library(lubridate)
library(arrow)
library(data.table)
library(tidyverse)
library(here)
library(ggplot2)

#Get fish from database--------------------------------------------------------------------------------

#this is commented out because already done, start in next section by reading in saved file

# #this finds the minnesota arrow file that contains all the Minnesota fish data - this is what Denver updated in Nov 2025, only has MN data
# mn_data <- open_dataset("Q:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Fish Survey Data/Parquet files/mn_update/part-0.parquet")
# 
# #the elf returns with info on the data similar to a str function
# glimpse(mn_data)
# 
# 
# #get just the good survey types with gill nets only, including historical surveys but no special or targeted
# #pulled trapnets too - need to look more closely into what type of trapnet it is if I actually use that data
# #only getting surveys for our long core and surface sed lakes (including upper Red Lake here - I have separate data for lower Red)
# #lakes where the regular search got me a most recent fish survey older than 2023 - so I search for specific surveys here:
#   #Josephine 62005700 - most recent standard survey was 2012 but there's a 2024 survey listed as targeted because standard gillnet only (no trapnet) but that's fine for this purpose
#   #Belle 47004901 - has a 2022 standard survey but a 2024 standard gillnet only survey that is even more recent
#   #White Bear 82016700 - most recent standard survey is 2018, has standard gillnet-only targeted surveys in 2020, 2022, and 2024
#   #Pelican 18030800 - most recent standard survey is 2020, has standard gillnet-only targeted surveys in 2022 and 2024
#   #Lotus 10000600 - most recent standard survey is 2021, it had another standard survey in 2025 (ok because we took the core in 2025) but that data isn't in Midfish because too recent
#   #Owasso 62005600 - most recent standard survey is 2021, has standard gillnet-only targeted survey in 2024
#   #Lake of the Woods 39000200 - most recent standard survey is 2004, Lakefinder says it should have annual surveys from 1996 to 2024 - need to investigate what is happening here
# 
# LOW.test <- mn_data %>%
#   filter(sampling_method == "gill_net_standard" & lake_id == "39000200") %>%
#   distinct(lake_id,
#            lake_name,
#            year,
#            month, #this is generated from "date_total_effort_ident" so it gives me the correct month that the fishing actually started with my specified gear
#            total_effort_ident,
#            total_effort_1,
#            sampling_method_simple,
#            sampling_method,
#            sampling_method_2,
#            survey_type,
#            nhdhr_id,
#            flag) %>%
#   collect()
# #Recent Lake of the Woods surveys were getting filtered out because they are flagged as "high effort" - adjusted flag filtering below to exempt LOW from this, they really do just sample a lot
# 
# sed.fish.surveys <- mn_data %>%
#   filter((sampling_method == "gill_net_standard" |
#             sampling_method == "gill_net_stratified_deep" |
#             sampling_method =="gill_net_stratified_shallow" |
#             (sampling_method == "trap_net" & sampling_method_2 == "trap_net; 0.75-in-mesh net; double-frame net")) & #just the standard trapnets - Denver helped me figure out which one
#            (survey_type == "Standard Survey" |
#               survey_type == "Population Assessment"|
#               survey_type == "Re-Survey"|
#               survey_type == "Large Lake Survey"|
#               survey_type == "Initial Survey" |
#               survey_type == "HISTORICAL"  |
#               ((lake_id == "62005700" | lake_id == "47004901") & survey_type == "Targeted Survey" & year == 2024)| #Josephine and Belle
#               (lake_id == "82016700" & survey_type == "Targeted Survey" & (year == 2024 | year == 2022 | year == 2020)) | #White Bear
#               (lake_id == "18030800" & survey_type == "Targeted Survey" & (year == 2024 | year == 2022)) | #Pelican
#               (lake_id == "62005600" & survey_type == "Targeted Survey" & year == 2024)) & #Owasso
#               (lake_id == "10000600" |
#               lake_id == "11041300" |
#               lake_id == "82016700" |
#               lake_id == "34007900" |
#               lake_id == "11020300" |
#               lake_id == "4003000" |
#               lake_id == "69081000" |
#               lake_id == "48000200" |
#               lake_id == "11014700" |
#               lake_id == "18030800" |
#               lake_id == "69025400" |
#               lake_id == "62005700" |
#               lake_id == "69037800" |
#               lake_id == "1009300" |
#               lake_id == "3035900" |
#               lake_id == "3038100" |
#               lake_id == "3047500" |
#               lake_id == "3057600" |
#               lake_id == "21005100" |
#               lake_id == "27003100" |
#               lake_id == "27003900" |
#               lake_id == "27010400" |
#               lake_id == "27013700" |
#               lake_id == "39000200" |
#               lake_id == "47004901" |
#               lake_id == "58006700" |
#               lake_id == "62005600" |
#               lake_id == "62008300" |
#               lake_id == "69061700" |
#               lake_id == "69069300" |
#               lake_id == "69069400" |
#               lake_id == "69084500" |
#               lake_id == "4003501" | 
#               lake_id == "21005700")) %>%
#   distinct(lake_id,
#            lake_name,
#            year,
#            month, #this is generated from "date_total_effort_ident" so it gives me the correct month that the fishing actually started with my specified gear
#            total_effort_ident,
#            total_effort_1,
#            sampling_method_simple,
#            sampling_method,
#            sampling_method_2,
#            survey_type,
#            nhdhr_id,
#            flag) %>%
#   collect()
# 
# 
# #Check that I got the lakes I want
# table(sed.fish.surveys$lake_name)
# #I did
# 
# #Check for flags
# table(sed.fish.surveys$flag)
# 
# 
# #mark rows/surveys that have survey-level problems (flags) that mean I can't use any of the data (got this code from Denver)
# fish.surveys.marked <- sed.fish.surveys %>%
#   mutate(cpue_invalid = case_when(str_detect(flag, "Do not use in CPUE") |
#                                     str_detect(flag, "Not representative sampling") |
#                                     str_detect(flag, "Questionable total effort") |
#                                     str_detect(flag, "database issue") |
#                                     str_detect(flag, "effort value error") |
#                                     str_detect(flag, "gear issue") |
#                                     (str_detect(flag, "high effort") & lake_id != "39000200") | #exempts lake of the woods because all the recent surveys say "high effort" - they really do sample a lot
#                                     str_detect(flag, "likely") |
#                                     str_detect(flag, "very high catch") ~ "y",
#                                   TRUE ~ "n"))
# #filter out these bad surveys
# fish.surveys.clean <- fish.surveys.marked %>%
#   filter(cpue_invalid == "n")
# 
# #Check for flags again
# table(fish.surveys.clean$flag)
# #only length issues remaining, so filtering worked
# 
# #select just the effort ident for the join
# fish.surveys.join <- fish.surveys.clean %>%
#   select(total_effort_ident)
# 
# #go back for the fish
# fish <- mn_data %>%
#   right_join(fish.surveys.join, by = c("total_effort_ident")) %>%
#   collect()
# 
# #separate east and west vermilion
# #"Gillnets 1-12 are in East Vermilion (see attached map) and 13-20 are in West Vermilion. The same gill net numbers are used every year."
# Verm <- fish %>%
#   filter(lake_name == "Vermilion")
# table(Verm$site_id, Verm$survey_type, useNA = "ifany")
# table(Verm$site_id, Verm$year, useNA = "ifany")
# #Looks like in the historical surveys you can't split east/west
# 
# #Identifies the nets I can split up and also adjusts the total effort
# fish.split.verm <- fish %>%
#   mutate(lake_name = ifelse(lake_name == "Vermilion" & is.na(site_id), "Vermilion",
#                             ifelse(lake_name == "Vermilion" & (site_id == "GN1" | site_id == "GN2" | site_id == "GN3" | site_id == "GN4" |
#                                                           site_id == "GN5" | site_id == "GN6" | site_id == "GN7" | site_id == "GN8" |
#                                                           site_id == "GN9" | site_id == "GN10" | site_id == "GN11" | site_id == "GN12"),
#                                     "East Vermilion",
#                                           ifelse(lake_name == "Vermilion" & (site_id == "GN13" | site_id == "GN14" | site_id == "GN15" | site_id == "GN16" |
#                                                                  site_id == "GN17" | site_id == "GN18" | site_id == "GN19" | site_id == "GN20"),
#                                            "West Vermilion",
#                                             fish$lake_name)))) %>%
#   mutate(total_effort_1 = ifelse(lake_name == "Vermilion" & is.na(site_id), fish$total_effort_1,
#                             ifelse(lake_name == "East Vermilion", 12,
#                                    ifelse(lake_name == "West Vermilion", 8, fish$total_effort_1))))
# 
# # #test if it worked
# table(fish.split.verm$lake_name)
# 
# Verm2 <- fish.split.verm %>%
#   filter(lake_name == "Vermilion" | lake_name == "East Vermilion" | lake_name == "West Vermilion")
# table(Verm2$total_effort_1, Verm2$year)
# #looks good
# 
# #SAVE THIS FISH DATA FILE
# #write.csv(fish.split.verm, "Data/Output/Sed_Lake_Fish_Data_DNR_Raw.csv")



#START HERE ONCE YOU ALREADY COLLECT AND SAVE FISH DATA-------------------------

#DNR CPUE calcs----------

#read in fish data from MidFish
fish_dnr <- read.csv("Data/Input/Sed_Lake_Fish_Data_DNR_Raw.csv")

names(fish_dnr)
unique(fish_dnr$sampling_method)

#isolate just gillnets
#I collected trapnet data, but I only have gillnets from Red Lake and keeps it consistent with my master's thesis analysis
fish_dnr_gillnet <- fish_dnr %>% 
  filter(sampling_method == "gill_net_standard") %>% 
  #also remove red lake from this
  filter(lake_name != "Red (Upper Red)")

#do we have any lake-years with multiple separate total effort idents?
effort_check <- fish_dnr_gillnet %>% 
  group_by(lake_name, year) %>% 
  summarize(efforts = n_distinct(total_effort_1),
            .groups = 'drop') %>% 
  filter(efforts > 1)
#just Bde Maka Ska in 1972 - just combine those efforts (2 + 6 = 8)
fish_dnr_gillnet_effort_corrected <- fish_dnr_gillnet %>% 
  mutate(total_effort_1 = ifelse(lake_name == "Bde Maka Ska" & year == 1972, 8, total_effort_1))


#combine a few species like I did for my thesis where sometimes they are IDed to species but not always
#what issues do we have?
sort(unique(fish_dnr_gillnet_effort_corrected$species_1))
#what's up with the "crappie"?
crappie <- fish_dnr_gillnet_effort_corrected %>% 
  filter(species_1 == "crappie") #this is one fish from Big Cormorant in 1981, all the other crappie in Big Corm are black crappie so renaming to that
#what's up with the "sunfish"?
sunfish <- fish_dnr_gillnet_effort_corrected %>% 
  filter(species_1 == "sunfish") #this is one fish from Bear Head in 1984, all other sunfishes in this sample are pumpkinseed so calling it that
#what's up with the walleye x sauger?
wxs <- fish_dnr_gillnet_effort_corrected %>% 
  filter(species_1 == "walleye_x_sauger") #this is 3 fish in lake of the woods - just leaving them, this hybrid species will get dropped with other rare species
#fix the issues
fish_dnr_clean <- fish_dnr_gillnet_effort_corrected %>% 
  mutate(species_1 = case_when(
    species_1 == "shorthead_redhorse" | species_1 == "greater_redhorse" | species_1 == "silver_redhorse" | species_1 == "golden_redhorse" ~ "redhorse", 
    species_1 == "black_bullhead" | species_1 == "brown_bullhead" | species_1 == "brown_bullhead" | species_1 == "yellow_bullhead" | species_1 == "bullheads" ~ "bullhead", 
    species_1 == "bowfin_dogfish" ~ "bowfin",
    species_1 == "crappie" ~ "black_crappie",
    species_1 == "sunfish" ~ "pumpkinseed",
    TRUE ~ species_1))

#check that I got everything
sort(unique(fish_dnr_clean$species_1))
#looks good
     
#calculate counts of each species and total effort for each lake in each year
#this deals with Bde Maka Ska in 1972 which had two different total effort idents
dnr_cpue <- fish_dnr_clean %>% 
  group_by(lake_name,  year, species_1,  total_effort_1) %>% 
  summarize(count = n(),
            .groups = 'drop') %>%
  mutate(cpue = count/total_effort_1)

#read in the 2025 Lotus standard survey gillnet data I got and formatted from LakeFinder because Midfish only goes to 2024
#There is also trapnet data in lakefinder but I only took the gillnet data
lotus2025 <- read.csv("Data/Input/Lotus_2025_Fish_Standard_Survey_from_LakeFinder.csv")

#add the lotus data to the rest of the dnr data
dnr_cpue_complete <- rbind(dnr_cpue, lotus2025)


#Red lake CPUE calcs---------------

#read in multispecies fish data from Red Lake Band
fish_red <- read.csv("Data/Input/Red Lake All Species Catches 2012-2024.csv")

#read in red lake gillnet effort data (packaged with walleye CPUE)
red_effort <- read.csv("Data/Input/Red_Lake_Gillnet_Effort_and_Walleye_CPUE.csv")

#read in Red lake CPUE data from 2024 report (for checking my calculations)
red_report_cpues <- read.csv("Data/Input/Red_Lake_CPUE_from_2024_report.csv")

#isolate red lake gillnet data from DNR dataset
fish_dnr_gillnet_red <- fish_dnr %>% 
  filter(sampling_method == "gill_net_standard") %>% 
  filter(lake_name == "Red (Upper Red)") %>% 
  #apply the relevant species corrections
  mutate(species_1 = case_when(
    species_1 == "shorthead_redhorse" | species_1 == "greater_redhorse" | species_1 == "silver_redhorse" | species_1 == "golden_redhorse" ~ "redhorse", 
    species_1 == "black_bullhead" | species_1 == "brown_bullhead" | species_1 == "brown_bullhead" | species_1 == "bullheads" ~ "bullhead", 
    TRUE ~ species_1))
#check species
sort(unique(fish_dnr_gillnet_red$species_1))




#how does report effort (should be dnr + band effort) compare to the reported effort from the dnr and the band?
#I checked that there is only one total effort ident per year in red lake from DNR data
red_dnr_effort_check <- fish_dnr_gillnet_red %>% 
  group_by(year) %>% 
  summarize(effort_count = n_distinct(total_effort_ident),
            .groups = 'drop')
#looks good
#isolate DNR effort
red_dnr_effort <- fish_dnr_gillnet_red %>% 
  group_by(year) %>% 
  summarize(dnr_effort = max(total_effort_1),
            .groups = 'drop')
#isolate the combined effort data in the report
red_total_effort <- red_effort %>% 
  filter(lake_name == "Red (Upper Red)") %>% 
  select(year, total_effort_1) %>% 
  rename(total_effort = total_effort_1)
#get effort from red lake tribal data
#this will miss nets that did not catch anything so could be an undercount
table(fish_red$NET, fish_red$MESH)
#444 is probably supposed to be 44, there are other fish caught in net 44 with the same date
red_band_effort <- fish_red %>% 
  #fix type in net number 444
  mutate(NET = ifelse(NET == 444, 44, NET)) %>% 
  #filter to only upper red
  filter(lake == "Upper") %>% 
  #count number of unique net numbers in each year
  group_by(YEAR_) %>% 
  summarize(band_effort = n_distinct(NET)) %>% 
  #format year column to match other data
  mutate(year = as.numeric(paste0(20, YEAR_))) %>% 
  select(-YEAR_)
#join the three types of effort data together for comparison
red_effort_combo1 <- left_join(red_dnr_effort, red_band_effort, by = "year")
red_effort_combo2 <- left_join(red_effort_combo1, red_total_effort, by = "year")
red_effort_test <- red_effort_combo2 %>% 
  mutate(SUM = dnr_effort + band_effort,
         DIFF = SUM - total_effort,
         TEST = ifelse(DIFF == 0, "good", "check"))
#this looks good
#in the years before I have tribal data, the difference between total and dnr is 16 nets which is consistent with what the tribe sets in upper red every year
#a few differences which can be attributed to tribal nets that did not catch anything
#Therefore, I am going to trust the total effort in the combined report for all species

#also check that the effort for lower red from the band matches with the effort from the report
lower_red_band_effort<- fish_red %>% 
  #filter to only lower red
  filter(lake == "Lower") %>% 
  #count number of unique net numbers in each year
  group_by(YEAR_) %>% 
  summarize(band_effort = n_distinct(NET)) %>% 
  #format year column to match other data
  mutate(year = as.numeric(paste0(20, YEAR_))) %>% 
  select(-YEAR_)
#32 every year, which matches the report


#get counts by species by year from red lake band data
red_band_counts <- fish_red %>% 
  #get rid of the empty rows at the bottom and the weird other row with no data in it
  filter(!is.na(NET) & NO != "-0- ") %>% 
  #reformat year to match the dnr data and make the count column numeric
  mutate(year = as.numeric(paste0(20, YEAR_)),
         NO = as.numeric(NO)) %>% 
  #get species names to match the dnr data and get rid of multiple codes for a single species
  rename(species_1 = SPEC) %>%  #renaming to reuse an old code chunk I already wrote:
  mutate(species_1 = case_when(
    species_1 == "bbt" ~ "burbot",
    species_1 == "blc" | species_1 == "BLC" ~ "black_crappie",
    species_1 == "blg" ~ "bluegill",
    species_1 == "blh" ~ "bullhead",
    species_1 == "frd" | species_1 == "FRD" ~ "freshwater_drum",
    species_1 == "goe" ~ "goldeye",
    species_1 == "lks" ~ "lake_sturgeon",
    species_1 == "lkw" ~ "lake_whitefish",
    species_1 == "nop" ~ "northern_pike",
    species_1 == "pmk" ~ "pumpkinseed",
    species_1 == "qui" ~ "quillback",
    species_1 == "rdh" ~ "redhorse",
    species_1 == "rkb" | species_1 == "rob" ~ "rock_bass",
    species_1 == "wae" | species_1 == "WAE" ~ "walleye",
    species_1 == "wts" ~ "white_sucker",
    species_1 == "yep" | species_1 == "YEP" ~ "yellow_perch", 
    TRUE ~ "ERROR")
  ) %>% 
  #get counts by species per year
  group_by(lake, year, species_1) %>% 
  summarize(count = sum(NO),
            .groups = 'drop') %>% 
  #update lake names to match the rest of my data
  mutate(lake_name = ifelse(lake == "Upper", "Red (Upper Red)", "Red (Lower Red)")) %>% 
  select(-lake) %>% 
  relocate(lake_name, .before = year) %>% 
  #add a column to identify these as band counts
  mutate(source = "band")

#check
table(red_band_counts$species_1)
#looks good

#get counts from the dnr data
red_dnr_counts <- fish_dnr_gillnet_red %>% 
  group_by(lake_name,  year, species_1) %>% 
  summarize(count = n(),
            .groups = 'drop') %>% 
  #add source column
  mutate(source = "dnr") 

#rowbind the two count data sources together
red_all_counts <- rbind(red_band_counts, red_dnr_counts)

#out of curiosity, visualize these counts (knowing that effort has been consistent in recent years but not early years)
red_all_walleye_counts <- red_all_counts %>% 
  filter(species_1 == "walleye")
ggplot(data = red_all_walleye_counts, aes(x = year, y = count, color = source))+
  geom_point()+
  geom_line()+
  facet_wrap(vars(lake_name))+
  theme_classic()
  
#combine the two different count sources for upper red
red_combined_counts <- red_all_counts %>% 
  group_by(lake_name, year, species_1) %>% 
  summarize(count = sum(count),
            .groups = 'drop')

#add effort to count data, calculate cpue
red_effort_simple <- red_effort %>% 
  select(lake_name, year, total_effort_1)
red_all_cpue <- left_join(red_combined_counts, red_effort_simple, by = c("lake_name", "year")) %>% 
  #get rid of rows before I have tribal data because effort will be wrong
  filter(year >= 2012) %>% 
  #calculate cpue
  mutate(cpue = count/total_effort_1) %>% 
  #move columns to match dnr data
  relocate(count, .after = total_effort_1)

#compare this to the cpue in the report
red_cpue_check <- left_join(red_all_cpue, red_report_cpues, by = c("lake_name", "year", "species_1")) %>% 
  rename(report_cpue = gillnet_cpue) %>% 
  mutate(DIFF = report_cpue - cpue)
#okay they are not always EXACTLY the same, it looks like there was some rounding in the report, but these are all at least really close
#which means I did my calculations correctly and I will move forward with those




#Combine, format, save data--------------

#rowbind the red lake and all other lake data
all_cpue_long <- rbind(dnr_cpue_complete, red_all_cpue)


#pivot wider, make NA = 0 to represent none of that species for a given lake-year if not present in data
all_cpue_wide <- all_cpue_long %>% 
  select(-total_effort_1, -count) %>%
  pivot_wider(names_from = species_1, values_from = cpue, values_fill = 0) 


#save this file
#write.csv(all_cpue_wide, file = "Data/Output/Sed_Lake_Fish_CPUE.csv", row.names = FALSE)




#Explore LOW Sites-----------------------------------
#We have surface sediments from 3 different locations in lake of the woods
#need to decide if I combine the sediment samples and compare that to the whole lake fish survey, or split the fish survey up by matching sites
LOW_fish <- fish_dnr %>% 
  filter(lake_name == "Lake of the Woods")

sort(unique(LOW_fish$site_id))

#look at just gillnets
LOW_gillnet <- LOW_fish %>% 
  filter(sampling_method == "gill_net_standard")
sort(unique(LOW_gillnet$site_id))
#There are site IDs for recent surveys if I were to want them




#Trapnet data coverage---------------
#How many lakes do I even have trapnet data for?
trapnet_fish <- fish_dnr %>% 
  filter(sampling_method == "trap_net")
sort(unique(trapnet_fish$lake_name))

#compare to all lake names
sort(unique(fish_dnr$lake_name))

#NO TRAPNET DATA FOR 10 of my lakes: Upper/lower red, Cass, E/W Vermilion, Kabetogama, Leech, Namakan, Sand Point, Winnibigoshish
