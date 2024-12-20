#this script takes the downloaded fish data and calculates fish metrics
#for now just doing walleye CPUE, later on will try to get to biomass with all species

#read in fish data
fish.data <- read.csv("Data/Input/FishData.csv")

#if there is a chance that some of the surveys caught no fish at all, go ask Denver what to do

#we want cpue of walleye for each lake and year - not grouping by total_effort_ident because I want to sum both catch and effort of shallow and deep nets in stratified surveys
#using total_effort_cse (combine stratified effort) that I calculated when I pulled the fish data
#also want largemouth bass CPUE for a walleye vs. centrarchid plot
walleye.cpue <- fish.data %>% 
  group_by(lake_id, year.x) %>% 
  mutate(walleye_count = sum(species_1 == "walleye"), 
         LMB_count = sum(species_1 == "largemouth_bass"),
         WAE.CPUE = walleye_count/total_effort_cse,
         LMB.CPUE = LMB_count/total_effort_cse
         ) %>% 
  distinct(lake_id, year.x, .keep_all = TRUE) %>% 
  select(lake_name, lake_id, year.x, walleye_count, WAE.CPUE, LMB_count, LMB.CPUE, fish.effort.sufficient)
#the distinct part of this collapses each sampling effort into one row but keeps all the columns - will only keep first row with each ident  
#select chooses the columns we want to keep  

# #save this as a .csv file
# write.csv(walleye.cpue, file = "Data/Output/Walleye_CPUE.csv")


#SAME THING AS ABOVE BUT THE MORE INCLUSIVE DATA FOR SAMPLE SIZE IMPROVEMENT:
fish.data.inclusive <- read.csv("Data/Input/FishData.inclusive.csv")
walleye.cpue.inclusive <- fish.data.inclusive %>% 
  group_by(lake_id, year.x) %>% 
  mutate(walleye_count = sum(species_1 == "walleye"), 
         LMB_count = sum(species_1 == "largemouth_bass"),
         WAE.CPUE = walleye_count/total_effort_cse,
         LMB.CPUE = LMB_count/total_effort_cse
  ) %>% 
  distinct(lake_id, year.x, .keep_all = TRUE) %>% 
  select(lake_name, lake_id, year.x, walleye_count, WAE.CPUE, LMB_count, LMB.CPUE, fish.effort.sufficient)

# #save this as a .csv file
# write.csv(walleye.cpue.inclusive, file = "Data/Output/Walleye_CPUE.inclusive.csv")
