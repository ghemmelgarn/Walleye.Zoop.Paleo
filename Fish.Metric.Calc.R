#this script takes the downloaded fish data and calculates fish metrics
#for now just doing walleye CPUE, later on will try to get to biomass with all species

#read in fish data
fish.data <- read.csv("Data/Input/FishData.csv")

#if there is a chance that some of the surveys caught no fish at all, go ask Denver what to do

#we want cpue of walleye for each lake and year - not grouping by total_effort_ident because I want to sum both catch and effort of shallow and deep nets in stratified surveys
#using total_effort_cse (combine stratified effort) that I calcualted when I pulled the fish data
walleye.cpue <- fish %>% 
  group_by(lake_id, year.x) %>% 
  mutate(walleye_count = sum(species_1 == "walleye"), 
         WAE.CPUE = walleye_count/total_effort_cse
         ) %>% 
  distinct(lake_id, year.x, .keep_all = TRUE) %>% 
  select(lake_name, lake_id, year.x, walleye_count, WAE.CPUE)
#the distinct part of this collapses each sampling effort into one row but keeps all the columns - will only keep first row with each ident  
#select chooses the columns we want to keep  

#save this as a .csv file
write.csv(walleye.cpue, file = "Data/Output/Walleye_CPUE.csv")
