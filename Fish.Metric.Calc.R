#this script takes the downloaded fish data and calculates walleye CPUE

#read in fish data

#if there is a chance that some of the surveys caught no fish at all, go ask Denver what to do

#we want counts of walleye for each total_effort_ident
fish.metric <- fish %>% 
  group_by(total_effort_ident) %>% 
  mutate(walleye_count = sum(species_1 == "walleye"), WAE.CPUE = walleye_count/total_effort_1) %>% 
  distinct(total_effort_ident, .keep_all = TRUE) %>% 
  select(lake_name, lake_id, walleye_count, WAE.CPUE, year.x, total_effort_ident)
#the distinct part of this collapses each sampling effort into one row but keeps all the columns - will only keep first row with each ident  
#select chooses the columns we want to keep  