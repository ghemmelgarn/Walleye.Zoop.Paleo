#this script takes pre-cleaned sediment zooplankton data and fish CPUE to make a transfer function to predict fish community


#packages
library(tidyverse)
library(ggplot2)
library(vegan)

#read in sediment zoop community composition
sed <- read.csv("Q:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Walleye paleolimnology and zooplankton/Data/Processed Data/Sediment Zoop Processed Data/Individual_Count_by_Taxa_2026-11-18.csv")
#read in pelagic fish gillnet cpue I collected and calculated in the script called "Fish data for Sediment Lakes.R"
fish <- read.csv("Data/Input/Sed_Lake_Fish_CPUE.csv")
#I have no idea why this says there is a chinook salmon in Cass in 2009, but this will get removed when I eliminate rare species

#need to make lake names the same in sed data and fish data so I can join them
sed.rename <- sed %>% 
  mutate(lake_name = case_when(
    LakeName == "BearHead" ~ "Bear Head",
    LakeName == "BigCarn" ~ "Big Cormorant",
    LakeName == "VermilionE" ~ "East Vermilion",
    LakeName == "MilleLacs" ~ "Mille Lacs",
    LakeName == "RedL" ~ "Red (Lower Red)",
    LakeName == "RedU" ~ "Red (Upper Red)",
    LakeName == "SandPoint" ~ "Sand Point",
    LakeName == "Tenmile" ~ "Ten Mile",
    LakeName == "VermilionW" ~ "West Vermilion",
    LakeName == "WhiteBear" ~ "White Bear",
    LakeName == "Winni" ~ "Winnibigoshish",
    TRUE ~ LakeName
  )) %>% 
  relocate(lake_name, .after = LakeName) %>% 
  select(-LakeName)

#need to sum counts by taxa for the three LOW sediment samples to make one LOW row
sed.LOW <- sed.rename %>% 
  filter(lake_name == "LOW BT4" | lake_name == "Low BT3" | lake_name == "LOW Muskey")
sed.LOW.sum <- sed.LOW %>% 
  summarize(across(Acroperus.harpae:Pleuroxus.straminius, ~sum(.x))) %>% 
  mutate(lake_name = "Lake of the Woods") %>% 
  relocate(lake_name, .before = Acroperus.harpae)

#remove old LOW columns and add new one to the whole dataset
sed.clean <- sed.rename %>% 
  filter(lake_name != "LOW BT4" & lake_name != "Low BT3" & lake_name != "LOW Muskey") %>% 
  rbind(sed.LOW.sum)

  
#select the most recent fish data year for each lake
fish.recent <- fish %>% 
  filter(lake_name != "Vermilion") %>% #this is just early data where I could not separate East/West
  group_by(lake_name) %>% 
  slice_max(order_by = year, n = 1) %>% 
  ungroup()


#join the fish data to the sed data
sed.fish <- left_join(sed.clean, fish.recent, by = "lake_name") %>% 
  rename(fish.year = year) %>% 
  relocate(fish.year, .after = lake_name)






#okay do RDA on the three CPUEs

#separate explanatory and response matrices (response = zoop, explanatory = fish)
sed.zoop.only <- sed.rename %>% 
  select(Acroperus.harpae:Pleuroxus.straminius)

Y.sed.zoop.hellinger <- sed.zoop.only %>% 
  decostand(method = "hellinger") %>% 
rownames(Y.sed.zoop.hellinger) <- sed.rename$lake_name

X.fish <- sed.fish %>% 
  filter(!is.na(walleye)) %>% 
  select(largemouth_bass:yellow_perch)
rownames(X.fish) <- sed.rename$lake_name

#PCA of the community comp data just to see how things separate out (and how different are the three LOW samples?)
pca1 <- pca(Y.sed.zoop.hellinger)
plot(pca1)
#this plot is saved as "Sed_Zoop_Comm_Comp_PCA.png"

#run rda with walleye as explanatory
rda_wae<- rda(Y.sed.zoop.hellinger ~ walleye, data = X.fish)
summary(rda_wae)
plot(rda_wae)

#run rda with yellow perch as explanatory
rda_yep<- rda(Y.sed.zoop.hellinger ~ yellow_perch, data = X.fish)
summary(rda_yep)
plot(rda_yep)










