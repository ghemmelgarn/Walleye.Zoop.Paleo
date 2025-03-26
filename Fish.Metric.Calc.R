#this script takes the downloaded fish data and calculates fish metrics: abundance and CPUE for all species
#below it investigates which fish we have length/weight data for

library(tidyverse)
library(dplyr)
library(ggplot2) #to make exploratory plots below


#read in fish data
fish.data <- read.csv("Data/Input/FishData.csv")

#if there is a chance that some of the surveys caught no fish at all, go ask Denver what to do

# #check a few things:
# #should all be gillnet
# unique(fish.data$sampling_method.x)
# #Good
# 
# #should all have net nights for effort units
# unique(fish.data$total_effort_1_units)
# #Good
# 
# #should all have caught something
# unique(fish.data$total_effort_nothing_caught)
# #Good

# #I need a list of all the species present
unique(fish.data$species_1)
# 
# #to get MN fish abbreiviations
# # install.packages("remotes")
# # remotes::install_github("mnsentinellakes/mnsentinellakes")
# library(mnsentinellakes)
# data("fishabbreviations")
# view(fishabbreviations)
# #export this as a .csv for easy reference
# #write.csv(fishabbreviations, file = "Data/Output/MNFishAbbreviations.csv")

#TAXONOMY RESOLUTION INVESTIGATION
# #investigate: Carpsucker vs. river carpsucker, vs. highfin carpsucker
# carpsuckers <- fish.data %>% 
#   filter(species_1 == "carpsucker" | species_1 == "river_carpsucker" | species_1 == "highfin_carpsucker")
# #they are all in the same lake, only one highfin, three carpsuckers, the rest sprecified as river carpsuckers
# #going to call them all carpsuckers because they are rare and unlikely to directly affect zoops much
# 
# #investigate bullheads vs. the specific bullheads
# bullheads <- fish.data %>% 
#   filter(species_1 == "bullheads" | species_1 == "yellow_bullhead" | species_1 == "brown_bullhead" | species_1 == "black_bullhead")
# #how many of each are there
# bullheads %>% 
#   as.tibble() %>% 
#   count(species_1)
# #only 1 fish called "bullheads" not specified to species, find it:
# bullheads.1 <- fish.data %>% 
#   filter(species_1 == "bullheads")
# #it's from Rainy lake, total effort ident 36984
# bullheads.2 <- fish.data %>% 
#   filter(total_effort_ident == "36984")
# #its the only bullhead in this effort, lets look at this lake more generally
# Rainy <- fish.data %>% 
#   filter(lake_name == "Rainy")
# Rainy %>% 
#   as.tibble() %>% 
#   count(species_1)
# #okay this is the only bullhead caught in this lake in all of the surveys we have here... probably okay to leave it as bullheads on its own because it is NOT going to have a strong effect on the data and will not double count a species in any lake

# #investigate redhorses
# redhorses <- fish.data %>% 
#   filter(species_1 == "redhorse" | species_1 == "shorthead_redhorse" | species_1 == "greater_redhorse" | species_1 == "silver_redhorse" | species_1 == "golden_redhorse" | species_1 == "river_redhorse")
# #how many of each are there
# redhorses %>% 
#    as.tibble() %>% 
#    count(species_1)
# #again, not that many of them (112 not to species level), and not expected to strongly affect zoops directly - also not mixed within surveys so won't inflate survey species abundance
# #leave the redhorse column separate


#we want cpue of each species for each lake and year - not grouping by total_effort_ident because I want to sum both catch and effort of shallow and deep nets in stratified surveys
#started with just largemouth bass and walleye, now getting all the species 
#using total_effort_cse (combine stratified effort) that I calculated when I pulled the fish data
#also want largemouth bass CPUE for a walleye vs. centrarchid plot
all.fish.cpue <- fish.data %>% 
  group_by(lake_id, year.x) %>% 
  mutate(WAE.count = sum(species_1 == "walleye"), 
         WAE.CPUE = WAE.count/total_effort_cse,
         LMB.count = sum(species_1 == "largemouth_bass"),
         LMB.CPUE = LMB.count/total_effort_cse,
         NOP.count = sum(species_1 == "northern_pike"),
         NOP.CPUE = NOP.count/total_effort_cse,
         WTS.count = sum(species_1 == "white_sucker"),
         WTS.CPUE = WTS.count/total_effort_cse,
         BLG.count = sum(species_1 == "bluegill"),
         BLG.CPUE = BLG.count/total_effort_cse,
         YEP.count = sum(species_1 == "yellow_perch"),
         YEP.CPUE = YEP.count/total_effort_cse,
         SHR.count = sum(species_1 == "shorthead_redhorse"),
         SHR.CPUE = SHR.count/total_effort_cse,
         YEB.count = sum(species_1 == "yellow_bullhead"),
         YEB.CPUE = YEB.count/total_effort_cse,
         BOF.count = sum(species_1 == "bowfin"),
         BOF.CPUE = BOF.count/total_effort_cse,
         HSF.count = sum(species_1 == "hybrid_sunfish"),
         HSF.CPUE = HSF.count/total_effort_cse,
         BLC.count = sum(species_1 == "black_crappie"),
         BLC.CPUE = BLC.count/total_effort_cse,
         BLB.count = sum(species_1 == "black_bullhead"),
         BLB.CPUE = BLB.count/total_effort_cse,
         RKB.count = sum(species_1 == "rock_bass"),
         RKB.CPUE = RKB.count/total_effort_cse,
         BRB.count = sum(species_1 == "brown_bullhead"),
         BRB.CPUE = BRB.count/total_effort_cse,
         PMK.count = sum(species_1 == "pumpkinseed"),
         PMK.CPUE = PMK.count/total_effort_cse,
         CAP.count = sum(species_1 == "common_carp"),
         CAP.CPUE = CAP.count/total_effort_cse,
         GOS.count = sum(species_1 == "golden_shiner"),
         GOS.CPUE = GOS.count/total_effort_cse,
         RHS.count = sum(species_1 == "redhorse"),
         RHS.CPUE = RHS.count/total_effort_cse,
         TLC.count = sum(species_1 == "cisco"),
         TLC.CPUE = TLC.count/total_effort_cse,
         MUE.count = sum(species_1 == "muskellunge"),
         MUE.CPUE = MUE.count/total_effort_cse,
         LKS.count = sum(species_1 == "lake_sturgeon"),
         LKS.CPUE = LKS.count/total_effort_cse,
         GSF.count = sum(species_1 == "green_sunfish"),
         GSF.CPUE = GSF.count/total_effort_cse,
         SMB.count = sum(species_1 == "smallmouth_bass"),
         SMB.CPUE = SMB.count/total_effort_cse,
         LKW.count = sum(species_1 == "lake_whitefish"),
         LKW.CPUE = LKW.count/total_effort_cse,
         GRR.count = sum(species_1 == "greater_redhorse"),
         GRR.CPUE = GRR.count/total_effort_cse,
         BUB.count = sum(species_1 == "burbot"),
         BUB.CPUE = BUB.count/total_effort_cse,
         FRD.count = sum(species_1 == "freshwater_drum"),
         FRD.CPUE = FRD.count/total_effort_cse,
         GOE.count = sum(species_1 == "goldeye"),
         GOE.CPUE = GOE.count/total_effort_cse,
         QBS.count = sum(species_1 == "quillback"),
         QBS.CPUE = QBS.count/total_effort_cse,
         TRP.count = sum(species_1 == "trout_perch"),
         TRP.CPUE = TRP.count/total_effort_cse,
         GIS.count = sum(species_1 == "gizzard_shad"),
         GIS.CPUE = GIS.count/total_effort_cse,
         WHC.count = sum(species_1 == "white_crappie"),
         WHC.CPUE = WHC.count/total_effort_cse,
         SNG.count = sum(species_1 == "shortnose_gar"),
         SNG.CPUE = SNG.count/total_effort_cse,
         BIB.count = sum(species_1 == "bigmouth_buffalo"),
         BIB.CPUE = BIB.count/total_effort_cse,
         CCF.count = sum(species_1 == "channel_catfish"),
         CCF.CPUE = CCF.count/total_effort_cse,
         WHB.count = sum(species_1 == "white_bass"),
         WHB.CPUE = WHB.count/total_effort_cse,
         SLR.count = sum(species_1 == "silver_redhorse"),
         SLR.CPUE = SLR.count/total_effort_cse,
         TPM.count = sum(species_1 == "tadpole_madtom"),
         TPM.CPUE = TPM.count/total_effort_cse,
         RBT.count = sum(species_1 == "rainbow_trout"),
         RBT.CPUE = RBT.count/total_effort_cse,
         LAT.count = sum(species_1 == "lake_trout"),
         LAT.CPUE = LAT.count/total_effort_cse,
         RBS.count = sum(species_1 == "rainbow_smelt"),
         RBS.CPUE = RBS.count/total_effort_cse,
         BKT.count = sum(species_1 == "brook_trout"),
         BKT.CPUE = BKT.count/total_effort_cse,
         CRC.count = sum(species_1 == "creek_chub"),
         CRC.CPUE = CRC.count/total_effort_cse,
         PRD.count = sum(species_1 == "pearl_dace"),
         PRD.CPUE = PRD.count/total_effort_cse,
         SAR.count = sum(species_1 == "sauger"),
         SAR.CPUE = SAR.count/total_effort_cse,
         MOE.count = sum(species_1 == "mooneye"),
         MOE.CPUE = MOE.count/total_effort_cse,
         NHS.count = sum(species_1 == "northern_hog_sucker"),
         NHS.CPUE = NHS.count/total_effort_cse,
         SAB.count = sum(species_1 == "smallmouth_buffalo"),
         SAB.CPUE = SAB.count/total_effort_cse,
         SLC.count = sum(species_1 == "silver_chub"),
         SLC.CPUE = SLC.count/total_effort_cse,
         BLS.count = sum(species_1 == "blue_sucker"),
         BLS.CPUE = BLS.count/total_effort_cse,
         CS.count = sum(species_1 == "carpsucker" | species_1 == "river_carpsucker" | species_1 == "highfin_carpsucker"),
         CS.CPUE = CS.count/total_effort_cse,
         GLR.count = sum(species_1 == "golden_redhorse"),
         GLR.CPUE = GLR.count/total_effort_cse,
         LNG.count = sum(species_1 == "longnose_gar"),
         LNG.CPUE = LNG.count/total_effort_cse,
         FCF.count = sum(species_1 == "flathead_catfish"),
         FCF.CPUE = FCF.count/total_effort_cse,
         WAS.count = sum(species_1 == "walleye_x_sauger"),
         WAS.CPUE = WAS.count/total_effort_cse,
         PAH.count = sum(species_1 == "paddlefish"),
         PAH.CPUE = PAH.count/total_effort_cse,
         RRH.count = sum(species_1 == "river_redhorse"),
         RRH.CPUE = RRH.count/total_effort_cse,
         SLS.count = sum(species_1 == "shovelnose_sturgeon"),
         SLS.CPUE = SLS.count/total_effort_cse,
         SIL.count = sum(species_1 == "silver_lamprey"),
         SIL.CPUE = SIL.count/total_effort_cse,
         OSS.count = sum(species_1 == "orangespotted_sunfish"),
         OSS.CPUE = OSS.count/total_effort_cse,
         SPO.count = sum(species_1 == "spottail_shiner"),
         SPO.CPUE = SPO.count/total_effort_cse,
         TME.count = sum(species_1 == "tiger_muskellunge"),
         TME.CPUE = TME.count/total_effort_cse,
         LNS.count = sum(species_1 == "longnose_sucker"),
         LNS.CPUE = LNS.count/total_effort_cse,
         BLH.count = sum(species_1 == "bullheads"),
         BLH.CPUE = BLH.count/total_effort_cse
         ) %>% 
  distinct(lake_id, year.x, .keep_all = TRUE) %>% 
  select(lake_name, 
         lake_id, 
         nhdhr_id, 
         year.x, 
         WAE.count,
         WAE.CPUE, 
         LMB.count, 
         LMB.CPUE,  
         NOP.count,
         NOP.CPUE,
         WTS.count,
         WTS.CPUE,
         BLG.count,
         BLG.CPUE,
         YEP.count,
         YEP.CPUE,
         SHR.count,
         SHR.CPUE,
         YEB.count,
         YEB.CPUE,
         BOF.count,
         BOF.CPUE,
         HSF.count,
         HSF.CPUE,
         BLC.count,
         BLC.CPUE,
         BLB.count,
         BLB.CPUE,
         RKB.count,
         RKB.CPUE,
         BRB.count,
         BRB.CPUE,
         PMK.count,
         PMK.CPUE,
         CAP.count,
         CAP.CPUE,
         GOS.count,
         GOS.CPUE,
         RHS.count,
         RHS.CPUE,
         TLC.count,
         TLC.CPUE,
         MUE.count,
         MUE.CPUE,
         LKS.count,
         LKS.CPUE,
         GSF.count,
         GSF.CPUE,
         SMB.count,
         SMB.CPUE,
         LKW.count,
         LKW.CPUE,
         GRR.count,
         GRR.CPUE,
         BUB.count,
         BUB.CPUE,
         FRD.count,
         FRD.CPUE,
         GOE.count,
         GOE.CPUE,
         QBS.count,
         QBS.CPUE,
         TRP.count,
         TRP.CPUE,
         GIS.count,
         GIS.CPUE,
         WHC.count,
         WHC.CPUE,
         SNG.count,
         SNG.CPUE,
         BIB.count,
         BIB.CPUE,
         CCF.count,
         CCF.CPUE,
         WHB.count,
         WHB.CPUE,
         SLR.count,
         SLR.CPUE,
         TPM.count,
         TPM.CPUE,
         RBT.count,
         RBT.CPUE,
         LAT.count,
         LAT.CPUE,
         RBS.count,
         RBS.CPUE,
         BKT.count,
         BKT.CPUE,
         CRC.count,
         CRC.CPUE,
         PRD.count,
         PRD.CPUE,
         SAR.count,
         SAR.CPUE,
         MOE.count,
         MOE.CPUE,
         NHS.count,
         NHS.CPUE,
         SAB.count,
         SAB.CPUE,
         SLC.count,
         SLC.CPUE,
         BLS.count,
         BLS.CPUE,
         CS.count,
         CS.CPUE,
         GLR.count,
         GLR.CPUE,
         LNG.count,
         LNG.CPUE,
         FCF.count,
         FCF.CPUE,
         WAS.count,
         WAS.CPUE,
         PAH.count,
         PAH.CPUE,
         RRH.count,
         RRH.CPUE,
         SLS.count,
         SLS.CPUE,
         SIL.count,
         SIL.CPUE,
         OSS.count,
         OSS.CPUE,
         SPO.count,
         SPO.CPUE,
         TME.count,
         TME.CPUE,
         LNS.count,
         LNS.CPUE,
         BLH.count,
         BLH.CPUE
         )
#the distinct part of this collapses each sampling effort into one row but keeps all the columns - will only keep first row with each ident  
#select chooses the columns we want to keep  

# # #save this as a .csv file
# write.csv(all.fish.cpue, file = "Data/Output/All_Fish_CPUE.csv")


#-----------------------------------------------------------------------------------------------------
#THIS SECTION INVESTIGATES WHICH FISHES HAVE LENGTH/WEIGHT DATA

#read in fish data again if you didn't run the above section
fish.data <- read.csv("Data/Input/FishData.csv")

# #check that lengths and weights are all the same units 
# unique(fish.data$length_unit_1)
# unique(fish.data$weight_unit_1)
# #looks good

#histogram of all fish lengths that we do have
hist(fish.data$length_1)

#first create a yes/no column if they have length
fish.data <- fish.data %>% 
  mutate(length_yn = ifelse(is.na(length_1), "no", "yes")) %>% 
  mutate(weight_yn = ifelse(is.na(weight_1), "no", "yes"))



#count of fish with and without lengths and weights
table(fish.data$length_yn)
table(fish.data$weight_yn)
#we see that most of the fish actually do have lengths

#counts together
table(fish.data$length_yn, fish.data$weight_yn)
#all the fish with weights have a length, but not all the fish with lengths have a weight

#LENGTH x SPP
#lets look at this by species
table(fish.data$species_1, fish.data$length_yn)
#make this into a data frame
length.spp <- data.frame(table(fish.data$species_1, fish.data$length_yn))
length.spp <- pivot_wider(data = length.spp, names_from = Var2, values_from = Freq)
length.spp <- rename(length.spp, species = Var1)

#calculate proportion of individuals with lengths for each species
length.spp$total <- length.spp$no + length.spp$yes
length.spp$prop <- length.spp$yes / length.spp$total

#plot it

#order species by decreasing proportion to make graph easy to read
length.spp$species <- factor(length.spp$species, levels = length.spp$species[order(length.spp$prop)])
#plot
length.spp.prop <- ggplot(length.spp, aes(x = species, y = prop)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(y = "Proprotion of Individuals with Length Data", x = "Species", title = "Length x Species")+
  geom_text(aes(label = total), hjust = -0.1)+
  geom_text(aes(label = "n", x = 68, y = 1.01), vjust = 0.9, size = 5)+
  theme_classic()
length.spp.prop

#WEIGHT x SPP
#lets look at this by species
table(fish.data$species_1, fish.data$weight_yn)
#make this into a data frame
weight.spp <- data.frame(table(fish.data$species_1, fish.data$weight_yn))
weight.spp <- pivot_wider(data = weight.spp, names_from = Var2, values_from = Freq)
weight.spp <- rename(weight.spp, species = Var1)

#calculate proportion of individuals with lengths for each species
weight.spp$total <- weight.spp$no + weight.spp$yes
weight.spp$prop <- weight.spp$yes / weight.spp$total

#plot it

#order species by decreasing proportion to make graph easy to read
weight.spp$species <- factor(weight.spp$species, levels = weight.spp$species[order(weight.spp$prop)])
#plot
weight.spp.prop <- ggplot(weight.spp, aes(x = species, y = prop)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(y = "Proprotion of Individuals with Weight Data", x = "Species", title = "Weight x Species")+
  geom_text(aes(label = total), hjust = -0.1)+
  geom_text(aes(label = "n", x = 68, y = 1.01), vjust = 0.9, size = 5)+
  theme_classic()
weight.spp.prop

#LENGTH AND WEIGHT x SPECIES
#combine the data frames
length.weight.spp <- rename(length.spp, length.prop = prop)
length.weight.spp$weight.prop <- weight.spp$prop

#plot them together
#tiff("Length_Weight_Data_by_species.tiff", width = 15, height = 10, units = "in", res = 300)                   
length.weight.spp.prop <- ggplot()+
  geom_bar(data = length.weight.spp, aes(x = species, y = length.prop), stat = "identity", fill = "gray") +
  geom_bar(data = length.weight.spp, aes(x = species, y = weight.prop), stat = "identity", fill = "lightblue")+
  coord_flip() +
  labs(y = "Proprotion of Individuals with Data", x = "Species", title = "Length and Weight x Species")+
  geom_text(data = length.weight.spp, aes(label = total, x = species, y = length.prop), hjust = -0.1)+
  geom_text(aes(label = "n", x = 68, y = 1.01), vjust = 0.9, size = 5)+
  theme_classic()+
  geom_tile(data = data.frame(x = 4, y = 0.9), aes(x = x, y = y), 
                            width = 1, height = 0.01, fill = "lightblue") +  # Color box
  annotate("text", x = 4, y = 0.95, label = "Weight Data", size = 4)+
  geom_tile(data = data.frame(x = 2, y = 0.9), aes(x = x, y = y), 
            width = 1, height = 0.01, fill = "gray") +  # Color box
  annotate("text", x = 2, y = 0.95, label = "Length Data", size = 4)
  #scale_fill_manual(values = c("length" = "gray", "weight" = "lightblue"))
length.weight.spp.prop
#dev.off()


#write .csv of length and weight data by species
lw.export <- rename(length.weight.spp, length.yes = yes, length.no = no)
lw.export$weight.yes <- weight.spp$yes
lw.export$weight.no <- weight.spp$no
#reorder
lw.export <- lw.export[, c(1,4,3,2,5,7,8,6)]
#write csv
#write.csv(lw.export, file = "Data/Output/Fish_Species_Length_Weight_Data.csv")

#LENGTH AND WEIGHT BY LAKE
table(fish.data$lake_name, fish.data$length_yn)
#make this into a data frame
length.lake <- data.frame(table(fish.data$lake_name, fish.data$length_yn))
length.lake <- pivot_wider(data = length.lake, names_from = Var2, values_from = Freq)
length.lake <- rename(length.lake, lake = Var1)

#calculate proportion of individuals with lengths for each lake
length.lake$total <- length.lake$no + length.lake$yes
length.lake$prop <- length.lake$yes / length.lake$total

#add weight data to data frame too
lw.lake <- rename(length.lake, length.yes = yes, length.no = no, length.prop = prop)
#reorder
lw.lake <- lw.lake[, c(1,4,3,2,5)]
#create weight data
table(fish.data$lake_name, fish.data$weight_yn)
#make this into a data frame
weight.lake <- data.frame(table(fish.data$lake_name, fish.data$weight_yn))
weight.lake <- pivot_wider(data = weight.lake, names_from = Var2, values_from = Freq)
weight.lake <- rename(weight.lake, species = Var1)
weight.lake$total <- weight.lake$no + weight.lake$yes
weight.lake$prop <- weight.lake$yes / weight.lake$total
#add weight data to length data
lw.lake$weight.yes <- weight.lake$yes
lw.lake$weight.no <- weight.lake$no
lw.lake$weight.prop <- weight.lake$prop

#write csv
#write.csv(lw.lake, file = "Data/Output/Fish_Lake_Length_Weight_Data.csv")

#order lakes by decreasing proportion to make graph easy to read
lw.lake$lake <- factor(lw.lake$lake, levels = lw.lake$lake[order(lw.lake$length.prop)])

#plot it
#tiff("Length_Weight_Data_by_Lake.tiff", width = 15, height = 10, units = "in", res = 300)                   
length.weight.lake.prop <- ggplot()+
  geom_bar(data = lw.lake, aes(x = lake, y = length.prop), stat = "identity", fill = "gray") +
  geom_bar(data = lw.lake, aes(x = lake, y = weight.prop), stat = "identity", fill = "lightblue")+
  coord_flip() +
  labs(y = "Proprotion of Individuals with Data", x = "Lake", title = "Length and Weight x Lake")+
  geom_text(data = lw.lake, aes(label = total, x = lake, y = length.prop), hjust = -0.1)+
  geom_text(aes(label = "n", x = 68, y = 1.01), vjust = 0.9, size = 5)+
  theme_classic()+
  geom_tile(data = data.frame(x = 4, y = 0.9), aes(x = x, y = y), 
            width = 1, height = 0.01, fill = "lightblue") +  # Color box
  annotate("text", x = 4, y = 0.95, label = "Weight Data", size = 4)+
  geom_tile(data = data.frame(x = 2, y = 0.9), aes(x = x, y = y), 
            width = 1, height = 0.01, fill = "gray") +  # Color box
  annotate("text", x = 2, y = 0.95, label = "Length Data", size = 4)
#scale_fill_manual(values = c("length" = "gray", "weight" = "lightblue"))
length.weight.lake.prop
#dev.off()

#LENGTH AND WEIGHT BY lake/year
#need to make parentdow fish year columns
fish_parentdow <- fish.data %>%
  mutate(parentdow = case_when(
    nchar(fish.data$lake_id) == 7 ~ substr(lake_id, 1, 5),
    nchar(fish.data$lake_id) == 8 ~ substr(lake_id, 1, 6)
  ))
fish_parentdow$parentdow.fish.year = paste(fish_parentdow$parentdow, fish_parentdow$year.x)
#look at the table
table(fish_parentdow$parentdow.fish.year, fish.data$length_yn)
#make this into a data frame
length.ly <- data.frame(table(fish_parentdow$parentdow.fish.year, fish.data$length_yn))
length.ly <- pivot_wider(data = length.ly, names_from = Var2, values_from = Freq)
length.ly <- rename(length.ly, parentdow.fish.year = Var1)

#calculate proportion of individuals with lengths for each lake
length.ly$total <- length.ly$no + length.ly$yes
length.ly$prop <- length.ly$yes / length.ly$total

#add weight data to data frame too
lw.ly<- rename(length.ly, length.yes = yes, length.no = no, length.prop = prop)
#reorder
lw.ly <- lw.ly[, c(1,4,3,2,5)]
#create weight data
table(fish_parentdow$parentdow.fish.year, fish.data$weight_yn)
#make this into a data frame
weight.ly <- data.frame(table(fish_parentdow$parentdow.fish.year, fish.data$weight_yn))
weight.ly <- pivot_wider(data = weight.ly, names_from = Var2, values_from = Freq)
weight.ly <- rename(weight.ly, species = Var1)
weight.ly$total <- weight.ly$no + weight.ly$yes
weight.ly$prop <- weight.ly$yes / weight.ly$total
#add weight data to length data
lw.ly$weight.yes <- weight.ly$yes
lw.ly$weight.no <- weight.ly$no
lw.ly$weight.prop <- weight.ly$prop

#add lake name back in
name.join <- fish_parentdow%>%
  group_by(parentdow.fish.year) %>%
  summarize(lake_name = first(lake_name), year = first(year.x), .groups = 'drop')
lw.ly.name <- left_join(lw.ly, name.join, by = "parentdow.fish.year")
#create lake name year column
lw.ly.name$lake.year = paste(lw.ly.name$lake_name, lw.ly.name$year)

#write csv
#write.csv(lw.ly.name, file = "Data/Output/Fish_Survey_Length_Weight_Data.csv")

#order lakes by decreasing proportion to make graph easy to read
lw.ly.name$lake.year <- factor(lw.ly.name$lake.year, levels = lw.ly.name$lake.year[order(lw.ly.name$length.prop)])

#plot it
#THIS PLOT LOOKS TERRIBLE IN R - BUT IF YOU MAKE THE TIFF WITH HEIGHT = 50 in, ZOOM, AND SCROLL YOU CAN SEE WHAT YOU NEED TO SEE
#tiff("Length_Weight_Data_by_Survey.tiff", width = 15, height = 50, units = "in", res = 300)                   
length.weight.surv.prop <- ggplot()+
  geom_bar(data = lw.ly.name, aes(x = lake.year, y = length.prop), stat = "identity", fill = "gray") +
  geom_bar(data = lw.ly.name, aes(x = lake.year, y = weight.prop), stat = "identity", fill = "lightblue")+
  coord_flip() +
  labs(y = "Proprotion of Individuals with Data", x = "Fish Survey", title = "Length and Weight x Fish Survey")+
  geom_text(data = lw.ly.name, aes(label = total, x = lake.year, y = length.prop), hjust = -0.1)+
  geom_text(aes(label = "n", x = 261, y = 1.01), vjust = 0.9, size = 5)+
  theme_classic()+
  geom_tile(data = data.frame(x = 4, y = 0.9), aes(x = x, y = y), 
            width = 1, height = 0.01, fill = "lightblue") +  # Color box
  annotate("text", x = 4, y = 0.95, label = "Weight Data", size = 4)+
  geom_tile(data = data.frame(x = 2, y = 0.9), aes(x = x, y = y), 
            width = 1, height = 0.01, fill = "gray") +  # Color box
  annotate("text", x = 2, y = 0.95, label = "Length Data", size = 4)
#scale_fill_manual(values = c("length" = "gray", "weight" = "lightblue"))
length.weight.surv.prop
#dev.off()
