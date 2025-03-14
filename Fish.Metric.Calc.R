#this script takes the downloaded fish data and calculates fish metrics
#for now just doing walleye CPUE, later on will try to get to biomass with all species

library(tidyverse)
library(dplyr)


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



