#This R script counts the number of individuals identified for each slide and sediment sample

#packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(vegan)
library(tidyr)


#read in data and make N/A, NA, or an empty cell all read in as NA
#update file with new data as needed
Sed_Data <- read.csv("Data/Input/Sediment_Zoop_ID_20250815.csv", na.strings = c("N/A", "NA", ""))

#Fix spelling errors and rename columns
Sed_Data <- Sed_Data %>% 
  rename(Carapace.Length.µm = Carapace.Length..µm.) %>% 
  rename(Mucro.Length.µm = Mucro.linear.Length..µm.) %>% 
  rename(Mucro.Segments = Mucro.Sutures) %>% 
  rename(Antennule.Length.µm = Attenule.Length..µm.) %>% 
  rename(Photo.File.Name = Photo.file.name.s.) %>% 
  select(-Unidentifiable.remain.coordinates) #removes an unused column

Sed_Data$Remain.Condition <- ifelse(Sed_Data$Remain.Condition == "Identifieable and measureable", "Identifiable and measureable", Sed_Data$Remain.Condition)

#Clean up taxa names to get rid of notes used in ID process
#FOR NOW = assuming mucro on carapace means B. longirostris and no mucro means E. coregoni
Sed_Data$Taxa <- ifelse(Sed_Data$Taxa == "Bosminid (headshield or claw - genus unsure)", "Bosminid", 
                        ifelse(Sed_Data$Taxa == "Chydorus brevilabris (previously C. sphaericus)", "Chydorus brevilabris",
                               ifelse(Sed_Data$Taxa == "Bosminid carapace without mucro", "Eubosmina coregoni",
                                      ifelse(Sed_Data$Taxa == "Bosminid carapace with mucro", "Bosmina longirostris",
                                             ifelse(Sed_Data$Taxa == "Acroperus harpae (use for carapace)", "Acroperus harpae", 
                                                    ifelse(Sed_Data$Taxa == "Eubosmina coregoni (headshield only)", "Eubosmina coregoni", 
                                                           ifelse(Sed_Data$Taxa == "Bosmina longirostris (headshield only)", "Bosmina longirostris",
                                                                  ifelse(Sed_Data$Taxa == "Alona affinis (use for anything exept carapace))", "Alona affinis",
                                                                         ifelse(Sed_Data$Taxa == "Chydorinae (sp. unsure)", "Chydorinae",
                                                                                ifelse(Sed_Data$Taxa == "Camptocercus sp. (use for carapace)", "Camptocercus sp.",
                                                                                       ifelse(Sed_Data$Taxa == "Alona sp. (use for carapace)", "Alona sp.",
                                                                                              ifelse(Sed_Data$Taxa == "Alona quadrangularis (use for anythine except carapace))", "Alona quadrangularis",
                                                                                                     ifelse(Sed_Data$Taxa == "Alona circumfimbriata, guttata, or setulosa (headshield)", "Alona circumfimbriata, guttata, or setulosa",
                                                                                                            ifelse(Sed_Data$Taxa == "Camptocercus or Acroperus (use for headshield)", "Camptocercus sp. or Acroperus sp.",
                                                                                                                   ifelse(Sed_Data$Taxa == "Aloninae (sp. unsure)", "Aloninae",
                                                                                                                          ifelse(Sed_Data$Taxa == "Holopedium sp. or Sida crystallina americana (postabdomen)", "Holopedium sp. or Sida crystallina americana",
                                                                                                                                 ifelse(Sed_Data$Taxa == "Sida crystalina", "Sida crystallina americana",
                                                                                                                                     ifelse(Sed_Data$Taxa == "Dapnnia (complex unsure)", "Daphnia sp.",
                                                                                                                                            ifelse(Sed_Data$Taxa == "Alona barbulata (headshield)", "Alona barbulata",
                                                                                                                                                   ifelse(Sed_Data$Taxa == "Alona barbulata (postabdomen)", "Alona barbulata",
                                                                                                                                                          ifelse(Sed_Data$Taxa == "Alona costata (headshield)", "Alona costata", Sed_Data$Taxa)))))))))))))))))))))


#Now remove remains that are not useful for counting: Unidentifiable, not zoops, yet to be checked for ID, or NA values
#ALSO REMOVING ANTENNAL SEGMENTS FOR THIS - because 2 antennae is a zoop
Sed_Data_Clean <- Sed_Data %>% 
  filter(Taxa != "Unidentifiable" & Taxa != "Not a zoop (after check)" & Taxa != "Mystery Spike (probably plant)" & Taxa != "Unsure: CHECK" & !is.na(Taxa))


#Count number of slides counted for each sample
Slide_Count <- Sed_Data_Clean %>% 
  group_by(LakeName) %>% 
  summarise(Slide.Count = n_distinct(Slide.Number))
Slide_Count


#We have to deal with things that are IDed to different levels by different structures
  #conflicts to deal with: 
      #Bosminids
      #Daphnia sp. vs. complexes
      #Aloninae and Chydorinae vs. genus and species
      #Alona circumfimbriata, guttata, or setulosa vs. individual species
      #Camptocercus and Acroperus headshields
      #Alona carapaces (can't distinguish) vs. postabdomens (can distinguish)
      #maybe others?
#FOR NOW: Group them all into higher taxonomic levels and sum the counts - all I am doing here is making sure we have counted enough individuals. If I am off, this will underestimate the number of individuals counted, which is not a problem.
    #Will deal with this for species ID later (with proportions of better IDed structures)
Specimen_Count_Modified$Taxa <- ifelse(Specimen_Count_Modified$Taxa == "Eubosmina coregoni" | Specimen_Count_Modified$Taxa == "Bosmina longirostris", "Bosminid", 
                                       ifelse(Specimen_Count_Modified$Taxa == )
                                                
#NEED TO FINISH THIS GROUPING FOR THE PRELIMINARY COUNTS


#Get counts by taxa AND remain type for each sample
#make a frequency table that gets saved as a data frame
Specimen_Count <- as.data.frame(table(Sed_Data_Clean$LakeName, Sed_Data_Clean$Taxa, Sed_Data_Clean$Remain.Type))
#rename columns
Specimen_Count <- Specimen_Count %>% 
  rename(LakeName = Var1) %>% 
  rename(Taxa = Var2) %>% 
  rename(Remain.Type = Var3) 
#remove counts of 0
Specimen_Count <- Specimen_Count %>% 
  filter(Freq != 0)

#ONE CLADOCERA = 1 Headshield + 2 Shell Valves ( = 1 Carapace) + 1 Postabdomen + 2 Postabdominal claws
#PROBLEM: We have not been keeping track if carapaces have both sides or only one side present, or if postab claws are paired or separate
    #Evelyn says all the daphnia claws she finds are paired... but not necessarily for other taxa
#FOR NOW: assume the claws are not paired and all the carapaces have both sides, also throwing out bosminid antennules segments as a way to count individuals - I only want these for the measurements
#this means divide the claw count by 2 (for all taxa except Daphnia) and leave carapace counts as they are - round up on things divided by 2 because 2.5 individuals means you had at least 3 individuals
Specimen_Count_Modified <- Specimen_Count %>% 
  mutate(Freq = ifelse(Taxa != "Daphnia longispina complex" & Taxa != "Daphnia sp." & Taxa != "Daphnia pulex complex" & Remain.Type == "Postabdominal Claw", ceiling(Freq/2), Freq))


#Now take the most frequent remain type for each taxa and use it as the individual count
Individual_Count <- Specimen_Count_Modified %>% 
  group_by(LakeName, Taxa) %>% 
  summarise(Count = max(Freq))



  
