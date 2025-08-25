#This R script counts the number of individuals identified for each slide and sediment sample

#packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(vegan)
library(tidyr)
library(stringr)


#read in data and make N/A, NA, or an empty cell all read in as NA
#update file with new data as needed
Sed_Data <- read.csv("Data/Input/Sediment_Zoop_ID_20250825.csv", na.strings = c("N/A", "NA", ""))

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
Sed_Data_Clean <- Sed_Data %>% 
  filter(Taxa != "Unidentifiable" & Taxa != "Not a zoop (after check)" & Taxa != "Mystery Spike (probably plant)" & Taxa != "Unsure: CHECK" & !is.na(Taxa))

#remove spaces before and after lake names to make for loops work properly
Sed_Data_Clean$LakeName <- str_trim(Sed_Data_Clean$LakeName, side = "both")

#Count number of slides counted for each sample
Slide_Count <- Sed_Data_Clean %>% 
  group_by(LakeName) %>% 
  summarise(Slide.Count = n_distinct(Slide.Number))
Slide_Count

#check that nothing is marked as Not Indentifiable and then given a taxa name
ID_Valid_Check <- Sed_Data_Clean %>% 
  filter(Remain.Condition == "Not identifiable")
count(ID_Valid_Check, Taxa)
#it's ok if larger taxonomic groups are here - it just means we couldn't get all the way to species with the structure


#We have to deal with things that are IDed to different levels by different structures
    #when we can identify to more detail with one structure than another, we will take the proportion of species from the detailed structure and apply that proportion to the more ambiguous structure
          #If we have multiple structures we can ID to same detailed taxonomic resolution and one that is more ambiguous: use most frequent remain types to get an individual count out of these detailed structures and use that for proportion
    #when we can't fully ID something due to to poor preservation or gunk on the slide - use proportion derived from OTHER SPECIMENS OF THAT SAME STRUCTURE that you can ID all the way
    #we will only do this when we have a reasonable sample size of fully IDed remains (3+ to create proportions with)

#get a list of lakes / samples to use in for loops:
lakes <- unique(Sed_Data_Clean$LakeName)
  
  #conflicts to deal with: 
      #Bosminids 
        #Headshields: can ID subgenus but sometimes headpore obscured or not easily visible: use proportion of headshields with headpores you can ID
        #Antennule segments and claws: cannot determine subgenus: use proportion of headshields with headpores you can ID
          #determine proportions of headshields with headpores you can ID for each lake
              #make empty matrix to store results
              bosminid_head_prop <- matrix(NA, nrow = length(lakes), ncol = 5, dimnames = list(NULL, c("LakeName", "b.longi.n", "b.longi.prop", "e.coreg.n", "e.coreg.prop")))
              #row index tracker - need to do this because looping over categorical data and lakenames can't be rows later on
              row_idx <- 1
              #calculate and store proportions for each lake
              for(i in unique(Sed_Data_Clean$LakeName)){
                #calculate the proportions for each lake
                temp_data <- Sed_Data_Clean %>% 
                  filter(LakeName == i & Remain.Type == "Headshield" & (Taxa == "Bosmina longirostris" | Taxa == "Eubosmina coregoni")) %>% 
                  count(Taxa) %>% 
                  mutate(prop = n / sum(n))
                #save the values from this calculation in the matrix
                    #save lakename
                       bosminid_head_prop[row_idx,1] <- i
                    #save bosmina longirostris n and proportion if present in data
                       if("Bosmina longirostris" %in% temp_data$Taxa){
                         bosminid_head_prop[row_idx,2] <- temp_data[temp_data$Taxa == "Bosmina longirostris", "n"]
                         bosminid_head_prop[row_idx,3] <- temp_data[temp_data$Taxa == "Bosmina longirostris", "prop"]
                       }
                   #save eubosmina coregoni n and proportion if present in data
                       if("Eubosmina coregoni" %in% temp_data$Taxa){
                         bosminid_head_prop[row_idx,4] <- temp_data[temp_data$Taxa == "Eubosmina coregoni", "n"]
                         bosminid_head_prop[row_idx,5] <- temp_data[temp_data$Taxa == "Eubosmina coregoni", "prop"]
                       }
                #increase the row_idx value for the next row
                row_idx <- row_idx + 1
              }
              #turn NA values into 0
              bosminid_head_prop[is.na(bosminid_head_prop)] <- 0
          
          #Carapaces: sometimes not clear if mucro present (broken) for subgenus: use proportion of carapaces that you can ID 
              bosminid_carp_prop <- matrix(NA, nrow = length(lakes), ncol = 5, dimnames = list(NULL, c("LakeName", "b.longi.n", "b.longi.prop", "e.coreg.n", "e.coreg.prop")))
              #row index tracker - need to do this because looping over categorical data and lakenames can't be rows later on
              row_idx <- 1
              #calculate and store proportions for each lake
              for(i in unique(Sed_Data_Clean$LakeName)){
                #calculate the proportions for each lake
                temp_data <- Sed_Data_Clean %>% 
                  filter(LakeName == i & Remain.Type == "Carapace" & (Taxa == "Bosmina longirostris" | Taxa == "Eubosmina coregoni")) %>% 
                  count(Taxa) %>% 
                  mutate(prop = n / sum(n))
                #save the values from this calculation in the matrix
                #save lakename
                bosminid_carp_prop[row_idx,1] <- i
                #save bosmina longirostris n and proportion if present in data
                if("Bosmina longirostris" %in% temp_data$Taxa){
                  bosminid_carp_prop[row_idx,2] <- temp_data[temp_data$Taxa == "Bosmina longirostris", "n"]
                  bosminid_carp_prop[row_idx,3] <- temp_data[temp_data$Taxa == "Bosmina longirostris", "prop"]
                }
                #save eubosmina coregoni n and proportion if present in data
                if("Eubosmina coregoni" %in% temp_data$Taxa){
                  bosminid_carp_prop[row_idx,4] <- temp_data[temp_data$Taxa == "Eubosmina coregoni", "n"]
                  bosminid_carp_prop[row_idx,5] <- temp_data[temp_data$Taxa == "Eubosmina coregoni", "prop"]
                }
                #increase the row_idx value for the next row
                row_idx <- row_idx + 1
              }
              #turn NA values into 0
              bosminid_carp_prop[is.na(bosminid_carp_prop)] <- 0
          
      #Daphnia postabdominal claws
        #can tell complex with most claws, but sometimes obscured or broken and can't tell
        #cam also tell with ephippia but we haven't been finding many (or any at all)
          #determine proportions of species complexes with claws you can ID for each lake
              #make empty matrix to store results
              daphnia_prop <- matrix(NA, nrow = length(lakes), ncol = 5, dimnames = list(NULL, c("LakeName", "d.longi.n", "d.longi.prop", "d.pulex.n", "d.pulex.prop")))
              #row index tracker - need to do this because looping over categorical data and lakenames can't be rows later on
              row_idx <- 1
              #calculate and store proportions for each lake
              for(i in unique(Sed_Data_Clean$LakeName)){
                #calculate the proportions for each lake
                temp_data <- Sed_Data_Clean %>% 
                  filter(LakeName == i & Remain.Type == "Postabdominal Claw" & (Taxa == "Daphnia longispina complex" | Taxa == "Daphnia pulex complex")) %>% 
                  count(Taxa) %>% 
                  mutate(prop = n / sum(n))
                #save the values from this calculation in the matrix
                #save lakename
                daphnia_prop[row_idx,1] <- i
                #save n and proportion for each taxa present in data
                if("Daphnia longispina complex" %in% temp_data$Taxa){
                  daphnia_prop[row_idx,2] <- temp_data[temp_data$Taxa == "Daphnia longispina complex", "n"]
                  daphnia_prop[row_idx,3] <- temp_data[temp_data$Taxa == "Daphnia longispina complex", "prop"]
                }
                if("Daphnia pulex complex" %in% temp_data$Taxa){
                  daphnia_prop[row_idx,4] <- temp_data[temp_data$Taxa == "Daphnia pulex complex", "n"]
                  daphnia_prop[row_idx,5] <- temp_data[temp_data$Taxa == "Daphnia pulex complex", "prop"]
                }
                #increase the row_idx value for the next row
                row_idx <- row_idx + 1
              }
              #turn NA values into 0
              daphnia_prop[is.na(daphnia_prop)] <- 0
              
      #Camptocercus and Acroperus headshields
        #can tell them apart with carapaces and postabdominal claws but not with headshields
          #determine proportions of species with carapaces and postabdomens that you can ID for each lake
          #THIS ONE IS DIFFERENT - we have multiple reliable ID structures so we have to count the most frequent remain type inside this proportion calculation to get a good individual count
              #make empty matrix to store results
              campto_acro_prop <- matrix(NA, nrow = length(lakes), ncol = 5, dimnames = list(NULL, c("LakeName", "a.harp.n", "a.harp.prop", "campto.n", "campto.prop")))
              #row index tracker - need to do this because looping over categorical data and lakenames can't be rows later on
              row_idx <- 1
              #calculate and store proportions for each lake
              for(i in unique(Sed_Data_Clean$LakeName)){
                #isolates data you want
                temp_data1 <- Sed_Data_Clean %>% 
                  filter(LakeName == i & (Remain.Type == "Postabdomen (no claw)" | Remain.Type == "Carapace") & (Taxa == "Acroperus harpae" | Taxa == "Camptocercus sp."))
                #IF there are none of these taxa in this sample at all, this will make all values 0 without causing code to stop executing
                if (nrow(temp_data1) < 1) {
                  campto_acro_prop[row_idx,1] <- i
                  campto_acro_prop[row_idx,2] <- 0
                  campto_acro_prop[row_idx,3] <- 0
                  campto_acro_prop[row_idx,4] <- 0
                  campto_acro_prop[row_idx,5] <- 0
                }
                #the rest is the normal code for samples with these taxa present
                else {
                  #get counts by both taxa and proportion
                  temp_data2 <- as.data.frame(table(temp_data1$Taxa, temp_data1$Remain.Type))
                  #rename columns, remove counts of zero, retain only the most frequent remain type for each taxa, then use that to calculate proportion
                  temp_data2 <- temp_data2 %>% 
                    rename(Taxa = Var1) %>% #rename column
                    rename(Remain.Type = Var2) %>% #rename column
                    filter(Freq != 0) %>% #remove counts of zero
                    group_by(Taxa) %>% #group by taxa
                    summarise(n = max(Freq)) %>% #keep only most frequent remain type for each taxa
                    mutate(prop = n / sum(n)) #calculate proportions
                  #save the values from this calculation in the matrix
                  #save lakename
                  campto_acro_prop[row_idx,1] <- i
                  #save n and proportion for each taxa present in data - this version extracts the values out of 1x1 tibbles
                  if("Acroperus harpae" %in% temp_data2$Taxa){
                    campto_acro_prop[row_idx,2] <- temp_data2 %>% 
                      filter(Taxa == "Acroperus harpae") %>% 
                      pull(n) %>% 
                      .[1]
                    
                    campto_acro_prop[row_idx,3] <- temp_data2 %>% 
                      filter(Taxa == "Acroperus harpae") %>% 
                      pull(prop) %>% 
                      .[1]
                  }
                  if("Camptocercus sp." %in% temp_data2$Taxa){
                    campto_acro_prop[row_idx,4] <- temp_data2 %>% 
                      filter(Taxa == "Camptocercus sp.") %>% 
                      pull(n) %>% 
                      .[1]
                    
                    campto_acro_prop[row_idx,5] <- temp_data2 %>% 
                      filter(Taxa == "Camptocercus sp.") %>% 
                      pull(prop) %>% 
                      .[1]
                  }
                }
                  #increase the row_idx value for the next row
                  row_idx <- row_idx + 1
              }
              #turn NA values into 0
              campto_acro_prop[is.na(campto_acro_prop)] <- 0
              
                          
              
      #Aloninae and Chydorinae vs. genus and species
      #Alona circumfimbriata, guttata, or setulosa vs. individual species
      
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



  
