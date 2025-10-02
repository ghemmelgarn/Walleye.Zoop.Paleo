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
Sed_Data <- read.csv("Data/Input/Sediment_Zoop_ID_20250829.csv", na.strings = c("N/A", "NA", ""))

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
                                                                  ifelse(Sed_Data$Taxa == "Alona affinis (use for anything exept carapace)", "Alona affinis",
                                                                         ifelse(Sed_Data$Taxa == "Chydorinae (sp. unsure)", "Chydorinae",
                                                                                ifelse(Sed_Data$Taxa == "Camptocercus sp. (use for carapace)", "Camptocercus sp.",
                                                                                       ifelse(Sed_Data$Taxa == "Alona sp. (use for carapace)", "Alona sp.",
                                                                                              ifelse((Sed_Data$Taxa == "Alona quadrangularis (use for anything except carapace)"|Sed_Data$Taxa == "Alona quadrangularis (use for anythine except carapace))"), "Alona quadrangularis",
                                                                                                     ifelse((Sed_Data$Taxa == "Alona circumfimbriata, guttata, or setulosa  (use for anything exept carapace)"|Sed_Data$Taxa == "Alona circumfimbriata, guttata, or setulosa (headshield)"), "Alona circumfimbriata, guttata, or setulosa",
                                                                                                            ifelse(Sed_Data$Taxa == "Camptocercus or Acroperus (use for headshield)", "Camptocercus sp. or Acroperus sp.",
                                                                                                                   ifelse(Sed_Data$Taxa == "Aloninae (sp. unsure)", "Aloninae",
                                                                                                                          ifelse(Sed_Data$Taxa == "Holopedium sp. or Sida crystallina americana (postabdomen)", "Holopedium sp. or Sida crystallina americana",
                                                                                                                                 ifelse(Sed_Data$Taxa == "Sida crystalina", "Sida crystallina americana",
                                                                                                                                     ifelse(Sed_Data$Taxa == "Dapnnia (complex unsure)", "Daphnia sp.",
                                                                                                                                            ifelse(Sed_Data$Taxa == "Alona barbulata (use for anything exept carapace)", "Alona barbulata",
                                                                                                                                                          ifelse(Sed_Data$Taxa == "Holopedium sp. (use for postabdominal CLAW)", "Holopedium sp.",
                                                                                                                                                                 ifelse(Sed_Data$Taxa == "Alona rustica (use for anything exept carapace)", "Alona rustica",
                                                                                                                                                                        ifelse(Sed_Data$Taxa == "Alona intermedia (use for anything exept carapace)", "Alona intermedia",
                                                                                                                                                                               ifelse((Sed_Data$Taxa == "Alona costata (use for anything exept carapace)"|Sed_Data$Taxa == "Alona costata (headshield)"), "Alona costata", Sed_Data$Taxa)))))))))))))))))))))))


#Now remove remains that are not useful for counting: Unidentifiable, not zoops, yet to be checked for ID, or NA values, also remove NA values for LakeName or Remain.Type
Sed_Data_Clean_Uncorrected_for_pairs <- Sed_Data %>% 
  filter(Taxa != "Unidentifiable" & Taxa != "Not a zoop (after check)" & Taxa != "Mystery Spike (probably plant)" & Taxa != "Unsure: CHECK" & !is.na(Taxa))

#remove spaces before and after lake names to make for loops work properly
Sed_Data_Clean_Uncorrected_for_pairs$LakeName <- str_trim(Sed_Data_Clean_Uncorrected_for_pairs $LakeName, side = "both")

#Count number of slides counted for each sample
Slide_Count <- Sed_Data_Clean_Uncorrected_for_pairs  %>% 
  group_by(LakeName) %>% 
  summarise(Slide.Count = n_distinct(Slide.Number))
Slide_Count

#check that nothing is marked as Not Indentifiable and then given a taxa name
ID_Valid_Check <- Sed_Data_Clean_Uncorrected_for_pairs %>% 
  filter(Remain.Condition == "Not identifiable")
count(ID_Valid_Check, Taxa)
#it's ok if larger taxonomic groups are here - it just means we couldn't get all the way to species with the structure
    #If you find a concern here, ask the person who did the ID


#QAQC CHECK: this code pulls out rows that are impossible: eg. you can't ID this precisely with this structure. Check that there is nothing here.
#Will also pull out rows with problematic NA values - spot check these and correct as needed
ID_QAQC <- Sed_Data_Clean_Uncorrected_for_pairs %>% 
  filter(
    ((Remain.Type != "Headshield" & Remain.Type != "Carapace") & (Taxa == "Eubosmina coregoni" | Taxa == "Bosmina longirostris")) |
    ((Remain.Type != "Postabdominal Claw" & Remain.Type != "Ephippium") & (Taxa == "Daphnia pulex complex" | Taxa == "Daphnia longispina complex")) |
    (Remain.Type == "Headshield" & (Taxa == "Acroperus harpae" | Taxa == "Camptocercus sp.")) |
    (Remain.Type == "Postabdomen (no claw)" & (Taxa == "Sida crystallina americana" | Taxa == "Holopedium sp.")) |
    (Remain.Type == "Carapace" & (Taxa == "Alona affinis" | Taxa == "Alona quadrangularis" | Taxa == "Alona intermedia" | Taxa == "Alona barbulata" | Taxa == "Alona costata" | Taxa == "Alona rustica" | Taxa == "Alona circumfimbriata, guttata, or setulosa")) |
     is.na(LakeName) | is.na(Remain.Type)
  )

#To fix these ID problems identified above, run this code:
Sed_Data_Clean_Uncorrected_for_pairs_QAQC <- Sed_Data_Clean_Uncorrected_for_pairs %>% 
  mutate(Taxa = ifelse(((Remain.Type != "Headshield" & Remain.Type != "Carapace") & (Taxa == "Eubosmina coregoni" | Taxa == "Bosmina longirostris")), "Bosminid", 
                 ifelse(((Remain.Type != "Postabdominal Claw" & Remain.Type != "Ephippium") & (Taxa == "Daphnia pulex complex" | Taxa == "Daphnia longispina complex")), "Daphnia sp.",
                        ifelse((Remain.Type == "Headshield" & (Taxa == "Acroperus harpae" | Taxa == "Camptocercus sp.")), "Camptocercus sp. or Acroperus sp.",
                               ifelse((Remain.Type == "Postabdomen (no claw)" & (Taxa == "Sida crystallina americana" | Taxa == "Holopedium sp.")), "Holopedium sp. or Sida crystallina americana",
                                     ifelse((Remain.Type == "Carapace" & (Taxa == "Alona affinis" | Taxa == "Alona quadrangularis" | Taxa == "Alona intermedia" | Taxa == "Alona barbulata" | Taxa == "Alona costata" | Taxa == "Alona rustica" | Taxa == "Alona circumfimbriata, guttata, or setulosa")), "Alona sp.", Taxa 
                  )))))
          )
                 
#Add code here to fix specific problematic NA values if needed
Sed_Data_Clean_Uncorrected_for_pairs_QAQC <- Sed_Data_Clean_Uncorrected_for_pairs_QAQC %>% 
  filter(!is.na(LakeName))


#Check that it fixed the issue appropriately
ID_QAQC2 <- Sed_Data_Clean_Uncorrected_for_pairs_QAQC %>% 
  filter(
    ((Remain.Type != "Headshield" & Remain.Type != "Carapace") & (Taxa == "Eubosmina coregoni" | Taxa == "Bosmina longirostris")) |
      ((Remain.Type != "Postabdominal Claw" & Remain.Type != "Ephippium") & (Taxa == "Daphnia pulex complex" | Taxa == "Daphnia longispina complex")) |
      (Remain.Type == "Headshield" & (Taxa == "Acroperus harpae" | Taxa == "Camptocercus sp.")) |
      (Remain.Type == "Postabdomen (no claw)" & (Taxa == "Sida crystallina americana" | Taxa == "Holopedium sp.")) |
      (Remain.Type == "Carapace" & (Taxa == "Alona affinis" | Taxa == "Alona quadrangularis" | Taxa == "Alona intermedia" | Taxa == "Alona barbulata" | Taxa == "Alona costata" | Taxa == "Alona rustica" | Taxa == "Alona circumfimbriata, guttata, or setulosa")) |
      is.na(LakeName) | is.na(Remain.Type)
  )




#DEAL WITH PAIRED STRUCTURES: postabdominal claws and carapaces
#IF a structure is paired, duplicate that row in the data, so when it gets divided by two later, the individual count is still accurate
#IF a structure is listed as not paired, leave the row alone - represents half of an individual when it gets divided by two later
#IF a structure has NA for paired, it was identified before we started keeping track of this:
      #FOR NOW: 
            #postabdominal claws: A + E say that bosminid and daphnid claws are almost always paired. Assume this and multiply them by 2. Assume all other postabdominal claws are unpaired (single claws) to avoid overcounting
            #carapaces: A + E say that about half are paired. Assume all are unpaired to undercount for sample size requirement and will fix this later with post-hoc estimation
      #WHEN WE FINISH ID:
            #Find proportion of carapaces and claws that are paired for each taxa and use that to post-hoc estimate number of individuals for the slides where we did not keep track

                        #make a dataframe of the rows you want to duplicate
                        rows_to_duplicate <- Sed_Data_Clean_Uncorrected_for_pairs_QAQC %>%
                          filter(
                            #if a postabdominal claw or carapace is labeled as paired
                            ((Remain.Type == "Postabdominal Claw" | Remain.Type == "Carapace") & Paired. == "Yes - both present") |
                            #if bosminid and daphnid postab claws are labeled NA for paired (not kept track of) - assuming all paired for now, but assuming unpaired for other spp.
                            (is.na(Paired.) & Remain.Type == "Postabdominal Claw" & 
                               (Taxa == "Bosminid" | Taxa == "Eubosmina coregoni" | Taxa == "Bosmina longirostris" | 
                                  Taxa == "Daphnia sp." | Taxa == "Daphnia pulex complex" | Taxa == "Daphnia longispina complex" | 
                                  Taxa == "Ceriodaphnia sp.")
                             )
                            #if a carapace is labeled NA for paired, leave it alone for now (assuming unpaired)
                          )
                          
                        #now bind these duplicated rows onto original data and sort by remain number
                        Sed_Data_Clean <- Sed_Data_Clean_Uncorrected_for_pairs_QAQC %>%
                          bind_rows(rows_to_duplicate) %>% 
                          arrange(Remain.Number)


#We have to deal with things that are IDed to different levels by different structures
    #when we can identify to more detail with one structure than another, we will take the proportion of species from the detailed structure and apply that proportion to the more ambiguous structure
          #If we have multiple structures we can ID to same detailed taxonomic resolution and one that is more ambiguous: use most frequent remain types to get an individual count out of these detailed structures and use that for proportion
    #when we can't fully ID something due to to poor preservation or gunk on the slide - use proportion derived from OTHER SPECIMENS OF THAT SAME STRUCTURE that you can ID all the way


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
              
              #make this matrix a data frame
              bosminid_head_prop <- as.data.frame(bosminid_head_prop)
              
          
          #Carapaces: sometimes not clear if mucro present (carapace broken or obscured) for subgenus: use proportion of carapaces that you can ID 
              bosminid_carap_prop <- matrix(NA, nrow = length(lakes), ncol = 5, dimnames = list(NULL, c("LakeName", "b.longi.n", "b.longi.prop", "e.coreg.n", "e.coreg.prop")))
              #row index tracker - need to do this because looping over categorical data and lakenames can't be rows later on
              row_idx <- 1
              #calculate and store proportions for each lake
              for(i in unique(Sed_Data_Clean$LakeName)){
                #calculate the proportions for each lake
                temp_data <- Sed_Data_Clean %>% 
                  filter(LakeName == i & Remain.Type == "Carapace" & (Taxa == "Bosmina longirostris" | Taxa == "Eubosmina coregoni")) %>% 
                  count(Taxa) %>% 
                  mutate(n = ceiling(n/2)) %>%  #divide carapace count by 2 and then round up (accounts for the fact that there are 2 carapace sides on each zoop)
                  mutate(prop = n / sum(n))
                #save the values from this calculation in the matrix
                #save lakename
                bosminid_carap_prop[row_idx,1] <- i
                #save bosmina longirostris n and proportion if present in data
                if("Bosmina longirostris" %in% temp_data$Taxa){
                  bosminid_carap_prop[row_idx,2] <- temp_data[temp_data$Taxa == "Bosmina longirostris", "n"]
                  bosminid_carap_prop[row_idx,3] <- temp_data[temp_data$Taxa == "Bosmina longirostris", "prop"]
                }
                #save eubosmina coregoni n and proportion if present in data
                if("Eubosmina coregoni" %in% temp_data$Taxa){
                  bosminid_carap_prop[row_idx,4] <- temp_data[temp_data$Taxa == "Eubosmina coregoni", "n"]
                  bosminid_carap_prop[row_idx,5] <- temp_data[temp_data$Taxa == "Eubosmina coregoni", "prop"]
                }
                #increase the row_idx value for the next row
                row_idx <- row_idx + 1
              }
              #turn NA values into 0
              bosminid_carap_prop[is.na(bosminid_carap_prop)] <- 0
              
              #make this matrix a data frame
              bosminid_carap_prop <- as.data.frame(bosminid_carap_prop)
          
      #Daphnia postabdominal claws
        #can tell complex with most claws, but sometimes obscured or broken and can't tell
        #can also tell with ephippia but we haven't been finding many (or any at all)
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
                  mutate(n = ceiling(n/2)) %>%  #divide postab claw count by 2 and then round up (accounts for the fact that there are 2 claws on each zoop)
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
              
              #make this matrix a data frame
              daphnia_prop <- as.data.frame(daphnia_prop)
              
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
                  filter(LakeName == i & (Remain.Type == "Postabdomen (no claw)" | Remain.Type == "Carapace" | Remain.Type == "Postabdominal Claw") & (Taxa == "Acroperus harpae" | Taxa == "Camptocercus sp."))
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
                    mutate(Freq = ifelse((Remain.Type == "Postabdominal Claw" | Remain.Type == "Carapace"), ceiling(Freq/2), Freq)) %>%  #divide postabdominal claw and carapace counts by 2 and then round up (accounts for the fact that there are 2 claws and 2 carapace sides on each zoop)
                    group_by(Taxa) %>% #group by taxa
                    summarise(n = max(Freq)) %>% #keep only the count of the most frequent remain type for each taxa
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
              
              #make this matrix a data frame
              campto_acro_prop <- as.data.frame(campto_acro_prop)
              
                          
              
      #Holopedium and Sida crystallina
        #Can tell them apart with postabdominal claws but not with postabdomens. Exopodite segments of sida are preserved but not of holodpedium.
        #Will need to do individual counts of claws vs. exopodie segments with sida - will this overcount sida becuase more parts can be preserved? Probably... but not much I can do about it.
              #make empty matrix to store results
              holo_sida_prop <- matrix(NA, nrow = length(lakes), ncol = 5, dimnames = list(NULL, c("LakeName", "holo.n", "holo.prop", "sida.n", "sida.prop")))
              #row index tracker - need to do this because looping over categorical data and lakenames can't be rows later on
              row_idx <- 1
              #calculate and store proportions for each lake
              for(i in unique(Sed_Data_Clean$LakeName)){
                #isolates data you want
                temp_data1 <- Sed_Data_Clean %>% 
                  filter(LakeName == i & (Remain.Type == "Postabdominal Claw" | Remain.Type == "3rd exopodite segment" | Remain.Type == "2nd exopodite segment" |  Remain.Type ==  "1st/basal exopodite segment") & (Taxa == "Holopedium sp." | Taxa == "Sida crystallina americana"))
                #IF there are none of these taxa in this sample at all, this will make all values 0 without causing code to stop executing
                if (nrow(temp_data1) < 1) {
                  holo_sida_prop[row_idx,1] <- i
                  holo_sida_prop[row_idx,2] <- 0
                  holo_sida_prop[row_idx,3] <- 0
                  holo_sida_prop[row_idx,4] <- 0
                  holo_sida_prop[row_idx,5] <- 0
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
                    mutate(Freq = ifelse(Remain.Type == "Postabdominal Claw", ceiling(Freq/2), Freq)) %>%  #divide postabdominal claw count by 2 and then round up (accounts for the fact that there are 2 claws on each zoop)
                    group_by(Taxa) %>% #group by taxa
                    summarise(n = max(Freq)) %>% #keep only the count of the most frequent remain type for each taxa
                    mutate(prop = n / sum(n))
                  #save the values from this calculation in the matrix
                  #save lakename
                  holo_sida_prop[row_idx,1] <- i
                  #save n and proportion for each taxa present in data - this version extracts the values out of 1x1 tibbles
                  if("Holopedium sp." %in% temp_data2$Taxa){
                    holo_sida_prop[row_idx,2] <- temp_data2 %>% 
                      filter(Taxa == "Holopedium sp.") %>% 
                      pull(n) %>% 
                      .[1]
                    
                    holo_sida_prop[row_idx,3] <- temp_data2 %>% 
                      filter(Taxa == "Holopedium sp.") %>% 
                      pull(prop) %>% 
                      .[1]
                  }
                  if("Sida crystallina americana" %in% temp_data2$Taxa){
                    holo_sida_prop[row_idx,4] <- temp_data2 %>% 
                      filter(Taxa == "Sida crystallina americana") %>% 
                      pull(n) %>% 
                      .[1]
                    
                    holo_sida_prop[row_idx,5] <- temp_data2 %>% 
                      filter(Taxa == "Sida crystallina americana") %>% 
                      pull(prop) %>% 
                      .[1]
                  }
                }
                #increase the row_idx value for the next row
                row_idx <- row_idx + 1
              }
              #turn NA values into 0
              holo_sida_prop[is.na(holo_sida_prop)] <- 0 
              
              #make this matrix a data frame
              holo_sida_prop <- as.data.frame(holo_sida_prop)
              
                
      #Aloninae - THIS ONE IS LAYERED
          #1) We have no way to tell apart circumfimbriata vs. setulosa - they will always stay grouped
          #2) We can distinguish guttata from circumfimbriara/setulosa with postabdomens but not headshields
                  #BUT these postabdomens are so uncommon that we don't have enough data (or any in some lakes) to infer this - so keep these three as a group always
          #3) Everything else in genus Alona we can tell with headshields/postabdomens but not carapaces - so need to get indvidual counts with postabs/headshields (most frequent) and apply that to carapaces
          #4) Then when we can just get to aloninae due to poor preservation - we need ratios of every single species in that subfamily  
             
               #lets try it: (ONLY using original data to create ratios - not using inferred Alona carapaces even when you could because of the layering of this)
          
              #3: Alona species - using postabdomens and headshields to infer carapaces   
                        #make empty matrix to store results
                        alona_spp_prop <- matrix(NA, nrow = length(lakes), ncol = 15, dimnames = list(NULL, c("LakeName", "a.affinis.n", "a.affinis.prop", "a.quad.n", "a.quad.prop", "a.inter.n", "a.inter.prop", "a.barb.n", "a.barb.prop", "a.cost.n", "a.cost.prop", "a.rust.n", "a.rust.prop", "a.circ.gutt.setu.n", "a.circ.gutt.setu.prop")))
                        #row index tracker - need to do this because looping over categorical data and lakenames can't be rows later on
                        row_idx <- 1
                        #calculate and store proportions for each lake
                        for(i in unique(Sed_Data_Clean$LakeName)){
                          #isolates data you want
                          temp_data1 <- Sed_Data_Clean %>% 
                            filter(LakeName == i & (Remain.Type == "Postabdomen (no claw)" | Remain.Type == "Headshield") & (Taxa == "Alona affinis" | Taxa == "Alona quadrangularis" | Taxa == "Alona intermedia" | Taxa == "Alona barbulata" | Taxa == "Alona costata" | Taxa == "Alona rustica" | Taxa == "Alona circumfimbriata, guttata, or setulosa"))
                          #IF there are none of these taxa in this sample at all, this will make all values 0 without causing code to stop executing
                          if (nrow(temp_data1) < 1) {
                            alona_spp_prop[row_idx,1] <- i
                            alona_spp_prop[row_idx,2] <- 0
                            alona_spp_prop[row_idx,3] <- 0
                            alona_spp_prop[row_idx,4] <- 0
                            alona_spp_prop[row_idx,5] <- 0
                            alona_spp_prop[row_idx,6] <- 0
                            alona_spp_prop[row_idx,7] <- 0
                            alona_spp_prop[row_idx,8] <- 0
                            alona_spp_prop[row_idx,9] <- 0
                            alona_spp_prop[row_idx,10] <- 0
                            alona_spp_prop[row_idx,11] <- 0
                            alona_spp_prop[row_idx,12] <- 0
                            alona_spp_prop[row_idx,13] <- 0
                            alona_spp_prop[row_idx,14] <- 0
                            alona_spp_prop[row_idx,15] <- 0
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
                              summarise(n = max(Freq)) %>% #keep only the count of the most frequent remain type for each taxa
                              mutate(prop = n / sum(n))
                            #save the values from this calculation in the matrix
                            #save lakename
                            alona_spp_prop[row_idx,1] <- i
                            #save n and proportion for each taxa present in data - this version extracts the values out of 1x1 tibbles
                            if("Alona affinis" %in% temp_data2$Taxa){
                              alona_spp_prop[row_idx,2] <- temp_data2 %>% 
                                filter(Taxa == "Alona affinis") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              alona_spp_prop[row_idx,3] <- temp_data2 %>% 
                                filter(Taxa == "Alona affinis") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                            if("Alona quadrangularis" %in% temp_data2$Taxa){
                              alona_spp_prop[row_idx,4] <- temp_data2 %>% 
                                filter(Taxa == "Alona quadrangularis") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              alona_spp_prop[row_idx,5] <- temp_data2 %>% 
                                filter(Taxa == "Alona quadrangularis") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                            if("Alona intermedia" %in% temp_data2$Taxa){
                              alona_spp_prop[row_idx,6] <- temp_data2 %>% 
                                filter(Taxa == "Alona intermedia") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              alona_spp_prop[row_idx,7] <- temp_data2 %>% 
                                filter(Taxa == "Alona intermedia") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                            if("Alona barbulata" %in% temp_data2$Taxa){
                              alona_spp_prop[row_idx,8] <- temp_data2 %>% 
                                filter(Taxa == "Alona barbulata") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              alona_spp_prop[row_idx,9] <- temp_data2 %>% 
                                filter(Taxa == "Alona barbulata") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                            if("Alona costata" %in% temp_data2$Taxa){
                              alona_spp_prop[row_idx,10] <- temp_data2 %>% 
                                filter(Taxa == "Alona costata") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              alona_spp_prop[row_idx,11] <- temp_data2 %>% 
                                filter(Taxa == "Alona costata") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                            if("Alona rustica" %in% temp_data2$Taxa){
                              alona_spp_prop[row_idx,12] <- temp_data2 %>% 
                                filter(Taxa == "Alona rustica") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              alona_spp_prop[row_idx,13] <- temp_data2 %>% 
                                filter(Taxa == "Alona rustica") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                            if("Alona circumfimbriata, guttata, or setulosa" %in% temp_data2$Taxa){
                              alona_spp_prop[row_idx,14] <- temp_data2 %>% 
                                filter(Taxa == "Alona circumfimbriata, guttata, or setulosa") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              alona_spp_prop[row_idx,15] <- temp_data2 %>% 
                                filter(Taxa == "Alona circumfimbriata, guttata, or setulosa") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                          }
                          #increase the row_idx value for the next row
                          row_idx <- row_idx + 1
                        }
                        #turn NA values into 0
                        alona_spp_prop[is.na(alona_spp_prop)] <- 0 
                        
                        #make this matrix a data frame
                        alona_spp_prop <- as.data.frame(alona_spp_prop)

                        
              #4: ratio of all aloninae species to infer when we can only get to "Aloninae" due to remain preservation 
                       
                  #CARAPACES: Includes all aloninae carapaces that are similar (VERY distinct or unpreserved carapaces are not included = Leydigia leydigi and Graptoleberis testudinaria)      
                        
                         #make empty matrix to store results
                        aloninae_carap_prop <- matrix(NA, nrow = length(lakes), ncol = 21, dimnames = list(NULL, c("LakeName", "a.affinis.n", "a.affinis.prop", "a.quad.n", "a.quad.prop", "a.inter.n", "a.inter.prop", "a.barb.n", "a.barb.prop", "a.cost.n", "a.cost.prop", "a.rust.n", "a.rust.prop", "a.circ.gutt.setu.n", "a.circ.gutt.setu.prop", "a.harp.n", "a.harp.prop", "campto.n", "campto.prop", "a.amer.n", "a.amer.prop")))
                        #row index tracker - need to do this because looping over categorical data and lakenames can't be rows later on
                        row_idx <- 1
                        #calculate and store proportions for each lake
                        for(i in unique(Sed_Data_Clean$LakeName)){
                          #isolates data you want - this only includes the structures that can be reliably used for certain species
                          temp_data1 <- Sed_Data_Clean %>% 
                            filter(LakeName == i & (((Remain.Type == "Postabdomen (no claw)" | Remain.Type == "Headshield") & (Taxa == "Alona affinis" | Taxa == "Alona quadrangularis" | Taxa == "Alona intermedia" | Taxa == "Alona barbulata" | Taxa == "Alona costata" | Taxa == "Alona rustica" | Taxa == "Alona circumfimbriata, guttata, or setulosa")) | ((Remain.Type == "Postabdomen (no claw)" | Remain.Type == "Carapace" | Remain.Type == "Postabdominal Claw") & (Taxa == "Acroperus harpae" | Taxa == "Camptocercus sp.")) | Taxa == "Alonopsis americana"))
                          #IF there are none of these taxa in this sample at all, this will make all values 0 without causing code to stop executing
                          if (nrow(temp_data1) < 1) {
                            aloninae_carap_prop[row_idx,1] <- i
                            aloninae_carap_prop[row_idx,2] <- 0
                            aloninae_carap_prop[row_idx,3] <- 0
                            aloninae_carap_prop[row_idx,4] <- 0
                            aloninae_carap_prop[row_idx,5] <- 0
                            aloninae_carap_prop[row_idx,6] <- 0
                            aloninae_carap_prop[row_idx,7] <- 0
                            aloninae_carap_prop[row_idx,8] <- 0
                            aloninae_carap_prop[row_idx,9] <- 0
                            aloninae_carap_prop[row_idx,10] <- 0
                            aloninae_carap_prop[row_idx,11] <- 0
                            aloninae_carap_prop[row_idx,12] <- 0
                            aloninae_carap_prop[row_idx,13] <- 0
                            aloninae_carap_prop[row_idx,14] <- 0
                            aloninae_carap_prop[row_idx,15] <- 0
                            aloninae_carap_prop[row_idx,16] <- 0
                            aloninae_carap_prop[row_idx,17] <- 0
                            aloninae_carap_prop[row_idx,18] <- 0
                            aloninae_carap_prop[row_idx,19] <- 0
                            aloninae_carap_prop[row_idx,20] <- 0
                            aloninae_carap_prop[row_idx,21] <- 0
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
                              mutate(Freq = ifelse((Remain.Type == "Postabdominal Claw" | Remain.Type == "Carapace"), ceiling(Freq/2), Freq)) %>%  #divide postabdominal claw and carapace counts by 2 and then round up (accounts for the fact that there are 2 claws and 2 carapace sides on each zoop)
                              group_by(Taxa) %>% #group by taxa
                              summarise(n = max(Freq)) %>% #keep only the count of the most frequent remain type for each taxa
                              mutate(prop = n / sum(n))
                            #save the values from this calculation in the matrix
                            #save lakename
                            aloninae_carap_prop[row_idx,1] <- i
                            #save n and proportion for each taxa present in data - this version extracts the values out of 1x1 tibbles
                            if("Alona affinis" %in% temp_data2$Taxa){
                              aloninae_carap_prop[row_idx,2] <- temp_data2 %>% 
                                filter(Taxa == "Alona affinis") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              aloninae_carap_prop[row_idx,3] <- temp_data2 %>% 
                                filter(Taxa == "Alona affinis") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                            if("Alona quadrangularis" %in% temp_data2$Taxa){
                              aloninae_carap_prop[row_idx,4] <- temp_data2 %>% 
                                filter(Taxa == "Alona quadrangularis") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              aloninae_carap_prop[row_idx,5] <- temp_data2 %>% 
                                filter(Taxa == "Alona quadrangularis") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                            if("Alona intermedia" %in% temp_data2$Taxa){
                              aloninae_carap_prop[row_idx,6] <- temp_data2 %>% 
                                filter(Taxa == "Alona intermedia") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              aloninae_carap_prop[row_idx,7] <- temp_data2 %>% 
                                filter(Taxa == "Alona intermedia") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                            if("Alona barbulata" %in% temp_data2$Taxa){
                              aloninae_carap_prop[row_idx,8] <- temp_data2 %>% 
                                filter(Taxa == "Alona barbulata") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              aloninae_carap_prop[row_idx,9] <- temp_data2 %>% 
                                filter(Taxa == "Alona barbulata") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                            if("Alona costata" %in% temp_data2$Taxa){
                              aloninae_carap_prop[row_idx,10] <- temp_data2 %>% 
                                filter(Taxa == "Alona costata") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              aloninae_carap_prop[row_idx,11] <- temp_data2 %>% 
                                filter(Taxa == "Alona costata") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                            if("Alona rustica" %in% temp_data2$Taxa){
                              aloninae_carap_prop[row_idx,12] <- temp_data2 %>% 
                                filter(Taxa == "Alona rustica") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              aloninae_carap_prop[row_idx,13] <- temp_data2 %>% 
                                filter(Taxa == "Alona rustica") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                            if("Alona circumfimbriata, guttata, or setulosa" %in% temp_data2$Taxa){
                              aloninae_carap_prop[row_idx,14] <- temp_data2 %>% 
                                filter(Taxa == "Alona circumfimbriata, guttata, or setulosa") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              aloninae_carap_prop[row_idx,15] <- temp_data2 %>% 
                                filter(Taxa == "Alona circumfimbriata, guttata, or setulosa") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                            if("Acroperus harpae" %in% temp_data2$Taxa){
                              aloninae_carap_prop[row_idx,16] <- temp_data2 %>% 
                                filter(Taxa == "Acroperus harpae") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              aloninae_carap_prop[row_idx,17] <- temp_data2 %>% 
                                filter(Taxa == "Acroperus harpae") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                            if("Camptocercus sp." %in% temp_data2$Taxa){
                              aloninae_carap_prop[row_idx,18] <- temp_data2 %>% 
                                filter(Taxa == "Camptocercus sp.") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              aloninae_carap_prop[row_idx,19] <- temp_data2 %>% 
                                filter(Taxa == "Camptocercus sp.") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                            if("Alonopsis americana" %in% temp_data2$Taxa){
                              aloninae_carap_prop[row_idx,20] <- temp_data2 %>% 
                                filter(Taxa == "Alonopsis americana") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              aloninae_carap_prop[row_idx,21] <- temp_data2 %>% 
                                filter(Taxa == "Alonopsis americana") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                            
                          }
                          
                          #increase the row_idx value for the next row
                          row_idx <- row_idx + 1
                        }
                        #turn NA values into 0
                        aloninae_carap_prop[is.na(aloninae_carap_prop)] <- 0 
                        
                        #make this matrix a data frame
                        aloninae_carap_prop <- as.data.frame(aloninae_carap_prop)
                        
                        
              #POSTABDOMENS: Includes all aloninae postabdomens that are similar (VERY distinct postabdomens are not included = Alonopsis americana, Camptocercus sp., Acroperus harpae, Leydigia leydigi and Graptoleberis testudinaria)      
                        
                        #This actually just leaves you with the exact same proportions as the Alona sp. proportions we already calculated for carapaces, so use that
                        
              #HEADSHIELDS: Includes all aloninae headshields that are similar (VERY distinct headshields are not included = Acroperus harpae, Camptocercuss sp., and Graptoleberis testudinaria)      
                        
                        #make empty matrix to store results
                        aloninae_head_prop <- matrix(NA, nrow = length(lakes), ncol = 19, dimnames = list(NULL, c("LakeName", "a.affinis.n", "a.affinis.prop", "a.quad.n", "a.quad.prop", "a.inter.n", "a.inter.prop", "a.barb.n", "a.barb.prop", "a.cost.n", "a.cost.prop", "a.rust.n", "a.rust.prop", "a.circ.gutt.setu.n", "a.circ.gutt.setu.prop", "a.amer.n", "a.amer.prop", "l.leyd.n", "l.leyd.prop")))
                        #row index tracker - need to do this because looping over categorical data and lakenames can't be rows later on
                        row_idx <- 1
                        #calculate and store proportions for each lake
                        for(i in unique(Sed_Data_Clean$LakeName)){
                          #isolates data you want - this only includes the structures that can be reliably used for certain species
                          temp_data1 <- Sed_Data_Clean %>% 
                            filter(LakeName == i & (((Remain.Type == "Postabdomen (no claw)" | Remain.Type == "Headshield") & (Taxa == "Alona affinis" | Taxa == "Alona quadrangularis" | Taxa == "Alona intermedia" | Taxa == "Alona barbulata" | Taxa == "Alona costata" | Taxa == "Alona rustica" | Taxa == "Alona circumfimbriata, guttata, or setulosa")) | Taxa == "Alonopsis americana" | Taxa == "Leydigia leydigi"))
                          #IF there are none of these taxa in this sample at all, this will make all values 0 without causing code to stop executing
                          if (nrow(temp_data1) < 1) {
                            aloninae_head_prop[row_idx,1] <- i
                            aloninae_head_prop[row_idx,2] <- 0
                            aloninae_head_prop[row_idx,3] <- 0
                            aloninae_head_prop[row_idx,4] <- 0
                            aloninae_head_prop[row_idx,5] <- 0
                            aloninae_head_prop[row_idx,6] <- 0
                            aloninae_head_prop[row_idx,7] <- 0
                            aloninae_head_prop[row_idx,8] <- 0
                            aloninae_head_prop[row_idx,9] <- 0
                            aloninae_head_prop[row_idx,10] <- 0
                            aloninae_head_prop[row_idx,11] <- 0
                            aloninae_head_prop[row_idx,12] <- 0
                            aloninae_head_prop[row_idx,13] <- 0
                            aloninae_head_prop[row_idx,14] <- 0
                            aloninae_head_prop[row_idx,15] <- 0
                            aloninae_head_prop[row_idx,16] <- 0
                            aloninae_head_prop[row_idx,17] <- 0
                            aloninae_head_prop[row_idx,18] <- 0
                            aloninae_head_prop[row_idx,19] <- 0
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
                              mutate(Freq = ifelse((Remain.Type == "Postabdominal Claw" | Remain.Type == "Carapace"), ceiling(Freq/2), Freq)) %>%  #divide postabdominal claw and carapace counts by 2 and then round up (accounts for the fact that there are 2 claws and 2 carapace sides on each zoop)
                              group_by(Taxa) %>% #group by taxa
                              summarise(n = max(Freq)) %>% #keep only the count of the most frequent remain type for each taxa
                              mutate(prop = n / sum(n))
                            #save the values from this calculation in the matrix
                            #save lakename
                            aloninae_head_prop[row_idx,1] <- i
                            #save n and proportion for each taxa present in data - this version extracts the values out of 1x1 tibbles
                            if("Alona affinis" %in% temp_data2$Taxa){
                              aloninae_head_prop[row_idx,2] <- temp_data2 %>% 
                                filter(Taxa == "Alona affinis") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              aloninae_head_prop[row_idx,3] <- temp_data2 %>% 
                                filter(Taxa == "Alona affinis") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                            if("Alona quadrangularis" %in% temp_data2$Taxa){
                              aloninae_head_prop[row_idx,4] <- temp_data2 %>% 
                                filter(Taxa == "Alona quadrangularis") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              aloninae_head_prop[row_idx,5] <- temp_data2 %>% 
                                filter(Taxa == "Alona quadrangularis") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                            if("Alona intermedia" %in% temp_data2$Taxa){
                              aloninae_head_prop[row_idx,6] <- temp_data2 %>% 
                                filter(Taxa == "Alona intermedia") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              aloninae_head_prop[row_idx,7] <- temp_data2 %>% 
                                filter(Taxa == "Alona intermedia") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                            if("Alona barbulata" %in% temp_data2$Taxa){
                              aloninae_head_prop[row_idx,8] <- temp_data2 %>% 
                                filter(Taxa == "Alona barbulata") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              aloninae_head_prop[row_idx,9] <- temp_data2 %>% 
                                filter(Taxa == "Alona barbulata") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                            if("Alona costata" %in% temp_data2$Taxa){
                              aloninae_head_prop[row_idx,10] <- temp_data2 %>% 
                                filter(Taxa == "Alona costata") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              aloninae_head_prop[row_idx,11] <- temp_data2 %>% 
                                filter(Taxa == "Alona costata") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                            if("Alona rustica" %in% temp_data2$Taxa){
                              aloninae_head_prop[row_idx,12] <- temp_data2 %>% 
                                filter(Taxa == "Alona rustica") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              aloninae_head_prop[row_idx,13] <- temp_data2 %>% 
                                filter(Taxa == "Alona rustica") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                            if("Alona circumfimbriata, guttata, or setulosa" %in% temp_data2$Taxa){
                              aloninae_head_prop[row_idx,14] <- temp_data2 %>% 
                                filter(Taxa == "Alona circumfimbriata, guttata, or setulosa") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              aloninae_head_prop[row_idx,15] <- temp_data2 %>% 
                                filter(Taxa == "Alona circumfimbriata, guttata, or setulosa") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                            if("Alonopsis americana" %in% temp_data2$Taxa){
                              aloninae_head_prop[row_idx,16] <- temp_data2 %>% 
                                filter(Taxa == "Alonopsis americana") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              aloninae_head_prop[row_idx,17] <- temp_data2 %>% 
                                filter(Taxa == "Alonopsis americana") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                            if("Leydigia leydigi" %in% temp_data2$Taxa){
                              aloninae_head_prop[row_idx,18] <- temp_data2 %>% 
                                filter(Taxa == "Leydigia leydigi") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              aloninae_head_prop[row_idx,19] <- temp_data2 %>% 
                                filter(Taxa == "Leydigia leydigi") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                          }
                          #increase the row_idx value for the next row
                          row_idx <- row_idx + 1
                        }
                        #turn NA values into 0
                        aloninae_head_prop[is.na(aloninae_head_prop)] <- 0 
                        
                        #make this matrix a data frame
                        aloninae_head_prop <- as.data.frame(aloninae_head_prop)
                        
                        
           #CHYDORINAE: ratio of all chydorinae species to infer when we can only get to "chydorinae" due to remain preservation
                        
                #POSTABDOMENS: includes all chydorinae because all quite similar
                        
                        #make empty matrix to store results
                        chydorinae_prop <- matrix(NA, nrow = length(lakes), ncol = 17, dimnames = list(NULL, c("LakeName", "a.excisa.n", "a.excisa.prop", "a.nana.n", "a.nana.prop", "a.pulch.n", "a.pulch.prop", "c.brevil.n", "c.brevil.prop", "c.biov.n", "c.biov.prop", "p.proc.n", "p.proc.prop", "p.stram.n", "p.stram.prop", "p.trig.n", "p.trig.prop")))
                        #row index tracker - need to do this because looping over categorical data and lakenames can't be rows later on
                        row_idx <- 1
                        #calculate and store proportions for each lake
                        for(i in unique(Sed_Data_Clean$LakeName)){
                          #isolates data you want - no restrictions on which structures can be used for ID with this subfamily
                          temp_data1 <- Sed_Data_Clean %>% 
                            filter(LakeName == i & (Taxa == "Alonella excisa" | Taxa == "Alonella nana" | Taxa == "Alonella pulchella" | Taxa == "Chydorus brevilabris" | Taxa == "Chydorus biovatus" | Taxa == "Pleuroxus procurvus" | Taxa == "Pleuroxus straminius" | Taxa == "Pleuroxus trigonellus"))
                          #IF there are none of these taxa in this sample at all, this will make all values 0 without causing code to stop executing
                          if (nrow(temp_data1) < 1) {
                            chydorinae_prop[row_idx,1] <- i
                            chydorinae_prop[row_idx,2] <- 0
                            chydorinae_prop[row_idx,3] <- 0
                            chydorinae_prop[row_idx,4] <- 0
                            chydorinae_prop[row_idx,5] <- 0
                            chydorinae_prop[row_idx,6] <- 0
                            chydorinae_prop[row_idx,7] <- 0
                            chydorinae_prop[row_idx,8] <- 0
                            chydorinae_prop[row_idx,9] <- 0
                            chydorinae_prop[row_idx,10] <- 0
                            chydorinae_prop[row_idx,11] <- 0
                            chydorinae_prop[row_idx,12] <- 0
                            chydorinae_prop[row_idx,13] <- 0
                            chydorinae_prop[row_idx,14] <- 0
                            chydorinae_prop[row_idx,15] <- 0
                            chydorinae_prop[row_idx,16] <- 0
                            chydorinae_prop[row_idx,17] <- 0
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
                              mutate(Freq = ifelse((Remain.Type == "Postabdominal Claw" | Remain.Type == "Carapace"), ceiling(Freq/2), Freq)) %>%  #divide postabdominal claw and carapace counts by 2 and then round up (accounts for the fact that there are 2 claws and 2 carapace sides on each zoop)
                              group_by(Taxa) %>% #group by taxa
                              summarise(n = max(Freq)) %>% #keep only the count of the most frequent remain type for each taxa
                              mutate(prop = n / sum(n))
                            #save the values from this calculation in the matrix
                            #save lakename
                            chydorinae_prop[row_idx,1] <- i
                            #save n and proportion for each taxa present in data - this version extracts the values out of 1x1 tibbles
                            if("Alonella excisa" %in% temp_data2$Taxa){
                              chydorinae_prop[row_idx,2] <- temp_data2 %>% 
                                filter(Taxa == "Alonella excisa") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              chydorinae_prop[row_idx,3] <- temp_data2 %>% 
                                filter(Taxa == "Alonella excisa") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                            if("Alonella nana" %in% temp_data2$Taxa){
                              chydorinae_prop[row_idx,4] <- temp_data2 %>% 
                                filter(Taxa == "Alonella nana") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              chydorinae_prop[row_idx,5] <- temp_data2 %>% 
                                filter(Taxa == "Alonella nana") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                            if("Alonella pulchella" %in% temp_data2$Taxa){
                              chydorinae_prop[row_idx,6] <- temp_data2 %>% 
                                filter(Taxa == "Alonella pulchella") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              chydorinae_prop[row_idx,7] <- temp_data2 %>% 
                                filter(Taxa == "Alonella pulchella") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                            if("Chydorus brevilabris" %in% temp_data2$Taxa){
                              chydorinae_prop[row_idx,8] <- temp_data2 %>% 
                                filter(Taxa == "Chydorus brevilabris") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              chydorinae_prop[row_idx,9] <- temp_data2 %>% 
                                filter(Taxa == "Chydorus brevilabris") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                            if("Chydorus biovatus" %in% temp_data2$Taxa){
                              chydorinae_prop[row_idx,10] <- temp_data2 %>% 
                                filter(Taxa == "Chydorus biovatus") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              chydorinae_prop[row_idx,11] <- temp_data2 %>% 
                                filter(Taxa == "Chydorus biovatus") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                            if("Pleuroxus procurvus" %in% temp_data2$Taxa){
                              chydorinae_prop[row_idx,12] <- temp_data2 %>% 
                                filter(Taxa == "Pleuroxus procurvus") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              chydorinae_prop[row_idx,13] <- temp_data2 %>% 
                                filter(Taxa == "Pleuroxus procurvus") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                            if("Pleuroxus straminius" %in% temp_data2$Taxa){
                              chydorinae_prop[row_idx,14] <- temp_data2 %>% 
                                filter(Taxa == "Pleuroxus straminius") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              chydorinae_prop[row_idx,15] <- temp_data2 %>% 
                                filter(Taxa == "Pleuroxus straminius") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                            if("Pleuroxus trigonellus" %in% temp_data2$Taxa){
                              chydorinae_prop[row_idx,16] <- temp_data2 %>% 
                                filter(Taxa == "Pleuroxus trigonellus") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              chydorinae_prop[row_idx,17] <- temp_data2 %>% 
                                filter(Taxa == "Pleuroxus trigonellus") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                          }
                          #increase the row_idx value for the next row
                          row_idx <- row_idx + 1
                        }
                        #turn NA values into 0
                        chydorinae_prop[is.na(chydorinae_prop)] <- 0 
                        
                        #make this matrix a data frame
                        chydorinae_prop <- as.data.frame(chydorinae_prop)
                        
                        
              #HEADSHIELDS: excludes Alonella nana and Pleuroxus trigonellus because those two are very different (left the other Pleuroxus species because their posterior headshield regions are not distinct)
                        
                        #make empty matrix to store results
                        chydorinae_head_prop <- matrix(NA, nrow = length(lakes), ncol = 13, dimnames = list(NULL, c("LakeName", "a.excisa.n", "a.excisa.prop", "a.pulch.n", "a.pulch.prop", "c.brevil.n", "c.brevil.prop", "c.biov.n", "c.biov.prop", "p.proc.n", "p.proc.prop", "p.stram.n", "p.stram.prop")))
                        #row index tracker - need to do this because looping over categorical data and lakenames can't be rows later on
                        row_idx <- 1
                        #calculate and store proportions for each lake
                        for(i in unique(Sed_Data_Clean$LakeName)){
                          #isolates data you want - no restrictions on which structures can be used for ID with this subfamily
                          temp_data1 <- Sed_Data_Clean %>% 
                            filter(LakeName == i & (Taxa == "Alonella excisa" | Taxa == "Alonella pulchella" | Taxa == "Chydorus brevilabris" | Taxa == "Chydorus biovatus" | Taxa == "Pleuroxus procurvus" | Taxa == "Pleuroxus straminius"))
                          #IF there are none of these taxa in this sample at all, this will make all values 0 without causing code to stop executing
                          if (nrow(temp_data1) < 1) {
                            chydorinae_prop[row_idx,1] <- i
                            chydorinae_prop[row_idx,2] <- 0
                            chydorinae_prop[row_idx,3] <- 0
                            chydorinae_prop[row_idx,4] <- 0
                            chydorinae_prop[row_idx,5] <- 0
                            chydorinae_prop[row_idx,6] <- 0
                            chydorinae_prop[row_idx,7] <- 0
                            chydorinae_prop[row_idx,8] <- 0
                            chydorinae_prop[row_idx,9] <- 0
                            chydorinae_prop[row_idx,10] <- 0
                            chydorinae_prop[row_idx,11] <- 0
                            chydorinae_prop[row_idx,12] <- 0
                            chydorinae_prop[row_idx,13] <- 0
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
                              mutate(Freq = ifelse((Remain.Type == "Postabdominal Claw" | Remain.Type == "Carapace"), ceiling(Freq/2), Freq)) %>%  #divide postabdominal claw and carapace counts by 2 and then round up (accounts for the fact that there are 2 claws and 2 carapace sides on each zoop)
                              group_by(Taxa) %>% #group by taxa
                              summarise(n = max(Freq)) %>% #keep only the count of the most frequent remain type for each taxa
                              mutate(prop = n / sum(n))
                            #save the values from this calculation in the matrix
                            #save lakename
                            chydorinae_head_prop[row_idx,1] <- i
                            #save n and proportion for each taxa present in data - this version extracts the values out of 1x1 tibbles
                            if("Alonella excisa" %in% temp_data2$Taxa){
                              chydorinae_head_prop[row_idx,2] <- temp_data2 %>% 
                                filter(Taxa == "Alonella excisa") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              chydorinae_head_prop[row_idx,3] <- temp_data2 %>% 
                                filter(Taxa == "Alonella excisa") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                            if("Alonella pulchella" %in% temp_data2$Taxa){
                              chydorinae_head_prop[row_idx,4] <- temp_data2 %>% 
                                filter(Taxa == "Alonella pulchella") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              chydorinae_head_prop[row_idx,5] <- temp_data2 %>% 
                                filter(Taxa == "Alonella pulchella") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                            if("Chydorus brevilabris" %in% temp_data2$Taxa){
                              chydorinae_head_prop[row_idx,6] <- temp_data2 %>% 
                                filter(Taxa == "Chydorus brevilabris") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              chydorinae_head_prop[row_idx,7] <- temp_data2 %>% 
                                filter(Taxa == "Chydorus brevilabris") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                            if("Chydorus biovatus" %in% temp_data2$Taxa){
                              chydorinae_head_prop[row_idx,8] <- temp_data2 %>% 
                                filter(Taxa == "Chydorus biovatus") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              chydorinae_head_prop[row_idx,9] <- temp_data2 %>% 
                                filter(Taxa == "Chydorus biovatus") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                            if("Pleuroxus procurvus" %in% temp_data2$Taxa){
                              chydorinae_head_prop[row_idx,10] <- temp_data2 %>% 
                                filter(Taxa == "Pleuroxus procurvus") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              chydorinae_head_prop[row_idx,11] <- temp_data2 %>% 
                                filter(Taxa == "Pleuroxus procurvus") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                            if("Pleuroxus straminius" %in% temp_data2$Taxa){
                              chydorinae_head_prop[row_idx,12] <- temp_data2 %>% 
                                filter(Taxa == "Pleuroxus straminius") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              chydorinae_head_prop[row_idx,13] <- temp_data2 %>% 
                                filter(Taxa == "Pleuroxus straminius") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                          }
                          #increase the row_idx value for the next row
                          row_idx <- row_idx + 1
                        }
                        #turn NA values into 0
                        chydorinae_head_prop[is.na(chydorinae_head_prop)] <- 0 
                        
                        #make this matrix a data frame
                        chydorinae_head_prop <- as.data.frame(chydorinae_head_prop)
                        
                        
                  #CARAPACES: excludes Alonella nana and all Plueroxus spp. because they are quite distinct
                        
                        #make empty matrix to store results
                        chydorinae_carap_prop <- matrix(NA, nrow = length(lakes), ncol = 9, dimnames = list(NULL, c("LakeName", "a.excisa.n", "a.excisa.prop", "a.pulch.n", "a.pulch.prop", "c.brevil.n", "c.brevil.prop", "c.biov.n", "c.biov.prop")))
                        #row index tracker - need to do this because looping over categorical data and lakenames can't be rows later on
                        row_idx <- 1
                        #calculate and store proportions for each lake
                        for(i in unique(Sed_Data_Clean$LakeName)){
                          #isolates data you want - no restrictions on which structures can be used for ID with this subfamily
                          temp_data1 <- Sed_Data_Clean %>% 
                            filter(LakeName == i & (Taxa == "Alonella excisa"| Taxa == "Alonella pulchella" | Taxa == "Chydorus brevilabris" | Taxa == "Chydorus biovatus"))
                          #IF there are none of these taxa in this sample at all, this will make all values 0 without causing code to stop executing
                          if (nrow(temp_data1) < 1) {
                            chydorinae_carap_prop[row_idx,1] <- i
                            chydorinae_carap_prop[row_idx,2] <- 0
                            chydorinae_carap_prop[row_idx,3] <- 0
                            chydorinae_carap_prop[row_idx,4] <- 0
                            chydorinae_carap_prop[row_idx,5] <- 0
                            chydorinae_carap_prop[row_idx,6] <- 0
                            chydorinae_carap_prop[row_idx,7] <- 0
                            chydorinae_carap_prop[row_idx,8] <- 0
                            chydorinae_carap_prop[row_idx,9] <- 0
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
                              mutate(Freq = ifelse((Remain.Type == "Postabdominal Claw" | Remain.Type == "Carapace"), ceiling(Freq/2), Freq)) %>%  #divide postabdominal claw and carapace counts by 2 and then round up (accounts for the fact that there are 2 claws and 2 carapace sides on each zoop)
                              group_by(Taxa) %>% #group by taxa
                              summarise(n = max(Freq)) %>% #keep only the count of the most frequent remain type for each taxa
                              mutate(prop = n / sum(n))
                            #save the values from this calculation in the matrix
                            #save lakename
                            chydorinae_carap_prop[row_idx,1] <- i
                            #save n and proportion for each taxa present in data - this version extracts the values out of 1x1 tibbles
                            if("Alonella excisa" %in% temp_data2$Taxa){
                              chydorinae_carap_prop[row_idx,2] <- temp_data2 %>% 
                                filter(Taxa == "Alonella excisa") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              chydorinae_carap_prop[row_idx,3] <- temp_data2 %>% 
                                filter(Taxa == "Alonella excisa") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                            if("Alonella pulchella" %in% temp_data2$Taxa){
                              chydorinae_carap_prop[row_idx,4] <- temp_data2 %>% 
                                filter(Taxa == "Alonella pulchella") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              chydorinae_carap_prop[row_idx,5] <- temp_data2 %>% 
                                filter(Taxa == "Alonella pulchella") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                            if("Chydorus brevilabris" %in% temp_data2$Taxa){
                              chydorinae_carap_prop[row_idx,6] <- temp_data2 %>% 
                                filter(Taxa == "Chydorus brevilabris") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              chydorinae_carap_prop[row_idx,7] <- temp_data2 %>% 
                                filter(Taxa == "Chydorus brevilabris") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                            if("Chydorus biovatus" %in% temp_data2$Taxa){
                              chydorinae_carap_prop[row_idx,8] <- temp_data2 %>% 
                                filter(Taxa == "Chydorus biovatus") %>% 
                                pull(n) %>% 
                                .[1]
                              
                              chydorinae_carap_prop[row_idx,9] <- temp_data2 %>% 
                                filter(Taxa == "Chydorus biovatus") %>% 
                                pull(prop) %>% 
                                .[1]
                            }
                          }
                          #increase the row_idx value for the next row
                          row_idx <- row_idx + 1
                        }
                        #turn NA values into 0
                        chydorinae_carap_prop[is.na(chydorinae_carap_prop)] <- 0 
                        
                        #make this matrix a data frame
                        chydorinae_carap_prop <- as.data.frame(chydorinae_carap_prop)
                        


#DEAL WITH SPECIMENS THAT NEED TO BE ASSGINED AN ID BASED ON PROPORTIONS OF OTHER BETTER IDed STRUCTURES IN THAT SAMPLE/LAKE
  #I will do this by taking random samples from a distribution with probabilities assigned by those calculated above
        
        #first make a new data frame to start reassigning things
            Sed_Data_Reassigned <- Sed_Data_Clean
        #set seed to keep random results consistent
            set.seed(1234567)

                                        
        #Bosminid headshields that can't be IDed due to unclear/obscured headpores

              #reassign based on proportions in each lake            
              for(i in unique(Sed_Data_Reassigned$LakeName)){
                #assign the calculated proportions for each lake
                Proportions <- c("Bosmina longirostris" = bosminid_head_prop[bosminid_head_prop$LakeName == i, "b.longi.prop"],
                                 "Eubosmina coregoni" = bosminid_head_prop[bosminid_head_prop$LakeName == i, "e.coreg.prop"]
                                 )
                #make proportions numeric
                Proportions.num <- as.numeric(Proportions)
                #Check that there are no NA values in Proportions
                if(anyNA(Proportions.num)){
                  stop(paste("Error: Proportions contain NA in sample:", i))
                }
                #Check that proportions add to one (with tolerance for rounding)
                if((abs(sum(Proportions.num)) - 1) > 0.000000001){
                  stop(paste("Error: Proportions do not sum to 1 in sample:", i))
                }   
                #create a logical vector of the rows you want to reassign, need to specifically say that LakeName = i so it only selects for the correct lake
                reassign <- (Sed_Data_Reassigned$Remain.Type == "Headshield") & (Sed_Data_Reassigned$Taxa == "Bosminid") & (Sed_Data_Reassigned$LakeName == i)
                  
                #sample from distribution and reassign the taxa only in the rows you selected above, when this taxa is present and proportions could be made
                #if there are none of the specified taxa in that sample, the proportions will all be 0 and you will get an error, so adding an if statement solves that
                if(sum(Proportions.num) > 0) {
                  Sed_Data_Reassigned$Taxa[reassign] <- sample(
                    names(Proportions), #you are drawing the species names associated with the Proportions vector
                    size = sum(reassign, na.rm = TRUE), #the number of samples to draw is the number of times the reassign logical vector = TRUE, ignoring NA values
                    replace = TRUE, #sample with replacement so the same species can be drawn multiple times
                    prob = Proportions #the probabilities are the values specified in the Proportions vector
                  )
                }
              }

   
        #Bosminid postabdominal claws that need to be assigned based on headshield proportions
            
            #reassign based on proportions in each lake            
            for(i in unique(Sed_Data_Reassigned$LakeName)){
              #assign the calculated proportions for each lake
              Proportions <- c("Bosmina longirostris" = bosminid_head_prop[bosminid_head_prop$LakeName == i, "b.longi.prop"],
                               "Eubosmina coregoni" = bosminid_head_prop[bosminid_head_prop$LakeName == i, "e.coreg.prop"]
              )
              #make proportions numeric
              Proportions.num <- as.numeric(Proportions)
              #Check that there are no NA values in Proportions
              if(anyNA(Proportions.num)){
                stop(paste("Error: Proportions contain NA in sample:", i))
              }
              #Check that proportions add to one (with tolerance for rounding)
              if((abs(sum(Proportions.num)) - 1) > 0.000000001){
                stop(paste("Error: Proportions do not sum to 1 in sample:", i))
              }   
              #create a logical vector of the rows you want to reassign, need to specifically say that LakeName = i so it only selects for the correct lake
              reassign <- ((Sed_Data_Reassigned$Remain.Type == "Postabdominal Claw" | Sed_Data_Reassigned$Remain.Type == "Postabdomen (no claw)") & (Sed_Data_Reassigned$Taxa == "Bosminid") & (Sed_Data_Reassigned$LakeName == i))
              
              #sample from distribution and reassign the taxa only in the rows you selected above, when this taxa is present and proportions could be made
              #if there are none of the specified taxa in that sample, the proportions will all be 0 and you will get an error, so adding an if statement solves that
              if(sum(Proportions.num) > 0) {
                Sed_Data_Reassigned$Taxa[reassign] <- sample(
                  names(Proportions), #you are drawing the species names associated with the Proportions vector
                  size = sum(reassign, na.rm = TRUE), #the number of samples to draw is the number of times the reassign logical vector = TRUE, ignoring NA values
                  replace = TRUE, #sample with replacement so the same species can be drawn multiple times
                  prob = Proportions #the probabilities are the values specified in the Proportions vector
                )
              }
            }
            
            
        #Bosminid antennule segments that need to be assigned based on headshield proportions
            
            #reassign based on proportions in each lake            
            for(i in unique(Sed_Data_Reassigned$LakeName)){
              #assign the calculated proportions for each lake
              Proportions <- c("Bosmina longirostris" = bosminid_head_prop[bosminid_head_prop$LakeName == i, "b.longi.prop"],
                               "Eubosmina coregoni" = bosminid_head_prop[bosminid_head_prop$LakeName == i, "e.coreg.prop"]
              )
              #make proportions numeric
              Proportions.num <- as.numeric(Proportions)
              #Check that there are no NA values in Proportions
              if(anyNA(Proportions.num)){
                stop(paste("Error: Proportions contain NA in sample:", i))
              }
              #Check that proportions add to one (with tolerance for rounding)
              if((abs(sum(Proportions.num)) - 1) > 0.000000001){
                stop(paste("Error: Proportions do not sum to 1 in sample:", i))
              }   
              #create a logical vector of the rows you want to reassign, need to specifically say that LakeName = i so it only selects for the correct lake
              reassign <- (Sed_Data_Reassigned$Remain.Type == "Antennal Segment") & (Sed_Data_Reassigned$Taxa == "Bosminid") & (Sed_Data_Reassigned$LakeName == i)
              
              #sample from distribution and reassign the taxa only in the rows you selected above, when this taxa is present and proportions could be made
              #if there are none of the specified taxa in that sample, the proportions will all be 0 and you will get an error, so adding an if statement solves that
              if(sum(Proportions.num) > 0) {
                Sed_Data_Reassigned$Taxa[reassign] <- sample(
                  names(Proportions), #you are drawing the species names associated with the Proportions vector
                  size = sum(reassign, na.rm = TRUE), #the number of samples to draw is the number of times the reassign logical vector = TRUE, ignoring NA values
                  replace = TRUE, #sample with replacement so the same species can be drawn multiple times
                  prob = Proportions #the probabilities are the values specified in the Proportions vector
                )
              }
            }
        
            
          #Bosminid ephippia that need to be assigned based on headshield proportions
           
            #reassign based on proportions in each lake            
            for(i in unique(Sed_Data_Reassigned$LakeName)){
              #assign the calculated proportions for each lake
              Proportions <- c("Bosmina longirostris" = bosminid_head_prop[bosminid_head_prop$LakeName == i, "b.longi.prop"],
                               "Eubosmina coregoni" = bosminid_head_prop[bosminid_head_prop$LakeName == i, "e.coreg.prop"]
              )
              #make proportions numeric
              Proportions.num <- as.numeric(Proportions)
              #Check that there are no NA values in Proportions
              if(anyNA(Proportions.num)){
                stop(paste("Error: Proportions contain NA in sample:", i))
              }
              #Check that proportions add to one (with tolerance for rounding)
              if((abs(sum(Proportions.num)) - 1) > 0.000000001){
                stop(paste("Error: Proportions do not sum to 1 in sample:", i))
              }   
              #create a logical vector of the rows you want to reassign, need to specifically say that LakeName = i so it only selects for the correct lake
              reassign <- (Sed_Data_Reassigned$Remain.Type == "Ephippium") & (Sed_Data_Reassigned$Taxa == "Bosminid") & (Sed_Data_Reassigned$LakeName == i)
              
              #sample from distribution and reassign the taxa only in the rows you selected above, when this taxa is present and proportions could be made
              #if there are none of the specified taxa in that sample, the proportions will all be 0 and you will get an error, so adding an if statement solves that
              if(sum(Proportions.num) > 0) {
                Sed_Data_Reassigned$Taxa[reassign] <- sample(
                  names(Proportions), #you are drawing the species names associated with the Proportions vector
                  size = sum(reassign, na.rm = TRUE), #the number of samples to draw is the number of times the reassign logical vector = TRUE, ignoring NA values
                  replace = TRUE, #sample with replacement so the same species can be drawn multiple times
                  prob = Proportions #the probabilities are the values specified in the Proportions vector
                )
              }
            }
        
          #Bosminid carapaces that can't be IDed due to broken off / obscured mucros - assigned based on identifiable carapace proportions
           
             #reassign based on proportions in each lake            
            for(i in unique(Sed_Data_Reassigned$LakeName)){
              #assign the calculated proportions for each lake
              Proportions <- c("Bosmina longirostris" = bosminid_carap_prop[bosminid_carap_prop$LakeName == i, "b.longi.prop"],
                               "Eubosmina coregoni" = bosminid_carap_prop[bosminid_carap_prop$LakeName == i, "e.coreg.prop"]
              )
              #make proportions numeric
              Proportions.num <- as.numeric(Proportions)
              #Check that there are no NA values in Proportions
              if(anyNA(Proportions.num)){
                stop(paste("Error: Proportions contain NA in sample:", i))
              }
              #Check that proportions add to one (with tolerance for rounding)
              if((abs(sum(Proportions.num)) - 1) > 0.000000001){
                stop(paste("Error: Proportions do not sum to 1 in sample:", i))
              }   
              #create a logical vector of the rows you want to reassign, need to specifically say that LakeName = i so it only selects for the correct lake
              reassign <- (Sed_Data_Reassigned$Remain.Type == "Carapace") & (Sed_Data_Reassigned$Taxa == "Bosminid") & (Sed_Data_Reassigned$LakeName == i)
              
              #sample from distribution and reassign the taxa only in the rows you selected above, when this taxa is present and proportions could be made
              #if there are none of the specified taxa in that sample, the proportions will all be 0 and you will get an error, so adding an if statement solves that
              if(sum(Proportions.num) > 0) {
                Sed_Data_Reassigned$Taxa[reassign] <- sample(
                  names(Proportions), #you are drawing the species names associated with the Proportions vector
                  size = sum(reassign, na.rm = TRUE), #the number of samples to draw is the number of times the reassign logical vector = TRUE, ignoring NA values
                  replace = TRUE, #sample with replacement so the same species can be drawn multiple times
                  prob = Proportions #the probabilities are the values specified in the Proportions vector
                )
              }
            }
        
        
        
        #Daphnia postab claws that can't be IDed because they are broken or obscured - assign based on identifiable claw proportions
        
            #reassign based on proportions in each lake            
            for(i in unique(Sed_Data_Reassigned$LakeName)){
              #assign the calculated proportions for each lake
              Proportions <- c("Daphnia longispina complex" = daphnia_prop[daphnia_prop$LakeName == i, "d.longi.prop"],
                               "Daphnia pulex complex" = daphnia_prop[daphnia_prop$LakeName == i, "d.pulex.prop"]
              )
              #make proportions numeric
              Proportions.num <- as.numeric(Proportions)
              #Check that there are no NA values in Proportions
              if(anyNA(Proportions.num)){
                stop(paste("Error: Proportions contain NA in sample:", i))
              }
              #Check that proportions add to one (with tolerance for rounding)
              if((abs(sum(Proportions.num)) - 1) > 0.000000001){
                stop(paste("Error: Proportions do not sum to 1 in sample:", i))
              }   
              #create a logical vector of the rows you want to reassign, need to specifically say that LakeName = i so it only selects for the correct lake
              reassign <- (Sed_Data_Reassigned$Remain.Type == "Postabdominal Claw" | Sed_Data_Reassigned$Remain.Type == "Postabdomen (no claw)") & (Sed_Data_Reassigned$Taxa == "Daphnia sp.") & (Sed_Data_Reassigned$LakeName == i)
              
              #sample from distribution and reassign the taxa only in the rows you selected above, when this taxa is present and proportions could be made
              #if there are none of the specified taxa in that sample, the proportions will all be 0 and you will get an error, so adding an if statement solves that
              if(sum(Proportions.num) > 0) {
                Sed_Data_Reassigned$Taxa[reassign] <- sample(
                  names(Proportions), #you are drawing the species names associated with the Proportions vector
                  size = sum(reassign, na.rm = TRUE), #the number of samples to draw is the number of times the reassign logical vector = TRUE, ignoring NA values
                  replace = TRUE, #sample with replacement so the same species can be drawn multiple times
                  prob = Proportions #the probabilities are the values specified in the Proportions vector
                  )
              }
            }
        
        #Camptocercus / Acroperus headshields that need to be assigned based on carapaces and postab claws
            
            #reassign based on proportions in each lake            
            for(i in unique(Sed_Data_Reassigned$LakeName)){
              #assign the calculated proportions for each lake
              Proportions <- c("Acroperus harpae" = campto_acro_prop[campto_acro_prop$LakeName == i, "a.harp.prop"],
                               "Camptocercus sp." = campto_acro_prop[campto_acro_prop$LakeName == i, "campto.prop"]
              )
              #make proportions numeric
              Proportions.num <- as.numeric(Proportions)
              #Check that there are no NA values in Proportions
              if(anyNA(Proportions.num)){
                stop(paste("Error: Proportions contain NA in sample:", i))
              }
              #Check that proportions add to one (with tolerance for rounding)
              if((abs(sum(Proportions.num)) - 1) > 0.000000001){
                stop(paste("Error: Proportions do not sum to 1 in sample:", i))
              }   
              #create a logical vector of the rows you want to reassign, need to specifically say that LakeName = i so it only selects for the correct lake
              reassign <- (Sed_Data_Reassigned$Remain.Type == "Headshield") & (Sed_Data_Reassigned$Taxa == "Camptocercus sp. or Acroperus sp.") & (Sed_Data_Reassigned$LakeName == i)
              
              #sample from distribution and reassign the taxa only in the rows you selected above, when this taxa is present and proportions could be made
              #if there are none of the specified taxa in that sample, the proportions will all be 0 and you will get an error, so adding an if statement solves that
              if(sum(Proportions.num) > 0) {
                Sed_Data_Reassigned$Taxa[reassign] <- sample(
                  names(Proportions), #you are drawing the species names associated with the Proportions vector
                  size = sum(reassign, na.rm = TRUE), #the number of samples to draw is the number of times the reassign logical vector = TRUE, ignoring NA values
                  replace = TRUE, #sample with replacement so the same species can be drawn multiple times
                  prob = Proportions #the probabilities are the values specified in the Proportions vector
                )
              }
            }
        
        
        #Holopedium / Sida postabdomens that need to be assigned based on claws
            
            #reassign based on proportions in each lake            
            for(i in unique(Sed_Data_Reassigned$LakeName)){
              #assign the calculated proportions for each lake
              Proportions <- c("Holopedium sp." = holo_sida_prop[holo_sida_prop$LakeName == i, "holo.prop"],
                               "Sida crystallina americana" = holo_sida_prop[holo_sida_prop$LakeName == i, "sida.prop"]
              )
              #make proportions numeric
              Proportions.num <- as.numeric(Proportions)
              #Check that there are no NA values in Proportions
              if(anyNA(Proportions.num)){
                stop(paste("Error: Proportions contain NA in sample:", i))
              }
              #Check that proportions add to one (with tolerance for rounding)
              if((abs(sum(Proportions.num)) - 1) > 0.000000001){
                stop(paste("Error: Proportions do not sum to 1 in sample:", i))
              }   
              #create a logical vector of the rows you want to reassign, need to specifically say that LakeName = i so it only selects for the correct lake
              reassign <- (Sed_Data_Reassigned$Remain.Type == "Postabdomen (no claw)") & (Sed_Data_Reassigned$Taxa == "Holopedium sp. or Sida crystallina americana") & (Sed_Data_Reassigned$LakeName == i)
              
              #sample from distribution and reassign the taxa only in the rows you selected above, when this taxa is present and proportions could be made
              #if there are none of the specified taxa in that sample, the proportions will all be 0 and you will get an error, so adding an if statement solves that
              if(sum(Proportions.num) > 0) {
                Sed_Data_Reassigned$Taxa[reassign] <- sample(
                  names(Proportions), #you are drawing the species names associated with the Proportions vector
                  size = sum(reassign, na.rm = TRUE), #the number of samples to draw is the number of times the reassign logical vector = TRUE, ignoring NA values
                  replace = TRUE, #sample with replacement so the same species can be drawn multiple times
                  prob = Proportions #the probabilities are the values specified in the Proportions vector
                )
              }
            }
                    #HERE WE HAVE A CASE WHERE THERE IS NO INFO IN THE CARLOS SAMPLE TO INFER WHAT THE POSTABDOMEN MIGHT BE
                    #IT STAYS UNASSIGNED, AND I WILL JUST HAVE TO DEAL WITH THAT LATER
        
        #Alona sp. carapaces (and any other structure that coulsn't be assinged for some reason) that need to be assigned based on other structures
            
            #reassign based on proportions in each lake
            for(i in unique(Sed_Data_Reassigned$LakeName)){
              #assign the calculated proportions for each lake
              Proportions <- c("Alona affinis" = alona_spp_prop[alona_spp_prop$LakeName == i, "a.affinis.prop"],
                               "Alona quadrangularis" = alona_spp_prop[alona_spp_prop$LakeName == i, "a.quad.prop"],
                               "Alona intermedia" = alona_spp_prop[alona_spp_prop$LakeName == i, "a.inter.prop"],
                               "Alona barbulata" = alona_spp_prop[alona_spp_prop$LakeName == i, "a.barb.prop"],
                               "Alona costata" = alona_spp_prop[alona_spp_prop$LakeName == i, "a.cost.prop"],
                               "Alona rustica" = alona_spp_prop[alona_spp_prop$LakeName == i, "a.rust.prop"],
                               "Alona circumfimbriata, guttata, or setulosa" = alona_spp_prop[alona_spp_prop$LakeName == i, "a.circ.gutt.setu.prop"]
              )
              #make proportions numeric
              Proportions.num <- as.numeric(Proportions)
              #Check that there are no NA values in Proportions
              if(anyNA(Proportions.num)){
                stop(paste("Error: Proportions contain NA in sample:", i))
              }
              #Check that proportions add to one (with tolerance for rounding)
              if((abs(sum(Proportions.num)) - 1) > 0.000000001){
                stop(paste("Error: Proportions do not sum to 1 in sample:", i))
              }   
              #create a logical vector of the rows you want to reassign, need to specifically say that LakeName = i so it only selects for the correct lake
              #not specifying structure... I think that's fine because each one is sampled independently based on that sample's proportions
              reassign <- (Sed_Data_Reassigned$Taxa == "Alona sp.") & (Sed_Data_Reassigned$LakeName == i)
              
              #sample from distribution and reassign the taxa only in the rows you selected above, when this taxa is present and proportions could be made
              #if there are none of the specified taxa in that sample, the proportions will all be 0 and you will get an error, so adding an if statement solves that
              if(sum(Proportions.num) > 0) {
                Sed_Data_Reassigned$Taxa[reassign] <- sample(
                  names(Proportions), #you are drawing the species names associated with the Proportions vector
                  size = sum(reassign, na.rm = TRUE), #the number of samples to draw is the number of times the reassign logical vector = TRUE, ignoring NA values
                  replace = TRUE, #sample with replacement so the same species can be drawn multiple times
                  prob = Proportions #the probabilities are the values specified in the Proportions vector
                )
              }
            }
        
        
        #"Aloninae" CARAPACES that can't be IDed further and need to be assigned by proportion of species
        
            #reassign based on proportions in each lake
            for(i in unique(Sed_Data_Reassigned$LakeName)){
              #assign the calculated proportions for each lake
              Proportions <- c("Alona affinis" = aloninae_carap_prop[aloninae_carap_prop$LakeName == i, "a.affinis.prop"],
                               "Alona quadrangularis" = aloninae_carap_prop[aloninae_carap_prop$LakeName == i, "a.quad.prop"],
                               "Alona intermedia" = aloninae_carap_prop[aloninae_carap_prop$LakeName == i, "a.inter.prop"],
                               "Alona barbulata" = aloninae_carap_prop[aloninae_carap_prop$LakeName == i, "a.barb.prop"],
                               "Alona costata" = aloninae_carap_prop[aloninae_carap_prop$LakeName == i, "a.cost.prop"],
                               "Alona rustica" = aloninae_carap_prop[aloninae_carap_prop$LakeName == i, "a.rust.prop"],
                               "Alona circumfimbriata, guttata, or setulosa" = aloninae_carap_prop[aloninae_carap_prop$LakeName == i, "a.circ.gutt.setu.prop"],
                               "Acroperus harpae" = aloninae_carap_prop[aloninae_carap_prop$LakeName == i, "a.harp.prop"],
                               "Camptocercus sp." = aloninae_carap_prop[aloninae_carap_prop$LakeName == i, "campto.prop"],
                               "Alonopsis americana" = aloninae_carap_prop[aloninae_carap_prop$LakeName == i, "a.amer.prop"]
              )
              #make proportions numeric
              Proportions.num <- as.numeric(Proportions)
              #Check that there are no NA values in Proportions
              if(anyNA(Proportions.num)){
                stop(paste("Error: Proportions contain NA in sample:", i))
              }
              #Check that proportions add to one (with tolerance for rounding)
              if((abs(sum(Proportions.num)) - 1) > 0.000000001){
                stop(paste("Error: Proportions do not sum to 1 in sample:", i))
              }   
              #create a logical vector of the rows you want to reassign, need to specifically say that LakeName = i so it only selects for the correct lake
              reassign <- (Sed_Data_Reassigned$Remain.Type == "Carapace") & (Sed_Data_Reassigned$Taxa == "Aloninae") & (Sed_Data_Reassigned$LakeName == i)
              
              #sample from distribution and reassign the taxa only in the rows you selected above, when this taxa is present and proportions could be made
              #if there are none of the specified taxa in that sample, the proportions will all be 0 and you will get an error, so adding an if statement solves that
              if(sum(Proportions.num) > 0) {
                Sed_Data_Reassigned$Taxa[reassign] <- sample(
                  names(Proportions), #you are drawing the species names associated with the Proportions vector
                  size = sum(reassign, na.rm = TRUE), #the number of samples to draw is the number of times the reassign logical vector = TRUE, ignoring NA values
                  replace = TRUE, #sample with replacement so the same species can be drawn multiple times
                  prob = Proportions #the probabilities are the values specified in the Proportions vector
                )
              }
            }
        
        
      #"Aloninae" POSTABDOMENS that can't be IDed further and need to be assigned by proportion of species (note that the only species this is reasonably confusing for is the Alona genus, so using Alona sp. proportions)
            #NOTE: When I wrote this code I did not have any data to try it out on, but it should work because its the same as the Alona sp. carapace code just with different data to reassign
            
            #reassign based on proportions in each lake
            for(i in unique(Sed_Data_Reassigned$LakeName)){
              #assign the calculated proportions for each lake
              Proportions <- c("Alona affinis" = alona_spp_prop[alona_spp_prop$LakeName == i, "a.affinis.prop"],
                               "Alona quadrangularis" = alona_spp_prop[alona_spp_prop$LakeName == i, "a.quad.prop"],
                               "Alona intermedia" = alona_spp_prop[alona_spp_prop$LakeName == i, "a.inter.prop"],
                               "Alona barbulata" = alona_spp_prop[alona_spp_prop$LakeName == i, "a.barb.prop"],
                               "Alona costata" = alona_spp_prop[alona_spp_prop$LakeName == i, "a.cost.prop"],
                               "Alona rustica" = alona_spp_prop[alona_spp_prop$LakeName == i, "a.rust.prop"],
                               "Alona circumfimbriata, guttata, or setulosa" = alona_spp_prop[alona_spp_prop$LakeName == i, "a.circ.gutt.setu.prop"]
              )
              #make proportions numeric
              Proportions.num <- as.numeric(Proportions)
              #Check that there are no NA values in Proportions
              if(anyNA(Proportions.num)){
                stop(paste("Error: Proportions contain NA in sample:", i))
              }
              #Check that proportions add to one (with tolerance for rounding)
              if((abs(sum(Proportions.num)) - 1) > 0.000000001){
                stop(paste("Error: Proportions do not sum to 1 in sample:", i))
              }   
              #create a logical vector of the rows you want to reassign, need to specifically say that LakeName = i so it only selects for the correct lake
              #not specifying structure... I think that's fine because each one is sampled independently based on that sample's proportions
              reassign <- (Sed_Data_Reassigned$Remain.Type == "Postabdomen (no claw)") & (Sed_Data_Reassigned$Taxa == "Aloninae") & (Sed_Data_Reassigned$LakeName == i)
              
              #sample from distribution and reassign the taxa only in the rows you selected above, when this taxa is present and proportions could be made
              #if there are none of the specified taxa in that sample, the proportions will all be 0 and you will get an error, so adding an if statement solves that
              if(sum(Proportions.num) > 0) {
                Sed_Data_Reassigned$Taxa[reassign] <- sample(
                  names(Proportions), #you are drawing the species names associated with the Proportions vector
                  size = sum(reassign, na.rm = TRUE), #the number of samples to draw is the number of times the reassign logical vector = TRUE, ignoring NA values
                  replace = TRUE, #sample with replacement so the same species can be drawn multiple times
                  prob = Proportions #the probabilities are the values specified in the Proportions vector
                )
              }
            }
            
            
      #"Aloninae" HEADSHIELDS that can't be IDed further and need to be assigned by proportion of species
            
            #reassign based on proportions in each lake
            for(i in unique(Sed_Data_Reassigned$LakeName)){
              #assign the calculated proportions for each lake
              Proportions <- c("Alona affinis" = aloninae_head_prop[aloninae_head_prop$LakeName == i, "a.affinis.prop"],
                               "Alona quadrangularis" = aloninae_head_prop[aloninae_head_prop$LakeName == i, "a.quad.prop"],
                               "Alona intermedia" = aloninae_head_prop[aloninae_head_prop$LakeName == i, "a.inter.prop"],
                               "Alona barbulata" = aloninae_head_prop[aloninae_head_prop$LakeName == i, "a.barb.prop"],
                               "Alona costata" = aloninae_head_prop[aloninae_head_prop$LakeName == i, "a.cost.prop"],
                               "Alona rustica" = aloninae_head_prop[aloninae_head_prop$LakeName == i, "a.rust.prop"],
                               "Alona circumfimbriata, guttata, or setulosa" = aloninae_head_prop[aloninae_head_prop$LakeName == i, "a.circ.gutt.setu.prop"],
                               "Alonopsis americana" = aloninae_head_prop[aloninae_head_prop$LakeName == i, "a.amer.prop"],
                               "Leydigia leydigi" = aloninae_head_prop[aloninae_head_prop$LakeName == i, "l.leyd.prop"]
              )
              #make proportions numeric
              Proportions.num <- as.numeric(Proportions)
              #Check that there are no NA values in Proportions
              if(anyNA(Proportions.num)){
                stop(paste("Error: Proportions contain NA in sample:", i))
              }
              #Check that proportions add to one (with tolerance for rounding)
              if((abs(sum(Proportions.num)) - 1) > 0.000000001){
                stop(paste("Error: Proportions do not sum to 1 in sample:", i))
              }   
              #create a logical vector of the rows you want to reassign, need to specifically say that LakeName = i so it only selects for the correct lake
              #not specifying structure... I think that's fine because each one is sampled independently based on that sample's proportions
              reassign <- (Sed_Data_Reassigned$Remain.Type == "Headshield") & (Sed_Data_Reassigned$Taxa == "Aloninae") & (Sed_Data_Reassigned$LakeName == i)
              
              #sample from distribution and reassign the taxa only in the rows you selected above, when this taxa is present and proportions could be made
              #if there are none of the specified taxa in that sample, the proportions will all be 0 and you will get an error, so adding an if statement solves that
              if(sum(Proportions.num) > 0) {
                Sed_Data_Reassigned$Taxa[reassign] <- sample(
                  names(Proportions), #you are drawing the species names associated with the Proportions vector
                  size = sum(reassign, na.rm = TRUE), #the number of samples to draw is the number of times the reassign logical vector = TRUE, ignoring NA values
                  replace = TRUE, #sample with replacement so the same species can be drawn multiple times
                  prob = Proportions #the probabilities are the values specified in the Proportions vector
                )
              }
            }
            
      #"Chydorinae" CARAPACES that can't be IDed further and need to be assigned by proportion of species: excludes Alonella nana and all Plueroxus spp. because they are quite distinct
            
            
            #reassign based on proportions in each lake
            for(i in unique(Sed_Data_Reassigned$LakeName)){
              #assign the calculated proportions for each lake
              Proportions <- c("Alonella excisa" = chydorinae_carap_prop[chydorinae_carap_prop$LakeName == i, "a.excisa.prop"],
                               "Alonella pulchella" = chydorinae_carap_prop[chydorinae_carap_prop$LakeName == i, "a.pulch.prop"],
                               "Chydorus brevilabris" = chydorinae_carap_prop[chydorinae_carap_prop$LakeName == i, "c.brevil.prop"],
                               "Chydorus biovatus" = chydorinae_carap_prop[chydorinae_carap_prop$LakeName == i, "c.biov.prop"]
              )
              #make proportions numeric
              Proportions.num <- as.numeric(Proportions)
              #Check that there are no NA values in Proportions
              if(anyNA(Proportions.num)){
                stop(paste("Error: Proportions contain NA in sample:", i))
              }
              #Check that proportions add to one (with tolerance for rounding)
              if((abs(sum(Proportions.num)) - 1) > 0.000000001){
                stop(paste("Error: Proportions do not sum to 1 in sample:", i))
              }   
              #create a logical vector of the rows you want to reassign, need to specifically say that LakeName = i so it only selects for the correct lake
              #not specifying structure... I think that's fine because each one is sampled independently based on that sample's proportions
              reassign <- (Sed_Data_Reassigned$Remain.Type == "Carapace") & (Sed_Data_Reassigned$Taxa == "Chydorinae") & (Sed_Data_Reassigned$LakeName == i)
              
              #sample from distribution and reassign the taxa only in the rows you selected above, when this taxa is present and proportions could be made
              #if there are none of the specified taxa in that sample, the proportions will all be 0 and you will get an error, so adding an if statement solves that
              if(sum(Proportions.num) > 0) {
                Sed_Data_Reassigned$Taxa[reassign] <- sample(
                  names(Proportions), #you are drawing the species names associated with the Proportions vector
                  size = sum(reassign, na.rm = TRUE), #the number of samples to draw is the number of times the reassign logical vector = TRUE, ignoring NA values
                  replace = TRUE, #sample with replacement so the same species can be drawn multiple times
                  prob = Proportions #the probabilities are the values specified in the Proportions vector
                )
              }
            }     
            
            
            
      #"Chydorinae" POSTABDOMENS that can't be IDed further and need to be assigned by proportion of species: includes all chydorinae because all quite similar
            
            
            #reassign based on proportions in each lake
            for(i in unique(Sed_Data_Reassigned$LakeName)){
              #assign the calculated proportions for each lake
              Proportions <- c("Alonella excisa" = chydorinae_prop[chydorinae_prop$LakeName == i, "a.excisa.prop"],
                               "Alonella nana" = chydorinae_prop[chydorinae_prop$LakeName == i, "a.nana.prop"],
                               "Alonella pulchella" = chydorinae_prop[chydorinae_prop$LakeName == i, "a.pulch.prop"],
                               "Chydorus brevilabris" = chydorinae_prop[chydorinae_prop$LakeName == i, "c.brevil.prop"],
                               "Chydorus biovatus" = chydorinae_prop[chydorinae_prop$LakeName == i, "c.biov.prop"],
                               "Pleuroxus procurvus" = chydorinae_prop[chydorinae_prop$LakeName == i, "p.proc.prop"],
                               "Pleuroxus straminius" = chydorinae_prop[chydorinae_prop$LakeName == i, "p.stram.prop"],
                               "Pleuroxus trigonellus" = chydorinae_prop[chydorinae_prop$LakeName == i, "p.trig.prop"]
              )
              #make proportions numeric
              Proportions.num <- as.numeric(Proportions)
              #Check that there are no NA values in Proportions
              if(anyNA(Proportions.num)){
                stop(paste("Error: Proportions contain NA in sample:", i))
              }
              #Check that proportions add to one (with tolerance for rounding)
              if((abs(sum(Proportions.num)) - 1) > 0.000000001){
                stop(paste("Error: Proportions do not sum to 1 in sample:", i))
              }   
              #create a logical vector of the rows you want to reassign, need to specifically say that LakeName = i so it only selects for the correct lake
              #not specifying structure... I think that's fine because each one is sampled independently based on that sample's proportions
              reassign <- (Sed_Data_Reassigned$Remain.Type == "Postabdomen (no claw)" | Sed_Data_Reassigned$Remain.Type == "Postabdominal Claw") & (Sed_Data_Reassigned$Taxa == "Chydorinae") & (Sed_Data_Reassigned$LakeName == i)
              
              #sample from distribution and reassign the taxa only in the rows you selected above, when this taxa is present and proportions could be made
              #if there are none of the specified taxa in that sample, the proportions will all be 0 and you will get an error, so adding an if statement solves that
              if(sum(Proportions.num) > 0) {
                Sed_Data_Reassigned$Taxa[reassign] <- sample(
                  names(Proportions), #you are drawing the species names associated with the Proportions vector
                  size = sum(reassign, na.rm = TRUE), #the number of samples to draw is the number of times the reassign logical vector = TRUE, ignoring NA values
                  replace = TRUE, #sample with replacement so the same species can be drawn multiple times
                  prob = Proportions #the probabilities are the values specified in the Proportions vector
                )
              }
            }
            
            
            
      #"Chydorinae" HEADSHIELDS that can't be IDed further and need to be assigned by proportion of species
            #excludes Alonella nana and Pleuroxus trigonellus because those two are very different (left the other Pleuroxus species because their posterior headshield regions are not distinct)
        
        
            #reassign based on proportions in each lake
            for(i in unique(Sed_Data_Reassigned$LakeName)){
              #assign the calculated proportions for each lake
              Proportions <- c("Alonella excisa" = chydorinae_head_prop[chydorinae_head_prop$LakeName == i, "a.excisa.prop"],
                               "Alonella pulchella" = chydorinae_head_prop[chydorinae_head_prop$LakeName == i, "a.pulch.prop"],
                               "Chydorus brevilabris" = chydorinae_head_prop[chydorinae_head_prop$LakeName == i, "c.brevil.prop"],
                               "Chydorus biovatus" = chydorinae_head_prop[chydorinae_head_prop$LakeName == i, "c.biov.prop"],
                               "Pleuroxus procurvus" = chydorinae_head_prop[chydorinae_head_prop$LakeName == i, "p.proc.prop"],
                               "Pleuroxus straminius" = chydorinae_head_prop[chydorinae_head_prop$LakeName == i, "p.stram.prop"]
              )
              #make proportions numeric
              Proportions.num <- as.numeric(Proportions)
              #Check that there are no NA values in Proportions
              if(anyNA(Proportions.num)){
                stop(paste("Error: Proportions contain NA in sample:", i))
              }
              #Check that proportions add to one (with tolerance for rounding)
              if((abs(sum(Proportions.num)) - 1) > 0.000000001){
                stop(paste("Error: Proportions do not sum to 1 in sample:", i))
              }   
              #create a logical vector of the rows you want to reassign, need to specifically say that LakeName = i so it only selects for the correct lake
              #not specifying structure... I think that's fine because each one is sampled independently based on that sample's proportions
              reassign <- (Sed_Data_Reassigned$Remain.Type == "Headshield") & (Sed_Data_Reassigned$Taxa == "Chydorinae") & (Sed_Data_Reassigned$LakeName == i)
              
              #sample from distribution and reassign the taxa only in the rows you selected above, when this taxa is present and proportions could be made
              #if there are none of the specified taxa in that sample, the proportions will all be 0 and you will get an error, so adding an if statement solves that
              if(sum(Proportions.num) > 0) {
                Sed_Data_Reassigned$Taxa[reassign] <- sample(
                  names(Proportions), #you are drawing the species names associated with the Proportions vector
                  size = sum(reassign, na.rm = TRUE), #the number of samples to draw is the number of times the reassign logical vector = TRUE, ignoring NA values
                  replace = TRUE, #sample with replacement so the same species can be drawn multiple times
                  prob = Proportions #the probabilities are the values specified in the Proportions vector
                )
              }
            }     
                        
                 
#Check that everything is reassigned and no ambiguous remains left (except those without data to inform a reassignment)
unique(Sed_Data_Reassigned$Taxa)
                        
#ONE CLADOCERA = 1 Headshield + 2 Shell Valves ( = 1 Carapace) + 1 Postabdomen + 2 Postabdominal claws
                        
                        
#Get counts by taxa AND remain type for each sample
#make a frequency table that gets saved as a data frame
Specimen_Count <- as.data.frame(table(Sed_Data_Reassigned$LakeName, Sed_Data_Reassigned$Taxa, Sed_Data_Reassigned$Remain.Type))
#rename columns, remove zero counts, divide paired structures by 2
Specimen_Count <- Specimen_Count %>% 
  rename(LakeName = Var1) %>% 
  rename(Taxa = Var2) %>% 
  rename(Remain.Type = Var3) %>% 
  mutate(Freq = ifelse((Remain.Type == "Postabdominal Claw" | Remain.Type == "Carapace"), ceiling(Freq/2), Freq)) %>%  #divide postabdominal claw and carapace counts by 2 and then round up (accounts for the fact that there are 2 claws and 2 carapace sides on each zoop)
  filter(Freq != 0) #remove counts of zero
  

#Now take the most frequent remain type for each taxa and use it as the individual count
Individual_Count_by_Taxa <- Specimen_Count %>% 
  group_by(LakeName, Taxa) %>% 
  summarise(Count = max(Freq))

#And count the total number of individuals counted on each slide, add to data frame that has number of slides counted
Individual_Count_Total <- Individual_Count_by_Taxa %>% 
  group_by(LakeName) %>% 
  summarise(Individual.Count = sum(Count))

Sample_summary <- Slide_Count %>% 
  right_join(Individual_Count_Total, by = "LakeName")

#save .csv files:
    #First convert individual count by taxa into wide format
        Individual_Count_by_Taxa_Wide <- Individual_Count_by_Taxa %>%
          pivot_wider(names_from = Taxa, values_from = Count)
          #turn NA values into 0
          Individual_Count_by_Taxa_Wide[is.na(Individual_Count_by_Taxa_Wide)] <- 0 

#write.csv(Individual_Count_by_Taxa_Wide, file = paste0("Data/Output/Sediment_Zoop_Taxa_Count", Sys.Date(), ".csv"))
#write.csv(Sample_summary, file = paste0("Data/Output/Sediment_Zoop_Sample_Summary", Sys.Date(), ".csv"))
          

          
#-------------------------------------------------------------------------------------------------------------
#Count the number of measured structures by lake, remain type, and taxa
    #Do this based on true specimen ID, do not use inferred ID based on proportions
    #UNPAIRED BUT MATCHING REMAINS COULD BE DOUBLE COUNTED/MEASURED - but there is no way to know for sure so that's just going to be a source of error... 
            #I think counting these up uncorrected for pairing will give us the most accurate data
          
          
measured.carapace <- Sed_Data_Clean_Uncorrected_for_pairs_QAQC %>%
  filter(!is.na(Carapace.Length.µm))
          
measured.carapace.lake.summary <- measured.carapace %>% 
  group_by(LakeName, Taxa) %>% 
  summarise(Carapace.Count = n())
            
          
          
measured.mucro.length <- Sed_Data_Clean_Uncorrected_for_pairs_QAQC %>%
  filter(!is.na(Mucro.Length.µm))

measured.mucro.length.lake.summary <- measured.mucro.length %>% 
  group_by(LakeName, Taxa) %>% 
  summarise(Mucro.Length.Count = n())


  
measured.mucro.segments <- Sed_Data_Clean_Uncorrected_for_pairs_QAQC %>% 
  filter(!is.na(Mucro.Segments))

measured.mucro.segments.lake.summary <- measured.mucro.segments %>% 
  group_by(LakeName, Taxa) %>% 
  summarise(Mucro.Segments.Count = n())


  
  
measured.antennule.length <- Sed_Data_Clean_Uncorrected_for_pairs_QAQC %>% 
  filter(!is.na(Antennule.Length.µm))

measured.antennule.length.lake.summary <- measured.antennule.length %>% 
  group_by(LakeName, Taxa) %>% 
  summarise(Antennule.Length.Count = n())


  
measured.antennule.segments <- Sed_Data_Clean_Uncorrected_for_pairs_QAQC %>% 
  filter(!is.na(Antennule.Segments))

measured.antennule.segments.lake.summary <- measured.antennule.segments %>% 
  group_by(LakeName, Taxa) %>% 
  summarise(Antennule.Segments.Count = n())

#join them all togther

df.to.join <- (list(measured.carapace.lake.summary, 
                    measured.mucro.length.lake.summary,
                    measured.mucro.segments.lake.summary,
                    measured.antennule.length.lake.summary,
                    measured.antennule.segments.lake.summary
                    ))

measured.remain.count <- reduce(df.to.join, full_join, by = c("LakeName", "Taxa"))

#turn NA values into 0
measured.remain.count[is.na(measured.remain.count)] <- 0

#save as a csv file
#write.csv(measured.remain.count, file = paste0("Data/Output/Sediment_Zoop_Measured_Remain_Count", Sys.Date(), ".csv"))
