#This file takes counts of zooplankton individuals by taxa and makes a Hellinger-transformed NMDS dissimilarity matrix

#packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(vegan)
library(tidyr)
library(stringr)

# #OPTIONAL: If you want to include the ID consistency checks:
# Sed_Data <- read.csv("Data/Input/Sediment_Zoop_ID_20250829.csv", na.strings = c("N/A", "NA", ""))
# Green_Check <- read.csv("Data/Input/QAQC_Exercise_Sediment_Zoop_ID.csv", na.strings = c("N/A", "NA", ""))
# Medicine_Check <- read.csv("Data/Input/Medicine_Zoop_Consistency.csv", na.strings = c("N/A", "NA", ""))
# 
# Green_Check$LakeName <- ifelse(Green_Check$IDed.by == "Grace", "Green.Check.Grace",
#                                              ifelse(Green_Check$IDed.by == "Evelyn", "Green.Check.Evelyn",
#                                                     ifelse(Green_Check$IDed.by == "Adele", "Green.Check.Adele", Green_Check$LakeName)))
# Green_Check$Paired. <- NA
# Green_Check$Microscope <- NA
# Green_Check_Reordered <- Green_Check[, c(1,2,3,18,4,5,6,7,8,17,9,10,11,12,13,14,15,16)]
# 
# 
# Medicine_Check$LakeName <- ifelse(Medicine_Check$IDed.by == "Grace", "Medicine.Check.Grace",
#                                ifelse(Medicine_Check$IDed.by == "Evelyn", "Medicine.Check.Evelyn",
#                                       ifelse(Medicine_Check$IDed.by == "Adele", "Medicine.Check.Adele", Medicine_Check$LakeName)))
# 
# Sed_Data <- rbind(Sed_Data, Green_Check, Medicine_Check)

#NOW RUN THIS THROUGH THE Count_Sediment_Zoops script


#START HERE IF YOU DON'T WANT TO ADD THE ID CHECK DATA IN:
Matrix.Data <- as.data.frame(Individual_Count_by_Taxa_Wide)
rownames(Matrix.Data) <- Matrix.Data$LakeName

#Calculate relative abundance and remove the lake name column (these are now rownames anyways)
Relative_Abundance <- Matrix.Data[,2:30] / rowSums(Matrix.Data[,2:30])

# #The old way I was doing this by referring to a different dataframe. Better to use rowsums as I did above
# Relative_Abundance <- Matrix.Data[,2:31] / Individual_Count_Total$Individual.Count

#Create vector to identify the consistency checks vs. unique slides AND UPDATE AS NECESSARY
Slide.Type <- c("Unique", "Unique", "Unique", "Unique", "GreenReal", 
                "GreenCheck", "GreenCheck", "GreenCheck", 
                "Unique", "Unique", "Unique", "MedicineReal",  
                "MedicineCheck", "MedicineCheck", "MedicineCheck", 
                "Unique", "Unique", "Unique", "Unique")

#square root transform to do Hellinger transformation to downweight rare species and deal with the many zeroes
Holl_Rel_Abundance <- sqrt(Relative_Abundance)

#Make an NMDS with euclidean distance on Hellinger-transformed data
euclidean_dist <- metaMDS(Holl_Rel_Abundance, distance = "euclidean")
#extract axis scores for the sites (slides) only
Plot.Data <- scores(euclidean_dist, display = "sites")
Plot.Data <- as.data.frame(Plot.Data)
#add vector of slide type to the scores data frame
Plot.Data$Sample.Type <- Slide.Type

#extract axis scores for the zoop species
Species.Plot.Data <- scores(euclidean_dist, display = c("species"))
Species.Plot.Data <- as.data.frame(Species.Plot.Data)

#plot it! - the slides, colored by if the slide is an ID test, a unique lake, or the real data for a lake that was also used for an ID test
#tiff("Zoop_Slides_NMDS_ColorBySlideType.tiff", width = 13, height = 6, units = "in", res = 300)
NMDS <- ggplot(data = Plot.Data, aes(x = NMDS1, y = NMDS2, color = Sample.Type)) +
  geom_point(size = 3)+
  geom_text(data = Species.Plot.Data, aes(x = NMDS1, y = NMDS2), color = "black", label = rownames(Species.Plot.Data)) +
  labs(title = "Sediment Zooplankton Slides Relative Abundance", y = "NMDS2", x = "NMDS1") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
    ) 
NMDS
#dev.off()

#Let's do this again but color it by who identified it

#isolate the info on who identified what
#start by removing spaces before and after lake names
Sed_Data_LakeTrim <- Sed_Data %>% 
  mutate(LakeName = trimws(LakeName))
IDed.by.temp <- Sed_Data_LakeTrim %>% 
  select(LakeName, IDed.by)
#if more than one person worked on it, just change it to "collaborative"
IDed.by.temp$IDed.by <- ifelse((IDed.by.temp$IDed.by != "Adele" & IDed.by.temp$IDed.by != "Grace" & IDed.by.temp$IDed.by != "Evelyn"), "Collaborative", IDed.by.temp$IDed.by) 
#check if any lakes have multiple differend IDers
library(mosaic)
tally(LakeName~IDed.by, data = IDed.by.temp)
#Owasso and Sallie need to be fixed - make them say collaborative
IDed.by.temp$IDed.by <- ifelse((IDed.by.temp$LakeName == "Owasso" | IDed.by.temp$LakeName == "Sallie"), "Collaborative", IDed.by.temp$IDed.by)
#summarize this into one row per lake
IDed.by.short <- IDed.by.temp  %>% 
  group_by(LakeName) %>% 
  summarise(IDed.by = first(IDed.by), .groups = "drop")
#add LakeName to the Plot.Data as a column not just a rowname
Plot.Data$LakeName <- rownames(Plot.Data)
#Join the ID info to the plot data
Plot.Data2 <- right_join(Plot.Data, IDed.by.short, by = "LakeName")
#Make IDed.by and Sample type factors, not character variables
Plot.Data2$Sample.Type <- as.factor(Plot.Data2$Sample.Type)
Plot.Data2$IDed.by <- as.factor(Plot.Data2$IDed.by)




#NMDS plot colored by slide type with a shape for who did the ID
#tiff("Zoop_Slides_NMDS_ShapeByIDer.tiff", width = 13, height = 6, units = "in", res = 300)
NMDS2 <- ggplot(data = Plot.Data2, aes(x = NMDS1, y = NMDS2, color = Sample.Type, shape = IDed.by)) +
  geom_point(size = 3)+
  geom_text(data = Species.Plot.Data, aes(x = NMDS1, y = NMDS2), color = "black", label = rownames(Species.Plot.Data), inherit.aes = FALSE) +
  labs(title = "Sediment Zooplankton Slides Relative Abundance", y = "NMDS2", x = "NMDS1") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  ) 
NMDS2
#dev.off()

#NMDS plot colored by who did ID - no shape for simplicity
#tiff("Zoop_Slides_NMDS_ColorByIDer.tiff", width = 13, height = 6, units = "in", res = 300)
NMDS3 <- ggplot(data = Plot.Data2, aes(x = NMDS1, y = NMDS2, color = IDed.by)) +
  geom_point(size = 3)+
  geom_text(data = Species.Plot.Data, aes(x = NMDS1, y = NMDS2), color = "black", label = rownames(Species.Plot.Data), inherit.aes = FALSE) +
  labs(title = "Sediment Zooplankton Slides Relative Abundance", y = "NMDS2", x = "NMDS1") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  ) 
NMDS3
#dev.off()

#no eigenvalues in NMDS so I Can't get % variation explained by each axis because it ranks distances instead of using raw distances...


