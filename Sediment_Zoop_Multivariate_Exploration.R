#This file takes counts of zooplankton individuals by taxa and makes a bray-curtis dissimilarity matrix

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

#Calculate relative abundance
Relative_Abundance <- Matrix.Data[,2:35] / Individual_Count_Total$Individual.Count

#Create vector to identify the consistency checks vs. unique slides AND UPDATE AS NECESSARY
Slide.Type <- c("Unique", "Unique", "Unique", "Unique", "Unique", 
                "GreenCheck", "GreenCheck", "GreenCheck", 
                "Unique", "Unique", "Unique", "Unique",  
                "MedicineCheck", "MedicineCheck", "MedicineCheck", 
                "Unique", "Unique", "Unique", "Unique")

#square root transform to do Hollinger transformation to downweight rare species
Holl_Rel_Abundance <- sqrt(Relative_Abundance)

#Make an NMDS with bray-curtis distance
bray_curtis_dist <- metaMDS(Holl_Rel_Abundance, distance = "euclidean")
#extract axis scores for the sites (slides) only
Plot.Data <- scores(bray_curtis_dist, display = "sites")
Plot.Data <- as.data.frame(Plot.Data)
#add vector of slide type to the scores data frame
Plot.Data$Sample.Type <- Slide.Type

#extract axis scores for the zoop species
Species.Plot.Data <- scores(bray_curtis_dist, display = c("species"))
Species.Plot.Data <- as.data.frame(Species.Plot.Data)

#plot it! - the slides
#tiff(Zoop_Slides_NMDS.tiff"), width = 13, height = 6, units = "in", res = 300)
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





