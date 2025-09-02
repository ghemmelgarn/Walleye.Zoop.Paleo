#this script compares the first 50 specimens found on the same slide by 3 different lab workers to determine consistency of identification


#packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(vegan)
library(tidyr)


#read in data and make N/A, NA, or an empty cell all read in as NA
Data <- read.csv("Data/Input/QAQC_Exercise_Sediment_Zoop_ID.csv", na.strings = c("N/A", "NA", ""))

#Set lake name for file names:
LakeName <- "Green"

#histograms of remain type, remain condition, and taxa by lab worker

#tiff(paste0(LakeName, "_Test_Remain_Type.tiff"), width = 8, height = 6, units = "in", res = 300)
Remain_Type <- ggplot(Data, aes(x = Remain.Type, fill = IDed.by)) +
  geom_bar(position = "dodge") +
  labs(title = "Remain Type", y = "Count", x = "Remain Type") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
    axis.text.x = element_text(angle = 60, hjust = 1) #Rotate x axis labels
    ) +
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.5)+ # label bars with counts
  scale_y_continuous(limits = c(0, 35))
Remain_Type  
#dev.off()


#tiff(paste0(LakeName, "_Test_Remain_Condition.tiff"), width = 8, height = 5.5, units = "in", res = 300)
Remain_Condition <- ggplot(Data, aes(x = Remain.Condition, fill = IDed.by)) +
  geom_bar(position = "dodge") +
  labs(title = "Remain Condition", y = "Count", x = "Remain Condition") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
    axis.text.x = element_text(angle = 60, hjust = 1) #Rotate x axis labels
  ) +
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.5)+ # label bars with counts
  scale_y_continuous(limits = c(0, 40))
Remain_Condition 
#dev.off()

#tiff(paste0(LakeName, "_Test_Taxa.tiff"), width = 11, height = 7, units = "in", res = 300)
Taxa <- ggplot(Data, aes(x = Taxa, fill = IDed.by)) +
  geom_bar(position = "dodge") +
  labs(title = "Taxa", y = "Count", x = "Taxa") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
    axis.text.x = element_text(angle = 80, hjust = 1) #Rotate x axis labels
  ) +
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.5)+ # label bars with counts
  scale_y_continuous(limits = c(0, 20))
Taxa
#dev.off()



Data$Specimen = paste(Data$Taxa, Data$Remain.Type)
#tiff(paste0(LakeName, "_Test_Taxa_and_Remain_Type.tiff"), width = 14, height = 10, units = "in", res = 300)
Taxa_and_Remain_Type <- ggplot(Data, aes(x = Specimen, fill = IDed.by)) +
  geom_bar(position = "dodge") +
  labs(title = "Taxa and Remain Type", y = "Count", x = "Taxa and Remain Type") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
    axis.text.x = element_text(angle = 80, hjust = 1) #Rotate x axis labels
  ) +
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.5)+
  scale_y_continuous(limits = c(0, 20))
Taxa_and_Remain_Type
#dev.off()

#-------------------------------------------------------------------------------------------------------
#PLOT OUR LENGTHS OF THE THINGS WE MEASURED TO COMPARE THOSE


#First isolate the things with measurements
Bosminid_Headshields <- Data %>% 
  filter((Taxa == "Bosmina longirostris (headshield only)" | Taxa == "Eubosmina coregoni (headshield only)" | Taxa == "Bosminid (headshield or claw - genus unsure)") 
         & Remain.Type == "Headshield" 
         & Remain.Condition == "Identifieable and measureable"
     )
Mucros <- Data %>% 
  filter(Taxa == "Bosminid carapace with mucro" 
         & Remain.Type == "Carapace" 
         & Remain.Condition == "Identifieable and measureable"
  )

Carapaces <- Data %>% 
  filter(Remain.Type == "Carapace" 
         & Remain.Condition == "Identifieable and measureable"
         & !is.na(Carapace.Length..µm.) 
  )


#now plot them
#tiff(paste0(LakeName, "_Test_Antennule_Length.tiff"), width = 10, height = 4.5, units = "in", res = 300)
Antennule_Length <- ggplot(Bosminid_Headshields, aes(x = Attenule.Length..µm. , fill = IDed.by)) +
  geom_histogram(position = "dodge", binwidth = 10) +
  labs(title = "Antennule Length", y = "Count", x = "Antennule Length (µm)") +
  facet_wrap(~IDed.by)+
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
    axis.text.x = element_text(angle = 60, hjust = 1), #Rotate x axis labels
    panel.border = element_rect(color = "black", fill = NA) #gives panels an outline
  ) +
  geom_text(stat = "bin", binwidth = 10, aes(label = ifelse(..count.. > 0, round((..xmax.. + ..xmin..) / 2, 0), ""), y = ..count..), position = position_dodge(width = 0.5), vjust = -0, angle = 60)+ #label bars with x values (bin midpoint)
  scale_y_continuous(limits = c(0, 2.1))
Antennule_Length
#dev.off()

#tiff(paste0(LakeName, "_Test_Antennule_Segments.tiff"), width = 10, height = 4.5, units = "in", res = 300)
Antennule_Segments <- ggplot(Bosminid_Headshields, aes(x = Antennule.Segments , fill = IDed.by)) +
  geom_histogram(position = "dodge", binwidth = 1) +
  labs(title = "Antennule Segments", y = "Count", x = "Antennule Segments (count)") +
  facet_wrap(~IDed.by)+
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
    axis.text.x = element_text(angle = 60, hjust = 1), #Rotate x axis labels
    panel.border = element_rect(color = "black", fill = NA) #gives panels an outline
  ) +
  geom_text(stat = "bin", binwidth = 1, aes(label = ifelse(..count.. > 0, round((..xmax.. + ..xmin..) / 2, 0), ""), y = ..count..), position = position_dodge(width = 0.9), vjust = -0.5)+ #label bars with x values (bin midpoint)
  scale_y_continuous(limits = c(0, 2.1))
Antennule_Segments
#dev.off()

#tiff(paste0(LakeName, "_Test_Mucro_Length.tiff"), width = 10, height = 4.5, units = "in", res = 300)
Mucro_Length <- ggplot(Mucros, aes(x = Mucro.linear.Length..µm. , fill = IDed.by)) +
  geom_histogram(position = "dodge", binwidth = 5) +
  labs(title = "Mucro Length", y = "Count", x = "Mucro Length (µm)") +
  facet_wrap(~IDed.by)+
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
    axis.text.x = element_text(angle = 60, hjust = 1), #Rotate x axis labels
    panel.border = element_rect(color = "black", fill = NA) #gives panels an outline
  ) +
  geom_text(stat = "bin", binwidth = 5, aes(label = ifelse(..count.. > 0, round((..xmax.. + ..xmin..) / 2, 0), ""), y = ..count..), position = position_dodge(width = 0.9), vjust = -0.5)+ #label bars with x values (bin midpoint)
  scale_y_continuous(limits = c(0, 4.1))
Mucro_Length
#dev.off()

#tiff(paste0(LakeName, "_Test_Mucro_Segments.tiff"), width = 10, height = 4.5, units = "in", res = 300)
Mucro_Segments <- ggplot(Mucros, aes(x = Mucro.Sutures , fill = IDed.by)) +
  geom_histogram(position = "dodge", binwidth = 1) +
  labs(title = "Mucro Segments", y = "Count", x = "Mucro Segments (count)") +
  facet_wrap(~IDed.by)+
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
    axis.text.x = element_text(angle = 60, hjust = 1), #Rotate x axis labels
    panel.border = element_rect(color = "black", fill = NA) #gives panels an outline
  ) +
  geom_text(stat = "bin", binwidth = 1, aes(label = ifelse(..count.. > 0, round((..xmax.. + ..xmin..) / 2, 0), ""), y = ..count..), position = position_dodge(width = 0.9), vjust = -0.5) #label bars with x values (bin midpoint)
  #scale_y_continuous(limits = c(0, 4.1))
Mucro_Segments
#dev.off()

#tiff(paste0(LakeName, "_Test_Carapace_Length.tiff"), width = 13, height = 6, units = "in", res = 300)
Carapace_Length <- ggplot(Carapaces, aes(x = Carapace.Length..µm. , fill = IDed.by)) +
  geom_histogram(position = "dodge", binwidth = 10) +
  labs(title = "Carapace Length", y = "Count", x = "Carapace Length (µm)") +
  facet_wrap(~IDed.by)+
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
    axis.text.x = element_text(angle = 60, hjust = 1), #Rotate x axis labels
    panel.border = element_rect(color = "black", fill = NA) #gives panels an outline
  ) +
  geom_text(stat = "bin", binwidth = 10, aes(label = ifelse(..count.. > 0, round((..xmax.. + ..xmin..) / 2, 0), ""), y = ..count..), position = position_dodge(width = 0.5), vjust = -0, angle = 60) #label bars with x values (bin midpoint)
  #scale_y_continuous(limits = c(0, 2.1))
Carapace_Length
#dev.off()


#thoughts: I am unlikely to record anything at all if it is an unidentified fragment, while Evelyn records those often. Adele also less likely to record. No harm to data here. 
#I bet my Leptodora tail spine is what they called the Camptocercus postabdomen - maybe go back and check this?
#We are good about bosminids

#We have some significant ID discrepancies

#lengths look okay
#mucro segments need work

#--------------------------------------------------------------------------------------------------------------------------------
#Calculate summary stats

    #make empty matrix to fill
    summary_stats <- matrix(NA, nrow = 3, ncol = 18, dimnames = list(NULL, c("IDed.by", "Total.Remains", "Total.Individuals", 
                                                                              "Mean.Antennule.Length", "Max.Antennule.Length", "Min.Antennule.Length", 
                                                                              "Mean.Antennule.Segments", "Max.Antennule.Segments", "Min.Antennule.Segments", 
                                                                              "Mean.Mucro.Length", "Max.Mucro.Length", "Min.Mucro.Length", 
                                                                              "Mean.Mucro.Segments", "Max.Mucro.Segments", "Min.Mucro.Segments",
                                                                              "Mean.Carapace.Length", "Max.Carapace.Length", "Min.Carapace.Length")))
    row_idx <- 1
    for(i in unique(Data$IDed.by)){
      #Name of lab worker
      summary_stats[row_idx,1] <- i
      #Number of  remains counted (excluding unidentifiable)
      summary_stats[row_idx,2] <- sum(Data$Taxa != "Unidentifiable" & Data$IDed.by == i)
      #Antennule length
      summary_stats[row_idx,4] <- mean(Data$Attenule.Length..µm.[Data$IDed.by == i], na.rm = TRUE)
      summary_stats[row_idx,5] <- max(Data$Attenule.Length..µm.[Data$IDed.by == i], na.rm = TRUE)
      summary_stats[row_idx,6] <- min(Data$Attenule.Length..µm.[Data$IDed.by == i], na.rm = TRUE)
      #Antennule segments
      summary_stats[row_idx,7] <- mean(Data$Antennule.Segments[Data$IDed.by == i], na.rm = TRUE)
      summary_stats[row_idx,8] <- max(Data$Antennule.Segments[Data$IDed.by == i], na.rm = TRUE)
      summary_stats[row_idx,9] <- min(Data$Antennule.Segments[Data$IDed.by == i], na.rm = TRUE)
      #Mucro length
      summary_stats[row_idx,10] <- mean(Data$Mucro.linear.Length..µm.[Data$IDed.by == i], na.rm = TRUE)
      summary_stats[row_idx,11] <- max(Data$Mucro.linear.Length..µm.[Data$IDed.by == i], na.rm = TRUE)
      summary_stats[row_idx,12] <- min(Data$Mucro.linear.Length..µm.[Data$IDed.by == i], na.rm = TRUE)
      #Mucro segments
      summary_stats[row_idx,13] <- mean(Data$Mucro.Sutures[Data$IDed.by == i], na.rm = TRUE)
      summary_stats[row_idx,14] <- max(Data$Mucro.Sutures[Data$IDed.by == i], na.rm = TRUE)
      summary_stats[row_idx,15] <- min(Data$Mucro.Sutures[Data$IDed.by == i], na.rm = TRUE)
      #Carapace length
      summary_stats[row_idx,16] <- mean(Data$Carapace.Length..µm.[Data$IDed.by == i], na.rm = TRUE)
      summary_stats[row_idx,17] <- max(Data$Carapace.Length..µm.[Data$IDed.by == i], na.rm = TRUE)
      summary_stats[row_idx,18] <- min(Data$Carapace.Length..µm.[Data$IDed.by == i], na.rm = TRUE)
      #increase the row_idx value for the next row
      row_idx <- row_idx + 1
    }
    #make this matrix a data frame
    summary_stats <- as.data.frame(summary_stats)
    

    
    #Total number of individuals and total number of individuals by species
          #Use the other script to count this up and then upload the data here
    
          # #Add a column named "Paired." for the Green Lake exercise ONLY:
          # 
          # Data$Paired. <- NA
          
          #RUN THIS CODE FOR EACH LAB WORKER AND THEN RUN THE DATA THROUGH THE COUNTING SCRIPT
    Sed_Data <- Data %>% 
      filter(IDed.by == "Grace")
    #Now run Sed_Data through counting script before running next line of code
    #add total indiviudal count to summary stats
    summary_stats[summary_stats$IDed.by == "Grace", 3] <- Sample_summary[1,3]
    #Save count by taxa for this lab worker
    Grace_Species <- Individual_Count_by_Taxa_Wide
    #Add IDed.by column
    Grace_Species$IDed.by <- "Grace"
    #remove LakeName column
    Grace_Species <- Grace_Species[, -1]
    
    
    Sed_Data <- Data %>% 
      filter(IDed.by == "Evelyn")
    #add total indiviudal count to summary stats
    #Now run Sed_Data through counting script before running next line of code
    summary_stats[summary_stats$IDed.by == "Evelyn", 3] <- Sample_summary[1,3]
    #Save count by taxa for this lab worker
    Evelyn_Species <- Individual_Count_by_Taxa_Wide
    #Add IDed.by column
    Evelyn_Species$IDed.by <- "Evelyn"
    #remove LakeName column
    Evelyn_Species <-  Evelyn_Species[, -1]
    
    
    
    Sed_Data <- Data %>% 
      filter(IDed.by == "Adele")
    #add total indiviudal count to summary stats
    #Now run Sed_Data through counting script before running next line of code
    summary_stats[summary_stats$IDed.by == "Adele", 3] <- Sample_summary[1,3]
    #Save count by taxa for this lab worker
    Adele_Species <- Individual_Count_by_Taxa_Wide
    #Add IDed.by column
    Adele_Species$IDed.by <- "Adele"
    #remove LakeName column
    Adele_Species <- Adele_Species[, -1]
    
    
    #Combine individual counts by taxa
    Combined_Species <- bind_rows(Evelyn_Species, Adele_Species, Grace_Species)
    #put taxa columns in alphabetical order
    fixed_col <- "IDed.by"
    other_cols <- setdiff(names(Combined_Species), fixed_col)
    sorted_cols <- (sort(other_cols))
    Combined_Species <- Combined_Species[ ,c(fixed_col, sorted_cols)]
    #turn NA values into 0
    Combined_Species[is.na(Combined_Species)] <- 0 
    
    
    # #export these two files
    # write.csv(summary_stats, file = paste0(LakeName, "_Summary_Stats.csv"))
    # write.csv(Combined_Species, file = paste0(LakeName, "_Individual_Count_by_Taxa.csv"))



