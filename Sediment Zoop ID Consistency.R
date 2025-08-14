#this script compares the first 50 specimens found on the same slide by 3 different lab workers to determine consistency of identification


#packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(vegan)
library(tidyr)


#read in data and make N/A, NA, or an empty cell all read in as NA
Data <- read.csv("Data/Input/QAQC_Exercise_Sediment_Zoop_ID.csv", na.strings = c("N/A", "NA", ""))


#histograms of remain type, remain condition, and taxa by lab worker

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



Data$Specimen = paste(Data$Taxa, Data$Remain.Type)
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


#thoughts: I am unlikely to record anything at all if it is an unidentified fragment, while Evelyn records those often. Adele also less likely to record. No harm to data here. 
#I bet my Leptodora tail spine is what they called the Camptocercus postabdomen - maybe go back and check this?
#We are good about bosminids

#We have some significant ID discrepancies

#lengths look okay
#mucro segments need work
