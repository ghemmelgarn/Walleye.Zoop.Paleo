#this script compares the first 50 specimens found on the same slide by 3 different lab workers to determine consistency of identification


#packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(vegan)
library(tidyr)


#read in data
Data <- read.csv("Data/Input/QAQC_Exercise_Sediment_Zoop_ID.csv")

#histograms of remain type, remain condition, and taxa by lab worker

Remain_Type <- ggplot(Data, aes(x = Remain.Type, fill = IDed.by)) +
  geom_bar(position = "dodge") +
  labs(title = "Remain Type", y = "Count", x = "Remain Type") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
    axis.text.x = element_text(angle = 60, hjust = 1) #Rotate x axis lables
    ) +
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.5)+
  scale_y_continuous(limits = c(0, 35))
Remain_Type  


Remain_Condition <- ggplot(Data, aes(x = Remain.Condition, fill = IDed.by)) +
  geom_bar(position = "dodge") +
  labs(title = "Remain Condition", y = "Count", x = "Remain Condition") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
    axis.text.x = element_text(angle = 60, hjust = 1) #Rotate x axis lables
  ) +
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.5)+
  scale_y_continuous(limits = c(0, 40))
Remain_Condition 


Taxa <- ggplot(Data, aes(x = Taxa, fill = IDed.by)) +
  geom_bar(position = "dodge") +
  labs(title = "Taxa", y = "Count", x = "Taxa") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
    axis.text.x = element_text(angle = 80, hjust = 1) #Rotate x axis lables
  ) +
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.5)+
  scale_y_continuous(limits = c(0, 20))
Taxa



Taxa_and_Remain_Type

#MAKE A COLUMN THAT HAS BOTH AND MAKE THE SAME PLOT WITH THAT

#PLOT OUR LENGHTS OF THE THINGS WE MEASURED TO COMPARE THOSE



#thoughts: I am unlikely to record anything at all if it is an unidentified fragment, while Evelyn records those often. Adele also less likely to record. No harm to data here. 
#I bet my Leptodora tail spine is what they called the Camptocercus postabdomen - maybe go back and check this?
#We are good about bosminids
