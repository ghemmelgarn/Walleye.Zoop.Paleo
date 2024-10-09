#Set workng directory
setwd("G:/My Drive/Thesis/Data/R Working Directories/Exploratory R Working Directory")

#import data
Data <- read.csv("WZ_Lake_Selection.csv")

#Packages
library(dplyr)
library(ggplot2)
library(gridExtra)
library(mosaic)

#filter out rows that dont have both fish and zoop data
#DataMatch <- filter(Data, Match != "No")
#Don't need to do this because I filter out everything below anyways

#Set DOW as categorical
Data$DOW <- as.factor(DataMatch$DOW)

#set WalleyeCPUE as numerical
Data$WalleyeCPUE <- as.numeric(Data$WalleyeCPUE)

#FILTER OUT VARIOUS DATASET OPTIONS

#Exact Match, 6+ zoop samples
DataExact6 <- filter(Data, Match == "Exact", ZoopTows >= 6)
  
#Exact Match, 4+ zoop samples
DataExact4 <- filter(Data, Match == "Exact", ZoopTows >= 4)
  
#+/- One Year, 6+ zoop samples
DataOneYear6 <- filter(Data, Match != "No", ZoopTows >= 6)

#+/- One Year, 4+ zoop samples
DataOneYear4 <- filter(Data, Match != "No", ZoopTows >= 4)


#PLOTS - walleye CPUE distribution, number of years per lake(sort by DOW), number of lakes

#Exact Match, 6+ zoop samples
Match6Plot <- ggplot(DataExact6, aes(x = WalleyeCPUE)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Exact Match, 6+ Zoop Tows", y = "Count", x = "Walleye Standard Gill Net CPUE") +
  scale_x_continuous(breaks = seq(0,65, by = 5))+
  annotate("text", x = 40, y = 25, label = "n = 276", size = 4, color = "black") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black")  # Add axis lines
  )

#Exact Match, 4+ zoop samples
Match4Plot <- ggplot(DataExact4, aes(x = WalleyeCPUE)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Exact Match, 4+ Zoop Tows", y = "Count", x = "Walleye Standard Gill Net CPUE") +
  scale_x_continuous(breaks = seq(0,65, by = 5)) +
  annotate("text", x = 40, y = 25, label = "n = 294", size = 4, color = "black") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black")  # Add axis lines
  )


#+/- One Year, 6+ zoop samples
OneYear6Plot <- ggplot(DataOneYear6, aes(x = WalleyeCPUE)) +
  geom_histogram(binwidth = 1) +
  labs(title = "+/- One Year, 6+ Zoop Tows", y = "Count", x = "Walleye Standard Gill Net CPUE") +
  scale_x_continuous(breaks = seq(0,65, by = 5)) +
  annotate("text", x = 40, y = 25, label = "n = 327", size = 4, color = "black") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black")  # Add axis lines
  )

#+/- One Year, 4+ zoop samples
OneYear4Plot <- ggplot(DataOneYear4, aes(x = WalleyeCPUE)) +
  geom_histogram(binwidth = 1) +
  labs(title = "+/- One Year, 4+ Zoop Tows", y = "Count", x = "Walleye Standard Gill Net CPUE") +
  scale_x_continuous(breaks = seq(0,65, by = 5)) +
  annotate("text", x = 40, y = 25, label = "n = 362", size = 4, color = "black") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black")  # Add axis lines
  )

#Make one figure with all the above figures:
#tiff("walleye CPUE Histograms.tiff", width = 8, height = 5, units = "in", res = 300)
WalleyeCPUE_Hist <- grid.arrange(Match6Plot, Match4Plot, OneYear6Plot, OneYear4Plot, nrow= 2, ncol = 2)
#dev.off()

tally(~LakeName, data = DataOneYear4)

#This graph below is terrible, but it works for what we need right now
#tiff("LakeHistograms.tiff", width = 15, height = 10, units = "in", res = 300)
ggplot(DataOneYear4, aes(x = LakeName)) +
  geom_bar() +
  labs(title = "Years per Lake", y = "Count", x = "Lake") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"),  # Add axis lines
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
#dev.off()
