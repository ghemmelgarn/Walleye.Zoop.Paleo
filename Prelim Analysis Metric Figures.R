#this code explores the initial zooplankton and other metrics before multiple regression for biometry class project
#Based on Preliminary Dataset I created
#Also has a plot that shows walleye vs. lmb CPUE for all MN lakes vs. the selected lakes in the dataset

#Packages
library(dplyr)
library(ggplot2)
library(gridExtra)
library(mosaic)
library(ggExtra)
library(GGally)
library(tidyverse)

PrelimData <- read.csv("Data/Input/Preliminary Data.csv")

#ZOOPLANKTON METRICS

#Histograms of each zooplankton metric
total.biomass <- ggplot(PrelimData, aes(x = zoop.total.biomass)) +
  geom_histogram() +
  labs(title = "", y = "Count", x = "Total Zooplankton Biomass (ug/L)") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black")  # Add axis lines
  )
total.biomass

prop.daphnia <- ggplot(PrelimData, aes(x = Daphnia.prop.Cladoceran.biomass)) +
  geom_histogram() +
  labs(title = "", y = "Count", x = "Proportion Daphnia Biomass") +
  scale_x_continuous(limits = c(0,1)) +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black")  # Add axis lines
  )
prop.daphnia

prop.cladoceran <- ggplot(PrelimData, aes(x = prop.Cladoceran.biomass)) +
  geom_histogram() +
  labs(title = "", y = "Count", x = "Proportion Cladoceran Biomass") +
  scale_x_continuous(limits = c(0,1)) +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black")  # Add axis lines
  )
prop.cladoceran

z.mean.length <- ggplot(PrelimData, aes(x = zoop.mean.length)) +
  geom_histogram() +
  labs(title = "", y = "Count", x = "Zoop Mean Length (mm)") +
  scale_x_continuous(limits = c(0.4,1.4)) +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black")  # Add axis lines
  )
z.mean.length

c.mean.length <- ggplot(PrelimData, aes(x = cladoceran.mean.length)) +
  geom_histogram() +
  labs(title = "", y = "Count", x = "Cladoceran Mean Length (mm)") +
  scale_x_continuous(limits = c(0.4,1.4)) +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black")  # Add axis lines
  )
c.mean.length

prop.large.c <- ggplot(PrelimData, aes(x = prop.large.cladoceran)) +
  geom_histogram() +
  labs(title = "", y = "Count", x = "Proportion Large Cladoceran") +
  scale_x_continuous(limits = c(0,1)) +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black")  # Add axis lines
  )
prop.large.c

z.shannon.DI <- ggplot(PrelimData, aes(x = zoop.Shannon.DI)) +
  geom_histogram() +
  labs(title = "", y = "Count", x = "Shannon Diversity Index") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black")  # Add axis lines
  )
z.shannon.DI

#tiff("Prelim Zoop Metric Histograms.tiff", width = 6, height = 8, units = "in", res = 300)
zoop.metric.hist <- grid.arrange(total.biomass, prop.daphnia, prop.cladoceran, z.mean.length, c.mean.length, prop.large.c, z.shannon.DI, nrow= 4, ncol = 2)
#dev.off()

#Scatterplot between all zoop variables
#first select just the variables you want to run the correlations on
Zoop.Metrics <- select(PrelimData, zoop.total.biomass, Daphnia.prop.Cladoceran.biomass, prop.Cladoceran.biomass, zoop.mean.length, cladoceran.mean.length, prop.large.cladoceran, zoop.Shannon.DI)
#label the variables
custom_labels <- c("zoop.total.biomass" = "TotZoopBiomass",
                   "Daphnia.prop.Cladoceran.biomass" = "PropDaphBiomass",
                   "prop.Cladoceran.biomass" = "PropCladBiom",
                   "zoop.mean.length" = "ZoopMeanLength",
                   "cladoceran.mean.length" = "CladMeanLength",
                   "prop.large.cladoceran" = "PropLargeClad",
                   "zoop.Shannon.DI" = "ShannonDI")

#tiff("Prelim Zoop Metric Correlations.tiff", width = 8, height = 8, units = "in", res = 300)                   
ggpairs(Zoop.Metrics, columnLabels = custom_labels)
#dev.off()




#-------------------------------------------------------------------------------------------------------------




#SAME AS ABOVE BUT FOR ALL CHOSEN PREDICTORS, including the selected zoop metrics
#also has total zoop biomass because I'm curious how strongly it correlates with the secchi data but probably won't include it
#let's throw walleye CPUE in here to look at it as well :)
#individual histograms (that don't already exist above):
degree.days <- ggplot(PrelimData, aes(x = gdd_wtr_5c)) +
  geom_histogram() +
  labs(title = "", y = "Count", x = "Degree Days Water Temp (5C)") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black")  # Add axis lines
  )
degree.days

secchi <- ggplot(PrelimData, aes(x = mean.summer.secchi.meters)) +
  geom_histogram() +
  labs(title = "", y = "Count", x = "Mean Summer Secchi (m)") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black")  # Add axis lines
  )
secchi

area <- ggplot(PrelimData, aes(x = lakesize)) +
  geom_histogram() +
  labs(title = "", y = "Count", x = "Lake Surface Area (acres)") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black")  # Add axis lines
  )
area

walleye.cpue <- ggplot(PrelimData, aes(x = WAE.CPUE.inc)) +
  geom_histogram() +
  labs(title = "", y = "Count", x = "Walleye CPUE") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black")  # Add axis lines
  )
walleye.cpue

#tiff("Prelim Metric Histograms.tiff", width = 6, height = 8, units = "in", res = 300)
metric.hist <- grid.arrange(total.biomass, prop.cladoceran, z.mean.length, z.shannon.DI, degree.days, secchi, area, walleye.cpue, nrow= 4, ncol = 2)
#dev.off()


#scatterplot between them all
#first select just the variables you want to run the correlations on
Prelim.Metrics <- PrelimData %>%
  select(zoop.total.biomass, 
         prop.Cladoceran.biomass, 
         zoop.mean.length, 
         zoop.Shannon.DI, 
         gdd_wtr_5c, 
         mean.summer.secchi.meters,
         lakesize,
         WAE.CPUE.inc
         )
#label the variables
custom_labels.2 <- c("zoop.total.biomass" = "TotZoopBiomass",
                   "prop.Cladoceran.biomass" = "PropCladBiom",
                   "zoop.mean.length" = "ZoopMeanLength",
                   "zoop.Shannon.DI" = "ShannonDI",
                   "gdd_wtr_5c" = "DegreeDays", 
                   "mean.summer.secchi.meters" = "Secchi",
                   "lakesize" = "SurfaceArea",
                   "WAE.CPUE.inc" = "WAE_CPUE"
                   )

#tiff("Prelim Metric Correlations.tiff", width = 8, height = 8, units = "in", res = 300)                   
ggpairs(Prelim.Metrics, columnLabels = custom_labels.2)
#dev.off()



#----------------------------------------------------------------------------------------------------------------------------



#WALLEYE VS. LMB CPUE IN ALL MN LAKES AND SELECTED LAKES
#All MN data from Denver's Spreadsheet
#Selected Lakes from my Preliminary Dataset

#Import all MN data
Data.MN.All <- read.csv("Data/Input/Copy of Exploratory Data.ALL_MN_Fish_Lakes.csv")

#need to join these two together
#make a column that marks the data in the Preliminary Dataset
PrelimData <- PrelimData %>%
  mutate(Data = "Prelim")
#filter down the prelim data to what you want to join
PrelimData.join <- PrelimData %>%
  select(parentdow.fish.year,
         WAE.CPUE.inc,
         LMB.CPUE.inc,
         Data)
#left join this data into the all Minnesota Data
Data.MN.joined <- left_join(Data.MN.All, PrelimData.join, by = "parentdow.fish.year")

#add to the Data column something that shows when lake is part of the All MN data so it's not just NA when it's not part of the prelim data
Data.MN.joined <- Data.MN.joined %>%
  mutate(Data = ifelse(is.na(Data), "AllMN", "Prelim"))
  
# #see if my prelim dataset values are different than the MN spreadsheet values for walleye and LMB CPUE
# Data.MN.joined <- Data.MN.joined %>%
#   mutate(WAE.test = GN.walleye - WAE.CPUE.inc,
#          LMB.test = GN.largemouth_bass - LMB.CPUE.inc)
# unique(Data.MN.joined$WAE.test)
# unique(Data.MN.joined$LMB.test)
# #GOOD NEWS: they are the same!! Except for one LMB that is very very very slightly different, not enough to make a difference here

#Walleye Gillnet CPUE vs. Largemouth CPUE (Gillnet) - all MN lakes, zoop/fish match data colored
#arrange data so selected preliminary zoop data get plotted last and show up on top of no zoop data
Data.filter.MN.All <- Data.MN.joined %>% arrange(Data)

#tiff("Prelim Data WAE vs. LMB All MN Lakes.tiff", width = 7, height = 7, units = "in", res = 300)
WAE.LMB.All.MN <- ggplot(Data.filter.MN.All, aes(x = GN.walleye)) + 
  geom_point(aes(y = GN.largemouth_bass, color = ifelse(Data == "Prelim", "Selected Data", "Not Selected Data")))+
  labs(title = "Preliminary Data Walleye vs. LMB", y = "Largemouth Bass Gillnet CPUE", x = "Walleye Gillnet CPUE", color = NULL) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 5), labels = seq(0, 30, by = 5)) +
  scale_x_continuous(limits = c(0, 120), breaks = seq(0, 120, by = 20), labels = seq(0, 120, by = 20)) +
  scale_color_manual(values = c("Selected Data" = "blue", "Not Selected Data" = "lightgray")) + 
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black")  # Add axis lines
  )
ggMarginal(WAE.LMB.All.MN, groupColour = TRUE, groupFill = TRUE)
#print(WAE.LMB.All.MN)
#print code not needed with ggMarginal density plots
#dev.off()

#same as above but zoomed in lower left corner
#tiff("Prelim Data WAE vs. LMB All MN Lakes.zoom.tiff", width = 7, height = 7, units = "in", res = 300)
WAE.LMB.All.MN.zoom <- ggplot(Data.filter.MN.All, aes(x = GN.walleye)) + 
  geom_point(aes(y = GN.largemouth_bass, color = ifelse(Data == "Prelim", "Selected Data", "Not Selected Data")))+
  labs(title = "Preliminary Data Walleye vs. LMB", y = "Largemouth Bass Gillnet CPUE", x = "Walleye Gillnet CPUE", color = NULL) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2), labels = seq(0, 10, by = 2)) +
  scale_x_continuous(limits = c(0, 65), breaks = seq(0, 65, by = 10), labels = seq(0, 65, by = 10)) +
  scale_color_manual(values = c("Selected Data" = "blue", "Not Selected Data" = "lightgray")) + 
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black")  # Add axis lines
  )
#print(WAE.LMB.All.MN.zoom)
#print code not needed with ggMarginal density plots
ggMarginal(WAE.LMB.All.MN.zoom, groupColour = TRUE, groupFill = TRUE)
#dev.off()




#-----------------------------------------------------------------------------------------------------------------




#I just want to take a closer look at lake area without the outliers
area.no.outliers <- ggplot(PrelimData, aes(x = lakesize)) +
  geom_histogram(binwidth = 50) +
  labs(title = "", y = "Count", x = "Lake Surface Area (acres)") +
  scale_x_continuous(limits = c(0,7500)) +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black")  # Add axis lines
  )
area.no.outliers
