#this code explores the initial zooplankton and other metrics before multiple regression for biometry class project

#Packages
library(dplyr)
library(ggplot2)
library(gridExtra)
library(mosaic)
library(ggExtra)
library(GGally)

PrelimData <- read.csv("Data/Input/Preliminary Data.csv")

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
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black")  # Add axis lines
  )
prop.daphnia

prop.cladoceran <- ggplot(PrelimData, aes(x = prop.Cladoceran.biomass)) +
  geom_histogram() +
  labs(title = "", y = "Count", x = "Proportion Cladoceran Biomass") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black")  # Add axis lines
  )
prop.cladoceran

z.mean.length <- ggplot(PrelimData, aes(x = zoop.mean.length)) +
  geom_histogram() +
  labs(title = "", y = "Count", x = "Zoop Mean Length (mm)") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black")  # Add axis lines
  )
z.mean.length

c.mean.length <- ggplot(PrelimData, aes(x = cladoceran.mean.length)) +
  geom_histogram() +
  labs(title = "", y = "Count", x = "Cladoceran Mean Length (mm)") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black")  # Add axis lines
  )
c.mean.length

prop.large.c <- ggplot(PrelimData, aes(x = prop.large.cladoceran)) +
  geom_histogram() +
  labs(title = "", y = "Count", x = "Proportion Large Cladoceran") +
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
custom_labels <- c("zoop.total.biomass" = "Total Zoop Biomass",
                   "Daphnia.prop.Cladoceran.biomass" = "Prop Daphnia Biomass",
                   "prop.Cladoceran.biomass" = "Prop Cladoceran Biomass",
                   "zoop.mean.length" = "Zoop Mean Length",
                   "cladoceran.mean.length" = "Cladoceran Mean Length",
                   "prop.large.cladoceran" = "Prop Large Cladoceran",
                   "zoop.Shannon.DI" = "Shannon DI")

#tiff("Prelim Zoop Metric Correlations.tiff", width = 8, height = 8, units = "in", res = 300)                   
ggpairs(Zoop.Metrics, columnLabels = custom_labels)
#dev.off()
