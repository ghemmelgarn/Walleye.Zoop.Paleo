#This code uses the exploratory dataset to make figures to look at walleye and LMB fish communities
#Also comapres LMB between trapnets and gillnets via a figure

#Packages
library(dplyr)
library(ggplot2)
library(gridExtra)
library(mosaic)

#import data
Exp.Data <- read.csv("Exploratory Data.csv")

#filter out lakes with no walleye or LMB - also filters out lakes with NA values for all of them
Exp.Data.filter <- filter(Exp.Data, GN.walleye > 0 | TN.walleye > 0 | GN.largemouth_bass > 0 | TN.largemouth_bass > 0)
#testing opposite filter to make sure I filter out what I wanted to
#test <- filter(Exp.Data, (is.na(GN.walleye) | GN.walleye == 0)& (is.na(TN.walleye) | TN.walleye == 0) & (is.na(GN.largemouth_bass)|GN.largemouth_bass == 0) & (is.na(TN.largemouth_bass)|TN.largemouth_bass == 0))
#Not going to use this data because its ok to have the 0,0 point, but it is interesting to know that there are 71 lakes with either 0,0 or null values

#Walleye Gillnet CPUE vs. Largemouth CPUE (Gillnet and trapnet)
#tiff("WAE vs. LMB.tiff", width = 7, height = 7, units = "in", res = 300)
WAE.LMB <- ggplot(Exp.Data, aes(x = GN.walleye)) + 
  geom_point(aes(y = GN.largemouth_bass, color = "Gillnet"))+
  geom_point(aes(y = TN.largemouth_bass, color = "Trapnet"))+
  labs(title = "Walleye vs. LMB", y = "Largemouth Bass CPUE", x = "Walleye CPUE (Gillnet)") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2), labels = seq(0, 10, by = 2)) +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black")  # Add axis lines
  )
#print(WAE.LMB)
dev.off()

#No LMB with waleye CPUE > 20, so zooming in on the other data
#tiff("WAE less than 20 vs. LMB.tiff", width = 7, height = 7, units = "in", res = 300)
WAE.LMB.zoom <- ggplot(Exp.Data, aes(x = GN.walleye)) + 
  geom_point(aes(y = GN.largemouth_bass, color = "Gillnet"))+
  geom_point(aes(y = TN.largemouth_bass, color = "Trapnet"))+
  labs(title = "Walleye vs. LMB", y = "Largemouth Bass CPUE", x = "Walleye CPUE (Gillnet)") +
  scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 5), labels = seq(0, 20, by = 5)) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2), labels = seq(0, 10, by = 2)) +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black")  # Add axis lines
  )
print(WAE.LMB.zoom)
#dev.off()

#Largemouth Trapnet vs. Largemouth Gillnet
#tiff("LMB Gear.tiff", width = 7, height = 7, units = "in", res = 300)
LMB.TN_GN <- ggplot(Exp.Data, aes(x = TN.largemouth_bass, y = GN.largemouth_bass)) +
  geom_point()+
  labs(title = "LMB Gillnet vs. Trapnet", y = "Largemouth Bass CPUE Gillnet", x = "Largemouth Bass CPUE Trapnet") +
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2), labels = seq(0, 10, by = 2)) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2), labels = seq(0, 10, by = 2)) +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black")  # Add axis lines
  )
#print(LMB.TN_GN)
dev.off()


#Walleye Gillnet vs. walleye Trapnet (just out of curiosity - I know gillnets are better)
WAE.TN_GN <- ggplot(Exp.Data, aes(x = TN.walleye, y = GN.walleye)) +
  geom_point()+
  labs(title = "WAE Gillnet vs. Trapnet", y = "Walleye CPUE Gillnet", x = "Walleye Trapnet") +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, by = 10), labels = seq(0, 40, by = 10)) +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, by = 10), labels = seq(0, 40, by = 10)) +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black")  # Add axis lines
  )
print(WAE.TN_GN)
#error message because lots of NA (goes away if you filter them out)
