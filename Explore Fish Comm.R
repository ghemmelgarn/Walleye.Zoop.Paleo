#This code uses the exploratory dataset to make figures to look at walleye and LMB fish communities
#Also comapres LMB between trapnets and gillnets via a figure

#Packages
library(dplyr)
library(ggplot2)
library(gridExtra)
library(mosaic)
library(ggExtra)

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
print(WAE.LMB)
#dev.off()

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
print(LMB.TN_GN)
#dev.off()


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

#PLOTS WITH ALL MN LAKES, not just lakes matched to zoop data
#import data
Exp.Data.MN.All <- read.csv("Exploratory Data.ALL_MN_Fish_Lakes.csv")

#filter out lakes with no walleye or LMB or Black Crappie in gillnets - also filters out lakes with NA values for all of them
#this should get rid of trout lakes
#only using gillnet data now, no trap nets
Exp.Data.filter.MN.All <- filter(Exp.Data.MN.All, GN.walleye > 0 | GN.largemouth_bass > 0 | GN.black_crappie > 0)
#Change NA Values in Match column to "No"
Exp.Data.filter.MN.All$Match [is.na(Exp.Data.filter.MN.All$Match)] <- "No"


#Walleye Gillnet CPUE vs. Largemouth CPUE (Gillnet) - all MN lakes, zoop/fish match data colored
#arrange data so matched zoop data get plotted last and show up on top of no zoop data
Exp.Data.filter.MN.All <- Exp.Data.filter.MN.All %>% arrange(desc(Match))
#tiff("WAE vs. LMB All MN Lakes.tiff", width = 7, height = 7, units = "in", res = 300)
WAE.LMB.All.MN <- ggplot(Exp.Data.filter.MN.All, aes(x = GN.walleye)) + 
  geom_point(aes(y = GN.largemouth_bass, color = ifelse(Match == "Exact", "Match Zoop Data", "No Zoop Data")))+
  labs(title = "Walleye vs. LMB", y = "Largemouth Bass Gillnet CPUE", x = "Walleye Gillnet CPUE", color = NULL) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 5), labels = seq(0, 30, by = 5)) +
  scale_x_continuous(limits = c(0, 120), breaks = seq(0, 120, by = 20), labels = seq(0, 120, by = 20)) +
  scale_color_manual(values = c("Match Zoop Data" = "blue", "No Zoop Data" = "lightgray")) + 
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
Exp.Data.filter.MN.All <- Exp.Data.filter.MN.All %>% arrange(desc(Match))
#tiff("WAE vs. LMB All MN Lakes.zoom.tiff", width = 7, height = 7, units = "in", res = 300)
WAE.LMB.All.MN.zoom <- ggplot(Exp.Data.filter.MN.All, aes(x = GN.walleye)) + 
  geom_point(aes(y = GN.largemouth_bass, color = ifelse(Match == "Exact", "Match Zoop Data", "No Zoop Data")))+
  labs(title = "Walleye vs. LMB", y = "Largemouth Bass Gillnet CPUE", x = "Walleye Gillnet CPUE", color = NULL) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2), labels = seq(0, 10, by = 2)) +
  scale_x_continuous(limits = c(0, 65), breaks = seq(0, 65, by = 10), labels = seq(0, 65, by = 10)) +
  scale_color_manual(values = c("Match Zoop Data" = "blue", "No Zoop Data" = "lightgray")) + 
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black")  # Add axis lines
  )
#print(WAE.LMB.All.MN.zoom)
#print code not needed with ggMarginal density plots
ggMarginal(WAE.LMB.All.MN.zoom, groupColour = TRUE, groupFill = TRUE)
#dev.off()

#Walleye Gillnet CPUE vs. Black Crappie CPUE (Gillnet) - all MN lakes, zoop/fish match data colored
#arrange data so matched zoop data get plotted last and show up on top of no zoop data
Exp.Data.filter.MN.All <- Exp.Data.filter.MN.All %>% arrange(desc(Match))
#tiff("WAE vs. BC All MN Lakes.tiff", width = 7, height = 7, units = "in", res = 300)
WAE.BC.All.MN <- ggplot(Exp.Data.filter.MN.All, aes(x = GN.walleye)) + 
  geom_point(aes(y = GN.black_crappie, color = ifelse(Match == "Exact", "Match Zoop Data", "No Zoop Data")))+
  labs(title = "Walleye vs. BC", y = "Black Crappie Gillnet CPUE", x = "Walleye Gillnet CPUE", color = NULL) +
  #scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 5), labels = seq(0, 30, by = 5)) +
  scale_x_continuous(limits = c(0, 120), breaks = seq(0, 120, by = 20), labels = seq(0, 120, by = 20)) +
  scale_color_manual(values = c("Match Zoop Data" = "blue", "No Zoop Data" = "lightgray")) + 
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black")  # Add axis lines
  )
#print(WAE.LMB.All.MN)
#print code not needed with ggMarginal density plots
ggMarginal(WAE.BC.All.MN, groupColour = TRUE, groupFill = TRUE)
#dev.off()

#same as above but zoomed in lower left corner
#arrange data so matched zoop data get plotted last and show up on top of no zoop data
Exp.Data.filter.MN.All <- Exp.Data.filter.MN.All %>% arrange(desc(Match))
#tiff("WAE vs. BC All MN Lakes.zoom.tiff", width = 7, height = 7, units = "in", res = 300)
WAE.BC.All.MN.zoom <- ggplot(Exp.Data.filter.MN.All, aes(x = GN.walleye)) + 
  geom_point(aes(y = GN.black_crappie, color = ifelse(Match == "Exact", "Match Zoop Data", "No Zoop Data")))+
  labs(title = "Walleye vs. BC", y = "Black Crappie Gillnet CPUE", x = "Walleye Gillnet CPUE", color = NULL) +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 10), labels = seq(0, 60, by = 10)) +
  scale_x_continuous(limits = c(0, 65), breaks = seq(0, 65, by = 10), labels = seq(0, 65, by = 10)) +
  scale_color_manual(values = c("Match Zoop Data" = "blue", "No Zoop Data" = "lightgray")) + 
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black")  # Add axis lines
  )
#print(WAE.BC.All.MN.zoom)
#print code not needed with ggMarginal density plots
ggMarginal(WAE.BC.All.MN.zoom, groupColour = TRUE, groupFill = TRUE)
#dev.off()
