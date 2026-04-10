#this script runs GLLVM modes on the contemporary data

#libraries
library(gllvm)
library(tidyverse)
library(ggplot2)
library(GGally) #for ggpairs function
library(beepr)
library(corrplot)
library(gclus)

#read in data
data <- read.csv("Data/Input/Contemporary_Dataset_2026_04_06.csv")

#set seed to keep results consistent
set.seed(13453)
#use all computer cores - except 1 to do other things
TMB::openmp(parallel::detectCores()-1, DLL = "gllvm", autopar = TRUE)

#DATA PREP----------------------------


#COVARIATES TO USE
# secchi.meters.MPCA.Jul.to.Sept, gdd.year.5c, precip_5yr_avg_mm, area_ha, CDOM.lake.avg, depth.max.m, photic_prop_secchi.meters.MPCA.Jul.to.Sept, 
# SpinyWaterflea.yn, ZebraMussel.yn

#filter data
data.filter <- data %>% 
  #removes Hill south that does not have zoop data :(
  filter(!is.na(total_zoop_biomass)) %>% 
  #no NA values for selected covariates
  filter(!is.na(secchi.meters.MPCA.Jul.to.Sept) & 
           !is.na(gdd.year.5c) & 
           !is.na(precip_5yr_avg_mm) & 
           !is.na(CDOM.lake.avg) &
           !is.na(area_ha) & 
           !is.na(depth.max.m) & 
           !is.na(photic_prop_secchi.meters.MPCA.Jul.to.Sept) &
           !is.na(SpinyWaterflea.yn) &
           !is.na(ZebraMussel.yn)) %>% 
  #remove trout lakes  
  filter(!(BKT.CPUE > 0 | LAT.CPUE > 0 | RBT.CPUE > 0)) %>%  
  #remove walleye yoy column
  select(-WAE.YOY.CPUE)

#make covariate dataframe
x <- data.filter %>% 
  select(secchi.meters.MPCA.Jul.to.Sept, gdd.year.5c, precip_5yr_avg_mm, CDOM.lake.avg, area_ha, depth.max.m, photic_prop_secchi.meters.MPCA.Jul.to.Sept, 
         SpinyWaterflea.yn, ZebraMussel.yn) %>% 
  #set categorical variables as factors
  mutate(SpinyWaterflea.yn = as.factor(SpinyWaterflea.yn),
         ZebraMussel.yn = as.factor(ZebraMussel.yn)) %>% 
  #rename everything shorter
  rename(Secchi = secchi.meters.MPCA.Jul.to.Sept,
         GDD = gdd.year.5c,
         Precip = precip_5yr_avg_mm,
         CDOM = CDOM.lake.avg,
         Area = area_ha,
         Max_Depth = depth.max.m,
         Photic = photic_prop_secchi.meters.MPCA.Jul.to.Sept,
         SWF = SpinyWaterflea.yn,
         ZM = ZebraMussel.yn)

  #standardize all quantitative variables (except the proportion) with scale function
x_scale <- x  %>% 
  mutate(Secchi = scale(Secchi),
         GDD = scale(GDD),
         Precip = scale(Precip),
         CDOM = scale(CDOM),
         Area = scale(Area),
         Max_Depth = scale(Max_Depth))
#set x as a dataframe for rownames later
x_scale <- as.data.frame(x_scale)
  
  

#make a species abundance dataframe
#calculate relative abundance within fish and within zoops, then cbind them together

#isolate fish data
fish <- data.filter %>% 
  select(BIB.CPUE:YEP.CPUE)
#proportion of zeroes in fish data
fish_prop_0 <- colSums(fish == 0, na.rm = TRUE)/nrow(fish)
#isolate proportions of zeroes over 95%
fish_prop_0_0.95 <- fish_prop_0[fish_prop_0 > 0.95]
#make this a vector of names
fish_names_rare <- names(fish_prop_0_0.95)
#make a version of fish that does not have rare species (occurs in at least 5% of lake-years)
fish_no_rare <- fish %>% 
  select(-all_of(fish_names_rare))
#relative abundance
fish_rel_abun <- fish_no_rare / rowSums(fish_no_rare)

#isolate zoop data
zoop <- data.filter %>% 
  select(Acroperus.harpae:nauplii)
#proportion of zeroes in zoop data
zoop_prop_0 <- colSums(zoop == 0, na.rm = TRUE)/nrow(zoop)
#isolate proportions of zeroes over 95%
zoop_prop_0_0.95 <- zoop_prop_0[zoop_prop_0 > 0.95]
#make this a vector of names
zoop_names_rare <- names(zoop_prop_0_0.95)
#make a version of zoop that does not have rare species (occurs in at least 5% of lake-years)
zoop_no_rare <- zoop %>% 
  select(-all_of(zoop_names_rare))
#relative abundance
zoop_rel_abun <- zoop_no_rare / rowSums(zoop_no_rare)

#isolate zoop summary metrics (may or may not include them)
zoop_sum <- data.filter %>% 
  select(total_zoop_biomass:clad_ratio)
#check for correlations with these metrics
ggpairs(zoop_sum)
#I should choose either mean length of all zoops or cladocerans
#going with cladocerans because more preserved in sediments
zoop_sum_filter <- zoop_sum %>% 
  select(-mean_length_all_zoop)

#create the fish and zoop abundance matrix together
y <- cbind(fish_rel_abun, zoop_rel_abun)

#set N as number of lake-years and N.taxa as number of taxa in response matrix
N <- nrow(x)
N.taxa <- ncol(y)

#set rownames to be the same in both matrices
rownames(x) <- paste0(data.filter$lake_name, data.filter$Year)
rownames(x_scale) <- paste0(data.filter$lake_name, data.filter$Year) #note that these rownames are there, just not visible in viewer
rownames(y) <- paste0(data.filter$lake_name, data.filter$Year)
 


#create a version with the zoop summary stats too
y_zoop_sum <- cbind(y, zoop_sum_filter)


#create study design matrix
studyDesignData <- data.frame(lake = as.factor(data.filter$lake_name),
                              year = as.factor(data.filter$Year))
rownames(studyDesignData) <- paste0(data.filter$lake_name, data.filter$Year)

#i have notes from the GLLVM workshop that for compositional data I need to use an offset
#but with compositional data separated by fish and zoops it is no longer strictly compositional...


#SOME DATA EXPLORATION, GUIDED BY ZURR & IENO BOOK--------------------------------------------

#correlations between covariates (I have looked at this before but just confirming again)
ggpairs(x)
#all looks good

#percentages of zeroes in fish and zoops, vertical lines at 95% and 90%
#before removing rare species
SpecNames = colnames(fish)
PercZeros <-100 * colSums(fish[,SpecNames] == 0) / nrow(fish)
MyData <- data.frame(PercZeros = PercZeros,
                     Species   = SpecNames)
ggplot(MyData, aes(x = PercZeros, y = Species)) +
  geom_bar(stat = "identity", width = 0.2) +
  geom_vline(xintercept = 95, color = "blue")+
  geom_vline(xintercept = 90, color = "red")+
  labs(x = "Percentage of zeroes", 
       y = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 15))

SpecNames = colnames(zoop)
PercZeros <-100 * colSums(zoop[,SpecNames] == 0) / nrow(zoop)
MyData <- data.frame(PercZeros = PercZeros,
                     Species   = SpecNames)
ggplot(MyData, aes(x = PercZeros, y = Species)) +
  geom_bar(stat = "identity", width = 0.2) +
  geom_vline(xintercept = 95, color = "blue")+
  geom_vline(xintercept = 90, color = "red")+
  labs(x = "Percentage of zeroes", 
       y = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 15))

#after removing rare species
SpecNames = colnames(fish_no_rare)
PercZeros <-100 * colSums(fish_no_rare[,SpecNames] == 0) / nrow(fish_no_rare)
MyData <- data.frame(PercZeros = PercZeros,
                     Species   = SpecNames)
ggplot(MyData, aes(x = PercZeros, y = Species)) +
  geom_bar(stat = "identity", width = 0.2) +
  geom_vline(xintercept = 95, color = "blue")+
  geom_vline(xintercept = 90, color = "red")+
  labs(x = "Percentage of zeroes", 
       y = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 15))

SpecNames = colnames(zoop_no_rare)
PercZeros <-100 * colSums(zoop_no_rare[,SpecNames] == 0) / nrow(zoop_no_rare)
MyData <- data.frame(PercZeros = PercZeros,
                     Species   = SpecNames)
ggplot(MyData, aes(x = PercZeros, y = Species)) +
  geom_bar(stat = "identity", width = 0.2) +
  geom_vline(xintercept = 95, color = "blue")+
  geom_vline(xintercept = 90, color = "red")+
  labs(x = "Percentage of zeroes", 
       y = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 15))

#correlations between species
#don't run the ggpairs - it takes forever the result is too small to read with this many species...
#ggpairs(y)
corrplot(cor(y),
         method = "circle",
         type = "lower",
         tl.col = "black",
         tl.cex = 0.5,
         mar = c(0,0,0,0))
#there are some correlations here - this is on relative abundance data

#PLOTS BELOW ARE ALL ON RAW CPUE OR BIOMASS DATA
#Cleveland dotplots of quantitative variables
MyVar <- c("secchi.meters.MPCA.Jul.to.Sept", "gdd.year.5c", "precip_5yr_avg_mm", "area_ha", "CDOM.lake.avg", "depth.max.m", "photic_prop_secchi.meters.MPCA.Jul.to.Sept")
#' First create the row number:
data.filter$RowNum <- 1:nrow(data.filter)  #' nrow is the number of rows
#' Convert from wide to long format.
data.filter_long_quant <- pivot_longer(data.filter, 
                          cols = all_of(MyVar), 
                          names_to = "ID2", 
                          values_to = "AllY")
#' all_of: Ensures that non-existent column names result in an error 
#'         rather than being silently ignored.
#' Make  multipanel Cleveland dotplots
p <- ggplot(data = data.filter_long_quant, 
            aes(x = AllY, y = RowNum)) +
  geom_point(alpha = 0.5, size = 0.5) + 
  theme_minimal() +
  theme_bw() +
  xlab("Variable") + 
  ylab("Order of the data in spreadsheet") +
  facet_wrap(~ID2, scales = "free_x")
p 


#scatterplots of each species vs. each covariate
#' Convert the data from wide to long format.
SpecNames <- colnames(y)
data.filter_long <- pivot_longer(data.filter, 
                         cols = all_of(SpecNames), 
                         names_to = "ID2", 
                         values_to = "AllY")
data.filter_long <- as.data.frame(data.filter_long)
#' - AllY: All species stacked in 1 column.
#' - ID2:  Names for the species.

#secchi
secchi.plot <- ggplot(data = data.filter_long, 
            aes(y = AllY,   
                x = secchi.meters.MPCA.Jul.to.Sept)) +
  geom_point(alpha = 0.7, size = 0.5) + 
  geom_smooth(se = FALSE, col = "red", span = 1) +
  xlab("Secchi (m)") + 
  ylab("CPUE (fish) or Biomass (zoop)") +
  theme_minimal() +
  facet_wrap(~ID2, scales = "free_y")
secchi.plot 

#GDD
GDD.plot <- ggplot(data = data.filter_long, 
            aes(y = AllY,   
                x = gdd.year.5c)) +
  geom_point(alpha = 0.7, size = 0.5) + 
  geom_smooth(se = FALSE, col = "red", span = 1) +
  xlab("GDD") + 
  ylab("CPUE (fish) or Biomass (zoop)") +
  theme_minimal() +
  facet_wrap(~ID2, scales = "free_y")
GDD.plot


#Precip
precip.plot <- ggplot(data = data.filter_long, 
                   aes(y = AllY,   
                       x = precip_5yr_avg_mm)) +
  geom_point(alpha = 0.7, size = 0.5) + 
  geom_smooth(se = FALSE, col = "red", span = 1) +
  xlab("Annual Precip (mm)") + 
  ylab("CPUE (fish) or Biomass (zoop)") +
  theme_minimal() +
  facet_wrap(~ID2, scales = "free_y")
precip.plot

#Photic zone
photic.plot <- ggplot(data = data.filter_long, 
                      aes(y = AllY,   
                          x = photic_prop_secchi.meters.MPCA.Jul.to.Sept)) +
  geom_point(alpha = 0.7, size = 0.5) + 
  geom_smooth(se = FALSE, col = "red", span = 1) +
  xlab("Photic Zone Proportion") + 
  ylab("CPUE (fish) or Biomass (zoop)") +
  theme_minimal() +
  facet_wrap(~ID2, scales = "free_y")
photic.plot

#CDOM
cdom.plot <- ggplot(data = data.filter_long, 
                    aes(y = AllY,   
                        x = CDOM.lake.avg)) +
  geom_point(alpha = 0.7, size = 0.5) + 
  geom_smooth(se = FALSE, col = "red", span = 1) +
  xlab("Lake Average CDOM (a440)") + 
  ylab("CPUE (fish) or Biomass (zoop)") +
  theme_minimal() +
  facet_wrap(~ID2, scales = "free_y")
cdom.plot

#Area
area.plot <- ggplot(data = data.filter_long, 
                    aes(y = AllY,   
                        x = area_ha)) +
  geom_point(alpha = 0.7, size = 0.5) + 
  geom_smooth(se = FALSE, col = "red", span = 1) +
  xlab("Area (ha)") + 
  ylab("CPUE (fish) or Biomass (zoop)") +
  theme_minimal() +
  facet_wrap(~ID2, scales = "free_y")
area.plot

#Max Depth
depth.plot <- ggplot(data = data.filter_long, 
                    aes(y = AllY,   
                        x = depth.max.m)) +
  geom_point(alpha = 0.7, size = 0.5) + 
  geom_smooth(se = FALSE, col = "red", span = 1) +
  xlab("Maxiumum Depth (m)") + 
  ylab("CPUE (fish) or Biomass (zoop)") +
  theme_minimal() +
  facet_wrap(~ID2, scales = "free_y")
depth.plot


#SWF
swf.plot <- ggplot(data = data.filter_long, 
                     aes(y = AllY,   
                         x = SpinyWaterflea.yn)) +
  geom_boxplot()+
  xlab("Spiny Water Flea Presence") + 
  ylab("CPUE (fish) or Biomass (zoop)") +
  theme_minimal() +
  facet_wrap(~ID2, scales = "free_y")
swf.plot


#ZM
zm.plot <- ggplot(data = data.filter_long, 
                   aes(y = AllY,   
                       x = ZebraMussel.yn)) +
  geom_boxplot()+
  xlab("Zebra Mussel Presence") + 
  ylab("CPUE (fish) or Biomass (zoop)") +
  theme_minimal() +
  facet_wrap(~ID2, scales = "free_y")
zm.plot       

#Total CPUE of fish and biomass of zoops by year and lake
data.filter$lake_name <- as.factor(data.filter$lake_name)
data.filter$Year <- as.factor(data.filter$Year)
data.filter$total_fish_cpue <- rowSums(data.filter[,47:92])

lake.fish <- ggplot(data = data.filter, 
                   aes(y = total_fish_cpue,   
                       x = lake_name)) +
  geom_boxplot(alpha = 0.3, size = 0.5) + 
  xlab("Lake") + 
  theme_minimal() +
  theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1)) +
  ylab("Total Fish CPUE")
lake.fish

lake.zoop <- ggplot(data = data.filter, 
                    aes(y = total_zoop_biomass,   
                        x = lake_name)) +
  geom_boxplot(alpha = 0.3, size = 0.5) + 
  xlab("Lake") + 
  theme_minimal() +
  theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1)) +
  ylab("Total Zoop Biomass")
lake.zoop
#we have overall differences among lakes

year.fish <- ggplot(data = data.filter, 
                    aes(y = total_fish_cpue,   
                        x = Year)) +
  geom_boxplot(alpha = 0.3, size = 0.5) + 
  xlab("Year") + 
  theme_minimal() +
  theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1)) +
  ylab("Total Fish CPUE")
year.fish

year.zoop <- ggplot(data = data.filter, 
                    aes(y = total_zoop_biomass,   
                        x = Year)) +
  geom_boxplot(alpha = 0.3, size = 0.5) + 
  xlab("Year") + 
  theme_minimal() +
  theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1)) +
  ylab("Total Zoop Biomass")
year.zoop
#also some differences among years, could be an artifact of which lakes sampled when

#plot total fish cpue vs total zoop biomass
fish.zoop <- ggplot(data = data.filter, 
                    aes(y = total_zoop_biomass,   
                        x = total_fish_cpue)) +
  geom_point() + 
  xlab("Total Fish CPUE") + 
  theme_minimal() +
  theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1)) +
  ylab("Total Zoop Biomass")
fish.zoop
cor(data.filter$total_fish_cpue, data.filter$total_zoop_biomass)
#actually not super correlated


#CPUE of fish and biomass of zoops by year and lake BY SPECIES
data.filter_long$lake_name <- as.factor(data.filter_long$lake_name)
data.filter_long$Year <- as.factor(data.filter_long$Year)

#separate by fish and zoops to make plots easier to read
data.filter_long_fish <- data.filter_long %>% 
  filter(str_ends(ID2, "CPUE"))
data.filter_long_zoop <- data.filter_long %>% 
  filter(!str_ends(ID2, "CPUE"))

spp.lake.fish <- ggplot(data = data.filter_long_fish, 
            aes(y = AllY,   
                x = lake_name)) +
  geom_boxplot(alpha = 0.3, size = 0.5) + 
  xlab("Lake") + 
  theme_minimal() +
  theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1)) +
  ylab("CPUE") +
  facet_wrap(~ID2, scales = "free_y", ncol = 2)
spp.lake.fish

spp.lake.zoop <- ggplot(data = data.filter_long_zoop, 
                        aes(y = AllY,   
                            x = lake_name)) +
  geom_boxplot(alpha = 0.3, size = 0.5) + 
  xlab("Lake") + 
  theme_minimal() +
  theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1)) +
  ylab("Biomass") +
  facet_wrap(~ID2, scales = "free_y", ncol = 2)
spp.lake.zoop
#there are definitely lake effects, we already knew this

spp.year.fish <- ggplot(data = data.filter_long_fish, 
                   aes(y = AllY,   
                       x = Year)) +
  geom_boxplot(alpha = 0.3, size = 0.5) + 
  xlab("Year") + 
  theme_minimal() +
  theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1)) +
  ylab("CPUE") +
  facet_wrap(~ID2, scales = "free_y", ncol = 2)
spp.year.fish

spp.year.zoop <- ggplot(data = data.filter_long_zoop, 
                        aes(y = AllY,   
                            x = Year)) +
  geom_boxplot(alpha = 0.3, size = 0.5) + 
  xlab("Lake") + 
  theme_minimal() +
  theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1)) +
  ylab("Biomass") +
  facet_wrap(~ID2, scales = "free_y", ncol = 2)
spp.year.zoop
#THERE IS AN INCREASE IN COPEPODITES AND NAUPLII WHEN KYLIE STARTED ID IN 2020 - be aware of this!!
#some other general changes over time, could be an artifact of which lakes were sampled when


#REL ABUND Data Exploration---------------------------------------------------------------------------
#make all of these plots again but with relative abundance
#just make my life easy and remake data.filter.... but remember that you did this

data.filter <- cbind(studyDesignData, x, y)
MyVar <- colnames(x[,1:7])

#' First create zm.plot#' First create the row number:
data.filter$RowNum <- 1:nrow(data.filter)  #' nrow is the number of rows


#scatterplots of each species vs. each covariate
#' Convert the data from wide to long format.
SpecNames <- colnames(y)
data.filter_long <- pivot_longer(data.filter, 
                                 cols = all_of(SpecNames), 
                                 names_to = "ID2", 
                                 values_to = "AllY")
data.filter_long <- as.data.frame(data.filter_long)
#' - AllY: All species stacked in 1 column.
#' - ID2:  Names for the species.

#secchi
secchi.plot <- ggplot(data = data.filter_long, 
                      aes(y = AllY,   
                          x = Secchi)) +
  geom_point(alpha = 0.7, size = 0.5) + 
  geom_smooth(se = FALSE, col = "red", span = 1) +
  xlab("Secchi (m)") + 
  ylab("Relative Abundance") +
  theme_minimal() +
  facet_wrap(~ID2, scales = "free_y")
secchi.plot 

#GDD
GDD.plot <- ggplot(data = data.filter_long, 
                   aes(y = AllY,   
                       x = GDD)) +
  geom_point(alpha = 0.7, size = 0.5) + 
  geom_smooth(se = FALSE, col = "red", span = 1) +
  xlab("GDD") + 
  ylab("Relative Abundance") +
  theme_minimal() +
  facet_wrap(~ID2, scales = "free_y")
GDD.plot


#Precip
precip.plot <- ggplot(data = data.filter_long, 
                      aes(y = AllY,   
                          x = Precip)) +
  geom_point(alpha = 0.7, size = 0.5) + 
  geom_smooth(se = FALSE, col = "red", span = 1) +
  xlab("Annual Precip (mm)") + 
  ylab("Relative Abundance") +
  theme_minimal() +
  facet_wrap(~ID2, scales = "free_y")
precip.plot

#Photic zone
photic.plot <- ggplot(data = data.filter_long, 
                      aes(y = AllY,   
                          x = Photic)) +
  geom_point(alpha = 0.7, size = 0.5) + 
  geom_smooth(se = FALSE, col = "red", span = 1) +
  xlab("Photic Zone Proportion") + 
  ylab("Relative Abundance") +
  theme_minimal() +
  facet_wrap(~ID2, scales = "free_y")
photic.plot

#CDOM
cdom.plot <- ggplot(data = data.filter_long, 
                    aes(y = AllY,   
                        x = CDOM)) +
  geom_point(alpha = 0.7, size = 0.5) + 
  geom_smooth(se = FALSE, col = "red", span = 1) +
  xlab("Lake Average CDOM (a440)") + 
  ylab("Relative Abundance") +
  theme_minimal() +
  facet_wrap(~ID2, scales = "free_y")
cdom.plot

#Area
area.plot <- ggplot(data = data.filter_long, 
                    aes(y = AllY,   
                        x = Area)) +
  geom_point(alpha = 0.7, size = 0.5) + 
  geom_smooth(se = FALSE, col = "red", span = 1) +
  xlab("Area (ha)") + 
  ylab("Relative Abundance") +
  theme_minimal() +
  facet_wrap(~ID2, scales = "free_y")
area.plot

#Max Depth
depth.plot <- ggplot(data = data.filter_long, 
                     aes(y = AllY,   
                         x = Max_Depth)) +
  geom_point(alpha = 0.7, size = 0.5) + 
  geom_smooth(se = FALSE, col = "red", span = 1) +
  xlab("Maxiumum Depth (m)") + 
  ylab("Relative Abundance") +
  theme_minimal() +
  facet_wrap(~ID2, scales = "free_y")
depth.plot


#SWF
swf.plot <- ggplot(data = data.filter_long, 
                   aes(y = AllY,   
                       x = SWF)) +
  geom_boxplot()+
  xlab("Spiny Water Flea Presence") + 
  ylab("Relative Abundance") +
  theme_minimal() +
  facet_wrap(~ID2, scales = "free_y")
swf.plot


#ZM
zm.plot <- ggplot(data = data.filter_long, 
                  aes(y = AllY,   
                      x = ZM)) +
  geom_boxplot()+
  xlab("Zebra Mussel Presence") + 
  ylab("Relative Abundance") +
  theme_minimal() +
  facet_wrap(~ID2, scales = "free_y")
zm.plot       


#Relative abundance of fish and biomass of zoops by year and lake BY SPECIES

#separate by fish and zoops to make plots easier to read
data.filter_long_fish <- data.filter_long %>% 
  filter(str_ends(ID2, "CPUE"))
data.filter_long_zoop <- data.filter_long %>% 
  filter(!str_ends(ID2, "CPUE"))

spp.lake.fish <- ggplot(data = data.filter_long_fish, 
                        aes(y = AllY,   
                            x = lake)) +
  geom_boxplot(alpha = 0.3, size = 0.5) + 
  xlab("Lake") + 
  theme_minimal() +
  theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1)) +
  ylab("Relative Abundance") +
  facet_wrap(~ID2, scales = "free_y", ncol = 2)
spp.lake.fish

spp.lake.zoop <- ggplot(data = data.filter_long_zoop, 
                        aes(y = AllY,   
                            x = lake)) +
  geom_boxplot(alpha = 0.3, size = 0.5) + 
  xlab("Lake") + 
  theme_minimal() +
  theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1)) +
  ylab("Relative Abundance") +
  facet_wrap(~ID2, scales = "free_y", ncol = 2)
spp.lake.zoop
#there are definitely lake effects, we already knew this

spp.year.fish <- ggplot(data = data.filter_long_fish, 
                        aes(y = AllY,   
                            x = year)) +
  geom_boxplot(alpha = 0.3, size = 0.5) + 
  xlab("Year") + 
  theme_minimal() +
  theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1)) +
  ylab("Relative Abundance") +
  facet_wrap(~ID2, scales = "free_y", ncol = 2)
spp.year.fish

spp.year.zoop <- ggplot(data = data.filter_long_zoop, 
                        aes(y = AllY,   
                            x = year)) +
  geom_boxplot(alpha = 0.3, size = 0.5) + 
  xlab("Lake") + 
  theme_minimal() +
  theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1)) +
  ylab("Relative Abundance") +
  facet_wrap(~ID2, scales = "free_y", ncol = 2)
spp.year.zoop
#THERE IS AN INCREASE IN COPEPODITES AND NAUPLII WHEN KYLIE STARTED ID IN 2020 - be aware of this!!
#some other general changes over time, could be an artifact of which lakes were sampled when


#pick a lake to look at change over time within a lake
table(data.filter$lake)
#I'll do Mille Lacs and Sand Point since they have the most lake-years

data.filter_long_fish_ML <- data.filter_long_fish %>% 
  filter(lake == "Mille Lacs")
data.filter_long_zoop_ML <- data.filter_long_zoop %>% 
  filter(lake == "Mille Lacs")

spp.year.fish.ML <- ggplot(data = data.filter_long_fish_ML, 
                        aes(y = AllY,   
                            x = year)) +
  geom_boxplot(alpha = 0.3, size = 0.5) + 
  xlab("Year") + 
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  ylab("Relative Abundance") +
  facet_wrap(~ID2, scales = "free_y", ncol = 2)
spp.year.fish.ML

spp.year.zoop.ML <- ggplot(data = data.filter_long_zoop_ML, 
                        aes(y = AllY,   
                            x = year)) +
  geom_boxplot(alpha = 0.3, size = 0.5) + 
  xlab("Lake") + 
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  ylab("Relative Abundance") +
  facet_wrap(~ID2, scales = "free_y", ncol = 2)
spp.year.zoop.ML



data.filter_long_fish_SP <- data.filter_long_fish %>% 
  filter(lake == "Sand Point")
data.filter_long_zoop_SP <- data.filter_long_zoop %>% 
  filter(lake == "Sand Point")

spp.year.fish.SP <- ggplot(data = data.filter_long_fish_SP, 
                        aes(y = AllY,   
                            x = year)) +
  geom_boxplot(alpha = 0.3, size = 0.5) + 
  xlab("Year") + 
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  ylab("Relative Abundance") +
  facet_wrap(~ID2, scales = "free_y", ncol = 2)
spp.year.fish.SP

spp.year.zoop.SP <- ggplot(data = data.filter_long_zoop_SP, 
                        aes(y = AllY,   
                            x = year)) +
  geom_boxplot(alpha = 0.3, size = 0.5) + 
  xlab("Year") + 
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  ylab("Relative Abundance") +
  facet_wrap(~ID2, scales = "free_y", ncol = 2)
spp.year.zoop.SP

#There is definitely change over time between years within a lake. The question is if we think years across lakes are related to each other




#MODELS----------------------------------------------------

#models with no covariates
model1 <- gllvm(y = y, num.lv = 2, family = "tweedie", Power = NULL,
            control.start = (n.init = 3))
#power argument tells R to find the tweedie coefficient that best fits my data
beep()
#summary and model performance plots
summary(model1)
gllvm::ordiplot(model1)
plot(model1)
#check power
model1$Power

#extract tweedie power that the model estimated:
m2$params$Power
m2$call


#okay we need to start accounting for study design: nested random lake effect and random year effect



#make a model with random intercept for each lake. 
#This assumes that random effect estimates are the same for all species
#species-specific intercepts and slopes
#no latent variables
model2 <- gllvm(y = y, x = x, studyDesign = studyDesignData, row.eff = ~(1|lake), num.lv = 0, family = "tweedie", Power = NULL, 
            control.start = (n.init = 5), jitter.var = 0.1)
beep()
#summary and model performance plots
summary(model2)
gllvm::ordiplot(model2)
plot(model2)
#check power
model2$Power



#same as model2 but now there is one latent variable
model3 <- gllvm(y = y, x = x, studyDesign = studyDesignData, row.eff = ~(1|lake), num.lv = 1, family = "tweedie", Power = NULL, 
                control.start = (n.init = 5), jitter.var = 0.1)
beep()
#check power
model3$Power
#summary and model performance plots
summary(model3)
gllvm::ordiplot(model3) #this is scatterplot of row number vs. latent variable value because only one LV
plot(model3)
#plot latent variable and factor loadings
#access latent variable
u1 <- model3$lvs[, "LV1"]
#access factor loadings
theta.sigmalv <- model3$lvs[, "LV1"] * model3$params$sigma.lv

plot(u1)
plot(theta.sigmalv)

#get residual correlations
Theta <- getResidualCor(model3)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#okay... so everything is 1 or negative 1 which seems wrong...

#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model3, "row.params.random"), Lake = names(coef(model3, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()
#look at sigma of random effects
model3$params$sigma


#same as model2 but now there are 2 latent variables
model4 <- gllvm(y = y, x = x, studyDesign = studyDesignData, row.eff = ~(1|lake), num.lv = 2, family = "tweedie", Power = NULL, 
                control.start = (n.init = 5), jitter.var = 0.1)
beep()
#check power
model4$Power
#summary and model performance plots
summary(model4)
gllvm::ordiplot(model4)
plot(model4)
#get residual correlations
Theta <- getResidualCor(model4)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#looking better!





#Let's jump off the deep end and try to code the whole model, but without latent variables
#the random effects here are NOT species-specific
model5 <- gllvm(y = y, x = x, studyDesign = studyDesignData, 
                formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM,
                row.eff = ~(1|lake) + (1|year), 
                num.lv = 0, family = "tweedie", Power = NULL, 
                sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                control.start = (n.init = 5), jitter.var = 0.1)
beep()
#check power
model5$Power
#summary and model performance plots
summary(model5)
plot(model5)


#make the random effects species-specific
#diag argument keeps the species-specific random effects from being correlated with each other (completeley independent) - also helps model converge
model6 <- gllvm(y = y, x = x, studyDesign = studyDesignData, 
                formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM + diag((1|lake)) + diag((1|year)),
                num.lv = 0, family = "tweedie", Power = NULL, 
                sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                control.start = (n.init = 5), jitter.var = 0.1)
beep()
#check power
model6$Power
#summary and model performance plots
summary(model6)
plot(model6)


#random effects are NOT species-specific but now we have 1 latent variable
model7 <- gllvm(y = y, x = x, studyDesign = studyDesignData, 
                formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM,
                row.eff = ~(1|lake) + (1|year), 
                num.lv = 1, family = "tweedie", Power = NULL, 
                sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                control.start = (n.init = 5), jitter.var = 0.1)
beep()


#random effects are NOT species-specific but now we have 2 latent variables
model8 <- gllvm(y = y, x = x, studyDesign = studyDesignData, 
                formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM,
                row.eff = ~(1|lake) + (1|year), 
                num.lv = 2, family = "tweedie", Power = NULL, 
                sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                control.start = (n.init = 5), jitter.var = 0.1)
beep()

