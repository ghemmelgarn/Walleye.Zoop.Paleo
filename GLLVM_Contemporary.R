#this script runs GLLVM modes on the contemporary data

#libraries
library(gllvm)
library(tidyverse)
library(ggplot2)
library(GGally) #for ggpairs function
library(beepr)
library(corrplot)
library(gclus)
library(gridExtra)
library(ggrepel)

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
  select(-WAE.YOY.CPUE) %>% 
  #make rainbow smelt and and common carp into yes/no covariates like bytho and zebra mussels, then remove their CPUE columns
  mutate(RBS.yn = ifelse(RBS.CPUE > 0, "yes", "no"),
         CAP.yn = ifelse(CAP.CPUE > 0, "yes", "no")) %>% 
  #make all teh VNP lakes say yes for RBS
  mutate(RBS.yn = ifelse(lake_name == "Kabetogama" | lake_name == "Sand Point" | lake_name == "Namakan", "yes", "no")) %>% 
  #make all the carp lakes say yes after initial detection year
  mutate(CAP.yn = ifelse((lake_name == "Artichoke" & Year >= 2008)|(lake_name == "Peltier" & Year >= 2021)|(lake_name == "Belle" & Year >= 2008)|(lake_name == "Carlos" & Year >= 2008)|(lake_name == "Green" & Year >= 2018)|(lake_name == "Madison" & Year >= 2008)|(lake_name == "Mille Lacs" & Year >= 2015), "yes", "no")) %>% 
  #add analyst column
  mutate(analyst = ifelse(Year < 2020, "A", "B"))
 

temp <- data.filter %>% 
  select(lake_name, Year, CAP.yn, CAP.CPUE, RBS.yn, RBS.CPUE, analyst)


#make covariate dataframe
x <- data.filter %>% 
  select(secchi.meters.MPCA.Jul.to.Sept, gdd.year.5c, precip_5yr_avg_mm, CDOM.lake.avg, area_ha, depth.max.m, photic_prop_secchi.meters.MPCA.Jul.to.Sept, 
         SpinyWaterflea.yn, ZebraMussel.yn, RBS.yn, CAP.yn, analyst) %>% 
  #set categorical variables as factors
  mutate(SpinyWaterflea.yn = as.factor(SpinyWaterflea.yn),
         ZebraMussel.yn = as.factor(ZebraMussel.yn),
         RBS.yn = as.factor(RBS.yn),
         CAP.yn = as.factor(CAP.yn),
         analyst = as.factor(analyst)) %>% 
  #rename everything shorter
  rename(Secchi = secchi.meters.MPCA.Jul.to.Sept,
         GDD = gdd.year.5c,
         Precip = precip_5yr_avg_mm,
         CDOM = CDOM.lake.avg,
         Area = area_ha,
         Max_Depth = depth.max.m,
         Photic = photic_prop_secchi.meters.MPCA.Jul.to.Sept,
         SWF = SpinyWaterflea.yn,
         ZM = ZebraMussel.yn,
         RBS = RBS.yn,
         CAP = CAP.yn)

  #standardize all quantitative variables (except the proportion) with scale function
x_scale <- x  %>% 
  mutate(Secchi = scale(Secchi),
         GDD = scale(GDD),
         Precip = scale(Precip),
         CDOM = scale(CDOM),
         Area = scale(Area),
         Max_Depth = scale(Max_Depth),
         Photic = scale(Photic, center = FALSE, scale = TRUE)) #SCALE BUT NOT CENTER FOR MODEL PERFORMANCE REASONS
#set x as a dataframe for rownames later
x_scale <- as.data.frame(x_scale)
  
  

#make a species abundance dataframe
#calculate relative abundance within fish and within zoops, then cbind them together
#now to be included, a taxa group must be present in at least 95% of samples (lake-years), be present in more than two lakes, and have passed an ecological gut check by Grace (is this species usually found in lakes?)

#INVESTIGATING RARE SPECIES
#First question: are there any species only present in one lake?
#regroup the daphnia how we discussed in committee meeting
data.daphnia <- data.filter %>% 
  mutate(Daphnia.small.rare = rowSums(across(c(Daphnia.rosea, Daphnia.ambigua, Daphnia.sp.)))) %>% 
  select(-Daphnia.rosea, -Daphnia.ambigua, -Daphnia.sp.) %>% 
  relocate(Daphnia.small.rare, .after = Daphnia.retrocurva)

#get the max value for each species in each lake (will be 0 if never present)
lake.spp <- data.daphnia %>% 
  group_by(lake_name) %>% 
  summarize(across(BIB.CPUE:nauplii, max),
            .groups = 'drop')
#for each species, count the number of lakes where it has a value greater than 0 in at least one year
spp.lake.count <- colSums(lake.spp > 0)
spp.lake.count
#make a list of species present in two or fewer lakes
spp.drop.lake <- names(spp.lake.count[spp.lake.count < 3])
spp.drop.lake

#calculate species present in less than 95% of lake-year samples
#isolate species
spp <- data.daphnia %>% 
  select(BIB.CPUE:nauplii)
#proportion of zeroes in species data
spp_prop_0 <- colSums(spp == 0, na.rm = TRUE)/nrow(spp)
#isolate proportions of zeroes over 95%
spp_prop_0_0.95 <- spp_prop_0[spp_prop_0 > 0.95]
#make this a vector of names
spp_names_rare <- names(spp_prop_0_0.95)

#combine the list of names for the two reasons to be dropped, only keep one if repeated in both lists
spp.drop <- unique(c(spp.drop.lake, spp_names_rare))
spp.drop

#remove columns for the species to drop
spp.filter <- spp %>% 
  select(-all_of(spp.drop))
#what's left?
names(spp.filter)

#look at magnitude of fish vs. zoop data
mag.plot.data <- spp.filter %>% 
  pivot_longer(cols = everything(), names_to = "species", values_to = "abundance") %>% 
  mutate(group = ifelse(str_ends(species, "CPUE"), "fish", "zoop"))
mag.plot.raw <- ggplot(data = mag.plot.data, aes(x = species, y = abundance, color = group))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
mag.plot.raw
#plot again but limit y axis to not include outliers
mag.plot.raw.zoom <- ggplot(data = mag.plot.data, aes(x = species, y = abundance, color = group))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  scale_y_continuous(limits = c(0,5))
mag.plot.raw.zoom
#actually looks okay, not transforming at all

#isolate fish data
fish <- spp.filter %>% 
  select(BLC.CPUE:YEP.CPUE)
#relative abundance
fish_rel_abun <- fish / rowSums(fish)

#isolate zoop data
zoop <- spp.filter %>% 
  select(Alona.sp.:nauplii)
#relative abundance
zoop_rel_abun <- zoop / rowSums(zoop)

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
y_rel_abun <- cbind(fish_rel_abun, zoop_rel_abun)
y_raw <- cbind(fish, zoop)

#set N as number of lake-years and N.taxa as number of taxa in response matrix
N <- nrow(x)
N.taxa <- ncol(y_rel_abun)

#set rownames to be the same in both matrices
rownames(x) <- paste0(data.filter$lake_name, data.filter$Year)
rownames(x_scale) <- paste0(data.filter$lake_name, data.filter$Year) #note that these rownames are there, just not visible in viewer
rownames(y_rel_abun) <- paste0(data.filter$lake_name, data.filter$Year)
rownames(y_raw) <- paste0(data.filter$lake_name, data.filter$Year)
 
# #create a version with the zoop summary stats too
# y_zoop_sum <- cbind(y, zoop_sum_filter)
#NOT DOING THIS


#create study design matrix
studyDesignData <- data.frame(lake = as.factor(data.filter$lake_name),
                              year = as.factor(data.filter$Year))
rownames(studyDesignData) <- paste0(data.filter$lake_name, data.filter$Year)

#i have notes from the GLLVM workshop that for compositional data I need to use an offset
#but with compositional data separated by fish and zoops it is no longer strictly compositional...


#SOME DATA EXPLORATION, GUIDED BY ZURR & IENO BOOK--------------------------------------------
#I CHANGED SOME OF THE CALCULATIONS PREVIOUS TO THIS SECTION AND DID NOT UPDATE THIS SECTION

#correlations between covariates (I have looked at this before but just confirming again)
ggpairs(x)
ggpairs(x_scale)
v#all looks good

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
data.filter_long_quant <- pivot_longer(data.daphnia, 
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
SpecNames <- colnames(data.daphnia[47:118])
data.filter_long <- pivot_longer(data.daphnia, 
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



#Look at walleye responses to each covariate
walleye <- data.daphnia %>% 
  select(lake_name, Year, WAE.CPUE, secchi.meters.MPCA.Jul.to.Sept, gdd.year.5c, precip_5yr_avg_mm, photic_prop_secchi.meters.MPCA.Jul.to.Sept,
         CDOM.lake.avg, area_ha, depth.max.m, SpinyWaterflea.yn, ZebraMussel.yn, RBS.yn, CAP.yn)
wae.secchi <- ggplot(data = walleye, aes(x = secchi.meters.MPCA.Jul.to.Sept, y = WAE.CPUE))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(title = "Secchi")+
  theme_classic()
wae.gdd <- ggplot(data = walleye, aes(x = gdd.year.5c, y = WAE.CPUE))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(title = "GDD")+
  theme_classic()
wae.precip <- ggplot(data = walleye, aes(x = precip_5yr_avg_mm, y = WAE.CPUE))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(title = "Precip")+
  theme_classic()
wae.no.outlier <- walleye %>% 
  filter(precip_5yr_avg_mm < 1000)
wae.precip.no.outlier <- ggplot(data = wae.no.outlier, aes(x = precip_5yr_avg_mm, y = WAE.CPUE))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(title = "Precip No Outlier")+
  theme_classic()
wae.photic <- ggplot(data = walleye, aes(x = photic_prop_secchi.meters.MPCA.Jul.to.Sept, y = WAE.CPUE))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(title = "Photic Proportion")+
  theme_classic()
wae.cdom <- ggplot(data = walleye, aes(x = CDOM.lake.avg, y = WAE.CPUE))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(title = "CDOM")+
  theme_classic()
wae.area <- ggplot(data = walleye, aes(x = area_ha, y = WAE.CPUE))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(title = "Area")+
  theme_classic()
wae.depth <- ggplot(data = walleye, aes(x = depth.max.m, y = WAE.CPUE))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(title = "Max Depth")+
  theme_classic()
wae.swf <- ggplot(data = walleye, aes(x = SpinyWaterflea.yn, y = WAE.CPUE))+
  geom_boxplot()+
  labs(title = "Spiny Water Flea")+
  theme_classic()
wae.zm <- ggplot(data = walleye, aes(x = ZebraMussel.yn, y = WAE.CPUE))+
  geom_boxplot()+
  labs(title = "Zebra Mussel")+
  theme_classic()
wae.rbs <- ggplot(data = walleye, aes(x = RBS.yn, y = WAE.CPUE))+
  geom_boxplot()+
  labs(title = "Rainbow Smelt")+
  theme_classic()
wae.cap <- ggplot(data = walleye, aes(x = CAP.yn, y = WAE.CPUE))+
  geom_boxplot()+
  labs(title = "Common Carp")+
  theme_classic()
wae.plots <- grid.arrange(wae.secchi, wae.gdd, wae.precip, wae.precip.no.outlier, wae.photic, wae.cdom, wae.area, wae.depth, wae.swf, wae.zm, wae.rbs, wae.cap, ncol = 4)
#ggsave(filename = "Wae_covariates_cpue.png", plot = wae.plots, width = 8, height = 6, units = "in", dpi = 300)

#look at the walleye but with relative abundance now
wae.rel.abun <- y_rel_abun[,"WAE.CPUE"]
walleye.rel.abun <- cbind(walleye, wae.rel.abun)
wae.secchi <- ggplot(data = walleye.rel.abun, aes(x = secchi.meters.MPCA.Jul.to.Sept, y = wae.rel.abun))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(title = "Secchi")+
  theme_classic()
wae.gdd <- ggplot(data = walleye.rel.abun, aes(x = gdd.year.5c, y = wae.rel.abun))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(title = "GDD")+
  theme_classic()
wae.precip <- ggplot(data = walleye.rel.abun, aes(x = precip_5yr_avg_mm, y = wae.rel.abun))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(title = "Precip")+
  theme_classic()
wae.no.outlier.rel <- walleye.rel.abun %>% 
  filter(precip_5yr_avg_mm < 1000)
wae.precip.no.outlier <- ggplot(data = wae.no.outlier.rel, aes(x = precip_5yr_avg_mm, y = wae.rel.abun))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(title = "Precip No Outlier")+
  theme_classic()
wae.photic <- ggplot(data = walleye.rel.abun, aes(x = photic_prop_secchi.meters.MPCA.Jul.to.Sept, y = wae.rel.abun))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(title = "Photic Proportion")+
  theme_classic()
wae.cdom <- ggplot(data = walleye.rel.abun, aes(x = CDOM.lake.avg, y = wae.rel.abun))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(title = "CDOM")+
  theme_classic()
wae.area <- ggplot(data = walleye.rel.abun, aes(x = area_ha, y = wae.rel.abun))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(title = "Area")+
  theme_classic()
wae.depth <- ggplot(data = walleye.rel.abun, aes(x = depth.max.m, y = wae.rel.abun))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(title = "Max Depth")+
  theme_classic()
wae.swf <- ggplot(data = walleye.rel.abun, aes(x = SpinyWaterflea.yn, y = wae.rel.abun))+
  geom_boxplot()+
  labs(title = "Spiny Water Flea")+
  theme_classic()
wae.zm <- ggplot(data = walleye.rel.abun, aes(x = ZebraMussel.yn, y = wae.rel.abun))+
  geom_boxplot()+
  labs(title = "Zebra Mussel")+
  theme_classic()
wae.rbs <- ggplot(data = walleye.rel.abun, aes(x = RBS.yn, y = wae.rel.abun))+
  geom_boxplot()+
  labs(title = "Rainbow Smelt")+
  theme_classic()
wae.cap <- ggplot(data = walleye.rel.abun, aes(x = CAP.yn, y = wae.rel.abun))+
  geom_boxplot()+
  labs(title = "Common Carp")+
  theme_classic()
wae.plots <- grid.arrange(wae.secchi, wae.gdd, wae.precip, wae.precip.no.outlier, wae.photic, wae.cdom, wae.area, wae.depth, wae.swf, wae.zm, wae.rbs, wae.cap, ncol = 4)
#ggsave(filename = "Wae_covariates_rel_abundance.png", plot = wae.plots, width = 8, height = 6, units = "in", dpi = 300)

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
model1 <- gllvm(y = y_raw, num.lv = 2, family = "tweedie", Power = NULL,
            control.start = (n.init = 3))
#power argument tells R to find the tweedie coefficient that best fits my data
beep()
#summary and model performance plots
summary(model1)
gllvm::ordiplot(model1)
plot(model1)
#check power
model1$Power
#get residual correlations
Theta <- getResidualCor(model1)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")

#extract tweedie power that the model estimated:
m2$params$Power
m2$call


#okay we need to start accounting for study design: nested random lake effect and random year effect



#make a model with random intercept for each lake. 
#This assumes that random effect estimates are the same for all species
#species-specific intercepts and slopes
#no latent variables
model2 <- gllvm(y = y, studyDesign = studyDesignData, row.eff = ~(1|lake), num.lv = 0, family = "tweedie", Power = NULL, 
            control.start = (n.init = 5), jitter.var = 0.1)
beep()
#summary and model performance plots
summary(model2)
gllvm::ordiplot(model2)
plot(model2)
#check power
model2$Power



#same as model2 but now there is one latent variable
model3 <- gllvm(y = y, studyDesign = studyDesignData, row.eff = ~(1|lake), num.lv = 1, family = "tweedie", Power = NULL, 
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
model4 <- gllvm(y = y, studyDesign = studyDesignData, row.eff = ~(1|lake), num.lv = 2, family = "tweedie", Power = NULL, 
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

#same as model4 but now we have random lake and random year effect
model4.1 <- gllvm(y = y, studyDesign = studyDesignData, row.eff = ~(1|lake) + (1|year), num.lv = 2, family = "tweedie", Power = NULL, 
                control.start = (n.init = 5), jitter.var = 0.1)
beep()
#check power
model4.1$Power
#summary and model performance plots
summary(model4.1)
gllvm::ordiplot(model4.1)
plot(model4.1)
#get residual correlations
Theta <- getResidualCor(model4.1)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")

#same as model4 but now we have NESTED random lake and random year effect - but I don't think we actually want these to be nested
model4.2 <- gllvm(y = y, studyDesign = studyDesignData, row.eff = ~(1|lake/year), num.lv = 2, family = "tweedie", Power = NULL, 
                  control.start = (n.init = 5), jitter.var = 0.1)
beep()
#check power
model4.2$Power
#summary and model performance plots
summary(model4.2)
gllvm::ordiplot(model4.2)
plot(model4.2)
#get residual correlations
Theta <- getResidualCor(model4.2)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#negligible difference from nesting



#Let's jump off the deep end and try to code the whole model, but without latent variables
#the random effects here are NOT species-specific
model5 <- gllvm(y = y, X = x_scale, studyDesign = studyDesignData, 
                formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM,
                row.eff = ~(1|lake) + (1|year), 
                num.lv = 0, family = "tweedie", Power = NULL, 
                #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                control.start = (n.init = 5), jitter.var = 0.1)
beep()
#save output so I don't always have to rerun it
#saveRDS(model5, file = "model5.rds")
#check power
model5$Power
#summary and model performance plots
summary(model5)
plot(model5)
#can't get residual correlations because no latent variables
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model5, "row.params.random"), Lake = names(coef(model5, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model5$params$sigma
#look at coefficient effects
coefplot(model5,
         #which.Xcoef = "CDOM",
         cex.ylab = 1,
         order = TRUE)



#make the random effects species-specific
#diag argument keeps the species-specific random effects from being correlated with each other (completeley independent) - also helps model converge
model6 <- gllvm(y = y, X = x_scale, studyDesign = studyDesignData, 
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
model7 <- gllvm(y = y, X = x_scale, studyDesign = studyDesignData, 
                formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM,
                row.eff = ~(1|lake) + (1|year), 
                num.lv = 1, family = "tweedie", Power = NULL, 
                #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                control.start = (n.init = 5), jitter.var = 0.1)
beep()
#save output so I don't always have to rerun it
#saveRDS(model7, file = "model7.rds")
#check power
model7$Power
#summary and model performance plots
summary(model7)
par(mfrow = c(1, 1))
plot(model7)
#can't get residual correlations because no latent variables
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model7, "row.params.random"), Lake = names(coef(model7, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model7$params$sigma
#get residual correlations
Theta <- getResidualCor(model7)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#look at coefficient effects
coefplot(model7,
         cex.ylab = 1,
         order = TRUE)
#ordination
gllvm::ordiplot(model7)
#THIS IS BAD, ONE LATENT VARIABLE DOES NOT WORK - caused overfitting and math collapsed




#random effects are NOT species-specific but now we have 2 latent variables
model8 <- gllvm(y = y, X = x_scale, studyDesign = studyDesignData, 
                formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM,
                row.eff = ~(1|lake) + (1|year), 
                num.lv = 2, family = "tweedie", Power = NULL, 
                #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                control.start = (n.init = 5), jitter.var = 0.1)
beep()
#save output so I don't always have to rerun it
#saveRDS(model8, file = "model8.rds")
#check power
model8$Power
#summary and model performance plots
summary(model8)
par(mfrow = c(1, 1))
plot(model8)
#can't get residual correlations because no latent variables
#ordination
gllvm::ordiplot(model8)
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model8, "row.params.random"), Lake = names(coef(model8, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model8$params$sigma
#get residual correlations
Theta <- getResidualCor(model8)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#look at coefficient effects
coefplot(model8,
         cex.ylab = 1,
         order = TRUE)



#random effects are NOT species-specific but now we have 3 latent variables
model9 <- gllvm(y = y, X = x_scale, studyDesign = studyDesignData, 
                formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM,
                row.eff = ~(1|lake) + (1|year), 
                num.lv = 3, family = "tweedie", Power = NULL, 
                #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                control.start = (n.init = 5), jitter.var = 0.1)
beep()
#save output so I don't always have to rerun it
#saveRDS(model9, file = "model9.rds")
#check power
model9$Power
#summary and model performance plots
summary(model9)
par(mfrow = c(1, 1))
plot(model9)
#ordination
gllvm::ordiplot(model9)
#can't get residual correlations because no latent variables
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model9, "row.params.random"), Lake = names(coef(model9, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model9$params$sigma
#get residual correlations
Theta <- getResidualCor(model9)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#look at coefficient effects
coefplot(model9,
         cex.ylab = 1,
         order = TRUE)



#Same as model 9, keeps random structure but no environmental predictors
model10 <- gllvm(y = y, studyDesign = studyDesignData,
                 row.eff = ~(1|lake) + (1|year), 
                 num.lv = 3, family = "tweedie", Power = NULL, 
                 #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                 control.start = (n.init = 5), jitter.var = 0.1)
beep()
#save output so I don't always have to rerun it
#saveRDS(model10, file = "model10.rds")
#check power
model10$Power
#summary and model performance plots
summary(model10)
par(mfrow = c(1, 1))
plot(model10)
gllvm::ordiplot(model10)
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model10, "row.params.random"), Lake = names(coef(model10, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model10$params$sigma
#get residual correlations
Theta <- getResidualCor(model10)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")





#let's try out the concurrent ordination with two latent variables, study design incorporated
model11 <- gllvm(y = y, X = x_scale, studyDesign = studyDesignData, 
                 lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM,
                 row.eff = ~(1|lake) + (1|year), 
                 num.lv.c = 3, family = "tweedie", Power = NULL, 
                 #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                 control.start = (n.init = 5), jitter.var = 0.1,
                 control = list(reltol.c = 1e-8) #this changes the optimization criteria to be stricter for convergence (added in reponse to a warning message I got)
                 )
beep()
#save output so I don't always have to rerun it
#saveRDS(model11, file = "model11.rds")
model11 <- readRDS("Models/model11.rds")
#check power
model11$Power
#summary and model performance plots
summary(model11)
par(mfrow = c(1, 1))
plot(model11)
gllvm::ordiplot(model11)
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model11, "row.params.random"), Lake = names(coef(model11, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model11$params$sigma
#get residual correlations
Theta <- getResidualCor(model11)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#get correlations due to environment/covariates
# Env <- getEnvironCor(model11)
# corrplot(Theta[order.single(Env), order.single(Env)],
#          diag = FALSE,
#          type = "lower",
#          method = "square",
#          tl.cex = 0.5,
#          t.srt = 45,
#          tl.col = "red")

#trying to get the environment correlations requires a random effect somewhere within the ordination, so I can't do this here
#run ?getEnvironCor for details!

#look at coefficient effects
coefplot(model11,
         cex.ylab = 1,
         order = FALSE)


#MODELS AFTER COMMITTEE MEETING----------------------------------------------------------------------
#overall changes:
  #no more random year effect
  #be more restrictive on species included in the model
      #lumping small rare daphnia

#super basic with the new response matrices
#model with no covariates on relative abundance data
model.nocov.rel <- gllvm(y = y_rel_abun, num.lv = 3, family = "tweedie", Power = NULL,
                control.start = (n.init = 3))
#power argument tells R to find the tweedie coefficient that best fits my data
beep()
#summary and model performance plots
summary(model.nocov.rel)
gllvm::ordiplot(model.nocov.rel)
plot(model.nocov.rel)
#check power
model.nocov.rel$Power
#get residual correlations
Theta <- getResidualCor(model.nocov.rel)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")



#model with no covariates on raw data
model.nocov.raw <- gllvm(y = y_raw, num.lv = 3, family = "tweedie", Power = NULL,
                         control.start = (n.init = 3))
#power argument tells R to find the tweedie coefficient that best fits my data
beep()
#summary and model performance plots
summary(model.nocov.raw)
gllvm::ordiplot(model.nocov.raw)
plot(model.nocov.raw)
#check power
model.nocov.raw$Power
#get residual correlations
Theta <- getResidualCor(model.nocov.raw)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")


#random effects of lake but still no covariates
#model with no covariates on relative abundance data
model.lake.rel <- gllvm(y = y_rel_abun, studyDesign = studyDesignData, num.lv = 3, family = "tweedie", Power = NULL,
                        row.eff = ~(1|lake), control.start = (n.init = 3))
#power argument tells R to find the tweedie coefficient that best fits my data
beep()
#summary and model performance plots
summary(model.lake.rel)
gllvm::ordiplot(model.lake.rel)
plot(model.lake.rel)
#check power
model.lake.rel$Power
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model.lake.rel, "row.params.random"), Lake = names(coef(model.lake.rel, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model.lake.rel$params$sigma
#get residual correlations
Theta <- getResidualCor(model.lake.rel)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")



#model with no covariates on raw data
model.lake.raw <- gllvm(y = y_raw, studyDesign = studyDesignData, num.lv = 3, family = "tweedie", Power = NULL,
                        row.eff = ~(1|lake), control.start = (n.init = 3))
#power argument tells R to find the tweedie coefficient that best fits my data
beep()
#summary and model performance plots
summary(model.lake.raw)
gllvm::ordiplot(model.lake.raw)
plot(model.lake.raw)
#check power
model.lake.raw$Power
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model.lake.raw, "row.params.random"), Lake = names(coef(model.lake.raw, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model.lake.raw$params$sigma
#get residual correlations
Theta <- getResidualCor(model.lake.raw)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")


#try species-specific random effects on raw data (not relative abundance)
model.lake.raw.spp <- gllvm(y = y_raw, studyDesign = studyDesignData, num.lv = 3, family = "tweedie", Power = NULL,
                        formula = ~diag((1|lake)), 
                        sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                        control.start = (n.init = 10), jitter.var = 0.1)
#power argument tells R to find the tweedie coefficient that best fits my data
beep()
#save output so I don't always have to rerun it
saveRDS(model.lake.raw.spp, file = "model_lake_raw_spp.rds")
#summary and model performance plots
summary(model.lake.raw.spp)
gllvm::ordiplot(model.lake.raw.spp)
plot(model.lake.raw.spp)
#check power
model.lake.raw.spp$Power
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model.lake.raw.spp, "row.params.random"), Lake = names(coef(model.lake.raw.spp, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model.lake.raw.spp$params$sigma
#get residual correlations
Theta <- getResidualCor(model.lake.raw.spp)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")





#env outside ordination with 3 lvs, raw response data
#random effects are NOT species-specific
model12_raw <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData, 
                formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM + RBS + CAP,
                row.eff = ~(1|lake), 
                num.lv = 3, family = "tweedie", Power = NULL, 
                #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                control.start = (n.init = 5), jitter.var = 0.1)
beep()
#save output so I don't always have to rerun it
#saveRDS(model12_raw, file = "model12_raw.rds")
#check power
model12_raw$Power
#summary and model performance plots
summary(model12_raw)
par(mfrow = c(1, 1))
plot(model12_raw)
#ordination
gllvm::ordiplot(model12_raw)
#can't get residual correlations because no latent variables
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model12_raw, "row.params.random"), Lake = names(coef(model12_raw, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model12_raw$params$sigma
#get residual correlations
Theta <- getResidualCor(model12_raw)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#look at coefficient effects
coefplot(model12_raw,
         cex.ylab = 1,
         order = TRUE)

#env outside ordination with 3 lvs, relative abundance response data
#random effects are NOT species-specific
#had a hard time getting this one to converge
model12_rel <- gllvm(y = y_rel_abun, X = x_scale, studyDesign = studyDesignData, 
                     formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM + RBS + CAP,
                     row.eff = ~(1|lake), 
                     num.lv = 3, family = "tweedie", Power = NULL, 
                     #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                     control.start = (n.init = 10), jitter.var = 0.1)
beep()
#save output so I don't always have to rerun it
#saveRDS(model12_rel, file = "model12_rel.rds")
#check power
model12_rel$Power
#summary and model performance plots
summary(model12_rel)
par(mfrow = c(1, 1))
plot(model12_rel)
#ordination
gllvm::ordiplot(model12_rel)
#can't get residual correlations because no latent variables
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model12_rel, "row.params.random"), Lake = names(coef(model12_rel, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model12_rel$params$sigma
#get residual correlations
Theta <- getResidualCor(model12_rel)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#look at coefficient effects
coefplot(model12_rel,
         cex.ylab = 1,
         order = TRUE)


#concurrent ordination with 3 concurrent lvs, random lake effect, no random cv effect
model13_raw <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData, 
                 lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM + RBS + CAP,
                 row.eff = ~(1|lake), 
                 num.lv.c = 3, family = "tweedie", Power = NULL, 
                 #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                 control.start = (n.init = 5), jitter.var = 0.1,
                 control = list(reltol.c = 1e-8)) #this changes the optimization criteria to be stricter for convergence (added in response to a warning message I got)
beep()
#save output so I don't always have to rerun it
#saveRDS(model13_raw, file = "model13_raw.rds")
#check power
model13_raw$Power
#summary and model performance plots
summary(model13_raw)
par(mfrow = c(1, 1))
plot(model13_raw)
gllvm::ordiplot(model13_raw)
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model13_raw, "row.params.random"), Lake = names(coef(model13_raw, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model13_raw$params$sigma
#get residual correlations
Theta <- getResidualCor(model13_raw)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#get correlations due to environment/covariates - CAN'T DO THIS WITHOUT A RANDOM COVARIATE STRUCTURE
# Env <- getEnvironCor(model13_raw)
# corrplot(Theta[order.single(Env), order.single(Env)],
#          diag = FALSE,
#          type = "lower",
#          method = "square",
#          tl.cex = 0.5,
#          t.srt = 45,
#          tl.col = "red")

#trying to get the environment correlations requires a random effect somewhere within the ordination, so I can't do this here
#run ?getEnvironCor for details!

#look at coefficient effects
coefplot(model13_raw,
         cex.ylab = 1,
         order = FALSE)



#concurrent ordination with 3 concurrent lvs, random lake effect, no random cv effect
model13_rel <- gllvm(y = y_rel_abun, X = x_scale, studyDesign = studyDesignData, 
                     lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM + RBS + CAP,
                     row.eff = ~(1|lake), 
                     num.lv.c = 3, family = "tweedie", Power = NULL, 
                     #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                     control.start = (n.init = 5), jitter.var = 0.1,
                     control = list(reltol.c = 1e-5)) #this changes the optimization criteria for convergence (In this model I changed it from 1e-8 to 1e-5 due to a warning message)
beep()
#save output so I don't always have to rerun it
#saveRDS(model13_rel, file = "model13_rel.rds")
#check power
model13_rel$Power
#summary and model performance plots
summary(model13_rel)
par(mfrow = c(1, 1))
plot(model13_rel)
gllvm::ordiplot(model13_rel)
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model13_rel, "row.params.random"), Lake = names(coef(model13_rel, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model13_rel$params$sigma
#get residual correlations
Theta <- getResidualCor(model13_rel)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#get correlations due to environment/covariates - CAN'T DO THIS WITHOUT A RANDOM COVARIATE STRUCTURE
# Env <- getEnvironCor(model13_rel)
# corrplot(Theta[order.single(Env), order.single(Env)],
#          diag = FALSE,
#          type = "lower",
#          method = "square",
#          tl.cex = 0.5,
#          t.srt = 45,
#          tl.col = "red")

#trying to get the environment correlations requires a random effect somewhere within the ordination, so I can't do this here
#run ?getEnvironCor for details!

#look at coefficient effects
coefplot(model13_rel,
         cex.ylab = 1,
         order = FALSE)



#env outside ordination with 3 lvs, raw response data, UNIMODAL RESPONSE WITH COMMON TOLERANCES AMONG ALL SPECIES
#random effects are NOT species-specific
model14 <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData, 
                     formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM + RBS + CAP,
                     row.eff = ~(1|lake), 
                     quadratic = "LV",
                     num.lv = 3, family = "tweedie", Power = NULL, 
                     #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                     control.start = (n.init = 10), jitter.var = 0.1)
beep()
#save output so I don't always have to rerun it
#saveRDS(model14, file = "model14.rds")
model14 <- readRDS("Models/model14.rds")
#check power
model14$Power
#summary and model performance plots
summary(model14)
par(mfrow = c(1, 1))
plot(model14)
#ordination
gllvm::ordiplot(model14, symbols = T, biplot = T, spp.arrows=FALSE)
#inspect the optima
optima(model14, sd.errors = FALSE)
#inspect the tolerances
tolerances(model14, sd.errors = FALSE)
#visualize species tolerances
LVs = getLV(model14)
newLV = cbind(LV1 = seq(min(LVs[,1]), max(LVs[,1]), length.out=137), LV2 = 0, LV3 = 0)
preds <- predict(model14, type = "response", newLV = newLV)
plot(NA, ylim = range(preds), xlim = c(range(getLV(model14))), ylab  = "Predicted response", xlab = "LV1")
segments(x0=optima(model14, sd.errors = FALSE)[,1],x1 = optima(model14, sd.errors = FALSE)[,1], y0 = rep(0, ncol(model14$y)), y1 = apply(preds,2,max), col = "red", lty = "dashed", lwd = 2)
rug(getLV(model14)[,1])
sapply(1:ncol(model14$y), function(j)lines(sort(newLV[,1]), preds[order(newLV[,1]),j], lwd = 2))

newLV = cbind(LV1 = 0, LV3 = 0, LV2 =  seq(min(LVs[,2]), max(LVs[,2]), length.out=137))
preds <- predict(model14, type = "response", newLV = newLV)
plot(NA, ylim = range(preds), xlim = c(range(getLV(model14))), ylab  = "Predicted response", xlab = "LV2")
segments(x0=optima(model14, sd.errors = FALSE)[,2],x1 = optima(model14, sd.errors = FALSE)[,2], y0 = rep(0, ncol(model14$y)), y1 = apply(preds,2,max), col = "red", lty = "dashed", lwd = 2)
rug(getLV(model14)[,2])
sapply(1:ncol(model14$y), function(j)lines(sort(newLV[,2]), preds[order(newLV[,2]),j], lwd = 2))

newLV = cbind(LV1 = 0, LV2 = 0, LV3 =  seq(min(LVs[,3]), max(LVs[,3]), length.out=137))
preds <- predict(model14, type = "response", newLV = newLV)
plot(NA, ylim = range(preds), xlim = c(range(getLV(model14))), ylab  = "Predicted response", xlab = "LV3")
segments(x0=optima(model14, sd.errors = FALSE)[,3],x1 = optima(model14, sd.errors = FALSE)[,3], y0 = rep(0, ncol(model14$y)), y1 = apply(preds,2,max), col = "red", lty = "dashed", lwd = 2)
rug(getLV(model14)[,3])
sapply(1:ncol(model14$y), function(j)lines(sort(newLV[,3]), preds[order(newLV[,3]),j], lwd = 2))

#can't get residual correlations because no latent variables
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model14, "row.params.random"), Lake = names(coef(model14, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model14$params$sigma
#get residual correlations
Theta <- getResidualCor(model14)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#look at coefficient effects
coefplot(model14,
         cex.ylab = 1,
         order = TRUE)


#env outside ordination with 3 lvs, raw response data, UNIMODAL RESPONSE WITH UNIQUE TOLERANCE FOR EACH SPECIES - see if it can fit
#random effects are NOT species-specific
# model15 <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData, 
#                      formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM + RBS + CAP,
#                      row.eff = ~(1|lake), 
#                      quadratic = TRUE,
#                      num.lv = 3, family = "tweedie", Power = NULL, 
#                      #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
#                      control.start = (n.init = 10), jitter.var = 0.1)
# beep()
model15 <- readRDS("model15.rds")
#save output so I don't always have to rerun it
#saveRDS(model15, file = "model15.rds")
#check power
model15$Power
#summary and model performance plots
summary(model15)
par(mfrow = c(1, 1))
#save residuals - this had to run overnight so saving as a csv
# res_model15 <- residuals(model15)
# res_model15_res <- as.data.frame(res_model15$residuals)
# res_model15_lin <- as.data.frame(res_model15$linpred) #these are the linear predictors
# write.csv(res_model15_res, file = "Models/model15_residuals.csv", row.names = TRUE)
# write.csv(res_model15_lin, file = "Models/model15_linpred.csv", row.names = TRUE)
res_model15_res <- read.csv("Models/model15_residuals.csv") %>% 
  select(-X)
res_model15_lin <- read.csv("Models/model15_linpred.csv") %>% 
  select(-X)
#then plot them manually - regular plot doesn't work because it takes so long to get residuals
res_long <- res_model15_res %>%
  pivot_longer(cols = everything(), 
               names_to = "Species", 
               values_to = "ResidualValue")
ggplot(res_long, aes(sample = ResidualValue)) +
  stat_qq(alpha = 0.3) + # alpha helps see density if you have many points
  stat_qq_line(color = "blue") +
  theme_minimal() +
  labs(title = "Combined Q-Q Plot for All Species",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")
#color by species to see if a certain speciesis the problem
ggplot(res_long, aes(sample = ResidualValue, color = Species)) +
  stat_qq(alpha = 0.3) + # alpha helps see density if you have many points
  stat_qq_line(color = "blue") +
  theme_minimal() +
  labs(title = "Combined Q-Q Plot for All Species",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")
#copepodites are a problem, try making a qqplot without them
res_long_nocop <- res_long %>% 
  filter(Species != "copepodites")
ggplot(res_long_nocop, aes(sample = ResidualValue)) +
  stat_qq(alpha = 0.3) + # alpha helps see density if you have many points
  stat_qq_line(color = "blue") +
  theme_minimal() +
  labs(title = "Combined Q-Q Plot for All Species",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")
#residuals vs. fitted values
linpred_long <- res_model15_lin %>%
  pivot_longer(cols = everything(), 
               names_to = "Species", 
               values_to = "LinearPredictor")
res.fv <- cbind(res_long, linpred_long) %>% 
  select(-3)
ggplot(res.fv, aes(x = LinearPredictor, y = ResidualValue)) +
  geom_point(alpha = 0.4, size = 0.8) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(method = "loess", color = "blue", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Residuals vs. Linear Predictor",
    x = "Linear Predictor (from residuals list)",
    y = "Randomized Quantile Residuals"
  )
#color by species
ggplot(res.fv, aes(x = LinearPredictor, y = ResidualValue, color = Species)) +
  geom_point(alpha = 0.4, size = 0.8) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(method = "loess", color = "blue", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Residuals vs. Linear Predictor",
    x = "Linear Predictor (from residuals list)",
    y = "Randomized Quantile Residuals"
  )
#without copepodites
res.fv_nocop <- res.fv %>% 
  filter(Species != "copepodites")
ggplot(res.fv_nocop, aes(x = LinearPredictor, y = ResidualValue)) +
  geom_point(alpha = 0.4, size = 0.8) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(method = "loess", color = "blue", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Residuals vs. Linear Predictor",
    x = "Linear Predictor (from residuals list)",
    y = "Randomized Quantile Residuals"
  )


#ordination
gllvm::ordiplot(model15, symbols = T, biplot = T)
#inspect the optima
optima(model15, sd.errors = FALSE)
plot(optima(model15, sd.errors = FALSE))
#inspect the tolerances
tolerances(model15, sd.errors = FALSE)
plot(tolerances(model15, sd.errors = FALSE))
#visualize species tolerances
LVs = getLV(model15)
newLV = cbind(LV1 = seq(min(LVs[,1]), max(LVs[,1]), length.out=137), LV2 = 0, LV3 = 0)
preds <- predict(model15, type = "response", newLV = newLV)
plot(NA, ylim = range(preds), xlim = c(range(getLV(model15))), ylab  = "Predicted response", xlab = "LV1")
segments(x0=optima(model15, sd.errors = FALSE)[,1],x1 = optima(model15, sd.errors = FALSE)[,1], y0 = rep(0, ncol(model15$y)), y1 = apply(preds,2,max), col = "red", lty = "dashed", lwd = 2)
rug(getLV(model15)[,1])
sapply(1:ncol(model15$y), function(j)lines(sort(newLV[,1]), preds[order(newLV[,1]),j], lwd = 2))

newLV = cbind(LV1 = 0, LV3 = 0, LV2 =  seq(min(LVs[,2]), max(LVs[,2]), length.out=137))
preds <- predict(model15, type = "response", newLV = newLV)
plot(NA, ylim = range(preds), xlim = c(range(getLV(model15))), ylab  = "Predicted response", xlab = "LV2")
segments(x0=optima(model15, sd.errors = FALSE)[,2],x1 = optima(model15, sd.errors = FALSE)[,2], y0 = rep(0, ncol(model15$y)), y1 = apply(preds,2,max), col = "red", lty = "dashed", lwd = 2)
rug(getLV(model15)[,2])
sapply(1:ncol(model15$y), function(j)lines(sort(newLV[,2]), preds[order(newLV[,2]),j], lwd = 2))

newLV = cbind(LV1 = 0, LV2 = 0, LV3 =  seq(min(LVs[,3]), max(LVs[,3]), length.out=137))
preds <- predict(model15, type = "response", newLV = newLV)
plot(NA, ylim = range(preds), xlim = c(range(getLV(model15))), ylab  = "Predicted response", xlab = "LV3")
segments(x0=optima(model15, sd.errors = FALSE)[,3],x1 = optima(model15, sd.errors = FALSE)[,3], y0 = rep(0, ncol(model15$y)), y1 = apply(preds,2,max), col = "red", lty = "dashed", lwd = 2)
rug(getLV(model15)[,3])
sapply(1:ncol(model15$y), function(j)lines(sort(newLV[,3]), preds[order(newLV[,3]),j], lwd = 2))

#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model15, "row.params.random"), Lake = names(coef(model15, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model15$params$sigma
#get residual correlations
Theta <- getResidualCor(model15)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#look at coefficient effects
coefplot(model15,
         cex.ylab = 1,
         order = TRUE)


#concurrent ordination with 3 concurrent lvs, random lake effect, random cv effect = LV - same variance within a latent variable
model16 <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData, 
                     lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM + RBS + CAP,
                     row.eff = ~(1|lake), 
                     num.lv.c = 3, family = "tweedie", Power = NULL, 
                     randomB = "LV",
                     #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                     control.start = (n.init = 10), jitter.var = 0.1,
                     control = list(reltol.c = 1e-8)) #this changes the optimization criteria to be stricter for convergence (added in response to a warning message I got)
beep()
#save output so I don't always have to rerun it
#saveRDS(model16, file = "model16.rds")
#check power
model16$Power
#summary and model performance plots
summary(model16)
par(mfrow = c(1, 1))
plot(model16)
gllvm::ordiplot(model16)
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model16, "row.params.random"), Lake = names(coef(model16, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model16$params$sigma
#get residual correlations
Theta <- getResidualCor(model16)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#get correlations due to environment/covariates - CAN'T DO THIS WITHOUT A RANDOM COVARIATE STRUCTURE
Env <- getEnvironCor(model16)
corrplot(Theta[order.single(Env), order.single(Env)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#trying to get the environment correlations requires a random effect somewhere within the ordination, so I can't do this here
#run ?getEnvironCor for details!
#look at coefficient effects
gllvm::randomCoefplot(model16)




#concurrent ordination with 3 concurrent lvs, random lake effect, random cv effect = LV - same variance within a latent variable
model16_noprecip <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData, 
                 lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM + RBS + CAP,
                 row.eff = ~(1|lake), 
                 num.lv.c = 3, family = "tweedie", Power = NULL, 
                 randomB = "LV",
                 #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                 control.start = (n.init = 10), jitter.var = 0.1,
                 control = list(reltol.c = 1e-8)) #this changes the optimization criteria to be stricter for convergence (added in response to a warning message I got)
beep()
#save output so I don't always have to rerun it
#saveRDS(model16_noprecip, file = "model16_noprecip.rds")
#check power
model16_noprecip$Power
#summary and model performance plots
summary(model16_noprecip)
par(mfrow = c(1, 1))
plot(model16_noprecip)
gllvm::ordiplot(model16_noprecip)
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model16_noprecip, "row.params.random"), Lake = names(coef(model16_noprecip, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model16_noprecip$params$sigma
#get residual correlations
Theta <- getResidualCor(model16_noprecip)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#get correlations due to environment/covariates - CAN'T DO THIS WITHOUT A RANDOM COVARIATE STRUCTURE
Env <- getEnvironCor(model16_noprecip)
corrplot(Theta[order.single(Env), order.single(Env)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#trying to get the environment correlations requires a random effect somewhere within the ordination, so I can't do this here
#run ?getEnvironCor for details!
#look at coefficient effects
gllvm::randomCoefplot(model16_noprecip)


#Do I need to specify each predictor as random? - seeing if I get different result from model 16
model16_test <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData, 
                 lv.formula = ~ (0+CDOM|1) + (0+Area|1) + (0+Max_Depth|1) + (0+Secchi|1) + (0+GDD|1) + (0+Precip|1) + (0+Photic|1) + (0+SWF|1) + (0+ZM|1) + (0+RBS|1) + (0+CAP|1),
                 row.eff = ~(1|lake), 
                 num.lv.c = 3, family = "tweedie", Power = NULL, 
                 randomB = "LV",
                 #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                 control.start = (n.init = 10), jitter.var = 0.1,
                 control = list(reltol.c = 1e-8)) #this changes the optimization criteria to be stricter for convergence (added in response to a warning message I got)
beep()
#save output so I don't always have to rerun it
#saveRDS(model16_test, file = "model16_test.rds")
#check power
model16_test$Power
#summary and model performance plots
summary(model16_test)
par(mfrow = c(1, 1))
plot(model16_test)
gllvm::ordiplot(model16_test)
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model16_test, "row.params.random"), Lake = names(coef(model16_test, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model16_test$params$sigma
#get residual correlations
Theta <- getResidualCor(model16_test)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#get correlations due to environment/covariates - CAN'T DO THIS WITHOUT A RANDOM COVARIATE STRUCTURE
Env <- getEnvironCor(model16_test)
corrplot(Theta[order.single(Env), order.single(Env)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#trying to get the environment correlations requires a random effect somewhere within the ordination, so I can't do this here
#run ?getEnvironCor for details!
#look at coefficient effects
gllvm::randomCoefplot(model16_test)


#concurrent ordination with 3 concurrent lvs, random lake effect, random cv effect = P - same variance within each predictor
# model17 <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData, 
#                  lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM + RBS + CAP,
#                  row.eff = ~(1|lake), 
#                  num.lv.c = 3, family = "tweedie", Power = NULL, 
#                  randomB = "P",
#                  #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
#                  control.start = (n.init = 10), jitter.var = 0.1,
#                  control = list(reltol.c = 1e-8)) #this changes the optimization criteria to be stricter for convergence (added in response to a warning message I got)
# beep()
model17 <- readRDS("model17.rds")
#save output so I don't always have to rerun it
#saveRDS(model17, file = "model17.rds")
#check power
model17$Power
#summary and model performance plots
summary(model17)
par(mfrow = c(1, 1))
plot(model17)
gllvm::ordiplot(model17)
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model17, "row.params.random"), Lake = names(coef(model17, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model17$params$sigma
#get residual correlations
Theta <- getResidualCor(model17)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#get correlations due to environment/covariates - CAN'T DO THIS WITHOUT A RANDOM COVARIATE STRUCTURE
Env <- getEnvironCor(model17)
corrplot(Theta[order.single(Env), order.single(Env)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#trying to get the environment correlations requires a random effect somewhere within the ordination, so I can't do this here
#run ?getEnvironCor for details!
#look at coefficient effects
gllvm::randomCoefplot(model17)



#concurrent ordination with 3 concurrent lvs, random lake effect, random cv effect = LV - same variance within a latent variable
model18 <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData, 
                 lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM + RBS + CAP,
                 row.eff = ~(1|lake), 
                 num.lv.c = 3, family = "tweedie", Power = NULL, 
                 randomB = "single",
                 #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                 control.start = (n.init = 10), jitter.var = 0.1,
                 control = list(reltol.c = 1e-8)) #this changes the optimization criteria to be stricter for convergence (added in response to a warning message I got)
beep()
#save output so I don't always have to rerun it
#saveRDS(model18, file = "model18.rds")
#check power
model18$Power
#summary and model performance plots
summary(model18)
par(mfrow = c(1, 1))
plot(model18)
gllvm::ordiplot(model18)
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model18, "row.params.random"), Lake = names(coef(model18, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model18$params$sigma
#get residual correlations
Theta <- getResidualCor(model18)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#get correlations due to environment/covariates - CAN'T DO THIS WITHOUT A RANDOM COVARIATE STRUCTURE
Env <- getEnvironCor(model18)
corrplot(Theta[order.single(Env), order.single(Env)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#trying to get the environment correlations requires a random effect somewhere within the ordination, so I can't do this here
#run ?getEnvironCor for details!
#look at coefficient effects
coefplot(model18,
         cex.ylab = 1,
         order = FALSE)



#CAN I GET QUADRATIC RESPONSES IN THE CONCURRENT ORDINATION??? - start simple... will it converge?
# model19 <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData, 
#                           lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM + RBS + CAP,
#                           row.eff = ~(1|lake), 
#                           num.lv.c = 3, family = "tweedie", Power = NULL, 
#                           randomB = "LV", quadratic = "LV",
#                           #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
#                           control.start = (n.init = 10), jitter.var = 0.1,
#                           control = list(reltol.c = 1e-8)) #this changes the optimization criteria to be stricter for convergence (added in response to a warning message I got)
# beep()
model19 <- readRDS("model19.rds")
#save output so I don't always have to rerun it
#saveRDS(model19, file = "model19.rds")
#check power
model19$Power
#summary and model performance plots
summary(model19)
par(mfrow = c(1, 1))
plot(model19)
gllvm::ordiplot(model19, symbol = T)
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model19, "row.params.random"), Lake = names(coef(model19, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model19$params$sigma
#get residual correlations
Theta <- getResidualCor(model19)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#get correlations due to environment/covariates - CAN'T DO THIS WITHOUT A RANDOM COVARIATE STRUCTURE
Env <- getEnvironCor(model19)
corrplot(Theta[order.single(Env), order.single(Env)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#trying to get the environment correlations requires a random effect somewhere within the ordination, so I can't do this here
#run ?getEnvironCor for details!
#inspect the optima
optima(model19, sd.errors = FALSE)
plot(optima(model19, sd.errors = FALSE))
#inspect the tolerances
tolerances(model19, sd.errors = FALSE)
plot(tolerances(model19, sd.errors = FALSE)) #right, I specified this to be the same for all species
#visualize species tolerances
LVs = getLV(model19)
newLV = cbind(LV1 = seq(min(LVs[,1]), max(LVs[,1]), length.out=137), LV2 = 0, LV3 = 0)
preds <- predict(model19, type = "response", newLV = newLV)
plot(NA, ylim = range(preds), xlim = c(range(getLV(model19))), ylab  = "Predicted response", xlab = "LV1")
segments(x0=optima(model19, sd.errors = FALSE)[,1],x1 = optima(model19, sd.errors = FALSE)[,1], y0 = rep(0, ncol(model19$y)), y1 = apply(preds,2,max), col = "red", lty = "dashed", lwd = 2)
rug(getLV(model19)[,1])
sapply(1:ncol(model19$y), function(j)lines(sort(newLV[,1]), preds[order(newLV[,1]),j], lwd = 2))

newLV = cbind(LV1 = 0, LV3 = 0, LV2 =  seq(min(LVs[,2]), max(LVs[,2]), length.out=137))
preds <- predict(model19, type = "response", newLV = newLV)
plot(NA, ylim = range(preds), xlim = c(range(getLV(model19))), ylab  = "Predicted response", xlab = "LV2")
segments(x0=optima(model19, sd.errors = FALSE)[,2],x1 = optima(model19, sd.errors = FALSE)[,2], y0 = rep(0, ncol(model19$y)), y1 = apply(preds,2,max), col = "red", lty = "dashed", lwd = 2)
rug(getLV(model19)[,2])
sapply(1:ncol(model19$y), function(j)lines(sort(newLV[,2]), preds[order(newLV[,2]),j], lwd = 2))

newLV = cbind(LV1 = 0, LV2 = 0, LV3 =  seq(min(LVs[,3]), max(LVs[,3]), length.out=137))
preds <- predict(model19, type = "response", newLV = newLV)
plot(NA, ylim = range(preds), xlim = c(range(getLV(model19))), ylab  = "Predicted response", xlab = "LV3")
segments(x0=optima(model19, sd.errors = FALSE)[,3],x1 = optima(model19, sd.errors = FALSE)[,3], y0 = rep(0, ncol(model19$y)), y1 = apply(preds,2,max), col = "red", lty = "dashed", lwd = 2)
rug(getLV(model19)[,3])
sapply(1:ncol(model19$y), function(j)lines(sort(newLV[,3]), preds[order(newLV[,3]),j], lwd = 2))




#TRY CHANGING THE RANDOM CANONICAL COEFFICIENT STRUCTURE AND LETTING SPECIES TOLERANCES VARY 
model21 <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData, 
                 lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM + RBS + CAP,
                 row.eff = ~(1|lake), 
                 num.lv.c = 3, family = "tweedie", Power = NULL, 
                 randomB = "P", quadratic = TRUE,
                 #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                 control.start = (n.init = 10), jitter.var = 0.1,
                 control = list(reltol.c = 1e-8)) #this changes the optimization criteria to be stricter for convergence (added in response to a warning message I got)
beep()
#save output so I don't always have to rerun it
saveRDS(model21, file = "model21.rds")
#check power
model21$Power
#summary and model performance plots
summary(model21)
par(mfrow = c(1, 1))
plot(model21)
gllvm::ordiplot(model21, symbol = T)
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model21, "row.params.random"), Lake = names(coef(model21, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model21$params$sigma
#get residual correlations
Theta <- getResidualCor(model20)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#get correlations due to environment/covariates - CAN'T DO THIS WITHOUT A RANDOM COVARIATE STRUCTURE
Env <- getEnvironCor(model20)
corrplot(Theta[order.single(Env), order.single(Env)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#trying to get the environment correlations requires a random effect somewhere within the ordination, so I can't do this here
#run ?getEnvironCor for details!
#inspect the optima
optima(model20, sd.errors = FALSE)
plot(optima(model20, sd.errors = FALSE))
#inspect the tolerances
tolerances(model20, sd.errors = FALSE)
plot(tolerances(model20, sd.errors = FALSE)) 

#TRY LETTING SPECIES TOLERANCES VARY NOW
model20 <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData, 
                 lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM + RBS + CAP,
                 row.eff = ~(1|lake), 
                 num.lv.c = 3, family = "tweedie", Power = NULL, 
                 randomB = "LV", quadratic = TRUE,
                 #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                 control.start = (n.init = 10), jitter.var = 0.1,
                 control = list(reltol.c = 1e-8)) #this changes the optimization criteria to be stricter for convergence (added in response to a warning message I got)
beep()
#save output so I don't always have to rerun it
#saveRDS(model20, file = "model20.rds")
#check power
model20$Power
#summary and model performance plots
summary(model20)
par(mfrow = c(1, 1))
plot(model20)
gllvm::ordiplot(model20, symbol = T)
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model20, "row.params.random"), Lake = names(coef(model20, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model20$params$sigma
#get residual correlations
Theta <- getResidualCor(model20)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#get correlations due to environment/covariates - CAN'T DO THIS WITHOUT A RANDOM COVARIATE STRUCTURE
Env <- getEnvironCor(model20)
corrplot(Theta[order.single(Env), order.single(Env)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#trying to get the environment correlations requires a random effect somewhere within the ordination, so I can't do this here
#run ?getEnvironCor for details!
#inspect the optima
optima(model20, sd.errors = FALSE)
plot(optima(model20, sd.errors = FALSE))
#inspect the tolerances
tolerances(model20, sd.errors = FALSE)
plot(tolerances(model20, sd.errors = FALSE))





#TRY CHANGING THE CANONICAL COEFFICIENT RANDOM STRUCTURE TO P AND LETTING TOLERANCE VARY BY SPECIES
model21 <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData, 
                 lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM + RBS + CAP,
                 row.eff = ~(1|lake), 
                 num.lv.c = 3, family = "tweedie", Power = NULL, 
                 randomB = "P", quadratic = TRUE,
                 #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                 control.start = (n.init = 10), jitter.var = 0.1,
                 control = list(reltol.c = 1e-8)) #this changes the optimization criteria to be stricter for convergence (added in response to a warning message I got)
beep()
#save output so I don't always have to rerun it
#saveRDS(model21, file = "model21.rds")
#check power
model21$Power
#summary and model performance plots
summary(model21)
par(mfrow = c(1, 1))
plot(model21)
gllvm::ordiplot(model21, symbol = T)
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model21, "row.params.random"), Lake = names(coef(model21, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model21$params$sigma
#get residual correlations
Theta <- getResidualCor(model21)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#get correlations due to environment/covariates - CAN'T DO THIS WITHOUT A RANDOM COVARIATE STRUCTURE
Env <- getEnvironCor(model21)
corrplot(Theta[order.single(Env), order.single(Env)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#trying to get the environment correlations requires a random effect somewhere within the ordination, so I can't do this here
#run ?getEnvironCor for details!
#inspect the optima
optima(model21, sd.errors = FALSE)
plot(optima(model21, sd.errors = FALSE))
#inspect the tolerances
tolerances(model21, sd.errors = FALSE)
plot(tolerances(model21, sd.errors = FALSE))




#KEEP THE CANONICAL COEFFICIENT RANDOM STRUCTURE AS P BUT NO LONGER LET TOLERANCE VARY BY SPECIES
model22 <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData, 
                 lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM + RBS + CAP,
                 row.eff = ~(1|lake), 
                 num.lv.c = 3, family = "tweedie", Power = NULL, 
                 randomB = "P", quadratic = "LV",
                 #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                 control.start = (n.init = 10), jitter.var = 0.1,
                 control = list(reltol.c = 1e-8)) #this changes the optimization criteria to be stricter for convergence (added in response to a warning message I got)
beep()
#save output so I don't always have to rerun it
#saveRDS(model22, file = "model22.rds")
#check power
model22$Power
#summary and model performance plots
summary(model22)
par(mfrow = c(1, 1))
plot(model22)
gllvm::ordiplot(model22, symbol = T)
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model22, "row.params.random"), Lake = names(coef(model22, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model22$params$sigma
#get residual correlations
Theta <- getResidualCor(model22)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#get correlations due to environment/covariates - CAN'T DO THIS WITHOUT A RANDOM COVARIATE STRUCTURE
Env <- getEnvironCor(model22)
corrplot(Theta[order.single(Env), order.single(Env)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#trying to get the environment correlations requires a random effect somewhere within the ordination, so I can't do this here
#run ?getEnvironCor for details!
#inspect the optima
optima(model22, sd.errors = FALSE)
plot(optima(model22, sd.errors = FALSE))
#inspect the tolerances
tolerances(model22, sd.errors = FALSE)
plot(tolerances(model22, sd.errors = FALSE))



#CODE EXAMPLE FROM VIGNETTE TO TEST NUMBER OF LATENT VARIABLES NEEDED:
# criteria <- NULL
# for(i in 1:5){
#   fiti <- gllvm(y, X, family = "negative.binomial", num.lv = i, sd.errors = FALSE,
#                 formula = ~ Bare.ground + Canopy.cover + Volume.lying.CWD, seed = 1234)
#   criteria[i] <- summary(fiti)$AICc
#   names(criteria)[i] = i
# }
# # Compare AICc values
# criteria
# #>        1        2        3        4        5 
# #> 4163.565 4215.446 4311.091 4462.069 4612.805



#TESTING MY LATENT VARIABLES ON MODEL 19
# criteria <- NULL
# for(i in 1:5){
#   fiti <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData,
#                 lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM + RBS + CAP,
#                 row.eff = ~(1|lake),
#                 num.lv.c = i, family = "tweedie", Power = NULL,
#                 randomB = "LV", quadratic = "LV",
#                 #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
#                 control.start = (n.init = 10), jitter.var = 0.1,
#                 control = list(reltol.c = 1e-8), seed = 13453) #this changes the optimization criteria to be stricter for convergence (added in response to a warning message I got)
#   criteria[i] <- summary(fiti)$AICc
#   names(criteria)[i] = i
# }
# # Compare AICc values
# criteria
#IF I HAVE CONVERGENCE ISSUES ON ONE MODEL HERE THEN I THE WHOLE LOOP STOPS: DO THEM INDIVIDUALLY INSTEAD:

model19.1LV <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData,
              lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM + RBS + CAP,
              row.eff = ~(1|lake),
              num.lv.c = 1, family = "tweedie", Power = NULL,
              randomB = "LV", quadratic = "LV",
              #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
              control.start = (n.init = 10), jitter.var = 0.1,
              control = list(reltol.c = 1e-8), seed = 13453) #this changes the optimization criteria to be stricter for convergence (added in response to a warning message I got)
#saveRDS(model19.1LV, file = "Models/model19.1LV.rds")

model19.2LV <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData,
                     lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM + RBS + CAP,
                     row.eff = ~(1|lake),
                     num.lv.c = 2, family = "tweedie", Power = NULL,
                     randomB = "LV", quadratic = "LV",
                     #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                     control.start = (n.init = 10), jitter.var = 0.1,
                     control = list(reltol.c = 1e-8), seed = 13453) #this changes the optimization criteria to be stricter for convergence (added in response to a warning message I got)
#saveRDS(model19.2LV, file = "Models/model19.2LV.rds")
model19.3LV <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData,
                     lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM + RBS + CAP,
                     row.eff = ~(1|lake),
                     num.lv.c = 3, family = "tweedie", Power = NULL,
                     randomB = "LV", quadratic = "LV",
                     #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                     control.start = (n.init = 10), jitter.var = 0.1,
                     control = list(reltol.c = 1e-8), seed = 13453) #this changes the optimization criteria to be stricter for convergence (added in response to a warning message I got)
#saveRDS(model19.3LV, file = "Models/model19.3LV.rds")
model19.4LV <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData,
                     lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM + RBS + CAP,
                     row.eff = ~(1|lake),
                     num.lv.c = 4, family = "tweedie", Power = 1.6, #I SET THE POWER TO HELP WITH CONVERGENCE
                     randomB = "LV", quadratic = "LV",
                     #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                     control.start = (n.init = 10), jitter.var = 0.1,
                     control = list(reltol.c = 1e-8), seed = 13453) #this changes the optimization criteria to be stricter for convergence (added in response to a warning message I got)
#saveRDS(model19.4LV, file = "Models/model19.4LV.rds")
model19.5LV <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData,
                     lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM + RBS + CAP,
                     row.eff = ~(1|lake),
                     num.lv.c = 5, family = "tweedie", Power = 1.6,  #I SET THE POWER TO HELP WITH CONVERGENCE
                     randomB = "LV", quadratic = "LV",
                     #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                     control.start = (n.init = 10), jitter.var = 0.1,
                     control = list(reltol.c = 1e-8), seed = 13453) #this changes the optimization criteria to be stricter for convergence (added in response to a warning message I got)
#saveRDS(model19.5LV, file = "Models/model19.5LV.rds")
#MODEL 5LV WON'T CONVERGE. BUT I GOT 6 TO WORK
model19.6LV <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData,
                     lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM + RBS + CAP,
                     row.eff = ~(1|lake),
                     num.lv.c = 6, family = "tweedie", Power = NULL,
                     randomB = "LV", quadratic = "LV",
                     #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                     control.start = (n.init = 10), jitter.var = 0.1,
                     control = list(reltol.c = 1e-8), seed = 13453) #this changes the optimization criteria to be stricter for convergence (added in response to a warning message I got)
#saveRDS(model19.6LV, file = "Models/model19.6LV.rds")
#read in the latent variable test models
model19.1LV <- readRDS("Models/model19.1LV.rds")
model19.2LV <- readRDS("Models/model19.2LV.rds")
model19.3LV <- readRDS("Models/model19.3LV.rds")
model19.4LV <- readRDS("Models/model19.4LV.rds")
model19.5LV <- readRDS("Models/model19.5LV.rds")
model19.6LV <- readRDS("Models/model19.6LV.rds")
#compare AIC values
summary(model19.1LV)$AICc
summary(model19.2LV)$AICc
summary(model19.3LV)$AICc
summary(model19.4LV)$AICc
summary(model19.5LV)$AICc
summary(model19.6LV)$AICc
#compare BIC values
summary(model19.1LV)$BIC
summary(model19.2LV)$BIC
summary(model19.3LV)$BIC #THIS ONE
summary(model19.4LV)$BIC
summary(model19.5LV)$BIC
summary(model19.6LV)$BIC

#Model with 6LV has the lowest AIC but that's a lot of latent variables... what does performance look like?
#check power
model19.6LV$Power
#summary and model performance plots
summary(model19.6LV)
par(mfrow = c(1, 1))
plot(model19.6LV)
gllvm::ordiplot(model19.6LV, symbol = T)
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model19.6LV, "row.params.random"), Lake = names(coef(model19.6LV, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model19.6LV$params$sigma
#get residual correlations
Theta <- getResidualCor(model19.6LV)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#get correlations due to environment/covariates - CAN'T DO THIS WITHOUT A RANDOM COVARIATE STRUCTURE
Env <- getEnvironCor(model19.6LV)
corrplot(Theta[order.single(Env), order.single(Env)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#trying to get the environment correlations requires a random effect somewhere within the ordination, so I can't do this here
#run ?getEnvironCor for details!
#inspect the optima
optima(model19.6LV, sd.errors = FALSE)
plot(optima(model19.6LV, sd.errors = FALSE))
#inspect the tolerances
tolerances(model19.6LV, sd.errors = FALSE)
plot(tolerances(model19.6LV, sd.errors = FALSE))

#Look at model with 4LV
#check power
model19.4LV$Power
#summary and model performance plots
summary(model19.4LV)
par(mfrow = c(1, 1))
plot(model19.4LV)
gllvm::ordiplot(model19.4LV, symbol = T)
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model19.4LV, "row.params.random"), Lake = names(coef(model19.4LV, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model19.4LV$params$sigma
#get residual correlations
Theta <- getResidualCor(model19.4LV)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#get correlations due to environment/covariates - CAN'T DO THIS WITHOUT A RANDOM COVARIATE STRUCTURE
Env <- getEnvironCor(model19.4LV)
corrplot(Env[order.single(Env), order.single(Env)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#trying to get the environment correlations requires a random effect somewhere within the ordination, so I can't do this here
#run ?getEnvironCor for details!
#inspect the optima
optima(model19.4LV, sd.errors = FALSE)
plot(optima(model19.4LV, sd.errors = FALSE))
#inspect the tolerances
tolerances(model19.4LV, sd.errors = FALSE)
plot(tolerances(model19.4LV, sd.errors = FALSE))

#Look at model with 3LV
#check power
model19.3LV$Power
#summary and model performance plots
summary(model19.3LV)
par(mfrow = c(1, 1))
plot(model19.3LV)
gllvm::ordiplot(model19.3LV, symbol = T)
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model19.3LV, "row.params.random"), Lake = names(coef(model19.3LV, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model19.3LV$params$sigma
#get residual correlations
Theta <- getResidualCor(model19.3LV)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#get correlations due to environment/covariates - CAN'T DO THIS WITHOUT A RANDOM COVARIATE STRUCTURE
Env <- getEnvironCor(model19.3LV)
corrplot(Theta[order.single(Env), order.single(Env)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#trying to get the environment correlations requires a random effect somewhere within the ordination, so I can't do this here
#run ?getEnvironCor for details!
#inspect the optima
optima(model19.3LV, sd.errors = FALSE)
plot(optima(model19.3LV, sd.errors = FALSE))
#inspect the tolerances
tolerances(model19.3LV, sd.errors = FALSE)
plot(tolerances(model19.3LV, sd.errors = FALSE))



#MOVING FORWARD WITH 4 LATENT VARIABLES - lowest AIC without losing model performance
#check power
model19.4LV$Power
#summary and model performance plots
summary(model19.4LV)
par(mfrow = c(1, 1))
plot(model19.4LV)
#conditional ordinations = like weighted average score in classical ordination - gives each site a location based on the scores of all the species it contains
#this has all the information, explained and unexplained by covariates
gllvm::ordiplot(model19.4LV, type= "conditional", symbol = T, which.lvs = c(1,2))
gllvm::ordiplot(model19.4LV, type= "conditional", symbol = T, which.lvs = c(1,3))
gllvm::ordiplot(model19.4LV, type= "conditional", symbol = T, which.lvs = c(1,4))
gllvm::ordiplot(model19.4LV, type= "conditional", symbol = T, which.lvs = c(2,3))
gllvm::ordiplot(model19.4LV, type= "conditional", symbol = T, which.lvs = c(2,4))
gllvm::ordiplot(model19.4LV, type= "conditional", symbol = T, which.lvs = c(3,4))
#marginal ordinations = explained by covariates
gllvm::ordiplot(model19.4LV, type= "marginal", symbol = T, which.lvs = c(1,2))
gllvm::ordiplot(model19.4LV, type= "marginal", symbol = T, which.lvs = c(1,3))
gllvm::ordiplot(model19.4LV, type= "marginal", symbol = T, which.lvs = c(1,4))
gllvm::ordiplot(model19.4LV, type= "marginal", symbol = T, which.lvs = c(2,3))
gllvm::ordiplot(model19.4LV, type= "marginal", symbol = T, which.lvs = c(2,4))
gllvm::ordiplot(model19.4LV, type= "marginal", symbol = T, which.lvs = c(3,4))
#residual ordinations = NOT explained by covariates
gllvm::ordiplot(model19.4LV, type= "residual", symbol = T, which.lvs = c(1,2))
gllvm::ordiplot(model19.4LV, type= "residual", symbol = T, which.lvs = c(1,3))
gllvm::ordiplot(model19.4LV, type= "residual", symbol = T, which.lvs = c(1,4))
gllvm::ordiplot(model19.4LV, type= "residual", symbol = T, which.lvs = c(2,3))
gllvm::ordiplot(model19.4LV, type= "residual", symbol = T, which.lvs = c(2,4))
gllvm::ordiplot(model19.4LV, type= "residual", symbol = T, which.lvs = c(3,4))


#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model19.4LV, "row.params.random"), Lake = names(coef(model19.4LV, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model19.4LV$params$sigma

#get residual correlations
Theta <- getResidualCor(model19.4LV)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#get correlations due to environment/covariates - CAN'T DO THIS WITHOUT A RANDOM COVARIATE STRUCTURE
Env <- getEnvironCor(model19.4LV)
corrplot(Theta[order.single(Env), order.single(Env)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#trying to get the environment correlations requires a random effect somewhere within the ordination, so I can't do this here
#run ?getEnvironCor for details!
#inspect the optima
optima(model19.4LV, sd.errors = FALSE)
plot(optima(model19.4LV, sd.errors = FALSE))
#inspect the tolerances
tolerances(model19.4LV, sd.errors = FALSE)
plot(tolerances(model19.4LV, sd.errors = FALSE))
#extract canonical coefficients
coef(model19.4LV, parm="Cancoef")

#ABDONDON SHIP ON THIS MODEL
#I looked at some species response curves and it looks awful


#---------------------------------------------------------------------------------------------------
#drop CAP from response matrix
y_raw_noCAP <- y_raw %>% 
  select(-CAP.CPUE)
#drop RBS from response matrix
y_raw_noRBS <- y_raw %>% 
  select(-RBS.CPUE)
#drop RBS AND CAP from response matrix
y_raw_noRBS_noCAP <- y_raw %>% 
  select(-RBS.CPUE, - CAP.CPUE)

#this is just running a simple model again without carp as a predictor
model23 <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData,
                     formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM,
                     row.eff = ~(1|lake),
                     num.lv = 3, family = "tweedie", Power = NULL,
                     #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                     control.start = (n.init = 10), jitter.var = 0.1)
beep()
#saveRDS(model23, file = "Models/model23.rds")
plot(model23)

#try model with covariates outside but make temp and secchi quadratic effects
#env outside ordination with 3 lvs, raw response data
#random effects are NOT species-specific
#no more CAP pr RBS predictors - now part of response matrix
model24 <- gllvm(y = y_raw_noRBS, X = x_scale, studyDesign = studyDesignData,
                 formula = ~ CDOM + Area + Max_Depth + poly(Secchi, 2, raw = TRUE) + poly(GDD, 2, raw = TRUE) + Precip + Photic + SWF + ZM + RBS,
                 row.eff = ~(1|lake),
                 num.lv = 3, family = "tweedie", Power = NULL,
                 #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                 control.start = (n.init = 10), jitter.var = 0.1)
beep()
#saveRDS(model24, file = "Models/model24.rds")
plot(model24)


#drop copepodites from y matrix
y_raw_no_copepodite <- y_raw %>% 
  select(-copepodites)

#model 14 without the carp or rainbow smelt predictors, and without copepodites
model25 <- gllvm(y = y_raw_no_copepodite, X = x_scale, studyDesign = studyDesignData,
                       formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM,
                       row.eff = ~(1|lake),
                       quadratic = "LV",
                       num.lv = 3, family = "tweedie", Power = NULL,
                       #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                       control.start = (n.init = 10), jitter.var = 0.1)
beep()
model25 <- readRDS("Models/model25.rds")
plot(model25)
#saveRDS(model25, file = "Models/model25.rds")


#model 15 without the carp or rainbow smelt predictors, and without copepodites
model26 <- gllvm(y = y_raw_no_copepodite, X = x_scale, studyDesign = studyDesignData,
                 formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM,
                 row.eff = ~(1|lake),
                 quadratic = TRUE,
                 num.lv = 3, family = "tweedie", Power = NULL,
                 #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                 control.start = (n.init = 10), jitter.var = 0.1)
beep()
plot(model26)
#saveRDS(model26, file = "Models/model26.rds")
model26 <- readRDS("Models/model26.rds")
#save residuals - this had to run overnight so saving as a csv
# res_model26 <- residuals(model26)
# res_model26_res <- as.data.frame(res_model26$residuals)
# res_model26_lin <- as.data.frame(res_model26$linpred) #these are the linear predictors
# write.csv(res_model26_res, file = "Models/model26_residuals.csv", row.names = TRUE)
# write.csv(res_model26_lin, file = "Models/model26_linpred.csv", row.names = TRUE)
res_model26_res <- read.csv("Models/model26_residuals.csv") %>% 
  select(-X)
res_model26_lin <- read.csv("Models/model26_linpred.csv") %>% 
  select(-X)
#make manual qqplot
#then plot them manually - regular plot doesn't work because it takes so long to get residuals
res_long <- res_model26_res %>%
  pivot_longer(cols = everything(), 
               names_to = "Species", 
               values_to = "ResidualValue")
ggplot(res_long, aes(sample = ResidualValue)) +
  stat_qq(alpha = 0.3) + # alpha helps see density if you have many points
  stat_qq_line(color = "blue") +
  theme_minimal() +
  labs(title = "Combined Q-Q Plot for All Species",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")
#color by species to see if a certain speciesis the problem
ggplot(res_long, aes(sample = ResidualValue, color = Species)) +
  stat_qq(alpha = 0.3) + # alpha helps see density if you have many points
  stat_qq_line(color = "blue") +
  theme_minimal() +
  labs(title = "Combined Q-Q Plot for All Species",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")


#this is model 19 without copepodites and no RBS or CAP predictors, CAP now in response matrix
model27 <- gllvm(y = y_raw_no_copepodite, X = x_scale, studyDesign = studyDesignData,
                          lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM,
                          row.eff = ~(1|lake),
                          num.lv.c = 3, family = "tweedie", Power = NULL,
                          randomB = "LV", quadratic = "LV",
                          #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                          control.start = (n.init = 10), jitter.var = 0.1,
                          control = list(reltol.c = 1e-8)) #this changes the optimization criteria to be stricter for convergence (added in response to a warning message I got)
beep()
#save output so I don't always have to rerun it
#saveRDS(model27, file = "model27.rds")
plot(model27)
#inspect the optima
optima(model27, sd.errors = FALSE)
plot(optima(model27, sd.errors = FALSE))
#inspect the tolerances
tolerances(model27, sd.errors = FALSE)
plot(tolerances(model27, sd.errors = FALSE))



#WHAT IF I GROUP ALL THE COPEPODS?
y_raw_copepod <- y_raw %>% 
  mutate(copepod = calanoids + copepodites + cyclopoids + nauplii) %>% 
  select(-calanoids, -copepodites, -cyclopoids, -nauplii)


#this is same as 27 but the copepods are now all grouped together into one column
model28 <- gllvm(y = y_raw_copepod, X = x_scale, studyDesign = studyDesignData,
                 lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM,
                 row.eff = ~(1|lake),
                 num.lv.c = 3, family = "tweedie", Power = NULL,
                 randomB = "LV", quadratic = "LV",
                 #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                 control.start = (n.init = 10), jitter.var = 0.1,
                 control = list(reltol.c = 1e-8)) #this changes the optimization criteria to be stricter for convergence (added in response to a warning message I got)
beep()
#save output so I don't always have to rerun it
#saveRDS(model28, file = "model28.rds")
plot(model28)
#inspect the optima
optima(model28, sd.errors = FALSE)
plot(optima(model28, sd.errors = FALSE))
#inspect the tolerances
tolerances(model28, sd.errors = FALSE)
plot(tolerances(model28, sd.errors = FALSE))



#WHAT ABOUT JUST NO COPEPODS
y_raw_no_copepod <- y_raw %>% 
  select(-calanoids, -copepodites, -cyclopoids, -nauplii)


#this is same as 27 and 28 but now I dropped the copepods entirely
model29 <- gllvm(y = y_raw_no_copepod, X = x_scale, studyDesign = studyDesignData,
                 lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM,
                 row.eff = ~(1|lake),
                 num.lv.c = 3, family = "tweedie", Power = NULL,
                 randomB = "LV", quadratic = "LV",
                 #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                 control.start = (n.init = 10), jitter.var = 0.1,
                 control = list(reltol.c = 1e-8)) #this changes the optimization criteria to be stricter for convergence (added in response to a warning message I got)
beep()
#save output so I don't always have to rerun it
#saveRDS(model29, file = "model29.rds")
plot(model29)
#inspect the optima
optima(model29, sd.errors = FALSE)
plot(optima(model29, sd.errors = FALSE))
#inspect the tolerances
tolerances(model29, sd.errors = FALSE)
plot(tolerances(model29, sd.errors = FALSE))
coef(model29, parm="Cancoef")


#quadratic effect, same tolerance for all species, all copepods grouped together, env outside
model30 <- gllvm(y = y_raw_copepod, X = x_scale, studyDesign = studyDesignData,
                 formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM,
                 row.eff = ~(1|lake),
                 quadratic = "LV",
                 num.lv = 3, family = "tweedie", Power = NULL,
                 #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                 control.start = (n.init = 10), jitter.var = 0.1)
beep()
plot(model30)
saveRDS(model30, file = "Models/model30.rds")


#quadratic effect, same tolerance for all species, NO COPEPODS AT ALL, env outside
model31 <- gllvm(y = y_raw_no_copepod, X = x_scale, studyDesign = studyDesignData,
                 formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM,
                 row.eff = ~(1|lake),
                 quadratic = "LV",
                 num.lv = 3, family = "tweedie", Power = NULL,
                 #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                 control.start = (n.init = 10), jitter.var = 0.1)
beep()
plot(model31)
#saveRDS(model31, file = "Models/model31.rds")

#model 14 but without CAP as a predictor, yes it's in response matrix
model14_noCAP <- gllvm(y = y_raw_noRBS, X = x_scale, studyDesign = studyDesignData, 
                 formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM + RBS,
                 row.eff = ~(1|lake), 
                 quadratic = "LV",
                 num.lv = 3, family = "tweedie", Power = NULL, 
                 #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                 control.start = (n.init = 10), jitter.var = 0.1)
beep()
#save output so I don't always have to rerun it
#saveRDS(model14_noCAP, file = "model14_noCAP.rds")
plot(model14_noCAP)
options(max.print = 2500)
summary(model14_noCAP)
summary(model14)



model14_CAPpred <- gllvm(y = y_raw_noRBS_noCAP, X = x_scale, studyDesign = studyDesignData, 
                         formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM + RBS + CAP,
                         row.eff = ~(1|lake), 
                         quadratic = "LV",
                         num.lv = 3, family = "tweedie", Power = NULL, 
                         #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                         control.start = (n.init = 10), jitter.var = 0.1)
beep()
#saveRDS(model14_CAPpred, file = "Models/model14_CAPpred.rds")
plot(model14_CAPpred)



#model 14 but without RBS as a predictor, yes it's in response matrix
model14_noRBS <- gllvm(y = y_raw_noCAP, X = x_scale, studyDesign = studyDesignData, 
                       formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM + CAP,
                       row.eff = ~(1|lake), 
                       quadratic = "LV",
                       num.lv = 3, family = "tweedie", Power = NULL, 
                       #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                       control.start = (n.init = 10), jitter.var = 0.1)
beep()
#save output so I don't always have to rerun it
#saveRDS(model14_noRBS, file = "model14_noRBS.rds")
plot(model14_noRBS)

#model 14 but without RBS OR CAP as predictors, yes they are in response matrix
model14_noRBS_noCAP <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData, 
                       formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM,
                       row.eff = ~(1|lake), 
                       quadratic = "LV",
                       num.lv = 3, family = "tweedie", Power = NULL, 
                       #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                       control.start = (n.init = 10), jitter.var = 0.1)
beep()
#save output so I don't always have to rerun it
#saveRDS(model14_noRBS_noCAP, file = "model14_noRBS_noCAP.rds")
plot(model14_noRBS_noCAP)
summary(model14_noRBS_noCAP)


#model 14 but without RBS OR CAP as predictors, yes they are in response matrix
model14_noRBS_noCAP_noCopepods <- gllvm(y = y_raw_no_copepod, X = x_scale, studyDesign = studyDesignData, 
                             formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM,
                             row.eff = ~(1|lake), 
                             quadratic = "LV",
                             num.lv = 3, family = "tweedie", Power = NULL, 
                             #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                             control.start = (n.init = 10), jitter.var = 0.1)
beep()
#save output so I don't always have to rerun it
#saveRDS(model14_noRBS_noCAP_noCopepods, file = "model14_noRBS_noCAP_noCopepods.rds")
plot(model14_noRBS_noCAP_noCopepods)

#simplify again
model32 <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData, 
                 formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM,
                 row.eff = ~(1|lake),
                 num.lv = 3, family = "tweedie", Power = NULL, 
                 #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                 control.start = (n.init = 10), jitter.var = 0.1)
beep()
#save output so I don't always have to rerun it
#saveRDS(model32, file = "model32.rds")
plot(model32)

#add analyst to simple model - does it help?
model33 <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData, 
                 formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM + analyst,
                 row.eff = ~(1|lake),
                 num.lv = 3, family = "tweedie", Power = NULL, 
                 #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                 control.start = (n.init = 10), jitter.var = 0.1)
beep()
#save output so I don't always have to rerun it
#saveRDS(model33, file = "model33.rds")
plot(model33)
summary(model33)

#back to concurrent. This is model 19 but made gdd and secchi quadratic, no universal quadratic response

model34 <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData,
                          lv.formula = ~ CDOM + Area + Max_Depth + poly(Secchi, 2, raw = TRUE) + poly(GDD, 2, raw = TRUE) + Photic + SWF + ZM,
                          row.eff = ~(1|lake),
                          num.lv.c = 3, family = "tweedie", Power = NULL,
                          randomB = "LV",
                          #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                          control.start = (n.init = 10), jitter.var = 0.1,
                          control = list(reltol.c = 1e-8)) #this changes the optimization criteria to be stricter for convergence (added in response to a warning message I got)
beep()
#save output so I don't always have to rerun it
#saveRDS(model34, file = "model34.rds")
par(mfrow = c(1, 1))
plot(model34)
summary(model34)
#look at coefficient effects
randomCoefplot(model34)



model34 <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData,
                 lv.formula = ~ CDOM + Area + Max_Depth + poly(Secchi, 2, raw = TRUE) + poly(GDD, 2, raw = TRUE) + Photic + SWF + ZM,
                 row.eff = ~(1|lake),
                 num.lv.c = 3, family = "tweedie", Power = NULL,
                 randomB = "LV",
                 #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                 control.start = (n.init = 10), jitter.var = 0.1,
                 control = list(reltol.c = 1e-8)) #this changes the optimization criteria to be stricter for convergence (added in response to a warning message I got)
beep()
#save output so I don't always have to rerun it
#saveRDS(model34, file = "model34.rds")
par(mfrow = c(1, 1))
plot(model34)
summary(model34)
#look at coefficient effects
randomCoefplot(model34)
#get residual correlations
Theta <- getResidualCor(model34)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#get correlations due to environment/covariates - CAN'T DO THIS WITHOUT A RANDOM COVARIATE STRUCTURE
Env <- getEnvironCor(model34)
corrplot(Theta[order.single(Env), order.single(Env)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")



model35 <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData,
                 lv.formula = ~ poly(CDOM, 2, raw = TRUE) + poly(Area, 2, raw = TRUE) + poly(Max_Depth, 2, raw = TRUE) + poly(Secchi, 2, raw = TRUE) + poly(GDD, 2, raw = TRUE) + poly(Photic, 2, raw = TRUE) + SWF + ZM,
                 row.eff = ~(1|lake),
                 num.lv.c = 3, family = "tweedie", Power = NULL,
                 randomB = "LV",
                 #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                 control.start = (n.init = 10), jitter.var = 0.1,
                 control = list(reltol.c = 1e-8)) #this changes the optimization criteria to be stricter for convergence (added in response to a warning message I got)
beep()
#save output so I don't always have to rerun it
#saveRDS(model35, file = "model35.rds")
par(mfrow = c(1, 1))
plot(model35)
summary(model35)
#look at coefficient effects
randomCoefplot(model35)
#get residual correlations
par(mfrow = c(1, 1))
Theta <- getResidualCor(model35)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#get correlations due to environment/covariates - CAN'T DO THIS WITHOUT A RANDOM COVARIATE STRUCTURE
Env <- getEnvironCor(model35)
corrplot(Theta[order.single(Env), order.single(Env)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")


#Okay I am beginning to think that model 19 with 4 latent variables might be okay
#But need to run latent variable checks again without CAP and without RBS
test.1LV <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData,
                     lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM,
                     row.eff = ~(1|lake),
                     num.lv.c = 1, family = "tweedie", Power = NULL,
                     randomB = "LV", quadratic = "LV",
                     #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                     control.start = (n.init = 10), jitter.var = 0.1,
                     control = list(reltol.c = 1e-8), seed = 13453) #this changes the optimization criteria to be stricter for convergence (added in response to a warning message I got)
#saveRDS(test.1LV, file = "Models/test.1LV.rds")

test.2LV <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData,
                     lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM,
                     row.eff = ~(1|lake),
                     num.lv.c = 2, family = "tweedie", Power = NULL,
                     randomB = "LV", quadratic = "LV",
                     #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                     control.start = (n.init = 10), jitter.var = 0.1,
                     control = list(reltol.c = 1e-8), seed = 13453) #this changes the optimization criteria to be stricter for convergence (added in response to a warning message I got)
#saveRDS(test.2LV, file = "Models/test.2LV.rds")
test.3LV <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData,
                     lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM,
                     row.eff = ~(1|lake),
                     num.lv.c = 3, family = "tweedie", Power = NULL,
                     randomB = "LV", quadratic = "LV",
                     #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                     control.start = (n.init = 10), jitter.var = 0.1,
                     control = list(reltol.c = 1e-8), seed = 13453) #this changes the optimization criteria to be stricter for convergence (added in response to a warning message I got)
#saveRDS(test.3LV, file = "Models/test.3LV.rds")
test.4LV <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData,
                     lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM,
                     row.eff = ~(1|lake),
                     num.lv.c = 4, family = "tweedie", Power = NULL,
                     randomB = "LV", quadratic = "LV",
                     #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                     control.start = (n.init = 10), jitter.var = 0.1)#,
                     #control = list(reltol.c = 1e-8), seed = 13453) #this changes the optimization criteria to be stricter for convergence (added in response to a warning message I got)
#saveRDS(test.4LV, file = "Models/test.4LV.rds")
test.5LV <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData,
                     lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM,
                     row.eff = ~(1|lake),
                     num.lv.c = 5, family = "tweedie", Power = NULL,
                     randomB = "LV", quadratic = "LV",
                     #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                     control.start = (n.init = 10), jitter.var = 0.1,
                     control = list(reltol.c = 1e-8), seed = 13453) #this changes the optimization criteria to be stricter for convergence (added in response to a warning message I got)
#saveRDS(test.5LV, file = "Models/test.5LV.rds")
test.6LV <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData,
                     lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM,
                     row.eff = ~(1|lake),
                     num.lv.c = 6, family = "tweedie", Power = NULL,
                     randomB = "LV", quadratic = "LV",
                     #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                     control.start = (n.init = 10), jitter.var = 0.1,
                     control = list(reltol.c = 1e-8), seed = 13453) #this changes the optimization criteria to be stricter for convergence (added in response to a warning message I got)
#saveRDS(test.6LV, file = "Models/test.6LV.rds")
#read in the latent variable test models
test.1LV <- readRDS("Models/test.1LV.rds")
test.2LV <- readRDS("Models/test.2LV.rds")
test.3LV <- readRDS("Models/test.3LV.rds")
test.4LV <- readRDS("Models/test.4LV.rds")
test.5LV <- readRDS("Models/test.5LV.rds")
test.6LV <- readRDS("Models/test.6LV.rds")
#compare AIC values
summary(test.1LV)$AICc
summary(test.2LV)$AICc
summary(test.3LV)$AICc
summary(test.4LV)$AICc
summary(test.5LV)$AICc
summary(test.6LV)$AICc
#compare BIC values
summary(test.1LV)$BIC
summary(test.2LV)$BIC
summary(test.3LV)$BIC #THIS ONE
summary(test.4LV)$BIC
summary(test.5LV)$BIC
summary(test.6LV)$BIC
#look at model performance
plot(test.1LV)
plot(test.2LV)
plot(test.3LV)
plot(test.4LV)
plot(test.5LV)
plot(test.6LV)


#move forward with 2 latent variables
summary(test.2LV)
#get residual correlations
par(mfrow = c(1, 1))
Theta <- getResidualCor(test.2LV)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#get correlations due to environment/covariates - CAN'T DO THIS WITHOUT A RANDOM COVARIATE STRUCTURE
Env <- getEnvironCor(test.2LV)
corrplot(Env[order.single(Env), order.single(Env)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")


#move forward with 5 latent variables
summary(test.4LV)
#get residual correlations
par(mfrow = c(1, 1))
Theta <- getResidualCor(test.4LV)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#get correlations due to environment/covariates - CAN'T DO THIS WITHOUT A RANDOM COVARIATE STRUCTURE
Env <- getEnvironCor(test.4LV)
corrplot(Env[order.single(Env), order.single(Env)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")


#TRY HYBRID ORDINATION
model36 <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData,
                  lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM,
                  row.eff = ~(1|lake),
                  num.RR = 2, num.lv = 2, family = "tweedie", Power = NULL,
                  randomB = "LV", quadratic = "LV",
                  control.start = (n.init = 10), jitter.var = 0.1)
#saveRDS(model36, file = "Models/model36.rds")
model36<-readRDS("Models/model36.rds")
plot(model36)
summary(model36)
#get residual correlations
par(mfrow = c(1, 1))
Theta <- getResidualCor(model36)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#get correlations due to environment/covariates
Env <- getEnvironCor(model36)
corrplot(Env[order.single(Env), order.single(Env)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#inspect the optima
optima(model36, sd.errors = FALSE)
plot(optima(model36, sd.errors = FALSE))
#inspect the tolerances
tolerances(model36, sd.errors = FALSE)
plot(tolerances(model36, sd.errors = FALSE))
#extract canonical coefficients
coef(model36, parm="Cancoef")

#TRY HYBRID ORDINATION but increase number of RR
#THIS IS CLOSE TO THE ONE-----------------------
model36_moreLV <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData,
                 lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM,
                 row.eff = ~(1|lake),
                 num.RR = 3, num.lv = 3, family = "tweedie", Power = NULL,
                 randomB = "LV", quadratic = "LV",
                 control.start = (n.init = 10), jitter.var = 0.1)
saveRDS(model36_moreLV, file = "Models/model36_moreLV.rds")
model36_moreLV <- readRDS("Models/model36_moreLV.rds")
plot(model36_moreLV)
summary(model36_moreLV)
#get residual correlations
par(mfrow = c(1, 1))
Theta <- getResidualCor(model36_moreLV)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#get correlations due to environment/covariates
Env <- getEnvironCor(model36_moreLV)
corrplot(Env[order.single(Env), order.single(Env)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model36_moreLV, "row.params.random"), Lake = names(coef(model36_moreLV, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model46$params$sigma
#inspect the optima
optima(model36_moreLV, sd.errors = FALSE)
plot(optima(model36_moreLV, sd.errors = FALSE))
#inspect the tolerances
tolerances(model36_moreLV, sd.errors = FALSE)
plot(tolerances(model36_moreLV, sd.errors = FALSE))
#extract canonical coefficients
coef(model36_moreLV, parm="Cancoef")
#visualize species tolerances
model <- model36_moreLV
LVs = getLV(model)
newLV = cbind(LV1 = seq(min(LVs[,1]), max(LVs[,1]), length.out=137), LV2 = 0, LV3 = 0)
preds <- predict(model, type = "response", newLV = newLV)
plot(NA, ylim = range(preds), xlim = c(range(getLV(model))), ylab  = "Predicted response", xlab = "LV1")
segments(x0=optima(model, sd.errors = FALSE)[,1],x1 = optima(model, sd.errors = FALSE)[,1], y0 = rep(0, ncol(model$y)), y1 = apply(preds,2,max), col = "red", lty = "dashed", lwd = 2)
rug(getLV(model)[,1])
sapply(1:ncol(model$y), function(j)lines(sort(newLV[,1]), preds[order(newLV[,1]),j], lwd = 2))

newLV = cbind(LV1 = 0, LV3 = 0, LV2 =  seq(min(LVs[,2]), max(LVs[,2]), length.out=137))
preds <- predict(model, type = "response", newLV = newLV)
plot(NA, ylim = range(preds), xlim = c(range(getLV(model))), ylab  = "Predicted response", xlab = "LV2")
segments(x0=optima(model, sd.errors = FALSE)[,2],x1 = optima(model, sd.errors = FALSE)[,2], y0 = rep(0, ncol(model$y)), y1 = apply(preds,2,max), col = "red", lty = "dashed", lwd = 2)
rug(getLV(model)[,2])
sapply(1:ncol(model$y), function(j)lines(sort(newLV[,2]), preds[order(newLV[,2]),j], lwd = 2))

newLV = cbind(LV1 = 0, LV2 = 0, LV3 =  seq(min(LVs[,3]), max(LVs[,3]), length.out=137))
preds <- predict(model, type = "response", newLV = newLV)
plot(NA, ylim = range(preds), xlim = c(range(getLV(model))), ylab  = "Predicted response", xlab = "LV3")
segments(x0=optima(model, sd.errors = FALSE)[,3],x1 = optima(model, sd.errors = FALSE)[,3], y0 = rep(0, ncol(model$y)), y1 = apply(preds,2,max), col = "red", lty = "dashed", lwd = 2)
rug(getLV(model)[,3])
sapply(1:ncol(model$y), function(j)lines(sort(newLV[,3]), preds[order(newLV[,3]),j], lwd = 2))

# Extract the effects of predictors on the Latent Variables (RR axes)
env_on_lv <- model36_moreLV$params$LvXcoef

# To see how they contribute to the first RR axis
barplot(env_on_lv[, 1], main = "Impact of predictors on RR Axis 1")
barplot(env_on_lv[, 2], main = "Impact of predictors on RR Axis 2")
barplot(env_on_lv[, 3], main = "Impact of predictors on RR Axis 3")

#manual ordination guided by google gemini - CHECK THIS THOROUGHLY IF YOU WANT TO USE THIS PLOT
# 1. Get Site Scores (Where the lakes sit on the axes)
# For a model with lv.formula, these are the "predicted" latent variables
site_scores <- model36_moreLV$lvs

# 2. Get Species Scores (Where species sit on the axes)
# These are the "loadings" or weights
spec_scores <- model36_moreLV$params$theta[, 1:(model36_moreLV$num.lv + model36_moreLV$num.lv.c)]

# 3. Get the Environmental Arrows (The "Rotation" matrix)
# This replaces your missing coefplot!
env_arrows <- model36_moreLV$params$LvXcoef

library(ggrepel)

# Create a data frame for species
spec_df <- as.data.frame(spec_scores)
colnames(spec_df) <- c("Axis1", "Axis2", "Axis3")
spec_df$Species <- rownames(spec_df)

# Highlight your important species
spec_df$IsImportant <- ifelse(spec_df$Species == "Your_Species_Name", "Yes", "No")

# Plotting Axis 1 vs Axis 2 (The first two Environmental Axes)
ggplot() +
  # Draw the Sites as small light-grey dots
  geom_point(data = as.data.frame(site_scores), aes(x = LV1, y = LV2), color = "grey80", alpha = 0.5) +
  # Draw the Species as colored points
  geom_point(data = spec_df, aes(x = Axis1, y = Axis2, color = IsImportant), size = 3) +
  # Add Species Labels (only for the important one or the top responders)
  geom_text_repel(data = spec_df, aes(x = Axis1, y = Axis2, label = Species)) +
  # Add the Environmental Arrows
  geom_segment(data = as.data.frame(env_arrows), 
               aes(x = 0, y = 0, xend = CLV1*2, yend = CLV2*2), # Multiplied by 2 to make arrows visible
               arrow = arrow(length = unit(0.2, "cm")), color = "blue") +
  geom_text(data = as.data.frame(env_arrows), 
            aes(x = CLV1*2.2, y = CLV2*2.2, label = rownames(env_arrows)), color = "blue") +
  theme_minimal() +
  scale_color_manual(values = c("Yes" = "red", "No" = "black")) +
  labs(title = "Concurrent GLLVM Biplot", x = "Environmental Axis 1", y = "Environmental Axis 2")


#TRY HYBRID ORDINATION but increase number of RR
model36_moreLV_nocop <- gllvm(y = y_nocopepod, X = x_scale, studyDesign = studyDesignData,
                        lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM,
                        row.eff = ~(1|lake),
                        num.RR = 3, num.lv = 3, family = "tweedie", Power = NULL,
                        randomB = "LV", quadratic = "LV",
                        control.start = (n.init = 10), jitter.var = 0.1)
saveRDS(model36_moreLV_nocop, file = "Models/model36_moreLV_nocop.rds")
model36_moreLV_nocop <- readRDS("Models/model36_moreLV_nocop.rds")
plot(model36_moreLV_nocop)
summary(model36_moreLV_nocop)
#get residual correlations
par(mfrow = c(1, 1))
Theta <- getResidualCor(model36_moreLV_nocop)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#get correlations due to environment/covariates
Env <- getEnvironCor(model36_moreLV_nocop)
corrplot(Env[order.single(Env), order.single(Env)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")




model36_4LV <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData,
                        lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM,
                        row.eff = ~(1|lake),
                        num.RR = 2, num.lv = 4, family = "tweedie", Power = NULL,
                        randomB = "LV", quadratic = "LV",
                        control.start = (n.init = 10), jitter.var = 0.1)
saveRDS(model36_4LV, file = "Models/model36_4LV.rds")
model36__4LV <- readRDS("Models/model36_4LV.rds")
plot(model36__4LV)
summary(model36__4LV)
#get residual correlations
par(mfrow = c(1, 1))
Theta <- getResidualCor(model36__4LV)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
Env <- getEnvironCor(model36__4LV)
corrplot(Env[order.single(Env), order.single(Env)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")



model36_4LV_3RR <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData,
                     lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM,
                     row.eff = ~(1|lake),
                     num.RR = 3, num.lv = 4, family = "tweedie", Power = NULL,
                     randomB = "LV", quadratic = "LV",
                     control.start = (n.init = 10), jitter.var = 0.1)
saveRDS(model36_4LV_3RR, file = "Models/model36_4LV_3RR.rds")
model36__4LV_3RR <- readRDS("Models/model36_4LV_3RR.rds")
plot(model36__4LV_3RR)
summary(model36__4LV_3RR)
#get residual correlations
par(mfrow = c(1, 1))
Theta <- getResidualCor(model36__4LV_3RR)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")





#TRY HYBRID ORDINATION without quadratic effects
model37 <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData,
                 lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM,
                 row.eff = ~(1|lake),
                 num.RR = 2, num.lv = 2, family = "tweedie", Power = NULL,
                 randomB = "LV",
                 control.start = (n.init = 10), jitter.var = 0.1)
#saveRDS(model37, file = "Models/model37.rds")
model37 <- readRDS("Models/model37.rds")
plot(model37)
summary(model37)
model37$Power
#get residual correlations
par(mfrow = c(1, 1))
Theta <- getResidualCor(model37)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#get correlations due to environment/covariates
Env <- getEnvironCor(model37)
corrplot(Env[order.single(Env), order.single(Env)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
randomCoefplot(model37)
coef(model37, parm="Cancoef")
randomCoefplot(model37, which.Xcoef = "GDD")
randomCoefplot(model37, which.Xcoef = "Secchi")


#TRY HYBRID ORDINATION without quadratic effects but square gdd and secchi
model38 <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData,
                 lv.formula = ~ CDOM + Area + Max_Depth + poly(Secchi, 2, raw = TRUE) + poly(GDD, 2, raw = TRUE) + Photic + SWF + ZM,
                 row.eff = ~(1|lake),
                 num.RR = 2, num.lv = 2, family = "tweedie", Power = NULL,
                 randomB = "LV",
                 control.start = (n.init = 10), jitter.var = 0.1)
#saveRDS(model38, file = "Models/model38.rds")
model38 <- readRDS("Models/model38.rds")
par(mfrow = c(1, 1))
plot(model38)
model38$Power
summary(model38)
#get residual correlations
par(mfrow = c(1, 1))
Theta <- getResidualCor(model38)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#get correlations due to environment/covariates
Env <- getEnvironCor(model38)
corrplot(Env[order.single(Env), order.single(Env)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
randomCoefplot(model38)
par(mfrow = c(1, 1))
randomCoefplot(model38, which.Xcoef = "Area")
randomCoefplot(model38, which.Xcoef = "poly(Secchi, 2, raw = TRUE)1")
randomCoefplot(model38, which.Xcoef = "poly(Secchi, 2, raw = TRUE)2")
randomCoefplot(model38, which.Xcoef = "poly(GDD, 2, raw = TRUE)1")
randomCoefplot(model38, which.Xcoef = "poly(GDD, 2, raw = TRUE)2")



#TRY HYBRID ORDINATION without quadratic effects but square gdd and secchi - BUT MORE LVS
model38_3LV <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData,
                 lv.formula = ~ CDOM + Area + Max_Depth + poly(Secchi, 2, raw = TRUE) + poly(GDD, 2, raw = TRUE) + Photic + SWF + ZM,
                 row.eff = ~(1|lake),
                 num.RR = 2, num.lv = 3, family = "tweedie", Power = NULL,
                 randomB = "LV",
                 control.start = (n.init = 10), jitter.var = 0.1)
#saveRDS(model38_3LV, file = "Models/model38_3LV.rds")
model38_3LV <- readRDS("Models/model38_3LV.rds")
par(mfrow = c(1, 1))
plot(model38_3LV)
model38_3LV$Power
Theta <- getResidualCor(model38_3LV)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")



#TRY HYBRID ORDINATION without quadratic effects but square gdd and secchi - BUT MORE LVS AND LOWER POWER
model38_3LV_lowpow <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData,
                     lv.formula = ~ CDOM + Area + Max_Depth + poly(Secchi, 2, raw = TRUE) + poly(GDD, 2, raw = TRUE) + Photic + SWF + ZM,
                     row.eff = ~(1|lake),
                     num.RR = 2, num.lv = 3, family = "tweedie", Power = 1.50,
                     randomB = "LV",
                     control.start = (n.init = 10), jitter.var = 0.1)
saveRDS(model38_3LV_lowpow, file = "Models/model38_3LV_lowpow.rds")
model38_3LV_lowpow <- readRDS("Models/model38_3LV_lowpow.rds")
par(mfrow = c(1, 1))
plot(model38_3LV_lowpow)
model38_3LV_lowpow$Power







#TRY HYBRID ORDINATION without quadratic effects but square gdd and secchi
model39 <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData,
                 lv.formula = ~ CDOM + Area + Max_Depth + poly(Secchi, 2, raw = TRUE) + poly(GDD, 2, raw = TRUE) + Photic + SWF + ZM,
                 row.eff = ~(1|lake),
                 num.RR = 3, num.lv = 4, family = "tweedie", Power = NULL,
                 randomB = "LV",
                 control.start = (n.init = 10), jitter.var = 0.1)
#saveRDS(model39, file = "Models/model39.rds")
par(mfrow = c(1, 1))
plot(model39)
summary(model39)
#get residual correlations
par(mfrow = c(1, 1))
Theta <- getResidualCor(model39)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#get correlations due to environment/covariates
Env <- getEnvironCor(model39)
corrplot(Env[order.single(Env), order.single(Env)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
randomCoefplot(model39)





#TRY HYBRID ORDINATION with quadratic effect but more latent variables
model40 <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData,
                            lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM,
                            row.eff = ~(1|lake),
                            num.RR = 2, num.lv = 3, family = "tweedie", Power = NULL,
                            randomB = "LV", quadratic = "LV",
                            control.start = (n.init = 10), jitter.var = 0.1)
#saveRDS(model40, file = "Models/model40.rds")
plot(model40)
summary(model40)
#get residual correlations
par(mfrow = c(1, 1))
Theta <- getResidualCor(model40)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#get correlations due to environment/covariates
Env <- getEnvironCor(model40)
corrplot(Env[order.single(Env), order.single(Env)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#inspect the optima
optima(model40, sd.errors = FALSE)
plot(optima(model40, sd.errors = FALSE))
#inspect the tolerances
tolerances(model40, sd.errors = FALSE)
plot(tolerances(model40, sd.errors = FALSE))


#TRY HYBRID ORDINATION with quadratic effect can we get 40 4 lvs?
model45 <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData,
                 lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM,
                 row.eff = ~(1|lake),
                 num.RR = 2, num.lv = 4, family = "tweedie", Power = NULL,
                 randomB = "LV", quadratic = "LV",
                 control.start = (n.init = 10), jitter.var = 0.1)
#saveRDS(model45, file = "Models/model45.rds")
plot(model45)
summary(model45)
#get residual correlations
par(mfrow = c(1, 1))
Theta <- getResidualCor(model45)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#get correlations due to environment/covariates
Env <- getEnvironCor(model45)
corrplot(Env[order.single(Env), order.single(Env)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#inspect the optima
optima(model45, sd.errors = FALSE)
plot(optima(model45, sd.errors = FALSE))
#inspect the tolerances
tolerances(model45, sd.errors = FALSE)
plot(tolerances(model45, sd.errors = FALSE))




#enough with this crap. environment is going outside
model41 <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData, 
                     formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM,
                     row.eff = ~(1|lake), 
                     num.lv = 3, family = "tweedie", Power = NULL, 
                     #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                     control.start = (n.init = 5), jitter.var = 0.1)
beep()
plot(model41)
summary(model41)
#saveRDS(model41, file = "Models/model41.rds")


#now add quadratic responses
model42 <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData, 
                 formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM,
                 row.eff = ~(1|lake), 
                 num.lv = 3, family = "tweedie", Power = NULL, quadratic = "LV",
                 #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                 control.start = (n.init = 5), jitter.var = 0.1)
beep()
plot(model42)
summary(model42)
#saveRDS(model42, file = "Models/model42.rds")

#fewer latent variables?
model43 <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData, 
                 formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM,
                 row.eff = ~(1|lake), 
                 num.lv = 2, family = "tweedie", Power = NULL, quadratic = "LV",
                 #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                 control.start = (n.init = 5), jitter.var = 0.1)
beep()
plot(model43)
summary(model43)
#saveRDS(model43, file = "Models/model43.rds")


#more latent variables?
model44 <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData, 
                 formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM,
                 row.eff = ~(1|lake), 
                 num.lv = 4, family = "tweedie", Power = NULL, quadratic = "LV",
                 #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                 control.start = (n.init = 5), jitter.var = 0.1)
beep()
plot(model44)
summary(model44)
#saveRDS(model44, file = "Models/model44.rds")
Theta <- getResidualCor(model44)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")



#TESTING LVS WITH COPEPODS------------------



#test how many latent variables I would need with the partially constrained ordination, quadratic response:
# 1. Create an empty dataframe to hold our scoreboard
bic_results <- data.frame(num.RR = integer(), 
                          num.lv = integer(), 
                          BIC = numeric(),
                          AICc=numeric())

# 2. Set up the grid (testing 1 to 3 for both parameters)
for (rr in 1:3) {
  for (lv in 1:3) {
    
    # Print a message so you know it hasn't frozen
    cat("Fitting model with num.RR =", rr, "and num.lv =", lv, "...\n")
    
    # 3. Use tryCatch to skip hard crashes
    model_fit <- tryCatch({
      
      gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData,
            lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM,
            row.eff = ~(1|lake),
            num.RR = rr, num.lv = lv, family = "tweedie", Power = NULL,
            randomB = "LV", quadratic = "LV",
            control.start = list(n.init = 10), jitter.var = 0.1)
      
    }, error = function(e) {
      cat("  -> Model crashed during fitting. Skipping...\n")
      return(NULL) 
    })
    
    # 4. Safely extract the BIC and save it
    if (!is.null(model_fit)) {
      
      # THE FIX: Use tryCatch on the BIC function itself
      current_bic <- tryCatch(BIC(model_fit), error = function(e) numeric(0))
      
      # ONLY add to the dataframe if current_bic actually contains a number
      if (length(current_bic) > 0 && !is.na(current_bic)) {
        bic_results <- rbind(bic_results, 
                             data.frame(num.RR = rr, num.lv = lv, BIC = current_bic))
        cat("  -> Success! BIC:", current_bic, "\n")
      } else {
        cat("  -> Model fit, but BIC could not be calculated (singular fit). Skipping...\n")
      }
    }
  }
}

# 5. Sort the scoreboard to put the best (lowest) BIC at the top!
bic_results <- bic_results[order(bic_results$BIC), ]
print("--- GRID SEARCH COMPLETE ---")
print(bic_results)


#test how many latent variables I would need with the partially constrained ordination, non quadratic response:
# 1. Create an empty dataframe to hold our scoreboard
bic_results_noquad <- data.frame(num.RR = integer(), 
                          num.lv = integer(), 
                          BIC = numeric(),
                          AICc=numeric())

# 2. Set up the grid (testing 1 to 3 for both parameters)
for (rr in 1:3) {
  for (lv in 1:3) {
    
    # Print a message so you know it hasn't frozen
    cat("Fitting model with num.RR =", rr, "and num.lv =", lv, "...\n")
    
    # 3. Use tryCatch to skip hard crashes
    model_fit <- tryCatch({
      
      gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData,
            lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM,
            row.eff = ~(1|lake),
            num.RR = rr, num.lv = lv, family = "tweedie", Power = NULL,
            randomB = "LV",
            control.start = list(n.init = 10), jitter.var = 0.1)
      
    }, error = function(e) {
      cat("  -> Model crashed during fitting. Skipping...\n")
      return(NULL) 
    })
    
    # 4. Safely extract the BIC and save it
    if (!is.null(model_fit)) {
      
      # THE FIX: Use tryCatch on the BIC function itself
      current_bic <- tryCatch(BIC(model_fit), error = function(e) numeric(0))
      
      # ONLY add to the dataframe if current_bic actually contains a number
      if (length(current_bic) > 0 && !is.na(current_bic)) {
        bic_results_noquad <- rbind(bic_results_noquad, 
                             data.frame(num.RR = rr, num.lv = lv, BIC = current_bic))
        cat("  -> Success! BIC:", current_bic, "\n")
      } else {
        cat("  -> Model fit, but BIC could not be calculated (singular fit). Skipping...\n")
      }
    }
  }
}

# 5. Sort the scoreboard to put the best (lowest) BIC at the top!
bic_results_noquad <- bic_results_noquad[order(bic_results_noquad$BIC), ]
print("--- GRID SEARCH COMPLETE ---")
print(bic_results_noquad)


# #test how many latent variables I would need with the partially constrained ordination, quadratic response with species specific tolerances:
# #THIS NEVER WORKED KEPT CRASHING
# # 1. Create an empty dataframe to hold our scoreboard
# bic_results_sppquad <- data.frame(num.RR = integer(), 
#                           num.lv = integer(), 
#                           BIC = numeric(),
#                           AICc=numeric())
# 
# # 2. Set up the grid (testing 1 to 3 for both parameters)
# for (rr in 1:3) {
#   for (lv in 1:3) {
#     
#     # Print a message so you know it hasn't frozen
#     cat("Fitting model with num.RR =", rr, "and num.lv =", lv, "...\n")
#     
#     # 3. Use tryCatch to skip hard crashes
#     model_fit <- tryCatch({
#       
#       gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData,
#             lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM,
#             row.eff = ~(1|lake),
#             num.RR = rr, num.lv = lv, family = "tweedie", Power = NULL,
#             randomB = "LV", quadratic = TRUE,
#             sd.errors = FALSE,
#             control.start = list(n.init = 10), jitter.var = 0.1)
#       
#     }, error = function(e) {
#       cat("  -> Model crashed during fitting. Skipping...\n")
#       return(NULL) 
#     })
#     
#     # 4. Safely extract the BIC and save it
#     if (!is.null(model_fit)) {
#       
#       # THE FIX: Use tryCatch on the BIC function itself
#       current_bic <- tryCatch(BIC(model_fit), error = function(e) numeric(0))
#       
#       # ONLY add to the dataframe if current_bic actually contains a number
#       if (length(current_bic) > 0 && !is.na(current_bic)) {
#         bic_results_sppquad <- rbind(bic_results_sppquad, 
#                              data.frame(num.RR = rr, num.lv = lv, BIC = current_bic))
#         cat("  -> Success! BIC:", current_bic, "\n")
#       } else {
#         cat("  -> Model fit, but BIC could not be calculated (singular fit). Skipping...\n")
#       }
#     }
#   }
# }
# 
# # 5. Sort the scoreboard to put the best (lowest) BIC at the top!
# bic_results_sppquad <- bic_results_sppquad[order(bic_results_sppquad$BIC), ]
# print("--- GRID SEARCH COMPLETE ---")
# print(bic_results_sppquad)




#test how many latent variables I would need with the partially constrained ordination, quadratic response, but DROP randomB:
# 1. Create an empty dataframe to hold our scoreboard
bic_results_norB <- data.frame(num.RR = integer(), 
                          num.lv = integer(), 
                          BIC = numeric(),
                          AICc=numeric())

# 2. Set up the grid (testing 1 to 3 for both parameters)
for (rr in 1:3) {
  for (lv in 1:3) {
    
    # Print a message so you know it hasn't frozen
    cat("Fitting model with num.RR =", rr, "and num.lv =", lv, "...\n")
    
    # 3. Use tryCatch to skip hard crashes
    model_fit <- tryCatch({
      
      gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData,
            lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM,
            row.eff = ~(1|lake),
            num.RR = rr, num.lv = lv, family = "tweedie", Power = NULL,
            quadratic = "LV",
            control.start = list(n.init = 10), jitter.var = 0.1)
      
    }, error = function(e) {
      cat("  -> Model crashed during fitting. Skipping...\n")
      return(NULL) 
    })
    
    # 4. Safely extract the BIC and save it
    if (!is.null(model_fit)) {
      
      # THE FIX: Use tryCatch on the BIC function itself
      current_bic <- tryCatch(BIC(model_fit), error = function(e) numeric(0))
      
      # ONLY add to the dataframe if current_bic actually contains a number
      if (length(current_bic) > 0 && !is.na(current_bic)) {
        bic_results_norB <- rbind(bic_results_norB, 
                             data.frame(num.RR = rr, num.lv = lv, BIC = current_bic))
        cat("  -> Success! BIC:", current_bic, "\n")
      } else {
        cat("  -> Model fit, but BIC could not be calculated (singular fit). Skipping...\n")
      }
    }
  }
}

# 5. Sort the scoreboard to put the best (lowest) BIC at the top!
bic_results_norB <- bic_results_norB[order(bic_results_norB$BIC), ]
print("--- GRID SEARCH COMPLETE ---")
print(bic_results_norB)




#testing how many lvs I need with environment completley outside, no quadratic effect
criteria.out <- matrix(NA, nrow = 3, ncol = 5)
for(i in 1:5){
  fiti <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData, 
                formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM,
                row.eff = ~(1|lake), 
                num.lv = i, family = "tweedie", Power = NULL, 
                control.start = list(n.init = 10), jitter.var = 0.1)
  criteria.out[1,i] <- i
  criteria.out[2,i] <- summary(fiti)$AICc
  criteria.out[3,i] <- summary(fiti)$BIC
}
# Compare AICc values
criteria.out


#testing how many lvs I need with environment completley outside, with quadratic effect
criteria.out.quad <- matrix(NA, nrow = 3, ncol = 5)
rownames(criteria.out.quad) <- c("Run_ID", "AICc", "BIC")
for(i in 1:5){
  fiti <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData, 
                formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM,
                row.eff = ~(1|lake), 
                num.lv = i, family = "tweedie", Power = NULL, quadratic = "LV",
                control.start = list(n.init = 10), jitter.var = 0.1)
  criteria.out.quad[1,i] <- i
  criteria.out.quad[2,i] <- summary(fiti)$AICc
  criteria.out.quad[3,i] <- summary(fiti)$BIC
}
# Compare AICc values
criteria.out.quad




#TESTING LVs WITHOUT COPEPODS------------------------------------------
y_nocopepod <- y_raw %>% 
  select(-copepodites, -cyclopoids, -calanoids, -nauplii)
  

#test how many latent variables I would need with the partially constrained ordination, quadratic response:
# 1. Create an empty dataframe to hold our scoreboard
bic_results <- data.frame(num.RR = integer(), 
                          num.lv = integer(), 
                          BIC = numeric(),
                          AICc=numeric())

# 2. Set up the grid (testing 1 to 3 for both parameters)
for (rr in 1:3) {
  for (lv in 1:3) {
    
    # Print a message so you know it hasn't frozen
    cat("Fitting model with num.RR =", rr, "and num.lv =", lv, "...\n")
    
    # 3. Use tryCatch to skip hard crashes
    model_fit <- tryCatch({
      
      gllvm(y = y_nocopepod, X = x_scale, studyDesign = studyDesignData,
            lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM,
            row.eff = ~(1|lake),
            num.RR = rr, num.lv = lv, family = "tweedie", Power = NULL,
            randomB = "LV", quadratic = "LV",
            control.start = list(n.init = 10), jitter.var = 0.1)
      
    }, error = function(e) {
      cat("  -> Model crashed during fitting. Skipping...\n")
      return(NULL) 
    })
    
    # 4. Safely extract the BIC and save it
    if (!is.null(model_fit)) {
      
      # THE FIX: Use tryCatch on the BIC function itself
      current_bic <- tryCatch(BIC(model_fit), error = function(e) numeric(0))
      
      # ONLY add to the dataframe if current_bic actually contains a number
      if (length(current_bic) > 0 && !is.na(current_bic)) {
        bic_results <- rbind(bic_results, 
                             data.frame(num.RR = rr, num.lv = lv, BIC = current_bic))
        cat("  -> Success! BIC:", current_bic, "\n")
      } else {
        cat("  -> Model fit, but BIC could not be calculated (singular fit). Skipping...\n")
      }
    }
  }
}

# 5. Sort the scoreboard to put the best (lowest) BIC at the top!
bic_results <- bic_results[order(bic_results$BIC), ]
print("--- GRID SEARCH COMPLETE ---")
print(bic_results)


#test how many latent variables I would need with the partially constrained ordination, non quadratic response:
# 1. Create an empty dataframe to hold our scoreboard
bic_results_noquad <- data.frame(num.RR = integer(), 
                                 num.lv = integer(), 
                                 BIC = numeric(),
                                 AICc=numeric())

# 2. Set up the grid (testing 1 to 3 for both parameters)
for (rr in 1:3) {
  for (lv in 1:3) {
    
    # Print a message so you know it hasn't frozen
    cat("Fitting model with num.RR =", rr, "and num.lv =", lv, "...\n")
    
    # 3. Use tryCatch to skip hard crashes
    model_fit <- tryCatch({
      
      gllvm(y = y_nocopepod, X = x_scale, studyDesign = studyDesignData,
            lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM,
            row.eff = ~(1|lake),
            num.RR = rr, num.lv = lv, family = "tweedie", Power = NULL,
            randomB = "LV",
            control.start = list(n.init = 10), jitter.var = 0.1)
      
    }, error = function(e) {
      cat("  -> Model crashed during fitting. Skipping...\n")
      return(NULL) 
    })
    
    # 4. Safely extract the BIC and save it
    if (!is.null(model_fit)) {
      
      # THE FIX: Use tryCatch on the BIC function itself
      current_bic <- tryCatch(BIC(model_fit), error = function(e) numeric(0))
      
      # ONLY add to the dataframe if current_bic actually contains a number
      if (length(current_bic) > 0 && !is.na(current_bic)) {
        bic_results_noquad <- rbind(bic_results_noquad, 
                                    data.frame(num.RR = rr, num.lv = lv, BIC = current_bic))
        cat("  -> Success! BIC:", current_bic, "\n")
      } else {
        cat("  -> Model fit, but BIC could not be calculated (singular fit). Skipping...\n")
      }
    }
  }
}

# 5. Sort the scoreboard to put the best (lowest) BIC at the top!
bic_results_noquad <- bic_results_noquad[order(bic_results_noquad$BIC), ]
print("--- GRID SEARCH COMPLETE ---")
print(bic_results_noquad)


#test how many latent variables I would need with the partially constrained ordination, quadratic response, but DROP randomB:
# 1. Create an empty dataframe to hold our scoreboard
bic_results_norB <- data.frame(num.RR = integer(), 
                               num.lv = integer(), 
                               BIC = numeric(),
                               AICc=numeric())

# 2. Set up the grid (testing 1 to 3 for both parameters)
for (rr in 1:3) {
  for (lv in 1:3) {
    
    # Print a message so you know it hasn't frozen
    cat("Fitting model with num.RR =", rr, "and num.lv =", lv, "...\n")
    
    # 3. Use tryCatch to skip hard crashes
    model_fit <- tryCatch({
      
      gllvm(y = y_nocopepod, X = x_scale, studyDesign = studyDesignData,
            lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM,
            row.eff = ~(1|lake),
            num.RR = rr, num.lv = lv, family = "tweedie", Power = NULL,
            quadratic = "LV",
            control.start = list(n.init = 10), jitter.var = 0.1)
      
    }, error = function(e) {
      cat("  -> Model crashed during fitting. Skipping...\n")
      return(NULL) 
    })
    
    # 4. Safely extract the BIC and save it
    if (!is.null(model_fit)) {
      
      # THE FIX: Use tryCatch on the BIC function itself
      current_bic <- tryCatch(BIC(model_fit), error = function(e) numeric(0))
      
      # ONLY add to the dataframe if current_bic actually contains a number
      if (length(current_bic) > 0 && !is.na(current_bic)) {
        bic_results_norB <- rbind(bic_results_norB, 
                                  data.frame(num.RR = rr, num.lv = lv, BIC = current_bic))
        cat("  -> Success! BIC:", current_bic, "\n")
      } else {
        cat("  -> Model fit, but BIC could not be calculated (singular fit). Skipping...\n")
      }
    }
  }
}

# 5. Sort the scoreboard to put the best (lowest) BIC at the top!
bic_results_norB <- bic_results_norB[order(bic_results_norB$BIC), ]
print("--- GRID SEARCH COMPLETE ---")
print(bic_results_norB)




#testing how many lvs I need with environment completely outside, no quadratic effect
criteria.out <- matrix(NA, nrow = 3, ncol = 5)
rownames(criteria.out) <- c("LV", "AICc", "BIC")
for(i in 1:5){
  fiti <- gllvm(y = y_nocopepod, X = x_scale, studyDesign = studyDesignData, 
                formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM,
                row.eff = ~(1|lake), 
                num.lv = i, family = "tweedie", Power = NULL, 
                control.start = list(n.init = 10), jitter.var = 0.1)
  criteria.out[1,i] <- i
  criteria.out[2,i] <- summary(fiti)$AICc
  criteria.out[3,i] <- summary(fiti)$BIC
}
# Compare AICc values
criteria.out


#testing how many lvs I need with environment completley outside, with quadratic effect
criteria.out.quad.nocop <- matrix(NA, nrow = 3, ncol = 5)
rownames(criteria.out.quad.nocop) <- c("LV", "AICc", "BIC")
for(i in 1:5){
  fiti <- gllvm(y = y_nocopepod, X = x_scale, studyDesign = studyDesignData, 
                formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM,
                row.eff = ~(1|lake), 
                num.lv = i, family = "tweedie", Power = NULL, quadratic = "LV",
                control.start = list(n.init = 10), jitter.var = 0.1)
  criteria.out.quad.nocop[1,i] <- i
  criteria.out.quad.nocop[2,i] <- summary(fiti)$AICc
  criteria.out.quad.nocop[3,i] <- summary(fiti)$BIC
}
# Compare AICc values
criteria.out.quad.nocop




#Test the quadratic terms for just gdd and secchi, have to do this without copepods or it won't converge
criteria.out.sq <- matrix(NA, nrow = 3, ncol = 5)
rownames(criteria.out.sq) <- c("Run_ID", "AICc", "BIC")
for(i in 1:5){
  fiti <- gllvm(y = y_nocopepod, X = x_scale, studyDesign = studyDesignData,
                formula = ~ CDOM + Area + Max_Depth + poly(Secchi, 2, raw = TRUE) + poly(GDD, 2, raw = TRUE) + Precip + Photic + SWF + ZM,
                row.eff = ~(1|lake),
                num.lv = i, family = "tweedie", Power = NULL,
                sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                control.start = (n.init = 10), jitter.var = 0.1)
  criteria.out.sq[1,i] <- i
  criteria.out.sq[2,i] <- summary(fiti)$AICc
  criteria.out.sq[3,i] <- summary(fiti)$BIC
}
# Compare AICc values
criteria.out.sq


#-------------------------------------------------------------------
#THE FINAL 3 CONTENDERS---------------------


#partially constrained, 1 RR (environment) and 2 LV(residual) axes, quadratic response with equal niche width
model45 <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData,
                 lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM,
                 row.eff = ~(1|lake),
                 num.RR = 1, num.lv = 2, family = "tweedie", Power = NULL,
                 randomB = "LV", quadratic = "LV",
                 control.start = list(n.init = 10), jitter.var = 0.1)
beep()
#save output so I don't always have to rerun it
#saveRDS(model45, file = "model45.rds")
plot(model45)
model45$Power
BIC(model45)
#inspect the optima
optima(model45, sd.errors = FALSE)
plot(optima(model45, sd.errors = FALSE))
#inspect the tolerances
tolerances(model45, sd.errors = FALSE)
plot(tolerances(model45, sd.errors = FALSE))
#look at latent variable loadings
coef(model45, parm="Cancoef")
plot(coef(model45, parm="Cancoef"))
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model45, "row.params.random"), Lake = names(coef(model45, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model45$params$sigma
#get residual correlations
Theta <- getResidualCor(model45)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#get correlations due to environment/covariates - CAN'T DO THIS WITHOUT A RANDOM COVARIATE STRUCTURE
Env <- getEnvironCor(model45)
corrplot(Env[order.single(Env), order.single(Env)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#trying to get the environment correlations requires a random effect somewhere within the ordination, so I can't do this here
#run ?getEnvironCor for details!
#look at coefficient effects
gllvm::randomCoefplot(model45)


#partially constrained, 1 RR (environment) and 2 LV(residual) axes, quadratic response with equal niche width BUT LIMIT THE POWER OF THE TWEEDIE
model45_lowpow <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData,
                 lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM,
                 row.eff = ~(1|lake),
                 num.RR = 1, num.lv = 2, family = "tweedie", Power = 1.50,
                 randomB = "LV", quadratic = "LV",
                 control.start = list(n.init = 10), jitter.var = 0.1)
beep()
#save output so I don't always have to rerun it
#saveRDS(model45_lowpow, file = "Models/model45_lowpow_50.rds")
plot(model45_lowpow)
model45_lowpow$Power
BIC(model45_lowpow)
#inspect the optima
optima(model45_lowpow, sd.errors = FALSE)
plot(optima(model45_lowpow, sd.errors = FALSE))
#inspect the tolerances
tolerances(model45_lowpow, sd.errors = FALSE)
plot(tolerances(model45_lowpow, sd.errors = FALSE))
#look at latent variable loadings
coef(model45_lowpow, parm="Cancoef")
plot(coef(model45_lowpow, parm="Cancoef"))
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model45_lowpow, "row.params.random"), Lake = names(coef(model45_lowpow, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model45_lowpow$params$sigma
#get residual correlations
Theta <- getResidualCor(model45_lowpow)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#get correlations due to environment/covariates - CAN'T DO THIS WITHOUT A RANDOM COVARIATE STRUCTURE
Env <- getEnvironCor(model45_lowpow)
corrplot(Env[order.single(Env), order.single(Env)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#trying to get the environment correlations requires a random effect somewhere within the ordination, so I can't do this here
#run ?getEnvironCor for details!
#look at coefficient effects
gllvm::randomCoefplot(model45_lowpow)


#environment outside, no quadratic effect, 1 LV
model46 <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData, 
              formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM,
              row.eff = ~(1|lake), 
              num.lv = 5, family = "tweedie", Power = NULL, 
              control.start = list(n.init = 10), jitter.var = 0.1)
#save output so I don't always have to rerun it
saveRDS(model46, file = "model46.rds")
plot(model46)
model46$Power
BIC(model46)
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model46, "row.params.random"), Lake = names(coef(model46, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model46$params$sigma
#get residual correlations
Theta <- getResidualCor(model46)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#look at coefficient effects
coefplot(model46,
         #which.Xcoef = "CDOM",
         cex.ylab = 1,
         order = TRUE)



#environment outside, quadratic effect with equal niche width, 2LV
model47 <- gllvm(y = y_raw, X = x_scale, studyDesign = studyDesignData, 
              formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM,
              row.eff = ~(1|lake), 
              num.lv = ___, family = "tweedie", Power = NULL, quadratic = "LV",
              control.start = list(n.init = 10), jitter.var = 0.1)
beep()
#save output so I don't always have to rerun it
saveRDS(model47, file = "model47.rds")
plot(model47)
model47$Power
BIC(model47)
#inspect the optima
optima(model47, sd.errors = FALSE)
plot(optima(model47, sd.errors = FALSE))
#inspect the tolerances
tolerances(model47, sd.errors = FALSE)
plot(tolerances(model47, sd.errors = FALSE))
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model47, "row.params.random"), Lake = names(coef(model47, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model47$params$sigma
#get residual correlations
par(mfrow = c(1, 1))
Theta <- getResidualCor(model47)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#look at coefficient effects
coefplot(model47,
         #which.Xcoef = "CDOM",
         cex.ylab = 1,
         order = TRUE)






#NO COPEPOD BEST MODELS

#Partially constrained ordination, no quadratic, no copepods
model48 <- gllvm(y = y_nocopepod, X = x_scale, studyDesign = studyDesignData,
                         lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM,
                          row.eff = ~(1|lake),
                          num.RR = 1, num.lv = 2, family = "tweedie", Power = NULL,
                          randomB = "LV",
                          control.start = list(n.init = 10), jitter.var = 0.1)
beep()
#saveRDS(model48, file = "model48.rds")
model48 <- readRDS("model48.RDS")
par(mfrow = c(1, 1))
plot(model48)
model48$Power
BIC(model48)
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model48, "row.params.random"), Lake = names(coef(model48, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model48$params$sigma
#get residual correlations
Theta <- getResidualCor(model48)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#look at latent variable loadings
coef(model48, parm="Cancoef")
plot(coef(model48, parm="Cancoef"))
#look at coefficient effects
gllvm::randomCoefplot(model48)


#Environment outside ordination, no quadratic, no copepods
model49 <- gllvm(y = y_nocopepod, X = x_scale, studyDesign = studyDesignData, 
                 formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM,
                 row.eff = ~(1|lake), 
                 num.lv = 4, family = "tweedie", Power = NULL, 
                 control.start = list(n.init = 10), jitter.var = 0.1)
#save output so I don't always have to rerun it
#saveRDS(model49, file = "model49.rds")
model49 <- readRDS("model49.rds")
plot(model49)
model49$Power
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model49, "row.params.random"), Lake = names(coef(model49, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model49$params$sigma
#get residual correlations
Theta <- getResidualCor(model49)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#look at coefficient effects
coefplot(model49,
         #which.Xcoef = "CDOM",
         cex.ylab = 1,
         order = TRUE)





#Environment outside ordination, quadratic, no copepodes
model50_3 <- gllvm(y = y_nocopepod, X = x_scale, studyDesign = studyDesignData, 
                 formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM,
                 row.eff = ~(1|lake), 
                 num.lv = 3, family = "tweedie", Power = 1.50, quadratic = "LV",
                 control.start = list(n.init = 10), jitter.var = 0.1)

#save output so I don't always have to rerun it
#saveRDS(model50_3, file = "model50_3.rds")
model50_4 <- gllvm(y = y_nocopepod, X = x_scale, studyDesign = studyDesignData, 
                   formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Precip + Photic + SWF + ZM,
                   row.eff = ~(1|lake), 
                   num.lv = 4, family = "tweedie", Power = NULL, quadratic = "LV",
                   control.start = list(n.init = 10), jitter.var = 0.1)

#save output so I don't always have to rerun it
#saveRDS(model50_4, file = "model50_4.rds")
model50_3 <- readRDS("model50_3.rds")
model50_4 <- readRDS("model50_4.rds")
par(mfrow = c(1, 1))
plot(model50_3)
plot(model50_4)
model50_3$Power
model50_4$Power
#inspect the optima
optima(model50_3, sd.errors = FALSE)
plot(optima(model50_3, sd.errors = FALSE))
optima(model50_4, sd.errors = FALSE)
plot(optima(model50_4, sd.errors = FALSE))
#inspect the tolerances
tolerances(model50, sd.errors = FALSE)
plot(tolerances(model50, sd.errors = FALSE))
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model50_3, "row.params.random"), Lake = names(coef(model50_3, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
rand.lake <- data.frame(Rand_Effect_Value = coef(model50_4, "row.params.random"), Lake = names(coef(model50_4, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model50_3$params$sigma
model50_4$params$sigma
#get residual correlations
par(mfrow = c(1, 2))
Theta_3 <- getResidualCor(model50_3)
Theta_4 <- getResidualCor(model50_4)
corrplot(Theta_3[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red",
         main = "3 LVs")
corrplot(Theta_4[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red",
         main = "4 LVs")
par(mfrow = c(1, 1))
#look at coefficient effects
coefplot(model50_3,
         #which.Xcoef = "CDOM",
         cex.ylab = 1,
         order = TRUE)
coefplot(model50_4,
         #which.Xcoef = "CDOM",
         cex.ylab = 1,
         order = TRUE)



#environment outside, square gdd and secchi predictors, no copepods
model51 <- gllvm(y = y_nocopepod, X = x_scale, studyDesign = studyDesignData,
                 formula = ~ CDOM + Area + Max_Depth + poly(Secchi, 2, raw = TRUE) + poly(GDD, 2, raw = TRUE) + Precip + Photic + SWF + ZM,
                 row.eff = ~(1|lake),
                 num.lv = ___, family = "tweedie", Power = NULL,
                 #sd.errors = FALSE, #this speeds up the model and be removed once I have my final model structure
                 control.start = (n.init = 10), jitter.var = 0.1)
beep()
#save output so I don't always have to rerun it
saveRDS(model51, file = "model51.rds")
plot(model51)
model51$Power
BIC(model51)
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model51, "row.params.random"), Lake = names(coef(model51, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model51$params$sigma
#get residual correlations
Theta <- getResidualCor(model51)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#look at coefficient effects
coefplot(model51,
         #which.Xcoef = "CDOM",
         cex.ylab = 1,
         order = TRUE)


#DATA PREP WITH TROUT LAKES----------------------------


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
  #filter(!(BKT.CPUE > 0 | LAT.CPUE > 0 | RBT.CPUE > 0)) %>%  
  #remove walleye yoy column
  select(-WAE.YOY.CPUE) %>% 
  #make rainbow smelt and and common carp into yes/no covariates like bytho and zebra mussels, then remove their CPUE columns
  mutate(RBS.yn = ifelse(RBS.CPUE > 0, "yes", "no"),
         CAP.yn = ifelse(CAP.CPUE > 0, "yes", "no")) %>% 
  #make all teh VNP lakes say yes for RBS
  mutate(RBS.yn = ifelse(lake_name == "Kabetogama" | lake_name == "Sand Point" | lake_name == "Namakan", "yes", "no")) %>% 
  #make all the carp lakes say yes after initial detection year
  mutate(CAP.yn = ifelse((lake_name == "Artichoke" & Year >= 2008)|(lake_name == "Peltier" & Year >= 2021)|(lake_name == "Belle" & Year >= 2008)|(lake_name == "Carlos" & Year >= 2008)|(lake_name == "Green" & Year >= 2018)|(lake_name == "Madison" & Year >= 2008)|(lake_name == "Mille Lacs" & Year >= 2015), "yes", "no")) %>% 
  #add analyst column
  mutate(analyst = ifelse(Year < 2020, "A", "B"))


#make covariate dataframe
x_trout <- data.filter %>% 
  select(secchi.meters.MPCA.Jul.to.Sept, gdd.year.5c, precip_5yr_avg_mm, CDOM.lake.avg, area_ha, depth.max.m, photic_prop_secchi.meters.MPCA.Jul.to.Sept, 
         SpinyWaterflea.yn, ZebraMussel.yn, RBS.yn, CAP.yn, analyst) %>% 
  #set categorical variables as factors
  mutate(SpinyWaterflea.yn = as.factor(SpinyWaterflea.yn),
         ZebraMussel.yn = as.factor(ZebraMussel.yn),
         RBS.yn = as.factor(RBS.yn),
         CAP.yn = as.factor(CAP.yn),
         analyst = as.factor(analyst)) %>% 
  #rename everything shorter
  rename(Secchi = secchi.meters.MPCA.Jul.to.Sept,
         GDD = gdd.year.5c,
         Precip = precip_5yr_avg_mm,
         CDOM = CDOM.lake.avg,
         Area = area_ha,
         Max_Depth = depth.max.m,
         Photic = photic_prop_secchi.meters.MPCA.Jul.to.Sept,
         SWF = SpinyWaterflea.yn,
         ZM = ZebraMussel.yn,
         RBS = RBS.yn,
         CAP = CAP.yn)

#standardize all quantitative variables (except the proportion) with scale function
x_scale_trout <- x_trout  %>% 
  mutate(Secchi = scale(Secchi),
         GDD = scale(GDD),
         Precip = scale(Precip),
         CDOM = scale(CDOM),
         Area = scale(Area),
         Max_Depth = scale(Max_Depth),
         Photic = scale(Photic, center = FALSE, scale = TRUE)) #SCALE BUT NOT CENTER FOR MODEL PERFORMANCE REASONS
#set x as a dataframe for rownames later
x_scale_trout <- as.data.frame(x_scale_trout)



#make a species abundance dataframe
#calculate relative abundance within fish and within zoops, then cbind them together
#now to be included, a taxa group must be present in at least 95% of samples (lake-years), be present in more than two lakes, and have passed an ecological gut check by Grace (is this species usually found in lakes?)

#INVESTIGATING RARE SPECIES
#First question: are there any species only present in one lake?
#regroup the daphnia how we discussed in committee meeting
data.daphnia <- data.filter %>% 
  mutate(Daphnia.small.rare = rowSums(across(c(Daphnia.rosea, Daphnia.ambigua, Daphnia.sp.)))) %>% 
  select(-Daphnia.rosea, -Daphnia.ambigua, -Daphnia.sp.) %>% 
  relocate(Daphnia.small.rare, .after = Daphnia.retrocurva)

#get the max value for each species in each lake (will be 0 if never present)
lake.spp <- data.daphnia %>% 
  group_by(lake_name) %>% 
  summarize(across(BIB.CPUE:nauplii, max),
            .groups = 'drop')
#for each species, count the number of lakes where it has a value greater than 0 in at least one year
spp.lake.count <- colSums(lake.spp > 0)
spp.lake.count
#make a list of species present in two or fewer lakes
spp.drop.lake <- names(spp.lake.count[spp.lake.count < 3])
spp.drop.lake

#calculate species present in less than 95% of lake-year samples
#isolate species
spp <- data.daphnia %>% 
  select(BIB.CPUE:nauplii)
#proportion of zeroes in species data
spp_prop_0 <- colSums(spp == 0, na.rm = TRUE)/nrow(spp)
#isolate proportions of zeroes over 95%
spp_prop_0_0.95 <- spp_prop_0[spp_prop_0 > 0.95]
#make this a vector of names
spp_names_rare <- names(spp_prop_0_0.95)

#combine the list of names for the two reasons to be dropped, only keep one if repeated in both lists
spp.drop <- unique(c(spp.drop.lake, spp_names_rare))
spp.drop

#remove columns for the species to drop
spp.filter <- spp %>% 
  select(-all_of(spp.drop))
#what's left?
names(spp.filter)

#look at magnitude of fish vs. zoop data
mag.plot.data <- spp.filter %>% 
  pivot_longer(cols = everything(), names_to = "species", values_to = "abundance") %>% 
  mutate(group = ifelse(str_ends(species, "CPUE"), "fish", "zoop"))
mag.plot.raw <- ggplot(data = mag.plot.data, aes(x = species, y = abundance, color = group))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
mag.plot.raw
#plot again but limit y axis to not include outliers
mag.plot.raw.zoom <- ggplot(data = mag.plot.data, aes(x = species, y = abundance, color = group))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  scale_y_continuous(limits = c(0,5))
mag.plot.raw.zoom
#actually looks okay, not transforming at all

#isolate fish data
fish <- spp.filter %>% 
  select(BLC.CPUE:YEP.CPUE)
#relative abundance
fish_rel_abun <- fish / rowSums(fish)

#isolate zoop data
zoop <- spp.filter %>% 
  select(Alona.sp.:nauplii)
#relative abundance
zoop_rel_abun <- zoop / rowSums(zoop)

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
y_rel_abun_trout <- cbind(fish_rel_abun, zoop_rel_abun)
y_raw_trout <- cbind(fish, zoop)

#set N as number of lake-years and N.taxa as number of taxa in response matrix
N <- nrow(x_trout)
N.taxa <- ncol(y_rel_abun_trout)

#set rownames to be the same in both matrices
rownames(x_trout) <- paste0(data.filter$lake_name, data.filter$Year)
rownames(x_scale_trout) <- paste0(data.filter$lake_name, data.filter$Year) #note that these rownames are there, just not visible in viewer
rownames(y_rel_abun_trout) <- paste0(data.filter$lake_name, data.filter$Year)
rownames(y_raw_trout) <- paste0(data.filter$lake_name, data.filter$Year)

# #create a version with the zoop summary stats too
# y_zoop_sum <- cbind(y, zoop_sum_filter)
#NOT DOING THIS


#create study design matrix
studyDesignData_trout <- data.frame(lake = as.factor(data.filter$lake_name),
                              year = as.factor(data.filter$Year))
rownames(studyDesignData_trout) <- paste0(data.filter$lake_name, data.filter$Year)

#with trout and WITHOUT any copepods
y_raw_trout_nocopepod <- y_raw_trout %>% 
  select(-copepodites, -nauplii, -cyclopoids, -calanoids)

#with trout and WITHOUT copepodites
y_raw_trout_nocopepodite <- y_raw_trout %>% 
  select(-copepodites)

#The ASLO model with trout lakes---------------
#Note that the trout themselves got dropped for being too rare but now we have lakes with no walleye
model36_moreLV_trout <- gllvm(y = y_raw_trout, X = x_scale_trout, studyDesign = studyDesignData_trout,
                        lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM,
                        row.eff = ~(1|lake),
                        num.RR = 3, num.lv = 3, family = "tweedie", Power = NULL,
                        randomB = "LV", quadratic = "LV",
                        control.start = (n.init = 10), jitter.var = 0.1)
beep()
#saveRDS(model36_moreLV_trout, file = "Models/model36_moreLV_trout.rds")
model36_moreLV_trout <- readRDS("Models/model36_moreLV_trout.rds")
plot(model36_moreLV_trout)
summary(model36_moreLV_trout)
#get residual correlations
par(mfrow = c(1, 1))
Theta <- getResidualCor(model36_moreLV_trout)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#get correlations due to environment/covariates
Env <- getEnvironCor(model36_moreLV_trout)
corrplot(Env[order.single(Env), order.single(Env)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model36_moreLV_trout, "row.params.random"), Lake = names(coef(model36_moreLV_trout, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model36_moreLV_trout$params$sigma
#inspect the optima
optima(model36_moreLV_trout, sd.errors = FALSE)
plot(optima(model36_moreLV_trout, sd.errors = FALSE))
#inspect the tolerances
tolerances(model36_moreLV_trout, sd.errors = FALSE)
plot(tolerances(model36_moreLV_trout, sd.errors = FALSE))
#extract canonical coefficients
coef(model36_moreLV_trout, parm="Cancoef")
#visualize species tolerances
model <- model36_moreLV_trout
LVs = getLV(model)
newLV = cbind(LV1 = seq(min(LVs[,1]), max(LVs[,1]), length.out=143), LV2 = 0, LV3 = 0)
preds <- predict(model, type = "response", newLV = newLV)
plot(NA, ylim = range(preds), xlim = c(range(getLV(model))), ylab  = "Predicted response", xlab = "LV1")
segments(x0=optima(model, sd.errors = FALSE)[,1],x1 = optima(model, sd.errors = FALSE)[,1], y0 = rep(0, ncol(model$y)), y1 = apply(preds,2,max), col = "red", lty = "dashed", lwd = 2)
rug(getLV(model)[,1])
sapply(1:ncol(model$y), function(j)lines(sort(newLV[,1]), preds[order(newLV[,1]),j], lwd = 2))

newLV = cbind(LV1 = 0, LV3 = 0, LV2 =  seq(min(LVs[,2]), max(LVs[,2]), length.out=143))
preds <- predict(model, type = "response", newLV = newLV)
plot(NA, ylim = range(preds), xlim = c(range(getLV(model))), ylab  = "Predicted response", xlab = "LV2")
segments(x0=optima(model, sd.errors = FALSE)[,2],x1 = optima(model, sd.errors = FALSE)[,2], y0 = rep(0, ncol(model$y)), y1 = apply(preds,2,max), col = "red", lty = "dashed", lwd = 2)
rug(getLV(model)[,2])
sapply(1:ncol(model$y), function(j)lines(sort(newLV[,2]), preds[order(newLV[,2]),j], lwd = 2))

newLV = cbind(LV1 = 0, LV2 = 0, LV3 =  seq(min(LVs[,3]), max(LVs[,3]), length.out=143))
preds <- predict(model, type = "response", newLV = newLV)
plot(NA, ylim = range(preds), xlim = c(range(getLV(model))), ylab  = "Predicted response", xlab = "LV3")
segments(x0=optima(model, sd.errors = FALSE)[,3],x1 = optima(model, sd.errors = FALSE)[,3], y0 = rep(0, ncol(model$y)), y1 = apply(preds,2,max), col = "red", lty = "dashed", lwd = 2)
rug(getLV(model)[,3])
sapply(1:ncol(model$y), function(j)lines(sort(newLV[,3]), preds[order(newLV[,3]),j], lwd = 2))

# Extract the effects of predictors on the Latent Variables (RR axes)
env_on_lv <- model36_moreLV_trout$params$LvXcoef

# To see how they contribute to the first RR axis
barplot(env_on_lv[, 1], main = "Impact of predictors on RR Axis 1")
barplot(env_on_lv[, 2], main = "Impact of predictors on RR Axis 2")
barplot(env_on_lv[, 3], main = "Impact of predictors on RR Axis 3")

#manual ordination guided by google gemini - CHECK THIS THOROUGHLY IF YOU WANT TO USE THIS PLOT
# 1. Get Site Scores (Where the lakes sit on the axes)
# For a model with lv.formula, these are the "predicted" latent variables
site_scores <- model36_moreLV_trout$lvs

# 2. Get Species Scores (Where species sit on the axes)
# These are the "loadings" or weights
spec_scores <- model36_moreLV_trout$params$theta[, 1:(model36_moreLV_trout$num.lv + model36_moreLV_trout$num.lv.c)]

# 3. Get the Environmental Arrows (The "Rotation" matrix)
# This replaces your missing coefplot!
env_arrows <- model36_moreLV_trout$params$LvXcoef

library(ggrepel)

# Create a data frame for species
spec_df <- as.data.frame(spec_scores)
colnames(spec_df) <- c("Axis1", "Axis2", "Axis3")
spec_df$Species <- rownames(spec_df)

# Highlight your important species
spec_df$IsImportant <- ifelse(spec_df$Species == "Your_Species_Name", "Yes", "No")

# Plotting Axis 1 vs Axis 2 (The first two Environmental Axes)
ggplot() +
  # Draw the Sites as small light-grey dots
  geom_point(data = as.data.frame(site_scores), aes(x = LV1, y = LV2), color = "grey80", alpha = 0.5) +
  # Draw the Species as colored points
  geom_point(data = spec_df, aes(x = Axis1, y = Axis2, color = IsImportant), size = 3) +
  # Add Species Labels (only for the important one or the top responders)
  geom_text_repel(data = spec_df, aes(x = Axis1, y = Axis2, label = Species)) +
  # Add the Environmental Arrows
  geom_segment(data = as.data.frame(env_arrows), 
               aes(x = 0, y = 0, xend = CLV1*2, yend = CLV2*2), # Multiplied by 2 to make arrows visible
               arrow = arrow(length = unit(0.2, "cm")), color = "blue") +
  geom_text(data = as.data.frame(env_arrows), 
            aes(x = CLV1*2.2, y = CLV2*2.2, label = rownames(env_arrows)), color = "blue") +
  theme_minimal() +
  scale_color_manual(values = c("Yes" = "red", "No" = "black")) +
  labs(title = "Concurrent GLLVM Biplot", x = "Environmental Axis 1", y = "Environmental Axis 2")







model36_moreLV_trout_nocopepodite <- gllvm(y = y_raw_trout_nocopepodite, X = x_scale_trout, studyDesign = studyDesignData_trout,
                              lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM,
                              row.eff = ~(1|lake),
                              num.RR = 3, num.lv = 3, family = "tweedie", Power = NULL,
                              randomB = "LV", quadratic = "LV",
                              control.start = (n.init = 10), jitter.var = 0.1)
beep()
#saveRDS(model36_moreLV_trout_nocopepodite, file = "Models/model36_moreLV_trout_nocopepodite.rds")
model36_moreLV_trout_nocopepodite <- readRDS("Models/model36_moreLV_trout_nocopepodite.rds")
plot(model36_moreLV_trout_nocopepodite)
summary(model36_moreLV_trout_nocopepodite)
#get residual correlations
par(mfrow = c(1, 1))
Theta <- getResidualCor(model36_moreLV_trout_nocopepodite)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#get correlations due to environment/covariates
Env <- getEnvironCor(model36_moreLV_trout_nocopepodite)
corrplot(Env[order.single(Env), order.single(Env)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model36_moreLV_trout_nocopepodite, "row.params.random"), Lake = names(coef(model36_moreLV_trout_nocopepodite, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model36_moreLV_trout_nocopepodite$params$sigma
#inspect the optima
optima(model36_moreLV_trout_nocopepodite, sd.errors = FALSE)
plot(optima(model36_moreLV_trout_nocopepodite, sd.errors = FALSE))
#inspect the tolerances
tolerances(model36_moreLV_trout_nocopepodite, sd.errors = FALSE)
plot(tolerances(model36_moreLV_trout_nocopepodite, sd.errors = FALSE))
#extract canonical coefficients
coef(model36_moreLV_trout_nocopepodite, parm="Cancoef")
#visualize species tolerances
model <- model36_moreLV_trout_nocopepodite
LVs = getLV(model)
newLV = cbind(LV1 = seq(min(LVs[,1]), max(LVs[,1]), length.out=143), LV2 = 0, LV3 = 0)
preds <- predict(model, type = "response", newLV = newLV)
plot(NA, ylim = range(preds), xlim = c(range(getLV(model))), ylab  = "Predicted response", xlab = "LV1")
segments(x0=optima(model, sd.errors = FALSE)[,1],x1 = optima(model, sd.errors = FALSE)[,1], y0 = rep(0, ncol(model$y)), y1 = apply(preds,2,max), col = "red", lty = "dashed", lwd = 2)
rug(getLV(model)[,1])
sapply(1:ncol(model$y), function(j)lines(sort(newLV[,1]), preds[order(newLV[,1]),j], lwd = 2))

newLV = cbind(LV1 = 0, LV3 = 0, LV2 =  seq(min(LVs[,2]), max(LVs[,2]), length.out=143))
preds <- predict(model, type = "response", newLV = newLV)
plot(NA, ylim = range(preds), xlim = c(range(getLV(model))), ylab  = "Predicted response", xlab = "LV2")
segments(x0=optima(model, sd.errors = FALSE)[,2],x1 = optima(model, sd.errors = FALSE)[,2], y0 = rep(0, ncol(model$y)), y1 = apply(preds,2,max), col = "red", lty = "dashed", lwd = 2)
rug(getLV(model)[,2])
sapply(1:ncol(model$y), function(j)lines(sort(newLV[,2]), preds[order(newLV[,2]),j], lwd = 2))

newLV = cbind(LV1 = 0, LV2 = 0, LV3 =  seq(min(LVs[,3]), max(LVs[,3]), length.out=143))
preds <- predict(model, type = "response", newLV = newLV)
plot(NA, ylim = range(preds), xlim = c(range(getLV(model))), ylab  = "Predicted response", xlab = "LV3")
segments(x0=optima(model, sd.errors = FALSE)[,3],x1 = optima(model, sd.errors = FALSE)[,3], y0 = rep(0, ncol(model$y)), y1 = apply(preds,2,max), col = "red", lty = "dashed", lwd = 2)
rug(getLV(model)[,3])
sapply(1:ncol(model$y), function(j)lines(sort(newLV[,3]), preds[order(newLV[,3]),j], lwd = 2))

# Extract the effects of predictors on the Latent Variables (RR axes)
env_on_lv <- model36_moreLV_trout_nocopepodite$params$LvXcoef

# To see how they contribute to the first RR axis
barplot(env_on_lv[, 1], main = "Impact of predictors on RR Axis 1")
barplot(env_on_lv[, 2], main = "Impact of predictors on RR Axis 2")
barplot(env_on_lv[, 3], main = "Impact of predictors on RR Axis 3")

#manual ordination guided by google gemini - CHECK THIS THOROUGHLY IF YOU WANT TO USE THIS PLOT
# 1. Get Site Scores (Where the lakes sit on the axes)
# For a model with lv.formula, these are the "predicted" latent variables
site_scores <- model36_moreLV_trout_nocopepodite$lvs

# 2. Get Species Scores (Where species sit on the axes)
# These are the "loadings" or weights
spec_scores <- model36_moreLV_trout_nocopepodite$params$theta[, 1:(model36_moreLV_trout_nocopepodite$num.lv + model36_moreLV_trout_nocopepodite$num.lv.c)]

# 3. Get the Environmental Arrows (The "Rotation" matrix)
# This replaces your missing coefplot!
env_arrows <- model36_moreLV_trout_nocopepodite$params$LvXcoef

library(ggrepel)

# Create a data frame for species
spec_df <- as.data.frame(spec_scores)
colnames(spec_df) <- c("Axis1", "Axis2", "Axis3")
spec_df$Species <- rownames(spec_df)

# Highlight your important species
spec_df$IsImportant <- ifelse(spec_df$Species == "Your_Species_Name", "Yes", "No")

# Plotting Axis 1 vs Axis 2 (The first two Environmental Axes)
ggplot() +
  # Draw the Sites as small light-grey dots
  geom_point(data = as.data.frame(site_scores), aes(x = LV1, y = LV2), color = "grey80", alpha = 0.5) +
  # Draw the Species as colored points
  geom_point(data = spec_df, aes(x = Axis1, y = Axis2, color = IsImportant), size = 3) +
  # Add Species Labels (only for the important one or the top responders)
  geom_text_repel(data = spec_df, aes(x = Axis1, y = Axis2, label = Species)) +
  # Add the Environmental Arrows
  geom_segment(data = as.data.frame(env_arrows), 
               aes(x = 0, y = 0, xend = CLV1*2, yend = CLV2*2), # Multiplied by 2 to make arrows visible
               arrow = arrow(length = unit(0.2, "cm")), color = "blue") +
  geom_text(data = as.data.frame(env_arrows), 
            aes(x = CLV1*2.2, y = CLV2*2.2, label = rownames(env_arrows)), color = "blue") +
  theme_minimal() +
  scale_color_manual(values = c("Yes" = "red", "No" = "black")) +
  labs(title = "Concurrent GLLVM Biplot", x = "Environmental Axis 1", y = "Environmental Axis 2")






model36_moreLV_trout_nocopepod <- gllvm(y = y_raw_trout_nocopepod, X = x_scale_trout, studyDesign = studyDesignData_trout,
                                           lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM,
                                           row.eff = ~(1|lake),
                                           num.RR = 3, num.lv = 3, family = "tweedie", Power = NULL,
                                           randomB = "LV", quadratic = "LV",
                                           control.start = (n.init = 10), jitter.var = 0.1)
beep()
#saveRDS(model36_moreLV_trout_nocopepod, file = "Models/model36_moreLV_trout_nocopepod.rds")
model36_moreLV_trout_nocopepod <- readRDS("Models/model36_moreLV_trout_nocopepod.rds")
plot(model36_moreLV_trout_nocopepod)
summary(model36_moreLV_trout_nocopepod)
#get residual correlations
par(mfrow = c(1, 1))
Theta <- getResidualCor(model36_moreLV_trout_nocopepod)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#get correlations due to environment/covariates
Env <- getEnvironCor(model36_moreLV_trout_nocopepod)
corrplot(Env[order.single(Env), order.single(Env)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model36_moreLV_trout_nocopepod, "row.params.random"), Lake = names(coef(model36_moreLV_trout_nocopepod, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model36_moreLV_trout_nocopepod$params$sigma
#inspect the optima
optima(model36_moreLV_trout_nocopepod, sd.errors = FALSE)
plot(optima(model36_moreLV_trout_nocopepod, sd.errors = FALSE))
#inspect the tolerances
tolerances(model36_moreLV_trout_nocopepod, sd.errors = FALSE)
plot(tolerances(model36_moreLV_trout_nocopepod, sd.errors = FALSE))
#extract canonical coefficients
coef(model36_moreLV_trout_nocopepod, parm="Cancoef")
#visualize species tolerances
model <- model36_moreLV_trout_nocopepod
LVs = getLV(model)
newLV = cbind(LV1 = seq(min(LVs[,1]), max(LVs[,1]), length.out=143), LV2 = 0, LV3 = 0)
preds <- predict(model, type = "response", newLV = newLV)
plot(NA, ylim = range(preds), xlim = c(range(getLV(model))), ylab  = "Predicted response", xlab = "LV1")
segments(x0=optima(model, sd.errors = FALSE)[,1],x1 = optima(model, sd.errors = FALSE)[,1], y0 = rep(0, ncol(model$y)), y1 = apply(preds,2,max), col = "red", lty = "dashed", lwd = 2)
rug(getLV(model)[,1])
sapply(1:ncol(model$y), function(j)lines(sort(newLV[,1]), preds[order(newLV[,1]),j], lwd = 2))

newLV = cbind(LV1 = 0, LV3 = 0, LV2 =  seq(min(LVs[,2]), max(LVs[,2]), length.out=143))
preds <- predict(model, type = "response", newLV = newLV)
plot(NA, ylim = range(preds), xlim = c(range(getLV(model))), ylab  = "Predicted response", xlab = "LV2")
segments(x0=optima(model, sd.errors = FALSE)[,2],x1 = optima(model, sd.errors = FALSE)[,2], y0 = rep(0, ncol(model$y)), y1 = apply(preds,2,max), col = "red", lty = "dashed", lwd = 2)
rug(getLV(model)[,2])
sapply(1:ncol(model$y), function(j)lines(sort(newLV[,2]), preds[order(newLV[,2]),j], lwd = 2))

newLV = cbind(LV1 = 0, LV2 = 0, LV3 =  seq(min(LVs[,3]), max(LVs[,3]), length.out=143))
preds <- predict(model, type = "response", newLV = newLV)
plot(NA, ylim = range(preds), xlim = c(range(getLV(model))), ylab  = "Predicted response", xlab = "LV3")
segments(x0=optima(model, sd.errors = FALSE)[,3],x1 = optima(model, sd.errors = FALSE)[,3], y0 = rep(0, ncol(model$y)), y1 = apply(preds,2,max), col = "red", lty = "dashed", lwd = 2)
rug(getLV(model)[,3])
sapply(1:ncol(model$y), function(j)lines(sort(newLV[,3]), preds[order(newLV[,3]),j], lwd = 2))

# Extract the effects of predictors on the Latent Variables (RR axes)
env_on_lv <- model36_moreLV_trout_nocopepod$params$LvXcoef

# To see how they contribute to the first RR axis
barplot(env_on_lv[, 1], main = "Impact of predictors on RR Axis 1")
barplot(env_on_lv[, 2], main = "Impact of predictors on RR Axis 2")
barplot(env_on_lv[, 3], main = "Impact of predictors on RR Axis 3")

#manual ordination guided by google gemini - CHECK THIS THOROUGHLY IF YOU WANT TO USE THIS PLOT
# 1. Get Site Scores (Where the lakes sit on the axes)
# For a model with lv.formula, these are the "predicted" latent variables
site_scores <- model36_moreLV_trout_nocopepod$lvs

# 2. Get Species Scores (Where species sit on the axes)
# These are the "loadings" or weights
spec_scores <- model36_moreLV_trout_nocopepod$params$theta[, 1:(model36_moreLV_trout_nocopepod$num.lv + model36_moreLV_trout_nocopepod$num.lv.c)]

# 3. Get the Environmental Arrows (The "Rotation" matrix)
# This replaces your missing coefplot!
env_arrows <- model36_moreLV_trout_nocopepod$params$LvXcoef



# Create a data frame for species
spec_df <- as.data.frame(spec_scores)
colnames(spec_df) <- c("Axis1", "Axis2", "Axis3")
spec_df$Species <- rownames(spec_df)

# Highlight your important species
spec_df$IsImportant <- ifelse(spec_df$Species == "Your_Species_Name", "Yes", "No")

# Plotting Axis 1 vs Axis 2 (The first two Environmental Axes)
ggplot() +
  # Draw the Sites as small light-grey dots
  geom_point(data = as.data.frame(site_scores), aes(x = LV1, y = LV2), color = "grey80", alpha = 0.5) +
  # Draw the Species as colored points
  geom_point(data = spec_df, aes(x = Axis1, y = Axis2, color = IsImportant), size = 3) +
  # Add Species Labels (only for the important one or the top responders)
  geom_text_repel(data = spec_df, aes(x = Axis1, y = Axis2, label = Species)) +
  # Add the Environmental Arrows
  geom_segment(data = as.data.frame(env_arrows), 
               aes(x = 0, y = 0, xend = CLV1*2, yend = CLV2*2), # Multiplied by 2 to make arrows visible
               arrow = arrow(length = unit(0.2, "cm")), color = "blue") +
  geom_text(data = as.data.frame(env_arrows), 
            aes(x = CLV1*2.2, y = CLV2*2.2, label = rownames(env_arrows)), color = "blue") +
  theme_minimal() +
  scale_color_manual(values = c("Yes" = "red", "No" = "black")) +
  labs(title = "Concurrent GLLVM Biplot", x = "Environmental Axis 1", y = "Environmental Axis 2")



#THIS IS THE ONE--------------------------------------------------------------
#make it 4 LVs
model36_moreLV_trout_nocopepod3RR4LV <- gllvm(y = y_raw_trout_nocopepod, X = x_scale_trout, studyDesign = studyDesignData_trout,
                                        lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM,
                                        row.eff = ~(1|lake),
                                        num.RR = 3, num.lv = 4, family = "tweedie", Power = NULL,
                                        randomB = "LV", quadratic = "LV",
                                        control.start = (n.init = 10), jitter.var = 0.1)
beep()
#saveRDS(model36_moreLV_trout_nocopepod3RR4LV, file = "Models/model36_moreLV_trout_nocopepod3RR4LV.rds")
model36_moreLV_trout_nocopepod3RR4LV <- readRDS("Models/model36_moreLV_trout_nocopepod3RR4LV.rds")
plot(model36_moreLV_trout_nocopepod3RR4LV)
summary(model36_moreLV_trout_nocopepod3RR4LV)
#get residual correlations
par(mfrow = c(1, 1))
Theta <- getResidualCor(model36_moreLV_trout_nocopepod3RR4LV)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#get correlations due to environment/covariates
Env <- getEnvironCor(model36_moreLV_trout_nocopepod3RR4LV)
corrplot(Env[order.single(Env), order.single(Env)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model36_moreLV_trout_nocopepod3RR4LV, "row.params.random"), Lake = names(coef(model36_moreLV_trout_nocopepod3RR4LV, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model36_moreLV_trout_nocopepod3RR4LV$params$sigma
#inspect the optima
optima(model36_moreLV_trout_nocopepod3RR4LV, sd.errors = FALSE)
plot(optima(model36_moreLV_trout_nocopepod3RR4LV, sd.errors = FALSE))
#inspect the tolerances
tolerances(model36_moreLV_trout_nocopepod3RR4LV, sd.errors = FALSE)
plot(tolerances(model36_moreLV_trout_nocopepod3RR4LV, sd.errors = FALSE))
#extract canonical coefficients
coef(model36_moreLV_trout_nocopepod3RR4LV, parm="Cancoef")
#visualize species tolerances
model <- model36_moreLV_trout_nocopepod3RR4LV
LVs = getLV(model)
newLV = cbind(LV1 = seq(min(LVs[,1]), max(LVs[,1]), length.out=143), LV2 = 0, LV3 = 0)
preds <- predict(model, type = "response", newLV = newLV)
plot(NA, ylim = range(preds), xlim = c(range(getLV(model))), ylab  = "Predicted response", xlab = "LV1")
segments(x0=optima(model, sd.errors = FALSE)[,1],x1 = optima(model, sd.errors = FALSE)[,1], y0 = rep(0, ncol(model$y)), y1 = apply(preds,2,max), col = "red", lty = "dashed", lwd = 2)
rug(getLV(model)[,1])
sapply(1:ncol(model$y), function(j)lines(sort(newLV[,1]), preds[order(newLV[,1]),j], lwd = 2))

newLV = cbind(LV1 = 0, LV3 = 0, LV2 =  seq(min(LVs[,2]), max(LVs[,2]), length.out=143))
preds <- predict(model, type = "response", newLV = newLV)
plot(NA, ylim = range(preds), xlim = c(range(getLV(model))), ylab  = "Predicted response", xlab = "LV2")
segments(x0=optima(model, sd.errors = FALSE)[,2],x1 = optima(model, sd.errors = FALSE)[,2], y0 = rep(0, ncol(model$y)), y1 = apply(preds,2,max), col = "red", lty = "dashed", lwd = 2)
rug(getLV(model)[,2])
sapply(1:ncol(model$y), function(j)lines(sort(newLV[,2]), preds[order(newLV[,2]),j], lwd = 2))

newLV = cbind(LV1 = 0, LV2 = 0, LV3 =  seq(min(LVs[,3]), max(LVs[,3]), length.out=143))
preds <- predict(model, type = "response", newLV = newLV)
plot(NA, ylim = range(preds), xlim = c(range(getLV(model))), ylab  = "Predicted response", xlab = "LV3")
segments(x0=optima(model, sd.errors = FALSE)[,3],x1 = optima(model, sd.errors = FALSE)[,3], y0 = rep(0, ncol(model$y)), y1 = apply(preds,2,max), col = "red", lty = "dashed", lwd = 2)
rug(getLV(model)[,3])
sapply(1:ncol(model$y), function(j)lines(sort(newLV[,3]), preds[order(newLV[,3]),j], lwd = 2))

# Extract the effects of predictors on the Latent Variables (RR axes)
env_on_lv <- model36_moreLV_trout_nocopepod3RR4LV$params$LvXcoef

# To see how they contribute to the first RR axis
barplot(env_on_lv[, 1], main = "Impact of predictors on RR Axis 1")
barplot(env_on_lv[, 2], main = "Impact of predictors on RR Axis 2")
barplot(env_on_lv[, 3], main = "Impact of predictors on RR Axis 3")

#manual ordination guided by google gemini - CHECK THIS THOROUGHLY IF YOU WANT TO USE THIS PLOT
# 1. Get Site Scores (Where the lakes sit on the axes)
# For a model with lv.formula, these are the "predicted" latent variables
site_scores <- model36_moreLV_trout_nocopepod3RR4LV$lvs

# 2. Get Species Scores (Where species sit on the axes)
# These are the "loadings" or weights
spec_scores <- model36_moreLV_trout_nocopepod3RR4LV$params$theta[, 1:(model36_moreLV_trout_nocopepod3RR4LV$num.lv + model36_moreLV_trout_nocopepod3RR4LV$num.lv.c)]

# 3. Get the Environmental Arrows (The "Rotation" matrix)
# This replaces your missing coefplot!
env_arrows <- model36_moreLV_trout_nocopepod3RR4LV$params$LvXcoef

# Create a data frame for species
spec_df <- as.data.frame(spec_scores)
colnames(spec_df) <- c("Axis1", "Axis2", "Axis3")
spec_df$Species <- rownames(spec_df)

# Highlight your important species
spec_df$IsImportant <- ifelse(spec_df$Species == "Your_Species_Name", "Yes", "No")

# Plotting Axis 1 vs Axis 2 (The first two Environmental Axes)
ggplot() +
  # Draw the Sites as small light-grey dots
  geom_point(data = as.data.frame(site_scores), aes(x = LV1, y = LV2), color = "grey80", alpha = 0.5) +
  # Draw the Species as colored points
  geom_point(data = spec_df, aes(x = Axis1, y = Axis2, color = IsImportant), size = 3) +
  # Add Species Labels (only for the important one or the top responders)
  geom_text_repel(data = spec_df, aes(x = Axis1, y = Axis2, label = Species)) +
  # Add the Environmental Arrows
  geom_segment(data = as.data.frame(env_arrows), 
               aes(x = 0, y = 0, xend = CLV1*2, yend = CLV2*2), # Multiplied by 2 to make arrows visible
               arrow = arrow(length = unit(0.2, "cm")), color = "blue") +
  geom_text(data = as.data.frame(env_arrows), 
            aes(x = CLV1*2.2, y = CLV2*2.2, label = rownames(env_arrows)), color = "blue") +
  theme_minimal() +
  scale_color_manual(values = c("Yes" = "red", "No" = "black")) +
  labs(title = "Concurrent GLLVM Biplot", x = "Environmental Axis 1", y = "Environmental Axis 2")




#make it 5 LVs
model36_moreLV_trout_nocopepod3RR5LV <- gllvm(y = y_raw_trout_nocopepod, X = x_scale_trout, studyDesign = studyDesignData_trout,
                                              lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM,
                                              row.eff = ~(1|lake),
                                              num.RR = 3, num.lv = 5, family = "tweedie", Power = NULL,
                                              randomB = "LV", quadratic = "LV",
                                              control.start = (n.init = 10), jitter.var = 0.1)
beep()
#saveRDS(model36_moreLV_trout_nocopepod3RR5LV, file = "Models/model36_moreLV_trout_nocopepod3RR5LV.rds")
model36_moreLV_trout_nocopepod3RR5LV <- readRDS("Models/model36_moreLV_trout_nocopepod3RR5LV.rds")
plot(model36_moreLV_trout_nocopepod3RR5LV)
summary(model36_moreLV_trout_nocopepod3RR5LV)
#get residual correlations
par(mfrow = c(1, 1))
Theta <- getResidualCor(model36_moreLV_trout_nocopepod3RR5LV)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#get correlations due to environment/covariates
Env <- getEnvironCor(model36_moreLV_trout_nocopepod3RR5LV)
corrplot(Env[order.single(Env), order.single(Env)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model36_moreLV_trout_nocopepod3RR5LV, "row.params.random"), Lake = names(coef(model36_moreLV_trout_nocopepod3RR5LV, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model36_moreLV_trout_nocopepod3RR5LV$params$sigma
#inspect the optima
optima(model36_moreLV_trout_nocopepod3RR5LV, sd.errors = FALSE)
plot(optima(model36_moreLV_trout_nocopepod3RR5LV, sd.errors = FALSE))
#inspect the tolerances
tolerances(model36_moreLV_trout_nocopepod3RR5LV, sd.errors = FALSE)
plot(tolerances(model36_moreLV_trout_nocopepod3RR5LV, sd.errors = FALSE))
#extract canonical coefficients
coef(model36_moreLV_trout_nocopepod3RR5LV, parm="Cancoef")

# Extract the effects of predictors on the Latent Variables (RR axes)
env_on_lv <- model36_moreLV_trout_nocopepod3RR5LV$params$LvXcoef

# To see how they contribute to the first RR axis
barplot(env_on_lv[, 1], main = "Impact of predictors on RR Axis 1")
barplot(env_on_lv[, 2], main = "Impact of predictors on RR Axis 2")
barplot(env_on_lv[, 3], main = "Impact of predictors on RR Axis 3")

#manual ordination guided by google gemini - CHECK THIS THOROUGHLY IF YOU WANT TO USE THIS PLOT
# 1. Get Site Scores (Where the lakes sit on the axes)
# For a model with lv.formula, these are the "predicted" latent variables
site_scores <- model36_moreLV_trout_nocopepod3RR5LV$lvs

# 2. Get Species Scores (Where species sit on the axes)
# These are the "loadings" or weights
spec_scores <- model36_moreLV_trout_nocopepod3RR5LV$params$theta[, 1:(model36_moreLV_trout_nocopepod3RR5LV$num.lv + model36_moreLV_trout_nocopepod3RR5LV$num.lv.c)]

# 3. Get the Environmental Arrows (The "Rotation" matrix)
# This replaces your missing coefplot!
env_arrows <- model36_moreLV_trout_nocopepod3RR5LV$params$LvXcoef

# Create a data frame for species
spec_df <- as.data.frame(spec_scores)
colnames(spec_df) <- c("Axis1", "Axis2", "Axis3")
spec_df$Species <- rownames(spec_df)

# Highlight your important species
spec_df$IsImportant <- ifelse(spec_df$Species == "Your_Species_Name", "Yes", "No")

# Plotting Axis 1 vs Axis 2 (The first two Environmental Axes)
ggplot() +
  # Draw the Sites as small light-grey dots
  geom_point(data = as.data.frame(site_scores), aes(x = LV1, y = LV2), color = "grey80", alpha = 0.5) +
  # Draw the Species as colored points
  geom_point(data = spec_df, aes(x = Axis1, y = Axis2, color = IsImportant), size = 3) +
  # Add Species Labels (only for the important one or the top responders)
  geom_text_repel(data = spec_df, aes(x = Axis1, y = Axis2, label = Species)) +
  # Add the Environmental Arrows
  geom_segment(data = as.data.frame(env_arrows), 
               aes(x = 0, y = 0, xend = CLV1*2, yend = CLV2*2), # Multiplied by 2 to make arrows visible
               arrow = arrow(length = unit(0.2, "cm")), color = "blue") +
  geom_text(data = as.data.frame(env_arrows), 
            aes(x = CLV1*2.2, y = CLV2*2.2, label = rownames(env_arrows)), color = "blue") +
  theme_minimal() +
  scale_color_manual(values = c("Yes" = "red", "No" = "black")) +
  labs(title = "Concurrent GLLVM Biplot", x = "Environmental Axis 1", y = "Environmental Axis 2")




#make it 4 LVs and 2 RR
model36_moreLV_trout_nocopepod2RR4LV <- gllvm(y = y_raw_trout_nocopepod, X = x_scale_trout, studyDesign = studyDesignData_trout,
                                              lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM,
                                              row.eff = ~(1|lake),
                                              num.RR = 2, num.lv = 4, family = "tweedie", Power = NULL,
                                              randomB = "LV", quadratic = "LV",
                                              control.start = (n.init = 10), jitter.var = 0.1)
beep()
#saveRDS(model36_moreLV_trout_nocopepod2RR4LV, file = "Models/model36_moreLV_trout_nocopepod2RR4LV.rds")
model36_moreLV_trout_nocopepod2RR4LV <- readRDS("Models/model36_moreLV_trout_nocopepod2RR4LV.rds")
plot(model36_moreLV_trout_nocopepod2RR4LV)
summary(model36_moreLV_trout_nocopepod2RR4LV)
#get residual correlations
par(mfrow = c(1, 1))
Theta <- getResidualCor(model36_moreLV_trout_nocopepod2RR4LV)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#get correlations due to environment/covariates
Env <- getEnvironCor(model36_moreLV_trout_nocopepod2RR4LV)
corrplot(Env[order.single(Env), order.single(Env)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model36_moreLV_trout_nocopepod2RR4LV, "row.params.random"), Lake = names(coef(model36_moreLV_trout_nocopepod2RR4LV, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model36_moreLV_trout_nocopepod2RR4LV$params$sigma
#inspect the optima
optima(model36_moreLV_trout_nocopepod2RR4LV, sd.errors = FALSE)
plot(optima(model36_moreLV_trout_nocopepod2RR4LV, sd.errors = FALSE))
#inspect the tolerances
tolerances(model36_moreLV_trout_nocopepod2RR4LV, sd.errors = FALSE)
plot(tolerances(model36_moreLV_trout_nocopepod2RR4LV, sd.errors = FALSE))
#extract canonical coefficients
coef(model36_moreLV_trout_nocopepod2RR4LV, parm="Cancoef")

# Extract the effects of predictors on the Latent Variables (RR axes)
env_on_lv <- model36_moreLV_trout_nocopepod2RR4LV$params$LvXcoef

# To see how they contribute to the first RR axis
barplot(env_on_lv[, 1], main = "Impact of predictors on RR Axis 1")
barplot(env_on_lv[, 2], main = "Impact of predictors on RR Axis 2")
barplot(env_on_lv[, 3], main = "Impact of predictors on RR Axis 3")

#manual ordination guided by google gemini - CHECK THIS THOROUGHLY IF YOU WANT TO USE THIS PLOT
# 1. Get Site Scores (Where the lakes sit on the axes)
# For a model with lv.formula, these are the "predicted" latent variables
site_scores <- model36_moreLV_trout_nocopepod2RR4LV$lvs

# 2. Get Species Scores (Where species sit on the axes)
# These are the "loadings" or weights
spec_scores <- model36_moreLV_trout_nocopepod2RR4LV$params$theta[, 1:(model36_moreLV_trout_nocopepod2RR4LV$num.lv + model36_moreLV_trout_nocopepod2RR4LV$num.lv.c)]

# 3. Get the Environmental Arrows (The "Rotation" matrix)
# This replaces your missing coefplot!
env_arrows <- model36_moreLV_trout_nocopepod2RR4LV$params$LvXcoef

# Create a data frame for species
spec_df <- as.data.frame(spec_scores)
colnames(spec_df) <- c("Axis1", "Axis2", "Axis3")
spec_df$Species <- rownames(spec_df)

# Highlight your important species
spec_df$IsImportant <- ifelse(spec_df$Species == "Your_Species_Name", "Yes", "No")

# Plotting Axis 1 vs Axis 2 (The first two Environmental Axes)
ggplot() +
  # Draw the Sites as small light-grey dots
  geom_point(data = as.data.frame(site_scores), aes(x = LV1, y = LV2), color = "grey80", alpha = 0.5) +
  # Draw the Species as colored points
  geom_point(data = spec_df, aes(x = Axis1, y = Axis2, color = IsImportant), size = 3) +
  # Add Species Labels (only for the important one or the top responders)
  geom_text_repel(data = spec_df, aes(x = Axis1, y = Axis2, label = Species)) +
  # Add the Environmental Arrows
  geom_segment(data = as.data.frame(env_arrows), 
               aes(x = 0, y = 0, xend = CLV1*2, yend = CLV2*2), # Multiplied by 2 to make arrows visible
               arrow = arrow(length = unit(0.2, "cm")), color = "blue") +
  geom_text(data = as.data.frame(env_arrows), 
            aes(x = CLV1*2.2, y = CLV2*2.2, label = rownames(env_arrows)), color = "blue") +
  theme_minimal() +
  scale_color_manual(values = c("Yes" = "red", "No" = "black")) +
  labs(title = "Concurrent GLLVM Biplot", x = "Environmental Axis 1", y = "Environmental Axis 2")


#make it 4 LVs and 4 RR
model36_moreLV_trout_nocopepod4RR4LV <- gllvm(y = y_raw_trout_nocopepod, X = x_scale_trout, studyDesign = studyDesignData_trout,
                                              lv.formula = ~ CDOM + Area + Max_Depth + Secchi + GDD + Photic + SWF + ZM,
                                              row.eff = ~(1|lake),
                                              num.RR = 4, num.lv = 4, family = "tweedie", Power = NULL,
                                              randomB = "LV", quadratic = "LV",
                                              control.start = (n.init = 10), jitter.var = 0.1)
beep()
#saveRDS(model36_moreLV_trout_nocopepod4RR4LV, file = "Models/model36_moreLV_trout_nocopepod4RR4LV.rds")
model36_moreLV_trout_nocopepod4RR4LV <- readRDS("Models/model36_moreLV_trout_nocopepod4RR4LV.rds")
plot(model36_moreLV_trout_nocopepod4RR4LV)
summary(model36_moreLV_trout_nocopepod4RR4LV)
#get residual correlations
par(mfrow = c(1, 1))
Theta <- getResidualCor(model36_moreLV_trout_nocopepod4RR4LV)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#standardized order for comparison
corrplot(Theta,
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#get correlations due to environment/covariates
Env <- getEnvironCor(model36_moreLV_trout_nocopepod4RR4LV)
corrplot(Env[order.single(Env), order.single(Env)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#collect and plot random effects
rand.lake <- data.frame(Rand_Effect_Value = coef(model36_moreLV_trout_nocopepod4RR4LV, "row.params.random"), Lake = names(coef(model36_moreLV_trout_nocopepod4RR4LV, "row.params.random")))
ggplot(data = rand.lake, aes(x = Lake, y = Rand_Effect_Value))+
  geom_point()+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
#look at sigma of random effects
model36_moreLV_trout_nocopepod4RR4LV$params$sigma
#inspect the optima
optima(model36_moreLV_trout_nocopepod4RR4LV, sd.errors = FALSE)
plot(optima(model36_moreLV_trout_nocopepod4RR4LV, sd.errors = FALSE))
#inspect the tolerances
tolerances(model36_moreLV_trout_nocopepod4RR4LV, sd.errors = FALSE)
plot(tolerances(model36_moreLV_trout_nocopepod4RR4LV, sd.errors = FALSE))
#extract canonical coefficients
coef(model36_moreLV_trout_nocopepod4RR4LV, parm="Cancoef")

# Extract the effects of predictors on the Latent Variables (RR axes)
env_on_lv <- model36_moreLV_trout_nocopepod4RR4LV$params$LvXcoef

# To see how they contribute to the first RR axis
barplot(env_on_lv[, 1], main = "Impact of predictors on RR Axis 1")
barplot(env_on_lv[, 2], main = "Impact of predictors on RR Axis 2")
barplot(env_on_lv[, 3], main = "Impact of predictors on RR Axis 3")
barplot(env_on_lv[, 4], main = "Impact of predictors on RR Axis 4")

#manual ordination guided by google gemini - CHECK THIS THOROUGHLY IF YOU WANT TO USE THIS PLOT
# 1. Get Site Scores (Where the lakes sit on the axes)
# For a model with lv.formula, these are the "predicted" latent variables
site_scores <- model36_moreLV_trout_nocopepod4RR4LV$lvs

# 2. Get Species Scores (Where species sit on the axes)
# These are the "loadings" or weights
spec_scores <- model36_moreLV_trout_nocopepod4RR4LV$params$theta[, 1:(model36_moreLV_trout_nocopepod4RR4LV$num.lv + model36_moreLV_trout_nocopepod4RR4LV$num.lv.c)]

# 3. Get the Environmental Arrows (The "Rotation" matrix)
# This replaces your missing coefplot!
env_arrows <- model36_moreLV_trout_nocopepod4RR4LV$params$LvXcoef

# Create a data frame for species
spec_df <- as.data.frame(spec_scores)
colnames(spec_df) <- c("Axis1", "Axis2", "Axis3")
spec_df$Species <- rownames(spec_df)

# Highlight your important species
spec_df$IsImportant <- ifelse(spec_df$Species == "Your_Species_Name", "Yes", "No")

# Plotting Axis 1 vs Axis 2 (The first two Environmental Axes)
ggplot() +
  # Draw the Sites as small light-grey dots
  geom_point(data = as.data.frame(site_scores), aes(x = LV1, y = LV2), color = "grey80", alpha = 0.5) +
  # Draw the Species as colored points
  geom_point(data = spec_df, aes(x = Axis1, y = Axis2, color = IsImportant), size = 3) +
  # Add Species Labels (only for the important one or the top responders)
  geom_text_repel(data = spec_df, aes(x = Axis1, y = Axis2, label = Species)) +
  # Add the Environmental Arrows
  geom_segment(data = as.data.frame(env_arrows), 
               aes(x = 0, y = 0, xend = CLV1*2, yend = CLV2*2), # Multiplied by 2 to make arrows visible
               arrow = arrow(length = unit(0.2, "cm")), color = "blue") +
  geom_text(data = as.data.frame(env_arrows), 
            aes(x = CLV1*2.2, y = CLV2*2.2, label = rownames(env_arrows)), color = "blue") +
  theme_minimal() +
  scale_color_manual(values = c("Yes" = "red", "No" = "black")) +
  labs(title = "Concurrent GLLVM Biplot", x = "Environmental Axis 1", y = "Environmental Axis 2")


#COMPARE AICc and BIC between 4 and 5 LVs:
AICc(model36_moreLV_trout_nocopepod3RR5LV)
AICc(model36_moreLV_trout_nocopepod3RR4LV)
BIC(model36_moreLV_trout_nocopepod3RR5LV)
BIC(model36_moreLV_trout_nocopepod3RR4LV)


#WE FOUND THE WINNER!!!!!!!!!!


