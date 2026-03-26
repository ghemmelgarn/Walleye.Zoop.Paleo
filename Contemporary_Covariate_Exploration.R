#This script takes the contemporary model dataset and explores covariates to help us choose which ones to put in the model
#Then it the second part it takes a deeper look at the chosen covariates


library(tidyverse)
library(ggplot2)
library(GGally) #for ggpairs function
library(gridExtra) #to export multiple plots together as .tiff files
library(corrplot) #to make corrollelograms
library(factoextra) #for pca biplot


#Import dataset
data <- read.csv("Data/Input/Contemporary_Dataset_2026_02_18.csv")

#remove the fish and zoop data for now and literally everything that isn't a covariate
data.cov <- data %>% 
  select(lake_name:stock.yn, -lagoslakeid, -lagoslakeid_verm, -nhdhr_id)

#make for everything:
#ggpairs
#corrollelogram
#density plots


#Everything all together-----------------------------------------------------------------------------------------------------------------------

#isolate just quantitative covariates
all.quant <- data.cov %>% 
  select(-lake_name, -Year, -parentdow,
         -remote.secchi.year, -cond_year, - lake_onlandborder, -lake_shapeflag, -lake_lat_decdeg, -lake_lon_decdeg, -lake_connectivity_class,
         -perimeter_m, #this was never actually intended to be a covariate
         -Polygon_Source, -total_pixels,
         -SpinyWaterflea.inv.year, -SpinyWaterflea.yn, -ZebraMussel.inv.year, -ZebraMussel.yn, -stock.yn)

#ggpairs
tiff("All Quantitative Pairs.tiff", width = 12, height = 12, units = "in", res = 300)
ggpairs(all.quant)
dev.off()

#corrollelogram
all.cor <- cor(all.quant, use = "pairwise.complete.obs")
tiff("All Quantitative Correlations.tiff", width = 12, height = 12, units = "in", res = 300)
corrplot(all.cor,
         method = "circle",
         type = "lower",
         tl.col = "black",
         tl.cex = 1)
dev.off()

#density plots
#pivot longer
all.long <- all.quant %>%
  pivot_longer(cols = everything(), 
               names_to = "covariate", 
               values_to = "value")
#plot
tiff("All Quantitative Distributions.tiff", width = 12, height = 12, units = "in", res = 300)
ggplot(all.long, aes(x = value)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  facet_wrap(~covariate, scales = "free") + # "free" allows each plot its own x/y axis
  theme_classic() +
  labs(title = "Density Distribution of All Quantitative Covariates",
       x = "Value",
       y = "Density")
dev.off()


#Do a PCA on all the quantitative stuff

pca_results <- prcomp(na.omit(all.quant %>% select(where(is.numeric))), 
                      center = TRUE, 
                      scale. = TRUE)
summary(pca_results)

#biplot
tiff("PC1vPC2.tiff", width = 6, height = 6, units = "in", res = 300)
fviz_pca_biplot(pca_results, 
                repel = TRUE,            # Avoids overlapping text
                col.var = "red",         # Color for variables
                col.ind = "steelblue")+  # Color for lakes
  scale_y_continuous(limits = c(-6, 6)) +
  scale_x_continuous(limits = c(-6, 6)) +
  labs(title = "PC1 v PC2")
dev.off()

tiff("PC2vPC3.tiff", width = 6, height = 6, units = "in", res = 300)
fviz_pca_biplot(pca_results, 
                axes = c(2,3),
                repel = TRUE,            # Avoids overlapping text
                col.var = "red",         # Color for variables
                col.ind = "steelblue")+  # Color for lakes
  scale_y_continuous(limits = c(-6, 6)) +
  scale_x_continuous(limits = c(-6, 6)) +
  labs(title = "PC2 v PC3")
dev.off()

tiff("PC1vPC3.tiff", width = 6, height = 6, units = "in", res = 300)
fviz_pca_biplot(pca_results, 
                axes = c(1,3),
                repel = TRUE,            # Avoids overlapping text
                col.var = "red",         # Color for variables
                col.ind = "steelblue")+  # Color for lakes
  scale_y_continuous(limits = c(-6, 6)) +
  scale_x_continuous(limits = c(-6, 6)) +
  labs(title = "PC1 v PC3")
dev.off()


#COMPARE PRIMARY PRODUCTIVITY METRICS----------------------------------------------------------------------


#Isolate all productivity metrics, including the different secchi measurements too
prod.test <- data.cov %>%
  select(remote.secchi.exact.year,
         remote.secchi.closest.year,
         secchi.meters.MPCA.Jun.to.Aug,
         secchi.meters.MPCA.Jul.to.Sept,
         cond_uscm_exact_year,
         cond_uscm_closest_year,
         remote.chla,
         remote.CDOM
  )
 #label the variables
custom_labels_prod <- c("Secchi Remote Exact Year" = "remote.secchi.exact.year",
                        "Secchi Remote Closest Year" = "remote.secchi.closest.year",
                        "Secchi MPCA Jun-Aug" = "secchi.meters.MPCA.Jun.to.Aug",
                        "Secchi MPCA Jul-Sept" = "secchi.meters.MPCA.Jul.to.Sept",
                        "Conductivity Exact Year" = "cond_uscm_exact_year",
                        "Conductivity Closest Year" = "cond_uscm_closest_year",
                        "Chlorophyll-a Remote" = "remote.chla",
                        "CDOM Remote" = "remote.CDOM"
)

tiff("Primary Productivity Pairs.tiff", width = 12, height = 12, units = "in", res = 300)
ggpairs(prod.test, columnLabels = custom_labels_prod)
dev.off()


#corrollelogram
prod.cor <- cor(prod.test, use = "pairwise.complete.obs")
tiff("Primary Productivity Correlations.tiff", width = 12, height = 12, units = "in", res = 300)
corrplot(prod.cor,
         method = "circle",
         type = "lower",
         tl.col = "black",
         tl.cex = 1)
dev.off()


#Commented out because I don't need to run this to make the plots, already have it summarized in a table

# #How many lake-years and lakes do I have for the other potential primary productivity metrics?
# #Chlorophyll-a
# nrow(filter(Join10.final,  !is.na(remote.chla)))
# #lake-years = 89
# length(unique(filter(Join10.final, !is.na(remote.chla))$lake_name))
# #lakes = 32
# 
# #CDOM
# nrow(filter(Join10.final, !is.na(remote.CDOM)))
# #lake-years = 89
# length(unique(filter(Join10.final, !is.na(remote.CDOM))$lake_name))
# #lakes = 32
# 
# #Conductivity exact year
# nrow(filter(Join10.final, !is.na(cond_uscm_exact_year)))
# #lake-years = 87
# length(unique(filter(Join10.final, !is.na(cond_uscm_exact_year))$lake_name))
# #lakes = 26
# 
# #Conductivity closest year
# nrow(filter(Join10.final, !is.na(cond_uscm_closest_year)))
# #lake-years = 198
# length(unique(filter(Join10.final, !is.na(cond_uscm_closest_year))$lake_name))
# #lakes = 40
# 


#Temperature------------------------------------------
#just get a density plot, only one variable

#plot
tiff("GDD Distribution.tiff", width = 6, height = 6, units = "in", res = 300)
ggplot(data.cov, aes(x = gdd.year.5c)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  theme_classic() +
  labs(title = "Density Distribution of Growing Degree Days",
       x = "GDD",
       y = "Density")
dev.off()


#Lake Morphometry / Habitat--------------------------------------------------


#Isolate all morphology metrics, including the different secchi measurements too
morph <- data.cov %>%
  select(area_ha,
         SDI,
         depth.max.m,
         depth.mean.m,
         vol.dev,
         dynam.ratio,
         lake_elevation_m,
         photic_prop_remote.secchi.exact.year.,
         photic_prop_remote.secchi.closest.year,
         photic_prop_secchi.meters.MPCA.Jun.to.Aug,
         photic_prop_secchi.meters.MPCA.Jul.to.Sept
  )


tiff("Morphology Pairs.tiff", width = 12, height = 12, units = "in", res = 300)
ggpairs(morph)
dev.off()


#corrollelogram
morph.cor <- cor(morph, use = "pairwise.complete.obs")
tiff("Morphology Correlations.tiff", width = 12, height = 12, units = "in", res = 300)
corrplot(morph.cor,
         method = "circle",
         type = "lower",
         tl.col = "black",
         tl.cex = 1)
dev.off()

#plot drainage connectivity
tiff("Drainage Connectivity.tiff", width = 6, height = 6, units = "in", res = 300)
ggplot(data.cov, aes(x = lake_connectivity_class)) +
  geom_bar(alpha = 0.5) +
  theme_classic() +
  labs(x = "Connectivity Class",
       y = "Count")
dev.off()



#Annual precip--------------------------------------

#just get a density plot, only one variable

#plot
tiff("Precip Distribution.tiff", width = 6, height = 6, units = "in", res = 300)
ggplot(data.cov, aes(x = precip_10yr_avg_mm)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  theme_classic() +
  labs(title = "Density Distribution of Annual Precipitation",
       x = "Mean Annual Precip (mm)",
       y = "Density")
dev.off()



#Invasive Species--------------------------------

#density plots for years since invasion,
#pivot longer
IV.long <- data.cov %>%
  pivot_longer(cols = c(ZebraMussel.ysi, SpinyWaterFlea.ysi), 
               names_to = "species", 
               values_to = "YearsSinceInvasion") %>% 
  mutate(species = ifelse(species == "ZebraMussel.ysi", "Zebra Mussel", "Spiny Water Flea"))

#plot
tiff("Inv Spp YSI Distribution.tiff", width = 6, height = 6, units = "in", res = 300)
ggplot(IV.long, aes(x = YearsSinceInvasion, fill = species, color = species)) +
  geom_density(alpha = 0.5) +
  theme_classic() +
  labs(title = "Density Distribution of Years Since Invasion",
       x = "Years since Invasion",
       y = "Density")
dev.off()


#bar chart for y/n
#pivot longer
IV.yn.long <- data.cov %>%
  pivot_longer(cols = c(ZebraMussel.yn, SpinyWaterflea.yn), 
               names_to = "species", 
               values_to = "Presence") %>% 
  mutate(species = ifelse(species == "ZebraMussel.yn", "Zebra Mussel", "Spiny Water Flea"))
#plot
tiff("Inv Spp Y-N.tiff", width = 6, height = 6, units = "in", res = 300)
ggplot(IV.yn.long, aes(x = Presence, fill = species, color = species)) +
  geom_bar(alpha = 0.5, position= "dodge") +
  theme_classic() +
  labs(x = "Invaded Status",
       y = "Count")
dev.off()



#Stocking-----------------------------------

#plot
tiff("Stock Y-N.tiff", width = 6, height = 6, units = "in", res = 300)
ggplot(data.cov, aes(x = stock.yn)) +
  geom_bar(alpha = 0.5) +
  theme_classic() +
  labs(x = "Stocking Status",
       y = "Count")
dev.off()


#Selected------------------------------------------------

#Isolate
selected <- data.cov %>%
  select(area_ha,
         SDI,
         depth.mean.m,
         photic_prop_remote.secchi.closest.year,
         gdd.year.5c,
         remote.secchi.closest.year,
         ZebraMussel.ysi,
         SpinyWaterFlea.ysi,
         precip_10yr_avg_mm
  )


tiff("selected Pairs.tiff", width = 12, height = 12, units = "in", res = 300)
ggpairs(selected)
dev.off()


#corrollelogram
selected.cor <- cor(selected, use = "pairwise.complete.obs")
tiff("Selected Correlations.tiff", width = 12, height = 12, units = "in", res = 300)
corrplot(selected.cor,
         method = "circle",
         type = "lower",
         tl.col = "black",
         tl.cex = 1)
dev.off()


#Part 2 ---------------------------------------------------------------------------------------------------------------------------

#Import new dataset
data2 <- read.csv("Data/Input/Contemporary_Dataset_2026_03_25.csv")

#remove the fish and zoop data for now and literally everything that isn't a covariate
data2.cov <- data2 %>% 
  filter(!is.na(total_zoop_biomass)) %>%  #removes Hill south that does not have zoop data :(
  select(lake_name:stock.yn, -lagoslakeid, -lagoslakeid_verm, -nhdhr_id)


#which month range do I have more in situ secchi data?
summary(data2.cov$secchi.meters.MPCA.Jun.to.Aug) #61 NAs
summary(data2.cov$secchi.meters.MPCA.Jul.to.Sept) #51 NAs - use this one

#isolate just chosen quantitative covariates and rename them shorter so you can read what they are in ggpairs
#also calcualte the log of chla
chosen.quant <- data2.cov %>% 
  select(remote.chla, remote.CDOM, secchi.meters.MPCA.Jul.to.Sept, cond_uscm_exact_year, gdd.year.5c,
         precip_10yr_avg_mm, area_ha, depth.max.m, photic_prop_secchi.meters.MPCA.Jul.to.Sept) %>% 
  rename(secchi.m = secchi.meters.MPCA.Jul.to.Sept,
         cond_uscm = cond_uscm_exact_year,
         gdd.5C = gdd.year.5c,
         precip_mm = precip_10yr_avg_mm,
         photic_prop = photic_prop_secchi.meters.MPCA.Jul.to.Sept) %>% 
  mutate(log.chla = log(remote.chla)) %>% 
  relocate(log.chla, .after = remote.chla)

#CHANGE NAME OF PRECIP VARIABLE TO 5

#ggpairs
pairs <- ggpairs(chosen.quant)
#ggsave(filename = "Selected_Quantitative_Pairs.png", plot = pairs, width = 10, height = 10, units = "in", dpi = 300)



#look at relationship between walleye yoy and zoops
#isolate data
yoy.zoop <- data2 %>% 
  select(WAE.YOY.CPUE, total_zoop_biomass) %>% 
  filter(!is.na(WAE.YOY.CPUE) & !is.na(total_zoop_biomass))

#scatterplot
yoy.zoop.plot <- ggplot(data = yoy.zoop, aes(x = WAE.YOY.CPUE, y = total_zoop_biomass))+
  geom_point()+
  theme_classic()
#ggsave(filename = "Walleye_YOY_v_Total_Zoop_Biomass.png", plot = yoy.zoop.plot, width = 4, height = 4, units = "in", dpi = 300)


cor(yoy.zoop$total_zoop_biomass, y =yoy.zoop$WAE.YOY.CPUE)
#cor = 0.11
#THESE ARE NOT CORRELATED

#filter out only data with all the selected covariates to look at sample size
sample <- data2.cov %>% 
  filter(!is.na(remote.chla) & 
           !is.na(remote.CDOM) & 
           !is.na(secchi.meters.MPCA.Jul.to.Sept) & 
           !is.na(cond_uscm_exact_year) &
           !is.na(gdd.year.5c) & 
           !is.na(precip_10yr_avg_mm) & 
           !is.na(area_ha) & 
           !is.na(depth.max.m) & 
           !is.na(photic_prop_secchi.meters.MPCA.Jul.to.Sept) &
           !is.na(lake_connectivity_class) &
           !is.na(SpinyWaterflea.yn) &
           !is.na(ZebraMussel.yn))
unique(sample$lake_name)
#This gets me 13 lake-years from 10 lakes... not good


#again but don't require conductivity because maybe correlated to temp
sample.no.cond <- data2.cov %>% 
  filter(!is.na(remote.chla) & 
           !is.na(remote.CDOM) & 
           !is.na(secchi.meters.MPCA.Jul.to.Sept) & 
           !is.na(gdd.year.5c) & 
           !is.na(precip_10yr_avg_mm) & 
           !is.na(area_ha) & 
           !is.na(depth.max.m) & 
           !is.na(photic_prop_secchi.meters.MPCA.Jul.to.Sept) &
           !is.na(lake_connectivity_class) &
           !is.na(SpinyWaterflea.yn) &
           !is.na(ZebraMussel.yn))
unique(sample.no.cond$lake_name)
#Now we are at 58 from 24 lakes... better but still not amazing

#how many NA's in each covariate in entire dataset
summary(chosen.quant)
#chla = 115
#CDOM = 115
#secchi/photic_prop = 51
#conductivity = 118
#everything else = 0

#THIS SUGGESTS WE SHOULD USE SECCHI, NOT CHLA + CDOM
#what would that give for a sample size?
sample.no.cond.chla.cdom <- data2.cov %>% 
  filter(!is.na(secchi.meters.MPCA.Jul.to.Sept) & 
           !is.na(gdd.year.5c) & 
           !is.na(precip_10yr_avg_mm) & 
           !is.na(area_ha) & 
           !is.na(depth.max.m) & 
           !is.na(photic_prop_secchi.meters.MPCA.Jul.to.Sept) &
           !is.na(lake_connectivity_class) &
           !is.na(SpinyWaterflea.yn) &
           !is.na(ZebraMussel.yn))
unique(sample.no.cond.chla.cdom$lake_name)
#this give us 153 lake-years from 35 lakes - much better...

# Check for trout lakes
trout <- data2 %>% 
  filter(BKT.CPUE > 0 | LAT.CPUE > 0 | RBT.CPUE > 0)
#15 lake-years from 5 lakes (all the years from these 5 lakes)


# Check for high CDOM lakes
max(data2$remote.CDOM, na.rm = TRUE)
min(data2$remote.CDOM, na.rm = TRUE)
#tait has high CDOM (5.5) - and internet says it is stained. Should I remove? - 3 lake-years
#next highest are all VNP lakes: Rainy (4.5), Sand Point (3.9), Namakan (3.8)
#Idea of removing them would be so that secchi is not low because of high CDOM - I really don't think these are stained enough to make that a problem??
#What would be a good cutoff?


# Do the PCA again
#get rid of log chla
pca.data <- chosen.quant %>% 
  select(-log.chla)

pca_results <- prcomp(na.omit(pca.data),
                      center = TRUE, 
                      scale. = TRUE)
summary(pca_results)

#biplots

pc1v2 <- fviz_pca_biplot(pca_results, 
                repel = TRUE,            # Avoids overlapping text
                col.var = "red",         # Color for variables
                col.ind = "steelblue")+  # Color for lakes
  scale_y_continuous(limits = c(-4, 4)) +
  scale_x_continuous(limits = c(-4, 4)) +
  labs(title = "PC1 (0.50) v PC2 (0.22)")
pc1v2
#ggsave(filename = "Chosen_PC1vPC2.png", plot = pc1v2, width = 6, height = 6, units = "in", dpi = 300)
#why are secchi and chla opposite each other?


pc2v3 <- fviz_pca_biplot(pca_results, 
                axes = c(2,3),
                repel = TRUE,            # Avoids overlapping text
                col.var = "red",         # Color for variables
                col.ind = "steelblue")+  # Color for lakes
  scale_y_continuous(limits = c(-4, 4)) +
  scale_x_continuous(limits = c(-4, 4)) +
  labs(title = "PC2 (0.22) v PC3 (0.13")
pc2v3
#ggsave(filename = "Chosen_PC2vPC3.png", plot = pc2v3, width = 6, height = 6, units = "in", dpi = 300)


pc1v3 <- fviz_pca_biplot(pca_results, 
                axes = c(1,3),
                repel = TRUE,            # Avoids overlapping text
                col.var = "red",         # Color for variables
                col.ind = "steelblue")+  # Color for lakes
  scale_y_continuous(limits = c(-4, 4)) +
  scale_x_continuous(limits = c(-4, 4)) +
  labs(title = "PC1 (0.50) v PC3 (0.13)")
pc1v3
#ggsave(filename = "Chosen_PC1vPC3.png", plot = pc1v3, width = 6, height = 6, units = "in", dpi = 300)


