#This script takes the contemporary model dataset and explores covariates to help us choose which ones to put in the model
#Then it the second part it takes a deeper look at the chosen covariates


library(tidyverse)
library(ggplot2)
library(GGally) #for ggpairs function
library(gridExtra) #to export multiple plots together as .tiff files
library(corrplot) #to make corrollelograms
library(factoextra) #for pca biplot
library(vegan) #for NMDS
library(sf)
library(ggExtra)
library(arrow)


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
         precip_5yr_avg_mm, area_ha, depth.max.m, photic_prop_secchi.meters.MPCA.Jul.to.Sept) %>% 
  rename(secchi.m = secchi.meters.MPCA.Jul.to.Sept,
         cond_uscm = cond_uscm_exact_year,
         gdd.5C = gdd.year.5c,
         precip_mm = precip_5yr_avg_mm,
         photic_prop = photic_prop_secchi.meters.MPCA.Jul.to.Sept) %>% 
  mutate(log.chla = log(remote.chla)) %>% 
  relocate(log.chla, .after = remote.chla)

#ggpairs
pairs <- ggpairs(chosen.quant)
pairs
#ggsave(filename = "Selected_Quantitative_Pairs.png", plot = pairs, width = 10, height = 10, units = "in", dpi = 300)

#corollelogram
chosen.quant.cor <- cor(chosen.quant, use = "pairwise.complete.obs")
#tiff("Selected_Quantitative_Corollelogram.tiff", width = 8, height = 8, units = "in", res = 300)
chosen.quant.cor.plot <- corrplot(chosen.quant.cor,
         method = "circle",
         type = "lower",
         tl.col = "black",
         tl.cex = 1)
#dev.off()

# Do the PCA
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
#ggsave(filename = "Selected_Quantitative_PC1vPC2.png", plot = pc1v2, width = 6, height = 6, units = "in", dpi = 300)


pc2v3 <- fviz_pca_biplot(pca_results, 
                         axes = c(2,3),
                         repel = TRUE,            # Avoids overlapping text
                         col.var = "red",         # Color for variables
                         col.ind = "steelblue")+  # Color for lakes
  scale_y_continuous(limits = c(-4, 4)) +
  scale_x_continuous(limits = c(-4, 4)) +
  labs(title = "PC2 (0.22) v PC3 (0.13")
pc2v3
#ggsave(filename = "Selected_Quantitative_PC2vPC3.png", plot = pc2v3, width = 6, height = 6, units = "in", dpi = 300)


pc1v3 <- fviz_pca_biplot(pca_results, 
                         axes = c(1,3),
                         repel = TRUE,            # Avoids overlapping text
                         col.var = "red",         # Color for variables
                         col.ind = "steelblue")+  # Color for lakes
  scale_y_continuous(limits = c(-4, 4)) +
  scale_x_continuous(limits = c(-4, 4)) +
  labs(title = "PC1 (0.50) v PC3 (0.13)")
pc1v3
#ggsave(filename = "Selected_Quantitative_PC1vPC3.png", plot = pc1v3, width = 6, height = 6, units = "in", dpi = 300)

#Do all this again but with the categorical variables too

#isolate all the chosen covariates including categorical
#also calcualte the log of chla
#make cateogorical variables into factors
chosen.all <- data2.cov %>% 
  select(remote.chla, remote.CDOM, secchi.meters.MPCA.Jul.to.Sept, cond_uscm_exact_year, gdd.year.5c,
         precip_5yr_avg_mm, area_ha, depth.max.m, photic_prop_secchi.meters.MPCA.Jul.to.Sept, 
         lake_connectivity_class, ZebraMussel.yn, SpinyWaterflea.yn) %>% 
  rename(secchi.m = secchi.meters.MPCA.Jul.to.Sept,
         cond_uscm = cond_uscm_exact_year,
         gdd.5C = gdd.year.5c,
         precip_mm = precip_5yr_avg_mm,
         photic_prop = photic_prop_secchi.meters.MPCA.Jul.to.Sept) %>% 
  mutate(log.chla = log(remote.chla),
         drain_con = as.factor(lake_connectivity_class),
         ZM_yn = as.factor(ZebraMussel.yn),
         SWF_yn = as.factor(SpinyWaterflea.yn)) %>% 
  relocate(log.chla, .after = remote.chla) %>% 
  #get rid of unfactored and long named categorical columns
  select(-lake_connectivity_class, -ZebraMussel.yn, -SpinyWaterflea.yn)

#ggpairs
pairs <- ggpairs(chosen.all)
pairs
#ggsave(filename = "Selected_Pairs.png", plot = pairs, width = 18, height = 12, units = "in", dpi = 300)

#Can't do corollelogram or PCA with categorical variables




#filter out only data with all the selected covariates to look at sample size
sample <- data2.cov %>% 
  filter(!is.na(remote.chla) & 
           !is.na(remote.CDOM) & 
           !is.na(secchi.meters.MPCA.Jul.to.Sept) & 
           !is.na(cond_uscm_exact_year) &
           !is.na(gdd.year.5c) & 
           !is.na(precip_5yr_avg_mm) & 
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
           !is.na(precip_5yr_avg_mm) & 
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
#everything else (including categorical) = 0

#THIS SUGGESTS WE SHOULD USE SECCHI, NOT CHLA + CDOM
#what would that give for a sample size?
sample.no.cond.chla.cdom <- data2.cov %>% 
  filter(!is.na(secchi.meters.MPCA.Jul.to.Sept) & 
           !is.na(gdd.year.5c) & 
           !is.na(precip_5yr_avg_mm) & 
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

#trout lakes in "sample.no.cond.chla.cdom" subset of data
trout2 <- data2 %>% 
  filter(!is.na(secchi.meters.MPCA.Jul.to.Sept) & 
           !is.na(gdd.year.5c) & 
           !is.na(precip_5yr_avg_mm) & 
           !is.na(area_ha) & 
           !is.na(depth.max.m) & 
           !is.na(photic_prop_secchi.meters.MPCA.Jul.to.Sept) &
           !is.na(lake_connectivity_class) &
           !is.na(SpinyWaterflea.yn) &
           !is.na(ZebraMussel.yn)) %>% 
  filter(BKT.CPUE > 0 | LAT.CPUE > 0 | RBT.CPUE > 0)
#6 lake-years from 2 lakes (all the years from these 5 lakes)


# Check for high CDOM lakes
max(data2$remote.CDOM, na.rm = TRUE)
min(data2$remote.CDOM, na.rm = TRUE)
#tait has high CDOM (5.5) - and internet says it is stained. Should I remove? - 3 lake-years
#next highest are all VNP lakes: Rainy (4.5), Sand Point (3.9), Namakan (3.8)
#Idea of removing them would be so that secchi is not low because of high CDOM - I really don't think these are stained enough to make that a problem??
#What would be a good cutoff?

#sample size with no conductivity, no chla, no cdom, no trout, and no CDOM > 5
sample.again <- data2 %>% 
  #removes Hill south that does not have zoop data :(
  filter(!is.na(total_zoop_biomass)) %>% 
  #no NA values for selected covariates
  filter(!is.na(secchi.meters.MPCA.Jul.to.Sept) & 
           !is.na(gdd.year.5c) & 
           !is.na(precip_5yr_avg_mm) & 
           !is.na(area_ha) & 
           !is.na(depth.max.m) & 
           !is.na(photic_prop_secchi.meters.MPCA.Jul.to.Sept) &
           !is.na(lake_connectivity_class) &
           !is.na(SpinyWaterflea.yn) &
           !is.na(ZebraMussel.yn)) %>% 
  #remove trout lakes  
  filter(!(BKT.CPUE > 0 | LAT.CPUE > 0 | RBT.CPUE > 0)) %>%  
  #remove CDOM > 5 but allow it to be NA
  filter(remote.CDOM < 5 | is.na(remote.CDOM))
unique(sample.again$lake_name) 

#how many lakes from this sample have no CDOM data in any year?
#get count of rows that are not NA for each lake
lakes.cdom.na <- sample.no.cond.chla.cdom %>% 
  group_by(lake_name, parentdow) %>% 
  summarize(CDOM = sum(!is.na(remote.CDOM)), .groups = 'drop')
#isolate lakes with only NA values for CDOM and format to match remote sensed data
lakes.nocdom <- lakes.cdom.na %>% 
  filter(CDOM == 0) %>% 
  rename(LakeName = lake_name)
  
#read in entire remote sensed dataset (2017-2024) and do the things I did to it to get CDOM in the format I need
        RS <- st_read("Data/Input/Minnesota Lake Water Quality 2017-2024.gpkg")
        #I checked that there is only one layer so don't need to specify
        
        #remove geometry aspects of this file
        RS.nogeom <- st_drop_geometry(RS)
        #now it is a regular dataframe
        
        
        #remove unnecessary identifiers and lake information, rename DOW and LakeName columns, and remove summarized data we don't want to deal with (the other time frames)
        #also get rid of any lakes without a DOW number - these won't have matchable fish surveys anyways
        RS.clean <- RS.nogeom %>% 
          select(-MN_LK_NUM, -unmlknum, -umnlknum_o, -PWI_CLASS, -AREA_BASIN, -WETTYPE, -X_UTM, -Y_UTM, -PolyAcres, -US_L3CODE, -US_L3NAME, -Unnamed..73) %>% 
          rename(DOW = dowlknum_1,
                 LakeName = RNAME_1
          ) %>% 
          select(-ends_with("0726_0824")) %>% 
          #also fix a naming issue for all metrics in june, july, august of 2022
          rename(
            SD_202206 = SD_202SD_206,
            SD_202207 = SD_202SD_207,
            SD_202208 = SD_202SD_208,
            CL_202206 = CL_202CL_206,
            CL_202207 = CL_202CL_207,
            CL_202208 = CL_202CL_208,
            a440_202206 = a440_202a440_206,
            a440_202207 = a440_202a440_207,
            a440_202208 = a440_202a440_208
          ) %>% 
          filter(!is.na(DOW))
        
        
        
        
        #I need to get this into rows for each lake-year
        #step 1 = pivot longer
        RS.long <- RS.clean %>% 
          pivot_longer(
            cols = c(-DOW, -LakeName),
            names_to = "Data",
            values_to = "Value"
          )
        
        
        #Step 2 = Create separate columns for datatype, year, and month,
        RS.long <- RS.long %>% 
          #separate data type
          separate(
            Data,
            into = c("DataType", "Date"),
            sep = "_", #split on underscore
            extra = "merge" #gets everything to the right of first underscore, including other underscores
          ) %>% 
          #separate year and month
          separate(
            Date,
            into = c("Year", "Month"),
            sep = "(?<=^.{4})",   # split after the first 4 characters
            fill = "right" #puts everything else into the other column
          )
        
        
        #Step 3 = now pivot it back wider so we have a row for each lake-year-datatype
        RS.year <- RS.long %>% 
          pivot_wider(
            names_from = Month,
            values_from = Value,
            values_fn = mean #when there are multiple observations for a single dow/lakename combo, this takes the mean of them (they were just sampled at a sub-basin scale)
          )
        
        
        #separate out the secchi data so I can filter it differently than the Chl-a and CDOM data
        SD.year <- RS.year %>%
          filter(DataType == "SD")
        
        CL.CDOM.year <- RS.year %>%
          filter(DataType == "CL" | DataType == "a440")
        
#filter to just cdom data and reformat DOW for join
CDOM.all <- CL.CDOM.year %>% 
  mutate(parentdow = case_when(
    (CL.CDOM.year$DOW == "01014202" | CL.CDOM.year$DOW == "01014201" | CL.CDOM.year$DOW == "04003502" | CL.CDOM.year$DOW == "04003501") ~ substr(CL.CDOM.year$DOW, 2, 8),   #takes care of North and Red lakes
    (CL.CDOM.year$DOW == "69037802" | CL.CDOM.year$DOW == "69037801") ~ substr(CL.CDOM.year$DOW, 1, 8),  #takes care of Vermilion (different because no leading 0),
    (str_detect(CL.CDOM.year$DOW, "^0") & (CL.CDOM.year$DOW != "01014202" & CL.CDOM.year$DOW != "01014201" & CL.CDOM.year$DOW != "04003502" & CL.CDOM.year$DOW != "04003501" & CL.CDOM.year$DOW != "69037802" & CL.CDOM.year$DOW != "69037801")) ~ substr(CL.CDOM.year$DOW, 2, 6), #this gets 5 digits from the DOWs that start with zero and are not those identified before
    (!str_detect(CL.CDOM.year$DOW, "^0") & (CL.CDOM.year$DOW != "01014202" & CL.CDOM.year$DOW != "01014201" & CL.CDOM.year$DOW != "04003502" & CL.CDOM.year$DOW != "04003501" & CL.CDOM.year$DOW != "69037802" & CL.CDOM.year$DOW != "69037801")) ~ substr(CL.CDOM.year$DOW, 1, 6) #this gets 6 digits from the DOWs that don't start with zero and are not those identified before
  )) %>% 
  filter(DataType == "a440") %>% 
  mutate(parentdow = as.numeric(parentdow))

        
#look for any rows in remote sensed dataset that match these lakes
cdom.test <- left_join(lakes.nocdom, CDOM.all, by = "parentdow")
#White Iron might be a problem
#everything else looks okay


#sample size with no conductivity, no chla, no cdom, no trout, and no CDOM > 5 (also eliminating White Iron)
sample.final <- data2 %>% 
  #removes Hill south that does not have zoop data :(
  filter(!is.na(total_zoop_biomass)) %>% 
  #no NA values for selected covariates
  filter(!is.na(secchi.meters.MPCA.Jul.to.Sept) & 
           !is.na(gdd.year.5c) & 
           !is.na(precip_5yr_avg_mm) & 
           !is.na(area_ha) & 
           !is.na(depth.max.m) & 
           !is.na(photic_prop_secchi.meters.MPCA.Jul.to.Sept) &
           !is.na(lake_connectivity_class) &
           !is.na(SpinyWaterflea.yn) &
           !is.na(ZebraMussel.yn)) %>% 
  #remove trout lakes  
  filter(!(BKT.CPUE > 0 | LAT.CPUE > 0 | RBT.CPUE > 0)) %>%  
  #remove CDOM > 5 but allow it to be NA
  filter(remote.CDOM < 5 | is.na(remote.CDOM)) %>% 
  #remove white iron and tait entirely
  filter(lake_name != "White Iron" & lake_name != "Tait")
unique(sample.final$lake_name) 

#with this subset of lakes, do we capture the clarity, temp, and fish community ranges we want?


#plot temp v clarity
temp.clar <- ggplot(data = sample.final, aes(x = gdd.year.5c, y = secchi.meters.MPCA.Jul.to.Sept))+
  geom_point()+
  theme_classic()
png("Temp_Clarity_Pelagic_Model.png", width = 6, height = 6, units = "in", res = 300)
ggMarginal(temp.clar, type = "density", fill = "skyblue", col = "blue", alpha = 0.8)
dev.off()

#plot wae v lmb
wae.lmb <- ggplot(data = sample.final, aes(x = WAE.CPUE, y = LMB.CPUE))+
  geom_point()+
  theme_classic()
ggMarginal(wae.lmb, type = "density", fill = "skyblue", col = "blue", alpha = 0.8)

#plot wae vs. lmb on scale of entire MN fish database
#Import all MN data with good fish surveys
#this finds the minnesota arrow file that contains all the Minnesota fish data - this is what Denver updated in Nov 2025, only has MN data
        mn_data <- open_dataset("G:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Fish Survey Data/Parquet files/mn_update/part-0.parquet")
        
        good.fish.surveys <- mn_data %>%
          filter((sampling_method == "gill_net_standard" |
                    sampling_method == "gill_net_stratified_deep" |
                    sampling_method =="gill_net_stratified_shallow" ) &
                   (survey_type == "Standard Survey" |
                      survey_type == "Population Assessment"|
                      survey_type == "Re-Survey"|
                      survey_type == "Large Lake Survey"|
                      survey_type == "Initial Survey" |
                      survey_type == "Special Assessment" | #will individually investigate if I can use these special assessments IF they match to zoop data
                      survey_type == "Targeted Survey")) %>% #will individually investigate if I can use these targeted surveys IF they match to zoop data
          distinct(lake_id,
                   lake_name,
                   year,
                   month, #this is generated from "date_total_effort_ident" so it gives me the correct month that the fishing actually started with my specified gear 
                   total_effort_ident,
                   total_effort_1,
                   sampling_method_simple,
                   sampling_method,
                   survey_type,
                   nhdhr_id,
                   flag) %>%
          collect()
        #collect actually brings data into R
        
        #fix Crane lake nhdhr_id (wrong in fish database - Denver is fixing it but I will do this for now) - need this to join to LAGOS data
        good.fish.surveys <- good.fish.surveys %>% 
          mutate(nhdhr_id = ifelse(nhdhr_id == "nhdhr_{E940A362-4076-4895-A23F-1B8CCC905DEE}", "nhdhr_105953135", nhdhr_id))
        
        #go get the fish
        all_fish <- mn_data %>% 
          right_join(good.fish.surveys, by = c("total_effort_ident")) %>% 
          collect()
        
        #need to calculate combined effort of stratified surveys 
        #separate out shallow + deep stratified surveys from the standard gillnet surveys
        stratified <- all_fish %>% 
          filter(sampling_method.x == "gill_net_stratified_shallow" | sampling_method.x == "gill_net_stratified_deep")
        
        #get one row for each lake/year/gillnet type
        stratified_surveys <- stratified %>%
          group_by(lake_id.x, lake_name.x, year.x, sampling_method.x) %>%
          summarize(total_effort_1.x = first(total_effort_1.x), .groups = 'drop')
        
        #add effort from shallow + deep stratified surveys
        combined_stratified_effort <- stratified_surveys %>%
          group_by(lake_id.x, year.x) %>%
          summarize(total_effort_cse = sum(total_effort_1.x), .groups = 'drop')
        
        #join this sum back to the stratified data
        stratified_sum <- left_join(stratified, combined_stratified_effort, by = c("lake_id.x", "year.x"))
        
        #remove original effort column 
        stratified_sum_order <- stratified_sum %>% 
          select(-total_effort_1.x) %>% 
          rename(total_effort_1.x = total_effort_cse) %>% #rename the new combined effort to match the rest of the data
          relocate(total_effort_1.x, .after = total_effort_ident) #reorder to put effort column back in original location
        
        #remove the stratified rows from the original dataset
        fish_no_strat <- all_fish %>% 
          filter(sampling_method.x != "gill_net_stratified_shallow" & sampling_method.x != "gill_net_stratified_deep")
        
        #now paste the new rows back in with the updated effort - now my cpue calculations will be correct
        fish_effort_corrected <- rbind(fish_no_strat, stratified_sum_order)
        

#calculate walleye and bass cpue by lake-year
all.fish.cpue <- fish_effort_corrected %>% 
  group_by(lake_id.x, year.x) %>% 
  mutate(WAE.count = sum(species_1 == "walleye"), 
         WAE.CPUE = WAE.count/total_effort_1.x,
         LMB.count = sum(species_1 == "largemouth_bass"),
         LMB.CPUE = LMB.count/total_effort_1.x,
         ) %>% 
  distinct(lake_id.x, year.x, .keep_all = TRUE) %>% #keep only one row per lake/year
  select(lake_name.x, lake_id.x, year.x, WAE.CPUE, LMB.CPUE) %>% #select only relevant columns
  mutate(type = "All MN") %>%  #add an data type ID column
  rename(lake_name = lake_name.x,
         parentdow = lake_id.x,
         Year = year.x)

#format and rbind in the sample data
sample.cpue <- sample.final %>% 
  select(lake_name, parentdow, Year, WAE.CPUE, LMB.CPUE) %>% 
  mutate(type = "Pelagic Model",
         parentdow = as.character(parentdow))
MN.plot.data <- rbind(all.fish.cpue, sample.cpue)

#plot this 
wae.lmb.allMN <- ggplot(data = MN.plot.data, aes(x = WAE.CPUE, y = LMB.CPUE, color = type))+
  geom_point(alpha = 0.8)+
  scale_color_manual(values = c("gray", "red"))+
  theme_classic()
ggMarginal(wae.lmb.allMN, type = "density", groupColor = TRUE, groupFill = TRUE)
wae.lmb.allMN

#plot this on log scae
wae.lmb.allMN.log <- ggplot(data = MN.plot.data, aes(x = WAE.CPUE, y = LMB.CPUE, color = type))+
  geom_point(alpha = 0.8)+
  scale_color_manual(values = c("gray", "red"))+
  theme_classic()+
  scale_x_log10(labels = scales::comma) + 
  scale_y_log10(labels = scales::comma)
ggMarginal(wae.lmb.allMN.log, type = "density", groupColor = TRUE, groupFill = TRUE)
wae.lmb.allMN.log



#ALSO PLOT WITH MOST RECENT FISH SURVEY IN OUR LONG CORE AND SURFACE SED LAKES:

#Read in list of core lakes
core <- read.csv("Data/Input/Core_lake_inclusion_table.csv")

#I AM NOT GOING TO WORRY ABOUT SPLITTING VERMILION
#fix parentdows in the core inclusion table and un-split vermilion
core.format <- core %>% 
  #for this I need to call the full dow "parentdow" - this is an artifact of me quickly combining data where it doesn't really matter
  select(-parentdow) %>% 
  rename(parentdow = DOW) %>% 
  #unsplit vermilion
  #remove west vermilion
  filter(lake_name != "West Vermilion") %>% 
  #rename East Vermilion
  mutate(parentdow = ifelse(lake_name == "East Vermilion", 69037800, parentdow),
         lake_name = ifelse(lake_name == "East Vermilion", "Vermilion", lake_name),
         parentdow = as.character(parentdow))
  

#left join to all MN fish data by parentdow
core.fish <- left_join(core.format, all.fish.cpue, by = "parentdow")

#summarize just the most recent year for these lakes
core.fish.select.year <- core.fish %>% 
  #deal with the multiple type and lake name columns
  select(-type.y, -lake_name.y) %>% 
  rename(type = type.x,
         lake_name = lake_name.x) %>% 
  group_by(lake_name, parentdow, type) %>% 
  mutate(recent.year = max(Year)) %>% 
  ungroup()

core.fish.recent <- core.fish.select.year %>% 
  filter(Year == recent.year)

#format to rbind
core.fish.format <- core.fish.recent %>% 
  relocate(parentdow, .after = lake_name) %>% 
  relocate(type, .after = LMB.CPUE) %>% 
  select(-recent.year)

#add separate rows for full vs surface from the same lake
core.full <- core.fish.format %>% 
  filter(type == "Full and Surface") %>% 
  mutate(type = "Full Core")

core.surface <- core.fish.format %>% 
  mutate(type = "Surface Sediments")

#rbind
ALL.PLOT.DATA <- rbind(MN.plot.data, core.surface, core.full)

#plot it!!
wae.lmb.core <- ggplot(data = ALL.PLOT.DATA, aes(x = WAE.CPUE, y = LMB.CPUE, color = type))+
  geom_point(alpha = 0.5)+
  scale_color_manual(values = c("gray", "saddlebrown", "red", "blue"))+
  theme_classic()
#png("WAE_LMB_representation.png", width = 6, height = 6, units = "in", res = 300)
ggMarginal(wae.lmb.core, type = "density", groupColor = TRUE, groupFill = TRUE)
#dev.off()

#plot this on log scae
wae.lmb.core.log <- ggplot(data = ALL.PLOT.DATA, aes(x = WAE.CPUE, y = LMB.CPUE, color = type))+
  geom_point(alpha = 0.6, size = 3)+
  scale_color_manual(values = c("gray", "saddlebrown", "red", "blue"))+
  theme_classic()+
  scale_x_log10(labels = scales::comma) + 
  scale_y_log10(labels = scales::comma)
#png("WAE_LMB_representation_log.png", width = 8, height = 6, units = "in", res = 300)
ggMarginal(wae.lmb.core.log, type = "density", groupColor = TRUE, groupFill = TRUE)
#dev.off()








#look at relationship between walleye yoy and zoops
#isolate data
yoy.zoop <- data2 %>% 
  select(WAE.YOY.CPUE, total_zoop_biomass, mean_length_all_zoop, mean_length_clad, clad_ratio) %>% 
  filter(!is.na(WAE.YOY.CPUE) & !is.na(total_zoop_biomass))

#scatterplots
yoy.zoop.plot <- ggplot(data = yoy.zoop, aes(x = WAE.YOY.CPUE, y = total_zoop_biomass))+
  geom_point()+
  theme_classic()
yoy.zoop.plot
#ggsave(filename = "Walleye_YOY_v_Total_Zoop_Biomass.png", plot = yoy.zoop.plot, width = 4, height = 4, units = "in", dpi = 300)

yoy.length.plot <- ggplot(data = yoy.zoop, aes(x = WAE.YOY.CPUE, y = mean_length_all_zoop))+
  geom_point()+
  theme_classic()
yoy.length.plot
#ggsave(filename = "Walleye_YOY_v_Mean_Zoop_Length.png", plot = yoy.length.plot, width = 4, height = 4, units = "in", dpi = 300)

yoy.clad.length.plot <- ggplot(data = yoy.zoop, aes(x = WAE.YOY.CPUE, y = mean_length_clad))+
  geom_point()+
  theme_classic()
yoy.clad.length.plot
#ggsave(filename = "Walleye_YOY_v_Mean_Clad_Length.png", plot = yoy.clad.length.plot, width = 4, height = 4, units = "in", dpi = 300)

yoy.clad.ratio.plot <- ggplot(data = yoy.zoop, aes(x = WAE.YOY.CPUE, y = clad_ratio))+
  geom_point()+
  theme_classic()+
  scale_y_continuous(limits = c(0, 20)) #removes visualizing an outlier
yoy.clad.ratio.plot
#ggsave(filename = "Walleye_YOY_v_Clad_Ratio.png", plot = yoy.clad.ratio.plot, width = 4, height = 4, units = "in", dpi = 300)


cor(yoy.zoop$total_zoop_biomass, y =yoy.zoop$WAE.YOY.CPUE)
#cor = 0.114

cor(yoy.zoop$mean_length_all_zoop, y =yoy.zoop$WAE.YOY.CPUE)
#cor = 0.002

cor(yoy.zoop$mean_length_clad, y =yoy.zoop$WAE.YOY.CPUE)
#cor = 0.155

cor(yoy.zoop$clad_ratio, y =yoy.zoop$WAE.YOY.CPUE)
#cor = -0.057
#THESE ARE NOT CORRELATED


#NMDS of zoops and color by walleye (above/below mean?)
yoy.zoop.nmds <- data2 %>% 
  select(WAE.YOY.CPUE:nauplii) %>% 
  filter(!is.na(Alona.sp.)) #removes the NA row

#Create vectors for color by walleye CPUE
color <- yoy.zoop.nmds$WAE.YOY.CPUE

#Calculate relative abundance and remove walleye column
zoop_rel_abun <- yoy.zoop.nmds[,2:31] / rowSums(yoy.zoop.nmds[,2:31])

#square root transform to do Hellinger transformation to downweight rare species and deal with the many zeroes
zoop_rel_abun_Holl <- sqrt(zoop_rel_abun)

#Make an NMDS with euclidean distance on Hellinger-transformed biomass data
euclidean_dist <- metaMDS(zoop_rel_abun_Holl, distance = "euclidean")
#extract axis scores for the sites (samples) only
Plot.Data <- as.data.frame(scores(euclidean_dist, display = "sites"))
#add walleye yoy back to dataframe
Plot.Data$WAE.YOY.cpue <- color


#plot with color by lake and fill by net mouth size
NMDS_zoop_yoy <- ggplot(data = Plot.Data, aes(x = NMDS1, y = NMDS2, color = WAE.YOY.cpue, alpha = 0.8)) +
  geom_point(size = 3)+
  labs(title = "Zoop Community Hellinger-Transformed NMDS", y = "NMDS2", x = "NMDS1") +
  scale_color_gradient(low = "blue", high = "red")+
  theme_classic()
NMDS_zoop_yoy
#ggsave(filename = "Walleye_YOY_Zoop_NMDS.png", plot = NMDS_zoop_yoy, width = 8, height = 6, units = "in", dpi = 300)

#walleye YOY on its own doesn't seem to affect zoops at all... also lots of missing data...




#DATA SELECTION 4/6/26------------------------------------------------------------
data3 <- read.csv("Data/Input/Contemporary_Dataset_2026_04_06.csv")

#apply no trout and no high CDOM filters
filter_3.38 <- data3 %>% 
  #removes Hill south that does not have zoop data :(
  filter(!is.na(total_zoop_biomass)) %>% 
  #no NA values for selected covariates
  filter(!is.na(secchi.meters.MPCA.Jul.to.Sept) & 
           !is.na(gdd.year.5c) & 
           !is.na(precip_5yr_avg_mm) & 
           !is.na(area_ha) & 
           !is.na(depth.max.m) & 
           !is.na(photic_prop_secchi.meters.MPCA.Jul.to.Sept) &
           !is.na(lake_connectivity_class) &
           !is.na(SpinyWaterflea.yn) &
           !is.na(ZebraMussel.yn)) %>% 
  #remove trout lakes  
  filter(!(BKT.CPUE > 0 | LAT.CPUE > 0 | RBT.CPUE > 0)) %>%  
  #remove CDOM >= 3.38
  filter(CDOM.lake.avg < 3.38)
unique(filter_3.38$lake_name) 


#isolate the high CDOM lakes
high.CDOM <- data3 %>% 
  #removes Hill south that does not have zoop data :(
  filter(!is.na(total_zoop_biomass)) %>% 
  #no NA values for selected covariates
  filter(!is.na(secchi.meters.MPCA.Jul.to.Sept) & 
           !is.na(gdd.year.5c) & 
           !is.na(precip_5yr_avg_mm) & 
           !is.na(area_ha) & 
           !is.na(depth.max.m) & 
           !is.na(photic_prop_secchi.meters.MPCA.Jul.to.Sept) &
           !is.na(lake_connectivity_class) &
           !is.na(SpinyWaterflea.yn) &
           !is.na(ZebraMussel.yn)) %>% 
  #remove trout lakes  
  filter(!(BKT.CPUE > 0 | LAT.CPUE > 0 | RBT.CPUE > 0)) %>%  
  #keep only high CDOM
  filter(CDOM.lake.avg >= 3.38) %>% 
  select(lake_name, Year, CDOM.lake.avg)

#what if we made the cdom filter 3.6 so we don't lose Rainy and Namakan?
filter_3.6 <- data3 %>% 
  #removes Hill south that does not have zoop data :(
  filter(!is.na(total_zoop_biomass)) %>% 
  #no NA values for selected covariates
  filter(!is.na(secchi.meters.MPCA.Jul.to.Sept) & 
           !is.na(gdd.year.5c) & 
           !is.na(precip_5yr_avg_mm) & 
           !is.na(area_ha) & 
           !is.na(depth.max.m) & 
           !is.na(photic_prop_secchi.meters.MPCA.Jul.to.Sept) &
           !is.na(lake_connectivity_class) &
           !is.na(SpinyWaterflea.yn) &
           !is.na(ZebraMussel.yn)) %>% 
  #remove trout lakes  
  filter(!(BKT.CPUE > 0 | LAT.CPUE > 0 | RBT.CPUE > 0)) %>%  
  #remove CDOM >= 3.6
  filter(CDOM.lake.avg < 3.6)
unique(filter_3.6$lake_name) 


#PLOTS FOR THE 3.38 CDOM filter:
#plot temp v clarity
temp.clar.3.38 <- ggplot(data = filter_3.38, aes(x = gdd.year.5c, y = secchi.meters.MPCA.Jul.to.Sept))+
  geom_point()+
  theme_classic()
#png("Temp_Clarity_Pelagic_Model_3.38CDOM.png", width = 6, height = 6, units = "in", res = 300)
ggMarginal(temp.clar.3.38, type = "density", fill = "skyblue", col = "blue", alpha = 0.8)
#dev.off()

#add in the MN fish data
#format and rbind in the sample data
filter_3.38.cpue <- filter_3.38 %>% 
  select(lake_name, parentdow, Year, WAE.CPUE, LMB.CPUE) %>% 
  mutate(type = "Pelagic Model",
         parentdow = as.character(parentdow))
MN.plot.data_3.38 <- rbind(all.fish.cpue, filter_3.38.cpue)

#add in core lake data
#rbind
ALL.PLOT.DATA_3.38 <- rbind(MN.plot.data_3.38, core.surface, core.full)

wae.lmb.core_3.38 <- ggplot(data = ALL.PLOT.DATA_3.38, aes(x = WAE.CPUE, y = LMB.CPUE, color = type))+
  geom_point(alpha = 0.5)+
  scale_color_manual(values = c("gray", "saddlebrown", "red", "blue"))+
  theme_classic()
#png("WAE_LMB_representation_3.38CDOM.png", width = 6, height = 6, units = "in", res = 300)
ggMarginal(wae.lmb.core_3.38, type = "density", groupColor = TRUE, groupFill = TRUE)
#dev.off()

#plot this on log scae
wae.lmb.core.log_3.38 <- ggplot(data = ALL.PLOT.DATA_3.38, aes(x = WAE.CPUE, y = LMB.CPUE, color = type))+
  geom_point(alpha = 0.6, size = 3)+
  scale_color_manual(values = c("gray", "saddlebrown", "red", "blue"))+
  theme_classic()+
  scale_x_log10(labels = scales::comma) + 
  scale_y_log10(labels = scales::comma)
#png("WAE_LMB_representation_log_3.38CDOM.png", width = 8, height = 6, units = "in", res = 300)
ggMarginal(wae.lmb.core.log_3.38, type = "density", groupColor = TRUE, groupFill = TRUE)
#dev.off()






#PLOTS FOR THE 3.6 CDOM filter:
#plot temp v clarity
temp.clar.3.6 <- ggplot(data = filter_3.6, aes(x = gdd.year.5c, y = secchi.meters.MPCA.Jul.to.Sept))+
  geom_point()+
  theme_classic()
#png("Temp_Clarity_Pelagic_Model_3.6CDOM.png", width = 6, height = 6, units = "in", res = 300)
ggMarginal(temp.clar.3.6, type = "density", fill = "skyblue", col = "blue", alpha = 0.8)
#dev.off()

#add in the MN fish data
#format and rbind in the sample data
filter_3.6.cpue <- filter_3.6 %>% 
  select(lake_name, parentdow, Year, WAE.CPUE, LMB.CPUE) %>% 
  mutate(type = "Pelagic Model",
         parentdow = as.character(parentdow))
MN.plot.data_3.6 <- rbind(all.fish.cpue, filter_3.6.cpue)

#add in core lake data
#rbind
ALL.PLOT.DATA_3.6 <- rbind(MN.plot.data_3.6, core.surface, core.full)

wae.lmb.core_3.6 <- ggplot(data = ALL.PLOT.DATA_3.6, aes(x = WAE.CPUE, y = LMB.CPUE, color = type))+
  geom_point(alpha = 0.5)+
  scale_color_manual(values = c("gray", "saddlebrown", "red", "blue"))+
  theme_classic()
#png("WAE_LMB_representation_3.6CDOM.png", width = 6, height = 6, units = "in", res = 300)
ggMarginal(wae.lmb.core_3.6, type = "density", groupColor = TRUE, groupFill = TRUE)
#dev.off()

#plot this on log scae
wae.lmb.core.log_3.6 <- ggplot(data = ALL.PLOT.DATA_3.6, aes(x = WAE.CPUE, y = LMB.CPUE, color = type))+
  geom_point(alpha = 0.6, size = 3)+
  scale_color_manual(values = c("gray", "saddlebrown", "red", "blue"))+
  theme_classic()+
  scale_x_log10(labels = scales::comma) + 
  scale_y_log10(labels = scales::comma)
#png("WAE_LMB_representation_log_3.6CDOM.png", width = 8, height = 6, units = "in", res = 300)
ggMarginal(wae.lmb.core.log_3.6, type = "density", groupColor = TRUE, groupFill = TRUE)
#dev.off()


#what happens to our sample size if we require walleye YOY data?
filter_3.6_waeyoy <- data3 %>% 
  #removes Hill south that does not have zoop data :(
  filter(!is.na(total_zoop_biomass)) %>% 
  #no NA values for selected covariates
  filter(!is.na(secchi.meters.MPCA.Jul.to.Sept) & 
           !is.na(gdd.year.5c) & 
           !is.na(precip_5yr_avg_mm) & 
           !is.na(area_ha) & 
           !is.na(depth.max.m) & 
           !is.na(photic_prop_secchi.meters.MPCA.Jul.to.Sept) &
           !is.na(lake_connectivity_class) &
           !is.na(SpinyWaterflea.yn) &
           !is.na(ZebraMussel.yn)) %>% 
  #remove trout lakes  
  filter(!(BKT.CPUE > 0 | LAT.CPUE > 0 | RBT.CPUE > 0)) %>%  
  #remove CDOM >= 3.6
  filter(CDOM.lake.avg < 3.6) %>% 
  filter(!is.na(WAE.YOY.CPUE))
unique(filter_3.6_waeyoy$lake_name) 
#BAD NEWS FOR THIS ONE
  
