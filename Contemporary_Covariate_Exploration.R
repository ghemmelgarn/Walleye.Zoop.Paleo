#This script takes the contemporary model dataset and explores covariates to help us choose which ones to put in the model


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
