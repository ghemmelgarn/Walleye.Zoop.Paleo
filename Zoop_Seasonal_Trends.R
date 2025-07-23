#this script makes plots that show seasonal trends in zooplankton by species for all lakes and our sample lakes
#this is a quick first attempt - more could be done to clean these up later if wanted
#exports as pdf files with one page per species

library(tidyverse)
library(dplyr)
library(ggplot2)
library(vegan)
library(gridExtra)

#read data
zoop <- read.csv("Data/Input/ZoopDB_data_20250204.csv")
#this is the zoop data Kylie sent me on Feb 4, 2025 - it is up to date with what she had processed at that time

#make parentdow column
zoop_parentdow <- zoop %>%
  mutate(parentdow = case_when(
    nchar(zoop$dowlknum) == 7 ~ substr(dowlknum, 1, 5),
    nchar(zoop$dowlknum) == 8 ~ substr(dowlknum, 1, 6)
  ))

#create columns for month, day, and year separately
zoop_months <- zoop_parentdow %>%
  mutate(year = substr(sample_date, 1, 4)) %>% 
  mutate(month = substr(sample_date, 6, 7)) %>%
  mutate(day = substr(sample_date, 9, 10))

#make parentdow.zoop.year column
zoop_months$parentdow.zoop.year = paste(zoop_months$parentdow, zoop_months$year)

#remove bythotrephes and leptodora from the zoop data - discussed with Kylie and Heidi and we don't have the data resolution to include them
zoop_months <- filter(zoop_months, species != "Bythotrephes longimanus" & species != "Leptodora kindti" & species != "Leptodora metanauplius")

#check species present
sort(unique(zoop_months$species))

#clean up taxa: did cladocerans as decided for analysis and copepods as convenient for this plot

      #Rename taxa that need it based on conversation with Heidi and Kylie
      #targeted just the Belle lake Daphnia based on Jodie's notes from when she IDed them
      #I know I can run these together but I was getting an error I didn't have time to deal with when I tried that
      zoop_clean_taxa <- zoop_months %>%
        mutate(species = ifelse(species == "Chydorus sp." | species == "Chydoridae" | species == "Chydorus bicornutus", "Chydorus sphaericus", species)) 
      zoop_clean_taxa <- zoop_clean_taxa %>%
        mutate(species = ifelse(species == "Bosmina longirostris", "Bosmina sp.", species))
      zoop_clean_taxa <- zoop_clean_taxa %>%
        mutate(species = ifelse(species == "Alona setulosa" | species == "Alona quadrangularis" , "Alona sp.", species))
      zoop_clean_taxa <- zoop_clean_taxa %>%  
        mutate(species = ifelse(species == "Ceriodaphnia reticulata" | species == "Ceriodaphnia quadrangula" | species == "Ceriodaphnia lacustris", "Ceriodaphnia sp.", species))
      zoop_clean_taxa <- zoop_clean_taxa %>%
        mutate(species = ifelse(species == "Daphnia pulex", "Daphnia pulicaria", species))
      zoop_clean_taxa <- zoop_clean_taxa %>%
        mutate(species = ifelse(species == "Acanthocyclops sp.", "Acanthocyclops vernalis", species))
      zoop_clean_taxa <- zoop_clean_taxa %>%
        mutate(species = ifelse(species == "Aglaodiaptomus clavipes" | species == "Aglaodiaptomus leptopus" | species == "Aglaodiaptomus saskatchewanensis", "Aglaodiaptomus sp.", species))
      zoop_clean_taxa <- zoop_clean_taxa %>%
        mutate(species = ifelse(species == "Diacyclops sp.", "Diacyclops bicuspidatus thomasi", species))
      zoop_clean_taxa <- zoop_clean_taxa %>%
        mutate(species = ifelse(species == "Leptodiaptomus minutus" | species == "Leptodiaptomus sicilis" | species == "Leptodiaptomus siciloides", "Leptodiaptomus sp.", species))
      zoop_clean_taxa <- zoop_clean_taxa %>%
        mutate(species = ifelse(species == "Mesocyclops sp.", "Mesocyclops edax", species))
      zoop_clean_taxa <- zoop_clean_taxa %>%
        mutate(species = ifelse(species == "Pleuroxus denticulatus" | species == "Pleuroxus procurvus", "Pleuroxus sp.", species))
      zoop_clean_taxa <- zoop_clean_taxa %>%
        mutate(species = ifelse(species == "Skistodiaptomus sp.", "Skistodiaptomus oregonensis", species))
      #below I am targeting just the Belle lake Daphnia based on Jodie's notes from when she IDed them
      zoop_clean_taxa <- zoop_clean_taxa %>%
        mutate(species = ifelse(species == "Daphnia sp." & parentdow.zoop.year == "470049 2008", "Daphnia rosea", species))
      #Remove the Daphnia sp. observations without species - counts are all low
      zoop_clean_taxa2 <- zoop_clean_taxa %>%
        filter(species != "Daphnia sp.")
      
      # #check that it worked
      sort(unique(zoop_clean_taxa2$species))
      # #yay!
      

#remove littoral samples   
      
      #check that none of these are littoral samples
      littoral <- grep("Littoral", zoop_clean_taxa2$remarks)
      zoop_clean_taxa_littoral <- zoop_clean_taxa2[littoral,]
      #confirmed these are littoral samples
      #remove littoral rows
      zoop_clean_taxa3 <- zoop_clean_taxa2[-littoral,]
      
      
#format date for plotting
      zoop_clean_taxa3$sample_date <- as.Date(zoop_clean_taxa3$sample_date)
      zoop_clean_taxa3$month_day <- format(zoop_clean_taxa3$sample_date, "%b-%d")
      #make a fake date with all samples in the same year for plot
      zoop_clean_taxa3$plot_date <- as.Date(sprintf("2000-%02d-%02d", month(zoop_clean_taxa3$sample_date), day(zoop_clean_taxa3$sample_date)))
      #make a list of dates for labels on the 1st and 15th of each month
      breaks <-  zoop_clean_taxa3 %>%
        filter(format(plot_date, "%d") %in% c("01", "15")) %>%
        pull(plot_date)
      
#filter out only summer samples
      zoop_summer <- zoop_clean_taxa3 %>% 
        filter(plot_date >= as.Date("2000-05-01") & plot_date <= as.Date("2000-10-31"))

      
#plot all zoop species together
      all_zoop <- ggplot(zoop_summer, aes(x = plot_date)) + 
        geom_point(aes(y = density))+
        labs(title = "Zooplankton Seasonal Trends:ALL SPECIES", y = "Density (count/L)", x = "Date") +
        scale_x_date(breaks = breaks, date_labels = "%b-%d")+ #only label 1st and 15th of each month in correct format
        #scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2), labels = seq(0, 10, by = 2)) +
        theme(
          panel.background = element_blank(),  # Remove panel background
          plot.background = element_blank(),    # Remove plot background
          axis.line = element_line(color = "black"),  # Add axis lines
          axis.text.x = element_text(angle = 45, hjust = 1), #rotate x axis labels
          #legend.position = "none" #removes all legends
        )
      print(all_zoop)
      
#plot by species for all lakes over all years (colored by year)     
      spp_zoop <- ggplot(zoop_summer, aes(x = plot_date, color = year)) + 
        geom_point(aes(y = density))+
        labs(title = "Zooplankton Seasonal Trends", y = "Density (count/L)", x = "Date") +
        scale_x_date(breaks = breaks, date_labels = "%b-%d")+ #only label 1st and 15th of each month in correct format
        theme(
          panel.background = element_blank(),  # Remove panel background
          plot.background = element_blank(),    # Remove plot background
          axis.line = element_line(color = "black"),  # Add axis lines
          axis.text.x = element_text(angle = 45, hjust = 1), #rotate x axis labels
          #legend.position = "none" #removes all legends
        )+
        facet_wrap(~ species) #creates a separate plot for each species
      print(spp_zoop)
      
#-----------------------------------------------------------------------------------------------------------------------------
#same but just for our lakes that have previous data
      #Christmas, Sturgeon, and Clear have not been sampled :(
      #Green = 34007900
      #Pelican = 18030800
      #Elephant = 69081000
      
  our_lakes <- zoop_summer %>% 
        filter(dowlknum == 34007900 | dowlknum == 18030800 | dowlknum == 69081000)

      #plot by species      
      our_zoop <- ggplot(our_lakes, aes(x = plot_date, color = lake_name)) + 
        geom_point(aes(y = density))+
        labs(title = "Zooplankton Seasonal Trends", y = "Density (count/L)", x = "Date") +
        scale_x_date(breaks = breaks, date_labels = "%b-%d")+ #only label 1st and 15th of each month in correct format
        theme(
          #panel.background = element_blank(),  # Remove panel background
          plot.background = element_blank(),    # Remove plot background
          axis.line = element_line(color = "black"),  # Add axis lines
          axis.text.x = element_text(angle = 45, hjust = 1), #rotate x axis labels
          #legend.position = "none" #removes all legends
        )+
        facet_wrap(~ species) #creates a separate plot for each species
      print(our_zoop)
      
      
#---------------------------------------------------------------------------------------------------------------------
#plots exported in a format big enough to actually see them

   #group data by species
      grouped_zoop_summer <- zoop_summer %>%
        group_by(species) %>%
        group_split()
      grouped_our_lakes <- our_lakes %>%
        group_by(species) %>%
        group_split()

# #ALL LAKES
#   #start a pdf
#       pdf("zoop_seasonal_by_spp_all_lakes.pdf", width = 8, height = 10)
# 
#   #one page per species
#       for (g in grouped_zoop_summer) {
#         p <- ggplot(g, aes(x = plot_date, color = year)) +
#           geom_point(aes(y = density), shape = 21, size = 2)+
#           labs(y = "Density (count/L)", x = "Date") +
#           scale_x_date(breaks = breaks, date_labels = "%b-%d", limits = as.Date(c("2000-05-01", "2000-10-31")))+ #only label 1st and 15th of each month in correct format
#           ylim(0, NA)+
#           ggtitle(unique(g$species))+
#           theme(
#             panel.background = element_blank(),  # Remove panel background
#             plot.background = element_blank(),    # Remove plot background
#             axis.line = element_line(color = "black"),  # Add axis lines
#             axis.text.x = element_text(angle = 45, hjust = 1), #rotate x axis labels
#           )
#         print(p)
#       }
# 
#   #finish the export
#       dev.off()
# 
# 
# 
# #OUR SAMPLE LAKES    
#       #start a pdf
#       pdf("zoop_seasonal_by_spp_GreenPelicanElephant.pdf", width = 8, height = 10)
#       
#       #one page per species
#       for (g in grouped_our_lakes) {
#         t <- ggplot(g, aes(x = plot_date, color = lake_name)) + 
#           geom_point(aes(y = density), shape = 21, size = 2)+
#           labs(y = "Density (count/L)", x = "Date") +
#           scale_x_date(breaks = breaks, date_labels = "%b-%d", limits = as.Date(c("2000-05-01", "2000-10-31")))+ #only label 1st and 15th of each month in correct format
#           ylim(0, NA)+
#           ggtitle(unique(g$species))+
#           theme(
#             panel.background = element_blank(),  # Remove panel background
#             plot.background = element_blank(),    # Remove plot background
#             axis.line = element_line(color = "black"),  # Add axis lines
#             axis.text.x = element_text(angle = 45, hjust = 1), #rotate x axis labels
#           )
#         print(t)  
#       }
#       
#       #finish the export
#       dev.off()
      
#---------------------------------------------------------------------------------------------------------------------------------------
#investigate bosmina without the outliers
      
bosmina <-zoop_summer %>% 
        filter(species == "Bosmina sp.")
      
bosmina_plot <- ggplot(bosmina, aes(x = plot_date, color = year)) + 
        geom_point(aes(y = density), shape = 21, size = 2)+
        labs(title = "Bosmina Seasonal Trends", y = "Density (count/L)", x = "Date") +
        scale_x_date(breaks = breaks, date_labels = "%b-%d")+ #only label 1st and 15th of each month in correct format
        theme(
          panel.background = element_blank(),  # Remove panel background
          plot.background = element_blank(),    # Remove plot background
          axis.line = element_line(color = "black"),  # Add axis lines
          axis.text.x = element_text(angle = 45, hjust = 1), #rotate x axis labels
          #legend.position = "none" #removes all legends
        )
print(bosmina_plot)


bosmina_no_outliers <- bosmina %>% 
  filter(density <= 1000)

bosmina_plot_no_out <- ggplot(bosmina_no_outliers, aes(x = plot_date, color = year)) + 
  geom_point(aes(y = density), shape = 21, size = 2)+
  labs(title = "Bosmina Seasonal Trends", y = "Density (count/L)", x = "Date") +
  scale_x_date(breaks = breaks, date_labels = "%b-%d")+ #only label 1st and 15th of each month in correct format
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"),  # Add axis lines
    axis.text.x = element_text(angle = 45, hjust = 1), #rotate x axis labels
    #legend.position = "none" #removes all legends
  )
print(bosmina_plot_no_out)

bosmina_under_200 <- bosmina %>% 
  filter(density <= 200)

bosmina_plot_under_200 <- ggplot(bosmina_under_200, aes(x = plot_date, color = year)) + 
  geom_point(aes(y = density), shape = 21, size = 2)+
  labs(title = "Bosmina Seasonal Trends", y = "Density (count/L)", x = "Date") +
  scale_x_date(breaks = breaks, date_labels = "%b-%d")+ #only label 1st and 15th of each month in correct format
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"),  # Add axis lines
    axis.text.x = element_text(angle = 45, hjust = 1), #rotate x axis labels
    #legend.position = "none" #removes all legends
  )
print(bosmina_plot_under_200)


#no obvious trend emerges when I zoom in here. May vary by lake, but too many lakes to look at them all individually


#try plotting a line for each lake-year to see if we have some lake-years that are generally high
bosmina_lakeyears <- ggplot(bosmina, aes(x = plot_date, color = parentdow.zoop.year)) + 
  geom_point(aes(y = density), shape = 21, size = 2)+
  geom_line(aes(y = density))+
  labs(title = "Bosmina Seasonal Trends", y = "Density (count/L)", x = "Date") +
  scale_x_date(breaks = breaks, date_labels = "%b-%d")+ #only label 1st and 15th of each month in correct format
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"),  # Add axis lines
    axis.text.x = element_text(angle = 45, hjust = 1), #rotate x axis labels
    legend.position = "none" #removes all legends
  )
print(bosmina_lakeyears)


bosmina_lakeyears_under200 <- ggplot(bosmina_under_200, aes(x = plot_date, color = parentdow.zoop.year)) + 
  geom_point(aes(y = density), shape = 21, size = 2)+
  geom_line(aes(y = density))+
  labs(title = "Bosmina Seasonal Trends", y = "Density (count/L)", x = "Date") +
  scale_x_date(breaks = breaks, date_labels = "%b-%d")+ #only label 1st and 15th of each month in correct format
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"),  # Add axis lines
    axis.text.x = element_text(angle = 45, hjust = 1), #rotate x axis labels
    legend.position = "none" #removes all legends
  )
print(bosmina_lakeyears_under200)


bosmina_under_20 <- bosmina %>% 
  filter(density <= 20)
bosmina_lakeyears_under20 <- ggplot(bosmina_under_20, aes(x = plot_date, color = parentdow.zoop.year)) + 
  geom_point(aes(y = density), shape = 21, size = 2)+
  geom_line(aes(y = density))+
  labs(title = "Bosmina Seasonal Trends", y = "Density (count/L)", x = "Date") +
  scale_x_date(breaks = breaks, date_labels = "%b-%d")+ #only label 1st and 15th of each month in correct format
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"),  # Add axis lines
    axis.text.x = element_text(angle = 45, hjust = 1), #rotate x axis labels
    legend.position = "none" #removes all legends
  )
print(bosmina_lakeyears_under20)


#what I am seeing here is that different lake-years have different peaks, but few of them are consistently high


#----------------------------------------------------------------------------------------------------------------------
#investigate bosmina and eubosmina in our lakes with both years and lakes


bosmina_our_lakes <-our_lakes %>% 
  filter(species == "Bosmina sp.")

#tiff("Bosmina_GreenPelicanElephant.tiff", width = 7, height = 7, units = "in", res = 300)
          our_bosmina_detail <- ggplot(bosmina_our_lakes, aes(x = plot_date, color = year, shape = lake_name)) + 
            geom_point(aes(y = density), fill = NA, stroke = 1)+
            labs(y = "Density (count/L)", x = "Date") +
            scale_x_date(breaks = breaks, date_labels = "%b-%d", limits = as.Date(c("2000-05-01", "2000-10-31")))+ #only label 1st and 15th of each month in correct format
            ylim(0, NA)+
            ggtitle("Bosmina sp.")+
            scale_shape_manual(values = c(21, 24, 22))+
            theme(
              panel.background = element_blank(),  # Remove panel background
              plot.background = element_blank(),    # Remove plot background
              axis.line = element_line(color = "black"),  # Add axis lines
              axis.text.x = element_text(angle = 45, hjust = 1), #rotate x axis labels
            )
          print(our_bosmina_detail)
#dev.off()
          
eubosmina_our_lakes <-our_lakes %>% 
  filter(species == "Eubosmina coregoni")


#tiff("Eubosmina_GreenPelicanElephant.tiff", width = 7, height = 7, units = "in", res = 300)
our_eubosmina_detail <- ggplot(eubosmina_our_lakes, aes(x = plot_date, color = year, shape = lake_name)) + 
  geom_point(aes(y = density), fill = NA, stroke = 1)+
  labs(y = "Density (count/L)", x = "Date") +
  scale_x_date(breaks = breaks, date_labels = "%b-%d", limits = as.Date(c("2000-05-01", "2000-10-31")))+ #only label 1st and 15th of each month in correct format
  ylim(0, NA)+
  ggtitle("Euosmina sp.")+
  scale_shape_manual(values = c(21, 24, 22))+
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"),  # Add axis lines
    axis.text.x = element_text(angle = 45, hjust = 1), #rotate x axis labels
  )
print(our_eubosmina_detail)
#dev.off()

#Eubosmina and Bosmina on the same plot for our lakes
eubosmina_bosmina_our_lakes <-our_lakes %>% 
  filter(species == "Eubosmina coregoni" | species == "Bosmina sp.")

#tiff("Eubosmina_Bosmina_GreenPelicanElephant.tiff", width = 7, height = 7, units = "in", res = 300)
Bosmina.Eubosmina <- ggplot(eubosmina_bosmina_our_lakes, aes(x = plot_date, shape = lake_name, color = species)) + 
  geom_point(aes(y = density), fill = NA, stroke = 1)+
  labs(y = "Density (count/L)", x = "Date") +
  scale_x_date(breaks = breaks, date_labels = "%b-%d", limits = as.Date(c("2000-05-01", "2000-10-31")))+ #only label 1st and 15th of each month in correct format
  ylim(0, NA)+
  scale_shape_manual(values = c(21, 24, 22))+
  ggtitle("Bosmina sp. and Eubsomina sp.")+
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"),  # Add axis lines
    axis.text.x = element_text(angle = 45, hjust = 1), #rotate x axis labels
  )
print(Bosmina.Eubosmina)
#dev.off()

