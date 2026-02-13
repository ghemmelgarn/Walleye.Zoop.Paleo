#this script investigates the relationship between fry stocking and zooplankton biomass

library(tidyverse)
library(ggplot2)
library(ggrepel) #to keep text in ggplots from overlapping
library(gridExtra)


#STOCKING DATA--------------------------------------------------------------------------------------------------------------------------------------------------
#Read in data
stock <- read.csv("Data/Input/Result_28.csv")

#Import inclusion table
Incl.Table <- read.csv("Data/Input/LakeYear_Pelagic_Inclusion_Table.csv")

#make separate year and dow columns in the inclusion table
Incl.Table.DOW <- Incl.Table %>% 
  mutate(Year = as.numeric(str_sub(parentdow.year, -4, -1))) %>% 
  mutate(parentdow = str_sub(parentdow.year, 1, -6))

#let's limit this list to just my lakes to see what we are working with here
#get a list of parentdows of my lakes
parentdow <- Incl.Table.DOW %>% 
  select(parentdow) %>% 
  unique()

#add parentdow column to stocking data - MODIFIED FOR THIS DATA
#vermilion and hill sub-basins are listed together (dow ending in 00), so need to modify their parentdows to get it to join to my data
stock.parentdow <- stock %>% 
  mutate(parentdow = case_when(
    (stock$UNIQUE_IDENTIFIER == "1014202" | stock$UNIQUE_IDENTIFIER == "1014201" | stock$UNIQUE_IDENTIFIER == "4003502" | stock$UNIQUE_IDENTIFIER == "4003501") ~ substr(stock$UNIQUE_IDENTIFIER, 1, 7),   #takes care of North and Red lakes (7 characters)
    (stock$UNIQUE_IDENTIFIER == "69037802" | stock$UNIQUE_IDENTIFIER == "69037801") ~ substr(stock$UNIQUE_IDENTIFIER, 1, 8),  #takes care of Vermilion (different because 8 characters)
    (nchar(stock$UNIQUE_IDENTIFIER) == 7 & (stock$UNIQUE_IDENTIFIER != "1014202" & stock$UNIQUE_IDENTIFIER != "1014201" & stock$UNIQUE_IDENTIFIER != "4003502" & stock$UNIQUE_IDENTIFIER != "4003501")) ~ substr(stock$UNIQUE_IDENTIFIER, 1, 5), #this gets 5 digits from the DOWs that have 7 characters and are not those identified before
    (nchar(stock$UNIQUE_IDENTIFIER) == 8 & (stock$UNIQUE_IDENTIFIER != "69037802" & stock$UNIQUE_IDENTIFIER != "69037801")) ~ substr(stock$UNIQUE_IDENTIFIER, 1, 6) #this gets 6 digits from the DOWs that have 8 characters and are not those identified before
  )) %>% 
  #modifies vermilion and hill:
  mutate(parentdow = ifelse(UNIQUE_IDENTIFIER == "1014200", "1014201",
                            ifelse(UNIQUE_IDENTIFIER == "69037800", "69037801", parentdow)
  ))

#isolate my lakes
stock.mylakes <- left_join(parentdow, stock.parentdow, by = "parentdow")

#what species do we have
unique(stock.mylakes$COMMON_NAME)

#what units of measure do we have
unique(stock.mylakes$UNIT_OF_MEASURE)

#what life stages do we have
unique(stock.mylakes$NAME)
# I want fry and fryling (and I guess the lake whitefish eggs in Leech in 2012??)

#which species at which stages?
table(stock.mylakes$COMMON_NAME, stock.mylakes$NAME)
#the fish being stocked as fry and frylings are: walleye, lake whitefish, musky, and northern pike

#filter to the life stages I want
stock.fry <- stock.mylakes %>% 
  filter(NAME == "Fry" | NAME == "Fryling" | NAME == "Egg")

#isolate this to just my lake-years
stock.fry.year <- stock.fry %>%
  rename(Year = STOCKING_YEAR) %>%
  mutate()
stock.lakeyear <- inner_join(Incl.Table.DOW, stock.fry.year, by = c("parentdow", "Year")) #only keep my lake-years that actually had fry stocked



#ZOOP DATA------------------------------------------------------------------------------------------------------------------------------------

#read in the zoop data
zoop <- read.csv("Data/Input/ZoopDB_data_20251016.csv")
#this is the zoop data Kylie sent me on Oct 16, 2025 - it is up to date with what she had processed at that time

#FORMAT AND CLEAN - COPIED FROM "Create Contemporary Dataset" as it was on 2/12/26, then slightly modified to keep only the relevant parts here

#create columns for month, day, and year separately
zoop_months <- zoop %>%
  mutate(year = substr(sample_date, 1, 4)) %>% 
  mutate(month = substr(sample_date, 6, 7)) %>%
  mutate(day = substr(sample_date, 9, 10))

#make parentdow column
zoop_parentdow <- zoop_months %>%
  mutate(parentdow = case_when(
    nchar(zoop$dowlknum) == 7 ~ substr(dowlknum, 1, 5),
    nchar(zoop$dowlknum) == 8 ~ substr(dowlknum, 1, 6)
  ))

#make parentdow.year column to join to other data
zoop_parentdow$parentdow.year = paste(zoop_parentdow$parentdow, zoop_parentdow$year)


#Read in the zoop survey inclusion table with vetted good surveys
zoop_incl <- read.csv("Data/Input/Zoop_Survey_Pelagic_Inclusion_Table.csv")

#get just the zoop surveys in the inclusion table
zoop_filter <- left_join(zoop_incl, zoop_parentdow, by = c("parentdow.year", "sample_id"))

#Check for any lake names that don't match
zoop_filter$NAME.MATCH <- zoop_filter$lake_name == zoop_filter$lake_name.y
table(zoop_filter$NAME.MATCH)
#Looks good

#Fix other sample-level problems:

#Separate Hill, Verm, and Red samples from each other:
zoop_split <- zoop_filter %>% 
  select(-lake_name.y, -NAME.MATCH) %>% #get rid of duplicate / unneccesary columns
  #Hill north and south are already split, just need to fix their parentdows
  mutate(parentdow = ifelse(lake_name == "Hill (north)" | lake_name == "Hill (south)", dowlknum, parentdow)) %>% 
  #Fix vermilion and Red names, also fix a spelling error in Shaokotan
  mutate(lake_name = ifelse(lake_name == "Vermilion" & (site_number == 1 | site_number == 2 | site_number == 3), "East Vermilion",
                            ifelse(lake_name == "Vermilion" & (site_number == 4 | site_number == 5 | site_number == 6), "West Vermilion",
                                   ifelse(lake_name == "Red" & (site_number == 1 | site_number == 2 | site_number == 3), "Red (Lower Red)",
                                          ifelse(lake_name == "Red" & (site_number == 4 | site_number == 5 | site_number == 6), "Red (Upper Red)", 
                                                 ifelse(lake_name == "Shaokatan", "Shaokotan", lake_name)))))) %>% 
  #Fix vermilion and Red parentdows
  mutate(parentdow = ifelse(lake_name == "East Vermilion", "69037801",
                            ifelse(lake_name == "West Vermilion", "69037802",
                                   ifelse(lake_name == "Red (Lower Red)", "4003502",
                                          ifelse(lake_name == "Red (Upper Red)", "4003501", parentdow))))) %>% 
  #recalculate parentdow.year
  mutate(parentdow.year = paste(parentdow, year)) 

#Remove the shallow VNP samples:
zoop_deep <- zoop_split %>% 
  filter(!((lake_name == "Rainy" & site_number == 1) | 
             (lake_name == "Namakan" & site_number == 1) | 
             (lake_name == "Sand Point" & site_number == 1)))

#Make changes based on exploration above and communication with Kylie:
#Remove:
#Duplicate Mille Lacs samples that are known issue in database
#bythotrephes and leptodora 
zoop_nodupes <- zoop_deep %>% 
  filter(!(lake_name == "Mille Lacs" & sample_id >= 4065 & sample_id <= 4072)) %>% 
  filter(species != "Bythotrephes longimanus" & species != "Leptodora kindti")
#ADD TO THIS AS NEEDED AFTER KYLIE RESPONDS TO MY QUESTIONS


#sum biomass, density, and count across all species in each zoop tow, also get weighted average of mean length
zoop_tow <- zoop_nodupes %>% 
  group_by(parentdow.year, parentdow, lake_name, sample_date, year, month, sample_id, site_number) %>% 
  summarize(density = sum(density),
            biomass = sum(biomass),
            mean_length_all =  sum(mean_length*count)/sum(count),
            .groups = 'drop')

#average the metrics from each tow on each day
zoop_daily <- zoop_tow %>% 
  group_by(parentdow.year, parentdow, lake_name, sample_date, year, month) %>% 
  summarize(density = mean(density),
            biomass = mean(biomass),
            mean_length_all = mean (mean_length_all), 
            .groups = 'drop') %>% 
  mutate(Year = as.numeric(year))

#Calculate the cladocercan-only length
clad_tow <- zoop_nodupes %>% 
  filter(grp == "Cladocerans") %>% 
  group_by(parentdow.year, parentdow, lake_name, sample_date, year, month, sample_id, site_number) %>% 
  summarize(mean_length_clad = sum(mean_length*count)/sum(count), .groups = 'drop')

clad_daily <- clad_tow %>% 
  group_by(parentdow.year, parentdow, lake_name, sample_date, year, month) %>% 
  summarize(mean_length_clad = mean (mean_length_clad), 
            .groups = 'drop') %>% 
  mutate(Year = as.numeric(year))

# clad <- zoop_nodupes %>% 
#   group_by(parentdow.year, parentdow, lake_name, sample_date, year, month, sample_id, site_number) %>%  
#   summarize(clad_count = length(grp == "Cladocerans"), 
#             .groups = 'drop')
  
#Abundance ratio of small:big cladocerans (Holopedium are large)
clad_size_daily <- zoop_nodupes %>% 
  filter(grp == "Cladocerans") %>%  
  mutate(size = ifelse(grp2 == "large daphnia", "large", ifelse(grp2 == "Holopedium", "large", "small"))) %>% #simplify large vs. small in size column
  group_by(parentdow.year, parentdow, lake_name, sample_date, year, month, size) %>% 
  summarize(count = sum(count), .groups = 'drop')  #get a count of each size for each lake-year
#convert long to wide
clad_size_wide <- pivot_wider(clad_size_daily, names_from = "size", values_from = "count")
#replace NA counts with 0.001 (close to 0 but finite so I don't get errors later)
clad_size_wide0 <- clad_size_wide %>% 
  mutate(large = ifelse(is.na(large), 1, large),
         small = ifelse(is.na(small), 1, small))
#calculate ratio of small:large cladocerans in each lake-year
clad_ratio <- clad_size_wide0 %>%
  mutate(clad_ratio = small/large) %>%  #I know it would be more intuitive to do large/small so bigger numbers are more bigger cladocerans, but this handles the one zero in the smalls
  select(-small, -large) %>% #get rid the the large and small columns
  mutate(Year = as.numeric(year))

#join this zoop data together
zoop1 <- left_join(zoop_daily, clad_daily, by = c("parentdow.year", "parentdow", "lake_name", "sample_date", "year", "Year", "month"))
zoop2 <- left_join(zoop1, clad_ratio, by = c("parentdow.year", "parentdow", "lake_name", "sample_date", "year", "Year", "month"))

#there are three days with no cladocerans, that's okay leave them as NA

#COMBINE AND PLOT-------------------------------------------------------------------------------------------------------------------------

#isolate the zoop data for the lake-years with fry stocking
stock.zoop <- left_join(zoop2, stock.lakeyear, by = c("parentdow", "Year"))
#We want other years for these lakes that I have in the zoop data, even if there wasn't any stocking for comparison

#Filter to just the lakes with stocking
stock.zoop.lakes <- stock.zoop %>% 
  filter(lake_name.x == "Hill (north)" | lake_name.x == "Leech" | lake_name.x == "Artichoke" | lake_name.x == "Kabetogama" | lake_name.x == "Madison" | lake_name.x == "Freeborn")

#Make a lake name year column
stock.zoop.lakes$lakeyear <- paste0(stock.zoop.lakes$lake_name.x, stock.zoop.lakes$Year)

#format dates correctly
stock.zoop.dates <- stock.zoop.lakes %>% 
  mutate(zoop.date = ymd(sample_date)) %>% 
  mutate(stock.date = mdy(STOCKING_DATE)) %>% 
  mutate(lakeyear = ifelse(lakeyear == "Hill (north)2009", "Hill2009", 
                           ifelse(lakeyear == "Hill (north)2015", "Hill2015", lakeyear)))


#PLOT EACH LAKE-YEAr

#get columns for max and min values by lake for setting y-axes
lakes <- unique(stock.zoop.dates$lake_name.x)
YLims <- data.frame(
  lake_name.x = character(),
  biomass_max = numeric(),
  density_max = numeric(),
  mean_length_all_max = numeric(),
  mean_length_clad_max = numeric(),
  clad_ratio_max = numeric()
)

z <- 0 

for(j in lakes){
  row <- 1 + z
  ylim.data <- stock.zoop.dates %>% 
    filter(lake_name.x == j)
  YLims[row, 1] <- j
  YLims[row, 2] <- max(ylim.data$biomass, na.rm = TRUE)
  YLims[row, 3] <- max(ylim.data$density, na.rm = TRUE)
  YLims[row, 4] <- max(ylim.data$mean_length_all, na.rm = TRUE)
  YLims[row, 5] <- max(ylim.data$mean_length_clad, na.rm = TRUE)
  YLims[row, 6] <- max(ylim.data$clad_ratio, na.rm = TRUE)
  
  z <- z + 1
}

#join y lims to dataset
stock.zoop.plot <- left_join(stock.zoop.dates, YLims, by = "lake_name.x")

#get list of unique lake-years
lakeyears <- unique(stock.zoop.plot$lakeyear)

#Create empty lists to store the plots of each type
Biomass_plots <- list()
Density_plots <- list()
Mean_Length_plots <- list()
Mean_Clad_Length_Plots <- list()
Clad_Ratio_plots <- list()


#BIOMASS----

#use a for loop to make the same plot for each lake-year for BIOMASS
for(i in lakeyears){
  #filter out data for correct lake-year
  plot.data <- stock.zoop.plot %>% 
    filter(lakeyear == i) %>% 
    mutate(zoop.dummy.date = update(zoop.date, year = 2000)) %>% 
    mutate(stock.dummy.date = update(stock.date, year = 2000))#if I make all the dates in the year 2000, I can set consistent axes limits
  
  #get unique stocking events for the lakeyear
  plot.stock.data <- plot.data %>% 
    distinct(stock.dummy.date, .keep_all = TRUE)
  
  #get y max from data for the correct lake
  y_max <- max(plot.data$biomass_max) #these values will be the same so max just gets me the value
  
  #make the plot
  plot <- ggplot(aes(x = zoop.dummy.date, y = biomass), data = plot.data)+
    geom_point()+
    geom_line()+
    labs(y = "Total Zooplankton Biomass", x = "Date", title = paste(i, "Total Zooplankton Biomass"))+
    geom_vline(aes(xintercept = stock.dummy.date), color = "red", linewidth = 1)+
    geom_text_repel(data = plot.stock.data,
                    aes(x = stock.dummy.date, y = Inf, label = paste(COMMON_NAME, NAME)),
              vjust = -0.2,
              hjust = 2,
              angle = 90,
              direction = "y",
              segment.color = NA)+
    scale_x_date(limits = c(as.Date("2000-01-01"), as.Date("2000-09-30")),
                 date_breaks = "1 month",
                 date_labels = "%b")+
    scale_y_continuous(limits = c(0, y_max))+
    theme_classic()
  plot
  
  #save the plot with the lakeyear as its name
  Biomass_plots[[i]] <- plot
}


#DENSITY----

#use a for loop to make the same plot for each lake-year for DENSITY
for(i in lakeyears){
  #filter out data for correct lake-year
  plot.data <- stock.zoop.plot %>% 
    filter(lakeyear == i) %>% 
    mutate(zoop.dummy.date = update(zoop.date, year = 2000)) %>% 
    mutate(stock.dummy.date = update(stock.date, year = 2000))#if I make all the dates in the year 2000, I can set consistent axes limits
  
  #get unique stocking events for the lakeyear
  plot.stock.data <- plot.data %>% 
    distinct(stock.dummy.date, .keep_all = TRUE)
  
  #get y max from data for the correct lake
  y_max <- max(plot.data$density_max) #these values will be the same so max just gets me the value
  
  #make the plot
  plot <- ggplot(aes(x = zoop.dummy.date, y = density), data = plot.data)+
    geom_point()+
    geom_line()+
    labs(y = "Total Zooplankton Density", x = "Date", title = paste(i, "Total Zooplankton Density"))+
    geom_vline(aes(xintercept = stock.dummy.date), color = "red", linewidth = 1)+
    geom_text_repel(data = plot.stock.data,
                    aes(x = stock.dummy.date, y = Inf, label = paste(COMMON_NAME, NAME)),
                    vjust = -0.2,
                    hjust = 2,
                    angle = 90,
                    direction = "y",
                    segment.color = NA)+
    scale_x_date(limits = c(as.Date("2000-01-01"), as.Date("2000-09-30")),
                 date_breaks = "1 month",
                 date_labels = "%b")+
    scale_y_continuous(limits = c(0, y_max))+
    theme_classic()
  plot
  
  #save the plot with the lakeyear as its name
  Density_plots[[i]] <- plot
}



#MEAN LENGTH----

#use a for loop to make the same plot for each lake-year for MEAN LENGTH
for(i in lakeyears){
  #filter out data for correct lake-year
  plot.data <- stock.zoop.plot %>% 
    filter(lakeyear == i) %>% 
    mutate(zoop.dummy.date = update(zoop.date, year = 2000)) %>% 
    mutate(stock.dummy.date = update(stock.date, year = 2000))#if I make all the dates in the year 2000, I can set consistent axes limits
  
  #get unique stocking events for the lakeyear
  plot.stock.data <- plot.data %>% 
    distinct(stock.dummy.date, .keep_all = TRUE)
  
  #get y max from data for the correct lake
  y_max <- max(plot.data$mean_length_all_max) #these values will be the same so max just gets me the value
  
  #make the plot
  plot <- ggplot(aes(x = zoop.dummy.date, y = mean_length_all), data = plot.data)+
    geom_point()+
    geom_line()+
    labs(y = "Zooplankton Mean Length", x = "Date", title = paste(i, "Zooplankton Mean Length"))+
    geom_vline(aes(xintercept = stock.dummy.date), color = "red", linewidth = 1)+
    geom_text_repel(data = plot.stock.data,
                    aes(x = stock.dummy.date, y = Inf, label = paste(COMMON_NAME, NAME)),
                    vjust = -0.2,
                    hjust = 2,
                    angle = 90,
                    direction = "y",
                    segment.color = NA)+
    scale_x_date(limits = c(as.Date("2000-01-01"), as.Date("2000-09-30")),
                 date_breaks = "1 month",
                 date_labels = "%b")+
    scale_y_continuous(limits = c(0, y_max))+
    theme_classic()
  plot
  
  #save the plot with the lakeyear as its name
  Mean_Length_plots[[i]] <- plot
}


#MEAN CLADOCERAN LENGTH----

#use a for loop to make the same plot for each lake-year for MEAN CLADOCERAN LENGTH
for(i in lakeyears){
  #filter out data for correct lake-year
  plot.data <- stock.zoop.plot %>% 
    filter(lakeyear == i) %>% 
    mutate(zoop.dummy.date = update(zoop.date, year = 2000)) %>% 
    mutate(stock.dummy.date = update(stock.date, year = 2000))#if I make all the dates in the year 2000, I can set consistent axes limits
  
  #get unique stocking events for the lakeyear
  plot.stock.data <- plot.data %>% 
    distinct(stock.dummy.date, .keep_all = TRUE)
  
  #get y max from data for the correct lake
  y_max <- max(plot.data$mean_length_clad_max) #these values will be the same so max just gets me the value
  
  #make the plot
  plot <- ggplot(aes(x = zoop.dummy.date, y = mean_length_clad), data = plot.data)+
    geom_point()+
    geom_line()+
    labs(y = "Cladoceran Mean Length", x = "Date", title = paste(i, "Cladoceran Mean Length"))+
    geom_vline(aes(xintercept = stock.dummy.date), color = "red", linewidth = 1)+
    geom_text_repel(data = plot.stock.data,
                    aes(x = stock.dummy.date, y = Inf, label = paste(COMMON_NAME, NAME)),
                    vjust = -0.2,
                    hjust = 2,
                    angle = 90,
                    direction = "y",
                    segment.color = NA)+
    scale_x_date(limits = c(as.Date("2000-01-01"), as.Date("2000-09-30")),
                 date_breaks = "1 month",
                 date_labels = "%b")+
    scale_y_continuous(limits = c(0, y_max))+
    theme_classic()
  plot
  
  #save the plot with the lakeyear as its name
  Mean_Clad_Length_Plots[[i]] <- plot
}


#CLADOCERAN RATIO----

#use a for loop to make the same plot for each lake-year for CLADOCERAN RATIO
for(i in lakeyears){
  #filter out data for correct lake-year
  plot.data <- stock.zoop.plot %>% 
    filter(lakeyear == i) %>% 
    mutate(zoop.dummy.date = update(zoop.date, year = 2000)) %>% 
    mutate(stock.dummy.date = update(stock.date, year = 2000))#if I make all the dates in the year 2000, I can set consistent axes limits
  
  #get unique stocking events for the lakeyear
  plot.stock.data <- plot.data %>% 
    distinct(stock.dummy.date, .keep_all = TRUE)
  
  #get y max from data for the correct lake
  y_max <- max(plot.data$clad_ratio_max) #these values will be the same so max just gets me the value
  
  #make the plot
  plot <- ggplot(aes(x = zoop.dummy.date, y = clad_ratio), data = plot.data)+
    geom_point()+
    geom_line()+
    labs(y = "Ratio Small:Large Cladocerans", x = "Date", title = paste(i, "Cladoceran Ratio"))+
    geom_vline(aes(xintercept = stock.dummy.date), color = "red", linewidth = 1)+
    geom_text_repel(data = plot.stock.data,
                    aes(x = stock.dummy.date, y = Inf, label = paste(COMMON_NAME, NAME)),
                    vjust = -0.2,
                    hjust = 2,
                    angle = 90,
                    direction = "y",
                    segment.color = NA)+
    scale_x_date(limits = c(as.Date("2000-01-01"), as.Date("2000-09-30")),
                 date_breaks = "1 month",
                 date_labels = "%b")+
    scale_y_continuous(limits = c(0, y_max))+
    theme_classic()
  plot
  
  #save the plot with the lakeyear as its name
  Clad_Ratio_plots[[i]] <- plot
}


#OUTPUT LAYOUTS----
#make layouts with all the plots for each lake

#tiff("Zooplankton_Fish_Stocking_Hill.tiff", width = 20, height = 6, units = "in", res = 300)
grid.arrange(Biomass_plots[["Hill2009"]], Density_plots[["Hill2009"]], Mean_Length_plots[["Hill2009"]], Mean_Clad_Length_Plots[["Hill2009"]], Clad_Ratio_plots[["Hill2009"]],
             Biomass_plots[["Hill2015"]], Density_plots[["Hill2015"]], Mean_Length_plots[["Hill2015"]], Mean_Clad_Length_Plots[["Hill2015"]], Clad_Ratio_plots[["Hill2015"]],
             nrow= 2, ncol = 5)
#dev.off()

#tiff("Zooplankton_Fish_Stocking_Leech.tiff", width = 20, height = 33, units = "in", res = 300)
grid.arrange(Biomass_plots[["Leech2012"]], Density_plots[["Leech2012"]], Mean_Length_plots[["Leech2012"]], Mean_Clad_Length_Plots[["Leech2012"]], Clad_Ratio_plots[["Leech2012"]],
             Biomass_plots[["Leech2013"]], Density_plots[["Leech2013"]], Mean_Length_plots[["Leech2013"]], Mean_Clad_Length_Plots[["Leech2013"]], Clad_Ratio_plots[["Leech2013"]],
             Biomass_plots[["Leech2014"]], Density_plots[["Leech2014"]], Mean_Length_plots[["Leech2014"]], Mean_Clad_Length_Plots[["Leech2014"]], Clad_Ratio_plots[["Leech2014"]],
             Biomass_plots[["Leech2015"]], Density_plots[["Leech2015"]], Mean_Length_plots[["Leech2015"]], Mean_Clad_Length_Plots[["Leech2015"]], Clad_Ratio_plots[["Leech2015"]],
             Biomass_plots[["Leech2016"]], Density_plots[["Leech2016"]], Mean_Length_plots[["Leech2016"]], Mean_Clad_Length_Plots[["Leech2016"]], Clad_Ratio_plots[["Leech2016"]],
             Biomass_plots[["Leech2017"]], Density_plots[["Leech2017"]], Mean_Length_plots[["Leech2017"]], Mean_Clad_Length_Plots[["Leech2017"]], Clad_Ratio_plots[["Leech2017"]],
             Biomass_plots[["Leech2019"]], Density_plots[["Leech2019"]], Mean_Length_plots[["Leech2019"]], Mean_Clad_Length_Plots[["Leech2019"]], Clad_Ratio_plots[["Leech2019"]],
             Biomass_plots[["Leech2020"]], Density_plots[["Leech2020"]], Mean_Length_plots[["Leech2020"]], Mean_Clad_Length_Plots[["Leech2020"]], Clad_Ratio_plots[["Leech2020"]],
             Biomass_plots[["Leech2021"]], Density_plots[["Leech2021"]], Mean_Length_plots[["Leech2021"]], Mean_Clad_Length_Plots[["Leech2021"]], Clad_Ratio_plots[["Leech2021"]],
             Biomass_plots[["Leech2022"]], Density_plots[["Leech2022"]], Mean_Length_plots[["Leech2022"]], Mean_Clad_Length_Plots[["Leech2022"]], Clad_Ratio_plots[["Leech2022"]],
             Biomass_plots[["Leech2024"]], Density_plots[["Leech2024"]], Mean_Length_plots[["Leech2024"]], Mean_Clad_Length_Plots[["Leech2024"]], Clad_Ratio_plots[["Leech2024"]],
             nrow= 11, ncol = 5)
#dev.off()

#tiff("Zooplankton_Fish_Stocking_Freeborn.tiff", width = 20, height = 3, units = "in", res = 300)
grid.arrange(Biomass_plots[["Freeborn1999"]], Density_plots[["Freeborn1999"]], Mean_Length_plots[["Freeborn1999"]], Mean_Clad_Length_Plots[["Freeborn1999"]], Clad_Ratio_plots[["Freeborn1999"]],
             nrow= 1, ncol = 5)
#dev.off()

#tiff("Zooplankton_Fish_Stocking_Artichoke.tiff", width = 20, height = 6, units = "in", res = 300)
grid.arrange(Biomass_plots[["Artichoke2008"]], Density_plots[["Artichoke2008"]], Mean_Length_plots[["Artichoke2008"]], Mean_Clad_Length_Plots[["Artichoke2008"]], Clad_Ratio_plots[["Artichoke2008"]],
             Biomass_plots[["Artichoke2009"]], Density_plots[["Artichoke2009"]], Mean_Length_plots[["Artichoke2009"]], Mean_Clad_Length_Plots[["Artichoke2009"]], Clad_Ratio_plots[["Artichoke2009"]],
             nrow= 2, ncol = 5)
#dev.off()

#tiff("Zooplankton_Fish_Stocking_Madison.tiff", width = 20, height = 6, units = "in", res = 300)
grid.arrange(Biomass_plots[["Madison2008"]], Density_plots[["Madison2008"]], Mean_Length_plots[["Madison2008"]], Mean_Clad_Length_Plots[["Madison2008"]], Clad_Ratio_plots[["Madison2008"]],
             Biomass_plots[["Madison2019"]], Density_plots[["Madison2019"]], Mean_Length_plots[["Madison2019"]], Mean_Clad_Length_Plots[["Madison2019"]], Clad_Ratio_plots[["Madison2019"]],
             nrow= 2, ncol = 5)
#dev.off()

#tiff("Zooplankton_Fish_Stocking_Kabetogama.tiff", width = 20, height = 36, units = "in", res = 300)
grid.arrange(Biomass_plots[["Kabetogama2007"]], Density_plots[["Kabetogama2007"]], Mean_Length_plots[["Kabetogama2007"]], Mean_Clad_Length_Plots[["Kabetogama2007"]], Clad_Ratio_plots[["Kabetogama2007"]],
             Biomass_plots[["Kabetogama2008"]], Density_plots[["Kabetogama2008"]], Mean_Length_plots[["Kabetogama2008"]], Mean_Clad_Length_Plots[["Kabetogama2008"]], Clad_Ratio_plots[["Kabetogama2008"]],
             Biomass_plots[["Kabetogama2009"]], Density_plots[["Kabetogama2009"]], Mean_Length_plots[["Kabetogama2009"]], Mean_Clad_Length_Plots[["Kabetogama2009"]], Clad_Ratio_plots[["Kabetogama2009"]],
             Biomass_plots[["Kabetogama2010"]], Density_plots[["Kabetogama2010"]], Mean_Length_plots[["Kabetogama2010"]], Mean_Clad_Length_Plots[["Kabetogama2010"]], Clad_Ratio_plots[["Kabetogama2010"]],
             Biomass_plots[["Kabetogama2013"]], Density_plots[["Kabetogama2013"]], Mean_Length_plots[["Kabetogama2013"]], Mean_Clad_Length_Plots[["Kabetogama2013"]], Clad_Ratio_plots[["Kabetogama2013"]],
             Biomass_plots[["Kabetogama2014"]], Density_plots[["Kabetogama2014"]], Mean_Length_plots[["Kabetogama2014"]], Mean_Clad_Length_Plots[["Kabetogama2014"]], Clad_Ratio_plots[["Kabetogama2014"]],
             Biomass_plots[["Kabetogama2015"]], Density_plots[["Kabetogama2015"]], Mean_Length_plots[["Kabetogama2015"]], Mean_Clad_Length_Plots[["Kabetogama2015"]], Clad_Ratio_plots[["Kabetogama2015"]],
             Biomass_plots[["Kabetogama2017"]], Density_plots[["Kabetogama2017"]], Mean_Length_plots[["Kabetogama2017"]], Mean_Clad_Length_Plots[["Kabetogama2017"]], Clad_Ratio_plots[["Kabetogama2017"]],
             Biomass_plots[["Kabetogama2018"]], Density_plots[["Kabetogama2018"]], Mean_Length_plots[["Kabetogama2018"]], Mean_Clad_Length_Plots[["Kabetogama2018"]], Clad_Ratio_plots[["Kabetogama2018"]],
             Biomass_plots[["Kabetogama2019"]], Density_plots[["Kabetogama2019"]], Mean_Length_plots[["Kabetogama2019"]], Mean_Clad_Length_Plots[["Kabetogama2019"]], Clad_Ratio_plots[["Kabetogama2019"]],
             Biomass_plots[["Kabetogama2021"]], Density_plots[["Kabetogama2021"]], Mean_Length_plots[["Kabetogama2021"]], Mean_Clad_Length_Plots[["Kabetogama2021"]], Clad_Ratio_plots[["Kabetogama2021"]],
             Biomass_plots[["Kabetogama2024"]], Density_plots[["Kabetogama2024"]], Mean_Length_plots[["Kabetogama2024"]], Mean_Clad_Length_Plots[["Kabetogama2024"]], Clad_Ratio_plots[["Kabetogama2024"]],
             nrow= 12, ncol = 5)
#dev.off()
