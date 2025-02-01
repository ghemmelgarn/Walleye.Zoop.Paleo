#Code copied from "Create Eploratory Dataset" and modified

#Starts with list of lakes/years with zoop data for 4+ tows/year
#filters to only lakes with exact match for fish/zoop sampling years
#calculates preliminary zoop metrics (Bytho and Lepto excluded) and joins them
#Formats and joins walleye cpue, mean summer secchi, water temp (degree days), and area data which are downloaded and calculated in other R scripts
#Downloads and joins invasive species information from DNR infested waters list
#Filters out to just complete and best data - see description at end


#packages
library(tidyverse)
library(readxl)
library(ggplot2)
library(cowplot)
library(scales)
library(readr)
library(dplyr)
library(vegan)

#Starting datset known issues:
#I made it manually in google sheets
#Inclusive to all potential data that may or may not be used due to various issues
#Has same fish data matched to multiple zoop years - only for +/- 1 year matches
#Includes data that disagrees between lakefinder and our spreadsheet
#no info on months of zoop data
#No disturbance info
#has walleye gillnet CPUE that should be overwritten when fish data joined in

#Import starting dataset
StartData <- read.csv("Data/Input/WZ_Lake_Selection (1).csv")

#filter to only lake/years with exact fish/zoop matches and at least 6 zoop tows
MatchData <- filter(StartData, ZoopTows >= 6, Match == "Exact")
#end up with only 282 rows instead of 300 like fish inclusion table because of the limitation on zoop tows here

#create parent dow column in MatchData to deal with the fact that Hill lake zoop data is separated by basin
#take first 5 numbers if missing leading 0, first 6 numbers if full 8-digit dow
MatchData_parentdow <- MatchData %>%
  mutate(parentdow = case_when(
    nchar(MatchData$DOW) == 7 ~ substr(DOW, 1, 5),
    nchar(MatchData$DOW) == 8 ~ substr(DOW, 1, 6)
  ))

#create parentdow.year columns to allow joining fish and zoop cpue data
#need to make these separate columns for cases when fish and zoop data are not the same year
MatchData_parentdow$parentdow.fish.year = paste(MatchData_parentdow$parentdow, MatchData_parentdow$FishYear)
MatchData_parentdow$parentdow.zoop.year = paste(MatchData_parentdow$parentdow, MatchData_parentdow$ZoopYear)

#remove unneeded data frames to keep environment clean
rm(StartData, MatchData)

#remove Hill lake south as a row from the dataset - the zoops will get summarized appropriately by parentdow and the fish will too
MatchData_parentdow <- filter(MatchData_parentdow, LakeName != "Hill (south)")

#ZOOP METRIC JOIN -------------------------------------------------------------------------

#read data
zoop <- read.csv("Data/Input/ZoopDB_data_20241004.csv")

#make parentdow column
zoop_parentdow <- zoop %>%
  mutate(parentdow = case_when(
    nchar(zoop$dowlknum) == 7 ~ substr(dowlknum, 1, 5),
    nchar(zoop$dowlknum) == 8 ~ substr(dowlknum, 1, 6)
  ))

#make parentdow.zoop.year column to join to master datasheet
zoop_parentdow$parentdow.zoop.year = paste(zoop_parentdow$parentdow, zoop_parentdow$year)

#remove bythotrephes and leptodora from the zoop data - discuss with Kylie if I want to include these taxa and get data separately in the future
zoop_parentdow <- filter(zoop_parentdow, species != "Bythotrephes longimanus" & species != "Leptodora kindti")

#with zoops we know we have many replicates within a year - we need to decide what to do with them
#For now, community compositon: mean total biomass across all sampling dates within a year, proportion daphnia biomass of cladoceran biomass, percent cladoceran biomass of all zoops, shannon diversity index, 
#For now, size structure: mean individual size, and proportion (relative abundance) of large vs. small cladocerans

#summarize data to calculate means of biomass at the 3 grouping levels and convert to wide with a prefix that specifies group level
#will use these in metric calculations
#first sum the biomass for each group on each sampling date in each lake/year
zoop_grp_sample_summary <- zoop_parentdow %>%
  group_by(parentdow.zoop.year, sample_date, grp) %>%
  summarize(biomass = sum(biomass), .groups = 'drop')
#now take mean over all the sampling dates in a year and calculate mean total zoop biomass
zoop_grp_summary <- zoop_grp_sample_summary %>%
  group_by(parentdow.zoop.year, grp) %>%
  summarize(biomass = mean(biomass), .groups = 'drop')
#convert long to wide
zoop_grp_wide<- pivot_wider(zoop_grp_summary, names_from = "grp", names_prefix = "zoop.grp.biom.", values_from = "biomass")

#repeat for group 2 level
zoop_grp2_sample_summary <- zoop_parentdow %>%
  group_by(parentdow.zoop.year, sample_date, grp2) %>%
  summarize(biomass = sum(biomass), .groups = 'drop')
zoop_grp2_summary <- zoop_grp2_sample_summary %>%
  group_by(parentdow.zoop.year, grp2) %>%
  summarize(biomass = mean(biomass), .groups = 'drop')
zoop_grp2_wide<- pivot_wider(zoop_grp2_summary, names_from = "grp2", names_prefix = "zoop.grp2.biom.", values_from = "biomass")

#repeat for species level
#no need to sum because only one row per species per sample date already
zoop_species_summary <- zoop_parentdow %>%
  group_by(parentdow.zoop.year, species) %>%
  summarize(biomass = mean(biomass), .groups = 'drop')
zoop_species_wide<- pivot_wider(zoop_species_summary, names_from = "species", names_prefix = "zoop.spp.biom.", values_from = "biomass")

#counts the number of months that were sampled for each lake/year
zoop_months <- zoop_parentdow %>%
  group_by(parentdow.zoop.year) %>%
  summarize(Zoop.Month.Count = n_distinct(month), .groups = 'drop')

#join the 4 zoop tables together
zoop_grp_month <- left_join(zoop_months, zoop_grp_wide, by = "parentdow.zoop.year")
zoop_grp_and_grp2 <- left_join(zoop_grp_month, zoop_grp2_wide, by = "parentdow.zoop.year")
zoop_all_grp_biomass <- left_join(zoop_grp_and_grp2, zoop_species_wide, by = "parentdow.zoop.year")
#change all NA values to 0 for any groups/species not observed
zoop_all_grp_biomass <- zoop_all_grp_biomass %>%
  mutate_all(~replace(., is.na(.), 0))

# #write .csv with this file of zoop biomass at all taxonomic levels
# write.csv(zoop_all_grp_biomass, file = "Data/Output/Zoop_Biomass_grp_grp2_Spp.csv")

#calculate biomass metrics, biomass unit = ug/L
#need to calculate each metric for each sample date and then average the stat across the sampling dates - if not some proportions end up greater than 1


#mean total biomass per lake/year
#calculate total biomass by adding cladoceran and copepod biomass on each sampling date
zoop_sample_total_biomass <- zoop_grp_sample_summary %>%
  group_by(parentdow.zoop.year, sample_date) %>%
  summarize(zoop.total.biomass = sum(biomass), .groups = 'drop')
#average this total biomass across sampling dates
zoop_total_biomass <- zoop_sample_total_biomass %>%
  group_by(parentdow.zoop.year) %>%
  summarize(zoop.total.biomass = mean(zoop.total.biomass), .groups = 'drop')

#proportion Daphnia biomass of cladocerans
#filter out just daphnia from original dataset
zoop_daphnia <- filter(zoop_parentdow, str_starts(species, "Daphnia"))
#sum the biomass of each daphnia species for each sample date
zoop_daphnia_sample_biomass <- zoop_daphnia %>%
  group_by(parentdow.zoop.year, sample_date) %>%
  summarize(daphnia.biomass = sum(biomass), .groups = 'drop')
#Create a dataframe with just cladoceran biomass, using what you calculated above
zoop_clad_biomass <- filter(zoop_grp_sample_summary, grp == "Cladocerans")
#rename cladoceran biomass to be descriptive
zoop_clad_biomass <- zoop_clad_biomass %>%
  rename(cladoceran.biomass = biomass)
#create join column in both datasets - this is the parentdow + the sample date
zoop_daphnia_sample_biomass$join = paste(zoop_daphnia_sample_biomass$parentdow.zoop.year, zoop_daphnia_sample_biomass$sample_date)
zoop_clad_biomass$join = paste(zoop_clad_biomass$parentdow.zoop.year, zoop_clad_biomass$sample_date)
#check that all samples with cladocerans have daphia (they may not)
all(zoop_clad_biomass$join %in% zoop_daphnia_sample_biomass$join)
#they don't
#check that all samples with daphnia have cladocerans (they should if I did this right)
all(zoop_daphnia_sample_biomass$join %in% zoop_clad_biomass$join)
#they do - good
#join cladoceran biomass to daphnia biomass - both data frames are average biomass within each sampling date
#I need to left join all to preserve all cladocera rows even when no daphnia
zoop_daph_clad_biomasss <- left_join(zoop_clad_biomass, zoop_daphnia_sample_biomass, by = "join")
#change NA to 0 when daphnia not present
zoop_daph_clad_biomasss$daphnia.biomass <- replace(zoop_daph_clad_biomasss$daphnia.biomass, is.na(zoop_daph_clad_biomasss$daphnia.biomass), 0)
#calculate proportion daphnia of cladocerans for each sample date
zoop_daph_clad_biomasss$Daphnia.prop.Cladoceran.biomass = zoop_daph_clad_biomasss$daphnia.biomass / zoop_daph_clad_biomasss$cladoceran.biomass
#average the proportion across sample dates
zoop_daph_prop <- zoop_daph_clad_biomasss %>%
  group_by(parentdow.zoop.year.x) %>%
  summarize(Daphnia.prop.Cladoceran.biomass = mean(Daphnia.prop.Cladoceran.biomass), .groups = 'drop')
#rename parentdow.zoop.year for future joins because it got funky
zoop_daph_prop <- zoop_daph_prop %>%
  rename(parentdow.zoop.year = parentdow.zoop.year.x)


#proportion cladoceran biomass of total biomass
#create join column in the data frame already made for total biomass by sampling date
zoop_sample_total_biomass$join = paste(zoop_sample_total_biomass$parentdow.zoop.year, zoop_sample_total_biomass$sample_date)
#check that all samples with total biomass have cladocerans (they may not)
all(zoop_sample_total_biomass$join %in% zoop_clad_biomass$join)
#they don't
#check that all samples with cladocerans have total biomass (they should if I did this right)
all(zoop_clad_biomass$join %in% zoop_sample_total_biomass$join)
#they do - good
#need left join
zoop_clad_total_biomasss <- left_join(zoop_sample_total_biomass, zoop_clad_biomass, by = "join")
#change NA to 0 when cladocerans not present
zoop_clad_total_biomasss$cladoceran.biomass <- replace(zoop_clad_total_biomasss$cladoceran.biomass, is.na(zoop_clad_total_biomasss$cladoceran.biomass), 0)
#calcualte proportion cladoceran of total biomass
zoop_clad_total_biomasss$prop.Cladoceran.biomass = zoop_clad_total_biomasss$cladoceran.biomass / zoop_clad_total_biomasss$zoop.total.biomass
#average the proportion across sample dates
zoop_clad_prop <- zoop_clad_total_biomasss %>%
  group_by(parentdow.zoop.year.x) %>%
  summarize(prop.Cladoceran.biomass = mean(prop.Cladoceran.biomass), .groups = 'drop')
#rename parentdow.zoop.year for future joins because it got funky
zoop_clad_prop <- zoop_clad_prop %>%
  rename(parentdow.zoop.year = parentdow.zoop.year.x)



#join the metrics to the zoop biomass table - full join to preserve all columns, rows should be same but did this just in case
zoop_biomass_a <- full_join(zoop_total_biomass, zoop_all_grp_biomass, by = "parentdow.zoop.year")
zoop_biomass_b <- full_join(zoop_daph_prop, zoop_biomass_a, by = "parentdow.zoop.year")
zoop_all_biomass <- full_join(zoop_clad_prop, zoop_biomass_b, by = "parentdow.zoop.year")


#filter out just the biomass metric columns you want
#modify this later to include the biomass of each species for multivariate analyses: add starts_with("spp.") as the last argument below
zoop_biomass_metrics <- dplyr::select(zoop_all_biomass, 
                               parentdow.zoop.year, 
                               Zoop.Month.Count, 
                               zoop.total.biomass, 
                               Daphnia.prop.Cladoceran.biomass, 
                               prop.Cladoceran.biomass)

#calculate average individual length of all zoops, unit = mm
#weighted average of mean length on each sampling date (mean length of each species weighted by count of that species)
zoop_length_sample <- zoop_parentdow %>%
  group_by(parentdow.zoop.year, sample_date) %>%
  summarize(mean_length = sum(mean_length*count)/sum(count), .groups = 'drop')
#now take mean over all the sampling dates in a year
zoop_length <- zoop_length_sample %>%
  group_by(parentdow.zoop.year) %>%
  summarize(zoop.mean.length = mean(mean_length), .groups = 'drop')

#calculate average individual length of just cladocerans, unit = mm
#filter just the cladocerans out of original zoop data import
zoop_cladoceran <- filter(zoop_parentdow, grp == "Cladocerans")
#weighted average of mean length on each sampling date (mean length of each species weighted by count of that species)
zoop_length_clad_sample <- zoop_cladoceran %>%
  group_by(parentdow.zoop.year, sample_date) %>%
  summarize(cladoceran.mean.length = sum(mean_length*count)/sum(count), .groups = 'drop')
#now take mean over all the sampling dates in a year
zoop_length_clad <- zoop_length_clad_sample %>%
  group_by(parentdow.zoop.year) %>%
  summarize(cladoceran.mean.length = mean(cladoceran.mean.length), .groups = 'drop')

#calculate average individual length of just Bosminidae, unit = mm
zoop_bosmina <- filter(zoop_parentdow, species == "Bosmina sp." | species == "Bosmina longirostris" | species == "Eubosmina coregoni")
#weighted average of mean length on each sampling date (mean length of each species weighted by count of that species)
zoop_length_bosmina_sample <- zoop_bosmina %>%
  group_by(parentdow.zoop.year, sample_date) %>%
  summarize(bosmina.mean.length = sum(mean_length*count)/sum(count), .groups = 'drop')
#now take mean over all the sampling dates in a year
zoop_length_bosmina <- zoop_length_bosmina_sample %>%
  group_by(parentdow.zoop.year) %>%
  summarize(bosmina.mean.length = mean(bosmina.mean.length), .groups = 'drop')

#calculate average individual length of just Chydoridae, subfamily Aloninae (no Eurycercus), unit = mm
#based on what has preserved carapaces
zoop_chydorid <- filter(zoop_parentdow, species == "Alona sp." | species == "Alona setulosa" | species == "Chydoridae" | species == "Acroperus harpae" | species == "Camptocercus sp." | species == "Chydorus sp." | species == "Chydorus sphaericus" | species == "Alonella sp." | species == "Graptoleberis sp." | species == "Alona quadrangularis")
#weighted average of mean length on each sampling date (mean length of each species weighted by count of that species)
zoop_length_chydorid_sample <- zoop_chydorid %>%
  group_by(parentdow.zoop.year, sample_date) %>%
  summarize(chydorid.mean.length = sum(mean_length*count)/sum(count), .groups = 'drop')
#now take mean over all the sampling dates in a year
zoop_length_chydorid <- zoop_length_chydorid_sample %>%
  group_by(parentdow.zoop.year) %>%
  summarize(chydorid.mean.length = mean(chydorid.mean.length), .groups = 'drop')


#join the various length metrics
zoop_length_a <- left_join(zoop_length, zoop_length_clad, by = "parentdow.zoop.year")
zoop_length_b <- left_join(zoop_length_a, zoop_length_bosmina, by = "parentdow.zoop.year")
zoop_length_all <- left_join(zoop_length_b, zoop_length_chydorid, by = "parentdow.zoop.year")

#calculate relative abundance of large vs. small cladocerans
#remove dapnia sp. with no size assigned, remember bythotrephes and leptodora already removed from data
zoop_cladoceran_noSp. <- filter(zoop_cladoceran, species != "Daphnia sp.")
#create a column for large vs. small cladocerans
#large are those in the large daphnia group and the holopedium
#small is everything else
zoop_cladoceran_size <- zoop_cladoceran_noSp. %>%
  mutate(size = ifelse(grp2 == "large daphnia", "large", ifelse(grp2 == "Holopedium", "large", "small")))
#summarize abundance (sum of count) of large and small on each sample date
zoop_size_sample_abundance <- zoop_cladoceran_size %>%
  group_by(parentdow.zoop.year, sample_date, size) %>%
  summarize(count = sum(count), .groups = 'drop')
#convert long to wide
zoop_size_sample_abundance_wide<- pivot_wider(zoop_size_sample_abundance, names_from = "size", names_prefix = "count.cladoceran.", values_from = "count")
#replace NA counts with 0
zoop_size_sample_abundance_wide$count.cladoceran.large <- replace(zoop_size_sample_abundance_wide$count.cladoceran.large, is.na(zoop_size_sample_abundance_wide$count.cladoceran.large), 0) 
zoop_size_sample_abundance_wide$count.cladoceran.small <- replace(zoop_size_sample_abundance_wide$count.cladoceran.small, is.na(zoop_size_sample_abundance_wide$count.cladoceran.small), 0) 
#calculate proportion of large cladocerans on each sampling date
zoop_size_sample_abundance_wide <- zoop_size_sample_abundance_wide %>%
  mutate(prop.large.cladoceran = count.cladoceran.large/(count.cladoceran.large + count.cladoceran.small))
#now take mean over all the sampling dates in a year
zoop_Prop_large_clad <- zoop_size_sample_abundance_wide %>%
  group_by(parentdow.zoop.year) %>%
  summarize(prop.large.cladoceran = mean(prop.large.cladoceran), .groups = 'drop')

#calculate diversity metrics
#first need to make sure we don't have duplicates where anything that ends in "sp." or have family names doesn't also have another entry with a species name
#zoop_diversity_species <- zoop_parentdow %>%
  #mutate(species = ifelse(species == ))
  #filter(zoop_parentdow, species == "Alona sp." 
#NOT SURE HOW TO DO THIS WITHOUT LOSING DATA - NEED TO ASK GRETCHEN, COME BACK TO IT
#EMAILED KYLIE AND HEIDI

#see how many rows of each unique species I have
table(zoop_parentdow$species)

#calculate shannon diversity index on each sampling date


zoop_SDI_sample <- zoop_parentdow %>%
  group_by(parentdow.zoop.year, sample_date) %>%
  summarize(zoop.Shannon.DI = diversity(count, index = "shannon"), .groups = 'drop')
#now take mean over all the sampling dates in a year
zoop_SDI <- zoop_SDI_sample %>%
  group_by(parentdow.zoop.year) %>%
  summarize(zoop.Shannon.DI = mean(zoop.Shannon.DI), .groups = 'drop')

#join all the zoop metrics together
zoop_a <- left_join(zoop_biomass_metrics, zoop_length_all, by = "parentdow.zoop.year")
zoop_b <- left_join(zoop_a, zoop_Prop_large_clad, by = "parentdow.zoop.year")
zoop_AllMetrics <- left_join(zoop_b, zoop_SDI, by = "parentdow.zoop.year")

# #write .csv with this file of all the preliminary zoop metrics
# write.csv(zoop_AllMetrics, file = "Data/Output/Zoop_Preliminary_Metrics.csv")

#join the relevant zoop metrics to the fish data
Data_a <- left_join(MatchData_parentdow, zoop_AllMetrics, by = "parentdow.zoop.year")

#remove unneeded intermediate data frames to keep environment clean
rm(zoop,
   zoop_a,
   zoop_all_biomass,
   zoop_all_grp_biomass,
   zoop_AllMetrics,
   zoop_b,
   zoop_biomass_a,
   zoop_biomass_b,
   zoop_biomass_metrics,
   zoop_clad_biomass,
   zoop_clad_prop,
   zoop_clad_total_biomasss,
   zoop_cladoceran,
   zoop_cladoceran_noSp.,
   zoop_cladoceran_size,
   zoop_daph_clad_biomasss,
   zoop_daph_prop,
   zoop_daphnia,
   zoop_daphnia_sample_biomass,
   zoop_grp_and_grp2,
   zoop_grp_month,
   zoop_grp_sample_summary,
   zoop_grp2_sample_summary,
   zoop_grp_summary,
   zoop_grp_wide,
   zoop_grp2_sample_summary,
   zoop_grp2_summary,
   zoop_grp2_wide,
   zoop_length,
   zoop_length_all,
   zoop_length_clad,
   zoop_length_clad_sample,
   zoop_length_sample,
   zoop_months,
   zoop_parentdow,
   zoop_Prop_large_clad,
   zoop_sample_total_biomass,
   zoop_SDI,
   zoop_SDI_sample,
   zoop_size_sample_abundance,
   zoop_size_sample_abundance_wide,
   zoop_species_summary,
   zoop_species_wide,
   zoop_total_biomass,
   zoop_bosmina,
   zoop_chydorid,
   zoop_length_a,
   zoop_length_b,
   zoop_length_bosmina,
   zoop_length_bosmina_sample,
   zoop_length_chydorid,
   zoop_length_chydorid_sample
   )





#FISH CPUE JOIN - Only walleye CPUE for now... :) ----------------------------------------------------------------

#read in fish metrics that I downloaded from fish database, quality checked, and calculated the metrics in other scripts
fish <- read.csv("Data/Input/Walleye_CPUE.csv")


#make parentdow column
fish_parentdow <- fish %>%
  mutate(parentdow = case_when(
    nchar(fish$lake_id) == 7 ~ substr(lake_id, 1, 5),
    nchar(fish$lake_id) == 8 ~ substr(lake_id, 1, 6)
  ))

#make parentdow.fish.year column to join to master datasheet
fish_parentdow$parentdow.fish.year = paste(fish_parentdow$parentdow, fish_parentdow$year)

#filter just the fish columns to join
fish.join <- fish_parentdow %>%
  select(parentdow.fish.year, walleye_count, WAE.CPUE, LMB_count, LMB.CPUE, fish.effort.sufficient)

#join walleye CPUE to dataset 
#full_join will retain all rows in both initial tables and create NA values when the other table doesn't have info for that lake/year
Data_b <- left_join(Data_a, fish.join, by = "parentdow.fish.year")

#add "no" when fish effort not sufficient instead of NA
Data_b <- Data_b %>%
  mutate(fish.effort.sufficient = if_else(is.na(fish.effort.sufficient), "no", "yes"))

#remove unneeded intermediate data frames to keep environment clean
rm(fish,
   fish_parentdow,
   fish.join
   )


#A VERSION OF THE FISH DATA ALLOWING EFFORT TO BE 3 NET NIGHTS LESS THAN REQUIRED MINIMUM

fish.inclusive <- read.csv("Data/Input/Walleye_CPUE.inclusive.csv")

#make parentdow column
fish_parentdow.inc <- fish.inclusive %>%
  mutate(parentdow = case_when(
    nchar(fish.inclusive$lake_id) == 7 ~ substr(lake_id, 1, 5),
    nchar(fish.inclusive$lake_id) == 8 ~ substr(lake_id, 1, 6)
  ))

#make parentdow.fish.year column to join to master datasheet
fish_parentdow.inc$parentdow.fish.year = paste(fish_parentdow.inc$parentdow, fish_parentdow.inc$year)

#filter just the fish columns to join
fish.join.inc <- fish_parentdow.inc %>%
  select(parentdow.fish.year, walleye_count, WAE.CPUE, LMB_count, LMB.CPUE)

#rename the walleye CPUE and count columns to show that they are the inclusive version
fish.join.inc <- fish.join.inc %>%
  rename(walleye_count_inc = walleye_count,
         WAE.CPUE.inc = WAE.CPUE,
         LMB_count_inc = LMB_count,
         LMB.CPUE.inc = LMB.CPUE
         )

#join walleye CPUE to dataset 
#full_join will retain all rows in both initial tables and create NA values when the other table doesn't have info for that lake/year
Data_b2 <- left_join(Data_b, fish.join.inc, by = "parentdow.fish.year")



#remove unneeded intermediate data frames to keep environment clean
rm(fish.inclusive,
   fish_parentdow.inc,
   fish.join.inc
)
   
   
   
   
   
#TEMP DATA ---------------------------------------------------------------------

#read in degree days for water temp that Gretchen already calculated and connected to DOWs
temp.data <- read.csv("Data/Input/MN_lake_Gdds_all_years.csv")

#create parentdow column
temp_parentdow <- temp.data %>%
  mutate(parentdow = case_when(
    nchar(temp.data$DOW) == 7 ~ substr(DOW, 1, 5),
    nchar(temp.data$DOW) == 8 ~ substr(DOW, 1, 6)
  ))

#create parentdow.fish.year for join
temp_parentdow$parentdow.fish.year = paste(temp_parentdow$parentdow, temp_parentdow$year)

#there are sub-basins for some lakes in this temp data, so we get multiple parentdow rows when joining is attempted
#need to condense into one row per parentdow
# #are the degree days different between subbasins of the same lake in the same year? check number of unique values:
# temp.subbasin.check <- temp_parentdow %>%
#   group_by(parentdow.fish.year) %>%
#   summarise(num_unique_values = n_distinct(gdd_wtr_5c))
# #there are some lake-years with different values in subbasins
# #join this to the MatchData original inclusion table to see if this is an issue for my lakes
# temp.check.2 <- left_join(MatchData_parentdow, temp.subbasin.check, by = "parentdow.fish.year")
# unique(temp.check.3$num_unique_values)

#it is an issue, but only for 4 lakes, corrected here:

#Minnetonka has 4 unique values - this makes sense, it is a huge lake with many sub-basins - fish data came from the non-divided 27013300 DOW so restrict temp to this
temp.dow.correction <- temp_parentdow %>%
  filter(DOW != 27013301 & 
           DOW != 27013302 & 
           DOW != 27013303 & 
           DOW != 27013304 &
           DOW != 27013305 &
           DOW != 27013309 &
           DOW != 27013310 &
           DOW != 27013311 &
           DOW != 27013312 &
           DOW != 27013313 &
           DOW != 27013314 &
           DOW != 27013315 
           )
#Red lake has upper and lower, fish data here came from upper basin (4003501), so restrict to this
temp.dow.correction <- temp.dow.correction %>%
  filter(DOW != 4003500 & 
           DOW != 4003502
          )
#Hill lake fish data came from lake without subbasin: 1014200, restrict to this
temp.dow.correction <- temp.dow.correction %>%
  filter(DOW != 1014201 & 
           DOW != 1014202
  )

#Cut Foot Sioux lake fish data came from lake without subbasin: 31085700, restrict to this
temp.dow.correction <- temp.dow.correction %>%
  filter(DOW != 31085701 & 
           DOW != 31085702 &
           DOW != 31085703
  )

# #check that this correction worked:
# temp.subbasin.check2 <- temp.dow.correction %>%
#   group_by(parentdow.fish.year) %>%
#   summarise(num_unique_values = n_distinct(gdd_wtr_5c))
# temp.check.3 <- left_join(MatchData_parentdow, temp.subbasin.check2, by = "parentdow.fish.year")
# unique(temp.check.3$num_unique_values)
# #it worked

#there are some lakes without temp data but that's ok for now, some of these are boundary waters
#summarize to get only one observation per parentdow.fish.year when there are multiple equivalent degree day values (all the differing ones have already been dealt with individually)
#use this to also select just the columns you want to join
temp.join <- temp.dow.correction %>%
  group_by(parentdow.fish.year) %>%
  summarize(gdd_wtr_5c = mean(gdd_wtr_5c), .groups = 'drop')


#join to the rest of the preliminary data
Data_c <- left_join(Data_b2, temp.join, by = "parentdow.fish.year")

#remove unneeded intermediate data frames to keep environment clean
rm(temp_parentdow,
   temp.check.2,
   temp.check.3,
   temp.subbasin.check,
   temp.subbasin.check2,
   temp.data,
   temp.dow.correction,
   temp.join,
   test
)





#PRODUCTIVITY DATA ------------------------------------------------------------------------------------------------------

#import selected data
secchi.data <- read.csv("Data/Input/Selected_Secchi_Data.csv")

#summarize the mean of the selected secchi data for each lake/year
secchi.mean <- secchi.data %>%
  group_by(parentdow, year) %>%
  summarize(mean.summer.secchi.meters = mean(secchi_meters), .groups = 'drop')
#note that some lake/years are missing because not enough data for them (or none at all)

#create parentdow.fish.year for join
secchi.mean$parentdow.fish.year = paste(secchi.mean$parentdow, secchi.mean$year)

#remove the parentdow and year columns so they don't get confusing and duplicated
secchi.mean <- secchi.mean %>%
  select(parentdow.fish.year, mean.summer.secchi.meters)

#join to the rest of the preliminary data
Data_d <- left_join(Data_c, secchi.mean, by = "parentdow.fish.year")

#remove unneeded intermediate data frames to keep environment clean
rm(secchi.data,
   secchi.mean
)








#LAKE AREA DATA - also brings coordinates and nhdhr-id ----------------------------------------------------------

#this came from the fish database
#import selected data
area.data <- read.csv("Data/Input/Selected_Lakes_Location_Area.csv")
#already has parentdow.fish.year to join
#select just the columns you want to add
area.data.select <- area.data %>%
  select(parentdow.fish.year, 
         nhdhr_id, 
         latitude_lake_centroid,
         longitude_lake_centroid,
         lakesize,
         lakesize_units
         )
#join to the rest of the preliminary data
Data_e <- left_join(Data_d, area.data.select, by = "parentdow.fish.year")

#remove unneeded intermediate data frames to keep environment clean
rm(area.data,
   area.data.select
)






#INFESTED WATERS DATA DOWNLOAD AND MERGE ------------------------------------------------------------------------------

# Data----
## Infested Waters List (UPDATE FILE PATHS, THEN RUN SECTION TO UPDATE INFESTED WATERS DATA) ----
# The code below will automatically download the newest infested-waters.xlsx document from our webpage
# as long as the url for the download hasn't changed. The code will also get the data into 
# a workable/summarized formats for the visualizations below. 

# Click the down arrow by the line number next to "Data ..." to hide/show
# the code for downloading and updating the infested waters data used here.


#ONLY RUN THIS THE FIRST TIME YOU NEED TO GET THE INFESTED WATERS DATA (OR UPDATE IT)
# Infested Waters URL
#url_iw <- "https://files.dnr.state.mn.us/eco/invasives/infested-waters.xlsx"

### UPDATE FILE PATH FOR IW DOWNLOAD----
# Path to where I want data stored
#data_path <- "G:/My Drive/Thesis/Data/R Working Directories/Walleye.Zoop.Paleo/"

# Date data was downloaded
# This will make it so that a new file is generated in my Data folder every time I run this code
#date_downloaded <- Sys.Date()

# Name I want to save infested waters under
#data_name <- paste("infested-waters_", date_downloaded, ".xlsx", sep="")

# Paste together data_path and data_name to tell R where to save downloaded infested waters list
#destfile <- paste(data_path, data_name, sep="")

# Download infested waters list
#download.file(url_iw, destfile, mode='wb')

# Read in infested waters list
#run this if downloading data for the first time
#iw <- read_excel(path=destfile, skip=1)
#RUN THIS IF JUST READING THE INFESTED WATERS SPREADSHEET ALREADY SAVED IN WORKING DIRECTORY
iw <- read_excel("Data/Input/Copy of infested-waters_2024-10-09.xlsx")

#ALWAYS RUN THIS
iw <- iw[, -dim(iw)[2]]

# Change column names
# I normally don't do this, but the default are pretty bulky to code with
colnames(iw) <- c("waterbody_name", "county", "species", "year", "year_conf", "dowlknum")

#Creates a new column that specifies if a water body is connected or confirmed
iw$connected <- factor(ifelse(grepl("connect", iw$year_conf) | grepl("Connect", iw$year_conf) | grepl("conect", iw$year_conf),
                              "connected", "confirmed"))

iw$species[iw$species=="Eurasian Watermilfoil"] <- "Eurasian watermilfoil"

# Fixing some dowlknum entries, hyphenating these really helps here
# This code fixes dowlknum issues up to 9/11/2024
# CC-LLLL-BB, where C=county, L=lake or parent dow, B=basin, is a nice convention to standardize to
unique(iw$dowlknum[!grepl("-", iw$dowlknum)])

iw$dowlknum[iw$dowlknum=="NA"] <- NA
iw$dowlknum[iw$dowlknum=="na"] <- NA
iw$dowlknum[iw$dowlknum=="none"] <- NA
iw$dowlknum[iw$dowlknum=="NONE"] <- NA

iw$dowlknum[iw$dowlknum=="none, part of Winnibigoshish"] <- "11-0147"

iw$dowlknum[iw$dowlknum=="18002900" ] <- "18-0029"

# There are some dowlknum that use hyphens, but not according to the convention above
# These all look good: CC-LLLL
unique(iw$dowlknum[grepl("-", iw$dowlknum) & nchar(iw$dowlknum)==7])
# None here:
unique(iw$dowlknum[grepl("-", iw$dowlknum) & nchar(iw$dowlknum)==8])
# Here's a mistake where there isn't a second hyphen
unique(iw$dowlknum[grepl("-", iw$dowlknum) & nchar(iw$dowlknum)==9])
# fix second hyphen
iw$dowlknum[iw$dowlknum=="18-012601"] <- "18-0126-01"
# These all look good; CC-LLLL-BB
unique(iw$dowlknum[grepl("-", iw$dowlknum) & nchar(iw$dowlknum)==10])

#Make column for parent dow - no sub-basin
iw$parentdow <- substr(iw$dowlknum, 1, 7)
#leading zeros not a problem because stored as character data


#INFESTED WATERS JOIN

#Combining infested water data with the matched fish/zoop data
#Column to match: DOW - but needs to be reformatted in match data

#filter iw to only include confirmed invasive species
iw_confirmed <-  filter(iw, connected=="confirmed")

#remove "-" from infested waters parent dow and remove leading zeroes to match other data
iw_confirmed_format <- iw_confirmed %>%
  mutate(parentdow=str_remove_all(parentdow, "-")) %>%
  mutate(parentdow=ifelse(substr(parentdow, 1, 1)=="0", substr(parentdow, 2, 6), parentdow))


#now the parentdow columns in the two datasets are the same format and can be used to match


#need to summarize infested water data into one row per lake - change from long to wide format
#make a column for each invasive species and fill it with the year it was found
iw_wide <- spread(iw_confirmed_format, species, year)
#combine the rows if multiple for each lake and drop unnecessary rows - only keep parentdow for match
iw_wide_format <- iw_wide %>%
  group_by(parentdow) %>%
  summarize(BigheadCarp = max(`bighead carp`, na.rm = TRUE), BrittleNaiad = max(`brittle naiad`, na.rm = TRUE), 
            EurasianWatermilfoil = max(`Eurasian watermilfoil`, na.rm = TRUE), FaucetSnail = max(`faucet snail`, na.rm = TRUE), 
            FloweringRush = max(`flowering rush`, na.rm = TRUE), GrassCarp = max(`grass carp`, na.rm = TRUE), 
            NZMudSnail = max(`New Zealand mud snail`, na.rm = TRUE), RedSwampCrayfish = max(`red swamp crayfish`, na.rm = TRUE), 
            RoundGoby = max(`round goby`, na.rm = TRUE), Ruffe = max(ruffe, na.rm = TRUE), SilverCarp = max(`silver carp`, na.rm = TRUE), 
            SpinyWaterflea = max(`spiny waterflea`, na.rm = TRUE), StarryStonewart = max(`starry stonewort`, na.rm = TRUE), 
            VHS = max(VHS, na.rm = TRUE), WhitePerch = max(`white perch`, na.rm = TRUE), ZebraMussel = max(`zebra mussel`, na.rm = TRUE))

#check if all values in infested waters are in match data (they probably aren't)
all(iw_wide_format$parentdow %in% MatchData_parentdow$parentdow)
#they are not

#we want to do a left join, where we keep all the data in MatchData and add any matching data in infested waters
#any DOWs in infested waters that do not match will be dropped
#any DOWs in MatchData that do not match will return NA for the new columns
Data_all <- left_join(Data_e, iw_wide_format, by = "parentdow")


#remove unneeded intermediate data frames to keep environment clean
rm(iw,
   iw_confirmed,
   iw_confirmed_format,
   iw_wide,
   iw_wide_format
)




#FILTER out to select the lakes with all the data I can actually use. ------------------------------------------------------------------------

#remove extra columns I never used or don't need like the empty invasive species and the walleye count from the old spreadsheet
Data_all <- Data_all %>%
  select(-ZoopMonths,
         -Invasive.species,
         -WalleyeCPUE)

#Requirements:
#conditions filtered above in this script:
  #lake/years with exact matches of fish/zoop data
  #zoops: at least 6 tows for each lake/year, removed Bytho and Lepto

#conditions filtered as I generated data in other scripts:
  #fish: acceptable survey and gear types, sufficient effort, removed surveys with gear issues
  #productivity: secchi data must have samples from June, July, and August in the lake/year, no secchi values of 0, all secchi data from MPCA and DNR 

#need to filter now:

# #6 distinct months of zoop tows
# Data.all.zoop.month.restrict.6 <- Data_all %>%
#   filter(Zoop.Month.Count >= 6)
# #have all the data: zoops, fish, temp, productivity, area (invasive species will only be listed if present - this is ok)
# Data.no.missing.6z <- Data.all.zoop.month.restrict.6 %>%
#   filter(!is.na(zoop.total.biomass) & 
#            !is.na(WAE.CPUE) & 
#            !is.na(gdd_wtr_5c) & 
#            !is.na(mean.summer.secchi.meters) & 
#            !is.na(lakesize)
#          )
# #end up with 61 observations
# #how many unique lakes is this?
# unique(Data.no.missing.6z$LakeName)
# #21 lakes... not many

#BELOW IS EXPERIMENTING WITH DIFFERENT DATA FILTERS TO INCREASE SAMPLE SIZE
#I started very restrictive knowing I may have to loosen some to get enough data

# #TRY BEING LESS RESTRICTIVE ON THE ZOOPS:
# #5 distinct months of zoop tows
# Data.all.zoop.month.restrict.5 <- Data_all %>%
#   filter(Zoop.Month.Count >= 5)
# #have all the data: zoops, fish, temp, productivity, area (invasive species will only be listed if present - this is ok)
# Data.no.missing.5z <- Data.all.zoop.month.restrict.5 %>%
#   filter(!is.na(zoop.total.biomass) &
#            !is.na(WAE.CPUE) &
#            !is.na(gdd_wtr_5c) &
#            !is.na(mean.summer.secchi.meters) &
#            !is.na(lakesize)
#   )
# #end up with 94 observations
# #how many unique lakes is this?
# unique(Data.no.missing.5z$LakeName)
# #24 lakes... still not many
# 
# #4 distinct months of zoop tows
# Data.all.zoop.month.restrict.4 <- Data_all %>%
#   filter(Zoop.Month.Count >= 4)
# #have all the data: zoops, fish, temp, productivity, area (invasive species will only be listed if present - this is ok)
# Data.no.missing.4z <- Data.all.zoop.month.restrict.4 %>%
#   filter(!is.na(zoop.total.biomass) &
#            !is.na(WAE.CPUE) &
#            !is.na(gdd_wtr_5c) &
#            !is.na(mean.summer.secchi.meters) &
#            !is.na(lakesize)
#   )
# #end up with 104 observations
# #how many unique lakes is this?
# unique(Data.no.missing.4z$LakeName)
# #26 lakes
# 
# #for comparison, no restriction on zoops:
# Data.no.missing.allZ <- Data_all %>%
#   filter(!is.na(zoop.total.biomass) &
#            !is.na(WAE.CPUE) &
#            !is.na(gdd_wtr_5c) &
#            !is.na(mean.summer.secchi.meters) &
#            !is.na(lakesize)
#   )
# #end up with 120 observations
# #how many unique lakes is this?
# unique(Data.no.missing.allZ$LakeName)
# #29 lakes
# 
# #TRY ALL THE ZOOP VARIATIONS WITH MORE INCLUSIVE FISH
# 
# #6 distinct months of zoop tows + inclusive fish
# Data.all.zoop.month.restrict.6 <- Data_all %>%
#   filter(Zoop.Month.Count >= 6)
# #have all the data: zoops, fish, temp, productivity, area (invasive species will only be listed if present - this is ok)
# Data.no.missing.6z.finc <- Data.all.zoop.month.restrict.6 %>%
#   filter(!is.na(zoop.total.biomass) & 
#            !is.na(WAE.CPUE.inc) & 
#            !is.na(gdd_wtr_5c) & 
#            !is.na(mean.summer.secchi.meters) & 
#            !is.na(lakesize)
#   )
# #end up with 80 observations
# #how many unique lakes is this?
# unique(Data.no.missing.6z.finc$LakeName)
# #28 lakes
# 
#USING THIS FOR NOW:

#5 distinct months of zoop tows + inclusive fish
Data.all.zoop.month.restrict.5 <- Data_all %>%
  filter(Zoop.Month.Count >= 5)
#have all the data: zoops, fish, temp, productivity, area (invasive species will only be listed if present - this is ok)
Data.no.missing.5z.finc <- Data.all.zoop.month.restrict.5 %>%
  filter(!is.na(zoop.total.biomass) &
           !is.na(WAE.CPUE.inc) &
           !is.na(gdd_wtr_5c) &
           !is.na(mean.summer.secchi.meters) &
           !is.na(lakesize)
  )
#end up with 115 observations
#how many unique lakes is this?
unique(Data.no.missing.5z.finc$LakeName)
#32 lakes... looking better!
# 
# #4 distinct months of zoop tows + inclusive fish
# Data.all.zoop.month.restrict.4 <- Data_all %>%
#   filter(Zoop.Month.Count >= 4)
# #have all the data: zoops, fish, temp, productivity, area (invasive species will only be listed if present - this is ok)
# Data.no.missing.4z.finc <- Data.all.zoop.month.restrict.4 %>%
#   filter(!is.na(zoop.total.biomass) &
#            !is.na(WAE.CPUE.inc) &
#            !is.na(gdd_wtr_5c) &
#            !is.na(mean.summer.secchi.meters) &
#            !is.na(lakesize)
#   )
# #end up with 131 observations
# #how many unique lakes is this?
# unique(Data.no.missing.4z.finc$LakeName)
# #37 lakes
# 
# #for comparison, no restriction on zoops + inclusive fish:
# Data.no.missing.allZ.finc <- Data_all %>%
#   filter(!is.na(zoop.total.biomass) &
#            !is.na(WAE.CPUE.inc) &
#            !is.na(gdd_wtr_5c) &
#            !is.na(mean.summer.secchi.meters) &
#            !is.na(lakesize)
#   )
# #end up with 158 observations
# #how many unique lakes is this?
# unique(Data.no.missing.allZ.finc$LakeName)
# #47 lakes
# 
# 
# 
# #What is the effect of the potential confounding variable lack of data?
# 
# #what is our sample size if we don't consider, temp, area, productivity data coverage but keep the 6 month zoop restriction, , fish strict?
# test <- Data.all.zoop.month.restrict.6 %>%
#   filter(!is.na(zoop.total.biomass) & 
#            !is.na(WAE.CPUE)
#   )
# unique(test$LakeName)
# #99 observations on 27 lakes
# #so these potential confounding variables take 38 observations and 6 lakes from us... not that many really
# 
# #what is our sample size if we don't consider, temp, area, productivity data coverage but keep the 5 month zoop restriction, , fish strict?
# test <- Data.all.zoop.month.restrict.5 %>%
#   filter(!is.na(zoop.total.biomass) & 
#            !is.na(WAE.CPUE)
#   )
# unique(test$LakeName)
# #162 observations on 29 lakes
# 
# #what is our sample size if we don't consider, temp, area, productivity data coverage and no zoop restriction, fish strict?
# test <- Data_all %>%
#   filter(!is.na(zoop.total.biomass) & 
#            !is.na(WAE.CPUE)
#   )
# unique(test$LakeName)
# #202 observations on 36 lakes
# 
# #what is our sample size if we don't consider, temp, area, productivity data coverage but keep the 6 month zoop restriction, , fish inclusive?
# test <- Data.all.zoop.month.restrict.6 %>%
#   filter(!is.na(zoop.total.biomass) & 
#            !is.na(WAE.CPUE.inc)
#   )
# unique(test$LakeName)
# #118 observations on 34 lakes
# #so these potential confounding variables take 38 observations and 6 lakes from us... not that many really
# 
# #what is our sample size if we don't consider, temp, area, productivity data coverage but keep the 5 month zoop restriction, , fish inclusive?
# test <- Data.all.zoop.month.restrict.5 %>%
#   filter(!is.na(zoop.total.biomass) & 
#            !is.na(WAE.CPUE.inc)
#   )
# unique(test$LakeName)
# #184 observations on 38 lakes
# 
# #what is our sample size if we don't consider, temp, area, productivity data coverage and no zoop restriction, fish inclusive?
# test <- Data_all %>%
#   filter(!is.na(zoop.total.biomass) & 
#            !is.na(WAE.CPUE.inc)
#   )
# unique(test$LakeName)
# #242 observations on 56 lakes

#DECIDED ON AT LEAST 5 MONTHS OF FISH AND FISH SURVEYS UP TO 3 LESS THAN IDEAL MINIMUM

# #save complete preliminary dataset as .csv
# write.csv(Data.no.missing.5z.finc, file = "Data/Output/Preliminary Data.csv")

#---------------------------------------------------------------------------------------------------------------

#create file of raw zoop data for only lake/years used in preliminary analysis
prelim.raw.zoop <- zoop_parentdow %>%
 # filter(parentdow.zoop.year %in% Data.no.missing.5z.finc$parentdow.zoop.year)
#to run this code, need to remake the zoop_parentdow data frame because it gets automatically removed in the code above
#save this file to send to Kylie and maybe refer to later
#write.csv(prelim.raw.zoop, file = "Data/Output/Preliminary Raw Zoop Data.csv")

#CODE BELOW IS TROUBLESHOOTING WHAT TO DO WITH TAXONOMIC RESOLUTION FOR ZOOP DIVERSITY METRICS:

#see the unique species names in the zoop data I am using 
unique(prelim.raw.zoop$species)

#see how many rows of each unique species I have
table(prelim.raw.zoop$species)

#problematic taxa:
  #juvenile copepods
      #"nauplii"  2642
      #"copepodites"  2695

  #"calanoids"  1208
      #"Senecella calanoides"  94
      #"Epischura lacustris"  393
      #"Limnocalanus macrurus"  140
      #"Diaptomidae"   1443
          #"Leptodiaptomus minutus"  13
          #"Leptodiaptomus sicilis"  2
          #"Onychodiaptomus sanguineus"  1
          #"Skistodiaptomus oregonensis"  4

  #"cyclopoids"  1592
      #"Acanthocyclops vernalis"  14
      #"Diacyclops sp."   1
          #"Diacyclops bicuspidatus thomasi"  1059
      #"Macrocyclops sp."  2
      #"Mesocyclops edax"  958
      #"Tropocyclops prasinus mexicanus"  914

  #"Bosmina sp."  1849
      #"Bosmina longirostris"  163
      #"Eubosmina coregoni"   771  - Do you consider this a genus or subgenus. Could it be included in "Bosmina sp."?

  #"Chydorus sp."  346
      #"Chydorus sphaericus"  922

  #"Alona sp."  22
      #"Alona setulosa"  9
      #"Alona quadrangularis"  1

  #"Ceriodaphnia sp."  487
      #"Ceriodaphnia reticulata"  1

  #"Daphnia sp."  6
      #"Daphnia galeata mendotae"  2115
      #"Daphnia retrocurva"  1202
      #"Daphnia parvula"  81
      #"Daphnia rosea"   3
      #"Daphnia pulicaria"  529
      #"Daphnia pulex"  80
      #"Daphnia ambigua"  6
      #"Daphnia longiremis"  281
      #"Daphnia lumholtzi"  26


#these taxa seem okay
    #"Acroperus harpae"  13
    #"Holopedium gibberum"   516
    #"Eurycercus lamellatus"  201
    #"Graptoleberis sp."  1
    #"Polyphemus pediculus"  10
    #"Latona setifera"  8
    #"Sida crystallina"  18
    #"Diaphanosoma birgei"  1045
    #"Moina sp."   28
    #"Simocephalus sp."  6
    #"Scapholeberis sp."  2
    #"Macrothricidae"  9
    #"Ergasilus sp."   16  (this is a copepod but in the order Poecilostomatoida, not a cylopoid or a calanoid)


#find the lake/years with just "Daphnia sp."
Daphnia.sp <- prelim.raw.zoop %>%
  filter(species == "Daphnia sp.")

#its Belle 2008: 470049 2008
#and Leech 2020: 110203 2020

#look at entire samples from those lake/years
Daphnia.sp.samples <- prelim.raw.zoop %>%
  filter(parentdow.zoop.year == "470049 2008" | parentdow.zoop.year == "110203 2020")

#It looks like there were a few samples Belle in 2008 (samples 4404, 4405, 4406) where all Daphnia just got called Daphnia sp., but all the other samples from Belle in 2008 IDed the Daphnia to species. In some samples from Leech in 2020 (samples 6763, 6766, and 6774), there are Daphnia sp. along with other identified Daphnia species.


#ORIGINAL METHOD - forgot there were multiple samples on each date...
#calculate shannon diversity index on each sampling date
zoop_SDI_sample <- prelim.raw.zoop %>%
  group_by(parentdow.zoop.year, sample_date) %>%
  summarize(zoop.Shannon.DI = diversity(count, index = "shannon"), .groups = 'drop')
#now take mean over all the sampling dates in a year
zoop_SDI <- zoop_SDI_sample %>%
  group_by(parentdow.zoop.year) %>%
  summarize(zoop.Shannon.DI = mean(zoop.Shannon.DI), .groups = 'drop')

#NEW CODE FOR THIS
#I want to simulate how we will find zoops in sediment, which means I should get a total count of each species for each lake/year and use this to calculate Shannon Diversity, richness, and evenness
zoop_diversity <- prelim.raw.zoop %>%
  group_by(parentdow.zoop.year, species) %>%
  summarize(count = sum(count), .groups = 'drop')
#now calculate Shannon diversity
zoop_SDI_new <- zoop_diversity %>%
  group_by(parentdow.zoop.year) %>%
  summarize(zoop.Shannon.DI = diversity(count, index = "shannon"), .groups = 'drop')

#compare the histograms of these two methods:
SDI.original <- ggplot(zoop_SDI, aes( x = zoop.Shannon.DI )) +
  geom_histogram() +
  theme_minimal()
print(SDI.original)

SDI.new <- ggplot(zoop_SDI_new, aes( x = zoop.Shannon.DI )) +
  geom_histogram() +
  theme_minimal()
print(SDI.new)

#already data looks much better without even fixing the taxonomy problems