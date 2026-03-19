#This script explores the need to correct for net mouth size and zoop ID analyst on zooplankton data for my project

#packages
library(tidyverse)
library(vegan)

#DO EVERYTHING TO THE ZOOP DATA THAT YOU DO TO MAKE YOUR DATASET-----------------------------------------
#read in the zoop data
zoop <- read.csv("Data/Input/ZoopDB_data_20251016.csv")
#this is the zoop data Kylie sent me on Oct 16, 2025 - it is up to date with what she had processed at that time

#A few Tenmile samples in this dataset need to be replaced (for some reason rows are duplicated weirdly)
#remove the problem samples
zoop.no.tenmile <- zoop %>% 
  filter(!(lake_name == "Ten Mile" & (sample_id == 165 | sample_id == 166 | sample_id == 184 | sample_id == 185)))
#read in the correct data from Kylie
tenmile <- read.csv("Data/Input/TenMile_165_166_184_185.csv")
#rowbind this corrected data in
zoop.tenmile <- rbind(zoop.no.tenmile, tenmile)

#create columns for month, day, and year separately
#Kylie informed me that sample 4729 from Artichoke lake needs it's date changed to 2009-07-21
zoop_months <- zoop.tenmile %>%
  mutate(sample_date = ifelse(sample_id == 4729 & lake_name == "Artichoke", "2009-07-21", sample_date)) %>% 
  mutate(year = substr(sample_date, 1, 4)) %>% 
  mutate(month = substr(sample_date, 6, 7)) %>%
  mutate(day = substr(sample_date, 9, 10))

#make parentdow column
zoop_parentdow <- zoop_months %>%
  mutate(parentdow = case_when(
    nchar(zoop_months$dowlknum) == 7 ~ substr(dowlknum, 1, 5),
    nchar(zoop_months$dowlknum) == 8 ~ substr(dowlknum, 1, 6)
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

#NOTE: some sample IDs refer to multiple unrelated samples, BUT when I made the inclusion table, everything was based on parentdows and years, so the inclusion table is okay
#I just need to always group and join by BOTH parentdow AND sample_id

#When I made the inclusion table, I did this:
#isolated just summer months and made sure all selected lake-years had all 5 months
#checked the remarks and flags
#removed littoral samples, horizontal samples, night samples, closing nets, oblique tows, and LMB stomachs
#Removed shallow samples that only sampled the epilimnion


# #To confirm all looks good, I checked:
# #check months
# unique(zoop_filter$month)
# #check that all 5 months are present
# zoop_month_count <- zoop_filter %>%
#   group_by(parentdow.year) %>%
#   summarize(Zoop.Month.Count = n_distinct(month), .groups = 'drop')
# max(zoop_month_count$Zoop.Month.Count)
# min(zoop_month_count$Zoop.Month.Count)
# #check flags
# flags <- data.frame(unique(zoop_filter$remarks)) #visual inspection = these look ok
# flags$remarks <- flags$`unique(zoop_filter$remarks)` #rename so it works in copied code below
# problem_flags <- flags %>% 
#   filter(grepl(pattern = "littoral", x = remarks, ignore.case = TRUE) | 
#            grepl(pattern = "horizontal", x = remarks, ignore.case = TRUE) | 
#            grepl(pattern = "night", x = remarks, ignore.case = TRUE) | 
#            grepl(pattern = "closing", x = remarks, ignore.case = TRUE) | 
#            grepl(pattern = "oblique", x = remarks, ignore.case = TRUE) | 
#            grepl(pattern = "LMB", x = remarks, ignore.case = TRUE) | 
#            grepl(pattern = "composite", x = remarks, ignore.case = TRUE) |
#            grepl(pattern = "special", x = remarks, ignore.case = TRUE) |
#            grepl(pattern = "depth", x = remarks, ignore.case = TRUE) |
#            grepl(pattern = "shallow", x = remarks, ignore.case = TRUE) |
#            grepl(pattern = "deep", x = remarks, ignore.case = TRUE))
# #Emailed Kylie to ask what the Rainy lake shallow/deep means - she says "shallow" is epilimnion only and "deep" is full water column - I will use deep)
# #THIS ALL LOOKS GOOD


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

#Apparently there is no Hill (south) 2015 zoop data... oh well :(

#Remove shallow Voyageurs National Park sites based on site numbers
# #create a df for each lake so I can investigate their site numbers
# Rainy <- zoop_split %>% 
#   filter(lake_name == "Rainy")
# table(Rainy$year, Rainy$site_number)
# #need to remove all site 1 from Rainy - it is a shallow site in all years
# 
# Kabe <- zoop_split %>% 
#   filter(lake_name == "Kabetogama")
# table(Kabe$year, Kabe$site_number)
# #this is fine
# 
# Nam <- zoop_split %>% 
#   filter(lake_name == "Namakan")
# table(Nam$year, Nam$site_number)
# #need to remove all site 1 from Namakan - it is a shallow site in all years
# 
# SP <- zoop_split %>% 
#   filter(lake_name == "Sand Point")
# table(SP$year, SP$site_number)
# #need to remove all site 1 from Sand Point - it is a shallow site in all years

#Remove the shallow VNP samples:
zoop_deep <- zoop_split %>% 
  filter(!((lake_name == "Rainy" & site_number == 1) | 
             (lake_name == "Namakan" & site_number == 1) | 
             (lake_name == "Sand Point" & site_number == 1)))

# #Check for duplicate samples
# #create a dataframe that has a list of name/date/sites that have multiple associated sample ids
# zoop_sample_duplicates <- zoop_deep %>% 
#   group_by(lake_name, sample_date, site_number) %>%
#   filter(n_distinct(sample_id) > 1) %>% 
#   summarize(sample_ids = paste(unique(sample_id), collapse = ", "),
#             .groups = 'drop')
# 
# #Do this again but after removing bythotrephes and leptodora rows - it appears that there are some samples just for these species - Kylie confirmed I can just remove the samples that only have these predator species
# zoop_sample_duplicates_no_predators <- zoop_deep %>% 
#   filter(species != "Bythotrephes longimanus" & species != "Leptodora kindti") %>% 
#   group_by(lake_name, sample_date, site_number) %>%
#   filter(n_distinct(sample_id) > 1) %>% 
#   summarize(sample_ids = paste(unique(sample_id), collapse = ", "),
#             .groups = 'drop')
# #Emailed Kylie to ask about problem samples - she gave me answers I have incorporated later in the script
# 
# 
# #Check for duplicate taxa within samples
# zoop_taxa_duplicates <- zoop_deep %>% 
#   group_by(lake_name, sample_id, species) %>%
#   filter(n() > 1) %>% 
#   summarize(num_dups = n(),
#             .groups = 'drop')
# #problem: Tenmile samples: 165, 166, 184, 185
# #WHAT TO DO ABOUT THESE: I emailed Kylie and she sent me corrected data for these samples which is now incorporated above
# 
# #Check for sentinel lakes pre-2013
# #make a list of Sentinel lake names that are in my dataset
# sentinel_lakes <- c("Artichoke", "Bearhead", "Belle", "Carlos", "Cedar", "Elk", "Greenwood", "Hill (north)", "Hill (south)", "Madison", "Pearl",
#                     "Peltier", "Portage", "Shaokatan", "South Center","Tait", "Ten Mile", "Trout", "White Iron")
# 
# old_sentinel_zoop <- zoop_split %>% 
#   filter(year < 2013 & lake_name %in% sentinel_lakes)
# 
# old_other_zoop <- zoop_split %>% 
#   filter(year < 2013) %>% 
#   filter(!(lake_name %in% sentinel_lakes))

#Make changes based on exploration above and communication with Kylie:
#Things to leave that are okay:
    #Artichoke samples 4725-4738 and Shaokotan 4626-4635 have intentional replicate samples and should be left in
#Remove:
    #bythotrephes and leptodora 
    #Duplicate Mille Lacs samples that are known issue in database
    #False starts from Minnetonka
    #False start in Lower Red
    #Tenmile samples with duplicate taxa
zoop_nodupes <- zoop_deep %>% 
  filter(!(lake_name == "Mille Lacs" & sample_id >= 4065 & sample_id <= 4072)) %>% 
  filter(species != "Bythotrephes longimanus" & species != "Leptodora kindti") %>% 
  filter(!(lake_name == "Minnetonka" & (sample_id == 853 | sample_id == 854))) %>% 
  filter(!(lake_name == "Red (Lower Red)" & sample_id == 2207))


#Taxonomy simplification based on conversations with Kylie
#what taxa do we have to start with?
sort(unique(zoop_nodupes$species))
#Any how many of each?
table(sort(zoop_nodupes$species))

#rename taxa that need it based on conversation with Heidi and Kylie
#targeted just the Belle lake Daphnia based on Jodie's notes from when she IDed them
#I know I can run these together but I was getting an error I didn't have time to deal with when I tried that
zoop_clean_taxa <- zoop_nodupes %>%
  mutate(species = ifelse(species == "Chydorus sp." | species == "Chydoridae" | species == "Chydorus bicornutus", "Chydorus sphaericus", species)) 
zoop_clean_taxa <- zoop_clean_taxa %>%
  mutate(species = ifelse(species == "Bosmina longirostris" | species == "Bosmina sp.", "Bosmina sp.", species))
zoop_clean_taxa <- zoop_clean_taxa %>%
  mutate(species = ifelse(species == "Alona setulosa" | species == "Alona quadrangularis" , "Alona sp.", species))
zoop_clean_taxa <- zoop_clean_taxa %>%  
  mutate(species = ifelse(species == "Ceriodaphnia reticulata" | species == "Ceriodaphnia quadrangula" | species == "Ceriodaphnia lacustris", "Ceriodaphnia sp.", species))
zoop_clean_taxa <- zoop_clean_taxa %>%
  mutate(species = ifelse(species == "Daphnia pulex", "Daphnia pulicaria", species))
#below I am targeting just the Belle lake Daphnia based on Jodie's notes from when she IDed them
zoop_clean_taxa <- zoop_clean_taxa %>%
  mutate(species = ifelse(species == "Daphnia sp." & parentdow.year == "470049 2008", "Daphnia rosea", species))
# rename copepod "species" to the taxonomic level we consistently have for them
zoop_clean_taxa <- zoop_clean_taxa %>%
  mutate(species = ifelse(grp2 == "calanoids", "calanoids", 
                          ifelse(grp2 == "cyclopoids", "cyclopoids", species)))
#fix group 2 classifications per Kylie's instructions
zoop_clean_taxa <- zoop_clean_taxa %>%
  mutate(grp2 = ifelse(species == "Daphnia sp." | species == "Daphnia rosea", "small cladocerans",
                       ifelse(species == "nauplii" | species == "copepodites", "immature copepods", 
                              ifelse(species == "Harpacticoida", "harpacticoids",  grp2))))

#check that it worked
table(sort(zoop_clean_taxa$species))
#yay!

#Now I might have multiple rows with the same species in the same sample after renaming things (especially copepods because I lumped a lot together)
#Here I make only row row per species per sample, and I sum the density, biomass, number percent, weight percent, and count if multiple rows
#this will only keep the columns in the dataset that I want
#Only keeping metrics I can sum together for rows
zoop_copepod_rows <- zoop_clean_taxa %>% 
  group_by(parentdow.year, parentdow, lake_name, sample_date, year, month, sample_id, site_number, is_slice_lake, net_mouth_diameter_cm, species) %>% 
  summarize(density = sum(density),
            biomass = sum(biomass),
            number_pct = sum(number_pct),
            weight_pct = sum(weight_pct),
            count = sum(count),
            .groups = 'drop')


#need to make sure all species have a row for all tows - even if the biomass value is 0 so that my means calculate correctly
#How many rows should I end up with?
n_distinct(zoop_clean_taxa$parentdow.year, zoop_clean_taxa$parentdow, zoop_clean_taxa$lake_name, zoop_clean_taxa$sample_date, zoop_clean_taxa$year, zoop_clean_taxa$month, zoop_clean_taxa$sample_id)
length(unique(zoop_clean_taxa$species))
#we have 3567 tows and they should each have 30 species so we should end up with a data frame that has 3567 * 30 = 107010 rows
#make all the empty rows you need, preserve the groups you need to average and other data you still want in each row, and fill the data values with 0 for the new rows
zoop_complete <- complete(data = zoop_copepod_rows, nesting(parentdow.year, parentdow, lake_name, sample_date, year, month, site_number, is_slice_lake, net_mouth_diameter_cm, sample_id), species, fill = list(density = 0, biomass = 0, number_pct = 0, weight_pct = 0, mean_weight = 0, mean_length = 0, count = 0), explicit = FALSE)
#The number of rows looks good!

#ALL ANALYSES THAT WANT TO LOOK AT INDIVIDUAL TOWS SHOULD START WITH THE zoop_complete DATA FRAME

#CALCULATE BIOMASS AVERAGES PER YEAR

# #are there the same number of tows in each month within each lake-year?
# zoop_month_tows <- zoop_complete %>%
#   group_by(parentdow.year, month) %>%
#   summarize(tow.count = n_distinct(sample_id), .groups = 'drop')
# #NO THERE ARE NOT - need to average within months first so timing of sampling effort does not affect final calculations
# 
# #what about each day?
# zoop_day_tows <- zoop_complete %>%
#   group_by(parentdow.year, sample_date) %>%
#   summarize(tow.count = n_distinct(sample_id), .groups = 'drop')
# #NO THERE ARE NOT - BUT:  daily sampling seems pretty consistent within each lake-year BUT is sometimes spread over 2-3 consecutive days - therefore I should not average by day


#first need to average tow biomasses in each month in each lake-year for each species
zoop_biom_month_mean <- zoop_complete %>%
  group_by(parentdow, lake_name, year, month, species) %>%
  summarize(biomass = mean(biomass), .groups = 'drop') 

#finally average monthly biomasses to get average biomass in each lake-year for each species
zoop_biom_year_mean <- zoop_biom_month_mean %>%
  group_by(parentdow, lake_name, year, species) %>%
  summarize(biomass = mean(biomass), .groups = 'drop')

#I should have 204 lake-years * 30 species = 6120 rows
#looks good

#convert to wide so there is a column for each species
zoop_wide <- pivot_wider(data = zoop_biom_year_mean, names_from = species, values_from = biomass)

#calculate a total biomass column
zoop_wide$total_zoop_biomass <- rowSums(zoop_wide[,4:33])

#Calculate three other zoop metrics I want to have: 
#use the zoop_clean_taxa dataframe for these because it does not matter if the copepods have multiple rows

#Mean length of all taxa (averaged by weighted abundance)
zoop_mean_length <- zoop_clean_taxa %>% 
  group_by(parentdow, year) %>%
  summarize(mean_length_all = sum(mean_length*count)/sum(count), .groups = 'drop')

#Mean length of Cladocerans (averaged by weighted abundance)
zoop_cladoceran <- filter(zoop_clean_taxa, grp == "Cladocerans")
clad_mean_length <- zoop_cladoceran %>% 
  group_by(parentdow, year) %>%
  summarize(mean_length_clad = sum(mean_length*count)/sum(count), .groups = 'drop')

#Abundance ratio of small:big cladocerans (Holopedium are large)
clad_size <- zoop_cladoceran %>% 
  mutate(size = ifelse(grp2 == "large daphnia", "large", ifelse(grp2 == "Holopedium", "large", "small"))) %>% #simplify large vs. small in size column
  group_by(parentdow, year, size) %>% 
  summarize(count = sum(count), .groups = 'drop')  #get a count of each size for each lake-year
#convert long to wide
clad_size_wide <- pivot_wider(clad_size, names_from = "size", values_from = "count")
#replace NA counts with 0
clad_size_wide0 <- clad_size_wide %>% 
  mutate(large = ifelse(is.na(large), 0, large),
         small = ifelse(is.na(small), 0, small))
#calculate ratio of small:large cladocerans in each lake-year
clad_ratio <- clad_size_wide0 %>%
  mutate(clad_ratio = small/large) %>%  #I know it would be more intuitive to do large/small so bigger numbers are more bigger cladocerans, but this handles the one zero in the smalls
  select(-small, -large) #get rid the the large and small columns

#join all the zoop metrics together
zoop1 <- left_join(zoop_wide, zoop_mean_length, by = c("parentdow", "year"))
zoop2 <- left_join(zoop1, clad_mean_length, by = c("parentdow", "year"))
zoop3 <- left_join(zoop2, clad_ratio, by = c("parentdow", "year"))

#a bit of formatting pre-join
zoop.join <- zoop3 %>% 
  rename(mean_length_all_zoop = mean_length_all) %>% 
  mutate(Year = as.numeric(year)) %>% 
  select(-lake_name, -year)



#EXPLORE NET MOUTH SIZE-------------------------------------

#Investigate the 13 cm vs 30 cm net mouth - does it make a difference?
#Isolate samples that Jodie took for comparison (Sentinel lakes 2013)
zoop_13 <- zoop_complete %>%
  filter(year == 2013 & is_slice_lake == "True") %>%
  mutate(net_mouth_diameter_cm = as.factor(net_mouth_diameter_cm))
#Check normality assumption for t-test
ggplot(zoop_13, aes(x = biomass, fill = net_mouth_diameter_cm, alpha = 0.4))+
  geom_density()+
  facet_wrap(~species, scales = "free")+
  theme_classic()
#so.... biomass is not normal
ggplot(zoop_13, aes(x = count, fill = net_mouth_diameter_cm, alpha = 0.4))+
  geom_density()+
  facet_wrap(~species, scales = "free")+
  theme_classic()
#neither is count
ggplot(zoop_13, aes(x = density, fill = net_mouth_diameter_cm, alpha = 0.4))+
  geom_density()+
  facet_wrap(~species, scales = "free")+
  theme_classic()
#tampoco density
#What I got from this is that there are differences between the net sizes but they aren't huge - the question is are they significant?

#remove rows for taxa with no individuals in this subset of data
zoop_13_taxa <- zoop_13 %>%
  filter(count != 0)
zoop_13_taxa_list <-unique(zoop_13_taxa$species)
zoop_13_simple <- zoop_13 %>%
  filter(species %in% zoop_13_taxa_list)

#new idea - scatterplot them on each other
#pivot wider so we have a 30 and a 13 cm column for density, count, biomass, and mean length
zoop_13_wide <- zoop_13_simple %>%
  pivot_wider(names_from = net_mouth_diameter_cm,
              values_from = c(density, biomass, count),
              id_cols = c(parentdow.year, sample_date, lake_name, species, year, parentdow),
              values_fill = 0)

#now plot these
biomass <- ggplot(zoop_13_wide, aes(x = biomass_13, y = biomass_30))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")+ #add a 1:1 line
  geom_smooth(method = "lm")+
  scale_y_continuous(limits = c(0,NA))+
  scale_x_continuous(limits = c(0,NA))+
  facet_wrap(~species, scales = "free")+
  theme_classic()
density <- ggplot(zoop_13_wide, aes(x = density_13, y = density_30))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")+ #add a 1:1 line
  geom_smooth(method = "lm")+
  scale_y_continuous(limits = c(0,NA))+
  scale_x_continuous(limits = c(0,NA))+
  facet_wrap(~species, scales = "free")+
  theme_classic()
count <- ggplot(zoop_13_wide, aes(x = count_13, y = count_30))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")+ #add a 1:1 line
  geom_smooth(method = "lm")+
  scale_y_continuous(limits = c(0,NA))+
  scale_x_continuous(limits = c(0,NA))+
  facet_wrap(~species, scales = "free")+
  theme_classic()
#We have some outliers here that pull the linear regression lines. Overall it looks like the 30 cm net mouth catches slightly more which makes sense
#save these for comparison
# ggsave(filename = "13v30_biomass_simple_taxa.png", plot = biomass, width = 6, height = 6, units = "in", dpi = 300)
# ggsave(filename = "13v30_density_simple_taxa.png", plot = density, width = 6, height = 6, units = "in", dpi = 300)
# ggsave(filename = "13v30_count_simple_taxa.png", plot = count, width = 6, height = 6, units = "in", dpi = 300)

#I guess what I need to know is how different are these lakes in multivariate space
  #how does the variation between net mouth samples from same sampling event compare to variation between lakes and between dates?


#Create wide format biomass dataframe
zoop_13_biom_wide <- zoop_13_simple %>%
  pivot_wider(names_from = species,
              values_from = biomass,
              id_cols = c(sample_date, lake_name, net_mouth_diameter_cm), #don't need year because all 2013
              values_fill = 0)

#Create wide format density dataframe
zoop_13_dens_wide <- zoop_13_simple %>%
  pivot_wider(names_from = species,
              values_from = density,
              id_cols = c(sample_date, lake_name, net_mouth_diameter_cm), #don't need year because all 2013
              values_fill = 0)

#Create wide format count dataframe
zoop_13_count_wide <- zoop_13_simple %>%
  pivot_wider(names_from = species,
              values_from = count,
              id_cols = c(sample_date, lake_name, net_mouth_diameter_cm), #don't need year because all 2013
              values_fill = 0)

#Create vectors for color by lake and fill by net mouth size, also sample month
lake.color <- zoop_13_biom_wide$lake_name
size.fill <- zoop_13_biom_wide$net_mouth_diameter_cm
month <- substr(zoop_13_biom_wide$sample_date, 6, 7)

#Calculate relative abundance and remove the identifier columns
biom_rel_abun <- zoop_13_biom_wide[,4:17] / rowSums(zoop_13_biom_wide[,4:17])
dens_rel_abun <- zoop_13_dens_wide[,4:17] / rowSums(zoop_13_dens_wide[,4:17])
count_rel_abun <- zoop_13_count_wide[,4:17] / rowSums(zoop_13_count_wide[,4:17])

#square root transform to do Hellinger transformation to downweight rare species and deal with the many zeroes
biom_rel_abun_Holl <- sqrt(biom_rel_abun)
dens_rel_abun_Holl <- sqrt(dens_rel_abun)
count_rel_abun_Holl <- sqrt(count_rel_abun)

#Make an NMDS with euclidean distance on Hellinger-transformed biomass data
biom_euclidean_dist <- metaMDS(biom_rel_abun_Holl, distance = "euclidean")
#extract axis scores for the sites (samples) only
Plot.Data.biom <- as.data.frame(scores(biom_euclidean_dist, display = "sites"))
#add LakeName and net size to the Plot.Data as a column
Plot.Data.biom$lake_name <- as.factor(lake.color)
Plot.Data.biom$size <- as.factor(size.fill)
Plot.Data.biom$month <- as.factor(month)

#plot with color by lake and fill by net mouth size
NMDS_biom <- ggplot(data = Plot.Data.biom, aes(x = NMDS1, y = NMDS2, color = lake_name, alpha = size, shape = month)) +
  geom_point(size = 3)+
  labs(title = "Biomass", y = "NMDS2", x = "NMDS1") +
  scale_alpha_manual(values = c("30" = 1.0, "13" = 0.4))+
  theme_classic()
NMDS_biom

#This generally looks really good - I'll look again after transforming to see if it improved

#Make an NMDS with euclidean distance on Hellinger-transformed density data
dens_euclidean_dist <- metaMDS(dens_rel_abun_Holl, distance = "euclidean")
#extract axis scores for the sites (samples) only
Plot.Data.dens <- as.data.frame(scores(dens_euclidean_dist, display = "sites"))
#add LakeName and net size to the Plot.Data as a column
Plot.Data.dens$lake_name <- as.factor(lake.color)
Plot.Data.dens$size <- as.factor(size.fill)
Plot.Data.dens$month <- as.factor(month)

#plot with color by lake and fill by net mouth size
NMDS_dens <- ggplot(data = Plot.Data.dens, aes(x = NMDS1, y = NMDS2, color = lake_name, alpha = size, shape = month)) +
  geom_point(size = 3)+
  labs(title = "Density", y = "NMDS2", x = "NMDS1") +
  scale_alpha_manual(values = c("30" = 1.0, "13" = 0.4))+
  theme_classic()
NMDS_dens
#density also looks decent

#Make an NMDS with euclidean distance on Hellinger-transformed count data
count_euclidean_dist <- metaMDS(count_rel_abun_Holl, distance = "euclidean")
#extract axis scores for the sites (samples) only
Plot.Data.count <- as.data.frame(scores(count_euclidean_dist, display = "sites"))
#add LakeName and net size to the Plot.Data as a column
Plot.Data.count$lake_name <- as.factor(lake.color)
Plot.Data.count$size <- as.factor(size.fill)
Plot.Data.count$month <- as.factor(month)

#plot with color by lake and fill by net mouth size
NMDS_count <- ggplot(data = Plot.Data.count, aes(x = NMDS1, y = NMDS2, color = lake_name, alpha = size, shape = month)) +
  geom_point(size = 3)+
  labs(title = "Count", y = "NMDS2", x = "NMDS1") +
  scale_alpha_manual(values = c("30" = 1.0, "13" = 0.4))+
  theme_classic()
NMDS_count
#count looks pretty much the same as density

#save these plots for comparsion
# ggsave(filename = "13v30_density_NMDS.png", plot = NMDS_dens, width = 8, height = 6, units = "in", dpi = 300)
# ggsave(filename = "13v30_biomass_NMDS.png", plot = NMDS_biom, width = 8, height = 6, units = "in", dpi = 300)
# ggsave(filename = "13v30_count_NMDS.png", plot = NMDS_count, width = 8, height = 6, units = "in", dpi = 300)

# # what happens when I apply the net size correction from Jodie/Kylie to this data and do all this again?
# # density correction to apply to 13 cm net data: y=1.8782x + 2.7255
#
# #looks like the 30cm is better at catching big guys so let's account for that
# ##let's use mean weight coefficient (0.071) in the correction equation to adjust for Biomass
# ## we know that B= density * mean weight
# ## therefore the new conversion should be
# #biomass2=density2*((0.07142*mean weight)+8.0742)
# #mean weight = biomass/density
#
# zoop_13_corrected <- zoop_13 %>%
#   mutate(density = ifelse(net_mouth_diameter_cm == "13" & density != 0, (1.8782*density)+2.7255, density),
#          biomass = ifelse(net_mouth_diameter_cm == "13" & biomass != 0, density*((0.07142*(biomass/density))+8.0742), biomass))

#WHEN I USED THESE, THEY MADE THE CORRECTED 13 CM DATA WAY LESS SIMILAR TO THE 30CM DATA THAN WE STARTED WITH
#I THINK MY SUBSET OF DATA NEETS ITS OWN CORRECTION FACTOR
#SEE PLOTS I SAVED WITH THIS ORIGINAL CORRECTION ATTEMPT

#calculate my own correction factors with the same method Kylie used but on my subset of data
summary(lm_dens <- lm(density_30~density_13, zoop_13_wide))
#intercept = 0.07889     slope = 2.04971      adjusted R2 = 0.796

summary(lm_biom <- lm(biomass_30 ~ biomass_13, zoop_13_wide))
#intercept = -4.03779     slope = 3.68784     adjusted R2 = 0.708

#try the mean weight thing
zoop_13_wide$mw13 <- zoop_13_wide$biomass_13/zoop_13_wide$density_13
zoop_13_wide$mw30 <- zoop_13_wide$biomass_30/zoop_13_wide$density_30
summary(lm_mw <- lm(mw30 ~ mw13, zoop_13_wide))
#intercept = 0.15940     slope = 0.94137     adjusted R2 = 0.7765

#Apply my own correction factors
zoop_13_corrected <- zoop_13_simple %>%
  mutate(density2 = ifelse(net_mouth_diameter_cm == "13" & density != 0, (2.04971*density)+0.07889, density),  #save as density 2 but keep original density for mean weight calculation
         biomass = ifelse(net_mouth_diameter_cm == "13" & biomass != 0, density2*((0.94137*(biomass/density))+0.15940), biomass),  #calculate mean weight with original density but multiply by new density
         density = density2,  #make the new density the main density column
         remarks = ifelse(net_mouth_diameter_cm == "13" & (biomass != 0 | density != 0), "net diff correction applied to density and biomass", "")) %>%
  select(-density2) #get rid of temporary density column

#Now do all the same plots to investigate the differences
#pivot wider so we have a 30 and a 13 cm column for density, count, biomass, and mean length
zoop_13_wide_corrected <- zoop_13_corrected %>%
  pivot_wider(names_from = net_mouth_diameter_cm,
              values_from = c(density, biomass, count),
              id_cols = c(parentdow.year, sample_date, lake_name, species, year, parentdow),
              values_fill = 0)
#now plot these
biomass_corrected <- ggplot(zoop_13_wide_corrected, aes(x = biomass_13, y = biomass_30))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")+ #add a 1:1 line
  geom_smooth(method = "lm")+
  scale_y_continuous(limits = c(0,NA))+
  scale_x_continuous(limits = c(0,NA))+
  facet_wrap(~species, scales = "free")+
  theme_classic()
density_corrected <- ggplot(zoop_13_wide_corrected, aes(x = density_13, y = density_30))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")+ #add a 1:1 line
  geom_smooth(method = "lm")+
  scale_y_continuous(limits = c(0,NA))+
  scale_x_continuous(limits = c(0,NA))+
  facet_wrap(~species, scales = "free")+
  theme_classic()
#We have some outliers here that pull the linear regression lines. Overall it looks like the 30 cm net mouth catches slightly more which makes sense
#save these for comparison
# ggsave(filename = "13v30_biomass_corrected_simple_taxa.png", plot = biomass_corrected, width = 6, height = 6, units = "in", dpi = 300)
# ggsave(filename = "13v30_density_corrected_simple_taxa.png", plot = density_corrected, width = 6, height = 6, units = "in", dpi = 300)



#Create wide format biomass dataframe
zoop_13_biom_wide_corrected <- zoop_13_simple_corrected %>%
  pivot_wider(names_from = species,
              values_from = biomass,
              id_cols = c(sample_date, lake_name, net_mouth_diameter_cm), #don't need year because all 2013
              values_fill = 0)

#Create wide format density dataframe
zoop_13_dens_wide_corrected <- zoop_13_simple_corrected %>%
  pivot_wider(names_from = species,
              values_from = density,
              id_cols = c(sample_date, lake_name, net_mouth_diameter_cm), #don't need year because all 2013
              values_fill = 0)

#Create vectors for color by lake and fill by net mouth size, also sample month
lake.color_corrected <- zoop_13_biom_wide_corrected$lake_name
size.fill_corrected <- zoop_13_biom_wide_corrected$net_mouth_diameter_cm
month_corrected <- substr(zoop_13_biom_wide_corrected$sample_date, 6, 7)

#Calculate relative abundance and remove the identifier columns
biom_rel_abun_corrected <- zoop_13_biom_wide_corrected[,4:17] / rowSums(zoop_13_biom_wide_corrected[,4:17])
dens_rel_abun_corrected <- zoop_13_dens_wide_corrected[,4:17] / rowSums(zoop_13_dens_wide_corrected[,4:17])

#square root transform to do Hellinger transformation to downweight rare species and deal with the many zeroes
biom_rel_abun_Holl_corrected <- sqrt(biom_rel_abun_corrected)
dens_rel_abun_Holl_corrected <- sqrt(dens_rel_abun_corrected)

#Make an NMDS with euclidean distance on Hellinger-transformed biomass data
biom_euclidean_dist_corrected <- metaMDS(biom_rel_abun_Holl_corrected, distance = "euclidean")
#extract axis scores for the sites (samples) only
Plot.Data.biom_corrected <- as.data.frame(scores(biom_euclidean_dist_corrected, display = "sites"))
#add LakeName and net size to the Plot.Data as a column
Plot.Data.biom_corrected$lake_name <- as.factor(lake.color_corrected)
Plot.Data.biom_corrected$size <- as.factor(size.fill_corrected)
Plot.Data.biom_corrected$month <- as.factor(month_corrected)

#plot with color by lake and fill by net mouth size
NMDS_biom_corrected <- ggplot(data = Plot.Data.biom_corrected, aes(x = NMDS1, y = NMDS2, color = lake_name, alpha = size, shape = month)) +
  geom_point(size = 3)+
  labs(title = "Biomass Corrected", y = "NMDS2", x = "NMDS1") +
  scale_alpha_manual(values = c("30" = 1.0, "13" = 0.4))+
  theme_classic()
NMDS_biom_corrected

#Make an NMDS with euclidean distance on Hellinger-transformed density data
dens_euclidean_dist_corrected <- metaMDS(dens_rel_abun_Holl_corrected, distance = "euclidean")
#extract axis scores for the sites (samples) only
Plot.Data.dens_corrected <- as.data.frame(scores(dens_euclidean_dist_corrected, display = "sites"))
#add LakeName and net size to the Plot.Data as a column
Plot.Data.dens_corrected$lake_name <- as.factor(lake.color_corrected)
Plot.Data.dens_corrected$size <- as.factor(size.fill_corrected)
Plot.Data.dens_corrected$month <- as.factor(month_corrected)

#plot with color by lake and fill by net mouth size
NMDS_dens_corrected <- ggplot(data = Plot.Data.dens_corrected, aes(x = NMDS1, y = NMDS2, color = lake_name, alpha = size, shape = month)) +
  geom_point(size = 3)+
  labs(title = "Density", y = "NMDS2", x = "NMDS1") +
  scale_alpha_manual(values = c("30" = 1.0, "13" = 0.4))+
  theme_classic()
NMDS_dens_corrected

#save these for comparison
# ggsave(filename = "13v30_density_NMDS_corrected.png", plot = NMDS_dens_corrected, width = 8, height = 6, units = "in", dpi = 300)
# ggsave(filename = "13v30_biomass_NMDS_corrected.png", plot = NMDS_biom_corrected, width = 8, height = 6, units = "in", dpi = 300)


#plot the biomass with 30, 13, and 13 corrected all togheter
NMDS_13_corrected_biom <- Plot.Data.biom_corrected %>%
  filter(size == 13) %>%
  mutate(size = "13 corrected")

Plot.Data.biom_all <- rbind(Plot.Data.biom, NMDS_13_corrected_biom)

NMDS_biom_all <- ggplot(data = Plot.Data.biom_all, aes(x = NMDS1, y = NMDS2, color = lake_name, alpha = size, shape = month)) +
  geom_point(size = 3)+
  labs(title = "Biomass Corrected and Uncorrected", y = "NMDS2", x = "NMDS1") +
  scale_alpha_manual(values = c("30" = 1.0, "13" = 0.6, "13 corrected" = 0.3))+
  theme_classic()
NMDS_biom_all
# ggsave(filename = "13v30_biomass_NMDS_all.png", plot = NMDS_biom_all, width = 8, height = 6, units = "in", dpi = 300)

# #keep environment clean if you want to get rid of all the stuff from this net mouth analysis
# rm(biom_rel_abun, biom_rel_abun_corrected, biom_rel_abun_Holl, biom_rel_abun_Holl_corrected, biom_euclidean_dist,
#    biom_euclidean_dist_corrected, count_rel_abun, count_rel_abun_Holl, count_euclidean_dist, dens_rel_abun,
#    dens_rel_abun_corrected, dens_rel_abun_Holl, dens_rel_abun_Holl_corrected, dens_euclidean_dist, dens_euclidean_dist_corrected,
#    lm_biom, lm_dens, lm_mw, NMDS_13_corrected_biom, Plot.Data.biom, Plot.Data.biom_all, Plot.Data.biom_corrected,
#    Plot.Data.count, Plot.Data.dens, Plot.Data.dens_corrected, zoop_13, zoop_13_biom_wide, zoop_13_biom_wide_corrected,
#    zoop_13_corrected, zoop_13_count_wide, zoop_13_dens_wide, zoop_13_dens_wide_corrected, zoop_13_simple, zoop_13_simple_corrected,
#    zoop_13_taxa, zoop_13_taxa_corrected, zoop_13_wide, zoop_13_wide_corrected, zoop_13_taxa_list, zoop_13_taxa_list_corrected,
#    biomass, biomass_corrected, count, density, density_corrected, lake.color, lake.color_corrected, month,
#    month_corrected, NMDS_biom, NMDS_biom_all, NMDS_biom_corrected, NMDS_count, NMDS_dens, NMDS_dens_corrected,
#    size.fill, size.fill_corrected, size.shape)

#CONCLUSION OF ALL THIS: NO NEED TO CORRECT FOR THE 13cm NETS


#EXPLORE ANALYST-------------------------------------------------

#Here I want to look at the overall effect of Kylie vs. Jodie on my final result: community composition by lake-year
#JODIE = pre-2020, Kylie = 2020-present

#Isolate sentinel lakes from the annual biomass by species summary in wide format
#Using sentinel lakes because they have lots of data both before and after analyst switch

#get list of sentinel lakes in my dataset
sentinel.mylakes <- zoop_split %>% 
  filter(is_slice_lake == "True")

#How many of each do we have
table(sentinel.mylakes$lake_name, sentinel.mylakes$year)

#okay I actually want to do this with the whole dataset not just my lakes to get more lake-years in here

#Starting from the top! Do everything except match to my inclusion table
#start with the zoop_parentdow dataframe

#isolate sentinel lakes
sentinel <- zoop_parentdow %>% 
  filter(is_slice_lake == "True")

#Do a few checks that were previously done in inclusion table:
#isolate just summer months we will use (May - September)
sentinel_summer <- sentinel %>%
  filter(month == "05" | month == "06" | month == "07" | month == "08" | month == "09")

#find and remove any littoral samples, horizontal samples, night samples, closing nets, oblique tows, and LMB stomachs
sentinel_summer_clean <- sentinel_summer %>% 
  filter(!grepl(pattern = "littoral", x = remarks, ignore.case = TRUE)) %>% 
  filter(!grepl(pattern = "horizontal", x = remarks, ignore.case = TRUE)) %>% 
  filter(!grepl(pattern = "night", x = remarks, ignore.case = TRUE)) %>% 
  filter(!grepl(pattern = "closing", x = remarks, ignore.case = TRUE)) %>% 
  filter(!grepl(pattern = "oblique", x = remarks, ignore.case = TRUE)) %>% 
  filter(!grepl(pattern = "LMB", x = remarks, ignore.case = TRUE))

#calculate number of unique summer months for each parentdow.year
sentinel_summer_month_count <- sentinel_summer_clean %>%
  group_by(parentdow.year) %>%
  summarize(Zoop.Month.Count = n_distinct(month), .groups = 'drop')

#filter to only those with all 5 months and get rid of any lakes without parentdows - these will be useless to me anyways
sentinel_5month <- sentinel_summer_month_count %>% 
  filter(Zoop.Month.Count == 5) %>% 
  filter(!grepl(pattern = "NA", x = parentdow.year, ignore.case = TRUE))


#I want more info than parentdow.year in my inclusion table, so joining detailed (and filtered) data back to this list
sentinel_5month_detail <- left_join(sentinel_5month, sentinel_summer_clean, by = "parentdow.year")

#check for the flags you want to individually assess
sentinel_check <- sentinel_5month_detail %>% 
  filter(grepl(pattern = "composite", x = remarks, ignore.case = TRUE) |
           grepl(pattern = "special", x = remarks, ignore.case = TRUE) |
           grepl(pattern = "depth", x = remarks, ignore.case = TRUE) |
           grepl(pattern = "shallow", x = remarks, ignore.case = TRUE) |
           grepl(pattern = "deep", x = remarks, ignore.case = TRUE)
  )
#none of these are an issue here

#make a list of all the unique remarks to check if there is anything else I should filter out
sentinel.remarks <- data.frame(unique(sentinel_5month_detail$remarks))
#looks good

#only some of this is relevant but just running it all to not waste time on this
#Separate Hill, Verm, and Red samples from each other:
sentinel_split <- sentinel_5month_detail %>% 
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

#remove bythotrephes and leptodora
sentinel_nopreds <- sentinel_split %>% 
  filter(species != "Bythotrephes longimanus" & species != "Leptodora kindti")


#create a dataframe that has a list of name/date/sites that have multiple associated sample ids
sentinel_sample_duplicates <- sentinel_nopreds %>%
  group_by(lake_name, sample_date, site_number) %>%
  filter(n_distinct(sample_id) > 1) %>%
  summarize(sample_ids = paste(unique(sample_id), collapse = ", "),
            .groups = 'drop')

#Check for duplicate taxa within samples
sentinel_taxa_duplicates <- sentinel_nopreds %>%
  group_by(lake_name, sample_id, species) %>%
  filter(n() > 1) %>%
  summarize(num_dups = n(),
            .groups = 'drop')

#There are a lot of problems here. For the purposes of this I am not going to worry about it 

#Taxonomy simplification based on conversations with Kylie
#what taxa do we have to start with?
sort(unique(sentinel_nopreds$species))
#Any how many of each?
table(sort(sentinel_nopreds$species))

#rename taxa that need it based on conversation with Heidi and Kylie
#targeted just the Belle lake Daphnia based on Jodie's notes from when she IDed them
#I know I can run these together but I was getting an error I didn't have time to deal with when I tried that
sentinel_clean_taxa <- sentinel_nopreds %>%
  mutate(species = ifelse(species == "Chydorus sp." | species == "Chydoridae" | species == "Chydorus bicornutus", "Chydorus sphaericus", species)) 
sentinel_clean_taxa <- sentinel_clean_taxa %>%
  mutate(species = ifelse(species == "Bosmina longirostris" | species == "Bosmina sp.", "Bosmina sp.", species))
sentinel_clean_taxa <- sentinel_clean_taxa %>%
  mutate(species = ifelse(species == "Alona setulosa" | species == "Alona quadrangularis" , "Alona sp.", species))
sentinel_clean_taxa <- sentinel_clean_taxa %>%  
  mutate(species = ifelse(species == "Ceriodaphnia reticulata" | species == "Ceriodaphnia quadrangula" | species == "Ceriodaphnia lacustris", "Ceriodaphnia sp.", species))
sentinel_clean_taxa <- sentinel_clean_taxa %>%
  mutate(species = ifelse(species == "Daphnia pulex", "Daphnia pulicaria", species))
#below I am targeting just the Belle lake Daphnia based on Jodie's notes from when she IDed them
sentinel_clean_taxa <- sentinel_clean_taxa %>%
  mutate(species = ifelse(species == "Daphnia sp." & parentdow.year == "470049 2008", "Daphnia rosea", species)) %>% 
  #get rid of the one "Daphnia sp." in the sentinel lakes
  filter(species != "Daphnia sp.")
# rename copepod "species" to the taxonomic level we consistently have for them
sentinel_clean_taxa <- sentinel_clean_taxa %>%
  mutate(species = ifelse(grp2 == "calanoids", "calanoids", 
                          ifelse(grp2 == "cyclopoids", "cyclopoids", species)))
#fix group 2 classifications per Kylie's instructions
sentinel_clean_taxa <- sentinel_clean_taxa %>%
  mutate(grp2 = ifelse(species == "Daphnia sp." | species == "Daphnia rosea", "small cladocerans",
                       ifelse(species == "nauplii" | species == "copepodites", "immature copepods", 
                              ifelse(species == "Harpacticoida", "harpacticoids",  grp2))))

#check that it worked
table(sort(sentinel_clean_taxa$species))
#yay!

#Now I might have multiple rows with the same species in the same sample after renaming things (especially copepods because I lumped a lot together)
#Here I make only row row per species per sample, and I sum the density, biomass, number percent, weight percent, and count if multiple rows
#this will only keep the columns in the dataset that I want
#Only keeping metrics I can sum together for rows
sentinel_copepod_rows <- sentinel_clean_taxa %>% 
  group_by(parentdow.year, parentdow, lake_name, sample_date, year, month, sample_id, site_number, is_slice_lake, net_mouth_diameter_cm, species) %>% 
  summarize(density = sum(density),
            biomass = sum(biomass),
            number_pct = sum(number_pct),
            weight_pct = sum(weight_pct),
            count = sum(count),
            .groups = 'drop')


#need to make sure all species have a row for all tows - even if the biomass value is 0 so that my means calculate correctly
#How many rows should I end up with?
n_distinct(sentinel_clean_taxa$parentdow.year, sentinel_clean_taxa$parentdow, sentinel_clean_taxa$lake_name, sentinel_clean_taxa$sample_date, sentinel_clean_taxa$year, sentinel_clean_taxa$month, sentinel_clean_taxa$sample_id)
length(unique(sentinel_clean_taxa$species))
#we have 1135 tows and they should each have 27 species so we should end up with a data frame that has 1135 * 27 = 30645 rows
#make all the empty rows you need, preserve the groups you need to average and other data you still want in each row, and fill the data values with 0 for the new rows
sentinel_complete <- complete(data = sentinel_copepod_rows, nesting(parentdow.year, parentdow, lake_name, sample_date, year, month, site_number, is_slice_lake, net_mouth_diameter_cm, sample_id), species, fill = list(density = 0, biomass = 0, number_pct = 0, weight_pct = 0, mean_weight = 0, mean_length = 0, count = 0), explicit = FALSE)
#The number of rows looks good!

#CALCULATE ANNUAL BIOMASS SUMMARIES

#first need to average tow biomasses in each month in each lake-year for each species
sentinel_biom_month_mean <- sentinel_complete %>%
  group_by(parentdow, lake_name, year, month, species) %>%
  summarize(biomass = mean(biomass), .groups = 'drop') 

#finally average monthly biomasses to get average biomass in each lake-year for each species
sentinel_biom_year_mean <- sentinel_biom_month_mean %>%
  group_by(parentdow, lake_name, year, species) %>%
  summarize(biomass = mean(biomass), .groups = 'drop')

#convert to wide so there is a column for each species
sentinel_wide <- pivot_wider(data = sentinel_biom_year_mean, names_from = species, values_from = biomass)

#what is the year coverage like now?
table(sentinel_wide$lake_name , sentinel_wide$year)
#decent

#select only lakes with at least 3 years (otherwise NMDS doesn't work)
sentinel_coverage <- as.data.frame(table(sentinel_wide$lake_name , sentinel_wide$year)) %>%
  group_by(Var1) %>%
  summarize(year.count = sum(Freq),
            .groups = 'drop')
#the years with problems are Hill (south), Peltier, and Red Sand
sentinel_wide_3plus <- sentinel_wide %>% 
  filter(lake_name != "Hill (south)" & lake_name != "Peltier" & lake_name != "Red Sand")

#make a Kylie/Jodie column
sentinel_wide_analyst <- sentinel_wide_3plus %>% 
  mutate(analyst = ifelse(year < 2020, "Jodie", "Kylie"))

#make a plot for each lake in a for loop

#get list of unique lakes
sentinel_lakes <- unique(sentinel_wide_analyst$lake_name)

#Create empty list to store the plots
Analyst_NMDS_plots <- list()
#Create empty lists to store NMDS species score data
Analyst_NMDS_scores <- list()
Analyst_NMDS_score_plots <- list()

for(i in sentinel_lakes){
  #filter out data for correct lake
  plot.data <- sentinel_wide_analyst %>% 
    filter(lake_name == i)
  
  #make analyst vector for color and a year vector just to keep track
  analyst <- plot.data$analyst
  years <- plot.data$year
  
  #get rid of all non-biomass columns
  plot.data.biom <- plot.data %>% 
    select(-parentdow, -lake_name, -year, -analyst)
  
  #get rid of taxa not present in that lake in any year
  plot.data.clean <- plot.data.biom %>% #remove columns where column sum is 0
    select(where( ~sum(.x) != 0))
  
  #Calculate relative abundances
  biom_rel_abundance <- plot.data.clean / rowSums(plot.data.clean)
  
  #square root transform to do Hellinger transformation to downweight rare species and deal with the many zeroes
  biom_rel_abun_Holl <- sqrt(biom_rel_abundance)
  
  #Make an NMDS with euclidean distance on Hellinger-transformed biomass data
  biom_euclidean_dist <- metaMDS(biom_rel_abun_Holl, distance = "euclidean")
  #extract axis scores for the sites (samples) only
  NMDS.scores <- as.data.frame(scores(biom_euclidean_dist, display = "sites"))
  #add LakeName and net size to the Plot.Data as a column
  NMDS.scores$lake_name <- i
  NMDS.scores$analyst <- as.factor(analyst)
  NMDS.scores$year <- as.factor(years)
  
  #set max and min values for axes
  max <- max(NMDS.scores[,1:2])
  min <- min(NMDS.scores[,1:2])
  
  
  #make the NMDS plot
  NMDS <- ggplot(data = NMDS.scores, aes(x = NMDS1, y = NMDS2, color = analyst)) +
    geom_point(size = 3)+
    labs(title = paste(i, "Biomass"), y = "NMDS2", x = "NMDS1") +
    geom_text(aes(label = year), vjust = -0.5)+
    #coord_fixed()+
    #lims(x = c(min, max), y = c(min, max))+
    theme_classic()

  #extract species scores
  spec_scores <- as.data.frame(scores(biom_euclidean_dist$species))
  spec_scores$species <- rownames(spec_scores)
  
  #plot species scores
  scores1 <- ggplot(data = spec_scores, aes(y = MDS1, x = species))+
    geom_col()+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  
  scores2 <- ggplot(data = spec_scores, aes(y = MDS2, x = species))+
    geom_col()+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

  #save the plot with the lake name as its name
  Analyst_NMDS_plots[[i]] <- NMDS
  #save the species scores with lake name as its name
  Analyst_NMDS_scores[[i]] <- spec_scores
  #same species score plots
  Analyst_NMDS_score_plots[[paste0(i, "MDS1")]] <- scores1
  Analyst_NMDS_score_plots[[paste0(i, "MDS2")]] <- scores2
}

#save all these plots outside of R
# iwalk(Analyst_NMDS_plots, ~{
#   ggsave(filename = paste0(.y, ".png"), plot = .x, width = 8, height = 6, units = "in", dpi = 300)
# })
# 
# iwalk(Analyst_NMDS_score_plots, ~{
#   ggsave(filename = paste0(.y, ".png"), plot = .x, width = 8, height = 6, units = "in", dpi = 300)
# })

#make one with all the sentinel lakes together
#BE CAREFUL WITH THE CODE BELOW AND RUN IN ORDER - I OVERWRITE THINGS WITH THE SAME NAMES... SORRY BUT NO TIME TO FIX

#Create vectors for color by lake and shape by analyst, also one for year
lake.name <- sentinel_wide$lake_name
sentinel_wide_analyst <- sentinel_wide %>% 
  mutate(analyst = ifelse(year < 2020, "Jodie", "Kylie"))
analyst <- sentinel_wide_analyst$analyst
year <- sentinel_wide$year

#Calculate relative abundance and remove the identifier columns
biom_rel_abundance <- sentinel_wide[,4:30] / rowSums(sentinel_wide[,4:30])

#square root transform to do Hellinger transformation to downweight rare species and deal with the many zeroes
biom_rel_abun_Holl <- sqrt(biom_rel_abundance)

#Make an NMDS with euclidean distance on Hellinger-transformed biomass data
biom_euclidean_dist <- metaMDS(biom_rel_abun_Holl, distance = "euclidean")
#extract axis scores for the sites (samples) only
Plot.Data <- as.data.frame(scores(biom_euclidean_dist, display = "sites"))
#add LakeName and analyst to the Plot.Data as a column
Plot.Data$lake_name <- as.factor(lake.name)
Plot.Data$analyst <- as.factor(analyst)
Plot.Data$year <- as.factor(year)


#plot with color by lake and shape by analyst
NMDS_biom_analyst_sentinel <- ggplot(data = Plot.Data, aes(x = NMDS1, y = NMDS2, color = lake_name, shape = analyst)) +
  geom_point(size = 3, alpha = 0.7)+
  labs(title = "Sentinel Lakes Biomass", y = "NMDS2", x = "NMDS1") +
  scale_shape_manual(values = c("Kylie" = 17, "Jodie" = 16))+
  theme_classic()
NMDS_biom_analyst_sentinel

#plot with color by analyst
NMDS_biom_analyst2_sentinel <- ggplot(data = Plot.Data, aes(x = NMDS1, y = NMDS2, color = analyst)) +
  geom_point(size = 3, alpha = 0.7)+
  labs(title = "Sentinel Lakes Biomass", y = "NMDS2", x = "NMDS1") +
  theme_classic()
NMDS_biom_analyst2_sentinel

# #plot with color by year
# NMDS_biom_year <- ggplot(data = Plot.Data, aes(x = NMDS1, y = NMDS2, color = year)) +
#   geom_point(size = 3)+
#   labs(title = "Biomass", y = "NMDS2", x = "NMDS1") +
#   theme_classic()
# NMDS_biom_year
# #this year plot is not very informative since we lose track of individual lakes and Kylie is all the recent years

#extract species scores
spec_scores <- as.data.frame(scores(biom_euclidean_dist$species))
spec_scores$species <- rownames(spec_scores)

#plot species scores
scores1_sentinel <- ggplot(data = spec_scores, aes(y = MDS1, x = species))+
  geom_col()+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

scores2_sentinel <- ggplot(data = spec_scores, aes(y = MDS2, x = species))+
  geom_col()+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

scores1_sentinel
scores2_sentinel



#Do this with all of my zoop data
#Create vectors for color by lake and shape by analyst, also one for year
lake.name <- zoop_wide$lake_name
zoop_wide_analyst <- zoop_wide %>% 
  mutate(analyst = ifelse(year < 2020, "Jodie", "Kylie"))
analyst <- zoop_wide_analyst$analyst
year <- zoop_wide$year

#Calculate relative abundance and remove the identifier columns
biom_rel_abundance <- zoop_wide[,4:33] / rowSums(zoop_wide[,4:33])

#square root transform to do Hellinger transformation to downweight rare species and deal with the many zeroes
biom_rel_abun_Holl <- sqrt(biom_rel_abundance)

#Make an NMDS with euclidean distance on Hellinger-transformed biomass data
biom_euclidean_dist <- metaMDS(biom_rel_abun_Holl, distance = "euclidean")
#extract axis scores for the sites (samples) only
Plot.Data <- as.data.frame(scores(biom_euclidean_dist, display = "sites"))
#add LakeName and analyst to the Plot.Data as a column
Plot.Data$lake_name <- as.factor(lake.name)
Plot.Data$analyst <- as.factor(analyst)
Plot.Data$year <- as.factor(year)


#plot with color by lake and shape by analyst
NMDS_biom_analyst_GraceLakes <- ggplot(data = Plot.Data, aes(x = NMDS1, y = NMDS2, color = lake_name, shape = analyst)) +
  geom_point(size = 3, alpha = 0.7)+
  labs(title = "GRace's Lakes Biomass", y = "NMDS2", x = "NMDS1") +
  scale_shape_manual(values = c("Kylie" = 17, "Jodie" = 16))+
  theme_classic()
NMDS_biom_analyst_GraceLakes

#plot with color by analyst
NMDS_biom_analyst2_GraceLakes <- ggplot(data = Plot.Data, aes(x = NMDS1, y = NMDS2, color = analyst)) +
  geom_point(size = 3, alpha = 0.7)+
  labs(title = "Grace's Lakes Biomass", y = "NMDS2", x = "NMDS1") +
  theme_classic()
NMDS_biom_analyst2_GraceLakes


#extract species scores
spec_scores <- as.data.frame(scores(biom_euclidean_dist$species))
spec_scores$species <- rownames(spec_scores)

#plot species scores
scores1_G <- ggplot(data = spec_scores, aes(y = MDS1, x = species))+
  geom_col()+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

scores2_G <- ggplot(data = spec_scores, aes(y = MDS2, x = species))+
  geom_col()+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

scores1_G
scores2_G

#save these plots
# ggsave(filename = "All_Sentinel_Lake_Analyst.png", plot = NMDS_biom_analyst_sentinel, width = 8, height = 6, units = "in", dpi = 300)
# ggsave(filename = "All_Sentinel_Analyst.png", plot = NMDS_biom_analyst2_sentinel, width = 8, height = 6, units = "in", dpi = 300)
# ggsave(filename = "All_Sentinel_MDS1.png", plot = scores1_sentinel, width = 8, height = 6, units = "in", dpi = 300)
# ggsave(filename = "All_Sentinel_MDS2.png", plot = scores2_sentinel, width = 8, height = 6, units = "in", dpi = 300)
# ggsave(filename = "Grace_Lakes_Lake_Analyst.png", plot = NMDS_biom_analyst_GraceLakes, width = 8, height = 6, units = "in", dpi = 300)
# ggsave(filename = "Grace_Lakes_Analyst.png", plot = NMDS_biom_analyst2_GraceLakes, width = 8, height = 6, units = "in", dpi = 300)
# ggsave(filename = "Grace_Lakes_MDS1.png", plot = scores1_G, width = 8, height = 6, units = "in", dpi = 300)
# ggsave(filename = "Grace_Lakes_MDS2.png", plot = scores2_G, width = 8, height = 6, units = "in", dpi = 300)



#Do the entire thing I did with Sentinel Lakes but this time with Large Lakes
#Starting from the top! Do everything except match to my inclusion table
#start with the zoop_parentdow dataframe

#isolate large lakes
large <- zoop_parentdow %>% 
  filter(is_ll_lake == "True")

#Do a few checks that were previously done in inclusion table:
#isolate just summer months we will use (May - September)
large_summer <- large %>%
  filter(month == "05" | month == "06" | month == "07" | month == "08" | month == "09")

#find and remove any littoral samples, horizontal samples, night samples, closing nets, oblique tows, and LMB stomachs
large_summer_clean <- large_summer %>% 
  filter(!grepl(pattern = "littoral", x = remarks, ignore.case = TRUE)) %>% 
  filter(!grepl(pattern = "horizontal", x = remarks, ignore.case = TRUE)) %>% 
  filter(!grepl(pattern = "night", x = remarks, ignore.case = TRUE)) %>% 
  filter(!grepl(pattern = "closing", x = remarks, ignore.case = TRUE)) %>% 
  filter(!grepl(pattern = "oblique", x = remarks, ignore.case = TRUE)) %>% 
  filter(!grepl(pattern = "LMB", x = remarks, ignore.case = TRUE))

#calculate number of unique summer months for each parentdow.year
large_summer_month_count <- large_summer_clean %>%
  group_by(parentdow.year) %>%
  summarize(Zoop.Month.Count = n_distinct(month), .groups = 'drop')

#filter to only those with all 5 months and get rid of any lakes without parentdows - these will be useless to me anyways
large_5month <- large_summer_month_count %>% 
  filter(Zoop.Month.Count == 5) %>% 
  filter(!grepl(pattern = "NA", x = parentdow.year, ignore.case = TRUE))


#I want more info than parentdow.year in my inclusion table, so joining detailed (and filtered) data back to this list
large_5month_detail <- left_join(large_5month, large_summer_clean, by = "parentdow.year")

#check for the flags you want to individually assess
large_check <- large_5month_detail %>% 
  filter(grepl(pattern = "composite", x = remarks, ignore.case = TRUE) |
           grepl(pattern = "special", x = remarks, ignore.case = TRUE) |
           grepl(pattern = "depth", x = remarks, ignore.case = TRUE) |
           grepl(pattern = "shallow", x = remarks, ignore.case = TRUE) |
           grepl(pattern = "deep", x = remarks, ignore.case = TRUE)
  )
#get rid of these that are problems, keep deep tows
large_5month_clean <- large_5month_detail %>% 
  filter(!(grepl(pattern = "composite", x = remarks, ignore.case = TRUE) |
                  grepl(pattern = "special", x = remarks, ignore.case = TRUE) |
                  grepl(pattern = "shallow", x = remarks, ignore.case = TRUE) |
             grepl(pattern = "depth", x = remarks, ignore.case = TRUE)
  ))

#make a list of all the unique remarks to check if there is anything else I should filter out
large.remarks <- data.frame(unique(large_5month_clean$remarks))
#looks good

#only some of this is relevant but just running it all to not waste time on this
#Separate Hill, Verm, and Red samples from each other:
large_split <- large_5month_detail %>% 
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
large_deep <- large_split %>% 
  filter(!((lake_name == "Rainy" & site_number == 1) | 
             (lake_name == "Namakan" & site_number == 1) | 
             (lake_name == "Sand Point" & site_number == 1)))

#remove bythotrephes and leptodora
large_nopreds <- large_deep %>% 
  filter(species != "Bythotrephes longimanus" & species != "Leptodora kindti")


#create a dataframe that has a list of name/date/sites that have multiple associated sample ids
large_sample_duplicates <- large_nopreds %>%
  group_by(lake_name, sample_date, site_number) %>%
  filter(n_distinct(sample_id) > 1) %>%
  summarize(sample_ids = paste(unique(sample_id), collapse = ", "),
            .groups = 'drop')

#Check for duplicate taxa within samples
large_taxa_duplicates <- large_nopreds %>%
  group_by(lake_name, sample_id, species) %>%
  filter(n() > 1) %>%
  summarize(num_dups = n(),
            .groups = 'drop')

#There are a lot of problems here. For the purposes of this I am not going to worry about it 

#Taxonomy simplification based on conversations with Kylie
#what taxa do we have to start with?
sort(unique(large_nopreds$species))
#Any how many of each?
table(sort(large_nopreds$species))

#rename taxa that need it based on conversation with Heidi and Kylie
#targeted just the Belle lake Daphnia based on Jodie's notes from when she IDed them
#I know I can run these together but I was getting an error I didn't have time to deal with when I tried that
large_clean_taxa <- large_nopreds %>%
  mutate(species = ifelse(species == "Chydorus sp." | species == "Chydoridae" | species == "Chydorus bicornutus", "Chydorus sphaericus", species)) 
large_clean_taxa <- large_clean_taxa %>%
  mutate(species = ifelse(species == "Bosmina longirostris" | species == "Bosmina sp.", "Bosmina sp.", species))
large_clean_taxa <- large_clean_taxa %>%
  mutate(species = ifelse(species == "Alona setulosa" | species == "Alona quadrangularis" , "Alona sp.", species))
large_clean_taxa <- large_clean_taxa %>%  
  mutate(species = ifelse(species == "Ceriodaphnia reticulata" | species == "Ceriodaphnia quadrangula" | species == "Ceriodaphnia lacustris", "Ceriodaphnia sp.", species))
large_clean_taxa <- large_clean_taxa %>%
  mutate(species = ifelse(species == "Daphnia pulex", "Daphnia pulicaria", species))
#below I am targeting just the Belle lake Daphnia based on Jodie's notes from when she IDed them
large_clean_taxa <- large_clean_taxa %>%
  mutate(species = ifelse(species == "Daphnia sp." & parentdow.year == "470049 2008", "Daphnia rosea", species)) %>% 
  #get rid of the one "Daphnia sp." in the sentinel lakes
  filter(species != "Daphnia sp.")
# rename copepod "species" to the taxonomic level we consistently have for them
large_clean_taxa <- large_clean_taxa %>%
  mutate(species = ifelse(grp2 == "calanoids", "calanoids", 
                          ifelse(grp2 == "cyclopoids", "cyclopoids", species)))
#fix group 2 classifications per Kylie's instructions
large_clean_taxa <- large_clean_taxa %>%
  mutate(grp2 = ifelse(species == "Daphnia sp." | species == "Daphnia rosea", "small cladocerans",
                       ifelse(species == "nauplii" | species == "copepodites", "immature copepods", 
                              ifelse(species == "Harpacticoida", "harpacticoids",  grp2))))

#check that it worked
table(sort(large_clean_taxa$species))
#yay!

#Now I might have multiple rows with the same species in the same sample after renaming things (especially copepods because I lumped a lot together)
#Here I make only row row per species per sample, and I sum the density, biomass, number percent, weight percent, and count if multiple rows
#this will only keep the columns in the dataset that I want
#Only keeping metrics I can sum together for rows
large_copepod_rows <- large_clean_taxa %>% 
  group_by(parentdow.year, parentdow, lake_name, sample_date, year, month, sample_id, site_number, is_slice_lake, net_mouth_diameter_cm, species) %>% 
  summarize(density = sum(density),
            biomass = sum(biomass),
            number_pct = sum(number_pct),
            weight_pct = sum(weight_pct),
            count = sum(count),
            .groups = 'drop')


#need to make sure all species have a row for all tows - even if the biomass value is 0 so that my means calculate correctly
#How many rows should I end up with?
n_distinct(large_clean_taxa$parentdow.year, large_clean_taxa$parentdow, large_clean_taxa$lake_name, large_clean_taxa$sample_date, large_clean_taxa$year, large_clean_taxa$month, large_clean_taxa$sample_id)
length(unique(large_clean_taxa$species))
#we have 3048 tows and they should each have 27 species so we should end up with a data frame that has 3048 * 27 = 82296 rows
#make all the empty rows you need, preserve the groups you need to average and other data you still want in each row, and fill the data values with 0 for the new rows
large_complete <- complete(data = large_copepod_rows, nesting(parentdow.year, parentdow, lake_name, sample_date, year, month, site_number, is_slice_lake, net_mouth_diameter_cm, sample_id), species, fill = list(density = 0, biomass = 0, number_pct = 0, weight_pct = 0, mean_weight = 0, mean_length = 0, count = 0), explicit = FALSE)
#The number of rows looks good!

#CALCULATE ANNUAL BIOMASS SUMMARIES

#first need to average tow biomasses in each month in each lake-year for each species
large_biom_month_mean <- large_complete %>%
  group_by(parentdow, lake_name, year, month, species) %>%
  summarize(biomass = mean(biomass), .groups = 'drop') 

#finally average monthly biomasses to get average biomass in each lake-year for each species
large_biom_year_mean <- large_biom_month_mean %>%
  group_by(parentdow, lake_name, year, species) %>%
  summarize(biomass = mean(biomass), .groups = 'drop')

#convert to wide so there is a column for each species
large_wide <- pivot_wider(data = large_biom_year_mean, names_from = species, values_from = biomass)

#what is the year coverage like now?
table(large_wide$lake_name , large_wide$year)
#decent

#make a Kylie/Jodie column
large_wide_analyst <- large_wide %>% 
  mutate(analyst = ifelse(year < 2020, "Jodie", "Kylie"))

#make a plot for each lake in a for loop

#get list of unique lakes
large_lakes <- unique(large_wide_analyst$lake_name)

#Create empty list to store the plots
Analyst_NMDS_plots <- list()
#Create empty lists to store NMDS species score data
Analyst_NMDS_scores <- list()
Analyst_NMDS_score_plots <- list()

for(i in large_lakes){
  #filter out data for correct lake
  plot.data <- large_wide_analyst %>% 
    filter(lake_name == i)
  
  #make analyst vector for color and a year vector just to keep track
  analyst <- plot.data$analyst
  years <- plot.data$year
  
  #get rid of all non-biomass columns
  plot.data.biom <- plot.data %>% 
    select(-parentdow, -lake_name, -year, -analyst)
  
  #get rid of taxa not present in that lake in any year
  plot.data.clean <- plot.data.biom %>% #remove columns where column sum is 0
    select(where( ~sum(.x) != 0))
  
  #Calculate relative abundances
  biom_rel_abundance <- plot.data.clean / rowSums(plot.data.clean)
  
  #square root transform to do Hellinger transformation to downweight rare species and deal with the many zeroes
  biom_rel_abun_Holl <- sqrt(biom_rel_abundance)
  
  #Make an NMDS with euclidean distance on Hellinger-transformed biomass data
  biom_euclidean_dist <- metaMDS(biom_rel_abun_Holl, distance = "euclidean")
  #extract axis scores for the sites (samples) only
  NMDS.scores <- as.data.frame(scores(biom_euclidean_dist, display = "sites"))
  #add LakeName and net size to the Plot.Data as a column
  NMDS.scores$lake_name <- i
  NMDS.scores$analyst <- as.factor(analyst)
  NMDS.scores$year <- as.factor(years)
  
  #set max and min values for axes
  max <- max(NMDS.scores[,1:2])
  min <- min(NMDS.scores[,1:2])
  
  
  #make the NMDS plot
  NMDS <- ggplot(data = NMDS.scores, aes(x = NMDS1, y = NMDS2, color = analyst)) +
    geom_point(size = 3)+
    labs(title = paste(i, "Biomass"), y = "NMDS2", x = "NMDS1") +
    geom_text(aes(label = year), vjust = -0.5)+
    #coord_fixed()+
    #lims(x = c(min, max), y = c(min, max))+
    theme_classic()
  
  #extract species scores
  spec_scores <- as.data.frame(scores(biom_euclidean_dist$species))
  spec_scores$species <- rownames(spec_scores)
  
  #plot species scores
  scores1 <- ggplot(data = spec_scores, aes(y = MDS1, x = species))+
    geom_col()+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  
  scores2 <- ggplot(data = spec_scores, aes(y = MDS2, x = species))+
    geom_col()+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  
  #save the plot with the lake name as its name
  Analyst_NMDS_plots[[i]] <- NMDS
  #save the species scores with lake name as its name
  Analyst_NMDS_scores[[i]] <- spec_scores
  #same species score plots
  Analyst_NMDS_score_plots[[paste0(i, "MDS1")]] <- scores1
  Analyst_NMDS_score_plots[[paste0(i, "MDS2")]] <- scores2
}

#save all these plots outside of R
# iwalk(Analyst_NMDS_plots, ~{
#   ggsave(filename = paste0(.y, ".png"), plot = .x, width = 8, height = 6, units = "in", dpi = 300)
# })
# 
# iwalk(Analyst_NMDS_score_plots, ~{
#   ggsave(filename = paste0(.y, ".png"), plot = .x, width = 8, height = 6, units = "in", dpi = 300)
# })

#make one with all the large lakes together

#Create vectors for color by lake and shape by analyst, also one for year
lake.name <- large_wide$lake_name
large_wide_analyst <- large_wide %>% 
  mutate(analyst = ifelse(year < 2020, "Jodie", "Kylie"))
analyst <- large_wide_analyst$analyst
year <- large_wide$year

#Calculate relative abundance and remove the identifier columns
biom_rel_abundance <- large_wide[,4:30] / rowSums(large_wide[,4:30])

#square root transform to do Hellinger transformation to downweight rare species and deal with the many zeroes
biom_rel_abun_Holl <- sqrt(biom_rel_abundance)

#Make an NMDS with euclidean distance on Hellinger-transformed biomass data
biom_euclidean_dist <- metaMDS(biom_rel_abun_Holl, distance = "euclidean")
#extract axis scores for the sites (samples) only
Plot.Data <- as.data.frame(scores(biom_euclidean_dist, display = "sites"))
#add LakeName and analyst to the Plot.Data as a column
Plot.Data$lake_name <- as.factor(lake.name)
Plot.Data$analyst <- as.factor(analyst)
Plot.Data$year <- as.factor(year)


#plot with color by lake and shape by analyst
NMDS_biom_analyst_large <- ggplot(data = Plot.Data, aes(x = NMDS1, y = NMDS2, color = lake_name, shape = analyst)) +
  geom_point(size = 3, alpha = 0.7)+
  labs(title = "Large Lakes Biomass", y = "NMDS2", x = "NMDS1") +
  scale_shape_manual(values = c("Kylie" = 17, "Jodie" = 16))+
  theme_classic()
NMDS_biom_analyst_large

#plot with color by analyst
NMDS_biom_analyst2_large <- ggplot(data = Plot.Data, aes(x = NMDS1, y = NMDS2, color = analyst)) +
  geom_point(size = 3, alpha = 0.7)+
  labs(title = "Large Lakes Biomass", y = "NMDS2", x = "NMDS1") +
  theme_classic()
NMDS_biom_analyst2_large

# #plot with color by year
# NMDS_biom_year <- ggplot(data = Plot.Data, aes(x = NMDS1, y = NMDS2, color = year)) +
#   geom_point(size = 3)+
#   labs(title = "Biomass", y = "NMDS2", x = "NMDS1") +
#   theme_classic()
# NMDS_biom_year
# #this year plot is not very informative since we lose track of individual lakes and Kylie is all the recent years

#extract species scores
spec_scores <- as.data.frame(scores(biom_euclidean_dist$species))
spec_scores$species <- rownames(spec_scores)

#plot species scores
scores1_large <- ggplot(data = spec_scores, aes(y = MDS1, x = species))+
  geom_col()+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

scores2_large <- ggplot(data = spec_scores, aes(y = MDS2, x = species))+
  geom_col()+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

scores1_large
scores2_large

#save plots
# ggsave(filename = "All_Large_Lake_Analyst.png", plot = NMDS_biom_analyst_large, width = 8, height = 6, units = "in", dpi = 300)
# ggsave(filename = "All_Large_Analyst.png", plot = NMDS_biom_analyst2_large, width = 8, height = 6, units = "in", dpi = 300)
# ggsave(filename = "All_Large_MDS1.png", plot = scores1_large, width = 8, height = 6, units = "in", dpi = 300)
# ggsave(filename = "All_Large_MDS2.png", plot = scores2_large, width = 8, height = 6, units = "in", dpi = 300)
