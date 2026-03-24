#This script compares the CPUE metrics calculated from the fish database to the biomass estimates for 7 common species that Denver made for my lakes
#The biomass estimates were only created in lakes where 100% of the 7 seven species had length measurements

#Denver's description: "I started with your original dataset (Pelagic_Fish_data.csv) and added three columns: pred_wt, lower, and upper.
#In short, I filtered the data to the seven species used in the model (walleye, northern pike, black crappie, largemouth bass, smallmouth bass, 
#bluegill, and yellow perch) and identified lake-years where 100% of fish for these species had recorded lengths (n = 117 lake-years). 
#For each fish, I generated predicted weights from the fitted model using 500 posterior expected draws. 
#The pred_wt column represents the median predicted weight, and lower and upper give the 95% credible interval. 
#Random effects were included in the predictions, so when a lake-year existed in the model fit, the corresponding lake-year intercept was used 
#(which should be the case for all of these lake-years). Predictions were generated on the log scale used in the model and then back-transformed to grams."

#load packages
library(tidyverse)
library(ggplot2)
library(vegan)

#read in fish data with predicted weights
fish.data <- read.csv("Data/Input/Pelagic_Fish_Data_pred_wt.csv")

#Do literally everything you do to calculate CPUE in the "Create Contemporary Dataset" script (literally copied and pasted)

        
        #check a few things:
        #should all be gillnet
        unique(fish.data$sampling_method)
        #Good
        
        #should all have net nights for effort units
        unique(fish.data$total_effort_1_units)
        #Good
        
        #should all have caught something
        unique(fish.data$total_effort_nothing_caught)
        #Good
        
        # #I need a list of all the species present
        sort(unique(fish.data$species_1))
        # 
        # #to get MN fish abbreiviations
        # # install.packages("remotes")
        # # remotes::install_github("mnsentinellakes/mnsentinellakes")
        # library(mnsentinellakes)
        # data("fishabbreviations")
        # view(fishabbreviations)
        # #export this as a .csv for easy reference
        # #write.csv(fishabbreviations, file = "Data/Output/MNFishAbbreviations.csv")
        
        
        #investigate redhorses
        redhorses <- fish.data %>%
          filter(species_1 == "redhorse" | species_1 == "shorthead_redhorse" | species_1 == "greater_redhorse" | species_1 == "silver_redhorse" | species_1 == "golden_redhorse" | species_1 == "river_redhorse")
        #how many of each are there
        table(redhorses$species)
        #Not that many of them (115 not to species level)
        #Let's just group the redhorses AND THE BULLHEADS because red lake does not distinguish bullhead species
        fish.data.redhorse <- fish.data %>% 
          mutate(species_1 = ifelse((species_1 == "redhorse" | species_1 == "shorthead_redhorse" | species_1 == "greater_redhorse" | species_1 == "silver_redhorse"), "redhorse", 
                                    ifelse(species_1 == "black_bullhead" | species_1 == "brown_bullhead" | species_1 == "brown_bullhead", "bullhead", species_1)))
        #check
        sort(unique(fish.data.redhorse$species_1))
        #looks good
        
        #need to calculate combined effort of stratified surveys 
        #separate out shallow + deep stratified surveys from the standard gillnet surveys
        stratified <- fish.data.redhorse %>% 
          filter(sampling_method == "gill_net_stratified_shallow" | sampling_method == "gill_net_stratified_deep")
        
        #get one row for each lake/year/gillnet type
        stratified_surveys <- stratified %>%
          group_by(lake_id, lake_name, year, sampling_method) %>%
          summarize(total_effort_1 = first(total_effort_1), .groups = 'drop')
        
        #add effort from shallow + deep stratified surveys
        combined_stratified_effort <- stratified_surveys %>%
          group_by(lake_id, year) %>%
          summarize(total_effort_cse = sum(total_effort_1), .groups = 'drop')
        
        #join this sum back to the stratified data
        stratified_sum <- left_join(stratified, combined_stratified_effort, by = c("lake_id", "year"))
        
        #remove original effort column 
        stratified_sum_order <- stratified_sum %>% 
          select(-total_effort_1) %>% 
          rename(total_effort_1 = total_effort_cse) %>% #rename the new combined effort to match the rest of the data
          relocate(total_effort_1, .after = total_effort_ident) #reorder to put effort column back in original location
        
        #remove the stratified rows from the original dataset
        fish_no_strat <- fish.data.redhorse %>% 
          filter(sampling_method != "gill_net_stratified_shallow" & sampling_method != "gill_net_stratified_deep")
        
        #now paste the new rows back in with the updated effort - now my cpue calculations will be correct
        fish_effort_corrected <- rbind(fish_no_strat, stratified_sum_order)
        
        
        #check if I have any lake-years with more than one fish survey
        fish_survey_test <- fish_effort_corrected %>%
          group_by(lake_id, year, total_effort_ident) %>% 
          summarize(lake_id = first(lake_id),
                    year = first(year),
                    total_effort_ident = first(total_effort_ident), 
                    .groups = 'drop') %>% 
          count(lake_id, year) %>%
          filter(n > 1)
        
        #okay the only ones are the stratified deep/shallow so we are good
        
        #Split Hill and Vermilion Fish data
        #Vermilion: East = 1-12, West = 13-20
        #Hill: South = sites 1-3, North = sites 4-12
        
        # #explore effort in Hill and Vermilion
        # verm.hill <- fish_effort_corrected %>% 
        #   filter(lake_name == "Vermilion"| lake_name == "Hill") %>% 
        #   relocate(total_effort_1, .after = site_id)
        # table(verm.hill$lake_name, verm.hill$total_effort_1)
        # #okay, it looks like each site has a net for one night each year so I can use the number of sites in each basin to split up total effort
        
        fish_split <- fish_effort_corrected %>% 
          #Split vermilion and Hill names
          mutate(lake_name = ifelse(lake_name == "Vermilion" & (site_id == "GN1" | site_id == "GN2" | site_id == "GN3" | site_id == "GN4" | site_id == "GN5" | site_id == "GN6" | site_id == "GN7" | site_id == "GN8" | site_id == "GN9" | site_id ==  "GN10" | site_id == "GN11" | site_id == "GN12"), "East Vermilion",
                                    ifelse(lake_name == "Vermilion" & (site_id == "GN13" | site_id == "GN14" | site_id == "GN15" | site_id == "GN16" | site_id == "GN17" | site_id == "GN18" | site_id == "GN19" | site_id == "GN20" ), "West Vermilion",
                                           ifelse(lake_name == "Hill" & (site_id == "GN1" | site_id == "GN2" | site_id == "GN3"), "Hill (south)",
                                                  ifelse(lake_name == "Hill" & (site_id == "GN4" | site_id == "GN5" | site_id == "GN6" | site_id == "GN7" | site_id == "GN8" | site_id == "GN9" | site_id ==  "GN10" | site_id == "GN11" | site_id == "GN12"), "Hill (north)", lake_name))))) %>% 
          #calculate parentdow for all
          mutate(parentdow = str_sub(lake_id, 1, -3)) %>%  #with the negative this chops off the last two digits no matter if there are a total of 7 or 8 (stops 3 characters from the end)
          #put the parentdow column near the beginning of the column order
          relocate(parentdow, .after = lake_id) %>% 
          #Fix vermilion and Hill parentdows
          mutate(parentdow = ifelse(lake_name == "East Vermilion", "69037801",
                                    ifelse(lake_name == "West Vermilion", "69037802",
                                           ifelse(lake_name == "Hill (south)", "1014202",
                                                  ifelse(lake_name == "Hill (north)", "1014201", parentdow))))) %>% 
          #relocate site and total effort columns for easy checking that this worked
          relocate(site_id, .after = parentdow) %>% 
          relocate(total_effort_1, .after = site_id) %>% 
          #adjust total effort for Hill and Vermilion
          mutate(total_effort_1 = ifelse(lake_name == "East Vermilion", 12,
                                         ifelse(lake_name == "West Vermilion", 8, 
                                                ifelse(lake_name == "Hill (north)", 9,
                                                       ifelse(lake_name == "Hill (south)", 3, total_effort_1)))))
        
                
  

#select just the columns I want from this dataset to calculate CPUE and biomass per unit effort
fish_select <- fish_split %>% 
  select(lake_name, lake_id, year, total_effort_1, species_1, pred_wt, lower, upper)


#how many of the fish did we predict for?
summary(fish_select$pred_wt)
#88977 NA values (all the other species) out of 170923 fish = 52% of the fish NOT predicted

#isolate fish with predicted weights and remove the flag columns
fish_filter <- fish_select %>% 
  filter(!(is.na(pred_wt)))

#check species
unique(fish_filter$species)
#looks good, just the 7 species we predicted weights for

#summarize count and biomass total for each for each lake-year-species, then calculate CPUE and bpue (biomass per unit effort) - going to do this more efficiently than I did in the other script
cpue_long <- fish_filter %>% 
  group_by(lake_name, lake_id, year, total_effort_1, species_1) %>%
  summarize(count = n(),
            biomass_total = sum(pred_wt),
            .groups = 'drop') %>% 
  mutate(cpue = count/total_effort_1,
         bpue = biomass_total/total_effort_1)

#Let's do some fun plots to compare cpue and bpue
linear <- ggplot(data = cpue_long, aes(x = cpue, y = bpue))+
  geom_point()+
  facet_wrap(~species_1, scales = "free")+
  #geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")+ #add a 1:1 line
  geom_smooth(method = "lm")+
  labs(y = "biomass (g)/net night", x = "count/net night")+
  theme_classic()+
  theme(axis.title = element_text(size = 15))
linear

#and then let's do a quick look at multivariate space differences

smooth <- ggplot(data = cpue_long, aes(x = cpue, y = bpue))+
  geom_point()+
  facet_wrap(~species_1, scales = "free")+
  #geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")+ #add a 1:1 line
  geom_smooth()+
  labs(y = "biomass (g)/net night", x = "count/net night")+
  theme_classic()+
  theme(axis.title = element_text(size = 15))
smooth

#save the plots
# ggsave(filename = "CPUE_Biomass_linear.png", plot = linear, width = 8, height = 6, units = "in", dpi = 300)
# ggsave(filename = "CPUE_Biomass_smooth.png", plot = smooth, width = 8, height = 6, units = "in", dpi = 300)

#check out multivariate space

#first pivot wider
cpue_select <- cpue_long %>% 
  select(lake_name, year, species_1, cpue)
cpue_wide <- pivot_wider(data = cpue_select, names_from = species_1, names_prefix = "cpue_", values_from = cpue, values_fill = 0)

bpue_select <- cpue_long %>% 
  select(lake_name, year, species_1, bpue)
bpue_wide <- pivot_wider(data = bpue_select, names_from = species_1, names_prefix = "bpue_", values_from = bpue, values_fill = 0)
  
  
#Create vectors for color by lake and shape by analyst, also one for year
lake.name <- cpue_wide$lake_name
year <- cpue_wide$year

#Calculate relative abundance and remove the identifier columns
cpue_rel_abundance <- cpue_wide[,3:9] / rowSums(cpue_wide[,3:9])
bpue_rel_abundance <- bpue_wide[,3:9] / rowSums(bpue_wide[,3:9])

#square root transform to do Hellinger transformation to downweight rare species and deal with the many zeroes
cpue_rel_abun_Holl <- sqrt(cpue_rel_abundance)
bpue_rel_abun_Holl <- sqrt(bpue_rel_abundance)

#Make an NMDS with euclidean distance on Hellinger-transformed CPUE data
cpue_euclidean_dist <- metaMDS(cpue_rel_abun_Holl, distance = "euclidean")
#extract axis scores for the sites (samples) only
cpue_Plot.Data <- as.data.frame(scores(cpue_euclidean_dist, display = "sites"))
#add LakeName and year to the Plot.Data as a column
cpue_Plot.Data$lake_name <- as.factor(lake.name)
cpue_Plot.Data$year <- as.factor(year)


#plot with color by lake
NMDS_cpue <- ggplot(data = cpue_Plot.Data, aes(x = NMDS1, y = NMDS2, color = lake_name)) +
  geom_point(size = 3, alpha = 0.7)+
  labs(title = "NMDS: Hellinger-Transformed CPUE", y = "NMDS2", x = "NMDS1")+
  theme_classic()+
  theme(axis.title = element_text(size = 12), plot.title = element_text(size =15))
NMDS_cpue



#Make an NMDS with euclidean distance on Hellinger-transformed BPUE data
bpue_euclidean_dist <- metaMDS(bpue_rel_abun_Holl, distance = "euclidean")
#extract axis scores for the sites (samples) only
bpue_Plot.Data <- as.data.frame(scores(bpue_euclidean_dist, display = "sites"))
#add LakeName and year to the Plot.Data as a column
bpue_Plot.Data$lake_name <- as.factor(lake.name)
bpue_Plot.Data$year <- as.factor(year)


#plot with color by lake and shape by analyst
NMDS_bpue <- ggplot(data = bpue_Plot.Data, aes(x = NMDS1, y = NMDS2, color = lake_name)) +
  geom_point(size = 3, alpha = 0.7)+
  labs(title = "NMDS: Hellinger-Transformed Biomass/net night", y = "NMDS2", x = "NMDS1")+
  theme_classic()+
  theme(axis.title = element_text(size = 12), plot.title = element_text(size =15))
NMDS_bpue

#save the plots
# ggsave(filename = "CPUE_NMDS.png", plot = NMDS_cpue, width = 8, height = 6, units = "in", dpi = 300)
# ggsave(filename = "Biomass_NMDS.png", plot = NMDS_bpue, width = 8, height = 6, units = "in", dpi = 300)
