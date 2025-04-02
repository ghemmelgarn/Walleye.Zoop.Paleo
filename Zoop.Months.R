#This scripts investigates the effect of which months of sampling are present in zooplankton data

#goal is to answer these questions in terms of cladoceran species richness, cladoceran shannon diversity, and all zoop biomass
#examine effect of removing each month - which one has the biggest effect? least effect? Pay attention to the months I am often missing
#which months matter the most?

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
zoop_months <- filter(zoop_months, species != "Bythotrephes longimanus" & species != "Leptodora kindti")

#remove lakes without DOWs
zoop_months <- filter(zoop_months, dowlknum != 0)

#clean up zoop taxonomy for shannon diversity
#check species present
unique(zoop_months$species)
# 1. remove all the copepods
zoop_nocopepods <- zoop_months %>%
  filter(grp == "Cladocerans")
# 2. rename taxa that need it based on conversation with Heidi and Kylie
#targeted just the Belle lake Daphnia based on Jodie's notes from when she IDed them
#I know I can run these together but I was getting an error I didn't have time to deal with when I tried that
zoop_clean_taxa <- zoop_nocopepods %>%
  mutate(species = ifelse(species == "Chydorus sp." | species == "Chydoridae" | species == "Chydorus bicornutus", "Chydorus sphaericus", species)) 
zoop_clean_taxa <- zoop_clean_taxa %>%
  mutate(species = ifelse(species == "Bosmina longirostris", "Bosmina sp.", species))
zoop_clean_taxa <- zoop_clean_taxa %>%
  mutate(species = ifelse(species == "Alona setulosa" | species == "Alona quadrangularis" , "Alona sp.", species))
zoop_clean_taxa <- zoop_clean_taxa %>%  
  mutate(species = ifelse(species == "Ceriodaphnia reticulata" | species == "Ceriodaphnia quadrangula" | species == "Ceriodaphnia lacustris", "Ceriodaphnia sp.", species))
zoop_clean_taxa <- zoop_clean_taxa %>%
  mutate(species = ifelse(species == "Daphnia pulex", "Daphnia pulicaria", species))
#below I am targeting just the Belle lake Daphnia based on Jodie's notes from when she IDed them
zoop_clean_taxa <- zoop_clean_taxa %>%
  mutate(species = ifelse(species == "Daphnia sp." & parentdow.zoop.year == "470049 2008", "Daphnia rosea", species))
# 3. remove the Daphnia sp. observations without species - counts are all low
zoop_clean_taxa2 <- zoop_clean_taxa %>%
  filter(species != "Daphnia sp.")
# 4. remove other problematic taxononimic resolutions with low counts
zoop_clean_taxa2 <- zoop_clean_taxa2 %>%
  filter(species != "Pleuroxus sp.")
# 
# #check that it worked
sort(unique(zoop_clean_taxa2$species))
# #yay!


#check that none of these are littoral samples
littoral <- grep("Littoral", zoop_clean_taxa2$remarks)
zoop_clean_taxa_littoral <- zoop_clean_taxa2[littoral,]
#confirmed these are littoral samples
#remove littoral rows
zoop_clean_taxa3 <- zoop_clean_taxa2[-littoral,]

#zoop cladoceran dataset is now clean and ready to use

#also remove littoral samples from the dataset with the copepods
littoral2 <- grep("Littoral", zoop_months$remarks)
zoop_months_littoral <- zoop_months[littoral2,]
#confirmed these are littoral samples
#remove littoral rows
zoop_months_clean <- zoop_months[-littoral2,]

#remove intermediate files to keep environment clean
rm(zoop,
   zoop_clean_taxa,
   zoop_clean_taxa_littoral,
   zoop_clean_taxa2,
   zoop_months,
   zoop_months_littoral,
   zoop_nocopepods,
   zoop_parentdow,
   littoral,
   littoral2
   )


#DATASETS ARE READY:
#The one with all taxa is zoop_months_clean - use for biomass
#The one with just cladocerans is zoop_clean_taxa3 - use for diversity metrics






#-----------------------------------------------------------------------------------------------------------------------------------------------------------
#WHICH MONTHS DO I HAVE IN MY DATA? WHICH ARE THE MOST COMMON TO BE MISSING?

#start by summarizing each survey into parentdow.zoop.year and date
zoop_month_summary <- zoop_months_clean %>%
  group_by(parentdow.zoop.year, sample_date) %>%
  summarize(month = first(month), year = first(year), day = first(day), .groups = 'drop')

#start by calling unique month values
sort(unique(zoop_month_summary$month))
#looks like every month except December
#look at counts of them
table(zoop_month_summary$month)
#majority are May to October - what I expected
#lets focus on just may to October samples - this is what we will use
zoop_summer_month <- zoop_month_summary %>%
  filter(month == "05" | month == "06" | month == "07" | month == "08" | month == "09" | month == "10")
#look at counts again
table(zoop_summer_month$month)
#definitely more sampling in June and Aug than other months
#we will see what effect this has!

#calculate number of unique summer months for each parentdow.zoop.year
zoop_summer_month_count <- zoop_summer_month %>%
  group_by(parentdow.zoop.year) %>%
  summarize(Zoop.Month.Count = n_distinct(month), .groups = 'drop')
#how many lakes have how many months sampled?
table(zoop_summer_month_count$Zoop.Month.Count)

#need to know which months each lake has
#this makes each month a column, and the data is the day it was sampled or NA for no sample that month
zoop_wide <- pivot_wider(data = zoop_summer_month, names_from = month, values_from = day)
#some lake/years have more than one sampling date per month - I am not worried about that right now but important for later steps
#reorder so the months are in chronological order
zoop_wide <- zoop_wide[,c(1,2,3,4,7,6,5,8,9)] 
#rename columns to make them easier to deal with and R doesn't think they are numeric
#Here I am calling them by their column number in the data frame, not the month number
zoop_wide <- zoop_wide %>% 
  rename(May = 4, June = 5, July = 6, Aug = 7, Sept = 8, Oct = 9)

#collapse into one column per lake/year - each month column has the count of days in that month where zoops were sampled
zoop_wide_short <- zoop_wide %>%
  group_by(parentdow.zoop.year) %>%
  summarize(May = length(unique(May[!is.na(May)])), June = length(unique(June[!is.na(June)])), July = length(unique(July[!is.na(July)])), Aug = length(unique(Aug[!is.na(Aug)])), Sept = length(unique(Sept[!is.na(Sept)])), Oct = length(unique(Oct[!is.na(Oct)])), .groups = 'drop')

#join the count of zoop months to this dataframe
zoop_summer_month_all <- left_join(zoop_wide_short, zoop_summer_month_count, by = "parentdow.zoop.year")

#when I only have 5 months, what tends to be missing?
zoop_5_months <-  zoop_summer_month_all %>%
  filter(Zoop.Month.Count == 5)

May5 <- zoop_5_months %>% 
  filter(May == 0) %>%
  nrow()
June5 <- zoop_5_months %>% 
  filter(June == 0) %>%
  nrow()
July5 <- zoop_5_months %>% 
  filter(July == 0) %>%
  nrow()
Aug5 <- zoop_5_months %>% 
  filter(Aug == 0) %>%
  nrow()
Sept5 <- zoop_5_months %>% 
  filter(Sept == 0) %>%
  nrow()
Oct5 <- zoop_5_months %>% 
  filter(Oct == 0) %>%
  nrow()

#when I only have 4 months, what tends to be missing?
zoop_4_months <-  zoop_summer_month_all %>%
  filter(Zoop.Month.Count == 4)

May4 <- zoop_4_months %>% 
  filter(May == 0) %>%
  nrow()
June4 <- zoop_4_months %>% 
  filter(June == 0) %>%
  nrow()
July4 <- zoop_4_months %>% 
  filter(July == 0) %>%
  nrow()
Aug4 <- zoop_4_months %>% 
  filter(Aug == 0) %>%
  nrow()
Sept4 <- zoop_4_months %>% 
  filter(Sept == 0) %>%
  nrow()
Oct4 <- zoop_4_months %>% 
  filter(Oct == 0) %>%
  nrow()
#create a table to show results
Months <- c("May", "June", "July", "Aug", "Sept", "Oct")
Five_missing_count <- c(May5, June5, July5, Aug5, Sept5, Oct5)
Four_missing_count <- c(May4, June4, July4, Aug4, Sept4, Oct4)
surveys_missing_months <- data.frame(Months, Five_missing_count, Four_missing_count)
surveys_missing_months

#we see most lake/years with 5 months are missing october (91), a fair number are missing May (26), and a few missing Sept (13), other months very few
#we see most lake/years with 4 months are missing october (73), a fair number are missing May (41) and  Sept (43), a few missing July (23), other months very few

#overall we almost always have data from June and August, and usually from July
#This is consistent with first observations of most sampled months



#remove unneeded and intermediate files to keep environment clean
rm(zoop_4_months,
   zoop_5_months,
   zoop_month_summary,
   zoop_summer_month,
   zoop_summer_month_count,
   zoop_wide,
   zoop_wide_short,
   Aug4,
   Aug5,
   Five_missing_count,
   Four_missing_count,
   July4,
   July5,
   June4,
   June5,
   May4,
   May5,
   Months,
   Oct4,
   Oct5,
   Sept4,
   Sept5,
   surveys_missing_months
   )






#--------------------------------------------------------------------------------------------------------------------------------------------------------------
#SOME HOUSEKEEPING TO GET READY TO ANSWER THE NEXT FEW QUESTIONS

#join the month data back into the zoop data
zoop_all_months <- left_join(zoop_months_clean, zoop_summer_month_all, by = "parentdow.zoop.year")
zoop_cladoceran_months <- left_join(zoop_clean_taxa3, zoop_summer_month_all, by = "parentdow.zoop.year")

#isolate the summer samples (May to Oct) 
zoop_all_summer <- zoop_all_months %>%
  filter(month == "05" | month == "06" | month == "07" | month == "08" | month == "09" | month == "10")
zoop_clad_summer <- zoop_cladoceran_months %>% 
  filter(month == "05" | month == "06" | month == "07" | month == "08" | month == "09" | month == "10")

#isolate samples that have samples in all 6 summer months
zoop_all_6month <- zoop_all_summer %>% 
  filter(Zoop.Month.Count == 6) 
zoop_clad_6month <- zoop_clad_summer %>% 
  filter(Zoop.Month.Count == 6) 
#these are what I will use for the follow sections

#remove the previous zoop data versions so I don't get confused later
rm(zoop_clean_taxa3,
   zoop_months_clean,
   zoop_all_summer,
   zoop_clad_summer,
   zoop_all_months,
   zoop_cladoceran_months
  )



#------------------------------------------------------------------------------------------------------------------------------------------------------------------
#HOW DOES SAMPLING MONTH AFFECT CLADOCERAN SPECIES RICHNESS?
#use just cladoceran data for this

#calculate species richness in each month
zoop_rich_month <- zoop_clad_6month %>%
  group_by(parentdow.zoop.year, month) %>%
  summarize(spp.richness = n_distinct(species), .groups = 'drop')

#make a boxplot of this
zoop_rich_month$month.num <- as.numeric(zoop_rich_month$month)
#tiff("Spp_Rich_Month_Boxplot.tiff", width = 8, height = 6, units = "in", res = 300) 
spp.rich.by.month <- ggplot(data = zoop_rich_month, aes(x = month.num, y = spp.richness, group = month))+
  geom_boxplot() +
  scale_x_continuous(breaks = seq(05, 10, by = 1)) +
  labs(y = "Cladoceran Species Richness", x = "Month", title = "Cladoceran Species Richness within each Month")+
  theme_classic()
spp.rich.by.month
#dev.off()

#take mean species richness in each month (averaging all the lakes)
zoop_rich_month_mean <- zoop_rich_month %>%
  group_by(month) %>%
  summarize(mean.spp.richness = mean(spp.richness), .groups = 'drop')

#plot a bar graph of mean species richness in each month
spp.rich.month <- ggplot()+
  geom_bar(data = zoop_rich_month_mean, aes(x = month, y = mean.spp.richness), stat = "identity", fill = "gray") +
  labs(y = "Mean Species Richness", x = "Month", title = "Mean Species Richness Across Lakes by Month")+
  theme_classic()
spp.rich.month
#looks like May has slightly lower species richness than the other months in general

#make a species accumulation curve by hand because I couldn't quickly find a function for this that didn't want to do fancy things to the data
#I need to calculate the cumulative species richness in each month
  #may richness
  may <- zoop_clad_6month %>% 
    filter(month == "05") %>% 
    group_by(parentdow.zoop.year) %>%
    summarize(may.rich = n_distinct(species), .groups = 'drop')
  #may+june richness
  june <- zoop_clad_6month %>% 
    filter(month == "05" | month == "06")%>% 
    group_by(parentdow.zoop.year) %>%
    summarize(june.rich = n_distinct(species), .groups = 'drop')
  #may+june+july richness
  july <- zoop_clad_6month %>% 
    filter(month == "05" | month == "06" | month == "07")%>% 
    group_by(parentdow.zoop.year) %>%
    summarize(july.rich = n_distinct(species), .groups = 'drop')
  #may+june+july+august richness
  aug <- zoop_clad_6month %>% 
    filter(month == "05" | month == "06" | month == "07" | month == "08") %>% 
    group_by(parentdow.zoop.year) %>%
    summarize(aug.rich = n_distinct(species), .groups = 'drop')
  #may+june+july+august+september richness
  sept <- zoop_clad_6month %>% 
    filter(month == "05" | month == "06" | month == "07" | month == "08" | month == "09")%>% 
    group_by(parentdow.zoop.year) %>%
    summarize(sept.rich = n_distinct(species), .groups = 'drop')
  #may+june+july+august+september+october richness
  oct <- zoop_clad_6month %>% 
    filter(month == "05" | month == "06" | month == "07" | month == "08" | month == "09" | month == "10") %>% 
    group_by(parentdow.zoop.year) %>%
    summarize(oct.rich = n_distinct(species), .groups = 'drop')
  #combine into one data frame
  month.rich <- data.frame(june$parentdow.zoop.year, june$june.rich, july$july.rich, aug$aug.rich, sept$sept.rich, oct$oct.rich)
  #rename the columns
  month.rich <- month.rich %>% 
    rename(parentdow.zoop.year = june.parentdow.zoop.year,
           june.rich = june.june.rich,
           july.rich = july.july.rich,
           aug.rich = aug.aug.rich,
           sept.rich = sept.sept.rich,
           oct.rich = oct.oct.rich
           )
  #need to add May with a join because it appears that a few lake/years have no cladocerans in May (probably only have copepods), so fewer rows
  clad_accum_data <- left_join(month.rich, may, by = "parentdow.zoop.year")
  #reorder columns so they make sense
  clad_accum_data <- clad_accum_data[,c(1,7,2,3,4,5,6)]
  #make NA values in May = 0
  clad_accum_data[is.na(clad_accum_data)] <- 0
  

#then plot species richness in may, may + june, may + june + july, etc.
  #this is a species accumulation curve
  #first need to put the data in long format
  clad_accum_data_long <- pivot_longer(data = clad_accum_data, cols = 2:7, names_to = "month", values_to = "richness")
  #change month values to numeric so R can plot it
  clad_accum_data_long$month <- ifelse(clad_accum_data_long$month == "may.rich", 5, 
                                       ifelse(clad_accum_data_long$month == "june.rich", 6, 
                                              ifelse(clad_accum_data_long$month == "july.rich", 7, 
                                                     ifelse(clad_accum_data_long$month == "aug.rich", 8, 
                                                            ifelse(clad_accum_data_long$month == "sept.rich", 9, 
                                                                   ifelse(clad_accum_data_long$month == "oct.rich", 10, clad_accum_data_long$month))))))
  #make month values numeric
  clad_accum_data_long$month <- as.numeric(clad_accum_data_long$month)
  
  #now finally plot it
  #mapping color to lake/year so they each get plotted separately, I don't care about actually identifying them
spp.accum.curve <- ggplot(data = clad_accum_data_long, aes(x = month, y = richness, color = parentdow.zoop.year))+
  geom_point() +
  geom_line() + 
  #geom_smooth(se = FALSE) + #uncomment this if you want a smooth curve
  labs(y = "Cumulative Species Richness", x = "Month", title = "Species Accumulation Curve")+
  theme_classic()+
  scale_y_continuous(limits = c(0, 18)) +
  theme(legend.position = "none")
spp.accum.curve
#ok so this is pretty artwork but rather uninformative... 




#lets try doing this with the mean of each month across all lakes
clad_accum_mean <- clad_accum_data_long %>%
  group_by(month) %>%
  summarize(mean.richness = mean(richness), .groups = 'drop')
spp.accum.curve.mean <- ggplot(data = clad_accum_mean, aes(x = month, y = mean.richness))+
  geom_point() +
  geom_line() + 
  #geom_smooth(se = FALSE) + #uncomment this if you want a smooth curve
  labs(y = "Mean Cumulative Species Richness", x = "Month", title = "Species Accumulation Curve (Mean of 263 lake-years)")+
  #scale_y_continuous(limits = c(0, 18)) + #uncomment this if you want y axis same as previous plot
  theme_classic()+
  theme(legend.position = "none")
spp.accum.curve.mean
#easier to see what's going on here

#export both plots together
#tiff("Species_Accumulation_by_Month.tiff", width = 7, height = 9, units = "in", res = 300)
spp.accum.month.plot <- grid.arrange(spp.accum.curve, spp.accum.curve.mean)
spp.accum.month.plot
#dev.off()


#make a boxplot of this
#tiff("Accum_Spp_Rich_Month_Boxplot.tiff", width = 8, height = 6, units = "in", res = 300) 
accum.spp.rich.by.month <- ggplot(data = clad_accum_data_long, aes(x = month, y = richness, group = month))+
  geom_boxplot() +
  scale_x_continuous(breaks = seq(05, 10, by = 1)) +
  labs(y = "Cumulative Cladoceran Species Richness", x = "Month", title = "Cumulative Cladoceran Species Richness by Month")+
  theme_classic()
accum.spp.rich.by.month
#dev.off()

#Here I am messing with a linear model to see if it gives me a nice plot, which it does, but this relationship is NOT LINEAR
#I WOULD NEED TO MAKE THIS MUCH MORE COMPLEX TO WORK - NOT WORTH MY TIME RIGHT NOW
# lm.test <- lm(richness ~ month, data = clad_accum_data_long)
# summary(lm.test)
# lm.test.plot <- ggplot(data = clad_accum_data_long, aes(x = month, y = richness))+
#   geom_point() + 
#   geom_smooth(method = "lm", forumla = y ~ x)
# lm.test.plot


#I also want to calculate the number of new species added each month
clad_spp_add <- clad_accum_data %>% 
  mutate(may.new.spp = may.rich,
         june.new.spp = june.rich - may.rich,
         july.new.spp = july.rich - june.rich,
         aug.new.spp = aug.rich - july.rich,
         sept.new.spp = sept.rich - aug.rich,
         oct.new.spp = oct.rich - sept.rich
  )
#remove columns you don't want
clad_spp_add <- clad_spp_add[,c(1,8:13)]

#plot this data
#first need to put the data in long format
clad_spp_add_long <- pivot_longer(data = clad_spp_add, cols = 2:7, names_to = "month", values_to = "new.spp")
#change month values to numeric so R can plot it
clad_spp_add_long$month <- ifelse(clad_spp_add_long$month == "may.new.spp", 5, 
                                     ifelse(clad_spp_add_long$month == "june.new.spp", 6, 
                                            ifelse(clad_spp_add_long$month == "july.new.spp", 7, 
                                                   ifelse(clad_spp_add_long$month == "aug.new.spp", 8, 
                                                          ifelse(clad_spp_add_long$month == "sept.new.spp", 9, 
                                                                 ifelse(clad_spp_add_long$month == "oct.new.spp", 10, clad_spp_add_long$month))))))
#make month values numeric
clad_spp_add_long$month <- as.numeric(clad_spp_add_long$month)

#now finally plot it
#mapping color to lake/year so they each get plotted separately, I don't care about actually identifying them
spp.addition <- ggplot(data = clad_spp_add_long, aes(x = month, y = new.spp, color = parentdow.zoop.year))+
  geom_point() +
  geom_line() + 
  #geom_smooth(se = FALSE) + #uncomment this if you want a smooth curve
  labs(y = "New Cladoceran Species Detected", x = "Month", title = "New Cladoceran Species Detected by Month")+
  theme_classic()+
  scale_y_continuous(breaks = seq(0, 10, by = 2)) +
  theme(legend.position = "none")
spp.addition
#again, pretty but way too much...

#try this one with the mean as well
spp_add_mean <- clad_spp_add_long %>%
  group_by(month) %>%
  summarize(mean.spp.add = mean(new.spp), .groups = 'drop')
spp.addition.mean <- ggplot(data = spp_add_mean, aes(x = month, y = mean.spp.add))+
  geom_point() + 
  geom_line() +
  #geom_smooth(se = FALSE) + #uncomment this if you want a smooth curve
  labs(y = "New Cladoceran Species Detected", x = "Month", title = "New Cladoceran Species Detected by Month (Mean of 263 lake-years)")+
  scale_y_continuous(breaks = seq(0, 10, by = 2)) + #uncomment this if you want y axis same as previous plot
  theme_classic()+
  theme(legend.position = "none")
spp.addition.mean
#easier to see what's going on here

#export both plots together
#tiff("Species_Addition_by_Month.tiff", width = 8, height = 8, units = "in", res = 300)
spp.add.month.plot <- grid.arrange(spp.addition, spp.addition.mean)
spp.add.month.plot
#dev.off()


#make a boxplot of this
#tiff("Spp_Add_Month_Boxplot.tiff", width = 8, height = 6, units = "in", res = 300) 
spp.add.by.month <- ggplot(data = clad_spp_add_long, aes(x = month, y = new.spp, group = month))+
  geom_boxplot() +
  scale_x_continuous(breaks = seq(05, 10, by = 1)) +
  labs(y = "New Cladoceran Species Detected", x = "Month", title = "New Cladoceran Species Detected by Month")+
  theme_classic()
spp.add.by.month
#dev.off()


#now want to plot species richness with 6 months vs. species richness with each month individually dropped for each lake-year

#calculate species richness with all 6 months present
zoop_rich_all <- zoop_clad_6month %>%
  group_by(parentdow.zoop.year) %>%
  summarize(rich.6.month = n_distinct(species), .groups = 'drop')

#calculate species richness without May
zoop_rich_no_may <- zoop_clad_6month %>%
  filter(month != "05") %>% 
  group_by(parentdow.zoop.year) %>%
  summarize(rich.no.may = n_distinct(species), .groups = 'drop')

#calculate species richness without June
zoop_rich_no_june <- zoop_clad_6month %>%
  filter(month != "06") %>% 
  group_by(parentdow.zoop.year) %>%
  summarize(rich.no.june = n_distinct(species), .groups = 'drop')

#calculate species richness without July
zoop_rich_no_july <- zoop_clad_6month %>%
  filter(month != "07") %>% 
  group_by(parentdow.zoop.year) %>%
  summarize(rich.no.july = n_distinct(species), .groups = 'drop')

#calculate species richness without August
zoop_rich_no_aug <- zoop_clad_6month %>%
  filter(month != "08") %>% 
  group_by(parentdow.zoop.year) %>%
  summarize(rich.no.aug = n_distinct(species), .groups = 'drop')

#calculate species richness without September
zoop_rich_no_sept <- zoop_clad_6month %>%
  filter(month != "09") %>% 
  group_by(parentdow.zoop.year) %>%
  summarize(rich.no.sept = n_distinct(species), .groups = 'drop')

#calculate species richness without October
zoop_rich_no_oct <- zoop_clad_6month %>%
  filter(month != "10") %>% 
  group_by(parentdow.zoop.year) %>%
  summarize(rich.no.oct = n_distinct(species), .groups = 'drop')

#combine these all into one dataframe
rich_month_elim <- data.frame(parentdow.zoop.year = zoop_rich_all$parentdow.zoop.year, 
                              rich.6.month = zoop_rich_all$rich.6.month, 
                              Drop.May = zoop_rich_no_may$rich.no.may,
                              Drop.June = zoop_rich_no_june$rich.no.june,
                              Drop.July = zoop_rich_no_july$rich.no.july,
                              Drop.August = zoop_rich_no_aug$rich.no.aug,
                              Drop.September = zoop_rich_no_sept$rich.no.sept,
                              Drop.October = zoop_rich_no_oct$rich.no.oct
                              )

#now plot these
#because we only have integers, I need to plot each month separately so its legible

rich.drop.may <- ggplot(data = rich_month_elim, aes(y = rich.6.month))+
  geom_abline(slope = 1, intercept = 0, color = "red") +
  geom_count(aes(x = Drop.May), color = "black") +  
  labs(y = "Complete Sample Richness", x = "Drop May Richness", title = "Cladoceran Species Richness: Drop May")+
  theme_classic()
rich.drop.may

rich.drop.june <- ggplot(data = rich_month_elim, aes(y = rich.6.month))+
  geom_abline(slope = 1, intercept = 0, color = "red") +
  geom_count(aes(x = Drop.June), color = "black") + 
  labs(y = "Complete Sample Richness", x = "Drop June Richness", title = "Cladoceran Species Richness: Drop June")+
  theme_classic()
rich.drop.june

rich.drop.july <- ggplot(data = rich_month_elim, aes(y = rich.6.month))+
  geom_abline(slope = 1, intercept = 0, color = "red") +
  geom_count(aes(x = Drop.July), color = "black") + 
  labs(y = "Complete Sample Richness", x = "Drop July Richness", title = "Cladoceran Species Richness: Drop July")+
  theme_classic()
rich.drop.july

rich.drop.aug <- ggplot(data = rich_month_elim, aes(y = rich.6.month))+
  geom_abline(slope = 1, intercept = 0, color = "red") +
  geom_count(aes(x = Drop.August), color = "black") + 
  labs(y = "Complete Sample Richness", x = "Drop August Richness", title = "Cladoceran Species Richness: Drop August")+
  theme_classic()
rich.drop.aug

rich.drop.sept <- ggplot(data = rich_month_elim, aes(y = rich.6.month))+
  geom_abline(slope = 1, intercept = 0, color = "red") +
  geom_count(aes(x = Drop.September), color = "black") + 
  labs(y = "Complete Sample Richness", x = "Drop September Richness", title = "Cladoceran Species Richness: Drop September")+
  theme_classic()


rich.drop.oct <- ggplot(data = rich_month_elim, aes(y = rich.6.month))+
  geom_abline(slope = 1, intercept = 0, color = "red") +
  geom_count(aes(x = Drop.October), color = "black") + 
  labs(y = "Complete Sample Richness", x = "Drop October Richness", title = "Cladoceran Species Richness: Drop October")+
  theme_classic()


#make a layout of all 6 plots together
#tiff("Species_Richness_Month_Drop.tiff", width = 10, height = 10, units = "in", res = 300) 
rich.drop.month.plot <- grid.arrange(rich.drop.may, rich.drop.june, rich.drop.july, rich.drop.aug, rich.drop.sept, rich.drop.oct)
rich.drop.month.plot
#dev.off()




#----------------------------------------------------------------------------------------------------------------------------------------------------
#HOW DOES SAMPLING MONTH AFFECT CLADOCERAN SHANNON DIVERSITY INDEX?
#use just cladoceran data for this


#calculate SDI with all 6 months present

    #first need counts of each species
    zoop_count_all <- zoop_clad_6month %>%
      group_by(parentdow.zoop.year, species) %>%
      summarize(count = sum(count), .groups = 'drop')
    #now calculate Shannon diversity
    zoop_SDI_all <- zoop_count_all %>%
      group_by(parentdow.zoop.year) %>%
      summarize(SDI.6.month = diversity(count, index = "shannon"), .groups = 'drop')

#calculate SDI without May

    #first need counts of each species
    zoop_count_no_may <- zoop_clad_6month %>%
      filter(month != "05") %>% 
      group_by(parentdow.zoop.year, species) %>%
      summarize(count = sum(count), .groups = 'drop')
    #now calculate Shannon diversity
    zoop_SDI_no_may <- zoop_count_no_may %>%
      group_by(parentdow.zoop.year) %>%
      summarize(SDI.no.may = diversity(count, index = "shannon"), .groups = 'drop')

#calculate SDI without June

    #first need counts of each species
    zoop_count_no_june <- zoop_clad_6month %>%
      filter(month != "06") %>% 
      group_by(parentdow.zoop.year, species) %>%
      summarize(count = sum(count), .groups = 'drop')
    #now calculate Shannon diversity
    zoop_SDI_no_june <- zoop_count_no_june %>%
      group_by(parentdow.zoop.year) %>%
      summarize(SDI.no.june = diversity(count, index = "shannon"), .groups = 'drop')


#calculate SDI without July

    #first need counts of each species
    zoop_count_no_july <- zoop_clad_6month %>%
      filter(month != "07") %>% 
      group_by(parentdow.zoop.year, species) %>%
      summarize(count = sum(count), .groups = 'drop')
    #now calculate Shannon diversity
    zoop_SDI_no_july <- zoop_count_no_july %>%
      group_by(parentdow.zoop.year) %>%
      summarize(SDI.no.july = diversity(count, index = "shannon"), .groups = 'drop')

#calculate SDI without August
    
    #first need counts of each species
    zoop_count_no_aug <- zoop_clad_6month %>%
      filter(month != "08") %>% 
      group_by(parentdow.zoop.year, species) %>%
      summarize(count = sum(count), .groups = 'drop')
    #now calculate Shannon diversity
    zoop_SDI_no_aug <- zoop_count_no_aug %>%
      group_by(parentdow.zoop.year) %>%
      summarize(SDI.no.aug = diversity(count, index = "shannon"), .groups = 'drop')

#calculate SDI without September

    #first need counts of each species
    zoop_count_no_sept <- zoop_clad_6month %>%
      filter(month != "09") %>% 
      group_by(parentdow.zoop.year, species) %>%
      summarize(count = sum(count), .groups = 'drop')
    #now calculate Shannon diversity
    zoop_SDI_no_sept <- zoop_count_no_sept %>%
      group_by(parentdow.zoop.year) %>%
      summarize(SDI.no.sept = diversity(count, index = "shannon"), .groups = 'drop')

#calculate SDI without October

    #first need counts of each species
    zoop_count_no_oct <- zoop_clad_6month %>%
      filter(month != "10") %>% 
      group_by(parentdow.zoop.year, species) %>%
      summarize(count = sum(count), .groups = 'drop')
    #now calculate Shannon diversity
    zoop_SDI_no_oct <- zoop_count_no_oct %>%
      group_by(parentdow.zoop.year) %>%
      summarize(SDI.no.oct = diversity(count, index = "shannon"), .groups = 'drop')

#combine these all into one dataframe
SDI_month_elim <- data.frame(parentdow.zoop.year = zoop_SDI_all$parentdow.zoop.year, 
                              SDI.6.month = zoop_SDI_all$SDI.6.month, 
                              Drop.May = zoop_SDI_no_may$SDI.no.may,
                              Drop.June = zoop_SDI_no_june$SDI.no.june,
                              Drop.July = zoop_SDI_no_july$SDI.no.july,
                              Drop.August = zoop_SDI_no_aug$SDI.no.aug,
                              Drop.September = zoop_SDI_no_sept$SDI.no.sept,
                              Drop.October = zoop_SDI_no_oct$SDI.no.oct
)

#now plot these

#convert to long format
SDI_month_elim_long <- pivot_longer(data = SDI_month_elim, cols = 3:8, names_to = "Month", values_to = "SDI")
#set month as a factor with the right order for legend
SDI_month_elim_long$Month <- factor(SDI_month_elim_long$Month, levels = c("Drop.May", "Drop.June", "Drop.July", "Drop.August", "Drop.September", "Drop.October"))


#plot together with months as colors
#tiff("SDI_Month_Drop_color.tiff", width = 8, height = 6, units = "in", res = 300) 
SDI.drop.month <- ggplot(data = SDI_month_elim_long, aes(y = SDI.6.month, x = SDI, color = Month))+
  geom_abline(slope = 1, intercept = 0, color = "red") +
  geom_point(size = 1) + 
  scale_y_continuous(limits = c(0, 2.1)) +
  scale_x_continuous(limits = c(0, 2.1)) +
  labs(y = "Complete Shannon Diversity Index", x = "Drop Month", title = "Cladoceran Shannon Diversity Index")+
  theme_classic()
SDI.drop.month
#dev.off()


#plot individual months
SDI.drop.may <- ggplot(data = SDI_month_elim, aes(y = SDI.6.month))+
  geom_abline(slope = 1, intercept = 0, color = "red") +
  geom_point(aes(x = Drop.May), color = "black") + 
  scale_y_continuous(limits = c(0, 2.1)) +
  scale_x_continuous(limits = c(0, 2.1)) +
  labs(y = "Complete Shannon Diversity Index", x = "Drop May Shannon Diversity", title = "Cladoceran Shannon Diversity Index: Drop May")+
  theme_classic()
SDI.drop.may

SDI.drop.june <- ggplot(data = SDI_month_elim, aes(y = SDI.6.month))+
  geom_abline(slope = 1, intercept = 0, color = "red") +
  geom_point(aes(x = Drop.June), color = "black") + 
  scale_y_continuous(limits = c(0, 2.1)) +
  scale_x_continuous(limits = c(0, 2.1)) +
  labs(y = "Complete Shannon Diversity Index", x = "Drop June Shannon Diversity", title = "Cladoceran Shannon Diversity Index: Drop June")+
  theme_classic()
SDI.drop.june

SDI.drop.july <- ggplot(data = SDI_month_elim, aes(y = SDI.6.month))+
  geom_abline(slope = 1, intercept = 0, color = "red") +
  geom_point(aes(x = Drop.July), color = "black") + 
  scale_y_continuous(limits = c(0, 2.1)) +
  scale_x_continuous(limits = c(0, 2.1)) +
  labs(y = "Complete Shannon Diversity Index", x = "Drop July Shannon Diversity", title = "Cladoceran Shannon Diversity Index: Drop July")+
  theme_classic()
SDI.drop.july

SDI.drop.aug <- ggplot(data = SDI_month_elim, aes(y = SDI.6.month))+
  geom_abline(slope = 1, intercept = 0, color = "red") +
  geom_point(aes(x = Drop.August), color = "black") + 
  scale_y_continuous(limits = c(0, 2.1)) +
  scale_x_continuous(limits = c(0, 2.1)) +
  labs(y = "Complete Shannon Diversity Index", x = "Drop August Shannon Diversity", title = "Cladoceran Shannon Diversity Index: Drop August")+
  theme_classic()
SDI.drop.aug

SDI.drop.sept <- ggplot(data = SDI_month_elim, aes(y = SDI.6.month))+
  geom_abline(slope = 1, intercept = 0, color = "red") +
  geom_point(aes(x = Drop.September), color = "black") + 
  scale_y_continuous(limits = c(0, 2.1)) +
  scale_x_continuous(limits = c(0, 2.1)) +
  labs(y = "Complete Shannon Diversity Index", x = "Drop September Shannon Diversity", title = "Cladoceran Shannon Diversity Index: Drop September")+
  theme_classic()
SDI.drop.sept

SDI.drop.oct <- ggplot(data = SDI_month_elim, aes(y = SDI.6.month))+
  geom_abline(slope = 1, intercept = 0, color = "red") +
  geom_point(aes(x = Drop.October), color = "black") + 
  scale_y_continuous(limits = c(0, 2.1)) +
  scale_x_continuous(limits = c(0, 2.1)) +
  labs(y = "Complete Shannon Diversity Index", x = "Drop October Shannon Diversity", title = "Cladoceran Shannon Diversity Index: Drop October")+
  theme_classic()
SDI.drop.oct

#make a layout of all 6 plots together
#tiff("SDI_Month_Drop_layout.tiff", width = 10, height = 12.5, units = "in", res = 300) 
SDI.drop.month.plot.6 <- grid.arrange(SDI.drop.may, SDI.drop.june, SDI.drop.july, SDI.drop.aug, SDI.drop.sept, SDI.drop.oct)
SDI.drop.month.plot.6
#dev.off()



#LOOK AT SDI WITHIN EACH MONTH - JUST BECAUSE I AM CURIOUS:
#first need counts of each species
zoop_count_month <- zoop_clad_6month %>%
  group_by(parentdow.zoop.year, species, month) %>%
  summarize(count = sum(count), .groups = 'drop')
#now calculate Shannon diversity
zoop_SDI_month <- zoop_count_month %>%
  group_by(parentdow.zoop.year, month) %>%
  summarize(zoop.Shannon.DI = diversity(count, index = "shannon"), .groups = 'drop')
#NOTE: shannon diversity of 0 means only one species present

#make a boxplot of this
#tiff("SDI_Month_Boxplot.tiff", width = 8, height = 6, units = "in", res = 300) 
SDI.by.month <- ggplot(data = zoop_SDI_month, aes(x = month, y = zoop.Shannon.DI))+
  geom_boxplot() +
  labs(y = "Shannon Diversity Index", x = "Month", title = "Cladoceran Shannon Diversity Index within each Month")+
  theme_classic()
SDI.by.month
#dev.off()







#---------------------------------------------------------------------------------------------------------------------------------------------------
#HOW DOES SAMPLING MONTH AFFECT ZOOPLANKTON TOTAL BIOMASS?
#use just all zoop data for this
#biomass data here is in ug/L so it is already scaled for sampling effort within each tow
#BUT we don't have the same numnber of tows for each lake-year or even within each month... so need to standardize that

#calculate sum biomass for all zoops for each lake-year divided by the number of tows with all 6 months present
zoop_biom_all <- zoop_all_6month %>%
  group_by(parentdow.zoop.year) %>%
  summarize(biom.6.month = sum(biomass)/n_distinct(sample_id), .groups = 'drop')

#calculate biomass without May
zoop_biom_no_may <- zoop_all_6month %>%
  filter(month != "05") %>% 
  group_by(parentdow.zoop.year) %>%
  summarize(biom.no.may = sum(biomass)/n_distinct(sample_id), .groups = 'drop')

#calculate biomass without June
zoop_biom_no_june <- zoop_all_6month %>%
  filter(month != "06") %>% 
  group_by(parentdow.zoop.year) %>%
  summarize(biom.no.june = sum(biomass)/n_distinct(sample_id), .groups = 'drop')

#calculate biomass without July
zoop_biom_no_july <- zoop_all_6month %>%
  filter(month != "07") %>% 
  group_by(parentdow.zoop.year) %>%
  summarize(biom.no.july = sum(biomass)/n_distinct(sample_id), .groups = 'drop')

#calculate biomass without August
zoop_biom_no_aug <- zoop_all_6month %>%
  filter(month != "08") %>% 
  group_by(parentdow.zoop.year) %>%
  summarize(biom.no.aug = sum(biomass)/n_distinct(sample_id), .groups = 'drop')

#calculate biomass without September
zoop_biom_no_sept <- zoop_all_6month %>%
  filter(month != "09") %>% 
  group_by(parentdow.zoop.year) %>%
  summarize(biom.no.sept = sum(biomass)/n_distinct(sample_id), .groups = 'drop')

#calculate biomass without October
zoop_biom_no_oct <- zoop_all_6month %>%
  filter(month != "10") %>% 
  group_by(parentdow.zoop.year) %>%
  summarize(biom.no.oct = sum(biomass)/n_distinct(sample_id), .groups = 'drop')

#combine these all into one dataframe
biom_month_elim <- data.frame(parentdow.zoop.year = zoop_biom_all$parentdow.zoop.year, 
                              biom.6.month = zoop_biom_all$biom.6.month, 
                              Drop.May = zoop_biom_no_may$biom.no.may,
                              Drop.June = zoop_biom_no_june$biom.no.june,
                              Drop.July = zoop_biom_no_july$biom.no.july,
                              Drop.August = zoop_biom_no_aug$biom.no.aug,
                              Drop.September = zoop_biom_no_sept$biom.no.sept,
                              Drop.October = zoop_biom_no_oct$biom.no.oct
                            )



#now plot these

#convert to long format
biom_month_elim_long <- pivot_longer(data = biom_month_elim, cols = 3:8, names_to = "Month", values_to = "biomass")
#set month as a factor with the right order for legend
biom_month_elim_long$Month <- factor(biom_month_elim_long$Month, levels = c("Drop.May", "Drop.June", "Drop.July", "Drop.August", "Drop.September", "Drop.October"))

#plot together with months as colors
#tiff("Biomass_Month_Drop_color.tiff", width = 8, height = 6, units = "in", res = 300) 
biom.drop.month <- ggplot(data = biom_month_elim_long, aes(y = biom.6.month, x = biomass, color = Month))+
  geom_abline(slope = 1, intercept = 0, color = "red") +
  geom_point(size = 1) + 
  # scale_y_continuous(limits = c(0, 2.1)) +
  # scale_x_continuous(limits = c(0, 2.1)) +
  labs(y = "Mean Biomass per Tow (µg/L)", x = "Drop Month", title = "Mean Zooplankton Biomass per Tow")+
  theme_classic()
biom.drop.month
#dev.off()


#zoom in on crowded lower corner
#tiff("Biomass_Month_Drop_color_zoom.tiff", width = 8, height = 6, units = "in", res = 300) 
biom.drop.month.zoom <- ggplot(data = biom_month_elim_long, aes(y = biom.6.month, x = biomass, color = Month))+
  geom_abline(slope = 1, intercept = 0, color = "red") +
  geom_point(size = 1) + 
  scale_y_continuous(limits = c(0, 500)) +
  scale_x_continuous(limits = c(0, 500)) +
  labs(y = "Mean Biomass per Tow (µg/L)", x = "Drop Month", title = "Mean Zooplankton Biomass per Tow")+
  theme_classic()
biom.drop.month.zoom
#dev.off()


#plot individual months
biom.drop.may <- ggplot(data = biom_month_elim, aes(y = biom.6.month))+
  geom_abline(slope = 1, intercept = 0, color = "red") +
  geom_point(aes(x = Drop.May), color = "black") + 
  scale_y_continuous(limits = c(0, 1000)) +
  scale_x_continuous(limits = c(0, 1000)) +
  labs(y = "Mean Biomass per Tow (µg/L)", x = "Drop May Mean Biomass per Tow (µg/L)", title = "Mean Zooplankton Biomass per Tow: Drop May")+
  theme_classic()
biom.drop.may

biom.drop.june <- ggplot(data = biom_month_elim, aes(y = biom.6.month))+
  geom_abline(slope = 1, intercept = 0, color = "red") +
  geom_point(aes(x = Drop.June), color = "black") + 
  scale_y_continuous(limits = c(0, 1000)) +
  scale_x_continuous(limits = c(0, 1000)) +
  labs(y = "Mean Biomass per Tow (µg/L)", x = "Drop June Mean Biomass per Tow (µg/L)",  title = "Mean Zooplankton Biomass per Tow: Drop June")+
  theme_classic()
biom.drop.june

biom.drop.july <- ggplot(data = biom_month_elim, aes(y = biom.6.month))+
  geom_abline(slope = 1, intercept = 0, color = "red") +
  geom_point(aes(x = Drop.July), color = "black") + 
  scale_y_continuous(limits = c(0, 1000)) +
  scale_x_continuous(limits = c(0, 1000)) +
  labs(y = "Mean Biomass per Tow (µg/L)", x = "Drop July Mean Biomass per Tow (µg/L)", title = "Mean Zooplankton Biomass per Tow: Drop July")+
  theme_classic()
biom.drop.july

biom.drop.aug <- ggplot(data = biom_month_elim, aes(y = biom.6.month))+
  geom_abline(slope = 1, intercept = 0, color = "red") +
  geom_point(aes(x = Drop.August), color = "black") + 
  scale_y_continuous(limits = c(0, 1000)) +
  scale_x_continuous(limits = c(0, 1000)) +
  labs(y = "Mean Biomass per Tow (µg/L)", x = "Drop August Mean Biomass per Tow (µg/L)", title = "Mean Zooplankton Biomass per Tow: Drop August")+
  theme_classic()
biom.drop.aug

biom.drop.sept <- ggplot(data = biom_month_elim, aes(y = biom.6.month))+
  geom_abline(slope = 1, intercept = 0, color = "red") +
  geom_point(aes(x = Drop.September), color = "black") + 
  scale_y_continuous(limits = c(0, 1000)) +
  scale_x_continuous(limits = c(0, 1000)) +
  labs(y = "Mean Biomass per Tow (µg/L)", x = "Drop September Mean Biomass per Tow (µg/L)", title = "Mean Zooplankton Biomass per Tow: Drop September")+
  theme_classic()
biom.drop.sept

biom.drop.oct <- ggplot(data = biom_month_elim, aes(y = biom.6.month))+
  geom_abline(slope = 1, intercept = 0, color = "red") +
  geom_point(aes(x = Drop.October), color = "black") + 
  scale_y_continuous(limits = c(0, 1000)) +
  scale_x_continuous(limits = c(0, 1000)) +
  labs(y = "Mean Biomass per Tow (µg/L)", x = "Drop October Mean Biomass per Tow (µg/L)", title = "Mean Zooplankton Biomass per Tow: Drop October")+
  theme_classic()
biom.drop.oct

#make a layout of all 6 plots together
#tiff("Biom_Month_Drop_layout.tiff", width = 10.2, height = 12.5, units = "in", res = 300) 
biom.drop.month.plot.6 <- grid.arrange(biom.drop.may, biom.drop.june, biom.drop.july, biom.drop.aug, biom.drop.sept, biom.drop.oct)
biom.drop.month.plot.6
#dev.off()

#LOOK AT BIOMASS/TOW WITHIN EACH MONTH - JUST BECAUSE I AM CURIOUS:
#first need counts of each species
zoop_biom_all_month <- zoop_all_6month %>%
  group_by(parentdow.zoop.year, month) %>%
  summarize(biomass = sum(biomass)/n_distinct(sample_id), .groups = 'drop')

#make a boxplot of this
#tiff("Biomass_Month_Boxplot.tiff", width = 8, height = 6, units = "in", res = 300) 
biom.by.month <- ggplot(data = zoop_biom_all_month, aes(x = month, y = biomass))+
  geom_boxplot() +
  labs(y = "Mean Biomass per Tow (µg/L)", x = "Month", title = "Zooplankton Biomass within each Month")+
  theme_classic()
biom.by.month
#dev.off()

#now restrict y axis so you can actually see the smaller values
#tiff("Biomass_Month_Boxplot_Zoom.tiff", width = 8, height = 12, units = "in", res = 300) 
biom.by.month.zoom <- ggplot(data = zoop_biom_all_month, aes(x = month, y = biomass))+
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 1000)) +
  labs(y = "Mean Biomass per Tow (µg/L)", x = "Month", title = "Zooplankton Biomass within each Month")+
  theme_classic() +
  theme(legend.position = "none")
biom.by.month.zoom
#dev.off()




#----------------------------------------------------------------------------------------------------------------------------------------------------
#PLOT EACH MONTH'S SPECIES RICHNESS, SHANNON DIVERSITY, and BIOMASS TOGETHER

#create a combined dataframe
#need to make a pdow.zoop.year.month column
zoop_biom_all_month$pdow.zoop.year.month <- paste(zoop_biom_all_month$parentdow.zoop.year, zoop_biom_all_month$month)
zoop_SDI_month$pdow.zoop.year.month <- paste(zoop_SDI_month$parentdow.zoop.year, zoop_SDI_month$month)
zoop_rich_month$pdow.zoop.year.month <- paste(zoop_rich_month$parentdow.zoop.year, zoop_rich_month$month)

month_data <- left_join(zoop_biom_all_month, zoop_SDI_month, by = "pdow.zoop.year.month")
month_data2 <- left_join(month_data, zoop_rich_month, by = "pdow.zoop.year.month")
#this data frame is messy but that's fine for now, it has everything I need and won't be used for anything else

#make NA values = 0 (these exist because some months don't have cladocerans at all)
month_data2$biomass[is.na(month_data2$biomass)] <- 0
month_data2$spp.richness[is.na(month_data2$spp.richness)] <- 0
month_data2$zoop.Shannon.DI[is.na(month_data2$zoop.Shannon.DI)] <- 0

#center and scale them all
month_data2$biomass.scale <- scale(month_data2$biomass)
month_data2$richness.scale <- scale(month_data2$spp.richness)
month_data2$sdi.scale <- scale(month_data2$zoop.Shannon.DI)

#convert to long format
month_data_long <- pivot_longer(data = month_data2, cols = 12:14, names_to = "Data", values_to = "standardized.value")

#rename data types
month_data_long$Data <- ifelse(month_data_long$Data == "biomass.scale", "Biomass per Tow (µg/L)", 
                                             ifelse(month_data_long$Data == "richness.scale", "Cladoceran Species Richness",
                                                    ifelse(month_data_long$Data == "sdi.scale", "Cladoceran Shannon Diversity Index", month_data_long$Data)))


#tiff("Month_Zoop_Boxplot.tiff", width = 11, height = 8, units = "in", res = 300) 
month.boxplot <- ggplot(data = month_data_long, aes(x = month.x, y = standardized.value, color = Data))+
  geom_boxplot() +
  scale_y_continuous(limits = c(-2.5, 5)) +
  labs(y = "Standardized Value", x = "Month", title = "Zooplankton Data within each Month")+
  theme_classic()
month.boxplot
#dev.off()

