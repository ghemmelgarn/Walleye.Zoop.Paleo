#This script calculates metrics with the measurements of the sediments zoops to use as potential inputs to the transfer function


#packages
library(tidyverse)
library(ggplot2)


#Read in data----
mes_data <- read.csv("Q:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Walleye paleolimnology and zooplankton/Data/Processed Data/Sediment Zoop Processed Data/All_Measured_Remains2026-11-18.csv")


#QAQC----


#make simpler names for the measurement columns and isolate columns you need for this
mes_data_rename <- mes_data %>% 
  rename(car_len_um = Carapace.Length.µm,
         muc_len_um = Mucro.Length.µm,
         muc_seg = Mucro.Segments,
         ant_len_um = Antennule.Length.µm,
         ant_seg = Antennule.Segments,
         claw_len_um = Postabdominal.Claw.Length) %>% 
  select(LakeName, IDed.by, Remain.Number, Remain.Type, Taxa, car_len_um, muc_len_um, muc_seg, ant_len_um, ant_seg, claw_len_um, Notes) %>% 
  #calculate the carapace divided by the mucro - useful to filter out mucros that are longer than carapace which is impossible
  mutate(carap.div.mucro = car_len_um / muc_len_um)



#pivot longer
mes_data_long <- mes_data_rename %>% 
  pivot_longer(cols = c(car_len_um, muc_len_um, muc_seg, ant_len_um, ant_seg, claw_len_um, carap.div.mucro), names_to = "Structure", values_to = "Measurement") %>% 
  filter(!is.na(Measurement))


#histograms of each structure to look for outliers
ggplot(data = mes_data_long, aes(x = Measurement, fill = IDed.by))+
  facet_wrap(vars(Structure), scales = "free")+
  geom_histogram()+
  theme_classic()

#histograms of each structure by lake
hist_by_lake <- list()

for (i in unique(mes_data_long$Structure)){
  plot_data <- mes_data_long %>% 
    filter(Structure == i)
  
  hist_by_lake[[i]] <- ggplot(data = plot_data, aes(x = Measurement, fill = IDed.by))+
    facet_wrap(vars(LakeName), scales = "free")+
    geom_histogram()+
    theme_classic()+
    labs(title = i)
}

#problems:
#definitely need to fix
  #LOWMuskty_1_42 Antennule segment = 137, ant length = 14 - need to switch these
  #Tenmile_1_176 Antennule segment = 67, no ant length - move 67 to ant.length 
  #Josephine_1_231 both mucro length and segment count listed as 33 - change segment count to NA
  #Silver_1_38 carap length = 43 and mucro length = 319 - need to switch there
  #silver_1_65 the mucro is so much shorter than the carapace and there's a note saying not sure if its actually a mucro - delete this one

#maybe fix depending on outlier analysis
  #There are 6 Leech mucros that are VERY long, some as long or longer than carapace length... lets delete these because they look like outliers and all happened around the same remain number - Adele probably forgot to select correct objective in microscope software
  #lets say the carapace has to be at least 1.5x times longer than the carapace to be considered actual data - there is a breakpoint in the data here, and mucro definitely can't be longer than carapace

#checked and okay
  #RedL_1_069 is very large carapace but its real, Erin left a note
  #other big carapaces also look okay, they are species that are known to be large
  #very small carapaces are also okay, they are all small species
  #claw lengths all look okay

#Make changes that need to happen for sure:
mes_data_rename_rev1 <- mes_data_rename %>% 
  mutate(ant_len_um = case_when(
    Remain.Number == "LOWMuskey_1_42" ~ 137,
    Remain.Number == "Tenmile_1_176" ~ 67,
    TRUE ~ ant_len_um),
    
    ant_seg = case_when(
      Remain.Number == "LOWMuskey_1_42" ~ 14,
      Remain.Number == "Tenmile_1_176" ~ NA,
      TRUE ~ ant_seg),
    
    muc_seg = case_when(
      Remain.Number == "Josephine_1_231" ~ NA,
      Remain.Number == "silver_1_65" ~ NA,
      TRUE ~ muc_seg),
    
    muc_len_um = case_when(
      Remain.Number == "Silver_1_38" ~ 43,
      Remain.Number == "silver_1_65" ~ NA,
      TRUE ~ muc_len_um),
      
    car_len_um = case_when(
      Remain.Number == "Silver_1_38" ~ 319,
      TRUE ~ car_len_um),
  )


#pivot longer with the corrections in it
mes_data_long_rev1 <- mes_data_rename_rev1 %>% 
  pivot_longer(cols = c(car_len_um, muc_len_um, muc_seg, ant_len_um, ant_seg, claw_len_um, carap.div.mucro), names_to = "Structure", values_to = "Measurement") %>% 
  filter(!is.na(Measurement))


#histograms of each structure to look for outliers
ggplot(data = mes_data_long_rev1, aes(x = Measurement, fill = IDed.by))+
  facet_wrap(vars(Structure), scales = "free")+
  geom_histogram()+
  theme_classic()
#looking better


#if we pull out outliers with the interquartile range method from each structure distribution
#remove points that are 1.5IQR below Q1 or 1.5IQR above Q3

#calculate interquartile ranges by structure
n <- length(unique(mes_data_long_rev1$Structure))

IQR_by_structure <- data.frame(
  Structure = character(n),
  Q1.general = numeric(n),
  Q3.general = numeric(n),
  IQR.general = numeric(n),
  lower_limit.general = numeric(n),
  upper_limit.general = numeric(n)
)

j <- 1

for (i in unique(mes_data_long_rev1$Structure)){
  #isolate structure
  IQR_data <- mes_data_long %>% 
    filter(Structure == i)
  
  #calculations
  Q1 <- quantile(IQR_data$Measurement, probs = 0.25, na.rm = TRUE)
  Q3 <- quantile(IQR_data$Measurement, probs = 0.75, na.rm = TRUE)
  IQR <- Q3-Q1
  lower <- Q1 - (1.5*IQR)
  upper <- Q3 + (1.5*IQR)
  
  #fill in data frame
    IQR_by_structure$Structure[j] <- i
    IQR_by_structure$Q1.general[j] <- Q1
    IQR_by_structure$Q3.general[j] <- Q3
    IQR_by_structure$IQR.general[j] <- IQR
    IQR_by_structure$lower_limit.general[j] <- lower
    IQR_by_structure$upper_limit.general[j] <- upper
  
  j <- j+1

}

#join these calculations to the measurement data and create a categorical metric for dropping or not
mes_data_long_rev1_IQR <- left_join(mes_data_long_rev1, IQR_by_structure, by = "Structure") %>% 
  mutate(drop.general = ifelse(Measurement < upper_limit.general & Measurement > lower_limit.general, "no", "yes"))

#plot with vertical lines and colors for dropping
ggplot(data = mes_data_long_rev1_IQR, aes(x = Measurement, fill = drop.general))+
  facet_wrap(vars(Structure), scales = "free")+
  geom_histogram()+
  geom_vline(aes(xintercept = Q1.general), color = "blue")+
  geom_vline(aes(xintercept = Q3.general), color = "blue")+
  geom_vline(aes(xintercept = upper_limit.general), color = "red")+
  geom_vline(aes(xintercept = lower_limit.general), color = "red")+
  theme_classic()

#histograms of each structure by lake
hist_by_lake2 <- list()

for (i in unique(mes_data_long_rev1_IQR$Structure)){
  plot_data <- mes_data_long_rev1_IQR %>% 
    filter(Structure == i)
  
  hist_by_lake2[[i]] <- ggplot(data = plot_data, aes(x = Measurement, fill = drop.general))+
    facet_wrap(vars(LakeName), scales = "free")+
    geom_histogram()+
    theme_classic()+
    geom_vline(aes(xintercept = Q1.general), color = "blue")+
    geom_vline(aes(xintercept = Q3.general), color = "blue")+
    geom_vline(aes(xintercept = upper_limit.general), color = "red")+
    geom_vline(aes(xintercept = lower_limit.general), color = "red")+
    labs(title = i)
}


#All these really need to be done by lake
#quartiles are not representative of all lakes... so telling me to drop things that are outliers on the whole but just on edge of distribution for that lake

#calculate interquartile ranges by structure by lake
n <- length(unique(mes_data_long_rev1$Structure)) * length(unique(mes_data_long_rev1$LakeName))

IQR_by_structure_lake <- data.frame(
  LakeName = character(n),
  Structure = character(n),
  Q1.lake = numeric(n),
  Q3.lake = numeric(n),
  IQR.lake = numeric(n),
  lower_limit.lake = numeric(n),
  upper_limit.lake = numeric(n)
)

j <- 1

for (k in unique(mes_data_long_rev1$LakeName)){

  IQR_lake <- mes_data_long_rev1 %>%
    filter(LakeName == k)

  for (i in unique(IQR_lake$Structure)){
    #isolate structure
    IQR_data <- IQR_lake %>%
      filter(Structure == i)

    #calculations
    Q1 <- quantile(IQR_data$Measurement, probs = 0.25, na.rm = TRUE)
    Q3 <- quantile(IQR_data$Measurement, probs = 0.75, na.rm = TRUE)
    IQR <- Q3-Q1
    lower <- Q1 - (1.5*IQR)
    upper <- Q3 + (1.5*IQR)

    #fill in data frame
    IQR_by_structure_lake$LakeName[j] <- k
    IQR_by_structure_lake$Structure[j] <- i
    IQR_by_structure_lake$Q1.lake[j] <- Q1
    IQR_by_structure_lake$Q3.lake[j] <- Q3
    IQR_by_structure_lake$IQR.lake[j] <- IQR
    IQR_by_structure_lake$lower_limit.lake[j] <- lower
    IQR_by_structure_lake$upper_limit.lake[j] <- upper

    j <- j+1

  }
}


#join these calculations to the measurement data and create a categorical metric for dropping or not
mes_data_long_rev1_IQR_lake <- left_join(mes_data_long_rev1, IQR_by_structure_lake, by = c("LakeName", "Structure")) %>%
  mutate(drop.lake = ifelse(Measurement < upper_limit.lake & Measurement > lower_limit.lake, "no", "yes"))

#plot with vertical lines and colors for dropping - this is uninterpretable with all the different quartiles
ggplot(data = mes_data_long_rev1_IQR_lake, aes(x = Measurement, fill = drop.lake))+
  facet_wrap(vars(Structure), scales = "free")+
  geom_histogram()+
  geom_vline(aes(xintercept = Q1.lake), color = "blue")+
  geom_vline(aes(xintercept = Q3.lake), color = "blue")+
  geom_vline(aes(xintercept = upper_limit.lake), color = "red")+
  geom_vline(aes(xintercept = lower_limit.lake), color = "red")+
  theme_classic()

#histograms of each structure by lake
hist_by_lake3 <- list()

for (i in unique(mes_data_long_rev1_IQR_lake$Structure)){
  plot_data <- mes_data_long_rev1_IQR_lake %>%
    filter(Structure == i)

  hist_by_lake3[[i]] <- ggplot(data = plot_data, aes(x = Measurement, fill = drop.lake))+
    facet_wrap(vars(LakeName), scales = "free")+
    geom_histogram()+
    theme_classic()+
    geom_vline(aes(xintercept = Q1.lake), color = "blue")+
    geom_vline(aes(xintercept = Q3.lake), color = "blue")+
    geom_vline(aes(xintercept = upper_limit.lake), color = "red")+
    geom_vline(aes(xintercept = lower_limit.lake), color = "red")+
    labs(title = i)
}


#okay actually now doing this I feel like it's too restrictive
#telling me to drop too much, maybe including species would make it make more sense but doesn't feel necessary and hard to deal with rare species because not enough data
#I know from counting there are really big and really small remains in the samples, and I'm targeting measurement error, not the true biological outliers
#Therefore, I am looking to filter out the biological impossibilities, so going back to the aggregated cutoffs
#I will hand-evaluate each one to make sure it's not a large species or a skewed but plausible distribution for a specific lake that should actually be kept, also checking notes

#HERE'S AN IDEA:
  #to be considered an outlier, it has to be outside the 1.5IQR limits for the data across all lakes AND outside the lake-specific 1.5IQR
  #this would let a skewed or generally large/small lake-specific distribution "save" points that are not odd for that specific lake

#combine the info on both ranges together
mes_data_long_rev1_IQR_all <- left_join(mes_data_long_rev1_IQR, IQR_by_structure_lake, by = c("LakeName", "Structure"))%>%
  mutate(drop.lake = ifelse(Measurement < upper_limit.lake & Measurement > lower_limit.lake, "no", "yes")) %>% 
  mutate(drop.both = case_when(
    drop.lake == "yes" & drop.general == "yes" ~ "yes",
    drop.lake == "yes" & drop.general == "no" ~ "lake only",
    drop.lake == "no" & drop.general == "yes" ~ "gen only",
    drop.lake == "no" & drop.general == "no" ~ "no")
  )
    
#plot this and color by the drop type
ggplot(data = mes_data_long_rev1_IQR_all, aes(x = Measurement, fill = drop.both))+
  facet_wrap(vars(Structure), scales = "free")+
  geom_histogram()+
  geom_vline(aes(xintercept = Q1.general), color = "blue")+
  geom_vline(aes(xintercept = Q3.general), color = "blue")+
  geom_vline(aes(xintercept = upper_limit.general), color = "red")+
  geom_vline(aes(xintercept = lower_limit.general), color = "red")+
  theme_classic()

#histograms of each structure by lake
hist_by_lake4 <- list()

for (i in unique(mes_data_long_rev1_IQR_all$Structure)){
  plot_data <- mes_data_long_rev1_IQR_all %>% 
    filter(Structure == i)
  
  hist_by_lake4[[i]] <- ggplot(data = plot_data, aes(x = Measurement, fill = Taxa))+
    facet_wrap(vars(LakeName), scales = "free")+
    geom_histogram()+
    theme_classic()+
    geom_vline(aes(xintercept = Q1.general), color = "blue")+
    geom_vline(aes(xintercept = Q3.general), color = "blue")+
    geom_vline(aes(xintercept = upper_limit.general), color = "red")+
    geom_vline(aes(xintercept = lower_limit.general), color = "red")+
    geom_vline(aes(xintercept = Q1.lake), color = "purple")+
    geom_vline(aes(xintercept = Q3.lake), color = "purple")+
    geom_vline(aes(xintercept = upper_limit.lake), color = "orange")+
    geom_vline(aes(xintercept = lower_limit.lake), color = "orange")+
    labs(title = i)
}


#great, now go through the ones it wants to drop and make sure it isn't just a large, rare species and check for any notes


#a filter script to look at carapaces easier:
carap_test <- mes_data_long_rev1_IQR_all %>% 
  filter(drop.both == "yes" & Structure == "car_len_um" & (Taxa == "Chydorus brevilabris" | Taxa == "Eubosmina coregoni" | Taxa == "Bosmina longirostris"))

#manually reassign where needed:
mes_data_long_rev1_IQR_all$drop.final <- case_when(
  mes_data_long_rev1_IQR_all$Structure == "ant_len_um" ~ "no",  #don't drop any of the super long antennules - I know there are times when there were a few ants much longer than the others and none of them are outside the realm of possibility, just a skewed distribution
  mes_data_long_rev1_IQR_all$Remain.Number == "Belle_1_266" ~ "no", #just a skewed but continuous distribution at lake level
  mes_data_long_rev1_IQR_all$Remain.Number == "LOWMuskey_1_44" ~ "no", #lots of big mucros in this lake and this one is not that far from 2nd biggest
  mes_data_long_rev1_IQR_all$Structure == "car_len_um" & (mes_data_long_rev1_IQR_all$Taxa != "Chydorus brevilabris"  & mes_data_long_rev1_IQR_all$Taxa != "Bosmina longirostris") ~ "no", #big carapaces are okay as long as they are not the two common small taxa - letting EUbosmina through because there are enough of them in the 400-500 range and I know they can get bigger so those are believable just rare
  mes_data_long_rev1_IQR_all$Structure == "carap.div.mucro" ~ "no", #not using the carap to mucro ratio to decide anything since the carapaces and mucros seem mostly in reasonable ranges
  TRUE ~ mes_data_long_rev1_IQR_all$drop.both)

#drop the measurements where needed, get rid of all the extra columns
mes_data_long_clean <- mes_data_long_rev1_IQR_all %>% 
  filter(drop.final != "yes") %>% 
  filter(Structure != "carap.div.mucro") %>% 
  select(LakeName, IDed.by, Remain.Number, Remain.Type, Taxa, Notes, Structure, Measurement)

#pivot wider for where that's useful
mes_data_wide_clean <- mes_data_long_clean %>% 
  pivot_wider(names_from = Structure, values_from = Measurement)

#remove all the datasets getting up to this point and just keep cleaned data
rm(carap_test, IQR_by_structure, IQR_by_structure_lake, IQR_data, IQR_lake, mes_data, mes_data_long, mes_data_long_rev1, mes_data_long_rev1_IQR,
   mes_data_long_rev1_IQR_all, mes_data_long_rev1_IQR_lake, mes_data_rename, mes_data_rename_rev1, i, IQR, j, k, lower, upper, n, Q1, Q3)

#try plotting length vs segments for antennules and mucros
ggplot(data = mes_data_wide_clean, aes(x = muc_len_um, y = muc_seg))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()
#there is a relationship between mucro length and segments but so much variation it would be hard to use/predict
#how correlated are they?
cor(mes_data_wide_clean$muc_len_um, mes_data_wide_clean$muc_seg, use = "complete.obs")
#cor = 0.79 - so they are correlated, might be confounding and should choose one
#what would the R2 be for a linear model (how much variation in segments does the length explain?)
mucro_lm <- lm(muc_seg ~ muc_len_um, data = mes_data_wide_clean)
summary(mucro_lm)
#R2 = 0.62 - so explains quite a bit of variation too

ggplot(data = subset(mes_data_wide_clean, !is.na(ant_len_um) & !is.na(ant_seg)), aes(x = ant_len_um, y = ant_seg, color = Taxa))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()
#definitely see longer antennules have more segments but lots of variation
#eubosmina have longer antennules but only a few more segments - must elongate segments instead of growing a lot more
#outliers definitely pull the bosmina line - I wonder if this is an ID issue for the super long bos ants?
#how correlated are they?
cor(mes_data_wide_clean$ant_len_um, mes_data_wide_clean$ant_seg, use = "complete.obs")
#cor = 0.84 - so they are correlated, might be confounding and should choose one
ant_lm <- lm(ant_seg ~ ant_len_um, data = mes_data_wide_clean)
summary(ant_lm)
#R2 = 0.70 - so explains quite a bit of variation too


#make boxplot of each structure by lake
ggplot(data = mes_data_long_clean, aes(x = LakeName, y = Measurement))+
  geom_boxplot()+
  facet_wrap(vars(Structure), scales = "free")+
  labs(title = "Mean")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#Means + top 20% means by structure----
#do some basic calculations for each measured structure
#top 10% accounts for the fact that they shed while they grow, so looks at how big they can get

mean_strucutre <- mes_data_long_clean %>% 
  group_by(LakeName, Structure) %>% 
  summarize(
    mean = mean(Measurement, na.rm = TRUE),
    mean_n = n(),
    .groups = "drop"
  )

top20_mean_structure <- mes_data_long_clean %>% 
  group_by(LakeName, Structure) %>% 
  slice_max(order_by = Measurement, prop = 0.2) %>%
  summarize(
    top20_mean = mean(Measurement, na.rm = TRUE),
    top20_mean_n = n(),
    .groups = "drop"
  )

lake_structure_means <- full_join(mean_strucutre, top20_mean_structure, by = c("LakeName", "Structure"))

#how much does it vary by lake?
ggplot(data = lake_structure_means, aes(x = LakeName, y = mean))+
  geom_col()+
  geom_text(aes(label = mean_n), vjust = -0.5, size = 3.5)+
  facet_wrap(vars(Structure), scales = "free")+
  labs(title = "Mean")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#really the same info that is in the boxplot above

#top20 will be different
ggplot(data = lake_structure_means, aes(x = LakeName, y = top20_mean))+
  geom_col()+
  geom_text(aes(label = top20_mean_n), vjust = -0.5, size = 3.5)+
  facet_wrap(vars(Structure), scales = "free")+
  labs(title = "Top 20% Mean")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#compare mean to top_20 mean
ggplot(data = lake_structure_means, aes(x = mean, y = top20_mean))+
  geom_point()+
  facet_wrap(vars(Structure), scales = "free")+
  labs(title = "Mean vs. Top 20% Mean with 1:1 reference line")+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red")+ #1:1 line for reference
  theme_classic()



#Means + top 10% means by structure AND taxon ----

mean_strucutre_taxa <- mes_data_long_clean %>% 
  group_by(LakeName, Structure, Taxa) %>% 
  summarize(
    mean = mean(Measurement, na.rm = TRUE),
    mean_n = n(),
    .groups = "drop"
  )

top20_mean_structure_taxa <- mes_data_long_clean %>% 
  group_by(LakeName, Structure, Taxa) %>% 
  slice_max(order_by = Measurement, prop = 0.2) %>%
  summarize(
    top20_mean = mean(Measurement, na.rm = TRUE),
    top20_mean_n = n(),
    .groups = "drop"
  )

lake_structure_taxa_means <- full_join(mean_strucutre_taxa, top20_mean_structure_taxa, by = c("LakeName", "Structure", "Taxa"))

#Plot these
taxa_mean_plots <- list()

#how much does it vary by lake?

#start with raw boxplot
boxplots_taxa <- list()

for (i in unique(mes_data_long_clean$Structure)){
  
  plot_data <- mes_data_long_clean %>% 
    filter(Structure == i)
  
  boxplots_taxa[[i]] <- ggplot(data = plot_data, aes(x = LakeName, y = Measurement))+
    geom_boxplot()+
    facet_wrap(vars(Taxa), scales = "free")+
    labs(title = i)+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}



for (i in unique(lake_structure_taxa_means$Structure)){
  
  plot_data <- lake_structure_taxa_means %>% 
    filter(Structure == i)
  
  taxa_mean_plots[[i]] <-ggplot(data = plot_data, aes(x = LakeName, y = mean))+
    geom_col()+
    geom_text(aes(label = mean_n), vjust = -0.5, size = 3.5)+
    facet_wrap(vars(Taxa))+
    labs(title = paste(i, "Mean"))+
    #theme_classic()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#really the same info that is in the boxplot above

#top20 will be different
ggplot(data = lake_structure_means, aes(x = LakeName, y = top20_mean))+
  geom_col()+
  geom_text(aes(label = top20_mean_n), vjust = -0.5, size = 3.5)+
  facet_wrap(vars(Structure), scales = "free")+
  labs(title = "Top 20% Mean")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#compare mean to top_20 mean
ggplot(data = lake_structure_means, aes(x = mean, y = top20_mean))+
  geom_point()+
  facet_wrap(vars(Structure), scales = "free")+
  labs(title = "Mean vs. Top 20% Mean with 1:1 reference line")+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red")+ #1:1 line for reference
  theme_classic()


#SELECTED MEASUREMENTS---------------------------

#TAKE A STEP BACK - I THINK WE CAN ONLY USE MEASUREMENTS WHERE WE HAVE DATA FOR ALL LAKES:
#what do we have data from all lakes for:
lake_count_by_structure_taxa <- mes_data_long_clean %>% 
  group_by(Structure, Taxa) %>% 
  summarize(lake_count = length(unique(LakeName)),
            .groups = "drop")
#37 total lakes
#data for all lakes for all Bosmina longirostris metrics
#All but Sant Point for chydorus brevilabris
#Focus on these

#isolate these
mes_long_complete <- mes_data_long_clean %>% 
  filter(Taxa == "Bosmina longirostris" | Taxa == "Chydorus brevilabris") %>% 
  mutate(Structure = case_when(
    Taxa == "Bosmina longirostris" & Structure == "car_len_um" ~ "bos_car_len_um",
    Taxa == "Chydorus brevilabris" & Structure == "car_len_um" ~ "chy_car_len_um",
    TRUE ~ Structure
  ))

#make boxplots of these - and save to show to Adam
length_boxplots <- ggplot(data = mes_long_complete, aes(x = LakeName, y = Measurement))+
  geom_boxplot()+
  facet_wrap(vars(Structure), scales = "free")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
length_boxplots

#ggsave(filename = "Structure_Length_Boxplots.png", plot = length_boxplots, width = 12, height = 8, units = "in", dpi = 600)


#summarize the mean and top20 mean for these 6 measurements
select_mean <- mes_long_complete %>% 
  group_by(LakeName, Structure) %>% 
  summarize(
    mean = mean(Measurement, na.rm = TRUE),
    mean_n = n(),
    .groups = "drop"
  )

select_top20_mean <- mes_long_complete %>% 
  group_by(LakeName, Structure) %>% 
  slice_max(order_by = Measurement, prop = 0.2) %>%
  summarize(
    top20_mean = mean(Measurement, na.rm = TRUE),
    top20_mean_n = n(),
    .groups = "drop"
  )

select_structure_means <- full_join(select_mean, select_top20_mean, by = c("LakeName", "Structure"))


#make a plot of how much data we have (Korosi et al. 2013 suggest at least 35 per interval)
n_plots <- ggplot(data = select_structure_means, aes(x = LakeName, y = mean_n))+
  geom_col()+
  facet_wrap(vars(Structure))+
  labs(y = "Sample Size")+
  theme_classic()+
  geom_hline(yintercept = 35, color = "red")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
n_plots

#ggsave(filename = "Structure_Length_Sample_Sizes.png", plot = n_plots, width = 12, height = 8, units = "in", dpi = 600)




#Bosmina mucro manipulations-----------------------------------------

#a little exploration of mucro and carapace length relationships

#pivot the data wider
mes_wide_complete <- mes_long_complete %>% 
  pivot_wider(names_from = Structure, values_from = Measurement)

#first regress mucro length on carapace length
mucro_lm <- lm(muc_len_um ~ bos_car_len_um, data = mes_wide_complete)
summary(mucro_lm)
#R2 = 0.13

#also visualize
ggplot(data = mes_wide_complete, aes(x = bos_car_len_um, y = muc_len_um))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()

#try this again by lake
mucro_lm_lake <- lm(muc_len_um ~ bos_car_len_um + LakeName, data = mes_wide_complete)
summary(mucro_lm_lake)
#R2 = 0.56
#so relationships within a lake are more similar that lumping them all

#also visualize
ggplot(data = mes_wide_complete, aes(x = bos_car_len_um, y = muc_len_um, color = LakeName))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+ #no SE contours so I can see lines
  theme_classic()


##FOLLOWING WHAT ARMSTRONG et al. 2025 did -----

#prepare my data for their code:
mdat <- mes_wide_complete %>% 
  rename(Mucro = muc_len_um,
         Carapace = bos_car_len_um) %>% 
  mutate(LakeName = as.factor(LakeName)) %>% 
  filter(!is.na(Mucro) & !is.na(Carapace))


#use their code (slightly modified - used LakeName instead of Depth because not doing a long core)
#NEED TO TROUBLESHOOT THIS CODE - issue might be because lakename can't be numeric

##Script written by Isaac Armstrong and Abbie Jones## 

## Packages ##

require(ggeffects) #needed for ggpredict to get marginal means
require(readxl)
require(dplyr)

## Load-in Data ##

#Your data here should be a data frame that has columns titled "Mucro", "Depth", and "Carapace"
#with all datapoints collected for all intervals.
#Mucro and carapace data should be paired, i.e., measured on the same individual

## Modelling ##

mod <- lm(log(Mucro)~log(Carapace)+LakeName, data=mdat)
#This model does an ANCOVA (response variable ~ continuous variable + factor variable)
summary(mod) #Keep in mind we aren't necessarily looking to assess significance of terms, but to better visualize them 

#Predicting marginal means for each depth interval
m.predict <- ggpredict(mod, terms=c("LakeName")) #This uses the model above to predict mucro length for each depth with carapace size held constant.
pred.mucro <- m.predict$predicted

#Now, collates the predicted values into a data frame:
mod.pred <- as.data.frame(pred.mucro)
mod.pred$lowCI <- m.predict$conf.low #Append low 95% confidence interval.
mod.pred$highCI <- m.predict$conf.high #Append high 95% confidence interval.
mod.pred$depth <- m.predict$x #Append x-value (Depth).
#Mod.pred is now a data frame with marginal effect mean (modeled mucro lengths) and confidence intervals.


## OPTIONAL: The following code calculates the sample size of each interval ##

loopdat <- data.frame(LakeName=as.numeric(0), SampleSize=as.numeric(0))

for (value in unique(mdat$LakeName)) 
{
  func.data <- mdat[mdat$LakeName == value,]
  f.data.m <- nrow(func.data)
  looprow <- c(value, f.data.m)
  loopdat <- rbind(loopdat, looprow) 
}

sample.size <- as.data.frame(loopdat[-1,])

mod.pred$samplesize <- sample.size$SampleSize

mod.pred$`Sample n` <- if_else(sample.size$SampleSize >= 35, ">= 35 ","< 35") #change accordingly to what your minimum is, so you can show intervals that did not meet minimum counts.

#Save the data.
write.csv(mod.pred,"bosmina_example_modelled_mucros.csv")


## OPTIONAL: ploting  the data ##

require(ggplot2)
require(tidypaleo) #help ggplot2 for stratigraphies

ggplot(mod.pred,aes(x=pred.mucro,y=depth))+
  geom_lineh()+ 
  geom_linerange(aes(xmin=lowCI,xmax=highCI))+
  geom_point(aes(shape=`Sample n`),size=3)+
  scale_shape_manual(values=c(1,16))+
  scale_y_reverse()+
  labs(x="Modelled mucro length (um)",y="Midpoint depth (cm)")+
  theme(panel.grid=element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, linetype = 1),
        panel.background = element_rect(fill = "white"),
        axis.text=element_text(size=12),
        #legend.position="none", #uncomment this line to remove legend
  )


#Now visualize how the data would look the standard way (just taking the means and standard deviations)

#This establishes a function to calculate means and stdev
sort_means <- function(y,d) {
  
  loopdat <- data.frame(LakeName=as.numeric(0),VarMean=as.numeric(0),VarStd=as.numeric(0))
  x <- data.frame(y,d)
  
  for (value in unique(x$d)) 
  {
    fun.data <- x[x$d == value,]
    f.data.m <- mean(fun.data$y)
    f.data.s <- sd(fun.data$y)
    looprow <- c(value,f.data.m,f.data.s)
    loopdat <- rbind(loopdat,looprow)
  }
  loopdat <- loopdat[-1,]
  
  return(loopdat)
}

#Using it to sort the means
m.means <- sort_means(mdat$Mucro,mdat$LakeName)
m.means$`Sample n` <- if_else(sample.size$SampleSize >= 35, ">= 35 ","< 35") 

#Plotting empirical data points 
ggplot(m.means,aes(y=LakeName,x=VarMean))+
  geom_point(aes(shape=`Sample n`),size=3)+
  scale_shape_manual(values=c(1,16))+
  geom_linerange(aes(xmin=VarMean-VarStd,xmax=VarMean+VarStd))+
  geom_lineh()+
  scale_y_reverse()+
  labs(y="Midpoint depth (cm)",x="Mean mucro length (um)")+
  theme(panel.grid=element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, linetype = 1),
        panel.background = element_rect(fill = "white"),
        axis.text=element_text(size=12),
        # legend.position="none", #uncomment this line to remove legend
  )



