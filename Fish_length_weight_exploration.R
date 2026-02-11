#This script investigates which fish in the contemporary dataset we have length/weight data for


library(tidyverse)
library(dplyr)
library(ggplot2)


#read in fish data
fish.data <- read.csv("Data/Input/Pelagic_Fish_Data.csv")

#Now a few data manipulation things to match dataset:
#group the redhorses and the bullheads
fish.data2 <- fish.data %>% 
  mutate(species_1 = ifelse((species_1 == "redhorse" | species_1 == "shorthead_redhorse" | species_1 == "greater_redhorse" | species_1 == "silver_redhorse"), "redhorse", 
                            ifelse(species_1 == "black_bullhead" | species_1 == "brown_bullhead" | species_1 == "brown_bullhead", "bullhead", species_1)))

#split East/West Vermilion and North/South Hill
fish.data3 <- fish.data2 %>% 
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


#check that lengths and weights are all the same units
unique(fish.data3$length_unit_1)
unique(fish.data3$weight_unit_1)
#looks good

#histogram of all fish lengths that we do have
hist(fish.data3$length_1)

#first create a yes/no column if they have length
fish.data4 <- fish.data3 %>% 
  mutate(length_yn = ifelse(is.na(length_1), "no", "yes")) %>% 
  mutate(weight_yn = ifelse(is.na(weight_1), "no", "yes"))



#count of fish with and without lengths and weights
table(fish.data4$length_yn)
table(fish.data4$weight_yn)
#we see that most of the fish actually do have lengths

#counts together
table(fish.data4$length_yn, fish.data4$weight_yn)
#all the fish with weights have a length, but not all the fish with lengths have a weight

#LENGTH x SPP
#lets look at this by species
table(fish.data4$species_1, fish.data4$length_yn)
#make this into a data frame
length.spp <- data.frame(table(fish.data4$species_1, fish.data4$length_yn))
length.spp <- pivot_wider(data = length.spp, names_from = Var2, values_from = Freq)
length.spp <- rename(length.spp, species = Var1)

#calculate proportion of individuals with lengths for each species
length.spp$total <- length.spp$no + length.spp$yes
length.spp$prop <- length.spp$yes / length.spp$total

#plot it

#order species by decreasing proportion to make graph easy to read
length.spp$species <- factor(length.spp$species, levels = length.spp$species[order(length.spp$prop)])
#plot
length.spp.prop <- ggplot(length.spp, aes(x = species, y = prop)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(y = "Proprotion of Individuals with Length Data", x = "Species", title = "Length x Species")+
  geom_text(aes(label = total), hjust = -0.1)+
  geom_text(aes(label = "n", x = 50, y = 1.01), vjust = 0.9, size = 5)+
  geom_hline(yintercept = 0.90, color = "red", linewidth = 1)+ #add a vertical like at 90% data coverage
  theme_classic()
length.spp.prop

#WEIGHT x SPP
#lets look at this by species
table(fish.data4$species_1, fish.data4$weight_yn)
#make this into a data frame
weight.spp <- data.frame(table(fish.data4$species_1, fish.data4$weight_yn))
weight.spp <- pivot_wider(data = weight.spp, names_from = Var2, values_from = Freq)
weight.spp <- rename(weight.spp, species = Var1)

#calculate proportion of individuals with lengths for each species
weight.spp$total <- weight.spp$no + weight.spp$yes
weight.spp$prop <- weight.spp$yes / weight.spp$total

#plot it

#order species by decreasing proportion to make graph easy to read
weight.spp$species <- factor(weight.spp$species, levels = weight.spp$species[order(weight.spp$prop)])
#plot
weight.spp.prop <- ggplot(weight.spp, aes(x = species, y = prop)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(y = "Proprotion of Individuals with Weight Data", x = "Species", title = "Weight x Species")+
  geom_text(aes(label = total), hjust = -0.1)+
  geom_text(aes(label = "n", x = 50, y = 1.01), vjust = 0.9, size = 5)+
  geom_hline(yintercept = 0.90, color = "red", linewidth = 1)+ #add a vertical like at 90% data coverage
  theme_classic()
weight.spp.prop

#LENGTH AND WEIGHT x SPECIES
#combine the data frames
length.weight.spp <- rename(length.spp, length.prop = prop)
length.weight.spp$weight.prop <- weight.spp$prop

#plot them together
#tiff("Length_Weight_Data_by_species.tiff", width = 15, height = 10, units = "in", res = 300)                   
length.weight.spp.prop <- ggplot()+
  geom_bar(data = length.weight.spp, aes(x = species, y = length.prop), stat = "identity", fill = "gray") +
  geom_bar(data = length.weight.spp, aes(x = species, y = weight.prop), stat = "identity", fill = "lightblue")+
  coord_flip() +
  labs(y = "Proprotion of Individuals with Data", x = "Species", title = "Length and Weight x Species")+
  geom_text(data = length.weight.spp, aes(label = total, x = species, y = length.prop), hjust = -0.1)+
  geom_text(aes(label = "n", x = 49, y = 1.01), vjust = 0.9, size = 5)+
  geom_hline(yintercept = 0.90, color = "red", linewidth = 1)+ #add a vertical like at 90% data coverage
  theme_classic()+
  geom_tile(data = data.frame(x = 3.7, y = 0.92), aes(x = x, y = y), 
                            width = 1, height = 0.01, fill = "lightblue") +  # Color box
  annotate("text", x = 3.7, y = 0.97, label = "Weight Data", size = 4)+
  geom_tile(data = data.frame(x = 2, y = 0.92), aes(x = x, y = y), 
            width = 1, height = 0.01, fill = "gray") +  # Color box
  annotate("text", x = 2, y = 0.97, label = "Length Data", size = 4)
  #scale_fill_manual(values = c("length" = "gray", "weight" = "lightblue"))
length.weight.spp.prop
#dev.off()


# #write .csv of length and weight data by species
# lw.export <- rename(length.weight.spp, length.yes = yes, length.no = no)
# lw.export$weight.yes <- weight.spp$yes
# lw.export$weight.no <- weight.spp$no
# #reorder
# lw.export <- lw.export[, c(1,4,3,2,5,7,8,6)]
# #write csv
# #write.csv(lw.export, file = "Data/Output/Fish_Species_Length_Weight_Data.csv")

#LENGTH AND WEIGHT BY LAKE
table(fish.data4$lake_name, fish.data4$length_yn)
#make this into a data frame
length.lake <- data.frame(table(fish.data4$lake_name, fish.data4$length_yn))
length.lake <- pivot_wider(data = length.lake, names_from = Var2, values_from = Freq)
length.lake <- rename(length.lake, lake = Var1)

#calculate proportion of individuals with lengths for each lake
length.lake$total <- length.lake$no + length.lake$yes
length.lake$prop <- length.lake$yes / length.lake$total

#add weight data to data frame too
lw.lake <- rename(length.lake, length.yes = yes, length.no = no, length.prop = prop)
#reorder
lw.lake <- lw.lake[, c(1,4,3,2,5)]
#create weight data
table(fish.data4$lake_name, fish.data4$weight_yn)
#make this into a data frame
weight.lake <- data.frame(table(fish.data4$lake_name, fish.data4$weight_yn))
weight.lake <- pivot_wider(data = weight.lake, names_from = Var2, values_from = Freq)
weight.lake <- rename(weight.lake, species = Var1)
weight.lake$total <- weight.lake$no + weight.lake$yes
weight.lake$prop <- weight.lake$yes / weight.lake$total
#add weight data to length data
lw.lake$weight.yes <- weight.lake$yes
lw.lake$weight.no <- weight.lake$no
lw.lake$weight.prop <- weight.lake$prop

#write csv
#write.csv(lw.lake, file = "Data/Output/Fish_Lake_Length_Weight_Data.csv")

#order lakes by decreasing proportion to make graph easy to read
lw.lake$lake <- factor(lw.lake$lake, levels = lw.lake$lake[order(lw.lake$length.prop)])

#plot it
#tiff("Length_Weight_Data_by_Lake.tiff", width = 15, height = 10, units = "in", res = 300)                   
length.weight.lake.prop <- ggplot()+
  geom_bar(data = lw.lake, aes(x = lake, y = length.prop), stat = "identity", fill = "gray") +
  geom_bar(data = lw.lake, aes(x = lake, y = weight.prop), stat = "identity", fill = "lightblue")+
  coord_flip() +
  labs(y = "Proprotion of Individuals with Data", x = "Lake", title = "Length and Weight x Lake")+
  geom_text(data = lw.lake, aes(label = total, x = lake, y = length.prop), hjust = -0.1)+
  geom_text(aes(label = "n", x = 44, y = 1.01), vjust = 0.9, size = 5)+
  geom_hline(yintercept = 0.90, color = "red", linewidth = 1)+ #add a vertical like at 90% data coverage
  theme_classic()+
  geom_tile(data = data.frame(x = 3.7, y = 0.92), aes(x = x, y = y), 
            width = 1, height = 0.01, fill = "lightblue") +  # Color box
  annotate("text", x = 3.7, y = 0.97, label = "Weight Data", size = 4)+
  geom_tile(data = data.frame(x = 2, y = 0.92), aes(x = x, y = y), 
            width = 1, height = 0.01, fill = "gray") +  # Color box
  annotate("text", x = 2, y = 0.97, label = "Length Data", size = 4)
#scale_fill_manual(values = c("length" = "gray", "weight" = "lightblue"))
length.weight.lake.prop
#dev.off()

#LENGTH AND WEIGHT BY lake/year
#need to make lake name fish year columns
fish.data4$survey = paste(fish.data4$lake_name, fish.data4$year)
#look at the table
table(fish.data4$survey, fish.data4$length_yn)
#make this into a data frame
length.ly <- data.frame(table(fish.data4$survey, fish.data4$length_yn))
length.ly <- pivot_wider(data = length.ly, names_from = Var2, values_from = Freq)
length.ly <- rename(length.ly, survey = Var1)

#calculate proportion of individuals with lengths for each lake
length.ly$total <- length.ly$no + length.ly$yes
length.ly$prop <- length.ly$yes / length.ly$total

#add weight data to data frame too
lw.ly<- rename(length.ly, length.yes = yes, length.no = no, length.prop = prop)
#reorder
lw.ly <- lw.ly[, c(1,4,3,2,5)]
#create weight data
table(fish.data4$survey, fish.data4$weight_yn)
#make this into a data frame
weight.ly <- data.frame(table(fish.data4$survey, fish.data4$weight_yn))
weight.ly <- pivot_wider(data = weight.ly, names_from = Var2, values_from = Freq)
weight.ly <- rename(weight.ly, survey = Var1)
weight.ly$total <- weight.ly$no + weight.ly$yes
weight.ly$prop <- weight.ly$yes / weight.ly$total
#add weight data to length data
lw.ly$weight.yes <- weight.ly$yes
lw.ly$weight.no <- weight.ly$no
lw.ly$weight.prop <- weight.ly$prop



#write csv
#write.csv(lw.ly, file = "Data/Output/Fish_Survey_Length_Weight_Data.csv")

#order lakes by decreasing proportion to make graph easy to read
lw.ly$survey <- factor(lw.ly$survey, levels = lw.ly$survey[order(lw.ly$length.prop)])

#plot it
#THIS PLOT LOOKS TERRIBLE IN R - BUT IF YOU MAKE THE TIFF WITH HEIGHT = 50 in, ZOOM, AND SCROLL YOU CAN SEE WHAT YOU NEED TO SEE
#tiff("Length_Weight_Data_by_Survey.tiff", width = 15, height = 40, units = "in", res = 300)                   
length.weight.surv.prop <- ggplot()+
  geom_bar(data = lw.ly, aes(x = survey, y = length.prop), stat = "identity", fill = "gray") +
  geom_bar(data = lw.ly, aes(x = survey, y = weight.prop), stat = "identity", fill = "lightblue")+
  coord_flip() +
  labs(y = "Proprotion of Individuals with Data", x = "Fish Survey", title = "Length and Weight x Fish Survey")+
  geom_text(data = lw.ly, aes(label = total, x = survey, y = length.prop), hjust = -0.1)+
  geom_text(aes(label = "n", x = 196, y = 1.01), vjust = 0.9, size = 5)+
  geom_hline(yintercept = 0.90, color = "red", linewidth = 1)+ #add a vertical like at 90% data coverage
  theme_classic()+
  geom_tile(data = data.frame(x = 4, y = 0.92), aes(x = x, y = y), 
            width = 1, height = 0.01, fill = "lightblue") +  # Color box
  annotate("text", x = 4, y = 0.97, label = "Weight Data", size = 4)+
  geom_tile(data = data.frame(x = 2, y = 0.92), aes(x = x, y = y), 
            width = 1, height = 0.01, fill = "gray") +  # Color box
  annotate("text", x = 2, y = 0.7, label = "Length Data", size = 4)
#scale_fill_manual(values = c("length" = "gray", "weight" = "lightblue"))
length.weight.surv.prop
#dev.off()
