#This script makes a list of zooplankton taxa than have been found in the lakes we need to ID zooplankton from


library(dplyr)


zoop <- read.csv("Data/Input/ZoopDB_data_20250204.csv")

#Green Lake
Green <- zoop %>%
  filter(dowlknum == 34007900)
Green.spp <- unique(Green$species)

#----------------------------------------------------------------------------------------------
#Christmas Lake
Christmas<- zoop %>%
  filter(dowlknum == 27013700)
unique(Christmas$species)

#Christmas has not been sampled :(

#----------------------------------------------------------------------------------------------
#Sturgeon Lake
Sturgeon <- zoop %>%
  filter(dowlknum == 58006700)
unique(Sturgeon$species)

#Sturgeon has not been sampled :(

#----------------------------------------------------------------------------------------------
#Clear Lake
Clear <- zoop %>%
  filter(dowlknum == 1009300)
unique(Clear$species)

#Clear has not been sampled :(

#----------------------------------------------------------------------------------------------
#Pelican Lake
Pelican <- zoop %>%
  filter(dowlknum == 18030800)
Pelican.spp <- unique(Pelican$species)


#----------------------------------------------------------------------------------------------
#Elephant Lake
Elephant <- zoop %>%
  filter(dowlknum == 69081000)
Elephant.spp <- unique(Elephant$species)


#----------------------------------------------------------------------------------------------
#export the lakes that have data as a .csv

# #some adjustments to make all the vectors the same length
# max_len <- max(length(Green.spp), length(Pelican.spp), length(Elephant.spp))
# pad_vector <- function(x, length_out) {
#   length(x) <- length_out
#   return(x)
# }
# Green.spp.pad <- pad_vector(Green.spp, max_len)
# Pelican.spp.pad <- pad_vector(Pelican.spp, max_len)
# Elephant.spp.pad <- pad_vector(Elephant.spp, max_len)
# 
# 
# spp.df <- data.frame(Green = Green.spp.pad, Pelican = Pelican.spp.pad, Elephant = Elephant.spp.pad)
# write.csv(spp.df, "Zoop_Taxa_In_Study_Lakes.csv")
