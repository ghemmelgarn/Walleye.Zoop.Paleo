#This script counts how many bosminids have been IDed by the DNR for Adele's UROP proposal

library(dplyr)


zoop <- read.csv("Data/Input/ZoopDB_data_20250204.csv")

unique(zoop$species)
bosmina <- zoop %>%
      filter(species %in% c("Bosmina longirostris", "Bosmina sp.", "Eubosmina coregoni"))
nrow(bosmina)
#9413 rows of bosminids

sum(bosmina$count)
#167615 Bosminids as of 2/4/25
