#Starts with list of lakes/years with zoop data for 4+ tows/year
#filters to only lakes with matching fish data exact or +/- 1 year or maybe
#Downloads and joins invasive species information from DNR infested waters list
#Joins zoop data
#Joins fish cpue by species for gillnet and trapnet


#packages
library(tidyverse)
library(readxl)
library(ggplot2)
library(cowplot)
library(scales)

#Starting datset known issues:
  #I made it manually in google sheets
  #Inclusive to all potential data that may or may not be used due to various issues
  #Has same fish data matched to multiple zoop years
  #Includes data that disagrees between lakefinder and our spreadsheet
  #no info on months of zoop data
  #No disturbance info
  #has walleye gillnet CPUE that should be overwritten when fish data joined in

#Import starting dataset
StartData <- read.csv("WZ_Lake_Selection.csv")

#filter to only lake/years with potential fish matches
MatchData <- filter(StartData, Match != "No")

#INFESTED WATERS DATA DOWNLOAD AND MERGE

# Data----
## Infested Waters List (UPDATE FILE PATHS, THEN RUN SECTION TO UPDATE INFESTED WATERS DATA) ----
# The code below will automatically download the newest infested-waters.xlsx document from our webpage
# as long as the url for the download hasn't changed. The code will also get the data into 
# a workable/summarized formats for the visualizations below. 

# Click the down arrow by the line number next to "Data ..." to hide/show
# the code for downloading and updating the infested waters data used here.


#ONLY RUN THE FIRST TIME YOU NEED TO GET THE INFESTED WATERS DATA (OR UPDATE IT)
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
#run this if just reading the infested waters spreadsheet already have saved in working directory
iw <- read_excel("infested-waters_2024-10-09.xlsx")
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

#INFESTED WATERS JOIN
#Combining infested water data with the matched fish/zoop data
#Column to match: DOW - but needs to be reformatted in match data

#filter iw to only include confirmed invasive species
iw_confirmed <-  filter(iw, connected=="confirmed")

#remove "-" from infested waters parent dow and remove leading zeroes to match other data
iw_confirmed_format <- iw_confirmed %>%
  mutate(parentdow=str_remove_all(parentdow, "-")) %>%
  mutate(parentdow=ifelse(substr(parentdow, 1, 1)=="0", substr(parentdow, 2, 6), parentdow))

#create parent dow column in MatchData to deal with the fact that Hill lake zoop data is separated by basin
#take first 5 numbers if missing leading 0, first 6 numbers if full 8-digit dow
MatchData_parentdow <- MatchData %>%
  mutate(parentdow = case_when(
    nchar(MatchData$DOW) == 7 ~ substr(DOW, 1, 5),
    nchar(MatchData$DOW) == 8 ~ substr(DOW, 1, 6)
  ))

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
Data_InvSp <- left_join(MatchData_parentdow, iw_wide_format, by = "parentdow")




