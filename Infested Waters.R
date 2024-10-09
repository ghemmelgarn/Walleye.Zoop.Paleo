
#Extracts and formats infested waters list from most recent list publishd by MN DNR

#packages
library(tidyverse)
library(readxl)
library(ggplot2)
library(cowplot)
library(scales)

# Data----
## Infested Waters List (UPDATE FILE PATHS, THEN RUN SECTION TO UPDATE INFESTED WATERS DATA) ----
# The code below will automatically download the newest infested-waters.xlsx document from our webpage
# as long as the url for the download hasn't changed. The code will also get the data into 
# a workable/summarized formats for the visualizations below. 

# Click the down arrow by the line number next to "Data ..." to hide/show
# the code for downloading and updating the infested waters data used here.

# Infested Waters URL
url_iw <- "https://files.dnr.state.mn.us/eco/invasives/infested-waters.xlsx"

### UPDATE FILE PATH FOR IW DOWNLOAD----
# Path to where I want data stored
data_path <- "G:/My Drive/Thesis/Data/R Working Directories/Walleye.Zoop.Paleo/"

# Date data was downloaded
# This will make it so that a new file is generated in my Data folder every time I run this code
date_downloaded <- Sys.Date()

# Name I want to save infested waters under
data_name <- paste("infested-waters_", date_downloaded, ".xlsx", sep="")

# Paste together data_path and data_name to tell R where to save downloaded infested waters list
destfile <- paste(data_path, data_name, sep="")

# Download infested waters list
download.file(url_iw, destfile, mode='wb')

# Read in infested waters list
iw <- read_excel(path=destfile, skip=1)
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

#DID NOT EDIT BELOW THIS - WILL MAKE NEW R SCRIPT FOR MERGING
#KEEPING BELOW FOR REFERENCE, COPYING ABOVE TO OTHER SCRIPT

## Walleye-Zoop lakes----
### UPDATE FILE PATH WHERE WAE DATA IS STORED ----
wae_lakes <- read_csv("D:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Infested Waters ZM WAE lakes - HK, JW/wae_NR_class.csv")


## Getting wae_lakes and iw to work together----
# now, "dowlknum" is mostly parent dows, with some 8-digit basin-level DOWS and some NAs for 
# waterbodies without DOWs

unique(iw$dowlknum)
unique(iw$parentdow)

# Looks like a mostly clean list of DOWs with the leading zero problem + a couple combined entries
unique(wae_lakes$DOW)
wae_lakes$DOW[substr(wae_lakes$DOW, 1, 1)=="0"]

# Breaking down what kinds of entries are in here
unique(wae_lakes$DOW[nchar(wae_lakes$DOW)<=6])
unique(wae_lakes$DOW[nchar(wae_lakes$DOW)==7])# these all look like counties 01 - 09 without the leading zero
print(unique(wae_lakes$DOW[nchar(wae_lakes$DOW)==8]), max=1500)# these look like clean, 8 digit DOWs
unique(wae_lakes$DOW[nchar(wae_lakes$DOW)>=9])# these should be fairly easy to account for

iw%>%filter(dowlknum%in%"18-0269" & species=="zebra mussel")# infested
iw%>%filter(dowlknum%in%"18-0268" & species=="zebra mussel")# infested
iw%>%filter(dowlknum%in%"18-0311" & species=="zebra mussel")# infested
iw%>%filter(dowlknum%in%"18-0270" & species=="zebra mussel")# not known to be infested

# Pulling hyphens for infested waters, adding "00" for entries with just parent DOWs, 
# then dropping leading zeros, should do the trick to match "iw" to "wae_lakes"
iw_wae <- iw %>%
  mutate(DOW=str_remove_all(dowlknum, "-")) %>%
  mutate(DOW=ifelse(nchar(DOW)==6, paste(DOW, "00", sep=""), DOW)) %>%
  mutate(DOW=ifelse(substr(DOW, 1, 1)=="0", substr(DOW, 2, 8), DOW)) %>%
  filter(!(is.na(year) | is.na(species) | is.na(dowlknum)))

unique(iw_wae$DOW[nchar(iw_wae$DOW)<=6])
unique(iw_wae$DOW[nchar(iw_wae$DOW)==7])# these all look like counties 01 - 09 without the leading zero
unique(iw_wae$DOW[nchar(iw_wae$DOW)==8])# these look like clean, 8 digit DOWs
unique(iw_wae$DOW[nchar(iw_wae$DOW)>=9])# these should be fairly easy to account for

# Identifying which infested waters lakes have walleye
iw_wae$wae <- ifelse(iw_wae$DOW%in%wae_lakes$DOW, 1, 0)

# The combined dow entries above will have wae=0 here, so need to change that manually
iw_wae$wae[iw_wae$parentdow=="18-0269"] <- 1
iw_wae$wae[iw_wae$parentdow=="18-0268"] <- 1
iw_wae$wae[iw_wae$parentdow=="18-0311"] <- 1

# Filtering down to confirmed zebra mussel infestations, then grouping by parentdow
# and summarizing to the earliest designation for a parent dow and wae presence
iw_wae_zm_parentdow <- iw_wae %>%
  filter(species=="zebra mussel" & connected=="confirmed") %>%
  group_by(parentdow) %>%
  summarize(year=min(year), 
            wae=max(wae)) %>%
  ungroup()

# Grouping by year, estimating number of infestations in that year, then 
# taking the cumulative sum of infestations.
iw_wae_zm_parentdow_yrsum <- iw_wae_zm_parentdow %>%
  group_by(year) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  mutate(cumsum_n=cumsum(n))

# Doing the same, but for walleye lakes
iw_wae_zm_parentdow_yrsum_wae <- iw_wae_zm_parentdow %>%
  filter(wae==1) %>%
  group_by(year) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  mutate(cumsum_n=cumsum(n))

# Summaries and data viz ----
#infested lakes 
nrow(iw_wae_zm_parentdow) #316
table(iw_wae_zm_parentdow$wae) #243 are walleye lakes

#proportion walleye lakes
sum(iw_wae_zm_parentdow$wae)/nrow(iw_wae_zm_parentdow) #77%

iw_wae_zm_parentdow_yrsum %>% filter(year==2022)
iw_wae_zm_parentdow_yrsum_wae %>% filter(year==2022)



# There are 1435 known walleye lakes in the state.
# There are 11,842 lakes > 10 acres in the state. https://www.dnr.state.mn.us/faq/mnfacts/water.html

# Statewide ZM known infestation rate = 2.6%
nrow(iw_wae_zm_parentdow)/11842

# ZM known infestation rate in known walleye lakes = 17% 
sum(iw_wae_zm_parentdow$wae)/1435

# ZM known infestation rate in lakes not known to have walleye = 0.7%
73/10407

# ZM are 24 times more likely to be found in lakes known to contain walleye
(sum(iw_wae_zm_parentdow$wae)/1435)/(73/10407)

# Updated infestation over time figure
p_zm_parentdow_cumsum_n <- ggplot() + 
  geom_line(data=iw_wae_zm_parentdow_yrsum, 
            aes(x=year, y=cumsum_n)) + 
  geom_line(data=iw_wae_zm_parentdow_yrsum_wae, 
            aes(x=year, y=cumsum_n), col='dodgerblue') + 
  scale_y_continuous(name="Cumulative infested lakes*") + 
  scale_x_continuous(name="Year designated as infested") + 
  #ggtitle(expression("*Confirmed infested lakesn "),
  #        subtitle=expression("(i.e., 6-digit DOW not including 'connected' waterbodies)")) + 
  theme_cowplot(10) + theme(legend.position='top', axis.title.x=element_blank())

p_zm_parentdow_n <- ggplot() + 
  geom_point(data=iw_wae_zm_parentdow_yrsum, 
             aes(x=year, y=n), cex=2) + 
  geom_point(data=iw_wae_zm_parentdow_yrsum_wae, 
             aes(x=year, y=n), col='dodgerblue', pch=17) + 
  scale_y_continuous(name="New infested lakes*", breaks=seq(0, 30, 10),
                     labels=c("  0", "  10", "  20", "  30")) + 
  scale_x_continuous(name="Year designated as infested") + 
  theme_cowplot(10) + theme(legend.position='top')

pdf("iw_zm_parentdow.pdf", width=3, height=5, pointsize=1/300, useDingbats=F)
plot_grid(p_zm_parentdow_cumsum_n, p_zm_parentdow_n, nrow=2, labels='auto')
dev.off()

# What was dropped from the original plot of all infested waters entries?
# It should only be like 20-30 entries
iw_sum %>% filter(species=="zebra mussel" & connected=="confirmed") %>% as.data.frame()#332

# Mostly rivers
iw %>% filter(species=="zebra mussel" & connected=="confirmed" & !parentdow%in%iw_wae_zm_parentdow$parentdow) %>% as.data.frame()


#list of lakes
write.csv(iw_wae_zm_parentdow, "infested_waters_parent_dow.csv", row.names = F)

