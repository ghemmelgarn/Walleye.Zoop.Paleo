#This script creates a list of fish and zooplankton surveys to include in the final pelagic analyses, as well as a list of lake-years that these surveys represent
  #starts with a list of good fish surveys (based on gear and survey type) from the fish database and then does the following:
  #adds area data from LAGOS to check effort and only keep surveys with sufficient effort
  #Imports the zooplankton data and filters to lake-years and surveys with good quality zoop data
  #matches the lists of good fish and zoop surveys to generate a final inclusion table


library(lubridate)
library(arrow)
library(data.table)
library(tidyverse)
library(dplyr)
library(here)


#SETP 1: Get a list of fish surveys from the fish database:------------------------------------------------------------------------------

#this finds the minnesota arrow file that contains all the Minnesota fish data - this is what Denver updated in Nov 2025, only has MN data
mn_data <- open_dataset("G:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Fish Survey Data/Parquet files/mn_update/part-0.parquet")

#the elf returns with info on the data similar to a str function
glimpse(mn_data)

#NOTES ABOUT THE FISH DATABASE:
          #compute function within arrow tells you how big the subset is and what it would be returning - good idea to use first to see if it will crash compu
          #collect function actually brings the data into R 
          
          #filtering join takes a filter table and tells R to filter out the records that match the table
          #I want to only include the lake/years that I have matching fish/zoop data for - my filter table will contain these years
          
          #DATA STRUCTURE NOTES
          #an effort is unit of sampling that includes all sampling done with one sampling method on one lake within a year- this only applies to MN
          #Minnesota doesn't report subefforts
          
          #for dates, year column is sufficient for this project
          
          #species are reported as common names, all lowercase, with underscores for spaces
          
          #pay attention to preserving both a metric and it's corresponding unit column
          
          #flag column tells you if there is some kind of error or concern with the data
          #notes could give you more information 
          
          #ind_fish_ident is agency reported
          #obs_id gives each row(usually a fish but could be nothing caught) a number within this dataset by state
          
          #unless grouped, n is number of fish that the criteria applies to


#first find the surveys that are good
#filter out just the gear I want: standard, shallow, and deep gillnets
#targeted surveys are acceptable for specific lakes (I investigate these individually later in the script after matching to zoop data)
#no historical surveys because they don't have length data so I won't be able to use them to calculate biomass
#takes the distinct columns from the mn_data
#glimpse at end shows me if it did what I wanted

good.fish.surveys <- mn_data %>%
  filter((sampling_method == "gill_net_standard" |
            sampling_method == "gill_net_stratified_deep" |
            sampling_method =="gill_net_stratified_shallow" ) &
           (survey_type == "Standard Survey" |
              survey_type == "Population Assessment"|
              survey_type == "Re-Survey"|
              survey_type == "Large Lake Survey"|
              survey_type == "Initial Survey" |
              survey_type == "Special Assessment" | #will individually investigate if I can use these special assessments IF they match to zoop data
              survey_type == "Targeted Survey")) %>% #will individually investigate if I can use these targeted surveys IF they match to zoop data
  distinct(lake_id,
           lake_name,
           year,
           month, #this is generated from "date_total_effort_ident" so it gives me the correct month that the fishing actually started with my specified gear 
           total_effort_ident,
           total_effort_1,
           sampling_method_simple,
           sampling_method,
           survey_type,
           nhdhr_id,
           flag) %>%
  collect()
#collect actually brings data into R

#fix Crane lake nhdhr_id (wrong in fish database - Denver is fixing it but I will do this for now) - need this to join to LAGOS data
good.fish.surveys <- good.fish.surveys %>% 
  mutate(nhdhr_id = ifelse(nhdhr_id == "nhdhr_{E940A362-4076-4895-A23F-1B8CCC905DEE}", "nhdhr_105953135", nhdhr_id))


#STEP 2: Bring in lake area data from LAGOS----------------------------------------------------------------------------------------------------------------------------------------------


#read in two necessary LAGOS files that contain area and nhdid
Locus.info <- read.csv("Data/Input/lake_information.csv")
Locus.char <- read.csv("Data/Input/lake_characteristics.csv")

#filter the info file to only lakes that are at least partially in Minnesota to make it smaller, then delete the behemoth file
Locus.info.MN <- Locus.info %>% 
  filter(str_detect(lake_states, "MN"))

rm(Locus.info)

#select only the columns you want from both dataframes
#also keeping a bunch of flags (for lakes that cross an international border or have a shape that shows they may be riverine)
Locus.info.select <- Locus.info.MN %>% 
  select(lagoslakeid,
         lake_nhdid,
         lake_namelagos,
         lake_onlandborder, #flag indicating lake polygon is adjacent to or crosses the border with Canada or Mexico
         lake_ismultipart, #flag indicating that the focal lake polygon is multipart
         lake_missingws,	#flag indicating that the WS for the lake was not delineated
         lake_shapeflag	#flag indicating lake polygon shape is angular, either triangular or rectangular, or is elongate and very thin relative to length; respectively may indicate the lake is less likely to be natural in origin or is more riverine
      )

Locus.char.select <- Locus.char %>% 
  select(lagoslakeid,
         lake_waterarea_ha,
  )

#Join the characteristics data to the info data so it only preserves lakes in the info dataset, which means it will only have Minnesota lakes
Locus.all <- left_join(Locus.info.select, Locus.char.select, by = "lagoslakeid")


#remove unneeded intermediate data frames to keep environment clean
rm(Locus.char,
   Locus.char.select,
   Locus.info.MN,
   Locus.info.select
)

#convert hectares to acres
Locus.all$area.acre <- Locus.all$lake_waterarea_ha * 2.4710538147
#I did spot check this to make sure it worked and compared to an online converter to make sure I did it right

#Isolate just the columns I want to join to the fish surveys and rename the nhdid column
Locus.join <- Locus.all %>% 
  select(c(-lagoslakeid, -lake_waterarea_ha)) %>% 
  rename(nhdhr_id = lake_nhdid)

#reformat nhdid to match fish database
Locus.join$nhdhr_id <- paste0("nhdhr_", Locus.join$nhdhr_id)

#Join the acre column and the lagos flags to the fish surveys
#left join to match and replicate all the rows in the fish surveys but not Locus data
#also bring lakename to check things are matching up
fish.surveys.area <- left_join(good.fish.surveys, Locus.join, by = "nhdhr_id")

#check that all the lake names match up
fish.surveys.area.check <- fish.surveys.area %>% 
  mutate(lake_name = ifelse(fish.surveys.area$lake_name == "Otter Tail River(Red Rive", "Otter Tail River(Red River)", fish.surveys.area$lake_name)) %>% #first fix a typo so the code runs
  rowwise() %>% #this makes it check row by row
  mutate(
    Area.Join.Check = grepl(
      pattern = lake_name,
      x = lake_namelagos,
      ignore.case = TRUE
    )
  ) %>% 
  ungroup() #undoes the rowwise function so it doesnt affect later code

#I will hand check the ones that did not auto match the names (for area) by visual inspection AFTER I join to zoop data to make this a much smaller task

#make parentdow column
fish.join <- fish.surveys.area.check %>%
  mutate(parentdow = case_when(
    nchar(fish.surveys.area.check$lake_id) == 7 ~ substr(lake_id, 1, 5),
    nchar(fish.surveys.area.check$lake_id) == 8 ~ substr(lake_id, 1, 6)
  ))

#make parentdow.year column to join to zoop dataset
fish.join$parentdow.year = paste(fish.join$parentdow, fish.join$year)

#mark rows/surveys that have survey-level problems that mean I can't use any of the data (got this code from Denver)
fish.join.marked <- fish.join %>%
  mutate(cpue_invalid = case_when(str_detect(flag, "Do not use in CPUE") |
                                    str_detect(flag, "Not representative sampling") |
                                    str_detect(flag, "Questionable total effort") |
                                    str_detect(flag, "database issue") |
                                    str_detect(flag, "effort value error") |
                                    str_detect(flag, "gear issue") |
                                    str_detect(flag, "high effort") |
                                    str_detect(flag, "likely") |
                                    str_detect(flag, "very high catch") ~ "y",
                                  TRUE ~ "n"))

#filter out these bad surveys
fish.join.clean <- fish.join.marked %>% 
  filter(cpue_invalid == "n")

#keep environment clean
rm(fish.join,
   fish.join.marked,
   fish.surveys.area,
   fish.surveys.area.check,
   good.fish.surveys,
   Locus.all,
   Locus.join)



#STEP 3: Read in zoop data and filter out good surveys (mostly - a few things to spot check with the final list)----------------------------------------------------------------------------------------------------------------------------------------------

zoop <- read.csv("Data/Input/ZoopDB_data_20251016.csv")
#this is the zoop data Kylie sent me on Oct 16, 2025 - it is up to date with what she had processed at that time

#create columns for month, day, and year separately
zoop_months <- zoop %>%
  mutate(year = substr(sample_date, 1, 4)) %>% 
  mutate(month = substr(sample_date, 6, 7)) %>%
  mutate(day = substr(sample_date, 9, 10))

#make parentdow column
zoop_parentdow <- zoop_months %>%
  mutate(parentdow = case_when(
    nchar(zoop$dowlknum) == 7 ~ substr(dowlknum, 1, 5),
    nchar(zoop$dowlknum) == 8 ~ substr(dowlknum, 1, 6)
  ))

#make parentdow.year column to join to fish dataset
zoop_parentdow$parentdow.year = paste(zoop_parentdow$parentdow, zoop_parentdow$year)

#isolate just summer months we will use (May - September)
zoop_summer <- zoop_parentdow %>%
  filter(month == "05" | month == "06" | month == "07" | month == "08" | month == "09")

#make a list of all the unique remarks to check if there is anything else I should filter out
zoop.remarks <- data.frame(unique(zoop_summer$remarks))

#find and remove any littoral samples, horizontal samples, night samples, closing nets, oblique tows, and LMB stomachs
zoop_summer_clean <- zoop_summer %>% 
  filter(!grepl(pattern = "littoral", x = remarks, ignore.case = TRUE)) %>% 
  filter(!grepl(pattern = "horizontal", x = remarks, ignore.case = TRUE)) %>% 
  filter(!grepl(pattern = "night", x = remarks, ignore.case = TRUE)) %>% 
  filter(!grepl(pattern = "closing", x = remarks, ignore.case = TRUE)) %>% 
  filter(!grepl(pattern = "oblique", x = remarks, ignore.case = TRUE)) %>% 
  filter(!grepl(pattern = "LMB", x = remarks, ignore.case = TRUE))

#things I check for later in script to see if it affects any of my lakes and maybe ask Kylie about
  #composite tow
  #special project
  #depth issues
  #shallow/deep


#calculate number of unique summer months for each parentdow.year
zoop_summer_month_count <- zoop_summer_clean %>%
  group_by(parentdow.year) %>%
  summarize(Zoop.Month.Count = n_distinct(month), .groups = 'drop')

#filter to only those with all 5 months and get rid of any lakes without parentdows - these will be useless to me anyways
zoop_5month <- zoop_summer_month_count %>% 
  filter(Zoop.Month.Count == 5) %>% 
  filter(!grepl(pattern = "NA", x = parentdow.year, ignore.case = TRUE))

#I want more info than parentdow.year in my inclusion table, so joining detailed (and filtered) data back to this list
zoop_5month_detail <- left_join(zoop_5month, zoop_summer_clean, by = "parentdow.year")

#lets summarize this into one row for each parentdow.year while preserving lake name and county as this will be good for checking matches later
zoop.good.surveys <- zoop_5month_detail %>%
  group_by(parentdow.year) %>%
  summarize(lake_namezoop = first(lake_name),
            county = first(county),
            .groups = 'drop')


#keep environment clean
rm(zoop,
   zoop_months,
   zoop_parentdow,
   zoop_summer,
   zoop_5month,
   zoop_summer_clean,
   zoop_summer_month_count,
   zoop.remarks)



#STEP 4: Inner join the fish and zoop survey lists to get a first shot at inclusion table ----------------------------------------------------------------------------------------------------------------------------------------------
#still some cleaning to do, but will be easier with shorter list

first.match <- inner_join(fish.join.clean, zoop.good.surveys, by = "parentdow.year")


#Checked lakes where names did not auto-match when I joined LAGOS area data to fish data (by hand, only a few now)
  #this info is in the Area.Join.Check column
#there are only spelling discrepancies in the lakes I am using - all the lakes joined correctly

#check that all the lake names are the same for the fish data and the zoop data
fish.zoop.name.check <- first.match %>% 
  rowwise() %>% #this makes it check row by row
  mutate(
    Name_Found = grepl(
      pattern = lake_name,
      x = lake_namezoop,
      ignore.case = TRUE
    )
  ) %>% 
  ungroup() #undoes the rowwise function so it doesnt affect later code
#this looks good - again only spelling discrepancies


#check for total effort idents that have two rows
Duplicate.rows.vector <- duplicated(first.match$total_effort_ident)
Duplicate.lakeyears <- first.match[Duplicate.rows.vector, ]
Duplicate.rows <- left_join(Duplicate.lakeyears, first.match, by = "total_effort_ident")

#there are 3 lake-years with double rows because they have fish-level flags, need to remove those duplicate rows (get rid of the ones without flags - all other info the same)

match.single <- first.match %>% 
  filter(!((total_effort_ident == 17216 | total_effort_ident == 30891 | total_effort_ident == 39305) & is.na(flag)))

#double check that no duplicates left 
anyDuplicated(match.single$total_effort_ident)
#looks good

#keep environment clean
rm(Duplicate.rows.vector,
   Duplicate.lakeyears,
   Duplicate.rows,
   fish.zoop.name.check)


#STEP 5: Filter out fish surveys with sufficient effort----------------------------------------------------------------------------------------------------------------------------------------------

#create lake size bin column
match.single.bin <- match.single %>%
  mutate(size.bin = ifelse(area.acre < 100, "<100", 
                           ifelse(area.acre >= 100 & area.acre < 300, "100-300", 
                                  ifelse(area.acre >= 300 & area.acre < 600, "300-600", 
                                         ifelse(area.acre >= 600 & area.acre < 1500, "600-1500", ">1500")))))


#create column for minimum net-night effort based on size bin - this is according to the decision rules Denver made
match.single.bin2 <- match.single.bin %>%
  mutate(min.effort = ifelse(size.bin == "<100", 2, ifelse(size.bin == "100-300", 3, ifelse(size.bin == "300-600", 3, ifelse(size.bin == "600-1500", 3, 5)))))

#separate out shallow + deep stratified surveys from the standard gillnet surveys
stratified <- match.single.bin2 %>% 
  filter(sampling_method == "gill_net_stratified_shallow" | sampling_method == "gill_net_stratified_deep")

#add effort from shallow + deep stratified surveys
combined.stratified.effort <- stratified %>%
  group_by(parentdow.year) %>%
  summarize(total_effort_cse = sum(total_effort_1), .groups = 'drop')

#join this sum back to the stratified data
stratified.sum <- left_join(stratified, combined.stratified.effort, by = "parentdow.year")
#remove original effort column 
stratified.sum <- stratified.sum %>% 
  select(-total_effort_1) %>% 
  rename(total_effort_1 = total_effort_cse) %>% #rename the new combined effort to match the rest of the data
  select(lake_id, lake_name, year, month, total_effort_ident, total_effort_1, everything()) #reorder to put effort column back in original location

#remove the stratified rows from the match data
match.no.strat <- match.single.bin2 %>% 
  filter(sampling_method != "gill_net_stratified_shallow" & sampling_method != "gill_net_stratified_deep")

#now paste the new rows back in with the updated effort - now my effort filter will treat them correctly
match.strat.sum <- rbind(match.no.strat, stratified.sum)


#create a column that subtracts total effort from minimum required effort
match.strat.sum$effort.test <- match.strat.sum$total_effort_1 - match.strat.sum$min.effort

#create a column with the effort conclusion 
match.strat.sum <- match.strat.sum %>%
  mutate(fish.effort.sufficient = ifelse (effort.test >= 0, "yes", "no"))

#filter only the good surveys with sufficient effort
match.good.effort <- match.strat.sum %>% 
  filter(fish.effort.sufficient == "yes")


#keep environment clean
rm(match.single.bin,
   match.single.bin2,
   match.single,
   stratified,
   combined.stratified.effort,
   stratified.sum,
   match.no.strat,
   match.strat.sum,
   first.match,
   fish.join.clean,
   zoop.good.surveys
   )


#STEP 6: Individually check the fish surveys that are not a standard survey or equivalent----------------------------------------------------------------------------------------------------------------------------------------------

#isolate the fish surveys to hand-check:
fish.check <- match.good.effort %>% 
  filter((survey_type == "Special Assessment" |
            survey_type == "Targeted Survey"))

#write_csv(fish.check, "Data/Output/Fish_Check.csv")

#I individually checked these surveys with lakefinder, and there are two I can use:
      #South Center 2023 Targeted Survey (total effort ident = 9540)
      #Bear Head 2008 Special Assessment (total effort ident = 37822)


fish.surveys.checked <- match.good.effort %>% 
  #these are the acceptable survey types:
  filter((survey_type == "Standard Survey" |
            survey_type == "Population Assessment"|
            survey_type == "Re-Survey"|
            survey_type == "Large Lake Survey"|
            survey_type == "Initial Survey") |
           #these are the special and targeted assessments that I decided are okay:
           total_effort_ident == 9540 |
           total_effort_ident == 37822
  )

#keep environment clean
rm(fish.check, match.good.effort)

#STEP 7: Investigate LAGOS lake flags-------------------------------------------------------------------------------------------

table(fish.surveys.checked$lake_onlandborder)
#FROM METADATA: flag indicating lake polygon is adjacent to or crosses the border with Canada or Mexico
#this flags Little Vermilion and all the Voyageurs National Park lakes as being on the Canadian border - this is fine

table(fish.surveys.checked$lake_ismultipart)
#FROM METADATA: flag indicating that the focal lake polygon is multipart
#lots of lakes get flagged here, not a problem for this study

table(fish.surveys.checked$lake_missingws)
#FROM METADATA: flag indicating that the WS for the lake was not delineated
#no lakes flagged, would not be a problem anyways

table(fish.surveys.checked$lake_shapeflag)
#FROM METADATA: flag indicating lake polygon shape is angular, either triangular or rectangular, or is elongate and very thin relative to length; respectively may indicate the lake is less likely to be natural in origin or is more riverine
#no lakes flagged, this could be a problem if it were present

#filter out lake Pepin, because I know it's in there and it is riverine - I don't want to include it in this study
no.Pepin <- fish.surveys.checked %>% 
  filter(lake_name != "Pepin")


#STEP 8: Filter out fish surveys that did not happen in June-Sept (no spring or fall) ----------------------------------------------------------------------------------------------------------------------------------------------

#check what months we have
table(no.Pepin$month)

#let's look at what our may and oct surveys are
spring.fall.fish <- no.Pepin %>% 
  filter(month == 5 | month == 10)

#So..... we end up flagging a Lake Crappie survey that happened in late May, usually they sample in early June
#Also flagged most of the Sand Point standard surveys because they always sample in late September or early October
#DECIDED: to leave all of these surveys in because there the month cutoff is somewhat arbitrary


#keep environment clean
rm(fish.surveys.checked,
   spring.fall.fish)



#STEP 9: Check for duplicate lake-year rows ----------------------------------------------------------------------------------------------------------------------------------------------

#check for lake-years that have two rows
Duplicate.rows.vector <- duplicated(no.Pepin$parentdow.year)
Duplicate.lakeyears <- no.Pepin[Duplicate.rows.vector, ]
Duplicate.rows <- left_join(Duplicate.lakeyears, no.Pepin, by = "parentdow.year")

#the only duplicates and the shallow-deep stratified surveys, this is fine

#rename this to indicate that it is the final inclusion list of fish surveys
fish.survey.inclusion.table <- no.Pepin

#keep environment clean
rm(no.Pepin,
   Duplicate.rows.vector,
   Duplicate.lakeyears,
   Duplicate.rows)



#STEP 10: Get list of good zoop surveys -------------------------------------------------------------------------------------------------------------------------------------------
#There are some individual zoop surveys I should not use but I still have enough other data from the lake-year to include it in the study
#Get a zoop survey inclusion table here

#check in zoop remarks for:
#composite tow
#special project
#depth issues
#shallow/deep


#join the detailed zoop data back to the inclusion table as it is now
zoop_inclusion <- left_join(fish.survey.inclusion.table, zoop_5month_detail, by = "parentdow.year")

#check for the flags you want to individually assess
zoop_check <- zoop_inclusion %>% 
  filter(grepl(pattern = "composite", x = remarks, ignore.case = TRUE) |
           grepl(pattern = "special", x = remarks, ignore.case = TRUE) |
           grepl(pattern = "depth", x = remarks, ignore.case = TRUE) |
           grepl(pattern = "shallow", x = remarks, ignore.case = TRUE) |
           grepl(pattern = "deep", x = remarks, ignore.case = TRUE)
  )

#you can't use the special project tows because they did not sample the whole water column
#also get rid of the mille lacs sample where depth was not recorded
#Emailed Kylie to ask what the Rainy lake shallow/deep means - she says "shallow" is epilimnion only and "deep" is full water column
#so get rid of the shallow surveys
#composite did not come up here, but if it does in the future, I need to investigate that
zoop_clean <- zoop_inclusion %>% 
  filter((!grepl(pattern = "special", x = remarks, ignore.case = TRUE))) %>% 
  filter((!grepl(pattern = "no record of depth", x = remarks, ignore.case = TRUE))) %>% 
  filter((!grepl(pattern = "shallow", x = remarks, ignore.case = TRUE)))

#refilter the zoop data to make sure the month requirements are still met

#calculate number of unique summer months for each parentdow.year
zoop_summer_month_count2 <- zoop_clean %>%
  group_by(parentdow.year) %>%
  summarize(Zoop.Month.Count = n_distinct(month.y), .groups = 'drop')

#filter to only those with all 5 months and get rid of any lakes without parentdows - these will be useless to me anyways
zoop_5month2 <- zoop_summer_month_count2 %>% 
  filter(Zoop.Month.Count == 5) %>% 
  filter(!grepl(pattern = "NA", x = parentdow.year, ignore.case = TRUE))

#I want info on individual zoop surveys so joining this back to the cleaned zoop detailed data
zoop_final_details <- left_join(zoop_5month2, zoop_clean, by = "parentdow.year")
#This did not remove any lake-years BUT keep this code in case in the future it does

#lets summarize this into one row for each zoop tow/survey 
zoop.survey.inclusion.table <- zoop_final_details %>%
  group_by(parentdow.year, lake_name.y, sample_id) %>%
  summarize(.groups = 'drop')


#As of 11/7/25 this second zoop quality check caused me to lose a few tows but I did not lose any lake-years entirely = no change to fish inclusion table
#however, pay attention in case with future data it would matter
  



#STEP 11: Finalize and save inclusion tables ---------------------------------------------------------------------------------------------------------------------------------------------

#Get a list / count of lake-years from both inclusion tables to make sure they match (after final zoop cleaning):
zoop.lakeyears <- zoop.survey.inclusion.table %>%
  group_by(parentdow.year) %>%
  summarize(.groups = 'drop')

fish.lakeyears <- fish.survey.inclusion.table %>%
  group_by(parentdow.year) %>%
  summarize(.groups = 'drop')

#check that these two lists are the same
identical(zoop.lakeyears, fish.lakeyears)
#They are!!

#Get a list of unique lakes
unique(fish.survey.inclusion.table$lake_name)

#Make and informative lake-year inclusion table
lakeyears <- fish.survey.inclusion.table %>%
  group_by(parentdow.year, lake_name) %>%
  summarize(.groups = 'drop')


# #Save the three inclusion tables
# #Let's be sneaky and save it in the input folder hehehe
# write_csv(fish.survey.inclusion.table, "Data/Input/Fish_Survey_Pelagic_Inclusion_Table.csv")
# write_csv(zoop.survey.inclusion.table, "Data/Input/Zoop_Survey_Pelagic_Inclusion_Table.csv")
# write_csv(lakeyears, "Data/Input/LakeYear_Pelagic_Inclusion_Table.csv")

#keep environment clean
rm(zoop.lakeyears,
   fish.lakeyears,
   zoop_5month_detail,
   zoop_5month_detail2,
   zoop_5month2,
   zoop_check,
   zoop_final_details,
   good.fish.surveys,
   zoop_clean,
   zoop_inclusion,
   zoop_summer_month_count2,
   zoop.good.lakeyears
   )
