
#This script explores the remote sensed clarity, Chl-a, and CDOM data to make decisions about how to summarize the data
#It creates many plots while it does this

#DECISION:
#Secchi: use late summer average (Jul 20 - Sept 20), no need to drop anything due to missing data
#Chl-a and CDOM: Use summer average (June 1 - Sept 30), drop anything that calculates an average based only on a June sample



library(tidyverse)
library(readxl)
library(ggplot2)
library(cowplot)
library(scales)
library(readr)
library(dplyr)
library(vegan)
library(tidyr)
library(sf) #to read in .gpkg files
library(foreign) #to read in .dbf files
library(stringr)
library(gridExtra) #to export multiple plots together as .tiff files


#READ IN AND FORMAT SAME AS WHEN MAKING DATASET -------------------------------------------------------------------------------


#read in data (2017-2024)
RS <- st_read("Data/Input/Minnesota Lake Water Quality 2017-2024.gpkg")
#I checked that there is only one layer so don't need to specify

#remove geometry aspects of this file
RS.nogeom <- st_drop_geometry(RS)
#now it is a regular dataframe


#remove unnecessary identifiers and lake information, rename DOW and LakeName columns, and remove summarized data we don't want to deal with (the other time frames)
#also get rid of any lakes without a DOW number - these won't have matchable fish surveys anyways
RS.clean <- RS.nogeom %>% 
  select(-MN_LK_NUM, -unmlknum, -umnlknum_o, -PWI_CLASS, -AREA_BASIN, -WETTYPE, -X_UTM, -Y_UTM, -PolyAcres, -US_L3CODE, -US_L3NAME, -Unnamed..73) %>% 
  rename(DOW = dowlknum_1,
         LakeName = RNAME_1
  ) %>% 
  select(-ends_with("0726_0824")) %>% 
  #also fix a naming issue for all metrics in june, july, august of 2022
  rename(
    SD_202206 = SD_202SD_206,
    SD_202207 = SD_202SD_207,
    SD_202208 = SD_202SD_208,
    CL_202206 = CL_202CL_206,
    CL_202207 = CL_202CL_207,
    CL_202208 = CL_202CL_208,
    a440_202206 = a440_202a440_206,
    a440_202207 = a440_202a440_207,
    a440_202208 = a440_202a440_208
  ) %>% 
  filter(!is.na(DOW))




#I need to get this into rows for each lake-year
#step 1 = pivot longer
RS.long <- RS.clean %>% 
  pivot_longer(
    cols = c(-DOW, -LakeName),
    names_to = "Data",
    values_to = "Value"
  )


#Step 2 = Create separate columns for datatype, year, and month,
RS.long <- RS.long %>% 
  #separate data type
  separate(
    Data,
    into = c("DataType", "Date"),
    sep = "_", #split on underscore
    extra = "merge" #gets everything to the right of first underscore, including other underscores
  ) %>% 
  #separate year and month
  separate(
    Date,
    into = c("Year", "Month"),
    sep = "(?<=^.{4})",   # split after the first 4 characters
    fill = "right" #puts everything else into the other column
  )


#Step 3 = now pivot it back wider so we have a row for each lake-year-datatype
RS.year <- RS.long %>% 
  pivot_wider(
    names_from = Month,
    values_from = Value,
    values_fn = mean #when there are multiple observations for a single dow/lakename combo, this takes the mean of them (they were just sampled at a sub-basin scale)
  )


#THIS IS WHERE EXPLORATORY PLOTS AND CORRELATIONS START-----------------------------------------------------------------------------

#see if I can calculate an equivalent mean to the reported June-Sept average
RS.year$JunSeptTest <- (RS.year$`06`+ RS.year$`07`+ RS.year$`08`+ RS.year$`09`)/4
#THESE ARE NOT THE SAME - BECAUSE IM SURE SAMPLE SIZE WITHIN EACH MONTH IS NOT CONSISTENT
#ALSO THERE ARE MANY MONTHS THAT ARE MISSING DATA - AND THEY REPORT THE SUMMER MEAN ANYWAYS EVEN IF ONLY ONE MONTH OF DATA
#I need to look into how much data is enough to predict the total summer average
#START HERE DOING CORRELATION TO TEST THIS OUT FOR LAKE-YEARS WITH ALL THE DATA

#I WILL USE THE SUMMER AVERAGE THEY REPORT, BUT I WILL RESTRICT TO LAKE-YEARS WITH WHAT I DETERMINE TO BE SUFFICIENT DATA

#filter out only lake-years with all data in all 4 summer months and remove May and October data
RS.test.data <- RS.year %>%
  filter(!is.na(`06`) & !is.na(`07`) & !is.na(`08`) & !is.na(`09`)) %>%
  select(-`05`, -`10`)

#create separate data frames for SD, CL, and CDOM
SD.test.data <- RS.test.data %>%
  filter(DataType == "SD")

CL.test.data <- RS.test.data %>%
  filter(DataType == "CL")

CDOM.test.data <- RS.test.data %>%
  filter(DataType == "a440")

#do correlations of the individual months vs. summer mean for secchi data:
SD_June_cor <- cor(SD.test.data$`06`, SD.test.data$`0601_0930`)

SD_June <- ggplot(data = SD.test.data, aes(x = `06`, y = `0601_0930`)) +
  geom_point()+
  annotate("text", x = 2, y = 7.5, label = paste("r^2 = ",round(SD_June_cor,3)), size = 5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "Secchi June vs. Summer Average (June 1 - Sept 30)", y = "Summer Average (June 1 - Sept 30)", x = "June") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  )


SD_July_cor <- cor(SD.test.data$`07`, SD.test.data$`0601_0930`)

SD_July <- ggplot(data = SD.test.data, aes(x = `07`, y = `0601_0930`)) +
  geom_point()+
  annotate("text", x = 2, y = 7.5, label = paste("r^2 = ",round(SD_July_cor,3)), size = 5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "Secchi July vs. Summer Average (June 1 - Sept 30)", y = "Summer Average (June 1 - Sept 30)", x = "July") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  )


SD_Aug_cor <- cor(SD.test.data$`08`, SD.test.data$`0601_0930`)

SD_Aug <- ggplot(data = SD.test.data, aes(x = `08`, y = `0601_0930`)) +
  geom_point()+
  annotate("text", x = 2, y = 7.5, label = paste("r^2 = ",round(SD_Aug_cor,3)), size = 5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "Secchi August vs. Summer Average (June 1 - Sept 30)", y = "Summer Average (June 1 - Sept 30)", x = "August") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  )


SD_Sept_cor <- cor(SD.test.data$`09`, SD.test.data$`0601_0930`)

SD_Sept <- ggplot(data = SD.test.data, aes(x = `09`, y = `0601_0930`)) +
  geom_point()+
  annotate("text", x = 2, y = 7.5, label = paste("r^2 = ",round(SD_Sept_cor,3)), size = 5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "Secchi September vs. Summer Average (June 1 - Sept 30)", y = "Summer Average (June 1 - Sept 30)", x = "September") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  )



#do correlations of the individual months vs. summer mean for chl-a data:

CL_June_cor <- cor(CL.test.data$`06`, CL.test.data$`0601_0930`)

CL_June <- ggplot(data = CL.test.data, aes(x = `06`, y = `0601_0930`)) +
  geom_point()+
  annotate("text", x = 50, y = 450, label = paste("r^2 = ",round(CL_June_cor,3)), size = 5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "Chlorophyll-a June vs. Summer Average (June 1 - Sept 30)", y = "Summer Average (June 1 - Sept 30)", x = "June") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  )



CL_July_cor <- cor(CL.test.data$`07`, CL.test.data$`0601_0930`)

CL_July <- ggplot(data = CL.test.data, aes(x = `07`, y = `0601_0930`)) +
  geom_point()+
  annotate("text", x = 50, y = 450, label = paste("r^2 = ",round(CL_July_cor,3)), size = 5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "Chlorophyll-a July vs. Summer Average (June 1 - Sept 30)", y = "Summer Average (June 1 - Sept 30)", x = "July") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  )



CL_Aug_cor <- cor(CL.test.data$`08`, CL.test.data$`0601_0930`)

CL_Aug <- ggplot(data = CL.test.data, aes(x = `08`, y = `0601_0930`)) +
  geom_point()+
  annotate("text", x = 100, y = 450, label = paste("r^2 = ",round(CL_Aug_cor,3)), size = 5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "Chlorophyll-a August vs. Summer Average (June 1 - Sept 30)", y = "Summer Average (June 1 - Sept 30)", x = "August") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  )



CL_Sept_cor <- cor(CL.test.data$`09`, CL.test.data$`0601_0930`)

CL_Sept <- ggplot(data = CL.test.data, aes(x = `09`, y = `0601_0930`)) +
  geom_point()+
  annotate("text", x = 100, y = 450, label = paste("r^2 = ",round(CL_Sept_cor,3)), size = 5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "Chlorophyll-a September vs. Summer Average (June 1 - Sept 30)", y = "Summer Average (June 1 - Sept 30)", x = "September") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  )



#do correlations of the individual months vs. summer mean for CDOM data:

CDOM_June_cor <- cor(CDOM.test.data$`06`, CDOM.test.data$`0601_0930`)

CDOM_June <- ggplot(data = CDOM.test.data, aes(x = `06`, y = `0601_0930`)) +
  geom_point()+
  annotate("text", x = 5, y = 27, label = paste("r^2 = ",round(CDOM_June_cor,3)), size = 5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "CDOM June vs. Summer Average (June 1 - Sept 30)", y = "Summer Average (June 1 - Sept 30)", x = "June") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  )



CDOM_July_cor <- cor(CDOM.test.data$`07`, CDOM.test.data$`0601_0930`)

CDOM_July <- ggplot(data = CDOM.test.data, aes(x = `07`, y = `0601_0930`)) +
  geom_point()+
  annotate("text", x = 5, y = 27, label = paste("r^2 = ",round(CDOM_July_cor,3)), size = 5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "CDOM July vs. Summer Average (June 1 - Sept 30)", y = "Summer Average (June 1 - Sept 30)", x = "July") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  )


CDOM_Aug_cor <- cor(CDOM.test.data$`08`, CDOM.test.data$`0601_0930`)

CDOM_Aug <- ggplot(data = CDOM.test.data, aes(x = `08`, y = `0601_0930`)) +
  geom_point()+
  annotate("text", x = 5, y = 27, label = paste("r^2 = ",round(CDOM_Aug_cor,3)), size = 5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "CDOM August vs. Summer Average (June 1 - Sept 30)", y = "Summer Average (June 1 - Sept 30)", x = "August") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  )


CDOM_Sept_cor <- cor(CDOM.test.data$`09`, CDOM.test.data$`0601_0930`)

CDOM_Sept <- ggplot(data = CDOM.test.data, aes(x = `09`, y = `0601_0930`)) +
  geom_point()+
  annotate("text", x = 5, y = 27, label = paste("r^2 = ",round(CDOM_Sept_cor,3)), size = 5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "CDOM September vs. Summer Average (June 1 - Sept 30)", y = "Summer Average (June 1 - Sept 30)", x = "September") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  )



#let's make a summary figure for each variable that I can export and easily view:

#tiff("Remote_Sensed_Clarity_Month_vs_Avg.tiff", width = 10, height = 10, units = "in", res = 300)
grid.arrange(SD_June,
             SD_July,
             SD_Aug,
             SD_Sept,
             ncol=2, nrow=2
)
#dev.off()

#tiff("Remote_Sensed_Chla_Month_vs_Avg.tiff", width = 10, height = 10, units = "in", res = 300)
grid.arrange(CL_June,
             CL_July,
             CL_Aug,
             CL_Sept,
             ncol=2, nrow=2
)
#dev.off()

#tiff("Remote_Sensed_CDOM_Month_vs_Avg.tiff", width = 10, height = 10, units = "in", res = 300)
grid.arrange(CDOM_June,
             CDOM_July,
             CDOM_Aug,
             CDOM_Sept,
             ncol=2, nrow=2
)
#dev.off()


#I ORIGINALLY WANTED THE REPORTED SUMMER AVERAGE (June-Sept) FOR SECCHI, BUT THE OLDER DATA ONLY HAS LATE SUMMER AVERAGE (July-Sept)
#Let's plot how similar these two averages are for all 3 metrics in the newer data (using the test data that is complete with all summer months)

SD_avgs <- cor(SD.test.data$`0720_0920`, SD.test.data$`0601_0930`)

SD_avgs_plot <- ggplot(data = SD.test.data, aes(x = `0720_0920`, y = `0601_0930`)) +
  geom_point()+
  annotate("text", x = 2, y = 7.5, label = paste("r^2 = ",round(SD_avgs,3)), size = 5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "Secchi Late Summer vs. Summer Averages", y = "Summer (June 1 - Sept 30)", x = "Late Summer (July 20 - Sept 20)") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  )


CL_avgs <- cor(CL.test.data$`0720_0920`, CL.test.data$`0601_0930`)

CL_avgs_plot <- ggplot(data = CL.test.data, aes(x = `0720_0920`, y = `0601_0930`)) +
  geom_point()+
  annotate("text", x = 100, y = 450, label = paste("r^2 = ",round(CL_avgs,3)), size = 5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "Chlorophyll-a Late Summer vs. Summer Averages", y = "Summer (June 1 - Sept 30)", x = "Late Summer (July 20 - Sept 20)") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  )

CDOM_avgs <- cor(CDOM.test.data$`0720_0920`, CDOM.test.data$`0601_0930`)

CDOM_avgs_plot <- ggplot(data = CDOM.test.data, aes(x = `0720_0920`, y = `0601_0930`)) +
  geom_point()+
  annotate("text", x = 4, y = 28, label = paste("r^2 = ",round(CDOM_avgs,3)), size = 5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "CDOM Late Summer vs. Summer Averages", y = "Summer (June 1 - Sept 30)", x = "Late Summer (July 20 - Sept 20)") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  )

#export this comparison of the different averages
#tiff("Remote_Sensed_Summer_vs_LateSummer_Avgs.tiff", width = 15, height = 5, units = "in", res = 300)
grid.arrange(SD_avgs_plot,
             CL_avgs_plot,
             CDOM_avgs_plot,
             ncol=3, nrow=1
)
#dev.off()

#What if I compare the two averages including lake-years with incomplete data?
SD.year <- RS.year %>%
  filter(DataType == "SD")

CL.year <- RS.year %>%
  filter(DataType == "CL")

CDOM.year <- RS.year %>%
  filter(DataType == "a440")


SD_avgs2 <- cor(SD.year$`0720_0920`, SD.year$`0601_0930`, use = "complete.obs")

SD_avgs_plot2 <- ggplot(data = SD.year, aes(x = `0720_0920`, y = `0601_0930`)) +
  geom_point()+
  annotate("text", x = 2, y = 7.5, label = paste("r^2 = ",round(SD_avgs2,3)), size = 5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "Secchi Late Summer vs. Summer Averages", y = "Summer (June 1 - Sept 30)", x = "Late Summer (July 20 - Sept 20)") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  )


CL_avgs2 <- cor(CL.year$`0720_0920`, CL.year$`0601_0930`, use = "complete.obs")

CL_avgs_plot2 <- ggplot(data = CL.year, aes(x = `0720_0920`, y = `0601_0930`)) +
  geom_point()+
  annotate("text", x = 100, y = 450, label = paste("r^2 = ",round(CL_avgs2,3)), size = 5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "Chlorophyll-a Late Summer vs. Summer Averages", y = "Summer (June 1 - Sept 30)", x = "Late Summer (July 20 - Sept 20)") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  )

CDOM_avgs2 <- cor(CDOM.year$`0720_0920`, CDOM.year$`0601_0930`, use = "complete.obs")

CDOM_avgs_plot2 <- ggplot(data = CDOM.year, aes(x = `0720_0920`, y = `0601_0930`)) +
  geom_point()+
  annotate("text", x = 4, y = 28, label = paste("r^2 = ",round(CDOM_avgs2,3)), size = 5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "CDOM Late Summer vs. Summer Averages", y = "Summer (June 1 - Sept 30)", x = "Late Summer (July 20 - Sept 20)") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  )

#export this comparison of the different averages
#tiff("Remote_Sensed_Summer_vs_LateSummer_Avgs_IncompleteData.tiff", width = 15, height = 5, units = "in", res = 300)
grid.arrange(SD_avgs_plot2,
             CL_avgs_plot2,
             CDOM_avgs_plot2,
             ncol=3, nrow=1
)
#dev.off()



#NOW TEST EACH MONTH INDIVIDUALLY AGAINST THE LATE SUMMER AVERAGE (drop June this time)
#do correlations of the individual months vs. late summer mean for secchi data:

SD_July_cor2 <- cor(SD.test.data$`07`, SD.test.data$`0720_0920`)

SD_July2 <- ggplot(data = SD.test.data, aes(x = `07`, y = `0720_0920`)) +
  geom_point()+
  annotate("text", x = 2, y = 7.5, label = paste("r^2 = ",round(SD_July_cor2,3)), size = 5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "Secchi July vs. Late Summer Average (July 20 - Sept 20)", y = "Late Summer Average (July 20 - Sept 20)", x = "July") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  )


SD_Aug_cor2 <- cor(SD.test.data$`08`, SD.test.data$`0720_0920`)

SD_Aug2 <- ggplot(data = SD.test.data, aes(x = `08`, y = `0720_0920`)) +
  geom_point()+
  annotate("text", x = 2, y = 7.5, label = paste("r^2 = ",round(SD_Aug_cor2,3)), size = 5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "Secchi August vs. Late Summer Average (July 20 - Sept 20)", y = "Late Summer Average (July 20 - Sept 20)", x = "August") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  )


SD_Sept_cor2 <- cor(SD.test.data$`09`, SD.test.data$`0720_0920`)

SD_Sept2 <- ggplot(data = SD.test.data, aes(x = `09`, y = `0720_0920`)) +
  geom_point()+
  annotate("text", x = 2, y = 7.5, label = paste("r^2 = ",round(SD_Sept_cor2,3)), size = 5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "Secchi September vs. Late Summer Average (July 20 - Sept 20)", y = "Late Summer Average (July 20 - Sept 20)", x = "September") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  )



#do correlations of the individual months vs. late summer mean for chl-a data:

CL_July_cor2 <- cor(CL.test.data$`07`, CL.test.data$`0720_0920`)

CL_July2 <- ggplot(data = CL.test.data, aes(x = `07`, y = `0720_0920`)) +
  geom_point()+
  annotate("text", x = 50, y = 450, label = paste("r^2 = ",round(CL_July_cor2,3)), size = 5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "Chlorophyll-a July vs. Late Summer Average (July 20 - Sept 20)", y = "Late Summer Average (July 20 - Sept 20)", x = "July") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  )



CL_Aug_cor2 <- cor(CL.test.data$`08`, CL.test.data$`0720_0920`)

CL_Aug2 <- ggplot(data = CL.test.data, aes(x = `08`, y = `0720_0920`)) +
  geom_point()+
  annotate("text", x = 100, y = 450, label = paste("r^2 = ",round(CL_Aug_cor2,3)), size = 5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "Chlorophyll-a August vs. Late Summer Average (July 20 - Sept 20)", y = "Late Summer Average (July 20 - Sept 20)", x = "August") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  )



CL_Sept_cor2 <- cor(CL.test.data$`09`, CL.test.data$`0720_0920`)

CL_Sept2 <- ggplot(data = CL.test.data, aes(x = `09`, y = `0720_0920`)) +
  geom_point()+
  annotate("text", x = 100, y = 450, label = paste("r^2 = ",round(CL_Sept_cor2,3)), size = 5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "Chlorophyll-a September vs. Late Summer Average (July 20 - Sept 20)", y = "Late Summer Average (July 20 - Sept 20)", x = "September") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  )



#do correlations of the individual months vs. late summer mean for CDOM data:

CDOM_July_cor2 <- cor(CDOM.test.data$`07`, CDOM.test.data$`0720_0920`)

CDOM_July2 <- ggplot(data = CDOM.test.data, aes(x = `07`, y = `0720_0920`)) +
  geom_point()+
  annotate("text", x = 5, y = 27, label = paste("r^2 = ",round(CDOM_July_cor2,3)), size = 5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "CDOM July vs. Late Summer Average (July 20 - Sept 20)", y = "Late Summer Average (July 20 - Sept 20)", x = "July") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  )


CDOM_Aug_cor2 <- cor(CDOM.test.data$`08`, CDOM.test.data$`0720_0920`)

CDOM_Aug2 <- ggplot(data = CDOM.test.data, aes(x = `08`, y = `0720_0920`)) +
  geom_point()+
  annotate("text", x = 5, y = 27, label = paste("r^2 = ",round(CDOM_Aug_cor2,3)), size = 5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "CDOM August vs. Late Summer Average (July 20 - Sept 20)", y = "Late Summer Average (July 20 - Sept 20)", x = "August") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  )


CDOM_Sept_cor2 <- cor(CDOM.test.data$`09`, CDOM.test.data$`0720_0920`)

CDOM_Sept2 <- ggplot(data = CDOM.test.data, aes(x = `09`, y = `0720_0920`)) +
  geom_point()+
  annotate("text", x = 5, y = 27, label = paste("r^2 = ",round(CDOM_Sept_cor2,3)), size = 5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "CDOM September vs. Late Summer Average (July 20 - Sept 20)", y = "Late Summer Average (July 20 - Sept 20)", x = "September") +
  theme(
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),    # Remove plot background
    axis.line = element_line(color = "black"), # Add axis lines
  )



#let's make a summary figure for each variable that I can export and easily view:

#tiff("Remote_Sensed_Clarity_Month_vs_LateSumAvg.tiff", width = 10, height = 10, units = "in", res = 300)
grid.arrange(SD_July2,
             SD_Aug2,
             SD_Sept2,
             ncol=2, nrow=2
)
#dev.off()

#tiff("Remote_Sensed_Chla_Month_vs_LateSumAvg.tiff", width = 10, height = 10, units = "in", res = 300)
grid.arrange(CL_July2,
             CL_Aug2,
             CL_Sept2,
             ncol=2, nrow=2
)
#dev.off()

#tiff("Remote_Sensed_CDOM_Month_vs_LateSumAvg.tiff", width = 10, height = 10, units = "in", res = 300)
grid.arrange(CDOM_July2,
             CDOM_Aug2,
             CDOM_Sept2,
             ncol=2, nrow=2
)
#dev.off()