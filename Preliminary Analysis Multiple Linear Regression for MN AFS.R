#this is an analysis of the preliminary data: quality checked and paired contemporary fish/zoop data:
  #zoops: 
    #at least 6 total tows that took place in at least 5 distinct months
  #fish: 
    #standard gillnet surveys (or equivalent) with effort no more than 3 net-nights less than DNR handbook, no gear issues
  #has all other data: temp, secchi, area
#this analysis put on poster and presented at MN AFS conference in Feb 2025
#copied from Biometry class final project and improved :) - see process below and many versions of analysis

library(mosaic)
library(gridExtra)
library(psych)
library(dplyr)
library(performance)
library(see)
library(patchwork)

#THE ORIGINAL VERSION FROM BIOMETRY PROJECT
#plot histograms of each variable to check for logical range and see distribution
Data <- read.csv("Preliminary Data.csv")
WAE.CPUE <- gf_histogram(~WAE.CPUE.inc, data = Data, xlab = "Walleye CPUE")
Prop.Clad <- gf_histogram(~prop.Cladoceran.biomass, data = Data, xlab = "Porportion Cladocerans")
Length <- gf_histogram(~zoop.mean.length, data = Data, xlab = "Zooplankton Mean Length (mm)")
SDI <- gf_histogram(~zoop.Shannon.DI, data = Data, xlab = "Zooplankton Shannon Diversity Index")
Temp <- gf_histogram(~gdd_wtr_5c, data = Data, xlab = "Water Temperature Degree Days")
Secchi <- gf_histogram(~mean.summer.secchi.meters, data = Data, xlab = "Secchi Depth (m)")
Area <- gf_histogram(~lakesize, data = Data, xlab = "Lake Area (acres)")
Area.no.outliers <- gf_histogram(~lakesize, data = (filter(Data, lakesize < 10000)), xlab = "Lake Area (acres) - no outliers")
grid.arrange(WAE.CPUE,
             Prop.Clad,
             Length,
             SDI,
             Temp,
             Secchi,
             Area,
             Area.no.outliers,
             ncol=2, nrow=4
            )

#check for colinearity between predictors
pairs.panels(select(Data,
                    
                    prop.Cladoceran.biomass,
                    zoop.mean.length,
                    zoop.Shannon.DI,
                    gdd_wtr_5c,
                    mean.summer.secchi.meters,
                    lakesize),
             gap = 0.1,
             ellipses = FALSE
            )

#make the linear model
lm.zoop.fish <- lm(WAE.CPUE.inc ~
                     
                     prop.Cladoceran.biomass +
                     zoop.mean.length +
                     zoop.Shannon.DI +
                     gdd_wtr_5c +
                     mean.summer.secchi.meters +
                     lakesize,
                   data = Data
                  )

#check assumptions for the linear model
check_model(lm.zoop.fish, check = c("linearity", "homogeneity", "qq", "normality"))

#look at linear model results
summary(lm.zoop.fish)

#calculate 95% confidence interval for coefficients of all predictor variables
confint(lm.zoop.fish, method = "percentile", level = 0.95)

#ADD RANDOM EFFECT TO SAME PREDICTORS FROM BIOMETRY PROJECT
#SCALE THE LAKE AREA BETTER SO IT MAKES SENSE (per 10,000 acres?)

#TAKE OUT PROP.CLAD BIOMASS?

#TAKE OUT ZOOP MEAN LENGTH?

#TAKE OUT PROP.CLAD BIOMASS AND ZOOP MEAN LENGTH?

#ADD INVASIVES: spiny water fleas and zebra mussels - try each separately and both (presence/absence)


#TRY PROP.LARGE.CLADOCERANS(look for covariance first)

#RICHNESS VS. EVENNESS PORTIONS OF SHANNON DIVERSITY

#TRY MEAN LENGTH AND DIVERSITY OF JUST TAXA THAT SHOW UP IN CORES

