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
library(lme4) #to run mixed effects model
library(lmerTest) #to get p values for mixed effects model
library(MuMIn) #to get r squared for mixed effects model

Data <- read.csv("Data/Input/Preliminary Data.csv")

#---------------------------------------------------------------------------------------------

#THE ORIGINAL VERSION FROM BIOMETRY PROJECT

#plot histograms of each variable to check for logical range and see distribution
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
pairs.panels(dplyr::select(Data,
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

#---------------------------------------------------------------------------------------------

#ADD RANDOM EFFECT TO SAME PREDICTORS FROM BIOMETRY PROJECT
#now need lmer function in lme4 package for a mixed effects model and lmerTest package to get p-values for these predictors

lm.random.effect <- lmer(WAE.CPUE.inc ~
                     prop.Cladoceran.biomass +
                     zoop.mean.length +
                     zoop.Shannon.DI +
                     gdd_wtr_5c +
                     mean.summer.secchi.meters +
                     lakesize +
                     (1 | DOW),
                   data = Data
                  )

summary(lm.random.effect)
r.squaredGLMM(lm.random.effect)

#marginal R2 = 0.37
#conditional R2 = 0.74

#HOW TO INTERPRET THESE R2 VALUES: marginal only considers fixed effects, conditional considers both fixed and random effects (includes lake)

#summary of changes:
  #intercept no longer significant
  #prop.Cladoceran.biomass very not significant (before p = 0.09)
  #zoop shannon DI still strongly significant
  #secchi no longer significant
  #lakesize still significant


#---------------------------------------------------------------------------------------------

#SCALE THE LAKE AREA BETTER SO IT MAKES SENSE (per 10,000 acres?) (ALSO DEGREE DAYS)

#change lake area to units of 10,000 acres and degree days to units of 1,000 days
Data$lakesize <- as.numeric(Data$lakesize)
Data.scaled <- Data %>%
  mutate(LS10000acres = lakesize/10000,
         GDD1000days = gdd_wtr_5c/1000)

#make the histograms again to see how this looks now
WAE.CPUE <- gf_histogram(~WAE.CPUE.inc, data = Data.scaled, xlab = "Walleye CPUE")
Prop.Clad <- gf_histogram(~prop.Cladoceran.biomass, data = Data.scaled, xlab = "Porportion Cladocerans")
Length <- gf_histogram(~zoop.mean.length, data = Data.scaled, xlab = "Zooplankton Mean Length (mm)")
SDI <- gf_histogram(~zoop.Shannon.DI, data = Data.scaled, xlab = "Zooplankton Shannon Diversity Index")
Temp <- gf_histogram(~GDD1000days, data = Data.scaled, xlab = "1,000 Water Temperature Degree Days")
Secchi <- gf_histogram(~mean.summer.secchi.meters, data = Data.scaled, xlab = "Secchi Depth (m)")
Area <- gf_histogram(~LS10000acres, data = Data.scaled, xlab = "Lake Area (10,000 acres)")
Area.no.outliers <- gf_histogram(~LS10000acres, data = (filter(Data.scaled, LS10000acres < 1)), xlab = "Lake Area (10,000 acres) - no outliers")
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

lm.random.effect.scaled <- lmer(WAE.CPUE.inc ~
                           prop.Cladoceran.biomass +
                           zoop.mean.length +
                           zoop.Shannon.DI +
                           GDD1000days +
                           mean.summer.secchi.meters +
                           LS10000acres +
                           (1 | DOW),
                         data = Data.scaled
)

summary(lm.random.effect.scaled)
r.squaredGLMM(lm.random.effect.scaled)

#marginal R2 = 0.37
#conditional R2 = 0.74

#summary of changes
  #results same (as expected)
  #estimates easier to interpret now.... but PAY ATTENTION TO UNITS
#---------------------------------------------------------------------------------------------

#ADD INTERACTION OF PRODUCTIVITY AND TEMP JUST TO SEE WHAT HAPPENS

lm.test1 <- lmer(WAE.CPUE.inc ~
                   prop.Cladoceran.biomass +
                   zoop.mean.length +
                   zoop.Shannon.DI +
                   GDD1000days +
                   mean.summer.secchi.meters +
                   LS10000acres +
                   GDD1000days:mean.summer.secchi.meters +
                   (1 | DOW),
                 data = Data.scaled
                                  
)

summary(lm.test1)
r.squaredGLMM(lm.test1)

#marginal R2 = 0.40
#conditional R2 = 0.74

#this interaction is not significant as the model is now
#fixed effects explain a little more of the variability now


#---------------------------------------------------------------------------------------------

#ADD INTERACTION OF PRODUCTIVITY AND ZOOP LENGTH JUST TO SEE WHAT HAPPENS

lm.test2 <- lmer(WAE.CPUE.inc ~
                   prop.Cladoceran.biomass +
                   zoop.mean.length +
                   zoop.Shannon.DI +
                   GDD1000days +
                   mean.summer.secchi.meters +
                   LS10000acres +
                   mean.summer.secchi.meters:zoop.mean.length +
                   (1 | DOW),
                 data = Data.scaled
                 
)

summary(lm.test2)
r.squaredGLMM(lm.test2)

#marginal R2 = 0.38
#conditional R2 = 0.75

#this interaction is not significant as the model is now

#---------------------------------------------------------------------------------------------

#ADD INTERACTION OF PRODUCTIVITY AND ZOOP DIVERSITY JUST TO SEE WHAT HAPPENS

lm.test3 <- lmer(WAE.CPUE.inc ~
                   prop.Cladoceran.biomass +
                   zoop.mean.length +
                   zoop.Shannon.DI +
                   GDD1000days +
                   mean.summer.secchi.meters +
                   LS10000acres +
                   mean.summer.secchi.meters:zoop.Shannon.DI +
                   (1 | DOW),
                 data = Data.scaled
                 
)

summary(lm.test3)
r.squaredGLMM(lm.test3)

#marginal R2 = 0.36
#conditional R2 = 0.74

#this interaction is not significant as the model is now

#---------------------------------------------------------------------------------------------

#TAKE OUT PROP.CLAD BIOMASS?

lm.test4 <- lmer(WAE.CPUE.inc ~
                   zoop.mean.length +
                   zoop.Shannon.DI +
                   GDD1000days +
                   mean.summer.secchi.meters +
                   LS10000acres +
                   (1 | DOW),
                 data = Data.scaled
                 
)

summary(lm.test4)
r.squaredGLMM(lm.test4)

#marginal R2 = 0.37
#conditional R2 = 0.74

#still getting zoop diversity and lake size as the important predictors

#---------------------------------------------------------------------------------------------

#TAKE OUT ZOOP MEAN LENGTH?

lm.test5 <- lmer(WAE.CPUE.inc ~
                   prop.Cladoceran.biomass +
                   zoop.Shannon.DI +
                   GDD1000days +
                   mean.summer.secchi.meters +
                   LS10000acres +
                   (1 | DOW),
                 data = Data.scaled
                 
)

summary(lm.test5)
r.squaredGLMM(lm.test5)

#marginal R2 = 0.37
#conditional R2 = 0.74

#still getting zoop diversity and lake size as the important predictors

#---------------------------------------------------------------------------------------------

#TAKE OUT PROP.CLAD BIOMASS AND ZOOP MEAN LENGTH?

lm.test6 <- lmer(WAE.CPUE.inc ~
                   zoop.Shannon.DI +
                   GDD1000days +
                   mean.summer.secchi.meters +
                   LS10000acres +
                   (1 | DOW),
                 data = Data.scaled
                 
)

summary(lm.test6)
r.squaredGLMM(lm.test6)

#marginal R2 = 0.37
#conditional R2 = 0.74

#still getting zoop diversity and lake size as the important predictors


#---------------------------------------------------------------------------------------------

#TAKE OUT PROP.CLAD BIOMASS AND ZOOP MEAN LENGTH AND DEGREE DAYS

lm.test7 <- lmer(WAE.CPUE.inc ~
                   zoop.Shannon.DI +
                   mean.summer.secchi.meters +
                   LS10000acres +
                   (1 | DOW),
                 data = Data.scaled
                 
)

summary(lm.test7)
r.squaredGLMM(lm.test7)

#marginal R2 = 0.38
#conditional R2 = 0.74

#still getting zoop diversity and lake size as the important predictors - wow this really isn't changing

#---------------------------------------------------------------------------------------------
#WHAT CAN WE PREDICT WITH JUST ZOOP DIVERISTY AND LAKESIZE

lm.test8 <- lmer(WAE.CPUE.inc ~
                   zoop.Shannon.DI +
                   LS10000acres +
                   (1 | DOW),
                 data = Data.scaled
                 
)

summary(lm.test8)
r.squaredGLMM(lm.test8)

#marginal R2 = 0.38
#conditional R2 = 0.75

#both are significant, and R2 values are the same - this tells me the other predictors really are not doing anything

#---------------------------------------------------------------------------------------------
#WHAT CAN WE PREDICT WITH JUST ZOOP DIVERISTY?

lm.test9 <- lmer(WAE.CPUE.inc ~
                   zoop.Shannon.DI +
                   (1 | DOW),
                 data = Data.scaled
                 
)

summary(lm.test9)
r.squaredGLMM(lm.test9)

#marginal R2 = 0.16
#conditional R2 = 0.71

#R2 drops - so lake size is important. I wonder if the few large outliers are having a disproportionate effect

#---------------------------------------------------------------------------------------------

#LETS GO BACK TO ORIGINAL MIXED EFFECTS MODEL BUT REMOVE THE DATA WITH LAKE SIZE OVER 150,000 acres

Data.scaled.filtered <- Data.scaled %>%
  filter(LS10000acres < 15)

#lost 3 observations with this

lm.test10 <- lmer(WAE.CPUE.inc ~
                     prop.Cladoceran.biomass +
                     zoop.mean.length +
                     zoop.Shannon.DI +
                     GDD1000days +
                     mean.summer.secchi.meters +
                     LS10000acres +
                     (1 | DOW),
                   data = Data.scaled.filtered
                  ) 
                                 

summary(lm.test10)
r.squaredGLMM(lm.test10)

#marginal R2 = 0.35
#conditional R2 = 0.75

#lake size suddenly not so important but still ALMOST significant ... I think this was a good thing to do
#R2 still around 35%
#zoop diversity the only significant predictor

#--------------------------------------------------------------------------------------------
#HOW ARE ZOOP DIVERSITY AND LAKE AREA RELATED

zoop.diversity.lakesize <- ggplot(data = Data.scaled.filtered, aes(LS10000acres, zoop.Shannon.DI)) +
  geom_point()
print(zoop.diversity.lakesize)

#what I see here is that small lakes can have low or fairly high diversity but big lakes always have high diversity
#not a linear relationship


#What if I run a model to see if lake size predicts zoop diversity 


lm.test11 <- lmer(zoop.Shannon.DI ~
                    LS10000acres +
                    (1 | DOW),
                  data = Data.scaled.filtered
) 


summary(lm.test11)
r.squaredGLMM(lm.test11)

#marginal R2 = 0.59
#conditional R2 = 0.83

#the answer is yes, zoop diversity and lake size are related and in the correlation matrix it does look like they covary some
#


#---------------------------------------------------------------------------------------------

#LETS GO TRY THE ORIGINAL MODEL MINUS LAKE AREA

lm.test12 <- lmer(WAE.CPUE.inc ~
                    prop.Cladoceran.biomass +
                    zoop.mean.length +
                    zoop.Shannon.DI +
                    GDD1000days +
                    mean.summer.secchi.meters +
                    (1 | DOW),
                  data = Data.scaled.filtered
) 


summary(lm.test12)
r.squaredGLMM(lm.test12)

#marginal R2 = 0.18
#conditional R2 = 0.69

#now only significant predictor is zoop diversity
#R2 dropped a lot
#I think lake area is important to have in there because they do not linearly covary but has an effect that should be accounted for

#--------------------------------------------------------------------------------------------

#ADD INVASIVES: spiny water fleas and zebra mussels - try each separately and both (presence/absence)
#adding this to original complete model

#calculate columns that are yes/no for zebra mussels and swf present
Data.scaled.filtered <- Data.scaled.filtered %>%
  mutate(zm.present = ifelse(is.na(ZebraMussel), "no", ifelse(ZoopYear > ZebraMussel, "yes", "no")))

Data.scaled.filtered <- Data.scaled.filtered %>%
  mutate(swf.present = ifelse(is.na(SpinyWaterflea), "no", ifelse(ZoopYear > SpinyWaterflea, "yes", "no")))

#first, just zebra mussels present
#plot zebra mussels against zoop diversity, zoop length, prop.clad, and secchi depth to see if those changed
zm.zoop.diversity <- ggplot(data = Data.scaled.filtered, aes(zm.present, zoop.Shannon.DI)) +
  geom_boxplot()+ 
  facet_wrap(~LakeName)
print(zm.zoop.diversity)

#zebra mussels associated with lower zoop diversity - magnitude varies by lake

zm.zoop.length <- ggplot(data = Data.scaled.filtered, aes(zm.present, zoop.mean.length)) +
  geom_boxplot()+ 
  facet_wrap(~LakeName)
print(zm.zoop.length)

#zebra mussels associated with slightly lower zoop length - magnitude small and varies by lake

zm.prop.clad <- ggplot(data = Data.scaled.filtered, aes(zm.present, prop.Cladoceran.biomass)) +
  geom_boxplot()+ 
  facet_wrap(~LakeName)
print(zm.prop.clad)

#zebra mussels associated with various effects on prop.clad.biomass - magnitude and direction varies by lake

zm.secchi <- ggplot(data = Data.scaled.filtered, aes(zm.present, mean.summer.secchi.meters)) +
  geom_boxplot()+ 
  facet_wrap(~LakeName)
print(zm.secchi)

#zebra mussels associated with higher secchi - magnitude varies by lake

#looks like zebra mussels may be correlated with some differences in these

#add zebra mussels to original model
lm.test13 <- lmer(WAE.CPUE.inc ~
                    prop.Cladoceran.biomass +
                    zoop.mean.length +
                    zoop.Shannon.DI +
                    GDD1000days +
                    mean.summer.secchi.meters +
                    LS10000acres +
                    zm.present +
                    (1 | DOW),
                  data = Data.scaled.filtered
) 


summary(lm.test13)
r.squaredGLMM(lm.test13)

#marginal R2 = 0.35
#conditional R2 = 0.75

#zebra mussels did not have a significant effect
         
#what if we make an interaction of zebra mussels and secchi
#add zebra mussels to original model
lm.test14 <- lmer(WAE.CPUE.inc ~
                    prop.Cladoceran.biomass +
                    zoop.mean.length +
                    zoop.Shannon.DI +
                    GDD1000days +
                    mean.summer.secchi.meters +
                    LS10000acres +
                    zm.present +
                    zm.present:mean.summer.secchi.meters +
                    (1 | DOW),
                  data = Data.scaled.filtered
) 
summary(lm.test14)
r.squaredGLMM(lm.test14)

#marginal R2 = 0.35
#conditional R2 = 0.75

#not a significant interaction

#lets try a zebra mussel and zoop diversity interaction
lm.test15 <- lmer(WAE.CPUE.inc ~
                    prop.Cladoceran.biomass +
                    zoop.mean.length +
                    zoop.Shannon.DI +
                    GDD1000days +
                    mean.summer.secchi.meters +
                    LS10000acres +
                    zm.present +
                    zm.present:zoop.Shannon.DI +
                    (1 | DOW),
                  data = Data.scaled.filtered
) 
summary(lm.test15)
r.squaredGLMM(lm.test15)
#marginal R2 = 0.35
#conditional R2 = 0.75

#also not a significant interaction

#DO IT ALL AGAIN WITH THE SPINY WATER FLEAS
#plot swf against zoop diversity, zoop length, and prop.clad to see if those changed

swf.zoop.diversity <- ggplot(data = Data.scaled.filtered, aes(swf.present, zoop.Shannon.DI)) +
  geom_boxplot() + 
  facet_wrap(~LakeName)
print(swf.zoop.diversity)

#it does look like bythotrephes may be associated with lower zoop diversity - magnitude varies by lake

swf.zoop.length <- ggplot(data = Data.scaled.filtered, aes(swf.present, zoop.mean.length)) +
  geom_boxplot()+ 
  facet_wrap(~LakeName)
print(swf.zoop.length)

#maybe slightly smaller zoops with swf but occasinally bigger, small magnitude

swf.prop.clad <- ggplot(data = Data.scaled.filtered, aes(swf.present, prop.Cladoceran.biomass)) +
  geom_boxplot()+ 
  facet_wrap(~LakeName)
print(swf.prop.clad)

#bytho associated wtih generally lower prop.clad.biomass but not by very much

swf.secchi <- ggplot(data = Data.scaled.filtered, aes(swf.present, mean.summer.secchi.meters)) +
  geom_boxplot()+ 
  facet_wrap(~LakeName)
print(swf.secchi)

#swf associated with variable small effects in secchi depth


#add swf to original model
lm.test16 <- lmer(WAE.CPUE.inc ~
                    prop.Cladoceran.biomass +
                    zoop.mean.length +
                    zoop.Shannon.DI +
                    GDD1000days +
                    mean.summer.secchi.meters +
                    LS10000acres +
                    swf.present +
                    (1 | DOW),
                  data = Data.scaled.filtered
) 


summary(lm.test16)
r.squaredGLMM(lm.test16)

#marginal R2 = 0.35
#conditional R2 = 0.75

#swf did not have a significant effect


#what if we make an interaction of swf and prop.clad
lm.test17 <- lmer(WAE.CPUE.inc ~
                    prop.Cladoceran.biomass +
                    zoop.mean.length +
                    zoop.Shannon.DI +
                    GDD1000days +
                    mean.summer.secchi.meters +
                    LS10000acres +
                    zm.present +
                    swf.present:prop.Cladoceran.biomass +
                    (1 | DOW),
                  data = Data.scaled.filtered
) 
summary(lm.test17)
r.squaredGLMM(lm.test17)

#marginal R2 = 0.35
#conditional R2 = 0.75

#not a significant interaction

#okay so overall it looks like these two invasives did not have a big impact. Maybe because the things they affect are not the important predictors in the model
#I also don't have many lakes both before and after an invasion so this may limit statistical power
#going to drop the invasive issue for now

#--------------------------------------------------------------------------------------------

#What happens when I run a model selection on the original model
library(MASS)
stepAIC(lm.zoop.fish)

#--------------------------------------------------------------------------------------------

#TRY PROP.LARGE.CLADOCERANS - abundance, not biomass (look for covariance first)

#prop.Cladoceran.biomass = cladoceran biomass / total zoop biomass
#prop.large.cladoceran = count large cladoceran / count of all clacoderan

#look at a histogram 
Prop.Clad.abundance <- gf_histogram(~prop.large.cladoceran, data = Data.scaled.filtered, xlab = "Porportion Large Cladocerans")
Prop.Clad.abundance
#seems okay, we range from very low (near zero) to very high (near 1)

#check for collinearity with other variables
pairs.panels(dplyr::select(Data.scaled.filtered,
                    prop.Cladoceran.biomass,
                    prop.large.cladoceran,
                    zoop.mean.length,
                    zoop.Shannon.DI,
                    gdd_wtr_5c,
                    mean.summer.secchi.meters,
                    lakesize),
             gap = 0.1,
             ellipses = FALSE
)
#the two cladoceran proportions do not have a linear correlation
#may be slight correlation with prop.large.cladoceran and zoop diversity and zoop mean length

#try adding the new proportion to original model
lm.test18 <- lmer(WAE.CPUE.inc ~
                    prop.Cladoceran.biomass +
                    prop.large.cladoceran +
                    zoop.mean.length +
                    zoop.Shannon.DI +
                    GDD1000days +
                    mean.summer.secchi.meters +
                    LS10000acres +
                    (1 | DOW),
                  data = Data.scaled.filtered
) 


summary(lm.test18)
r.squaredGLMM(lm.test18)

#marginal R2 = 0.35
#conditional R2 = 0.76

#interesting... now shannon diversity no longer significant and lake area is again
#probably because there is generally less zoop diversity with greater proportion of large cladocerans

#then replacing the other cladoceran proportion
lm.test19 <- lmer(WAE.CPUE.inc ~
                    prop.large.cladoceran +
                    zoop.mean.length +
                    zoop.Shannon.DI +
                    GDD1000days +
                    mean.summer.secchi.meters +
                    LS10000acres +
                    (1 | DOW),
                  data = Data.scaled.filtered
) 


summary(lm.test19)
r.squaredGLMM(lm.test19)
#marginal R2 = 0.35
#conditional R2 = 0.76

#really no change here

#try replacing shannon diversity with prop.large.clad
lm.test21 <- lmer(WAE.CPUE.inc ~
                    prop.Cladoceran.biomass +
                    prop.large.cladoceran +
                    zoop.mean.length +
                    GDD1000days +
                    mean.summer.secchi.meters +
                    LS10000acres +
                    (1 | DOW),
                  data = Data.scaled.filtered
) 


summary(lm.test21)
r.squaredGLMM(lm.test21)

#marginal R2 = 0.33
#conditional R2 = 0.77

#lake area got more significant, prop large clad p = 0.09 so marignally significant. R2 got lower. so shannon diversity is better indicator

#try replacing zoop length with prop.large.clad
lm.test22 <- lmer(WAE.CPUE.inc ~
                    prop.Cladoceran.biomass +
                    prop.large.cladoceran +
                    zoop.Shannon.DI +
                    GDD1000days +
                    mean.summer.secchi.meters +
                    LS10000acres +
                    (1 | DOW),
                  data = Data.scaled.filtered
) 


summary(lm.test22)
r.squaredGLMM(lm.test22)

#marginal R2 = 0.35
#conditional R2 = 0.76

#only significant thing here is lake area...

#try replacing shannon diversity AND zoop mean length with prop.large.clad
lm.test23 <- lmer(WAE.CPUE.inc ~
                    prop.Cladoceran.biomass +
                    prop.large.cladoceran +
                    GDD1000days +
                    mean.summer.secchi.meters +
                    LS10000acres +
                    (1 | DOW),
                  data = Data.scaled.filtered
) 


summary(lm.test23)
r.squaredGLMM(lm.test23)

#marginal R2 = 0.33
#conditional R2 = 0.77

#prop.large.cladoceran still only getting a p = 0.09
#lake area very significant
#intercept significant
#R2 dropped again

#try interaction between the two cladoceran variables
lm.test24 <- lmer(WAE.CPUE.inc ~
                    prop.Cladoceran.biomass +
                    prop.large.cladoceran +
                    GDD1000days +
                    mean.summer.secchi.meters +
                    LS10000acres +
                    prop.Cladoceran.biomass:prop.large.cladoceran +
                    (1 | DOW),
                  data = Data.scaled.filtered
) 


summary(lm.test24)
r.squaredGLMM(lm.test24)

#marginal R2 = 0.33
#conditional R2 = 0.77

#not a significant interaction

#try interaction with prop.large.clad and zoop diversity
lm.test25 <- lmer(WAE.CPUE.inc ~
                    prop.Cladoceran.biomass +
                    zoop.Shannon.DI +
                    GDD1000days +
                    mean.summer.secchi.meters +
                    LS10000acres +
                    zoop.Shannon.DI:prop.large.cladoceran +
                    (1 | DOW),
                  data = Data.scaled.filtered
) 


summary(lm.test25)
r.squaredGLMM(lm.test25)

#marginal R2 = 0.35
#conditional R2 = 0.76

#only significant thing is lake area

#okay so what all this with the proportion of large cladocercans told me is that it is related to shannon diversity but a worse predictor than shannon diversity
#will not include this in model


#---------------------------------------------------------------------------------------------

#RICHNESS VS. EVENNESS PORTIONS OF SHANNON DIVERSITY

#---------------------------------------------------------------------------------------------

#TRY MEAN LENGTH AND DIVERSITY OF JUST TAXA THAT SHOW UP IN CORES

#---------------------------------------------------------------------------------------------

#VISUALIZE THE DATA

#try effect plots - see effects package in John's book chapter 16


#play with added-variable plots / partial regression plots
#John Fiebierg's code:
# library(car)
# avPlots(lmxall.y)
