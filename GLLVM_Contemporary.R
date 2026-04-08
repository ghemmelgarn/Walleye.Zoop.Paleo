#this script runs GLLVM modes on the contemporary data

#libraries
library(gllvm)
library(tidyverse)
library(ggplot2)
library(GGally) #for ggpairs function
library(beepr)

#read in data
data <- read.csv("Data/Input/Contemporary_Dataset_2026_04_06.csv")

#set seed to keep results consistent
set.seed(13453)
#use all computer cores - except 1 to do other things
TMB::openmp(parallel::detectCores()-1, DLL = "gllvm", autopar = TRUE)


#COVARIATES TO USE
# secchi.meters.MPCA.Jul.to.Sept, gdd.year.5c, precip_5yr_avg_mm, area_ha, CDOM.lake.avg, depth.max.m, photic_prop_secchi.meters.MPCA.Jul.to.Sept, 
# SpinyWaterflea.yn, ZebraMussel.yn

#DECIDE ABOUT INCLUDING LAKE CONNECTIVITY OR NOT

#filter data
data.filter <- data %>% 
  #removes Hill south that does not have zoop data :(
  filter(!is.na(total_zoop_biomass)) %>% 
  #no NA values for selected covariates
  filter(!is.na(secchi.meters.MPCA.Jul.to.Sept) & 
           !is.na(gdd.year.5c) & 
           !is.na(precip_5yr_avg_mm) & 
           !is.na(CDOM.lake.avg) &
           !is.na(area_ha) & 
           !is.na(depth.max.m) & 
           !is.na(photic_prop_secchi.meters.MPCA.Jul.to.Sept) &
           !is.na(SpinyWaterflea.yn) &
           !is.na(ZebraMussel.yn)) %>% 
  #remove trout lakes  
  filter(!(BKT.CPUE > 0 | LAT.CPUE > 0 | RBT.CPUE > 0)) %>%  
  #remove walleye yoy column
  select(-WAE.YOY.CPUE)


x <- data.filter %>% 
  select(lake_name, Year, secchi.meters.MPCA.Jul.to.Sept, gdd.year.5c, precip_5yr_avg_mm, CDOM.lake.avg, area_ha, depth.max.m, photic_prop_secchi.meters.MPCA.Jul.to.Sept, 
         SpinyWaterflea.yn, ZebraMussel.yn)
  

#make a species abundance dataframe
#calculate relative abundance within fish and within zoops, then cbind them together

#isolate fish data
fish <- data.filter %>% 
  select(BIB.CPUE:YEP.CPUE) %>%
  select(where(~ sum(.x, na.rm = TRUE) != 0)) #remove columns (species) with no observations for any lake-year
#relative abundance
fish_rel_abun <- fish / rowSums(fish)

#isolate zoop data
zoop <- data.filter %>% 
  select(Acroperus.harpae:nauplii) %>% 
  select(where(~ sum(.x, na.rm = TRUE) != 0)) #remove columns (species) with no observations for any lake-year
#relative abundance
zoop_rel_abun <- zoop / rowSums(zoop)

#isolate zoop summary metrics (may or may not include them)
zoop_sum <- data.filter %>% 
  select(total_zoop_biomass:clad_ratio)
#check for correlations with these metrics
ggpairs(zoop_sum)
#I should choose either mean length of all zoops or cladocerans
#going with cladocerans because more preserved in sediments
zoop_sum_filter <- zoop_sum %>% 
  select(-mean_length_all_zoop)

#create the fish and zoop abundance matrix together
y <- cbind(fish_rel_abun, zoop_rel_abun)

#create a version with the zoop summary stats too
y_zoop_sum <- cbind(y, zoop_sum_filter)

#model with no covariates
m1 <- gllvm(y = y, num.lv = 2, family = "tweedie", n.init = 3)
#n.init makes it start at 3 different starting to places to help with convergence
beep()
#tweedie seems like the right distribution to use based on the paper and my notes and the internet

#model performance plots
plot(m1)
gllvm::ordiplot(m1)
summary(m1)

#i have notes from the GLLVM workshop that for compositional data I need to use an offset
#but with compositional data separated by fish and zoops it is no longer strictly compositional...

#maybe the convergence issue is because some species are too rare...
prop_0 <- colSums(y == 0, na.rm = TRUE)/132
#what if we say a species needs to occur in at least 5% of the lake-years?
prop_0_0.95 <- prop_0[prop_0 > 0.95]
names(prop_0_0.95)
#what about 10?
prop_0_0.90 <- prop_0[prop_0 > 0.90]

#make a version of y that does not have rare species (occurs in at least 5% of lake-years)
y_no_rare <- y %>% 
  select(-BIB.CPUE, -CCF.CPUE, -FRD.CPUE, -GIS.CPUE, -GSF.CPUE, - LKS.CPUE, -LNS.CPUE,
         -MOE.CPUE, -OSS.CPUE, -SNG.CPUE, -SPO.CPUE, -TPM.CPUE, -WHB.CPUE, -WHC.CPUE,
         -Acroperus.harpae, -Alonella.sp., -Daphnia.ambigua, -Daphnia.sp., -Harpacticoida,
         -Latona.setifera, -Macrothricidae, -Pleuroxus.sp., -Polyphemus.pediculus, -Scapholeberis.sp., 
         -Simocephalus.sp.)

#model with no covariates and no rare species
#model with no covariates
m2 <- gllvm(y = y_no_rare, num.lv = 2, family = "tweedie", Power = NULL,
            control.start = (n.init = 3))
#power argument tells R to find the tweedie coefficient that best fits my data
beep()
#model performance plots
plot(m2)
summary(m2)
gllvm::ordiplot(m2)
#extract tweedie power that the model estimated:
m2$params$Power
m2$call

#just to see the difference what happens if we use a normal distribution
m3 <- gllvm(y = y_no_rare, num.lv = 2, family = "gaussian", n.init = 3)
beep()
#model performance plots
plot(m3)
summary(m3)
gllvm::ordiplot(m3)
#okay confirmation that the first tweedie wasn't so bad... this looks horrible

#try the first model with 1 and 3 latent variables - don't worry about # latent variables yet just get your model structure
# m1.1 <- gllvm(y = y, num.lv = 1, family = "tweedie", n.init = 3)
# m1.3 <- gllvm(y = y, num.lv = 3, family = "tweedie", n.init = 3)
# beep()
# summary(m1.1)
# summary(m1.3)
# plot(m1.1)
# plot(m1.3)
# gllvm::ordiplot(m1.1)
# gllvm::ordiplot(m1.3)


#okay we need to start accounting for study design: random lake effect and random year effect

#make a model with random intercept for each lake. This assumes that random effect estimates are the same for all species
#correlations only within random effects, not between random effects
m4 <- gllvm(y = y, x = x, num.lv = 2, family = "tweedie", n.init = 3, row.eff = ~(1|lake_name))
beep()




#DON't FORGET TO SCALE PREDICTORS
