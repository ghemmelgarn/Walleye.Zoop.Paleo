#this script runs a contemporary fish-zoop gllvm with different covariates as suggested by my master's thesis committee
#this is the version we will use for peer-reviewed publication

#COMPUTER SETUP-----------------------------------------

#libraries
library(gllvm)
library(tidyverse)
library(ggplot2)
library(GGally) #for ggpairs function
library(beepr)
library(corrplot)
library(gclus)
library(gridExtra)
library(ggrepel)

#read in data
data <- read.csv("Data/Input/Contemporary_Dataset_2026_09_30.csv")

#set seed to keep results consistent
set.seed(13453)
#use all computer cores - except 1 to do other things
TMB::openmp(parallel::detectCores()-1, DLL = "gllvm", autopar = TRUE)

#DATA PREP-------------------------------------

#includes trout lakes, and all copepods are dropped, as done in masters thesis model

#new list of covariates:
  #CHANGED or NEW:
    #temperature (base 5 gdd) 1999-2024 lake mean and lake-year deviation from mean
    #precipitation (mm) 1999-2024 lake mean and lake-year deviation from mean
    #secchi (MPCA jul-sept mean) 1999-2024 lake mean and lake-year deviation from mean
    #categorical walleye stocking yes/no in the last 5 years
    #mean depth instead of littoral proportion
    #SWF NOT a covariate anymore - now part of y matrix
  #SAME AS BEFORE:
    #zebra mussel presence/absence
    #lake surface area
    #lake max depth
    #CDOM lake average all availabile years
  #list of covariate column names:
    #gdd.lake.mean, gdd.year.dev, precip.lake.mean.mm, precip.year.dev.mm, secchi.lake.mean.meters, secchi.year.dev.meters, CDOM.lake.avg, area_ha, depth.max.m, depth.mean.m, wae.stock.5yr.yn, ZebraMussel.yn

#taxa in matrix are all fish, all cladoceran zooplankton, spiny water fleas (THE NEW PART) - no copepods, and rare taxa dropped as defined below


#filter data
data.filter <- data %>% 
  #removes Hill south that does not have zoop data :(
  filter(!is.na(total_zoop_biomass)) %>% 
  #no NA values for selected covariates
  filter(!is.na(secchi.year.dev.meters) & 
           !is.na(gdd.year.dev) & 
           !is.na(precip.year.dev.mm) & 
           !is.na(CDOM.lake.avg) &
           !is.na(area_ha) & 
           !is.na(depth.max.m) & 
           !is.na(depth.mean.m) &
           !is.na(wae.stock.5yr.yn) &
           !is.na(ZebraMussel.yn)) %>% 
  #remove walleye yoy column
  select(-WAE.YOY.CPUE)

#end up with 149 lake-years instead of 143 as before
  #reason: lost 4 years (the 2022 years that don't have precip data) but gained 10 years that didn't have a photic proportion but do have mean depth


#Save this dataframe for later reference
#write.csv(data.filter, file = "Data/Input/GLLVM_Complete_Dataset_Pub_Rev.csv", row.names = FALSE)


#make covariate dataframe
x <- data.filter %>% 
  select(gdd.lake.mean, gdd.year.dev, precip.lake.mean.mm, precip.year.dev.mm, 
         secchi.lake.mean.meters, secchi.year.dev.meters, CDOM.lake.avg, area_ha, 
         depth.max.m, depth.mean.m, wae.stock.5yr.yn, ZebraMussel.yn) %>% 
  #set categorical variables as factors
  mutate(wae.stock.5yr.yn = as.factor(wae.stock.5yr.yn),
         ZebraMussel.yn = as.factor(ZebraMussel.yn)) %>% 
  #rename everything shorter
  rename(secchi.mean = secchi.lake.mean.meters,
         secchi.dev = secchi.year.dev.meters,
         gdd.mean = gdd.lake.mean,
         gdd.dev = gdd.year.dev,
         precip.mean = precip.lake.mean.mm,
         precip.dev = precip.year.dev.mm,
         cdom = CDOM.lake.avg,
         area = area_ha,
         depth.max = depth.max.m,
         depth.mean = depth.mean.m,
         wae.stock = wae.stock.5yr.yn,
         zm = ZebraMussel.yn
         )


#standardize all quantitative variables with scale function
x_scale <- x %>% 
  mutate(secchi.mean = as.numeric(scale(secchi.mean)),
         secchi.dev = as.numeric(scale(secchi.dev)),
         gdd.mean = as.numeric(scale(gdd.mean)),
         gdd.dev = as.numeric(scale(gdd.dev)),
         precip.mean = as.numeric(scale(precip.mean)),
         precip.dev = as.numeric(scale(precip.dev)),
         cdom = as.numeric(scale(cdom)),
         log.area = as.numeric(scale(log(area))),
         area = as.numeric(scale(area)),
         depth.max = as.numeric(scale(depth.max)),
         depth.mean = as.numeric(scale(depth.mean)))


#list of covariatse to copy later: secchi.mean, secchi.dev, gdd.mean, gdd.dev, precip.mean, precip.dev, cdom, log.area, depth.max, depth.mean, wae.stock, zm

#set x_scale as a dataframe for rownames later
x_scale <- as.data.frame(x_scale)



#make a species abundance dataframe - raw abundance NOT relative abundance
#to be included, a taxa group must be present in at least 95% of samples (lake-years) AND be present in at least 3 different lakes

#INVESTIGATING RARE SPECIES
#First question: are there any species only present in one lake?
#regroup the daphnia how we discussed in committee meeting
data.daphnia <- data.filter %>% 
  mutate(Daphnia.small.rare = rowSums(across(c(Daphnia.rosea, Daphnia.ambigua, Daphnia.sp.)))) %>% 
  select(-Daphnia.rosea, -Daphnia.ambigua, -Daphnia.sp.) %>% 
  relocate(Daphnia.small.rare, .after = Daphnia.retrocurva) %>% 
  relocate(SpinyWaterflea.density, .after = YEP.CPUE) #put the spiny water fleas between the fish and the zoops

#get the max value for each taxa in each lake (will be 0 if never present)
lake.spp <- data.daphnia %>% 
  group_by(lake_name) %>%
  summarize(across(BIB.CPUE:nauplii, max),
            .groups = 'drop')
#for each taxa, count the number of lakes where it has a value greater than 0 in at least one year
spp.lake.count <- colSums(lake.spp > 0)
spp.lake.count
#make a list of species present in two or fewer lakes
spp.drop.lake <- names(spp.lake.count[spp.lake.count < 3])
spp.drop.lake

#calculate taxa present in less than 95% of lake-year samples
#isolate taxa
spp <- data.daphnia %>% 
  select(BIB.CPUE:nauplii)
#proportion of zeroes in species data
spp_prop_0 <- colSums(spp == 0, na.rm = TRUE)/nrow(spp)
spp_prop_0
#isolate proportions of zeroes over 95%
spp_prop_0_0.95 <- spp_prop_0[spp_prop_0 > 0.95]
#make this a vector of names
spp_names_rare <- names(spp_prop_0_0.95)

#combine the list of names for the two reasons to be dropped, only keep one if repeated in both lists
spp.drop <- unique(c(spp.drop.lake, spp_names_rare))
spp.drop

#remove columns for the species to drop AND remove all the copepods
y.matrix <- spp %>% 
  select(-all_of(spp.drop)) %>% 
  select(-cyclopoids, -nauplii, -calanoids, -copepodites)
#what's left?
names(y.matrix)

# #look at magnitude of fish vs. SWp vs. zoop data
# mag.plot.data <- y.matrix %>%
#   pivot_longer(cols = everything(), names_to = "species", values_to = "abundance") %>%
#   mutate(group = ifelse(str_ends(species, "CPUE"), "fish", "zoop"))
# mag.plot.raw <- ggplot(data = mag.plot.data, aes(x = species, y = abundance, color = group))+
#   geom_boxplot()+
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
# mag.plot.raw
# #plot again but limit y axis to not include outliers
# mag.plot.raw.zoom <- ggplot(data = mag.plot.data, aes(x = species, y = abundance, color = group))+
#   geom_boxplot()+
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
#   scale_y_continuous(limits = c(0,5))
# mag.plot.raw.zoom
# #actually looks okay, not transforming at all, even with SWF


#set N as number of lake-years and N.taxa as number of taxa in response matrix
N <- nrow(x)
N.taxa <- ncol(y.matrix)


#set rownames to be the same in both matrices
rownames(x_scale) <- paste0(data.filter$lake_name, data.filter$Year)
rownames(y.matrix) <- paste0(data.filter$lake_name, data.filter$Year)


#create study design matrix
studyDesignData <- data.frame(lake = as.factor(data.filter$lake_name),
                                    year = as.factor(data.filter$Year))
rownames(studyDesignData) <- paste0(data.filter$lake_name, data.filter$Year)


#save the x matrix with only the desired coefficients for use later
x_save <- x_scale %>%
  mutate(wae.stock = ifelse(wae.stock == "yes", 1, 0),
         zm = ifelse(zm == "yes", 1, 0),
         lake_name = data.filter$lake_name,
         year = data.filter$Year)
#write.csv(x_save, "Data/Input/gllvm_x_matrix_standardized_Pub_Rev.csv", row.names = FALSE)

#save a version that is NOT standardized
x_save_raw <- x %>% 
  mutate(wae.stock = ifelse(wae.stock == "yes", 1, 0),
         zm = ifelse(zm == "yes", 1, 0),
         lake_name = data.filter$lake_name,
         year = data.filter$Year)
#write.csv(x_save_raw, "Data/Input/gllvm_x_matrix_raw_Pub_Rev.csv", row.names = FALSE)

# #run a quick ggpairs on all the predictors
# ggpairs(x)
# ggpairs(x_scale)





#MODELS--------------------------------------------------------------------------
#I AM GOING TO HAVE A "model_name" FOR ALL MY DIAGNOSTIC THINGS SO I CAN JUST ADJUST THE NAME ONCE INSTEAD OF A MILLION TIMES


#start simple, no predictors, have random lake effect, NO quadratic structure, tweedie distribution letting R determine power, no latent variables, no random structure of the latent variables
model_rev_1 <- gllvm(y = y.matrix, studyDesign = studyDesignData,
                                                 row.eff = ~(1|lake), num.lv = 0, family = "tweedie", Power = NULL,
                                                 trace = TRUE)
beep()
#save the model
#saveRDS(model_rev_1, file = "Models/model_rev_1.rds")
#set current name for exploration
model_name <- model_rev_1
#notes
  #diagnostics looked good, a little funky in QQ and slight fan in residuals vs linear predictors
  #can't examine much without lvs or predictors
  #AICc = 15033.67



#add in one latent variable
model_rev_2 <- gllvm(y = y.matrix, studyDesign = studyDesignData,
                     row.eff = ~(1|lake), num.lv = 1, family = "tweedie", Power = NULL,
                     trace = TRUE)
beep()
#save the model
#saveRDS(model_rev_2, file = "Models/model_rev_2.rds")
#set current name for exploration
model_name <- model_rev_2
#notes
  #diagnostics looked great
  #residual corrplot collapsed - need more LV
  #AICc = 14041.12



#try 2 latent variables, at this point have to make it try multiple times to converge but just doing 5
model_rev_3 <- gllvm(y = y.matrix, studyDesign = studyDesignData,
                     row.eff = ~(1|lake), num.lv = 2, family = "tweedie", Power = NULL,
                     control.start = list(n.init = 5, jitter.var = 0.1),
                     trace = TRUE)
beep()
#save the model
#saveRDS(model_rev_3, file = "Models/model_rev_3.rds")
#set current name for exploration
model_name <- model_rev_3
#notes
  #diagnostics look great
  #residual correlations are less collapsed but still chunky
  #AICc = 13543.32



#try 3 latent variables
model_rev_4 <- gllvm(y = y.matrix, studyDesign = studyDesignData,
                     row.eff = ~(1|lake), num.lv = 3, family = "tweedie", Power = NULL,
                     control.start = list(n.init = 5, jitter.var = 0.1),
                     trace = TRUE)
beep()
#save the model
#saveRDS(model_rev_4, file = "Models/model_rev_4.rds")
#set current name for exploration
model_name <- model_rev_4
#notes
  #got a convergence error but not going to worry about it right now ("In se.gllvm(out) :27 parameter(s) have negative variance estimates (lambda x26, sigmaLV x1). The model likely has not converged - consider re-fitting.")
  #diagnostics look great
  #residual correlations look great
  #AICc = 13354.28



#try 4 latent variables
model_rev_5 <- gllvm(y = y.matrix, studyDesign = studyDesignData,
                     row.eff = ~(1|lake), num.lv = 4, family = "tweedie", Power = NULL,
                     control.start = list(n.init = 5, jitter.var = 0.1),
                     trace = TRUE)
beep()
#save the model
#saveRDS(model_rev_5, file = "Models/model_rev_5.rds")
#set current name for exploration
model_name <- model_rev_5
#notes
  #no convergence error here
  #diagnostics are okay but have some shooting stars going on
  #residual corrplot looks good
  #AICc = 13025.7

#ALRIGHT, THE AICc IS STILL DROPPING BUT THIS IS NOT WHY WE ARE HERE, LETS GET SOME PREDICTORS ROLLING!!

#add predictors but 0 LV
model_rev_6 <- gllvm(y = y.matrix, X = x_scale, studyDesign = studyDesignData,
                     formula = ~ secchi.mean + secchi.dev + gdd.mean + gdd.dev + precip.mean + precip.dev + cdom + log.area + depth.max + depth.mean + wae.stock + zm,
                     row.eff = ~(1|lake), num.lv = 0, family = "tweedie", Power = NULL,
                     trace = TRUE)
beep()
#save the model
#saveRDS(model_rev_6, file = "Models/model_rev_6.rds")
#set current name for exploration
model_name <- model_rev_6
#notes
  #so many shooting stars, QQ not great, the walleye (species 21) are being problematic
  #AICc = 12576.41
  #so this model is way better with predictors but we don't have any LVs to really look at yet

#simple predictors and 1 LV
model_rev_7 <- gllvm(y = y.matrix, X = x_scale, studyDesign = studyDesignData,
                     formula = ~ secchi.mean + secchi.dev + gdd.mean + gdd.dev + precip.mean + precip.dev + cdom + log.area + depth.max + depth.mean + wae.stock + zm,
                     row.eff = ~(1|lake), num.lv = 1, family = "tweedie", Power = NULL,
                     trace = TRUE)
beep()
#save the model
#saveRDS(model_rev_7, file = "Models/model_rev_7.rds")
#set current name for exploration
model_name <- model_rev_7
#notes
  #diagnostics still have a lot of shooting stars and QQ is bendy, walleye are a problem especially in residuals vs. row
  #corrplot collapsed, need more LV
  #AICc = 12171.34



#simple predictors and 2 LV, back to multiple inits
model_rev_8 <- gllvm(y = y.matrix, X = x_scale, studyDesign = studyDesignData,
                     formula = ~ secchi.mean + secchi.dev + gdd.mean + gdd.dev + precip.mean + precip.dev + cdom + log.area + depth.max + depth.mean + wae.stock + zm,
                     row.eff = ~(1|lake), num.lv = 2, family = "tweedie", Power = NULL,
                     control.start = list(n.init = 5, jitter.var = 0.1),
                     trace = TRUE)
beep()
#save the model
#saveRDS(model_rev_8, file = "Models/model_rev_8.rds")
#set current name for exploration
model_name <- model_rev_8
#notes
  #no convergence warnings
  #diagnostic plots are awful
  #shooting stars getting worse, walleye still a problem - this is telling me I need to go back to the quadratic structure and multivariate predictors
  #QQ off the line a lot
  #residual corrplot looks okay but walleye and yellow perch predicted very differently (the walleye are not predicted well I can tell by diagnostic plots)
  #AICc = 12023.74



#DIAGNOSTICS ON A MODEL-------------------------------------

#set the model you want to explore to "model_name"

#diagnostics
par(mfrow = c(3,2))
plot(model_name) 
par(mfrow = c(1,1))
#looks great
#check power
model_name$Power
#get residual correlations
Theta <- getResidualCor(model_name)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")

#summaries
summary(model_name)
VP(model_name)
AICc(model_name)


