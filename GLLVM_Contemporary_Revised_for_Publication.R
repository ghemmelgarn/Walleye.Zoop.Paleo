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
model_rev_4 <- readRDS(file = "Models/model_rev_4.rds")
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
model_rev_7 <- readRDS(file = "Models/model_rev_7.rds")
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


#simple predictors and 3 LV
model_rev_9 <- gllvm(y = y.matrix, X = x_scale, studyDesign = studyDesignData,
                     formula = ~ secchi.mean + secchi.dev + gdd.mean + gdd.dev + precip.mean + precip.dev + cdom + log.area + depth.max + depth.mean + wae.stock + zm,
                     row.eff = ~(1|lake), num.lv = 3, family = "tweedie", Power = NULL,
                     control.start = list(n.init = 5, jitter.var = 0.1),
                     trace = TRUE)
beep()
#save the model
#saveRDS(model_rev_9, file = "Models/model_rev_9.rds")
#set current name for exploration
model_name <- model_rev_9
#notes
  #the diagnostics are still bad, shooting star continues, and walleye in residuals vs row are an abomination
  #residual corrplot looks fine
  #in VP, the LVs are explaining a very small amount of variance, mostly the predictors
  #AICc = 11870.4


#switch to concurrent ordination (again, not where I think I'll end up but let's be complete), 1 concurrent LV
model_rev_10 <- gllvm(y = y.matrix, X = x_scale, studyDesign = studyDesignData,
                     lv.formula = ~ secchi.mean + secchi.dev + gdd.mean + gdd.dev + precip.mean + precip.dev + cdom + log.area + depth.max + depth.mean + wae.stock + zm,
                     row.eff = ~(1|lake), num.lv.c =  1, family = "tweedie", Power = NULL,
                     control.start = list(n.init = 5, jitter.var = 0.1),
                     trace = TRUE)
beep()
#save the model
#saveRDS(model_rev_10, file = "Models/model_rev_10.rds")
#set current name for exploration
model_name <- model_rev_10
#notes
  #diagnostic plots look great
  #residual corrplot collapsed
  #can't get Env corrplot with this structure
  #AIcc = 13824.76



#concurrent ordination, 2 LVs
model_rev_11 <- gllvm(y = y.matrix, X = x_scale, studyDesign = studyDesignData,
                      lv.formula = ~ secchi.mean + secchi.dev + gdd.mean + gdd.dev + precip.mean + precip.dev + cdom + log.area + depth.max + depth.mean + wae.stock + zm,
                      row.eff = ~(1|lake), num.lv.c =  2, family = "tweedie", Power = NULL,
                      control.start = list(n.init = 5, jitter.var = 0.1),
                      trace = TRUE)
beep()
#save the model
#saveRDS(model_rev_11, file = "Models/model_rev_11.rds")
model_rev_11 <- readRDS(file = "Models/model_rev_11.rds")
#set current name for exploration
model_name <- model_rev_11
#notes
  #diagnostics pretty good, walleye a little weird in scale-location but that's it
  #residual corr looks good and walleye makes more sense, matches yellow perch
  #AICc = 13213.7



#concurrent ordination, 3 LVs
model_rev_12 <- gllvm(y = y.matrix, X = x_scale, studyDesign = studyDesignData,
                      lv.formula = ~ secchi.mean + secchi.dev + gdd.mean + gdd.dev + precip.mean + precip.dev + cdom + log.area + depth.max + depth.mean + wae.stock + zm,
                      row.eff = ~(1|lake), num.lv.c =  3, family = "tweedie", Power = NULL,
                      control.start = list(n.init = 5, jitter.var = 0.1),
                      trace = TRUE)
beep()
#save the model
#saveRDS(model_rev_12, file = "Models/model_rev_12.rds")
#set current name for exploration
model_name <- model_rev_12
#notes
  #QQ great but the others have some shooting stars
  #residual corrplot looks good
  #AICc = 12794.1

#ADDING IN THE RandomB covariance structure to stabilize model now!! - forgot that before
#sticking with LV (not P) for RandomB because that's what worked well in the past for this data and makes ecological sense - shared random effect variance on an LV (environmental gradient) rather than each predictor separately
#needed it to get models to converge...

#START BY RERUNNING THIS WITH RANDOMB
#lets switch into quadratic mode, concurrent ordination, 1 LV
model_rev_13 <- gllvm(y = y.matrix, X = x_scale, studyDesign = studyDesignData,
                      lv.formula = ~ secchi.mean + secchi.dev + gdd.mean + gdd.dev + precip.mean + precip.dev + cdom + log.area + depth.max + depth.mean + wae.stock + zm,
                      row.eff = ~(1|lake), 
                      num.lv.c =  1, family = "tweedie", Power = NULL,
                      randomB = "LV", quadratic = "LV",
                      control.start = list(n.init = 5, jitter.var = 0.1),
                      trace = TRUE)
beep()
#save the model
#saveRDS(model_rev_13, file = "Models/model_rev_13.rds")
model_rev_13 <- readRDS(file = "Models/model_rev_13.rds")
#set current name for exploration
model_name <- model_rev_13
#notes
  #for some reason it won't let me use the plot function on this model?? - so I can't see performance plots
  #residual correlation somewhat collapsed into zeroes and ones but not completely
  #env correlation plot looks good
  #AICc = 13812.26
  #no warnings when this ran



#quadratic response, concurrent ordination, 2 LVs
model_rev_14 <- gllvm(y = y.matrix, X = x_scale, studyDesign = studyDesignData,
                      lv.formula = ~ secchi.mean + secchi.dev + gdd.mean + gdd.dev + precip.mean + precip.dev + cdom + log.area + depth.max + depth.mean + wae.stock + zm,
                      row.eff = ~(1|lake), 
                      num.lv.c =  2, family = "tweedie", Power = NULL,
                      randomB = "LV", quadratic = "LV",
                      control.start = list(n.init = 5, jitter.var = 0.05),
                      trace = TRUE)
beep()
#save the model
#saveRDS(model_rev_14, file = "Models/model_rev_14.rds")
model_rev_14 <- readRDS(file = "Models/model_rev_14.rds")
#set current name for exploration
model_name <- model_rev_14
#notes
  #performance looks awesome
  #residual correlations look good (not collapsed)
  #env correlations almost all positive
  #AICc = 13266.21


#quadratic response, concurrent ordination, 3 LVs
model_rev_15 <- gllvm(y = y.matrix, X = x_scale, studyDesign = studyDesignData,
                      lv.formula = ~ secchi.mean + secchi.dev + gdd.mean + gdd.dev + precip.mean + precip.dev + cdom + log.area + depth.max + depth.mean + wae.stock + zm,
                      row.eff = ~(1|lake), 
                      num.lv.c =  3, family = "tweedie", Power = NULL,
                      randomB = "LV", quadratic = "LV",
                      control.start = list(n.init = 5, jitter.var = 0.1),
                      trace = TRUE)
beep()
#save the model
#saveRDS(model_rev_15, file = "Models/model_rev_15.rds")
#set current name for exploration
model_name <- model_rev_15
#notes
  #performance plots look pretty good, very slight shooting stars
  #residual correlation plot looks good
  #env correlation plot looks good
  #AICc = 12904.51

#quadratic response, concurrent ordination, 4 LVs
model_rev_16 <- gllvm(y = y.matrix, X = x_scale, studyDesign = studyDesignData,
                      lv.formula = ~ secchi.mean + secchi.dev + gdd.mean + gdd.dev + precip.mean + precip.dev + cdom + log.area + depth.max + depth.mean + wae.stock + zm,
                      row.eff = ~(1|lake), 
                      num.lv.c =  4, family = "tweedie", Power = NULL,
                      randomB = "LV", quadratic = "LV",
                      control.start = list(n.init = 5, jitter.var = 0.1),
                      trace = TRUE)
beep()
#save the model
#saveRDS(model_rev_16, file = "Models/model_rev_16.rds")
#set current name for exploration
model_name <- model_rev_16
#notes
  #diagnostic plots look pretty darn good
  #Residual correlation plot looks good
  #Env correlation plot looks good
  #AICc = 12689.58


#NOT A QUADRATIC RESPONSE, hybrid ordination, try 1 to 3 LVs for each type in all combos

# 1. Create an empty dataframe to hold our scoreboard
not_quad_lv_results <- data.frame(num.RR = integer(), 
                          num.lv = integer(),
                          AICc=numeric())

# 2. Set up the grid (testing 1 to 3 for both parameters)
for (rr in 1:3) {
  for (lv in 1:3) {
    
    # Print a message so you know it hasn't frozen
    cat("Fitting model with num.RR =", rr, "and num.lv =", lv, "...\n")
    
    # 3. Use tryCatch to skip hard crashes
    model_fit <- tryCatch({
      
      gllvm(y = y.matrix, X = x_scale, studyDesign = studyDesignData,
            lv.formula = ~ secchi.mean + secchi.dev + gdd.mean + gdd.dev + precip.mean + precip.dev + cdom + log.area + depth.max + depth.mean + wae.stock + zm,
            row.eff = ~(1|lake), 
            num.RR =  rr, num.lv = lv, family = "tweedie", Power = NULL,
            randomB = "LV",
            control.start = list(n.init = 10, jitter.var = 0.1),
            trace = TRUE)
      
    }, error = function(e) {
      cat("  -> Model crashed during fitting. Skipping...\n")
      return(NULL) 
    })
    
    # 4. Safely extract the AICc and save it
    if (!is.null(model_fit)) {
      #save model
      saveRDS(model_fit, file = paste0("Models/model_rev_17_", rr, "RR_", lv, "LV.rds"))
      
      #extract AICc
      current_aicc <- tryCatch(AICc(model_fit), error = function(e) numeric(0))
      
      # ONLY add to the dataframe if current_aicc actually contains a number
      if (length(current_aicc) > 0 && !is.na(current_aicc)) {
        not_quad_lv_results <- rbind(not_quad_lv_results, 
                             data.frame(num.RR = rr, num.lv = lv, AICc = current_aicc))
        cat("  -> Success! AICc:", current_aicc, "\n")
      } else {
        cat("  -> Model fit, but AICc could not be calculated (singular fit). Skipping...\n")
      }
    }
  }
}

# 5. Sort the scoreboard to put the best (lowest) AICc at the top!
not_quad_lv_results <- not_quad_lv_results[order(not_quad_lv_results$AICc), ]
print("--- GRID SEARCH COMPLETE ---")
print(not_quad_lv_results)

#best model without quadratic is 3RR and 3LV, with an AICc of 12437.46
#Did not get to the point of AIC increasing again, so take this with caution, but is a lot better than next best model with AICc = 12557.90
#lets investigate the performance of this model:
model_rev_17_3RR_3LV <- readRDS(file = "Models/model_rev_17_3RR_3LV.rds")
model_name <- model_rev_17_3RR_3LV
#notes
  #performance plots not good, qq not on line, we have shooting stars and walleye are consistently predicted low
  #both corrplots look okay - not collapsed

#test out the next best options here
model_rev_17_3RR_2LV <- readRDS(file = "Models/model_rev_17_3RR_2LV.rds")
model_name <- model_rev_17_3RR_2LV
#notes
  #okay performance plots look pretty decent - THERE IS HOPE
  #both corrplots are acceptable
  #AICc = 12557.9
  #THIS IS AN OPTION



model_rev_17_2RR_3LV <- readRDS(file = "Models/model_rev_17_2RR_3LV.rds")
model_name <- model_rev_17_2RR_3LV
#notes
  #performance plots slightly worse than the 3RR 2LV version above this, but not totally awful
  #both corrplots look okay
  #AICc = 12656.32
  #results look about the same as the previous model above too
  #but the model above is slightly better so we should go with that one if we choose this structure



#QUADRATIC RESPONSE, hybrid ordination, try 1 to 3 LVs for each type in all combos
# 1. Create an empty dataframe to hold our scoreboard
quad_lv_results <- data.frame(num.RR = integer(), 
                                  num.lv = integer(),
                                  AICc=numeric())

# 2. Set up the grid (testing 1 to 3 for both parameters)
for (rr in 1:3) {
  for (lv in 1:3) {
    
    # Print a message so you know it hasn't frozen
    cat("Fitting model with num.RR =", rr, "and num.lv =", lv, "...\n")
    
    # 3. Use tryCatch to skip hard crashes
    model_fit <- tryCatch({
      
      gllvm(y = y.matrix, X = x_scale, studyDesign = studyDesignData,
            lv.formula = ~ secchi.mean + secchi.dev + gdd.mean + gdd.dev + precip.mean + precip.dev + cdom + log.area + depth.max + depth.mean + wae.stock + zm,
            row.eff = ~(1|lake), 
            num.RR =  rr, num.lv = lv, family = "tweedie", Power = NULL,
            randomB = "LV", quadratic = "LV",
            control.start = list(n.init = 10, jitter.var = 0.1),
            trace = TRUE)
      
    }, error = function(e) {
      cat("  -> Model crashed during fitting. Skipping...\n")
      return(NULL) 
    })
    
    # 4. Safely extract the AICc and save it
    if (!is.null(model_fit)) {
      #save model
      saveRDS(model_fit, file = paste0("Models/model_rev_18_", rr, "RR_", lv, "LV.rds"))
      
      #extract AICc
      current_aicc <- tryCatch(AICc(model_fit), error = function(e) numeric(0))
      
      # ONLY add to the dataframe if current_aicc actually contains a number
      if (length(current_aicc) > 0 && !is.na(current_aicc)) {
        quad_lv_results <- rbind(quad_lv_results, 
                                     data.frame(num.RR = rr, num.lv = lv, AICc = current_aicc))
        cat("  -> Success! AICc:", current_aicc, "\n")
      } else {
        cat("  -> Model fit, but AICc could not be calculated (singular fit). Skipping...\n")
      }
    }
  }
}

# 5. Sort the scoreboard to put the best (lowest) AICc at the top!
quad_lv_results <- quad_lv_results[order(quad_lv_results$AICc), ]
print("--- GRID SEARCH COMPLETE ---")
print(quad_lv_results)
#best model here is 1 RR and 3 LV (AICc = 12668.46), but only slightly better than 2RR and 3LV (12672.37)
#I trust this more because increasing model complexity did start to make AICc go up again
#investigate these two models

model_rev_18_1RR_3LV <- readRDS(file = "Models/model_rev_18_1RR_3LV.rds")
model_name <- model_rev_18_1RR_3LV
#notes
  #performance plots bad, qq is terrible, lots of shooting stars, but walleye not predicted consistently bad...
  #residual correlations collapsed
  #env correlations okay

model_rev_18_2RR_3LV <- readRDS(file = "Models/model_rev_18_2RR_3LV.rds")
model_name <- model_rev_18_2RR_3LV
#notes
  #performance better generally but walleye don't look great...
  #residual correlations look good
  #environmental correlations almost all positive

#alright so the performance on exactly none of these is acceptable... lets try the third best quadratic option
model_rev_18_3RR_2LV <- readRDS(file = "Models/model_rev_18_3RR_2LV.rds")
model_name <- model_rev_18_3RR_2LV
#notes
  #performance is okay-ish but walleye still low
  #residual performance is okay 
  #environment is all positive = problem
  #AICc = 12703.72

#try one more
model_rev_18_3RR_1LV <- readRDS(file = "Models/model_rev_18_3RR_1LV.rds")
model_name <- model_rev_18_3RR_1LV
#notes
  #performance plots look good
  #residuals totally collapsed - this won't work

#try 2 and 2
model_rev_18_2RR_2LV <- readRDS(file = "Models/model_rev_18_2RR_2LV.rds")
model_name <- model_rev_18_2RR_2LV
#notes
  #performance plots are okay
  #almost all correlations are positive in both corrplots
  

#ALRIGHT, SO THE QUADRATIC RESPONSE IS NO LONGER HELPFUL!!


#only last thing to check is the concurrent ordination with the randomB = LV structure, with and without quadratic response

#CONCURRENT ORD, RandomB = LV, QUADRATIC, 1 to 4 lvs
#NOTE - I actually ran a few of these separately above but doing it again here to see if I get the same results and allowing more direct comparison
# 1. Create an empty dataframe to hold our scoreboard
conc_quad_results <- data.frame(num.lv.c = integer(),
                              AICc=numeric())

# 2. Set up the loop
for (lv.c in 1:4) {
    
    # Print a message so you know it hasn't frozen
    cat("Fitting model with num..v.c =", lv.c, "...\n")
    
    # 3. Use tryCatch to skip hard crashes
    model_fit <- tryCatch({
      
      gllvm(y = y.matrix, X = x_scale, studyDesign = studyDesignData,
            lv.formula = ~ secchi.mean + secchi.dev + gdd.mean + gdd.dev + precip.mean + precip.dev + cdom + log.area + depth.max + depth.mean + wae.stock + zm,
            row.eff = ~(1|lake), 
            num.lv.c = lv.c, family = "tweedie", Power = NULL,
            randomB = "LV", quadratic = "LV",
            control.start = list(n.init = 10, jitter.var = 0.1),
            trace = TRUE)
      
    }, error = function(e) {
      cat("  -> Model crashed during fitting. Skipping...\n")
      return(NULL) 
    })
    
    # 4. Safely extract the AICc and save it
    if (!is.null(model_fit)) {
      #save model
      saveRDS(model_fit, file = paste0("Models/model_rev_19_", lv.c, "lv.c.rds"))
      
      #extract AICc
      current_aicc <- tryCatch(AICc(model_fit), error = function(e) numeric(0))
      
      # ONLY add to the dataframe if current_aicc actually contains a number
      if (length(current_aicc) > 0 && !is.na(current_aicc)) {
        conc_quad_results <- rbind(conc_quad_results, 
                                 data.frame(num.lv.c = lv.c, AICc = current_aicc))
        cat("  -> Success! AICc:", current_aicc, "\n")
      } else {
        cat("  -> Model fit, but AICc could not be calculated (singular fit). Skipping...\n")
      }
    }
  }
beep()
# 5. Sort the scoreboard to put the best (lowest) AICc at the top!
conc_quad_results <- conc_quad_results[order(conc_quad_results$AICc), ]
print("--- GRID SEARCH COMPLETE ---")
print(conc_quad_results)


#THE FINAL MODEL-------------------------------------------------------
#so this just shows what we would expect where more lvs are better, did not get to increasing again
#lets look at best one
model_rev_19_4lv.c <- readRDS(file = "Models/model_rev_19_4lv.c.rds")
model_name <- model_rev_19_4lv.c
#notes
  #performance plots pretty good - maybe slight issue with species 13 which is NOP
  #both corrplots look good
  #AICc = 12698.7
  #THIS IS THE OPTION FOR THIS MODEL STRUCTURE
  #YES THIS IS THE ONE, BECAUSE BERT SAYS CONCURRENT ORD IS BEST AND WE WANT THE QUADRATIC/UNIMODAL STRUCTURE BECAUSE IT MAKES ECOLOGICAL SENSE

#lets see if we can get 5lvs to fit
model_rev_19_5lv.c <- gllvm(y = y.matrix, X = x_scale, studyDesign = studyDesignData,
                            lv.formula = ~ secchi.mean + secchi.dev + gdd.mean + gdd.dev + precip.mean + precip.dev + cdom + log.area + depth.max + depth.mean + wae.stock + zm,
                            row.eff = ~(1|lake), 
                            num.lv.c = 5, family = "tweedie", Power = NULL,
                            randomB = "LV", quadratic = "LV",
                            control.start = list(n.init = 10, jitter.var = 0.1),
                            trace = TRUE)
beep()
#save the model
#saveRDS(model_rev_19_5lv.c, file = "Models/model_rev_19_5lv.c.rds")
model_rev_19_5lv.c <- readRDS(file = "Models/model_rev_19_5lv.c.rds")
#set current name for exploration
model_name <- model_rev_19_5lv.c
#notes
  #performance plots bad, 4 was the best model
  #residual correlations collapsed
  #AICc = 12421.54


#CONCURRENT ORD, RandomB = LV, NOT QUADRATIC, 1 to 4 lvs
# 1. Create an empty dataframe to hold our scoreboard
conc_results <- data.frame(num.lv.c = integer(),
                                AICc=numeric())

# 2. Set up the loop
for (lv.c in 1:4) {
  
  # Print a message so you know it hasn't frozen
  cat("Fitting model with num..v.c =", lv.c, "...\n")
  
  # 3. Use tryCatch to skip hard crashes
  model_fit <- tryCatch({
    
    gllvm(y = y.matrix, X = x_scale, studyDesign = studyDesignData,
          lv.formula = ~ secchi.mean + secchi.dev + gdd.mean + gdd.dev + precip.mean + precip.dev + cdom + log.area + depth.max + depth.mean + wae.stock + zm,
          row.eff = ~(1|lake), 
          num.lv.c = lv.c, family = "tweedie", Power = NULL,
          randomB = "LV",
          control.start = list(n.init = 10, jitter.var = 0.1),
          trace = TRUE)
    
  }, error = function(e) {
    cat("  -> Model crashed during fitting. Skipping...\n")
    return(NULL) 
  })
  
  # 4. Safely extract the AICc and save it
  if (!is.null(model_fit)) {
    #save model
    saveRDS(model_fit, file = paste0("Models/model_rev_20_", lv.c, "lv.c.rds"))
    
    #extract AICc
    current_aicc <- tryCatch(AICc(model_fit), error = function(e) numeric(0))
    
    # ONLY add to the dataframe if current_aicc actually contains a number
    if (length(current_aicc) > 0 && !is.na(current_aicc)) {
      conc_results <- rbind(conc_results, 
                                 data.frame(num.lv.c = lv.c, AICc = current_aicc))
      cat("  -> Success! AICc:", current_aicc, "\n")
    } else {
      cat("  -> Model fit, but AICc could not be calculated (singular fit). Skipping...\n")
    }
  }
}
beep()
# 5. Sort the scoreboard to put the best (lowest) AICc at the top!
conc_results <- conc_results[order(conc_results$AICc), ]
print("--- GRID SEARCH COMPLETE ---")
print(conc_results)

#same thing here, more lvs drops the AICc
#lets look at best model
model_rev_20_4lv.c <- readRDS(file = "Models/model_rev_20_4lv.c.rds")
model_name <- model_rev_20_4lv.c
#notes
  #performancs plots bad, lots of shooting stars and underestimating walleye consistently
  #corrplots look fine
  #AICc = 12725.52

#does fewer lvs have better performance?
model_rev_20_3lv.c <- readRDS(file = "Models/model_rev_20_3lv.c.rds")
model_name <- model_rev_20_3lv.c
#notes
  #performance plots better and probably acceptable
  #both corrplots look fine
  #AICc = 12930.9
  #THIS IS THE OPTION FOR THIS MODEL STRUCTURE

#what about 2 LVs
model_rev_20_2lv.c <- readRDS(file = "Models/model_rev_20_2lv.c.rds")
model_name <- model_rev_20_2lv.c
#notes
  #performance plots pretty good
  #corrplots slightly chunky
  #AICc = 13305.27



#DIAGNOSTICS ON A MODEL-------------------------------------

#set the model you want to explore to "model_name"

#diagnostics
par(mfrow = c(3,2))
plot(model_name) 
par(mfrow = c(1,1))
#looks great
#check power
model_name$Power
#get residual correlations - can only do when LVs are present
Theta <- getResidualCor(model_name)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE,
         type = "lower",
         method = "square",
         tl.cex = 0.5,
         t.srt = 45,
         tl.col = "red")
#get correlations due to environment/covariates - can only do if you have random effects for covariates in the model (needs to be constrained or concurrent ordination) - run ?getEnvironCor for details!
Env <- getEnvironCor(model_name)
corrplot(Env[order.single(Env), order.single(Env)],
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

