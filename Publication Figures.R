#This script makes the figures for publication based on the revised model (post-master's thesis)



library(ggplot2)
library(corrplot)
library(gclus)
library(gridExtra)
library(ggrepel)
library(tidyverse)
library(gllvm)
library(ggh4x) #advanced faceting features
library(ggforce) #lets me manually give values for ellipses and helps with repelling points from each other
library(ggrepel) #to repel clustered points
library(patchwork) #for multi-panel plots
library(sf) #for GIS map stuff
library(maps) #basemaps
library(ggspatial) #more GIS stuff
library(cowplot) #layouts for maps
library(scales) #something with the maps
library(legendry) #to manually set axis tick marks
library(ggExtra) #for ggMarginal plots
library(grid) #for special annotations
library(ggtext) #for special annotations
library(gridtext) #for special annotations
library(geomtextpath) #for text on lines

#SETUP---------------------------------------------------
#read in model
#concurrent ordination, 4lvc, quadratic:
model <- readRDS(file = "Models/model_rev_19_4lv.c.rds")

#Correct species names for nice plots and create a dataframe to easily append abbreviations when needed
new_names <- c("Black Crappie", "Bluegill", "Bullhead", "Bowfin", "Burbot", "Cisco", "Common Carp", "Golden Shiner", "Hybrid Sunfish", "Lake Whitefish", 
               "Largemouth Bass", "Muskellunge", "Northern Pike", "Pumpkinseed", "Rainbow Smelt", "Redhorse", "Rock Bass", "Sauger", "Silver Lamprey", "Smallmouth Bass",
               "Walleye", "White Sucker", "Yellow Perch", "Bythotrephes spp.", "Alona spp.", "Bosmina longirostris", "Ceriodaphnia spp.", "Chydorus sphaericus",
               "Daphnia galeata mendotae", "Daphnia longiremis", "Daphnia parvula", "Daphnia pulicaria", "Daphnia retrocurva",
               "Daphnia rare", "Diaphanosoma birgei", "Eubosmina coregoni", "Eurycercus lamellatus", "Holopedium gibberum",
               "Sida crystallina")
abbrv_names <- c("BLC", "BLG", "BLH", "BOF", "BUB", "TLC", "CAP", "GOS", "HSF", "LKW", 
                 "LMB", "MUE", "NOP", "PMK", "RBS", "RHS", "RKB", "SAR", "SIL", "SMB",
                 "WAE", "WTS", "YEP", "Byth. spp.", "Alona spp.", "Bosm. long.", "Ceri. spp.", "Chyd. spha.",
                 "Daph. g. m.", "Daph. long.", "Daph. parv.", "Daph. puli.", "Daph. retr.",
                 "Daph. rare", "Diap. birg.", "Eubo. core.", "Eury. lame.", "Holo. gibb.",
                 "Sida crys.")
group <- c("Fish", "Fish", "Fish", "Fish", "Fish", "Fish", "Fish", "Fish", "Fish", "Fish", 
           "Fish", "Fish", "Fish", "Fish", "Fish", "Fish", "Fish", "Fish", "Fish",
           "Fish", "Fish", "Fish", "Fish", "Zooplankton", "Zooplankton", "Zooplankton", "Zooplankton",
           "Zooplankton", "Zooplankton", "Zooplankton", "Zooplankton", "Zooplankton",
           "Zooplankton", "Zooplankton", "Zooplankton", "Zooplankton", "Zooplankton",
           "Zooplankton", "Zooplankton")
master.names<- as.data.frame(cbind(new_names, abbrv_names, group)) %>% 
  rename(Species = new_names)

colnames(model$y) <- new_names
rownames(model$params$theta) <- new_names
names(model$params$beta0) <- new_names
names(model$params$phi) <- new_names
names(model$sd$beta0) <- new_names
names(model$sd$phi) <- new_names


#my chosen color-blind friendly palette frotm https://davidmathlogic.com/colorblind/#%23332288-%23117733-%2344AA99-%2388CCEE-%23DDCC77-%23CC6677-%23AA4499-%23882255
cb_colors <- c(
  "#332288",
  "#117733",
  "#44AA99",
  "#88CCEE",
  "#DDCC77",
  "#CC6677",
  "#AA4499",
  "#882255"
)




#MODEL POWER-----------------------------------
model$Power


#MODEL SUMMARY--------------------
summary(model)
#note that this summary function works on personal laptop but not work laptop because of different gllvm versions
#model was fit on personal laptop, so summary function works in same version of gllvm in which the model was fit

#DIAGNOSTIC PLOTS----------------------------------------------------------
#look at all
plot(model)
#look at all without color
plot(model, var.color = "black")

#make and save a layout
#png("performance_layout.png", width = 6.5, height = 6, units = "in", res = 600)
#set up 2x2 grid
par(mfrow = c(3,2))
#first plot
plot(model, which = 1, cex = 0.7, lwd = 0.5, ann = FALSE)
#custom titles to make things capitalized
title(xlab = "Linear Predictors", ylab = "Dunn-Smyth-Residuals", main = "Residuals vs. Linear Predictors")   
#annotate letter label
mtext("A", 
      side = 3, #3 for the top margin
      line = 1, #bigger number here moves it further up in plot space
      adj = -0.05, #this is horizontal (0 = left aligned, 1 = right aligned)
      cex = 1.1, #text size
      font = 2) #2 = bold
#same thing for the rest of the plots  
plot(model, which = 2, cex = 0.7, lwd = 0.5, ann = FALSE)
title(xlab = "Theoretical Quantiles", ylab = "Dunn-Smyth-Residuals", main = "Normal Q-Q")
mtext("B", 
      side = 3, #3 for the top margin
      line = 1, #bigger number here moves it further up in plot space
      adj = -0.05, #this is horizontal (0 = left aligned, 1 = right aligned)
      cex = 1.1, #text size
      font = 2) #2 = bold

plot(model, which = 3, cex = 0.7, lwd = 0.5, ann = FALSE)
title(xlab = "Site Index", ylab = "Dunn-Smyth-Residuals", main = "Residuals vs. Row")
mtext("C", 
      side = 3, #3 for the top margin
      line = 1, #bigger number here moves it further up in plot space
      adj = -0.05, #this is horizontal (0 = left aligned, 1 = right aligned)
      cex = 1.1, #text size
      font = 2) #2 = bold

plot(model, which = 4, cex = 0.7, lwd = 0.5, ann = FALSE) 
title(xlab = "Species Index", ylab = "Dunn-Smyth-Residuals", main = "Residuals vs. Column")
mtext("D", 
      side = 3, #3 for the top margin
      line = 1, #bigger number here moves it further up in plot space
      adj = -0.05, #this is horizontal (0 = left aligned, 1 = right aligned)
      cex = 1.1, #text size
      font = 2) #2 = bold

plot(model, which = 5, cex = 0.7, lwd = 0.5, ann = FALSE) 
title(xlab = "Linear Predictors", ylab = expression(sqrt("Dunn-Smyth-Residuals")), main = "Scale-Location")
mtext("E", 
      side = 3, #3 for the top margin
      line = 1, #bigger number here moves it further up in plot space
      adj = -0.05, #this is horizontal (0 = left aligned, 1 = right aligned)
      cex = 1.1, #text size
      font = 2) #2 = bold
#reset par
par(mfrow = c(1,1))
#dev.off()


#VARIANCE PARTITIONING----------------------------------
#calculate variation explained by environment vs. residual

#OKAY RIGHT NOW I HAVE THIS AS EACH AXIS, BUT CAN I GET ENV vs. RESIDUAL PARTS OF EACH AXIS??
    #ANSWER = NO
    #What I can get is an R2 for each environmental variable on each axis, because each axis is essentially a linear regression
    #This shows the importance of each predictor on each LV

#colors for my variance partitioning where I want random lake effect to be gray (checked on colorblind checker https://davidmathlogic.com/colorblind/#%23D4D4D-%236292BB-%2334C7A0-%239453A0-%23E2A826 )
vp_colors <- c(
  "#4D4D4D",
  "#E2A826",
  "#56B4E9",
  "#9453A0",
  "#009069"
)

#use these colors of I decide to plot linear and quadratic separately:
# vp_colors <- c(
#   "#4D4D4D",
#   "#CC79A7",
#   "#753B5D",
#   "#56B4E9",
#   "#004987",
#   "#E69F00",
#   "#D55E00",
#   "#04ECAD",
#   "#009069"
# )

#average of all species
VP <- VP(model)
VP
#plot by species base R
plot(VP, col = vp_colors, main = "", xlab = "Taxon", ylab = "Variance Proportion", legend.text = NULL)
#ggplot by species for publication
#extract values as data frame
VP.df <- as.data.frame(VP[["PropExplainedVarSp"]]) %>% 
  rename("Axis 1" = "CLV1:X/CLV1",
         "Axis 2" = "CLV2:X/CLV2",
         "Axis 3" = "CLV3:X/CLV3",
         "Axis 4" = "CLV4:X/CLV4",
         "Axis 1^2" = "CLV1^2",
         "Axis 2^2" = "CLV2^2",
         "Axis 3^2" = "CLV3^2",
         "Axis 4^2" = "CLV4^2",
         "Random Lake Effect" = "Row random effect: lake"
         )
VP.df.spp <- VP.df %>% 
  mutate(Species = rownames(VP.df))%>% 
  left_join(master.names, by = "Species")

#add the variance of the linear and quadratic terms together
VP.df.sum <- VP.df.spp %>% 
  mutate(`Axis 1` = `Axis 1` + `Axis 1^2`,
         `Axis 2` = `Axis 2` + `Axis 2^2`,
         `Axis 3` = `Axis 3` + `Axis 3^2`,
         `Axis 4` = `Axis 4` + `Axis 4^2`) %>% 
  select(-`Axis 1^2`, -`Axis 2^2`, -`Axis 3^2`, -`Axis 4^2`)

#pivot longer
VP.df.long <- pivot_longer(VP.df.sum, cols = "Axis 1":"Random Lake Effect", names_to = "Variance Component", values_to = "Proportion") %>% 
  #make species a factor so I can put them in the order I want
  mutate(Species = factor(Species, levels = c("Rainbow Smelt", "Cisco", "Lake Whitefish", "Burbot", "Yellow Perch", "Sauger", "Northern Pike",
                                              "Golden Shiner", "Walleye", "Redhorse", "Black Crappie", "White Sucker", "Rock Bass", "Smallmouth Bass", "Silver Lamprey",
                                              "Muskellunge", "Bullhead", "Common Carp", "Pumpkinseed", "Largemouth Bass", "Hybrid Sunfish", "Bluegill",
                                              "Bowfin", "Chydorus sphaericus", "Bosmina longirostris", "Ceriodaphnia spp.", "Eubosmina coregoni",
                                              "Alona spp.", "Diaphanosoma birgei", "Holopedium gibberum", "Daphnia rare", "Daphnia longiremis",
                                              "Daphnia parvula", "Daphnia retrocurva", "Eurycercus lamellatus", "Sida crystallina",
                                              "Daphnia galeata mendotae", "Daphnia pulicaria", "Bythotrephes spp."))) %>% 
  #change variance component factor levels so I put them in the order I want
  mutate(`Variance Component` = factor(`Variance Component`, levels = c("Random Lake Effect",
                                                                        "Axis 4^2", "Axis 4",
                                                                        "Axis 3^2", "Axis 3",
                                                                        "Axis 2^2", "Axis 2",
                                                                        "Axis 1^2", "Axis 1")))

#plot
VP_plot_coordflip <- ggplot(data = VP.df.long, aes(x = Species, y =Proportion, fill = `Variance Component`))+
  geom_col()+
  scale_fill_manual(values = vp_colors)+
  labs(x = "Taxon", y = "Variance Proportion", fill = "")+
  theme_classic(base_size = 11)+
  theme(legend.position = "right",
        legend.box.margin = margin(0,0,0,-10))+
  coord_flip()+
  guides(fill = guide_legend(reverse = TRUE))+
  scale_x_discrete(limits = rev)
VP_plot_coordflip
#save plot
#ggsave("VP_plot_coordflip.png", plot = VP_plot_coordflip, width = 6.5, height = 6.5, units = "in", dpi = 600)

#percentages split by fish and zoops
VP.fish.zoop <- VP.df.sum %>% 
  group_by(group) %>% 
  summarize(`Axis 1` = mean(`Axis 1`),
            `Axis 2` = mean(`Axis 2`),
            `Axis 3` = mean(`Axis 3`),
            `Axis 4` = mean(`Axis 4`),
            `Random Lake Effect` = mean(`Random Lake Effect`),
            .groups = 'drop')

#plot this
#pivot longer
VP.fish.zoop.long <- pivot_longer(VP.fish.zoop, cols = "Axis 1":"Random Lake Effect", names_to = "Variance Component", values_to = "Proportion") %>% 
  #change variance component factor levels so I put them in the order I want
  mutate(`Variance Component` = factor(`Variance Component`, levels = c("Random Lake Effect",
                                                                        "Axis 4^2", "Axis 4",
                                                                        "Axis 3^2", "Axis 3",
                                                                        "Axis 2^2", "Axis 2",
                                                                        "Axis 1^2", "Axis 1")))
VP_fish_zoop <- ggplot(data = VP.fish.zoop.long, aes(x = group, y =Proportion, fill = `Variance Component`))+
  geom_col()+
  scale_fill_manual(values = vp_colors)+
  labs(x = "Taxon Group", y = "Variance Proportion", fill = "")+
  theme_classic(base_size = 11)+
  theme(legend.position = "none")+
  coord_flip()+
  scale_x_discrete(limits = rev)
VP_fish_zoop


#make a VP layout
VP_layout <- VP_fish_zoop / plot_spacer() / VP_plot_coordflip +
  plot_layout(height = c(1, 0.1, 5))+ 
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 12, face = "bold"),
        plot.tag.position = c(0.1, 1)) 
VP_layout
#ggsave("VP_layout.png", plot = VP_layout, width = 6.5, height = 7.5, units = "in", dpi = 600)
#ggsave("VP_layout.svg", plot = VP_layout, width = 6.5, height = 7.5, units = "in", dpi = 600)



#ORDINATIONS-------------------
#not actually using these, I was just curious... But ordinations are not good for the quadratic resposne
#environment and residuals
gllvm::ordiplot(model, type = "conditional", rotate = FALSE, spp.arrows = FALSE)

#just environment
gllvm::ordiplot(model, type = "marginal", rotate = FALSE, spp.arrows = FALSE)

#just residuals
gllvm::ordiplot(model, type = "residual", rotate = FALSE, spp.arrows = FALSE)



#GOODNESS OF FIT------------------------------------------------------------
#okay, but how much of the variation is actually explained by the model?

#https://github.com/JenniNiku/gllvm/discussions/203
#Bert says that we really can't get at how much total variation the model explains 
#because we don't estimate all the dimensions like traditional ordination methods
#Best we can do is partition out the variance we can explain
#He also warns against R^2 values here because they don't do well with non-normal distributions

#BUT, WE CAN DO A GOODNESS OF FIT METRIC LIKE BELOW:
#goodness of fit metrics
goodnessOfFit(model)

#make these species-specific
GOF.spp <- as.data.frame(goodnessOfFit(model, species = TRUE))
rownames(GOF.spp) <- new_names
#save this
#write.csv(GOF.spp, file = "Data/Output/gllvm_GOF_by_species_rev_for_pub.csv")

#plot MARNE
GOF.spp.plot <- GOF.spp %>% 
  mutate(Species = rownames(GOF.spp)) %>% 
  left_join(master.names, by = "Species") %>% 
  #reorder species from high to low but within fish/zoop groups
  arrange(group, MARNE) %>% 
  mutate(Species = fct_inorder(Species))
  
MARNE_plot <- ggplot(data = GOF.spp.plot, aes(x = Species, y = MARNE, fill = group))+
  scale_x_discrete(limits = rev)+
  geom_col()+
  labs(x = "Taxon", y = "Mean Absolute Range Normalized Error", fill = "")+
  scale_fill_manual(values = c("#88CCEE", "#CC6677"))+
  scale_y_continuous(labels = scales::label_percent())+
  theme_classic(base_size = 11)+
  coord_flip()+
  theme(legend.key.size = unit(0.4, "cm"))
MARNE_plot
#save plot
#ggsave("MARNE_plot.png", plot = MARNE_plot, width = 6, height = 6, units = "in", dpi = 600)
#ggsave("MARNE_plot.svg", plot = MARNE_plot, width = 6, height = 6, units = "in", dpi = 600)

#make an MAE plot that I probably won't use
MAE_plot <- ggplot(data = GOF.spp.plot, aes(x = Species, y = MAE))+
  geom_col()+
  labs(x = "Taxon", y = "Mean Absolute Error")+
  theme_classic(base_size = 11)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
MAE_plot
#save plot
#ggsave("MAE_plot.png", plot = MAE_plot, width = 6.5, height = 5, units = "in", dpi = 600)




#EXTRACT CLV AXIS LOADINGS-------------------------------------------------------
# Extract the effects of predictors on the Latent Variables (RR axes)
clv_load <- as.data.frame(model$params$LvXcoef) %>% 
  mutate(Param = rownames(model$params$LvXcoef)) %>% 
  mutate(Param = ifelse(Param == "log.area", "Lake Area",
                         ifelse(Param == "secchi.mean", "Mean Secchi",
                                ifelse(Param == "secchi.dev", "Secchi Deviation", 
                                       ifelse(Param == "gdd.mean", "Mean Degree Days",
                                              ifelse(Param == "gdd.dev", "Degree Days Deviation",
                                                     ifelse(Param == "precip.mean", "Mean Annual Precipitation",
                                                            ifelse(Param == "precip.dev", "Annual Precipitation Deviation",
                                                                   ifelse(Param == "cdom", "CDOM",
                                                                          ifelse(Param == "depth.max", "Maximum Depth",
                                                                                 ifelse(Param == "depth.mean", "Mean Depth",
                                                                                        ifelse(Param == "wae.stockyes", "Walleye stocking",
                                                                                               ifelse(Param == "zmyes", "Zebra Mussel Presence",Param)))))))))))))

clv_load_long <- pivot_longer(clv_load, cols = CLV1:CLV4, names_to = "CLV", values_to = "Loading")


#PREDICTOR EFFECTS ON CLVs-------------------------------------------------

# #This "semi-partial R^2" "measures the importance of the predictors in explaining the latent variables, not the importance of the predictors in explaining the response data"
# #"the correct interpretation of R2B is the (generalized) residual variation a particular predictor can account for, after accounting for all other predictors in the model"
# #See (van der Veen et al. 2023) page 688
# 
# 
# #got this function from (van der Veen et al. 2024):
# partR2.gllvm <- function(object, digits = 3L, ...){
#   if(object$num.lv.c==0&object$num.RR==0){
#     stop("Cannot calculate partial R^2 for the latent variables if no predictor are included. \n")
#   }
#   if(is.null(object$sd)|all(unlist(object$sd)==FALSE)){
#     cat("Standard errors not present in model, calculating...\n")
#     object$Hess<-se.gllvm(object)$Hess 
#   }
#   covB <- object$Hess$cov.mat.mod
#   colnames(covB) <- row.names(covB) <- names(object$TMBfn$par)[object$Hess$incl]
#   covB <- covB[row.names(covB)=="b_lv",colnames(covB)=="b_lv"]
#   K <- ncol(object$lv.X)
#   N <- nrow(object$lv.X)
#   d <- object$num.lv.c+object$num.RR
#   df=d*K
#   resid.df = dim(object$y)[1]*d-df
#   num.df <- d
#   partR2<-rep(0,ncol(object$lv.X))
#   names(partR2)<-colnames(object$lv.X)
#   
#   for(i in 1:ncol(object$lv.X)){
#     covBinv <- try(solve(covB[seq(i,K*d,by=K),seq(i,K*d,by=K)]),silent=T)
#     if(inherits(covBinv,"try-error")){
#       covBinv <- MASS::ginv(covB[seq(i,K*d,by=K),seq(i,K*d,by=K)],tol=0)
#     }
#     Ft <- object$params$LvXcoef[i,]%*%covBinv%*%object$params$LvXcoef[i,]/d
#     partR2[i]<-(num.df*resid.df^-1*Ft)/(1+num.df*resid.df^-1*Ft)
#   }
#   
#   covBinv <- try(solve(covB),silent=T)
#   if(inherits(covBinv,"try-error")){
#     covBinv <- MASS::ginv(covB)
#   }
#   
#   Ftfull <- c(object$params$LvXcoef)%*%covBinv%*%c(object$params$LvXcoef)/(K*d)
#   
#   partR2f<-(df*resid.df^-1*Ftfull)/(1+df*resid.df^-1*Ftfull)
#   
#   cat("R^2 for latent variables:", zapsmall(partR2f,digits), "\n \n")
#   cat("Partial R^2 for predictors and all LVs: \n")
#   print(zapsmall(partR2,digits))
#   
#   invisible(list(partR2full=partR2f,partR2=partR2))
# }
# 
# #try the function!
# partR2.gllvm(model)
#so this function does not work because I have random structure in my canonical coefficients (RandomB = LV)
#but in the paper, Bert says:
#"To assess importance of predictors when the canonical coefficients are treated as random-effects instead, the scale parameters for the canonical coefficients and for the LV-level error can  be used to construct a R2 similar to, for example Nakagawa and  Schielzeth (2013)."
#so I need to try that...
#also from lecture slides "we can use the variance of the random effects for proportion of explained variation" - same idea


#calculate an proportion of LV variance explained per predictor per latent variable
#extract predictor matrix, make categorical variables 1 and 0
x_matrix <- model$lv.X %>% 
  mutate(wae.stock = ifelse(wae.stock == "yes", 1, 0),
         zm = ifelse(zm == "yes", 1, 0))
#calculate variance of each predictor
var_x <- apply(x_matrix, 2, var) #mostly comes out to 1 because they are standardized...
#calculate total variance of predctors by summing those
total_var_x <- sum(var_x)
#save the number of variables and number of lvs
K <- ncol(x_matrix) #number of variables = 12
d <- model$num.lv.c #number of latent variables = 4
#extract canonical coefficients (slopes) for each predictor on each LV
B_hat <- model$params$LvXcoef
#extract the shared variance of all predictors for each LV - I forced this with random structure
var_LvXcoef <- model$params$sigmaLvXcoef^2
#extract residual unexplained variance per lv
var_lv_resid <- model$params$sigma.lv^2
#calculate the marginal R2 for each axis
var_pred_perLV <- total_var_x * var_LvXcoef #multiple total variance of predictors by the variance of the canonical coefficients
total_var_perLV <- var_pred_perLV + var_lv_resid #calculate total variance on each axis by adding total predictor variance to residual variance
r2_perLV <- var_pred_perLV/total_var_perLV
#create a matrix for the R2 of each predictor for each LV
r2_matrix <- matrix(0, nrow=K+1, ncol=d)
rownames(r2_matrix) <- c(colnames(x_matrix), "Unexplained Residual")
colnames(r2_matrix) <- names(r2_perLV)
#fill the matrix
for(q in 1:d){
  #calculate the R2 of each predictor on each Lv by multiplying each canonical coefficient squared by it's variance in the predictor matrix
  pred_var_q <- B_hat[, q]^2 * var_x
  #extract the unexplained residual variance for desired LV
  resid_var_q <- var_lv_resid[q]
  #calculate total axis variance for desired LV
  total_var_q <- sum(pred_var_q) + resid_var_q
  #calculate proportion of total variance contributed by each predictor and the residual
  r2_matrix[1:K, q] <- pred_var_q / total_var_q
  r2_matrix[K+1, q] <- resid_var_q / total_var_q
}
r2_matrix <- as.data.frame(r2_matrix)
colSums(r2_matrix)
#great, it all adds up to 1

#for each axis, I want to make a plot showing the proportion of variance explained by that LV attributable to each predictor and the unexplained portion too
#format the canonical coefficent loadings into positive and negative to join to this
clv_load_direction <- clv_load %>% 
  mutate(CLV1_dir = ifelse(CLV1 > 0, "Positive", "Negative"),
         CLV2_dir = ifelse(CLV2 > 0, "Positive", "Negative"),
         CLV3_dir = ifelse(CLV3 > 0, "Positive", "Negative"),
         CLV4_dir = ifelse(CLV4 > 0, "Positive", "Negative")) %>% 
  add_row() %>% 
  select(-CLV1, -CLV2, -CLV3, -CLV4, -Param)

LV_vp_plotdata <- r2_matrix %>% 
  mutate(Param = c(clv_load$Param, "Unexplained Residual")) %>% 
  cbind(clv_load_direction) %>% 
  mutate(CLV1_dir = factor(CLV1_dir, levels = c("Positive", "Negative")),
         CLV2_dir = factor(CLV2_dir, levels = c("Positive", "Negative")),
         CLV3_dir = factor(CLV3_dir, levels = c("Positive", "Negative")),
         CLV4_dir = factor(CLV4_dir, levels = c("Positive", "Negative")))


clv1_vp <- ggplot(data = LV_vp_plotdata, aes(x = reorder(Param, -CLV1), y = CLV1, fill = CLV1_dir))+
  geom_col()+
  labs(x = NULL, y = "Proportion of CLV1\nVariance Explained", fill = "Direction of Influence")+
  scale_fill_manual(values = c("#5e3c99", "#E66101"))+
  theme_classic(base_size = 11)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.3, "cm"),
    legend.spacing.y = unit(0.1, "cm"),
    legend.margin = margin(t = 2, r = 2, b = 2, l = 2))
clv1_vp

clv2_vp <- ggplot(data = LV_vp_plotdata, aes(x = reorder(Param, -CLV2), y = CLV2, fill = CLV2_dir))+
  geom_col()+
  labs(x = NULL, y = "Proportion of CLV2\nVariance Explained", fill = "Direction of Influence")+
  scale_fill_manual(values = c("#5e3c99", "#E66101"))+
  theme_classic(base_size = 11)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.3, "cm"),
    legend.spacing.y = unit(0.1, "cm"),
    legend.margin = margin(t = 2, r = 2, b = 2, l = 2))
clv2_vp

clv3_vp <- ggplot(data = LV_vp_plotdata, aes(x = reorder(Param, -CLV3), y = CLV3, fill = CLV3_dir))+
  geom_col()+
  labs(x = NULL, y = "Proportion of CLV3\nVariance Explained", fill = "Direction of Influence")+
  scale_fill_manual(values = c("#5e3c99", "#E66101"))+
  theme_classic(base_size = 11)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.3, "cm"),
    legend.spacing.y = unit(0.1, "cm"),
    legend.margin = margin(t = 2, r = 2, b = 2, l = 2))
clv3_vp

clv4_vp <- ggplot(data = LV_vp_plotdata, aes(x = reorder(Param, -CLV4), y = CLV4, fill = CLV4_dir))+
  geom_col()+
  labs(x = NULL, y = "Proportion of CLV4\nVariance Explained", fill = "Direction of Influence")+
  scale_fill_manual(values = c("#5e3c99", "#E66101"))+
  theme_classic(base_size = 11)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.3, "cm"),
    legend.spacing.y = unit(0.1, "cm"),
    legend.margin = margin(t = 2, r = 2, b = 2, l = 2))
clv4_vp



#SPECIES OPTIMA / lv.X#SPECIES OPTIMA / LINEAR COEF -------------------
#BASED ON van der Veen et al. 2021, it is best to plot species optima directly with the quadratic effect rather than doing ordinations
#can use ellipses to show species range tolerances (in an ordination if I did that, which I'm not)

#this section also make layouts with the CLV-specific variance partitioning plots I made in previous section
#No longer using the raw canonical coefficient plots because need to show portion of unexplained variance too

#extract species coefficients
spec_coef <- as.data.frame(model[["params"]][["theta"]])
spec_coef$taxon = rownames(spec_coef)
spec_coef <- spec_coef %>% 
  rename_with(~paste0(., "_coef"), .cols = -taxon)
#extract species coefficient standard errors
spec_coef_se <- as.data.frame(model[["sd"]][["theta"]])
spec_coef_se$taxon = new_names
spec_coef_se <- spec_coef_se %>% 
  rename_with(~paste0(., "_coef_se"), .cols = -taxon)
spec_coef <- full_join(spec_coef, spec_coef_se, by = "taxon")
#extract species optima
species_opt <- as.data.frame(optima(model))
species_opt$taxon = rownames(species_opt)
species_opt <- species_opt %>% 
  rename_with(~paste0(., "_opt"), .cols = -taxon)
#extract species tolerances
species_tol <- as.data.frame(tolerances(model))
species_tol$taxon = rownames(species_tol)
#add a suffix to the tolerance columns
species_tol <- species_tol %>% 
  rename_with(~paste0(., "_tol"), .cols = -taxon)
#join into one dataframe
coef_opt <- full_join(spec_coef, species_opt, by = "taxon")
coef_opt_tol <- full_join(coef_opt, species_tol, by = "taxon")
#calculate precision of tolerances as inverse of squared tolerances
coef_opt_tol_prec <- coef_opt_tol %>% 
  mutate(CLV1_prec = 1/(CLV1_tol^2),
         CLV2_prec = 1/(CLV2_tol^2),
         CLV3_prec = 1/(CLV3_tol^2),
         CLV4_prec = 1/(CLV4_tol^2)
         )


#Get standard errors for my optima - have to calculate myself to get CONDITIONAL STANDARD ERROR (ignores uncertainty of quadratic term)
#can I hand calculate my optima?
coef_opt_tol_prec$opt_test <- -coef_opt_tol_prec$CLV1_coef/(2*coef_opt_tol_prec$`CLV1^2_coef`)
coef_opt_tol_prec$test <- coef_opt_tol_prec$CLV1_opt-coef_opt_tol_prec$opt_test
#YES!
#now scale the standard errors the same way - this gives me CONDITIONAL STANDARD ERROR - because it ignores the uncertainty of the quadratic term
coef_opt_tol_prec$CLV1_opt_se <- coef_opt_tol_prec$CLV1_coef_se/abs(2*coef_opt_tol_prec$`CLV1^2_coef`)
coef_opt_tol_prec$CLV2_opt_se <- coef_opt_tol_prec$CLV2_coef_se/abs(2*coef_opt_tol_prec$`CLV2^2_coef`)
coef_opt_tol_prec$CLV3_opt_se <- coef_opt_tol_prec$CLV3_coef_se/abs(2*coef_opt_tol_prec$`CLV3^2_coef`)
coef_opt_tol_prec$CLV4_opt_se <- coef_opt_tol_prec$CLV4_coef_se/abs(2*coef_opt_tol_prec$`CLV4^2_coef`)


#separate fish vs. zoop groups for plot colors
coef_opt_tol_prec$Species <- coef_opt_tol_prec$taxon
coef_opt_tol_prec <- coef_opt_tol_prec %>% 
  left_join(master.names, by = "Species")

#do some weird stuff to make a key for the number points that correspond to species
opt_plot_data <- coef_opt_tol_prec %>%
  mutate(taxon = factor(taxon, levels = unique(taxon)), #don't let it change the taxon factor order to alphabetical order
         species_num = as.numeric(taxon),
         legend_label = paste0(species_num, " ", taxon)
         ) %>% 
  #make species a factor so I can put them in the order I want
  mutate(Species = factor(Species, levels = c("Rainbow Smelt", "Cisco", "Lake Whitefish", "Burbot", "Yellow Perch", "Sauger", "Northern Pike",
                                              "Golden Shiner", "Walleye", "Redhorse", "Black Crappie", "White Sucker", "Rock Bass", "Smallmouth Bass", "Silver Lamprey",
                                              "Muskellunge", "Bullhead", "Common Carp", "Pumpkinseed", "Largemouth Bass", "Hybrid Sunfish", "Bluegill",
                                              "Bowfin", "Chydorus sphaericus", "Bosmina longirostris", "Ceriodaphnia spp.", "Eubosmina coregoni",
                                              "Alona spp.", "Diaphanosoma birgei", "Holopedium gibberum", "Daphnia rare", "Daphnia longiremis",
                                              "Daphnia parvula", "Daphnia retrocurva", "Eurycercus lamellatus", "Sida crystallina",
                                              "Daphnia galeata mendotae", "Daphnia pulicaria", "Bythotrephes spp."))) %>% 
  #order species abbreviations same way
  mutate(abbrv_names = factor(abbrv_names, levels = c("RBS", "TLC", "LKW", "BUB", "YEP", "SAR", "NOP",
                                              "GOS", "WAE", "RHS", "BLC", "WTS", "RKB", "SMB", "SIL",
                                              "MUE", "BLH", "CAP", "PMK", "LMB", "HSF", "BLG",
                                              "BOF", "Chyd. spha.", "Bosm. long.", "Ceri. spp.", "Eubo. core.",
                                              "Alona spp.", "Diap. birg.", "Holo. gibb.", "Daph. rare", "Daph. long.",
                                              "Daph. parv.", "Daph. retr.", "Eury. lame.", "Sida crys.",
                                              "Daph. g. m.", "Daph. puli.", "Byth. spp.")))


#CAN NO LONGER DO THE BIPLOT THING BECAUSE I HAVE 4 AXES
#Just deleted all the code for that
#Rely on corrplots for this instead
#Could use the annotation style for the individual optimum plots though and present the weird R2 things in a plot instead of coefficients


#In this model, CLVs 1,2, and 4 have linear responses while CLV3 has a quadratic response

#make 3 plots with linear coefficients (LvXCoef) for the axes with linear responses, combined with the canonical coefficient (loading) plots for each axis

#CLV1
opt_plot_data_CLV1 <- opt_plot_data %>% 
  mutate(lower = CLV1_coef-(1.96*CLV1_coef_se),
         upper = CLV1_coef+(1.96*CLV1_coef_se),
         sig = ifelse(lower <= 0 & upper >= 0, "no", "yes"))
CLV1_plot_forlayout <- ggplot(data = opt_plot_data_CLV1, aes(x = CLV1_coef, y = Species))+
  scale_y_discrete(limits = rev)+
  #vertical line at 0
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", size = 0.5) +
  #95% confidence intervals
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.4, linewidth = 0.2)+
  #point estimate
  geom_point(aes(color = group, shape = sig), size = 3, stroke = 0.75)+
  scale_color_manual(values = c("#88CCEE", "#CC6677"))+
  scale_shape_manual(values = c("no" = 1, "yes" = 16))+
  guides(shape = "none", color = guide_legend(byrow = TRUE, keyheight = unit(0.5, "cm")))+
  labs(x = "CLV1 Linear Coefficient", y = "Taxon", color = "")+
  #scale_x_continuous(limits = c(-19, 22))+
  theme_classic(base_size = 11)+
  theme(legend.position = "right")+
  theme(
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.3, "cm"),
    legend.spacing.y = unit(0.3, "cm"),
    legend.margin = margin(t = 2, r = 2, b = 2, l = 2))
CLV1_plot_forlayout
# clv1_loadings <- ggplot(data = clv_load, aes(x = reorder(Param, CLV1), y = CLV1))+
#   geom_col()+
#   labs(x = "Environmental Variable", y = "CLV1 Canonical Coefficient")+
#   theme_classic(base_size = 11)+
#   coord_flip()
# #scale_y_continuous(limits = c(-0.8, 0.4), breaks = c(-0.8, -0.4, 0, 0.4))
# clv1_loadings

CLV1_layout <- clv1_vp / plot_spacer() / CLV1_plot_forlayout +
  plot_layout(height = c(1, 0.05, 3))+ 
  plot_annotation(tag_levels = 'A') &
  theme(plot.margin = margin(5,5,5,12), #gives extra space on the left for long Eurycercus label
        plot.tag = element_text(size = 12, face = "bold"),
        plot.tag.position = c(0.1, 1))
CLV1_layout
# ggsave("clv1_layout.png", plot = CLV1_layout, width = 6.5, height = 8, units = "in", dpi = 600)
# ggsave("clv1_layout.svg", plot = CLV1_layout, width = 6.5, height = 8, units = "in", dpi = 600)


#CLV2
opt_plot_data_CLV2 <- opt_plot_data %>% 
  mutate(lower = CLV2_coef-(1.96*CLV2_coef_se),
         upper = CLV2_coef+(1.96*CLV2_coef_se),
         sig = ifelse(lower <= 0 & upper >= 0, "no", "yes"))
CLV2_plot_forlayout <- ggplot(data = opt_plot_data_CLV2, aes(x = CLV2_coef, y = Species))+
  scale_y_discrete(limits = rev)+
  #vertical line at 0
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", size = 0.5) +
  #95% confidence intervals
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.4, linewidth = 0.2)+
  #point estimate
  geom_point(aes(color = group, shape = sig), size = 3, stroke = 0.75)+
  scale_color_manual(values = c("#88CCEE", "#CC6677"))+
  scale_shape_manual(values = c("no" = 1, "yes" = 16))+
  guides(shape = "none", color = guide_legend(byrow = TRUE, keyheight = unit(0.5, "cm")))+
  labs(x = "CLV2 Linear Coefficient", y = "Taxon", color = "")+
  #scale_x_continuous(limits = c(-19, 22))+
  theme_classic(base_size = 11)+
  theme(legend.position = "right")+
  theme(
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.3, "cm"),
    legend.spacing.y = unit(0.3, "cm"),
    legend.margin = margin(t = 2, r = 2, b = 2, l = 2))
CLV2_plot_forlayout
# clv2_loadings <- ggplot(data = clv_load, aes(x = reorder(Param, CLV2), y = CLV2))+
#   geom_col()+
#   labs(x = "Environmental Variable", y = "CLV2 Canonical Coefficient")+
#   theme_classic(base_size = 11)+
#   coord_flip()
#   #scale_y_continuous(limits = c(-0.8, 0.4), breaks = c(-0.8, -0.4, 0, 0.4))
# clv2_loadings

CLV2_layout <- clv2_vp / plot_spacer() / CLV2_plot_forlayout +
  plot_layout(height = c(1, 0.05, 3))+ 
  plot_annotation(tag_levels = 'A') &
  theme(plot.margin = margin(5,5,5,12), #gives extra space on the left for long Eurycercus label
        plot.tag = element_text(size = 12, face = "bold"),
        plot.tag.position = c(0.1, 1))
CLV2_layout
# ggsave("clv2_layout.png", plot = CLV2_layout, width = 6.5, height = 8, units = "in", dpi = 600)
# ggsave("clv2_layout.svg", plot = CLV2_layout, width = 6.5, height = 8, units = "in", dpi = 600)


#CLV4
opt_plot_data_CLV4 <- opt_plot_data %>% 
  mutate(lower = CLV4_coef-(1.96*CLV4_coef_se),
         upper = CLV4_coef+(1.96*CLV4_coef_se),
         sig = ifelse(lower <= 0 & upper >= 0, "no", "yes"))
CLV4_plot_forlayout <- ggplot(data = opt_plot_data_CLV4, aes(x = CLV4_coef, y = Species))+
  scale_y_discrete(limits = rev)+
  #vertical line at 0
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", size = 0.5) +
  #95% confidence intervals
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.4, linewidth = 0.2)+
  #point estimate
  geom_point(aes(color = group, shape = sig), size = 3, stroke = 0.75)+
  scale_color_manual(values = c("#88CCEE", "#CC6677"))+
  scale_shape_manual(values = c("no" = 1, "yes" = 16))+
  guides(shape = "none", color = guide_legend(byrow = TRUE, keyheight = unit(0.5, "cm")))+
  labs(x = "CLV4 Linear Coefficient", y = "Taxon", color = "")+
  #scale_x_continuous(limits = c(-19, 22))+
  theme_classic(base_size = 11)+
  theme(legend.position = "right")+
  theme(
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.3, "cm"),
    legend.spacing.y = unit(0.3, "cm"),
    legend.margin = margin(t = 2, r = 2, b = 2, l = 2))
CLV4_plot_forlayout
# clv4_loadings <- ggplot(data = clv_load, aes(x = reorder(Param, CLV4), y = CLV4))+
#   geom_col()+
#   labs(x = "Environmental Variable", y = "CLV4 Canonical Coefficient")+
#   theme_classic(base_size = 11)+
#   coord_flip()
# #scale_y_continuous(limits = c(-0.8, 0.4), breaks = c(-0.8, -0.4, 0, 0.4))
# clv4_loadings

CLV4_layout <- clv4_vp / plot_spacer() / CLV4_plot_forlayout +
  plot_layout(height = c(1, 0.05, 3))+ 
  plot_annotation(tag_levels = 'A') &
  theme(plot.margin = margin(5,5,5,12), #gives extra space on the left for long Eurycercus label
        plot.tag = element_text(size = 12, face = "bold"),
        plot.tag.position = c(0.1, 1))
CLV4_layout
# ggsave("clv4_layout.png", plot = CLV4_layout, width = 6.5, height = 8, units = "in", dpi = 600)
# ggsave("clv4_layout.svg", plot = CLV4_layout, width = 6.5, height = 8, units = "in", dpi = 600)



#Make similar layout for CLV3 with the optima
opt_plot_data_CLV3 <- opt_plot_data %>%
  mutate(lower = CLV3_opt-(1.96*CLV3_opt_se),
         upper = CLV3_opt+(1.96*CLV3_opt_se),
         linear = ifelse(CLV3_opt < -10 | CLV3_opt > 10, "yes", "no"))
CLV3_plot_forlayout <- ggplot(data = opt_plot_data_CLV3, aes(x = CLV3_opt, y = Species))+
  scale_y_discrete(limits = rev)+
  #vertical line at 0
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", size = 0.5) +
  #vertical lines at -10 and 10
  geom_vline(xintercept = 10, linetype = "dashed", color = "red", size = 0.2) +
  geom_vline(xintercept = -10, linetype = "dashed", color = "red", size = 0.2) +
  #95% confidence intervals
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.4, linewidth = 0.2)+
  #point estimate
  geom_point(aes(color = group, shape = linear), size = 3)+
  scale_shape_manual(values = c("no" = 16, "yes" = 1))+
  guides(shape = "none", color = guide_legend(byrow = TRUE, keyheight = unit(0.5, "cm")))+
  scale_color_manual(values = c("#88CCEE", "#CC6677"))+
  labs(x = "CLV3 Optimum", y = "Taxon", color = "")+
  #scale_x_continuous(limits = c(-19, 22))+
  theme_classic(base_size = 11)+
  theme(legend.position = "right")+
  theme(
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.3, "cm"),
    legend.spacing.y = unit(0.3, "cm"),
    legend.margin = margin(t = 2, r = 2, b = 2, l = 2))
CLV3_plot_forlayout
# clv3_loadings <- ggplot(data = clv_load, aes(x = reorder(Param, CLV3), y = CLV3))+
#   geom_col()+
#   labs(x = "Environmental Variable", y = "CLV3 Canonical Coefficient")+
#   theme_classic(base_size = 11)+
#   coord_flip()
#   #scale_y_continuous(limits = c(-0.8, 0.4), breaks = c(-0.8, -0.4, 0, 0.4))
# clv3_loadings

CLV3_layout <- clv3_vp / plot_spacer() / CLV3_plot_forlayout +
  plot_layout(height = c(1, 0.05, 3))+
  plot_annotation(tag_levels = 'A') &
  theme(plot.margin = margin(5,5,5,12), #gives extra space on the left for long Eurycercus label
        plot.tag = element_text(size = 12, face = "bold"),
        plot.tag.position = c(0.1, 1))
CLV3_layout
# ggsave("clv3_layout.png", plot = CLV3_layout, width = 6.5, height = 8, units = "in", dpi = 600)
# ggsave("clv3_layout.svg", plot = CLV3_layout, width = 6.5, height = 8, units = "in", dpi = 600)




#INTERCEPTS----------------------------------
#extract species-specific intercepts and their standard errors
intercept <- as.data.frame(model[["params"]][["beta0"]])

#format, add standard errors, and separate fish vs. zoop groups
intercept_se <- intercept %>% 
  rename(beta0 = "model[[\"params\"]][[\"beta0\"]]") %>% 
  mutate(Species = rownames(intercept),
         beta0_se = model[["sd"]][["beta0"]]) %>% 
  left_join(master.names, by = "Species") %>% 
  mutate(Species = factor(Species, levels = c("Rainbow Smelt", "Cisco", "Lake Whitefish", "Burbot", "Yellow Perch", "Sauger", "Northern Pike",
                                              "Golden Shiner", "Walleye", "Redhorse", "Black Crappie", "White Sucker", "Rock Bass", "Smallmouth Bass", "Silver Lamprey",
                                              "Muskellunge", "Bullhead", "Common Carp", "Pumpkinseed", "Largemouth Bass", "Hybrid Sunfish", "Bluegill",
                                              "Bowfin", "Chydorus sphaericus", "Bosmina longirostris", "Ceriodaphnia spp.", "Eubosmina coregoni",
                                              "Alona spp.", "Diaphanosoma birgei", "Holopedium gibberum", "Daphnia rare", "Daphnia longiremis",
                                              "Daphnia parvula", "Daphnia retrocurva", "Eurycercus lamellatus", "Sida crystallina",
                                              "Daphnia galeata mendotae", "Daphnia pulicaria", "Bythotrephes spp.")))

#plot
intercept_plot <- ggplot(data = intercept_se, aes(x = beta0, y = Species))+
  scale_y_discrete(limits = rev)+
  #vertical line at 0
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", size = 0.5) +
  #95% confidence intervals
  geom_errorbar(aes(xmin = beta0-(1.96*beta0_se), xmax = beta0+(1.96*beta0_se)), width = 0.4, linewidth = 0.2)+
  #point estimate
  geom_point(aes(fill = group), color = "transparent", shape = 21, size = 3)+
  scale_fill_manual(values = c("#88CCEE", "#CC6677"))+
  labs(x = "Intercept", y = "Taxon", fill = "")+
  #scale_x_continuous(limits = c(-19, 22))+
  theme_classic(base_size = 11)
intercept_plot
# ggsave("intercepts.png", plot = intercept_plot, width = 6.5, height = 7, units = "in", dpi = 600)
# ggsave("intercepts.svg", plot = intercept_plot, width = 6.5, height = 7, units = "in", dpi = 600)


#PHI------------------------------
#run intercept code first to make this work 
#extract species-specific intercepts and their standard errors
phi <- as.data.frame(model[["params"]][["phi"]])

#format, add standard errors, and separate fish vs. zoop groups
phi_se <- phi %>% 
  rename(phi = "model[[\"params\"]][[\"phi\"]]") %>% 
  mutate(Species = rownames(intercept),
         phi_se = model[["sd"]][["phi"]]) %>% 
  left_join(master.names, by = "Species") %>% 
  mutate(Species = factor(Species, levels = c("Rainbow Smelt", "Cisco", "Lake Whitefish", "Burbot", "Yellow Perch", "Sauger", "Northern Pike",
                                              "Golden Shiner", "Walleye", "Redhorse", "Black Crappie", "White Sucker", "Rock Bass", "Smallmouth Bass", "Silver Lamprey",
                                              "Muskellunge", "Bullhead", "Common Carp", "Pumpkinseed", "Largemouth Bass", "Hybrid Sunfish", "Bluegill",
                                              "Bowfin", "Chydorus sphaericus", "Bosmina longirostris", "Ceriodaphnia spp.", "Eubosmina coregoni",
                                              "Alona spp.", "Diaphanosoma birgei", "Holopedium gibberum", "Daphnia rare", "Daphnia longiremis",
                                              "Daphnia parvula", "Daphnia retrocurva", "Eurycercus lamellatus", "Sida crystallina",
                                              "Daphnia galeata mendotae", "Daphnia pulicaria", "Bythotrephes spp.")))

#plot
phi_plot <- ggplot(data = phi_se, aes(x = phi, y = Species))+
  scale_y_discrete(limits = rev)+
  #vertical line at 0
  #geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", size = 0.5) +
  #95% confidence intervals
  geom_errorbar(aes(xmin = phi-(1.96*phi_se), xmax = phi+(1.96*phi_se)), width = 0.7, linewidth = 0.2)+
  #point estimate
  geom_point(aes(fill = group), color = "transparent", shape = 21, size = 3)+
  scale_fill_manual(values = c("#88CCEE", "#CC6677"))+
  labs(x = "Tweedie Dispersion Parameter (\u03d5)", y = "Taxon", fill = "")+
  #scale_x_continuous(limits = c(-19, 22))+
  theme_classic(base_size = 11)
phi_plot
# ggsave("phi.png", plot = phi_plot, width = 6.5, height = 7, units = "in", dpi = 600)
# ggsave("phi.svg", plot = phi_plot, width = 6.5, height = 7, units = "in", dpi = 600)





#CORRELATION MATRICES----------------

#create vector with species abbreviation in order I want

cor.order <- c("RBS", "TLC", "LKW", "BUB", "YEP", "SAR", "NOP",
               "GOS", "WAE", "RHS", "BLC", "WTS", "RKB", "SMB", "SIL",
               "MUE", "BLH", "CAP", "PMK", "LMB", "HSF", "BLG",
               "BOF", "Chyd. spha.", "Bosm. long.", "Ceri. spp.", "Eubo. core.",
               "Alona spp.", "Diap. birg.", "Holo. gibb.", "Daph. rare", "Daph. long.",
               "Daph. parv.", "Daph. retr.", "Eury. lame.", "Sida crys.",
               "Daph. g. m.", "Daph. puli.", "Byth. spp.")
#NOTE I JUST STUCK SIL IN HERE ON THE EDGE OF COLD AND WARM - COME BACK TO THAT LATER

#environmental correlations
Env <- getEnvironCor(model)
#reset row and column names to the abbreviations
rownames(Env) <- master.names$abbrv_names
colnames(Env) <- master.names$abbrv_names
#png("cor_all_env.png", width = 6.5, height = 6.5, units = "in", res = 600)
corrplot(Env[cor.order, cor.order],
         diag = FALSE, #do not include the exact diagonal where speices overlap with themselves
         type = "lower", #lower half of diagonal
         method = "square",
         tl.cex = 0.7, #label size
         tl.srt = 45, #rotate top labels
         tl.col = "black", #label color
         col = COL2('PuOr'), #purple-orange colors for color-blind friendly
         mar = c(0, 0, 0, 0)) #get rid of white space around edges
#dev.off()


#residual correlations
Theta <- getResidualCor(model)
#reset row and column names to the abbreviations
rownames(Theta) <- master.names$abbrv_names
colnames(Theta) <- master.names$abbrv_names
#png("cor_all_res.png", width = 6.5, height = 6.5, units = "in", res = 600)
corrplot(Theta[cor.order, cor.order],
         diag = FALSE, #do not include the exact diagonal where speices overlap with themselves
         type = "lower", #lower half of diagonal
         method = "square",
         tl.cex = 0.7, #label size
         tl.srt = 45, #rotate top labels
         tl.col = "black", #label color
         col = COL2('PuOr'), #purple-orange colors for color-blind friendly
         mar = c(0, 0, 0, 0)) #get rid of white space around edges
#dev.off()



#what proportions of correlations are positive vs. negative?
env_corr_prop <- as.data.frame(Env) %>% 
  rbind(group) %>% 
  mutate(Species = c(rownames(Env), NA),
         group = c(group, NA))

fish_fish <- env_corr_prop %>% 
  filter(group == "Fish" | is.na(group)) %>% 
  select(which(env_corr_prop["40",] == "Fish" & !is.na(env_corr_prop["40",]))) %>% 
  filter(BLC != "Fish")%>% 
  mutate(across(everything(), as.numeric))
fish_fish <- as.matrix(fish_fish)
diag(fish_fish) <- NA
fish_fish <- as.numeric(unlist(fish_fish))
fish_fish_env_prop_pos <- (sum(fish_fish > 0, na.rm = TRUE)/(sum(fish_fish > 0, na.rm = TRUE)+sum(fish_fish < 0, na.rm = TRUE)))
fish_fish_env_prop_pos

zoop_zoop <- env_corr_prop %>% 
  filter(group == "Zooplankton" | is.na(group)) %>% 
  select(which(env_corr_prop["40",] == "Zooplankton" & !is.na(env_corr_prop["40",]))) %>% 
  filter(`Alona spp.` != "Zooplankton")%>% 
  mutate(across(everything(), as.numeric))
zoop_zoop <- as.matrix(zoop_zoop)
diag(zoop_zoop) <- NA
zoop_zoop <- as.numeric(unlist(zoop_zoop))
zoop_zoop_env_prop_pos <- (sum(zoop_zoop > 0, na.rm = TRUE)/(sum(zoop_zoop > 0, na.rm = TRUE)+sum(zoop_zoop < 0, na.rm = TRUE)))
zoop_zoop_env_prop_pos

fish_zoop <- env_corr_prop %>% 
  filter(group == "Fish" | is.na(group)) %>% 
  select(which(env_corr_prop["40",] == "Zooplankton" & !is.na(env_corr_prop["40",]))) %>% 
  filter(`Alona spp.` != "Zooplankton")%>% 
  mutate(across(everything(), as.numeric))
fish_zoop <- as.matrix(fish_zoop) #you do want the diagonal on this one because nothing is compared to itself
fish_zoop <- as.numeric(unlist(fish_zoop))
fish_zoop_env_prop_pos <- (sum(fish_zoop > 0, na.rm = TRUE)/(sum(fish_zoop > 0, na.rm = TRUE)+sum(fish_zoop < 0, na.rm = TRUE)))
fish_zoop_env_prop_pos


res_corr_prop <- as.data.frame(Theta) %>% 
  rbind(group) %>% 
  mutate(Species = c(rownames(Theta), NA),
         group = c(group, NA))

fish_fish <- res_corr_prop %>% 
  filter(group == "Fish" | is.na(group)) %>% 
  select(which(res_corr_prop["40",] == "Fish" & !is.na(res_corr_prop["40",]))) %>% 
  filter(`Black Crappie` != "Fish") %>% 
  mutate(across(everything(), as.numeric))
fish_fish <- as.matrix(fish_fish)
diag(fish_fish) <- NA
fish_fish <- as.numeric(unlist(fish_fish))
fish_fish_res_prop_pos <- (sum(fish_fish > 0, na.rm = TRUE)/(sum(fish_fish > 0, na.rm = TRUE)+sum(fish_fish < 0, na.rm = TRUE)))
fish_fish_res_prop_pos

zoop_zoop <- res_corr_prop %>% 
  filter(group == "Zooplankton" | is.na(group)) %>% 
  select(which(res_corr_prop["40",] == "Zooplankton" & !is.na(res_corr_prop["40",]))) %>% 
  filter(`Alona spp.` != "Zooplankton")%>% 
  mutate(across(everything(), as.numeric))
zoop_zoop <- as.matrix(zoop_zoop)
diag(zoop_zoop) <- NA
zoop_zoop <- as.numeric(unlist(zoop_zoop))
zoop_zoop_res_prop_pos <- (sum(zoop_zoop > 0, na.rm = TRUE)/(sum(zoop_zoop > 0, na.rm = TRUE)+sum(zoop_zoop < 0, na.rm = TRUE)))
zoop_zoop_res_prop_pos

fish_zoop <- res_corr_prop %>% 
  filter(group == "Fish" | is.na(group)) %>% 
  select(which(res_corr_prop["40",] == "Zooplankton" & !is.na(res_corr_prop["40",]))) %>% 
  filter(`Alona spp.` != "Zooplankton")%>% 
  mutate(across(everything(), as.numeric))
fish_zoop <- as.matrix(fish_zoop) #you do want the diagonal on this one because nothing is compared to itself
fish_zoop <- as.numeric(unlist(fish_zoop))
fish_zoop_res_prop_pos <- (sum(fish_zoop > 0, na.rm = TRUE)/(sum(fish_zoop > 0, na.rm = TRUE)+sum(fish_zoop < 0, na.rm = TRUE)))
fish_zoop_res_prop_pos

  


#ENV-RES BIPLOTS-------------------------------------------------------------

#run correlation matrix and species optima prep code first

#make a dataframe of the walleye correlations to all other species 
WAE.env <- as.data.frame(Env) %>% 
  select(WAE) %>% 
  rename(env = WAE)
WAE.res <- as.data.frame(Theta) %>% 
  select(WAE)%>% 
  rename(res = WAE)
WAE.corr <- cbind(WAE.env, WAE.res) %>% 
  mutate(Species = rownames(WAE.env),
         highlight.spp = "Walleye",
         res = ifelse(Species == "WAE", NA, res),
         env = ifelse(Species == "WAE", NA, env))

#make a centrchid color key
WAE.corr.grp <- WAE.corr %>% 
  mutate(group2 = ifelse(WAE.corr$Species == "LMB" | WAE.corr$Species == "BLC" | WAE.corr$Species == "PMK" | WAE.corr$Species == "BLG" | 
                            WAE.corr$Species == "RKB" | WAE.corr$Species == "HSF" | WAE.corr$Species == "SMB", "Centrarchid",  "Other"))

WAE.biplot <- ggplot(data = WAE.corr.grp, aes(x = env, y = res))+
  geom_hline(yintercept = 0, color = "gray70")+
  geom_vline(xintercept = 0, color = "gray70")+
  # annotate(geom = "text", x = -0.88, y = 1, label = "Environment (-)\nResidual (+)", fontface = "bold")+
  # annotate(geom = "text", x = -0.88, y = -0.97, label = "Environment (-)\nResidual (-)", fontface = "bold")+
  # annotate(geom = "text", x = 0.88, y = 1, label = "Environment (+)\nResidual (+)", fontface = "bold")+
  # annotate(geom = "text", x = 0.88, y = -0.97, label = "Environment (+)\nResidual (-)", fontface = "bold")+
  annotate(geom = "text", x = -0.95, y = 0.99, label = "Env (-)\nRes (+)", fontface = "bold")+
  annotate(geom = "text", x = -0.95, y = -0.97, label = "Env (-)\nRes (-)", fontface = "bold")+
  annotate(geom = "text", x = 0.95, y = 0.99, label = "Env (+)\nRes (+)", fontface = "bold")+
  annotate(geom = "text", x = 0.95, y = -0.97, label = "Env (+)\nRes (-)", fontface = "bold")+
  geom_point(size = 3, aes(color = group2))+
  scale_color_manual(values = c("#882255", "gray80"))+
  geom_text_repel(aes(label = Species), size = 3.5, force = 0.2, box.padding = 0.3, min.segment.length = 0, segment.size = 0.25) +
  labs(x = "Environmental Correlation", y = "Residual Correlation", color = NULL)+
  scale_y_continuous(limits = c(-1,1))+
  scale_x_continuous(limits = c(-1,1))+
  theme_classic(base_size = 11)+
  theme(legend.position = "bottom")+
  facet_wrap(~fct_rev(highlight.spp), nrow = 2)+
  theme(strip.text = element_text(face = "bold", size = 11, color = "black"),
        strip.background = element_rect(linewidth = 0.5, color = "black"))
WAE.biplot

# ggsave(filename = "Walleye_env_res_biplot.png", plot = WAE.biplot, width = 6.5, height = 6.5, units = "in", dpi = 600)
# ggsave(filename = "Walleye_env_res_biplot.svg", plot = WAE.biplot, width = 6.5, height = 6.5, units = "in", dpi = 600)


#make one for LMB
#make a dataframe of the walleye correlations to all other species
LMB.env <- as.data.frame(Env) %>%
  select(LMB) %>%
  rename(env = LMB)
LMB.res <- as.data.frame(Theta) %>%
  select(LMB)%>%
  rename(res = LMB)
LMB.corr <- cbind(LMB.env, LMB.res) %>%
  mutate(Species = rownames(LMB.env),
         highlight.spp = "Largemouth Bass",
         res = ifelse(Species == "LMB", NA, res),
         env = ifelse(Species == "LMB", NA, env))

# 
LMB.biplot <- ggplot(data = LMB.corr, aes(x = env, y = res))+
  geom_hline(yintercept = 0, color = "gray70")+
  geom_vline(xintercept = 0, color = "gray70")+
  annotate(geom = "text", x = -0.88, y = 1, label = "Environment (-)\nResidual (+)", fontface = "bold")+
  annotate(geom = "text", x = -0.88, y = -0.97, label = "Environment (-)\nResidual (-)", fontface = "bold")+
  annotate(geom = "text", x = 0.88, y = 1, label = "Environment (+)\nResidual (+)", fontface = "bold")+
  annotate(geom = "text", x = 0.88, y = -0.97, label = "Environment (+)\nResidual (-)", fontface = "bold")+
  geom_point(size = 3)+
  geom_text_repel(aes(label = Species), size = 3.5, force = 0.2, box.padding = 0.2) +
  labs(x = "Environmental Correlation", y = "Residual Correlation")+
  scale_y_continuous(limits = c(-1,1))+
  scale_x_continuous(limits = c(-1,1))+
  theme_classic(base_size = 11)
LMB.biplot
# 
# #ggsave(filename = "LMB_env_res_biplot.png", plot = LMB.biplot, width = 6.5, height = 6.5, units = "in", dpi = 600)
# #ggsave(filename = "LMB_env_res_biplot.svg", plot = LMB.biplot, width = 6.5, height = 6.5, units = "in", dpi = 600)

#try a yellow perch version
#make a dataframe of the walleye correlations to all other species 
YEP.env <- as.data.frame(Env) %>% 
  select(YEP) %>% 
  rename(env = YEP)
YEP.res <- as.data.frame(Theta) %>% 
  select(YEP)%>% 
  rename(res = YEP)
YEP.corr <- cbind(YEP.env, YEP.res) %>% 
  mutate(Species = rownames(YEP.env),
         highlight.spp = "Yellow Perch",
         res = ifelse(Species == "YEP", NA, res),
         env = ifelse(Species == "YEP", NA, env))

#make a centrchid color key
YEP.corr.grp <- YEP.corr %>% 
  mutate(group2 = ifelse(YEP.corr$Species == "LMB" | YEP.corr$Species == "BLC" | YEP.corr$Species == "PMK" | YEP.corr$Species == "BLG" | 
                           YEP.corr$Species == "RKB" | YEP.corr$Species == "HSF" | YEP.corr$Species == "SMB", "Centrarchid",  "Other"))

YEP.biplot <- ggplot(data = YEP.corr.grp, aes(x = env, y = res))+
  geom_hline(yintercept = 0, color = "gray70")+
  geom_vline(xintercept = 0, color = "gray70")+
  # annotate(geom = "text", x = -0.88, y = 1, label = "Environment (-)\nResidual (+)", fontface = "bold")+
  # annotate(geom = "text", x = -0.88, y = -0.97, label = "Environment (-)\nResidual (-)", fontface = "bold")+
  # annotate(geom = "text", x = 0.88, y = 1, label = "Environment (+)\nResidual (+)", fontface = "bold")+
  # annotate(geom = "text", x = 0.88, y = -0.97, label = "Environment (+)\nResidual (-)", fontface = "bold")+
  annotate(geom = "text", x = -0.95, y = 0.99, label = "Env (-)\nRes (+)", fontface = "bold")+
  annotate(geom = "text", x = -0.95, y = -0.97, label = "Env (-)\nRes (-)", fontface = "bold")+
  annotate(geom = "text", x = 0.95, y = 0.99, label = "Env (+)\nRes (+)", fontface = "bold")+
  annotate(geom = "text", x = 0.95, y = -0.97, label = "Env (+)\nRes (-)", fontface = "bold")+
  geom_point(size = 3, aes(color = group2))+
  scale_color_manual(values = c("#882255", "gray80"))+
  geom_text_repel(aes(label = Species), size = 3.5, force = 0.2, box.padding = 0.3, min.segment.length = 0, segment.size = 0.25) +
  labs(x = "Environmental Correlation", y = "Residual Correlation", color = NULL)+
  scale_y_continuous(limits = c(-1,1))+
  scale_x_continuous(limits = c(-1,1))+
  theme_classic(base_size = 11)+
  theme(legend.position = "bottom")+
  facet_wrap(~fct_rev(highlight.spp), nrow = 2)+
  theme(strip.text = element_text(face = "bold", size = 11, color = "black"),
        strip.background = element_rect(linewidth = 0.5, color = "black"))
YEP.biplot

#ggsave(filename = "YEP_env_res_biplot.png", plot = YEP.biplot, width = 6.5, height = 6.5, units = "in", dpi = 600)
#ggsave(filename = "YEP_env_res_biplot.svg", plot = YEP.biplot, width = 6.5, height = 6.5, units = "in", dpi = 600)




#make one with both combined
WAE.LMB.corr <- rbind(WAE.corr, LMB.corr)

# WAE.LMB.biplot <- ggplot(data = WAE.LMB.corr, aes(x = env, y = res))+
#   geom_hline(yintercept = 0, color = "gray70")+
#   geom_vline(xintercept = 0, color = "gray70")+
#   annotate(geom = "text", x = -0.95, y = 0.99, label = "Env (-)\nRes (+)", fontface = "bold", size = 3.2)+
#   annotate(geom = "text", x = -0.95, y = -0.97, label = "Env (-)\nRes (-)", fontface = "bold", size = 3.2)+
#   annotate(geom = "text", x = 0.95, y = 0.99, label = "Env (+)\nRes (+)", fontface = "bold", size = 3.2)+
#   annotate(geom = "text", x = 0.95, y = -0.97, label = "Env (+)\nRes (-)", fontface = "bold", size = 3.2)+
#   geom_point(size = 2)+
#   geom_text_repel(aes(label = Species), size = 3, force = 0.2, box.padding = 0.2) +
#   labs(x = "Environmental Correlation", y = "Residual Correlation")+
#   scale_y_continuous(limits = c(-1,1))+
#   scale_x_continuous(limits = c(-1,1))+
#   theme_classic(base_size = 11)+
#   facet_wrap(~fct_rev(highlight.spp), nrow = 2)+
#   theme(strip.text = element_text(face = "bold", size = 11, color = "black"),
#         strip.background = element_rect(linewidth = 0.5, color = "black"))
# WAE.LMB.biplot
# 
# #ggsave(filename = "WAE_LMB_env_res_biplot.png", plot = WAE.LMB.biplot, width = 5, height = 8, units = "in", dpi = 600)
# #ggsave(filename = "WAE_LMB_env_res_biplot.svg", plot = WAE.LMB.biplot, width = 5, height = 8, units = "in", dpi = 600)


#make one with both combined but JUST RELATIONSHIPS WITH ZOOPS
WAE.LMB.corr.grp <- WAE.LMB.corr %>% 
  rename(abbrv_names = Species)
WAE.LMB.corr.grp <- left_join(WAE.LMB.corr.grp, master.names, by = "abbrv_names")
WAE.LMB.corr.zoop <- WAE.LMB.corr.grp %>% 
  filter(group == "Zooplankton")

WAE.LMB.biplot.zoop <- ggplot(data = WAE.LMB.corr.zoop, aes(x = env, y = res))+
  geom_hline(yintercept = 0, color = "gray70")+
  geom_vline(xintercept = 0, color = "gray70")+
  annotate(geom = "text", x = -0.95, y = 0.99, label = "Env (-)\nRes (+)", fontface = "bold", size = 3.2)+
  annotate(geom = "text", x = -0.95, y = -0.97, label = "Env (-)\nRes (-)", fontface = "bold", size = 3.2)+
  annotate(geom = "text", x = 0.95, y = 0.99, label = "Env (+)\nRes (+)", fontface = "bold", size = 3.2)+
  annotate(geom = "text", x = 0.95, y = -0.97, label = "Env (+)\nRes (-)", fontface = "bold", size = 3.2)+
  geom_point(size = 2, alpha = 0.5)+
  geom_text_repel(aes(label = abbrv_names), size = 3, force = 0.2, box.padding = 0.3, min.segment.length = 0, segment.size = 0.25) +
  labs(x = "Environmental Correlation", y = "Residual Correlation")+
  scale_y_continuous(limits = c(-1,1))+
  scale_x_continuous(limits = c(-1,1))+
  theme_classic(base_size = 11)+
  facet_wrap(~fct_rev(highlight.spp), nrow = 2)+
  theme(strip.text = element_text(face = "bold", size = 11, color = "black"),
        strip.background = element_rect(linewidth = 0.5, color = "black"))
WAE.LMB.biplot.zoop

# ggsave(filename = "WAE_LMB_env_res_biplot_zoop.png", plot = WAE.LMB.biplot.zoop, width = 5, height = 8, units = "in", dpi = 600)
# ggsave(filename = "WAE_LMB_env_res_biplot_zoop.svg", plot = WAE.LMB.biplot.zoop, width = 5, height = 8, units = "in", dpi = 600)



#SITE MAPS---------------------------------------------------------


#import lake centroid coordinates - I saved them with the precipitation data
coords <- read.csv("Data/Input/Contemp_Lake_Centroid_Coords.csv")
#extract list of lakes in final model
lake_incl <- names(model[["params"]][["row.params.random"]])
#replaces the periods with spaces in lake names
lake_incl <- gsub(".", " ", lake_incl, fixed = TRUE)
#select only the lake name and coordinate columns, and filter the rows to just lakes included in final model
model_lake_coords <- coords %>% 
  filter(lake_name %in% lake_incl)
#convert the sites to an sf object
study_lakes_sf <- st_as_sf(model_lake_coords, coords = c("Longitude", "Latitude"), crs = 4326) #these coordinates are in wgs94
#transform coordinates to NAD83 / UTM zone 15N for Minnesota
study_lakes_sf_15N <- st_transform(study_lakes_sf, crs = 26915)

#get Minnesota boundary
minnesota <- st_as_sf(maps::map("state", "minnesota", plot = FALSE, fill = TRUE))

#get usa boundary
usa <- st_as_sf(maps::map("usa", plot = FALSE, fill = TRUE))


#MAP RANDOM EFFECTS
#extract random effects from model
rand.lake <- data.frame(Rand_Effect_Value = coef(model, "row.params.random"), lake_name = names(coef(model, "row.params.random")))
#replaces the periods with spaces in lake names
rand.lake$lake_name <- gsub(".", " ", rand.lake$lake_name, fixed = TRUE)
#join random effects to lake coordinate sf 
study_lakes_sf_rand_lake <- left_join(study_lakes_sf, rand.lake, by = "lake_name") %>% 
  rename(`Lake Random Intercept` = Rand_Effect_Value)

#get colors
puor_colors <- rev(RColorBrewer::brewer.pal(11, "PuOr"))

rand_lake_map <- ggplot()+
  geom_sf(data = minnesota, fill = "white")+
  geom_sf(data = study_lakes_sf_rand_lake, shape = 21, alpha = 0.9, color = "black", aes(fill = `Lake Random Intercept`), size = 3, )+
  scale_fill_gradientn(colors = puor_colors, values = scales:: rescale(c(-1.6,0,1.2)), limits = c(-1.6,1.2), , oob = scales::squish, breaks = c(-1.6, 0, 1.2), labels = c("-1.6", "0", "1.2"))+
  #scale_fill_viridis_c(option = "inferno", direction = 1)+
  coord_sf(crs = 26915)+ #this projection is NAD83 / UTM zone 15N for Minnesota
  theme_void()+
  annotation_scale(location = "tr", width_hint = 0.25, style = "ticks", pad_x = unit(1, "in"), pad_y = unit(0.7, "in"), text_cex = 1)+
  annotation_north_arrow(location = "tr", which_north = "grid", style = north_arrow_fancy_orienteering(), pad_x = unit(0.4, "in"), pad_y = unit(0.55, "in"), height = unit(1, "cm"), width = unit(1, "cm"))+
  labs(fill = "Lake\nRandom\nIntercept")+
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12),
        legend.margin = margin(t = 10, r = 15, b = -100, l = -100, unit = "pt"),)
rand_lake_map
# ggsave(filename = "random_effect_map.png", plot = rand_lake_map, width = 7, height = 7, units = "in", dpi = 600)
# ggsave(filename = "random_effect_map.svg", plot = rand_lake_map, width = 7, height = 7, units = "in", dpi = 600)


# #a version with lake names labeled- not updated with the final formatting and colors
# rand_lake_map_lab <- ggplot()+
#   geom_sf(data = minnesota, fill = "white")+
#   geom_sf(data = study_lakes_sf_rand_lake, shape = 21, alpha = 0.9, color = "black", aes(fill = `Lake Random Intercept`), size = 3, )+
#   scale_fill_distiller(palette = "RdBu", direction = 1, limits = c(-1,1), , oob = scales::squish, breaks = c(-1, 0, 1), labels = c("<-1", "0", ">1"))+
#   geom_text_repel(data = study_lakes_sf_rand_lake, aes(label = lake_name, geometry = geometry), stat = "sf_coordinates")+
#   coord_sf(crs = 26915)+ #this projection is NAD83 / UTM zone 15N for Minnesota
#   theme_void()+
#   annotation_scale(location = "tr", width_hint = 0.25, style = "ticks", pad_x = unit(1, "in"), pad_y = unit(0.7, "in"), text_cex = 1)+
#   annotation_north_arrow(location = "tr", which_north = "grid", style = north_arrow_fancy_orienteering(), pad_x = unit(0.4, "in"), pad_y = unit(0.55, "in"), height = unit(1, "cm"), width = unit(1, "cm"))+
#   labs(fill = "Lake\nRandom\nIntercept")+
#   theme(legend.title = element_text(size = 12, face = "bold"),
#         legend.text = element_text(size = 12),
#         legend.margin = margin(t = 10, r = 15, b = -100, l = -100, unit = "pt"),)
# rand_lake_map_lab
# #ggsave(filename = "random_effect_map_labeled.png", plot = rand_lake_map_lab, width = 7, height = 7, units = "in", dpi = 600)
# #ggsave(filename = "random_effect_map_labeled.svg", plot = rand_lake_map_lab, width = 7, height = 7, units = "in", dpi = 600)




#MAKE STUDY REGION PLOT

#plot MN in USA
mn_in_us <- ggplot()+
  geom_sf(data = usa, fill = "white")+
  geom_sf(data = minnesota, fill = "darkgray")+
  coord_sf(crs = 5070)+ #this projection iss NAD83 / COnus Albers which is typically used for plotting the US
  theme_void()
  #annotation_scale(location = "bl", width_hint = 0.25, style = "ticks", pad_x = unit(0.5, "in"))+
  #annotation_north_arrow(location = "bl", which_north = "grid", style = north_arrow_fancy_orienteering(), pad_x = unit(0.75, "in"), pad_y = unit(0.25, "in"))
  
mn_in_us


#plot sites in mn
#plot MN in USA
sites_in_mn <- ggplot()+
  geom_sf(data = minnesota, fill = "darkgray")+
  geom_sf(data = study_lakes_sf, shape = 21, alpha = 0.7, color = "black", fill = "darkblue", size = 3, )+
  coord_sf(crs = 26915)+ #this projection is NAD83 / UTM zone 15N for Minnesota
  theme_void()+
  annotation_scale(location = "tr", width_hint = 0.25, style = "ticks", pad_x = unit(0.6, "in"), pad_y = unit(0.7, "in"), text_cex = 1)+
  annotation_north_arrow(location = "tr", which_north = "grid", style = north_arrow_fancy_orienteering(), pad_x = unit(0.1, "in"), pad_y = unit(0.55, "in"), height = unit(1, "cm"), width = unit(1, "cm"))
sites_in_mn
#plot them together with US map as inset
both_maps <- ggdraw()+
  draw_plot(sites_in_mn, x = 0, y = 0, width = 1, height = 0.9)+
  draw_plot(mn_in_us, x = 0.65, y = 0.18, width = 0.30, height = 0.30)
both_maps
#export map
# ggsave(filename = "map.png", plot = both_maps, width = 7, height = 7, units = "in", dpi = 600)
# ggsave(filename = "map.svg", plot = both_maps, width = 7, height = 7, units = "in", dpi = 600)

#again but label the lakes
sites_in_mn_lab <- ggplot()+
  geom_sf(data = minnesota, fill = "darkgray")+
  geom_sf(data = study_lakes_sf, shape = 21, alpha = 0.7, color = "black", fill = "darkblue", size = 3, )+
  coord_sf(crs = 26915)+ #this projection is NAD83 / UTM zone 15N for Minnesota
  geom_text_repel(data = study_lakes_sf_rand_lake, aes(label = lake_name, geometry = geometry), stat = "sf_coordinates")+
  theme_void()+
  annotation_scale(location = "tr", width_hint = 0.25, style = "ticks", pad_x = unit(0.6, "in"), pad_y = unit(0.7, "in"), text_cex = 1)+
  annotation_north_arrow(location = "tr", which_north = "grid", style = north_arrow_fancy_orienteering(), pad_x = unit(0.1, "in"), pad_y = unit(0.55, "in"), height = unit(1, "cm"), width = unit(1, "cm"))
sites_in_mn_lab
#plot them together with US map as inset
both_maps_lab <- ggdraw()+
  draw_plot(sites_in_mn_lab, x = 0, y = 0, width = 1, height = 0.9)+
  draw_plot(mn_in_us, x = 0.65, y = 0.18, width = 0.30, height = 0.30)
both_maps_lab
#export map
# ggsave(filename = "map_labeled.png", plot = both_maps_lab, width = 7, height = 7, units = "in", dpi = 600)
# ggsave(filename = "map_labeled.svg", plot = both_maps_lab, width = 7, height = 7, units = "in", dpi = 600)




#Map the other covariates - raw values #GET THIS UNSTANDARDIZED
#take average across years for lakes with multiple years of data, max for SWF and ZM to so if ever invaded they come out as yes
#extract covariate data from model
x <- read.csv("Data/Input/gllvm_x_matrix_raw_Pub_Rev.csv")
#average repeated measures on each lake
x_lake_avg <- x %>% 
  group_by(lake_name) %>% 
  summarize(CDOM = mean(cdom),
            `Lake Area` = log(mean(area)),
            `Maximum Depth` = mean(depth.max),
            Secchi = mean(secchi.mean),
            `Degree Days` = mean(gdd.mean),
            `Mean Depth` = (mean(depth.mean)),
            `Annual Precipitation` = mean(precip.mean),
            `Walleye Stocking` = max(wae.stock),
            `Zebra Mussel Presence` = max(zm),
            .groups = 'drop') %>% 
  mutate(`Walleye Stocking` = ifelse(`Walleye Stocking` == 1, "yes", "no"),
         `Zebra Mussel Presence` = ifelse(`Zebra Mussel Presence` == 1, "yes", "no"))
#join to spatial point data
study_lakes_sf_x <- left_join(study_lakes_sf_rand_lake, x_lake_avg, by = "lake_name")

#Make maps of each covariate
CDOM_map <- ggplot()+
  geom_sf(data = minnesota, fill = "white")+
  geom_sf(data = study_lakes_sf_x, shape = 21, alpha = 0.9, color = "black", aes(fill = CDOM), size = 2)+
  scale_fill_viridis_c(option = "inferno", direction = 1)+
  labs(fill = "CDOM\n(a440/m)")+
  coord_sf(crs = 26915)+ #this projection is NAD83 / UTM zone 15N for Minnesota
  theme_void()
CDOM_map


area_map <- ggplot()+
  geom_sf(data = minnesota, fill = "white")+
  geom_sf(data = study_lakes_sf_x, shape = 21, alpha = 0.9, color = "black", aes(fill = `Lake Area`), size = 2)+
  scale_fill_viridis_c(option = "inferno", direction = 1)+
  labs(fill = "Lake\nArea\n(log(ha))")+
  coord_sf(crs = 26915)+ #this projection is NAD83 / UTM zone 15N for Minnesota
  theme_void()
area_map

depth_map <- ggplot()+
  geom_sf(data = minnesota, fill = "white")+
  geom_sf(data = study_lakes_sf_x, shape = 21, alpha = 0.9, color = "black", aes(fill = `Maximum Depth`), size = 2)+
  scale_fill_viridis_c(option = "inferno", direction = 1)+
  labs(fill = "Maximum\nDepth\n(m)")+
  coord_sf(crs = 26915)+ #this projection is NAD83 / UTM zone 15N for Minnesota
  theme_void()
depth_map

secchi_map <- ggplot()+
  geom_sf(data = minnesota, fill = "white")+
  geom_sf(data = study_lakes_sf_x, shape = 21, alpha = 0.9, color = "black", aes(fill = Secchi), size = 2)+
  scale_fill_viridis_c(option = "inferno", direction = 1)+
  labs(fill = "Secchi\nDepth\n(m)")+
  coord_sf(crs = 26915)+ #this projection is NAD83 / UTM zone 15N for Minnesota
  theme_void()
secchi_map

dd_map <- ggplot()+
  geom_sf(data = minnesota, fill = "white")+
  geom_sf(data = study_lakes_sf_x, shape = 21, alpha = 0.9, color = "black", aes(fill = `Degree Days`), size = 2)+
  scale_fill_viridis_c(option = "inferno", direction = 1)+
  labs(fill = "Degree\nDays")+
  coord_sf(crs = 26915)+ #this projection is NAD83 / UTM zone 15N for Minnesota
  theme_void()
dd_map

mean_depth_map <- ggplot()+
  geom_sf(data = minnesota, fill = "white")+
  geom_sf(data = study_lakes_sf_x, shape = 21, alpha = 0.9, color = "black", aes(fill = `Mean Depth`), size = 2)+
  scale_fill_viridis_c(option = "inferno", direction = 1)+
  labs(fill = "Mean\nDepth)")+
  coord_sf(crs = 26915)+ #this projection is NAD83 / UTM zone 15N for Minnesota
  theme_void()
mean_depth_map

precip_map <- ggplot()+
  geom_sf(data = minnesota, fill = "white")+
  geom_sf(data = study_lakes_sf_x, shape = 21, alpha = 0.9, color = "black", aes(fill = `Annual Precipitation`), size = 2)+
  scale_fill_viridis_c(option = "inferno", direction = 1)+
  labs(fill = "Annual\nPrecipitation\n(mm)")+
  coord_sf(crs = 26915)+ #this projection is NAD83 / UTM zone 15N for Minnesota
  theme_void()
precip_map

wae_map <- ggplot()+
  geom_sf(data = minnesota, fill = "white")+
  geom_sf(data = study_lakes_sf_x, shape = 21, alpha = 0.9, color = "black", aes(fill = `Walleye Stocking`), size = 2)+
  scale_fill_manual(values = c("yes" = "#FCA50A", "no" = "#000004"))+
  labs(fill = "Walleye\nStocking")+
  guides(fill = guide_legend(reverse = TRUE))+
  coord_sf(crs = 26915)+ #this projection is NAD83 / UTM zone 15N for Minnesota
  theme_void()
wae_map

zm_map <- ggplot()+
  geom_sf(data = minnesota, fill = "white")+
  geom_sf(data = study_lakes_sf_x, shape = 21, alpha = 0.9, color = "black", aes(fill = `Zebra Mussel Presence`), size = 2)+
  scale_fill_manual(values = c("yes" = "#FCA50A", "no" = "#000004"))+
  labs(fill = "Zebra\nMussel\nPresence")+
  guides(fill = guide_legend(reverse = TRUE))+
  coord_sf(crs = 26915)+ #this projection is NAD83 / UTM zone 15N for Minnesota
  theme_void()
zm_map

#combine maps in 3x3 grid
cov_maps <- wrap_plots(precip_map, CDOM_map, dd_map, area_map, depth_map, mean_depth_map, secchi_map, wae_map, zm_map, ncol = 3, nrow = 3)
#add letter labels
cov_maps_labels <- cov_maps +
  plot_annotation(tag_levels = 'A')

#adjust legends so they fit in layout and adjust labels
cov_maps_legends <- cov_maps_labels & 
  coord_sf() & 
  theme(
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.25, "cm"),
    legend.margin = margin(t = 10, r = 15, b = -25, l = -35, unit = "pt"),
    plot.tag = element_text(size = 12, face = "bold"),
    plot.tag.position = c(0.12, 0.83)
  )
cov_maps_legends
# ggsave(filename = "covariate_maps.png", plot = cov_maps_legends, width = 7, height = 7, units = "in", dpi = 600)
# ggsave(filename = "covariate_maps.svg", plot = cov_maps_legends, width = 7, height = 7, units = "in", dpi = 600)




#LAKE-YEAR COVERAGE--------------------------
#read in the covariate data
#I already did this above but have the code again here in case I'm just running this plot
x <- read.csv("Data/Input/gllvm_x_matrix_raw_Pub_Rev.csv")

lakeyear_plot <- ggplot(data = x, aes(x = year, y = lake_name))+
  geom_vline(xintercept = (1999:2024)-0.5, color = "gray95", linewidth = 0.5)+
  geom_tile(color = "#332288", fill = "#332288", linewidth = 0.5)+
  geom_hline(yintercept = (1:34)-0.5, color = "gray95", linewidth = 0.5)+
  scale_x_continuous(breaks = seq(2000, 2024, by = 5), minor_breaks = 1999:2024, guide = guide_axis_base(key = key_minor()), expand = c(0,0))+
  scale_y_discrete(limits = rev)+
  labs(x = "Year", y = "Lake")+
  theme_classic(base_size = 11)
lakeyear_plot
# ggsave(filename = "lakeyears.png", plot = lakeyear_plot, width = 7, height = 5, units = "in", dpi = 600)
# ggsave(filename = "lakeyears.svg", plot = lakeyear_plot, width = 7, height = 5, units = "in", dpi = 600)



#WAE v LMB STATEWIDE CONTEXT---------------------------------------------
# #get the walleye and lmb data from entire state - just do this once and save the csv
# #plot wae vs. lmb on scale of entire MN fish database
# #Import all MN data with good fish surveys
# #this finds the minnesota arrow file that contains all the Minnesota fish data - this is what Denver updated in Nov 2025, only has MN data
# library(lubridate)
# library(arrow)
# mn_data <- open_dataset("E:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Fish Survey Data/Parquet files/mn_update/part-0.parquet")
# 
# good.fish.surveys <- mn_data %>%
#   filter((sampling_method == "gill_net_standard" |
#             sampling_method == "gill_net_stratified_deep" |
#             sampling_method =="gill_net_stratified_shallow" ) &
#            (survey_type == "Standard Survey" |
#               survey_type == "Population Assessment"|
#               survey_type == "Re-Survey"|
#               survey_type == "Large Lake Survey"|
#               survey_type == "Initial Survey" |
#               survey_type == "Special Assessment" | #will individually investigate if I can use these special assessments IF they match to zoop data
#               survey_type == "Targeted Survey")) %>% #will individually investigate if I can use these targeted surveys IF they match to zoop data
#   distinct(lake_id,
#            lake_name,
#            year,
#            month, #this is generated from "date_total_effort_ident" so it gives me the correct month that the fishing actually started with my specified gear 
#            total_effort_ident,
#            total_effort_1,
#            sampling_method_simple,
#            sampling_method,
#            survey_type,
#            nhdhr_id,
#            flag) %>%
#   collect()
# #collect actually brings data into R
# 
# #fix Crane lake nhdhr_id (wrong in fish database - Denver is fixing it but I will do this for now) - need this to join to LAGOS data
# good.fish.surveys <- good.fish.surveys %>% 
#   mutate(nhdhr_id = ifelse(nhdhr_id == "nhdhr_{E940A362-4076-4895-A23F-1B8CCC905DEE}", "nhdhr_105953135", nhdhr_id))
# 
# #go get the fish
# all_fish <- mn_data %>% 
#   right_join(good.fish.surveys, by = c("total_effort_ident")) %>% 
#   collect()
# 
# #need to calculate combined effort of stratified surveys 
# #separate out shallow + deep stratified surveys from the standard gillnet surveys
# stratified <- all_fish %>% 
#   filter(sampling_method.x == "gill_net_stratified_shallow" | sampling_method.x == "gill_net_stratified_deep")
# 
# #get one row for each lake/year/gillnet type
# stratified_surveys <- stratified %>%
#   group_by(lake_id.x, lake_name.x, year.x, sampling_method.x) %>%
#   summarize(total_effort_1.x = first(total_effort_1.x), .groups = 'drop')
# 
# #add effort from shallow + deep stratified surveys
# combined_stratified_effort <- stratified_surveys %>%
#   group_by(lake_id.x, year.x) %>%
#   summarize(total_effort_cse = sum(total_effort_1.x), .groups = 'drop')
# 
# #join this sum back to the stratified data
# stratified_sum <- left_join(stratified, combined_stratified_effort, by = c("lake_id.x", "year.x"))
# 
# #remove original effort column 
# stratified_sum_order <- stratified_sum %>% 
#   select(-total_effort_1.x) %>% 
#   rename(total_effort_1.x = total_effort_cse) %>% #rename the new combined effort to match the rest of the data
#   relocate(total_effort_1.x, .after = total_effort_ident) #reorder to put effort column back in original location
# 
# #remove the stratified rows from the original dataset
# fish_no_strat <- all_fish %>% 
#   filter(sampling_method.x != "gill_net_stratified_shallow" & sampling_method.x != "gill_net_stratified_deep")
# 
# #now paste the new rows back in with the updated effort - now my cpue calculations will be correct
# fish_effort_corrected <- rbind(fish_no_strat, stratified_sum_order)
# 
# 
# #calculate walleye and bass cpue by lake-year
# all.fish.cpue <- fish_effort_corrected %>% 
#   group_by(lake_id.x, year.x) %>% 
#   mutate(WAE.count = sum(species_1 == "walleye"), 
#          WAE.CPUE = WAE.count/total_effort_1.x,
#          LMB.count = sum(species_1 == "largemouth_bass"),
#          LMB.CPUE = LMB.count/total_effort_1.x,
#   ) %>% 
#   distinct(lake_id.x, year.x, .keep_all = TRUE) %>% #keep only one row per lake/year
#   select(lake_name.x, lake_id.x, year.x, WAE.CPUE, LMB.CPUE) %>% #select only relevant columns
#   mutate(type = "All MN") %>%  #add an data type ID column
#   rename(lake_name = lake_name.x,
#          parentdow = lake_id.x,
#          Year = year.x)
# 
# #save as a .csv file
# #write.csv(all.fish.cpue, file = "Data/Input/All_MN_WAE_LMB_CPUE.csv", row.names = FALSE)

#START HERE TO READ IN THE STATEWIDE FISH DATA CONTEXT THAT I PULLED ABOVE ON 6/16/2026
mn_fish <- read.csv("Data/Input/All_MN_WAE_LMB_CPUE.csv") %>% 
  select(-parentdow)

#extract the wae and lmb cpue that went into the model and format to join
model_fish <- as.data.frame(model$y) %>% 
  select(Walleye, `Largemouth Bass`) %>% 
  mutate(lake_name = substr(rownames(model$y),1, nchar(rownames(model$y)) - 4),
         Year = substr(rownames(model$y), nchar(rownames(model$y))-3, nchar(rownames(model$y))),
         type = "model") %>% 
  rename(WAE.CPUE = Walleye,
         LMB.CPUE = `Largemouth Bass`) %>% 
  select(lake_name, Year, WAE.CPUE, LMB.CPUE, type)
  

#rbind the minnesota and sample data
MN.plot.data <- rbind(mn_fish, model_fish)


#plot this on log scae
wae.lmb.allMN.log <- ggplot(data = MN.plot.data, aes(x = WAE.CPUE, y = LMB.CPUE, color = type))+
  geom_point(alpha = 0.8, size = 1.5)+
  scale_color_manual(values = c("gray", "#332288"), labels = c("All Minnesota", "GLLVM Model"))+
  theme_classic()+
  scale_x_log10(labels = scales::comma, name = expression(log[10]*"(Walleye CPUE)")) + 
  scale_y_log10(labels = scales::comma, name = expression(log[10]*"(Largemouth Bass CPUE)"))+
  coord_cartesian(clip = "off")+
  labs(color = NULL)+
  theme(legend.position = c(0.15, 0.9),
        legend.background = element_rect(fill = "white", color = "black"))
fish.context.plot <- ggMarginal(wae.lmb.allMN.log, type = "density", groupColor = TRUE, groupFill = TRUE)
fish.context.plot
# ggsave(filename = "fish_context.png", plot = fish.context.plot, width = 7, height = 5, units = "in", dpi = 600)



#TEMP CLARITY CONTEXT-----------------------------------------

#I ran this temp extraction code once and saved the GDD result
#also combined the calculated gdd with secchi and saved that so I can start there in the future to modify this figure

# #read in and format secchi data
# secchi.data <- read.csv("Data/Input/WQP_1998-2025_Secchi_20251124_FILTERED_FORMATTED.csv") %>% 
#   #get rid of any 0 and Na values
#   filter(secchi_meters != 0 & !is.na(secchi_meters)) %>% 
#   #filter only July to Sept data
#   filter(month == "7" | month == "8" | month == "9") %>% 
#   #summarize the mean of the selected secchi data for each lake/year
#   group_by(parentdow, year) %>%
#   summarize(secchi.meters.MPCA.Jul.to.Sept = mean(secchi_meters), .groups = 'drop') %>% 
#   #make parentdow character
#   mutate(parentdow = as.character(parentdow))
# 
# 
# 
# #get daymet temp data on all lakes with LAGOS centroid coordinates from 1999 to 2024
# library(daymetr) #allows access to the data
# 
# #get centroid coordinates
# Locus.info <- read.csv("Data/Input/lake_information.csv")
# Locus.centroids <- Locus.info %>% 
#   select(lagoslakeid,
#          lake_nhdid,
#          lake_lat_decdeg,
#          lake_lon_decdeg)
# 
# #read in crosswalk that Denver made from fish database
# crosswalk <- read.csv("Data/Input/dow_nhdhr_fish_lakes.csv")
# #create a parentdow column in crosswalk to match my inclusion table (no leading zeroes included here)
# cw.parentdow <- crosswalk %>%
#   mutate(parentdow = case_when(
#     (crosswalk$lake_id == "1014202" | crosswalk$lake_id == "1014201" | crosswalk$lake_id == "4003502" | crosswalk$lake_id == "4003501") ~ substr(crosswalk$lake_id, 1, 7),   #takes care of North and Red lakes (7 characters)
#     (crosswalk$lake_id == "69037802" | crosswalk$lake_id == "69037801") ~ substr(crosswalk$lake_id, 1, 8),  #takes care of Vermilion (different because 8 characters)
#     (nchar(crosswalk$lake_id) == 7 & (crosswalk$lake_id != "01014202" & crosswalk$lake_id != "01014201" & crosswalk$lake_id != "04003502" & crosswalk$lake_id != "04003501" & crosswalk$lake_id != "69037802" & crosswalk$lake_id != "69037801")) ~ substr(crosswalk$lake_id, 1, 5), #this gets 5 digits from the DOWs that have 7 characters and are not those identified before
#     (nchar(crosswalk$lake_id) == 8 & (crosswalk$lake_id != "01014202" & crosswalk$lake_id != "01014201" & crosswalk$lake_id != "04003502" & crosswalk$lake_id != "04003501" & crosswalk$lake_id != "69037802" & crosswalk$lake_id != "69037801")) ~ substr(crosswalk$lake_id, 1, 6) #this gets 6 digits from the DOWs that have 8 characters and are not those identified before
#   )) %>% 
#   #get rid of "nhdhr_" in front of the nhdid's
#   mutate(nhdhr_id = str_sub(nhdhr_id, 7)) %>% 
#   rename(lake_nhdid = nhdhr_id) %>% 
#   select(-lake_id)
#   
# 
# #join parentdows to centroid coordinates and only save the lakes with parentdows
# centroid.parentdow <- left_join(Locus.centroids, cw.parentdow, by = "lake_nhdid") %>% 
#   filter(!is.na(parentdow))
# 
# #Write a function that pulls 1999-2024 data for one point
# get_daymet_data <- function(id, latitude, longitude) {
#   
#   temp_data <- download_daymet(
#     site = id,
#     lat = latitude,
#     lon = longitude,
#     start = 1999,
#     end = 2024,
#     internal = TRUE
#   )$data
#   
#   # Clean up and add the site name - other data gets downloaded but this just selects the temp data
#   temp_data %>%
#     mutate(
#       site = id,
#       date = as.Date(paste(year, yday, sep = "-"), format = "%Y-%j"),
#       tmax = tmax..deg.c.,
#       tmin = tmin..deg.c.
#     ) %>%
#     select(site, date, tmax, tmin)
# }
# 
# #run a loop that gets this data for my list of coordinates
# #pmap_dfr runs the function I made for each row and "binds" the results together
# temp_all <- pmap_dfr(list(centroid.parentdow$parentdow, centroid.parentdow$lake_lat_decdeg, centroid.parentdow$lake_lon_decdeg), get_daymet_data)
# 
# #rename site ID as parentdow
# temp_final <- temp_all %>% 
#   rename(parentdow = site)
# 
# #save this csv so I never have to wait for that to run again
# write.csv(temp_final, file = "Data/Output/All_MN_temp_Daymet_daily_air_temps.csv", row.names = FALSE)
# 
# #calcular promedio del temp para cada dia
# temp_final$tmean <- (temp_final$tmax + temp_final$tmin)/2
# 
# #calculate GDD for each day (base = 5 degrees C)
# temp_final$gdd.day.5c <- temp_final$tmean - 5
# 
# #sum the growing degree days for each lake in each year
#     #first have to isolate the year
#     temp_final$Year <- as.numeric(substr(temp_final$date, 1, 4))
#     
#     #drop gdd values that are negative
#     temp_positive <- temp_final %>% 
#       filter(gdd.day.5c > 0)
#     
#     #calculate the sum of the positive gdd thoughtout the year and make parentdow character for joins
#     temp_year <- temp_positive %>%
#       group_by(parentdow, Year) %>%
#       summarize(gdd.year.5c = sum(gdd.day.5c), .groups = 'drop') %>% 
#       mutate(parentdow = as.character(parentdow)) %>% 
#       rename(year = Year)
#     
#     
#     
# #combine the gdd and secchi data
# mn_temp_secchi <- full_join(secchi.data, temp_year, by = c("parentdow", "year"))
# 
# #save this
# write.csv(mn_temp_secchi, file = "Data/Output/All_MN_gdd5_secchi.csv", row.names = FALSE)

#START HERE to read in the temp and secchi data I collected for all of MN with available MPCA secchi and LAGOS centroid coordinate data
mn_temp_secchi <-read.csv("Data/Input/All_MN_gdd5_secchi.csv")

#filter out lakes that don't have data on both and add a type column to mark that these are MN context lakes
mn_temp_secchi_filter <- mn_temp_secchi %>% 
  filter(!is.na(secchi.meters.MPCA.Jul.to.Sept) & !is.na(gdd.year.5c)) %>% 
  mutate(type = "All MN") %>% 
  rename(Secchi = secchi.meters.MPCA.Jul.to.Sept,
         GDD = gdd.year.5c,
         lake = parentdow)

#raw covariate data that went into model (in a standardized form)
x <- read.csv("Data/Input/gllvm_x_matrix_raw_Pub_Rev.csv")
#format data from model to match
model_temp_secchi <- x %>% 
  select(lake_name, year, secchi.mean, secchi.dev, gdd.mean, gdd.dev) %>% 
  mutate(Secchi = secchi.mean + secchi.dev,
         GDD = gdd.mean + gdd.dev) %>% 
  select(-secchi.mean, -secchi.dev, -gdd.mean, -gdd.dev) %>% 
  rename(lake = lake_name) %>% 
  mutate(type = "model")
#I have lakes referred to with parentdows for all MN and names for the model but that's fine for this purpose

#rowbind these together
MN_WQ_plot_data <- rbind(mn_temp_secchi_filter, model_temp_secchi)

#plot!
wq.allMN <- ggplot(data = MN_WQ_plot_data, aes(x = GDD, y = Secchi, color = type))+
  geom_point(alpha = 0.8, size = 1.5)+
  scale_color_manual(values = c("gray", "#332288"), labels = c("All Minnesota", "GLLVM Model"))+
  theme_classic()+
  labs(color = NULL, y = "Secchi Depth (m)", x = "Degree Days (°C-days)")+
  theme(legend.position = c(0.85, 0.9),
        legend.background = element_rect(fill = "white", color = "black"))
wq.context.plot <- ggMarginal(wq.allMN, type = "density", groupColor = TRUE, groupFill = TRUE)
wq.context.plot
# ggsave(filename = "wq_context.png", plot = wq.context.plot, width = 7, height = 5, units = "in", dpi = 600)


#LAKE TABLE INFO ------------------------------------------
lakeyears <- read.csv("Data/Input/GLLVM_Complete_Dataset_Pub_Rev.csv")

#get a mean secchi and lake-year count for each one
lakes <- lakeyears %>% 
  group_by(lake_name, parentdow, lake_lat_decdeg, lake_lon_decdeg, depth.max.m, area_ha) %>% 
  summarize(mean_secchi_m = mean(secchi.meters.MPCA.Jul.to.Sept),
            mean_GDD = mean(gdd.year.5c),
            lakeyear_n = n(),
            .groups = "drop")


#VARIABLE TABLE INFO------------------------------------
#NOT UPDATED WITH NEW MODEL

#only making a table with variables I actually used
#isolate the columns - categorical and quantitative separately
env.var.quant <- lakeyears %>% 
  select(secchi.meters.MPCA.Jul.to.Sept, gdd.year.5c, precip_5yr_avg_mm, CDOM.lake.avg, area_ha, depth.max.m, photic_prop_secchi.meters.MPCA.Jul.to.Sept)

env.var.cat <- lakeyears %>% 
  select(SpinyWaterflea.yn, ZebraMussel.yn, lake_name, parentdow.year)

#how many lake-years and lakes for each invader
swf <- env.var.cat %>% 
  filter(SpinyWaterflea.yn == "yes")
(unique(swf$lake)) #10
(unique(swf$parentdow.year)) #64

zm <- env.var.cat %>% 
  filter(ZebraMussel.yn == "yes")
(unique(zm$lake)) #8
(unique(zm$parentdow.year)) #33



#calculate stats for the quantitative ones
env.var.quant.long <- pivot_longer(env.var.quant, cols = everything(), names_to = "variable", values_to = "value")
env.var.quant.stat <- env.var.quant.long %>% 
  group_by(variable) %>% 
  summarize(min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE),
            mean = mean(value, na.rm = TRUE),
            median = median(value, na.rm = TRUE),
            .groups = "drop")

#round things at the end
env.var.quant.round <- env.var.quant.stat %>% 
  mutate(across(where(is.numeric), ~ ifelse((variable == "area_ha" | variable == "depth.max.m" | variable == "precip_5yr_avg_mm" | variable == "gdd.year.5c"), round(.x, digits = 0), round(.x, digits = 2)))) %>% 
  #add a row with the labels you actually want
  mutate(name = c("CDOM", "Lake Area", "Maximum Depth", "Degree Days", "Littoral Zone", "Annual Precipitation", "Secchi Depth"))

#How many lakes in each drainage connectivity class?
con_class <- lakeyears %>% 
  group_by(lake_name) %>% 
  summarize(cat = first(lake_connectivity_class))

#how correlated are these?
library(GGally) #for ggpairs function
ggpairs(env.var.quant)


#SPECIES TABLE---------------------------------
#NOT UPDATED WITH NEW MODEL

#filter out just species data and remove species that are all zeroes in this dataset
spp <- lakeyears %>% 
  select(BIB.CPUE:nauplii) %>% 
  select(-GOE.CPUE, -QBS.CPUE, -TRP.CPUE) 

#pivot longer
spp.long <- pivot_longer(spp, cols = everything(), names_to = "species", values_to = "abundance")

#calculate stats
spp.stat <- spp.long %>% 
  group_by(species) %>% 
  summarize(min = min(na_if(abundance,0), na.rm = TRUE),
            max = max(na_if(abundance,0), na.rm = TRUE),
            median = median(na_if(abundance,0), na.rm = TRUE),
            lakeyear = sum(abundance > 0),
            .groups = "drop") %>% 
  #round values
  mutate(across(min:median, ~ round(.x, digits = 2)))

#how many distinct lakes are they in?

#get the max value for each species in each lake (will be 0 if never present)
lake.spp <- lakeyears %>% 
  select(-GOE.CPUE, -QBS.CPUE, -TRP.CPUE) %>% 
  group_by(lake_name) %>% 
  summarize(across(BIB.CPUE:nauplii, max),
            .groups = 'drop')
#for each species, count the number of lakes where it has a value greater than 0 in at least one year
spp.lake.count <- as.data.frame(colSums(lake.spp[,2:74] > 0))
spp.lake.count$species = rownames(spp.lake.count)

#add this lake count to the stat dataframe
spp.stat <- full_join(spp.stat, spp.lake.count, by = "species")


#stocking in lake-years / study lakes - all taxa and wae?----------------------------
#NOT UPDATED WITH NEW MODEL

#Read in stocking data
stock <- read.csv("Data/Input/Result_28.csv")
#read in model data
lakeyears <- read.csv("Data/Input/GLLVM_Complete_Dataset.csv")

#add parentdow column to stocking data - MODIFIED FOR THIS DATA
#vermilion and hill sub-basins are listed together (dow ending in 00), so need to modify their parentdows to get it to join to my data
stock.parentdow <- stock %>% 
  mutate(parentdow = case_when(
    (stock$UNIQUE_IDENTIFIER == "1014202" | stock$UNIQUE_IDENTIFIER == "1014201" | stock$UNIQUE_IDENTIFIER == "4003502" | stock$UNIQUE_IDENTIFIER == "4003501") ~ substr(stock$UNIQUE_IDENTIFIER, 1, 7),   #takes care of North and Red lakes (7 characters)
    (stock$UNIQUE_IDENTIFIER == "69037802" | stock$UNIQUE_IDENTIFIER == "69037801") ~ substr(stock$UNIQUE_IDENTIFIER, 1, 8),  #takes care of Vermilion (different because 8 characters)
    (nchar(stock$UNIQUE_IDENTIFIER) == 7 & (stock$UNIQUE_IDENTIFIER != "1014202" & stock$UNIQUE_IDENTIFIER != "1014201" & stock$UNIQUE_IDENTIFIER != "4003502" & stock$UNIQUE_IDENTIFIER != "4003501")) ~ substr(stock$UNIQUE_IDENTIFIER, 1, 5), #this gets 5 digits from the DOWs that have 7 characters and are not those identified before
    (nchar(stock$UNIQUE_IDENTIFIER) == 8 & (stock$UNIQUE_IDENTIFIER != "69037802" & stock$UNIQUE_IDENTIFIER != "69037801")) ~ substr(stock$UNIQUE_IDENTIFIER, 1, 6) #this gets 6 digits from the DOWs that have 8 characters and are not those identified before
  )) %>% 
  #modifies vermilion and hill:
  mutate(parentdow = ifelse(UNIQUE_IDENTIFIER == "1014200", "1014201",
                            ifelse(UNIQUE_IDENTIFIER == "69037800", "69037801", parentdow)
  ))

#join to my dataset
stock.parentdow <- stock.parentdow %>% 
  mutate(parentdow = as.numeric(parentdow))
stock.lakes <- left_join(lakeyears, stock.parentdow, by = "parentdow") %>% 
  select(parentdow, parentdow.year, Year, lake_name, LAKE_NAME, COMMON_NAME, NAME, STOCKING_DATE, STOCKING_YEAR, FISH_QUANTITY, UNIT_OF_MEASURE) %>% 
  filter(!is.na(COMMON_NAME))

#lakes with any stocking of any species at any time
unique(stock.lakes$lake_name)

#lakes with walleye stocking at any age at any time
stock.lakes.wae <- stock.lakes %>% 
  filter(COMMON_NAME == "walleye")
unique(stock.lakes.wae$lake_name)

#lakes with walleye fry stocking at any time
stock.lakes.wae.fry <- stock.lakes %>% 
  filter(COMMON_NAME == "walleye") %>% 
  filter(NAME == "Fry" | NAME == "Fryling" | NAME == "Egg")
unique(stock.lakes.wae.fry$lake_name)

#lake-years with any stocking of any species
stock.lakeyear <- stock.lakes %>% 
  filter(Year == STOCKING_YEAR)
unique(stock.lakeyear$lake_name)
unique(stock.lakeyear$parentdow.year)

#lake-years with walleye stocking at any age
stock.lakeyear.wae <- stock.lakeyear %>% 
  filter(COMMON_NAME == "walleye")
unique(stock.lakeyear.wae$lake_name)
unique(stock.lakeyear.wae$parentdow.year)

#lake-years with walleye fry stocking
stock.lakeyear.wae.fry <- stock.lakeyear.wae %>% 
  filter(NAME == "Fry" | NAME == "Fryling" | NAME == "Egg")
unique(stock.lakeyear.wae.fry$lake_name)
unique(stock.lakeyear.wae.fry$parentdow.year)





#zoop length vs. predictors-------------------------------
#Just because Heidi asked about this in revisions
#NOT UPDATED WITH NEW MODEL

#read in model data
lakeyears <- read.csv("Data/Input/GLLVM_Complete_Dataset.csv")

#all zoop v temp
ggplot(data = lakeyears, aes(x = gdd.year.5c, y = mean_length_all_zoop)) +
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()
#maybe slight but high scatter

#cladocerans v temp
ggplot(data = lakeyears, aes(x = gdd.year.5c, y = mean_length_clad)) +
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()
#maybe slight but high scatter

#all zoop v area
ggplot(data = lakeyears, aes(x = area_ha, y = mean_length_all_zoop)) +
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()

#cladocerans v area
ggplot(data = lakeyears, aes(x = area_ha, y = mean_length_clad)) +
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()

#all zoop v secchi
ggplot(data = lakeyears, aes(x = secchi.meters.MPCA.Jul.to.Sept, y = mean_length_all_zoop)) +
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()

#cladocerans v secchi
ggplot(data = lakeyears, aes(x = secchi.meters.MPCA.Jul.to.Sept, y = mean_length_clad)) +
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()


#all zoop v precip
ggplot(data = lakeyears, aes(x = precip_5yr_avg_mm, y = mean_length_all_zoop)) +
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()

#cladocerans v precip
ggplot(data = lakeyears, aes(x = precip_5yr_avg_mm, y = mean_length_clad)) +
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()


#all zoop v cdom
ggplot(data = lakeyears, aes(x = CDOM.lake.avg, y = mean_length_all_zoop)) +
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()

#cladocerans v cdom
ggplot(data = lakeyears, aes(x = CDOM.lake.avg, y = mean_length_clad)) +
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()
#maybe actually a relationship with CDOM here??

#all zoop v max depth
ggplot(data = lakeyears, aes(x = depth.max.m, y = mean_length_all_zoop)) +
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()

#cladocerans v max depth
ggplot(data = lakeyears, aes(x = depth.max.m, y = mean_length_clad)) +
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()


#all zoop v mean depth
ggplot(data = lakeyears, aes(x = depth.mean.m, y = mean_length_all_zoop)) +
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()

#cladocerans v mean depth
ggplot(data = lakeyears, aes(x = depth.mean.m, y = mean_length_clad)) +
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()


#all zoop v littoral prop
ggplot(data = lakeyears, aes(x = photic_prop_secchi.meters.MPCA.Jul.to.Sept, y = mean_length_all_zoop)) +
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()

#cladocerans v littoral prop
ggplot(data = lakeyears, aes(x = photic_prop_secchi.meters.MPCA.Jul.to.Sept, y = mean_length_clad)) +
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()


#all zoop v swf
ggplot(data = lakeyears, aes(x = SpinyWaterflea.yn, y = mean_length_all_zoop)) +
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()

#cladocerans v swf
ggplot(data = lakeyears, aes(x = SpinyWaterflea.yn, y = mean_length_clad)) +
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()


#all zoop v zm
ggplot(data = lakeyears, aes(x = ZebraMussel.yn, y = mean_length_all_zoop)) +
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()

#cladocerans v zm
ggplot(data = lakeyears, aes(x = ZebraMussel.yn, y = mean_length_clad)) +
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()


#maybe relationships with cdom, depth littoral area, zebra mussels - SOME OF THESE ARE ALSO RELATED TO WALLEYE...


