#This script makes figures from the GLLVM model for ASLO presentation

#THIS IS NOT THE FINAL MODEL I USED FOR MY THESIS - SEE MASTERS THESIS FIGURE SCRIPT FOR THAT

library(ggplot2)
library(corrplot)
library(gclus)
library(gridExtra)
library(ggrepel)
library(tidyverse)
library(gllvm)


#read in model
model <- readRDS("Models/model36_moreLV_trout_nocopepod3RR4LV.rds")

#Correct species names for nice plots
new_names <- c("Black Crappie", "Bluegill", "Bullhead", "Bowfin", "Burbot", "Cisco", "Common Carp", "Golden Shiner", "Hybrid Sunfish", "Lake Whitefish", 
               "Largemouth Bass", "Muskellunge", "Northern Pike", "Pumpkinseed", "Rainbow Smelt", "Redhorse", "Rock Bass", "Sauger", "Smallmouth Bass",
               "Walleye", "White Sucker", "Yellow Perch", "Alona spp.", "Bosmina longirostris", "Ceriodaphnia spp.", "Chydorus sphaericus",
               "Daphnia galeata mendotae", "Daphnia longiremis", "Daphnia parvula", "Daphnia pulicaria", "Daphnia retrocurva",
               "Daphnia small rare", "Diaphanosoma birgei", "Eubosmina coregoni", "Eurycercus lamellatus", "Holopedium gibberum",
               "Sida crystallina")
colnames(model$y) <- new_names
rownames(model$params$theta) <- new_names
names(model$params$beta0) <- new_names
names(model$params$phi) <- new_names
names(model$sd$beta0) <- new_names
names(model$sd$phi) <- new_names


#calculate variation explained by environment vs. residual

#VARIANCE PARTITIONING
VP(model)
#plot by species
plot(VP(model))
#GET MORE INTO THIS IN YOUR THESIS - LOOK AT VARIANCE PARTITIONING FOR EACH SPECIES

#goodness of fit metrics
goodnessOfFit(model)
#make these species-specific
goodnessOfFit(model, species = TRUE)
GOF.spp <- as.data.frame(goodnessOfFit(model, species = TRUE))
rownames(GOF.spp) <- new_names
#save this
#write.csv(GOF.spp, file = "Data/Output/gllvm_GOF_by_species.csv")


#EVERYTHING BELOW THIS IN THIS SECTION IS WRONG SOMEHOW (but leaving code because I worked hard on it):
# #residual covariance matrix
# res_cov_matrix <- getResidualCov(model)
# #total variance explained by residuals
# res_cov <- getResidualCov(model)$trace
# #I checked that this adds to the sum of variance explained by each latent variable as extracted below:
# #variance explained for each linear latent variable
# getResidualCov(model)$var.q
# #variance explained for each latent variable by the quadratic term
# getResidualCov(model)$var.q2
# 
# 
# 
# #environmental covariance matrix
# env_cov_matrix <- getEnvironCov(model)
# # the relative = TRUE argument apparently doesn't work for tweedie distributions so I have to do the math myself
# 
# #calculate the trace of the environmental covariance matrix = total variance explained by environment
# env_cov <- sum(diag(env_cov_matrix$cov))
# #I checked that this adds to the sum of variance explained by each latent variable as extracted below:
# #variance explained by linear CLVs
# getEnvironCov(model, relative = TRUE)$trace.randomB
# #vairance explained by quadratic CLVs
# getEnvironCov(model, relative = TRUE)$trace.randomB.quad
# 
# #extract variance explained by the random lake effect
# var_lake <- model$params$sigma^2
# 
# #calculate total variance explained by model
# total_var <- res_cov + env_cov + var_lake
# 
# #proportions explained
# #THIS IS THE PROPORTION OF THE VARIANCE EXPLAINED BY THE MODEL THAT COMES FROM EACH SOURCE
# (prop_env <- env_cov / total_var)
# (prop_res <- res_cov  / total_var)
# (prop_row <- var_lake / total_var)
# #don't worry about the name of the value here this is just a leftover from calcualtions that i didn't bother to remove
# 
# #variance explained for each species
# res_cov_species <- diag(res_cov_matrix$cov)
# env_cov_species <- diag(env_cov_matrix$cov)
# 
# #calculate total variance for each species
# var_total_spp <- res_cov_species + env_cov_species + var_lake
# 
# #calculate totally unexplained variance from tweedie phi parameter (species-specific variance)
# 
# 
# #calculate proportion of total variance for each source for each species
# prop_env_spp <- env_cov_species / var_total_spp
# prop_res_spp <- res_cov_species  / var_total_spp
# prop_row_spp <- var_lake / var_total_spp
# 
# #summarize results
# species_variance <- data.frame(
#   Species = names(env_cov_species),
#   Env_Proportion = round(prop_env_spp, 3),
#   Residual_Proportion = round(prop_res_spp, 3),
#   Lake_Proportion = round(prop_row_spp, 3),
#   Phi = model$params$phi
# )
# #phi is the species-specific variance parameter of the tweedie distribution
# #lower phi = better predicted by model. 1 is about average, 4 is very high and kinda bad


#Variance explained by model---------------------------
#okay, but how much of the variation is actually explained by the model?

#https://github.com/JenniNiku/gllvm/discussions/203
#Bert says that we really can't get at how much total variation the model explains 
#because we don't estimate all the dimensions like traditional ordination methods
#Best we can do is partition out the variance we can explain
#He also warns against R^2 values here because they don't do well with non-normal distributions




#Ordination-------------------
#manual ordination guided by google gemini but modified by me
# 1. Get Site Scores (Where the lakes sit on the axes)
# this extracts the LV (unconstrained axes) scores for each site
site_scores_lv <- model$lvs
#excract CLV scores for each site
#extract environmental coefficients
coef <- model$params$LvXcoef
x <- read.csv("Data/Input/gllvm_x_matrix.csv")
x_matrix <- as.matrix(x)
site_scores_clv <- x_matrix %*% coef
#cbind them together
site_scores_all <- cbind(site_scores_clv, site_scores_lv)

# 2. Get Species Scores (Where species sit on the axes)
# These are the "loadings" or weights
spec_scores <- model$params$theta[]

# 3. Get the Environmental Arrows (The "Rotation" matrix)
# This replaces your missing coefplot!
env_arrows <- model$params$LvXcoef
#rename these how I want them to appear in plot
rownames(env_arrows) <- c("CDOM", "Area", "Max Depth", "Secchi", "Temp", "Littoral", "Spiny Water Flea", "Zebra Mussel")

# Create a data frame for species
spec_df <- as.data.frame(spec_scores)
spec_df$Species <- rownames(spec_df)
spec_df$Species_label_zoop <- ifelse((spec_df$Species == "Bosmina longirostris" | spec_df$Species == "Eubosmina coregoni" | 
                                   spec_df$Species == "Chydorus sphaericus" | spec_df$Species == "Alona spp." | spec_df$Species == "Ceriodaphnia spp." |
                                   spec_df$Species == "Daphnia galeata mendotae" | spec_df$Species == "Daphnia pulicaria" | spec_df$Species == "Daphnia longiremis" | 
                                   spec_df$Species == "Daphnia parvula" | spec_df$Species == "Daphnia retrocurva" | 
                                   spec_df$Species == "Daphnia small rare" | spec_df$Species == "Walleye"), spec_df$Species, NA)
spec_df$Species_label_fish <- ifelse((spec_df$Species == "Black Crappie" | spec_df$Species == "Bluegill" | spec_df$Species == "Hybrid Sunfish" | 
                                   spec_df$Species == "Largemouth Bass" | spec_df$Species == "Pumpkinseed" | spec_df$Species == "Rock Bass" | 
                                   spec_df$Species == "Smallmouth Bass" | spec_df$Species == "Walleye"), spec_df$Species, NA)
spec_df$Species_label <- ifelse((spec_df$Species == "Black Crappie" | spec_df$Species == "Bluegill" | spec_df$Species == "Hybrid Sunfish" | 
                                        spec_df$Species == "Largemouth Bass" | spec_df$Species == "Pumpkinseed" | spec_df$Species == "Rock Bass" | 
                                        spec_df$Species == "Smallmouth Bass" | spec_df$Species == "Walleye" | 
                                   spec_df$Species == "Bosmina longirostris" | spec_df$Species == "Eubosmina coregoni" | 
                                   spec_df$Species == "Chydorus sphaericus" | spec_df$Species == "Alona spp." | spec_df$Species == "Ceriodaphnia spp." |
                                   spec_df$Species == "Daphnia galeata mendotae" | spec_df$Species == "Daphnia pulicaria" | spec_df$Species == "Daphnia longiremis" | 
                                   spec_df$Species == "Daphnia parvula" | spec_df$Species == "Daphnia retrocurva" | 
                                   spec_df$Species == "Daphnia small rare"), spec_df$Species, NA)
#create a dataframe with just species I want to point out in my ASLO talk
spec_df_no_other <- filter(spec_df, spec_df$Species == "Black Crappie" | spec_df$Species == "Bluegill" | spec_df$Species == "Hybrid Sunfish" | 
                                  spec_df$Species == "Largemouth Bass" | spec_df$Species == "Pumpkinseed" | spec_df$Species == "Rock Bass" | 
                                  spec_df$Species == "Smallmouth Bass" | spec_df$Species == "Walleye" | 
                                  spec_df$Species == "Bosmina longirostris" | spec_df$Species == "Eubosmina coregoni" | 
                                  spec_df$Species == "Chydorus sphaericus" | spec_df$Species == "Alona spp." | spec_df$Species == "Ceriodaphnia spp." |
                                  spec_df$Species == "Daphnia galeata mendotae" | spec_df$Species == "Daphnia pulicaria" | spec_df$Species == "Daphnia longiremis" | 
                                  spec_df$Species == "Daphnia parvula" | spec_df$Species == "Daphnia retrocurva" | 
                                  spec_df$Species == "Daphnia small rare")


# Highlight your important species
spec_df$Group <- ifelse(spec_df$Species == "Walleye", "Walleye", 
                              ifelse((spec_df$Species == "Black Crappie" | spec_df$Species == "Bluegill" | spec_df$Species == "Hybrid Sunfish" | spec_df$Species == "Largemouth Bass" | spec_df$Species == "Pumpkinseed" | spec_df$Species == "Rock Bass" | spec_df$Species == "Smallmouth Bass"), "Centrarchid",
                                     ifelse((spec_df$Species == "Bosmina longirostris" | spec_df$Species == "Eubosmina coregoni" | spec_df$Species == "Chydorus sphaericus" | spec_df$Species == "Alona spp." | spec_df$Species == "Ceriodaphnia spp." | 
                                               spec_df$Species == "Daphnia galeata mendotae" | spec_df$Species == "Daphnia pulicaria" | spec_df$Species == "Daphnia longiremis" | spec_df$Species == "Daphnia parvula" | spec_df$Species == "Daphnia retrocurva" | 
                                               spec_df$Species == "Daphnia small rare"),"Preserved Zooplankton","Other")))
spec_df$Group <- factor(spec_df$Group, levels = c("Walleye", "Centrarchid", "Preserved Zooplankton", "Other"))
spec_df_no_other$Group <- ifelse(spec_df_no_other$Species == "Walleye", "Walleye", 
                        ifelse((spec_df_no_other$Species == "Black Crappie" | spec_df_no_other$Species == "Bluegill" | spec_df_no_other$Species == "Hybrid Sunfish" | spec_df_no_other$Species == "Largemouth Bass" | spec_df_no_other$Species == "Pumpkinseed" | spec_df_no_other$Species == "Rock Bass" | spec_df_no_other$Species == "Smallmouth Bass"), "Centrarchid",
                               ifelse((spec_df_no_other$Species == "Bosmina longirostris" | spec_df_no_other$Species == "Eubosmina coregoni" | spec_df_no_other$Species == "Chydorus sphaericus" | spec_df_no_other$Species == "Alona spp." | spec_df_no_other$Species == "Ceriodaphnia spp." | 
                                         spec_df_no_other$Species == "Daphnia galeata mendotae" | spec_df_no_other$Species == "Daphnia pulicaria" | spec_df_no_other$Species == "Daphnia longiremis" | spec_df_no_other$Species == "Daphnia parvula" | spec_df_no_other$Species == "Daphnia retrocurva" | 
                                         spec_df_no_other$Species == "Daphnia small rare"),"Zooplankton","Error")))
spec_df_no_other$Group <- factor(spec_df_no_other$Group, levels = c("Walleye", "Centrarchid", "Zooplankton", "Error"))

# Plotting CLV1 vs CLV2 (The first two Environmental Axes)
env_biplot <- ggplot() +
  # Draw the Sites as small light-grey dots
  geom_point(data = as.data.frame(site_scores_all), aes(x = CLV1, y = CLV2), color = "darkgray", alpha = 0.5) +
  # Draw the Species as colored points
  geom_point(data = spec_df, aes(x = CLV1, y = CLV2, color = Group), size = 3,  alpha = 0.7) +
  # Add Species Labels
  geom_text_repel(data = spec_df, aes(x = CLV1, y = CLV2, label = Species), max.overlaps = Inf) +
  # Add the Environmental Arrows
  geom_segment(data = as.data.frame(env_arrows), 
               aes(x = 0, y = 0, xend = CLV1*4, yend = CLV2*4), # Multiplied by 2 to make arrows visible
               arrow = arrow(length = unit(0.2, "cm")), color = "blue") +
  geom_text(data = as.data.frame(env_arrows), 
            aes(x = CLV1*4.4, y = CLV2*4.4, label = rownames(env_arrows)), color = "blue") +
  theme_classic() +
  scale_color_manual(values = c("Walleye" = "orange", "Centrarchid" = "purple3", "Preserved Zooplankton" = "#8B4513", "Other" = "black")) +
  labs(x = "Environmental Axis 1", y = "Environmental Axis 2")+
  scale_x_continuous(limits = c(-6,5))+
  scale_y_continuous(limits = c(-6,4))
#ggsave(filename = "Environmental_Biplot_All.png", plot = env_biplot, width = 9, height = 5, units = "in", dpi = 150)


# Plotting LV1 vs LV2 (The first two Residual Axes)
res_ordination <- ggplot() +
  # Draw the Sites as small light-grey dots
  geom_point(data = as.data.frame(site_scores_all), aes(x = LV1, y = LV2), color = "darkgray", alpha = 0.5) +
  # Draw the Species as colored points
  geom_point(data = spec_df, aes(x = LV1, y = LV2, color = Group), size = 3, alpha = 0.7) +
  # Add Species Labels
  geom_text_repel(data = spec_df, aes(x = LV1, y = LV2, label = Species_label), size = 4, max.overlaps = Inf) +
  theme_classic() +
  scale_color_manual(values = c("Walleye" = "orange", "Centrarchid" = "purple3", "Preserved Zooplankton" = "#8B4513", "Other" = "black")) +
  labs(x = "Residual Axis 1", y = "Residual Axis 2")+
  theme(legend.position = "none") #don't include legend, I will include this separately
#ggsave(filename = "Residual_Ordination_All.png", plot = res_ordination, width = 9, height = 5, units = "in", dpi = 150)



#build the env biolot piece by piece for powerpoint
env_biplot1 <- ggplot() +
  # Draw the Sites as small light-grey dots
  #geom_point(data = as.data.frame(site_scores_all), aes(x = CLV1, y = CLV2), color = "darkgray", alpha = 0.5) +
  # Draw the Species as colored points
  #geom_point(data = spec_df, aes(x = CLV1, y = CLV2, color = Group), size = 3) +
  # Add Species Labels
  #geom_text_repel(data = spec_df, aes(x = CLV1, y = CLV2, label = Species)) +
  # Add the Environmental Arrows
  geom_segment(data = as.data.frame(env_arrows), 
               aes(x = 0, y = 0, xend = CLV1*4, yend = CLV2*4), # Multiplied by 2 to make arrows visible
               arrow = arrow(length = unit(0.2, "cm")), color = "blue") +
  geom_text(data = as.data.frame(env_arrows), 
                  aes(x = CLV1*4.4, y = CLV2*4.4, label = rownames(env_arrows)), color = "blue") +
  theme_classic()+
  #scale_color_manual(values = c("Walleye" = "orange", "Centrarchid" = "purple3", "Preserved Zooplankton" = "#8B4513", "Other" = "black")) +
  labs(x = "Environmental Axis 1", y = "Environmental Axis 2")+
  scale_x_continuous(limits = c(-6,5))+
  scale_y_continuous(limits = c(-6,4))
#ggsave(filename = "Environmental_Biplot1.png", plot = env_biplot1, width = 9, height = 5, units = "in", dpi = 150)


env_biplot2 <- ggplot() +
  # Draw the Sites as small light-grey dots
  geom_point(data = as.data.frame(site_scores_all), aes(x = CLV1, y = CLV2), color = "darkgray", alpha = 0.5) +
  # Draw the Species as colored points
  #geom_point(data = spec_df, aes(x = CLV1, y = CLV2, color = Group), size = 3) +
  # Add Species Labels
  #geom_text_repel(data = spec_df, aes(x = CLV1, y = CLV2, label = Species)) +
  # Add the Environmental Arrows
  geom_segment(data = as.data.frame(env_arrows), 
               aes(x = 0, y = 0, xend = CLV1*4, yend = CLV2*4), # Multiplied by 2 to make arrows visible
               arrow = arrow(length = unit(0.2, "cm")), color = "blue") +
  geom_text(data = as.data.frame(env_arrows), 
            aes(x = CLV1*4.4, y = CLV2*4.4, label = rownames(env_arrows)), color = "blue") +
  theme_classic()+
  #scale_color_manual(values = c("Walleye" = "orange", "Centrarchid" = "purple3", "Preserved Zooplankton" = "#8B4513", "Other" = "black")) +
  labs(x = "Environmental Axis 1", y = "Environmental Axis 2")+
  scale_x_continuous(limits = c(-6,5))+
  scale_y_continuous(limits = c(-6,4))
#ggsave(filename = "Environmental_Biplot2.png", plot = env_biplot2, width = 9, height = 5, units = "in", dpi = 150)


env_biplot3 <- ggplot() +
  # Draw the Sites as small light-grey dots
  geom_point(data = as.data.frame(site_scores_all), aes(x = CLV1, y = CLV2), color = "darkgray", alpha = 0.5) +
  # Draw the Species as colored points
  geom_point(data = spec_df, aes(x = CLV1, y = CLV2, color = Group), size = 3,  alpha = 0.7) +
  # Add Species Labels
  #geom_text_repel(data = spec_df, aes(x = CLV1, y = CLV2, label = Species)) +
  # Add the Environmental Arrows
  geom_segment(data = as.data.frame(env_arrows), 
               aes(x = 0, y = 0, xend = CLV1*4, yend = CLV2*4), # Multiplied by 2 to make arrows visible
               arrow = arrow(length = unit(0.2, "cm")), color = "blue") +
  geom_text(data = as.data.frame(env_arrows), 
            aes(x = CLV1*4.4, y = CLV2*4.4, label = rownames(env_arrows)), color = "blue") +
  theme_classic()+
  scale_color_manual(values = c("Walleye" = "orange", "Centrarchid" = "purple3", "Preserved Zooplankton" = "#8B4513", "Other" = "black")) +
  labs(x = "Environmental Axis 1", y = "Environmental Axis 2")+
  scale_x_continuous(limits = c(-6,5))+
  scale_y_continuous(limits = c(-6,4))+
  theme(legend.position = "none") #don't include legend, I will include this separately
#ggsave(filename = "Environmental_Biplot3.png", plot = env_biplot3, width = 9, height = 5, units = "in", dpi = 150)


env_biplot3_no_other <- ggplot() +
  # Draw the Sites as small light-grey dots
  geom_point(data = as.data.frame(site_scores_all), aes(x = CLV1, y = CLV2), color = "darkgray", alpha = 0.5) +
  # Draw the Species as colored points
  geom_point(data = spec_df_no_other, aes(x = CLV1, y = CLV2, color = Group), size = 3,  alpha = 0.7) +
  # Add Species Labels
  #geom_text_repel(data = spec_df, aes(x = CLV1, y = CLV2, label = Species)) +
  # Add the Environmental Arrows
  geom_segment(data = as.data.frame(env_arrows), 
               aes(x = 0, y = 0, xend = CLV1*4, yend = CLV2*4), # Multiplied by 2 to make arrows visible
               arrow = arrow(length = unit(0.2, "cm")), color = "blue") +
  geom_text(data = as.data.frame(env_arrows), 
            aes(x = CLV1*4.4, y = CLV2*4.4, label = rownames(env_arrows)), color = "blue") +
  theme_classic()+
  scale_color_manual(values = c("Walleye" = "orange", "Centrarchid" = "purple3", "Zooplankton" = "black")) +
  labs(x = "Environmental Axis 1", y = "Environmental Axis 2")+
  scale_x_continuous(limits = c(-6,5))+
  scale_y_continuous(limits = c(-6,4))+
  theme(legend.position = "none") #don't include legend, I will include this separately
#ggsave(filename = "Environmental_Biplot3_no_other.png", plot = env_biplot3_no_other, width = 9, height = 5, units = "in", dpi = 150)

env_biplot4 <- ggplot() +
  # Draw the Sites as small light-grey dots
  geom_point(data = as.data.frame(site_scores_all), aes(x = CLV1, y = CLV2), color = "darkgray", alpha = 0.5) +
  # Draw the Species as colored points
  geom_point(data = spec_df, aes(x = CLV1, y = CLV2, color = Group), size = 3,  alpha = 0.7) +
  # Add Species Labels
  geom_text_repel(data = spec_df, aes(x = CLV1, y = CLV2, label = Species_label_fish), size = 4, max.overlaps = Inf) +
  # Add the Environmental Arrows
  geom_segment(data = as.data.frame(env_arrows), 
               aes(x = 0, y = 0, xend = CLV1*4, yend = CLV2*4), # Multiplied by 2 to make arrows visible
               arrow = arrow(length = unit(0.2, "cm")), color = "blue", alpha = 0.5) +
  #geom_text(data = as.data.frame(env_arrows), 
            #aes(x = CLV1*4.4, y = CLV2*4.4, label = rownames(env_arrows)), color = "blue") +
  theme_classic()+
  scale_color_manual(values = c("Walleye" = "orange", "Centrarchid" = "purple3", "Preserved Zooplankton" = "#8B4513", "Other" = "black")) +
  labs(x = "Environmental Axis 1", y = "Environmental Axis 2")+
  scale_x_continuous(limits = c(-6,5))+
  scale_y_continuous(limits = c(-6,4))+
  theme(legend.position = "none") #don't include legend, I will include this separately
#ggsave(filename = "Environmental_Biplot4.png", plot = env_biplot4, width = 9, height = 5, units = "in", dpi = 150)

env_biplot4_no_other <- ggplot() +
  # Draw the Sites as small light-grey dots
  geom_point(data = as.data.frame(site_scores_all), aes(x = CLV1, y = CLV2), color = "darkgray", alpha = 0.5) +
  # Draw the Species as colored points
  geom_point(data = spec_df_no_other, aes(x = CLV1, y = CLV2, color = Group), size = 3,  alpha = 0.7) +
  # Add Species Labels
  geom_text_repel(data = spec_df_no_other, aes(x = CLV1, y = CLV2, label = Species_label_fish), size = 4, max.overlaps = Inf) +
  # Add the Environmental Arrows
  geom_segment(data = as.data.frame(env_arrows), 
               aes(x = 0, y = 0, xend = CLV1*4, yend = CLV2*4), # Multiplied by 2 to make arrows visible
               arrow = arrow(length = unit(0.2, "cm")), color = "blue", alpha = 0.5) +
  #geom_text(data = as.data.frame(env_arrows), 
  #aes(x = CLV1*4.4, y = CLV2*4.4, label = rownames(env_arrows)), color = "blue") +
  theme_classic()+
  scale_color_manual(values = c("Walleye" = "orange", "Centrarchid" = "purple3", "Zooplankton" = "black")) +
  labs(x = "Environmental Axis 1", y = "Environmental Axis 2")+
  scale_x_continuous(limits = c(-6,5))+
  scale_y_continuous(limits = c(-6,4))+
  theme(legend.position = "none") #don't include legend, I will include this separately
#ggsave(filename = "Environmental_Biplot4_no_other.png", plot = env_biplot4_no_other, width = 9, height = 5, units = "in", dpi = 150)


env_biplot5 <- ggplot() +
  # Draw the Sites as small light-grey dots
  geom_point(data = as.data.frame(site_scores_all), aes(x = CLV1, y = CLV2), color = "darkgray", alpha = 0.5) +
  # Draw the Species as colored points
  geom_point(data = spec_df, aes(x = CLV1, y = CLV2, color = Group), size = 3,  alpha = 0.7) +
  # Add Species Labels
  geom_text_repel(data = spec_df, aes(x = CLV1, y = CLV2, label = Species_label_zoop), size = 4, max.overlaps = Inf) +
  # Add the Environmental Arrows
  geom_segment(data = as.data.frame(env_arrows), 
               aes(x = 0, y = 0, xend = CLV1*4, yend = CLV2*4), # Multiplied by 2 to make arrows visible
               arrow = arrow(length = unit(0.2, "cm")), color = "blue", alpha = 0.5) +
  #geom_text(data = as.data.frame(env_arrows), 
  #aes(x = CLV1*4.4, y = CLV2*4.4, label = rownames(env_arrows)), color = "blue") +
  theme_classic()+
  scale_color_manual(values = c("Walleye" = "orange", "Centrarchid" = "purple3", "Preserved Zooplankton" = "#8B4513", "Other" = "black")) +
  labs(x = "Environmental Axis 1", y = "Environmental Axis 2")+
  scale_x_continuous(limits = c(-6,5))+
  scale_y_continuous(limits = c(-6,4))+
  theme(legend.position = "none") #don't include legend, I will include this separately
#ggsave(filename = "Environmental_Biplot5.png", plot = env_biplot5, width = 9, height = 5, units = "in", dpi = 150)

env_biplot5_no_other <- ggplot() +
  # Draw the Sites as small light-grey dots
  geom_point(data = as.data.frame(site_scores_all), aes(x = CLV1, y = CLV2), color = "darkgray", alpha = 0.5) +
  # Draw the Species as colored points
  geom_point(data = spec_df_no_other, aes(x = CLV1, y = CLV2, color = Group), size = 3,  alpha = 0.7) +
  # Add Species Labels
  geom_text_repel(data = spec_df_no_other, aes(x = CLV1, y = CLV2, label = Species_label_zoop), size = 4, max.overlaps = Inf) +
  # Add the Environmental Arrows
  geom_segment(data = as.data.frame(env_arrows), 
               aes(x = 0, y = 0, xend = CLV1*4, yend = CLV2*4), # Multiplied by 2 to make arrows visible
               arrow = arrow(length = unit(0.2, "cm")), color = "blue", alpha = 0.5) +
  #geom_text(data = as.data.frame(env_arrows), 
  #aes(x = CLV1*4.4, y = CLV2*4.4, label = rownames(env_arrows)), color = "blue") +
  theme_classic()+
  scale_color_manual(values = c("Walleye" = "orange", "Centrarchid" = "purple3", "Zooplankton" = "black")) +
  labs(x = "Environmental Axis 1", y = "Environmental Axis 2")+
  scale_x_continuous(limits = c(-6,5))+
  scale_y_continuous(limits = c(-6,4))+
  theme(legend.position = "none") #don't include legend, I will include this separately
#ggsave(filename = "Environmental_Biplot5_no_other.png", plot = env_biplot5_no_other, width = 9, height = 5, units = "in", dpi = 150)


#Corrplot legend------------------------
#png("cor_legend.png", width = 6, height = 4, units = "in", res = 300, bg = "transparent")
plot.new()
plot.window(xlim=c(0,10), ylim=c(0,10))
colorlegend(colbar = COL2('PuOr'), 
            labels = seq(-1, 1, 0.5), 
            cex = 1.5, #label size
            align = "l", 
            vertical = FALSE, 
            xlim = c(0.2, 9.8), 
            ylim = c(1, 3),
            bg = "transparent")
#dev.off()

#All species correlations----------------

#environmental correlations
Env <- getEnvironCor(model)
#png("cor_all_env.png", width = 6, height = 4, units = "in", res = 150)
corrplot(Env[order.single(Env), order.single(Env)],
         diag = FALSE, #do not include the exact diagonal where speices overlap with themselves
         type = "lower", #lower half of diagonal
         method = "square",
         tl.cex = 0.6, #label size
         tl.srt = 45, #rotate top labels
         tl.col = "black", #label color
         col = COL2('PuOr'), #purple-orange colors for color-blind friendly
         cl.pos = 'n') #no legend
#dev.off()

#environmental correlations original order
#png("cor_all_env_OriginalOrder.png", width = 6, height = 4, units = "in", res = 150)
corrplot(Env,
         diag = FALSE, #do not include the exact diagonal where speices overlap with themselves
         type = "lower", #lower half of diagonal
         method = "square",
         tl.cex = 0.6, #label size
         tl.srt = 45, #rotate top labels
         tl.col = "black", #label color
         col = COL2('PuOr'), #purple-orange colors for color-blind friendly
         cl.pos = 'n') #no legend
#dev.off()

#residual correlations
Theta <- getResidualCor(model)
#png("cor_all_res.png", width = 6, height = 4, units = "in", res = 150)
corrplot(Theta[order.single(Theta), order.single(Theta)],
         diag = FALSE, #do not include the exact diagonal where speices overlap with themselves
         type = "lower", #lower half of diagonal
         method = "square",
         tl.cex = 0.6, #label size
         tl.srt = 45, #rotate top labels
         tl.col = "black", #label color
         col = COL2('PuOr'), #purple-orange colors for color-blind friendly
         cl.pos = 'n') #no legend
#dev.off()

#residual correlation but original order
#png("cor_all_res_OriginalOrder.png", width = 6, height = 4, units = "in", res = 150)
corrplot(Theta,
         diag = FALSE, #do not include the exact diagonal where speices overlap with themselves
         type = "lower", #lower half of diagonal
         method = "square",
         tl.cex = 0.6, #label size
         tl.srt = 45, #rotate top labels
         tl.col = "black", #label color
         col = COL2('PuOr'), #purple-orange colors for color-blind friendly
         cl.pos = 'n') #no legend
#dev.off()

#residual correlation but Env order
#png("cor_all_res_EnvOrder.png", width = 6, height = 4, units = "in", res = 150)
corrplot(Theta[order.single(Env), order.single(Env)],
         diag = FALSE, #do not include the exact diagonal where speices overlap with themselves
         type = "lower", #lower half of diagonal
         method = "square",
         tl.cex = 0.6, #label size
         tl.srt = 45, #rotate top labels
         tl.col = "black", #label color
         col = COL2('PuOr'), #purple-orange colors for color-blind friendly
         cl.pos = 'n') #no legend
#dev.off()


#Walleye-Centrarchid correlations------

#make list of species to isolate
wae_cent <- c("Walleye", "Black Crappie", "Bluegill", "Hybrid Sunfish", "Largemouth Bass",  "Pumpkinseed", "Rock Bass", "Smallmouth Bass")
#isolate the environment and residual correlations for these species
Env_wae_cent <- Env[wae_cent, wae_cent]
Theta_wae_cent <- Theta[wae_cent, wae_cent]

#png("cor_wae_cent_env_OriginalOrder.png", width = 6, height = 4, units = "in", res = 150)
corrplot(Env_wae_cent,
         diag = FALSE, #do not include the exact diagonal where speices overlap with themselves
         type = "lower", #lower half of diagonal
         method = "square",
         tl.cex = 1.2, #label size
         tl.srt = 45, #rotate top labels
         tl.col = "black", #label color
         col = COL2('PuOr'), #purple-orange colors for color-blind friendly
         cl.pos = 'n') #no legend
#dev.off()

#png("cor_wae_cent_res_OriginalOrder.png", width = 6, height = 4, units = "in", res = 150)
corrplot(Theta_wae_cent,
         diag = FALSE, #do not include the exact diagonal where speices overlap with themselves
         type = "lower", #lower half of diagonal
         method = "square",
         tl.cex = 1.2, #label size
         tl.srt = 45, #rotate top labels
         tl.col = "black", #label color
         col = COL2('PuOr'), #purple-orange colors for color-blind friendly
         cl.pos = 'n') #no legend
#dev.off()


#Walleye-zoop correlations-----
wae_zoop <- c("Walleye", "Largemouth Bass", "Alona spp.", "Bosmina longirostris", "Ceriodaphnia spp.", "Chydorus sphaericus",
              "Daphnia galeata mendotae", "Daphnia longiremis", "Daphnia parvula", "Daphnia pulicaria", "Daphnia retrocurva",
              "Daphnia small rare", "Diaphanosoma birgei", "Eubosmina coregoni", "Eurycercus lamellatus", "Holopedium gibberum",
              "Sida crystallina")
#isolate the environment and residual correlations for these species
Env_wae_zoop <- Env[wae_zoop, wae_zoop]
Theta_wae_zoop <- Theta[wae_zoop, wae_zoop]

#png("cor_wae_zoop_env_OriginalOrder.png", width = 6, height = 4, units = "in", res = 150)
corrplot(Env_wae_zoop,
         diag = FALSE, #do not include the exact diagonal where speices overlap with themselves
         type = "lower", #lower half of diagonal
         method = "square",
         tl.cex = 0.9, #label size
         tl.srt = 45, #rotate top labels
         tl.col = "black", #label color
         col = COL2('PuOr'), #purple-orange colors for color-blind friendly
         cl.pos = 'n') #no legend
#dev.off()

#png("cor_wae_zoop_res_OriginalOrder.png", width = 6, height = 4, units = "in", res = 150)
corrplot(Theta_wae_zoop,
         diag = FALSE, #do not include the exact diagonal where speices overlap with themselves
         type = "lower", #lower half of diagonal
         method = "square",
         tl.cex = 0.9, #label size
         tl.srt = 45, #rotate top labels
         tl.col = "black", #label color
         col = COL2('PuOr'), #purple-orange colors for color-blind friendly
         cl.pos = 'n') #no legend
#dev.off()



#Walleye- preserved zoop correlations-----
wae_pres_zoop <- c("Walleye", "Largemouth Bass", "Bosmina longirostris", "Eubosmina coregoni", "Chydorus sphaericus", "Alona spp.", "Ceriodaphnia spp.",
              "Daphnia galeata mendotae", "Daphnia pulicaria", "Daphnia longiremis", "Daphnia parvula", "Daphnia retrocurva",
              "Daphnia small rare")
#isolate the environment and residual correlations for these species
Env_wae_pres_zoop <- Env[wae_pres_zoop, wae_pres_zoop]
Theta_wae_pres_zoop <- Theta[wae_pres_zoop, wae_pres_zoop]

#png("cor_wae_pres_zoop_env_OriginalOrder.png", width = 6, height = 4, units = "in", res = 150)
corrplot(Env_wae_pres_zoop,
         diag = FALSE, #do not include the exact diagonal where speices overlap with themselves
         type = "lower", #lower half of diagonal
         method = "square",
         tl.cex = 0.9, #label size
         tl.srt = 45, #rotate top labels
         tl.col = "black", #label color
         col = COL2('PuOr'), #purple-orange colors for color-blind friendly
         cl.pos = 'n') #no legend
#dev.off()

#png("cor_wae_pres_zoop_res_OriginalOrder.png", width = 6, height = 4, units = "in", res = 150)
corrplot(Theta_wae_pres_zoop,
         diag = FALSE, #do not include the exact diagonal where speices overlap with themselves
         type = "lower", #lower half of diagonal
         method = "square",
         tl.cex = 0.9, #label size
         tl.srt = 45, #rotate top labels
         tl.col = "black", #label color
         col = COL2('PuOr'), #purple-orange colors for color-blind friendly
         cl.pos = 'n') #no legend
#dev.off()



#example environment output plot-----

#vision is to plot wae and lmb species tolerance curves on first environmental axis
#This is guided by AI but actually written by me to make sure it makes sense 
#it was my idea to use coefficients instead of trying to predict with so many axes
#AI wanted me to use the predict function and I said no thank you....

#make site scores a data frame
site_scores_df <- as.data.frame(site_scores_all)

#make a fake gradient
# Generate 100 points between the minimum and maximum observed CLV1 scores for each species
clv1_seq <- seq(min(spec_df$CLV1), max(spec_df$CLV1), length.out = 1000)

#name the species I want to plot - this makes it easier to plot various species by just changing the name once here
spp1 <- "Walleye"
spp2 <- "Largemouth Bass"

#Extract the exact model parameters for species of interest
b01 <- model$params$beta0[spp1] #intercept
b02 <- model$params$beta0[spp2] #intercept 
theta_linear1 <- model$params$theta[spp1, "CLV1"] #linear coefficient
theta_linear2 <- model$params$theta[spp2, "CLV1"] #linear coefficient
theta_quad1 <- model$params$theta[spp1, "CLV1^2"] #quadratic coefficient 
theta_quad2 <- model$params$theta[spp2, "CLV1^2"] #quadratic coefficient 

#use extracted coefficients to build quadratic equation
#Technically the quadratic term should be subtracted, but the quadratic coefficient is stored as a negative number in the gllvm object so I need to add here
eta1 <- b01 + (clv1_seq * theta_linear1) + ((clv1_seq^2) * theta_quad1)
eta2 <- b02 + (clv1_seq * theta_linear2) + ((clv1_seq^2) * theta_quad2)

#Inverse link function (exp for tweedie distribution) to get back to response scale
predicted_response1 <- exp(eta1) 
predicted_response2 <- exp(eta2)

#save as data frame for ggplot
pred_response_plot_data <- data.frame(CLV1 = clv1_seq, sp1 = predicted_response1, sp2 = predicted_response2)
#pivot longer
pred_response_plot_data_long <- pred_response_plot_data %>% 
  pivot_longer(cols = sp1:sp2, names_to = "Species", values_to = "pred_resp") %>% 
  mutate(Species = ifelse(Species == "sp1", spp1, spp2))

#plot!!
pred <- ggplot(data = pred_response_plot_data_long, aes(x = CLV1, y = pred_resp, color = Species))+
        geom_line(linewidth = 1.2)+
        labs(x = "Environmental Axis 1", y = "Predicted CPUE")+
        scale_color_manual(values = c("Walleye" = "orange", "Largemouth Bass" = "purple3")) +
        theme_classic()+
        theme(axis.title = element_text(size = 14), 
              legend.text = element_text(size = 12), 
              legend.title = element_blank(),
              legend.position = "bottom")
pred
#ggsave(filename = "WAE_LMB_v_CLV1.png", plot = pred, width = 6, height = 5, units = "in", dpi = 150)














