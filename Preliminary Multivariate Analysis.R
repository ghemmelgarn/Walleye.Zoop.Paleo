#to d0: check for and remove uncommon species (too many zeroes) - or apply transformations to correct for this
#consider standardizing
#look for any obvious non-linear relationships - transform if needed

#useful website:
#https://sites.google.com/site/mb3gustame/indirect-gradient-analysis/principal-components-analysis

#another useful website
#https://r.qcbs.ca/workshop09/book-en/transformations.html

#Hellinger transformation is the square root of relative abundances
#If I do proportions of CPUE / Biomass (essentially relative abundance)

#IN THIS SCRIPT:
#PCA of raw and Hellinger transformed fish data (CPUE (abundance/net night) per species AND pooled total fish CPUE as an additional variable)
#PCA of raw and Hellinger transformed zoop data (biomass/L per species AND total zoop biomass as an additional variable)
            #This allows me to look at relative abundance separately from total abundance/biomass
          #when total abundance or biomass included, uses correlation matrix instead of covariance matrix because not same unit or scale
#PCA of fish and zoops together - Kieran suggested this
        #correlation matrix for these


#Partial least squares on how these two sets of variables covary


library(tidyverse)
library(ggplot2)
library(dplyr)
library(vegan) #contains the Hellinger transformation
library(tidyr)
library(factoextra) #for fancy biplots
library(geomorph) #for PLS
library(viridis) # for plot colors
library(igraph) #for minimum spanning tree
library(vegdist) #for bray-curtis distance
library(ape) #for PCoA

#read in data
Data <- read.csv("Data/Input/PrelimMultivarData.csv")

#Uncomment code below if you want a separate color for each lake in any plots - ends up being too many colors to be useful
# #before we start, assign the lakes colors
# lake.colors <- c("Bearhead" = "red", "Belle" = "blue", "Carlos" = "green3", "Carrie" = "orange",
#                  "Cass" = "purple", "Cedar" = "brown","Cut Foot Sioux" = "pink", "Elk" = "gold",              
#                  "Freeborn" = "cyan", "Garfield" = "magenta","Green" = "darkgreen","Greenwood" = "darkblue",
#                  "Hill (north)" = "darkred", "Kabetogama" = "darkorange", "Lake of the Woods" = "darkviolet",
#                  "Leech" = "deepskyblue","Madison" = "chartreuse3", "Mille Lacs" = "orchid", "Minnetonka" = "sienna",
#                  "Pearl" = "steelblue","Peltier" = "turquoise", "Pepin" = "slateblue","Portage" = "firebrick",
#                  "Sand Point" = "tomato","South Center" = "yellow3", "Tait" = "gray40","Ten Mile" = "olivedrab",
#                  "Trout" = "mediumvioletred","Vermilion" = "dodgerblue","White Iron" = "indianred","Winnibigoshish" =  "black"  
#                  )


#------------------------------------------------------------------------------------------------------------------------------------------------

#FISH PCA


#isolate fish columns you want
fish <- Data %>% 
  select(lake_id, LakeName, year, parentdow.fish.year,
         WAE.CPUE,
         LMB.CPUE,
         NOP.CPUE,
         WTS.CPUE,
         BLG.CPUE,
         YEP.CPUE,
         SHR.CPUE,
         YEB.CPUE,
         BOF.CPUE,
         HSF.CPUE,
         BLC.CPUE,
         BLB.CPUE,
         RKB.CPUE,
         BRB.CPUE,
         PMK.CPUE,
         CAP.CPUE,
         GOS.CPUE,
         RHS.CPUE,
         TLC.CPUE,
         MUE.CPUE,
         LKS.CPUE,
         GSF.CPUE,
         SMB.CPUE,
         LKW.CPUE,
         GRR.CPUE,
         BUB.CPUE,
         FRD.CPUE,
         GOE.CPUE,
         QBS.CPUE,
         TRP.CPUE,
         GIS.CPUE,
         WHC.CPUE,
         SNG.CPUE,
         BIB.CPUE,
         CCF.CPUE,
         WHB.CPUE,
         SLR.CPUE,
         TPM.CPUE,
         RBT.CPUE,
         LAT.CPUE,
         RBS.CPUE,
         BKT.CPUE,
         CRC.CPUE,
         PRD.CPUE,
         SAR.CPUE,
         MOE.CPUE,
         NHS.CPUE,
         SAB.CPUE,
         SLC.CPUE,
         BLS.CPUE,
         CS.CPUE,
         GLR.CPUE,
         LNG.CPUE,
         FCF.CPUE,
         WAS.CPUE,
         PAH.CPUE,
         RRH.CPUE,
         SLS.CPUE,
         SIL.CPUE,
         OSS.CPUE,
         SPO.CPUE,
         TME.CPUE,
         LNS.CPUE,
         BLH.CPUE
         )

#figure out how many rows are zero for each species
zero_counts_fish <- fish %>% 
  summarize(across(everything(), ~sum(. ==0)))
zero_counts_fish

#definitely remove columns for species where no lakes have any individuals (106 zeroes)
fish2 <- fish %>% 
  select(-GOE.CPUE, -CRC.CPUE, -NHS.CPUE, -BLS.CPUE, -PAH.CPUE, -LNS.CPUE, -BLH.CPUE)

#okay lets say that at least 5% of the lakes need to have the species
106*0.95
#so anything with more than 100 zero rows will be removed
fish3 <- fish2 %>% 
  select(-LKS.CPUE, -GRR.CPUE, -FRD.CPUE, -QBS.CPUE, -TRP.CPUE, -GIS.CPUE, -WHC.CPUE, -SNG.CPUE, -BIB.CPUE, -WHB.CPUE, -TPM.CPUE, -RBT.CPUE,
         -LAT.CPUE, -BKT.CPUE, -PRD.CPUE, -MOE.CPUE, -SAB.CPUE, -SLC.CPUE, -CS.CPUE, -GLR.CPUE, -LNG.CPUE, -FCF.CPUE, -WAS.CPUE, -RRH.CPUE, -SLS.CPUE, 
         -OSS.CPUE, -SPO.CPUE, -TME.CPUE)

  #check
zero_counts_fish3 <- fish3 %>% 
  summarize(across(everything(), ~sum(. ==0)))
zero_counts_fish3
#This looks ok for now

#now add a row that is total fish abundance for each lake/year
fish3$Total.CPUE <- rowSums(fish3[,5:33])
#center and scale total abundance variable 
fish3$Total.CPUE.scaled <- scale(fish3$Total.CPUE)

#create a metric that shows relative abundance of walleye vs. LMB for coloring / categorizing lakes
#ratio of walleye:bass, avoiding errors when denominaor = 0
fish3$WAE.LMB.Ind <- fish3$WAE.CPUE/(fish3$LMB.CPUE+0.01) #this takes care of the zeroes
  #look at a histogram of this metric
  hist(fish3$WAE.LMB.Ind)
#lets center and scale this metric
fish3$WAE.LMB.Ind.scale <- scale(fish3$WAE.LMB.Ind)
hist(fish3$WAE.LMB.Ind.scale)
#still skewed = good, but now 0 is the mean of the walleye vs. bass division
#make the NA values 4.5 because this is the upper edge of the range and shows walleye dominance (there are no bass)

#how different is this if I include all centrarchids as the denominator?
fish3$WAE.Cent.Ind <- fish3$WAE.CPUE/(fish3$LMB.CPUE + fish3$BLG.CPUE + fish3$HSF.CPUE + fish3$BLC.CPUE + fish3$RKB.CPUE + fish3$PMK.CPUE + fish3$GSF.CPUE + fish3$SMB.CPUE +1) #extra bit for the one lake with no centrarchids (denominator = 0)
#look at a histogram of this metric
hist(fish3$WAE.Cent.Ind)
#lets center and scale this metric
fish3$WAE.Cent.Ind.scale <- scale(fish3$WAE.Cent.Ind)
hist(fish3$WAE.Cent.Ind.scale)

# #DO A PCA ON RAW FISH DATA - COMMENTED OUT BECAUSE NOT GOING TO USE THIS
# #select just the columns for the pca
# fish.pca <- fish3 %>% 
#   select(-LakeName, -parentdow.fish.year, -lake_id, -year, -Total.CPUE.scaled, -Total.CPUE)
#
# pca.fish <- prcomp(fish.pca)
# 
#   #look at importance of each PC axis
#   summary(pca.fish)
#   #create a vector that has the proportion of variance explained by each new principal component
#   eigval.fish <- pca.fish$sdev^2/sum(pca.fish$sdev^2)
#          #eigval.fish
#   #look at the eigenvectors
#   pca.fish
#   #create a matrix of the eigenvectors
#   eigvec.fish <- pca.fish$rotation
#         #eigvec.fish
#   #create a matrix that has the PC scores of the lake-years
#   scores.fish <- pca.fish$x
#   rownames(scores.fish) <- fish3$parentdow.fish.year
#         #head(scores.fish)
#   
#   #plot the first two PC axes, color by lake
#       #apply the lake color vector
#       col_vector <- lake.colors[fish3$LakeName]
#   #make sure axes are the same scale and export as a square
#   plot(scores.fish[,1], scores.fish[,2], xlab="PC1 (44.9%)", ylab="PC2 (17.2%)", ylim = c(-10,80), xlim = c(-10,80), cex=0.8, pch=19, col=col_vector, cex.lab=1.25)
#   abline(h=0,v=0,lty=2)
#   legend(x=50,y=80,pch=19,legend=c("Bearhead", "Belle", "Carlos","Carrie","Cass","Cedar","Cut Foot Sioux", "Elk",              
#                                      "Freeborn","Garfield","Green", "Greenwood","Hill", "Kabetogama","Lake of the Woods",
#                                      "Leech",  "Madison","Mille Lacs","Minnetonka","Pearl", "Peltier", "Pepin",
#                                      "Portage","Sand Point", "South Center", "Tait","Ten Mile", "Trout", "Vermilion",
#                                      "White Iron", "Winnibigoshish"), 
#                       col=c("red", "blue", "green3", "orange", "purple", "brown", "pink", "gold",
#                             "cyan", "magenta", "darkgreen", "darkblue", "darkred", "darkorange",
#                             "darkviolet", "deepskyblue", "chartreuse3", "orchid", "sienna", "steelblue",
#                             "turquoise", "slateblue", "firebrick", "tomato", "yellow3", "gray40",
#                             "olivedrab", "mediumvioletred", "dodgerblue", "indianred", "black"),cex=.8)
#       #I don't think coloring by lake adds value - lets do it without that
#   #make sure axes are the same scale and export as a square
#   plot(scores.fish[,1], scores.fish[,2], xlab="PC1 (44.9%)", ylab="PC2 (17.1%)", ylim = c(-10,80), xlim = c(-10,80), cex=0.8, pch=19, cex.lab=1.25)
#   abline(h=0,v=0,lty=2)
#   
#   #we have a big outlier - probably driving up importance of PC2 with the white sucker outlier in Peltier
#   
#   #lets make a biplot out of this
#   biplot(pca.fish)
#   #ok this is way too much to look at and is very clumped, but this is visual confirmation that PC1 is almost entirely yellow perch and PC2 is white sucker and black crappie
#   fviz_pca_biplot(pca.fish,
#                   repel = TRUE,     # avoids text overlap
#                   col.var = "red",  # variable arrows color
#                   col.ind = "blue",  # individuals color
#                   label = "var"    #only label the variables, not the individuals
#                   )
#   #clean up the variable labels if I want to use this biplot for anything

#DO A PCA ON HELLINGER-TRANSFORMED DATA - THIS REDUCES THE EFFECT OF HIGHLY ABUNDANT SPECIES AND INCREASES THE INFLUENCE OF RARE SPECIES
  #do the transformation
  fish.Htrans <- decostand(fish3[,5:33], method = "hellinger")
  #add in the centered and scaled total fish abundance metric
  fish.Htrans$Total.CPUE.scaled <- fish3$Total.CPUE.scaled

  #now do the PCA on this data and see how it changes
  pca.fish.Htrans <- prcomp(fish.Htrans, scale. = TRUE)
  
  #look at importance of each PC axis
  summary(pca.fish.Htrans)
  #create a vector that has the proportion of variance explained by each new principal component
  eigval.fish.Htrans <- pca.fish.Htrans$sdev^2/sum(pca.fish.Htrans$sdev^2)
  eigval.fish.Htrans
  #look at the eigenvectors
  pca.fish.Htrans
  #create a matrix of the eigenvectors
  eigvec.fish.Htrans <- pca.fish.Htrans$rotation
  #eigvec.fish.Htrans
  #create a matrix that has the PC scores of the lake-years
  scores.fish.Htrans <- pca.fish.Htrans$x
  rownames(scores.fish.Htrans) <- fish3$parentdow.fish.year
  #head(scores.fish.Htrans)
  
  # #plot the first two PC axes, color by lake - DON'T BOTHER
  # #apply the lake color vector
  # col_vector <- lake.colors[fish3$LakeName]
  # #make sure axes are the same scale and export as a square
  # plot(scores.fish.Htrans[,1], scores.fish.Htrans[,2], xlab="PC1 (20.6%)", ylab="PC2 (11.3%)", ylim = c(-0.7,0.8), xlim = c(-0.7,0.8), cex=0.8, pch=19, col=col_vector, cex.lab=1.25)
  # abline(h=0,v=0,lty=2)
  # legend(x=.8,y=.8,pch=19,legend=c("Bearhead", "Belle", "Carlos","Carrie","Cass","Cedar","Cut Foot Sioux", "Elk",              
  #                                  "Freeborn","Garfield","Green", "Greenwood","Hill", "Kabetogama","Lake of the Woods",
  #                                  "Leech",  "Madison","Mille Lacs","Minnetonka","Pearl", "Peltier", "Pepin",
  #                                  "Portage","Sand Point", "South Center", "Tait","Ten Mile", "Trout", "Vermilion",
  #                                  "White Iron", "Winnibigoshish"), 
  #        col=c("red", "blue", "green3", "orange", "purple", "brown", "pink", "gold",
  #              "cyan", "magenta", "darkgreen", "darkblue", "darkred", "darkorange",
  #              "darkviolet", "deepskyblue", "chartreuse3", "orchid", "sienna", "steelblue",
  #              "turquoise", "slateblue", "firebrick", "tomato", "yellow3", "gray40",
  #              "olivedrab", "mediumvioletred", "dodgerblue", "indianred", "black"),cex=.8)
  # 
  
  #add color scale variables to PCA output
fish.pca.plot.data <- data.frame(scores.fish.Htrans, fish3$WAE.LMB.Ind.scale, fish3$WAE.Cent.Ind.scale)
 
  #I don't think coloring by lake adds value - lets do it without that
  #make sure axes are the same scale and export as a square
  plot(scores.fish.Htrans[,1], scores.fish.Htrans[,2], xlab="PC1 (20.6%)", ylab="PC2 (11.3%)", ylim = c(-6,6.2), xlim = c(-6,6.2), cex=0.8, pch=19, cex.lab=1.25)
  abline(h=0,v=0,lty=2)
  
  #ggplot to color by walleye vs. bass ratio
  #tiff("PCA_Fish_HTrans_TA.tiff", width = 10, height = 7, units = "in", res = 300)
  ggplot(fish.pca.plot.data, aes(x = PC1, y = PC2, color = fish3.WAE.LMB.Ind.scale))+
    geom_point()+
    #scale_color_gradient(low = "blue", high = "red")+
    labs(title = "Fish Community PCA", y = "PC2 (11.3%)", x = "PC1 (20.6%)") +
    scale_x_continuous(limits = c(-6.5,6.5))+
    scale_y_continuous(limits = c(-6.5,6.5))+
    scale_color_gradient(low = "blue", high = "red")+
    coord_fixed()+
    theme_classic()
  #dev.off()
  
  # #ggplot to color by walleye vs. centrarchid ratio - same pattern as bass but harder to see
  # #ggplot to color by walleye vs. bass ratio
  # ggplot(fish.pca.plot.data, aes(x = PC1, y = PC2, color = fish3.WAE.Cent.Ind.scale))+
  #   geom_point()+
  #   #scale_color_gradient(low = "blue", high = "red")+
  #   labs(title = "Fish Community PCA", y = "PC2 (8.7%)", x = "PC1 (75.1%)") +
  #   scale_x_continuous(limits = c(-2,5))+
  #   scale_y_continuous(limits = c(-2,5))+
  #   scale_color_gradient(low = "blue", high = "red")+
  #   #scale_color_viridis(option = "viridis", direction = -1, limits = c(-4,4))+
  #             #can play with this fancy color scale if you want to
  #   theme_classic()
  
  # #lets make a biplot out of this
  # biplot(pca.fish.Htrans)
  #ok this is way too much to look at and is very clumped, but this is visual confirmation that PC1 is almost entirely yellow perch and PC2 is white sucker and black crappie
  #tiff("PCA_biplot_Fish_HTrans_TA.tiff", width = 10, height = 7, units = "in", res = 300)
  fviz_pca_biplot(pca.fish.Htrans,
                  repel = TRUE,     # avoids text overlap
                  col.var = "red",  # variable arrows color
                  col.ind = "blue",  # individuals color
                  label = "var",    #only label the variables, not the individuals
                  asp = 1           #makes axis scales the same
  )
  #dev.off()
  #clean up the variable labels if I want to use this biplot for anything
  
  #this solved the outlier problem, now we see most variation driven by yellow perch and cisco
  #may want to explore later PC axes when I have time
  

  
#DO THE FISH PCA WITHOUT TOTAL ABUNDANCE
  #do the transformation
  fish.Htrans2 <- decostand(fish3[,5:33], method = "hellinger")
  #don't add the total abundance to this
  
  #now do the PCA on this data and see how it changes
  pca.fish.Htrans2 <- prcomp(fish.Htrans2)
  
  #look at importance of each PC axis
  summary(pca.fish.Htrans2)
  #create a vector that has the proportion of variance explained by each new principal component
  eigval.fish.Htrans2 <- pca.fish.Htrans2$sdev^2/sum(pca.fish.Htrans2$sdev^2)
  eigval.fish.Htrans2
  #look at the eigenvectors
  pca.fish.Htrans2
  #create a matrix of the eigenvectors
  eigvec.fish.Htrans2 <- pca.fish.Htrans2$rotation
  #eigvec.fish.Htrans
  #create a matrix that has the PC scores of the lake-years
  scores.fish.Htrans2 <- pca.fish.Htrans2$x
  rownames(scores.fish.Htrans2) <- fish3$parentdow.fish.year
  #head(scores.fish.Htrans)
  
  #add color scale variables to PCA output
  fish.pca.plot.data2 <- data.frame(scores.fish.Htrans2, fish3$WAE.LMB.Ind.scale, fish3$WAE.Cent.Ind.scale)
  
  #I don't think coloring by lake adds value - lets do it without that
  #make sure axes are the same scale and export as a square
  plot(scores.fish.Htrans2[,1], scores.fish.Htrans2[,2], xlab="PC1 (35.8%)", ylab="PC2 (13.8%)", ylim = c(-0.7,0.8), xlim = c(-0.7,0.8), cex=0.8, pch=19, cex.lab=1.25)
  abline(h=0,v=0,lty=2)
  
  #ggplot to color by walleye vs. bass ratio
  #tiff("PCA_Fish_HTrans.tiff", width = 10, height = 7, units = "in", res = 300)
  ggplot(fish.pca.plot.data2, aes(x = PC1, y = PC2, color = fish3.WAE.LMB.Ind.scale))+
    geom_point()+
    #scale_color_gradient(low = "blue", high = "red")+
    labs(title = "Fish Community PCA", y = "PC2 (13.8%)", x = "PC1 (35.8%)") +
    #scale_x_continuous(limits = c(-0.7,0.8))+
    #scale_y_continuous(limits = c(-0.7,0.8))+
    scale_color_gradient(low = "blue", high = "red")+
    coord_fixed()+
    theme_classic()
  #dev.off()
  
  
  
  # #lets make a biplot out of this
  # biplot(pca.fish.Htrans)
  #tiff("PCA_biplot_Fish_HTrans.tiff", width = 10, height = 7, units = "in", res = 300)
  fviz_pca_biplot(pca.fish.Htrans2,
                  repel = TRUE,     # avoids text overlap
                  col.var = "red",  # variable arrows color
                  col.ind = "blue",  # individuals color
                  label = "var",    #only label the variables, not the individuals
                  asp = 1           #makes axis scales the same
  )
  #dev.off()

#----------------------------------------------------------------------------------------------------------------------------------------------

#ZOOPS!!!!!!!!    PCA

#isolate zoop columns you want
zoop <- Data %>% 
  select(lake_id, LakeName, year, parentdow.fish.year,
         Acroperus.harpae,
         Alona.sp.,
         Alonella.sp.,
         Bosminidae,
         Camptocercus.sp.,
         Ceriodaphnia.sp.,
         Chydorus.sphaericus,
         Daphnia.ambigua,
         Daphnia.catawba,
         Daphnia.galeata.mendotae,
         Daphnia.laevis,
         Daphnia.longiremis,
         Daphnia.lumholtzi,
         Daphnia.parvula,
         Daphnia.pulicaria,
         Daphnia.retrocurva,
         Daphnia.rosea,
         Diaphanosoma.birgei,
         Eubosmina.coregoni,
         Eurycercus.lamellatus,
         Graptoleberis.sp.,
         Holopedium.gibberum,
         Ilyocryptus.sp.,
         Latona.setifera,
         Macrothricidae,
         Moina.sp.,
         Polyphemus.pediculus,
         Scapholeberis.sp.,
         Sida.crystallina,
         Simocephalus.sp.,
         calanoids,
         copepodites,
         cyclopoids,
         nauplii
         )

#figure out how many rows are zero for each taxon
zero_counts_zoop <- zoop %>% 
  summarize(across(everything(), ~sum(. ==0)))
zero_counts_zoop

#definitely remove columns for species where no lakes have any individuals (106 zeroes)
zoop2 <- zoop %>% 
  select(-Alonella.sp., -Camptocercus.sp., -Daphnia.catawba, -Daphnia.laevis, -Graptoleberis.sp., -Ilyocryptus.sp.)

#okay lets say that at least 5% of the lakes need to have the species
106*0.95
#so anything with more than 100 zero rows will be removed
zoop3 <- zoop2 %>% 
  select(-Daphnia.ambigua, -Daphnia.lumholtzi, -Daphnia.rosea, -Macrothricidae, -Moina.sp., -Polyphemus.pediculus, -Scapholeberis.sp., -Simocephalus.sp.)

#check
zero_counts_zoop3 <- zoop3 %>% 
  summarize(across(everything(), ~sum(. ==0)))
zero_counts_zoop3
#This looks ok for now

#now add a row that is total zoop biomass for each lake/year
zoop3$Total.biomass <- rowSums(zoop3[,5:24])
#center and scale total abundance variable 
zoop3$Total.biomass.scaled <- scale(zoop3$Total.biomass)


# #DO A PCA ON RAW ZOOP DATA - never even added total biomass to this because transformed data is better
# pca.zoop <- prcomp(zoop3[,5:24])
# 
#     #look at importance of each PC axis
#     summary(pca.zoop)
#     #create a vector that has the proportion of variance explained by each new principal component
#     eigval.zoop <- pca.zoop$sdev^2/sum(pca.zoop$sdev^2)
#     #eigval.zoop
#     #look at the eigenvectors
#     pca.zoop
#     #create a matrix of the eigenvectors
#     eigvec.zoop <- pca.zoop$rotation
#     #eigvec.zoop
#     #create a matrix that has the PC scores of the lake-years
#     scores.zoop <- pca.zoop$x
#     rownames(scores.zoop) <- zoop3$parentdow.fish.year
#     #head(scores.zoop)
#     
#     #plot the first two PC axes, color by lake
#     #apply the lake color vector
#     col_vector <- lake.colors[zoop3$LakeName]
#     #make sure axes are the same scale and export as a square
#     plot(scores.zoop[,1], scores.zoop[,2], xlab="PC1 (55.1%)", ylab="PC2 (16.2%)",  ylim = c(-150,150), xlim = c(-150,150),cex=0.8, pch=19, col=col_vector, cex.lab=1.25)
#     abline(h=0,v=0,lty=2)
#     legend(x=-150,y=160,pch=19,legend=c("Bearhead", "Belle", "Carlos","Carrie","Cass","Cedar","Cut Foot Sioux", "Elk",              
#                                      "Freeborn","Garfield","Green", "Greenwood","Hill", "Kabetogama","Lake of the Woods",
#                                      "Leech",  "Madison","Mille Lacs","Minnetonka","Pearl", "Peltier", "Pepin",
#                                      "Portage","Sand Point", "South Center", "Tait","Ten Mile", "Trout", "Vermilion",
#                                      "White Iron", "Winnibigoshish"), 
#            col=c("red", "blue", "green3", "orange", "purple", "brown", "pink", "gold",
#                  "cyan", "magenta", "darkgreen", "darkblue", "darkred", "darkorange",
#                  "darkviolet", "deepskyblue", "chartreuse3", "orchid", "sienna", "steelblue",
#                  "turquoise", "slateblue", "firebrick", "tomato", "yellow3", "gray40",
#                  "olivedrab", "mediumvioletred", "dodgerblue", "indianred", "black"),cex=.8)
#     #I don't think coloring by lake adds value - lets do it without that
#     #make sure axes are the same scale and export as a square
#     plot(scores.zoop[,1], scores.zoop[,2], xlab="PC1 (55.1%)", ylab="PC2 (16.2%)",  ylim = c(-150,150), xlim = c(-150,150),cex=0.8, pch=19, cex.lab=1.25)
#     abline(h=0,v=0,lty=2)
#     
#     #we have a big outlier - probably driving up importance of PC2 - did not investigate what it is because it's late and I am tired :(
#     
#     #lets make a biplot out of this
#     biplot(pca.zoop)
#     #make a better biplot
#     fviz_pca_biplot(pca.zoop,
#                     repel = TRUE,     # avoids text overlap
#                     col.var = "red",  # variable arrows color
#                     col.ind = "blue",  # individuals color
#                     label = "var"    #only label the variables, not the individuals
#     )
#     #clean up the variable labels if I want to use this biplot for anything
#     
#     #overall, we see that calanoids vs. cyclopoids makes a big difference on PC2, Daphnia galeata mendotae, Daphnia pulicaria, and E. coregoni also important

#DO A PCA ON HELLINGER-TRANSFORMED ZOOP DATA - THIS REDUCES THE EFFECT OF HIGHLY ABUNDANT SPECIES AND INCREASES THE INFLUENCE OF RARE SPECIES

    #do the transformation
    zoop.Htrans <- decostand(zoop3[,5:24], method = "hellinger")
    #add in the centered and scaled total zoop biomass metric
    zoop.Htrans$Total.biomass.scaled <- zoop3$Total.biomass.scaled
    
    #now do the PCA on this data and see how it changes
    pca.zoop.Htrans <- prcomp(zoop.Htrans, scale. = TRUE) #scale to use correlation matrix here - no longer same unit and scale
    
    #look at importance of each PC axis
    summary(pca.zoop.Htrans)
          #now we are explaining much less of the variance
    #create a vector that has the proportion of variance explained by each new principal component
    eigval.zoop.Htrans <- pca.zoop.Htrans$sdev^2/sum(pca.zoop.Htrans$sdev^2)
    eigval.zoop.Htrans
    #look at the eigenvectors
    pca.zoop.Htrans
    #create a matrix of the eigenvectors
    eigvec.zoop.Htrans <- pca.zoop.Htrans$rotation
    #eigvec.zoop.Htrans
    #create a matrix that has the PC scores of the lake-years
    scores.zoop.Htrans <- pca.zoop.Htrans$x
    rownames(scores.zoop.Htrans) <- zoop3$parentdow.fish.year
    #head(scores.zoop.Htrans)
    
    # #plot the first two PC axes, color by lake - DON't BOTHEr
    # #apply the lake color vector
    # col_vector <- lake.colors[zoop3$LakeName]
    # #make sure axes are the same scale and export as a square
    # plot(scores.zoop.Htrans[,1], scores.zoop.Htrans[,2], xlab="PC1 (16.2%)", ylab="PC2 (13.3%)", ylim = c(-0.7,0.5), xlim = c(-0.7,0.5), cex=0.8, pch=19, col=col_vector, cex.lab=1.25)
    # abline(h=0,v=0,lty=2)
    # legend(x=.8,y=.8,pch=19,legend=c("Bearhead", "Belle", "Carlos","Carrie","Cass","Cedar","Cut Foot Sioux", "Elk",              
    #                                  "Freeborn","Garfield","Green", "Greenwood","Hill", "Kabetogama","Lake of the Woods",
    #                                  "Leech",  "Madison","Mille Lacs","Minnetonka","Pearl", "Peltier", "Pepin",
    #                                  "Portage","Sand Point", "South Center", "Tait","Ten Mile", "Trout", "Vermilion",
    #                                  "White Iron", "Winnibigoshish"), 
    #        col=c("red", "blue", "green3", "orange", "purple", "brown", "pink", "gold",
    #              "cyan", "magenta", "darkgreen", "darkblue", "darkred", "darkorange",
    #              "darkviolet", "deepskyblue", "chartreuse3", "orchid", "sienna", "steelblue",
    #              "turquoise", "slateblue", "firebrick", "tomato", "yellow3", "gray40",
    #              "olivedrab", "mediumvioletred", "dodgerblue", "indianred", "black"),cex=.8)
    
    #I don't think coloring by lake adds value - lets do it without that
    #make sure axes are the same scale and export as a square
    plot(scores.zoop.Htrans[,1], scores.zoop.Htrans[,2], xlab="PC1 (16.2%)", ylab="PC2 (13.3%)", ylim = c(-4.5,4), xlim = c(-4.5,4), cex=0.8, pch=19, cex.lab=1.25)
    abline(h=0,v=0,lty=2)
    
    #add color scale variables to PCA output
    zoop.pca.plot.data <- data.frame(scores.zoop.Htrans, fish3$WAE.LMB.Ind.scale, fish3$WAE.Cent.Ind.scale)
    
    #ggplot to color by walleye vs. bass ratio
    #tiff("PCA_Zoop_HTrans_TB.tiff", width = 10, height = 7, units = "in", res = 300)
    ggplot(zoop.pca.plot.data, aes(x = PC1, y = PC2, color = fish3.WAE.LMB.Ind.scale))+
      geom_point()+
      #scale_color_gradient(low = "blue", high = "red")+
      labs(title = "Zoop Community PCA", x = "PC1 (16.2%)", y = "PC2 (13.3%)") +
      scale_x_continuous(limits = c(-4.5,4))+
      scale_y_continuous(limits = c(-4.5,4))+
      scale_color_gradient(low = "blue", high = "red")+
      coord_fixed()+
      theme_classic()
    #dev.off()
    
    # #ggplot to color by walleye vs. centrarchid ratio - same pattern as bass but harder to see
    # #ggplot to color by walleye vs. bass ratio
    # ggplot(zoop.pca.plot.data, aes(x = PC1, y = PC2, color = fish3.WAE.Cent.Ind.scale))+
    #   geom_point()+
    #   #scale_color_gradient(low = "blue", high = "red")+
    #   labs(title = "Zoop Community PCA", x = "PC1 (86.7%)", y = "PC2 (3.2%)") +
    #   scale_x_continuous(limits = c(-4.5,1))+
    #   scale_y_continuous(limits = c(-4.5,1))+
    #   scale_color_gradient(low = "blue", high = "red")+
    #   #scale_color_viridis(option = "viridis", direction = -1, limits = c(-4,4))+
    #   #can play with this fancy color scale if you want to
    #   theme_classic()
    # 
    # #lets make a biplot out of this
    # biplot(pca.zoop.Htrans)
    #ok this is way too much to look at and is very clumped, but this is visual confirmation that PC1 is almost entirely yellow perch and PC2 is white sucker and black crappie
    #tiff("PCA_biplot_Zoop_HTrans_TB.tiff", width = 10, height = 7, units = "in", res = 300)
    fviz_pca_biplot(pca.zoop.Htrans,
                    repel = TRUE,     # avoids text overlap
                    col.var = "red",  # variable arrows color
                    col.ind = "blue",  # individuals color
                    label = "var",    #only label the variables, not the individuals
                  )
    #dev.off()
    #similar most important species, describing less variance after transformation, there are other important PC axes. 
    #dominated by total biomass

    
#DO THE ZOOP PCA ON HELLINGER TRANSFORMED DATA WITHOUT TOTAL BIOMASS
    #take the total biomass out of the dataset
    zoop.Htrans.noTB <- select(zoop.Htrans, -Total.biomass.scaled)
    
    #now do the PCA on this data and see how it changes
    pca.zoop.Htrans.noTB <- prcomp(zoop.Htrans.noTB)
    
    #look at importance of each PC axis
    summary(pca.zoop.Htrans.noTB)
    #now we are explaining much less of the variance
    #create a vector that has the proportion of variance explained by each new principal component
    eigval.zoop.Htrans.noTB <- pca.zoop.Htrans.noTB$sdev^2/sum(pca.zoop.Htrans.noTB$sdev^2)
    #look at eigenvalues
    eigval.zoop.Htrans.noTB
    #look at the eigenvectors
    pca.zoop.Htrans.noTB
    #create a matrix of the eigenvectors
    eigvec.zoop.Htrans.noTB <- pca.zoop.Htrans.noTB$rotation
    #eigvec.zoop.Htrans
    #create a matrix that has the PC scores of the lake-years
    scores.zoop.Htrans.noTB <- pca.zoop.Htrans.noTB$x
    rownames(scores.zoop.Htrans.noTB) <- zoop3$parentdow.fish.year
    
    #I don't think coloring by lake adds value - lets do it without that
    #make sure axes are the same scale and export as a square
    plot(scores.zoop.Htrans.noTB[,1], scores.zoop.Htrans.noTB[,2], xlab="PC1 (24.5%)", ylab="PC2 (18.3%)", ylim = c(-0.65,0.5), xlim = c(-0.65,0.5), cex=0.8, pch=19, cex.lab=1.25)
    abline(h=0,v=0,lty=2)
    
    #add color scale variables to PCA output
    zoop.pca.plot.data.noTB <- data.frame(scores.zoop.Htrans.noTB, fish3$WAE.LMB.Ind.scale, fish3$WAE.Cent.Ind.scale)
    
    #ggplot to color by walleye vs. bass ratio
    #tiff("PCA_Zoop_HTrans.tiff", width = 10, height = 7, units = "in", res = 300)
    ggplot(zoop.pca.plot.data.noTB, aes(x = PC1, y = PC2, color = fish3.WAE.LMB.Ind.scale))+
      geom_point()+
      #scale_color_gradient(low = "blue", high = "red")+
      labs(title = "Zoop Community PCA", x = "PC1 (24.5%)", y = "PC2 (18.3%)") +
      #scale_x_continuous(limits = c(-0.65,0.5))+
      #scale_y_continuous(limits = c(-0.65,0.5))+
      scale_color_gradient(low = "blue", high = "red")+
      coord_fixed()+
      theme_classic()
    #dev.off()
    # 
    # #lets make a biplot out of this
    # biplot(pca.zoop.Htrans)
    #ok this is way too much to look at and is very clumped, but this is visual confirmation that PC1 is almost entirely yellow perch and PC2 is white sucker and black crappie
    #tiff("PCA_biplot_Zoop_HTrans.tiff", width = 10, height = 7, units = "in", res = 300)
    fviz_pca_biplot(pca.zoop.Htrans.noTB,
                    repel = TRUE,     # avoids text overlap
                    col.var = "red",  # variable arrows color
                    col.ind = "blue",  # individuals color
                    label = "var",    #only label the variables, not the individuals
                    asp = 1           #makes axis scales the same
    )
    #dev.off()
    
 
#-----------------------------------------------------------------------------------------------------------------------------------------------
  #PCA OF FISH AND ZOOPS TOGETHER WITH TOTAL ABUNDANCE/BIOMASS
    #use correlation matrix here (probably should have above but oh well and not going to use it anwyays)
    
    #combine the data
    FZ.data <- data.frame(fish.Htrans, zoop.Htrans)
    
    #now do the PCA on this data
    pca.fish.zoop.T <- prcomp(FZ.data, scale. = TRUE) #scale argument uses correlation matrix instead of covariance matrix
    
    #look at importance of each PC axis
    summary(pca.fish.zoop.T)
    #now we are explaining much less of the variance
    #create a vector that has the proportion of variance explained by each new principal component
    eigval.fish.zoop.T <- pca.fish.zoop.T$sdev^2/sum(pca.fish.zoop.T$sdev^2)
    #look at eigenvalues
    eigval.fish.zoop.T
    #look at the eigenvectors
    pca.fish.zoop.T
    #create a matrix of the eigenvectors
    eigvec.fish.zoop.T <- pca.fish.zoop.T$rotation
    #eigvec.zoop.Htrans
    #create a matrix that has the PC scores of the lake-years
    scores.fish.zoop.T <- pca.fish.zoop.T$x
    rownames(scores.fish.zoop.T) <- zoop3$parentdow.fish.year
    
    #I don't think coloring by lake adds value - lets do it without that
    #make sure axes are the same scale and export as a square
    plot(scores.fish.zoop.T[,1], scores.fish.zoop.T[,2], xlab="PC1 (15.4%)", ylab="PC2 (9.1%)", ylim = c(-6,6), xlim = c(-6,6), cex=0.8, pch=19, cex.lab=1.25)
    abline(h=0,v=0,lty=2)
    
    #add color scale variables to PCA output
    fish.zoop.pca.plot.data.T <- data.frame(scores.fish.zoop.T, fish3$WAE.LMB.Ind.scale, fish3$WAE.Cent.Ind.scale)
    
    #ggplot to color by walleye vs. bass ratio
    #tiff("PCA_Fish_Zoop_T.tiff", width = 10, height = 7, units = "in", res = 300)
    ggplot(fish.zoop.pca.plot.data.T, aes(x = PC1, y = PC2, color = fish3.WAE.LMB.Ind.scale))+
      geom_point()+
      #scale_color_gradient(low = "blue", high = "red")+
      labs(title = "Fish and Zoop Community PCA", x = "PC1 (15.4%)", y = "PC2 (9.1%)") +
      scale_x_continuous(limits = c(-6,6))+
      scale_y_continuous(limits = c(-6,6))+
      scale_color_gradient(low = "blue", high = "red")+
      coord_fixed()+
      theme_classic()
    #dev.off()
    # 
    # #lets make a biplot out of this
    #tiff("PCA_biplot_Fish_Zoop_T.tiff", width = 10, height = 7, units = "in", res = 300)
    fviz_pca_biplot(pca.fish.zoop.T,
                    repel = TRUE,     # avoids text overlap
                    col.var = "red",  # variable arrows color
                    col.ind = "blue",  # individuals color
                    label = "var"    #only label the variables, not the individuals
    )
    #dev.off()
    
    
    
    
    
    
  #PCA OF FISH AND ZOOPS TOGETHER WITHOUT TOTAL ABUNDANCE/BIOMASS
    #remove total abundance and biomass
    FZ.data2 <- select(FZ.data, -Total.biomass.scaled, -Total.CPUE.scaled)
    
    #now do the PCA on this data
    pca.fish.zoop <- prcomp(FZ.data2, scale. = TRUE) #scale argument uses correlation matrix instead of covariance matrix
    
    #look at importance of each PC axis
    summary(pca.fish.zoop)
    #now we are explaining much less of the variance
    #create a vector that has the proportion of variance explained by each new principal component
    eigval.fish.zoop <- pca.fish.zoop$sdev^2/sum(pca.fish.zoop$sdev^2)
    #look at eigenvalues
    eigval.fish.zoop
    #look at the eigenvectors
    pca.fish.zoop
    #create a matrix of the eigenvectors
    eigvec.fish.zoop <- pca.fish.zoop$rotation
    #eigvec.zoop.Htrans
    #create a matrix that has the PC scores of the lake-years
    scores.fish.zoop <- pca.fish.zoop$x
    rownames(scores.fish.zoop) <- zoop3$parentdow.fish.year
    
    #I don't think coloring by lake adds value - lets do it without that
    #make sure axes are the same scale and export as a square
    plot(scores.fish.zoop[,1], scores.fish.zoop[,2], xlab="PC1 (15.2%)", ylab="PC2 (8.7%)", ylim = c(-6,6), xlim = c(-6,6), cex=0.8, pch=19, cex.lab=1.25)
    abline(h=0,v=0,lty=2)
    
    #add color scale variables to PCA output
    fish.zoop.pca.plot.data <- data.frame(scores.fish.zoop, fish3$WAE.LMB.Ind.scale, fish3$WAE.Cent.Ind.scale)
    
    #ggplot to color by walleye vs. bass ratio
    #tiff("PCA_Fish_Zoop.tiff", width = 10, height = 7, units = "in", res = 300)
    ggplot(fish.zoop.pca.plot.data, aes(x = PC1, y = PC2, color = fish3.WAE.LMB.Ind.scale))+
      geom_point()+
      #scale_color_gradient(low = "blue", high = "red")+
      labs(title = "Fish and Zoop Community PCA", x = "PC1 (15.2%)", y = "PC2 (8.7%)") +
      scale_x_continuous(limits = c(-6,6))+
      scale_y_continuous(limits = c(-6,6))+
      scale_color_gradient(low = "blue", high = "red")+
      coord_fixed()+
      theme_classic()
    #dev.off()
    # 
    # #lets make a biplot out of this
    #tiff("PCA_biplot_Fish_Zoop.tiff", width = 10, height = 7, units = "in", res = 300)
    fviz_pca_biplot(pca.fish.zoop,
                    repel = TRUE,     # avoids text overlap
                    col.var = "red",  # variable arrows color
                    col.ind = "blue",  # individuals color
                    label = "var"    #only label the variables, not the individuals
    )
    #dev.off()
    
    
#--------------------------------------------------------------------------------------------------------------------------------
#PUT A MINIMUM SPANNING TREE ON BEST PCA (fish and zoops together, Hellinger transformed, with total abundance and biomass, scaled with correlation matrix)
    #PURPOSE = LOOK AT DISTORTION BECAUSE THESE LINES CONNECT MOST CLOSELY RELATED POINTS ON ALL PC AXES (ENTIRE MULTIVARIATE SPACE), NOT JUST THE FIRST 2
    
    #pull out coordintes from the PCA you want to use (which is all of them to include the full-dimensional space)
    coords <- as.data.frame(pca.fish.zoop.T$x)
    coords$label <- rownames(coords)
    
    #calculate MST
    dist_matrix <- dist(coords[,1:51])
    g <- graph_from_adjacency_matrix(as.matrix(dist_matrix), mode = "undirected", weighted = TRUE)
    mst <- mst(g)  # get the minimum spanning tree
    
    #extract MST edges to plot
    edges <- as_data_frame(mst, what = "edges")
    
    
    # Merge edges with coordinates
    edge_coords <- edges %>%
      left_join(coords, by = c("from" = "label")) %>%
      rename(x = PC1, y = PC2) %>%
      left_join(coords, by = c("to" = "label")) %>%
      rename(xend = PC1, yend = PC2)
    
    #plot
    ggplot(coords, aes(x = PC1, y = PC2)) +
      geom_point(color = "steelblue") +
      geom_segment(data = edge_coords, 
                   aes(x = x, y = y, xend = xend, yend = yend), 
                   color = "red", linewidth = 0.7) +
      theme_minimal()
    
    
#okay what we see here is actually not too much distortion - makes me happy :), especially since our PC axes explain so little variance
    
    
    
#------------------------------------------------------------------------------------------------------------------------------------------
    #DO A BGPCA BECAUSE OF REPEATED MEASURES DATA - HAVE EACH LAKE INFLUENCE ORDINATION ONLY ONCE BUT THEN PLOT ALL DATA POINTS SEPARATELY

#using the Hellinger transformed dataset with both fish and zoops, including total abundance and biomass
    #still need to scale to use correlation matrix

#need to require morpho package
require(Morpho)  

#scale data to use correlation matrix instead of covariance matrix    
   FZ.data.scale <- data.frame(scale(FZ.data))

#need to get lake info back into the FZ.data
BGPCA.FZ.data <- data.frame(fish3$LakeName, FZ.data.scale)

#run the BGPCA
bgpca <- groupPCA(BGPCA.FZ.data[,2:52], BGPCA.FZ.data$fish3.LakeName, weighting = F)
#weighting accounts for differences in group sizes - I DON'T WANT TO DO THIS - I want each unique lake to weight equally on the covariance structure, which is why I am doing this
#I don't want lakes that are sampled more to have a greater influence, so I set weighting = F (If I set Weighting = T it looks exactly like the regular PCA)

#look at eigenvalues
eigval.bgpca <- bgpca$eigenvalues/sum(bgpca$eigenvalues)
eigval.bgpca

#look at eigenvectors and extract them into a matrix
bgpca.eigvec <- data.frame(bgpca$groupPCs)
bgpca.eigvec


#extract scores to plot and color variable
bgpca.scores <- data.frame(bgpca$Scores[,1:4], fish3$WAE.LMB.Ind.scale)
bgpca.scores <- rename(bgpca.scores, "PC1" = "X1", "PC2" = "X2", "PC3" = "X3", "PC4" = "X4")




#plot the BGPCA - all lake-years, PC1 and PC2
ggplot(bgpca.scores, aes(x = PC1, y = PC2, color = fish3.WAE.LMB.Ind.scale))+
  geom_point()+
  labs(title = "Fish and Zoop Community BGPCA", x = "PC1 (17.6%)", y = "PC2 (13.1%)") +
  scale_x_continuous(limits = c(-6.3,7.6))+
  scale_y_continuous(limits = c(-6.3,7.6))+
  scale_color_gradient(low = "blue", high = "red")+
  coord_fixed()+
  theme_classic()

# #plot the BGPCA - all lake-years, PC3 and PC4
# ggplot(bgpca.scores, aes(x = PC3, y = PC4, color = fish3.WAE.LMB.Ind.scale))+
#   geom_point()+
#   labs(title = "Fish and Zoop Community BGPCA", x = "PC3 (11.1%)", y = "PC4 (7.9%)") +
#   #scale_x_continuous(limits = c(-6.3,6))+
#   #scale_y_continuous(limits = c(-6.3,6))+
#   scale_color_gradient(low = "blue", high = "red")+
#   coord_fixed()+
#   theme_classic()
# #this does not separate walleye-centrarchid... I was just curious


#plot just the group means of the BGPCA, PC1 and PC2
#CALCULATE SCORES FOR MEANS
      #average the walleye-lmb indicator for color for the lake means
      lake.walleye.indicator.mean <- fish3 %>%
        group_by(LakeName) %>%
        summarize(WAE.LMB.Ind.scale = mean(WAE.LMB.Ind.scale), .groups = 'drop')
      #extract group means as a data frame
      bgpca.means <- data.frame(bgpca$groupmeans)
      #need to turn these means into scores
      #first make them both numeric matrices and get rid of color indicator
      bgpca.eigvec.num <- as.matrix(bgpca.eigvec)
      bgpca.means.num <- as.matrix(bgpca.means)
      #now do matrix multiplication
      bgpca.means.scores <- bgpca.means.num %*% bgpca.eigvec.num
      #add color indicator variable to this
      bgpca.means.scores.color <- data.frame(bgpca.means.scores, lake.walleye.indicator.mean$WAE.LMB.Ind.scale)
      #rename the color variable so it doesn't look ridiculous
      bgpca.means.scores.color <- rename(bgpca.means.scores.color, "WalleyeVCentrarchid" = "lake.walleye.indicator.mean.WAE.LMB.Ind.scale")

#now plot on same axis scales as when you plotted all the data
ggplot(bgpca.means.scores.color, aes(x = X1, y = X2, color = WalleyeVCentrarchid))+
  geom_point()+
  labs(title = "Fish and Zoop Community BGPCA LAKE MEANS", x = "PC1 (17.6%)", y = "PC2 (13.1%)") +
  scale_x_continuous(limits = c(-6.3,7.6))+
  scale_y_continuous(limits = c(-6.3,7.6))+
  scale_color_gradient(low = "blue", high = "red")+
  coord_fixed()+
  theme_classic()
  

    
#--------------------------------------------------------------------------------------------------------------------------------------
#LOOK AT HISTOGRAMS OF SCALED INPUTS

    #create scaled dataframe
    FZ.hist.data <- data.frame(scale(FZ.data))
    #make it long format
    FZ.hist.data.long <- pivot_longer(FZ.hist.data, cols = everything(), names_to = "variable", values_to = "value")
    
    ggplot(FZ.hist.data.long, aes(x = value))+
      geom_histogram()+
      facet_wrap(~variable, scales = "free")+
      theme_minimal()
    
#This is good to look at - most things are not normally distributed, even if they aren't usually 0. 
    #may need to use NMDS... worry about this later, not for class project
             
#----------------------------------------------------------------------------------------------------------------------------------------------
    #do a correspondence analysis on fish and zoops - don't include total abundance here - this ends up being relative composition when input is "raw"/untransformed data
    require(ca) #need this package apparently
    
    #remove row 93 because it has no fish...
    fish.ca.data <- filter(fish3, Total.CPUE != 0)
    
    #do the correspondence analysis on untransformed fish data           
    fish.ca <- ca(fish.ca.data[,5:33])
    
    summary(fish.ca)          
    
    plot(fish.ca, mass = T, arrows = c(T,T))
    
    #plot the 2nd and 3rd axes
    plot(fish.ca, , mass = T, dim = c(2,3), arrows = c(T,T))
    
    
    #DO THE ZOOPS
    zoop.ca <- ca(zoop3[,5:24])
    summary(zoop.ca)
    
    plot(zoop.ca, mass = T, arrows = c(T,T))
    
    
    #Can't get rid of effects of outliers here and don't have the time to dig into it right now
    #none of the literature suggests this anyways... not going to do it
    
#-------------------------------------------------------------------------------------------------------------------------------------------
    #OKAY TIME TO DO WHAT I ACTUALLY WANT TO PRESENT:
    #Principal coordinates analysis (MDS) On Bray-curtis distances
    
    #according to https://www.davidzeleny.net/anadat-r/doku.php/en:pcoa_nmds
    #I can use bray-curtis distance for NMDS
    #if I take the square root of bray-curtis I can make it metric and use PCoA (MDS)
    #don't get species loadings, but can plot species in the same space
    
    #compile data to use for bray-curtis distance: raw fish and zoop data
    bc.data <- data.frame(fish3[,5:33], zoop3[,5:24])
    
    
    #first calculate the dissimilarity matrix based on bray-curtis distances
    bc.dist <- vegdist(bc.data, method = "bray")
    bc.dist
    #to look at the bc.dist values
    bc.dist.data.frame <- data.frame(bc.dist)
    hist(bc.dist.data.frame$bc.dist)
    
    #do the PCoA
    pcoa <- pcoa(bc.dist)
    
    
    pcoa2 <- capscale(bc.dist ~ 1)
    
    #look at eigenvalues
    pcoa$values$Relative_eig
    
    #pull out scores and add color variable
    pcoa.scores <- data.frame("PCo1" = pcoa$vectors[, 1], "PCo2" = pcoa$vectors[, 2], fish3$WAE.LMB.Ind.scale)
    
    #tiff("PCoA_Bray_Curtis.tiff", width = 10, height = 7, units = "in", res = 300)
    ggplot(pcoa.scores, aes(x = PCo1, y = PCo2, color = fish3.WAE.LMB.Ind.scale))+
      geom_point()+
      labs(title = "Fish Community PCoA based on Bray-Curtis Similarity", y = "PCo2 (17.3%)", x = "PCo1 (29.2%)") +
      scale_x_continuous(limits = c(-0.6,0.5))+
      scale_y_continuous(limits = c(-0.6,0.5))+
      scale_color_gradient(low = "blue", high = "red")+
      coord_fixed()+
      theme_classic()
    #dev.off()                      
    
    #you can't make a true biplot for a PCoA, but you can add vectors that point in the direction of strongest correlation with axes
    #fit variables as vectors
    var.vec <- envfit(pcoa$vectors, bc.data, permutations = 999)
    
    #extract variable vectors
    arrows_df <- as.data.frame(scores(var.vec, display = "vectors"))
    arrows_df$var <- rownames(arrows_df) 
    
    #scale arrows for plotting
    arrow_mult <- 0.5
    arrows_df$xend <- arrows_df$Axis.1 * arrow_mult
    arrows_df$yend <- arrows_df$Axis.2 * arrow_mult
    
    #plot it in ggplot2
    ggplot(pcoa.scores, aes(x = PCo1, y = PCo2))+
      geom_point()+
      labs(title = "Fish Community PCoA based on Bray-Curtis Similarity", y = "PCo2 (17.3%)", x = "PCo1 (29.2%)") +
      scale_x_continuous(limits = c(-0.6,0.5))+
      scale_y_continuous(limits = c(-0.6,0.5))+
      #scale_color_gradient(low = "blue", high = "red")+
      geom_segment(data = arrows_df, 
                   aes(x = 0, y = 0, xend = xend, yend = yend),
                   arrow = arrow(length = unit(0.2, "cm")),
                   color = "red")+
      geom_text(data = arrows_df,
                aes(x = xend, y = yend, label = var),
                color = "red",
                vjust = -0.05)+
      coord_fixed()+
      theme_classic()
    
    #-------------------------------------------------------------------------------------------------------------------------------------
#LETS DO THE PARTIAL LEAST SQUARES
    
    
    #DO THE PLS ON THE TRANSFORMED DATA - WITHOUT TOTAL ABUNDANCE / BIOMASS
    PLS.data.Htrans <- cbind(zoop.Htrans, fish.Htrans)
    
    PLS.data.Htrans <- PLS.data.Htrans %>% 
      select(-Total.biomass.scaled, - Total.CPUE.scaled)
    
    #create a vector with a's for left block (fish) and b's for right block (zoops) to tell geomorph which variable is in which group
    gp.Htrans <- c(rep("a",times=20),rep("b",times=29)) 
    
    #run the pls
    #arguments: data frame, vector for which variables go in which block, if you want to see progress bar)
    pls.Htrans <- integration.test(PLS.data.Htrans, partition.gp = gp.Htrans, print.progress = FALSE)
    #for lots of good info on this function, run ?integration.test
    #view summarized results
    pls.Htrans
    
    #Extract the proportional eigenvalues, **two sets** of eigenvectors, and two sets of scores from this analysis as separate objects.
    #The svd object contains the eigenvalues, but not as proportions, so we have to calculate that.
    #first view the pls$svd info:
    pls.Htrans$svd
    #calculate the eigenvalues as proportions
    #these eigenvalues are a shared set that describes how much covariation is explained by corresponding components in both blocks
    eigval.pls.Htrans <- pls.Htrans$svd$d/sum(pls.Htrans$svd$d)
    eigval.pls.Htrans
    #the first PLS component explains 33.2% of the covariation and the second pls component explains 16.0% of the covariation
    #now calculate two sets of separate eigenvectors as separate objects
    # left eigenvector matrix (the zoops)
    eigvec.zoop.pls.Htrans <- pls.Htrans$left.pls.vectors
    eigvec.zoop.pls.Htrans
    
    # right eigenvector matrix (the fish)
    eigvec.fish.pls.Htrans <- pls.Htrans$right.pls.vectors
    eigvec.fish.pls.Htrans
    #looks like walleye and LMB go in opposite directions for the first 4 pC axes (other species have strong effects too)
    
    
    #now calculate the two separate scores
    # Left block scores (the zoops)
    scores.zoop.pls.Htrans <- pls.Htrans$XScores
    rownames(scores.zoop.pls.Htrans) <- zoop3$parentdow.fish.year
    head(scores.zoop.pls.Htrans)
    
    
    # Right block scores (the fish)
    scores.fish.pls.Htrans <- pls.Htrans$YScores
    rownames(scores.fish.pls.Htrans) <- zoop3$parentdow.fish.year
    head(scores.fish.pls.Htrans)
    
    #make a dataframe with all the scores and the color variable
    pls.plot.data <- data.frame(scores.zoop.pls.Htrans, scores.fish.pls.Htrans, fish3$WAE.LMB.Ind.scale, fish3$WAE.Cent.Ind.scale)
    
    #plot the first PLS component (not dealing with colors here)
    #axes don't have to be on same scale here (I don't think)
    plot(scores.zoop.pls.Htrans[, 1], scores.fish.pls.Htrans[, 1], xlab = "Zooplankton Community", ylab = "Fish community",  main = "PLS First Components (Hellinger transformed) (33.2%)", cex = 0.7, pch = 19, ylim = c(-0.5,0.7), xlim = c(-0.5,0.7))
    #alternate way to plot this
    plot(pls.Htrans, pch = 19)
    #plot with ggplot to get the fun walleye/bass colors
    #tiff("PLS1.tiff", width = 10, height = 7, units = "in", res = 300)
    ggplot(pls.plot.data, aes(x = X1, y = X1.1, color = fish3.WAE.LMB.Ind.scale))+
      geom_point()+
      labs(title = "Two-Block Partial Least Squares Analysis", x = "Zooplankton Community PLS 1", y = "Fish Community PLS 1",) +
      scale_x_continuous(limits = c(-0.55,0.7))+
      scale_y_continuous(limits = c(-0.55,0.7))+
      scale_color_gradient(low = "blue", high = "red", name = " ", labels = c("Centrarchid Dominated", "Walleye Dominated"), breaks = c(-0.8993171, 2.6700774))+
      coord_fixed()+
      theme_classic()+
      theme(plot.title = element_text(hjust = 0.5, size = 23), legend.text = element_text(size = 12), axis.title = element_text(size = 18), axis.text= element_text(size = 12))
    #dev.off()
    
    #plot PLS axis weightings (eigenvector weightings)
    #ZOOPS
    #need a vector of names for the categories
    zoop.pls.loadings <- data.frame(eigvec.zoop.pls.Htrans)
    zoop.pls.loadings$taxa <- c("Acroperus harpae", "Alona spp.", "Bosminidae", "Ceriodaphnia spp.", "Chydorus sphaericus", 
                                "Daphnia galeata mendotae", "Daphnia longiremis", "Daphnia parvula", "Daphnia pulicaria", 
                                "Daphnia retrocurva", "Diaphanosoma birgei", "Eubosmina coregoni", "Eurycercus lamellatus", 
                                "Holopedium gibberum", "Latona setifera", "Sida crystallina", "Calanoids", "Copepodites", 
                                "Cyclopoids", "Nauplii")
    #make a column that will make sediment preservation level different colors
    zoop.pls.loadings$color.cat <- c("Well Preserved in Sediments", "Well Preserved in Sediments", "Well Preserved in Sediments", "Partially Preserved in Sediments", "Well Preserved in Sediments", 
                                     "Partially Preserved in Sediments", "Partially Preserved in Sediments", "Partially Preserved in Sediments", "Partially Preserved in Sediments", 
                                     "Partially Preserved in Sediments", "Partially Preserved in Sediments", "Well Preserved in Sediments", "Well Preserved in Sediments", 
                                     "Partially Preserved in Sediments", "Partially Preserved in Sediments", "Partially Preserved in Sediments", "Not Preserved in Sediments", "Not Preserved in Sediments", 
                                     "Not Preserved in Sediments", "Not Preserved in Sediments")
    #now plot
    #tiff("PLS1.Zoop.Loadings.tiff", width = 8, height = 5, units = "in", res = 300)
    ggplot(zoop.pls.loadings, aes(x = taxa, y = X1, fill = color.cat))+
      geom_bar(stat = "identity")+
      labs(title = "PLS 1 Loadings by Zooplankton Taxa", x = "Taxon", y = "Relative Contribution to PLS 1", fill = NULL) +
      scale_fill_manual(values = c("Well Preserved in Sediments" = "navyblue", "Partially Preserved in Sediments" = "steelblue", "Not Preserved in Sediments" = "gray"))+
      theme_classic()+
      geom_hline(yintercept = 0)+
      theme(plot.title = element_text(hjust = 0.5, size = 17), legend.text = element_text(size = 11), axis.title = element_text(size = 14), axis.text = element_text(angle = 45, hjust = 1, size = 11), legend.position = "bottom")
    #dev.off()
    
    
    #FISH
    #need a vector of names for the categories
    fish.pls.loadings <- data.frame(eigvec.fish.pls.Htrans)
    fish.pls.loadings$species <- c("Walleye", "Largemouth Bass", "Northern Pike", "White Sucker", "Bluegill", "Yellow Perch", 
                                        "Shorthead Redhorse",  "Yellow Bullhead", "Bowfin", "Hybrid Sunfish", "Black Crappie", 
                                        "Black Bullhead", "Rock Bass", "Brown Bullhead", "Pumpkinseed", "Common Carp", "Golden Shiner", 
                                        "Redhorse", "Cisco", "Muskellunge", "Green Sunfish", "Smallmouth Bass", "Lake Whitefish", "Burbot", 
                                        "Channel Catfish", "Silver Redhorse", "Rainbow Smelt", "Sauger", "Silver Lamprey")
    #make a column that will make walleye red and centrarchids blue
    fish.pls.loadings$color.cat <- c("Walleye", "Centrarchid", "Other", "Other", "Centrarchid", "Other", 
                                 "Other",  "Other", "Other", "Centrarchid", "Centrarchid", 
                                 "Other", "Centrarchid", "Other", "Centrarchid", "Other", "Other", 
                                 "Other", "Other", "Other", "Centrarchid", "Centrarchid", "Other", "Other", 
                                 "Other", "Other", "Other", "Other", "Other")
    #now plot
    #tiff("PLS1.Fish.Loadings.tiff", width = 10, height = 5, units = "in", res = 300)
    ggplot(fish.pls.loadings, aes(x = species, y = X1, fill = color.cat))+
      geom_bar(stat = "identity")+
      labs(title = "PLS 1 Loadings by Fish Species", x = "Species", y = "Relative Contribution to PLS 1", fill = NULL) +
      scale_fill_manual(values = c("Walleye" = "red", "Centrarchid" = "blue", "Other" = "gray"))+
      theme_classic()+
      geom_hline(yintercept = 0)+
      theme(plot.title = element_text(hjust = 0.5, size = 17), legend.text = element_text(size = 11), axis.title = element_text(size = 14), axis.text = element_text(angle = 45, hjust = 1, size = 11), legend.position = "bottom")
    #dev.off()
    
    
    
    
    #plot the second PLS component
    plot(scores.zoop.pls.Htrans[, 2], scores.fish.pls.Htrans[, 2], xlab = "Zooplankton Community", ylab = "Fish community",  main = "PLS Second Components (Hellinger transformed) (16.0%)", cex = 0.7, pch = 19, ylim = c(-0.4,0.5), xlim = c(-0.4,0.5))
    #plot with ggplot to get the fun walleye/bass colors
    #tiff("PLS2.tiff", width = 10, height = 7, units = "in", res = 300)
    ggplot(pls.plot.data, aes(x = X2, y = X2.1, color = fish3.WAE.LMB.Ind.scale))+
      geom_point()+
      labs(title = "PLS Second Components (Hellinger transformed) (16.0%)", x = "Zooplankton Community", y = "Fish community") +
      scale_x_continuous(limits = c(-0.4,0.5))+
      scale_y_continuous(limits = c(-0.4,0.5))+
      scale_color_gradient(low = "blue", high = "red")+
      coord_fixed()+
      theme_classic()
    #dev.off()
    
    
          #plot zoop loadings
          #tiff("PLS2.Zoop.Loadings.tiff", width = 8, height = 5, units = "in", res = 300)
          ggplot(zoop.pls.loadings, aes(x = taxa, y = X2, fill = color.cat))+
            geom_bar(stat = "identity")+
            labs(title = "PLS 2 Loadings by Zooplankton Taxa", x = "Taxon", y = "Relative Contribution to PLS 2", fill = NULL) +
            scale_fill_manual(values = c("Well Preserved in Sediments" = "navyblue", "Partially Preserved in Sediments" = "steelblue", "Not Preserved in Sediments" = "gray"))+
            theme_classic()+
            geom_hline(yintercept = 0)+
            theme(plot.title = element_text(hjust = 0.5, size = 17), legend.text = element_text(size = 11), axis.title = element_text(size = 14), axis.text = element_text(angle = 45, hjust = 1, size = 11), legend.position = "bottom")
          #dev.off()
    
          #plot fish loadings
          #tiff("PLS2.Fish.Loadings.tiff", width = 10, height = 5, units = "in", res = 300)
          ggplot(fish.pls.loadings, aes(x = species, y = X2, fill = color.cat))+
            geom_bar(stat = "identity")+
            labs(title = "PLS 2 Loadings by Fish Species", x = "Species", y = "Relative Contribution to PLS 2", fill = NULL) +
            scale_fill_manual(values = c("Walleye" = "red", "Centrarchid" = "blue", "Other" = "gray"))+
            theme_classic()+
            geom_hline(yintercept = 0)+
            theme(plot.title = element_text(hjust = 0.5, size = 17), legend.text = element_text(size = 11), axis.title = element_text(size = 14), axis.text = element_text(angle = 45, hjust = 1, size = 11), legend.position = "bottom")
          #dev.off()
          
          
    #plot the third PLS component
    plot(scores.zoop.pls.Htrans[, 3], scores.fish.pls.Htrans[, 3], xlab = "Zooplankton Community", ylab = "Fish community",  main = "PLS Third Components (Hellinger transformed) (12.1%)", cex = 0.7, pch = 19,ylim = c(-0.5,0.4), xlim = c(-0.5,0.4))
    #plot with ggplot to get the fun walleye/bass colors
    #tiff("PLS3.tiff", width = 10, height = 7, units = "in", res = 300)
    ggplot(pls.plot.data, aes(x = X3, y = X3.1, color = fish3.WAE.LMB.Ind.scale))+
      geom_point()+
      labs(title = "PLS Third Components (Hellinger transformed) (12.1%)", x = "Zooplankton Community", y = "Fish community") +
      scale_x_continuous(limits = c(-0.5,0.4))+
      scale_y_continuous(limits = c(-0.5,0.4))+
      coord_fixed()+
      scale_color_gradient(low = "blue", high = "red")+
      theme_classic()
    #dev.off()
    
          #plot zoop loadings
          #tiff("PLS3.Zoop.Loadings.tiff", width = 8, height = 5, units = "in", res = 300)
          ggplot(zoop.pls.loadings, aes(x = taxa, y = X3, fill = color.cat))+
            geom_bar(stat = "identity")+
            labs(title = "PLS 3 Loadings by Zooplankton Taxa", x = "Taxon", y = "Relative Contribution to PLS 3", fill = NULL) +
            scale_fill_manual(values = c("Well Preserved in Sediments" = "navyblue", "Partially Preserved in Sediments" = "steelblue", "Not Preserved in Sediments" = "gray"))+
            theme_classic()+
            geom_hline(yintercept = 0)+
            theme(plot.title = element_text(hjust = 0.5, size = 17), legend.text = element_text(size = 11), axis.title = element_text(size = 14), axis.text = element_text(angle = 45, hjust = 1, size = 11), legend.position = "bottom")
          #dev.off()
          
          #plot fish loadings
          #tiff("PLS3.Fish.Loadings.tiff", width = 10, height = 5, units = "in", res = 300)
          ggplot(fish.pls.loadings, aes(x = species, y = X3, fill = color.cat))+
            geom_bar(stat = "identity")+
            labs(title = "PLS 3 Loadings by Fish Species", x = "Species", y = "Relative Contribution to PLS 3", fill = NULL) +
            scale_fill_manual(values = c("Walleye" = "red", "Centrarchid" = "blue", "Other" = "gray"))+
            theme_classic()+
            geom_hline(yintercept = 0)+
            theme(plot.title = element_text(hjust = 0.5, size = 17), legend.text = element_text(size = 11), axis.title = element_text(size = 14), axis.text = element_text(angle = 45, hjust = 1, size = 11), legend.position = "bottom")
          #dev.off()    
  
    
    
    
#DO THE PLS ON THE TRANSFORMED DATA - THIS TIME WITH TOTAL ABUNDANCE / BIOMASS
      PLS.data.Htrans2 <- cbind(zoop.Htrans, fish.Htrans)
      
      #create a vector with a's for left block (fish) and b's for right block (zoops) to tell geomorph which variable is in which group
      gp.Htrans2 <- c(rep("a",times=21),rep("b",times=30)) 
      
      #run the pls
      #arguments: data frame, vector for which variables go in which block, if you want to see progress bar)
      pls.Htrans2 <- integration.test(PLS.data.Htrans2, partition.gp = gp.Htrans2, print.progress = FALSE)
      #for lots of good info on this function, run ?integration.test
      #view summarized results
      pls.Htrans2
      
      #Extract the proportional eigenvalues, **two sets** of eigenvectors, and two sets of scores from this analysis as separate objects.
      #The svd object contains the eigenvalues, but not as proportions, so we have to calculate that.
      #first view the pls$svd info:
      pls.Htrans2$svd
      #calculate the eigenvalues as proportions
      #these eigenvalues are a shared set that describes how much covariation is explained by corresponding components in both blocks
      eigval.pls.Htrans2 <- pls.Htrans2$svd$d/sum(pls.Htrans2$svd$d)
      eigval.pls.Htrans2
      #the first PLS component explains 81.9% of the covariation and the second pls component explains 4.9% of the covariation
      #now calculate two sets of separate eigenvectors as separate objects
      # left eigenvector matrix (the zoops)
      eigvec.zoop.pls.Htrans2 <- pls.Htrans2$left.pls.vectors
      eigvec.zoop.pls.Htrans2
      
      # right eigenvector matrix (the fish)
      eigvec.fish.pls.Htrans2 <- pls.Htrans2$right.pls.vectors
      eigvec.fish.pls.Htrans2
      #looks like walleye and LMB go in opposite directions for the first 4 pC axes (other species have strong effects too)
      
      
      #now calculate the two separate scores
      # Left block scores (the zoops)
      scores.zoop.pls.Htrans2 <- pls.Htrans2$XScores
      rownames(scores.zoop.pls.Htrans2) <- zoop3$parentdow.fish.year
      head(scores.zoop.pls.Htrans2)
      
      
      # Right block scores (the fish)
      scores.fish.pls.Htrans2 <- pls.Htrans2$YScores
      rownames(scores.fish.pls.Htrans2) <- zoop3$parentdow.fish.year
      head(scores.fish.pls.Htrans2)
      
      #make a dataframe with all the scores and the color variable
      pls.plot.data2 <- data.frame(scores.zoop.pls.Htrans2, scores.fish.pls.Htrans2, fish3$WAE.LMB.Ind.scale, fish3$WAE.Cent.Ind.scale)
      
      #make zoop loading data frame
          zoop.pls.loadings2 <- data.frame(eigvec.zoop.pls.Htrans2)
          zoop.pls.loadings2$taxa <- c("Acroperus harpae", "Alona spp.", "Bosminidae", "Ceriodaphnia spp.", "Chydorus sphaericus", 
                                      "Daphnia galeata mendotae", "Daphnia longiremis", "Daphnia parvula", "Daphnia pulicaria", 
                                      "Daphnia retrocurva", "Diaphanosoma birgei", "Eubosmina coregoni", "Eurycercus lamellatus", 
                                      "Holopedium gibberum", "Latona setifera", "Sida crystallina", "Calanoids", "Copepodites", 
                                      "Cyclopoids", "Nauplii", "Total Biomass Scaled")
          #make a column that will make sediment preservation level different colors
          zoop.pls.loadings2$color.cat <- c("Well Preserved in Sediments", "Well Preserved in Sediments", "Well Preserved in Sediments", "Partially Preserved in Sediments", "Well Preserved in Sediments", 
                                           "Partially Preserved in Sediments", "Partially Preserved in Sediments", "Partially Preserved in Sediments", "Partially Preserved in Sediments", 
                                           "Partially Preserved in Sediments", "Partially Preserved in Sediments", "Well Preserved in Sediments", "Well Preserved in Sediments", 
                                           "Partially Preserved in Sediments", "Partially Preserved in Sediments", "Partially Preserved in Sediments", "Not Preserved in Sediments", "Not Preserved in Sediments", 
                                           "Not Preserved in Sediments", "Not Preserved in Sediments", "Not Preserved in Sediments")
      #make fish loading data frame
          #need a vector of names for the categories
          fish.pls.loadings2 <- data.frame(eigvec.fish.pls.Htrans2)
          fish.pls.loadings2$species <- c("Walleye", "Largemouth Bass", "Northern Pike", "White Sucker", "Bluegill", "Yellow Perch", 
                                         "Shorthead Redhorse",  "Yellow Bullhead", "Bowfin", "Hybrid Sunfish", "Black Crappie", 
                                         "Black Bullhead", "Rock Bass", "Brown Bullhead", "Pumpkinseed", "Common Carp", "Golden Shiner", 
                                         "Redhorse", "Cisco", "Muskellunge", "Green Sunfish", "Smallmouth Bass", "Lake Whitefish", "Burbot", 
                                         "Channel Catfish", "Silver Redhorse", "Rainbow Smelt", "Sauger", "Silver Lamprey", "Total CPUE Scaled")
          #make a column that will make walleye red and centrarchids blue
          fish.pls.loadings2$color.cat <- c("Walleye", "Centrarchid", "Other", "Other", "Centrarchid", "Other", 
                                           "Other",  "Other", "Other", "Centrarchid", "Centrarchid", 
                                           "Other", "Centrarchid", "Other", "Centrarchid", "Other", "Other", 
                                           "Other", "Other", "Other", "Centrarchid", "Centrarchid", "Other", "Other", 
                                           "Other", "Other", "Other", "Other", "Other", "Other")
          
      #plot the first PLS component (not dealing with colors here)
      #axes don't have to be on same scale here (I don't think)
      plot(scores.zoop.pls.Htrans2[, 1], scores.fish.pls.Htrans2[, 1], xlab = "Zooplankton Community", ylab = "Fish community",  main = "PLS First Components (Hellinger transformed) (81.9%)", cex = 0.7, pch = 19, ylim = c(-1.5,5), xlim = c(-1.5,5))
      #alternate way to plot this
      plot(pls.Htrans2, pch = 19)
      #plot with ggplot to get the fun walleye/bass colors
      #tiff("PLS1.totals.tiff", width = 10, height = 7, units = "in", res = 300)
      ggplot(pls.plot.data2, aes(x = X1, y = X1.1, color = fish3.WAE.LMB.Ind.scale))+
        geom_point()+
        labs(title = "PLS First Components (Hellinger transformed) (81.9%)", x = "Zooplankton Community", y = "Fish community") +
        scale_x_continuous(limits = c(-1.5,5))+
        scale_y_continuous(limits = c(-1.5,5))+
        scale_color_gradient(low = "blue", high = "red")+
        coord_fixed()+
        theme_classic()
      #dev.off() 
      
            #plot zoop loadings
            #tiff("PLS1.Zoop.Loadings.totals.tiff", width = 8, height = 5, units = "in", res = 300)
            ggplot(zoop.pls.loadings2, aes(x = taxa, y = X1, fill = color.cat))+
              geom_bar(stat = "identity")+
              labs(title = "PLS 1 Loadings by Zooplankton Taxa", x = "Taxon", y = "Relative Contribution to PLS 1", fill = NULL) +
              scale_fill_manual(values = c("Well Preserved in Sediments" = "navyblue", "Partially Preserved in Sediments" = "steelblue", "Not Preserved in Sediments" = "gray"))+
              theme_classic()+
              geom_hline(yintercept = 0)+
              theme(plot.title = element_text(hjust = 0.5, size = 17), legend.text = element_text(size = 11), axis.title = element_text(size = 14), axis.text = element_text(angle = 45, hjust = 1, size = 11), legend.position = "bottom")
            #dev.off()
          
            #plot fish loadings
            #tiff("PLS1.Fish.Loadings.totals.tiff", width = 10, height = 5, units = "in", res = 300)
            ggplot(fish.pls.loadings2, aes(x = species, y = X1, fill = color.cat))+
              geom_bar(stat = "identity")+
              labs(title = "PLS 1 Loadings by Fish Species", x = "Species", y = "Relative Contribution to PLS 1", fill = NULL) +
              scale_fill_manual(values = c("Walleye" = "red", "Centrarchid" = "blue", "Other" = "gray"))+
              theme_classic()+
              geom_hline(yintercept = 0)+
              theme(plot.title = element_text(hjust = 0.5, size = 17), legend.text = element_text(size = 11), axis.title = element_text(size = 14), axis.text = element_text(angle = 45, hjust = 1, size = 11), legend.position = "bottom")
            #dev.off()
      
      #plot the second PLS component
      plot(scores.zoop.pls.Htrans2[, 2], scores.fish.pls.Htrans2[, 2], xlab = "Zooplankton Community", ylab = "Fish community",  main = "PLS Second Components (Hellinger transformed) (4.9%)", cex = 0.7, pch = 19, ylim = c(-0.7,1.5), xlim = c(-0.7,1.5))
      #plot with ggplot to get the fun walleye/bass colors
      
      #tiff("PLS2.totals.tiff", width = 10, height = 7, units = "in", res = 300)
      ggplot(pls.plot.data2, aes(x = X2, y = X2.1, color = fish3.WAE.LMB.Ind.scale))+
        geom_point()+
        labs(title = "PLS Second Components (Hellinger transformed) (4.9%)", x = "Zooplankton Community", y = "Fish community") +
        scale_x_continuous(limits = c(-0.7,1.5))+
        scale_y_continuous(limits = c(-0.7,1.5))+
        scale_color_gradient(low = "blue", high = "red")+
        coord_fixed()+
        theme_classic()
      #dev.off()  
      
              #plot zoop loadings
              #tiff("PLS2.Zoop.Loadings.totals.tiff", width = 8, height = 5, units = "in", res = 300)
              ggplot(zoop.pls.loadings2, aes(x = taxa, y = X2, fill = color.cat))+
                geom_bar(stat = "identity")+
                labs(title = "PLS 2 Loadings by Zooplankton Taxa", x = "Taxon", y = "Relative Contribution to PLS 2", fill = NULL) +
                scale_fill_manual(values = c("Well Preserved in Sediments" = "navyblue", "Partially Preserved in Sediments" = "steelblue", "Not Preserved in Sediments" = "gray"))+
                theme_classic()+
                geom_hline(yintercept = 0)+
                theme(plot.title = element_text(hjust = 0.5, size = 17), legend.text = element_text(size = 11), axis.title = element_text(size = 14), axis.text = element_text(angle = 45, hjust = 1, size = 11), legend.position = "bottom")
              #dev.off()
              
              #plot fish loadings
              #tiff("PLS2.Fish.Loadings.totals.tiff", width = 10, height = 5, units = "in", res = 300)
              ggplot(fish.pls.loadings2, aes(x = species, y = X2, fill = color.cat))+
                geom_bar(stat = "identity")+
                labs(title = "PLS 2 Loadings by Fish Species", x = "Species", y = "Relative Contribution to PLS 2", fill = NULL) +
                scale_fill_manual(values = c("Walleye" = "red", "Centrarchid" = "blue", "Other" = "gray"))+
                theme_classic()+
                geom_hline(yintercept = 0)+
                theme(plot.title = element_text(hjust = 0.5, size = 17), legend.text = element_text(size = 11), axis.title = element_text(size = 14), axis.text = element_text(angle = 45, hjust = 1, size = 11), legend.position = "bottom")
              #dev.off()
      
      #plot the third PLS component
      plot(scores.zoop.pls.Htrans2[, 3], scores.fish.pls.Htrans2[, 3], xlab = "Zooplankton Community", ylab = "Fish community",  main = "PLS Third Components (Hellinger transformed) (3.9%)", cex = 0.7, pch = 19, ylim = c(-0.5,0.6), xlim = c(-0.5,0.6))
      #plot with ggplot to get the fun walleye/bass colors
      #tiff("PLS3.totals.tiff", width = 10, height = 7, units = "in", res = 300)
      ggplot(pls.plot.data2, aes(x = X3, y = X3.1, color = fish3.WAE.LMB.Ind.scale))+
        geom_point()+
        labs(title = "PLS Third Components (Hellinger transformed) (3.9%)", x = "Zooplankton Community", y = "Fish community") +
        scale_x_continuous(limits = c(-0.5,0.6))+
        scale_y_continuous(limits = c(-0.5,0.6))+
        scale_color_gradient(low = "blue", high = "red")+
        coord_fixed()+
        theme_classic()
      #dev.off()  
      
            #plot zoop loadings
            #tiff("PLS3.Zoop.Loadings.totals.tiff", width = 8, height = 5, units = "in", res = 300)
            ggplot(zoop.pls.loadings2, aes(x = taxa, y = X3, fill = color.cat))+
              geom_bar(stat = "identity")+
              labs(title = "PLS 3 Loadings by Zooplankton Taxa", x = "Taxon", y = "Relative Contribution to PLS 3", fill = NULL) +
              scale_fill_manual(values = c("Well Preserved in Sediments" = "navyblue", "Partially Preserved in Sediments" = "steelblue", "Not Preserved in Sediments" = "gray"))+
              theme_classic()+
              geom_hline(yintercept = 0)+
              theme(plot.title = element_text(hjust = 0.5, size = 17), legend.text = element_text(size = 11), axis.title = element_text(size = 14), axis.text = element_text(angle = 45, hjust = 1, size = 11), legend.position = "bottom")
            #dev.off()
            
            #plot fish loadings
            #tiff("PLS3.Fish.Loadings.totals.tiff", width = 10, height = 5, units = "in", res = 300)
            ggplot(fish.pls.loadings2, aes(x = species, y = X3, fill = color.cat))+
              geom_bar(stat = "identity")+
              labs(title = "PLS 3 Loadings by Fish Species", x = "Species", y = "Relative Contribution to PLS 3", fill = NULL) +
              scale_fill_manual(values = c("Walleye" = "red", "Centrarchid" = "blue", "Other" = "gray"))+
              theme_classic()+
              geom_hline(yintercept = 0)+
              theme(plot.title = element_text(hjust = 0.5, size = 17), legend.text = element_text(size = 11), axis.title = element_text(size = 14), axis.text = element_text(angle = 45, hjust = 1, size = 11), legend.position = "bottom")
            #dev.off() 


            
#----------------------------------------------------------------------------------------------------------------------------------------------
            
            
#IN PRESENTATION, TALK ABOUT THE DOUBLE ZERO PROBLEM:
#Double zero problem
# If a species is present at sites and we take this as a reection of the
# two sites' similarity
# If a species is absent from one of or we take this as a reection of
# some ecological difference between the two sites
# But, what if the species is absent from both and ?
# The species could be absent for different reasons
# too hot in and too cold in
# too dry in and to wet in
# i j
# i j
# i j
# i j
# i j
# 17 / 95
# Double zero problem
# We may choose not to attach ecological meaning to a joint or double
# absence when computing association
# Coefcients that skip double zeroes are asymmetric
# Coefcients that do not skip double zeroes are symmetric
#FROM https://gavinsimpson.github.io/physalia-multivariate/01-monday/slides.pdf
         