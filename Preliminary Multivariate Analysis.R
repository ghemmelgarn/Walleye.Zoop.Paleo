#to d0: check for and remove uncommon species (too many zeroes) - or apply transformations to correct for this
#consider standardizing
#look for any obvious non-linear relationships - transform if needed

#useful website:
#https://sites.google.com/site/mb3gustame/indirect-gradient-analysis/principal-components-analysis

#another useful website
#https://r.qcbs.ca/workshop09/book-en/transformations.html


library(tidyverse)
library(ggplot2)
library(dplyr)
library(vegan) #contains the Hellinger transformation
library(tidyr)
library(factoextra) #for fancy biplots
library(geomorph) #for PLS

#read in data
Data <- read.csv("Data/Input/PrelimMultivarData.csv")

#before we start, assign the lakes colors
lake.colors <- c("Bearhead" = "red",
                 "Belle" = "blue",
                 "Carlos" = "green3",
                 "Carrie" = "orange",
                 "Cass" = "purple",
                 "Cedar" = "brown",
                 "Cut Foot Sioux" = "pink",
                 "Elk" = "gold",              
                 "Freeborn" = "cyan",
                 "Garfield" = "magenta",
                 "Green" = "darkgreen",
                 "Greenwood" = "darkblue",
                 "Hill (north)" = "darkred",
                 "Kabetogama" = "darkorange",
                 "Lake of the Woods" = "darkviolet",
                 "Leech" = "deepskyblue",
                 "Madison" = "chartreuse3",
                 "Mille Lacs" = "orchid",
                 "Minnetonka" = "sienna",
                 "Pearl" = "steelblue",
                 "Peltier" = "turquoise",
                 "Pepin" = "slateblue",
                 "Portage" = "firebrick",
                 "Sand Point" = "tomato",       
                 "South Center" = "yellow3",
                 "Tait" = "gray40",
                 "Ten Mile" = "olivedrab",
                 "Trout" = "mediumvioletred",
                 "Vermilion" = "dodgerblue",
                 "White Iron" = "indianred",
                 "Winnibigoshish" =  "black"  
                 )


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

#DO A PCA ON RAW FISH DATA
pca.fish <- prcomp(fish3[,5:33])

  #look at importance of each PC axis
  summary(pca.fish)
  #create a vector that has the proportion of variance explained by each new principal component
  eigval.fish <- pca.fish$sdev^2/sum(pca.fish$sdev^2)
         #eigval.fish
  #look at the eigenvectors
  pca.fish
  #create a matrix of the eigenvectors
  eigvec.fish <- pca.fish$rotation
        #eigvec.fish
  #create a matrix that has the PC scores of the lake-years
  scores.fish <- pca.fish$x
  rownames(scores.fish) <- fish3$parentdow.fish.year
        #head(scores.fish)
  
  #plot the first two PC axes, color by lake
      #apply the lake color vector
      col_vector <- lake.colors[fish3$LakeName]
  #make sure axes are the same scale and export as a square
  plot(scores.fish[,1], scores.fish[,2], xlab="PC1 (44.9%)", ylab="PC2 (17.1%)", ylim = c(-10,80), xlim = c(-10,80), cex=0.8, pch=19, col=col_vector, cex.lab=1.25)
  abline(h=0,v=0,lty=2)
  legend(x=50,y=80,pch=19,legend=c("Bearhead", "Belle", "Carlos","Carrie","Cass","Cedar","Cut Foot Sioux", "Elk",              
                                     "Freeborn","Garfield","Green", "Greenwood","Hill", "Kabetogama","Lake of the Woods",
                                     "Leech",  "Madison","Mille Lacs","Minnetonka","Pearl", "Peltier", "Pepin",
                                     "Portage","Sand Point", "South Center", "Tait","Ten Mile", "Trout", "Vermilion",
                                     "White Iron", "Winnibigoshish"), 
                      col=c("red", "blue", "green3", "orange", "purple", "brown", "pink", "gold",
                            "cyan", "magenta", "darkgreen", "darkblue", "darkred", "darkorange",
                            "darkviolet", "deepskyblue", "chartreuse3", "orchid", "sienna", "steelblue",
                            "turquoise", "slateblue", "firebrick", "tomato", "yellow3", "gray40",
                            "olivedrab", "mediumvioletred", "dodgerblue", "indianred", "black"),cex=.8)
      #I don't think coloring by lake adds value - lets do it without that
  #make sure axes are the same scale and export as a square
  plot(scores.fish[,1], scores.fish[,2], xlab="PC1 (44.9%)", ylab="PC2 (17.1%)", ylim = c(-10,80), xlim = c(-10,80), cex=0.8, pch=19, cex.lab=1.25)
  abline(h=0,v=0,lty=2)
  
  #we have a big outlier - probably driving up importance of PC2 with the white sucker outlier in Peltier
  
  #lets make a biplot out of this
  biplot(pca.fish)
  #ok this is way too much to look at and is very clumped, but this is visual confirmation that PC1 is almost entirely yellow perch and PC2 is white sucker and black crappie
  fviz_pca_biplot(pca.fish,
                  repel = TRUE,     # avoids text overlap
                  col.var = "red",  # variable arrows color
                  col.ind = "blue",  # individuals color
                  label = "var"    #only label the variables, not the individuals
                  )
  #clean up the variable labels if I want to use this biplot for anything

#DO A PCA ON HELLINGER-TRANSFORMED DATA - THIS REDUCES THE EFFECT OF HIGHLY ABUNDANT SPECIES AND INCREASES THE INFLUENCE OF RARE SPECIES
  #do the transformation
  fish.Htrans <- decostand(fish3[,5:33], method = "hellinger")

  #now do the PCA on this data and see how it changes
  pca.fish.Htrans <- prcomp(fish.Htrans)
  
  #look at importance of each PC axis
  summary(pca.fish.Htrans)
  #create a vector that has the proportion of variance explained by each new principal component
  eigval.fish.Htrans <- pca.fish.Htrans$sdev^2/sum(pca.fish.Htrans$sdev^2)
  #eigval.fish.Htrans
  #look at the eigenvectors
  pca.fish.Htrans
  #create a matrix of the eigenvectors
  eigvec.fish.Htrans <- pca.fish.Htrans$rotation
  #eigvec.fish.Htrans
  #create a matrix that has the PC scores of the lake-years
  scores.fish.Htrans <- pca.fish.Htrans$x
  rownames(scores.fish.Htrans) <- fish3$parentdow.fish.year
  #head(scores.fish.Htrans)
  
  #plot the first two PC axes, color by lake
  #apply the lake color vector
  col_vector <- lake.colors[fish3$LakeName]
  #make sure axes are the same scale and export as a square
  plot(scores.fish.Htrans[,1], scores.fish.Htrans[,2], xlab="PC1 (34.8%)", ylab="PC2 (13.8%)", ylim = c(-0.7,0.8), xlim = c(-0.7,0.8), cex=0.8, pch=19, col=col_vector, cex.lab=1.25)
  abline(h=0,v=0,lty=2)
  legend(x=.8,y=.8,pch=19,legend=c("Bearhead", "Belle", "Carlos","Carrie","Cass","Cedar","Cut Foot Sioux", "Elk",              
                                   "Freeborn","Garfield","Green", "Greenwood","Hill", "Kabetogama","Lake of the Woods",
                                   "Leech",  "Madison","Mille Lacs","Minnetonka","Pearl", "Peltier", "Pepin",
                                   "Portage","Sand Point", "South Center", "Tait","Ten Mile", "Trout", "Vermilion",
                                   "White Iron", "Winnibigoshish"), 
         col=c("red", "blue", "green3", "orange", "purple", "brown", "pink", "gold",
               "cyan", "magenta", "darkgreen", "darkblue", "darkred", "darkorange",
               "darkviolet", "deepskyblue", "chartreuse3", "orchid", "sienna", "steelblue",
               "turquoise", "slateblue", "firebrick", "tomato", "yellow3", "gray40",
               "olivedrab", "mediumvioletred", "dodgerblue", "indianred", "black"),cex=.8)
  
  
  
 
  #I don't think coloring by lake adds value - lets do it without that
  #make sure axes are the same scale and export as a square
  plot(scores.fish.Htrans[,1], scores.fish.Htrans[,2], xlab="PC1 (34.8%)", ylab="PC2 (13.8%)", ylim = c(-0.7,0.8), xlim = c(-0.7,0.8), cex=0.8, pch=19, cex.lab=1.25)
  abline(h=0,v=0,lty=2)
  
  #lets make a biplot out of this
  biplot(pca.fish.Htrans)
  #ok this is way too much to look at and is very clumped, but this is visual confirmation that PC1 is almost entirely yellow perch and PC2 is white sucker and black crappie
  fviz_pca_biplot(pca.fish.Htrans,
                  repel = TRUE,     # avoids text overlap
                  col.var = "red",  # variable arrows color
                  col.ind = "blue",  # individuals color
                  label = "var"    #only label the variables, not the individuals
  )
  #clean up the variable labels if I want to use this biplot for anything
  
  #this solved the outlier problem, now we see most variation driven by yellow perch and cisco
  #may want to explore later PC axes when I have time

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


#DO A PCA ON RAW ZOOP DATA
pca.zoop <- prcomp(zoop3[,5:24])

    #look at importance of each PC axis
    summary(pca.zoop)
    #create a vector that has the proportion of variance explained by each new principal component
    eigval.zoop <- pca.zoop$sdev^2/sum(pca.zoop$sdev^2)
    #eigval.zoop
    #look at the eigenvectors
    pca.zoop
    #create a matrix of the eigenvectors
    eigvec.zoop <- pca.zoop$rotation
    #eigvec.zoop
    #create a matrix that has the PC scores of the lake-years
    scores.zoop <- pca.zoop$x
    rownames(scores.zoop) <- zoop3$parentdow.fish.year
    #head(scores.zoop)
    
    #plot the first two PC axes, color by lake
    #apply the lake color vector
    col_vector <- lake.colors[zoop3$LakeName]
    #make sure axes are the same scale and export as a square
    plot(scores.zoop[,1], scores.zoop[,2], xlab="PC1 (55.1%)", ylab="PC2 (16.2%)",  ylim = c(-150,150), xlim = c(-150,150),cex=0.8, pch=19, col=col_vector, cex.lab=1.25)
    abline(h=0,v=0,lty=2)
    legend(x=-150,y=160,pch=19,legend=c("Bearhead", "Belle", "Carlos","Carrie","Cass","Cedar","Cut Foot Sioux", "Elk",              
                                     "Freeborn","Garfield","Green", "Greenwood","Hill", "Kabetogama","Lake of the Woods",
                                     "Leech",  "Madison","Mille Lacs","Minnetonka","Pearl", "Peltier", "Pepin",
                                     "Portage","Sand Point", "South Center", "Tait","Ten Mile", "Trout", "Vermilion",
                                     "White Iron", "Winnibigoshish"), 
           col=c("red", "blue", "green3", "orange", "purple", "brown", "pink", "gold",
                 "cyan", "magenta", "darkgreen", "darkblue", "darkred", "darkorange",
                 "darkviolet", "deepskyblue", "chartreuse3", "orchid", "sienna", "steelblue",
                 "turquoise", "slateblue", "firebrick", "tomato", "yellow3", "gray40",
                 "olivedrab", "mediumvioletred", "dodgerblue", "indianred", "black"),cex=.8)
    #I don't think coloring by lake adds value - lets do it without that
    #make sure axes are the same scale and export as a square
    plot(scores.zoop[,1], scores.zoop[,2], xlab="PC1 (55.1%)", ylab="PC2 (16.2%)",  ylim = c(-150,150), xlim = c(-150,150),cex=0.8, pch=19, cex.lab=1.25)
    abline(h=0,v=0,lty=2)
    
    #we have a big outlier - probably driving up importance of PC2 - did not investigate what it is because it's late and I am tired :(
    
    #lets make a biplot out of this
    biplot(pca.zoop)
    #make a better biplot
    fviz_pca_biplot(pca.zoop,
                    repel = TRUE,     # avoids text overlap
                    col.var = "red",  # variable arrows color
                    col.ind = "blue",  # individuals color
                    label = "var"    #only label the variables, not the individuals
    )
    #clean up the variable labels if I want to use this biplot for anything
    
    #overall, we see that calanoids vs. cyclopoids makes a big difference on PC2, Daphnia galeata mendotae, Daphnia pulicaria, and E. coregoni also important

#DO A PCA ON HELLINGER-TRANSFORMED ZOOP DATA - THIS REDUCES THE EFFECT OF HIGHLY ABUNDANT SPECIES AND INCREASES THE INFLUENCE OF RARE SPECIES

    #do the transformation
    zoop.Htrans <- decostand(zoop3[,5:24], method = "hellinger")
    
    #now do the PCA on this data and see how it changes
    pca.zoop.Htrans <- prcomp(zoop.Htrans)
    
    #look at importance of each PC axis
    summary(pca.zoop.Htrans)
          #now we are explaining much less of the variance
    #create a vector that has the proportion of variance explained by each new principal component
    eigval.zoop.Htrans <- pca.zoop.Htrans$sdev^2/sum(pca.zoop.Htrans$sdev^2)
    #eigval.zoop.Htrans
    #look at the eigenvectors
    pca.zoop.Htrans
    #create a matrix of the eigenvectors
    eigvec.zoop.Htrans <- pca.zoop.Htrans$rotation
    #eigvec.zoop.Htrans
    #create a matrix that has the PC scores of the lake-years
    scores.zoop.Htrans <- pca.zoop.Htrans$x
    rownames(scores.zoop.Htrans) <- zoop3$parentdow.zoop.year
    #head(scores.zoop.Htrans)
    
    #plot the first two PC axes, color by lake
    #apply the lake color vector
    col_vector <- lake.colors[zoop3$LakeName]
    #make sure axes are the same scale and export as a square
    plot(scores.zoop.Htrans[,1], scores.zoop.Htrans[,2], xlab="PC1 (24.5%)", ylab="PC2 (18.3%)", ylim = c(-0.7,0.5), xlim = c(-0.7,0.5), cex=0.8, pch=19, col=col_vector, cex.lab=1.25)
    abline(h=0,v=0,lty=2)
    legend(x=.8,y=.8,pch=19,legend=c("Bearhead", "Belle", "Carlos","Carrie","Cass","Cedar","Cut Foot Sioux", "Elk",              
                                     "Freeborn","Garfield","Green", "Greenwood","Hill", "Kabetogama","Lake of the Woods",
                                     "Leech",  "Madison","Mille Lacs","Minnetonka","Pearl", "Peltier", "Pepin",
                                     "Portage","Sand Point", "South Center", "Tait","Ten Mile", "Trout", "Vermilion",
                                     "White Iron", "Winnibigoshish"), 
           col=c("red", "blue", "green3", "orange", "purple", "brown", "pink", "gold",
                 "cyan", "magenta", "darkgreen", "darkblue", "darkred", "darkorange",
                 "darkviolet", "deepskyblue", "chartreuse3", "orchid", "sienna", "steelblue",
                 "turquoise", "slateblue", "firebrick", "tomato", "yellow3", "gray40",
                 "olivedrab", "mediumvioletred", "dodgerblue", "indianred", "black"),cex=.8)
    
    #I don't think coloring by lake adds value - lets do it without that
    #make sure axes are the same scale and export as a square
    plot(scores.zoop.Htrans[,1], scores.zoop.Htrans[,2], xlab="PC1 (24.5%)", ylab="PC2 (18.3%)", ylim = c(-0.7,0.5), xlim = c(-0.7,0.5), cex=0.8, pch=19, cex.lab=1.25)
    abline(h=0,v=0,lty=2)
    
    #lets make a biplot out of this
    biplot(pca.zoop.Htrans)
    #ok this is way too much to look at and is very clumped, but this is visual confirmation that PC1 is almost entirely yellow perch and PC2 is white sucker and black crappie
    fviz_pca_biplot(pca.zoop.Htrans,
                    repel = TRUE,     # avoids text overlap
                    col.var = "red",  # variable arrows color
                    col.ind = "blue",  # individuals color
                    label = "var"    #only label the variables, not the individuals
                  )
    
    #similar most important species, describing less variance after transformation, there are other important PC axes. 

    

    
    
    
    
#-------------------------------------------------------------------------------------------------------------------------------------
#LETS DO THE PARTIAL LEAST SQUARES
    
    
#start with the raw data, then we will do this with the transformed data afterwards
#make the dataset by combining the fish and zoop datasets we used above
PLS.data.prep <- left_join(zoop3, fish3, by = c("parentdow.fish.year", "lake_id", "LakeName", "year"))
    
#remove the extra columns that are not data for the PLS
PLS.data <- PLS.data.prep %>% 
  select(-"parentdow.fish.year",- "lake_id", -"LakeName", -"year")
    
      #create a vector with a's for left block (fish) and b's for right block (zoops) to tell geomorph which variable is in which group
      gp <- c(rep("a",times=20),rep("b",times=29)) 
      
      #run the pls
      #arguments: data frame, vector for which variables go in which block, if you want to see progress bar)
      pls <- integration.test(PLS.data, partition.gp = gp, print.progress = FALSE)
      #for lots of good info on this function, run ?integration.test
      #view summarized results
      pls
    
      #Extract the proportional eigenvalues, **two sets** of eigenvectors, and two sets of scores from this analysis as separate objects.
      #The svd object contains the eigenvalues, but not as proportions, so we have to calculate that.
            #first view the pls$svd info:
            pls$svd
            #calculate the eigenvalues as proportions
            #these eigenvalues are a shared set that describes how much covariation is explained by corresponding components in both blocks
            eigval.pls <- pls$svd$d/sum(pls$svd$d)
            eigval.pls
        #the first PLS component explains 54.0% of the covariation and the second pls component explains 15.7% of the covariation
            #now calculate two sets of separate eigenvectors as separate objects
            # left eigenvector matrix (the zoops)
            eigvec1.pls <- pls$left.pls.vectors
            eigvec1.pls
                    
            # right eigenvector matrix (the fish)
            eigvec2.pls <- pls$right.pls.vectors
            eigvec2.pls
                  #looks like the second PLS component separates out walleye and lmb in opposite directions... (other species have strong effects too)
            
    
      #now calculate the two separate scores
      # Left block scores (the zoops)
      scores1.pls <- pls$XScores
      head(scores1.pls)
      
      # Right block scores (the fish)
      scores2.pls <- pls$YScores
      head(scores2.pls)
    
    #plot the first PLS component (not dealing with colors here)
      #axes don't have to be on same scale here (I don't think)
      plot(scores1.pls[, 1], scores2.pls[, 1], xlab = "Zooplankton Community", ylab = "Fish community",  main = "PLS First Components (54.0%)", cex = 0.7, pch = 19, ylim = c(-40,140), xlim = c(-40,140))
    #alternate way to plot this
      plot(pls, pch = 19)
    
    
      #plot the second PLS component
      plot(scores1.pls[, 2], scores2.pls[, 2], xlab = "Zooplankton Community", ylab = "Fish community",  main = "PLS Second Components (15.7%)", cex = 0.7, pch = 19, ylim = c(-100,100), xlim = c(-100,100))
    
      #plot the third PLS component
      plot(scores1.pls[, 3], scores2.pls[, 3], xlab = "Zooplankton Community", ylab = "Fish community",  main = "PLS Third Components (14.5%)", cex = 0.7, pch = 19, ylim = c(-100,90), xlim = c(-100,90))
      
    
#THESE PLOTS COULD GO ON POSTER IF YOU CAN SHOW THE EIGENVECTOR LOADINGS SOMEHOW  
    #WHAT IF YOU COLORED THEM AS MORE WALLEYE VS. MORE BASS (or could try with centrarchids in general not just bass)
      
      
      
#DO THE PLS ON THE TRANSFORMED DATA
      PLS.data.Htrans <- cbind(zoop.Htrans, fish.Htrans)
      
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
      eigvec1.pls.Htrans <- pls.Htrans$left.pls.vectors
      eigvec1.pls.Htrans
      
      # right eigenvector matrix (the fish)
      eigvec2.pls.Htrans <- pls.Htrans$right.pls.vectors
      eigvec2.pls.Htrans
      #looks like walleye and LMB go in opposite directions for the first 4 pC axes (other species have strong effects too)
      
      
      #now calculate the two separate scores
      # Left block scores (the zoops)
      scores1.pls.Htrans <- pls.Htrans$XScores
      head(scores1.pls.Htrans)
      
      # Right block scores (the fish)
      scores2.pls.Htrans <- pls.Htrans$YScores
      head(scores2.pls.Htrans)
      
      #plot the first PLS component (not dealing with colors here)
      #axes don't have to be on same scale here (I don't think)
      plot(scores1.pls.Htrans[, 1], scores2.pls.Htrans[, 1], xlab = "Zooplankton Community", ylab = "Fish community",  main = "PLS First Components (Hellinger transformed) (33.2%)", cex = 0.7, pch = 19, ylim = c(-0.5,0.7), xlim = c(-0.5,0.7))
      #alternate way to plot this
      plot(pls.Htrans, pch = 19)
      
      
      #plot the second PLS component
      plot(scores1.pls.Htrans[, 2], scores2.pls.Htrans[, 2], xlab = "Zooplankton Community", ylab = "Fish community",  main = "PLS Second Components (Hellinger transformed) (16.0%)", cex = 0.7, pch = 19, ylim = c(-0.4,0.5), xlim = c(-0.4,0.5))
      
      #plot the third PLS component
      plot(scores1.pls.Htrans[, 3], scores2.pls.Htrans[, 3], xlab = "Zooplankton Community", ylab = "Fish community",  main = "PLS Third Components (Hellinger transformed) (12.1%)", cex = 0.7, pch = 19,ylim = c(-0.5,0.4), xlim = c(-0.5,0.4))
      
      
    

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
         