library(mvabund)
library(gllvm)
library(ggpubr)
library(dplyr)

#Abund data
#y <- read.csv("Phyto.21.R.count_group.csv",row.names=1,header=TRUE)
#y <- read.csv("zoop.2021.R.csv",row.names=1,header=TRUE)
#y <- read.csv("2021_phyto.zoop.csv",row.names=1,header=TRUE)
y <- read.csv("inverts.order.csv",row.names=1,header=TRUE)
y<-y[, colSums(y) != 0]
y<-y%>%
 rename(Ephem.=Ephemeroptera,
       Rhynch.=Rhynchobdellida,
      Hydrach.=Hydrachnidia)

y<-y%>%
  rename(
    Baccil.=Baccilariophyta,
    Euglen.=Euglenophyta)

y<-y%>%
 rename(
        Cop.nauplii=Copepod.nauplii,
         Cerio.=Ceriodaphnia,
         Diaphano.=Diaphanosoma)



#WQ data
#X <- read.csv("wq_phyto.zoop.21.csv",row.names=1,header=TRUE)
X <- read.csv("wq_inverts.21.csv",row.names=1,header=TRUE)

X$Lake<-factor(X$Lake)

#Check for missing data
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(X,2,pMiss)
#fix missing data
library(mice)
tempData <- mice(X,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)
tempData$imp$DO
X <- complete(tempData,3)
#check imputed values 
xyplot(tempData,DO ~ Wtemp+ph+TP,pch=18,cex=1)
densityplot(tempData)


#y<-y[, colSums(y) > 10]

# Model without predictors:
gllvm(y, family = "negative.binomial")
# Model where environmental variables, that is, all variables in X are included:
gllvm(y, X, family = "negative.binomial")
fitp <- gllvm(y, family = poisson())
fitp
fit_ord <- gllvm(y, family = "negative.binomial")
fit_ord
# Plot residuals for the Poisson model
par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))
plot(fitp, var.colors = 1)
# Plot residuals for the NB model
plot(fit_ord, var.colors = 1)
#NB better reps distribution

library(RColorBrewer)
library(corrplot)
library(ggplot2)
par(mfrow = c(1,1))
M<-cor(X[,c(1:4,6:10,13:16)],method=c("spearman"))
testRes<-cor.mtest(X[,c(1:4,6:10,13:16)],method=c("spearman"),conf.level=0.95)
corrplot(M, p.mat=testRes$p,type = 'lower', order = 'hclust', tl.col = 'black',insig='blank',diag=FALSE,
         cl.ratio = 0.2, tl.srt = 45, col = COL2('PuOr', 10))

#determine optimal # of latent variables
criterias <- NULL
for(i in 0:6){
  fiti <- gllvm(y, X, family = "negative.binomial", num.lv = i, sd.errors = FALSE,
                formula = ~ TP +  NO2 +Tur +Wtemp+Cond, seed = 123)
  criterias[i + 1] <- summary(fiti)$AICc
  names(criterias)[i + 1] = i
}
# Compare AIC values
criterias
#Pick num.lv based on AICc scores 
fit_env <- gllvm( y,X,
                       family = "negative.binomial",
                       num.lv = 1,
                       formula = ~ TP + NO3 + Wtemp,
                       seed = 1234
)

fit_env_rand <- gllvm( y,X,
  family = "negative.binomial",
  num.lv = 1,
  formula = ~ TP + NO3 + Wtemp,
  seed = 1234,
  row.eff=~(1|Lake)
)
fit_env1 <- gllvm( y,X,
                  family = "negative.binomial",
                  num.lv = 1,
                  formula = ~ TP + Tur + Wtemp,
                  seed = 1234
)

fit_env2 <- gllvm(y,X,
  family = "negative.binomial",
  num.lv = 1,
  formula = ~ TP + NO3 + Wtemp + Cond,
  seed = 1234
)

fit_env3 <- gllvm(y,X,
  family = "negative.binomial",
  num.lv = 1,
  formula = ~ TP  + Tur + Wtemp + Cond +  DO,
  seed = 1234
)
fit_env3_rand <- gllvm(y,X,
                  family = "negative.binomial",
                  num.lv = 1,
                  formula = ~ TP  + Tur + Wtemp + Cond +  DO,
                  seed = 1234,
                  row.eff=~(1|Lake)
)

AIC(fit_env,fit_env1,fit_env2,fit_env3)
anova(fit_env,fit_env_rand)
summary(fit_env)

fit_env3_rand$params$sigma


gllvm:coefplot(fit_env,cex.ylab = 1.,cex.xlab = 1.2,cex.axis=1.2,mfrow=c(1,3))
gllvm:coefplot(fit_env3_rand,cex.ylab = 1.,cex.xlab = 1.2,cex.axis=1.2,mfrow=c(2,3))


# Residual correlation matrix:
cr <- getResidualCor(fit_env3)

# Fit GLLVM without environmental variables and 1 latent variables:
fit4lv <- gllvm(y, family = "negative.binomial", num.lv = 1, seed = 1234)
cr0 <- getResidualCor(fit4lv)

ordiplot(fit_env,biplot=TRUE)


library(corrplot); library(gclus)
library(RColorBrewer)

# Correlation matrix with significance stars
par(mfrow = c(1,2))
res1 <- cor.mtest(cr, conf.level = .95)

#adjust p-values
pAdj <- p.adjust(c(res1$p), method = "BH")
resAdj <- matrix(pAdj, ncol = dim(res1$p)[1])
rownames(resAdj)<-colnames(y)
colnames(resAdj)<-colnames(y)

#plot env controlled model
cr_env<-corrplot(cr,diag=FALSE, p.mat = resAdj,
                 type="lower", method="square",font=3,tl.cex=0.8,
                 tl.srt=45,tl.col="black",col=brewer.pal(n=8,name="PuOr"),
         insig = "label_sig",
         sig.level = c(.001, .01, .05), pch.cex = .9, pch.col = "black")

res2 <- cor.mtest(cr0, conf.level = .95)
#adjust p-values
pAdj <- p.adjust(c(res2$p), method = "BH")
resAdj <- matrix(pAdj, ncol = dim(res2$p)[1])
rownames(resAdj)<-colnames(y)
colnames(resAdj)<-colnames(y)

#plot env controlled model

cr_all<-corrplot(cr0,diag=FALSE,type="lower", method="square",font=3,tl.cex=0.8,
                 tl.srt=45,tl.col="black",col=brewer.pal(n=8,name="PuOr"),
         p.mat = resAdj, insig = "label_sig",
         sig.level = c(.001, .01, .05), pch.cex = .9, pch.col = "black")


#Determine amount of variation explained by env variables
rcov <- getResidualCov(fit_env, adjust = 0)
rcov0 <- getResidualCov(fit4lv, adjust = 0)
rcov0$trace; rcov$trace
1 - rcov$trace / rcov0$trace
#result tells what percent of species abund covariation is explained by environmental variables

#concurrent ordination
CGLLVM <- gllvm(y, X = X, family = "negative.binomial", num.lv.c = 2,lv.formula = ~ TP +  NO2 +Tur +Wtemp+Cond+DO)
summary(CGLLVM)
par(mfrow=c(1,1))
ordiplot(CGLLVM, biplot = TRUE)
