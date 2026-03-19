## data corrections for differences between analyst (i.e. Jodie (pre 2020) v. Kylie (2020-present))
## apply these to the data for both year density and year biomass for zoop retro and beyond 
# code updated Apr 2024- Kylie Cattoor

#====load packages===================================
library(tidyverse)
library(ggpubr)
library(vegan)
library(Matrix)
library(lme4)#mixed effects modeling
library(lmerTest) #adds p-values to model output


###=====test for analyst effect (Jodie v. Kylie)=====
sent_z <- read.csv("data/processed_data/sent_z_retro2013-2023_20251020.csv")

effect<-sent_z%>%filter(Tiers=='One'& year>2016)%>%
  group_by(lake_name, site_number, sample_date, month, year)%>%
  summarise(TDens=sum(density), TBio=sum(biomass))%>%
  group_by(lake_name,sample_date,month,year)%>%
  summarise(Ddens=mean(TDens), Dbio=mean(TBio))%>%
  group_by(lake_name,month,year)%>%
  summarise(Mdens=mean(Ddens),Mbio=mean(Dbio))%>%
  group_by(lake_name, year)%>%
  summarise(yearD=mean(Mdens), yearB=mean(Mbio))%>% #annual mean of zoops
  mutate(analyst=if_else(year<2020,"J","K"))         

#first look at if the T biomass or T dens are different within the last 6 years, p=0.71 (no they are not!)
adonis2(effect$yearD~effect$analyst, method="bray", perm=999)

##=========1. for density==========
sent_z$grp2[sent_z$species=='nauplii']<-'nauplii'
sent_z$grp2[sent_z$species=='copepodites']<-'copepodites'

sent_analyst<-sent_z%>%filter(Tiers=='One'& year>2016)%>%
  spread(grp2,density)
sent_analyst[is.na(sent_analyst)]<-0

#look at differences in cladocerans/daphnia between analysts
colnames(sent_analyst)
sent_clad_prop<-sent_analyst%>%select(1:2,6,14:15,25,27)%>%
  gather(grp2,density, 'large daphnia':'small cladocerans')%>%
  group_by(lake_name, year, grp2)%>%summarise(YearD=mean(density))%>% mutate(proportionD=round(YearD/sum(YearD),2))%>%
  select(1:3,5)%>%group_by(lake_name, year)%>%spread(grp2, proportionD)
sent_clad_prop$analyst<-if_else(sent_clad_prop$year>2019,"kylie","jodie")
names(sent_clad_prop)[3]<-"large_daphnia"
names(sent_clad_prop)[4]<-"small_cladocerans"
clad2=sent_clad_prop[,3:4]

#plot differences among lakes
ggplot(sent_clad_prop, aes(x= year, y= large_daphnia, color= lake_name))+geom_point(size=3)
ggplot(sent_clad_prop, aes(x= year, y= small_cladocerans, color= lake_name))+geom_point(size=3)

adonis2(clad2~sent_clad_prop$lake_name, method="bray", perm=100) #differences in lake name, p<0.01**
adonis2(clad2~sent_clad_prop$analyst, method="bray", perm=100) #differences among analyst, p=0.50

#now let's look at if the proportions of copeopod groups v. immatures is different between Jodie v. Kylie
sent_cope_prop<-sent_analyst%>%select(1:2,6,14:15,21:23,26)%>%
  gather(grp2,density, 'calanoids':'nauplii')%>%
  group_by(lake_name, month, year, grp2)%>%summarise(monthD=mean(density))%>%
  group_by(lake_name, year, grp2)%>%summarise(YearD=mean(monthD))%>% mutate(proportionD=round(YearD/sum(YearD),2))%>%
  select(1:3,5)%>%group_by(lake_name, year)%>%spread(grp2, proportionD)
#ll_cope_prop<-ll_cope_prop[c(-1)]
sent_cope_prop$analyst<-if_else(sent_cope_prop$year>2019,"kylie","jodie")
#create matrix of just numeric
cope2=sent_cope_prop[,3:6]

#plot differences among lakes
ggplot(sent_cope_prop, aes(x= year, y= calanoids, color= lake_name))+geom_point(size=3)
ggplot(sent_cope_prop, aes(x= year, y= cyclopoids, color= lake_name))+geom_point(size=3)
ggplot(sent_cope_prop, aes(x= year, y= nauplii, color= lake_name))+geom_point(size=3)
ggplot(sent_cope_prop, aes(x= year, y= copepodites, color= lake_name))+geom_point(size=3)

adonis2(cope2~sent_cope_prop$lake_name, method="bray", perm=100) #differences in lake name, p<0.01*
adonis2(cope2~sent_cope_prop$analyst, method="bray", perm=100) #differences among analyst, p<0.01**


#but now we want to account for both! 
#new df for mixed effects model
sent_cope2<-sent_analyst%>%select(1:2,6,14:15,21:23,26)%>%
  gather(grp2,density, 'calanoids':'nauplii')%>%
  group_by(lake_name, month, year, grp2)%>%summarise(monthD=mean(density))%>%
  group_by(lake_name, year, grp2)%>%summarise(YearD=mean(monthD))%>% mutate(proportionD=round(YearD/sum(YearD),2))%>%
  select(1:3,5)
sent_cope2$analyst<-if_else(sent_cope2$year>2019,"Kylie","Jodie")

sent_cope2$lake_name<-as.factor(sent_cope2$lake_name)
sent_cope2$grp2<-factor(sent_cope2$grp2, levels=c("nauplii","copepodites","calanoids","cyclopoids"))
sent_cope2$Analyst<-as.factor(sent_cope2$analyst)
summary(sent_cope2)

#let's plot this
p1 <- ggboxplot(sent_cope2, x = 'grp2', y = 'proportionD', fill = 'Analyst',palette = c('#257ABA', '#C9B826'))+
  labs(x="",y="Relative density")+theme(legend.position="right")
p1

ggsave("figs/meta analysis/analyst_corrections1.jpeg", width=8, height=6)


#let's try some different models
lmm<-lmer(proportionD~ Analyst+(1|lake_name), data=filter(sent_cope2, grp2=='copepodites'), REML=FALSE) # **use this one for codes for correction!! 
lmm1<-lmer(proportionD~ 1+(1|lake_name), data=filter(sent_cope2, grp2=='calanoids'), REML=FALSE) #null model to test with the analyst model 


#use to compare models, put the null first
anova(lmm1,lmm)
qqnorm(resid(lmm))
summary(lmm)
summary(lmm1)
#look at the estimate for the data correction
#cope= -20.56%, nauplii= -11.78%, cyclop= +24.09%, calan= +8.30%, modify Kylie years to these corrections for Year DENSITY


##=========2. for biomass==========
sent_analyst<-sent_z%>%filter(Tiers=='One'&year>2016)%>%
  spread(grp2,biomass)
sent_analyst[is.na(sent_analyst)]<-0

#====a. look at differences in proportions of cladocerans/daphnia between analysts====
sent_clad_prop<-sent_analyst%>%select(1:2,6,14:15,25,27)%>%
  gather(grp2,biomass, 'large daphnia':'small cladocerans')%>%
  group_by(lake_name, month, year, grp2)%>%summarise(monthB=mean(biomass))%>%
  group_by(lake_name, year, grp2)%>%summarise(YearB=mean(monthB))%>% mutate(proportionB=round(YearB/sum(YearB),2))%>%
  select(1:3,5)%>%group_by(lake_name, year)%>%spread(grp2, proportionB)
sent_clad_prop$analyst<-if_else(sent_clad_prop$year>2019,"kylie","jodie")
names(sent_clad_prop)[3]<-"large_daphnia"
names(sent_clad_prop)[4]<-"small_cladocerans"
clad2=sent_clad_prop[,3:4]

#plot differences among lakes
ggplot(sent_clad_prop, aes(x= year, y= large_daphnia, color= lake_name))+geom_point(size=3)

adonis2(clad2~sent_clad_prop$lake_name, method="bray", perm=100) #differences in lake name, p<0.01**
adonis2(clad2~sent_clad_prop$analyst, method="bray", perm=100) #differences among analyst, p=0.21

#====b. look at differences in  proportions of copeopod groups v. immatures between analysts====
sent_cope_prop<-sent_analyst%>%select(1:2,6,14:15,21:23,26)%>%
  gather(grp2,biomass, 'calanoids':'nauplii')%>%
  group_by(lake_name, month, year, grp2)%>%summarise(monthB=mean(biomass))%>%
  group_by(lake_name, year, grp2)%>%summarise(YearB=mean(monthB))%>% mutate(proportionB=round(YearB/sum(YearB),5))%>%
  select(1:3,5)%>%group_by(lake_name, year)%>%spread(grp2, proportionB)
sent_cope_prop$analyst<-if_else(sent_cope_prop$year>2019,"kylie","jodie")
#create matrix of just numeric
cope2=sent_cope_prop[,3:6]

#plot differences among lakes
ggplot(sent_cope_prop, aes(x= year, y= calanoids, color= lake_name))+geom_point(size=3)

adonis2(cope2~sent_cope_prop$lake_name, method="bray", perm=100) #differences in lake name, p<0.01*
adonis2(cope2~sent_cope_prop$analyst, method="bray", perm=100) #differences among analyst, p<0.01**

#but now we want to account for both! 
#new df for mixed effects model
sent_cope2<-sent_analyst%>%select(1:2,6,14:15,21:23,26)%>%
  gather(grp2,biomass, 'calanoids':'nauplii')%>%
  group_by(lake_name, month, year, grp2)%>%summarise(monthB=mean(biomass))%>%
  group_by(lake_name, year, grp2)%>%summarise(YearB=mean(monthB))%>% mutate(proportionB=round(YearB/sum(YearB),5))%>%
  select(1:3,5)%>%filter(year>2016)
sent_cope2$analyst<-if_else(sent_cope2$year>2019,"Kylie","Jodie")

sent_cope2$lake_name<-as.factor(sent_cope2$lake_name)
sent_cope2$grp2<-factor(sent_cope2$grp2, levels=c("nauplii","copepodites","calanoids","cyclopoids"))
sent_cope2$Analyst<-as.factor(sent_cope2$analyst)
summary(sent_cope2)

#let's plot this
p2 <- ggboxplot(sent_cope2, x = 'grp2', y = 'proportionB', fill = 'Analyst',palette = c('#257ABA', '#C9B826'))+
  labs(x="",y="Relative biomass")+theme(legend.position="right")
p2

ggsave("figs/meta analysis/analyst_corrections2.jpeg", width=8, height=6)

fig<-ggarrange(p1,p2, ncol=1, nrow=2, common.legend=TRUE, legend="right", align="v")
fig<-annotate_figure(fig, top = text_grob("Adult and immature copepods", face = "bold", size=14))
fig
ggsave("figs/meta analysis/analyst_corrections3_densbio_boxplot.jpeg", width=8, height=6)


#let's try some different models
lmm<-lmer(proportionB~ Analyst+(1|lake_name), data=filter(sent_cope2, grp2=='copepodites'), REML=FALSE) # **use this one for codes for correction!! 
lmm1<-lmer(proportionB~ 1+(1|lake_name), data=filter(sent_cope2, grp2=='copepodites'), REML=FALSE) # use null to show analyst in the model is important

anova(lmm1,lmm)
qqnorm(resid(lmm))
summary(lmm)
summary(lmm1)
#look at the estimate for the data correction
#cope= -22.81%, nauplii= -3.15%, cyclop= +17.54%, calan= +8.32%, modify Kylie years to these corrections for Year BIOMASS
