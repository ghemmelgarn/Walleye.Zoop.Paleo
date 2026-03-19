#I'm revisiting Jodie's conversion using a 13cm pre-2013 to a 30cm starting in 2013 - Sentinel LTM
# Heidi helped with this!!! Oct27, 2025- KC


library(tidyverse)
library(ggpubr)
library(ggplot2)
library(lme4)#mixed effects modeling

#let's look at Jodie's original net diff spreadsheet. I had to reformat it to read into R
jodie <- 
  read.csv("C:/Users/kycattoo/OneDrive - State of Minnesota - MN365/Documents/zooplankton/ZOOPS_lakes/1_Sentinel/net conversion analysis_2013/net_diff_use in R_2025Oct.csv")
jodie$month<-as.factor(jodie$month)

m1<-lm(density_30~density_13, jodie)
summary(m1)
intercept<-coef(m1)[1] #0.319
slope<-coef(m1)[2] #2.07
#adjusted R squared 0.853
equation<-sprintf("y=%.4f+%.4fx", intercept, slope)

#the regression equation for all lake-month was slightly different than the original spreadsheet (net_diff). I noticed the september data were duplicated and the october data were not included. 

#let's plot it by lake and month, both look good! 
g<-ggplot(jodie, aes(y=density_30, x=density_13))+geom_point(aes(size=1, color=lake_name))+
  geom_smooth(method='lm', se=FALSE)+stat_regline_equation(label.x=2, label.y=200)
g

ggplot(jodie, aes(x=density_13, y=density_30))+geom_point(aes(size=0.25, color=month))+
  geom_smooth(method='lm', se=FALSE)+stat_regline_equation(label.x=2, label.y=200)

#lets rerun the regressions with all 2013 data for Sentinel LTM

db_sent2013 <- 
  read.csv("C:/Users/kycattoo/OneDrive - State of Minnesota - MN365/Documents/zooplankton/ZOOPS_lakes/1_Sentinel/net conversion analysis_2013/Sentinel_query_2013.csv")

date_sum<-db_sent2013%>%filter(net_mouth_diameter_cm!=20)%>%
  group_by(lake_name,site_number,sample_date, net_mouth_diameter_cm)%>%
  summarize(TDen=sum(density), TBio=sum(biomass))%>%
  group_by(lake_name, sample_date, net_mouth_diameter_cm)%>%
  summarize(Dden=mean(TDen), Dbio=mean(TBio))%>%
  mutate(sample_date=as.Date(sample_date))%>%select(lake_name, sample_date,net_mouth_diameter_cm,Dden)%>%pivot_wider(names_from = net_mouth_diameter_cm,values_from = Dden)%>%drop_na()%>%rename(density_13=`13`, density_30=`30`)%>%arrange(lake_name,sample_date)

m2<-lm(density_30~density_13, date_sum)
#slope 1.8782, intercept 2.7255, r squared adujsted 0.7206
summary(m2)

date_sum$month<-as.numeric(format(date_sum$sample_date,"%m"))
date_sum$month<-as.factor(date_sum$month)

#more plots 
ggplot(date_sum, aes(x=density_13, y=density_30))+geom_point(aes(size=0.25, color=month))+
  geom_smooth(method='lm', se=FALSE)+stat_regline_equation(label.x=2, label.y=200)

ggplot(date_sum, aes(x=density_13, y=density_30))+geom_point(aes(size=0.25, color=lake_name))+
  geom_smooth(method='lm', se=FALSE)+stat_regline_equation(label.x=2, label.y=200)

ggsave("figs/meta analysis/netdff_density_corrections.jpeg", width=8, height=6)
#data correction to apply: y=1.8782x + 2.7255 
# ***use for density correction pre-2013 data 

date_sum2<-db_sent2013%>%filter(net_mouth_diameter_cm!=20)%>%
  group_by(lake_name,site_number,sample_date, net_mouth_diameter_cm)%>%
  summarize(TDen=sum(density), TBio=sum(biomass))%>%
  group_by(lake_name, sample_date, net_mouth_diameter_cm)%>%
  summarize(Dden=mean(TDen), Dbio=mean(TBio))%>%mutate(sample_date=as.Date(sample_date))%>%select(lake_name, sample_date,net_mouth_diameter_cm,Dbio)%>%pivot_wider(names_from = net_mouth_diameter_cm,values_from = Dbio)%>%drop_na()%>%rename(biomass_13=`13`, biomass_30=`30`)%>%arrange(lake_name,sample_date)

m3<-lm(biomass_30~biomass_13, date_sum2)
summary(m3)
#slope 3.0927, intercept -67.9689, r squared adjusted 0.6536

date_sum2$month<-as.numeric(format(date_sum2$sample_date,"%m"))
date_sum2$month<-as.factor(date_sum2$month)

#more plots 
ggplot(date_sum2, aes(x=biomass_13, y=biomass_30))+geom_point(aes(size=0.25, color=month))+
  geom_smooth(method='lm', se=FALSE)+stat_regline_equation(label.x=2, label.y=3000)

ggplot(date_sum2, aes(x=biomass_13, y=biomass_30))+geom_point(aes(size=0.25, color=lake_name))+
  geom_smooth(method='lm', se=FALSE)+stat_regline_equation(label.x=2, label.y=3000)

#hmmmm not as good... let's look at the relationship of mean weight (Biomass/Density)

date_sum2.1<-db_sent2013%>%filter(net_mouth_diameter_cm!=20)%>%
  group_by(lake_name,site_number,sample_date, net_mouth_diameter_cm)%>%
  summarize(Tmw=sum(mean_weight))%>%
  group_by(lake_name, sample_date, net_mouth_diameter_cm)%>%
  summarize(Dmw=mean(Tmw))%>%mutate(sample_date=as.Date(sample_date))%>%
  select(lake_name, sample_date,net_mouth_diameter_cm,Dmw)%>%pivot_wider(names_from = net_mouth_diameter_cm,values_from = Dmw)%>%drop_na()%>%rename(mw13=`13`, mw30=`30`)%>%arrange(lake_name,sample_date)

ggplot(date_sum2.1, aes(x=mw13, y=mw30))+geom_point()+
  geom_smooth(method='lm', se=FALSE)+stat_regline_equation(label.x=2, label.y=100)

m3.1<-lm(mw30~mw13, date_sum2.1)
summary(m3.1)

ggsave("figs/meta analysis/netdff_meanweight_relation.jpeg", width=8, height=6)

#slope 0.07142, intercept +8.0742, r squared adjusted 0.3471
#looks like the 30cm is better at catching big guys so let's account for that

##let's use mean weight coefficient (0.071) in the correction equation to adjust for Biomass 
## we know that B= density * mean weight 
## therefore the new conversion should be 

#biomass2=density2*((0.07142*mean weight)+8.0742)

##=====apply corrections to pre 2013 data=====

#load pre 2013 data and apply correction
##query from historicDB, is_slice_lake=TRUE 
db_sent_hist13 <- 
  read.csv("data/raw_data/sentinel zoops query_historicDB_20251030.csv")
db_sent_hist13$haul_depth_m<-as.numeric(db_sent_hist13$haul_depth_m)
##query from currentDB, is_slice_lake=TRUE and net_mouth_diameter_cm=13
db_sent_current13 <- 
  read.csv("data/raw_data/sentinel zoops query_currentDB_13cmnet_20251028.csv")

sent13 <- bind_rows(db_sent_hist13, db_sent_current13)#combine
sent13$sample_date<-as.Date(sent13$sample_date)

sent13<-sent13%>%filter(net_mouth_diameter_cm!=30& sample_date<'2013-01-01'& sample_date>'2008-01-01')%>%mutate(density=(1.8782*density)+2.7255, biomass=density*((0.07142*mean_weight)+8.0742),remarks="net diff correction applied to N and B") #calcs new density first, then uses new density to calc biomass

write.csv(sent13, "data/processed_data/sent_z_retro2008-2011corrected_20251028.csv", row.names=FALSE)
