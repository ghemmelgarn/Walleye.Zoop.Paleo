##Data tidy exmaples to zoop DB for Large Lakes and Sentinel Lakes- For Grace Hemmelgarn- 2025Dec12

##example below of data correction for Large Lakes, based on Retro analysis
# these account for changes between analysts in adult/immature copepod ID
correction<-leech%>%filter(year>2019)%>%mutate(density=case_when(grp2=='cyclopoids'~(density*1.1582),
                                                                 grp2=='calanoids'~(density*1.1418),
                                                                 species=='nauplii'~(density*0.9191),
                                                                 species=='copepodites'~(density*0.7804),
                                                                 TRUE~density))%>%
  mutate(biomass=case_when(grp2=='cyclopoids'~(biomass*1.1403),
                           grp2=='calanoids'~(biomass*1.2047),
                           species=='nauplii'~(biomass*0.9684),
                           species=='copepodites'~(biomass*0.6866),
                           TRUE~biomass))


jodie<-leech%>%filter(year<2020)
leech<-bind_rows(jodie,correction)
rm('jodie','correction')

#example below of data correction for Sentinel Lakes, based on PCA analysis
# these account for changes between analysts in adult/immature copepod ID
correction<-sent_z%>%filter(year>2019)%>%mutate(density=case_when(grp3=='cyclopoids'~(density*1.2409),
                                                                  grp3=='calanoids'~(density*1.83),
                                                                  species=='nauplii'~(density*0.8831),
                                                                  species=='copepodites'~(density*0.7944),
                                                                  TRUE~density))%>%
  mutate(biomass=case_when(grp3=='cyclopoids'~(biomass*1.1754),
                           grp3=='calanoids'~(biomass*1.0832),
                           species=='nauplii'~(biomass*0.9685),
                           species=='copepodites'~(biomass*0.7719),
                           TRUE~biomass))


jodie<-sent_z%>%filter(year<2020)
sent_z<-bind_rows(jodie,correction)
rm('jodie','correction')

##example taxonomic lumping 
sent_z$species[sent_z$species == 'Bosmina longirostris']<- "Bosmina sp."
sent_z$species[sent_z$species == 'Alona setulosa']<- "Alona sp."
sent_z$species[sent_z$species == 'Daphnia pulex']<- "Daphnia pulicaria"
sent_z$grp2[sent_z$species == 'Daphnia sp.']<- "small cladocerans"
sent_z$grp2[sent_z$grp2 == 'NULL']<- "immature copepods"