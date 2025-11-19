
library(tidyr)


data <- read.csv("Data/Input/UROP DATASHEET - Sheet1.csv")

data.clean <- data %>% 
  filter(!is.na(Diameter.of.pore.um))


diameter.spp <- pivot_wider(data.clean, names_from = "Species", values_from = "Diameter.of.pore.um")
diameter.spp2 <- diameter.spp %>% 
  select(Bosmina, Eubosmina)
#replace NA values with blank
diameter.spp2[is.na(diameter.spp2)] <- ""


write.csv(diameter.spp, file = "Diameter_by_species.csv")


data.clean <- lmer()