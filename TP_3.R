rm(list=ls())
library(dplyr)
library(sf)
library(mapsf)
library(classInt)
library(leaflet)
library(ggplot2)
# Import des donnees 
# Fond communes France metropolitaine
communes_fm<- st_read("commune_francemetro_2021.shp", options = "ENCODING=WINDOWS-1252") %>% 
  select(code,libelle,surf)
# Import des population légales des communes en 2019
pop_com_2019<-openxlsx::read.xlsx("Pop_legales_2019.xlsx")
# Correction pour la ville de Paris
pop_com_2019<-pop_com_2019 %>% 
  mutate(COM=if_else(substr(COM,1,3)=="751","75056",COM)) %>% 
  group_by(code=COM) %>% 
  summarise(pop=sum(PMUN19))
# Jointure
communes_fm<-communes_fm %>% 
  left_join(pop_com_2019,
            by="code") %>% 
  mutate(densite=pop/surf)

ggplot(data = communes_fm, aes(x = densite)) +
  geom_density(binwidth = 5, fill = "blue", color = "black") +  # binwidth contrôle la largeur des barres de l'histogramme
  labs(title = "Histogramme des valeurs", x = "Communes", y = "Densité") +
  theme_minimal()
        ytitleFont=c(14,"bold", "blue"))

summary(communes_fm$densite)
plot(communes_fm["densite"], border=FALSE)
plot(communes_fm["densite"], breaks="quantile", main="quantile", border = FALSE)
plot(communes_fm["densite"], breaks="sd", main="sd", border = FALSE)
plot(communes_fm["densite"], breaks="jenks", main="jenks", border = FALSE)
plot(communes_fm["densite"], breaks="pretty", main="pretty", border = FALSE)
