library(sf)
library(dplyr)

commune <- st_read("commune_francemetro_2021.shp", options = "ENCODING=WINDOWS-1252")

summary(commune)
View(commune)
st_crs(commune)

communes_bretagne <- commune %>% filter(reg == 53) %>% select(code,libelle,epc,dep,surf)
#Nota bene: la table contient également une colonne géographie

summary(communes_bretagne)
plot(communes_bretagne)
plot(communes_bretagne,lwd=0.1)
plot(communes_bretagne,lwd=0.1)
plot(communes_bretagne,lwd=0.1)
plot(st_geometry(communes_bretagne),lwd = 0.3)
communes_bretagne <- communes_bretagne %>% mutate(surf2= st_area(communes_bretagne))
communes_bretagne <- communes_bretagne %>% mutate(surf2= units::set_units(surf2,"km^2"))

cor(communes_bretagne$surf,communes_bretagne$surf2)
# Les variables ne sont pas égales mais presque, sans doute les arrondis

dept_bretagne <- communes_bretagne %>%  distinct(code) %>% select(code,surf2)
