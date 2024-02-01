library(sf)
library(dplyr)
library(ggplot2)

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


dept_bretagne <- communes_bretagne %>%
  group_by(dep) %>%
  summarize(surf = sum(surf2))


plot(dept_bretagne)

dept_bretagne_sf <- communes_bretagne %>%
  group_by(dep) %>%
  summarize(geometry= st_union(geometry))

plot(dept_bretagne_sf)

centroid_dept_bretagne <- dept_bretagne %>% summarise(centroide = st_centroid(geometry))
plot(centroid_dept_bretagne)

ggplot() +
  geom_sf(data = dept_bretagne_sf, fill = "lightblue", color = "black") +
  geom_sf(data = centroid_dept_bretagne, color = "red", size = 2) +
  theme_minimal()

summary(centroid_dept_bretagne)
centroid_dept_bretagne <- centroid_dept_bretagne %>% mutate(dept_lib =c("Côtes-d'Armor","Finistère", "Ille-et-Vilaine", "Morbihan" ))
ggplot() +
  geom_sf(data = dept_bretagne_sf, fill = "lightblue", color = "black") +
  geom_sf(data = centroid_dept_bretagne, color = "red", size = 3) +
  geom_sf_text(data = centroid_dept_bretagne, aes(label = dept_lib), color = "darkblue", size = 5, nudge_y = 10000) +
  theme_minimal()

centroid_coords <- st_coordinates(centroid_dept_bretagne)
#centroid_coords <- bind_cols(centroid_coords,centroid_dept_bretagne$dept_lib)
#centroid_coords <- bind_cols(centroid_coords,dept_bretagne$dep)

nom_commune_bretagne <- communes_bretagne %>% select(libelle)
intersection<-st_intersects(centroid_dept_bretagne,nom_commune_bretagne)
summary(intersection)
commune_centre <- NULL
for (i in 1:4) {
  
commune_centre[i]<-nom_commune_bretagne[intersection[[i]],1]$libelle
}
  
  
intersection2<-st_within(centroid_dept_bretagne,communes_bretagne)
commune_centre2 <- NULL
for (i in 1:4) {
  
  commune_centre2[i]<-nom_commune_bretagne[intersection2[[i]],1]$libelle
}
