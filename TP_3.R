rm(list=ls())
library(dplyr)
library(sf)
library(mapsf)
library(classInt)
library(leaflet)
library(ggplot2)
library(gridExtra)
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
        ytitleFont=c(14,"bold", "blue")

summary(communes_fm$densite)
plot(communes_fm["densite"], border=FALSE)
plot(communes_fm["densite"], breaks="quantile", main="quantile", border = FALSE)
plot(communes_fm["densite"], breaks="sd", main="sd", border = FALSE)
plot(communes_fm["densite"], breaks="jenks", main="jenks", border = FALSE)
plot(communes_fm["densite"], breaks="pretty", main="pretty", border = FALSE)


pal1 <- RColorBrewer::brewer.pal(n = 5, name = "YlOrRd")
denspop_quant <- classIntervals(
  communes_fm$densite,
  style = "quantile", 
  n = 5
)
str(denspop_quant)
head(denspop_quant$var)
denspop_quant$brks 

plot(
  denspop_quant,
  pal = pal1,
  main = "quantile"
)
analyser_discret <- function(method, nb_classes){
  denspop_c <- classIntervals(
    communes_fm$densite,
    style = method, 
    n = nb_classes
  )
  print(denspop_c$brks)
  plot(
    denspop_c,
    pal = pal1,
    main = method
  )
  return(denspop_c)
}
# Avec cinq classes:
par(mfrow=c(2,2))
all_discret <- sapply(c("quantile", "sd","pretty","jenks"), analyser_discret, nb_classes = 5)
par(mfrow=c(1,1))

# A partir des informations obtenues, on peut définir nos propres intervalles. 
quantile(communes_fm$densite, probs = seq(0,1,0.1))
summary(communes_fm$densite)
#40 = médiane
#162 = moyenne
#on reprend certaines bornes de Jenks - en fusionnant les derniers intervalles
# Un exemple de découpage manuel avec 7 classes
denspop_man_brks7 <- c(0,40,162,500,1000,4000,8000,27200)
# Un exemple de découpage manuel avec 5 classes
denspop_man_brks5 <- c(0,40,162,1000,8000,27200)

popcomfm_sf <- communes_fm %>%
  mutate(
    densite_c = cut(
      densite,
      breaks = denspop_man_brks5,
      include.lowest = TRUE,
      right = FALSE,
      ordered_result = TRUE
    )
  )


#e.  Analyser la distribution de cette variable. Représenter la variable discrétisée sur une carte, en créant préalablement une nouvelle palette de couleurs ayant le bon nombre de classes.


table(popcomfm_sf$densite_c)
pal2 <- c(
  RColorBrewer::brewer.pal(
    n=5,
    name="Greens"
  )[4:3],
  RColorBrewer::brewer.pal(
    n=5,
    name="YlOrRd"
  )[c(2,4:5)]
)
pal2 <-RColorBrewer::brewer.pal(5, "Set3")
plot(
  popcomfm_sf["densite_c"], 
  pal=pal2, 
  border = FALSE,
  main = "Densité de population",
)
ggplot() +
  geom_sf(data = popcomfm_sf, aes(fill = densite_c),color="purple",size=0.01) +
  scale_fill_manual(name = "Densité de population", values = pal2) +
  labs(title = "Densité de population") +
  theme_minimal()

#II
rm(list=ls())
pauvreté <- openxlsx::read.xlsx("Donnees/Taux_pauvrete_2018.xlsx")
communes_fm<- st_read("commune_francemetro_2021.shp", options = "ENCODING=WINDOWS-1252") %>% 
  select(code,libelle,surf,dep)
departement <- communes_fm %>% group_by(dep)  %>%
  summarize(geometry= st_union(geometry))
departement<-departement %>% 
  left_join(pauvreté,
            by=c("dep" = "Code"))


plot(departement["Tx_pauvrete"], border=FALSE)
plot(departement["Tx_pauvrete"], border=FALSE,breaks = "fisher")
plot(departement["Tx_pauvrete"], border=FALSE,breaks = "equal")
plot(departement["Tx_pauvrete"], border=FALSE,breaks = "quantile")

man_cut <- c(0, 13, 17, 25)
departement <- departement %>%
  mutate(
    tp_c = cut(
      Tx_pauvrete,
      breaks = man_cut,
      include.lowest = TRUE,
      right = FALSE,
      ordered_result = TRUE
    )
  )



table(departement$tp_c)
pal2 <- c(
  RColorBrewer::brewer.pal(
    n=5,
    name="Greens"
  )[4:3],
  RColorBrewer::brewer.pal(
    n=5,
    name="YlOrRd"
  )[c(2,4:5)]
)
paris <- departement %>% filter(dep %in% c(75, 92, 93, 94))


# Création d'une colonne pour la disposition
departement$layout <- "France"
paris$layout <- "Paris"

# Combinaison des données des départements et de Paris
combined_data <- rbind(departement, paris)

# Création du tracé avec facet_wrap
ggplot(combined_data) +
  geom_sf(aes(fill = tp_c), color = NA) +
  scale_fill_manual(name = "Taux de pauvreté", values = pal2) +
  labs(title = "Taux de pauvreté") +
  facet_wrap(~ layout, scales = "free") +
  theme_minimal()

