library(dplyr)
library(sf)
library(spdep)
library(RColorBrewer)
library(ggplot2)
library(gridExtra)
# Charger le package spdep
library(spdep)



# Importggplot2# Import des données
iris<-st_read("./TP4/iris_franceentiere_2021/iris_franceentiere_2021.shp")
data<-read.csv2("./TP4/data/BASE_TD_FILO_DISP_IRIS_2018.csv",sep=";",dec=".")

# Jointure
marseille<-iris %>% 
  filter(substr(depcom,1,3)=="132") %>% 
  left_join(
    data %>% select(code=IRIS,DISP_MED18),
    by="code"
  )

marseille <- st_transform(marseille, "+init=epsg:2154")

summary(marseille$DISP_MED18)

marseille <- marseille %>% filter(!is.na(DISP_MED18))

summary(marseille$DISP_MED18)
boxplot(DISP_MED18~depcom, data= marseille)

plot_marseille_map <-ggplot(marseille) +
  geom_sf(aes(fill = DISP_MED18)) +
  scale_fill_viridis_c(name = "Revenus", na.value = "white", option = "magma") +
  labs(title = "Revenus à Marseille")+
  theme_void()

# Créer une permutation aléatoire des revenus DISP_MED18
marseille$DISP_MED18_ALEA <- sample(marseille$DISP_MED18)

# Créer la carte de la distribution aléatoire des revenus à Marseille
plot_marseille_alea_map<-ggplot(marseille) +
  geom_sf(aes(fill = DISP_MED18_ALEA)) +
  scale_fill_viridis_c(name = "Revenus aléatoires", na.value = "grey", option = "magma") +
  labs(title = "Revenus aléatoires à Marseille")+
  theme_void()
# Affichage des deux cartes côte à côte
grid.arrange(plot_marseille_map, plot_marseille_alea_map, ncol = 2)


# Créer la liste des voisins pour chaque iris
iris_nb <- poly2nb(marseille, queen = TRUE)

# Résumé de l'objet résultant
summary(iris_nb)

#64 voisins pour le 4e elt

irirs_pds <- nb2listw(iris_nb,zero.policy=TRUE)

str(irirs_pds,max.level = 1)
# Initialiser un vecteur pour stocker les sommes des poids
sum_weights <- numeric(length(irirs_pds$weights))

# Boucle pour calculer la somme des poids de tous les voisins pour chaque iris
for (i in seq_along(irirs_pds$weights)) {
  sum_weights[i] <- sum(irirs_pds$weights[[i]])
}

marseille$DISP_MED18_STD <- scale(marseille$DISP_MED18)
mean(marseille$DISP_MED18_STD)
sd(marseille$DISP_MED18_STD)
marseille$DISP_MED18_STD<- as.vector(marseille$DISP_MED18_STD)

moran.plot(marseille$DISP_MED18_STD,irirs_pds)

moran.test(marseille$DISP_MED18_STD, irirs_pds, randomisation = TRUE)
#on a bien autocorrelation

mars_rev_lisa <-spdep::localmoran(marseille$DISP_MED18,irirs_pds)

