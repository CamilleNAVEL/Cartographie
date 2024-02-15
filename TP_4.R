rm(list = ls())
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

bruz<-iris %>% 
  filter(depcom=="35047") %>% 
  left_join(
    data %>% select(code=IRIS,DISP_MED18),
    by="code"
  )
marseille <- st_transform(marseille, "+init=epsg:2154")
bruz <- st_transform(bruz, "+init=epsg:2154")
summary(marseille$DISP_MED18)
summary(bruz$DISP_MED18)

marseille <- marseille %>% filter(!is.na(DISP_MED18))
bruz <- bruz %>% filter(!is.na(DISP_MED18))

summary(marseille$DISP_MED18)
boxplot(DISP_MED18~depcom, data= marseille)
boxplot(DISP_MED18~depcom, data= bruz)

plot_marseille_map <-ggplot(marseille) +
  geom_sf(aes(fill = DISP_MED18)) +
  scale_fill_viridis_c(name = "Revenus", na.value = "white", option = "magma") +
  labs(title = "Revenus à Marseille")+
  theme_void()
ggplot(bruz) +
  geom_sf(aes(fill = DISP_MED18)) +
  scale_fill_viridis_c(name = "Revenus", na.value = "white", option = "magma") +
  labs(title = "Revenus à Bruz")+
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
iris_nb_b <- poly2nb(bruz, queen = TRUE)

# Résumé de l'objet résultant
summary(iris_nb)
summary(iris_nb_b)

#64 voisins pour le 4e elt

irirs_pds <- nb2listw(iris_nb,zero.policy=TRUE)

str(irirs_pds,max.level = 1)
irirs_pds_b <- nb2listw(iris_nb_b,zero.policy=TRUE)

str(irirs_pds_b,max.level = 1)

# Initialiser un vecteur pour stocker les sommes des poids
sum_weights <- numeric(length(irirs_pds$weights))

sum_weights_b <- numeric(length(irirs_pds_b$weights))

# Boucle pour calculer la somme des poids de tous les voisins pour chaque iris
for (i in seq_along(irirs_pds$weights)) {
  sum_weights[i] <- sum(irirs_pds$weights[[i]])
}

for (i in seq_along(irirs_pds_b$weights)) {
  sum_weights[i] <- sum(irirs_pds_b$weights[[i]])
}

marseille$DISP_MED18_STD <- scale(marseille$DISP_MED18)
mean(marseille$DISP_MED18_STD)
sd(marseille$DISP_MED18_STD)
marseille$DISP_MED18_STD<- as.vector(marseille$DISP_MED18_STD)

bruz$DISP_MED18_STD <- scale(bruz$DISP_MED18)
mean(bruz$DISP_MED18_STD)
sd(bruz$DISP_MED18_STD)
bruz$DISP_MED18_STD<- as.vector(bruz$DISP_MED18_STD)


par(mfrow=c(1,2))
moran.plot(marseille$DISP_MED18,irirs_pds)

moran.plot(marseille$DISP_MED18_STD,irirs_pds)
par(mfrow=c(1,1))

moran.test(marseille$DISP_MED18_STD, irirs_pds, randomisation = TRUE)
#on a bien autocorrelation
par(mfrow=c(1,2))
moran.plot(bruz$DISP_MED18,irirs_pds_b)

moran.plot(bruz$DISP_MED18_STD,irirs_pds_b)
par(mfrow=c(1,1))

moran.test(bruz$DISP_MED18_STD, irirs_pds_b, randomisation = TRUE)
#pas d'autocorrelation significative statistiquement à Bruz

mars_rev_lisa <-spdep::localmoran(marseille$DISP_MED18,irirs_pds)
class(mars_rev_lisa)
str(mars_rev_lisa,max.level = 1)
summary(mars_rev_lisa)
as.data.frame(mars_rev_lisa) %>% filter(Ii<0) %>% count()


# Créer un dataframe avec les résultats LISA et les indices des iris
mars_rev_lisa_df <- data.frame(mars_rev_lisa)
mars_rev_lisa_df$IRIS <- rownames(mars_rev_lisa_df)
str(mars_rev_lisa_df)
head(mars_rev_lisa_df)
# Ajouter la nouvelle variable LISA au fond d'Iris marseillais
marseille <- cbind(marseille, mars_rev_lisa_df)
head(marseille)
plot(marseille["Ii"])
#L'autocorreelation est plus forte dans les quartiers centre

names(mars_rev_lisa_df)
mars_rev_lisa_df %>% filter(Pr.z....E.Ii..<=0.95) %>% count()

plot(marseille["Pr.z....E.Ii.."],breaks = c(0,0.01,0.05,0.1,1))
#j yep
