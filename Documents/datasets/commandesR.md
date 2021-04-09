
# TRAITEMENT DES DONNEES DU MEMOIRE (Par ADI A.D. Junior)

## importation des données
```R
donnees <- read.csv("C:\\R\\taxons.csv",h=F,sep=",")
don <- data.frame(donnees)
don<- data.frame(donnees,row.names = T)
```  
  

## extraire le nom de la variable de la première ligne
```R
don$V1[1]
```  
  

## obtenir les données sans les noms des variables des lignes
```R
data <- don[1:18,2:27]
# extraire une ligne
data[1,]
```  

## calculer les indices de dissimlarité de Jaccard, Gower, etc.
### indice de Gower
```R
daisy.mat <- as.matrix(daisy(df, metric="gower", type=list(asymm=c(2,3,4,5,6))))
gower.mat <- gower.dist(df)

indDissGower<- as.matrix(daisy(data, metric="gower", type=list(asymm=c(1:26))))
```
## jaccard Compute a Jaccard/Tanimoto similarity coefficient
(Le fichier se trouve à l'adresse https://cran.r-project.org/web/packages/jaccard/jaccard.pdf)[https://cran.r-project.org/web/packages/jaccard/jaccard.pdf]

## Autres liens utiles expliquant comment calculer et interpréter les résultats
  .  (https://www.statology.org/jaccard-similarity-in-r/)[https://www.statology.org/jaccard-similarity-in-r/]
  

## site pouvant faire le calcul même sur un dataset comme c'est le cas avec R : 
  .  (Site substitut de R (pour la distance de Jaccard) : https://nnnn.shinyapps.io/jaccard/)[https://nnnn.shinyapps.io/jaccard/]

  .  (https://stats.stackexchange.com/questions/176613/jaccard-similarity-in-r)[https://stats.stackexchange.com/questions/176613/jaccard-similarity-in-r]

## utilisation de librairie sous R 4.0.5

```R

library('clusteval')
cluster_similarity(data3$IDS, data3$CESD, similarity="jaccard", method="independence")
```  

# fonctions faites maison 

# Le jeu de données
```R
df2 <- data.frame(
  IDS = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
  CESD = c(1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))

# Function returns the Jaccard index and Jaccard distance
jaccard_byADI <- function(df, margin) {
  if (margin == 1 | margin == 2) {
    M_00 <- apply(df, margin, sum) == 0
    M_11 <- apply(df, margin, sum) == 2
    if (margin == 1) {
      df <- df[!M_00, ]
      JSim <- sum(M_11) / nrow(df)
    } else {
      df <- df[, !M_00]
      JSim <- sum(M_11) / length(df)
    }
    JDist <- 1 - JSim
    return(c(JSim = JSim, JDist = JDist))
  } else break
}

# usage 

jaccard_byADI(df2, 1)
```

# Améliorations du code précédent pour permettre un calcul de similarité sur dataset à plus de deux lignes

```R
# Modification du code pour permettre une mesure de similarité entre toutes les observations.


# Jaccar Index
library(dplyr)

# Your dataset
df <- data.frame(t(data.frame(c1=rnorm(100),
                              c2=rnorm(100),
                              c3=rnorm(100),
                              c4=rnorm(100),
                              c5=rnorm(100),
                              c6=rnorm(100))))

df[df > 0] <- 1
df[df <= 0] <- 0
df

# Function returns the Jaccard index and Jaccard distance
# Parameters:
# 1. df, dataframe of interest
# 2. margin, axis in which the apply function is meant to move along
jaccard <- function(df, margin=1) {
  if (margin == 1 | margin == 2) {
    M_00 <- apply(df, margin, sum) == 0
    M_11 <- apply(df, margin, sum) == 2
    if (margin == 1) {
      df <- df[!M_00, ]
      JSim <- sum(M_11) / nrow(df)
    } else {
      df <- df[, !M_00]
      JSim <- sum(M_11) / length(df)
    }
    JDist <- 1 - JSim
    return(c(JSim = JSim, JDist = JDist))
  } else break
}

jaccard(df[1:2,], margin=2)


jaccard_per_row <- function(df, margin=1){
   require(magrittr)
   require(dplyr)
   key_pairs <- expand.grid(row.names(df), row.names(df))
   results <- t(apply(key_pairs, 1, function(row) jaccard(df[c(row[1], row[2]),], margin=margin)))
   key_pair <- key_pairs %>% mutate(pair = paste(Var1,"_",Var2,sep=""))
   results <- data.frame(results)
   row.names(results) <- key_pair$pair
   results
}

jaccard_per_row(df, margin=2)


```



































































