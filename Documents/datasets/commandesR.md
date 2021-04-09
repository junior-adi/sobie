
# TRAITEMENT DES DONNEES

# importation des données

donnees <- read.csv("C:\\R\\taxons.csv",h=F,sep=",")
don <- data.frame(donnees)
# don<- data.frame(donnees,row.names = T)

# extraire le nom de la variable de la première ligne
# don$V1[1]

# obtenir les données sans les noms des variables des lignes
data <- don[1:18,2:27]

# calculer les indices de dissimlarité de Jaccard, Gower, etc.
# indice de Gower
# daisy.mat <- as.matrix(daisy(df, metric="gower", type=list(asymm=c(2,3,4,5,6))))
# gower.mat <- gower.dist(df)

indDissGower<- as.matrix(daisy(data, metric="gower", type=list(asymm=c(1:26))))

