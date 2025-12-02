# --- Fixer le répertoire automatiquement
path           = rstudioapi::getActiveDocumentContext()$path
Encoding(path) = "UTF-8"
setwd(dirname(path))


# ______________________________________________________________________________
#  --------------------------------- Packages  ---------------------------------
# ______________________________________________________________________________
library(gplots)    # Graphiques
library(corpcor)   # Corrélations partielles
library(ppcor)     # Corrélations partielles 
library(tidyr)     # Traitement des données
library(dplyr)     # Traitement des données
library(ggplot2)   # Graphiques
library(lubridate) # Traitement des dates
library(qgraph)    # Graphiques en réseau
library(xts)       # Gestion des séries temporelles

# - Si importation des données depuis le datalab
# https://app.taceconomics.com/user # récupération de l'API key
# https://github.com/taceconomics/taceconomics-r # détail sur le fonctionnement du package taceconomics
# install_github("taceconomics/taceconomics-r")
library(taceconomics)
taceconomics.apikey(YOUR_APIKEY) # A MODIFIER

# - Fixer la graine
set.seed(123)

# ______________________________________________________________________________
#  ------------------------ Importation des données  ---------------------------
# ______________________________________________________________________________
cnt = c('NZL','FJI','TON','AUS','CAN','DOM','JAM','CRI','GTM','MEX',
        'NIC','ARG','BRA','CHL','COL','GUY','PER','PRY','SUR','URY',
        'KAZ','KGZ','TJK','JPN','KOR','TWN','CHN','MNG','SGP','IDN',
        'PHL','THA','KHM','LAO','IND','PAK','BGD','ARM','AZE','GEO',
        'KWT','TUR','BLR','POL','ROU','RUS','UKR','MDA','GBR','ISL',
        'NOR','SWE','ALB','CHE','EUZ','DZA','EGY','LBY','MUS','BDI',
        'KEN','MDG','MOZ','RWA','UGA','AGO','ZAF','GHA','GMB','LBR',
        'NGA') 

# --- Boucle d'importation

# 1) Initialiser un dataset au format xts vide (l'appeler "dataset")
dataset = xts()

# 2) a- Construire une boucle qui importe automatiquement les taux de change de toutes les économies situées dans "cnt" depuis janvier 2000
#    b- Passer les taux de change mensuels en taux de croissance a/a dès l'importation (IFS/ENDA_XDC_USD_RATE_M/CODE_PAYS)
#    c- Stocker les taux de change en colonne dans "dataset"

for (c in cnt){
  exr_cnt = taceconomics::getdata(paste0("IFS/ENDA_XDC_USD_RATE_M/", c, "?start_date=2000-01-01&collapse=M&transform=growth_yoy"))
  dataset = cbind(dataset, exr_cnt)
}

# 3) Mettre les codes ISO des pays en colnames de "dataset" et le transformer en dataframe
colnames(dataset) = cnt
dataset = data.frame(dataset)

# --- Traitement des données
# 4) Vérifier le nombre de valeurs manquantes par colonnes
#   - Si trop de valeurs manquantes pour un pays, supprimer le pays
#   - Sinon, remplacer les quelques NA (valeur précédente)
# Vérifier que tous les NA ont été traités
colSums(is.na(dataset))
dataset = na.locf(dataset, na.rm=FALSE)
colSums(is.na(dataset))
#dataset = na.omit(dataset)

# ______________________________________________________________________________
#  ----------------------- Analyse des corrélations ----------------------------
# ______________________________________________________________________________
# --- Correlations
# 5) Construire la matrice de corrélations et la stocker dans "cor_matrix"
cor_matrix = round(cor(dataset), 2)

# 6) Ordonner les pays dans la matrice de corrélation via un algorithme de clustering
#    - Stocker l'ordre dans "col_order"
#    - Réorganiser "dataset" selon "col_order" dans un nouveau dataframe appelé "dataset_reordered"
#    -> Aide à mieux identifier les pattern
col_order = hclust(dist(cor_matrix))$order
dataset_reordered = dataset[,col_order]
plot(hclust(dist(cor_matrix)))

# 7) Visualiser les corrélations calculées sur "dataset_reordered" à l'aide de la fonction "heatmap.2"
heatmap.2(as.matrix(cor(dataset_reordered)),
          dendrogram = "none",   # Set to "none" to disable clustering
          Rowv  = FALSE,         # Do not reorder rows
          Colv  = FALSE,         # Do not reorder columns
          main  = "Correlation Heatmap", # Title
          xlab  = "Variables",           # Label x-axis
          ylab  = "Variables",           # Label y-axis
          col   = colorRampPalette(c("blue", "white", "red"))(100), # Legend colors
          trace = "none",         # Do not show trace lines
          key   = TRUE,           # Show color key
          density.info = "none"   # Do not show density plot
)


# - Fonction testant la significativité des corrélations
calculate_pvalues <- function(cor_matrix) {
  pvalues = matrix(NA, nrow = ncol(cor_matrix), ncol = ncol(cor_matrix))
  for (i in seq_along(cor_matrix)) {
    for (j in seq_along(cor_matrix)) {
      if (i != j) {
        test_result   = cor.test(dataset[[i]], dataset[[j]])
        pvalues[i, j] = test_result$p.value
      }
    }
  }
  return(pvalues)
}

# 8) Appliquer la fonction "calculate_pvalues" sur "dataset" et stocker le résultat dans "pvalues"
pvalues = round(calculate_pvalues(dataset),2)

# 9) Construire un graph des corrélations significatives (appliquer "qgraph" (layout="spring") sur "cor_matrix")
#    - Attention, seules les corrélations significatives doivent apparaître
edge_colors = ifelse(cor_matrix > 0, "red", "darkblue")
edge_colors[pvalues > 0.05] = "white"

qgraph(cor_matrix
       , layout='spring'
       , edge.color = edge_colors)

# --- Corrélations partielles
# 10) Calculer les corrélations partielles sur "dataset" et stocker les résulats dans "pcor_matrix"
dataset_per = dataset[(as.Date(rownames(dataset)) >= "2000-01-01") & (as.Date(rownames(dataset)) <= "2025-01-01"),]
pcor_matrix = pcor(dataset_per, method = "pearson")$estimate

# 11) Calculer la significativité des corrélations partielles
#     - Nécessite un nombre suffisant d'observations si beaucoup de variables de controle
N = nrow(dataset)
k = ncol(dataset)-2  # Nombre de variables de controle
t_stat_partial = pcor_matrix * sqrt(N - k - 2) / sqrt(1 - pcor_matrix^2)

# 12) Construire un graph des corrélations partielles significatives
# a- Fixer 0 dans le triangle supérieur pour ne pas avoir de doublons
pcor_matrix[lower.tri(pcor_matrix)] = 0

# b- Graph
edge_colors = ifelse(pcor_matrix > 0, "red", "darkblue")
edge_colors[(t_stat_partial < 1.96) & (t_stat_partial > -1.96) ] <- "white"  # 1.96 pour 5%, 2.5756 pour 1%

qgraph(pcor_matrix,
       labels     = colnames(pcor_matrix),
       layout     = "spring",
       edge.color = edge_colors,
       arrows     = FALSE)


# ______________________________________________________________________________
#  ----------------------- Datation des crises de change -----------------------
# ______________________________________________________________________________
# --- Datation (critère de Frankel & Rose)
th1 = 25 # Dépréciation a/a supérieure ou égale à 25%
th2 = 10 # Taux de dépréciation 10 pps supérieur à l'année précédente si dépréciation il y a eu

# 13) Initialisation de 2 datasets ("dataset_th1" et "dataset_th2") avec le contenu de "dataset"
dataset_th1 = dataset_th2 = dataset

# 14) Dans "dataset_th1":
#     - Remplacer par 1 les observations qui remplissent le critère "th1", mettre 0 sinon
#     - Vérifier le nombre de fois que la condition est remplie pour chaque pays
dataset_th1 = ifelse(dataset_th1 >= th1, 1, 0)
colSums(dataset_th1)


# 15) Dans "dataset_th2":
#     - Remplacer par 1 les observations qui remplissent le critère "th2", mettre 0 sinon
#     - Vérifier le nombre de fois que la condition est remplie pour chaque pays
dataset_th2 = apply(dataset_th2, 2, function(x) diff(x, lag=12))
dataset_th2 = as.data.frame(dataset_th2)
dataset_th2 = ifelse(dataset_th2 >= th2, 1, 0)
colSums(dataset_th2)

# 16) Vérifier que dataset_th1 et dataset_th2 ont les mêmes dimensions (lignes et colonnes)
#      - Faire le nécessaire sinon
dataset_th1 = dataset_th1[rownames(dataset_th1) %in% rownames(dataset_th2),]


# 17) Combiner les deux critères (crise que si les deux critères sont remplis simultanément)
condition = dataset_th1 == 1 & dataset_th2 == 1
dataset_th1[condition]  = 1
dataset_th1[!condition] = 0
dataset_th1 = data.frame(dataset_th1)

# --- Affichage des résultats
# 18) Stocker dans "dataset_crisis" le total de crises par date
#  a- Renommer la colonne "Value"
#  b- Ajouter une colonne "Date" pour stocker la date au format date (as.Date)
#  c- Ordonner les colonnes en "Date", "Value"

dataset_crisis = data.frame(rowSums(dataset_th1))
# plot(as.xts(dataset_crisis))
colnames(dataset_crisis) = "Value"
dataset_crisis$Date = as.Date(rownames(dataset_crisis))
dataset_crisis = dataset_crisis[,c(2,1)]

# 19) Faire un graph représentant l'occurence des crises de change par date (ggplot + geom_bar)
ggplot(dataset_crisis, aes(x = Date, y = Value, fill = Value)) +
  geom_bar(stat = "identity", color = "#454141") +
  theme_minimal() +
  labs(title = "Monthly currency crises", x = "Date", y = "Number of currency crises") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "24 month")


