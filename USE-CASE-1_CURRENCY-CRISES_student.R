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

# 2) a- Construire une boucle qui importe automatiquement les taux de change de toutes les économies situées dans "cnt" depuis janvier 2000
#    b- Passer les taux de change mensuels en taux de croissance a/a dès l'importation (IFS/ENDA_XDC_USD_RATE_M/)
#    c- Stocker les taux de change en colonne dans "dataset"

# 3) Mettre les codes ISO des pays en colnames de "dataset" et le transformer en dataframe


# --- Traitement des données
# 4) Vérifier le nombre de valeurs manquantes par colonnes
#   - Si trop de valeurs manquantes pour un pays, supprimer le pays
#   - Sinon, remplacer les quelques NA (valeur précédente)
# Vérifier que tous les NA ont été traités


# ______________________________________________________________________________
#  ----------------------- Analyse des corrélations ----------------------------
# ______________________________________________________________________________
# --- Correlations
# 5) Construire la matrice de corrélations et la stocker dans "cor_matrix"

# 6) Ordonner les pays dans la matrice de corrélation via un algorithme de clustering
#    - Stocker l'ordre dans "col_order"
#    - Réorganiser "dataset" selon "col_order" dans un nouveau dataframe appelé "dataset_reordered"
#    -> Aide à mieux identifier les pattern

# 7) Visualiser les corrélations calculées sur "dataset_reordered" à l'aide de la fonction "heatmap.2"

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

# 9) Construire un graph des corrélations significatives (appliquer "qgraph" (layout="spring") sur "cor_matrix")
#    - Attention, seules les corrélations significatives doivent apparaître


# --- Corrélations partielles
# 10) Calculer les corrélations partielles sur "dataset" et stocker les résulats dans "pcor_matrix"

# 11) Calculer la significativité des corrélations partielles
#     - Nécessite un nombre suffisant d'observations si beaucoup de variables de controle
N = nrow(dataset)
k = ncol(dataset)-2  # Nombre de variables de controle
t_stat_partial = pcor_matrix * sqrt(N - k - 2) / sqrt(1 - pcor_matrix^2)

# 12) Construire un graph des corrélations partielles significatives
# a- Fixer 0 dans le triangle supérieur pour ne pas avoir de doublons
pcor_matrix[lower.tri(pcor_matrix)] = 0

# b- Graph
edge_colors = ifelse(pcor_matrix > 0, "darkgreen", "red")
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

# 14) Dans "dataset_th1":
#     - Remplacer par 1 les observations qui remplissent le critère "th1", mettre 0 sinon
#     - Vérifier le nombre de fois que la condition est remplie pour chaque pays

# 15) Dans "dataset_th2":
#     - Remplacer par 1 les observations qui remplissent le critère "th2", mettre 0 sinon
#     - Vérifier le nombre de fois que la condition est remplie pour chaque pays

# 16) Vérifier que dataset_th1 et dataset_th2 ont les mêmes dimensions (lignes et colonnes)
#      - Faire le nécessaire sinon

# 17) Combiner les deux critères (crise que si les deux critères sont remplis simultanément)


# --- Affichage des résultats
# 18) Stocker dans "dataset_crisis" le total de crises par date
#  a- Renommer la colonne "Value"
#  b- Ajouter une colonne "Date" pour stocker la date au format date (as.Date)
#  c- Ordonner les colonnes en "Date", "Value"

# 19) Faire un graph représentant l'occurence des crises de change par date (ggplot + geom_bar)


# ______________________________________________________________________________
#  ------------------------ Corrélations et crises  ----------------------------
# ______________________________________________________________________________
# --- Evolution des corrélations
# 20) Calculer différent type de corrélations glissantes (fenêtre de trois ans)
start_date = as.Date("2000-01-01")
end_date   = as.Date("2002-12-01")

dataset_cor   = data.frame(dataset[,1]) # Récupérer la structure du dataframe "dataset"
dataset_cor[] = NA                      # Affecter NA partout (rempli avec la boucle suivante)
colnames(dataset_cor) = "Cor"
rownames(dataset_cor) = rownames(dataset)

dataset_cor_pos = dataset_cor_neg = dataset_cor_abs = dataset_cor # initialisation des dataset pour les différentes corrélations

while (end_date <= "2024-12-01"){
  # Calcul des corrélations et test de significativité
  dataset_per    = dataset[(as.Date(rownames(dataset)) >= start_date) & (as.Date(rownames(dataset)) <= end_date),]
  cor_matrix_per = round(cor(dataset_per),2)
  cor_matrix_per[is.na(cor_matrix_per)] = 0 # Traitement des NA (taux de change qui ne varient pas)
  pvalues_per    = round(calculate_pvalues(dataset_per),2)
  
  # Garder seulement corrélations significatives + matrice triangulaire
  condition = pvalues_per <= 0.05
  cor_matrix_per[!condition]   = 0
  cor_matrix_per[lower.tri(cor_matrix_per)] = 0
  
  # Traitement des corrélations
  cor_sum     = sum(cor_matrix_per)      / ncol(cor_matrix)
  cor_sum_abs = sum(abs(cor_matrix_per)) / ncol(cor_matrix) # corrélations en valeurs absolues
  cor_sum_pos = sum(cor_matrix_per[cor_matrix_per > 0]) / ncol(cor_matrix)
  cor_sum_neg = sum(cor_matrix_per[cor_matrix_per < 0]) / ncol(cor_matrix) * (-1)
  
  # Affectation dans le dataset de sortie
  dataset_cor[rownames(dataset_cor) == end_date,] = cor_sum
  dataset_cor_abs[rownames(dataset_cor_abs) == end_date,] = cor_sum_abs
  dataset_cor_pos[rownames(dataset_cor_pos) == end_date,] = cor_sum_pos
  dataset_cor_neg[rownames(dataset_cor_neg) == end_date,] = cor_sum_neg
  
  # Incrémentation
  start_date = start_date + months(1)
  end_date   = end_date   + months(1)
}


# --- Corrélations vs datation des crises de change
# 21) Transformer "dataset_cor" et "dataset_crisis" au format ts (l'un commence en 2000 et l'autre 2001)
#     - Stocker le ts de "datset_cor" dans une variable "xx"
#     - Stocker le ts de "datset_crisis" dans une variable "yy"

# 22) Faire un graphique simple avec les deux séries (fonction plot et lines)


# --- Corrélations vs crises financières
# 23) Faire un plot de "xx"
#     - Mettre le titre "Correlations during Financial Crises"
#     - Fixe le min max de l'axe des ordonnées à 0 et 25

# 24) Coloration des périodes de crises majeures
periods <- list(
  GFC       = window(xx, start = c(2007, 08), end = c(2009, 08)),  # Crise financière de 2008
  Sovereign = window(xx, start = c(2010, 04), end = c(2012, 07)),  # Crise de la dette européenne
  Taper     = window(xx, start = c(2013, 05), end = c(2013, 10)),  # Taper Tantrum
  China     = window(xx, start = c(2015, 06), end = c(2016, 02)),  # Krach boursier chinois
  Covid     = window(xx, start = c(2019, 12), end = c(2022, 04))  # Covid
)

for (i in seq_along(periods)) {
  abline(v = as.numeric(index(periods[[i]])), col = abline_v_colors[i], lty = abline_lty)
}

# Ajout de la légende
legend("topleft", legend = names(periods), col = abline_v_colors, lty = abline_lty, bg = "transparent", cex=0.7)

# 25) Ajouter "dataset_cor_abs", "dataset_cor_pos", "dataset_cor_neg" sur le plot 
#     - Passer les séries au format ts au préalable


