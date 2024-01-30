# --- Fixer le répertoire automatiquement
path           = rstudioapi::getActiveDocumentContext()$path
Encoding(path) = "UTF-8"
setwd(dirname(path))


# ______________________________________________________________________________
#  --------------------------------- Packages  ---------------------------------
# ______________________________________________________________________________
# Pour pcalg
# install.packages("BiocManager")
# BiocManager::install("graph")
# BiocManager::install("RBGL")
# BiocManager::install("Rgraphviz")

library(pcalg)     # Directe Acyclic Graph (DAG)
library(gplots)    # Graphiques
library(igraph)    # Graph des DAG
library(corpcor)   # Corrélations partielles
library(ppcor)     # Corrélations partielles 
library(qgraph)    # Graphique en réseau
library(tidyr)     # Traitement des données
library(dplyr)     # Traitement des données
library(ggplot2)   # Graphiques
library(lubridate) # Traitement des dates
library(xlsx)      # Importation Excel alternative 1
library(readxl)    # Importation Excel alternative 2



# - Si importation des données depuis le datalab
# https://github.com/taceconomics/taceconomics-r
library(taceconomics)
taceconomics.apikey("YOUR API KEY")

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
        'NGA') # classement par niveau de dev puis région

# Fonction taux de croissance
growth_rate <- function(x,l=12, start_date=c(1999,1), freq=12){
  if (any(class(x)==c("xts"))){
    y = 100*(x/stats::lag(x,l)-1)
  } else {
    x = ts(x, start=start_date, freq=freq)
    y = 100*(x/stats::lag(x,-l)-1)
  }
  colnames(y) = colnames(x)
  return(y)
}

# # --- Mode d'imporation numéro 1: API TAC Datalab
# dataset = xts()
# 
# for (c in cnt){
#   exr_cnt = taceconomics::getdata(paste0("IFS/ENDA_XDC_USD_RATE_M/", c, "?start_date=1999-01-01&frequency=M'"))
#   exr_cnt = growth_rate(exr_cnt)
#  #exr_cnt = diff(log(exr_cnt), lag=12)
# 
#   dataset = cbind(dataset, exr_cnt)
# }
# 
# colnames(dataset) = cnt
# dataset = data.frame(dataset)


# --- Mode d'imporation numéro 2: IMF excel
#- Importation excel
dataset_imf  = data.frame(read_excel('USE-CASE-1_EXR_IFS.xlsx', 1)) #library(readxl))

# Suppression des colonnes inutiles
columns_to_remove = c("Country.Name", "Country.Code", "Indicator.Name", "Indicator.Code", "Attribute", "Base.Year")
dataset_imf = dataset_imf[, !(names(dataset_imf) %in% columns_to_remove)]

# Suppression de la première colonne
dataset_imf = data.frame(t(dataset_imf))
colnames(dataset_imf) = dataset_imf[1, ]
dataset_imf = dataset_imf[-1, ]

# Transformation des rownames en date
start_date = as.Date("1999-01-01")
num_date   = nrow(dataset_imf)
monthly_dates = seq.Date(from = start_date, by = "months", length.out = num_date)
rownames(dataset_imf) = monthly_dates

# Conserver seulement les pays qui nous intéressent (liste c)
dataset_imf = dataset_imf[, names(dataset_imf) %in% cnt]
dataset_imf = dataset_imf[,cnt] #dans l'ordre du vecteur "cnt"

# Colonne en type numérique
str(dataset_imf)
dataset_imf = mutate_all(dataset_imf, as.numeric)

# Passage en taux de croissance, lag=12
#dataset_imf = apply(dataset_imf, 2, function(x) diff(log(x), lag=12))
dataset = apply(dataset_imf, 2, function(x) growth_rate(x))
dataset = as.data.frame(dataset)
rownames(dataset) = rownames(dataset_imf)[13:nrow(dataset_imf)]


# --- Traitement des données
# Traitement des valeurs manquantes
colSums(is.na(dataset))
dataset = na.locf(dataset, na.rm=FALSE)
dataset = na.omit(dataset)


# ______________________________________________________________________________
#  ----------------------- Analyse des corrélations ----------------------------
# ______________________________________________________________________________
# --- Correlations
# Matrice de corrélations
cor_matrix = round(cor(dataset),2)

# Ordonner les pays avec un algorithme de clustering
col_order         =  hclust(dist(cor_matrix))$order 
dataset_reordered = dataset[, col_order]

# Visualiser les corrélations
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

# Significativité des corrélations (pvalue)
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

pvalues = round(calculate_pvalues(dataset),2)


# Graph des corrélations significatives
edge_colors = ifelse(cor_matrix > 0, "green", "red")
edge_colors[pvalues > 0.05 ] = "white"  # Liens de corrélations blanc pour les corrélations NS (seuil 5%)

qgraph(cor_matrix,
       labels = colnames(cor_matrix), # Label dans les bulles
       layout = "spring",             # Sortie sous forme de réseau
       edge.color = edge_colors       # Intégre la condition sur les liens NS
)


# --- Corrélations partielles
# Corrlations partielles
dataset_per = dataset[(as.Date(rownames(dataset)) >= "2000-01-01") & (as.Date(rownames(dataset)) <= "2024-01-01"),]
pcor_matrix = pcor(dataset_per, method = "pearson")$estimate

# Renomer les colonnes et lignes
colnames(pcor_matrix) = colnames(dataset_per)
rownames(pcor_matrix) = colnames(dataset_per)

# Significativité des corrélations partielles (tstat)
#Nécessite un nombre suffisant d'observations si beaucoup de variables de controle
N = nrow(dataset_per)
k = ncol(dataset_per)-2  # Nombre de variables de controle
t_stat_partial = pcor_matrix * sqrt(N - k - 2) / sqrt(1 - pcor_matrix^2)

# Fixe 0 dans le triangle supérieur pour ne pas avoir de doublons
pcor_matrix[lower.tri(pcor_matrix)] = 0

# Graph des corrélations partielles significatives
edge_colors = ifelse(pcor_matrix > 0, "green", "red")
edge_colors[(t_stat_partial < 1.96) & (t_stat_partial > -1.96) ] <- "white"  # 1.96 pour 5%, 2.5756 pour 1%

qgraph(pcor_matrix,
       labels     = colnames(pcor_matrix),
       layout     = "spring",
       edge.color = edge_colors,
       arrows     = FALSE)

# ______________________________________________________________________________
#  ----------------------- Datation des crises de change -----------------------
# ______________________________________________________________________________
# --- Datation
th1 = 25 # Dépréciation a/a de 25%
th2 = 10 # Taux de dépréciation 10 pps supérieur à l'année précédente si dépréciation il y a eu

# Initialisation des datasets
dataset_th2 = dataset_th1 = dataset

# Identification critère 2
dataset_th2 = apply(dataset_th2, 2, function(x) diff(x, lag=12))
dataset_th2 = as.data.frame(dataset_th2)
dataset_th2 = ifelse(dataset_th2 >= th2, 1, 0)
colSums(dataset_th2)

# Identification critère 1
dataset_th1 = ifelse(dataset_th1 >= th1, 1, 0)
colSums(dataset_th1)
dataset_th1 = dataset_th1[rownames(dataset_th1) %in% rownames(dataset_th2),]

# Combinaison des deux critères
condition = dataset_th1 == 1 & dataset_th2 == 1
dataset_th1[condition]  = 1
dataset_th1[!condition] = 0
dataset_th1 = data.frame(dataset_th1)

# --- Affichage des résultats
dataset_crisis = data.frame(rowSums(dataset_th1))
colnames(dataset_crisis) = "Value"
dataset_crisis$Date = as.Date(rownames(dataset_crisis))
dataset_crisis = dataset_crisis[,c(2,1)]

ggplot(dataset_crisis, aes(x = Date, y = Value, fill = Value)) +
  geom_bar(stat = "identity", color = "#454141") +
  theme_minimal() +
  labs(title = "Monthly currency crises", x = "Date", y = "Number of currency crises") +
  scale_fill_gradient(low = "#FFFFFF", high = "#C21A1A") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "24 month")


# Aggrégation annuelle
dataset_crisis_an      = dataset_crisis
dataset_crisis_an$Date = format(as.Date(dataset_crisis_an$Date), "%Y")
dataset_crisis_an      = aggregate(Value ~ Date, data = dataset_crisis_an, sum)

ggplot(dataset_crisis_an, aes(x = Date, y = Value, fill = Value)) +
  geom_bar(stat = "identity", color = "#454141") +
  theme_minimal() +
  labs(title = "Currency crises", x = "Date", y = "Number of currency crises") +
  scale_fill_gradient(low = "#FFFFFF", high = "#C21A1A") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# ______________________________________________________________________________
#  ------------------------ Corrélations et crises  ----------------------------
# ______________________________________________________________________________
# --- Evolution des corrélations
start_date = as.Date("2000-01-01")
end_date   = as.Date("2002-12-01")

dataset_cor   = data.frame(dataset[,1]) # Récupérer la structure du dataframe dataset
dataset_cor[] = NA                      # Affecter NA partout (rempli avec la boucle suivante)
colnames(dataset_cor) = "Cor"
rownames(dataset_cor) = rownames(dataset)

dataset_cor_pos = dataset_cor_neg = dataset_cor_abs = dataset_cor

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
  cor_sum      = sum(cor_matrix_per)     / ncol(cor_matrix)
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


# --- Corrélation vs datation des crises
xx = dataset_cor
xx = ts(xx, start=c(2000,01), freq=12)

yy = dataset_crisis
yy = ts(yy, start=c(2001,01), freq=12)

dataset_plot = cbind(xx, yy)
dataset_plot = dataset_plot[, c(1,3)]
colnames(dataset_plot) = c('Correlations', 'Crises')

plot(dataset_plot, 
     main = "Correlations during Currency Crises",
     xlab = "Time",
     ylab = "Values",
     col  = 1:2,  
     type = "l", 
     lty  = 1)  



# --- Graphique synthétique
xx = dataset_cor
xx = ts(xx, start=c(2000,01), freq=12)
plot(xx, 
     main = "Correlations during Financial Crises", 
     sub  = "2 years rolling window",
     xlab = "", 
     ylab = "Total correlations",
     ylim = c(0, 25))

# Périodes de crises majeures
periods <- list(
  GFC       = window(xx, start = c(2007, 8), end = c(2010, 4)),
  Sovereign = window(xx, start = c(2010, 5), end = c(2012, 12)),
  China     = window(xx, start = c(2015, 6), end = c(2016, 2)),
  Covid     = window(xx, start = c(2020, 3), end = c(2022, 1)),
  Ukraine   = window(xx, start = c(2022, 2), end = c(2023, 12))
)

# Ajout des crises sur le graph
abline_v_colors <- c("#6FB7DE", "#DED26F", "#6FDE72", "#DE6F6F", "#9E6FDE")
abline_lty <- 1

for (i in seq_along(periods)) {
  abline(v = as.numeric(index(periods[[i]])), col = abline_v_colors[i], lty = abline_lty)
}

# Ajout de la légende
legend("topleft", legend = names(periods), col = abline_v_colors, lty = abline_lty, bg = "transparent", cex=0.7)

lines(ts(dataset_cor_abs, start=c(2000,01), freq=12), col='red')
lines(ts(dataset_cor_pos, start=c(2000,01), freq=12), col='purple')
lines(ts(dataset_cor_neg, start=c(2000,01), freq=12), col='blue')


