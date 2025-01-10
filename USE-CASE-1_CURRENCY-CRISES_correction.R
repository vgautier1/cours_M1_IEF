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
library(qgraph)    # Graphiques en réseau
library(tidyr)     # Traitement des données
library(dplyr)     # Traitement des données
library(ggplot2)   # Graphiques
library(lubridate) # Traitement des dates


# - Si importation des données depuis le datalab
# https://github.com/taceconomics/taceconomics-r
library(taceconomics)
#taceconomics.apikey("YOUR API KEY")
taceconomics.apikey(Sys.getenv("TACECONOMICS_APIKEY"))

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


# --- Mode d'imporation numéro 1: API TAC Datalab
dataset = xts()

for (c in cnt){
  exr_cnt = taceconomics::getdata(paste0("IFS/ENDA_XDC_USD_RATE_M/", c, "?start_date=2000-01-01&collapse=M&transform=growth_yoy"))
  dataset = cbind(dataset, exr_cnt)
}

colnames(dataset) = cnt
dataset = data.frame(dataset)


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
edge_colors = ifelse(cor_matrix > 0, "darkgreen", "red")
edge_colors[pvalues > 0.05 ] = "white"  # Liens de corrélations blanc pour les corrélations NS (seuil 5%)

qgraph(cor_matrix,
       labels = colnames(cor_matrix), # Label dans les bulles
       layout = "spring",             # Sortie sous forme de réseau
       edge.color = edge_colors       # Intégre la condition sur les liens NS
       )


# --- Corrélations partielles
# Corrlations partielles
dataset_per = dataset[(as.Date(rownames(dataset)) >= "2000-01-01") & (as.Date(rownames(dataset)) <= "2025-01-01"),]
pcor_matrix = pcor(dataset_per, method = "pearson")$estimate

# Significativité des corrélations partielles (tstat)
#Nécessite un nombre suffisant d'observations si beaucoup de variables de controle
N = nrow(dataset_per)
k = ncol(dataset_per)-2  # Nombre de variables de controle
t_stat_partial = pcor_matrix * sqrt(N - k - 2) / sqrt(1 - pcor_matrix^2)

# Fixer 0 dans le triangle supérieur pour ne pas avoir de doublons
pcor_matrix[lower.tri(pcor_matrix)] = 0

# Graph des corrélations partielles significatives
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
# --- Datation
th1 = 25 # Dépréciation a/a de 25%
th2 = 10 # Taux de dépréciation 10 pps supérieur à l'année précédente si dépréciation il y a eu

# Initialisation des datasets
dataset_th2 = dataset_th1 = dataset

# Identification critère 1
dataset_th1 = ifelse(dataset_th1 >= th1, 1, 0)
colSums(dataset_th1)

# Identification critère 2
dataset_th2 = apply(dataset_th2, 2, function(x) diff(x, lag=12))
dataset_th2 = as.data.frame(dataset_th2)
dataset_th2 = ifelse(dataset_th2 >= th2, 1, 0)
colSums(dataset_th2)

# Garder le même nombre de lignes entre condition 1 et 2 dans le dataset (diff supprime les premières lignes)
dataset_th1 = dataset_th1[rownames(dataset_th1) %in% rownames(dataset_th2),]

# Combinaison des deux critères
condition = dataset_th1 == 1 & dataset_th2 == 1
dataset_th1[condition]  = 1
dataset_th1[!condition] = 0
dataset_th1 = data.frame(dataset_th1)

# --- Affichage des résultats
dataset_crisis = data.frame(rowSums(dataset_th1))
# plot(as.xts(dataset_crisis))
colnames(dataset_crisis) = "Value"
dataset_crisis$Date = as.Date(rownames(dataset_crisis))
dataset_crisis = dataset_crisis[,c(2,1)]

ggplot(dataset_crisis, aes(x = Date, y = Value, fill = Value)) +
  geom_bar(stat = "identity", color = "#454141") +
  theme_minimal() +
  labs(title = "Monthly currency crises", x = "Date", y = "Number of currency crises") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "24 month")


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


# --- Corrélations vs datation des crises
xx = dataset_cor
xx = ts(xx, start=c(2000,01), freq=12)

yy = dataset_crisis
yy = ts(yy, start=c(2001,01), freq=12)

plot(yy[,"Value"], 
     main = "Correlations during Currency Crises",
     xlab = "Time",
     ylab = "Values",
     col  = 1:2,  
     type = "l", 
     lty  = 1)  

lines(xx, col="red")

# --- Corrélations vs crises financières
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
  GFC       = window(xx, start = c(2007, 08), end = c(2009, 08)),  # Crise financière de 2008
  Sovereign = window(xx, start = c(2010, 04), end = c(2012, 07)),  # Crise de la dette européenne
  Taper     = window(xx, start = c(2013, 05), end = c(2013, 10)),  # Taper Tantrum
  China     = window(xx, start = c(2015, 06), end = c(2016, 02)),  # Krach boursier chinois
  Covid     = window(xx, start = c(2019, 12), end = c(2022, 04))  # Covid
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


