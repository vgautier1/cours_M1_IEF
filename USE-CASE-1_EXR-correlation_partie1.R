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
#library(urca)     # Test racine unitaire
#library(vars)     # Modèles VAR
#library(forecast) # Prévisions
# library(moments) # Moyenne, médianne...
# library(tseries) # Format séries temporelles
# library(dyn)     # Régressions



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

# cnt = c('CAN','JPN','KOR','TWN','SGP','CZE','GBR','ISL','NOR','SWE',
#         'CHE','EUZ','AUS','NZL','DZA','EGY','LBY','MAR','TUN','MUS',
#         'AGO','BWA','ZAF','DOM','JAM','CRI','GTM','MEX','ARG','BRA',
#         'CHL','COL','GUY','PER','SUR','URY','KAZ','CHN','MNG','IDN',
#         'PHL','THA','IND','PAK','ARM','AZE','GEO','KWT','TUR','BLR',
#         'POL','ROU','RUS','UKR','ALB','MKD','FJI','TON','BDI','KEN',
#         'MDG','MOZ','RWA','UGA','GHA','GMB','LBR','NGA','NIC','KGZ',
#         'TJK','KHM','LAO','BGD','MDA') # classement par région puis niveau de dev

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

# --- Mode d'imporation numéro 1: API TAC Datalab
dataset = xts()

for (c in cnt){
  exr_cnt = taceconomics::getdata(paste0("IFS/ENDA_XDC_USD_RATE_M/", c, "?start_date=1999-01-01&frequency=M'"))
  exr_cnt = growth_rate(exr_cnt)
  #exr_cnt = diff(log(exr_cnt), lag=12)
  
  dataset = cbind(dataset, exr_cnt)
}

colnames(dataset) = cnt
dataset = data.frame(dataset)


# # --- Mode d'imporation numéro 2: IMF excel
# #- Importation excel
# #https://data.imf.org/?sk=4c514d48-b6ba-49ed-8ab9-52b0c1a0179b&sId=-1
# #dataset_imf = read.xlsx('USE-CASE-1_EXR_IFS.xlsx', 1)               #library(xlsx)
# dataset_imf  = data.frame(read_excel('USE-CASE-1_EXR_IFS.xlsx', 1)) #library(readxl))
# 
# # - Mise en forme du dataset
# # Ordre alphabétique des codes ISO
# # dataset_imf = dataset_imf[order(dataset_imf$ISO), ]
# 
# # Suppression des colonnes inutiles
# columns_to_remove = c("Country.Name", "Country.Code", "Indicator.Name", "Indicator.Code", "Attribute", "Base.Year")
# dataset_imf = dataset_imf[, !(names(dataset_imf) %in% columns_to_remove)]
# 
# # Suppression de la première colonne
# dataset_imf = data.frame(t(dataset_imf))
# colnames(dataset_imf) = dataset_imf[1, ]
# dataset_imf = dataset_imf[-1, ]
# 
# # Transformation des rownames en date
# start_date = as.Date("1999-01-01")
# num_date   = nrow(dataset_imf)
# monthly_dates = seq.Date(from = start_date, by = "months", length.out = num_date)
# rownames(dataset_imf) = monthly_dates
# 
# # Conserver seulement les pays qui nous intéressent (liste c)
# dataset_imf = dataset_imf[, names(dataset_imf) %in% cnt]
# dataset_imf = dataset_imf[,cnt] #dans l'ordre du vecteur "cnt"
# 
# # Colonne en type numérique
# str(dataset_imf)
# dataset_imf = mutate_all(dataset_imf, as.numeric)
# 
# # Passage en taux de croissance, lag=12
# #dataset_imf = apply(dataset_imf, 2, function(x) diff(log(x), lag=12))
# dataset = apply(dataset_imf, 2, function(x) growth_rate(x))
# dataset = as.data.frame(dataset)
# rownames(dataset) = rownames(dataset_imf)[13:nrow(dataset_imf)]


# --- Traitement des données
# Traitement des valeurs manquantes
colSums(is.na(dataset))
dataset = na.locf(dataset, na.rm=FALSE)
dataset = na.omit(dataset)


# # Détection des outliers
# find_outliers <- function(x, threshold = 3) {
#   z_scores     = scale(x)
#   abs_z_scores = abs(z_scores)
#   return(which(abs_z_scores > threshold))
# }
# outliers_list = lapply(dataset, find_outliers)


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
