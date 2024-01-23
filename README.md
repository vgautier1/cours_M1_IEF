**1) Calcul des corrélations et corrélations partielles + graphiques associés**  

**2) Datation des crises de change à l’aide du critère de Frankel et Rose**  
- Définir à l’aide d’un graphique les périodes de crise majeures
  
**3) Confrontation des corrélations et des périodes de crises majeures**  
- Graphiques impliquant la somme des corrélations absolues et les crises: quel constat? (même chose avec seulement les corrélations positives et seulement négatives)
   
**4) Construction d’un DAG (directed acyclic graph)**  
- corrélation n’est pas causalité. Comparer les graphs pour différentes périodes

***Packages utilisés***
- library(pcalg)     # Directe Acyclic Graph (DAG)
- library(gplots)    # Graphiques
- library(igraph)    # Graph des DAG
- library(corpcor)   # Corrélations partielles
- library(ppcor)     # Corrélations partielles 
- library(qgraph)    # Graphique en réseau
- library(tidyr)     # Traitement des données
- library(dplyr)     # Traitement des données
- library(ggplot2)   # Graphiques
- library(lubridate) # Traitement des dates
- library(xlsx)      # Importation Excel

**Installation du package *pcalg***  
- install.packages("BiocManager")  
- BiocManager::install("graph")  
- BiocManager::install("RBGL")  
- BiocManager::install("Rgraphviz")  

***Code pays***
cnt = c('NZL','FJI','TON','AUS','CAN','DOM','JAM','CRI','GTM','MEX',
        'NIC','ARG','BRA','CHL','COL','GUY','PER','PRY','SUR','URY',
        'KAZ','KGZ','TJK','JPN','KOR','TWN','CHN','MNG','SGP','IDN',
        'PHL','THA','KHM','LAO','IND','PAK','BGD','ARM','AZE','GEO',
        'KWT','TUR','BLR','POL','ROU','RUS','UKR','MDA','GBR','ISL',
        'NOR','SWE','ALB','CHE','EUZ','DZA','EGY','LBY','MUS','BDI',
        'KEN','MDG','MOZ','RWA','UGA','AGO','ZAF','GHA','GMB','LBR',
        'NGA') # classement par niveau de dev puis région


dataset = xts()

for (c in cnt){
  exr_cnt = taceconomics::getdata(paste0("IFS/ENDA_XDC_USD_RATE_M/", c, "?start_date=1999-01-01&frequency=M'"))
  exr_cnt = growth_rate(exr_cnt)
 #exr_cnt = diff(log(exr_cnt), lag=12)

  dataset = cbind(dataset, exr_cnt)
}

colnames(dataset) = cnt
dataset = data.frame(dataset)


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


