# --- Fixer le répertoire automatiquement
path           = rstudioapi::getActiveDocumentContext()$path
Encoding(path) = "UTF-8"
setwd(dirname(path))


# ______________________________________________________________________________
#  --------------------------------- Packages  ---------------------------------
# ______________________________________________________________________________
library(dplyr)
library(xlsx)
library(taceconomics)
taceconomics.apikey(YOUR_APIKEY) # A MODIFIER

# - Fixer la graine
set.seed(123)


# ______________________________________________________________________________
#  ------------------------ Importation des données  ---------------------------
# ______________________________________________________________________________
# --- Pays sur lesquels la boucle d'importation va itérer
cnt = c('BRA', 'MEX', 'CHN', 'IDN', 'IND', 'TUR', 'RUS', 'ZAF') #'CHN', 'KOR', 'POL', 


# --- Initialisation d'un dataset vide (rempli par la boucle d'importation)
dataset           = data.frame(matrix(nrow=0, ncol=21))
colnames(dataset) = c('country', 'date',
                      'tgdp', 'unemp', 'infl',
                      'tgov_debt', 'gov_debt_gdp', 'ext_debt_ratio',
                      'tot', 'res_imp',
                      'npl', 'cred_gdp',
                      'gini', 'literacy', 'power_gen', #education',
                      'pol_stab', 'voice', 'gov_eff', 'reg_qual', 'rule_law', 'corrup')


# --- Boucle d'importation
for (c in cnt){
  # --- Importation des données
  # Pilier 1: Stabilité macroéconomique
  
  
  # Pilier 2: Stabilité de la dette
  
  
  # Pilier 3: Vulnérabilité du secteur externe
  
  
  # Pilier 4: Risque bancaire
  
  
  # Pilier 5: Social resilience
  
  
  # Pilier 6: Risque politique et de gouvernance
  
  
  # --- Orientation des indicateurs (augmentation <=> augmentation du risque)

  
  # --- Mise en forme du dataset de sortie
  # - Création d'un dataset unique

  
  # - Filtre sur la période 2000-2024 + inclure la colonne date

  
  # - Nommage des colonnes
  

  # -Traitement des NA (remplacer les NA au mileu et fin dataset)

  
  # --- Combiner les pays dans un dataset unique
  dataset = rbind(dataset, df)
}


# ______________________________________________________________________________
#  ---------------------------- Mise à l'échelle  ------------------------------
# ______________________________________________________________________________
# --- Fonction normalisation min-max


# --- Application de la fonction sur les colonnes concernées de "dataset" (toutes "country" et "date")


# ______________________________________________________________________________
#  ---------------- Scoring (équipondéré intra et inter piliers)  --------------
# ______________________________________________________________________________
# --- Pilier 1: Stabilité macroéconomique
w1     = ...
score1 = ...

# --- Pilier 2: Stabilité de la dette
w2     = ...
score2 = ...

# --- Pilier 3: Vulnérabilité du secteur externe
w3     = ...
score3 = ...

# --- Pilier 4: Risque bancaire
w4     = ...
score4 = ...

# --- Pilier 5: Social resilience
w5     = ...
score5 = ...

# --- Pilier 6: Risque politique et de gouvernance
w6     = ...
score6 = ...


# --- Dataset final (combiner l'ID country, la date et les 6 scores dans un dataset)


# --- Extraire le dataset dans excel
