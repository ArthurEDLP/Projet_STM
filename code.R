### ------------------------------------------------------------------
# Chargement des librairies

library(dplyr)

### ------------------------------------------------------------------
# Importation des données

# On est en dollars

gold <- read.csv2("data/donnees_mensuel_or_90_25.csv", sep=",") 

gold[c("Ouv.", "X.Plus.Haut", "Plus.Bas")] <- NULL

gold <- as.data.frame(gold)

gold <- gold |>
  rename(
    Volume = `Vol.`,
    Variation_pct = `Variation..`
  )



