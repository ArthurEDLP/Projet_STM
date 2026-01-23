
### Chargement des librairies ------------------------------------------------------------------

library(dplyr)
library(readxl)

### Importation des données --------------------------------------------------------------------

## OR

# On est en dollars

gold <- read.csv2("data/OR.csv", sep=",") 

gold[c("Ouv.", "X.Plus.Haut", "Plus.Bas")] <- NULL

gold <- as.data.frame(gold)

gold <- gold |>
  rename(
    Volume = `Vol.`,
    Variation_pct = `Variation..`
  )

## GPR

GPR <- read_excel("data/GPR.xls")


## MSCI

MSCI <- read_excel("data/MSCI.xlsx")
