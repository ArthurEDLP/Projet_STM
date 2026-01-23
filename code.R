# Chargement des librairies ------------------------------------------------------------------

library(dplyr)
library(readxl)

# Importation des données --------------------------------------------------------------------

## OR

# On est en dollars

gold <- read.csv2("data/OR.csv", sep=",") 


## GPR

GPR <- read_excel("data/GPR.xls")

GPR <- GPR[, c("month", "GPR")]


## MSCI

MSCI <- read_excel("data/MSCI.xlsx")

## US DOLLAR INDEX

USDI <- read.csv2("data/US Dollar Index.csv", sep=",") 

# Nettoyage des données --------------------------------------------------------------------


liste_data <- list(gold, USDI)

liste_data <- lapply(liste_data, function(data) {
  
  data[c("Ouv.", "X.Plus.Haut", "Plus.Bas")] <- NULL
  
  data <- as.data.frame(data)
  
  data$Date <- as.Date(data$Date, format = "%d/%m/%Y")
  
  
  data |>
    rename(
      Volume = `Vol.`,
      Variation_pct = `Variation..`
    )
  
})

gold <- liste_data[[1]]
USDI <- liste_data[[2]]

str(gold)
str(USDI)

# on enlève le "%"

gold$Variation_pct <- as.numeric(
  gsub("%", "", gsub(",", ".", gold$Variation_pct))
)


USDI$Variation_pct <- as.numeric(
  gsub("%", "", gsub(",", ".", USDI$Variation_pct))
)

# on transforme les M en millions et k en milliers

gold$Volume <- ifelse(
  grepl("M", gold$Volume),
  as.numeric(gsub(",", ".", gsub("M", "", gold$Volume))) * 1e6,
  ifelse(
    grepl("K", gold$Volume),
    as.numeric(gsub(",", ".", gsub("K", "", gold$Volume))) * 1e3,
    NA
  )
)


USDI$Volume <- ifelse(
  grepl("M", USDI$Volume),
  as.numeric(gsub(",", ".", gsub("M", "", USDI$Volume))) * 1e6,
  ifelse(
    grepl("K", USDI$Volume),
    as.numeric(gsub(",", ".", gsub("K", "", USDI$Volume))) * 1e3,
    NA
  )
)