# Chargement des librairies ------------------------------------------------------------------

library(dplyr)
library(readxl)

# Importation des données --------------------------------------------------------------------

## OR

# On est en dollars

gold <- read.csv2("data/OR.csv", sep=",") 


## GPR

# GPR <- read_excel("data/GPR.xls")

## MSCI

MSCI <- read_excel("data/MSCI.xlsx")

## US DOLLAR INDEX

USDI <- read.csv2("data/US Dollar Index.csv", sep=",") 

## EPU

EPU <- read.csv2("data/GEPUCURRENT.csv", sep=",")

## DFII10

DFII10 <- read.csv2("data/DFII10.csv", sep=",")

## NFCI

NFCI <- read.csv2("data/NFCI.csv", sep=",")


# Nettoyage des données --------------------------------------------------------------------

# GPR <- GPR[, c("month", "GPR")]


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

gold$Dernier <- as.numeric(
  gsub(",", ".", gsub("\\.", "", gold$Dernier))
)

DFII10$DFII10 <- as.numeric(DFII10$DFII10)
EPU$GEPUCURRENT <- as.numeric(EPU$GEPUCURRENT)
NFCI$NFCI <- as.numeric(NFCI$NFCI)

# TRANSFORMATION EN DATE ET LES DATES

liste_autres <- list(DFII10, EPU, NFCI)

library(lubridate)

liste_autres <- lapply(liste_autres, function(data) {
  
  data <- data |>
    rename(Date = observation_date)
  
  data$Date <- as.Date(data$Date, format = "%Y-%m-%d")
  
  data <- data |>
    filter(
      (year(Date) > 2003 | (year(Date) == 2003 & month(Date) >= 1)) &
        (year(Date) < 2025 | (year(Date) == 2025 & month(Date) <= 12))
    )
  
  data
})




DFII10 <- liste_autres[[1]]
EPU <- liste_autres[[2]]
NFCI <- liste_autres[[3]]

MSCI$dates <- as.Date(
  paste0(
    substr(MSCI$dates, 1, 4), "-",                 # année
    sprintf("%02d", as.numeric(substr(MSCI$dates, 6, nchar(MSCI$dates)))), 
    "-01"
  )
)


gold <- gold |>
  filter(
    (year(Date) > 2003 | (year(Date) == 2003 & month(Date) >= 1)) &
      (year(Date) < 2025 | (year(Date) == 2025 & month(Date) <= 12))
  )

USDI <- USDI |>
  filter(
    (year(Date) > 2003 | (year(Date) == 2003 & month(Date) >= 1)) &
      (year(Date) < 2025 | (year(Date) == 2025 & month(Date) <= 12))
  )

MSCI <- MSCI |>
  filter(
    (year(dates) > 2003 | (year(dates) == 2003 & month(dates) >= 1)) &
      (year(dates) < 2025 | (year(dates) == 2025 & month(dates) <= 12))
  )


NFCI <- NFCI |>
  mutate(YearMonth = floor_date(Date, "month")) |>
  group_by(YearMonth) |>
  summarise(
    NFCI = mean(NFCI, na.rm = TRUE),
    .groups = "drop"
  )


# Test de stationnarité : --------------------------------------------------------
library(tseries)   # adf.test, kpss.test

# DFII10

# ADF
adf.test(DFII10$DFII10) 
# 0.7348 > 0.05 La série n’est pas stationnaire en niveau

# KPSS
kpss.test(DFII10$DFII10)
# <0.05 La série n’est pas stationnaire en niveau

#####################
# Gold
#####################

adf.test(gold$Dernier) 
# 0.06297 > 0.05 La série n’est pas stationnaire en niveau à 5%


kpss.test(gold$Dernier)
# <0.05 La série n’est pas stationnaire en niveau

#####################
# EPU
#####################

adf.test(EPU$GEPUCURRENT) 
# 0.01929 < 0.05 La série est  stationnaire à 5%  !!!!


kpss.test(EPU$GEPUCURRENT)
# <0.05 La série n’est pas stationnaire en niveau

#####################
# USDI
#####################

adf.test(USDI$Dernier) 
# 0.5932 > 0.05 La série n’est pas stationnaire en niveau


kpss.test(USDI$Dernier)
# <0.05 La série n’est pas stationnaire en niveau

#####################
# NFCI
#####################

adf.test(NFCI$NFCI) 
#  0.1886 > 0.05 La série n’est pas stationnaire en niveau


kpss.test(NFCI$NFCI)
# 0.1 > 0.05 La série est stationnaire en niveau !!!!

#####################
# MSCI
#####################

adf.test(MSCI$MSCI) 
#  0.9855 > 0.05 La série n’est pas stationnaire en niveau


kpss.test(MSCI$MSCI)
# <0.05 La série n’est pas stationnaire en niveau



# Test de saisonnalité : --------------------------------------------------------


liste_des_series <- list(
  DFII10 = DFII10$DFII10,
  GOLD  = gold$Dernier,
  EPU   = EPU$GEPUCURRENT,
  USDI  = USDI$Dernier,
  NFCI  = NFCI$NFCI,
  MSCI  = MSCI$MSCI
)


liste_des_series <- lapply(liste_des_series, function(x) ts(as.numeric(x), frequency = 12)) # 12 = mensuel

library(TSA)

for (nm in names(liste_des_series)) {
  
  s <- liste_des_series[[nm]]  # Récupère le nom du dataframe (string)

  cat("\n=============================\n")
  cat("Série :", nm, "\n")
  cat("=============================\n")
  
  saison <- cycle(s)

  print(
    tryCatch(
      kruskal.test(as.numeric(s) ~ factor(saison)),
      error = function(e) e
    )
  )
  
  # Différence première + périodogramme
  d1 <- diff(s, differences = 1)
  
  if (length(na.omit(d1)) > 10) {
    periodogram(d1, main = paste("Périodogramme (diff. 1ère) :", nm))
  } else {
    cat("Différence première trop courte -> périodogramme ignoré.\n")
  }
  
  
}

















