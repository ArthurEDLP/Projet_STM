# Chargement des librairies ------------------------------------------------------------------

library(dplyr)
library(readxl)
library(ggplot2)
library(tidyr)
library(lubridate)
library(TSA)

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


# Illustration des series : --------------------------------------------------------


 # Mise à la mème freq pour comparer 



 # --- GOLD mensuel (dernier du mois) ---
 gold_m <- gold %>%
  mutate(YearMonth = floor_date(Date, "month")) %>%
  group_by(YearMonth) %>%
  summarise(GOLD = last(Dernier), .groups = "drop")

 # --- USDI mensuel (dernier du mois) ---
 USDI_m <- USDI %>%
  mutate(YearMonth = floor_date(Date, "month")) %>%
  group_by(YearMonth) %>%
  summarise(USDI = last(Dernier), .groups = "drop")

 # --- DFII10/EPU mensuel (moyenne du mois si daily ; sinon ça ne change rien) ---
 DFII10_m <- DFII10 %>%
  mutate(YearMonth = floor_date(Date, "month")) %>%
  group_by(YearMonth) %>%
  summarise(DFII10 = mean(DFII10, na.rm = TRUE), .groups = "drop")

 EPU_m <- EPU %>%
  mutate(YearMonth = floor_date(Date, "month")) %>%
  group_by(YearMonth) %>%
  summarise(EPU = mean(GEPUCURRENT, na.rm = TRUE), .groups = "drop")

 # --- NFCI : tu l'as déjà agrégé, je renomme propre ---
 NFCI_m <- NFCI %>%
  rename(YearMonth = YearMonth)

 # --- MSCI mensuel (déjà mensuel) ---
 MSCI_m <- MSCI %>%
  transmute(YearMonth = dates, MSCI = MSCI)

 # --- Fusion ---
 df_all <- gold_m %>%
  full_join(USDI_m,  by = "YearMonth") %>%
  full_join(DFII10_m, by = "YearMonth") %>%
  full_join(EPU_m,   by = "YearMonth") %>%
  full_join(NFCI_m,  by = "YearMonth") %>%
  full_join(MSCI_m,  by = "YearMonth") %>%
  arrange(YearMonth)

 str(df_all)
 summary(df_all)
 
 #Création des graph
 
 df_long_level <- df_all %>%
   pivot_longer(cols = -YearMonth, names_to = "Serie", values_to = "Valeur")
 
 ggplot(df_long_level, aes(x = YearMonth, y = Valeur)) +
   geom_line(na.rm = TRUE) +
   facet_wrap(~ Serie, scales = "free_y", ncol = 2) +
   labs(title = "Séries en niveau (mensuel)", x = "Date", y = "") +
   theme_minimal()
 
 #Construction des variations
 # Helpers
 log_return_pct <- function(x) 100 * (log(x) - log(dplyr::lag(x)))  # % log-return
 diff_simple    <- function(x) x - dplyr::lag(x)                    # diff
 
 df_var <- df_all %>%
   arrange(YearMonth) %>%
   mutate(
     # Prix/indices -> log-returns %
     dGOLD = log_return_pct(GOLD),
     dUSDI = log_return_pct(USDI),
     dMSCI = log_return_pct(MSCI),
     
     # Indices/taux -> différences
     dDFII10 = diff_simple(DFII10), 
     dEPU    = diff_simple(EPU),
     dNFCI   = diff_simple(NFCI)
   ) %>%
   select(YearMonth, dGOLD, dUSDI, dMSCI, dDFII10, dEPU, dNFCI) %>%
   filter(!is.na(dGOLD) | !is.na(dUSDI) | !is.na(dMSCI) | !is.na(dDFII10) | !is.na(dEPU) | !is.na(dNFCI))
 
 str(df_var)
 summary(df_var)
 
 
 #Graphiques des séries en variation

 
 df_long_var <- df_var %>%
   pivot_longer(cols = -YearMonth, names_to = "Serie", values_to = "Variation")
 
 ggplot(df_long_var, aes(x = YearMonth, y = Variation)) +
   geom_hline(yintercept = 0, linetype = "dashed") +
   geom_line(na.rm = TRUE) +
   facet_wrap(~ Serie, scales = "free_y", ncol = 2) +
   labs(title = "Séries en variation (mensuel)", x = "Date", y = "") +
   theme_minimal()
 
 
 
 # Valeurs Aberrantes : --------------------------------------------------------
 
 
 #Visualisation des valeurs aberrantes (boxplots

 
 df_long_var <- df_var %>%
   pivot_longer(cols = -YearMonth, names_to = "Serie", values_to = "Variation")
 
 ggplot(df_long_var, aes(x = Serie, y = Variation)) +
   geom_boxplot(outlier.colour = "red", outlier.alpha = 0.6) +
   coord_flip() +
   labs(
     title = "Détection visuelle des valeurs aberrantes (variations)",
     x = "",
     y = "Variation"
   ) +
   theme_minimal()
 
 #Détection statistique par méthode IQR (robuste)
 detect_outliers_iqr <- function(x) {
   q1 <- quantile(x, 0.25, na.rm = TRUE)
   q3 <- quantile(x, 0.75, na.rm = TRUE)
   iqr <- q3 - q1
   (x < (q1 - 1.5 * iqr)) | (x > (q3 + 1.5 * iqr))
 }
 
 outliers_iqr <- df_var %>%
   summarise(across(
     -YearMonth,
     ~ sum(detect_outliers_iqr(.x), na.rm = TRUE),
     .names = "outliers_{.col}"
   ))
 
 outliers_iqr
 
 #Détection par z-score robuste (MAD) : Plus fiable que le z-score classique.
 zscore_robust <- function(x) {
   med <- median(x, na.rm = TRUE)
   mad_val <- mad(x, na.rm = TRUE)
   (x - med) / mad_val
 }
 
 outliers_mad <- df_var %>%
   summarise(across(
     -YearMonth,
     ~ sum(abs(zscore_robust(.x)) > 3, na.rm = TRUE),
     .names = "outliers_{.col}"
   ))
 
 outliers_mad
 
 















