# ============================================================
# VECM : Gold "valeur refuge" face aux chocs d'incertitude (EPU/NFCI)
# Endogènes I(1) : log(Gold), log(USDI), DFII10, log(MSCI)
# Exogènes I(0)  : EPU, NFCI (mensuel), + option trend/const à choisir
# Période : janv 2003 -> déc 2025 (si dispo)
# ============================================================

# Packages ---------------------------------------------------
library(dplyr)
library(lubridate)
library(zoo)
library(vars)
library(urca)
library(tsDyn)

# ------------------------------------------------------------
# 0) Harmoniser les dates (mensuel) + préparer séries
#    Hypothèses sur tes objets :
#    gold: Date, Dernier
#    USDI: Date, Dernier
#    DFII10: Date, DFII10
#    MSCI: dates, MSCI
#    EPU: Date, GEPUCURRENT
#    NFCI: YearMonth, NFCI (déjà agrégé en mensuel dans ton code)
# ------------------------------------------------------------

# Helper : passer toute date en début de mois
to_month <- function(x) floor_date(as.Date(x), "month")

gold_m <- gold %>%
  mutate(Date = to_month(Date)) %>%
  group_by(Date) %>%                            # si tu as du quotidien, sinon ça ne change rien
  summarise(GOLD = mean(Dernier, na.rm = TRUE), .groups = "drop")

usdi_m <- USDI %>%
  mutate(Date = to_month(Date)) %>%
  group_by(Date) %>%
  summarise(USDI = mean(Dernier, na.rm = TRUE), .groups = "drop")

dfii_m <- DFII10 %>%
  mutate(Date = to_month(Date)) %>%
  group_by(Date) %>%
  summarise(DFII10 = mean(DFII10, na.rm = TRUE), .groups = "drop")

msci_m <- MSCI %>%
  rename(Date = dates) %>%
  mutate(Date = to_month(Date)) %>%
  group_by(Date) %>%
  summarise(MSCI = mean(MSCI, na.rm = TRUE), .groups = "drop")

epu_m <- EPU %>%
  mutate(Date = to_month(Date)) %>%
  group_by(Date) %>%
  summarise(EPU = mean(GEPUCURRENT, na.rm = TRUE), .groups = "drop")

nfci_m <- NFCI %>%
  rename(Date = YearMonth) %>%                  # tu avais YearMonth comme date mensuelle
  mutate(Date = to_month(Date)) %>%
  group_by(Date) %>%
  summarise(NFCI = mean(NFCI, na.rm = TRUE), .groups = "drop")

# ------------------------------------------------------------
# 1) Fusion sur l'intersection des dates (important pour VECM)
# ------------------------------------------------------------
df_all <- gold_m %>%
  inner_join(usdi_m, by = "Date") %>%
  inner_join(dfii_m, by = "Date") %>%
  inner_join(msci_m, by = "Date") %>%
  inner_join(epu_m,  by = "Date") %>%
  inner_join(nfci_m, by = "Date") %>%
  filter(Date >= as.Date("2003-01-01"),
         Date <= as.Date("2025-12-01")) %>%
  arrange(Date)

# Check rapides
stopifnot(all(!is.na(df_all$Date)))
# Si tu veux voir la couverture :
print(range(df_all$Date))
print(nrow(df_all))

# ------------------------------------------------------------
# 2) Transformations : logs sur prix/indices strictement positifs
# ------------------------------------------------------------
df_all <- df_all %>%
  mutate(
    lGOLD = log(GOLD),
    lUSDI = log(USDI),
    lMSCI = log(MSCI)
  )

# Endogènes (I(1)) en niveau
Y <- df_all %>%
  dplyr::select(lGOLD, lUSDI, DFII10, lMSCI)

# Exogènes stationnaires (I(0)) : EPU + NFCI
Xexo <- df_all %>%
  dplyr::select(EPU, NFCI)


# ------------------------------------------------------------
# 3) Choix du nombre de retards VAR (p) sur Y
#    Pour ca.jo : on utilisera K = p + 1 (VAR en niveau)
# ------------------------------------------------------------
lag_sel <- VARselect(Y, lag.max = 12, type = "const")
print(lag_sel$selection)

# Choix pratique : prendre le p suggéré par AIC (ou SC), ici AIC
p <- lag_sel$selection[["AIC(n)"]]
if (is.na(p)) p <- 2
cat("p choisi =", p, "\n")

# ------------------------------------------------------------
# 4) Test de cointégration de Johansen (urca)
#    ecdet: "const" ou "trend" (souvent "const" en macro-finance)
#    K = p + 1
# ------------------------------------------------------------
K <- p + 1

jo_trace <- ca.jo(as.matrix(Y),
                  type  = "trace",
                  ecdet = "const",
                  K     = K,
                  spec  = "transitory")
summary(jo_trace)

jo_eigen <- ca.jo(as.matrix(Y),
                  type  = "eigen",
                  ecdet = "const",
                  K     = K,
                  spec  = "transitory")
summary(jo_eigen)

# ------------------------------------------------------------
# 5) Choisir le rang r (à décider à partir des summaries ci-dessus)
# ------------------------------------------------------------

"
 test          10pct  5pct  1pct
r <= 3 |  5.03  7.52  9.24 12.97
r <= 2 |  9.77 13.75 15.67 20.20
r <= 1 | 16.91 19.77 22.00 26.81
r = 0  | 47.58 25.56 28.14 33.24
"

r <- 1

# ------------------------------------------------------------
# 6) Estimation VECM avec tsDyn en incluant exogènes (EPU, NFCI)
#    Attention : dans tsDyn, lag = nombre de retards en Δ (p)
# ------------------------------------------------------------
vecm_tsdyn <- VECM(
  data    = as.matrix(Y),
  lag     = p,
  r       = r,
  estim   = "ML",
  include = "const",     # constante hors/avec espace selon ta logique
  exogen  = as.matrix(Xexo)
)

summary(vecm_tsdyn)

# ------------------------------------------------------------
# 7) Passer en VAR (pour IRF/FEVD) via urca::vec2var
#    vec2var travaille avec l'objet ca.jo (pas tsDyn).
#    Donc on estime aussi le VECM "urca" pour l'analyse dynamique.
# ------------------------------------------------------------
vecm_urca_r <- cajorls(jo_trace, r = r)  # coefficients VECM (restricted LS)
print(summary(vecm_urca_r$rlm))

var_level <- vec2var(jo_trace, r = r)

# Diagnostics VAR en niveau (utile avant IRF)
serial.test(var_level, lags.pt = 16, type = "PT.asymptotic")
arch.test(var_level, lags.multi = 5)
normality.test(var_level)

# ------------------------------------------------------------
# 8) IRF : "valeur refuge" -> choc d'incertitude et réponse de l'or
#    NOTE : vec2var ici ne contient pas directement EPU/NFCI en exogènes.
#    Deux options :
#      A) faire IRF sur chocs internes (USDI, DFII10, MSCI) -> proxy
#      B) estimer un VARX/VECMX structurel via tsDyn (plus avancé)
#    On te donne A) immédiatement + une B) simple avec tsDyn::irf
# ------------------------------------------------------------

# A) IRF internes (ex: choc USDI -> réponse lGOLD)
ir_gold_usdi <- irf(var_level,
                    impulse = "lUSDI",
                    response = "lGOLD",
                    n.ahead = 36,
                    boot = TRUE,
                    runs = 500)
plot(ir_gold_usdi)

# A) IRF internes (ex: choc DFII10 -> réponse lGOLD)
ir_gold_dfii <- irf(var_level,
                    impulse = "DFII10",
                    response = "lGOLD",
                    n.ahead = 36,
                    boot = TRUE,
                    runs = 500)
plot(ir_gold_dfii)
