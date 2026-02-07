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

# Passer toute date en début de mois
to_month <- function(x) floor_date(as.Date(x), "month")  

gold_m <- gold |>
  mutate(Date = to_month(Date)) |>
  group_by(Date) |>                            
  summarise(GOLD = mean(Dernier, na.rm = TRUE), .groups = "drop")

usdi_m <- USDI |>
  mutate(Date = to_month(Date)) |>
  group_by(Date) |>
  summarise(USDI = mean(Dernier, na.rm = TRUE), .groups = "drop")

dfii_m <- DFII10 |>
  mutate(Date = to_month(Date)) |>
  group_by(Date) |>
  summarise(DFII10 = mean(DFII10, na.rm = TRUE), .groups = "drop")

msci_m <- MSCI |>
  rename(Date = dates) |>
  mutate(Date = to_month(Date)) |>
  group_by(Date) |>
  summarise(MSCI = mean(MSCI, na.rm = TRUE), .groups = "drop")

epu_m <- EPU |>
  mutate(Date = to_month(Date)) |>
  group_by(Date) |>
  summarise(EPU = mean(GEPUCURRENT, na.rm = TRUE), .groups = "drop")

nfci_m <- NFCI |>
  rename(Date = YearMonth) |>                  # on change YearMonth pour avoir date mensuelle
  mutate(Date = to_month(Date)) |>
  group_by(Date) |>
  summarise(NFCI = mean(NFCI, na.rm = TRUE), .groups = "drop")

# ------------------------------------------------------------
# 1) Fusion sur l'intersection des dates (important pour VECM)
# ------------------------------------------------------------
df_all <- gold_m |>
  inner_join(usdi_m, by = "Date") |>
  inner_join(dfii_m, by = "Date") |>
  inner_join(msci_m, by = "Date") |>
  inner_join(epu_m,  by = "Date") |>
  inner_join(nfci_m, by = "Date") |>
  filter(Date >= as.Date("2003-01-01"),
         Date <= as.Date("2025-12-01")) |>
  arrange(Date)

# Check rapides
stopifnot(all(!is.na(df_all$Date)))
# Pour voir la couverture :
print(range(df_all$Date))
print(nrow(df_all))

# ------------------------------------------------------------
# 2) Transformations : logs sur prix/indices strictement positifs
# ------------------------------------------------------------
df_all <- df_all |>
  mutate(
    lGOLD = log(GOLD),
    lUSDI = log(USDI),
    lMSCI = log(MSCI)
  )

# Endogènes (I(1)) en niveau
Y <- df_all |>
  dplyr::select(lGOLD, lUSDI, DFII10, lMSCI)

# Exogènes stationnaires (I(0)) : EPU + NFCI
Xexo <- df_all |>
  dplyr::select(EPU, NFCI)


# ------------------------------------------------------------
# 3) Choix du nombre de retards VAR (p) sur Y
#    Pour ca.jo : on utilisera K = p + 1 (VAR en niveau)
# ------------------------------------------------------------
lag_sel <- VARselect(Y, lag.max = 12, type = "const")
print(lag_sel$selection)

# Prendre le p suggéré par AIC, ici AIC
p <- lag_sel$selection[["AIC(n)"]]
if (is.na(p)) p <- 2
cat("p choisi =", p, "\n")

# ------------------------------------------------------------
# 4) Test de cointégration de Johansen (urca)
#    ecdet: "const" ou "trend" 
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
r = 0  | 47.58 25.56 28.14 33.24   rejet à 10%
"

r <- 1

# ------------------------------------------------------------
# 5.5) Restrictions linéaires : alpha_USDI = 0
# Teste l’exogénéité faible d’une variable, 
# ------------------------------------------------------------

# Matrice de restriction sur alpha
# 1 ligne par variable, 1 colonne par relation de cointégration (r = 1)

# lGOLD
A_gold <- matrix(c(1,0,0,0), nrow = 4)
test_gold <- alrtest(jo_trace, A = A_gold, r = r)

# lUSDI
A_usdi <- matrix(c(0,1,0,0), nrow = 4)
test_usdi <- alrtest(jo_trace, A = A_usdi, r = r)

# DFII10
A_dfii <- matrix(c(0,0,1,0), nrow = 4)
test_dfii <- alrtest(jo_trace, A = A_dfii, r = r)

# lMSCI
A_msci <- matrix(c(0,0,0,1), nrow = 4)
test_msci <- alrtest(jo_trace, A = A_msci, r = r)

## les stats:

res_alpha <- data.frame(
  Variable = c("lGOLD", "lUSDI", "DFII10", "lMSCI"),
  Statistique = c(
    test_gold@teststat,
    test_usdi@teststat,
    test_dfii@teststat,
    test_msci@teststat
  ),
  p_value = c(
    test_gold@pval,
    test_usdi@pval,
    test_dfii@pval,
    test_msci@pval
  )
)

res_alpha$Conclusion <- ifelse(
  res_alpha$p_val < 0.05,
  "Endogène",
  "Faiblement exogène"
)

print(res_alpha)



# ------------------------------------------------------------
# 6.0) Choix de la spécification déterministe du VECM
# ------------------------------------------------------------


suppressWarnings({
  C1 <- VECM(as.matrix(Y), lag = p, r = r, estim = "ML",
             include = "none", exogen = as.matrix(Xexo))
})

suppressWarnings({
  C2 <- VECM(as.matrix(Y), lag = p, r = r, estim = "ML",
             LRinclude = "const", exogen = as.matrix(Xexo))
})

suppressWarnings({
  C3 <- VECM(as.matrix(Y), lag = p, r = r, estim = "ML",
             include = "const", exogen = as.matrix(Xexo))
})

suppressWarnings({
  C4 <- VECM(as.matrix(Y), lag = p, r = r, estim = "ML",
             LRinclude = "trend", exogen = as.matrix(Xexo))
})

suppressWarnings({
  C5 <- VECM(as.matrix(Y), lag = p, r = r, estim = "ML",
             include = "both", exogen = as.matrix(Xexo))
})

IC <- dplyr::bind_rows(
  data.frame(Model = "C1_none",       AIC = AIC(C1), BIC = BIC(C1)),
  data.frame(Model = "C2_LRconst",    AIC = AIC(C2), BIC = BIC(C2)),
  data.frame(Model = "C3_const",      AIC = AIC(C3), BIC = BIC(C3)),
  data.frame(Model = "C4_LRtrend",    AIC = AIC(C4), BIC = BIC(C4)),
  data.frame(Model = "C5_both",       AIC = AIC(C5), BIC = BIC(C5))
) %>% arrange(BIC)

print(IC)

# Choix final :
# Malgré un BIC légèrement plus faible pour LRinclude = "const",
# nous retenons include = "const" pour des raisons économiques
# (prix et indices financiers non stationnaires autour d'un niveau fixe).

"Après avoir déterminé le nombre de retards et le rang de cointégration,
différentes spécifications déterministes du VECM ont été comparées.
Bien que les critères d’information favorisent marginalement l’inclusion d’une constante dans l’espace de cointégration,
la spécification avec constante hors de la relation de long terme est retenue pour des raisons économiques,
conformément aux standards de la littérature macro-financière.
"
# ------------------------------------------------------------
# 6) Estimation VECM avec tsDyn en incluant exogènes (EPU, NFCI)
#    Attention : dans tsDyn, lag = nombre de retards en Δ (p)
# ------------------------------------------------------------
vecm_tsdyn <- VECM(
  data    = as.matrix(Y),
  lag     = p,
  r       = r,
  estim   = "ML",
  include = "const",     
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

# on test la stabilité

var_fit <- VAR(Y, p = p, type = "const")

stability(var_fit, type = "OLS-CUSUM")
plot(stability(var_fit, type = "OLS-CUSUM"))

# ------------------------------------------------------------
# 8) IRF : "valeur refuge" -> choc d'incertitude et réponse de l'or
#    NOTE : vec2var ici ne contient pas directement EPU/NFCI en exogènes.
#    Deux options :
#      A) faire IRF sur chocs internes (USDI, DFII10, MSCI) -> proxy
# ------------------------------------------------------------

# A) IRF internes (ex: choc USDI -> réponse lGOLD)
ir_gold_usdi <- irf(var_level,
                    impulse = "lUSDI",
                    response = "lGOLD",
                    n.ahead = 12,
                    boot = TRUE,
                    runs = 500)
plot(ir_gold_usdi)

# A) IRF internes (ex: choc DFII10 -> réponse lGOLD)
ir_gold_dfii <- irf(var_level,
                    impulse = "DFII10",
                    response = "lGOLD",
                    n.ahead = 12,
                    boot = TRUE,
                    runs = 500)
plot(ir_gold_dfii)

# A) IRF internes (ex: choc IMSCI -> réponse lGOLD)
ir_gold_IMSCI <- irf(var_level,
                    impulse = "lMSCI",
                    response = "lGOLD",
                    n.ahead = 12,
                    boot = TRUE,
                    runs = 500)
plot(ir_gold_IMSCI) 

# ------------------------------------------------------------
# 9) FEVD : Décomposition de la variance des erreurs de prévision
#  barplots empilés (style "figure")
# ------------------------------------------------------------
fevd_res <- fevd(var_level, n.ahead = 12)

# Afficher les tableaux (parts expliquées à chaque horizon)
print(fevd_res)


# Fonction pour tracer un barplot empilé pour une variable donnée
plot_fevd_stacked <- function(fevd_obj, varname, main_title = NULL) {
  M <- fevd_obj[[varname]]  # matrice horizons x chocs
  # convertir en % (optionnel mais plus lisible)
  M_pct <- 100 * M
  
  # barplot attend chocs x horizons
  barplot(t(M_pct),
          beside = FALSE,
          xlab = "Horizon",
          ylab = "Pourcentage",
          main = ifelse(is.null(main_title),
                        paste("FEVD for", varname),
                        main_title),
          legend.text = colnames(M_pct),
          args.legend = list(x = "right", bty = "o", cex = 0.8))
}

# Tracer FEVD pour toutes les équations (une par une)
par(mfrow = c(4,1), mar = c(4,4,2,8))  # marge à droite pour la légende
plot_fevd_stacked(fevd_res, "lGOLD")
plot_fevd_stacked(fevd_res, "lUSDI")
plot_fevd_stacked(fevd_res, "DFII10")
plot_fevd_stacked(fevd_res, "lMSCI")
par(mfrow = c(1,1))
