##############################################################################
# Bachelorarbeit: Hybrides Arbeiten, Workplace FoMO (wFoMO) und
#                 Arbeitszufriedenheit im öffentlichen Dienst
#
# Schritt 5: Robuste Regression auf dem AIC-reduzierten Modell und
#            Prüfung der Regressionsannahmen (Nachtests):
#              - Shapiro-Wilk-Test  (Normalverteilung der Residuen)
#              - Breusch-Pagan-Test (Homoskedastizität)
#              - VIF                (Multikollinearität)
#              - Durbin-Watson-Test (Autokorrelation der Residuen)
#              - RESET-Test         (funktionale Form/Modellspezifikation)
#
# Voraussetzung: 01_deskriptive_statistik.R und 04_aic_selektion.R wurden
#                bereits ausgeführt (Objekte "daten" und "modell_aic" im
#                Workspace vorhanden).
##############################################################################

# install.packages(c("MASS", "lmtest", "car"))
library(MASS)
library(lmtest)
library(car)

# ---- 1. Reduziertes Modell (Ergebnis der AIC-Selektion, Schritt 4) ---------
modell_reduziert <- modell_aic          # KQ-Modell mit den von AIC gewählten Prädiktoren
formel_reduziert  <- formula(modell_reduziert)

# Anzahl der TERME (nicht Koeffizienten!) im Modell: HB01_kat zählt als EIN
# Term, auch wenn er 10 Dummy-Koeffizienten erzeugt. VIF/Multikollinearität
# sind erst ab zwei Termen sinnvoll berechenbar.
n_terme <- length(attr(terms(modell_reduziert), "term.labels"))

cat("\n=== Reduziertes Modell (KQ, nach AIC-Selektion) ===\n")
print(formel_reduziert)
print(summary(modell_reduziert))

# ---- 2. Robuste Regression (Huber-M-Schätzer) auf demselben Modell ---------
modell_robust <- MASS::rlm(formel_reduziert, data = daten)

cat("\n=== Robuste Regression (rlm, Huber-M-Schätzer) ===\n")
print(summary(modell_robust))

# rlm() liefert standardmäßig keine p-Werte (es wird keine Verteilung für die
# Teststatistik unterstellt) -> approximative p-Werte über die t-Verteilung
# mit den Residual-Freiheitsgraden ergänzen.
koef_robust <- summary(modell_robust)$coefficients
df_robust   <- modell_robust$df.residual
koef_robust <- cbind(
  koef_robust,
  p_wert = 2 * pt(abs(koef_robust[, "t value"]), df = df_robust, lower.tail = FALSE)
)
cat("\nRobuste Koeffizienten inkl. approximativem p-Wert:\n")
print(round(koef_robust, 4))

# ---- 3. Vergleich KQ- vs. robuste Koeffizienten ----------------------------
vergleich_koef <- data.frame(
  Praediktor = names(coef(modell_reduziert)),
  KQ         = round(coef(modell_reduziert), 4),
  Robust     = round(coef(modell_robust), 4)
)
cat("\n=== Vergleich: KQ-Koeffizienten vs. robuste Koeffizienten ===\n")
print(vergleich_koef)

# ---- 4. Prüfung der Regressionsannahmen (Nachtests) auf dem KQ-Modell -----

## 4.1 Shapiro-Wilk-Test: Normalverteilung der Residuen
cat("\n--- Shapiro-Wilk-Test (Normalverteilung der Residuen) ---\n")
print(shapiro.test(residuals(modell_reduziert)))

## 4.2 Breusch-Pagan-Test: Homoskedastizität
cat("\n--- Breusch-Pagan-Test (Homoskedastizität) ---\n")
if (n_terme >= 1) {
  print(lmtest::bptest(modell_reduziert))
} else {
  cat("Das AIC-optimale Modell enthält keinen Prädiktor (nur Achsenabschnitt);\n")
  cat("ein Test auf Homoskedastizität setzt jedoch mindestens einen Regressor\n")
  cat("voraus und ist hier nicht durchführbar.\n")
}

## 4.3 Varianzinflationsfaktor (VIF): Multikollinearität
cat("\n--- Varianzinflationsfaktoren (VIF) ---\n")
if (n_terme > 1) {
  print(car::vif(modell_reduziert))
} else {
  cat("Das reduzierte Modell enthält nur einen Prädiktor(-block) - VIF ist bei\n")
  cat("nur einem Term nicht definiert und daher nicht berechenbar.\n")
}

## 4.4 Durbin-Watson-Test: Autokorrelation der Residuen
cat("\n--- Durbin-Watson-Test (Autokorrelation der Residuen) ---\n")
if (n_terme >= 1) {
  print(lmtest::dwtest(modell_reduziert))
} else {
  cat("Nicht durchführbar ohne Regressor (nur Achsenabschnitt).\n")
}

## 4.5 RESET-Test nach Ramsey: funktionale Form/Modellspezifikation
cat("\n--- RESET-Test nach Ramsey (funktionale Form) ---\n")
if (n_terme >= 1) {
  print(lmtest::resettest(modell_reduziert))
} else {
  cat("Nicht durchführbar ohne Regressor (nur Achsenabschnitt).\n")
}
