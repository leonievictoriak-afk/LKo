##############################################################################
# Bachelorarbeit: Hybrides Arbeiten, Workplace FoMO (wFoMO) und
#                 Arbeitszufriedenheit im öffentlichen Dienst
#
# Schritt 9: Robuste Regression auf dem AIC-reduzierten Modell (Pfad 2,
#            bereinigter Datensatz) und Prüfung der Regressionsannahmen
#            (Nachtests):
#              - Shapiro-Wilk-Test  (Normalverteilung der Residuen)
#              - Breusch-Pagan-Test (Homoskedastizität)
#              - VIF                (Multikollinearität)
#              - Durbin-Watson-Test (Autokorrelation der Residuen)
#              - RESET-Test         (funktionale Form/Modellspezifikation)
#
# Voraussetzung: 01_deskriptive_statistik.R, 07_regression_bereinigt.R und
#                08_aic_selektion_bereinigt.R wurden bereits ausgeführt
#                (Objekte "daten_bereinigt" und "modell_aic_bereinigt" im
#                Workspace vorhanden).
##############################################################################

# install.packages(c("MASS", "lmtest", "car"))
library(MASS)
library(lmtest)
library(car)

# ---- 1. Reduziertes Modell (Ergebnis der AIC-Selektion, Pfad 2) ------------
modell_reduziert_b <- modell_aic_bereinigt
formel_reduziert_b  <- formula(modell_reduziert_b)

# Anzahl der TERME (nicht Koeffizienten!) - siehe Kommentar in Schritt 5.
n_terme_b <- length(attr(terms(modell_reduziert_b), "term.labels"))

cat("\n=== Reduziertes Modell (KQ, nach AIC-Selektion, bereinigter Datensatz) ===\n")
print(formel_reduziert_b)
print(summary(modell_reduziert_b))

# ---- 2. Robuste Regression (Huber-M-Schätzer) auf demselben Modell ---------
modell_robust_b <- MASS::rlm(formel_reduziert_b, data = daten_bereinigt)

cat("\n=== Robuste Regression (rlm, Huber-M-Schätzer), bereinigter Datensatz ===\n")
print(summary(modell_robust_b))

# rlm() liefert standardmäßig keine p-Werte -> approximative p-Werte über die
# t-Verteilung mit den Residual-Freiheitsgraden ergänzen.
koef_robust_b <- summary(modell_robust_b)$coefficients
df_robust_b   <- modell_robust_b$df.residual
koef_robust_b <- cbind(
  koef_robust_b,
  p_wert = 2 * pt(abs(koef_robust_b[, "t value"]), df = df_robust_b, lower.tail = FALSE)
)
cat("\nRobuste Koeffizienten inkl. approximativem p-Wert:\n")
print(round(koef_robust_b, 4))

# ---- 3. Vergleich KQ- vs. robuste Koeffizienten ----------------------------
vergleich_koef_b <- data.frame(
  Praediktor = names(coef(modell_reduziert_b)),
  KQ         = round(coef(modell_reduziert_b), 4),
  Robust     = round(coef(modell_robust_b), 4)
)
cat("\n=== Vergleich: KQ-Koeffizienten vs. robuste Koeffizienten (bereinigt) ===\n")
print(vergleich_koef_b)

# ---- 4. Prüfung der Regressionsannahmen (Nachtests) auf dem KQ-Modell -----

## 4.1 Shapiro-Wilk-Test: Normalverteilung der Residuen
cat("\n--- Shapiro-Wilk-Test (Normalverteilung der Residuen) ---\n")
print(shapiro.test(residuals(modell_reduziert_b)))

## 4.2 Breusch-Pagan-Test: Homoskedastizität
cat("\n--- Breusch-Pagan-Test (Homoskedastizität) ---\n")
if (n_terme_b >= 1) {
  print(lmtest::bptest(modell_reduziert_b))
} else {
  cat("Das AIC-optimale Modell enthält keinen Prädiktor (nur Achsenabschnitt);\n")
  cat("ein Test auf Homoskedastizität setzt jedoch mindestens einen Regressor\n")
  cat("voraus und ist hier nicht durchführbar.\n")
}

## 4.3 Varianzinflationsfaktor (VIF): Multikollinearität
cat("\n--- Varianzinflationsfaktoren (VIF) ---\n")
if (n_terme_b > 1) {
  print(car::vif(modell_reduziert_b))
} else {
  cat("Das reduzierte Modell enthält nur einen Prädiktor(-block) - VIF ist bei\n")
  cat("nur einem Term nicht definiert und daher nicht berechenbar.\n")
}

## 4.4 Durbin-Watson-Test: Autokorrelation der Residuen
cat("\n--- Durbin-Watson-Test (Autokorrelation der Residuen) ---\n")
if (n_terme_b >= 1) {
  print(lmtest::dwtest(modell_reduziert_b))
} else {
  cat("Nicht durchführbar ohne Regressor (nur Achsenabschnitt).\n")
}

## 4.5 RESET-Test nach Ramsey: funktionale Form/Modellspezifikation
cat("\n--- RESET-Test nach Ramsey (funktionale Form) ---\n")
if (n_terme_b >= 1) {
  print(lmtest::resettest(modell_reduziert_b))
} else {
  cat("Nicht durchführbar ohne Regressor (nur Achsenabschnitt).\n")
}
