##############################################################################
# Bachelorarbeit: Hybrides Arbeiten, Workplace FoMO (wFoMO) und
#                 Arbeitszufriedenheit im öffentlichen Dienst
#
# Schritt 7: Vollständige Regressionsanalyse (KQ-Methode) auf dem
#            bereinigten Datensatz (Pfad 2: ohne die über die
#            Hampel-Distanz identifizierten Ausreißer, siehe Schritt 6)
#
# Voraussetzung: 01_deskriptive_statistik.R und 06_mm_regression_ausreisser.R
#                wurden bereits ausgeführt (Objekte "daten" und "modell_mm"
#                im Workspace vorhanden).
##############################################################################

library(dplyr)

# ---- 1. Bereinigten Datensatz erstellen ------------------------------------
# Ausschluss der Fälle mit Hampel-Distanz > 5 (Kriterium aus Schritt 6),
# berechnet auf Basis der Residuen der robusten MM-Regression (modell_mm).
resid_mm        <- residuals(modell_mm)
median_resid    <- median(resid_mm)
mad_resid       <- mad(resid_mm, constant = 1.4826)
hampel_distanz  <- abs(resid_mm - median_resid) / mad_resid
schwelle_hampel <- 5

daten_bereinigt <- daten %>%
  mutate(hampel_distanz = hampel_distanz) %>%
  filter(hampel_distanz <= schwelle_hampel)

cat("N vor Bereinigung: ", nrow(daten), "\n")
cat("N nach Bereinigung:", nrow(daten_bereinigt),
    "(", nrow(daten) - nrow(daten_bereinigt), "Fall/Fälle entfernt)\n")

# ---- 2. Vollständiges Regressionsmodell (KQ) auf bereinigtem Datensatz ----
modell_bereinigt <- lm(Arbeitszufriedenheit ~ HB01_tage + Informationale_wFoMO + Relationale_wFoMO,
                        data = daten_bereinigt)

cat("\n=== Multiple Regression (KQ-Methode) auf bereinigtem Datensatz ===\n")
print(summary(modell_bereinigt))

# ---- 3. Signifikanzprüfung des Gesamtmodells (F-Test) ----------------------
f_werte  <- summary(modell_bereinigt)$fstatistic
f_wert   <- f_werte["value"]
df1      <- f_werte["numdf"]
df2      <- f_werte["dendf"]
p_wert_f <- pf(f_wert, df1, df2, lower.tail = FALSE)

cat("\n--- F-Test auf globale Modellgüte (H0: alle Beta = 0) ---\n")
cat("F(", df1, ",", df2, ") = ", round(f_wert, 2),
    ", p = ", format.pval(p_wert_f, digits = 3), "\n", sep = "")
cat("R²       = ", round(summary(modell_bereinigt)$r.squared, 3), "\n", sep = "")
cat("korr. R² = ", round(summary(modell_bereinigt)$adj.r.squared, 3), "\n", sep = "")

cat("\n--- ANOVA-Tabelle (Modell vs. Residuen) ---\n")
print(anova(modell_bereinigt))

# ---- 4. Standardisierte Koeffizienten (Beta) -------------------------------
daten_bereinigt_z <- daten_bereinigt %>%
  mutate(across(c(Arbeitszufriedenheit, HB01_tage, Informationale_wFoMO, Relationale_wFoMO),
                ~ as.numeric(scale(.))))

modell_bereinigt_standardisiert <- lm(
  Arbeitszufriedenheit ~ HB01_tage + Informationale_wFoMO + Relationale_wFoMO,
  data = daten_bereinigt_z
)

cat("\n=== Standardisierte Koeffizienten (Beta) ===\n")
print(round(coef(modell_bereinigt_standardisiert)[-1], 3))

# ---- 5. Vergleich: Pfad 1 (vollständiger Datensatz) vs. Pfad 2 (bereinigt) -
modell_voll <- lm(Arbeitszufriedenheit ~ HB01_tage + Informationale_wFoMO + Relationale_wFoMO,
                   data = daten)
f_voll <- summary(modell_voll)$fstatistic
p_voll <- pf(f_voll["value"], f_voll["numdf"], f_voll["dendf"], lower.tail = FALSE)

vergleich_modelle <- data.frame(
  Modell = c("Pfad 1: vollständiger Datensatz", "Pfad 2: bereinigter Datensatz"),
  N      = c(nrow(daten), nrow(daten_bereinigt)),
  R2     = round(c(summary(modell_voll)$r.squared, summary(modell_bereinigt)$r.squared), 3),
  F_Wert = round(c(f_voll["value"], f_wert), 2),
  p_Wert = round(c(p_voll, p_wert_f), 4)
)

cat("\n=== Vergleich Pfad 1 (mit Ausreißern) vs. Pfad 2 (ohne Ausreißer) ===\n")
print(vergleich_modelle)
