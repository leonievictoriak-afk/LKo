##############################################################################
# Bachelorarbeit: Hybrides Arbeiten, Workplace FoMO (wFoMO) und
#                 Arbeitszufriedenheit im öffentlichen Dienst
#
# Schritt 8: AIC-basierte Modellselektion auf dem bereinigten Datensatz
#            (Pfad 2, siehe Schritt 7)
#
# Voraussetzung: 01_deskriptive_statistik.R und 07_regression_bereinigt.R
#                wurden bereits ausgeführt (Objekte "daten_bereinigt" und
#                "modell_bereinigt" im Workspace vorhanden).
##############################################################################

# ---- 1. Ausgangsmodelle -----------------------------------------------------
modell_voll_bereinigt <- modell_bereinigt   # volles Modell (H1 + H2a + H2b), bereinigter Datensatz
modell_null_bereinigt <- lm(Arbeitszufriedenheit ~ 1, data = daten_bereinigt)

# ---- 2. Schrittweise Selektion nach AIC (direction = "both") ---------------
cat("\n=== Schrittweise Modellselektion nach AIC (bereinigter Datensatz) ===\n")
modell_aic_bereinigt <- step(modell_voll_bereinigt,
                              scope = list(lower = modell_null_bereinigt,
                                           upper = modell_voll_bereinigt),
                              direction = "both",
                              trace = 1)

cat("\n=== Finales Modell nach AIC-Selektion (bereinigter Datensatz) ===\n")
print(summary(modell_aic_bereinigt))

if (length(coef(modell_aic_bereinigt)) > 1) {
  f_werte  <- summary(modell_aic_bereinigt)$fstatistic
  p_wert_f <- pf(f_werte["value"], f_werte["numdf"], f_werte["dendf"], lower.tail = FALSE)
  cat("\nF(", f_werte["numdf"], ",", f_werte["dendf"], ") = ", round(f_werte["value"], 2),
      ", p = ", format.pval(p_wert_f, digits = 3), "\n", sep = "")
} else {
  cat("\nDas AIC-optimale Modell enthält keinen Prädiktor mehr (nur Achsenabschnitt);\n")
  cat("ein F-Test ist hier nicht sinnvoll interpretierbar.\n")
}

# ---- 3. Vollständiger Modellvergleich (alle Prädiktor-Kombinationen) -------
praediktoren <- c("HB01_tage", "Informationale_wFoMO", "Relationale_wFoMO")

alle_modelle <- list(Nullmodell = modell_null_bereinigt)
for (k in seq_along(praediktoren)) {
  kombinationen <- combn(praediktoren, k, simplify = FALSE)
  for (komb in kombinationen) {
    formel <- as.formula(paste("Arbeitszufriedenheit ~", paste(komb, collapse = " + ")))
    name <- paste(komb, collapse = " + ")
    alle_modelle[[name]] <- lm(formel, data = daten_bereinigt)
  }
}

aic_tabelle_bereinigt <- data.frame(
  Modell      = names(alle_modelle),
  k_Parameter = sapply(alle_modelle, function(m) length(coef(m))),
  AIC         = round(sapply(alle_modelle, AIC), 2),
  R2          = round(sapply(alle_modelle, function(m) summary(m)$r.squared), 4)
)
aic_tabelle_bereinigt <- aic_tabelle_bereinigt[order(aic_tabelle_bereinigt$AIC), ]
rownames(aic_tabelle_bereinigt) <- NULL

cat("\n=== Vollständiger Modellvergleich (bereinigter Datensatz), ")
cat("sortiert nach AIC (kleinster Wert = bestes Modell) ===\n")
print(aic_tabelle_bereinigt)

# ---- 4. Vergleich: AIC-Modell Pfad 1 (Schritt 4) vs. Pfad 2 ---------------
if (exists("modell_aic")) {
  cat("\n=== Vergleich: AIC-optimales Modell Pfad 1 vs. Pfad 2 ===\n")
  vergleich_aic_pfade <- data.frame(
    Pfad         = c("Pfad 1: vollständiger Datensatz", "Pfad 2: bereinigter Datensatz"),
    Praediktoren = c(paste(names(coef(modell_aic))[-1], collapse = " + "),
                      paste(names(coef(modell_aic_bereinigt))[-1], collapse = " + ")),
    AIC          = round(c(AIC(modell_aic), AIC(modell_aic_bereinigt)), 2)
  )
  print(vergleich_aic_pfade)
}
