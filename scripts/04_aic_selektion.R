##############################################################################
# Bachelorarbeit: Hybrides Arbeiten, Workplace FoMO (wFoMO) und
#                 Arbeitszufriedenheit im öffentlichen Dienst
#
# Schritt 4: AIC-basierte Modellselektion
#
# Voraussetzung: 01_deskriptive_statistik.R wurde bereits ausgeführt,
#                sodass das Objekt "daten" im Workspace vorhanden ist.
##############################################################################

# ---- 1. Ausgangsmodelle -----------------------------------------------------
# Volles Modell: alle drei theoretisch hergeleiteten Prädiktoren (H1, H2a, H2b)
modell_voll <- lm(az_kern ~ hb_moeglichkeit_tage + wfomo_informational + wfomo_relational,
                   data = daten)

# Nullmodell (nur Achsenabschnitt) als untere Grenze der Selektion
modell_null <- lm(az_kern ~ 1, data = daten)

# ---- 2. Schrittweise Selektion nach AIC (direction = "both") ---------------
cat("\n=== Schrittweise Modellselektion nach AIC (direction = 'both') ===\n")
modell_aic <- step(modell_voll,
                    scope = list(lower = modell_null, upper = modell_voll),
                    direction = "both",
                    trace = 1)

cat("\n=== Finales Modell nach schrittweiser AIC-Selektion ===\n")
print(summary(modell_aic))

if (length(coef(modell_aic)) > 1) {
  f_werte  <- summary(modell_aic)$fstatistic
  p_wert_f <- pf(f_werte["value"], f_werte["numdf"], f_werte["dendf"], lower.tail = FALSE)
  cat("\nF(", f_werte["numdf"], ",", f_werte["dendf"], ") = ", round(f_werte["value"], 2),
      ", p = ", format.pval(p_wert_f, digits = 3), "\n", sep = "")
} else {
  cat("\nDas AIC-optimale Modell enthält keinen Prädiktor mehr (nur Achsenabschnitt);\n")
  cat("ein F-Test ist hier nicht sinnvoll interpretierbar.\n")
}

# ---- 3. Vollständiger Modellvergleich (alle Prädiktor-Kombinationen) -------
# Bei nur drei Prädiktoren lassen sich alle 2^3 = 8 möglichen Modelle
# (inkl. Nullmodell) direkt vergleichen ("Best-Subset"-Ansatz), statt sich
# auf den (nur lokal optimalen) Pfad der schrittweisen Selektion zu verlassen.
praediktoren <- c("hb_moeglichkeit_tage", "wfomo_informational", "wfomo_relational")

alle_modelle <- list(Nullmodell = modell_null)
for (k in seq_along(praediktoren)) {
  kombinationen <- combn(praediktoren, k, simplify = FALSE)
  for (komb in kombinationen) {
    formel <- as.formula(paste("az_kern ~", paste(komb, collapse = " + ")))
    name <- paste(komb, collapse = " + ")
    alle_modelle[[name]] <- lm(formel, data = daten)
  }
}

aic_tabelle <- data.frame(
  Modell = names(alle_modelle),
  k_Parameter = sapply(alle_modelle, function(m) length(coef(m))),
  AIC = round(sapply(alle_modelle, AIC), 2),
  R2  = round(sapply(alle_modelle, function(m) summary(m)$r.squared), 4)
)
aic_tabelle <- aic_tabelle[order(aic_tabelle$AIC), ]
rownames(aic_tabelle) <- NULL

cat("\n=== Vollständiger Modellvergleich (alle Prädiktor-Kombinationen), ")
cat("sortiert nach AIC (kleinster Wert = bestes Modell) ===\n")
print(aic_tabelle)
