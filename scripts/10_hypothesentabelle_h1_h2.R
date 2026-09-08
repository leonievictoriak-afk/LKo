##############################################################################
# Bachelorarbeit: Hybrides Arbeiten, Workplace FoMO (wFoMO) und
#                 Arbeitszufriedenheit im öffentlichen Dienst
#
# Schritt 10: Zusammenfassende Prüfung von H1, H2a, H2b über alle bisher
#             gerechneten Modelle (Pfad 1 vs. Pfad 2, jeweils volles und
#             AIC-reduziertes Modell)
#
# HB01_kat (H1) ist eine Faktorvariable (10 Dummy-Kontraste) und liefert
# deshalb KEINEN einzelnen Koeffizienten/p-Wert wie ein metrischer Prädiktor.
# H1 wird daher über einen Omnibus-F-Test geprüft (Modellvergleich MIT vs.
# OHNE den gesamten HB01_kat-Block, via anova()); als ergänzende Information
# für die Richtung wird der Zusammenhang zwischen der Kategorien-Rangfolge
# ("0 Tage" < "0-1 Tag" < ... < "5 Tage") und den mittleren AV-Werten je
# Kategorie über eine Rangkorrelation (Spearman) beschrieben - rein
# deskriptiv, OHNE dass dafür im Regressionsmodell selbst gleiche Abstände
# zwischen den Kategorien unterstellt werden.
#
# H2a und H2b bleiben metrische Einzelprädiktoren und werden weiterhin über
# den klassischen t-Test des jeweiligen Regressionskoeffizienten geprüft.
#
# Voraussetzung: Skripte 01, 03, 04, 07 und 08 wurden bereits ausgeführt
#                (Objekte modell_voll, modell_aic, modell_bereinigt,
#                modell_aic_bereinigt im Workspace vorhanden).
##############################################################################

library(dplyr)

# ---- Hilfsfunktionen: H2a/H2b (metrische Einzelprädiktoren) ----------------
koef_extrahieren <- function(modell, praediktor) {
  koef <- summary(modell)$coefficients
  if (praediktor %in% rownames(koef)) {
    c(b = koef[praediktor, "Estimate"], p = koef[praediktor, "Pr(>|t|)"])
  } else {
    c(b = NA_real_, p = NA_real_)   # Prädiktor wurde ggf. durch AIC-Selektion entfernt
  }
}

entscheidung <- function(b, p, erwartete_richtung, alpha = 0.05) {
  if (is.na(b)) return("nicht im Modell enthalten")
  if (is.na(p) || p >= alpha) return("H0 nicht verworfen (n.s.)")
  richtung_passt <- (erwartete_richtung == "positiv" && b > 0) ||
                     (erwartete_richtung == "negativ" && b < 0)
  if (richtung_passt) "Hypothese angenommen" else "signifikant, falsche Richtung"
}

# ---- Hilfsfunktion: H1 (Faktor HB01_kat) -----------------------------------
# Omnibus-F-Test: Modell MIT HB01_kat vs. dasselbe Modell OHNE HB01_kat
# (update() verwendet automatisch denselben Datensatz wie das Ausgangsmodell).
omnibus_test_hb <- function(modell, praediktor = "HB01_kat") {
  terme <- attr(terms(modell), "term.labels")
  if (!(praediktor %in% terme)) {
    return(list(F = NA_real_, df1 = NA_real_, df2 = NA_real_, p = NA_real_, enthalten = FALSE))
  }
  modell_ohne <- update(modell, as.formula(paste(". ~ . -", praediktor)))
  vgl <- anova(modell_ohne, modell)
  list(F = vgl$F[2], df1 = vgl$Df[2], df2 = vgl$Res.Df[2], p = vgl$`Pr(>F)`[2], enthalten = TRUE)
}

# Deskriptive Richtungsprüfung: Rangkorrelation (Spearman) zwischen der
# Kategorien-Reihenfolge von HB01_kat und dem mittleren AV-Wert je Kategorie.
richtung_hb <- function(daten_input) {
  mittelwerte <- daten_input %>%
    filter(!is.na(HB01_kat), !is.na(Arbeitszufriedenheit)) %>%
    group_by(HB01_kat) %>%
    summarise(m_az = mean(Arbeitszufriedenheit), .groups = "drop") %>%
    mutate(rang = as.integer(factor(HB01_kat, levels = levels(daten_input$HB01_kat))))
  if (nrow(mittelwerte) < 3) return(NA_real_)
  suppressWarnings(cor(mittelwerte$rang, mittelwerte$m_az, method = "spearman", use = "complete.obs"))
}

entscheidung_h1 <- function(omnibus, rho, alpha = 0.05) {
  if (!omnibus$enthalten) return("nicht im Modell enthalten")
  if (is.na(omnibus$p) || omnibus$p >= alpha) return("H0 nicht verworfen (n.s.)")
  if (!is.na(rho) && rho > 0) "Hypothese angenommen (signifikant, positiver Trend)"
  else "signifikant, aber kein klar positiver Trend (siehe Dummy-Koeffizienten)"
}

# ---- H1: Omnibus-Tests + Richtungsprüfung für alle vier Modelle ------------
rho_voll      <- richtung_hb(daten_mm)
rho_bereinigt <- richtung_hb(daten_bereinigt)

h1_voll      <- omnibus_test_hb(modell_voll)
h1_bereinigt <- omnibus_test_hb(modell_bereinigt)
h1_aic       <- omnibus_test_hb(modell_aic)
h1_aic_ber   <- omnibus_test_hb(modell_aic_bereinigt)

h1_tabelle <- data.frame(
  Modell = c("Pfad 1: volles Modell", "Pfad 2: volles Modell (bereinigt)",
             "Pfad 1: AIC-reduziertes Modell", "Pfad 2: AIC-reduziertes Modell (bereinigt)"),
  F_Wert = round(c(h1_voll$F, h1_bereinigt$F, h1_aic$F, h1_aic_ber$F), 3),
  df1    = c(h1_voll$df1, h1_bereinigt$df1, h1_aic$df1, h1_aic_ber$df1),
  df2    = c(h1_voll$df2, h1_bereinigt$df2, h1_aic$df2, h1_aic_ber$df2),
  p      = round(c(h1_voll$p, h1_bereinigt$p, h1_aic$p, h1_aic_ber$p), 4),
  rho_Richtung = round(c(rho_voll, rho_bereinigt, rho_voll, rho_bereinigt), 3),
  Entscheidung = c(entscheidung_h1(h1_voll, rho_voll),
                   entscheidung_h1(h1_bereinigt, rho_bereinigt),
                   entscheidung_h1(h1_aic, rho_voll),
                   entscheidung_h1(h1_aic_ber, rho_bereinigt))
)

cat("\n=== H1 (HB01_kat, Omnibus-F-Test über alle Dummy-Kontraste) ===\n")
cat("rho_Richtung = Spearman-Rangkorrelation zwischen Kategorien-Reihenfolge\n")
cat("und mittlerer Arbeitszufriedenheit je Kategorie (rein deskriptiv)\n")
print(h1_tabelle)

# ---- H2a/H2b: Tabelle A - volle Modelle (Pfad 1 vs. Pfad 2) ----------------
hypothesen_basis <- data.frame(
  Hypothese  = c("H2a", "H2b"),
  Praediktor = c("wFoMO_informational", "wFoMO_relational"),
  Erwartung  = c("negativ", "negativ"),
  stringsAsFactors = FALSE
)

tabelle_bauen <- function(modell) {
  res <- sapply(hypothesen_basis$Praediktor, function(p) koef_extrahieren(modell, p))
  data.frame(
    b = round(as.numeric(res["b", ]), 4),
    p = round(as.numeric(res["p", ]), 4)
  )
}

res_voll      <- tabelle_bauen(modell_voll)
res_bereinigt <- tabelle_bauen(modell_bereinigt)

tabelle_voll <- hypothesen_basis
tabelle_voll$b_Pfad1 <- res_voll$b
tabelle_voll$p_Pfad1 <- res_voll$p
tabelle_voll$Entscheidung_Pfad1 <- mapply(entscheidung, tabelle_voll$b_Pfad1,
                                           tabelle_voll$p_Pfad1, tabelle_voll$Erwartung)
tabelle_voll$b_Pfad2 <- res_bereinigt$b
tabelle_voll$p_Pfad2 <- res_bereinigt$p
tabelle_voll$Entscheidung_Pfad2 <- mapply(entscheidung, tabelle_voll$b_Pfad2,
                                           tabelle_voll$p_Pfad2, tabelle_voll$Erwartung)

cat("\n=== H2a/H2b: volle Modelle (Pfad 1 vs. Pfad 2) ===\n")
print(tabelle_voll)

# ---- H2a/H2b: Tabelle B - AIC-reduzierte Modelle (Pfad 1 vs. Pfad 2) -------
res_aic1 <- tabelle_bauen(modell_aic)
res_aic2 <- tabelle_bauen(modell_aic_bereinigt)

tabelle_aic <- hypothesen_basis
tabelle_aic$b_Pfad1 <- res_aic1$b
tabelle_aic$p_Pfad1 <- res_aic1$p
tabelle_aic$Entscheidung_Pfad1 <- mapply(entscheidung, tabelle_aic$b_Pfad1,
                                          tabelle_aic$p_Pfad1, tabelle_aic$Erwartung)
tabelle_aic$b_Pfad2 <- res_aic2$b
tabelle_aic$p_Pfad2 <- res_aic2$p
tabelle_aic$Entscheidung_Pfad2 <- mapply(entscheidung, tabelle_aic$b_Pfad2,
                                          tabelle_aic$p_Pfad2, tabelle_aic$Erwartung)

cat("\n=== H2a/H2b: AIC-reduzierte Modelle (Pfad 1 vs. Pfad 2) ===\n")
print(tabelle_aic)
