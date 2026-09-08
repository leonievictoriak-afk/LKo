##############################################################################
# Bachelorarbeit: Hybrides Arbeiten, Workplace FoMO (wFoMO) und
#                 Arbeitszufriedenheit im öffentlichen Dienst
#
# Schritt 10: Zusammenfassende Prüfung von H1, H2a, H2b über alle bisher
#             gerechneten Modelle (Pfad 1 vs. Pfad 2, jeweils volles und
#             AIC-reduziertes Modell)
#
# Voraussetzung: Skripte 01, 03, 04, 07 und 08 wurden bereits ausgeführt
#                (Objekte modell_voll, modell_aic, modell_bereinigt,
#                modell_aic_bereinigt im Workspace vorhanden).
##############################################################################

# ---- Hilfsfunktionen --------------------------------------------------------
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

hypothesen_basis <- data.frame(
  Hypothese  = c("H1", "H2a", "H2b"),
  Praediktor = c("hb_moeglichkeit_tage", "wfomo_informational", "wfomo_relational"),
  Erwartung  = c("positiv", "negativ", "negativ"),
  stringsAsFactors = FALSE
)

tabelle_bauen <- function(modell) {
  res <- sapply(hypothesen_basis$Praediktor, function(p) koef_extrahieren(modell, p))
  data.frame(
    b = round(as.numeric(res["b", ]), 4),
    p = round(as.numeric(res["p", ]), 4)
  )
}

# ---- Tabelle A: volle Modelle (Pfad 1 vs. Pfad 2) --------------------------
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

cat("\n=== H1/H2a/H2b: volle Modelle (Pfad 1 vs. Pfad 2) ===\n")
print(tabelle_voll)

# ---- Tabelle B: AIC-reduzierte Modelle (Pfad 1 vs. Pfad 2) -----------------
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

cat("\n=== H1/H2a/H2b: AIC-reduzierte Modelle (Pfad 1 vs. Pfad 2) ===\n")
print(tabelle_aic)
