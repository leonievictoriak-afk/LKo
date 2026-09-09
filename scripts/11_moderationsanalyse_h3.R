##############################################################################
# Bachelorarbeit: Hybrides Arbeiten, Workplace FoMO (wFoMO) und
#                 Arbeitszufriedenheit im öffentlichen Dienst
#
# Schritt 11: Moderationsanalyse H3a und H3b
#   H3a: Mit zunehmender informationaler wFoMO wird ein schwächerer
#        positiver Zusammenhang zwischen hybridem Arbeiten und
#        Arbeitszufriedenheit erwartet (negative Interaktion).
#   H3b: Mit zunehmender relationaler wFoMO wird ein schwächerer
#        positiver Zusammenhang zwischen hybridem Arbeiten und
#        Arbeitszufriedenheit erwartet (negative Interaktion).
#
# Voraussetzung: 01_deskriptive_statistik.R und 07_regression_bereinigt.R
#                wurden bereits ausgeführt (Objekte "daten" und
#                "daten_bereinigt" im Workspace vorhanden).
##############################################################################

library(dplyr)

# ---- Funktion: hierarchische Moderationsanalyse für einen Datensatz -------
moderation_analyse <- function(daten_input, bezeichnung) {

  # z-Standardisierung vor Bildung der Interaktionsterme, um Multikollinearität
  # zwischen Haupteffekten und Interaktionstermen zu reduzieren
  # (Aiken & West, 1991)
  daten_z <- daten_input %>%
    mutate(
      hb_z         = as.numeric(scale(HB01_tage)),
      wfomo_info_z = as.numeric(scale(Informationale_wFoMO)),
      wfomo_rel_z  = as.numeric(scale(Relationale_wFoMO))
    )

  # Modell 1: Haupteffekte (H1, H2a, H2b)
  modell_haupt <- lm(Arbeitszufriedenheit ~ hb_z + wfomo_info_z + wfomo_rel_z, data = daten_z)

  # Modell 2: Haupteffekte + Interaktionsterme (H3a, H3b)
  modell_moderation <- lm(Arbeitszufriedenheit ~ hb_z + wfomo_info_z + wfomo_rel_z +
                             hb_z:wfomo_info_z + hb_z:wfomo_rel_z,
                           data = daten_z)

  cat("\n============================================================\n")
  cat("Moderationsanalyse -", bezeichnung, "\n")
  cat("============================================================\n")

  cat("\n--- Modell 1: Haupteffekte ---\n")
  print(summary(modell_haupt))

  cat("\n--- Modell 2: Haupteffekte + Interaktionsterme ---\n")
  print(summary(modell_moderation))

  cat("\n--- Inkrementeller F-Test: erklärt der Interaktionsblock ")
  cat("zusätzliche Varianz? (Modell 1 vs. Modell 2) ---\n")
  print(anova(modell_haupt, modell_moderation))

  delta_r2 <- summary(modell_moderation)$r.squared - summary(modell_haupt)$r.squared
  cat("\nDelta R² durch die Interaktionsterme:", round(delta_r2, 4), "\n")

  list(haupt = modell_haupt, moderation = modell_moderation)
}

# ---- Pfad 1: vollständiger Datensatz ---------------------------------------
ergebnis_pfad1 <- moderation_analyse(daten, "Pfad 1 (vollständiger Datensatz)")

# ---- Pfad 2: bereinigter Datensatz -----------------------------------------
ergebnis_pfad2 <- moderation_analyse(daten_bereinigt, "Pfad 2 (bereinigter Datensatz)")

# ---- Zusammenfassende Tabelle: H3a und H3b ---------------------------------
extrahiere_interaktion <- function(modell, term) {
  koef <- summary(modell)$coefficients
  if (term %in% rownames(koef)) {
    c(b = koef[term, "Estimate"], p = koef[term, "Pr(>|t|)"])
  } else {
    c(b = NA_real_, p = NA_real_)
  }
}

entscheidung_moderation <- function(b, p, alpha = 0.05) {
  if (is.na(b)) return("nicht im Modell enthalten")
  if (is.na(p) || p >= alpha) return("H0 nicht verworfen (n.s.)")
  if (b < 0) "Hypothese angenommen (abschwächende Interaktion)" else "signifikant, falsche Richtung"
}

h3_tab <- data.frame(
  Hypothese = c("H3a (hybrid x wFoMO informational)", "H3b (hybrid x wFoMO relational)"),
  Term      = c("hb_z:wfomo_info_z", "hb_z:wfomo_rel_z"),
  stringsAsFactors = FALSE
)

res_p1 <- sapply(h3_tab$Term, function(t) extrahiere_interaktion(ergebnis_pfad1$moderation, t))
h3_tab$b_Pfad1 <- round(as.numeric(res_p1["b", ]), 4)
h3_tab$p_Pfad1 <- round(as.numeric(res_p1["p", ]), 4)
h3_tab$Entscheidung_Pfad1 <- mapply(entscheidung_moderation, h3_tab$b_Pfad1, h3_tab$p_Pfad1)

res_p2 <- sapply(h3_tab$Term, function(t) extrahiere_interaktion(ergebnis_pfad2$moderation, t))
h3_tab$b_Pfad2 <- round(as.numeric(res_p2["b", ]), 4)
h3_tab$p_Pfad2 <- round(as.numeric(res_p2["p", ]), 4)
h3_tab$Entscheidung_Pfad2 <- mapply(entscheidung_moderation, h3_tab$b_Pfad2, h3_tab$p_Pfad2)

cat("\n=== Zusammenfassende Prüfung H3a und H3b (Pfad 1 vs. Pfad 2) ===\n")
print(h3_tab)
