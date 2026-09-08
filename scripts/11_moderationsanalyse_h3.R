##############################################################################
# Bachelorarbeit: Hybrides Arbeiten, Workplace FoMO (wFoMO) und
#                 Arbeitszufriedenheit im öffentlichen Dienst
#
# Schritt 11: Moderationsanalyse H3a und H3b
#   H3a: Mit zunehmender informationaler wFoMO wird ein schwächerer
#        positiver Zusammenhang zwischen hybridem Arbeiten und
#        Arbeitszufriedenheit erwartet (abschwächende Interaktion).
#   H3b: Mit zunehmender relationaler wFoMO wird ein schwächerer
#        positiver Zusammenhang zwischen hybridem Arbeiten und
#        Arbeitszufriedenheit erwartet (abschwächende Interaktion).
#
# HB01_kat ist eine Faktorvariable (10 Dummy-Kontraste) und kann daher nicht
# z-standardisiert und zu EINEM einzelnen Interaktionsterm (Zahl x Zahl)
# verrechnet werden. Eine Interaktion HB01_kat:wFoMO erzeugt stattdessen 10
# einzelne Interaktionskoeffizienten (je Dummy-Kontrast einen). H3a/H3b werden
# deshalb über einen Omnibus-F-Test geprüft: Erklärt der GESAMTE
# Interaktionsblock (alle 10 HB01_kat:wFoMO-Kontraste gemeinsam) signifikant
# zusätzliche Varianz gegenüber dem Modell ohne Interaktion? Das ist ein
# Test auf das VORHANDENSEIN einer Moderation über die Kategorien hinweg,
# keine gerichtete Aussage über eine einzelne Steigung.
#
# Voraussetzung: 01_deskriptive_statistik.R und 07_regression_bereinigt.R
#                wurden bereits ausgeführt (Objekte "daten_mm" und
#                "daten_bereinigt" im Workspace vorhanden).
##############################################################################

library(dplyr)

# ---- Funktion: hierarchische Moderationsanalyse für einen Datensatz -------
moderation_analyse <- function(daten_input, bezeichnung) {

  # z-Standardisierung der metrischen wFoMO-Prädiktoren (Aiken & West, 1991);
  # HB01_kat bleibt als Faktor unverändert (kategorial, keine Standardisierung
  # möglich/sinnvoll).
  daten_z <- daten_input %>%
    mutate(
      wfomo_info_z = as.numeric(scale(wFoMO_informational)),
      wfomo_rel_z  = as.numeric(scale(wFoMO_relational))
    )

  # Modell 1: Haupteffekte (H1, H2a, H2b)
  modell_haupt <- lm(Arbeitszufriedenheit ~ HB01_kat + wfomo_info_z + wfomo_rel_z, data = daten_z)

  # Modell 2a/2b: Haupteffekte + jeweils EIN Interaktionsblock (H3a bzw. H3b)
  modell_h3a <- update(modell_haupt, . ~ . + HB01_kat:wfomo_info_z)
  modell_h3b <- update(modell_haupt, . ~ . + HB01_kat:wfomo_rel_z)

  cat("\n============================================================\n")
  cat("Moderationsanalyse -", bezeichnung, "\n")
  cat("============================================================\n")

  cat("\n--- Modell 1: Haupteffekte ---\n")
  print(summary(modell_haupt))

  cat("\n--- H3a: Omnibus-F-Test für den Interaktionsblock HB01_kat:wFoMO_informational ---\n")
  vgl_h3a <- anova(modell_haupt, modell_h3a)
  print(vgl_h3a)
  delta_r2_h3a <- summary(modell_h3a)$r.squared - summary(modell_haupt)$r.squared
  cat("Delta R² durch den Interaktionsblock (H3a):", round(delta_r2_h3a, 4), "\n")

  cat("\n--- H3b: Omnibus-F-Test für den Interaktionsblock HB01_kat:wFoMO_relational ---\n")
  vgl_h3b <- anova(modell_haupt, modell_h3b)
  print(vgl_h3b)
  delta_r2_h3b <- summary(modell_h3b)$r.squared - summary(modell_haupt)$r.squared
  cat("Delta R² durch den Interaktionsblock (H3b):", round(delta_r2_h3b, 4), "\n")

  list(haupt = modell_haupt, h3a = modell_h3a, h3b = modell_h3b,
       vgl_h3a = vgl_h3a, vgl_h3b = vgl_h3b)
}

# ---- Pfad 1: vollständiger Datensatz ---------------------------------------
ergebnis_pfad1 <- moderation_analyse(daten_mm, "Pfad 1 (vollständiger Datensatz)")

# ---- Pfad 2: bereinigter Datensatz -----------------------------------------
ergebnis_pfad2 <- moderation_analyse(daten_bereinigt, "Pfad 2 (bereinigter Datensatz)")

# ---- Zusammenfassende Tabelle: H3a und H3b ---------------------------------
extrahiere_omnibus <- function(vgl) {
  c(F = vgl$F[2], df1 = vgl$Df[2], df2 = vgl$Res.Df[2], p = vgl$`Pr(>F)`[2])
}

entscheidung_moderation <- function(p, alpha = 0.05) {
  if (is.na(p)) return("nicht bestimmbar")
  if (p >= alpha) "H0 nicht verworfen (n.s.)" else "signifikanter Interaktionsblock (Moderation vorhanden)"
}

h3a_p1 <- extrahiere_omnibus(ergebnis_pfad1$vgl_h3a)
h3a_p2 <- extrahiere_omnibus(ergebnis_pfad2$vgl_h3a)
h3b_p1 <- extrahiere_omnibus(ergebnis_pfad1$vgl_h3b)
h3b_p2 <- extrahiere_omnibus(ergebnis_pfad2$vgl_h3b)

h3_tab <- data.frame(
  Hypothese = c("H3a (HB01_kat x wFoMO informational)", "H3b (HB01_kat x wFoMO relational)"),
  F_Pfad1   = round(c(h3a_p1["F"], h3b_p1["F"]), 3),
  p_Pfad1   = round(c(h3a_p1["p"], h3b_p1["p"]), 4),
  Entscheidung_Pfad1 = c(entscheidung_moderation(h3a_p1["p"]), entscheidung_moderation(h3b_p1["p"])),
  F_Pfad2   = round(c(h3a_p2["F"], h3b_p2["F"]), 3),
  p_Pfad2   = round(c(h3a_p2["p"], h3b_p2["p"]), 4),
  Entscheidung_Pfad2 = c(entscheidung_moderation(h3a_p2["p"]), entscheidung_moderation(h3b_p2["p"]))
)

cat("\n=== Zusammenfassende Prüfung H3a und H3b (Pfad 1 vs. Pfad 2) ===\n")
cat("Omnibus-F-Test des jeweiligen Interaktionsblocks (10 Kontraste); eine\n")
cat("gerichtete Aussage ('abschwächend') erfordert zusätzlich die visuelle\n")
cat("Prüfung der Interaktionskoeffizienten je Kategorie (siehe Modell-\n")
cat("Summaries oben) bzw. eine grafische Slope-Darstellung je HB01_kat-Stufe.\n")
print(h3_tab)
