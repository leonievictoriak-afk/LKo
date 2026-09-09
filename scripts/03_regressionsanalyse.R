##############################################################################
# Bachelorarbeit: Hybrides Arbeiten, Workplace FoMO (wFoMO) und
#                 Arbeitszufriedenheit im öffentlichen Dienst
#
# Schritt 3: Multiple lineare Regression (KQ-Methode) für die Haupteffekte
#            H1  (hybrides Arbeiten -> Arbeitszufriedenheit, positiv)
#            H2a (informationale wFoMO -> Arbeitszufriedenheit, negativ)
#            H2b (relationale wFoMO -> Arbeitszufriedenheit, negativ)
#            inkl. Signifikanzprüfung des Gesamtmodells über den F-Test
#
# Voraussetzung: 01_deskriptive_statistik.R wurde bereits ausgeführt,
#                sodass das Objekt "daten" im Workspace vorhanden ist.
##############################################################################

library(dplyr)

# ---- 1. Modellspezifikation -------------------------------------------------
# AV:  Arbeitszufriedenheit  = Arbeitszufriedenheit (Mittelwert AZ01_01-06,
#                               AZ02_01, AZ03_01; umskaliert auf -3 bis +3)
# UV1: HB01_tage              = Möglichkeit zum hybriden Arbeiten (H1)
# UV2: Informationale_wFoMO   = informationale wFoMO (H2a)
# UV3: Relationale_wFoMO      = relationale wFoMO (H2b)
modell_h1_h2 <- lm(Arbeitszufriedenheit ~ HB01_tage + Informationale_wFoMO + Relationale_wFoMO,
                    data = daten)

# ---- 2. Modellzusammenfassung (Regressionskoeffizienten, t-Tests, R²) ------
cat("\n=== Multiple Regression (KQ-Methode): Arbeitszufriedenheit ~ ")
cat("hybrides Arbeiten + wFoMO informational + wFoMO relational ===\n")
print(summary(modell_h1_h2))

# ---- 3. Signifikanzprüfung des Gesamtmodells (F-Test) ----------------------
f_werte  <- summary(modell_h1_h2)$fstatistic
f_wert   <- f_werte["value"]
df1      <- f_werte["numdf"]
df2      <- f_werte["dendf"]
p_wert_f <- pf(f_wert, df1, df2, lower.tail = FALSE)

cat("\n--- F-Test auf globale Modellgüte (H0: alle Beta = 0) ---\n")
cat("F(", df1, ",", df2, ") = ", round(f_wert, 2),
    ", p = ", format.pval(p_wert_f, digits = 3), "\n", sep = "")
cat("R²       = ", round(summary(modell_h1_h2)$r.squared, 3), "\n", sep = "")
cat("korr. R² = ", round(summary(modell_h1_h2)$adj.r.squared, 3), "\n", sep = "")

# Äquivalente Darstellung über die klassische ANOVA-Tabelle des Modells:
cat("\n--- ANOVA-Tabelle (Modell vs. Residuen) ---\n")
print(anova(modell_h1_h2))

# ---- 4. Standardisierte Koeffizienten (Beta) --------------------------------
# Für den Vergleich der relativen Effektstärke der drei Prädiktoren
# (z-standardisierte Variablen -> Regressionskoeffizient = standardisiertes Beta)
daten_z <- daten %>%
  mutate(across(c(Arbeitszufriedenheit, HB01_tage, Informationale_wFoMO, Relationale_wFoMO),
                ~ as.numeric(scale(.))))

modell_standardisiert <- lm(Arbeitszufriedenheit ~ HB01_tage + Informationale_wFoMO + Relationale_wFoMO,
                             data = daten_z)

cat("\n=== Standardisierte Koeffizienten (Beta) ===\n")
print(round(coef(modell_standardisiert)[-1], 3))
