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
#                sodass das Objekt "Dataset" im Workspace vorhanden ist.
##############################################################################

library(dplyr)

# ---- 1. Modellspezifikation -------------------------------------------------
# AV:  Arbeitszufriedenheit = rekodierte IAZ-K-Skala (-3 bis +3, Schritt 1)
# UV1: HB01_ord = Homeoffice-/hybride-Arbeiten-Möglichkeit (H1)
# UV2: wFoMO_informational  = informationale wFoMO (H2a)
# UV3: wFoMO_relational     = relationale wFoMO (H2b)
#
# HB01_ord ist gemäß Schritt 1 ein GEORDNETER Faktor (11 Stufen "0 Tage" bis
# "5 Tage"), keine metrische Tage-Näherung. R kodiert geordnete Faktoren
# standardmäßig über orthogonale Polynomkontraste (.L = linear, .Q =
# quadratisch, ...). Für die gerichtete Hypothese H1 ("je mehr Möglichkeit
# zum hybriden Arbeiten, desto höher die Arbeitszufriedenheit") ist der
# LINEARE Kontrast HB01_ord.L der relevante Koeffizient; die höhergradigen
# Kontraste (.Q, .C, ...) verbleiben im Modell, um nicht-lineare Effekte
# statistisch zu kontrollieren, sind selbst aber nicht Gegenstand von H1.
modell_h1_h2 <- lm(Arbeitszufriedenheit ~ HB01_ord + wFoMO_informational + wFoMO_relational,
                    data = Dataset)

# ---- 2. Modellzusammenfassung (Regressionskoeffizienten, t-Tests, R²) ------
cat("\n=== Multiple Regression (KQ-Methode): Arbeitszufriedenheit ~ ")
cat("HB01_ord (hybrides Arbeiten) + wFoMO informational + wFoMO relational ===\n")
print(summary(modell_h1_h2))

cat("\n--- H1-Testgröße: linearer Trend HB01_ord.L (aus obiger Tabelle) ---\n")
koef_h1 <- summary(modell_h1_h2)$coefficients
print(round(koef_h1["HB01_ord.L", , drop = FALSE], 4))
cat("\n(zum Vergleich die höhergradigen HB01_ord-Kontraste, die nicht Teil der\n")
cat("H1-Hypothese sind, sondern nur mögliche Nichtlinearität kontrollieren:)\n")
print(round(koef_h1[grepl("^HB01_ord", rownames(koef_h1)) & rownames(koef_h1) != "HB01_ord.L", , drop = FALSE], 4))

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
# Für den Vergleich der relativen Effektstärke: die metrischen Prädiktoren
# (AV und wFoMO-Subskalen) werden z-standardisiert. HB01_ord ist kategorial
# (geordneter Faktor) und wird NICHT mit skaliert; der lineare Polynomkontrast
# HB01_ord.L liegt bereits auf einer orthogonalen, näherungsweise
# standardisierten Skala und bleibt daher unverändert direkt vergleichbar.
daten_z <- Dataset %>%
  mutate(across(c(Arbeitszufriedenheit, wFoMO_informational, wFoMO_relational),
                ~ as.numeric(scale(.))))

modell_standardisiert <- lm(Arbeitszufriedenheit ~ HB01_ord + wFoMO_informational + wFoMO_relational,
                             data = daten_z)

cat("\n=== Standardisierte Koeffizienten (Beta) der metrischen Prädiktoren ===\n")
cat("(HB01_ord.L ist ein orthogonaler Polynomkontrast, keine z-standardisierte\n")
cat("Beta im engeren Sinn - Betrag daher nur eingeschränkt mit den beiden\n")
cat("z-standardisierten wFoMO-Betas vergleichbar.)\n")
koef_std <- coef(modell_standardisiert)
print(round(koef_std[c("HB01_ord.L", "wFoMO_informational", "wFoMO_relational")], 3))
