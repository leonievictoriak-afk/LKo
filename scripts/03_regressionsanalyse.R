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
# AV:  Arbeitszufriedenheit = Mittelwert aus AZ01_01-06, AZ02_01, AZ03_01
# UV1: HB01_kat             = Möglichkeit zum hybriden Arbeiten (H1)
#      Faktor mit 11 Kategorien ("0 Tage" ... "5 Tage"), Referenzkategorie
#      "0 Tage" -> geht über Dummy-Kodierung (10 Kontraste) ins Modell ein,
#      OHNE gleiche Abstände zwischen den Kategorien zu unterstellen.
# UV2: wFoMO_informational  = informationale wFoMO (H2a)
# UV3: wFoMO_relational     = relationale wFoMO (H2b)
#
# Hinweis zur Fallzahl: Arbeitszufriedenheit ist nur für die Teilstichprobe
# definiert, die zusätzlich AZ02_01 UND AZ03_01 beantwortet hat (siehe
# Kommentar in Schritt 1); lm() schließt die übrigen Fälle automatisch aus
# (listwise deletion). Die tatsächliche Fallzahl steht unten unter
# "Residual standard error: ... on X degrees of freedom".
modell_h1_h2 <- lm(Arbeitszufriedenheit ~ HB01_kat + wFoMO_informational + wFoMO_relational,
                    data = daten)

# ---- 2. Modellzusammenfassung (Regressionskoeffizienten, t-Tests, R²) ------
cat("\n=== Multiple Regression (KQ-Methode): Arbeitszufriedenheit ~ ")
cat("HB01_kat (Möglichkeit hybrides Arbeiten) + wFoMO informational + wFoMO relational ===\n")
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

# Äquivalente Darstellung über die klassische ANOVA-Tabelle des Modells
# (Typ-I-Summenquadrate; für HB01_kat als Faktor liefert dies zugleich den
# gemeinsamen Omnibus-F-Test über alle 10 Dummy-Kontraste hinweg -> das ist
# der zentrale Signifikanztest für H1).
cat("\n--- ANOVA-Tabelle (Modell vs. Residuen, je Term) ---\n")
print(anova(modell_h1_h2))

# ---- 4. Standardisierte Koeffizienten (Beta) für die metrischen Prädiktoren
# HB01_kat ist eine Faktorvariable (Dummy-kodiert) und besitzt daher KEIN
# einzelnes standardisiertes Beta; die Effektstärke von HB01_kat zeigt sich
# in den (unstandardisierten) Dummy-Koeffizienten aus Abschnitt 2 sowie im
# Omnibus-F-Test/der partiellen SS aus der ANOVA-Tabelle. Für wFoMO
# informational/relational (metrisch) lassen sich Beta-Koeffizienten wie
# gewohnt über z-Standardisierung berechnen und damit deren relative
# Effektstärke vergleichen.
daten_z <- daten %>%
  mutate(across(c(Arbeitszufriedenheit, wFoMO_informational, wFoMO_relational),
                ~ as.numeric(scale(.))))

modell_standardisiert <- lm(Arbeitszufriedenheit ~ HB01_kat + wFoMO_informational + wFoMO_relational,
                             data = daten_z)

cat("\n=== Standardisierte Koeffizienten (Beta) für wFoMO informational/relational ===\n")
cat("(HB01_kat ist kategorial und wird hier nicht mit ausgegeben, siehe Kommentar oben)\n")
beta_koef <- coef(modell_standardisiert)
print(round(beta_koef[c("wFoMO_informational", "wFoMO_relational")], 3))
