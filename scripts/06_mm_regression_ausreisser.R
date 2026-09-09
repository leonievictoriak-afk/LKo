##############################################################################
# Bachelorarbeit: Hybrides Arbeiten, Workplace FoMO (wFoMO) und
#                 Arbeitszufriedenheit im öffentlichen Dienst
#
# Schritt 6: Robuste MM-Regression auf dem vollständigen Modell
#            (H1 + H2a + H2b) zur Identifikation von Ausreißern
#            über die Hampel-Distanz (Cut-off: Hampel-Distanz > 5)
#
# Voraussetzung: 01_deskriptive_statistik.R wurde bereits ausgeführt,
#                sodass das Objekt "Dataset" im Workspace vorhanden ist.
##############################################################################

# install.packages("robustbase")
library(dplyr)
library(robustbase)

# ---- 1. Volles Modell (H1 + H2a + H2b) mittels MM-Schätzer -----------------
# lmrob() schätzt standardmäßig einen MM-Schätzer: hoher Bruchpunkt (Start
# über S-Schätzer) kombiniert mit hoher Effizienz (M-Schritt) -> robust
# gegenüber Ausreißern UND Hebelpunkten. HB01_ord (geordneter Faktor) wird
# wie in lm() über seine Polynomkontraste in die Modellmatrix aufgenommen.
modell_mm <- lmrob(Arbeitszufriedenheit ~ HB01_ord + wFoMO_informational + wFoMO_relational,
                    data = Dataset)

cat("\n=== Robuste MM-Regression (robustbase::lmrob) - volles Modell ===\n")
print(summary(modell_mm))

# ---- 2. Vergleich KQ- vs. MM-robuste Koeffizienten -------------------------
modell_kq_voll <- lm(Arbeitszufriedenheit ~ HB01_ord + wFoMO_informational + wFoMO_relational,
                      data = Dataset)

vergleich_koef <- data.frame(
  Praediktor = names(coef(modell_kq_voll)),
  KQ         = round(coef(modell_kq_voll), 4),
  MM_robust  = round(coef(modell_mm), 4)
)
cat("\n=== Vergleich: KQ-Koeffizienten vs. MM-robuste Koeffizienten ===\n")
print(vergleich_koef)

# ---- 3. Ausreißeridentifikation über die Hampel-Distanz --------------------
# Die Hampel-Distanz ist ein robustes Analogon zum z-Wert: Sie basiert auf
# Median und MAD (Median Absolute Deviation) statt auf Mittelwert und
# Standardabweichung und ist dadurch selbst nicht durch Ausreißer verzerrt.
#
#   Hampel-Distanz_i = | resid_i - median(resid) | / (1.4826 * MAD(resid))
#
# Cut-off (Vorgabe): Hampel-Distanz > 5 => Fall wird als Ausreißer eingestuft.
resid_mm     <- residuals(modell_mm)
median_resid <- median(resid_mm)
mad_resid    <- mad(resid_mm, constant = 1.4826)   # 1.4826 = Konsistenzkonstante für Normalverteilung

hampel_distanz <- abs(resid_mm - median_resid) / mad_resid

schwelle_hampel <- 5

diagnose_tab <- Dataset %>%
  transmute(CASE,
            residuum       = round(resid_mm, 3),
            hampel_distanz = round(hampel_distanz, 2)) %>%
  arrange(desc(hampel_distanz))

cat("\n=== Die 15 Fälle mit den höchsten Hampel-Distanzen ===\n")
print(head(diagnose_tab, 15))

ausreisser_hampel <- diagnose_tab %>% filter(hampel_distanz > schwelle_hampel)

cat("\nAnzahl Fälle mit Hampel-Distanz >", schwelle_hampel, ":",
    nrow(ausreisser_hampel), "von", nrow(Dataset), "\n")

cat("\n=== Als Ausreißer identifizierte Fälle (Hampel-Distanz > ",
    schwelle_hampel, ") ===\n", sep = "")
if (nrow(ausreisser_hampel) > 0) {
  print(ausreisser_hampel)
} else {
  cat("Keine Fälle überschreiten den Schwellenwert.\n")
}

# ---- 4. Diagnoseplot: Hampel-Distanz je Fall -------------------------------
plot(hampel_distanz, pch = 19,
     col = ifelse(hampel_distanz > schwelle_hampel, "red", "grey50"),
     xlab = "Fall (Index)", ylab = "Hampel-Distanz",
     main = "Ausreißerdiagnose (MM-Regression, volles Modell): Hampel-Distanz")
abline(h = schwelle_hampel, lty = 2, col = "red")

if (nrow(ausreisser_hampel) > 0) {
  idx_ausreisser <- which(hampel_distanz > schwelle_hampel)
  text(idx_ausreisser, hampel_distanz[idx_ausreisser],
       labels = Dataset$CASE[idx_ausreisser],
       pos = 3, cex = 0.7, col = "red")
}
