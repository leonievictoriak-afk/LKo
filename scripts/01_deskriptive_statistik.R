##############################################################################
# Bachelorarbeit: Hybrides Arbeiten, Workplace FoMO (wFoMO) und
#                 Arbeitszufriedenheit im oeffentlichen Dienst
#
# Schritt 1: Datenimport, Fallauswahl, Variablenbildung und
#            deskriptive Statistik
#
# angelehnt an das Modul-Skript "RSkript_robuste_Regression_und_Hampel_und_
# stepupdown_Prozedur" (Datenimport mittels openxlsx)
#
# Datenquelle: SoSci-Survey-Export "Umfragewerte_BA_4.xlsx"
##############################################################################

# ---- 0. Pakete --------------------------------------------------------------
# install.packages(c("openxlsx", "psych"))
library(openxlsx)
library(psych)

# ---- 1. Datenimport ----------------------------------------------------------
# WICHTIG: Zeile 2 der Exceldatei enthaelt die ausgeschriebenen Fragetexte
# (Item-Labels) und KEINE echten Falldaten. Diese Zeile darf nicht als
# Datensatz eingelesen werden. Deshalb wird der Header separat aus Zeile 1
# gelesen und den Falldaten (ab Zeile 3) als Spaltennamen zugewiesen.
filepath <- "data/Umfragewerte_BA_4.xlsx"

header  <- read.xlsx(filepath, sheet = 1, rows = 1, colNames = FALSE)
Dataset <- read.xlsx(filepath, sheet = 1, startRow = 3, colNames = FALSE)
colnames(Dataset) <- as.character(unlist(header[1, ]))

cat("Eingelesene Faelle (Rohdatensatz, vor Filterung):", nrow(Dataset), "\n")
cat("Anzahl Spalten:                                  ", ncol(Dataset), "\n")

# ---- 2. Fallauswahl -----------------------------------------------------------
n_gesamt <- nrow(Dataset)

# a) nur Fragebogenversion "LKo"; b) nur vollstaendig abgeschlossene Interviews
Dataset <- Dataset[Dataset$QUESTNNR == "LKo" & Dataset$STATUS == "complete", ]
n_lko_complete <- nrow(Dataset)

# c) Faelle mit SD02 (Geschlecht) == 4 ("keine Angabe") ausschliessen
Dataset$SD02 <- as.numeric(Dataset$SD02)
Dataset <- Dataset[Dataset$SD02 != 4, ]
n_final <- nrow(Dataset)

cat("\nFaelle gesamt (Rohdatensatz):                       ", n_gesamt, "\n")
cat("... davon QUESTNNR == 'LKo' & STATUS == 'complete':   ", n_lko_complete, "\n")
cat("... davon ohne SD02 == 4 ('keine Angabe') (finales N):", n_final, "\n")

# ---- 3. Variablenbildung ------------------------------------------------------

## 3.1 Workplace FoMO (wFoMO) ---------------------------------------------------
fomo_informational_items <- c("FM01_01", "FM01_02", "FM01_03", "FM01_04", "FM01_05")
fomo_relational_items    <- c("FM01_06", "FM01_07", "FM01_08", "FM01_09", "FM01_10")

Dataset[fomo_informational_items] <- lapply(Dataset[fomo_informational_items], as.numeric)
Dataset[fomo_relational_items]    <- lapply(Dataset[fomo_relational_items], as.numeric)

Dataset$wFoMO_informational <- rowMeans(Dataset[fomo_informational_items], na.rm = TRUE)
Dataset$wFoMO_relational    <- rowMeans(Dataset[fomo_relational_items], na.rm = TRUE)

## 3.2 Arbeitszufriedenheit (IAZ-K-Skala, rekodiert auf -3 bis +3) --------------
# Rohskala: 1 (extrem unzufrieden) bis 7 (extrem zufrieden).
# AZ01_01 - AZ01_06 sind Pflichtitems, AZ02_01 und AZ03_01 sind freiwillig
# (nicht von allen Personen beantwortet). Der Skalenwert wird ueber alle
# tatsaechlich beantworteten Items gemittelt (na.rm = TRUE).
az_items <- c("AZ01_01", "AZ01_02", "AZ01_03", "AZ01_04", "AZ01_05", "AZ01_06",
              "AZ02_01", "AZ03_01")
Dataset[az_items] <- lapply(Dataset[az_items], as.numeric)

az_items_rekodiert <- paste0(az_items, "_r")
Dataset[az_items_rekodiert] <- lapply(Dataset[az_items], function(x) x - 4)

Dataset$Arbeitszufriedenheit <- rowMeans(Dataset[az_items_rekodiert], na.rm = TRUE)

# Interpretation gemaess IAZ-K-Handbuch:
#   -3.00 bis -0.51 = unzufrieden | -0.50 bis +0.50 = neutral | +0.51 bis +3.00 = zufrieden
# (Grenzen bei +/-0.505 gesetzt, damit der kontinuierliche Skalenwert korrekt
# auf die auf zwei Nachkommastellen gerundeten Interpretationsbereiche faellt.)
Dataset$Arbeitszufriedenheit_kat <- cut(
  Dataset$Arbeitszufriedenheit,
  breaks = c(-Inf, -0.505, 0.505, Inf),
  labels = c("unzufrieden", "neutral", "zufrieden")
)

## 3.3 Homeoffice-Moeglichkeit / -Nutzung (kategorial) ---------------------------
Dataset$HB01_kat <- factor(Dataset$HB01)
Dataset$HB02_kat <- factor(Dataset$HB02)

# Zusaetzlich fuer die Regressionsmodelle (Schritt 3 ff.): HB01 hat eine
# natuerliche Rangordnung (0 Tage < 0-1 Tag < ... < 5 Tage). HB01_ord bildet
# diese Ordnung als geordneter Faktor ab (weiterhin kategorial, KEINE
# Tage-Metrik/Abstandsannahme wie bei einer numerischen Naeherung), damit die
# gerichteten Hypothesen H1 und H3a/H3b ueber den linearen Polynomkontrast
# (".L") interpretierbar bleiben. HB01_kat (ungeordnet) bleibt unveraendert
# fuer die deskriptive Haeufigkeitsauszaehlung erhalten.
Dataset$HB01_ord <- factor(Dataset$HB01, ordered = TRUE)

# ---- 4. Deskriptive Statistik --------------------------------------------------

## 4.1 Metrische Variablen: M, SD, Median, Min, Max -------------------------------
metrische_variablen <- Dataset[, c("wFoMO_informational", "wFoMO_relational",
                                    "Arbeitszufriedenheit")]

deskriptiv <- psych::describe(metrische_variablen)[, c("n", "mean", "sd", "median", "min", "max")]

cat("\n--- Deskriptive Statistik: metrische Variablen ---\n")
print(round(deskriptiv, 2))

## 4.2 Kategoriale Variablen: absolute & relative Haeufigkeiten -------------------
haeufigkeitstabelle <- function(x) {
  tab <- table(x, useNA = "no")
  data.frame(
    Auspraegung = names(tab),
    n = as.integer(tab),
    Prozent = round(100 * as.integer(tab) / sum(tab), 1)
  )
}

cat("\n--- Arbeitszufriedenheit (kategorisiert) ---\n")
print(haeufigkeitstabelle(Dataset$Arbeitszufriedenheit_kat))

cat("\n--- Homeoffice-Moeglichkeit (HB01_kat) ---\n")
print(haeufigkeitstabelle(Dataset$HB01_kat))

cat("\n--- Homeoffice-Nutzung (HB02_kat) ---\n")
print(haeufigkeitstabelle(Dataset$HB02_kat))

cat("\n--- Geschlecht (SD02) ---\n")
print(haeufigkeitstabelle(factor(Dataset$SD02, levels = c(1, 2, 3),
                                  labels = c("weiblich", "maennlich", "divers"))))

## 4.3 Antwortquote der freiwilligen Arbeitszufriedenheits-Items ------------------
cat("\n--- Antwortquote freiwillige AZ-Items (von N =", n_final, ") ---\n")
cat("AZ02_01 (Zufriedenheit mit Mitarbeitenden):", sum(!is.na(Dataset$AZ02_01)), "\n")
cat("AZ03_01 (Zufriedenheit mit Kundinnen/Kunden):", sum(!is.na(Dataset$AZ03_01)), "\n")
