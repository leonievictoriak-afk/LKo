##############################################################################
# Bachelorarbeit: Hybrides Arbeiten, Workplace FoMO (wFoMO) und
#                 Arbeitszufriedenheit im öffentlichen Dienst
#
# Schritt 1: Datenimport, Datenaufbereitung, Fallauswahl und
#            deskriptive Statistik (relative Häufigkeiten, M, SD, Median,
#            Minimum, Maximum)
#
# Datenquelle: SoSci Survey Export "Umfragewerte_BA_4.xlsx"
#              Sheet "arbeit-oeffentlicher-dienst"
##############################################################################

# ---- 0. Pakete -------------------------------------------------------------
# install.packages(c("readxl", "dplyr", "tidyr", "psych"))
library(readxl)
library(dplyr)
library(tidyr)
library(psych)

# ---- 1. Daten einlesen ------------------------------------------------------
# Pfad ggf. anpassen (Datei im Arbeitsverzeichnis bzw. im Unterordner "data/")
pfad <- "data/Umfragewerte_BA_4.xlsx"

# Zeile 1 enthält die Variablennamen (Header), Zeile 2 enthält die
# ausgeschriebenen Fragetexte (Item-Labels) und KEINE echten Falldaten.
# Deshalb wird der Header separat aus Zeile 1 gelesen (n_max = 0 liest keine
# Datenzeilen ein, nur die Spaltennamen) und anschließend beim Einlesen der
# eigentlichen Falldaten (ab Zeile 3, also skip = 2) explizit zugewiesen.
# Dadurch landet die Label-Zeile nie im Datensatz, und read_excel() erkennt
# die Spaltentypen (numerisch vs. Text) korrekt anhand der echten Werte.
header <- names(read_excel(pfad, n_max = 0))
rohdaten <- read_excel(pfad, skip = 2, col_names = header)

# ---- 2. Fallauswahl (Filterung) --------------------------------------------
# a) nur Fragebogenversion "LKo" (nicht z. B. Interview-Testfragebögen)
# b) nur vollständig beantwortete Interviews (STATUS == "complete")
# c) anschließend zusätzlich alle Personen mit SD02 (Geschlecht) == 4
#    ("keine Angabe") ausschließen
daten <- rohdaten %>%
  filter(QUESTNNR == "LKo") %>%
  filter(STATUS == "complete") %>%
  filter(SD02 != 4)

n_gesamt   <- nrow(rohdaten)
n_lko      <- rohdaten %>% filter(QUESTNNR == "LKo") %>% nrow()
n_complete <- rohdaten %>%
  filter(QUESTNNR == "LKo", STATUS == "complete") %>%
  nrow()
n_final <- nrow(daten)

cat("Fälle gesamt (Rohdatensatz):                     ", n_gesamt, "\n")
cat("... davon Fragebogen 'LKo':                       ", n_lko, "\n")
cat("... davon vollständig beantwortet (STATUS complete):", n_complete, "\n")
cat("... davon ohne 'keine Angabe' bei Geschlecht (final N):", n_final, "\n")

# ---- 3. Aufbereitung der Variablen -----------------------------------------

## 3.1 Soziodemografie -------------------------------------------------------
# Alter (SD01) und Arbeitszeit (SD03_01) sind bereits numerisch.
# Berufserfahrung (SD04_01) wurde als Text mit Komma als Dezimaltrennzeichen
# erhoben (z. B. "2,5") -> vor der Umwandlung wird das Komma durch einen
# Punkt ersetzt.
daten <- daten %>%
  mutate(
    Alter                  = as.numeric(SD01),
    Geschlecht             = factor(SD02, levels = c(1, 2, 3),
                                     labels = c("weiblich", "männlich", "divers")),
    Arbeitszeit            = as.numeric(SD03_01),      # in %, 100 = Vollzeit
    Berufserfahrung        = as.numeric(gsub(",", ".", SD04_01, fixed = TRUE)),  # in Jahren
    taetigkeitsbereich     = factor(SD05, levels = 1:6,
                                     labels = c("Kommunalverwaltung",
                                                "Landesbehörde",
                                                "Bundesbehörde",
                                                "Bildung (Schule/Hochschule)",
                                                "Gesundheit/Soziales",
                                                "Sonstiger öffentlicher Dienst")),
    Personalverantwortung = factor(SD06, levels = c(1, 2), labels = c("ja", "nein"))
  )

# Kontrolle: Anzahl Fälle, bei denen die Umwandlung von Berufserfahrung
# (Komma -> Punkt -> numerisch) zu NA geführt hat, obwohl SD04_01 ursprünglich
# nicht leer war (z. B. weil ein Bruch wie "1 1/2" eingegeben wurde). Diese
# Fälle sollten manuell geprüft werden.
n_berufserfahrung_na <- sum(is.na(daten$Berufserfahrung) & !is.na(daten$SD04_01))
cat("\nBerufserfahrung: nicht-numerisch konvertierbare, nicht-leere Angaben:",
    n_berufserfahrung_na, "\n")

## 3.2 Homeoffice-Möglichkeit / -Nutzung (HB01/HB02) -------------------------
# HB01 und HB02 wurden mit 11 Antwortkategorien ("0 Tage" bis "5 Tage")
# KATEGORIAL erhoben. Da die Kategorien nicht als gleichabständig angenommen
# werden können (z. B. ist der Abstand zwischen "0 Tage" und "0-1 Tag" nicht
# zwingend gleich groß wie zwischen "4-5 Tage" und "5 Tage"), werden HB01_kat
# und HB02_kat als reine Faktorvariablen gebildet (NICHT in eine metrische
# "Tage"-Variable umgerechnet). In Regressionsmodellen werden sie dadurch
# automatisch über Dummy-Kodierung (Referenzkategorie "0 Tage") berücksichtigt,
# ohne eine bestimmte Skalierung zwischen den Kategorien zu unterstellen.
hb_labels <- c("0 Tage", "0-1 Tag", "1 Tag", "1-2 Tage", "2 Tage",
               "2-3 Tage", "3 Tage", "3-4 Tage", "4 Tage", "4-5 Tage", "5 Tage")

daten <- daten %>%
  mutate(
    HB01_kat = factor(HB01, levels = 1:11, labels = hb_labels),
    HB02_kat = factor(HB02, levels = 1:11, labels = hb_labels)
  )

## 3.3 Workplace Fear of Missing Out (wFoMO-G, Ebner et al.) -----------------
# Informationale Subskala (Sorge, wichtige Informationen zu verpassen):
#   FM01_01 - FM01_05
# Relationale Subskala (Sorge, geschäftliche Beziehungen/Kontakte zu
# verpassen):
#   FM01_06 - FM01_10
# (Zuordnung entspricht Table A1 in Ebner et al., Applied Psychology, 2026)
daten <- daten %>%
  mutate(
    wFoMO_informational = rowMeans(across(FM01_01:FM01_05), na.rm = FALSE),
    wFoMO_relational     = rowMeans(across(FM01_06:FM01_10), na.rm = FALSE)
  )

## 3.4 Arbeitszufriedenheit ---------------------------------------------------
# Zeilenmittelwert aus AZ01_01 - AZ01_06 (Kernskala, verpflichtend) sowie
# AZ02_01 (Zufriedenheit mit Mitarbeitenden; nur bei Führungsverantwortung)
# und AZ03_01 (Zufriedenheit mit Kundinnen/Kunden; nur bei Kundenkontakt).
#
# ACHTUNG: Die Items wurden auf einer Skala von -3 bis +3 erfasst (nicht
# 1 bis 7 wie in einer früheren Skriptversion angenommen).
#
# WICHTIGE KONSEQUENZ (na.rm = FALSE): Da AZ02_01 und AZ03_01 nur von einem
# Teil der Stichprobe (Führungskräfte bzw. Personen mit Kundenkontakt)
# beantwortet wurden, ergibt der Zeilenmittelwert für alle anderen Fälle NA.
# Die Variable "Arbeitszufriedenheit" ist dadurch nur für die Teilstichprobe
# definiert, die BEIDE optionalen Items zusätzlich beantwortet hat; lm() etc.
# schließen die übrigen Fälle in den späteren Regressionsmodellen automatisch
# per Fallausschluss (listwise deletion) aus. Die resultierende Stichprobe für
# die Regressionsanalyse (Schritt 3 ff.) ist daher deutlich kleiner als N_final
# oben. Bitte in Schritt 3 die tatsächliche Fallzahl (Zeile "Residual standard
# error: ... on X degrees of freedom" bzw. n in summary()) kontrollieren.
daten <- daten %>%
  mutate(
    Arbeitszufriedenheit = rowMeans(
      across(c(AZ01_01, AZ01_02, AZ01_03, AZ01_04, AZ01_05, AZ01_06, AZ02_01, AZ03_01)),
      na.rm = FALSE
    )
  )

n_az_gueltig <- sum(!is.na(daten$Arbeitszufriedenheit))
cat("\nArbeitszufriedenheit (8-Item-Mittelwert) gültig (nicht NA) für:",
    n_az_gueltig, "von", n_final, "Fällen\n")
cat("(Grund: AZ02_01 und AZ03_01 wurden nur von einem Teil der Stichprobe\n")
cat(" beantwortet; siehe Kommentar oben.)\n")

# ---- 4. Deskriptive Statistik ----------------------------------------------

## 4.1 Relative Häufigkeiten kategorialer Variablen --------------------------
haeufigkeitstabelle <- function(var) {
  daten %>%
    filter(!is.na({{ var }})) %>%
    count({{ var }}, name = "n") %>%
    mutate(Prozent = round(100 * n / sum(n), 1))
}

cat("\n--- Geschlecht ---\n")
print(haeufigkeitstabelle(Geschlecht))

cat("\n--- Tätigkeitsbereich ---\n")
print(haeufigkeitstabelle(taetigkeitsbereich))

cat("\n--- Personalverantwortung ---\n")
print(haeufigkeitstabelle(Personalverantwortung))

cat("\n--- Homeoffice-Möglichkeit (kategorial, HB01_kat) ---\n")
print(haeufigkeitstabelle(HB01_kat))

cat("\n--- Homeoffice-Nutzung (kategorial, HB02_kat) ---\n")
print(haeufigkeitstabelle(HB02_kat))

## 4.2 M, SD, Median, Min, Max metrischer Variablen --------------------------
metrische_variablen <- daten %>%
  select(Alter, Berufserfahrung, Arbeitszeit,
         wFoMO_informational, wFoMO_relational, Arbeitszufriedenheit)

deskriptiv <- psych::describe(metrische_variablen) %>%
  as.data.frame() %>%
  select(n, mean, sd, median, min, max)

cat("\n--- Deskriptive Statistik: metrische Variablen ---\n")
cat("(n bei Arbeitszufriedenheit < N_final, siehe Hinweis in Abschnitt 3.4)\n")
print(round(deskriptiv, 2))

## 4.3 Optionale Arbeitszufriedenheits-Items (deskriptiv, nur Antwortende) ---
az_optional <- daten %>%
  select(AZ02_01, AZ03_01) %>%
  psych::describe() %>%
  as.data.frame() %>%
  select(n, mean, sd, median, min, max)

cat("\n--- Deskriptive Statistik: optionale AZ-Items (nur Antwortende) ---\n")
cat("AZ02_01 = Zufriedenheit mit Mitarbeitenden (nur Führungsverantwortung)\n")
cat("AZ03_01 = Zufriedenheit mit Kundinnen/Kunden (nur Kundenkontakt)\n")
print(round(az_optional, 2))
