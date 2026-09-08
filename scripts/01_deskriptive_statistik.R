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
rohdaten <- read_excel(pfad, sheet = "arbeit-oeffentlicher-dienst")

# Zeile 2 der Exportdatei enthält die Itemformulierungen (Fragetexte) und
# keine echten Falldaten -> diese Zeile wird entfernt. CASE ist bei echten
# Fällen immer numerisch, bei der Label-Zeile hingegen Text -> wird zu NA.
rohdaten <- rohdaten %>%
  mutate(CASE = suppressWarnings(as.numeric(CASE))) %>%
  filter(!is.na(CASE))

# Je nach Zellformatierung im Excel-Export liest read_excel() einzelne
# Item-/Antwortspalten mitunter als Text statt als Zahl ein (das führt u. a.
# zu Fehlern wie "'x' must be numeric or complex" bei rowMeans() oder zu
# stillen Fehlzuordnungen bei Indizierungen wie hb_mitte[HB01]). Deshalb
# werden alle relevanten Item-Spalten hier zentral in numerische Werte
# umgewandelt, bevor sie weiterverarbeitet werden.
item_spalten <- c(
  paste0("AZ01_0", 1:6), "AZ02_01", "AZ03_01",
  paste0("FM01_0", 1:9), "FM01_10",
  "HB01", "HB02",
  paste0("HB03_0", 1:5), paste0("HB04_0", 1:5)
)

rohdaten <- rohdaten %>%
  mutate(across(all_of(item_spalten), ~ suppressWarnings(as.numeric(.))))

# Kontrolle: Sollten die Pflicht-Items (AZ01_xx, FM01_xx, HB01/HB02) nach der
# Umwandlung NAs enthalten, deutet das auf untypische Zeichen in der
# Originaldatei hin (z. B. Komma statt Punkt, Leerzeichen, Text) und sollte
# geprüft werden.
na_check <- rohdaten %>%
  filter(QUESTNNR == "LKo") %>%
  summarise(across(all_of(item_spalten), ~ sum(is.na(.))))
cat("\nAnzahl NA je Item-Spalte nach numerischer Umwandlung (sollte für\n")
cat("AZ01_xx, FM01_xx, HB01, HB02 = 0 sein):\n")
print(na_check)

# ---- 2. Fallauswahl (Filterung) --------------------------------------------
# a) nur Fragebogenversion "LKo" (nicht z. B. Interview-Testfragebögen)
# b) nur vollständig beantwortete Interviews (STATUS == "complete" und
#    FINISHED == 1, d. h. letzte Seite wurde erreicht)
# c) Fälle ohne Angabe zum Geschlecht ausschließen
#    (SD02: 1 = weiblich, 2 = männlich, 3 = divers, 4 = keine Angabe)
daten <- rohdaten %>%
  filter(QUESTNNR == "LKo") %>%
  filter(STATUS == "complete", FINISHED == 1) %>%
  filter(!is.na(SD02), SD02 != 4)

n_gesamt   <- nrow(rohdaten)
n_lko      <- rohdaten %>% filter(QUESTNNR == "LKo") %>% nrow()
n_complete <- rohdaten %>%
  filter(QUESTNNR == "LKo", STATUS == "complete", FINISHED == 1) %>%
  nrow()
n_final <- nrow(daten)

cat("Fälle gesamt (Rohdatensatz):                     ", n_gesamt, "\n")
cat("... davon Fragebogen 'LKo':                       ", n_lko, "\n")
cat("... davon vollständig beantwortet:                ", n_complete, "\n")
cat("... davon mit gültiger Geschlechtsangabe (final N):", n_final, "\n")

# ---- 3. Aufbereitung der Variablen -----------------------------------------

## 3.1 Soziodemografie -------------------------------------------------------
daten <- daten %>%
  mutate(
    alter                  = as.numeric(SD01),
    geschlecht             = factor(SD02, levels = c(1, 2, 3),
                                     labels = c("weiblich", "männlich", "divers")),
    beschaeftigungsumfang  = as.numeric(SD03_01),      # in %, 100 = Vollzeit
    # SD04_01 wurde als Text erhoben; einzelne Werte nutzen ein Komma als
    # Dezimaltrennzeichen (z. B. "2,5") -> vor der Umwandlung ersetzen.
    berufserfahrung        = as.numeric(gsub(",", ".", SD04_01, fixed = TRUE)),  # in Jahren
    taetigkeitsbereich     = factor(SD05, levels = 1:6,
                                     labels = c("Kommunalverwaltung",
                                                "Landesbehörde",
                                                "Bundesbehörde",
                                                "Bildung (Schule/Hochschule)",
                                                "Gesundheit/Soziales",
                                                "Sonstiger öffentlicher Dienst")),
    fuehrungsverantwortung = factor(SD06, levels = c(1, 2), labels = c("ja", "nein"))
  )

## 3.2 Homeoffice-Möglichkeit / -Nutzung (HB01/HB02) -------------------------
# Beide Variablen wurden KATEGORIAL erhoben (11 Antwortkategorien von
# "0 Tage" bis "5 Tage"), daher primär als Faktor auswerten (relative
# Häufigkeiten). Zusätzlich wird eine numerische Näherung über die
# Kategorienmitte gebildet, die für die spätere Regressionsanalyse
# (H1: Möglichkeit zum hybriden Arbeiten) benötigt wird.
hb_labels <- c("0 Tage", "0-1 Tag", "1 Tag", "1-2 Tage", "2 Tage",
               "2-3 Tage", "3 Tage", "3-4 Tage", "4 Tage", "4-5 Tage", "5 Tage")
hb_mitte  <- c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)

daten <- daten %>%
  mutate(
    hb_moeglichkeit_kat  = factor(HB01, levels = 1:11, labels = hb_labels, ordered = TRUE),
    hb_nutzung_kat       = factor(HB02, levels = 1:11, labels = hb_labels, ordered = TRUE),
    hb_moeglichkeit_tage = hb_mitte[HB01],
    hb_nutzung_tage      = hb_mitte[HB02]
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
    wfomo_informational = rowMeans(across(FM01_01:FM01_05), na.rm = FALSE),
    wfomo_relational     = rowMeans(across(FM01_06:FM01_10), na.rm = FALSE)
  )

## 3.4 Arbeitszufriedenheit ---------------------------------------------------
# Kernskala: 6 verpflichtende Items (AZ01_01 - AZ01_06); bei allen Fällen
# vollständig beantwortet.
# AZ02_01 (Zufriedenheit mit Mitarbeitenden; nur bei Führungsverantwortung)
# und AZ03_01 (Zufriedenheit mit Kundinnen/Kunden; nur bei Kundenkontakt)
# waren freiwillig zu beantworten. Sie fließen NICHT in den Summenscore der
# Kernskala ein, sondern werden separat für die jeweilige Subgruppe
# deskriptiv ausgewertet (siehe Abschnitt 4.3).
daten <- daten %>%
  mutate(
    az_kern = rowMeans(across(AZ01_01:AZ01_06), na.rm = FALSE)
  )

# ---- 4. Deskriptive Statistik ----------------------------------------------

## 4.1 Relative Häufigkeiten kategorialer Variablen --------------------------
haeufigkeitstabelle <- function(var) {
  daten %>%
    filter(!is.na({{ var }})) %>%
    count({{ var }}, name = "n") %>%
    mutate(Prozent = round(100 * n / sum(n), 1))
}

cat("\n--- Geschlecht ---\n")
print(haeufigkeitstabelle(geschlecht))

cat("\n--- Tätigkeitsbereich ---\n")
print(haeufigkeitstabelle(taetigkeitsbereich))

cat("\n--- Führungsverantwortung ---\n")
print(haeufigkeitstabelle(fuehrungsverantwortung))

cat("\n--- Homeoffice-Möglichkeit (kategorial, HB01) ---\n")
print(haeufigkeitstabelle(hb_moeglichkeit_kat))

cat("\n--- Homeoffice-Nutzung (kategorial, HB02) ---\n")
print(haeufigkeitstabelle(hb_nutzung_kat))

## 4.2 M, SD, Median, Min, Max metrischer Variablen --------------------------
metrische_variablen <- daten %>%
  select(alter, berufserfahrung, beschaeftigungsumfang,
         hb_moeglichkeit_tage, hb_nutzung_tage,
         wfomo_informational, wfomo_relational, az_kern)

deskriptiv <- psych::describe(metrische_variablen) %>%
  as.data.frame() %>%
  select(n, mean, sd, median, min, max)

cat("\n--- Deskriptive Statistik: metrische Variablen ---\n")
print(round(deskriptiv, 2))

## 4.3 Optionale Arbeitszufriedenheits-Items ---------------------------------
# Deskriptivstatistik nur auf Basis der Personen, die das jeweilige Item
# tatsächlich beantwortet haben (nicht-fehlende Werte).
az_optional <- daten %>%
  select(AZ02_01, AZ03_01) %>%
  psych::describe() %>%
  as.data.frame() %>%
  select(n, mean, sd, median, min, max)

cat("\n--- Deskriptive Statistik: optionale AZ-Items (nur Antwortende) ---\n")
cat("AZ02_01 = Zufriedenheit mit Mitarbeitenden (nur Führungsverantwortung)\n")
cat("AZ03_01 = Zufriedenheit mit Kundinnen/Kunden (nur Kundenkontakt)\n")
print(round(az_optional, 2))
