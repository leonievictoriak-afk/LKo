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
pfad  <- "data/Umfragewerte_BA_4.xlsx"
sheet <- "arbeit-oeffentlicher-dienst"

# Zeile 1 enthält die Spaltennamen (Variablencodes), Zeile 2 die
# ausgeschriebenen Fragetexte (keine echten Antworten, sondern nur Labels).
# Deshalb werden die Spaltennamen separat aus Zeile 1 gelesen und die
# eigentlichen Falldaten erst ab Zeile 3 eingelesen (Zeile 2 wird komplett
# übersprungen).
spaltennamen <- names(read_excel(pfad, sheet = sheet, n_max = 0))
rohdaten <- read_excel(pfad, sheet = sheet, skip = 2, col_names = spaltennamen)

# Sicherheitsprüfung: CASE muss bei echten Fällen numerisch sein. Sollten
# durch fehlerhafte Zeilen dennoch NAs auftreten, werden diese entfernt.
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
# b) nur vollständig beantwortete Interviews (STATUS == "complete")
# c) Fälle ohne Angabe zum Geschlecht ausschließen
#    (SD02: 1 = weiblich, 2 = männlich, 3 = divers, 4 = keine Angabe)
daten <- rohdaten %>%
  filter(QUESTNNR == "LKo") %>%
  filter(STATUS == "complete") %>%
  filter(!is.na(SD02), SD02 != 4)

n_gesamt   <- nrow(rohdaten)
n_lko      <- rohdaten %>% filter(QUESTNNR == "LKo") %>% nrow()
n_complete <- rohdaten %>%
  filter(QUESTNNR == "LKo", STATUS == "complete") %>%
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
    Alter                  = as.numeric(SD01),
    Geschlecht             = factor(SD02, levels = c(1, 2, 3),
                                     labels = c("weiblich", "männlich", "divers")),
    Arbeitszeit            = as.numeric(SD03_01),      # Beschäftigungsumfang in %, 100 = Vollzeit
    # SD04_01 wurde als Text erhoben; einzelne Werte nutzen ein Komma als
    # Dezimaltrennzeichen (z. B. "2,5") -> vor der Umwandlung ersetzen.
    Berufserfahrung        = as.numeric(gsub(",", ".", SD04_01, fixed = TRUE)),  # in Jahren
    taetigkeitsbereich     = factor(SD05, levels = 1:6,
                                     labels = c("Kommunalverwaltung",
                                                "Landesbehörde",
                                                "Bundesbehörde",
                                                "Bildung (Schule/Hochschule)",
                                                "Gesundheit/Soziales",
                                                "Sonstiger öffentlicher Dienst")),
    Personalverantwortung  = factor(SD06, levels = c(1, 2), labels = c("ja", "nein"))
  )

## 3.2 Homeoffice-Möglichkeit / -Nutzung (HB01/HB02) -------------------------
# Beide Variablen wurden KATEGORIAL erhoben (11 Antwortkategorien von
# "0 Tage" bis "5 Tage") und werden als Faktoren gebildet (HB01_kat,
# HB02_kat; für relative Häufigkeiten). Zusätzlich wird eine numerische
# Näherung über die Kategorienmitte gebildet (HB01_tage, HB02_tage), die
# für die spätere Regressionsanalyse (H1: Möglichkeit zum hybriden
# Arbeiten) als metrischer Prädiktor benötigt wird.
hb_labels <- c("0 Tage", "0-1 Tag", "1 Tag", "1-2 Tage", "2 Tage",
               "2-3 Tage", "3 Tage", "3-4 Tage", "4 Tage", "4-5 Tage", "5 Tage")
hb_mitte  <- c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)

daten <- daten %>%
  mutate(
    HB01_kat  = factor(HB01, levels = 1:11, labels = hb_labels, ordered = TRUE),
    HB02_kat  = factor(HB02, levels = 1:11, labels = hb_labels, ordered = TRUE),
    HB01_tage = hb_mitte[HB01],
    HB02_tage = hb_mitte[HB02]
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
    Informationale_wFoMO = rowMeans(across(FM01_01:FM01_05), na.rm = FALSE),
    Relationale_wFoMO    = rowMeans(across(FM01_06:FM01_10), na.rm = FALSE)
  )

## 3.4 Arbeitszufriedenheit ---------------------------------------------------
# Alle acht AZ-Items (AZ01_01 - AZ01_06, AZ02_01, AZ03_01) sind 7-stufig
# (1 = sehr unzufrieden ... 7 = sehr zufrieden) erhoben. Für die Analyse wird
# die Rohskala 1-7 durch Zentrierung um den neutralen Mittelpunkt (4) auf
# -3 bis +3 umgerechnet (Item - 4).
# AZ02_01 (Zufriedenheit mit Mitarbeitenden) und AZ03_01 (Zufriedenheit mit
# Kundinnen/Kunden) wurden nur den jeweils relevanten Teilgruppen
# (Führungsverantwortung bzw. Kundenkontakt) vorgelegt und sind daher bei
# den übrigen Fällen NA. Damit die Gesamtstichprobe (N = 464) für die
# Regression erhalten bleibt, wird der Mittelwert über die tatsächlich
# beantworteten Items gebildet (na.rm = TRUE): für die meisten Fälle also
# über die 6 Pflichtitems, für die jeweiligen Teilgruppen zusätzlich über
# 7 bzw. 8 Items.
az_items_umkodiert <- c("AZ01_01", "AZ01_02", "AZ01_03", "AZ01_04", "AZ01_05", "AZ01_06",
                         "AZ02_01", "AZ03_01")

daten <- daten %>%
  mutate(across(all_of(az_items_umkodiert), ~ . - 4, .names = "{.col}_m4")) %>%
  mutate(
    Arbeitszufriedenheit = rowMeans(across(paste0(az_items_umkodiert, "_m4")), na.rm = TRUE)
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
         HB01_tage, HB02_tage,
         Informationale_wFoMO, Relationale_wFoMO, Arbeitszufriedenheit)

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
