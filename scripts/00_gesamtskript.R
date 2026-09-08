##############################################################################
# Bachelorarbeit: Hybrides Arbeiten, Workplace FoMO (wFoMO) und
#                 Arbeitszufriedenheit im öffentlichen Dienst
#
# GESAMTSKRIPT - alles in einer Datei, von Datenimport bis Moderationsanalyse
#
# ANLEITUNG:
#   1. Diese Datei in RStudio öffnen.
#   2. Unten bei "pfad <-" den Speicherort deiner Excel-Datei eintragen
#      (z. B. "C:/Users/DeinName/Documents/Umfragewerte_BA_4.xlsx" oder,
#      wenn die Datei im Unterordner "data" dieses Projekts liegt:
#      "data/Umfragewerte_BA_4.xlsx"). WICHTIG: Schrägstriche "/" verwenden,
#      keine Backslashes "\".
#   3. Das gesamte Skript ausführen (in RStudio: Strg+Alt+R bzw. Button
#      "Source", bzw. Zeile für Zeile mit Strg+Enter durchgehen).
#
# Die Pakete unten werden beim ersten Ausführen automatisch installiert,
# falls sie noch nicht vorhanden sind (kann beim ersten Mal etwas dauern).
#
# Inhalt:
#   Schritt 1  - Datenimport, Datenaufbereitung, Fallauswahl, deskriptive Statistik
#   Schritt 2  - Reliabilitätsanalyse (Cronbachs Alpha)
#   Schritt 3  - Multiple lineare Regression (KQ) für H1, H2a, H2b
#   Schritt 4  - AIC-basierte Modellselektion
#   Schritt 5  - Robuste Regression + Regressionsdiagnostik (Nachtests)
#   Schritt 6  - Robuste MM-Regression + Ausreißeranalyse (Hampel-Distanz)
#   Schritt 7  - Regression auf dem bereinigten Datensatz (ohne Ausreißer)
#   Schritt 8  - AIC-Selektion auf dem bereinigten Datensatz
#   Schritt 9  - Robuste Regression + Diagnostik auf dem bereinigten Datensatz
#   Schritt 10 - Zusammenfassende Hypothesenprüfung H1, H2a, H2b
#   Schritt 11 - Moderationsanalyse H3a, H3b
##############################################################################

# ============================================================================
# SCHRITT 0: Pakete installieren (nur beim ersten Mal nötig) und laden
# ============================================================================
benoetigte_pakete <- c("readxl", "psych", "MASS", "lmtest", "car", "robustbase",
                        "tidyr", "dplyr")

fehlende_pakete <- benoetigte_pakete[!(benoetigte_pakete %in% installed.packages()[, "Package"])]
if (length(fehlende_pakete) > 0) install.packages(fehlende_pakete)

# Reihenfolge wichtig: dplyr wird als LETZTES geladen, damit dplyr::select()/
# dplyr::filter() nicht von gleichnamigen Funktionen aus MASS bzw. stats
# überdeckt werden (R verwendet bei Namenskonflikten die zuletzt geladene
# Paketversion).
invisible(lapply(benoetigte_pakete, library, character.only = TRUE))

# ============================================================================
# SCHRITT 1: Datenimport, Datenaufbereitung, Fallauswahl, deskriptive Statistik
# ============================================================================

# ---- 1.1 Daten einlesen -----------------------------------------------------
# <<< HIER DEN PFAD ZU DEINER EXCEL-DATEI EINTRAGEN >>>
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

# ---- 1.2 Fallauswahl (Filterung) -------------------------------------------
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

# ---- 1.3 Aufbereitung der Variablen -----------------------------------------

## 1.3.1 Soziodemografie ------------------------------------------------------
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

## 1.3.2 Homeoffice-Möglichkeit / -Nutzung (HB01/HB02) -----------------------
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

## 1.3.3 Workplace Fear of Missing Out (wFoMO-G, Ebner et al.) ---------------
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

## 1.3.4 Arbeitszufriedenheit (IAZ-K) ----------------------------------------
# Die AZ-Items basieren auf der IAZ-K-Originalskala. Gemäß Literaturquelle
# reicht diese von -3 (extrem unzufrieden) über 0 (weder noch) bis +3
# (extrem zufrieden). SoSci Survey kodiert Antwortoptionen beim Export
# jedoch grundsätzlich als fortlaufende Ganzzahlen beginnend bei 1 (in
# Erhebungsreihenfolge) - bei einer 7-stufigen Skala also 1 bis 7. Diese
# SoSci-Rohwerte sind NICHT die IAZ-K-Originalwerte und dürfen nicht direkt
# gemittelt werden.
#
# Polung: Im Fragebogen wurden die Antwortoptionen in der Reihenfolge
# "Extrem zufrieden" ... "Extrem unzufrieden" erhoben (absteigende
# Zufriedenheit), SoSci-Rohwert 1 entspricht also "Extrem zufrieden" (+3) und
# Rohwert 7 "Extrem unzufrieden" (-3). Die korrekte Rücktransformation lautet
# daher:
#
#   IAZ-K-Wert = 4 - SoSci-Rohwert
#
# (SoSci-Rohwert 1 -> IAZ-K +3, SoSci-Rohwert 4 -> IAZ-K 0 ("weder noch"),
# SoSci-Rohwert 7 -> IAZ-K -3.)
#
# Betroffen sind alle acht AZ-Items: AZ01_01 - AZ01_06 (Kernskala,
# verpflichtend), AZ02_01 (Zufriedenheit mit Mitarbeitenden; nur bei
# Führungsverantwortung) und AZ03_01 (Zufriedenheit mit Kundinnen/Kunden; nur
# bei Kundenkontakt).
#
# Bitte trotzdem einmal im SoSci-Feldeditor (Item AZ01_01, Reiter "Werte")
# gegenprüfen, ob die Erhebungsreihenfolge tatsächlich mit der Anzeige-
# reihenfolge im Fragebogen-PDF übereinstimmt. Falls dort SoSci-Rohwert 1
# "Extrem unzufrieden" bedeutet (Polung umgekehrt), stattdessen verwenden:
# across(all_of(az_items_soSci), ~ . - 4)
az_items_soSci <- c("AZ01_01", "AZ01_02", "AZ01_03", "AZ01_04", "AZ01_05", "AZ01_06",
                     "AZ02_01", "AZ03_01")

daten <- daten %>%
  mutate(across(all_of(az_items_soSci), ~ 4 - ., .names = "{.col}_iazk"))

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
      across(c(AZ01_01_iazk, AZ01_02_iazk, AZ01_03_iazk, AZ01_04_iazk,
               AZ01_05_iazk, AZ01_06_iazk, AZ02_01_iazk, AZ03_01_iazk)),
      na.rm = FALSE
    )
  )

n_az_gueltig <- sum(!is.na(daten$Arbeitszufriedenheit))
cat("\nArbeitszufriedenheit (8-Item-Mittelwert, IAZ-K-Skala -3 bis +3) gültig ")
cat("(nicht NA) für:", n_az_gueltig, "von", n_final, "Fällen\n")
cat("(Grund: AZ02_01 und AZ03_01 wurden nur von einem Teil der Stichprobe\n")
cat(" beantwortet; siehe Kommentar oben.)\n")

# ---- 1.4 Deskriptive Statistik ----------------------------------------------

## 1.4.1 Relative Häufigkeiten kategorialer Variablen ------------------------
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

## 1.4.2 M, SD, Median, Min, Max metrischer Variablen ------------------------
metrische_variablen <- daten %>%
  select(Alter, Berufserfahrung, Arbeitszeit,
         wFoMO_informational, wFoMO_relational, Arbeitszufriedenheit)

deskriptiv <- psych::describe(metrische_variablen) %>%
  as.data.frame() %>%
  select(n, mean, sd, median, min, max)

cat("\n--- Deskriptive Statistik: metrische Variablen ---\n")
cat("(n bei Arbeitszufriedenheit < N_final, siehe Hinweis oben)\n")
print(round(deskriptiv, 2))

## 1.4.3 Optionale Arbeitszufriedenheits-Items (deskriptiv, nur Antwortende) -
# Auf der zurücktransformierten IAZ-K-Skala (-3 bis +3), siehe oben.
az_optional <- daten %>%
  select(AZ02_01_iazk, AZ03_01_iazk) %>%
  psych::describe() %>%
  as.data.frame() %>%
  select(n, mean, sd, median, min, max)

cat("\n--- Deskriptive Statistik: optionale AZ-Items (nur Antwortende, IAZ-K -3 bis +3) ---\n")
cat("AZ02_01_iazk = Zufriedenheit mit Mitarbeitenden (nur Führungsverantwortung)\n")
cat("AZ03_01_iazk = Zufriedenheit mit Kundinnen/Kunden (nur Kundenkontakt)\n")
print(round(az_optional, 2))

# ============================================================================
# SCHRITT 2: Reliabilitätsanalyse (Cronbachs Alpha)
# ============================================================================

# psych::alpha() benötigt eine rein numerische Matrix/data.frame.
numerisch_df <- function(x) as.data.frame(lapply(x, as.numeric))

## 2.1 Arbeitszufriedenheit (Kernskala, 6 Items: AZ01_01 - AZ01_06) ---------
az_items <- daten %>% select(AZ01_01:AZ01_06) %>% numerisch_df()
alpha_az <- psych::alpha(az_items, check.keys = TRUE)   # check.keys = TRUE erkennt/korrigiert
                                                          # automatisch invers gepolte Items

cat("\n=== Cronbachs Alpha: Arbeitszufriedenheit (Kernskala, 6 Items) ===\n")
print(alpha_az$total[, c("raw_alpha", "std.alpha", "average_r")])
cat("\nTrennschärfe je Item (corrected item-total correlation) und Alpha bei\n")
cat("Ausschluss des jeweiligen Items:\n")
print(round(data.frame(
  r.drop      = alpha_az$item.stats$r.drop,
  alpha.drop  = alpha_az$alpha.drop$raw_alpha,
  row.names   = rownames(alpha_az$item.stats)
), 3))

## 2.2 Workplace FoMO - informationale Subskala (5 Items: FM01_01-05) -------
fomo_info_items <- daten %>% select(FM01_01:FM01_05) %>% numerisch_df()
alpha_fomo_info <- psych::alpha(fomo_info_items, check.keys = TRUE)

cat("\n=== Cronbachs Alpha: wFoMO informational (5 Items) ===\n")
print(alpha_fomo_info$total[, c("raw_alpha", "std.alpha", "average_r")])
cat("\nTrennschärfe je Item und Alpha bei Ausschluss des jeweiligen Items:\n")
print(round(data.frame(
  r.drop      = alpha_fomo_info$item.stats$r.drop,
  alpha.drop  = alpha_fomo_info$alpha.drop$raw_alpha,
  row.names   = rownames(alpha_fomo_info$item.stats)
), 3))

## 2.3 Workplace FoMO - relationale Subskala (5 Items: FM01_06-10) ----------
fomo_rel_items <- daten %>% select(FM01_06:FM01_10) %>% numerisch_df()
alpha_fomo_rel <- psych::alpha(fomo_rel_items, check.keys = TRUE)

cat("\n=== Cronbachs Alpha: wFoMO relational (5 Items) ===\n")
print(alpha_fomo_rel$total[, c("raw_alpha", "std.alpha", "average_r")])
cat("\nTrennschärfe je Item und Alpha bei Ausschluss des jeweiligen Items:\n")
print(round(data.frame(
  r.drop      = alpha_fomo_rel$item.stats$r.drop,
  alpha.drop  = alpha_fomo_rel$alpha.drop$raw_alpha,
  row.names   = rownames(alpha_fomo_rel$item.stats)
), 3))

## 2.4 Zusammenfassende Übersicht --------------------------------------------
zusammenfassung_alpha <- data.frame(
  Skala   = c("Arbeitszufriedenheit (Kernskala)", "wFoMO informational", "wFoMO relational"),
  Items   = c(6, 5, 5),
  N       = nrow(daten),
  Alpha   = round(c(alpha_az$total$raw_alpha,
                     alpha_fomo_info$total$raw_alpha,
                     alpha_fomo_rel$total$raw_alpha), 3),
  Alpha_std = round(c(alpha_az$total$std.alpha,
                       alpha_fomo_info$total$std.alpha,
                       alpha_fomo_rel$total$std.alpha), 3)
)

cat("\n=== Übersicht Cronbachs Alpha aller Skalen ===\n")
print(zusammenfassung_alpha)

# Faustregel zur Einordnung (Nunnally & Bernstein, 1994):
# alpha >= .90 exzellent | >= .80 gut | >= .70 akzeptabel | < .70 fragwürdig

## 2.5 Arbeitszufriedenheit: Kernskala vs. erweiterte Skala ------------------
# Zusatzprüfung: Wie verändert sich die interne Konsistenz, wenn man nur die
# Subgruppe betrachtet, die zusätzlich BEIDE freiwilligen Items beantwortet
# hat (AZ02_01 = Zufriedenheit mit Mitarbeitenden, AZ03_01 = Zufriedenheit
# mit Kundinnen/Kunden)?
subgruppe_freiwillig <- daten %>%
  filter(!is.na(AZ02_01), !is.na(AZ03_01))

n_subgruppe <- nrow(subgruppe_freiwillig)
cat("\nN Gesamtstichprobe (6 Pflichtitems):                           ", nrow(daten), "\n")
cat("N Subgruppe (zusätzlich beide freiwillige Items beantwortet):   ", n_subgruppe, "\n")

## (b) 6 Pflichtitems, nur Subgruppe
az6_sub_items <- subgruppe_freiwillig %>% select(AZ01_01:AZ01_06) %>% numerisch_df()
alpha_az6_sub <- psych::alpha(az6_sub_items, check.keys = TRUE)

cat("\n=== Cronbachs Alpha: 6 Pflichtitems, nur Subgruppe (n = ", n_subgruppe, ") ===\n", sep = "")
print(alpha_az6_sub$total[, c("raw_alpha", "std.alpha", "average_r")])

## (c) 8 Items (6 Pflicht- + 2 freiwillige), nur Subgruppe
az8_sub_items <- subgruppe_freiwillig %>%
  select(AZ01_01:AZ01_06, AZ02_01, AZ03_01) %>%
  numerisch_df()
alpha_az8_sub <- psych::alpha(az8_sub_items, check.keys = TRUE)

cat("\n=== Cronbachs Alpha: 8 Items (inkl. freiwillige), Subgruppe (n = ", n_subgruppe, ") ===\n", sep = "")
print(alpha_az8_sub$total[, c("raw_alpha", "std.alpha", "average_r")])
cat("\nTrennschärfe je Item und Alpha bei Ausschluss des jeweiligen Items:\n")
print(round(data.frame(
  r.drop      = alpha_az8_sub$item.stats$r.drop,
  alpha.drop  = alpha_az8_sub$alpha.drop$raw_alpha,
  row.names   = rownames(alpha_az8_sub$item.stats)
), 3))

## Vergleichstabelle
vergleich_az <- data.frame(
  Variante = c("6 Pflichtitems, Gesamtstichprobe",
               "6 Pflichtitems, Subgruppe (freiwillige Items zusätzlich beantwortet)",
               "8 Items (6 Pflicht- + 2 freiwillige), Subgruppe"),
  Items = c(6, 6, 8),
  N     = c(nrow(daten), n_subgruppe, n_subgruppe),
  Alpha = round(c(alpha_az$total$raw_alpha,
                   alpha_az6_sub$total$raw_alpha,
                   alpha_az8_sub$total$raw_alpha), 3)
)

cat("\n=== Vergleich: Arbeitszufriedenheit - Kernskala vs. erweiterte Skala ===\n")
print(vergleich_az)

# ============================================================================
# SCHRITT 3: Multiple lineare Regression (KQ) für H1, H2a, H2b
# ============================================================================
# AV:  Arbeitszufriedenheit = Mittelwert aus AZ01_01-06, AZ02_01, AZ03_01
# UV1: HB01_kat             = Möglichkeit zum hybriden Arbeiten (H1)
#      Faktor mit 11 Kategorien ("0 Tage" ... "5 Tage"), Referenzkategorie
#      "0 Tage" -> geht über Dummy-Kodierung (10 Kontraste) ins Modell ein,
#      OHNE gleiche Abstände zwischen den Kategorien zu unterstellen.
# UV2: wFoMO_informational  = informationale wFoMO (H2a)
# UV3: wFoMO_relational     = relationale wFoMO (H2b)
modell_h1_h2 <- lm(Arbeitszufriedenheit ~ HB01_kat + wFoMO_informational + wFoMO_relational,
                    data = daten)

cat("\n=== Multiple Regression (KQ-Methode): Arbeitszufriedenheit ~ ")
cat("HB01_kat (Möglichkeit hybrides Arbeiten) + wFoMO informational + wFoMO relational ===\n")
print(summary(modell_h1_h2))

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

cat("\n--- ANOVA-Tabelle (Modell vs. Residuen, je Term) ---\n")
print(anova(modell_h1_h2))

# Standardisierte Koeffizienten (Beta) für wFoMO informational/relational.
# HB01_kat ist eine Faktorvariable (Dummy-kodiert) und besitzt daher KEIN
# einzelnes standardisiertes Beta.
daten_z <- daten %>%
  mutate(across(c(Arbeitszufriedenheit, wFoMO_informational, wFoMO_relational),
                ~ as.numeric(scale(.))))

modell_standardisiert <- lm(Arbeitszufriedenheit ~ HB01_kat + wFoMO_informational + wFoMO_relational,
                             data = daten_z)

cat("\n=== Standardisierte Koeffizienten (Beta) für wFoMO informational/relational ===\n")
cat("(HB01_kat ist kategorial und wird hier nicht mit ausgegeben, siehe Kommentar oben)\n")
beta_koef <- coef(modell_standardisiert)
print(round(beta_koef[c("wFoMO_informational", "wFoMO_relational")], 3))

# ============================================================================
# SCHRITT 4: AIC-basierte Modellselektion
# ============================================================================
# HB01_kat ist eine Faktorvariable (Dummy-Kodierung, 10 Kontraste) -> step()
# behandelt sie automatisch als EINEN Block (alle Kontraste werden gemeinsam
# aufgenommen bzw. entfernt), nicht als einzelne Kontraste.
modell_voll <- lm(Arbeitszufriedenheit ~ HB01_kat + wFoMO_informational + wFoMO_relational,
                   data = daten)
modell_null <- lm(Arbeitszufriedenheit ~ 1, data = daten)

cat("\n=== Schrittweise Modellselektion nach AIC (direction = 'both') ===\n")
modell_aic <- step(modell_voll,
                    scope = list(lower = modell_null, upper = modell_voll),
                    direction = "both",
                    trace = 1)

cat("\n=== Finales Modell nach schrittweiser AIC-Selektion ===\n")
print(summary(modell_aic))

if (length(coef(modell_aic)) > 1) {
  f_werte  <- summary(modell_aic)$fstatistic
  p_wert_f <- pf(f_werte["value"], f_werte["numdf"], f_werte["dendf"], lower.tail = FALSE)
  cat("\nF(", f_werte["numdf"], ",", f_werte["dendf"], ") = ", round(f_werte["value"], 2),
      ", p = ", format.pval(p_wert_f, digits = 3), "\n", sep = "")
} else {
  cat("\nDas AIC-optimale Modell enthält keinen Prädiktor mehr (nur Achsenabschnitt);\n")
  cat("ein F-Test ist hier nicht sinnvoll interpretierbar.\n")
}

# Vollständiger Modellvergleich (alle Prädiktor-Kombinationen, "Best-Subset")
praediktoren <- c("HB01_kat", "wFoMO_informational", "wFoMO_relational")

alle_modelle <- list(Nullmodell = modell_null)
for (k in seq_along(praediktoren)) {
  kombinationen <- combn(praediktoren, k, simplify = FALSE)
  for (komb in kombinationen) {
    formel <- as.formula(paste("Arbeitszufriedenheit ~", paste(komb, collapse = " + ")))
    name <- paste(komb, collapse = " + ")
    alle_modelle[[name]] <- lm(formel, data = daten)
  }
}

aic_tabelle <- data.frame(
  Modell = names(alle_modelle),
  k_Parameter = sapply(alle_modelle, function(m) length(coef(m))),
  AIC = round(sapply(alle_modelle, AIC), 2),
  R2  = round(sapply(alle_modelle, function(m) summary(m)$r.squared), 4)
)
aic_tabelle <- aic_tabelle[order(aic_tabelle$AIC), ]
rownames(aic_tabelle) <- NULL

cat("\n=== Vollständiger Modellvergleich (alle Prädiktor-Kombinationen), ")
cat("sortiert nach AIC (kleinster Wert = bestes Modell) ===\n")
print(aic_tabelle)

# ============================================================================
# SCHRITT 5: Robuste Regression + Regressionsdiagnostik (Nachtests)
# ============================================================================
modell_reduziert <- modell_aic
formel_reduziert  <- formula(modell_reduziert)

# Anzahl der TERME (nicht Koeffizienten!) im Modell: HB01_kat zählt als EIN
# Term, auch wenn er 10 Dummy-Koeffizienten erzeugt. VIF/Multikollinearität
# sind erst ab zwei Termen sinnvoll berechenbar.
n_terme <- length(attr(terms(modell_reduziert), "term.labels"))

cat("\n=== Reduziertes Modell (KQ, nach AIC-Selektion) ===\n")
print(formel_reduziert)
print(summary(modell_reduziert))

# Robuste Regression (Huber-M-Schätzer) auf demselben Modell
modell_robust <- MASS::rlm(formel_reduziert, data = daten)

cat("\n=== Robuste Regression (rlm, Huber-M-Schätzer) ===\n")
print(summary(modell_robust))

koef_robust <- summary(modell_robust)$coefficients
df_robust   <- modell_robust$df.residual
koef_robust <- cbind(
  koef_robust,
  p_wert = 2 * pt(abs(koef_robust[, "t value"]), df = df_robust, lower.tail = FALSE)
)
cat("\nRobuste Koeffizienten inkl. approximativem p-Wert:\n")
print(round(koef_robust, 4))

vergleich_koef <- data.frame(
  Praediktor = names(coef(modell_reduziert)),
  KQ         = round(coef(modell_reduziert), 4),
  Robust     = round(coef(modell_robust), 4)
)
cat("\n=== Vergleich: KQ-Koeffizienten vs. robuste Koeffizienten ===\n")
print(vergleich_koef)

## Prüfung der Regressionsannahmen (Nachtests) auf dem KQ-Modell
cat("\n--- Shapiro-Wilk-Test (Normalverteilung der Residuen) ---\n")
print(shapiro.test(residuals(modell_reduziert)))

cat("\n--- Breusch-Pagan-Test (Homoskedastizität) ---\n")
if (n_terme >= 1) {
  print(lmtest::bptest(modell_reduziert))
} else {
  cat("Das AIC-optimale Modell enthält keinen Prädiktor (nur Achsenabschnitt);\n")
  cat("ein Test auf Homoskedastizität setzt jedoch mindestens einen Regressor\n")
  cat("voraus und ist hier nicht durchführbar.\n")
}

cat("\n--- Varianzinflationsfaktoren (VIF) ---\n")
if (n_terme > 1) {
  print(car::vif(modell_reduziert))
} else {
  cat("Das reduzierte Modell enthält nur einen Prädiktor(-block) - VIF ist bei\n")
  cat("nur einem Term nicht definiert und daher nicht berechenbar.\n")
}

cat("\n--- Durbin-Watson-Test (Autokorrelation der Residuen) ---\n")
if (n_terme >= 1) {
  print(lmtest::dwtest(modell_reduziert))
} else {
  cat("Nicht durchführbar ohne Regressor (nur Achsenabschnitt).\n")
}

cat("\n--- RESET-Test nach Ramsey (funktionale Form) ---\n")
if (n_terme >= 1) {
  print(lmtest::resettest(modell_reduziert))
} else {
  cat("Nicht durchführbar ohne Regressor (nur Achsenabschnitt).\n")
}

# ============================================================================
# SCHRITT 6: Robuste MM-Regression + Ausreißeranalyse (Hampel-Distanz)
# ============================================================================
# lmrob() schätzt standardmäßig einen MM-Schätzer: hoher Bruchpunkt (Start
# über S-Schätzer) kombiniert mit hoher Effizienz (M-Schritt) -> robust
# gegenüber Ausreißern UND Hebelpunkten. HB01_kat geht als Faktor (Dummy-
# Kodierung) ein.
modell_mm <- lmrob(Arbeitszufriedenheit ~ HB01_kat + wFoMO_informational + wFoMO_relational,
                    data = daten)

cat("\n=== Robuste MM-Regression (robustbase::lmrob) - volles Modell ===\n")
print(summary(modell_mm))

modell_kq_voll <- lm(Arbeitszufriedenheit ~ HB01_kat + wFoMO_informational + wFoMO_relational,
                      data = daten)

vergleich_koef <- data.frame(
  Praediktor = names(coef(modell_kq_voll)),
  KQ         = round(coef(modell_kq_voll), 4),
  MM_robust  = round(coef(modell_mm), 4)
)
cat("\n=== Vergleich: KQ-Koeffizienten vs. MM-robuste Koeffizienten ===\n")
print(vergleich_koef)

# Ausreißeridentifikation über die Hampel-Distanz:
#   Hampel-Distanz_i = | resid_i - median(resid) | / (1.4826 * MAD(resid))
# Cut-off (Vorgabe): Hampel-Distanz > 5 => Fall wird als Ausreißer eingestuft.
#
# Wichtig: lmrob() schließt Fälle mit fehlenden Werten (z. B. wegen NA bei
# Arbeitszufriedenheit) automatisch aus. Für die nachfolgende Zuordnung der
# CASE-Nummern wird deshalb der zum Modell gehörende (bereits gefilterte)
# Teildatensatz "daten_mm" verwendet, nicht der vollständige "daten".
daten_mm <- daten %>%
  filter(!is.na(Arbeitszufriedenheit), !is.na(HB01_kat),
         !is.na(wFoMO_informational), !is.na(wFoMO_relational))

resid_mm     <- residuals(modell_mm)
median_resid <- median(resid_mm)
mad_resid    <- mad(resid_mm, constant = 1.4826)

hampel_distanz <- abs(resid_mm - median_resid) / mad_resid
schwelle_hampel <- 5

diagnose_tab <- daten_mm %>%
  transmute(CASE,
            residuum       = round(resid_mm, 3),
            hampel_distanz = round(hampel_distanz, 2)) %>%
  arrange(desc(hampel_distanz))

cat("\n=== Die 15 Fälle mit den höchsten Hampel-Distanzen ===\n")
print(head(diagnose_tab, 15))

ausreisser_hampel <- diagnose_tab %>% filter(hampel_distanz > schwelle_hampel)

cat("\nAnzahl Fälle mit Hampel-Distanz >", schwelle_hampel, ":",
    nrow(ausreisser_hampel), "von", nrow(daten_mm), "\n")

cat("\n=== Als Ausreißer identifizierte Fälle (Hampel-Distanz > ",
    schwelle_hampel, ") ===\n", sep = "")
if (nrow(ausreisser_hampel) > 0) {
  print(ausreisser_hampel)
} else {
  cat("Keine Fälle überschreiten den Schwellenwert.\n")
}

# Diagnoseplot: Hampel-Distanz je Fall
plot(hampel_distanz, pch = 19,
     col = ifelse(hampel_distanz > schwelle_hampel, "red", "grey50"),
     xlab = "Fall (Index)", ylab = "Hampel-Distanz",
     main = "Ausreißerdiagnose (MM-Regression, volles Modell): Hampel-Distanz")
abline(h = schwelle_hampel, lty = 2, col = "red")

if (nrow(ausreisser_hampel) > 0) {
  idx_ausreisser <- which(hampel_distanz > schwelle_hampel)
  text(idx_ausreisser, hampel_distanz[idx_ausreisser],
       labels = daten_mm$CASE[idx_ausreisser],
       pos = 3, cex = 0.7, col = "red")
}

# ============================================================================
# SCHRITT 7: Regression auf dem bereinigten Datensatz (ohne Ausreißer)
# ============================================================================
# Ausschluss der Fälle mit Hampel-Distanz > 5 (Kriterium aus Schritt 6).
# Basis ist "daten_mm" (nicht "daten"), da modell_mm nur mit den Fällen
# gerechnet wurde, für die Arbeitszufriedenheit, HB01_kat, wFoMO_informational
# und wFoMO_relational alle nicht-fehlend sind (siehe Schritt 6).
daten_bereinigt <- daten_mm %>%
  mutate(hampel_distanz = hampel_distanz) %>%
  filter(hampel_distanz <= schwelle_hampel)

cat("N vor Bereinigung: ", nrow(daten_mm), "\n")
cat("N nach Bereinigung:", nrow(daten_bereinigt),
    "(", nrow(daten_mm) - nrow(daten_bereinigt), "Fall/Fälle entfernt)\n")

modell_bereinigt <- lm(Arbeitszufriedenheit ~ HB01_kat + wFoMO_informational + wFoMO_relational,
                        data = daten_bereinigt)

cat("\n=== Multiple Regression (KQ-Methode) auf bereinigtem Datensatz ===\n")
print(summary(modell_bereinigt))

f_werte  <- summary(modell_bereinigt)$fstatistic
f_wert   <- f_werte["value"]
df1      <- f_werte["numdf"]
df2      <- f_werte["dendf"]
p_wert_f <- pf(f_wert, df1, df2, lower.tail = FALSE)

cat("\n--- F-Test auf globale Modellgüte (H0: alle Beta = 0) ---\n")
cat("F(", df1, ",", df2, ") = ", round(f_wert, 2),
    ", p = ", format.pval(p_wert_f, digits = 3), "\n", sep = "")
cat("R²       = ", round(summary(modell_bereinigt)$r.squared, 3), "\n", sep = "")
cat("korr. R² = ", round(summary(modell_bereinigt)$adj.r.squared, 3), "\n", sep = "")

cat("\n--- ANOVA-Tabelle (Modell vs. Residuen, je Term) ---\n")
print(anova(modell_bereinigt))

daten_bereinigt_z <- daten_bereinigt %>%
  mutate(across(c(Arbeitszufriedenheit, wFoMO_informational, wFoMO_relational),
                ~ as.numeric(scale(.))))

modell_bereinigt_standardisiert <- lm(
  Arbeitszufriedenheit ~ HB01_kat + wFoMO_informational + wFoMO_relational,
  data = daten_bereinigt_z
)

cat("\n=== Standardisierte Koeffizienten (Beta) für wFoMO informational/relational ===\n")
beta_koef_b <- coef(modell_bereinigt_standardisiert)
print(round(beta_koef_b[c("wFoMO_informational", "wFoMO_relational")], 3))

# Vergleich: Pfad 1 (vollständiger Datensatz) vs. Pfad 2 (bereinigt)
modell_voll <- lm(Arbeitszufriedenheit ~ HB01_kat + wFoMO_informational + wFoMO_relational,
                   data = daten_mm)
f_voll <- summary(modell_voll)$fstatistic
p_voll <- pf(f_voll["value"], f_voll["numdf"], f_voll["dendf"], lower.tail = FALSE)

vergleich_modelle <- data.frame(
  Modell = c("Pfad 1: vollständiger Datensatz", "Pfad 2: bereinigter Datensatz"),
  N      = c(nrow(daten_mm), nrow(daten_bereinigt)),
  R2     = round(c(summary(modell_voll)$r.squared, summary(modell_bereinigt)$r.squared), 3),
  F_Wert = round(c(f_voll["value"], f_wert), 2),
  p_Wert = round(c(p_voll, p_wert_f), 4)
)

cat("\n=== Vergleich Pfad 1 (mit Ausreißern) vs. Pfad 2 (ohne Ausreißer) ===\n")
print(vergleich_modelle)

# ============================================================================
# SCHRITT 8: AIC-Selektion auf dem bereinigten Datensatz
# ============================================================================
modell_voll_bereinigt <- modell_bereinigt
modell_null_bereinigt <- lm(Arbeitszufriedenheit ~ 1, data = daten_bereinigt)

cat("\n=== Schrittweise Modellselektion nach AIC (bereinigter Datensatz) ===\n")
modell_aic_bereinigt <- step(modell_voll_bereinigt,
                              scope = list(lower = modell_null_bereinigt,
                                           upper = modell_voll_bereinigt),
                              direction = "both",
                              trace = 1)

cat("\n=== Finales Modell nach AIC-Selektion (bereinigter Datensatz) ===\n")
print(summary(modell_aic_bereinigt))

if (length(coef(modell_aic_bereinigt)) > 1) {
  f_werte  <- summary(modell_aic_bereinigt)$fstatistic
  p_wert_f <- pf(f_werte["value"], f_werte["numdf"], f_werte["dendf"], lower.tail = FALSE)
  cat("\nF(", f_werte["numdf"], ",", f_werte["dendf"], ") = ", round(f_werte["value"], 2),
      ", p = ", format.pval(p_wert_f, digits = 3), "\n", sep = "")
} else {
  cat("\nDas AIC-optimale Modell enthält keinen Prädiktor mehr (nur Achsenabschnitt);\n")
  cat("ein F-Test ist hier nicht sinnvoll interpretierbar.\n")
}

praediktoren <- c("HB01_kat", "wFoMO_informational", "wFoMO_relational")

alle_modelle <- list(Nullmodell = modell_null_bereinigt)
for (k in seq_along(praediktoren)) {
  kombinationen <- combn(praediktoren, k, simplify = FALSE)
  for (komb in kombinationen) {
    formel <- as.formula(paste("Arbeitszufriedenheit ~", paste(komb, collapse = " + ")))
    name <- paste(komb, collapse = " + ")
    alle_modelle[[name]] <- lm(formel, data = daten_bereinigt)
  }
}

aic_tabelle_bereinigt <- data.frame(
  Modell      = names(alle_modelle),
  k_Parameter = sapply(alle_modelle, function(m) length(coef(m))),
  AIC         = round(sapply(alle_modelle, AIC), 2),
  R2          = round(sapply(alle_modelle, function(m) summary(m)$r.squared), 4)
)
aic_tabelle_bereinigt <- aic_tabelle_bereinigt[order(aic_tabelle_bereinigt$AIC), ]
rownames(aic_tabelle_bereinigt) <- NULL

cat("\n=== Vollständiger Modellvergleich (bereinigter Datensatz), ")
cat("sortiert nach AIC (kleinster Wert = bestes Modell) ===\n")
print(aic_tabelle_bereinigt)

if (exists("modell_aic")) {
  cat("\n=== Vergleich: AIC-optimales Modell Pfad 1 vs. Pfad 2 ===\n")
  vergleich_aic_pfade <- data.frame(
    Pfad         = c("Pfad 1: vollständiger Datensatz", "Pfad 2: bereinigter Datensatz"),
    Praediktoren = c(paste(names(coef(modell_aic))[-1], collapse = " + "),
                      paste(names(coef(modell_aic_bereinigt))[-1], collapse = " + ")),
    AIC          = round(c(AIC(modell_aic), AIC(modell_aic_bereinigt)), 2)
  )
  print(vergleich_aic_pfade)
}

# ============================================================================
# SCHRITT 9: Robuste Regression + Diagnostik auf dem bereinigten Datensatz
# ============================================================================
modell_reduziert_b <- modell_aic_bereinigt
formel_reduziert_b  <- formula(modell_reduziert_b)
n_terme_b <- length(attr(terms(modell_reduziert_b), "term.labels"))

cat("\n=== Reduziertes Modell (KQ, nach AIC-Selektion, bereinigter Datensatz) ===\n")
print(formel_reduziert_b)
print(summary(modell_reduziert_b))

modell_robust_b <- MASS::rlm(formel_reduziert_b, data = daten_bereinigt)

cat("\n=== Robuste Regression (rlm, Huber-M-Schätzer), bereinigter Datensatz ===\n")
print(summary(modell_robust_b))

koef_robust_b <- summary(modell_robust_b)$coefficients
df_robust_b   <- modell_robust_b$df.residual
koef_robust_b <- cbind(
  koef_robust_b,
  p_wert = 2 * pt(abs(koef_robust_b[, "t value"]), df = df_robust_b, lower.tail = FALSE)
)
cat("\nRobuste Koeffizienten inkl. approximativem p-Wert:\n")
print(round(koef_robust_b, 4))

vergleich_koef_b <- data.frame(
  Praediktor = names(coef(modell_reduziert_b)),
  KQ         = round(coef(modell_reduziert_b), 4),
  Robust     = round(coef(modell_robust_b), 4)
)
cat("\n=== Vergleich: KQ-Koeffizienten vs. robuste Koeffizienten (bereinigt) ===\n")
print(vergleich_koef_b)

cat("\n--- Shapiro-Wilk-Test (Normalverteilung der Residuen) ---\n")
print(shapiro.test(residuals(modell_reduziert_b)))

cat("\n--- Breusch-Pagan-Test (Homoskedastizität) ---\n")
if (n_terme_b >= 1) {
  print(lmtest::bptest(modell_reduziert_b))
} else {
  cat("Das AIC-optimale Modell enthält keinen Prädiktor (nur Achsenabschnitt);\n")
  cat("ein Test auf Homoskedastizität setzt jedoch mindestens einen Regressor\n")
  cat("voraus und ist hier nicht durchführbar.\n")
}

cat("\n--- Varianzinflationsfaktoren (VIF) ---\n")
if (n_terme_b > 1) {
  print(car::vif(modell_reduziert_b))
} else {
  cat("Das reduzierte Modell enthält nur einen Prädiktor(-block) - VIF ist bei\n")
  cat("nur einem Term nicht definiert und daher nicht berechenbar.\n")
}

cat("\n--- Durbin-Watson-Test (Autokorrelation der Residuen) ---\n")
if (n_terme_b >= 1) {
  print(lmtest::dwtest(modell_reduziert_b))
} else {
  cat("Nicht durchführbar ohne Regressor (nur Achsenabschnitt).\n")
}

cat("\n--- RESET-Test nach Ramsey (funktionale Form) ---\n")
if (n_terme_b >= 1) {
  print(lmtest::resettest(modell_reduziert_b))
} else {
  cat("Nicht durchführbar ohne Regressor (nur Achsenabschnitt).\n")
}

# ============================================================================
# SCHRITT 10: Zusammenfassende Hypothesenprüfung H1, H2a, H2b
# ============================================================================
# HB01_kat (H1) ist eine Faktorvariable (10 Dummy-Kontraste) und liefert
# deshalb KEINEN einzelnen Koeffizienten/p-Wert wie ein metrischer Prädiktor.
# H1 wird daher über einen Omnibus-F-Test geprüft (Modellvergleich MIT vs.
# OHNE den gesamten HB01_kat-Block, via anova()); als ergänzende Information
# für die Richtung wird der Zusammenhang zwischen der Kategorien-Reihenfolge
# und den mittleren AV-Werten je Kategorie über eine Rangkorrelation
# (Spearman) beschrieben - rein deskriptiv.
#
# H2a und H2b bleiben metrische Einzelprädiktoren und werden weiterhin über
# den klassischen t-Test des jeweiligen Regressionskoeffizienten geprüft.

koef_extrahieren <- function(modell, praediktor) {
  koef <- summary(modell)$coefficients
  if (praediktor %in% rownames(koef)) {
    c(b = koef[praediktor, "Estimate"], p = koef[praediktor, "Pr(>|t|)"])
  } else {
    c(b = NA_real_, p = NA_real_)
  }
}

entscheidung <- function(b, p, erwartete_richtung, alpha = 0.05) {
  if (is.na(b)) return("nicht im Modell enthalten")
  if (is.na(p) || p >= alpha) return("H0 nicht verworfen (n.s.)")
  richtung_passt <- (erwartete_richtung == "positiv" && b > 0) ||
                     (erwartete_richtung == "negativ" && b < 0)
  if (richtung_passt) "Hypothese angenommen" else "signifikant, falsche Richtung"
}

omnibus_test_hb <- function(modell, praediktor = "HB01_kat") {
  terme <- attr(terms(modell), "term.labels")
  if (!(praediktor %in% terme)) {
    return(list(F = NA_real_, df1 = NA_real_, df2 = NA_real_, p = NA_real_, enthalten = FALSE))
  }
  modell_ohne <- update(modell, as.formula(paste(". ~ . -", praediktor)))
  vgl <- anova(modell_ohne, modell)
  list(F = vgl$F[2], df1 = vgl$Df[2], df2 = vgl$Res.Df[2], p = vgl$`Pr(>F)`[2], enthalten = TRUE)
}

richtung_hb <- function(daten_input) {
  mittelwerte <- daten_input %>%
    filter(!is.na(HB01_kat), !is.na(Arbeitszufriedenheit)) %>%
    group_by(HB01_kat) %>%
    summarise(m_az = mean(Arbeitszufriedenheit), .groups = "drop") %>%
    mutate(rang = as.integer(factor(HB01_kat, levels = levels(daten_input$HB01_kat))))
  if (nrow(mittelwerte) < 3) return(NA_real_)
  suppressWarnings(cor(mittelwerte$rang, mittelwerte$m_az, method = "spearman", use = "complete.obs"))
}

entscheidung_h1 <- function(omnibus, rho, alpha = 0.05) {
  if (!omnibus$enthalten) return("nicht im Modell enthalten")
  if (is.na(omnibus$p) || omnibus$p >= alpha) return("H0 nicht verworfen (n.s.)")
  if (!is.na(rho) && rho > 0) "Hypothese angenommen (signifikant, positiver Trend)"
  else "signifikant, aber kein klar positiver Trend (siehe Dummy-Koeffizienten)"
}

rho_voll      <- richtung_hb(daten_mm)
rho_bereinigt <- richtung_hb(daten_bereinigt)

h1_voll      <- omnibus_test_hb(modell_voll)
h1_bereinigt <- omnibus_test_hb(modell_bereinigt)
h1_aic       <- omnibus_test_hb(modell_aic)
h1_aic_ber   <- omnibus_test_hb(modell_aic_bereinigt)

h1_tabelle <- data.frame(
  Modell = c("Pfad 1: volles Modell", "Pfad 2: volles Modell (bereinigt)",
             "Pfad 1: AIC-reduziertes Modell", "Pfad 2: AIC-reduziertes Modell (bereinigt)"),
  F_Wert = round(c(h1_voll$F, h1_bereinigt$F, h1_aic$F, h1_aic_ber$F), 3),
  df1    = c(h1_voll$df1, h1_bereinigt$df1, h1_aic$df1, h1_aic_ber$df1),
  df2    = c(h1_voll$df2, h1_bereinigt$df2, h1_aic$df2, h1_aic_ber$df2),
  p      = round(c(h1_voll$p, h1_bereinigt$p, h1_aic$p, h1_aic_ber$p), 4),
  rho_Richtung = round(c(rho_voll, rho_bereinigt, rho_voll, rho_bereinigt), 3),
  Entscheidung = c(entscheidung_h1(h1_voll, rho_voll),
                   entscheidung_h1(h1_bereinigt, rho_bereinigt),
                   entscheidung_h1(h1_aic, rho_voll),
                   entscheidung_h1(h1_aic_ber, rho_bereinigt))
)

cat("\n=== H1 (HB01_kat, Omnibus-F-Test über alle Dummy-Kontraste) ===\n")
cat("rho_Richtung = Spearman-Rangkorrelation zwischen Kategorien-Reihenfolge\n")
cat("und mittlerer Arbeitszufriedenheit je Kategorie (rein deskriptiv)\n")
print(h1_tabelle)

hypothesen_basis <- data.frame(
  Hypothese  = c("H2a", "H2b"),
  Praediktor = c("wFoMO_informational", "wFoMO_relational"),
  Erwartung  = c("negativ", "negativ"),
  stringsAsFactors = FALSE
)

tabelle_bauen <- function(modell) {
  res <- sapply(hypothesen_basis$Praediktor, function(p) koef_extrahieren(modell, p))
  data.frame(
    b = round(as.numeric(res["b", ]), 4),
    p = round(as.numeric(res["p", ]), 4)
  )
}

res_voll      <- tabelle_bauen(modell_voll)
res_bereinigt <- tabelle_bauen(modell_bereinigt)

tabelle_voll <- hypothesen_basis
tabelle_voll$b_Pfad1 <- res_voll$b
tabelle_voll$p_Pfad1 <- res_voll$p
tabelle_voll$Entscheidung_Pfad1 <- mapply(entscheidung, tabelle_voll$b_Pfad1,
                                           tabelle_voll$p_Pfad1, tabelle_voll$Erwartung)
tabelle_voll$b_Pfad2 <- res_bereinigt$b
tabelle_voll$p_Pfad2 <- res_bereinigt$p
tabelle_voll$Entscheidung_Pfad2 <- mapply(entscheidung, tabelle_voll$b_Pfad2,
                                           tabelle_voll$p_Pfad2, tabelle_voll$Erwartung)

cat("\n=== H2a/H2b: volle Modelle (Pfad 1 vs. Pfad 2) ===\n")
print(tabelle_voll)

res_aic1 <- tabelle_bauen(modell_aic)
res_aic2 <- tabelle_bauen(modell_aic_bereinigt)

tabelle_aic <- hypothesen_basis
tabelle_aic$b_Pfad1 <- res_aic1$b
tabelle_aic$p_Pfad1 <- res_aic1$p
tabelle_aic$Entscheidung_Pfad1 <- mapply(entscheidung, tabelle_aic$b_Pfad1,
                                          tabelle_aic$p_Pfad1, tabelle_aic$Erwartung)
tabelle_aic$b_Pfad2 <- res_aic2$b
tabelle_aic$p_Pfad2 <- res_aic2$p
tabelle_aic$Entscheidung_Pfad2 <- mapply(entscheidung, tabelle_aic$b_Pfad2,
                                          tabelle_aic$p_Pfad2, tabelle_aic$Erwartung)

cat("\n=== H2a/H2b: AIC-reduzierte Modelle (Pfad 1 vs. Pfad 2) ===\n")
print(tabelle_aic)

# ============================================================================
# SCHRITT 11: Moderationsanalyse H3a, H3b
# ============================================================================
# HB01_kat ist eine Faktorvariable (10 Dummy-Kontraste) und kann daher nicht
# z-standardisiert und zu EINEM einzelnen Interaktionsterm verrechnet werden.
# H3a/H3b werden deshalb über einen Omnibus-F-Test geprüft: Erklärt der
# GESAMTE Interaktionsblock (alle 10 HB01_kat:wFoMO-Kontraste gemeinsam)
# signifikant zusätzliche Varianz gegenüber dem Modell ohne Interaktion?

moderation_analyse <- function(daten_input, bezeichnung) {

  daten_z <- daten_input %>%
    mutate(
      wfomo_info_z = as.numeric(scale(wFoMO_informational)),
      wfomo_rel_z  = as.numeric(scale(wFoMO_relational))
    )

  modell_haupt <- lm(Arbeitszufriedenheit ~ HB01_kat + wfomo_info_z + wfomo_rel_z, data = daten_z)
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

ergebnis_pfad1 <- moderation_analyse(daten_mm, "Pfad 1 (vollständiger Datensatz)")
ergebnis_pfad2 <- moderation_analyse(daten_bereinigt, "Pfad 2 (bereinigter Datensatz)")

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

cat("\n\n=== ANALYSE VOLLSTÄNDIG DURCHGELAUFEN ===\n")
