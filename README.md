# Bachelorarbeit: Hybrides Arbeiten, Workplace FoMO und Arbeitszufriedenheit

R-Auswertung des SoSci-Survey-Datensatzes `Umfragewerte_BA_4.xlsx`
(Fragebogen "LKo") zu den Hypothesen:

- **H1:** Möglichkeit zum hybriden Arbeiten ↔ Arbeitszufriedenheit (positiv)
- **H2a:** Informationale Workplace FoMO ↔ Arbeitszufriedenheit (negativ)
- **H2b:** Relationale Workplace FoMO ↔ Arbeitszufriedenheit (negativ)
- **H3a/b:** Moderation des H1-Zusammenhangs durch informationale/relationale
  Workplace FoMO (abschwächender Effekt)

## Datei ablegen

Die Rohdatendatei liegt bewusst nicht im Repository (personenbezogene
Umfragedaten). Bitte `Umfragewerte_BA_4.xlsx` lokal unter `data/` ablegen,
bevor die Skripte ausgeführt werden.

## Fallauswahl und Variablenbildung

Aus dem SoSci-Survey-Export (Zeile 1 = Spaltennamen, Zeile 2 = ausgeschriebene
Fragetexte ohne echte Antworten, Daten ab Zeile 3) werden nur Fälle mit
`QUESTNNR == "LKo"` und `STATUS == "complete"` behalten; Fälle ohne
Geschlechtsangabe (`SD02 == 4`) werden zusätzlich ausgeschlossen (N = 464).

Gebildete Variablen (Skript 01):

- `Informationale_wFoMO` — Mittelwert FM01_01–05
- `Relationale_wFoMO` — Mittelwert FM01_06–10
- `Arbeitszufriedenheit` — Mittelwert aus AZ01_01–06, AZ02_01, AZ03_01,
  jeweils um 4 zentriert (Rohskala 1–7 → −3 bis +3). AZ02_01/AZ03_01 wurden
  nur Teilgruppen (Führungsverantwortung bzw. Kundenkontakt) vorgelegt;
  der Mittelwert wird über die jeweils beantworteten Items gebildet
  (`na.rm = TRUE`), damit die Gesamtstichprobe erhalten bleibt.
- `HB01_kat`, `HB02_kat` — Homeoffice-Möglichkeit/-Nutzung als geordnete
  Faktoren (11 Kategorien); `HB01_tage`, `HB02_tage` — dieselben Angaben als
  metrische Näherung (Kategorienmitte) für die Regression
- `Geschlecht`, `Personalverantwortung` — Faktoren
- `Alter`, `Arbeitszeit` (Beschäftigungsumfang in %) — metrisch
- `Berufserfahrung` — SD04_01, Komma durch Punkt ersetzt und in eine Zahl
  umgewandelt

## Skripte

- `scripts/01_deskriptive_statistik.R` — Datenimport, Fallauswahl,
  Variablenbildung (s. o.) sowie deskriptive Statistik (relative
  Häufigkeiten für kategoriale Variablen; M, SD, Median, Min, Max für
  metrische Variablen).
- `scripts/02_cronbachs_alpha.R` — Reliabilitätsanalyse (Cronbachs Alpha)
  der Arbeitszufriedenheits- und wFoMO-Skalen.
- `scripts/03_regressionsanalyse.R` — Multiple Regression (KQ) für H1/H2a/H2b.
- `scripts/04_aic_selektion.R` — AIC-basierte Modellselektion (step-up/down).
- `scripts/05_robuste_regression_diagnostik.R` — Robuste Regression (Huber-M)
  und Prüfung der Regressionsannahmen auf dem AIC-reduzierten Modell.
- `scripts/06_mm_regression_ausreisser.R` — Robuste MM-Regression und
  Ausreißeridentifikation über die Hampel-Distanz.
- `scripts/07_regression_bereinigt.R` bis `09_robuste_regression_diagnostik_bereinigt.R`
  — dieselben Analyseschritte (KQ, AIC-Selektion, robuste Regression/Diagnostik)
  auf dem um Ausreißer bereinigten Datensatz (Pfad 2).
- `scripts/10_hypothesentabelle_h1_h2.R` — zusammenfassende Prüfung von
  H1/H2a/H2b über alle Modelle.
- `scripts/11_moderationsanalyse_h3.R` — Moderationsanalyse H3a/H3b.

Benötigte R-Pakete: `readxl`, `dplyr`, `tidyr`, `psych`, `MASS`, `lmtest`,
`car`, `robustbase`.
