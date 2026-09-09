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

## Skripte

- `scripts/01_deskriptive_statistik.R` — Datenimport (Header separat aus
  Zeile 1, Falldaten ab Zeile 3, da Zeile 2 nur die ausgeschriebenen
  Fragetexte enthält), Fallauswahl (QUESTNNR == "LKo", STATUS == "complete",
  Ausschluss SD02 == 4 "keine Angabe"), Bildung von `wFoMO_informational`
  (Mittelwert FM01_01–FM01_05), `wFoMO_relational` (Mittelwert
  FM01_06–FM01_10), `Arbeitszufriedenheit` (Mittelwert der auf −3 bis +3
  rekodierten AZ-Items, inkl. der freiwilligen Items AZ02_01/AZ03_01, jeweils
  na.rm = TRUE) mit Interpretationskategorien (unzufrieden/neutral/zufrieden)
  sowie `HB01_kat`/`HB02_kat` als kategoriale Faktoren (rein deskriptiv) und
  `HB01_ord` als zusätzlich geordneter Faktor für die Regressionsmodelle
  (siehe Hinweis unten). Anschließend deskriptive Statistik (M, SD, Median,
  Min, Max für metrische Variablen; absolute/relative Häufigkeiten für
  kategoriale Variablen).
- `scripts/02_cronbachs_alpha.R` — Reliabilitätsanalyse (Cronbachs Alpha) für
  die Arbeitszufriedenheits-Kernskala und die beiden wFoMO-Subskalen.
- `scripts/03_regressionsanalyse.R` — multiple KQ-Regression
  `Arbeitszufriedenheit ~ HB01_ord + wFoMO_informational + wFoMO_relational`
  (H1, H2a, H2b), F-Test, standardisierte Koeffizienten.
- `scripts/04_aic_selektion.R` — schrittweise und vollständige
  AIC-Modellselektion auf dem vollständigen Datensatz.
- `scripts/05_robuste_regression_diagnostik.R` — robuste Regression
  (Huber-M-Schätzer) auf dem AIC-reduzierten Modell sowie Shapiro-Wilk-,
  Breusch-Pagan-, VIF-, Durbin-Watson- und RESET-Test.
- `scripts/06_mm_regression_ausreisser.R` — robuste MM-Regression auf dem
  vollen Modell und Ausreißeridentifikation über die Hampel-Distanz
  (Cut-off 5).
- `scripts/07_regression_bereinigt.R` — Regressionsanalyse auf dem um
  Hampel-Ausreißer bereinigten Datensatz (`Dataset_bereinigt`, Pfad 2).
- `scripts/08_aic_selektion_bereinigt.R` — AIC-Modellselektion auf Pfad 2.
- `scripts/09_robuste_regression_diagnostik_bereinigt.R` — robuste Regression
  und Nachtests auf Pfad 2.
- `scripts/10_hypothesentabelle_h1_h2.R` — zusammenfassende Prüfung von
  H1/H2a/H2b über alle vier Modelle (Pfad 1/2 × voll/AIC-reduziert).
- `scripts/11_moderationsanalyse_h3.R` — hierarchische Moderationsanalyse
  für H3a/H3b (Interaktion hybrides Arbeiten × wFoMO), Pfad 1 und Pfad 2.

Benötigte R-Pakete: `openxlsx`, `psych`, `dplyr`, `MASS`, `lmtest`, `car`,
`robustbase`.

**Methodischer Hinweis zu HB01 (Homeoffice-/hybride-Arbeiten-Möglichkeit):**
Gemäß Vorgabe wird HB01 in Schritt 1 als kategoriale Variable gebildet
(`HB01_kat = factor(HB01)`, 11 Stufen, für die deskriptive Statistik). Da
HB01 eine natürliche Rangordnung besitzt (0 Tage < … < 5 Tage) und die
Hypothesen H1 sowie H3a/H3b eine GERICHTETE Beziehung postulieren
("je mehr hybrides Arbeiten, desto …"), wird für die Regressionsmodelle
(Skripte 03–11) zusätzlich `HB01_ord` gebildet — derselbe Faktor, aber mit
`ordered = TRUE`. Dadurch bleibt HB01 weiterhin kategorial (keine
Tage-Metrik/Abstandsannahme), während der lineare Polynomkontrast
`HB01_ord.L` einen einzelnen, testbaren Koeffizienten für die gerichteten
Hypothesen liefert. Diese Modellierungsentscheidung sollte mit dem
betreuenden Dozenten abgestimmt werden; alternativ könnte HB01 als
ungeordneter Faktor (Omnibus-F-Test statt gerichtetem Einzelkoeffizienten)
oder wie im ursprünglichen Skript als metrische Tage-Näherung modelliert
werden.
