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
  Zeile 1, Falldaten ab Zeile 3, da Zeile 2 nur die Item-Fragetexte enthält),
  Fallauswahl (Fragebogen "LKo", STATUS == "complete", ohne "keine Angabe"
  bei Geschlecht), Bildung der wFoMO-Subskalen (`wFoMO_informational`,
  `wFoMO_relational`) und von `Arbeitszufriedenheit` (Mittelwert aus
  AZ01_01-06, AZ02_01, AZ03_01; da AZ02_01/AZ03_01 nur von einem Teil der
  Stichprobe beantwortet wurden, ist diese Variable nur für die entsprechende
  Teilstichprobe definiert) sowie deskriptive Statistik (relative
  Häufigkeiten für kategoriale Variablen, u. a. `HB01_kat`/`HB02_kat`; M, SD,
  Median, Min, Max für metrische Variablen).
- `scripts/02_cronbachs_alpha.R` bis `scripts/11_moderationsanalyse_h3.R` —
  Reliabilitätsanalyse, multiple Regression (KQ), AIC-Selektion, robuste
  Regression/Diagnostik, Hampel-Ausreißeranalyse und Moderationsanalyse für
  H1-H3; siehe Kommentare in den jeweiligen Skripten für Details.
  `HB01_kat`/`HB02_kat` gehen als kategoriale Faktoren (nicht als metrische
  "Tage"-Variable) in die Modelle ein, H1 wird daher über einen
  Omnibus-F-Test statt über einen einzelnen Regressionskoeffizienten geprüft.

Benötigte R-Pakete: `readxl`, `dplyr`, `tidyr`, `psych`.
