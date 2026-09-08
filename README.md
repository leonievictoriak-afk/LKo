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

- `scripts/00_gesamtskript.R` — Alles-in-einer-Datei-Variante: installiert
  fehlende Pakete automatisch, importiert die Excel-Datei und führt die
  komplette Analyse (Schritt 1-11) in einem Durchlauf aus. Empfohlen, um die
  Analyse einmal komplett von Anfang bis Ende laufen zu lassen (Pfad zur
  Excel-Datei am Anfang der Datei anpassen).
- `scripts/01_deskriptive_statistik.R` bis `scripts/11_moderationsanalyse_h3.R`
  — dieselbe Analyse in einzelne, nacheinander auszuführende Schritte
  aufgeteilt (z. B. für die Methodendokumentation in der Bachelorarbeit).
  Inhaltlich identisch mit `00_gesamtskript.R`.
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

Benötigte R-Pakete: `readxl`, `dplyr`, `tidyr`, `psych`, `MASS`, `lmtest`,
`car`, `robustbase` (werden von `00_gesamtskript.R` automatisch installiert,
falls noch nicht vorhanden).
