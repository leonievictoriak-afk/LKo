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

- `scripts/01_deskriptive_statistik.R` — Datenimport, Fallauswahl
  (Fragebogen "LKo", nur vollständige Interviews, gültige Geschlechtsangabe),
  Bildung der wFoMO-Subskalen (informational/relational) und der
  Arbeitszufriedenheits-Kernskala sowie deskriptive Statistik (relative
  Häufigkeiten für kategoriale Variablen; M, SD, Median, Min, Max für
  metrische Variablen).

Benötigte R-Pakete: `readxl`, `dplyr`, `tidyr`, `psych`.
