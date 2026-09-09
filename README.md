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
  sowie `HB01_kat`/`HB02_kat` als kategoriale Faktoren. Anschließend
  deskriptive Statistik (M, SD, Median, Min, Max für metrische Variablen;
  absolute/relative Häufigkeiten für kategoriale Variablen).

Benötigte R-Pakete: `openxlsx`, `psych`.

**Hinweis:** Die nachgelagerten Skripte `02`–`11` referenzieren noch die
Variablennamen und den Importweg der vorherigen Version von Skript 01
(u. a. `az_kern`, `wfomo_informational`, `hb_moeglichkeit_tage` als
metrische Näherung). Sie müssen an die oben beschriebene, aktualisierte
Variablenbildung aus Skript 01 angepasst werden, bevor sie erneut
ausgeführt werden.
