##############################################################################
# Bachelorarbeit: Hybrides Arbeiten, Workplace FoMO (wFoMO) und
#                 Arbeitszufriedenheit im öffentlichen Dienst
#
# Schritt 2: Reliabilitätsanalyse (Cronbachs Alpha) der eingesetzten Skalen
#
# Voraussetzung: 01_deskriptive_statistik.R wurde bereits ausgeführt,
#                sodass das Objekt "Dataset" (gefilterte Stichprobe) im
#                Workspace vorhanden ist.
##############################################################################

# install.packages("psych")
library(dplyr)
library(psych)

# psych::alpha() benötigt eine rein numerische Matrix/data.frame. Die
# Item-Spalten wurden in Schritt 1 bereits explizit in numerische Werte
# umgewandelt; die Umwandlung wird hier defensiv wiederholt, falls dieses
# Skript unabhängig von Schritt 1 auf einem anderen Datenobjekt läuft.
numerisch_df <- function(x) as.data.frame(lapply(x, as.numeric))

# ---- 1. Arbeitszufriedenheit (Kernskala, 6 Items: AZ01_01 - AZ01_06) -------
az_items <- Dataset %>% select(AZ01_01:AZ01_06) %>% numerisch_df()
alpha_az <- psych::alpha(az_items, check.keys = TRUE)   # check.keys = TRUE erkennt/korrigiert
                                                          # automatisch invers gepolte Items
                                                          # (hier nicht der Fall, aber als
                                                          # Absicherung sinnvoll)

cat("\n=== Cronbachs Alpha: Arbeitszufriedenheit (Kernskala, 6 Items) ===\n")
print(alpha_az$total[, c("raw_alpha", "std.alpha", "average_r")])
cat("\nTrennschärfe je Item (corrected item-total correlation) und Alpha bei\n")
cat("Ausschluss des jeweiligen Items:\n")
print(round(data.frame(
  r.drop      = alpha_az$item.stats$r.drop,
  alpha.drop  = alpha_az$alpha.drop$raw_alpha,
  row.names   = rownames(alpha_az$item.stats)
), 3))

# ---- 2. Workplace FoMO - informationale Subskala (5 Items: FM01_01-05) -----
fomo_info_items <- Dataset %>% select(FM01_01:FM01_05) %>% numerisch_df()
alpha_fomo_info <- psych::alpha(fomo_info_items, check.keys = TRUE)

cat("\n=== Cronbachs Alpha: wFoMO informational (5 Items) ===\n")
print(alpha_fomo_info$total[, c("raw_alpha", "std.alpha", "average_r")])
cat("\nTrennschärfe je Item und Alpha bei Ausschluss des jeweiligen Items:\n")
print(round(data.frame(
  r.drop      = alpha_fomo_info$item.stats$r.drop,
  alpha.drop  = alpha_fomo_info$alpha.drop$raw_alpha,
  row.names   = rownames(alpha_fomo_info$item.stats)
), 3))

# ---- 3. Workplace FoMO - relationale Subskala (5 Items: FM01_06-10) --------
fomo_rel_items <- Dataset %>% select(FM01_06:FM01_10) %>% numerisch_df()
alpha_fomo_rel <- psych::alpha(fomo_rel_items, check.keys = TRUE)

cat("\n=== Cronbachs Alpha: wFoMO relational (5 Items) ===\n")
print(alpha_fomo_rel$total[, c("raw_alpha", "std.alpha", "average_r")])
cat("\nTrennschärfe je Item und Alpha bei Ausschluss des jeweiligen Items:\n")
print(round(data.frame(
  r.drop      = alpha_fomo_rel$item.stats$r.drop,
  alpha.drop  = alpha_fomo_rel$alpha.drop$raw_alpha,
  row.names   = rownames(alpha_fomo_rel$item.stats)
), 3))

# ---- 4. Zusammenfassende Übersicht ----------------------------------------
zusammenfassung_alpha <- data.frame(
  Skala   = c("Arbeitszufriedenheit (Kernskala)", "wFoMO informational", "wFoMO relational"),
  Items   = c(6, 5, 5),
  N       = nrow(Dataset),
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

# ---- 5. Arbeitszufriedenheit: Kernskala vs. erweiterte Skala ---------------
# Zusatzprüfung: Wie verändert sich die interne Konsistenz, wenn man nur die
# Subgruppe betrachtet, die zusätzlich BEIDE freiwilligen Items beantwortet
# hat (AZ02_01 = Zufriedenheit mit Mitarbeitenden, AZ03_01 = Zufriedenheit
# mit Kundinnen/Kunden)? Diese 8-Item-Variante entspricht konzeptionell der
# in Schritt 1 gebildeten Variable "Arbeitszufriedenheit" (dort auf Basis
# aller tatsächlich beantworteten Items, na.rm = TRUE, über die gesamte
# Stichprobe).
#   (a) 6 Pflichtitems, gesamte gültige Stichprobe        [siehe Abschnitt 1]
#   (b) dieselben 6 Pflichtitems, aber nur für die Subgruppe
#   (c) vollständige 8-Item-Skala (6 Pflicht- + 2 freiwillige), Subgruppe

subgruppe_freiwillig <- Dataset %>%
  filter(!is.na(AZ02_01), !is.na(AZ03_01))

n_subgruppe <- nrow(subgruppe_freiwillig)
cat("\nN Gesamtstichprobe (6 Pflichtitems):                           ", nrow(Dataset), "\n")
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
  N     = c(nrow(Dataset), n_subgruppe, n_subgruppe),
  Alpha = round(c(alpha_az$total$raw_alpha,
                   alpha_az6_sub$total$raw_alpha,
                   alpha_az8_sub$total$raw_alpha), 3)
)

cat("\n=== Vergleich: Arbeitszufriedenheit - Kernskala vs. erweiterte Skala ===\n")
print(vergleich_az)
