##############################################################################
# Bachelorarbeit: Hybrides Arbeiten, Workplace FoMO (wFoMO) und
#                 Arbeitszufriedenheit im öffentlichen Dienst
#
# Schritt 2: Reliabilitätsanalyse (Cronbachs Alpha) der eingesetzten Skalen
#
# Voraussetzung: 01_deskriptive_statistik.R wurde bereits ausgeführt,
#                sodass das Objekt "daten" (gefilterte Stichprobe) im
#                Workspace vorhanden ist.
##############################################################################

# install.packages("psych")
library(dplyr)
library(psych)

# psych::alpha() benötigt eine rein numerische Matrix/data.frame. Da Excel-
# Importe (readxl) Item-Spalten je nach Formatierung gelegentlich nicht als
# reine Zahl einlesen, werden die Items hier vorsorglich explizit in
# numerische Werte umgewandelt und in einen Basis-data.frame überführt.
numerisch_df <- function(x) as.data.frame(lapply(x, as.numeric))

# ---- 1. Arbeitszufriedenheit (Kernskala, 6 Items: AZ01_01 - AZ01_06) -------
az_items <- daten %>% select(AZ01_01:AZ01_06) %>% numerisch_df()
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

# ---- 3. Workplace FoMO - relationale Subskala (5 Items: FM01_06-10) --------
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

# ---- 4. Zusammenfassende Übersicht ----------------------------------------
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
