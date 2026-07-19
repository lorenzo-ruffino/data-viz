suppressMessages({
  library(eurostat)
  library(dplyr)
})

dir_in <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Spesa Pubblica Europa/input"

# gov_10a_exp = General government expenditure by function (COFOG)
# Scarico filtrato per i paesi di interesse + media UE, settore S13 (general government)
geo_keep <- c("IT","FR","DE","ES","EU27_2020")

# bulk download (l'API JSON con filtri fallisce su dataset grandi)
full <- get_eurostat("gov_10a_exp", time_format = "num", cache = FALSE)
raw <- as.data.frame(full) %>%
  filter(geo %in% geo_keep, sector == "S13")
saveRDS(raw, file.path(dir_in, "gov_10a_exp_raw.rds"))
write.csv(raw, file.path(dir_in, "gov_10a_exp_raw.csv"), row.names = FALSE)

cat("=== DIMENSIONI ===\n")
cat("righe:", nrow(raw), " colonne:", ncol(raw), "\n")
cat("nomi colonne:", paste(names(raw), collapse=", "), "\n\n")

cat("=== ANNI DISPONIBILI ===\n")
print(sort(unique(raw$TIME_PERIOD)))

cat("\n=== UNIT ===\n")
print(sort(unique(raw$unit)))

cat("\n=== NA_ITEM ===\n")
print(sort(unique(raw$na_item)))

cat("\n=== COFOG99 ===\n")
print(sort(unique(raw$cofog99)))

cat("\n=== SECTOR ===\n")
print(sort(unique(raw$sector)))

cat("\n=== Disponibilita 2024 per paese (na_item TE, unit PC_GDP, cofog GF01) ===\n")
chk <- raw %>%
  filter(TIME_PERIOD == 2024, na_item == "TE", unit == "PC_GDP", cofog99 == "GF01")
print(chk[, c("geo","cofog99","values")])
