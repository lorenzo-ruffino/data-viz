# ============================================================
# Mismatch VERTICALE - Serie storica sovraistruzione 2015-2025
# ============================================================
# Tasso = laureati occupati in ISCO 4-8 / laureati occupati ISCO 1-8
#
# Attenzione ai break di classificazione (riforma LFS 2021):
#  - Titolo terziario:
#      2015-2020 : TISTUD (10 classi) in {6,7,8,9,10}
#                  (AFAM, diploma univ., triennale, magistrale,
#                   ciclo unico / vecchio ordinamento)
#      2021-2025 : HATLEV3MOD == 3  (ISCED 5-8)
#  - Peso:
#      2015-2020 : COEFMI  /10
#      2021-2025 : COEF_CCP /10  (eccezione 2021 Q1: /1)
#  - Professione: PROF1 (ISCO 1-digit) stabile su tutto il periodo.
# ============================================================

library(dplyr); library(tidyr); library(readr); library(stringr); library(purrr)

micro_dir <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Lavoro da remoto/input"
out_dir   <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Mismatch istruzione e lavoro/output"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

files_df <- tibble(filename = list.files(micro_dir, pattern = "\\.txt$")) %>%
  mutate(
    anno = as.integer(str_extract(filename, "20\\d{2}")),
    trim = case_when(
      str_detect(filename, "Primo")   ~ 1L,
      str_detect(filename, "Secondo") ~ 2L,
      str_detect(filename, "Terzo")   ~ 3L,
      str_detect(filename, "Quarto")  ~ 4L),
    era        = if_else(anno <= 2020, "old", "new"),
    weight_col = if_else(anno <= 2020, "COEFMI", "COEF_CCP"),
    weight_div = if_else(anno == 2021 & trim == 1, 1, 10),
    path = file.path(micro_dir, filename)) %>%
  filter(!is.na(trim)) %>% arrange(anno, trim)

cat(sprintf("File: %d (%dQ%d - %dQ%d)\n", nrow(files_df),
            min(files_df$anno), files_df$trim[1], max(files_df$anno), tail(files_df$trim,1)))

proc <- function(path, era, weight_col, weight_div, anno, trim) {
  cols <- c("COND3", weight_col, "PROF1", "TISTUD")
  if (era == "new") cols <- c(cols, "HATLEV3MOD")
  df <- read_delim(path, delim = "\t", col_select = all_of(cols),
                   col_types = cols(.default = "c"), show_col_types = FALSE, progress = FALSE)
  df <- df %>% mutate(
    peso  = as.numeric(str_trim(.data[[weight_col]])) / weight_div,
    cond3 = as.integer(str_trim(COND3)),
    prof1 = as.integer(str_trim(PROF1)),
    tist  = as.integer(str_trim(TISTUD)))
  if (era == "new") {
    df <- df %>% mutate(terz = as.integer(str_trim(HATLEV3MOD)) == 3L)
  } else {
    df <- df %>% mutate(terz = tist %in% c(6L,7L,8L,9L,10L))
  }
  df %>%
    filter(cond3 == 1L, !is.na(peso), peso > 0, !is.na(prof1)) %>%
    summarise(
      occ_tot      = sum(peso),
      terz_occ     = sum(peso[terz]),
      base         = sum(peso[terz & prof1 %in% 1:8]),
      sovra        = sum(peso[terz & prof1 %in% 4:8]),
      n_terz       = sum(terz & prof1 %in% 1:8),
      .groups = "drop") %>%
    mutate(anno = anno, trim = trim, era = era)
}

cat("Lettura microdati (44 trimestri)...\n")
serie_q <- files_df %>%
  mutate(r = pmap(list(path, era, weight_col, weight_div, anno, trim), proc)) %>%
  select(r) %>% unnest(r) %>%
  mutate(
    quota_laureati = round(terz_occ / occ_tot * 100, 1),
    tasso_sovra    = round(sovra / base * 100, 1),
    trimestre      = sprintf("%dQ%d", anno, trim)) %>%
  arrange(anno, trim)

cat("\n===== SERIE TRIMESTRALE =====\n")
print(as.data.frame(serie_q %>% select(trimestre, era, quota_laureati, tasso_sovra, n_terz)), row.names = FALSE)

# Media annuale
serie_a <- serie_q %>% group_by(anno) %>%
  summarise(
    n_trim         = n(),
    quota_laureati = round(mean(quota_laureati), 1),
    tasso_sovra    = round(sum(sovra)/sum(base)*100, 1),
    laureati_occ_migliaia = round(mean(terz_occ)/1000, 0),
    .groups = "drop")
cat("\n===== MEDIA ANNUALE =====\n")
print(as.data.frame(serie_a), row.names = FALSE)

write_csv(serie_q, file.path(out_dir, "v10_serie_trimestrale_2015_2025.csv"))
write_csv(serie_a, file.path(out_dir, "v11_serie_annuale_2015_2025.csv"))
cat(sprintf("\nOutput salvati in %s\n", out_dir))
