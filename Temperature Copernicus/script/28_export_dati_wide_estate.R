# Esporta in "grafici interattivi/" i dati dei quattro grafici dell'estate in
# formato wide (una riga per valore dell'asse X, una colonna per serie).
# Gemello di 15_export_dati_wide.R.
#   - estate_annuale_wide.csv            anno | media | media_massime | media_minime
#   - giornaliero_maggio_agosto_wide.csv data | etichetta | t_2026 | t_2003 | t_2022 |
#                                        media_1961_1990 | media_1991_2020
#   - notti_tropicali_estate_wide.csv    anno | notti | media_1961_1990 | media_1991_2020
#   - ondate_calore_estate_wide.csv      anno | giorni_ondata | media_1961_1990 | media_1991_2020
#     (le due colonne-media sono valorizzate solo nei rispettivi trentenni, per
#      disegnare i segmenti di riferimento)
# Notti tropicali e ondate: legge il file definitivo di 20 se esiste, altrimenti
# il provvisorio (_provv.csv) creato da 24/27.

library(tidyverse)

setwd("/Users/lorenzoruffino/Documents/Progetti/data-viz/Temperature Copernicus")
dir.create("grafici interattivi", showWarnings = FALSE)

leggi_def_o_provv <- function(def, provv) {
  f <- if (file.exists(def)) def else provv
  if (!file.exists(f)) stop("Manca sia ", def, " sia ", provv)
  cat("  fonte:", f, "\n")
  read_csv(f, show_col_types = FALSE)
}

serie <- read_csv("output/serie_giornaliera_italia.csv", show_col_types = FALSE) |>
  mutate(anno = lubridate::year(data), mese = lubridate::month(data),
         giorno = lubridate::mday(data)) |>
  filter(!is.na(t_area_mean))

# ---- 1. Serie annuale dell'estate (stessa finestra di giorni di 21) ---------

estate <- serie |> filter(mese %in% 6:8)
ultimo_2026 <- max(estate$data[estate$anno == 2026])
annuale <- estate |>
  filter(mese < lubridate::month(ultimo_2026) |
           (mese == lubridate::month(ultimo_2026) & giorno <= lubridate::mday(ultimo_2026))) |>
  group_by(anno) |>
  summarise(media         = round(mean(t_area_mean), 2),
            media_massime = round(mean(t_area_max), 2),
            media_minime  = round(mean(t_area_min), 2), .groups = "drop")
write_csv(annuale, "grafici interattivi/estate_annuale_wide.csv")

# ---- 2. Giornaliero maggio-agosto -------------------------------------------

media_trentennio <- function(x, min_anni = 20) {
  if (sum(!is.na(x)) < min_anni) NA_real_ else round(mean(x, na.rm = TRUE), 2)
}
media_anno <- function(x) if (length(x) == 0) NA_real_ else round(mean(x), 2)

mesi_it <- c("maggio", "giugno", "luglio", "agosto")
giorni <- serie |>
  filter(mese %in% 5:8) |>
  group_by(mese, giorno) |>
  summarise(
    t_2026          = media_anno(t_area_mean[anno == 2026]),
    t_2003          = media_anno(t_area_mean[anno == 2003]),
    t_2022          = media_anno(t_area_mean[anno == 2022]),
    media_1961_1990 = media_trentennio(t_area_mean[anno %in% 1961:1990]),
    media_1991_2020 = media_trentennio(t_area_mean[anno %in% 1991:2020]),
    .groups = "drop") |>
  mutate(data = sprintf("2026-%02d-%02d", mese, giorno),
         etichetta = paste(giorno, mesi_it[mese - 4])) |>
  arrange(data) |>
  select(data, etichetta, t_2026, t_2003, t_2022, media_1961_1990, media_1991_2020)
write_csv(giorni, "grafici interattivi/giornaliero_maggio_agosto_wide.csv", na = "")

# ---- 3. Notti tropicali per abitante (estate) -------------------------------

cat("Notti tropicali:\n")
soglie <- leggi_def_o_provv("output/soglie_estate_italia_pop.csv",
                            "output/soglie_estate_italia_pop_provv.csv")
if ("peso" %in% names(soglie)) soglie <- soglie |> filter(peso == "pop")
soglie <- soglie |> filter(!is.na(n_tmin20))
m6190 <- mean(soglie$n_tmin20[soglie$anno %in% 1961:1990])
m9120 <- mean(soglie$n_tmin20[soglie$anno %in% 1991:2020])

notti <- soglie |>
  transmute(anno,
            notti = round(n_tmin20, 1),
            media_1961_1990 = ifelse(anno %in% 1961:1990, round(m6190, 1), NA),
            media_1991_2020 = ifelse(anno %in% 1991:2020, round(m9120, 1), NA))
write_csv(notti, "grafici interattivi/notti_tropicali_estate_wide.csv", na = "")

# ---- 4. Giorni in ondata di calore (estate) ---------------------------------

cat("Ondate di calore:\n")
ondate <- leggi_def_o_provv("output/ondate_calore_estate.csv",
                            "output/ondate_calore_estate_provv.csv")
if ("peso" %in% names(ondate)) ondate <- ondate |> filter(peso == "area")
ondate <- ondate |> filter(!is.na(giorni_ondata))
o6190 <- mean(ondate$giorni_ondata[ondate$anno %in% 1961:1990])
o9120 <- mean(ondate$giorni_ondata[ondate$anno %in% 1991:2020])

ondate_w <- ondate |>
  transmute(anno,
            giorni_ondata,
            media_1961_1990 = ifelse(anno %in% 1961:1990, round(o6190, 1), NA),
            media_1991_2020 = ifelse(anno %in% 1991:2020, round(o9120, 1), NA))
write_csv(ondate_w, "grafici interattivi/ondate_calore_estate_wide.csv", na = "")

cat("Esportati:\n  estate_annuale_wide.csv:", nrow(annuale), "righe\n",
    " giornaliero_maggio_agosto_wide.csv:", nrow(giorni), "righe\n",
    " notti_tropicali_estate_wide.csv:", nrow(notti), "righe\n",
    " ondate_calore_estate_wide.csv:", nrow(ondate_w), "righe\n")
