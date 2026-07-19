# Analisi di maggio e giugno 2026 sulle serie ERA5-Land:
#   - confronto con le medie 1961-1990 e 1991-2020 e con il 2003 (differenze in °C)
#   - sia con media pesata per superficie sia pesata per popolazione 2021
#   - dettaglio giornaliero di giugno e dettaglio regionale
#
# NB: finché giugno 2026 non è completo, tutti i confronti di giugno sono
# calcolati sulla stessa finestra di giorni disponibile nel 2026 (es. 1-26)
# anche per baseline e 2003, così il confronto è omogeneo.
#
# Output: output/analisi_riepilogo_2026.csv
#         output/giugno_2026_giornaliero.csv
#         output/analisi_regioni_2026.csv

library(tidyverse)

setwd("/Users/lorenzoruffino/Documents/Progetti/data-viz/Temperature Copernicus")

fmt <- function(x, d = 2) formatC(x, format = "f", digits = d, decimal.mark = ",")

allunga <- function(df) {
  df |>
    pivot_longer(starts_with("t_"),
                 names_to = c("peso", "stat"),
                 names_pattern = "t_(area|pop)_(mean|min|max)",
                 values_to = "valore") |>
    mutate(anno = lubridate::year(data),
           mese = lubridate::month(data),
           giorno = lubridate::mday(data))
}

serie <- allunga(read_csv("output/serie_giornaliera_italia.csv", show_col_types = FALSE))

finestra_giugno <- serie |> filter(anno == 2026, mese == 6) |> pull(giorno) |> max()
cat("Giorni disponibili per giugno 2026: 1-", finestra_giugno, "\n\n", sep = "")

# ---- Riepilogo mensile Italia ----------------------------------------------

riepilogo_mese <- function(df, m, max_giorno = 31, gruppi = NULL) {
  df |>
    filter(mese == m, giorno <= max_giorno) |>
    group_by(across(all_of(c(gruppi, "peso", "stat"))), anno) |>
    summarise(t = mean(valore), .groups = "drop_last") |>
    summarise(
      n_anni_6190     = sum(anno %in% 1961:1990),
      n_anni_9120     = sum(anno %in% 1991:2020),
      media_1961_1990 = mean(t[anno %in% 1961:1990]),
      media_1991_2020 = mean(t[anno %in% 1991:2020]),
      t_2003          = mean(t[anno == 2003]),
      t_2026          = mean(t[anno == 2026]),
      .groups = "drop") |>
    mutate(mese = m,
           giorni = paste0("1-", max_giorno),
           vs_1961_1990 = t_2026 - media_1961_1990,
           vs_1991_2020 = t_2026 - media_1991_2020,
           vs_2003      = t_2026 - t_2003)
}

riepilogo <- bind_rows(
  riepilogo_mese(serie, 5),
  riepilogo_mese(serie, 6, finestra_giugno)
) |>
  relocate(mese, giorni)

write_csv(riepilogo, "output/analisi_riepilogo_2026.csv")

if (any(riepilogo$n_anni_6190 < 30) || any(riepilogo$n_anni_9120 < 30)) {
  cat("ATTENZIONE: baseline incompleta (mancano anni nei download)\n")
  print(riepilogo |> distinct(mese, n_anni_6190, n_anni_9120))
}

for (m in c(5, 6)) {
  nome <- ifelse(m == 5, "MAGGIO", "GIUGNO")
  fin <- ifelse(m == 5, "mese intero", paste0("giorni 1-", finestra_giugno, " (stessa finestra per tutti i periodi)"))
  cat("=== ", nome, " 2026, Italia — ", fin, " ===\n", sep = "")
  tab <- riepilogo |>
    filter(mese == m) |>
    mutate(etichetta = paste0(recode(stat, mean = "T media", min = "T minima", max = "T massima"),
                              ", peso ", recode(peso, area = "superficie", pop = "popolazione")))
  for (i in seq_len(nrow(tab))) {
    r <- tab[i, ]
    cat(sprintf("%-32s  2026: %s°C | 61-90: %s (%+s) | 91-20: %s (%+s) | 2003: %s (%+s)\n",
                r$etichetta, fmt(r$t_2026),
                fmt(r$media_1961_1990), fmt(r$vs_1961_1990),
                fmt(r$media_1991_2020), fmt(r$vs_1991_2020),
                fmt(r$t_2003), fmt(r$vs_2003)))
  }
  cat("\n")
}

# ---- Dettaglio giornaliero di giugno ---------------------------------------

giugno_giorni <- serie |>
  filter(mese == 6) |>
  group_by(peso, stat, giorno) |>
  summarise(
    t_2026          = mean(valore[anno == 2026]),
    clim_1961_1990  = mean(valore[anno %in% 1961:1990]),
    clim_1991_2020  = mean(valore[anno %in% 1991:2020]),
    t_2003          = mean(valore[anno == 2003]),
    .groups = "drop") |>
  mutate(vs_1961_1990 = t_2026 - clim_1961_1990,
         vs_1991_2020 = t_2026 - clim_1991_2020,
         vs_2003      = t_2026 - t_2003)

write_csv(giugno_giorni, "output/giugno_2026_giornaliero.csv")

cat("=== GIUGNO 2026 giorno per giorno (T media, peso superficie) ===\n")
gg <- giugno_giorni |> filter(peso == "area", stat == "mean", giorno <= finestra_giugno)
for (i in seq_len(nrow(gg))) {
  r <- gg[i, ]
  cat(sprintf("%2d giugno  2026: %s°C | vs 61-90: %+s | vs 91-20: %+s | vs 2003: %+s\n",
              r$giorno, fmt(r$t_2026, 1), fmt(r$vs_1961_1990, 1),
              fmt(r$vs_1991_2020, 1), fmt(r$vs_2003, 1)))
}
cat("\n")

# ---- Dettaglio regionale ----------------------------------------------------

regioni <- allunga(read_csv("output/serie_giornaliera_regioni.csv.gz", show_col_types = FALSE) |>
                     rename(data = data) )

riepilogo_reg <- bind_rows(
  riepilogo_mese(regioni, 5, gruppi = "regione"),
  riepilogo_mese(regioni, 6, finestra_giugno, gruppi = "regione")
)

write_csv(riepilogo_reg, "output/analisi_regioni_2026.csv")

cat("=== GIUGNO 2026 per regione (T media, peso superficie) — vs 1991-2020 ===\n")
rr <- riepilogo_reg |>
  filter(mese == 6, peso == "area", stat == "mean") |>
  arrange(desc(vs_1991_2020))
for (i in seq_len(nrow(rr))) {
  r <- rr[i, ]
  cat(sprintf("%-22s 2026: %s°C | vs 61-90: %+s | vs 91-20: %+s | vs 2003: %+s\n",
              r$regione, fmt(r$t_2026, 1), fmt(r$vs_1961_1990, 1),
              fmt(r$vs_1991_2020, 1), fmt(r$vs_2003, 1)))
}
