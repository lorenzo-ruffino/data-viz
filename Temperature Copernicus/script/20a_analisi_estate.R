# Analisi dell'ESTATE 2026 (giugno+luglio+agosto, "JJA") in Italia, gemello
# stagionale di 05_analisi_maggio_giugno.R + 12_analisi_articolo.R.
# Legge le serie prodotte da 04_elabora_dati.R e 07_giorno_notte.R e produce:
#   1. riepilogo JJA 2026 (media, massime, minime; peso superficie e popolazione)
#      con anomalie vs 1961-1990, 1991-2020, 2003 e 2022 e classifica dal 1961
#   2. tabella mese per mese (maggio-agosto 2026)
#   3. dinamica giornaliera: record per la data, giorni oltre +5, sequenze
#   4. ondate di calore sulla serie nazionale (90° pct delle massime JJA 91-20)
#   6. trend 1961-2026 (nazionale, per cella, per regione) e giorno vs notte
#   7. esposizione della popolazione per fascia di anomalia; celle più calde
#   8. regioni: anomalia, posto nella storia, record
# (il punto 5, soglie per cella dagli orari, sta in 20b_soglie_estate.R)
#
# Finestra omogenea: finché agosto 2026 è parziale (es. giorni 1-30), TUTTI i
# confronti stagionali e mensili usano la stessa finestra di giorni anche per
# baseline e anni di confronto. Gli anni con la stagione incompleta (agosto
# storico non ancora scaricato) sono esclusi da medie e classifiche, e il log
# segnala quanti anni entrano in ogni baseline.
#
# Output (tutti in output/):
#   estate_annuale.csv, analisi_riepilogo_estate_2026.csv,
#   analisi_mensile_estate_2026.csv, estate_2026_giornaliero.csv,
#   ondate_calore_estate.csv, ondate_calore_estate_2026_episodi.csv,
#   trend_estate_italia.csv, trend_estate_celle.csv, trend_estate_regioni.csv,
#   giorno_notte_estate_riepilogo.csv, griglia_estate.csv.gz,
#   esposizione_estate_2026.csv, celle_top_estate_2026.csv,
#   analisi_regioni_estate_2026.csv, classifica_regioni_estate_2026.csv

suppressPackageStartupMessages(library(tidyverse))

setwd("/Users/lorenzoruffino/Documents/Progetti/data-viz/Temperature Copernicus")
t_inizio <- Sys.time()

fmt  <- function(x, d = 2) ifelse(is.na(x), "n.d.", formatC(x, format = "f", digits = d, decimal.mark = ","))
fmts <- function(x, d = 2) ifelse(is.na(x), "n.d.", formatC(x, format = "f", digits = d, decimal.mark = ",", flag = "+"))
sep  <- function(titolo) cat("\n", strrep("=", 78), "\n", titolo, "\n", strrep("=", 78), "\n", sep = "")

ANNO   <- 2026L
BASE1  <- 1961:1990
BASE2  <- 1991:2020
MESI_JJA <- 6:8
nome_mese <- c("5" = "maggio", "6" = "giugno", "7" = "luglio", "8" = "agosto")

celle <- read_csv("input/geo/celle_griglia.csv", show_col_types = FALSE)

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

# ---- Finestra di giorni disponibile nel 2026 per ogni mese ------------------
finestre <- serie |>
  filter(anno == ANNO, mese %in% 5:8) |>
  group_by(mese) |>
  summarise(fin = max(giorno), .groups = "drop")
fin_di <- function(m) { f <- finestre$fin[finestre$mese == m]; if (length(f) == 0) 0L else f }
n_attesi_jja <- sum(map_int(MESI_JJA, fin_di))
etichetta_finestra <- paste(map_chr(MESI_JJA, ~ sprintf("%s 1-%d", nome_mese[as.character(.x)], fin_di(.x))),
                            collapse = ", ")
agosto_parziale <- fin_di(8) < 31

sep("ESTATE 2026 — impostazioni")
cat("Ultimo giorno disponibile nel 2026: ", paste(nome_mese[as.character(finestre$mese)],
                                                    finestre$fin, collapse = " | "), "\n", sep = "")
cat("Finestra stagionale JJA usata per tutti gli anni: ", etichetta_finestra,
    " (", n_attesi_jja, " giorni)\n", sep = "")
if (agosto_parziale) cat("ATTENZIONE: agosto 2026 PARZIALE (giorni 1-", fin_di(8),
                         "): confronti su finestra omogenea.\n", sep = "")

in_finestra <- function(df) df |> filter(mese %in% MESI_JJA, giorno <= map_int(mese, fin_di))

# ---- Funzione di riepilogo comune -------------------------------------------
# ann: tibble con anno, t (NA se stagione/mese incompleto) e colonne di gruppo
riepiloga <- function(ann, gruppi) {
  ann |>
    group_by(across(all_of(gruppi))) |>
    summarise(
      n_anni_6190     = sum(!is.na(t[anno %in% BASE1])),
      n_anni_9120     = sum(!is.na(t[anno %in% BASE2])),
      n_anni_tot      = sum(!is.na(t)),
      media_1961_1990 = mean(t[anno %in% BASE1], na.rm = TRUE),
      media_1991_2020 = mean(t[anno %in% BASE2], na.rm = TRUE),
      t_2003          = t[anno == 2003][1],
      t_2022          = t[anno == 2022][1],
      t_2026          = t[anno == ANNO][1],
      posto_2026      = if (is.na(t[anno == ANNO][1])) NA_integer_
                        else sum(t > t[anno == ANNO][1], na.rm = TRUE) + 1L,
      record          = !is.na(t[anno == ANNO][1]) & t[anno == ANNO][1] >= suppressWarnings(max(t, na.rm = TRUE)),
      .groups = "drop") |>
    mutate(vs_1961_1990 = t_2026 - media_1961_1990,
           vs_1991_2020 = t_2026 - media_1991_2020,
           vs_2003      = t_2026 - t_2003,
           vs_2022      = t_2026 - t_2022)
}

stampa_riga <- function(r, etichetta, d = 2) {
  cat(sprintf("%-34s 2026: %s°C | 61-90: %s (%s, %d anni) | 91-20: %s (%s, %d anni) | 2003: %s (%s) | 2022: %s (%s) | %s° su %d\n",
              etichetta, fmt(r$t_2026, d),
              fmt(r$media_1961_1990, d), fmts(r$vs_1961_1990, d), r$n_anni_6190,
              fmt(r$media_1991_2020, d), fmts(r$vs_1991_2020, d), r$n_anni_9120,
              fmt(r$t_2003, d), fmts(r$vs_2003, d), fmt(r$t_2022, d), fmts(r$vs_2022, d),
              r$posto_2026, r$n_anni_tot))
}

# ============ 1. RIEPILOGO STAGIONALE JJA ===================================

sep("1. ESTATE (JJA) 2026, Italia — media dei giorni sulla finestra omogenea")

estate_annuale <- serie |>
  in_finestra() |>
  group_by(peso, stat, anno) |>
  summarise(t = mean(valore), n_giorni = n(), .groups = "drop") |>
  mutate(completo = n_giorni == n_attesi_jja,
         t = ifelse(completo, t, NA_real_))
write_csv(estate_annuale, "output/estate_annuale.csv")

anni_completi <- estate_annuale |> filter(peso == "area", stat == "mean", completo) |> pull(anno)
cat("Estati con dati completi sulla finestra: ", length(anni_completi), " (",
    paste(range(anni_completi), collapse = "-"), ")\n", sep = "")
if (length(anni_completi) < 66) {
  mancanti <- setdiff(1961:ANNO, anni_completi)
  cat("ATTENZIONE: estati incomplete (agosto non ancora scaricato) escluse da medie e classifiche: ",
      length(mancanti), " anni [", min(mancanti), "-", max(mancanti), "]\n", sep = "")
}

riepilogo_estate <- riepiloga(estate_annuale, c("peso", "stat")) |>
  mutate(finestra = etichetta_finestra, n_giorni = n_attesi_jja) |>
  relocate(finestra, n_giorni)
write_csv(riepilogo_estate, "output/analisi_riepilogo_estate_2026.csv")

cat("\n")
for (p in c("area", "pop")) for (s in c("mean", "max", "min")) {
  r <- riepilogo_estate |> filter(peso == p, stat == s)
  stampa_riga(r, paste0(recode(s, mean = "T media", min = "T minima", max = "T massima"),
                        ", peso ", recode(p, area = "superficie", pop = "popolazione")))
}

cat("\nClassifica delle estati più calde dal 1961 (T media, sulla finestra omogenea):\n")
for (p in c("area", "pop")) {
  top <- estate_annuale |> filter(peso == p, stat == "mean", !is.na(t)) |>
    arrange(desc(t)) |> head(5)
  cat(sprintf("  peso %-11s ", recode(p, area = "superficie", pop = "popolazione")))
  cat(paste0(seq_len(nrow(top)), "° ", top$anno, " (", fmt(top$t), ")", collapse = " | "), "\n")
}
cat("Le 5 estati più calde per le massime (superficie): ")
top <- estate_annuale |> filter(peso == "area", stat == "max", !is.na(t)) |> arrange(desc(t)) |> head(5)
cat(paste0(top$anno, " (", fmt(top$t), ")", collapse = ", "), "\n")
cat("Le 5 estati più calde per le minime (superficie): ")
top <- estate_annuale |> filter(peso == "area", stat == "min", !is.na(t)) |> arrange(desc(t)) |> head(5)
cat(paste0(top$anno, " (", fmt(top$t), ")", collapse = ", "), "\n")

# ============ 2. MESE PER MESE ==============================================

sep("2. MESE PER MESE 2026 (maggio-agosto), Italia")

mensile_annuale <- serie |>
  filter(mese %in% 5:8, giorno <= map_int(mese, fin_di)) |>
  group_by(peso, stat, mese, anno) |>
  summarise(t = mean(valore), n_giorni = n(), .groups = "drop") |>
  mutate(completo = n_giorni == map_int(mese, fin_di),
         t = ifelse(completo, t, NA_real_))

riepilogo_mensile <- riepiloga(mensile_annuale, c("mese", "peso", "stat")) |>
  mutate(giorni = paste0("1-", map_int(mese, fin_di))) |>
  relocate(mese, giorni) |>
  arrange(mese, peso, stat)
write_csv(riepilogo_mensile, "output/analisi_mensile_estate_2026.csv")

for (m in 5:8) {
  cat("\n--- ", toupper(nome_mese[as.character(m)]), " 2026, giorni 1-", fin_di(m),
      if (m == 8 && agosto_parziale) " (stessa finestra per tutti gli anni)" else "", " ---\n", sep = "")
  for (p in c("area", "pop")) for (s in c("mean", "max", "min")) {
    r <- riepilogo_mensile |> filter(mese == m, peso == p, stat == s)
    stampa_riga(r, paste0(recode(s, mean = "T media", min = "T minima", max = "T massima"),
                          ", peso ", recode(p, area = "superficie", pop = "popolazione")))
  }
}

cat("\nTabella sintetica (T media, peso superficie):\n")
cat(sprintf("%-8s %7s %9s %9s %8s %8s %8s\n", "mese", "2026", "vs 61-90", "vs 91-20", "vs 2003", "vs 2022", "posto"))
for (m in 5:8) {
  r <- riepilogo_mensile |> filter(mese == m, peso == "area", stat == "mean")
  cat(sprintf("%-8s %7s %9s %9s %8s %8s %5d°/%d\n", nome_mese[as.character(m)], fmt(r$t_2026),
              fmts(r$vs_1961_1990), fmts(r$vs_1991_2020), fmts(r$vs_2003), fmts(r$vs_2022),
              r$posto_2026, r$n_anni_tot))
}

# controllo di coerenza con findings_giugno_2026.md (giugno 2026: 22,63; +3,25 vs 91-20)
chk <- riepilogo_mensile |> filter(mese == 6, peso == "area", stat == "mean")
cat(sprintf("\nControllo giugno 2026 (atteso 22,63 e +3,25 vs 91-20): %s e %s -> %s\n",
            fmt(chk$t_2026), fmts(chk$vs_1991_2020),
            ifelse(abs(chk$t_2026 - 22.63) < 0.006 && abs(chk$vs_1991_2020 - 3.25) < 0.006, "OK", "DIVERSO!")))

# ============ 3. DINAMICA GIORNALIERA =======================================

sep("3. DINAMICA GIORNALIERA DELL'ESTATE 2026")

giornaliero <- serie |>
  filter(mese %in% MESI_JJA) |>
  group_by(peso, stat, mese, giorno) |>
  summarise(
    t_2026         = valore[anno == ANNO][1],
    clim_1961_1990 = mean(valore[anno %in% BASE1]),
    clim_1991_2020 = mean(valore[anno %in% BASE2]),
    t_2003         = valore[anno == 2003][1],
    t_2022         = valore[anno == 2022][1],
    max_storico    = max(valore[anno < ANNO]),
    anno_max_storico = anno[anno < ANNO][which.max(valore[anno < ANNO])],
    n_anni         = sum(anno < ANNO),
    .groups = "drop") |>
  mutate(vs_1961_1990 = t_2026 - clim_1961_1990,
         vs_1991_2020 = t_2026 - clim_1991_2020,
         vs_2003      = t_2026 - t_2003,
         vs_2022      = t_2026 - t_2022,
         record_2026  = !is.na(t_2026) & t_2026 >= max_storico,
         data_2026    = as.Date(sprintf("%d-%02d-%02d", ANNO, mese, giorno))) |>
  arrange(peso, stat, mese, giorno)
write_csv(giornaliero, "output/estate_2026_giornaliero.csv")

gg <- giornaliero |> filter(peso == "area", stat == "mean", !is.na(t_2026))
cat("Giorni analizzati:", nrow(gg), "| anni di confronto per data: ",
    paste(range(gg$n_anni), collapse = "-"), "(agosto ha meno anni finché il download non è completo)\n")
cat(sprintf("Media delle anomalie giornaliere vs 91-20: %s | giorni sopra la media 91-20: %d su %d | sotto: %d\n",
            fmts(mean(gg$vs_1991_2020)), sum(gg$vs_1991_2020 > 0), nrow(gg), sum(gg$vs_1991_2020 < 0)))

eti_data <- function(d) paste(lubridate::mday(d), nome_mese[as.character(lubridate::month(d))])
sequenze <- function(flag, date) {
  r <- rle(flag)
  fine <- cumsum(r$lengths); inizio <- fine - r$lengths + 1
  tibble(inizio = date[inizio], fine = date[fine], lunghezza = r$lengths, valore = r$values) |>
    filter(valore) |> select(-valore)
}
stampa_seq <- function(sq, min_len = 2) {
  sq <- sq |> filter(lunghezza >= min_len) |> arrange(desc(lunghezza))
  if (nrow(sq) == 0) { cat("  nessuna\n"); return(invisible()) }
  for (i in seq_len(nrow(sq))) cat(sprintf("  %s - %s (%d giorni)\n", eti_data(sq$inizio[i]), eti_data(sq$fine[i]), sq$lunghezza[i]))
}

cat(sprintf("\nGiorni record per la data (T media superficie, dal 1961): %d su %d\n", sum(gg$record_2026), nrow(gg)))
cat("  ", paste(eti_data(gg$data_2026[gg$record_2026]), collapse = ", "), "\n")
cat("  Sequenze di giorni record consecutivi (2+):\n"); stampa_seq(sequenze(gg$record_2026, gg$data_2026))
gp <- giornaliero |> filter(peso == "pop", stat == "mean", !is.na(t_2026))
cat(sprintf("Giorni record per la data, peso popolazione: %d su %d\n", sum(gp$record_2026), nrow(gp)))
for (s in c("max", "min")) {
  gx <- giornaliero |> filter(peso == "area", stat == s, !is.na(t_2026))
  cat(sprintf("Giorni record per la data sulle %s (superficie): %d su %d\n",
              ifelse(s == "max", "massime", "minime"), sum(gx$record_2026), nrow(gx)))
}

for (soglia in c(3, 4, 5)) {
  sopra <- gg$vs_1991_2020 >= soglia
  cat(sprintf("\nGiorni con anomalia >= +%d vs 91-20 (T media superficie): %d\n", soglia, sum(sopra)))
  if (any(sopra)) cat("  ", paste(eti_data(gg$data_2026[sopra]), collapse = ", "), "\n")
  cat("  Sequenze consecutive (2+):\n"); stampa_seq(sequenze(sopra, gg$data_2026))
}
cat("\nGiorni con anomalia >= +5 vs 91-20, peso popolazione:", sum(gp$vs_1991_2020 >= 5), "\n")
piu_caldo <- gg |> slice_max(t_2026, n = 5)
cat("\nI 5 giorni più caldi dell'estate 2026 (T media superficie):\n")
for (i in seq_len(nrow(piu_caldo))) cat(sprintf("  %-10s %s°C (%s vs 91-20)\n", eti_data(piu_caldo$data_2026[i]),
                                                fmt(piu_caldo$t_2026[i], 1), fmts(piu_caldo$vs_1991_2020[i], 1)))
piu_anom <- gg |> slice_max(vs_1991_2020, n = 5)
cat("I 5 giorni con l'anomalia più forte vs 91-20:\n")
for (i in seq_len(nrow(piu_anom))) cat(sprintf("  %-10s %s (%s°C)\n", eti_data(piu_anom$data_2026[i]),
                                               fmts(piu_anom$vs_1991_2020[i], 1), fmt(piu_anom$t_2026[i], 1)))
sotto <- gg |> filter(vs_1991_2020 < 0)
cat("Giorni sotto la media 91-20:", if (nrow(sotto)) paste(eti_data(sotto$data_2026), collapse = ", ") else "nessuno", "\n")
cat("\nMedie per decade (T media superficie, anomalia vs 91-20):\n")
dec <- gg |> mutate(decade = paste0(nome_mese[as.character(mese)], " ", c("1-10", "11-20", "21-31")[pmin(3, (giorno - 1) %/% 10 + 1)])) |>
  group_by(mese, decade) |> summarise(t = mean(t_2026), a = mean(vs_1991_2020), a2003 = mean(vs_2003), a2022 = mean(vs_2022), .groups = "drop")
for (i in seq_len(nrow(dec))) cat(sprintf("  %-14s %s°C  %s vs 91-20 | %s vs 2003 | %s vs 2022\n", dec$decade[i], fmt(dec$t[i], 1), fmts(dec$a[i], 1), fmts(dec$a2003[i], 1), fmts(dec$a2022[i], 1)))

# ============ 4. ONDATE DI CALORE (serie nazionale) ==========================

sep("4. ONDATE DI CALORE — massima nazionale > 90° percentile JJA 1991-2020, episodi di 3+ giorni")

serie_w <- read_csv("output/serie_giornaliera_italia.csv", show_col_types = FALSE) |>
  mutate(anno = lubridate::year(data), mese = lubridate::month(data), giorno = lubridate::mday(data)) |>
  filter(mese %in% MESI_JJA) |>
  arrange(data)

ondate_per_peso <- function(col_max, peso) {
  base <- serie_w |> filter(anno %in% BASE2)
  p90 <- quantile(base[[col_max]], 0.9)
  n_anni_base <- serie_w |> filter(anno %in% BASE2) |> group_by(anno) |> summarise(n = n(), .groups = "drop")
  cat(sprintf("\n[peso %s] soglia 90° percentile = %s°C (calcolata su %d giorni di %d anni della baseline; anni con agosto: %d)\n",
              recode(peso, area = "superficie", pop = "popolazione"), fmt(p90, 1), nrow(base), nrow(n_anni_base),
              sum(n_anni_base$n >= 92)))
  per_anno <- serie_w |>
    mutate(sopra = .data[[col_max]] > p90) |>
    group_by(anno) |>
    summarise(n_giorni = n(), completo = n_giorni >= n_attesi_jja,
              r = list(rle(sopra)), date = list(data), tmax = list(.data[[col_max]]), .groups = "drop") |>
    mutate(giorni_sopra  = map_int(r, ~ sum(.x$lengths[.x$values])),
           episodi_3g    = map_int(r, ~ sum(.x$values & .x$lengths >= 3)),
           giorni_ondata = map_int(r, ~ sum(.x$lengths[.x$values & .x$lengths >= 3])),
           striscia_max  = map_int(r, ~ ifelse(any(.x$values), max(.x$lengths[.x$values]), 0L)),
           peso = peso, soglia_p90 = p90)
  episodi <- per_anno |>
    select(anno, r, date, tmax) |>
    mutate(ep = pmap(list(r, date, tmax), function(r, date, tmax) {
      fine <- cumsum(r$lengths); inizio <- fine - r$lengths + 1
      tibble(inizio = date[inizio], fine = date[fine], lunghezza = r$lengths, valore = r$values,
             tmax_media = map2_dbl(inizio, fine, ~ mean(tmax[date >= .x & date <= .y])),
             tmax_picco = map2_dbl(inizio, fine, ~ max(tmax[date >= .x & date <= .y]))) |>
        filter(valore, lunghezza >= 3) |> select(-valore)
    })) |>
    select(anno, ep) |> unnest(ep) |> mutate(peso = peso)
  list(anni = per_anno |> select(-r, -date, -tmax), episodi = episodi, p90 = p90)
}

oa <- ondate_per_peso("t_area_max", "area")
op <- ondate_per_peso("t_pop_max", "pop")
ondate_anno <- bind_rows(oa$anni, op$anni) |> relocate(peso, anno)
episodi <- bind_rows(oa$episodi, op$episodi) |> relocate(peso, anno)
write_csv(ondate_anno, "output/ondate_calore_estate.csv")
write_csv(episodi |> filter(anno == ANNO), "output/ondate_calore_estate_2026_episodi.csv")

for (p in c("area", "pop")) {
  cat(sprintf("\n--- peso %s ---\n", recode(p, area = "superficie", pop = "popolazione")))
  oc <- ondate_anno |> filter(peso == p)
  e26 <- episodi |> filter(peso == p, anno == ANNO) |> arrange(inizio)
  cat(sprintf("Estate 2026: %d episodi, %d giorni in ondata (%d giorni totali sopra soglia), striscia più lunga %d giorni\n",
              oc$episodi_3g[oc$anno == ANNO], oc$giorni_ondata[oc$anno == ANNO],
              oc$giorni_sopra[oc$anno == ANNO], oc$striscia_max[oc$anno == ANNO]))
  for (i in seq_len(nrow(e26))) {
    cat(sprintf("  episodio %d: %s - %s (%d giorni), Tmax media %s°C, picco %s°C%s\n", i,
                eti_data(e26$inizio[i]), eti_data(e26$fine[i]), e26$lunghezza[i],
                fmt(e26$tmax_media[i], 1), fmt(e26$tmax_picco[i], 1),
                ifelse(e26$fine[i] == max(serie_w$data[serie_w$anno == ANNO]), " [in corso all'ultimo giorno disponibile]", "")))
  }
  for (a in c(2003, 2022)) {
    r <- oc |> filter(anno == a)
    ea <- episodi |> filter(peso == p, anno == a) |> arrange(inizio)
    cat(sprintf("%d: %d episodi, %d giorni in ondata, striscia più lunga %d giorni%s\n", a, r$episodi_3g, r$giorni_ondata, r$striscia_max,
                ifelse(r$completo, "", " [estate INCOMPLETA nei dati]")))
    if (nrow(ea)) cat("  ", paste(sprintf("%s-%s (%d)", eti_data(ea$inizio), eti_data(ea$fine), ea$lunghezza), collapse = "; "), "\n")
  }
  comp <- oc |> filter(completo)
  cat(sprintf("Media giorni in ondata: 61-90 %s (%d anni completi) | 91-20 %s (%d anni completi) | 2021-2025 %s\n",
              fmt(mean(comp$giorni_ondata[comp$anno %in% BASE1]), 1), sum(comp$anno %in% BASE1),
              fmt(mean(comp$giorni_ondata[comp$anno %in% BASE2]), 1), sum(comp$anno %in% BASE2),
              fmt(mean(comp$giorni_ondata[comp$anno %in% 2021:2025]), 1)))
  top_g <- oc |> arrange(desc(giorni_ondata)) |> head(5)
  cat("Estati con più giorni in ondata:", paste0(top_g$anno, " (", top_g$giorni_ondata, ")", collapse = ", "), "\n")
  top_s <- episodi |> filter(peso == p) |> arrange(desc(lunghezza)) |> head(5)
  cat("Strisce più lunghe dal 1961:", paste0(top_s$anno, " ", eti_data(top_s$inizio), "-", eti_data(top_s$fine),
                                             " (", top_s$lunghezza, " gg)", collapse = "; "), "\n")
  cat("Posto del 2026 per giorni in ondata (solo estati complete):", sum(comp$giorni_ondata > comp$giorni_ondata[comp$anno == ANNO]) + 1, "su", nrow(comp), "\n")
}

# ============ 6. TREND ======================================================

sep("6. TREND DI RISCALDAMENTO (°C per decennio, regressione lineare)")

pendenza <- function(t, anno) { ok <- !is.na(t); if (sum(ok) < 5) return(NA_real_); coef(lm(t[ok] ~ anno[ok]))[[2]] * 10 }

trend_it <- estate_annuale |>
  group_by(peso, stat) |>
  summarise(trend_1961_2026 = pendenza(t, anno), n_1961_2026 = sum(!is.na(t)),
            trend_1991_2026 = pendenza(t[anno >= 1991], anno[anno >= 1991]), n_1991_2026 = sum(!is.na(t[anno >= 1991])),
            .groups = "drop")
write_csv(trend_it, "output/trend_estate_italia.csv")
cat("Serie nazionale JJA (solo estati complete sulla finestra):\n")
for (i in seq_len(nrow(trend_it))) {
  r <- trend_it[i, ]
  cat(sprintf("  %-5s %-4s  1961-2026: %s (%d anni) | 1991-2026: %s (%d anni)\n", r$peso, r$stat,
              fmts(r$trend_1961_2026), r$n_1961_2026, fmts(r$trend_1991_2026), r$n_1991_2026))
}
if (any(trend_it$n_1961_2026 < 60)) cat("  ATTENZIONE: trend calcolato su poche estati complete, poco affidabile finché manca agosto storico.\n")

cat("\nPer confronto, trend mensili 1961-2026 della T media (superficie): ")
tm <- mensile_annuale |> filter(peso == "area", stat == "mean") |> group_by(mese) |>
  summarise(tr = pendenza(t, anno), n = sum(!is.na(t)), .groups = "drop")
cat(paste0(nome_mese[as.character(tm$mese)], " ", fmts(tm$tr), " (", tm$n, " anni)", collapse = " | "), "\n")

# --- griglia stagionale per cella (pesi = giorni di ogni mese) ---
giorni_mese <- serie |>
  filter(peso == "area", stat == "mean", mese %in% MESI_JJA) |>
  count(anno, mese, name = "giorni")

griglia <- read_csv("output/griglia_mensile.csv.gz", show_col_types = FALSE) |>
  filter(stat == "mean", mese %in% MESI_JJA, finestra == "mese_intero") |>
  inner_join(giorni_mese, by = c("anno", "mese"))

griglia_estate <- griglia |>
  group_by(ilon, ilat, lon, lat, regione, anno) |>
  summarise(valore = sum(valore * giorni) / sum(giorni), giorni = sum(giorni), n_mesi = n(), .groups = "drop") |>
  filter(n_mesi == 3) |>
  select(-n_mesi)
write_csv(griglia_estate, "output/griglia_estate.csv.gz")
cat(sprintf("\nGriglia stagionale: %d celle x %d anni completi (JJA con 3 mesi); giorni 2026 = %d\n",
            n_distinct(paste(griglia_estate$ilon, griglia_estate$ilat)), n_distinct(griglia_estate$anno),
            griglia_estate$giorni[griglia_estate$anno == ANNO][1]))
if (agosto_parziale) cat("  NB: a livello di cella il 2026 usa agosto 1-", fin_di(8),
                         " mentre gli altri anni il mese intero (differenza trascurabile, sparisce col dato completo).\n", sep = "")

min_anni_trend <- min(60L, n_distinct(griglia_estate$anno))
trend_celle <- griglia_estate |>
  group_by(ilon, ilat, lon, lat, regione) |>
  summarise(trend_dec = pendenza(valore, anno), n = n(), .groups = "drop") |>
  filter(n >= min_anni_trend, !is.na(trend_dec))
write_csv(trend_celle, "output/trend_estate_celle.csv")
cat(sprintf("Trend per cella su %d anni (min %d): quantili 2/25/50/75/98%%: %s °C/decennio\n",
            max(trend_celle$n), min_anni_trend,
            paste(fmt(quantile(trend_celle$trend_dec, c(0.02, 0.25, 0.5, 0.75, 0.98))), collapse = " | ")))

trend_reg <- trend_celle |>
  group_by(regione) |>
  summarise(trend_dec = mean(trend_dec), n_celle = n(), .groups = "drop") |>
  arrange(desc(trend_dec))
write_csv(trend_reg, "output/trend_estate_regioni.csv")
cat("Trend per regione (media delle celle):\n")
for (i in seq_len(nrow(trend_reg))) cat(sprintf("  %-22s %s\n", trend_reg$regione[i], fmts(trend_reg$trend_dec[i])))

# --- giorno vs notte (07_giorno_notte.R) ---
cat("\n--- Giorno (ore 7-19) vs notte (19-7), da giorno_notte_italia.csv ---\n")
gn <- read_csv("output/giorno_notte_italia.csv", show_col_types = FALSE) |>
  mutate(anno = lubridate::year(data), mese = lubridate::month(data), giorno = lubridate::mday(data)) |>
  in_finestra() |>
  pivot_longer(c(t_area, t_pop), names_to = "peso", values_to = "valore") |>
  mutate(peso = sub("t_", "", peso)) |>
  group_by(fascia, peso, anno) |>
  summarise(t = mean(valore), n_giorni = n(), .groups = "drop") |>
  # 07 elabora un file al mese: la notte del giorno 1 (che inizia la sera prima)
  # resta monca e viene scartata, quindi si tollera un giorno mancante per mese
  mutate(t = ifelse(n_giorni >= n_attesi_jja - length(MESI_JJA), t, NA_real_))
cat("Ultimo giorno presente nel file giorno/notte:", format(max(read_csv("output/giorno_notte_italia.csv", show_col_types = FALSE)$data)), "\n")
gn_riep <- riepiloga(gn, c("fascia", "peso")) |>
  left_join(gn |> group_by(fascia, peso) |> summarise(trend_1961_2026 = pendenza(t, anno), .groups = "drop"),
            by = c("fascia", "peso"))
write_csv(gn_riep, "output/giorno_notte_estate_riepilogo.csv")
if (all(is.na(gn_riep$t_2026))) {
  cat("  Estate 2026 non ancora disponibile nel file giorno/notte (rilanciare 07_giorno_notte.R).\n")
} else {
  for (i in seq_len(nrow(gn_riep))) {
    r <- gn_riep[i, ]
    cat(sprintf("  %-7s peso %-11s 2026: %s°C | 61-90: %s (%s) | 91-20: %s (%s) | 2003: %s (%s) | 2022: %s (%s) | trend %s/dec\n",
                r$fascia, recode(r$peso, area = "superficie", pop = "popolazione"), fmt(r$t_2026, 1),
                fmt(r$media_1961_1990, 1), fmts(r$vs_1961_1990, 1), fmt(r$media_1991_2020, 1), fmts(r$vs_1991_2020, 1),
                fmt(r$t_2003, 1), fmts(r$vs_2003, 1), fmt(r$t_2022, 1), fmts(r$vs_2022, 1), fmts(r$trend_1961_2026)))
  }
}

# ============ 7. ESPOSIZIONE DELLA POPOLAZIONE ===============================

sep("7. ESPOSIZIONE DELLA POPOLAZIONE — anomalia JJA 2026 vs 1991-2020 per cella")

anomalia_celle <- griglia_estate |>
  group_by(ilon, ilat, lon, lat, regione) |>
  summarise(baseline = mean(valore[anno %in% BASE2]), n_base = sum(anno %in% BASE2),
            t26 = valore[anno == ANNO][1], .groups = "drop") |>
  filter(!is.na(t26), n_base > 0) |>
  mutate(anomalia = t26 - baseline) |>
  left_join(celle |> select(ilon, ilat, pop, area_kmq), by = c("ilon", "ilat"))
cat(sprintf("Anni di baseline 91-20 disponibili per cella: %d su 30\n", max(anomalia_celle$n_base)))

pop_tot <- sum(anomalia_celle$pop)
soglie_esp <- c(1.5, 2, 2.5, 3, 3.5)
esposizione <- map_dfr(soglie_esp, function(s) tibble(
  soglia = s,
  pop_milioni = sum(anomalia_celle$pop[anomalia_celle$anomalia >= s]) / 1e6,
  pop_pct = sum(anomalia_celle$pop[anomalia_celle$anomalia >= s]) / pop_tot * 100,
  celle = sum(anomalia_celle$anomalia >= s),
  area_pct = sum(anomalia_celle$area_kmq[anomalia_celle$anomalia >= s]) / sum(anomalia_celle$area_kmq) * 100))
write_csv(esposizione, "output/esposizione_estate_2026.csv")
cat(sprintf("Popolazione totale nelle celle: %s milioni\n", fmt(pop_tot / 1e6, 1)))
for (i in seq_len(nrow(esposizione))) {
  r <- esposizione[i, ]
  cat(sprintf("  oltre %s: %s%% della popolazione (%s milioni) | %d celle, %s%% della superficie\n",
              fmts(r$soglia, 1), fmt(r$pop_pct, 0), fmt(r$pop_milioni, 1), r$celle, fmt(r$area_pct, 0)))
}
cat(sprintf("Anomalia media per abitante (dalle celle): %s | mediana celle: %s | min %s | max %s\n",
            fmts(weighted.mean(anomalia_celle$anomalia, anomalia_celle$pop)), fmts(median(anomalia_celle$anomalia)),
            fmts(min(anomalia_celle$anomalia)), fmts(max(anomalia_celle$anomalia))))

top_anom <- anomalia_celle |> slice_max(anomalia, n = 12) |> select(regione, lon, lat, t26, anomalia, pop)
cat("\nLe 12 celle con l'anomalia più forte:\n")
for (i in seq_len(nrow(top_anom))) cat(sprintf("  %-21s (%.1f, %.1f)  %s  (%s°C)\n", top_anom$regione[i], top_anom$lon[i], top_anom$lat[i], fmts(top_anom$anomalia[i]), fmt(top_anom$t26[i], 1)))
top_calde <- anomalia_celle |> slice_max(t26, n = 12) |> select(regione, lon, lat, t26, anomalia, pop)
cat("Le 12 celle più calde in assoluto (T media JJA 2026):\n")
for (i in seq_len(nrow(top_calde))) cat(sprintf("  %-21s (%.1f, %.1f)  %s°C  (%s vs 91-20)\n", top_calde$regione[i], top_calde$lon[i], top_calde$lat[i], fmt(top_calde$t26[i], 1), fmts(top_calde$anomalia[i])))
write_csv(bind_rows(top_anom |> mutate(tipo = "anomalia"), top_calde |> mutate(tipo = "livello")), "output/celle_top_estate_2026.csv")

# ============ 8. REGIONI ====================================================

sep("8. REGIONI — estate 2026 sulla finestra omogenea")

regioni <- allunga(read_csv("output/serie_giornaliera_regioni.csv.gz", show_col_types = FALSE)) |>
  in_finestra() |>
  group_by(regione, peso, stat, anno) |>
  summarise(t = mean(valore), n_giorni = n(), .groups = "drop") |>
  mutate(t = ifelse(n_giorni == n_attesi_jja, t, NA_real_))

analisi_reg <- riepiloga(regioni, c("regione", "peso", "stat")) |>
  mutate(finestra = etichetta_finestra)
write_csv(analisi_reg, "output/analisi_regioni_estate_2026.csv")

classifica_reg <- analisi_reg |>
  filter(peso == "area", stat == "mean") |>
  select(regione, t_2026, vs_1961_1990, vs_1991_2020, vs_2003, vs_2022, posto_2026, record, n_anni_tot) |>
  arrange(posto_2026, desc(vs_1991_2020))
write_csv(classifica_reg, "output/classifica_regioni_estate_2026.csv")

cat("T media JJA 2026 per regione (peso superficie), ordinate per anomalia vs 91-20:\n")
rr <- classifica_reg |> arrange(desc(vs_1991_2020))
for (i in seq_len(nrow(rr))) {
  r <- rr[i, ]
  cat(sprintf("  %-22s %s°C | vs 61-90 %s | vs 91-20 %s | vs 2003 %s | vs 2022 %s | %d° su %d%s\n",
              r$regione, fmt(r$t_2026, 1), fmts(r$vs_1961_1990, 1), fmts(r$vs_1991_2020, 1),
              fmts(r$vs_2003, 1), fmts(r$vs_2022, 1), r$posto_2026, r$n_anni_tot, ifelse(r$record, " RECORD", "")))
}
cat(sprintf("Regioni con estate record: %d (%s)\n", sum(rr$record), paste(rr$regione[rr$record], collapse = ", ")))
cat("Distribuzione dei posti:", paste(names(table(rr$posto_2026)), "°: ", table(rr$posto_2026), sep = "", collapse = " | "), "\n")

cat("\nPer abitante (peso popolazione), vs 91-20:\n")
rp <- analisi_reg |> filter(peso == "pop", stat == "mean") |> arrange(desc(vs_1991_2020))
cat(paste0(rp$regione, " ", fmts(rp$vs_1991_2020, 1), collapse = "; "), "\n")

cat(sprintf("\nFatto in %s secondi. CSV salvati in output/.\n", fmt(as.numeric(difftime(Sys.time(), t_inizio, units = "secs")), 0)))
