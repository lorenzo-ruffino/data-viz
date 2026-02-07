###############################################################################
#  Analisi accessibilità ospedaliera in Europa
#  - Per ogni stato EU: popolazione per fascia di 5 min + media e mediana
#  - Per l'Italia: stesse statistiche per regione (NUTS2) e provincia (NUTS3)
###############################################################################

# ===========================================================================
# 0. LIBRERIE
# ===========================================================================

pkgs <- c("terra", "sf", "giscoR", "dplyr", "tidyr", "readr", "exactextractr")

to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install)

library(terra)
library(sf)
library(giscoR)
library(dplyr)
library(tidyr)
library(readr)
library(exactextractr)

# ===========================================================================
# 1. PARAMETRI
# ===========================================================================

# Cartella di lavoro
work_dir <- "/Users/lorenzoruffino/Desktop/Ospedali"
setwd(work_dir)

# File raster accessibilita' (banda 1 = tempo verso ospedale piu' vicino, in secondi)
access_tif <- file.path(work_dir, "euro_access_healthcare_2023_100m_v2026_01.tif")

# Cartella per dati scaricati
data_dir <- file.path(work_dir, "data")
dir.create(data_dir, showWarnings = FALSE)

# Cartella output
out_dir <- file.path(work_dir, "output")
dir.create(out_dir, showWarnings = FALSE)

# Intervalli di tempo in minuti (fasce di 5 minuti, fino a 60+)
breaks_min <- seq(0, 60, by = 5)
labels_min <- c(paste0(head(breaks_min, -1), "-", tail(breaks_min, -1), " min"), "60+ min")

# ===========================================================================
# 2. DOWNLOAD DATI POPOLAZIONE GHS-POP (se non gia' presenti)
# ===========================================================================

# GHS-POP R2023A - epoca 2020, 100m, Mollweide
# File scaricati manualmente nella cartella GHS/

ghspop_dir <- file.path(work_dir, "GHS")

# Tutti i tile europei disponibili (100m, Mollweide)
# R1-R5 x C17-C21 per coprire tutta l'Europa incluso sud Italia/isole
tiles <- c(
  "R1_C18", "R1_C19", "R1_C20", "R1_C21",
  "R2_C17", "R2_C18", "R2_C19", "R2_C20", "R2_C21",
  "R3_C18", "R3_C19", "R3_C20", "R3_C21",
  "R4_C18", "R4_C19", "R4_C20", "R4_C21",
  "R5_C18", "R5_C19", "R5_C20", "R5_C21"
)

cat("=== Preparazione GHS-POP 100m tiles ===\n")
pop_files <- c()
for (tile in tiles) {
  fname_zip <- paste0("GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_", tile, ".zip")
  fname_tif <- paste0("GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_", tile, ".tif")
  fpath_zip <- file.path(ghspop_dir, fname_zip)
  fpath_tif <- file.path(ghspop_dir, fname_tif)
  pop_files <- c(pop_files, fpath_tif)

  if (file.exists(fpath_tif)) {
    cat("Gia' decompresso:", fname_tif, "\n")
    next
  }

  # Decomprimi lo zip se presente
  if (file.exists(fpath_zip)) {
    cat("Decomprimendo:", fname_zip, "\n")
    unzip(fpath_zip, exdir = ghspop_dir)
  } else {
    cat("ATTENZIONE: manca", fname_zip, "\n")
  }
}

# Verifica che almeno un file esista
pop_files <- pop_files[file.exists(pop_files)]
if (length(pop_files) == 0) {
  stop("Nessun tile GHS-POP trovato. Verifica che i file .zip siano nella cartella GHS/")
}
cat("Tile trovati:", length(pop_files), "su", length(tiles), "\n")

# ===========================================================================
# 3. CARICAMENTO RASTER
# ===========================================================================

cat("\n=== Caricamento raster accessibilita' ===\n")
r_access <- rast(access_tif)
cat("Bande:", nlyr(r_access), "\n")
cat("CRS:", crs(r_access, describe = TRUE)$name, "\n")
cat("Risoluzione:", res(r_access), "\n")

# Prendiamo solo banda 1: tempo minimo verso ospedale piu' vicino (in secondi)
r_access_n1 <- r_access[[1]]

# Convertiamo da secondi a minuti
r_access_min <- r_access_n1 / 60
names(r_access_min) <- "tempo_min"

cat("\n=== Caricamento raster popolazione GHS-POP ===\n")
if (length(pop_files) == 1) {
  r_pop_raw <- rast(pop_files[1])
} else {
  # Mosaico dei tile
  pop_rasters <- lapply(pop_files, rast)
  r_pop_raw <- do.call(merge, pop_rasters)
}
cat("CRS popolazione:", crs(r_pop_raw, describe = TRUE)$name, "\n")

# ===========================================================================
# 4. ALLINEAMENTO RASTER (riproiezione popolazione su griglia accessibilita')
# ===========================================================================

cat("\n=== Allineamento raster (aggregazione a 1000m + riproiezione) ===\n")

# Aggreghiamo accessibilita' da 100m a 1000m (media dei pixel)
cat("1/4 - Aggregazione accessibilita' 100m -> 1000m...\n")
r_access_1km <- aggregate(r_access_min, fact = 10, fun = "mean", na.rm = TRUE)
names(r_access_1km) <- "tempo_min"

# Aggreghiamo prima la popolazione da 100m a 1000m (SOMMA, non media!)
# Questo preserva il conteggio totale di popolazione
cat("2/4 - Aggregazione popolazione 100m -> 1000m (somma)...\n")
r_pop_1km <- aggregate(r_pop_raw, fact = 10, fun = "sum", na.rm = TRUE)

# Riproiettiamo la popolazione aggregata (Mollweide 1000m) su EPSG:3035
cat("3/4 - Riproiezione popolazione Mollweide -> EPSG:3035...\n")
cat("      Questo puo' richiedere alcuni minuti...\n")
r_pop_3035 <- project(r_pop_1km, crs(r_access_1km), method = "bilinear", res = 1000)

# Resample sulla griglia esatta dell'accessibilita'
r_pop <- resample(r_pop_3035, r_access_1km, method = "bilinear")
names(r_pop) <- "pop"

# Verifica
n_pop <- global(r_pop, "sum", na.rm = TRUE)[[1]]
cat("4/4 - Verifica: popolazione totale nel raster =", format(round(n_pop), big.mark = ","), "\n")
cat("      Accessibilita':", ncol(r_access_1km), "x", nrow(r_access_1km), "\n")
cat("      Popolazione:   ", ncol(r_pop), "x", nrow(r_pop), "\n")

cat("Allineamento completato.\n")

rm(r_pop_1km, r_pop_3035)

# Pulizia memoria
rm(r_pop_raw)
gc()

# ===========================================================================
# 5. DOWNLOAD CONFINI AMMINISTRATIVI (NUTS)
# ===========================================================================

cat("\n=== Download confini NUTS ===\n")

# NUTS0 = stati, NUTS2 = regioni, NUTS3 = province
# Usiamo EPSG:3035 per allineamento con i raster
nuts0 <- gisco_get_nuts(year = "2021", resolution = "03", nuts_level = 0, epsg = 3035)
nuts2 <- gisco_get_nuts(year = "2021", resolution = "03", nuts_level = 2, epsg = 3035)
nuts3 <- gisco_get_nuts(year = "2021", resolution = "03", nuts_level = 3, epsg = 3035)

cat("Stati (NUTS0):", nrow(nuts0), "\n")
cat("Regioni (NUTS2):", nrow(nuts2), "\n")
cat("Province (NUTS3):", nrow(nuts3), "\n")

# ===========================================================================
# 6. FUNZIONE: calcolo statistiche per zona
# ===========================================================================

calc_stats <- function(zones, r_tempo, r_pop, breaks_min, labels_min, id_col, name_col) {
  #' @param zones      sf con le zone (poligoni)
  #' @param r_tempo    SpatRaster con tempo in minuti
  #' @param r_pop      SpatRaster con popolazione
  #' @param breaks_min vettore dei limiti delle fasce (es. seq(0,60,5))
  #' @param labels_min etichette delle fasce

  #' @param id_col     nome colonna con codice zona (es. "NUTS_ID")
  #' @param name_col   nome colonna con nome zona (es. "NAME_LATN")

  cat("Calcolo per", nrow(zones), "zone...\n")

  # Stack dei due raster
  r_stack <- c(r_tempo, r_pop)

  results_list <- list()

  for (i in seq_len(nrow(zones))) {
    zone <- zones[i, ]
    zone_id   <- zone[[id_col]]
    zone_name <- zone[[name_col]]

    if (i %% 10 == 0 || i == 1) {
      cat("  ", i, "/", nrow(zones), "-", zone_name, "\n")
    }

    # Estrai valori con exact_extract (piu' efficiente di terra::extract per poligoni)
    vals <- tryCatch({
      exact_extract(r_stack, zone, progress = FALSE)[[1]]
    }, error = function(e) {
      cat("    Errore per", zone_name, ":", e$message, "\n")
      return(NULL)
    })

    if (is.null(vals) || nrow(vals) == 0) {
      results_list[[i]] <- tibble(
        id = zone_id, nome = zone_name,
        fascia = labels_min,
        pop = 0,
        media_min = NA_real_,
        mediana_min = NA_real_,
        pop_totale = 0
      )
      next
    }

    # Filtra NA
    vals <- vals[!is.na(vals$tempo_min) & !is.na(vals$pop), ]

    if (nrow(vals) == 0) {
      results_list[[i]] <- tibble(
        id = zone_id, nome = zone_name,
        fascia = labels_min,
        pop = 0,
        media_min = NA_real_,
        mediana_min = NA_real_,
        pop_totale = 0
      )
      next
    }

    # Peso = pop * coverage_fraction
    vals$pop_w <- vals$pop * vals$coverage_fraction

    # Classifica in fasce
    vals$fascia <- cut(vals$tempo_min,
                       breaks = c(breaks_min, Inf),
                       labels = labels_min,
                       right = FALSE,
                       include.lowest = TRUE)

    # Popolazione per fascia
    pop_fasce <- vals %>%
      group_by(fascia, .drop = FALSE) %>%
      summarise(pop = sum(pop_w, na.rm = TRUE), .groups = "drop")

    # Media ponderata per popolazione
    pop_tot <- sum(vals$pop_w, na.rm = TRUE)
    if (pop_tot > 0) {
      media <- weighted.mean(vals$tempo_min, vals$pop_w, na.rm = TRUE)

      # Mediana ponderata
      ord <- order(vals$tempo_min)
      tempo_ord <- vals$tempo_min[ord]
      pop_ord   <- vals$pop_w[ord]
      cum_pop   <- cumsum(pop_ord)
      mediana   <- tempo_ord[which(cum_pop >= pop_tot / 2)[1]]
    } else {
      media   <- NA_real_
      mediana <- NA_real_
    }

    results_list[[i]] <- tibble(
      id = zone_id,
      nome = zone_name,
      fascia = pop_fasce$fascia,
      pop = round(pop_fasce$pop, 0),
      media_min = round(media, 2),
      mediana_min = round(mediana, 2),
      pop_totale = round(pop_tot, 0)
    )
  }

  bind_rows(results_list)
}

# ===========================================================================
# 7. ANALISI PER STATI EUROPEI (NUTS0)
# ===========================================================================

cat("\n=== ANALISI PER STATI EUROPEI ===\n")

# Nomi in italiano e filtro microstati/paesi con dati parziali
nomi_it <- c(
  AT = "Austria",        BE = "Belgio",          BG = "Bulgaria",
  CH = "Svizzera",       CY = "Cipro",           CZ = "Cechia",
  DE = "Germania",       DK = "Danimarca",       EE = "Estonia",
  EL = "Grecia",         ES = "Spagna",          FI = "Finlandia",
  FR = "Francia",        HR = "Croazia",         HU = "Ungheria",
  IE = "Irlanda",        IT = "Italia",          LT = "Lituania",
  LU = "Lussemburgo",    LV = "Lettonia",        NL = "Paesi Bassi",
  NO = "Norvegia",       PL = "Polonia",         PT = "Portogallo",
  RO = "Romania",        SE = "Svezia",          SI = "Slovenia",
  SK = "Slovacchia",     AL = "Albania",         MT = "Malta"
)

# Microstati e paesi con dati parziali/fuori copertura da escludere
escludi <- c("LI", "MC", "AD", "SM", "VA",  # microstati
             "IS",                             # Islanda (fuori copertura)
             "ME", "MK", "RS", "TR", "UK")    # dati parziali nel raster

# Filtra solo i paesi da includere
nuts0_filtrato <- nuts0 %>% filter(!NUTS_ID %in% escludi)

df_stati <- calc_stats(
  zones      = nuts0_filtrato,
  r_tempo    = r_access_1km,
  r_pop      = r_pop,
  breaks_min = breaks_min,
  labels_min = labels_min,
  id_col     = "NUTS_ID",
  name_col   = "NAME_LATN"
)

# Sostituisci nomi con versione italiana
df_stati <- df_stati %>%
  mutate(nome = ifelse(id %in% names(nomi_it), nomi_it[id], nome))

# Tabella larga: una riga per stato, colonne = fasce
df_stati_wide <- df_stati %>%
  select(id, nome, fascia, pop, media_min, mediana_min, pop_totale) %>%
  pivot_wider(
    id_cols     = c(id, nome, media_min, mediana_min, pop_totale),
    names_from  = fascia,
    values_from = pop,
    values_fill = 0
  )

write_csv(df_stati, file.path(out_dir, "stati_eu_fasce_5min_long.csv"))
write_csv(df_stati_wide, file.path(out_dir, "stati_eu_fasce_5min_wide.csv"))

cat("Risultati stati salvati in output/\n")

# ===========================================================================
# 8. ANALISI ITALIA PER REGIONE (NUTS2)
# ===========================================================================

cat("\n=== ANALISI ITALIA PER REGIONE (NUTS2) ===\n")

# Filtriamo solo le regioni italiane (codice NUTS che inizia con "IT")
nuts2_it <- nuts2 %>% filter(grepl("^IT", NUTS_ID))
cat("Regioni italiane trovate:", nrow(nuts2_it), "\n")

df_regioni <- calc_stats(
  zones      = nuts2_it,
  r_tempo    = r_access_1km,
  r_pop      = r_pop,
  breaks_min = breaks_min,
  labels_min = labels_min,
  id_col     = "NUTS_ID",
  name_col   = "NAME_LATN"
)

df_regioni_wide <- df_regioni %>%
  select(id, nome, fascia, pop, media_min, mediana_min, pop_totale) %>%
  pivot_wider(
    id_cols     = c(id, nome, media_min, mediana_min, pop_totale),
    names_from  = fascia,
    values_from = pop,
    values_fill = 0
  )

write_csv(df_regioni, file.path(out_dir, "italia_regioni_fasce_5min_long.csv"))
write_csv(df_regioni_wide, file.path(out_dir, "italia_regioni_fasce_5min_wide.csv"))

cat("Risultati regioni salvati in output/\n")

# ===========================================================================
# 9. ANALISI ITALIA PER PROVINCIA (NUTS3)
# ===========================================================================

cat("\n=== ANALISI ITALIA PER PROVINCIA (NUTS3) ===\n")

nuts3_it <- nuts3 %>% filter(grepl("^IT", NUTS_ID))
cat("Province italiane trovate:", nrow(nuts3_it), "\n")

df_province <- calc_stats(
  zones      = nuts3_it,
  r_tempo    = r_access_1km,
  r_pop      = r_pop,
  breaks_min = breaks_min,
  labels_min = labels_min,
  id_col     = "NUTS_ID",
  name_col   = "NAME_LATN"
)

df_province_wide <- df_province %>%
  select(id, nome, fascia, pop, media_min, mediana_min, pop_totale) %>%
  pivot_wider(
    id_cols     = c(id, nome, media_min, mediana_min, pop_totale),
    names_from  = fascia,
    values_from = pop,
    values_fill = 0
  )

write_csv(df_province, file.path(out_dir, "italia_province_fasce_5min_long.csv"))
write_csv(df_province_wide, file.path(out_dir, "italia_province_fasce_5min_wide.csv"))

cat("Risultati province salvati in output/\n")

# ===========================================================================
# 10. RIEPILOGO
# ===========================================================================

cat("\n")
cat("===================================================\n")
cat("  ANALISI COMPLETATA\n")
cat("===================================================\n")
cat("\nFile generati in:", out_dir, "\n\n")
cat("STATI EUROPEI:\n")
cat("  - stati_eu_fasce_5min_long.csv  (formato lungo)\n")
cat("  - stati_eu_fasce_5min_wide.csv  (formato largo)\n")
cat("\nITALIA - REGIONI:\n")
cat("  - italia_regioni_fasce_5min_long.csv\n")
cat("  - italia_regioni_fasce_5min_wide.csv\n")
cat("\nITALIA - PROVINCE:\n")
cat("  - italia_province_fasce_5min_long.csv\n")
cat("  - italia_province_fasce_5min_wide.csv\n")
cat("\nOgni file contiene:\n")
cat("  - Popolazione per fascia di 5 minuti (0-5, 5-10, ..., 60+)\n")
cat("  - Media ponderata del tempo (pesata per popolazione)\n")
cat("  - Mediana ponderata del tempo (pesata per popolazione)\n")
cat("  - Popolazione totale della zona\n")
cat("===================================================\n")
