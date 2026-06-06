#!/usr/bin/env Rscript
# Download dati Eurostat per l'articolo "Che fine ha fatto l'industria italiana"
suppressMessages({library(eurostat); library(dplyr); library(tidyr); library(readr)})

inp <- "../input"
geo_cmp <- c("IT","DE","FR","ES","EU27_2020")
options(timeout = 600)

ok <- function(x) cat("  OK righe:", nrow(x), "\n")

try_step <- function(label, fn) {
  cat(label, "\n")
  out <- tryCatch(fn(), error = function(e) {cat("  ERRORE:", conditionMessage(e), "\n"); NULL})
  if (!is.null(out)) cat("  OK righe:", nrow(out), "\n")
  invisible(out)
}

# 1) Indice della produzione industriale (annuale) ---------------------------
try_step("1) sts_inpr_a", function() {
  ip <- get_eurostat("sts_inpr_a", filters = list(
          geo = geo_cmp, indic_bt = "PRD", s_adj = "CA", unit = "I21"),
          time_format = "num")
  ip <- ip |> filter(nace_r2 %in% c("B-D","C","MIG_CAG","MIG_COG","MIG_ING","MIG_NRG_X_D_E")) |>
        select(geo, nace_r2, anno = time, indice = values) |> arrange(geo, nace_r2, anno)
  write_csv(ip, file.path(inp, "eurostat_produzione_industriale.csv")); ip
})

# 2) Valore aggiunto per 10 branche ------------------------------------------
try_step("2) nama_10_a10 (valore aggiunto)", function() {
  va <- get_eurostat("nama_10_a10", filters = list(
          geo = geo_cmp, na_item = "B1G",
          unit = c("CP_MEUR","CLV15_MEUR","CLV_PCH_PRE")),
          time_format = "num")
  va <- va |> filter(nace_r2 %in% c("TOTAL","A","B-E","C","F","G-I","B-D")) |>
        select(geo, nace_r2, unit, anno = time, valore = values) |> arrange(geo, nace_r2, unit, anno)
  write_csv(va, file.path(inp, "eurostat_valore_aggiunto_a10.csv")); va
})

# 2b) Occupazione per 10 branche ---------------------------------------------
try_step("2b) nama_10_a10_e (occupazione)", function() {
  emp <- get_eurostat("nama_10_a10_e", filters = list(
          geo = geo_cmp, na_item = "EMP_DC", unit = "THS_PER"),
          time_format = "num")
  emp <- emp |> filter(nace_r2 %in% c("TOTAL","A","B-E","C","F","G-I","B-D")) |>
        select(geo, nace_r2, anno = time, occupati_migliaia = values) |> arrange(geo, nace_r2, anno)
  write_csv(emp, file.path(inp, "eurostat_occupazione_a10.csv")); emp
})

# 3) PIL e componenti: export di beni e servizi ------------------------------
try_step("3) nama_10_gdp (PIL ed export)", function() {
  gdp <- get_eurostat("nama_10_gdp", filters = list(
          geo = geo_cmp, unit = "CP_MEUR",
          na_item = c("B1GQ","P6","P61","P62")),
          time_format = "num")
  gdp <- gdp |> select(geo, na_item, anno = time, valore_meur = values) |> arrange(geo, na_item, anno)
  write_csv(gdp, file.path(inp, "eurostat_pil_export.csv")); gdp
})

# 4) Valore aggiunto per 64 branche - Italia ---------------------------------
try_step("4) nama_10_a64 (VA 64 branche, IT)", function() {
  a64 <- get_eurostat("nama_10_a64", filters = list(
          geo = "IT", na_item = "B1G", unit = c("CP_MEUR","CLV15_MEUR")),
          time_format = "num")
  a64 <- a64 |> select(nace_r2, unit, anno = time, valore = values) |> arrange(nace_r2, unit, anno)
  write_csv(a64, file.path(inp, "eurostat_valore_aggiunto_a64_italia.csv")); a64
})

# 4b) Occupazione per 64 branche - Italia ------------------------------------
try_step("4b) nama_10_a64_e (occupazione 64 branche, IT)", function() {
  a64e <- get_eurostat("nama_10_a64_e", filters = list(
          geo = "IT", na_item = "EMP_DC", unit = "THS_PER"),
          time_format = "num")
  a64e <- a64e |> select(nace_r2, anno = time, occupati_migliaia = values) |> arrange(nace_r2, anno)
  write_csv(a64e, file.path(inp, "eurostat_occupazione_a64_italia.csv")); a64e
})

# 5) Conti regionali: valore aggiunto per regione NUTS2 ----------------------
try_step("5) nama_10r_3gva (VA regionale)", function() {
  rgva <- get_eurostat("nama_10r_3gva", filters = list(unit = "CP_MEUR"),
          time_format = "num")
  rgva <- rgva |> filter(grepl("^IT", geo), nchar(geo) == 4,
                         nace_r2 %in% c("TOTAL","C","B-E","F")) |>
        select(geo, nace_r2, anno = time, va_meur = values) |> arrange(geo, nace_r2, anno)
  write_csv(rgva, file.path(inp, "eurostat_valore_aggiunto_regionale.csv")); rgva
})

# 5b) Conti regionali: occupazione per regione NUTS2 -------------------------
try_step("5b) nama_10r_3empers (occupazione regionale)", function() {
  remp <- get_eurostat("nama_10r_3empers", filters = list(wstatus = "EMP"),
          time_format = "num")
  remp <- remp |> filter(grepl("^IT", geo), nchar(geo) == 4,
                         nace_r2 %in% c("TOTAL","C","B-E","F")) |>
        select(geo, nace_r2, anno = time, occupati_migliaia = values) |> arrange(geo, nace_r2, anno)
  write_csv(remp, file.path(inp, "eurostat_occupazione_regionale.csv")); remp
})

cat("FATTO.\n")
