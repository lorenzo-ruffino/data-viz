#!/usr/bin/env Rscript
# Dati per produttivita', costo del lavoro per unita' di prodotto (CLUP) e
# salari nella manifattura: redditi da lavoro e dipendenti per branca.
suppressMessages({library(eurostat); library(dplyr); library(readr)})
inp <- "../input"; options(timeout = 600)
geo_cmp <- c("IT","DE","FR","ES","EU27_2020")
try_step <- function(label, fn) {
  cat(label, "\n")
  out <- tryCatch(fn(), error = function(e){cat("  ERRORE:",conditionMessage(e),"\n");NULL})
  if (!is.null(out)) cat("  OK righe:", nrow(out), "\n"); invisible(out)
}

# Redditi da lavoro (D1) e retribuzioni (D11) per branca, prezzi correnti -----
try_step("nama_10_a10 (redditi da lavoro D1, retribuzioni D11)", function() {
  cmp <- get_eurostat("nama_10_a10", filters = list(
        geo = geo_cmp, na_item = c("D1","D11"), unit = "CP_MEUR"),
        time_format = "num")
  cmp <- cmp |> filter(nace_r2 %in% c("TOTAL","C","B-E")) |>
    select(geo, nace_r2, voce = na_item, anno = time, valore_meur = values) |>
    arrange(geo, nace_r2, voce, anno)
  write_csv(cmp, file.path(inp, "eurostat_redditi_lavoro_a10.csv")); cmp
})

# Dipendenti (lavoratori dipendenti, concetto domestico) per branca ----------
try_step("nama_10_a10_e (dipendenti SAL_DC)", function() {
  sal <- get_eurostat("nama_10_a10_e", filters = list(
        geo = geo_cmp, na_item = "SAL_DC", unit = "THS_PER"),
        time_format = "num")
  sal <- sal |> filter(nace_r2 %in% c("TOTAL","C","B-E")) |>
    select(geo, nace_r2, anno = time, dipendenti_migliaia = values) |>
    arrange(geo, nace_r2, anno)
  write_csv(sal, file.path(inp, "eurostat_dipendenti_a10.csv")); sal
})
cat("FATTO.\n")
