#!/usr/bin/env Rscript
# Dati per la Sezione 12 (energia): produzione industriale per settore nei
# quattro Paesi, uso di energia per settore (per identificare gli energivori),
# prezzi dell'energia per l'industria.
suppressMessages({library(eurostat); library(dplyr); library(readr)})
inp <- "../input"; options(timeout = 600)
geo_cmp <- c("IT","DE","FR","ES","EU27_2020")
try_step <- function(label, fn) {
  cat(label, "\n")
  out <- tryCatch(fn(), error=function(e){cat("  ERRORE:",conditionMessage(e),"\n");NULL})
  if (!is.null(out)) cat("  OK righe:", nrow(out), "\n"); invisible(out)
}
divs <- paste0("C", c(10:33))

# 1) Indice produzione industriale per divisione manifatturiera, 5 Paesi -----
try_step("1) sts_inpr_a per divisione (IT/DE/FR/ES/UE)", function() {
  ip <- get_eurostat("sts_inpr_a", filters = list(
        geo = geo_cmp, indic_bt = "PRD", s_adj = "CA", unit = "I21"),
        time_format = "num")
  ip <- ip |> filter(nace_r2 %in% c(divs, "C")) |>
    select(geo, nace_r2, anno = time, indice = values) |>
    arrange(geo, nace_r2, anno)
  write_csv(ip, file.path(inp, "eurostat_produzione_settore_paesi.csv")); ip
})

# 2) Uso di energia per settore manifatturiero - Italia (per intensita') -----
try_step("2) env_ac_pefasu (uso di energia per settore, IT)", function() {
  en <- get_eurostat("env_ac_pefasu", filters = list(
        geo = "IT", stk_flow = "USE", prod_nrg = "P00"),
        time_format = "num")
  en <- en |> filter(nace_r2 %in% c("C", divs)) |>
    select(nace_r2, anno = time, energia_tj = values) |>
    arrange(nace_r2, anno)
  write_csv(en, file.path(inp, "eurostat_energia_per_settore_italia.csv")); en
})

# 3) Prezzi dell'energia per l'industria, IT/DE/FR/ES/UE ---------------------
try_step("3a) nrg_pc_205 (prezzo elettricita' industria)", function() {
  el <- get_eurostat("nrg_pc_205", filters = list(
        geo = geo_cmp, nrg_cons = "MWH20000-69999", unit = "KWH",
        tax = c("X_TAX","I_TAX"), currency = "EUR"),
        time_format = "num")
  el <- el |> select(geo, tassazione = tax, anno = time, prezzo_eur_kwh = values) |>
    arrange(geo, tassazione, anno)
  write_csv(el, file.path(inp, "eurostat_prezzo_elettricita_industria.csv")); el
})
try_step("3b) nrg_pc_203 (prezzo gas industria)", function() {
  gs <- get_eurostat("nrg_pc_203", filters = list(
        geo = geo_cmp, nrg_cons = "GJ100000-999999", unit = "KWH",
        tax = c("X_TAX","I_TAX"), currency = "EUR"),
        time_format = "num")
  gs <- gs |> select(geo, tassazione = tax, anno = time, prezzo_eur_kwh = values) |>
    arrange(geo, tassazione, anno)
  write_csv(gs, file.path(inp, "eurostat_prezzo_gas_industria.csv")); gs
})
cat("FATTO.\n")
