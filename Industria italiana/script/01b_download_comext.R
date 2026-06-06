#!/usr/bin/env Rscript
# Dati di commercio estero Eurostat (famiglia Comext) per il confronto
# internazionale: ext_lt_intratrd = trade by SITC product group.
suppressMessages({library(eurostat); library(dplyr); library(readr)})
inp <- "../input"
options(timeout = 600)

cat("ext_lt_intratrd (export/import per gruppo merceologico SITC)\n")
tr <- get_eurostat("ext_lt_intratrd", filters = list(
        geo = c("IT","DE","FR","ES","EU27_2020"),
        indic_et = c("MIO_EXP_VAL","MIO_IMP_VAL","MIO_BAL_VAL"),
        partner = c("WORLD","EXT_EU27_2020")),
        time_format = "num")
tr <- tr |>
  select(geo, indicatore = indic_et, sitc = sitc06, partner,
         anno = time, valore_mln_eur = values) |>
  arrange(geo, indicatore, sitc, partner, anno)
write_csv(tr, file.path(inp, "eurostat_comext_export_sitc.csv"))
cat("  OK righe:", nrow(tr), "\n")
cat("FATTO.\n")
