#!/usr/bin/env Rscript
# Dati aggiuntivi: deflatore dell'inflazione (HICP) e demografia d'impresa.
suppressMessages({library(eurostat); library(dplyr); library(readr)})
inp <- "../input"; options(timeout = 600)
try_step <- function(label, fn) {
  cat(label, "\n")
  out <- tryCatch(fn(), error = function(e){cat("  ERRORE:",conditionMessage(e),"\n");NULL})
  if (!is.null(out)) cat("  OK righe:", nrow(out), "\n"); invisible(out)
}

# 1) Indice dei prezzi al consumo (HICP) Italia - deflatore -----------------
try_step("1) prc_hicp_aind (HICP Italia, deflatore)", function() {
  h <- get_eurostat("prc_hicp_aind", filters = list(
        geo = "IT", coicop = "CP00", unit = "INX_A_AVG"), time_format = "num")
  h <- h |> select(anno = time, hicp_indice = values) |> arrange(anno)
  write_csv(h, file.path(inp, "eurostat_hicp_italia.csv")); h
})

# 2) Demografia d'impresa: imprese attive, nate, cessate --------------------
try_step("2) bd_9ac_l_form_r2 (demografia d'impresa, manifattura)", function() {
  bd <- get_eurostat("bd_9ac_l_form_r2", filters = list(
        geo = "IT", leg_form = "TOTAL",
        indic_sb = c("V11910","V11920","V11930","V16910","V16920","V16930",
                     "V97010","V97020","V97030","V97040","V97050")),
        time_format = "num")
  bd <- bd |> filter(nace_r2 %in% c("B-E","B-N_X_K642","C","C10-C12","C13_C14",
            "C15","C16","C17_C18","C19","C20_C21","C22","C23","C24_C25",
            "C26_C27","C28","C29_C30","C31_C32","C33")) |>
        select(nace_r2, indic_sb, anno = time, valore = values) |>
        arrange(nace_r2, indic_sb, anno)
  write_csv(bd, file.path(inp, "eurostat_demografia_impresa.csv")); bd
})
cat("FATTO.\n")
