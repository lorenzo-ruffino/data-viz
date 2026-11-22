# Primo stadio DLNM: un modello per provincia (110) per il totale dei decessi,
# per gli strati (classi d'età, sesso) e per le specifiche di sensibilità.
# Specifiche e funzioni in dlnm_funzioni.R.
#
# Output: input/derivati/dlnm/primo_stadio_<nome>.rds

source("/Users/lorenzoruffino/Documents/Progetti/data-viz/Mortalità caldo 2026/script/dlnm_funzioni.R")

dec  <- carica_decessi()
temp <- carica_temperature()

esegui <- function(spec, salva_fitted = FALSE) {
  f <- file.path(DIR_DLNM, sprintf("primo_stadio_%s.rds", spec$nome))
  t0 <- Sys.time()
  ps <- primo_stadio(spec, dec, temp, salva_fitted)
  saveRDS(ps, f)
  ok <- Filter(function(r) is.null(r$errore), ps$prov)
  cat(sprintf("%-15s province %3d, non convergenti %d, coef NA %d, dispersione mediana %.2f (%.0fs)\n",
              spec$nome, length(ok), sum(!sapply(ok, `[[`, "conv")),
              sum(sapply(ok, `[[`, "na_coef")), median(sapply(ok, `[[`, "disp")),
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  invisible(ps)
}

esegui(spec_base, salva_fitted = TRUE)
for (s in SPEC_STRATI) esegui(s)
for (s in SPEC_SENS) esegui(s, salva_fitted = s$nome == "senza_2026")
