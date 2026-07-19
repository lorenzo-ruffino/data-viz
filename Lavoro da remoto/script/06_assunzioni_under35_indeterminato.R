# ============================================================
# Assunzioni a tempo indeterminato di under 35 - 2023/2024/2025
# Stima del flusso annuo di nuovi assunti e costo di un incentivo
# di 200 euro/mese per assunto.
# ============================================================
# Fonte: microdati RCFL (MICRO.STAT), 4 trimestri x 3 anni.
#
# Definizione di "nuovo assunto a tempo indeterminato under 35":
#   COND3  == 1   occupato
#   DIPAUT == 1   dipendente
#   DETIND == 2   contratto a tempo indeterminato
#   CLETAS in 5:8 eta' 15-34 (under 35)
#   INIATT == 1   ha iniziato il lavoro ATTUALE nelle ultime 13 settimane
#   peso = COEF_CCP / 10
#
# Logica del flusso annuo:
#   In ogni trimestre INIATT==1 cattura chi ha iniziato il lavoro
#   attuale nelle ~13 settimane precedenti l'intervista (~un trimestre).
#   Le 4 finestre trimestrali coprono l'anno => la SOMMA dei 4 trimestri
#   stima il numero di assunzioni avvenute nell'anno.
#   (E' una stima "dei sopravvissuti": conta chi e' ancora occupato al
#    momento dell'intervista, quindi e' un limite inferiore del flusso
#    lordo di assunzioni rispetto ai dati amministrativi INPS/COB.)
# ============================================================

suppressMessages({
  library(data.table)
})

base_dir   <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Lavoro da remoto"
input_dir  <- file.path(base_dir, "input")
output_dir <- file.path(base_dir, "output")

anni      <- c(2023, 2024, 2025)
trimestri <- c("Primo", "Secondo", "Terzo", "Quarto")

vars <- c("COND3", "DIPAUT", "DETIND", "CLETAS", "INIATT", "COEF_CCP", "SESSO")

leggi_trim <- function(anno, trim) {
  f <- file.path(input_dir, sprintf("RCFL_Microdati_%d_%s_trimestre.txt", anno, trim))
  dt <- fread(f, sep = "\t", select = vars, colClasses = "character",
              showProgress = FALSE)
  dt[, `:=`(
    peso   = as.numeric(COEF_CCP) / 10,
    cond3  = as.integer(COND3),
    dipaut = as.integer(DIPAUT),
    detind = as.integer(DETIND),
    cletas = as.integer(CLETAS),
    iniatt = as.integer(INIATT),
    sesso  = as.integer(SESSO),
    anno   = anno,
    trim   = trim
  )]
  dt[]
}

cat("Lettura 12 trimestri RCFL (2023-2025)...\n")
dati <- rbindlist(lapply(anni, function(a) {
  rbindlist(lapply(trimestri, function(t) {
    cat(sprintf("  %d %s\n", a, t))
    leggi_trim(a, t)
  }))
}))

# Universo: occupati dipendenti under 35
base_u35 <- dati[cond3 == 1L & dipaut == 1L & cletas %in% 5:8 & !is.na(peso) & peso > 0]

# Flag nuovo assunto a tempo indeterminato (INIATT==1, DETIND==2)
base_u35[, neo_indet := iniatt == 1L & detind == 2L]
base_u35[, neo_det   := iniatt == 1L & detind == 1L]

# ---- Dettaglio trimestrale ----
trim_tab <- base_u35[, .(
  assunti_indet_migliaia = sum(peso[neo_indet]) / 1000,
  assunti_det_migliaia   = sum(peso[neo_det])   / 1000,
  n_obs_indet            = sum(neo_indet)
), by = .(anno, trim)]
trim_tab[, trim := factor(trim, levels = trimestri)]
setorder(trim_tab, anno, trim)

cat("\n================ DETTAGLIO TRIMESTRALE ================\n")
print(trim_tab[, .(anno, trim,
                   assunti_indet_migliaia = round(assunti_indet_migliaia, 1),
                   n_obs_indet,
                   assunti_det_migliaia = round(assunti_det_migliaia, 1))],
      row.names = FALSE)

# ---- Aggregato annuo (somma dei 4 trimestri = flusso annuo) ----
anno_tab <- base_u35[, .(
  assunti_indet = sum(peso[neo_indet]),
  assunti_det   = sum(peso[neo_det]),
  n_obs_indet   = sum(neo_indet)
), by = anno]
setorder(anno_tab, anno)

# ---- Costo della misura: 200 euro/mese per assunto ----
incentivo_mese <- 200
anno_tab[, `:=`(
  costo_mensile_mln = assunti_indet * incentivo_mese / 1e6,          # se pagati 1 mese
  costo_12mesi_mln  = assunti_indet * incentivo_mese * 12 / 1e6      # se pagati 12 mesi
)]

cat("\n================ FLUSSO ANNUO + COSTO ================\n")
cat(sprintf("Incentivo ipotizzato: %d euro/mese per under-35 assunto a tempo indeterminato\n\n",
            incentivo_mese))
out <- anno_tab[, .(
  anno,
  assunti_indet_under35 = round(assunti_indet),
  in_migliaia           = round(assunti_indet / 1000, 1),
  n_obs                 = n_obs_indet,
  costo_12mesi_mln_eur  = round(costo_12mesi_mln, 1)
)]
print(out, row.names = FALSE)

cat("\n--- Costo totale misura (incentivo pagato per 12 mesi a ciascun assunto) ---\n")
for (i in seq_len(nrow(anno_tab))) {
  cat(sprintf("  %d: %s assunti  ->  %.1f mln euro/anno  (run-rate mensile %.1f mln)\n",
              anno_tab$anno[i],
              format(round(anno_tab$assunti_indet[i]), big.mark = ".", decimal.mark = ","),
              anno_tab$costo_12mesi_mln[i],
              anno_tab$costo_mensile_mln[i]))
}

tot_assunti <- sum(anno_tab$assunti_indet)
cat(sprintf("\n  TOTALE 2023-2025: %s assunti  ->  %.1f mln euro (12 mesi cad.)\n",
            format(round(tot_assunti), big.mark = ".", decimal.mark = ","),
            sum(anno_tab$costo_12mesi_mln)))

# ---- Salvataggio ----
fwrite(trim_tab, file.path(output_dir, "assunzioni_u35_indet_trimestrale.csv"))
fwrite(out,      file.path(output_dir, "assunzioni_u35_indet_annuale_costo.csv"))
cat(sprintf("\nCSV salvati in: %s/\n", output_dir))
