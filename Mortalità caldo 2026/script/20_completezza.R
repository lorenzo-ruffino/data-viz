# Correzione per l'incompletezza dei dati provvisori più recenti.
#
# I decessi delle ultime settimane di ogni rilascio Istat vengono rivisti al
# rialzo nei rilasci successivi (registrazioni tardive). La revisione dipende
# dall'"età" del dato al momento del rilascio (giorni tra la data di morte e la
# pubblicazione) e si misura confrontando due rilasci precedenti con quello
# attuale (25/8/2026, dati al 30/6/2026):
#   - rilascio 5/8/2025 (dati al 30/6/2025), anno 2025;
#   - rilascio 14/5/2026 (dati al 31/3/2026), anno 2026.
# Revisione nazionale: curva loess della revisione giornaliera in funzione
# dell'età del dato (due rilasci insieme, pesi = decessi), letta all'età che i
# dati 2026 hanno nel rilascio attuale (56 giorni per il 30 giugno).
# Differenze regionali: moltiplicatore = revisione della regione / revisione
# nazionale sui giorni con età 42-120 (stesso metodo di 05_confronto_rilasci.R).
# Fattore = 1 + revisione nazionale(età) x moltiplicatore regionale.
#
# Output: input/derivati/fattori_completezza.csv (cod_reg x data 2026 -> fattore)
#         output/completezza_revisioni.csv (revisione nazionale per settimana)

library(data.table)

setwd("/Users/lorenzoruffino/Documents/Progetti/data-viz/Mortalità caldo 2026")

leggi <- function(path, cols) {
  x <- fread(path, select = c("REG", "GE", cols), colClasses = "character",
             encoding = "Latin-1")
  x <- melt(x, id.vars = c("REG", "GE"), variable.name = "col", value.name = "v")
  x[, v := suppressWarnings(as.integer(v))]
  x[, .(n = sum(v, na.rm = TRUE)), by = .(cod_reg = as.integer(REG), GE, col)]
}

RILASCIO_ATTUALE <- as.IDate("2026-08-25")
attuale <- leggi("input/istat/comuni_giornaliero_30giugno26.csv", c("T_25", "T_26"))
coppie <- list(
  list(file = "input/istat_rilasci_precedenti/rilascio_2025-08-05_dati_al_2025-06-30/comuni_giornaliero_30giugno25.csv",
       col = "T_25", anno = 2025, fine = as.IDate("2025-06-30"), rilascio = as.IDate("2025-08-05")),
  list(file = "input/istat_rilasci_precedenti/rilascio_2026-05-14_dati_al_2026-03-31/comuni_giornaliero_31marzo26.csv",
       col = "T_26", anno = 2026, fine = as.IDate("2026-03-31"), rilascio = as.IDate("2026-05-14")))

rev <- rbindlist(lapply(coppie, function(cp) {
  a <- leggi(cp$file, cp$col)
  r <- merge(a, attuale[col == cp$col], by = c("cod_reg", "GE"), suffixes = c("_prov", "_att"))
  r[, data := as.IDate(sprintf("%d-%s-%s", cp$anno, substr(GE, 1, 2), substr(GE, 3, 4)))]
  r <- r[!is.na(data) & data <= cp$fine]
  r[, `:=`(eta_dato = as.integer(cp$rilascio - data), rilascio = cp$rilascio,
           sett = (as.integer(cp$fine) - as.integer(data)) %/% 7L)]
  r[, .(cod_reg, data, rilascio, eta_dato, sett, n_prov, n_att)]
}))

naz <- rev[, .(n_prov = sum(n_prov), n_att = sum(n_att)), by = .(rilascio, data, eta_dato, sett)]
naz[, revisione := n_att / n_prov - 1]
fwrite(naz[sett <= 12, .(n_prov = sum(n_prov), n_att = sum(n_att),
                         revisione = sum(n_att) / sum(n_prov) - 1), by = .(rilascio, sett)][order(rilascio, -sett)],
       "output/completezza_revisioni.csv")

mod <- loess(revisione ~ eta_dato, data = naz[eta_dato <= 200], weights = n_prov, span = 0.5)

reg <- rev[eta_dato %between% c(42, 120), .(rev_reg = sum(n_att) / sum(n_prov) - 1), by = cod_reg]
rev_naz <- rev[eta_dato %between% c(42, 120), sum(n_att) / sum(n_prov) - 1]
reg[, moltiplicatore := rev_reg / rev_naz]

giorni <- data.table(data = seq(as.IDate("2026-04-01"), as.IDate("2026-06-30"), by = 1))
giorni[, eta_dato := as.integer(RILASCIO_ATTUALE - data)]
giorni[, rev_naz := predict(mod, newdata = giorni)]
stopifnot(!anyNA(giorni$rev_naz))
fatt <- CJ(cod_reg = reg$cod_reg, data = giorni$data)
fatt <- reg[, .(cod_reg, moltiplicatore)][giorni[fatt, on = "data"], on = "cod_reg"]
fatt[, fattore := 1 + rev_naz * moltiplicatore]
fwrite(fatt[order(cod_reg, data), .(cod_reg, data, eta_dato, rev_naz, moltiplicatore, fattore)],
       "input/derivati/fattori_completezza.csv")

cat("Revisione nazionale stimata per età del dato:\n")
print(giorni[data %in% as.IDate(c("2026-04-01", "2026-05-01", "2026-05-21", "2026-06-01",
                                  "2026-06-17", "2026-06-30"))], digits = 3)
cat("\nMoltiplicatori regionali:\n")
print(reg[order(cod_reg)], digits = 3)
