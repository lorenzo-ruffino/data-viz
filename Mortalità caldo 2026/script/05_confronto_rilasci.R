# Revisioni dei dati provvisori: stessi giorni in rilasci Istat successivi.
#
# Input:  input/istat_rilasci_precedenti/<rilascio>/comuni_giornaliero_*.csv
#         input/istat/comuni_giornaliero_30giugno26.csv (rilascio 25/8/2026)
# Output: output/qualita_rilasci_giornaliero.csv  decessi giornalieri Italia per rilascio
#         output/qualita_rilasci_revisioni.csv    revisione per finestra prima della data di chiusura
#         output/qualita_revisione_eta_dato.csv   revisione per età del dato al rilascio (giorni)
#         output/qualita_fattori_giugno2026.csv   fattore di correzione proposto per giorno, giugno 2026
#         output/qualita_fattori_regione.csv      moltiplicatore regionale della revisione
#
# Per ogni rilascio precedente si confrontano gli ultimi giorni coperti con gli stessi
# giorni nel rilascio più recente: la differenza misura quanto mancava alla
# pubblicazione (ritardo di registrazione) a parità di distanza dalla data di chiusura.

library(data.table)

setwd("/Users/lorenzoruffino/Documents/Progetti/data-viz/Mortalità caldo 2026")

rilasci <- data.table(
  rilascio = c("2025-08-05", "2026-03-31", "2026-05-14", "2026-08-25"),
  chiusura = as.IDate(c("2025-06-30", "2025-12-31", "2026-03-31", "2026-06-30")),
  file = c("input/istat_rilasci_precedenti/rilascio_2025-08-05_dati_al_2025-06-30/comuni_giornaliero_30giugno25.csv",
           "input/istat_rilasci_precedenti/rilascio_2026-03-31_dati_al_2025-12-31/comuni_giornaliero_31dicembre25.csv",
           "input/istat_rilasci_precedenti/rilascio_2026-05-14_dati_al_2026-03-31/comuni_giornaliero_31marzo26.csv",
           "input/istat/comuni_giornaliero_30giugno26.csv"))

leggi <- function(f, chiusura) {
  h <- names(fread(f, nrows = 0))
  tcol <- grep("^T_(2[3-6])$", h, value = TRUE)
  d <- fread(f, select = c("REG", "COD_PROVCOM", "GE", tcol), encoding = "Latin-1",
             colClasses = "character")
  d <- melt(d, id.vars = c("REG", "COD_PROVCOM", "GE"), variable.name = "var", value.name = "n",
            variable.factor = FALSE)
  d <- d[n != "n.d."]
  d[, data := as.IDate(sprintf("20%s-%s-%s", substr(var, 3, 4), substr(GE, 1, 2), substr(GE, 3, 4)))]
  d <- d[!is.na(data) & data <= chiusura]
  list(naz = d[, .(n = sum(as.integer(n))), by = data],
       reg = d[data >= as.IDate("2025-01-01"), .(n = sum(as.integer(n))), by = .(cod_reg = as.integer(REG), data)],
       comuni = d[data > chiusura - 28, .(n = sum(as.integer(n))), by = .(cod = COD_PROVCOM, data)])
}

ris <- lapply(seq_len(nrow(rilasci)), function(i) leggi(rilasci$file[i], rilasci$chiusura[i]))
giorn <- rbindlist(lapply(seq_len(nrow(rilasci)), function(i)
  ris[[i]]$naz[, rilascio := rilasci$rilascio[i]]))
fwrite(giorn, "output/qualita_rilasci_giornaliero.csv")

ultimo <- giorn[rilascio == "2026-08-25", .(data, n_ultimo = n)]
# per i rilasci successivi a quello in esame, il confronto più aggiornato disponibile
riv <- rbindlist(lapply(1:3, function(i) {
  prec <- giorn[rilascio == rilasci$rilascio[i]]
  succ <- giorn[rilascio %in% rilasci$rilascio[(i + 1):4]]
  rbindlist(lapply(unique(succ$rilascio), function(r) {
    x <- merge(prec, succ[rilascio == r, .(data, n_dopo = n)], by = "data")
    x[, giorni_da_chiusura := as.integer(rilasci$chiusura[i] - data)]
    x[, finestra := fcase(giorni_da_chiusura < 7, "ultimi 7 giorni",
                          giorni_da_chiusura < 14, "8-14 giorni prima",
                          giorni_da_chiusura < 28, "15-28 giorni prima",
                          giorni_da_chiusura < 90, "29-90 giorni prima",
                          default = "oltre 90 giorni")]
    x[, .(n_prima = sum(n), n_dopo = sum(n_dopo)), by = finestra][
      , `:=`(rilascio_prima = rilasci$rilascio[i], chiusura = rilasci$chiusura[i],
             rilascio_dopo = r, revisione_pct = 100 * (n_dopo / n_prima - 1))]
  }))
}))
setcolorder(riv, c("rilascio_prima", "chiusura", "rilascio_dopo", "finestra"))
print(riv, digits = 4)
fwrite(riv, "output/qualita_rilasci_revisioni.csv")

# giorno per giorno negli ultimi 10 giorni di ciascun rilascio
for (i in 1:3) {
  x <- merge(giorn[rilascio == rilasci$rilascio[i], .(data, prima = n)],
             giorn[rilascio %in% rilasci$rilascio[(i + 1):4]] |>
               dcast(data ~ rilascio, value.var = "n"), by = "data")
  cat("\nRilascio", rilasci$rilascio[i], "- ultimi 10 giorni\n")
  print(tail(x, 10))
}

# comuni con almeno un decesso negli ultimi 7 vs 7 giorni precedenti, per rilascio
cop <- rbindlist(lapply(seq_len(nrow(rilasci)), function(i) {
  c <- ris[[i]]$comuni; ch <- rilasci$chiusura[i]
  data.table(rilascio = rilasci$rilascio[i],
             comuni_ultimi7 = c[data > ch - 7 & n > 0, uniqueN(cod)],
             comuni_prec7 = c[data <= ch - 7 & data > ch - 14 & n > 0, uniqueN(cod)])
}))
print(cop)

# Revisione per età del dato al rilascio (giorni tra data di decesso e rilascio) --------
# confronto dei rilasci del 5/8/2025 e del 14/5/2026 con quello del 25/8/2026
x <- giorn[rilascio %in% c("2025-08-05", "2026-05-14")][ultimo, on = "data", nomatch = 0]
x[, eta_dato := as.integer(as.IDate(rilascio) - data)]
x <- x[eta_dato <= 200]
x[, classe := cut(eta_dato, c(35, 42, 49, 56, 63, 70, 77, 84, 91, 105, 120, 150, 200))]
rev_eta <- x[, .(n_prima = sum(n), n_dopo = sum(n_ultimo)), by = .(rilascio, classe)][
  , revisione_pct := 100 * (n_dopo / n_prima - 1)]
setorder(rev_eta, rilascio, classe)
print(dcast(rev_eta, classe ~ rilascio, value.var = "revisione_pct"), digits = 3)
fwrite(rev_eta, "output/qualita_revisione_eta_dato.csv")

# curva liscia della revisione (due rilasci insieme) e fattore per i giorni di giugno 2026
x[, rev := n_ultimo / n - 1]
mod <- loess(rev ~ eta_dato, data = x, weights = n, span = 0.5)
giu26 <- data.table(data = seq(as.IDate("2026-06-01"), as.IDate("2026-06-30"), by = 1))
giu26[, eta_dato := as.integer(as.IDate("2026-08-25") - data)]
giu26[, fattore := 1 + predict(mod, newdata = giu26)]
giu26 <- giorn[rilascio == "2026-08-25", .(data, n)][giu26, on = "data"]
giu26[, n_corretto := n * fattore]
print(giu26, digits = 4)
cat("Giugno 2026: osservati", giu26[, sum(n)], "- corretti", round(giu26[, sum(n_corretto)]),
    sprintf("(+%.1f%%)\n", 100 * (giu26[, sum(n_corretto) / sum(n)] - 1)))
fwrite(giu26, "output/qualita_fattori_giugno2026.csv")

# Moltiplicatore regionale: revisione regionale / nazionale, dati con età 42-120 giorni
reg <- rbindlist(lapply(c(1, 3), function(i)
  ris[[i]]$reg[, rilascio := rilasci$rilascio[i]]))
reg_u <- ris[[4]]$reg[, .(cod_reg, data, n_ultimo = n)]
reg <- reg[reg_u, on = .(cod_reg, data), nomatch = 0]
reg[, eta_dato := as.integer(as.IDate(rilascio) - data)]
reg <- reg[eta_dato %between% c(42, 120)]
molt <- reg[, .(n_prima = sum(n), n_dopo = sum(n_ultimo)), by = .(cod_reg, rilascio)][
  , rev_pct := 100 * (n_dopo / n_prima - 1)]
molt <- dcast(molt, cod_reg ~ rilascio, value.var = "rev_pct")
naz_rev <- reg[, 100 * (sum(n_ultimo) / sum(n) - 1)]
tot_reg <- reg[, .(rev_pct = 100 * (sum(n_ultimo) / sum(n) - 1)), by = cod_reg]
molt <- molt[tot_reg, on = "cod_reg"][, moltiplicatore := rev_pct / naz_rev]
print(molt, digits = 3)
fwrite(molt, "output/qualita_fattori_regione.csv")
