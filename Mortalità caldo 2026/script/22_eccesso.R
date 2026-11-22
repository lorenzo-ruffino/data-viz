# Eccesso di mortalità osservato meno atteso, 2026, con intervalli di previsione.
#
# Osservati: decessi Istat 2026 corretti per l'incompletezza dei dati provvisori
# (fattori di 20_completezza.R per regione e giorno). Ogni tabella
# riporta anche l'eccesso senza correzione.
# Attesi: 21_baseline_attesi.R (trend, stagionalità, giorni di caldo esclusi).
#
# Intervallo di previsione al 95% dell'eccesso, per ogni gruppo e periodo:
#   atteso simulato = somma delle estrazioni dei coefficienti (incertezza di stima)
#                     x (1 + errore di livello dell'anno)
#                     + rumore di Poisson sovradisperso (phi per strato);
#   l'errore di livello dell'anno è la parte della variabilità tra anni che il
#   modello non spiega, misurata nella validazione leave-one-year-out sugli stessi
#   giorni di calendario (solo giorni non caldi), al netto del rumore di Poisson.
#   Si stima a livello nazionale (gruppi nazionali) e regionale (regioni,
#   province, comuni).
#
# Gruppi di comuni (classe d'ampiezza, grado di urbanizzazione, zona altimetrica,
# grandi città): l'atteso di un comune è l'atteso dello strato provinciale
# (età x sesso) per la quota di decessi del comune nello strato, aprile-settembre
# 2023-2025. Classificazioni Istat dei comuni al 1/1/2026.
#
# Variante "calibrata": l'atteso è moltiplicato per il rapporto osservati/attesi
# del 1 aprile-14 maggio 2026 (prima del caldo) per ripartizione e classe d'età.
# Misura quanto cambia l'eccesso se il livello basso di mortalità della
# primavera 2026 fosse proseguito in giugno.
#
# Output (output/): eccesso_italia_periodi.csv, eccesso_italia_giornaliero.csv,
#   eccesso_ripartizioni.csv, eccesso_regioni.csv, eccesso_province.csv,
#   eccesso_eta_sesso.csv, eccesso_comuni_classi.csv, eccesso_citta.csv,
#   validazione_errore_anno.csv

library(data.table)
library(arrow)
library(readxl)

setwd("/Users/lorenzoruffino/Documents/Progetti/data-viz/Mortalità caldo 2026")
set.seed(626)

sim <- readRDS("input/derivati/attesi_sim.rds")
strati <- sim$strati
PERIODI <- sim$periodi
B <- dim(sim$sim_periodo)[3]
np <- nrow(PERIODI)
FINE <- as.IDate("2026-06-30")

comuni <- fread("input/derivati/comuni.csv", encoding = "Latin-1",
                colClasses = list(character = "cod_comune"))
comuni[, `:=`(comune = enc2utf8(comune), provincia = enc2utf8(provincia),
              regione = enc2utf8(regione))]
prov <- unique(comuni[, .(cod_prov, provincia, cod_reg, regione)])
strati <- prov[, .(cod_prov, provincia, regione)][strati, on = "cod_prov"]
strati[, id := .I]

pred <- as.data.table(read_parquet("input/derivati/attesi_strato_giorno.parquet"))
pred[, data := as.IDate(data)]
pred <- strati[, .(cod_prov, eta, sesso, id, cod_reg, rip)][pred, on = .(cod_prov, eta, sesso)]

# ---- Correzione di completezza -------------------------------------------------

fatt <- fread("input/derivati/fattori_completezza.csv")
fatt[, data := as.IDate(data)]
pred <- fatt[, .(cod_reg, data, fattore)][pred, on = .(cod_reg, data)]
stopifnot(!anyNA(pred$fattore))
pred[, oss_c := oss * fattore]

# ---- Errore di livello dell'anno dalla validazione --------------------------------

bt <- as.data.table(read_parquet("input/derivati/backtest_strato_giorno.parquet"))
bt <- bt[startsWith(prova, "loyo")]
bt[, md := format(data, "%m-%d")]
bt <- prov[, .(cod_prov, cod_reg)][bt, on = "cod_prov"]
err_anno <- rbindlist(lapply(seq_len(np), function(p) {
  da <- format(PERIODI$inizio[p], "%m-%d"); a <- format(PERIODI$fine[p], "%m-%d")
  x <- bt[md >= da & md <= a & !escluso]
  naz <- x[, .(e = sum(oss) / sum(atteso) - 1, vp = sum(phi * atteso) / sum(atteso)^2), by = anno]
  reg <- x[, .(e = sum(oss) / sum(atteso) - 1, vp = sum(phi * atteso) / sum(atteso)^2),
           by = .(anno, cod_reg)]
  reg <- reg[, .(v = var(e) - mean(vp)), by = cod_reg]
  data.table(periodo = PERIODI$periodo[p],
             distorsione_media = mean(naz$e),
             sd_osservata_naz = sd(naz$e),
             sigma_naz = sqrt(max(0, var(naz$e) - mean(naz$vp))),
             sigma_reg = max(sqrt(max(0, var(naz$e) - mean(naz$vp))), sqrt(max(0, median(reg$v)))))
}))
fwrite(err_anno, "output/validazione_errore_anno.csv")
cat("Errore di livello dell'anno (relativo):\n"); print(err_anno, digits = 3)

# ---- Funzione di sintesi ----------------------------------------------------------

eps <- rnorm(B)  # errore comune di livello (stesso per tutti i gruppi in una estrazione)

# w: pesi sugli strati (lunghezza nrow(strati)); oss: osservati (corretti e no);
# p: indice del periodo; livello: "naz" o "reg"; k: fattore di calibrazione per strato
sintesi <- function(w, oss_c, oss_r, p, livello, k = NULL) {
  mu_p <- mu_pt[, p]
  if (!is.null(k)) w <- w * k
  E <- sum(w * mu_p)
  Eb <- drop(crossprod(w, sim$sim_periodo[, p, ]))
  vn <- sum(w * phi_s * mu_p)
  sig <- err_anno[p, if (livello == "naz") sigma_naz else sigma_reg]
  yb <- Eb * (1 + sig * eps) + sqrt(vn) * rnorm(B)
  q <- quantile(yb, c(0.025, 0.975), names = FALSE)
  data.table(periodo = PERIODI$periodo[p], oss = oss_c, oss_grezzi = oss_r,
             atteso = E, eccesso = oss_c - E, ecc_inf = oss_c - q[2], ecc_sup = oss_c - q[1],
             ecc_perc = 100 * (oss_c / E - 1),
             ecc_perc_inf = 100 * ((oss_c - q[2]) / E), ecc_perc_sup = 100 * ((oss_c - q[1]) / E),
             eccesso_grezzi = oss_r - E,
             p_valore = mean(yb >= oss_c))
}

# atteso puntuale e phi per strato e periodo
pred[, (PERIODI$periodo) := lapply(seq_len(np), function(p) data %between% c(PERIODI$inizio[p], PERIODI$fine[p]))]
mu_pt <- as.matrix(pred[, lapply(PERIODI$periodo, function(pp) sum(atteso[get(pp)])), keyby = id][, -1])
phi_s <- pred[, .(phi = phi[1]), keyby = id]$phi
oss_pt  <- as.matrix(pred[, lapply(PERIODI$periodo, function(pp) sum(oss_c[get(pp)])), keyby = id][, -1])
ossr_pt <- as.matrix(pred[, lapply(PERIODI$periodo, function(pp) sum(oss[get(pp)])), keyby = id][, -1])
colnames(mu_pt) <- colnames(oss_pt) <- colnames(ossr_pt) <- PERIODI$periodo

# calibrazione sulla primavera (1 aprile-14 maggio) per ripartizione x età
cal <- strati[, .(id, rip, eta)]
cal[, `:=`(o = oss_pt[id, 1], e = mu_pt[id, 1])]
cal[, k := sum(o) / sum(e), by = .(rip, eta)]
k_cal <- cal[order(id)]$k
cat("\nFattori di calibrazione (O/E 1 aprile-14 maggio, osservati corretti):\n")
print(dcast(unique(cal[, .(rip, eta, k = round(k, 3))]), rip ~ eta, value.var = "k"))

per_gruppi <- function(tab, by_cols, livello, calibra = FALSE) {
  # tab: strati con colonne di raggruppamento; restituisce tutti i periodi
  gr <- unique(tab[, ..by_cols])
  rbindlist(lapply(seq_len(nrow(gr)), function(g) {
    sel <- tab[gr[g], on = by_cols, id]
    w <- numeric(nrow(strati)); w[sel] <- 1
    out <- rbindlist(lapply(seq_len(np), function(p)
      sintesi(w, sum(oss_pt[sel, p]), sum(ossr_pt[sel, p]), p, livello,
              if (calibra) k_cal else NULL)))
    cbind(gr[g], out)
  }))
}

arrot <- function(x) {
  num <- names(x)[sapply(x, is.double)]
  x[, (num) := lapply(.SD, round, 1), .SDcols = num][]
}

# ---- Italia -------------------------------------------------------------------------

strati[, Italia := "Italia"]
ita <- per_gruppi(strati, "Italia", "naz")
ita_cal <- per_gruppi(strati, "Italia", "naz", calibra = TRUE)
ita <- rbind(ita[, baseline := "trend"], ita_cal[, baseline := "calibrata primavera 2026"])
fwrite(arrot(ita), "output/eccesso_italia_periodi.csv")
cat("\nItalia:\n")
print(ita[, .(baseline, periodo, oss = round(oss), atteso = round(atteso), eccesso = round(eccesso),
              inf = round(ecc_inf), sup = round(ecc_sup), perc = round(ecc_perc, 1),
              grezzi = round(eccesso_grezzi))])

# ---- Giornaliero nazionale ------------------------------------------------------------

giorni <- sim$giorni
gg <- pred[, .(oss = sum(oss_c), oss_grezzi = sum(oss), atteso = sum(atteso),
               vn = sum(phi * atteso)), keyby = data]
stopifnot(all(gg$data == giorni))
sig_g <- err_anno[periodo == "15 maggio-30 giugno", sigma_naz]
Eb <- sim$sim_giorno["Italia", , ]
yb <- Eb * (1 + sig_g * matrix(eps, nrow(Eb), B, byrow = TRUE)) +
  sqrt(gg$vn) * matrix(rnorm(length(Eb)), nrow(Eb))
gg[, `:=`(att_inf = apply(yb, 1, quantile, 0.025), att_sup = apply(yb, 1, quantile, 0.975))]
tp <- as.data.table(read_parquet("input/derivati/giorni_caldi_provincia.parquet"))
tp[, data := as.IDate(data)]
popw <- as.data.table(read_parquet("input/derivati/popolazione_provincia.parquet"))[
  anno == 2026, .(pop = sum(pop)), by = cod_prov]
tn <- popw[tp[year(data) == 2026], on = "cod_prov"][
  , .(tmean = weighted.mean(tmean, pop), tmax = weighted.mean(tmax, pop),
      tmin = weighted.mean(tmin, pop)), keyby = data]
clim <- popw[tp[year(data) %between% c(2011, 2025)], on = "cod_prov"][
  , .(tmean = weighted.mean(tmean, pop)), by = data][
  , .(tmean_clim = mean(tmean)), by = .(md = format(data, "%m-%d"))]
tn[, md := format(data, "%m-%d")]
tn <- clim[tn, on = "md"][, anomalia := tmean - tmean_clim][, md := NULL]
gg <- tn[gg, on = "data"]
gg[, `:=`(eccesso = oss - atteso, vn = NULL)]
fwrite(arrot(gg), "output/eccesso_italia_giornaliero.csv")

# ---- Ripartizioni, regioni, province, età x sesso ---------------------------------------

rip <- per_gruppi(strati, "rip", "reg")
fwrite(arrot(rip), "output/eccesso_ripartizioni.csv")
reg <- per_gruppi(strati, c("cod_reg", "regione"), "reg")
fwrite(arrot(reg), "output/eccesso_regioni.csv")
prv <- per_gruppi(strati, c("cod_prov", "provincia", "regione"), "reg")
fwrite(arrot(prv), "output/eccesso_province.csv")

strati[, `:=`(tutte = "tutte", entrambi = "entrambi")]
es <- rbind(per_gruppi(strati, c("eta", "sesso"), "naz"),
            setnames(per_gruppi(strati, c("eta", "entrambi"), "naz"), "entrambi", "sesso"),
            setnames(per_gruppi(strati, c("tutte", "sesso"), "naz"), "tutte", "eta"))
fwrite(arrot(es), "output/eccesso_eta_sesso.csv")
cat("\nEtà x sesso, 17-30 giugno:\n")
print(es[periodo == "17-30 giugno", .(eta, sesso, oss = round(oss), atteso = round(atteso),
                                      eccesso = round(eccesso), perc = round(ecc_perc, 1),
                                      inf = round(ecc_perc_inf, 1), sup = round(ecc_perc_sup, 1))])

# ---- Comuni -------------------------------------------------------------------------------

dc <- as.data.table(read_parquet("input/derivati/decessi_comune.parquet"))
dc[, data := as.IDate(data)]
dc <- comuni[, .(cod_comune, cod_prov)][dc, on = "cod_comune"]

# quote di ogni comune nello strato provinciale, aprile-settembre 2023-2025
base <- dc[year(data) %in% 2023:2025 & month(data) %between% c(4, 9),
           .(n = sum(n)), by = .(cod_comune, cod_prov, eta, sesso)]
base[, quota := n / sum(n), by = .(cod_prov, eta, sesso)]
base <- strati[, .(cod_prov, eta, sesso, id)][base, on = .(cod_prov, eta, sesso)]

# osservati 2026 per comune, strato e periodo (corretti con il fattore della regione)
oc <- dc[data >= min(giorni) & data <= FINE]
oc <- comuni[, .(cod_comune, cod_reg)][oc, on = "cod_comune"]
oc <- fatt[, .(cod_reg, data, fattore)][oc, on = .(cod_reg, data)]
stopifnot(!anyNA(oc$fattore))

# classificazioni Istat dei comuni (1/1/2026), riportate ai codici dei decessi
F_CLASS <- "../Referendum_Giustizia_2026/utilities/Classificazioni statistiche-e-dimensione-dei-comuni_01_01_2026.xlsx"
cl <- as.data.table(read_excel(F_CLASS, sheet = 1, col_types = "text"))
cl <- cl[, .(cod_comune = `Codice Istat del Comune \r\n(alfanumerico)`,
             pop = as.numeric(`Popolazione residente al 31/12/2024`),
             zona = as.integer(`Zona altimetrica`),
             degurba = as.integer(`Grado di urbanizzazione 2021`),
             litoraneo = as.integer(`Comune litoraneo`))]
variazioni <- data.table(da = c("018082", "024027", "024071"),
                         a  = c("018094", "024129", "024129"))
cl[variazioni, on = .(cod_comune = da), cod_comune := i.a]
cl <- cl[order(-pop), .(pop = sum(pop), zona = zona[1], degurba = degurba[1],
                        litoraneo = max(litoraneo)), by = cod_comune]
stopifnot(setequal(cl$cod_comune, comuni$cod_comune))
cl[, `:=`(
  ampiezza = cut(pop, c(0, 5e3, 2e4, 5e4, 2.5e5, Inf), right = FALSE,
                 labels = c("meno di 5mila", "5-20mila", "20-50mila", "50-250mila",
                            "250mila e oltre")),
  urbanizzazione = c("città", "piccole città e sobborghi", "zone rurali")[degurba],
  altimetria = fcase(zona %in% 1:2, "montagna", zona %in% 3:4, "collina", zona == 5, "pianura"),
  costa = ifelse(litoraneo == 1, "litoraneo", "non litoraneo"))]

CITTA <- c("001272" = "Torino", "015146" = "Milano", "010025" = "Genova",
           "027042" = "Venezia", "023091" = "Verona", "028060" = "Padova",
           "032006" = "Trieste", "037006" = "Bologna", "048017" = "Firenze",
           "058091" = "Roma", "063049" = "Napoli", "072006" = "Bari",
           "082053" = "Palermo", "087015" = "Catania", "083048" = "Messina",
           "118006" = "Cagliari", "017029" = "Brescia", "003106" = "Novara",
           "054039" = "Perugia", "059011" = "Latina")
cl[, citta := CITTA[cod_comune]]

oss_comuni <- function(cods, eta_sel = NULL) {
  x <- oc[cod_comune %in% cods & (is.null(eta_sel) | eta %in% eta_sel)]
  sapply(seq_len(np), function(p) {
    y <- x[data %between% c(PERIODI$inizio[p], PERIODI$fine[p])]
    c(sum(y$n * y$fattore), sum(y$n))
  })
}

per_comuni <- function(var, eta_sel = NULL, livello = "naz") {
  valori <- cl[!is.na(get(var)), unique(get(var))]
  rbindlist(lapply(valori, function(v) {
    cods <- cl[get(var) == v, cod_comune]
    q <- base[cod_comune %in% cods & (is.null(eta_sel) | eta %in% eta_sel),
              .(q = sum(quota)), by = id]
    w <- numeric(nrow(strati)); w[q$id] <- q$q
    o <- oss_comuni(cods, eta_sel)
    out <- rbindlist(lapply(seq_len(np), function(p) sintesi(w, o[1, p], o[2, p], p, livello)))
    cbind(data.table(variabile = var, gruppo = v, n_comuni = length(cods),
                     pop = cl[cod_comune %in% cods, sum(pop)]), out)
  }))
}

cc <- rbindlist(lapply(c("ampiezza", "urbanizzazione", "altimetria", "costa"), per_comuni))
fwrite(arrot(cc), "output/eccesso_comuni_classi.csv")
cat("\nClassi di comuni, 17-30 giugno:\n")
print(cc[periodo == "17-30 giugno", .(variabile, gruppo, atteso = round(atteso),
                                      eccesso = round(eccesso), perc = round(ecc_perc, 1),
                                      inf = round(ecc_perc_inf, 1), sup = round(ecc_perc_sup, 1))])

citta <- rbind(per_comuni("citta", livello = "reg")[, eta := "tutte"],
               per_comuni("citta", eta_sel = c("65-74", "75-84", "85+"), livello = "reg")[, eta := "65+"])

# confronto con una baseline alla SiSMG: media degli stessi giorni 2016-2025
# (osservati grezzi, anni COVID compresi), solo 65+
naive <- rbindlist(lapply(seq_len(np), function(p) {
  md0 <- format(PERIODI$inizio[p], "%m-%d"); md1 <- format(PERIODI$fine[p], "%m-%d")
  x <- dc[cod_comune %in% names(CITTA) & eta != "0-64" & year(data) %between% c(2016, 2025) &
            format(data, "%m-%d") >= md0 & format(data, "%m-%d") <= md1]
  x[, .(atteso_media_2016_2025 = sum(n) / 10), by = .(gruppo = CITTA[cod_comune])][
    , periodo := PERIODI$periodo[p]]
}))
citta <- naive[, eta := "65+"][citta, on = .(gruppo, periodo, eta)]
citta[, eccesso_media_2016_2025 := oss_grezzi - atteso_media_2016_2025]
fwrite(arrot(citta), "output/eccesso_citta.csv")
cat("\nCittà, 65+, 18-30 giugno e 25 maggio-30 giugno (finestre SiSMG):\n")
print(citta[eta == "65+" & periodo %in% c("18-30 giugno", "25 maggio-30 giugno"),
            .(gruppo, periodo, oss = round(oss), atteso = round(atteso), eccesso = round(eccesso),
              perc = round(ecc_perc, 1), inf = round(ecc_perc_inf, 1), sup = round(ecc_perc_sup, 1),
              media1625 = round(atteso_media_2016_2025),
              perc_media = round(100 * (oss_grezzi / atteso_media_2016_2025 - 1), 1))])
