# Mortalità attesa "senza caldo anomalo" per provincia x età x sesso x giorno,
# 1 aprile - 30 giugno 2026, con intervalli di previsione.
#
# Modello (uno per strato provincia x classe d'età x sesso, 880 strati):
#   decessi ~ quasi-Poisson, log E[y] = log(pop) + a + b*t + c*post_covid
#                                       + armoniche annuali (k = 1, 2) + giorno della settimana
#   - t in anni (trend di lungo periodo), post_covid = 1 dal 2023 (scalino di livello
#     dopo la pandemia: la mortalità per età del 2023-25 non è sulla retta del 2014-19);
#   - offset popolazione per strato se è disponibile popolazione_provincia.parquet,
#     altrimenti nessun offset (il trend assorbe invecchiamento e crescita della popolazione).
#   - Stima su 1 aprile - 30 settembre degli anni 2014-2019 e 2023-2025:
#     esclusi 2020-2022 (COVID) e i mesi invernali (influenza);
#   - esclusi i giorni di caldo anomalo della provincia e i 3 giorni successivi.
#     Giorno caldo: tmean sopra il 90° percentile provinciale dei giorni dell'anno
#     entro ±7 giorni, anni 2011-2025. Per 2014-2019 le temperature coprono solo
#     maggio-agosto: aprile e settembre di quegli anni restano tutti nella stima.
#   L'atteso è quindi quello di una primavera-estate "senza caldo anomalo".
#
# Incertezza: B estrazioni dei coefficienti da N(beta, V) per strato (incertezza di
# stima) e, a valle, rumore di Poisson sovradisperso (phi >= 1) sugli osservati.
# Le estrazioni dell'atteso sono salvate già sommate per periodo (per strato) e per
# giorno (per gruppi nazionali), così ogni aggregazione successiva è una somma.
#
# Validazione (stesso modello, stesse esclusioni):
#   - leave-one-year-out: per ogni anno 2014-2019 e 2023-2025 stima sugli altri
#     anni e previsione dell'anno escluso, 1 aprile - 30 settembre;
#   - previsione "in avanti": 2024 stimato fino al 2023, 2025 fino al 2024.
#
# Output (input/derivati/):
#   - giorni_caldi_provincia.parquet  cod_prov x data -> soglia, caldo, escluso
#   - attesi_strato_giorno.parquet    strato x giorno 2026 -> oss, atteso, phi
#   - attesi_sim.rds                  estrazioni per periodo e giornaliere
#   - backtest_strato_giorno.parquet  strato x giorno x prova -> oss, atteso, var
#   - parametri_strato.csv            phi e numero di giorni di stima per strato

library(data.table)
library(arrow)
library(MASS)

setwd("/Users/lorenzoruffino/Documents/Progetti/data-viz/Mortalità caldo 2026")
set.seed(2026)

B          <- 1000L
ANNI_STIMA <- c(2014:2019, 2023:2025)
INIZIO_STAG <- 4L   # aprile
FINE_STAG   <- 9L   # settembre
GIORNI_2026 <- seq(as.IDate("2026-04-01"), as.IDate("2026-06-30"), by = 1)

PERIODI <- data.table(
  periodo = c("1 aprile-14 maggio", "15-20 maggio", "21-29 maggio", "30-31 maggio",
              "1-16 giugno", "17-30 giugno", "giugno", "15 maggio-30 giugno",
              "25 maggio-30 giugno", "18-30 giugno"),
  inizio = as.IDate(c("2026-04-01", "2026-05-15", "2026-05-21", "2026-05-30",
                      "2026-06-01", "2026-06-17", "2026-06-01", "2026-05-15",
                      "2026-05-25", "2026-06-18")),
  fine   = as.IDate(c("2026-05-14", "2026-05-20", "2026-05-29", "2026-05-31",
                      "2026-06-16", "2026-06-30", "2026-06-30", "2026-06-30",
                      "2026-06-30", "2026-06-30")))

# ---- Dati ---------------------------------------------------------------------

dec <- as.data.table(read_parquet("input/derivati/decessi_provincia.parquet"))
dec[, data := as.IDate(data)]
comuni <- fread("input/derivati/comuni.csv", encoding = "Latin-1",
                colClasses = list(character = "cod_comune"))
prov <- unique(comuni[, .(cod_prov, provincia, cod_reg, regione)])
stopifnot(nrow(prov) == 110)

dec <- dec[month(data) >= INIZIO_STAG & month(data) <= FINE_STAG]
dec[, anno := year(data)]
dec <- dec[anno %in% c(ANNI_STIMA, 2026)]

# popolazione per strato (se disponibile): interpolazione lineare giornaliera tra
# i valori al 1° gennaio, estrapolazione con l'ultima variazione annua
F_POP <- "input/derivati/popolazione_provincia.parquet"
CON_POP <- file.exists(F_POP)
if (CON_POP) {
  pop <- as.data.table(read_parquet(F_POP))
  pop <- pop[, .(pop = as.numeric(sum(pop))), by = .(cod_prov, anno, eta, sesso)]
  stopifnot(setequal(unique(pop$eta), unique(dec$eta)))
  ultimo <- max(pop$anno)
  # punti di appoggio: 1° gennaio di ogni anno, più un anno estrapolato
  ext <- pop[anno >= ultimo - 1][order(anno)][
    , .(anno = ultimo + 1L, pop = pop[2] * pop[2] / pop[1]), by = .(cod_prov, eta, sesso)]
  pop <- rbind(pop, ext)
  pop[, data0 := as.numeric(as.IDate(sprintf("%d-01-01", anno)))]
  setkey(pop, cod_prov, eta, sesso, data0)
  dec[, pop := {
    p <- pop[.(.BY$cod_prov, .BY$eta, .BY$sesso)]
    approx(p$data0, p$pop, xout = as.numeric(data), rule = 2)$y
  }, by = .(cod_prov, eta, sesso)]
  stopifnot(!anyNA(dec$pop), all(dec$pop > 0))
  cat("Offset di popolazione: sì (anni", paste(range(pop$anno), collapse = "-"), ")\n")
} else {
  dec[, pop := 1]
  cat("Offset di popolazione: no (manca", F_POP, ")\n")
}

# ---- Giorni di caldo anomalo per provincia -------------------------------------

tp <- as.data.table(read_parquet("input/derivati/temperatura_provincia.parquet"))
tp[, data := as.IDate(data)]
tp[, `:=`(anno = year(data), doy = yday(data))]
tp[, doy := ifelse(anno %% 4 == 0 & doy > 59, doy - 1L, doy)]  # 29 febbraio -> 28
rif <- tp[anno %between% c(2011, 2025)]
soglie <- rbindlist(lapply(91:273, function(g) {
  rif[abs(doy - g) <= 7, .(doy = g, soglia = quantile(tmean, 0.9)), by = cod_prov]
}))
tp <- soglie[tp, on = .(cod_prov, doy)]
tp[, caldo := !is.na(soglia) & tmean > soglia]
setorder(tp, cod_prov, data)
# escluso: caldo il giorno stesso o in uno dei 3 giorni precedenti (giorni consecutivi)
tp[, escluso := {
  x <- caldo
  r <- x
  for (l in 1:3) {
    cons <- (data - shift(data, l)) == l
    r <- r | (shift(x, l, fill = FALSE) & !is.na(cons) & cons)
  }
  r
}, by = cod_prov]
write_parquet(tp[, .(cod_prov, data, tmean, tmin, tmax, soglia, caldo, escluso)],
              "input/derivati/giorni_caldi_provincia.parquet")

dec <- tp[, .(cod_prov, data, escluso)][dec, on = .(cod_prov, data)]
dec[is.na(escluso), escluso := FALSE]
cat("Quota di giorni-provincia esclusi per caldo, anni di stima:",
    round(dec[anno %in% ANNI_STIMA & eta == "85+" & sesso == "F", mean(escluso)], 3), "\n")

# ---- Covariate -----------------------------------------------------------------

dec[, `:=`(t = as.numeric(data - as.IDate("2020-01-01")) / 365.25,
           post = as.numeric(anno >= 2023),
           dow = factor(wday(data), levels = 1:7))]
w <- 2 * pi * yday(dec$data) / 365.25
dec[, `:=`(s1 = sin(w), c1 = cos(w), s2 = sin(2 * w), c2 = cos(2 * w))]
rm(w)
FORMULA <- n ~ t + post + s1 + c1 + s2 + c2 + dow + offset(log(pop))

setkey(dec, cod_prov, eta, sesso, data)
strati <- unique(dec[, .(cod_prov, eta, sesso)])
cat("Strati:", nrow(strati), "\n")

# stima di uno strato: restituisce coefficienti, vcov e phi
stima_strato <- function(dd) {
  m <- glm(FORMULA, family = quasipoisson(), data = dd)
  phi <- max(1, sum(residuals(m, type = "pearson")^2) / m$df.residual)
  V <- vcov(m) / summary(m)$dispersion * phi
  list(beta = coef(m), V = V, phi = phi, n_giorni = nrow(dd))
}

matrice <- function(dd) model.matrix(update(FORMULA, NULL ~ .), data = dd)

# ---- 1) Modello principale e previsione 2026 ------------------------------------

nd <- length(GIORNI_2026)
np <- nrow(PERIODI)
ind_periodo <- sapply(seq_len(np), function(p)
  GIORNI_2026 >= PERIODI$inizio[p] & GIORNI_2026 <= PERIODI$fine[p])  # nd x np

ripartizione <- function(reg) fcase(reg %in% c(1, 2, 3, 7), "Nord-Ovest",
                                    reg %in% 4:6 | reg == 8, "Nord-Est",
                                    reg %in% 9:12, "Centro",
                                    reg %in% 13:18, "Sud",
                                    default = "Isole")
strati <- prov[, .(cod_prov, cod_reg)][strati, on = "cod_prov"]
strati[, rip := ripartizione(cod_reg)]
gruppi_giorno <- c("Italia", unique(strati$rip), unique(strati$eta))
sim_periodo <- array(0, dim = c(nrow(strati), np, B))
sim_giorno  <- array(0, dim = c(length(gruppi_giorno), nd, B),
                     dimnames = list(gruppi_giorno, as.character(GIORNI_2026), NULL))
pred <- vector("list", nrow(strati))
param <- vector("list", nrow(strati))

for (i in seq_len(nrow(strati))) {
  s <- strati[i]
  dd <- dec[.(s$cod_prov, s$eta, s$sesso)]
  st <- stima_strato(dd[anno %in% ANNI_STIMA & !escluso])
  nw <- dd[anno == 2026 & data %in% GIORNI_2026]
  stopifnot(nrow(nw) == nd)
  X <- matrice(nw)
  off <- log(nw$pop)
  mu <- exp(drop(X %*% st$beta) + off)
  bb <- mvrnorm(B, st$beta, st$V)
  mub <- exp(X %*% t(bb) + off)                       # nd x B
  sim_periodo[i, , ] <- crossprod(ind_periodo, mub)   # np x B
  for (g in c("Italia", s$rip, s$eta)) sim_giorno[g, , ] <- sim_giorno[g, , ] + mub
  pred[[i]] <- data.table(cod_prov = s$cod_prov, eta = s$eta, sesso = s$sesso,
                          data = nw$data, oss = nw$n, atteso = mu, phi = st$phi)
  param[[i]] <- data.table(cod_prov = s$cod_prov, eta = s$eta, sesso = s$sesso,
                           phi = st$phi, n_giorni = st$n_giorni,
                           trend = st$beta[["t"]], post = st$beta[["post"]])
  if (i %% 100 == 0) cat("  strato", i, "\n")
}

pred <- rbindlist(pred)
param <- rbindlist(param)
write_parquet(pred, "input/derivati/attesi_strato_giorno.parquet")
fwrite(param, "input/derivati/parametri_strato.csv")
saveRDS(list(strati = strati, periodi = PERIODI, giorni = GIORNI_2026,
             sim_periodo = sim_periodo, sim_giorno = sim_giorno, con_pop = CON_POP),
        "input/derivati/attesi_sim.rds")

cat("\nItalia, osservati e attesi per periodo:\n")
print(PERIODI[, .(periodo,
                  oss = sapply(seq_len(np), function(p) pred[data %between% c(inizio[p], fine[p]), sum(oss)]),
                  atteso = round(sapply(seq_len(np), function(p) pred[data %between% c(inizio[p], fine[p]), sum(atteso)])))])

# ---- 2) Validazione: leave-one-year-out e previsione in avanti -------------------

prove <- c(lapply(ANNI_STIMA, function(a) list(nome = paste0("loyo_", a), test = a,
                                               stima = setdiff(ANNI_STIMA, a))),
           list(list(nome = "avanti_2024", test = 2024, stima = c(2014:2019, 2023)),
                list(nome = "avanti_2025", test = 2025, stima = c(2014:2019, 2023:2024))))

bt <- vector("list", length(prove) * nrow(strati))
k <- 0
for (pr in prove) {
  cat("Prova", pr$nome, "\n")
  for (i in seq_len(nrow(strati))) {
    s <- strati[i]
    dd <- dec[.(s$cod_prov, s$eta, s$sesso)]
    st <- stima_strato(dd[anno %in% pr$stima & !escluso])
    nw <- dd[anno == pr$test]
    X <- matrice(nw)
    mu <- exp(drop(X %*% st$beta) + log(nw$pop))
    k <- k + 1
    bt[[k]] <- data.table(prova = pr$nome, anno = pr$test, cod_prov = s$cod_prov,
                          eta = s$eta, sesso = s$sesso, data = nw$data, oss = nw$n,
                          atteso = mu, phi = st$phi, escluso = nw$escluso)
  }
}
bt <- rbindlist(bt)
write_parquet(bt, "input/derivati/backtest_strato_giorno.parquet")

cat("\nValidazione, Italia 15 maggio-30 giugno (O/E):\n")
print(bt[format(data, "%m-%d") >= "05-15" & format(data, "%m-%d") <= "06-30",
         .(oss = sum(oss), atteso = round(sum(atteso)), oe = round(sum(oss) / sum(atteso), 3),
           oe_giorni_non_caldi = round(sum(oss[!escluso]) / sum(atteso[!escluso]), 3)),
         by = prova])
