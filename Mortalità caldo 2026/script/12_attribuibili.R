# Morti attribuibili alla temperatura, prospettiva all'indietro (backward,
# come attrdl di Gasparrini e Leone 2014, ma con la crossbasis completa): i
# decessi del giorno t attribuibili alle esposizioni dei giorni t, ..., t-L.
#   - caldo:     esposizioni sopra l'MMT provinciale, rispetto all'MMT;
#   - estremo:   esposizioni sopra il 97,5° percentile, rispetto all'MMT;
#   - anomalia:  rispetto a uno scenario in cui ogni giorno di maggio-agosto ha
#                la temperatura climatologica 2011-2025 di quel giorno
#                (media per provincia e giorno della stagione lisciata con
#                ns a 6 gradi di libertà); i giorni fuori da maggio-agosto
#                (aprile nei lag) restano osservati. È l'effetto netto
#                dell'anomalia: comprende i giorni più freschi del normale;
#   - anomalia_calda: come sopra, ma solo i giorni più caldi della climatologia
#                (nei giorni più freschi lo scenario coincide con l'osservato).
# Intervalli empirici (eCI 95%) da 1.000 simulazioni Monte Carlo congiunte dei
# BLUP provinciali (coefficienti fissi della meta-regressione comuni a tutte le
# province, effetti casuali provinciali condizionati ai dati: vedi
# sim_blup_congiunte in dlnm_funzioni.R). Con simulazioni indipendenti per
# provincia, come spesso nella rete MCC, l'intervallo nazionale risulterebbe
# molto più stretto.
#
# In più, per la specifica principale, la prospettiva in avanti (forward) con i
# BLUP della curva cumulata (come attrdl con coefficienti ridotti): attribuisce
# i decessi al giorno di esposizione. Per le esposizioni degli ultimi 21 giorni
# di giugno 2026 i decessi futuri sono solo in parte disponibili (media sui
# giorni disponibili).
#
# Output: input/derivati/dlnm/attr_<nome>.rds (simulazioni per periodo e provincia)
#         output/attribuibili_periodi.csv, output/attribuibili_giornalieri_italia.csv,
#         output/attribuibili_regioni.csv, output/attribuibili_sensibilita.csv

source("/Users/lorenzoruffino/Documents/Progetti/data-viz/Mortalità caldo 2026/script/dlnm_funzioni.R")

NSIM <- 1000
ana  <- carica_anagrafica()
dec  <- carica_decessi()
temp <- carica_temperature()

# ---- Periodi ----------------------------------------------------------------

rng <- function(a, b) seq(as.IDate(a), as.IDate(b), by = 1)
periodi <- rbindlist(list(
  data.table(periodo = "2026 15 mag-30 giu", data = rng("2026-05-15", "2026-06-30")),
  data.table(periodo = "2026 maggio",        data = rng("2026-05-01", "2026-05-31")),
  data.table(periodo = "2026 giugno",        data = rng("2026-06-01", "2026-06-30")),
  data.table(periodo = "2026 21-29 mag",     data = rng("2026-05-21", "2026-05-29")),
  data.table(periodo = "2026 30 mag-16 giu", data = rng("2026-05-30", "2026-06-16")),
  data.table(periodo = "2026 6-14 giu",      data = rng("2026-06-06", "2026-06-14")),
  data.table(periodo = "2026 17-30 giu",     data = rng("2026-06-17", "2026-06-30")),
  data.table(periodo = "2026 15-28 giu",     data = rng("2026-06-15", "2026-06-28")),
  data.table(periodo = "2026 25 mag-30 giu", data = rng("2026-05-25", "2026-06-30")),
  data.table(periodo = "2025 giugno",        data = rng("2025-06-01", "2025-06-30")),
  data.table(periodo = "2025 luglio",        data = rng("2025-07-01", "2025-07-31")),
  data.table(periodo = "2025 giu-ago",       data = rng("2025-06-01", "2025-08-31")),
  data.table(periodo = "2024 giu-ago",       data = rng("2024-06-01", "2024-08-31")),
  data.table(periodo = "2023 giu-ago",       data = rng("2023-06-01", "2023-08-31")),
  data.table(periodo = "2022 giu-ago",       data = rng("2022-06-01", "2022-08-31")),
  data.table(periodo = "2024 giugno",        data = rng("2024-06-01", "2024-06-30")),
  data.table(periodo = "2023 giugno",        data = rng("2023-06-01", "2023-06-30")),
  data.table(periodo = "2022 giugno",        data = rng("2022-06-01", "2022-06-30")),
  data.table(periodo = "2019 giugno",        data = rng("2019-06-01", "2019-06-30")),
  data.table(periodo = "2019 giu-ago",       data = rng("2019-06-01", "2019-08-31"))
))
ORDINE_PERIODI <- unique(periodi$periodo)
date_target <- sort(unique(c(periodi$data, rng("2026-05-01", "2026-06-30"))))
date_giorn  <- rng("2026-05-01", "2026-06-30")   # serie giornaliera nazionale

# Completamento dei decessi di giugno 2026 (sensibilità): fattore nazionale per
# giorno (output/qualita_fattori_giugno2026.csv) con la revisione regionale
# scalata dal moltiplicatore della regione (output/qualita_fattori_regione.csv):
# fattore = 1 + (fattore nazionale - 1) x moltiplicatore. Maggio non è corretto.
fatt_g <- fread(file.path(DIR_OUT, "qualita_fattori_giugno2026.csv"))[, .(data = as.IDate(data), fattore)]
fatt_r <- fread(file.path(DIR_OUT, "qualita_fattori_regione.csv"))[, .(cod_reg, moltiplicatore)]

# ---- Attribuzione per provincia ---------------------------------------------

attr_prov <- function(r, dp, tp, tvar, coefs, completezza = FALSE, forward = FALSE, coefs_red = NULL) {
  p <- r$cod_prov
  set.seed(1000 + p)
  dd <- dp[data.table(data = date_target), on = "data"]
  casi <- dd$n
  if (completezza) {
    reg <- ana[cod_prov == p, cod_reg]
    f <- fatt_g$fattore[match(dd$data, fatt_g$data)]
    f <- ifelse(is.na(f), 1, 1 + (f - 1) * fatt_r[cod_reg == reg, moltiplicatore])
    casi <- casi * f
  }
  X <- matrice_lag(tp, tvar, date_target, r$lag)
  cen <- r$mmt
  p975 <- r$perc[976]
  Xh <- X; Xh[Xh < cen] <- cen
  Xe <- X; Xe[Xe < p975] <- cen
  Xcf <- matrice_cf(X, date_target, climatologia(tp, tvar))
  an <- list(caldo    = an_da_xpred(xpred_back(Xh, r, cen = cen), coefs, casi),
             estremo  = an_da_xpred(xpred_back(Xe, r, cen = cen), coefs, casi),
             anomalia = an_da_xpred(xpred_back(X, r, Xcf = Xcf), coefs, casi),
             anomalia_calda = an_da_xpred(xpred_back(X, r, Xcf = pmin(X, Xcf)), coefs, casi))
  if (forward) {
    # forward con i BLUP della curva cumulata: AF del giorno di esposizione
    # applicata alla media dei decessi dei giorni t..t+L
    cr <- coefs_red
    x0 <- X[, 1]
    tutti <- dp[data.table(data = seq(min(date_target), max(date_target) + r$lag, by = 1)), on = "data"]
    fut <- sapply(seq_along(date_target), function(i) {
      v <- tutti$n[match(date_target[i] + 0:r$lag, tutti$data)]
      mean(v, na.rm = TRUE)
    })
    mmt4 <- r$perc[round(r$mmp_blup4 * 10) + 1]
    Bx <- do.call(onebasis, c(list(x = pmax(x0, mmt4)), r$argvar))
    Bc <- do.call(onebasis, c(list(x = mmt4), r$argvar))
    Xf <- scale(Bx, center = Bc, scale = FALSE)
    an$forward_caldo <- fut * (1 - exp(-(Xf %*% cr)))
  }
  # aggregazione per periodo (righe) x simulazioni (colonne)
  agg <- lapply(an, function(m) {
    t(sapply(ORDINE_PERIODI, function(pp) {
      i <- date_target %in% periodi[periodo == pp, data]
      colSums(m[i, , drop = FALSE], na.rm = TRUE)
    }))
  })
  ig <- match(date_giorn, date_target)
  list(cod_prov = p, periodi = agg,
       casi_periodo = sapply(ORDINE_PERIODI, function(pp) sum(casi[date_target %in% periodi[periodo == pp, data]])),
       giorn = lapply(an[c("caldo", "anomalia", "anomalia_calda", "estremo")], function(m) m[ig, , drop = FALSE]),
       giorn_prov = data.table(cod_prov = p, data = date_giorn, n = casi[ig],
                               an_caldo = an$caldo[ig, 1], an_anomalia = an$anomalia[ig, 1],
                               an_anomalia_calda = an$anomalia_calda[ig, 1],
                               an_estremo = an$estremo[ig, 1]))
}

attribuisci <- function(nome, nsim = NSIM, completezza = FALSE, forward = FALSE, etichetta = nome) {
  mt <- readRDS(file.path(DIR_DLNM, sprintf("meta_%s.rds", nome)))
  s <- mt$spec
  dec_s <- decessi_strato(dec, s$eta, s$sesso)
  setkey(dec_s, cod_prov, data)
  sims <- sim_blup_congiunte(mt$m_cb, mt$prov, nsim, seed = 101)
  sims_red <- if (forward) sim_blup_congiunte(mt$m_red, mt$prov, nsim, seed = 102, quale = "red") else NULL
  res <- mclapply(names(mt$prov), function(p) {
    r <- mt$prov[[p]]
    attr_prov(r, dec_s[J(r$cod_prov)], temp[cod_prov == r$cod_prov], s$tvar, sims[[p]],
              completezza, forward, sims_red[[p]])
  }, mc.cores = N_CORE)
  out <- list(nome = etichetta, spec = s, prov = res)
  saveRDS(out, file.path(DIR_DLNM, sprintf("attr_%s.rds", etichetta)))
  out
}

# ---- Sintesi -----------------------------------------------------------------

riassumi <- function(sims, casi = NULL) {
  data.table(an = sims[1], an_lo = quantile(sims[-1], 0.025, names = FALSE),
             an_hi = quantile(sims[-1], 0.975, names = FALSE))
}

# per area (vettore di codici provincia) e periodo
sintesi_area <- function(at, tipi = c("caldo", "estremo", "anomalia", "anomalia_calda")) {
  pr <- at$prov
  cp <- sapply(pr, `[[`, "cod_prov")
  aree <- list(Italia = cp)
  for (a in unique(ana$macroarea)) aree[[a]] <- intersect(cp, ana[macroarea == a, cod_prov])
  for (g in unique(ana$regione)) aree[[g]] <- intersect(cp, ana[regione == g, cod_prov])
  livello <- c("Italia", rep("macroarea", 5), rep("regione", length(unique(ana$regione))))
  rbindlist(lapply(seq_along(aree), function(k) {
    sel <- pr[cp %in% aree[[k]]]
    rbindlist(lapply(intersect(tipi, names(sel[[1]]$periodi)), function(tp) {
      M <- Reduce(`+`, lapply(sel, function(x) x$periodi[[tp]]))
      casi <- Reduce(`+`, lapply(sel, `[[`, "casi_periodo"))
      rbindlist(lapply(ORDINE_PERIODI, function(pp) cbind(
        data.table(strato = at$nome, livello = livello[k], area = names(aree)[k],
                   tipo = tp, periodo = pp, decessi = casi[pp]),
        riassumi(M[pp, ]))))
    }))
  }))
}

# ---- Esecuzione ----------------------------------------------------------------

t0 <- Sys.time()
principali <- c("principale", names(SPEC_STRATI))
ris <- list()
for (nm in principali) {
  at <- attribuisci(nm, forward = nm == "principale")
  ris[[nm]] <- sintesi_area(at, c("caldo", "estremo", "anomalia", "anomalia_calda", "forward_caldo"))
  cat(nm, format(Sys.time() - t0), "\n")
}
# completezza dei dati 2026 (correzione per le registrazioni tardive)
at_c <- attribuisci("principale", completezza = TRUE, etichetta = "principale_completezza")
ris[["principale_completezza"]] <- sintesi_area(at_c)

tab <- rbindlist(ris)
tab[, `:=`(af_pct = 100 * an / decessi, af_pct_lo = 100 * an_lo / decessi, af_pct_hi = 100 * an_hi / decessi)]

# tassi per 100.000 abitanti (popolazione al 1° gennaio dell'anno del periodo)
pop <- as.data.table(read_parquet(file.path(DIR_DER, "popolazione_provincia.parquet")))
pop <- merge(pop, ana[, .(cod_prov, regione, macroarea)], by = "cod_prov")
strato_pop <- list(principale = pop, principale_completezza = pop,
                   eta_0_64 = pop[eta == "0-64"], eta_65_74 = pop[eta == "65-74"],
                   eta_75_84 = pop[eta == "75-84"], eta_85 = pop[eta == "85+"],
                   maschi = pop[sesso == "M"], femmine = pop[sesso == "F"])
pop_area <- rbindlist(lapply(names(strato_pop), function(st) {
  x <- strato_pop[[st]]
  rbind(x[, .(strato = st, area = "Italia", pop = sum(pop)), by = anno],
        x[, .(strato = st, area = macroarea, pop = sum(pop)), by = .(anno, macroarea)][, macroarea := NULL],
        x[, .(strato = st, area = regione, pop = sum(pop)), by = .(anno, regione)][, regione := NULL])
}))
tab[, anno := as.integer(substr(periodo, 1, 4))]
tab <- merge(tab, pop_area, by = c("strato", "area", "anno"), all.x = TRUE)
tab[, `:=`(tasso_100k = 1e5 * an / pop, tasso_100k_lo = 1e5 * an_lo / pop,
           tasso_100k_hi = 1e5 * an_hi / pop)]
tab[, periodo := factor(periodo, levels = ORDINE_PERIODI)]
setorder(tab, strato, tipo, periodo, livello, area)
tab[, periodo := as.character(periodo)]
fwrite(tab, file.path(DIR_OUT, "attribuibili_periodi.csv"))

# tabella regionale compatta (specifica principale, 2026)
reg <- tab[strato == "principale" & livello == "regione" & tipo %in% c("caldo", "anomalia", "anomalia_calda") &
             periodo %in% c("2026 15 mag-30 giu", "2026 giugno", "2026 17-30 giu", "2026 21-29 mag")]
fwrite(reg, file.path(DIR_OUT, "attribuibili_regioni.csv"))

# ---- Serie giornaliera nazionale --------------------------------------------

at <- readRDS(file.path(DIR_DLNM, "attr_principale.rds"))
ps <- readRDS(file.path(DIR_DLNM, "primo_stadio_principale.rds"))
fit <- rbindlist(lapply(ps$prov, `[[`, "fitted"))[data %in% date_giorn,
                                                   .(predetti = sum(fitted)), by = data]
gi <- rbindlist(lapply(c("caldo", "anomalia", "anomalia_calda", "estremo"), function(tp) {
  M <- Reduce(`+`, lapply(at$prov, function(x) x$giorn[[tp]]))
  data.table(data = date_giorn, tipo = tp, an = M[, 1],
             an_lo = apply(M[, -1], 1, quantile, 0.025),
             an_hi = apply(M[, -1], 1, quantile, 0.975))
}))
gi <- dcast(gi, data ~ tipo, value.var = c("an", "an_lo", "an_hi"))
oss <- rbindlist(lapply(at$prov, `[[`, "giorn_prov"))[, .(osservati = sum(n)), by = data]
tn <- temp[data %in% date_giorn]
w <- dec[year(data) %in% 2023:2025, .(n = sum(n)), by = cod_prov]   # pesi per la temperatura nazionale
tn <- merge(tn, w, by = "cod_prov")[, .(tmean_naz = weighted.mean(tmean, n)), by = data]
gi <- Reduce(function(a, b) merge(a, b, by = "data"), list(oss, fit, gi, tn))
gi[, attesi_senza_caldo := osservati - an_caldo]
gi[, attesi_clima_normale := osservati - an_anomalia]
fwrite(gi, file.path(DIR_OUT, "attribuibili_giornalieri_italia.csv"))
fwrite(rbindlist(lapply(at$prov, `[[`, "giorn_prov")), file.path(DIR_DER, "dlnm", "attribuibili_giornalieri_provincia.csv"))

# ---- Sensibilità --------------------------------------------------------------

sens <- list()
for (nm in names(SPEC_SENS)) {
  a <- attribuisci(nm, nsim = 500)
  sens[[nm]] <- sintesi_area(a)[livello == "Italia"]
}
sens <- rbind(tab[strato %in% c("principale", "principale_completezza") & livello == "Italia" &
                    tipo %in% c("caldo", "estremo", "anomalia", "anomalia_calda"),
                  .(strato, livello, area, tipo, periodo, decessi, an, an_lo, an_hi)],
              rbindlist(sens)[, .(strato, livello, area, tipo, periodo, decessi, an, an_lo, an_hi)])
setnames(sens, "strato", "specifica")
fwrite(sens, file.path(DIR_OUT, "attribuibili_sensibilita.csv"))

cat("tempo totale:", format(Sys.time() - t0), "\n")
print(dcast(sens[tipo == "caldo" & periodo %in% c("2026 15 mag-30 giu", "2026 giugno", "2026 17-30 giu", "2026 21-29 mag", "2024 giu-ago")],
            specifica ~ periodo, value.var = "an", fun.aggregate = function(x) round(sum(x))))
