# Validazione e diagnostica del modello DLNM.
#   1. stime per le estati 2022-2024 (giugno-agosto) e 2025, da confrontare con
#      la letteratura;
#   2. residui, sovradispersione, autocorrelazione; osservati e predetti per la
#      serie nazionale 2026 (nel campione e fuori campione: modello stimato sul
#      2011-2025 e proiettato sul 2026 con l'effetto anno del 2025);
#   3. sensibilità della stima di giugno 2026 e 15 maggio-30 giugno;
#   4. harvesting: lag 0-3 contro 0-21 e scomposizione dei decessi attribuibili
#      per giorno di esposizione (quanto del picco di fine maggio è "restituito"
#      a inizio giugno).
#
# Output: output/validazione_*.csv, output/harvesting_*.csv, output/diagnostica/*.png

source("/Users/lorenzoruffino/Documents/Progetti/data-viz/Mortalità caldo 2026/script/dlnm_funzioni.R")
suppressPackageStartupMessages(library(ggplot2))

ana  <- carica_anagrafica()
dec  <- carica_decessi()
temp <- carica_temperature()
tab  <- fread(file.path(DIR_OUT, "attribuibili_periodi.csv"))
sens <- fread(file.path(DIR_OUT, "attribuibili_sensibilita.csv"))

tema <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"))
salva <- function(g, f, w = 9, h = 5.5) ggsave(file.path(DIR_DIAG, f), g, width = w, height = h, dpi = 150, bg = "white")

# ---- 1. Validazione 2022-2025 --------------------------------------------------

lett <- data.table(
  periodo = c("2022 giu-ago", "2022 giu-ago", "2023 giu-ago", "2023 giu-ago", "2024 giu-ago"),
  fonte = c("Ballester et al. 2023 (30 mag-4 set, settimanali)",
            "Janos et al. 2025 (giu-set, giornalieri)",
            "Gallo et al. 2024 (giu-set, settimanali)",
            "Janos et al. 2025 (giu-set, giornalieri)",
            "Janos et al. 2025 (giu-set, giornalieri)"),
  stima_letteratura = c(18010, 18801, 12743, 13790, 19038))
val <- sens[tipo == "caldo" & periodo %in% c("2022 giu-ago", "2023 giu-ago", "2024 giu-ago",
                                             "2025 giu-ago", "2025 giugno", "2025 luglio",
                                             "2019 giu-ago") &
              specifica %in% c("principale", "lag10", "tv_lin", "tv_post", "covid_20_22", "senza_2026"),
            .(specifica, periodo, decessi, an, an_lo, an_hi)]
val <- merge(val, lett, by = "periodo", all.x = TRUE, allow.cartesian = TRUE)
fwrite(val, file.path(DIR_OUT, "validazione_estati.csv"))
cat("\n== Validazione estati (giugno-agosto) ==\n")
print(val[specifica == "principale"], digits = 5)
print(dcast(val, specifica ~ periodo, value.var = "an", fun.aggregate = function(x) round(x[1])))

# ---- 2. Residui e predetti -------------------------------------------------

ps <- readRDS(file.path(DIR_DLNM, "primo_stadio_principale.rds"))
dg <- rbindlist(lapply(ps$prov, function(r) data.table(cod_prov = r$cod_prov, dispersione = r$disp,
                                                       nobs = r$nobs, decessi = r$ndec)))
fit <- rbindlist(lapply(ps$prov, `[[`, "fitted"))
pacf_m <- do.call(rbind, lapply(ps$prov, `[[`, "pacf"))
diag_tab <- data.table(
  indicatore = c("dispersione mediana", "dispersione min", "dispersione max",
                 "PACF lag1 mediana", "PACF lag1 max", "PACF lag7 mediana"),
  valore = c(median(dg$dispersione), min(dg$dispersione), max(dg$dispersione),
             median(pacf_m[, 1]), max(pacf_m[, 1]), median(pacf_m[, 7])))
print(diag_tab)
fwrite(diag_tab, file.path(DIR_OUT, "validazione_residui.csv"))

g <- ggplot(dg, aes(dispersione)) + geom_histogram(bins = 30, fill = "grey40") + tema +
  labs(title = "Parametro di dispersione quasi-Poisson per provincia", x = NULL, y = "province")
salva(g, "dispersione_province.png", 7, 4)
pm <- data.table(lag = 1:ncol(pacf_m), mediana = apply(pacf_m, 2, median),
                 q10 = apply(pacf_m, 2, quantile, .1), q90 = apply(pacf_m, 2, quantile, .9))
g <- ggplot(pm, aes(lag, mediana)) + geom_hline(yintercept = 0) +
  geom_linerange(aes(ymin = q10, ymax = q90), colour = "grey60") + geom_point() + tema +
  labs(title = "Autocorrelazione parziale dei residui di devianza",
       subtitle = "mediana e 10°-90° percentile tra le province", x = "lag (giorni)", y = "PACF")
salva(g, "pacf_residui.png", 7, 4)

# residui nazionali per giorno (osservati - predetti) per anno
naz <- fit[, .(n = sum(n), fitted = sum(fitted)), by = data]
naz[, `:=`(anno = year(data), res_rel = n / fitted - 1)]
g <- ggplot(naz, aes(as.Date(paste0("2000-", format(data, "%m-%d"))), 100 * res_rel)) +
  geom_hline(yintercept = 0) + geom_line(linewidth = .3) + facet_wrap(~anno, ncol = 3) + tema +
  scale_x_date(date_labels = "%b") +
  labs(title = "Residui della serie nazionale: osservati / predetti - 1 (%)", x = NULL, y = "%")
salva(g, "residui_nazionali_per_anno.png", 10, 8)
tn <- merge(temp, dec[year(data) %in% 2023:2025, .(w = sum(n)), by = cod_prov], by = "cod_prov")[
  , .(tmean = weighted.mean(tmean, w)), by = data]
naz <- merge(naz, tn, by = "data")
g <- ggplot(naz, aes(tmean, 100 * res_rel)) + geom_hline(yintercept = 0) +
  geom_point(alpha = .2, size = .6) + geom_smooth(method = "loess", se = FALSE, colour = "firebrick") + tema +
  labs(title = "Residui nazionali contro la temperatura media nazionale", x = "°C", y = "%")
salva(g, "residui_vs_temperatura.png", 7, 4.5)

# Fuori campione: modello senza 2026, predizione di maggio-giugno 2026 con
# l'effetto anno del 2025
spec_oos <- SPEC_SENS$senza_2026
dec_tot <- decessi_strato(dec)
pred_oos <- rbindlist(mclapply(sort(unique(dec_tot$cod_prov)), function(p) {
  tp <- temp[cod_prov == p]
  dp <- dec_tot[cod_prov == p, .(data, n)]
  ds <- dati_stima_prov(dp, tp, spec_oos)
  argvar <- argvar_prov(tp, spec_oos$tvar, spec_oos$pct_var)
  arglag <- arglag_spec(spec_oos)
  cb <- crossbasis(ds$X, lag = spec_oos$lag, argvar = argvar, arglag = arglag)
  d <- ds$d; S <- base_stagione(d, spec_oos)
  mod <- glm(d$n ~ cb + d$anno + S + d$dow + d$fest, family = quasipoisson())
  nd <- dp[data >= as.IDate("2026-05-01") & data <= DATA_FINE]
  X2 <- matrice_lag(tp, spec_oos$tvar, nd$data, spec_oos$lag)
  cb2 <- crossbasis(X2, lag = spec_oos$lag, argvar = argvar, arglag = arglag)
  # stessa base della stima (nodi calcolati su maggio-agosto, non su maggio-giugno)
  S2 <- predict(S, giorno_stagione(nd$data))
  colnames(S2) <- colnames(S)
  b <- coef(mod)
  dw <- wday(nd$data)
  lp <- b[1] + unclass(cb2) %*% b[grep("^cb", names(b))] + b["d$anno2025"] +
    S2 %*% b[paste0("S", colnames(S2))] +
    ifelse(dw == 1, 0, b[paste0("d$dow", dw)]) + festivi(nd$data) * b["d$fest"]
  data.table(cod_prov = p, data = nd$data, pred_oos = exp(drop(lp)))
}, mc.cores = N_CORE))
gi <- fread(file.path(DIR_OUT, "attribuibili_giornalieri_italia.csv"))
gi[, data := as.IDate(data)]
if ("predetti_fuori_campione" %in% names(gi)) gi[, predetti_fuori_campione := NULL]
gi <- merge(gi, pred_oos[, .(predetti_fuori_campione = sum(pred_oos)), by = data], by = "data")
fwrite(gi, file.path(DIR_OUT, "attribuibili_giornalieri_italia.csv"))
cmp <- gi[, .(osservati = sum(osservati), predetti = sum(predetti),
              predetti_fuori_campione = sum(predetti_fuori_campione)),
          by = .(periodo = fcase(data < as.IDate("2026-05-15"), "1-14 mag",
                                 data <= as.IDate("2026-05-31"), "15-31 mag",
                                 data <= as.IDate("2026-06-16"), "1-16 giu", default = "17-30 giu"))]
print(cmp)
fwrite(cmp, file.path(DIR_OUT, "validazione_predetti_2026.csv"))
cat("Correlazione giornaliera osservati-predetti (fuori campione):",
    round(cor(gi$osservati, gi$predetti_fuori_campione), 3), "\n")

gl <- melt(gi[, .(data, osservati, `predetti (nel campione)` = predetti,
                  `predetti (fuori campione)` = predetti_fuori_campione,
                  `attesi senza caldo` = attesi_senza_caldo,
                  `attesi con clima normale` = attesi_clima_normale)], id.vars = "data")
g <- ggplot(gl, aes(as.Date(data), value, colour = variable, linewidth = variable)) + geom_line() +
  scale_colour_manual(values = c("black", "steelblue", "darkorange", "firebrick", "darkgreen")) +
  scale_linewidth_manual(values = c(.7, .5, .5, .4, .4)) + tema +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  labs(title = "Decessi giornalieri in Italia, maggio-giugno 2026",
       subtitle = "osservati, predetti dal modello e attesi negli scenari controfattuali", x = NULL, y = NULL)
salva(g, "osservati_predetti_2026.png", 10, 5.5)

# ---- Curve ------------------------------------------------------------------

cu <- fread(file.path(DIR_OUT, "curve_esposizione_risposta.csv"))
g <- ggplot(cu[spec == "principale"], aes(temperatura, rr)) +
  geom_hline(yintercept = 1) + geom_ribbon(aes(ymin = rr_lo, ymax = rr_hi), alpha = .2) +
  geom_line() + facet_wrap(~area) + tema + coord_cartesian(ylim = c(.8, 2)) +
  labs(title = "Curve esposizione-risposta (cumulate sui lag 0-21)",
       subtitle = "maggio-agosto; predizione della meta-regressione sulla distribuzione media delle temperature",
       x = "temperatura media giornaliera (°C)", y = "rischio relativo")
salva(g, "curve_italia_macroaree.png", 10, 6.5)
g <- ggplot(cu[spec != "principale"], aes(temperatura, rr, colour = periodo, fill = periodo)) +
  geom_hline(yintercept = 1) + geom_ribbon(aes(ymin = rr_lo, ymax = rr_hi), alpha = .15, colour = NA) +
  geom_line() + facet_wrap(~spec) + tema + coord_cartesian(ylim = c(.8, 2.2)) +
  labs(title = "Rischio variabile nel tempo: curva recente e curva passata (Italia)",
       x = "temperatura media giornaliera (°C)", y = "rischio relativo")
salva(g, "curve_rischio_variabile.png", 10, 5)
mmt <- fread(file.path(DIR_OUT, "mmt_provincia.csv"))
g <- ggplot(mmt, aes(mmp)) + geom_histogram(binwidth = 5, boundary = 0, fill = "grey40") + tema +
  labs(title = "Percentile della temperatura minima di mortalità (MMP) per provincia", x = "percentile", y = "province")
salva(g, "mmp_province.png", 7, 4)

# ---- 3. Sensibilità ------------------------------------------------------

per_s <- c("2026 15 mag-30 giu", "2026 giugno", "2026 17-30 giu", "2026 21-29 mag")
st <- sens[tipo %in% c("caldo", "anomalia") & periodo %in% per_s]
fwrite(dcast(st, specifica + tipo ~ periodo, value.var = c("an", "an_lo", "an_hi")),
       file.path(DIR_OUT, "validazione_sensibilita.csv"))
st[, periodo := factor(periodo, levels = per_s)]
g <- ggplot(st, aes(an, specifica)) + geom_vline(xintercept = 0) +
  geom_pointrange(aes(xmin = an_lo, xmax = an_hi), size = .2) +
  facet_grid(tipo ~ periodo, scales = "free_x") + tema +
  labs(title = "Sensibilità: decessi attribuibili in Italia", x = "decessi", y = NULL)
salva(g, "sensibilita.png", 12, 8)

# ---- 4. Harvesting -------------------------------------------------------

harv1 <- sens[specifica %in% c("principale", "lag10", "lag3") & tipo == "caldo" &
                periodo %in% c(per_s, "2026 6-14 giu", "2026 30 mag-16 giu", "2026 maggio")]
print(dcast(harv1, periodo ~ specifica, value.var = "an", fun.aggregate = function(x) round(x[1])))

# Scomposizione per giorno di esposizione (specifica principale, BLUP completi):
# il log-RR del giorno t è la somma dei contributi delle esposizioni t-l;
# ciascun contributo riceve la quota corrispondente dei decessi attribuibili
# del giorno t (approssimazione additiva, esatta per effetti piccoli).
mp <- readRDS(file.path(DIR_DLNM, "meta_principale.rds"))
date_m <- seq(as.IDate("2026-05-01"), DATA_FINE, by = 1)
NS <- 1000
sims <- sim_blup_congiunte(mp$m_cb, mp$prov, NS, seed = 101)
# modo "caldo": esposizioni sopra l'MMT rispetto all'MMT;
# modo "anomalia": esposizioni osservate rispetto alla climatologia.
scomponi <- function(modo) {
  contrib <- mclapply(names(mp$prov), function(p) {
    r <- mp$prov[[p]]
    dp <- dec_tot[cod_prov == r$cod_prov][data.table(data = date_m), on = "data"]
    tp <- temp[cod_prov == r$cod_prov]
    X <- matrice_lag(tp, "tmean", date_m, r$lag)
    if (modo == "caldo") {
      X[X < r$mmt] <- r$mmt
      Xr <- matrix(r$mmt, nrow(X), ncol(X))
    } else {
      Xr <- matrice_cf(X, date_m, climatologia(tp, "tmean"))
    }
    coefs <- sims[[p]]
    C <- do.call(onebasis, c(list(x = 0:r$lag), r$arglag))
    nv <- length(r$argvar$knots) + 1; nl <- ncol(C)
    # contributo al log-RR per giorno (righe) e lag, per ogni simulazione
    lp_l <- lapply(0:r$lag, function(l) {
      Bl <- do.call(onebasis, c(list(x = X[, l + 1]), r$argvar)) -
        do.call(onebasis, c(list(x = Xr[, l + 1]), r$argvar))
      D <- Bl[, rep(1:nv, each = nl), drop = FALSE] * matrix(rep(C[l + 1, ], nv), nrow(Bl), nv * nl, byrow = TRUE)
      D %*% coefs
    })
    lp <- Reduce(`+`, lp_l)
    an <- dp$n * (1 - exp(-lp))
    sc <- ifelse(abs(lp) < 1e-10, dp$n, an / lp)
    lapply(0:r$lag, function(l) lp_l[[l + 1]] * sc)
  }, mc.cores = N_CORE)
  # somma sulle province: lista per lag di matrici giorno x simulazione
  Reduce(function(a, b) Map(`+`, a, b), contrib)
}
tot_l <- list(caldo = scomponi("caldo"), anomalia = scomponi("anomalia"))
somma_celle <- function(cond, tot) {
  v <- numeric(NS + 1)
  for (l in 0:21) {
    ii <- which(cond(date_m, date_m - l, l))
    if (length(ii)) v <- v + colSums(tot[[l + 1]][ii, , drop = FALSE])
  }
  v
}
d <- as.IDate
esp_mag <- function(e) e >= d("2026-05-21") & e <= d("2026-05-29")
harv <- rbindlist(lapply(names(tot_l), function(sc) rbindlist(lapply(list(
  list("esposizione 21-29 mag: decessi a lag 0-3", function(t, e, l) esp_mag(e) & l <= 3),
  list("esposizione 21-29 mag: decessi a lag 4-21", function(t, e, l) esp_mag(e) & l >= 4),
  list("esposizione 21-29 mag: totale netto (lag 0-21)", function(t, e, l) esp_mag(e)),
  list("esposizione 21-29 mag: decessi entro il 29 mag", function(t, e, l) esp_mag(e) & t <= d("2026-05-29")),
  list("esposizione 21-29 mag: decessi 30 mag-30 giu", function(t, e, l) esp_mag(e) & t > d("2026-05-29")),
  list("decessi 6-14 giu da esposizioni fino al 29 mag", function(t, e, l) t >= d("2026-06-06") & t <= d("2026-06-14") & e <= d("2026-05-29")),
  list("decessi 6-14 giu da esposizioni 30 mag-14 giu", function(t, e, l) t >= d("2026-06-06") & t <= d("2026-06-14") & e > d("2026-05-29")),
  list("decessi 6-14 giu (tutte le esposizioni)", function(t, e, l) t >= d("2026-06-06") & t <= d("2026-06-14")),
  list("esposizione 17-30 giu: decessi entro il 30 giu", function(t, e, l) e >= d("2026-06-17")),
  list("esposizione 17-30 giu: solo lag 0-3", function(t, e, l) e >= d("2026-06-17") & l <= 3),
  list("decessi 17-30 giu (tutte le esposizioni)", function(t, e, l) t >= d("2026-06-17"))
), function(x) {
  v <- somma_celle(x[[2]], tot_l[[sc]])
  data.table(scenario = sc, voce = x[[1]], an = v[1], an_lo = quantile(v[-1], .025), an_hi = quantile(v[-1], .975))
}))))
print(harv, digits = 4)
fwrite(harv, file.path(DIR_OUT, "harvesting_scomposizione.csv"))
fwrite(harv1, file.path(DIR_OUT, "harvesting_lag.csv"))

# per il grafico: decessi attribuibili per giorno di decesso, distinti per
# finestra di esposizione
fin <- function(e) fcase(e < d("2026-05-21"), "fino al 20 mag", e <= d("2026-05-29"), "21-29 mag",
                         e < d("2026-06-17"), "30 mag-16 giu", default = "17-30 giu")
gd <- rbindlist(lapply(names(tot_l), function(sc) rbindlist(lapply(0:21, function(l)
  data.table(scenario = sc, data = date_m, esposizione = fin(date_m - l), an = tot_l[[sc]][[l + 1]][, 1])))))[
  , .(an = sum(an)), by = .(scenario, data, esposizione)]
gd[, esposizione := factor(esposizione, levels = c("fino al 20 mag", "21-29 mag", "30 mag-16 giu", "17-30 giu"))]
fwrite(gd, file.path(DIR_OUT, "harvesting_giornaliero.csv"))
g <- ggplot(gd[data >= d("2026-05-15")], aes(as.Date(data), an, fill = esposizione)) + geom_hline(yintercept = 0) +
  geom_col(position = "stack", width = 1) + facet_wrap(~scenario, ncol = 1,
    labeller = as_labeller(c(caldo = "sopra l'MMT", anomalia = "rispetto alla climatologia 2011-2025"))) +
  tema + theme(legend.position = "bottom") +
  scale_fill_manual(values = c("grey60", "darkorange", "steelblue", "firebrick")) +
  labs(title = "Decessi attribuibili alla temperatura per giorno di decesso e periodo di esposizione",
       subtitle = "Italia, modello principale (lag 0-21); i valori negativi sono decessi anticipati (harvesting)",
       x = NULL, y = "decessi al giorno", fill = "esposizione")
salva(g, "harvesting_2026.png", 10, 8)

# ---- 5. Osservati contro predetti nei periodi caldi ------------------------------
# Se la curva storica fosse troppo ripida per il 2026 (o il 2025), il modello
# sovrastimerebbe i decessi nei giorni caldi. Rapporto osservati/predetti
# (nel campione) per mese e anno, e "eccesso osservato" rispetto al modello
# senza caldo: osservati - predetti x (1 - AF), con AF la frazione attribuibile
# provinciale del giorno.
om <- fit[year(data) >= 2021, .(osservati = sum(n), predetti = sum(fitted)),
          by = .(anno = year(data), mese = month(data))]
om[, rapporto := osservati / predetti]
print(dcast(om, anno ~ mese, value.var = "rapporto"), digits = 3)
fwrite(om, file.path(DIR_OUT, "validazione_osservati_predetti_mese.csv"))

gp <- fread(file.path(DIR_DLNM, "attribuibili_giornalieri_provincia.csv"))[, data := as.IDate(data)]
ec <- merge(gp, fit[, .(cod_prov, data, fitted)], by = c("cod_prov", "data"))
ec[, af := fifelse(n > 0, an_caldo / n, 0)]
ec <- ec[, .(osservati = sum(n), predetti = sum(fitted),
             attesi_modello_senza_caldo = sum(fitted * (1 - af)), an_caldo = sum(an_caldo)),
         by = .(periodo = fcase(data < as.IDate("2026-05-15"), "1-14 mag",
                                data <= as.IDate("2026-05-20"), "15-20 mag",
                                data <= as.IDate("2026-05-29"), "21-29 mag",
                                data <= as.IDate("2026-06-16"), "30 mag-16 giu",
                                default = "17-30 giu"))]
ec[, eccesso_osservato := osservati - attesi_modello_senza_caldo]
print(ec)
fwrite(ec, file.path(DIR_OUT, "validazione_eccesso_osservato_2026.csv"))
