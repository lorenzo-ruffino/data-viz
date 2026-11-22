# Funzioni comuni dell'analisi DLNM a due stadi (decessi giornalieri e
# temperatura per provincia). Usate da 10_dlnm_primo_stadio.R, 11_dlnm_meta.R,
# 12_attribuibili.R e 13_validazione.R.
#
# Impostazione (Gasparrini et al. 2015, rete MCC), stagione calda maggio-agosto:
#   - primo stadio per provincia: GLM quasi-Poisson con crossbasis sulla
#     temperatura (ns con nodi al 10°, 75°, 90° percentile della distribuzione
#     provinciale di maggio-agosto 2011-2025; lag 0-21 con ns a 3 nodi
#     equidistanti in scala log), effetto anno, stagionalità ns(giorno della
#     stagione), giorno della settimana, festivi (1 maggio, 2 giugno, 15 agosto);
#   - le temperature 2011-2019 esistono solo per maggio-agosto: i giorni dall'1
#     al 21 maggio di quegli anni non hanno la storia dei lag e restano fuori;
#     dal 2020 la storia c'è (anni interi) e maggio entra tutto.
#   - secondo stadio: meta-regressione multivariata (mixmeta) con
#     meta-predittori temperatura media e range provinciali;
#   - attribuzione all'indietro (backward) con la crossbasis completa.

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(splines)
  library(dlnm)
  library(mixmeta)
  library(parallel)
})

RADICE_PROG <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Mortalità caldo 2026"
DIR_DER  <- file.path(RADICE_PROG, "input/derivati")
DIR_DLNM <- file.path(DIR_DER, "dlnm")
DIR_OUT  <- file.path(RADICE_PROG, "output")
DIR_DIAG <- file.path(DIR_OUT, "diagnostica")
for (d in c(DIR_DLNM, DIR_OUT, DIR_DIAG)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

N_CORE <- max(1L, detectCores() - 2L)
DATA_FINE <- as.IDate("2026-06-30")   # ultimo giorno con i decessi

# ---- Specifiche -----------------------------------------------------------------

spec_base <- list(
  nome      = "principale",
  tvar      = "tmean",          # tmean, tmin, tmax
  lag       = 21,
  nk_lag    = 3,                # nodi del lag (equidistanti in scala log)
  pct_var   = c(10, 75, 90),    # nodi della funzione esposizione-risposta
  stag      = "comune",         # "comune": ns(giorno stagione) uguale per tutti gli anni + effetto anno
                                # "anno": in più interazione ns x anno (contrasti a somma zero
                                #         tra gli anni completi; il 2026 prende la forma media)
  df_stag   = 4,
  anni_escl = 2020,
  anno_max  = 2026,             # ultimo anno usato nella stima
  tv        = "no",             # rischio variabile nel tempo: "no", "lin", "post"
  eta       = NULL,             # filtro strato: classi d'età
  sesso     = NULL              # filtro strato: "M" / "F"
)

modifica_spec <- function(nome, ...) {
  s <- spec_base
  args <- list(...)
  for (k in names(args)) s[k] <- list(args[[k]])
  s$nome <- nome
  s
}

# Strati e sensibilità
SPEC_STRATI <- list(
  eta_0_64  = modifica_spec("eta_0_64",  eta = "0-64"),
  eta_65_74 = modifica_spec("eta_65_74", eta = "65-74"),
  eta_75_84 = modifica_spec("eta_75_84", eta = "75-84"),
  eta_85    = modifica_spec("eta_85",    eta = "85+"),
  maschi    = modifica_spec("maschi",    sesso = "M"),
  femmine   = modifica_spec("femmine",   sesso = "F")
)

SPEC_SENS <- list(
  lag10        = modifica_spec("lag10", lag = 10),
  lag3         = modifica_spec("lag3", lag = 3, nk_lag = 1),
  tmax         = modifica_spec("tmax", tvar = "tmax"),
  tmin         = modifica_spec("tmin", tvar = "tmin"),
  stag_df3     = modifica_spec("stag_df3", df_stag = 3),
  stag_df6     = modifica_spec("stag_df6", df_stag = 6),
  stag_anno    = modifica_spec("stag_anno", stag = "anno"),
  covid_20_22  = modifica_spec("covid_20_22", anni_escl = 2020:2022),
  nodi_50_90   = modifica_spec("nodi_50_90", pct_var = c(50, 90)),
  nodi_25_50_75 = modifica_spec("nodi_25_50_75", pct_var = c(25, 50, 75)),
  senza_2026   = modifica_spec("senza_2026", anno_max = 2025),
  tv_lin       = modifica_spec("tv_lin", tv = "lin"),
  tv_post      = modifica_spec("tv_post", tv = "post", anni_escl = 2020:2022)
)

# ---- Dati -------------------------------------------------------------------------

carica_anagrafica <- function() {
  com <- fread(file.path(DIR_DER, "comuni.csv"), encoding = "Latin-1",
               colClasses = list(character = "cod_comune"))
  prov <- unique(com[, .(cod_prov, provincia, cod_reg, regione)])
  stopifnot(!anyDuplicated(prov$cod_prov))
  prov[, regione := sub("/.*$", "", regione)]
  prov[, macroarea := fcase(cod_reg %in% c(1, 2, 3, 7), "Nord-Ovest",
                            cod_reg %in% c(4, 5, 6, 8), "Nord-Est",
                            cod_reg %in% 9:12, "Centro",
                            cod_reg %in% 13:18, "Sud",
                            default = "Isole")]
  prov[order(cod_prov)]
}

carica_decessi <- function() {
  as.data.table(read_parquet(file.path(DIR_DER, "decessi_provincia.parquet")))
}

# decessi di uno strato (classi d'età e/o sesso), sommati per provincia e giorno
decessi_strato <- function(dec, eta = NULL, sesso = NULL) {
  sel <- rep(TRUE, nrow(dec))
  if (!is.null(eta))   sel <- sel & dec$eta %chin% eta
  if (!is.null(sesso)) sel <- sel & dec$sesso %chin% sesso
  dec[sel, .(n = sum(n)), by = .(cod_prov, data)]
}

carica_temperature <- function() {
  as.data.table(read_parquet(file.path(DIR_DER, "temperatura_provincia.parquet")))
}

# Matrice delle esposizioni ritardate (righe = date di 'date_out', colonne = lag 0..L)
matrice_lag <- function(tdt, tvar, date_out, L) {
  griglia <- seq(min(tdt$data) - L, max(tdt$data), by = 1)
  x <- rep(NA_real_, length(griglia))
  x[match(tdt$data, griglia)] <- tdt[[tvar]]
  i <- match(date_out, griglia)
  m <- sapply(0:L, function(l) x[i - l])
  if (is.null(dim(m))) m <- matrix(m, nrow = 1)
  m
}

# Percentili e range della distribuzione provinciale di maggio-agosto 2011-2025
distr_temp <- function(tp, tvar) {
  v <- tp[month(data) %in% 5:8 & year(data) <= 2025][[tvar]]
  list(perc = quantile(v, (0:1000) / 1000, names = FALSE),
       media = mean(v), range = diff(range(v)))
}

argvar_prov <- function(tp, tvar, pct_var) {
  dist <- distr_temp(tp, tvar)
  # bordi: tutte le temperature che possono entrare nei lag (anche aprile)
  v_all <- tp[month(data) %in% 4:8][[tvar]]
  list(fun = "ns",
       knots = quantile(tp[month(data) %in% 5:8 & year(data) <= 2025][[tvar]],
                        pct_var / 100, names = FALSE),
       Boundary.knots = range(v_all, na.rm = TRUE))
}

arglag_spec <- function(spec) {
  if (spec$lag <= 3) list(fun = "integer", intercept = TRUE) else
    list(fun = "ns", knots = logknots(spec$lag, spec$nk_lag), intercept = TRUE)
}

festivi <- function(data) {
  md <- format(data, "%m-%d")
  as.integer(md %chin% c("05-01", "06-02", "08-15"))
}

giorno_stagione <- function(data) as.integer(as.IDate(data)) - as.integer(as.IDate(sprintf("%d-05-01", year(data))))

# Tabella di stima per una provincia: righe maggio-agosto con storia dei lag completa
dati_stima_prov <- function(dp, tp, spec) {
  dp <- dp[month(data) %in% 5:8 & data <= DATA_FINE &
             year(data) <= spec$anno_max & !(year(data) %in% spec$anni_escl)]
  setorder(dp, data)
  X <- matrice_lag(tp, spec$tvar, dp$data, spec$lag)
  ok <- stats::complete.cases(X)
  dp <- dp[ok]; X <- X[ok, , drop = FALSE]
  dp[, `:=`(anno = factor(year(data)), dos = giorno_stagione(data),
            dow = factor(wday(data)), fest = festivi(data))]
  list(d = dp, X = X)
}

# Base della stagionalità
base_stagione <- function(d, spec) {
  B <- ns(d$dos, df = spec$df_stag, Boundary.knots = c(0, 122))
  colnames(B) <- paste0("s", seq_len(ncol(B)))
  if (spec$stag == "anno") {
    anni <- sort(unique(as.integer(as.character(d$anno))))
    completi <- setdiff(anni, 2026)
    yy <- as.integer(as.character(d$anno))
    ult <- completi[length(completi)]
    blocchi <- lapply(completi[-length(completi)], function(a) {
      w <- (yy == a) - (yy == ult)
      Bi <- B * w
      colnames(Bi) <- paste0(colnames(B), "_", a)
      Bi
    })
    B <- cbind(B, do.call(cbind, blocchi))
  }
  B
}

# ---- Primo stadio -------------------------------------------------------------

stima_prov <- function(p, dec_s, temp, spec, salva_fitted = FALSE) {
  dp <- dec_s[cod_prov == p, .(data, n)]
  tp <- temp[cod_prov == p]
  ds <- dati_stima_prov(dp, tp, spec)
  d <- ds$d
  argvar <- argvar_prov(tp, spec$tvar, spec$pct_var)
  cb <- crossbasis(ds$X, lag = spec$lag, argvar = argvar, arglag = arglag_spec(spec))
  S <- base_stagione(d, spec)
  yy <- as.integer(as.character(d$anno))
  if (spec$tv == "no") {
    mod <- glm(d$n ~ cb + d$anno + S + d$dow + d$fest, family = quasipoisson())
  } else {
    # rischio variabile nel tempo: interazione della crossbasis con un trend
    # lineare centrato sul 2026 ("lin") o con l'indicatore 2011-2019 ("post").
    # In entrambi i casi i coefficienti di 'cb' sono la curva recente
    # (2026 per "lin", 2023-2026 per "post", che esclude 2020-2022).
    w <- if (spec$tv == "lin") yy - 2026 else as.numeric(yy <= 2019)
    zint <- unclass(cb) * w
    mod <- glm(d$n ~ cb + zint + d$anno + S + d$dow + d$fest, family = quasipoisson())
  }
  # coefficienti della crossbasis (completi) e curva cumulata (ridotti)
  ind <- grep("^cb", names(coef(mod)))
  cf <- coef(mod)[ind]; vc <- vcov(mod)[ind, ind]
  red <- riduci_cb(cf, vc, arglag_spec(spec), spec$lag)
  dist <- distr_temp(tp, spec$tvar)
  out <- list(cod_prov = p, coef = cf, vcov = vc,
              coef_red = red$coef, vcov_red = red$vcov,
              argvar = argvar, arglag = arglag_spec(spec), lag = spec$lag,
              perc = dist$perc, media = dist$media, range = dist$range,
              disp = summary(mod)$dispersion, conv = mod$converged,
              nobs = nrow(d), ndec = sum(d$n), na_coef = any(is.na(cf)))
  if (spec$tv != "no") {
    # curva del periodo di confronto: 2011 per "lin", 2011-2019 per "post"
    iz <- grep("^zint", names(coef(mod)))
    k <- if (spec$tv == "lin") 2011 - 2026 else 1
    M <- cbind(diag(length(ind)), k * diag(length(ind)))
    cfv <- drop(M %*% coef(mod)[c(ind, iz)])
    vcv <- M %*% vcov(mod)[c(ind, iz), c(ind, iz)] %*% t(M)
    redv <- riduci_cb(cfv, vcv, arglag_spec(spec), spec$lag)
    out$coef_red_prima <- redv$coef; out$vcov_red_prima <- redv$vcov
  }
  if (salva_fitted) {
    out$fitted <- data.table(cod_prov = p, data = d$data, n = d$n,
                             fitted = fitted(mod),
                             res_pearson = residuals(mod, type = "pearson"),
                             res_dev = residuals(mod, type = "deviance"))
    out$pacf <- as.numeric(pacf(residuals(mod, type = "deviance"), lag.max = 30, plot = FALSE)$acf)
  }
  out
}

primo_stadio <- function(spec, dec, temp, salva_fitted = FALSE) {
  dec_s <- decessi_strato(dec, spec$eta, spec$sesso)
  prov <- sort(unique(dec_s$cod_prov))
  res <- mclapply(prov, function(p) {
    tryCatch(stima_prov(p, dec_s, temp, spec, salva_fitted),
             error = function(e) list(cod_prov = p, errore = conditionMessage(e)))
  }, mc.cores = N_CORE)
  names(res) <- prov
  err <- Filter(function(r) !is.null(r$errore), res)
  if (length(err)) warning(spec$nome, ": errori in ", length(err), " province: ",
                           paste(names(err), collapse = ", "))
  list(spec = spec, prov = res)
}

# ---- Secondo stadio -----------------------------------------------------------

# Meta-regressione multivariata: 'quale' = "red" (curva cumulata, 4 coef.)
# o "cb" (crossbasis completa, 20 coef.). Restituisce i BLUP per provincia.
meta_stadio <- function(ps, quale = c("red", "cb"), metodo = "reml", bscov = NULL,
                        predittori = ~ media + range) {
  quale <- match.arg(quale)
  pr <- Filter(function(r) is.null(r$errore) && !r$na_coef, ps$prov)
  Y <- t(sapply(pr, function(r) if (quale == "red") r$coef_red else r$coef))
  S <- lapply(pr, function(r) if (quale == "red") r$vcov_red else r$vcov)
  md <- data.frame(cod_prov = sapply(pr, `[[`, "cod_prov"),
                   media = sapply(pr, `[[`, "media"),
                   range = sapply(pr, `[[`, "range"))
  ana <- carica_anagrafica()
  md$macroarea <- ana$macroarea[match(md$cod_prov, ana$cod_prov)]
  if (is.null(bscov)) bscov <- if (quale == "red") "unstr" else "diag"
  frm <- update(predittori, Y ~ .)
  environment(frm) <- environment()
  mm <- mixmeta(frm, S = S, data = md, method = metodo, bscov = bscov,
                control = list(maxiter = 500))
  bl <- blup(mm, vcov = TRUE)
  blup_l <- lapply(seq_len(nrow(md)), function(i) list(coef = bl[[i]]$blup, vcov = bl[[i]]$vcov))
  names(blup_l) <- md$cod_prov
  list(modello = mm, blup = blup_l, metadati = md, quale = quale)
}

# Curva cumulata (sull'intera finestra di lag) dai coefficienti completi
riduci_cb <- function(coef, vcov, arglag, lag) {
  C <- do.call(onebasis, c(list(x = 0:lag), arglag))
  M <- diag(length(coef) / ncol(C)) %x% matrix(colSums(C), nrow = 1)
  list(coef = drop(M %*% coef), vcov = M %*% vcov %*% t(M))
}

# MMT: minimo della curva cumulata tra il 1° e il 99° percentile
trova_mmt <- function(coef_red, r, lim = c(10, 990)) {
  xs <- r$perc[(lim[1]:lim[2]) + 1]
  B <- do.call(onebasis, c(list(x = xs), r$argvar))
  lp <- drop(B %*% coef_red)
  i <- which.min(lp)
  list(mmt = xs[i], mmp = (lim[1] + i - 1) / 10)
}

# ---- Attribuzione ------------------------------------------------------------

# Climatologia 2011-2025 per provincia e giorno della stagione (maggio-agosto)
climatologia <- function(tp, tvar) {
  x <- tp[month(data) %in% 5:8 & year(data) <= 2025]
  x[, dos := giorno_stagione(data)]
  fit <- lm(x[[tvar]] ~ ns(x$dos, df = 6))
  B <- ns(0:122, knots = attr(ns(x$dos, df = 6), "knots"),
          Boundary.knots = attr(ns(x$dos, df = 6), "Boundary.knots"))
  drop(cbind(1, B) %*% coef(fit))
}

# Storia controfattuale: temperatura climatologica nei giorni di maggio-agosto
matrice_cf <- function(X, date_out, clim) {
  Xcf <- X
  for (l in seq_len(ncol(X)) - 1) {
    d <- date_out - l
    m <- month(d) %in% 5:8
    Xcf[m, l + 1] <- clim[giorno_stagione(d[m]) + 1]
  }
  Xcf
}


# Matrice di predizione backward: somma sui lag dei contributi delle esposizioni
# passate, centrata su 'cen' (o su una storia controfattuale 'Xcf').
xpred_back <- function(X, r, cen = NULL, Xcf = NULL) {
  base_cb <- function(M) crossbasis(M, lag = r$lag, argvar = r$argvar, arglag = r$arglag)
  if (is.null(Xcf)) Xcf <- matrix(cen, nrow(X), ncol(X))
  unclass(base_cb(X))[, , drop = FALSE] - unclass(base_cb(Xcf))[, , drop = FALSE]
}

# Morti attribuibili giornalieri (colonna 1 = stima puntuale, poi simulazioni)
an_da_xpred <- function(Xp, coefs, casi) {
  lp <- Xp %*% coefs
  casi * (1 - exp(-lp))
}

# Simulazioni congiunte dei BLUP provinciali. I BLUP di province diverse
# condividono i coefficienti fissi della meta-regressione: simularli in modo
# indipendente sottostima l'incertezza dei totali. Qui per ogni simulazione si
# estraggono i coefficienti fissi (comuni a tutte le province) e poi, per ogni
# provincia, l'effetto casuale dalla sua distribuzione condizionata ai dati
# provinciali:  theta_i | y_i, beta ~ N(mu_i + A_i (y_i - mu_i), Psi - A_i Psi),
# con mu_i = X_i beta e A_i = Psi (Psi + S_i)^-1.
# Restituisce, per provincia, una matrice k x (nsim + 1): la prima colonna è il
# BLUP puntuale.
sim_blup_congiunte <- function(mm, prov, nsim, seed = 1, quale = c("cb", "red")) {
  quale <- match.arg(quale)
  set.seed(seed)
  b <- coef(mm); Vb <- vcov(mm); Psi <- mm$Psi
  k <- ncol(Psi); np <- length(b) / k
  bs <- cbind(b, if (nsim > 0) t(MASS::mvrnorm(nsim, b, Vb)) else NULL)
  lapply(prov, function(r) {
    y <- if (quale == "cb") r$y else r$y_red
    S <- if (quale == "cb") r$S else r$S_red
    x <- c(1, r$media, r$range)
    A <- Psi %*% solve(Psi + S)
    Vp <- Psi - A %*% Psi
    Vp <- (Vp + t(Vp)) / 2
    # coefficienti ordinati per predittore: (Intercept) di tutti gli esiti, poi media, poi range
    mu <- sapply(seq_len(ncol(bs)), function(s) drop(matrix(bs[, s], k, np) %*% x))
    th <- mu + A %*% (y - mu)
    if (nsim > 0) {
      e <- eigen(Vp, symmetric = TRUE)
      R <- e$vectors %*% diag(sqrt(pmax(e$values, 0)), k)
      th[, -1] <- th[, -1] + R %*% matrix(rnorm(k * nsim), k, nsim)
    }
    th
  })
}
