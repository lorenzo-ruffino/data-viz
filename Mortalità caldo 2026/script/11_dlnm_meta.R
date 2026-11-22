# Secondo stadio: meta-regressione multivariata (mixmeta, REML) dei coefficienti
# provinciali, con meta-predittori temperatura media e range di maggio-agosto.
#   - "red": curva cumulata sui lag (4 coefficienti, covarianza non strutturata);
#            serve per le curve nazionali e per macroarea;
#   - "cb":  crossbasis completa (20 coefficienti, covarianza diagonale); i BLUP
#            servono all'attribuzione backward, che richiede la struttura dei lag.
# MMT di ogni provincia: minimo della curva cumulata BLUP (da "cb") tra il 1° e
# il 99° percentile di maggio-agosto 2011-2025.
#
# Output: input/derivati/dlnm/meta_<nome>.rds
#         output/mmt_provincia.csv, output/curve_esposizione_risposta.csv,
#         output/meta_regressione.csv

source("/Users/lorenzoruffino/Documents/Progetti/data-viz/Mortalità caldo 2026/script/dlnm_funzioni.R")

ana <- carica_anagrafica()
specs <- c(list(principale = spec_base), SPEC_STRATI, SPEC_SENS)

wald <- function(mm, var) {
  b <- coef(mm); V <- vcov(mm)
  i <- grep(paste0(":", var, "$|^", var, "$"), names(b))
  if (!length(i)) i <- grep(var, names(b))
  st <- drop(t(b[i]) %*% solve(V[i, i]) %*% b[i])
  c(stat = st, df = length(i), p = pchisq(st, length(i), lower.tail = FALSE))
}

esegui_meta <- function(s) {
  ps <- readRDS(file.path(DIR_DLNM, sprintf("primo_stadio_%s.rds", s$nome)))
  m_red <- meta_stadio(ps, "red")
  m_cb  <- meta_stadio(ps, "cb")
  m_prima <- NULL
  if (s$tv != "no") {
    ps2 <- ps
    ps2$prov <- lapply(ps$prov, function(r) { r$coef_red <- r$coef_red_prima; r$vcov_red <- r$vcov_red_prima; r })
    m_prima <- meta_stadio(ps2, "red")
  }
  # BLUP completi, curva cumulata e MMT per provincia
  prov <- lapply(names(m_cb$blup), function(p) {
    r <- ps$prov[[p]]; b <- m_cb$blup[[p]]
    red <- riduci_cb(b$coef, b$vcov, r$arglag, r$lag)
    mm <- trova_mmt(red$coef, r)
    list(cod_prov = r$cod_prov, coef = b$coef, vcov = b$vcov,
         coef_red = red$coef, vcov_red = red$vcov,
         coef_red_blup4 = m_red$blup[[p]]$coef, vcov_red_blup4 = m_red$blup[[p]]$vcov,
         y = r$coef, S = r$vcov, y_red = r$coef_red, S_red = r$vcov_red,
         mmt = mm$mmt, mmp = mm$mmp,
         mmp_blup4 = trova_mmt(m_red$blup[[p]]$coef, r)$mmp,
         argvar = r$argvar, arglag = r$arglag, lag = r$lag, perc = r$perc,
         media = r$media, range = r$range)
  })
  names(prov) <- names(m_cb$blup)
  stat <- function(m, quale) {
    sm <- summary(m$modello)
    w1 <- wald(m$modello, "media"); w2 <- wald(m$modello, "range")
    data.table(spec = s$nome, meta = quale, Q = sm$qstat$Q[1], df = sm$qstat$df[1],
               p_Q = sm$qstat$pvalue[1], I2 = sm$i2stat[1],
               wald_media_p = w1["p"], wald_range_p = w2["p"],
               converged = m$modello$converged, aic = AIC(m$modello))
  }
  st <- rbind(stat(m_red, "red"), stat(m_cb, "cb"))
  out <- list(spec = s, prov = prov, m_red = m_red$modello, m_cb = m_cb$modello, m_prima = m_prima$modello,
              metadati = m_red$metadati, stat = st)
  saveRDS(out, file.path(DIR_DLNM, sprintf("meta_%s.rds", s$nome)))
  st
}

t0 <- Sys.time()
stat <- mclapply(specs, function(s) tryCatch(esegui_meta(s), error = function(e) {
  data.table(spec = s$nome, meta = "errore", errore = conditionMessage(e)) }),
  mc.cores = N_CORE, mc.preschedule = FALSE)
stat <- rbindlist(stat, fill = TRUE)
print(stat)
cat("tempo:", format(Sys.time() - t0), "\n")
fwrite(stat, file.path(DIR_OUT, "meta_regressione.csv"))

# ---- Specifica principale: MMT e curve --------------------------------------

mp <- readRDS(file.path(DIR_DLNM, "meta_principale.rds"))
ps <- readRDS(file.path(DIR_DLNM, "primo_stadio_principale.rds"))
mmt <- rbindlist(lapply(mp$prov, function(r) data.table(
  cod_prov = r$cod_prov, tmean_media = r$media, tmean_range = r$range,
  p01 = r$perc[11], p50 = r$perc[501], p975 = r$perc[976], p99 = r$perc[991],
  mmt = r$mmt, mmp = r$mmp, mmp_blup_ridotto = r$mmp_blup4)))
mmt <- merge(ana[, .(cod_prov, provincia, regione, macroarea)], mmt, by = "cod_prov")
# rischio relativo al 99° percentile rispetto all'MMT (curva BLUP)
mmt[, c("rr_p99", "rr_p99_lo", "rr_p99_hi") := {
  res <- lapply(as.character(cod_prov), function(p) {
    r <- mp$prov[[p]]
    B <- do.call(onebasis, c(list(x = c(r$perc[991], r$mmt)), r$argvar))
    xb <- B[1, ] - B[2, ]
    est <- sum(xb * r$coef_red); se <- sqrt(drop(t(xb) %*% r$vcov_red %*% xb))
    exp(est + c(0, -1.96, 1.96) * se)
  })
  as.list(as.data.table(do.call(rbind, res)))
}]
fwrite(mmt, file.path(DIR_OUT, "mmt_provincia.csv"))
cat("\nMMP (percentile della temperatura minima di mortalità):\n"); print(summary(mmt$mmp))
cat("MMP ai bordi (<=2 o >=98):", mmt[mmp <= 2 | mmp >= 98, .N], "\n")
cat("Correlazione MMT da BLUP completo e ridotto:", cor(mmt$mmp, mmt$mmp_blup_ridotto), "\n")

# Curve nazionale e per macroarea: predizione della meta-regressione "red" ai
# valori medi dei meta-predittori, sulla distribuzione media delle temperature
# (percentili mediati tra le province, pesati per i decessi).
curva_area <- function(mm, prov_sel, area, spec_nome = "principale", periodo = "tutto") {
  pr <- mp$prov[as.character(prov_sel)]
  w <- sapply(as.character(prov_sel), function(p) ps$prov[[p]]$ndec); w <- w / sum(w)
  perc <- Reduce(`+`, Map(function(r, wi) r$perc * wi, pr, w))
  knots <- perc[c(101, 751, 901)]
  bk <- Reduce(`+`, Map(function(r, wi) r$argvar$Boundary.knots * wi, pr, w))
  nd <- data.frame(media = sum(sapply(pr, `[[`, "media") * w),
                   range = sum(sapply(pr, `[[`, "range") * w))
  pred <- predict(mm, newdata = nd, vcov = TRUE)
  xs <- perc[seq(1, 1001, by = 5)]
  B <- onebasis(xs, fun = "ns", knots = knots, Boundary.knots = bk)
  sel <- 11:991
  lp_all <- drop(B %*% pred$fit)
  i_min <- which.min(lp_all[seq(1, length(xs))][xs >= perc[11] & xs <= perc[991]])
  xmin <- xs[xs >= perc[11] & xs <= perc[991]][i_min]
  Bc <- onebasis(xmin, fun = "ns", knots = knots, Boundary.knots = bk)
  Xc <- scale(B, center = Bc, scale = FALSE)
  est <- drop(Xc %*% pred$fit); se <- sqrt(rowSums((Xc %*% pred$vcov) * Xc))
  data.table(spec = spec_nome, periodo = periodo, area = area,
             percentile = seq(0, 100, by = 0.5), temperatura = xs,
             rr = exp(est), rr_lo = exp(est - 1.96 * se), rr_hi = exp(est + 1.96 * se),
             mmt = xmin)
}

curve <- list(curva_area(mp$m_red, ana$cod_prov, "Italia"))
for (a in unique(ana$macroarea)) curve[[length(curve) + 1]] <- curva_area(mp$m_red, ana[macroarea == a, cod_prov], a)
# curve recenti e passate dei modelli con rischio variabile nel tempo
for (nm in c("tv_lin", "tv_post")) {
  mt <- readRDS(file.path(DIR_DLNM, sprintf("meta_%s.rds", nm)))
  per <- if (nm == "tv_lin") c("2026", "2011") else c("2023-2026", "2011-2019")
  curve[[length(curve) + 1]] <- curva_area(mt$m_red, ana$cod_prov, "Italia", nm, per[1])
  curve[[length(curve) + 1]] <- curva_area(mt$m_prima, ana$cod_prov, "Italia", nm, per[2])
}
curve <- rbindlist(curve)
fwrite(curve, file.path(DIR_OUT, "curve_esposizione_risposta.csv"))
print(curve[percentile %in% c(1, 50, 75, 90, 97.5, 99), .(spec, periodo, area, percentile,
      temperatura = round(temperatura, 1), rr = round(rr, 3), rr_lo = round(rr_lo, 3), rr_hi = round(rr_hi, 3))])
