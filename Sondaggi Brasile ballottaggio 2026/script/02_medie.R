#!/usr/bin/env Rscript
# ============================================================================
# Media dei sondaggi del ballottaggio Lula - Flavio Bolsonaro, 2026.
#
# Stesso metodo della media del primo turno (Sondaggi Brasile 2026/02_medie.R):
# filtro di Kalman a livello locale + smoother RTS, sigma_day e sigma_extra a
# massima verosimiglianza, effetti istituto con vincolo di somma zero e
# shrinkage, stimati alternandoli con i parametri.
#
# Differenza: la serie e' spezzata al primo turno (4 ottobre). Le due fasi
# sono stimate separatamente, ognuna con i suoi effetti istituto, perche':
#   - il primo turno e' un'informazione nuova che sposta il livello di colpo
#     (Flavio e' arrivato al 47%, i sondaggi lo davano al 42%);
#   - dopo l'errore del primo turno molti istituti hanno cambiato pesi e
#     metodo, quindi la distorsione di prima non vale per dopo.
#
# Grandezze:
#   - VOTI VALIDI: Lula / (Lula + Flavio) * 100, Flavio e' il complemento.
#     E' la grandezza che decide il ballottaggio.
#   - VOTI GREZZI (solo dopo il primo turno): Lula, Flavio e bianche/nulle/
#     indecisi separati, per vedere chi si muove davvero e da dove.
#
# Output: output/medie_giornaliere.csv, output/sondaggi_validi.csv,
#         output/medie_grezze.csv, output/effetti_istituto.csv,
#         output/stesso_istituto.csv
# ============================================================================

suppressPackageStartupMessages({ library(data.table) })

BASE   <- dirname(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))))
INPUT  <- file.path(BASE, "input")
OUTPUT <- file.path(BASE, "output")

DEFF      <- 1.5
TAU_HOUSE <- 2.0
P0        <- 400
PRIMO     <- as.Date("2026-10-04")
VOTO      <- as.Date("2026-10-25")
OGGI      <- as.Date("2026-10-25")
# Primo turno, voti validi (TSE): Flavio 56.104.503, Lula 53.879.538
PRIMO_LULA_2P <- 53879538 / (53879538 + 56104503) * 100

# ── Sondaggi ─────────────────────────────────────────────────────────────────
p <- fread(file.path(INPUT, "sondaggi_grezzi.csv"))
p[, data_media := as.Date(data_media)]
p[, n := as.numeric(n)]
p[is.na(n), n := median(p$n, na.rm = TRUE)]
p[, validi := lula + flavio]
p[, n_validi := n * validi / 100]
p[, lula_2p := lula / validi * 100]

# ── Kalman a livello locale + smoother RTS ───────────────────────────────────
kalman <- function(t_idx, y, s2, q, ng) {
  idx <- split(seq_along(y), t_idx)
  m_f <- P_f <- m_p <- P_p <- numeric(ng)
  m <- y[1]; P <- P0; ll <- 0
  for (t in seq_len(ng)) {
    if (t > 1) P <- P + q
    m_p[t] <- m; P_p[t] <- P
    k <- idx[[as.character(t)]]
    if (!is.null(k)) for (i in k) {
      v <- y[i] - m; F <- P + s2[i]
      ll <- ll - 0.5 * (log(2 * pi * F) + v^2 / F)
      K <- P / F; m <- m + K * v; P <- (1 - K) * P
    }
    m_f[t] <- m; P_f[t] <- P
  }
  m_s <- m_f; P_s <- P_f
  for (t in (ng - 1):1) {
    C <- P_f[t] / P_p[t + 1]
    m_s[t] <- m_f[t] + C * (m_s[t + 1] - m_p[t + 1])
    P_s[t] <- P_f[t] + C^2 * (P_s[t + 1] - P_p[t + 1])
  }
  list(m = m_s, sd = sqrt(pmax(P_s, 0)), ll = ll)
}

# Una serie (un candidato, una fase): stima alternata parametri <-> effetti
# istituto. `par_fissi` permette di imporre sigma_day/sigma_extra (per la fase
# dopo il primo turno, troppo corta per stimarli bene da sola).
media_fase <- function(d, giorni, par_fissi = NULL, etichetta = "") {
  d <- copy(d)[order(data_media)]
  d[, t := match(data_media, giorni)]
  q_m <- mean(d$y) / 100
  d[, f := sqrt(q_m * (1 - q_m)) / 0.5]
  d[, s2_camp := DEFF * pmax(y, 1) / 100 * (1 - pmax(y, 1) / 100) / n_validi * 1e4]
  d[, h := 0]
  ng <- length(giorni)
  fit <- function(sd_day, sd_extra)
    kalman(d$t, d$y - d$h, d$s2_camp + (d$f * sd_extra)^2, (d$f[1] * sd_day)^2, ng)
  par <- log(c(0.3, 1.0))
  for (it in 1:25) {
    if (is.null(par_fissi))
      par <- optim(par, function(x) fit(exp(x[1]), exp(x[2]))$ll,
                   control = list(fnscale = -1))$par
    else par <- log(par_fissi)
    sd_day <- exp(par[1]); sd_extra <- exp(par[2])
    k <- fit(sd_day, sd_extra)
    h_vecchio <- d$h
    d[, r := y - k$m[t]]
    d[, w := 1 / (s2_camp + (f * sd_extra)^2)]
    he <- d[, .(h = sum(w * r) / (sum(w) + 1 / (f[1] * TAU_HOUSE)^2), np = .N), by = istituto]
    he[, h := h - weighted.mean(h, np)]
    d[, h := he$h[match(istituto, he$istituto)]]
    if (max(abs(d$h - h_vecchio)) < 1e-3) break
  }
  k <- fit(sd_day, sd_extra)
  cat(sprintf("%-22s giri %2d  sigma_day %.3f  sigma_extra %.3f\n",
              etichetta, it, sd_day, sd_extra))
  list(serie = data.table(data = giorni, media = k$m, sd = k$sd),
       sond = d[, .(istituto, data = data_media, y, h)],
       he = he[, .(istituto, h, np)], par = c(sd_day, sd_extra))
}

FASI <- list(prima = seq(min(p$data_media), PRIMO - 1, by = "day"),
             dopo  = seq(PRIMO + 1, OGGI, by = "day"))

# ── Voti validi ──────────────────────────────────────────────────────────────
v <- p[, .(istituto, fase, data_media, n_validi, y = lula_2p)]
fit_prima <- media_fase(v[fase == "prima"], FASI$prima, etichetta = "validi prima")
# Dopo il primo turno: 15 giorni, i parametri si stimano male. Si stimano
# comunque e si confrontano con quelli di prima; se la fase e' troppo corta per
# dire qualcosa si tengono quelli della fase lunga.
fit_dopo_libero <- media_fase(v[fase == "dopo"], FASI$dopo, etichetta = "validi dopo (libero)")
fit_dopo <- media_fase(v[fase == "dopo"], FASI$dopo, par_fissi = fit_prima$par,
                       etichetta = "validi dopo (fissi)")

serie <- rbind(fit_prima$serie[, fase := "prima"], fit_dopo$serie[, fase := "dopo"])
serie <- rbind(serie[, .(data, fase, cand = "lula", candidato = "Lula",
                         media = round(media, 2), sd = round(sd, 2))],
               serie[, .(data, fase, cand = "flavio", candidato = "Flavio Bolsonaro",
                         media = round(100 - media, 2), sd = round(sd, 2))])
serie[, giorni_al_voto := as.integer(VOTO - data)]
fwrite(serie[order(cand, data)], file.path(OUTPUT, "medie_giornaliere.csv"))

sond <- rbind(fit_prima$sond[, fase := "prima"], fit_dopo$sond[, fase := "dopo"])
sond <- rbind(sond[, .(istituto, data, fase, cand = "lula", validi = round(y, 2),
                       effetto_istituto = round(h, 2), corretto = round(y - h, 2))],
              sond[, .(istituto, data, fase, cand = "flavio", validi = round(100 - y, 2),
                       effetto_istituto = round(-h, 2), corretto = round(100 - y + h, 2))])
fwrite(sond[order(data, cand)], file.path(OUTPUT, "sondaggi_validi.csv"))

he <- merge(fit_prima$he[, .(istituto, prima = round(h, 2), sondaggi_prima = np)],
            fit_dopo$he[, .(istituto, dopo = round(h, 2), sondaggi_dopo = np)],
            by = "istituto", all = TRUE)
fwrite(he[order(-sondaggi_dopo)], file.path(OUTPUT, "effetti_istituto.csv"))

# ── Voti grezzi dopo il primo turno ──────────────────────────────────────────
dopo <- p[fase == "dopo"]
grezze <- rbindlist(lapply(c(lula = "lula", flavio = "flavio", indecisi = "bianche_indecisi"),
  function(col) {
    d <- dopo[, .(istituto, data_media, n_validi = n, y = get(col))]
    f <- media_fase(d, FASI$dopo, par_fissi = fit_prima$par, etichetta = paste("grezzi", col))
    f$serie[, voce := col]
  }))
grezze[voce == "bianche_indecisi", voce := "indecisi"]
fwrite(grezze[, .(data, voce, media = round(media, 2), sd = round(sd, 2))],
       file.path(OUTPUT, "medie_grezze.csv"))

# ── Stesso istituto: primo sondaggio dopo il voto contro l'ultimo ────────────
stesso <- dopo[, if (.N >= 2) {
  a <- .SD[which.min(data_media)]; b <- .SD[which.max(data_media)]
  .(dal = a$data_media, al = b$data_media,
    lula_2p_da = round(a$lula_2p, 1), lula_2p_a = round(b$lula_2p, 1),
    delta_lula_2p = round(b$lula_2p - a$lula_2p, 1),
    delta_lula = b$lula - a$lula, delta_flavio = b$flavio - a$flavio,
    delta_indecisi = round(b$bianche_indecisi - a$bianche_indecisi, 1))
}, by = istituto]
fwrite(stesso[order(-al)], file.path(OUTPUT, "stesso_istituto.csv"))

# ── Controlli e numeri per l'analisi ─────────────────────────────────────────
cat(sprintf("\nPrimo turno, Lula sui voti validi a due: %.2f (Flavio %.2f)\n",
            PRIMO_LULA_2P, 100 - PRIMO_LULA_2P))

banda <- function(d) d[, .(data, media, lo = round(media - 1.645 * sd, 1),
                           hi = round(media + 1.645 * sd, 1))]
L <- serie[cand == "lula"]
cat("\n-- Lula sui voti validi (Flavio = 100 - Lula), banda 90% --\n")
print(banda(L[data %in% c(as.Date(c("2026-04-15", "2026-06-15", "2026-08-15",
                                  "2026-09-01", "2026-09-15", "2026-10-03",
                                  "2026-10-05", "2026-10-08", "2026-10-11",
                                  "2026-10-14", "2026-10-17")), OGGI)]), row.names = FALSE)

pend <- function(d, da, a) d[data == a, media] - d[data == da, media]
cat(sprintf("\nPrima: da 1/9 a 3/10 Lula %+.1f | da 15/8 a 3/10 %+.1f\n",
            pend(L, as.Date("2026-09-01"), PRIMO - 1), pend(L, as.Date("2026-08-15"), PRIMO - 1)))
cat(sprintf("Salto al primo turno (media 3/10 -> 5/10): Lula %+.1f\n",
            pend(L, PRIMO - 1, PRIMO + 1)))
cat(sprintf("Dopo: da 5/10 a %s Lula %+.1f | ultimi 7 giorni %+.1f\n", format(OGGI),
            pend(L, PRIMO + 1, OGGI), pend(L, OGGI - 7, OGGI)))
sorpasso <- L[fase == "prima" & data >= as.Date("2026-06-15") & media < 50, min(data)]
cat("Prima: Flavio passa avanti sui voti validi il", format(sorpasso), "\n")
cat(sprintf("Errore medio dei sondaggi sul primo turno (media 3/10 contro voto): Lula %+.1f\n",
            L[data == PRIMO - 1, media] - PRIMO_LULA_2P))

cat("\n-- Parametri: prima", round(fit_prima$par, 3), "| dopo libero",
    round(fit_dopo_libero$par, 3), "--\n")
cat("-- Dopo, con parametri liberi: Lula al", format(OGGI),
    round(tail(fit_dopo_libero$serie$media, 1), 1), "--\n")

cat("\n-- Voti grezzi dopo il primo turno --\n")
print(dcast(grezze[data %in% c(as.Date(c("2026-10-05", "2026-10-09", "2026-10-13",
                                          "2026-10-17")), OGGI)],
            data ~ voce, value.var = "media"), row.names = FALSE)

cat("\n-- Stesso istituto, primo e ultimo sondaggio dopo il voto --\n")
print(stesso[order(-al)], row.names = FALSE)
cat(sprintf("Mediana delta Lula (validi): %+.1f | istituti in cui Lula sale: %d su %d\n",
            median(stesso$delta_lula_2p), sum(stesso$delta_lula_2p > 0), nrow(stesso)))

cat("\n-- Effetti istituto (Lula, voti validi) --\n")
print(he[order(-sondaggi_dopo)], row.names = FALSE)

cat("\n-- Media semplice ultimi 7 giorni, ultimo per istituto --\n")
ult <- dopo[data_media >= OGGI - 7][order(-data_media)][, .SD[1], by = istituto]
cat(sprintf("Lula %.1f su %d istituti\n", mean(ult$lula_2p), nrow(ult)))
