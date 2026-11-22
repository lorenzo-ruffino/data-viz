#!/usr/bin/env Rscript
# ============================================================================
# Media dei sondaggi del primo turno delle presidenziali brasiliane 2026.
#
# Mette insieme i due metodi gia' in uso:
#   - Focus America: filtro di Kalman a livello locale + smoother RTS, con la
#     velocita' di spostamento (sigma_day) stimata a massima verosimiglianza;
#   - DoppiaCifra: effetti istituto (house effect) stimati e sottratti, con
#     vincolo di somma zero (l'istituto medio non e' distorto) e shrinkage.
#
# Grandezza: VOTI VALIDI, come li conta il Tribunale superiore elettorale.
# Ogni sondaggio viene riportato a 100 togliendo bianche, nulle e indecisi
# (che vanno dall'1 al 19 per cento a seconda dell'istituto: e' la principale
# fonte di differenza fra istituti sui voti grezzi). E' anche la grandezza della
# soglia del 50 per cento per vincere al primo turno.
#
#   stato:        m_t = m_{t-1} + w_t               w_t ~ N(0, (f_c sigma_day)^2)
#   osservazione: y_i = m_{t(i)} + h_{p(i)} + e_i   e_i ~ N(0, s_i^2)
#   s_i^2 = DEFF q(1-q)/n_validi 100^2 + (f_c sigma_extra)^2
#
# f_c = sqrt(q_c (1 - q_c)) / 0.5 riscala rumore e movimento sulla quota media
# del candidato: chi sta al 4 per cento non si muove quanto chi sta al 45.
# sigma_day e sigma_extra sono stimati a massima verosimiglianza, alternandoli
# con la stima degli effetti istituto, per DUE gruppi: i due candidati di testa
# (un solo valore per Lula e Flavio, cosi' le due linee sono confrontabili) e i
# quattro minori. Insieme, il picco di Cury di fine agosto spingeva sigma_day
# verso 0,8 punti al giorno e rendeva nervose anche le linee di testa (da sole
# ne vogliono 0,36).
#
# Output: output/medie_giornaliere.csv, output/sondaggi_validi.csv,
#         output/effetti_istituto.csv
# ============================================================================

suppressPackageStartupMessages({ library(data.table) })

BASE   <- dirname(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))))
INPUT  <- file.path(BASE, "input")
OUTPUT <- file.path(BASE, "output")

DEFF      <- 1.5          # effetto del disegno campionario
TAU_HOUSE <- 2.0          # sd a priori dell'effetto istituto (punti, f_c = 1)
P0        <- 400          # prior diffuso sul livello iniziale
VOTO      <- as.Date("2026-10-04")
OGGI      <- as.Date("2026-10-03")

CAND <- c(lula = "Lula", flavio = "Flávio Bolsonaro", caiado = "Caiado",
          zema = "Zema", santos = "Renan Santos", cury = "Cury")

# ── Sondaggi in voti validi ──────────────────────────────────────────────────
p <- fread(file.path(INPUT, "sondaggi_grezzi.csv"))
p[, data_media := as.Date(data_media)]
p[, validi := 100 - bianche_indecisi]
p[, n_validi := n * validi / 100]
for (c in names(CAND)) p[, (c) := get(c) / validi * 100]

lungo <- melt(p, id.vars = c("istituto", "data_media", "n_validi"),
              measure.vars = names(CAND), variable.name = "cand", value.name = "y",
              variable.factor = FALSE)[!is.na(y)]
f_c <- lungo[, .(f = sqrt(mean(y) / 100 * (1 - mean(y) / 100)) / 0.5), by = cand]
lungo <- merge(lungo, f_c, by = "cand")
lungo[, s2_camp := DEFF * pmax(y, 1) / 100 * (1 - pmax(y, 1) / 100) / n_validi * 1e4]
lungo[, h := 0]
lungo[, gruppo := fifelse(cand %in% c("lula", "flavio"), "testa", "minori")]

GIORNI <- seq(min(p$data_media), OGGI, by = "day")

# ── Kalman a livello locale + smoother RTS ───────────────────────────────────
kalman <- function(t_idx, y, s2, q) {
  ng <- length(GIORNI)
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

lungo[, t := match(data_media, GIORNI)]
setorder(lungo, cand, t)

fit_cand <- function(d, sd_day, sd_extra) {
  f <- d$f[1]
  kalman(d$t, d$y - d$h, d$s2_camp + (f * sd_extra)^2, (f * sd_day)^2)
}

ll_gruppo <- function(par, gr_nome) {
  d_g <- lungo[gruppo == gr_nome]
  sum(vapply(split(d_g, d_g$cand), function(d) fit_cand(d, exp(par[1]), exp(par[2]))$ll, 0))
}

# ── Stima alternata: parametri <-> effetti istituto ──────────────────────────
par <- list(testa = log(c(0.3, 1.0)), minori = log(c(0.3, 1.0)))
for (it in 1:15) {
  for (g in names(par))
    par[[g]] <- optim(par[[g]], ll_gruppo, gr_nome = g, control = list(fnscale = -1))$par
  SD <- lapply(par, exp)

  h_vecchio <- copy(lungo$h)
  for (c in names(CAND)) {
    d <- lungo[cand == c]
    sd_day <- SD[[d$gruppo[1]]][1]; sd_extra <- SD[[d$gruppo[1]]][2]
    k <- fit_cand(d, sd_day, sd_extra)
    d[, r := y - k$m[t]]                        # scarto dalla media senza h
    d[, w := 1 / (s2_camp + (f * sd_extra)^2)]
    he <- d[, .(h = sum(w * r) / (sum(w) + 1 / (f[1] * TAU_HOUSE)^2), np = .N),
            by = istituto]
    he[, h := h - weighted.mean(h, np)]         # somma zero, pesata per sondaggi
    lungo[cand == c, h := he$h[match(istituto, he$istituto)]]
  }
  delta <- max(abs(lungo$h - h_vecchio))
  cat(sprintf("giro %2d  testa: day %.3f extra %.3f | minori: day %.3f extra %.3f | delta_h %.4f\n",
              it, SD$testa[1], SD$testa[2], SD$minori[1], SD$minori[2], delta))
  if (delta < 1e-3) break
}

# ── Serie finali ─────────────────────────────────────────────────────────────
serie <- rbindlist(lapply(names(CAND), function(c) {
  d <- lungo[cand == c]
  k <- fit_cand(d, SD[[d$gruppo[1]]][1], SD[[d$gruppo[1]]][2])
  data.table(data = GIORNI, cand = c, candidato = CAND[[c]],
             media = round(k$m, 2), sd = round(k$sd, 2))
}))
serie[, giorni_al_voto := as.integer(VOTO - data)]
fwrite(serie, file.path(OUTPUT, "medie_giornaliere.csv"))

sond <- lungo[, .(istituto, data = data_media, cand, candidato = CAND[cand],
                  validi = round(y, 2), effetto_istituto = round(h, 2),
                  corretto = round(y - h, 2))]
fwrite(sond[order(data, cand)], file.path(OUTPUT, "sondaggi_validi.csv"))

he <- dcast(unique(lungo[, .(istituto, cand, h = round(h, 2))]), istituto ~ cand,
            value.var = "h")
he <- merge(he, p[, .(sondaggi = .N), by = istituto], by = "istituto")
setcolorder(he, c("istituto", "sondaggi", names(CAND)))
fwrite(he[order(-sondaggi)], file.path(OUTPUT, "effetti_istituto.csv"))

# ── Controlli ────────────────────────────────────────────────────────────────
cat("\n-- Effetti istituto (punti di voti validi; + = sovrastima il candidato) --\n")
print(he[order(-sondaggi)], row.names = FALSE)

cat("\n-- Media al", format(OGGI), "(voti validi, banda 90%) --\n")
ult <- serie[data == OGGI][order(-media)]
ult[, `:=`(lo = round(media - 1.645 * sd, 1), hi = round(media + 1.645 * sd, 1))]
print(ult[, .(candidato, media, sd, lo, hi)], row.names = FALSE)
cat(sprintf("Somma dei sei: %.1f  |  vantaggio Lula: %.1f\n",
            sum(ult$media), ult[cand == "lula", media] - ult[cand == "flavio", media]))

cat("\n-- Tappe (Lula / Flavio) --\n")
tappe <- serie[data %in% as.Date(c("2026-04-15", "2026-05-15", "2026-06-15",
                                   "2026-07-15", "2026-08-15", "2026-09-15", "2026-10-03"))]
print(dcast(tappe, data ~ cand, value.var = "media")[, .(data, lula, flavio, santos, cury, caiado, zema)],
      row.names = FALSE)

cat("\n-- Confronto: media semplice ultimi 14 giorni, per istituto l'ultimo --\n")
ultimi <- lungo[data_media >= OGGI - 14][order(-data_media)][, .SD[1], by = .(istituto, cand)]
print(ultimi[, .(semplice = round(mean(y), 1), istituti = .N), by = cand], row.names = FALSE)
