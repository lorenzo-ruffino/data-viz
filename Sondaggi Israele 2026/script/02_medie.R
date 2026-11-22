#!/usr/bin/env Rscript
# ============================================================================
# Media dei sondaggi per le elezioni della Knesset del 27 ottobre 2026.
#
# Stesso metodo della media brasiliana (Sondaggi Brasile 2026/script/02_medie.R):
#   - filtro di Kalman a livello locale + smoother RTS, con la velocita' di
#     spostamento (sigma_day) e il rumore extra (sigma_extra) stimati a massima
#     verosimiglianza;
#   - effetti istituto (house effect) stimati e sottratti, con vincolo di somma
#     zero (l'istituto medio non e' distorto) e shrinkage.
#
# Grandezza: SEGGI su 120, come li pubblicano gli istituti israeliani.
#   - Coalizioni: somma dei seggi delle liste del blocco (0 per chi resta sotto
#     la soglia del 3,25 per cento), riportata a 120 dove la somma del
#     sondaggio sbaglia di uno o due. Tutti i sondaggi del 2026.
#   - Partiti: seggi; per chi e' sotto soglia, la percentuale x 1,2 (i seggi
#     che avrebbe senza soglia, come nel grafico di Wikipedia), cosi' la linea
#     non salta fra 0 e 4. Dal 26 aprile, quando nasce Together (Bennett e Yesh
#     Atid): da li' le liste sono quasi quelle definitive.
#
#   stato:        m_t = m_{t-1} + w_t               w_t ~ N(0, (f_c sigma_day)^2)
#   osservazione: y_i = m_{t(i)} + h_{p(i)} + e_i   e_i ~ N(0, s_i^2)
#   s_i^2 = DEFF q(1-q)/n 120^2 + (f_c sigma_extra)^2,  q = y_i / 120
#
# f_c = sqrt(q_c (1 - q_c)) / 0.5 riscala rumore e movimento sulla quota media
# della serie. sigma_day e sigma_extra si stimano per gruppo: le tre coalizioni
# insieme; per i partiti, i tre grandi (Likud, Together, Yashar) e gli altri.
#
# Blocchi:
#   governo     Likud, Sionismo religioso (con Zehut), Otzma Yehudit, Shas,
#               Giudaismo unito della Torah, Amcha Yisrael di Ofer Winter (che
#               dichiara di sostenere solo Netanyahu), Haredi Public
#   opposizione Together, Yashar, Yisrael Beiteinu, Democratici, Riservisti,
#               Blu e Bianco, Unity
#   arabi       Ra'am, Lista comune (Hadash-Ta'al e Balad)
#
# Output: output/medie_coalizioni.csv, output/medie_partiti.csv,
#         output/sondaggi_coalizioni.csv, output/sondaggi_partiti.csv,
#         output/effetti_istituto.csv
# ============================================================================

suppressPackageStartupMessages({ library(data.table) })

BASE   <- dirname(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))))
INPUT  <- file.path(BASE, "input")
OUTPUT <- file.path(BASE, "output")

DEFF      <- 1.5          # effetto del disegno campionario
TAU_HOUSE <- 3.0          # sd a priori dell'effetto istituto (seggi, f_c = 1)
P0        <- 400          # prior diffuso sul livello iniziale
VOTO      <- as.Date("2026-10-27")
OGGI      <- as.Date("2026-10-23")   # ultimo giorno in cui si puo' pubblicare
INIZIO_PARTITI <- as.Date("2026-04-26")

# lista sulla voce (senza spazi) -> serie
LISTE <- c(
  "Likud" = "likud", "Together" = "together", "YeshAtid" = "together",
  "Bennett2026" = "together",
  "RZP" = "rzp", "Zehut" = "rzp", "RZP+Zehut" = "rzp", "RZP-Zehut" = "rzp",
  "Otzma" = "otzma", "Shas" = "shas", "UTJ" = "utj",
  "AmchaYisrael" = "amcha", "Winterparty" = "amcha", "HarediPublic" = "haredi",
  "YisraelBeiteinu" = "yb", "Dems" = "dems", "Yashar" = "yashar",
  "Reserv." = "riservisti", "ZionistHome" = "riservisti",
  "Reserv.-NEP" = "riservisti", "Blue&White" = "blubianco", "Unity" = "unity",
  "Ra'am" = "raam", "JointList" = "lista_comune", "Hadash–Ta'al" = "lista_comune",
  "Balad" = "lista_comune", "Hadash–Ta'al+Balad" = "lista_comune",
  "Ra'am+Hadash–Ta'al+Balad" = "arabi_insieme")

BLOCCO <- c(likud = "governo", rzp = "governo", otzma = "governo", shas = "governo",
            utj = "governo", amcha = "governo", haredi = "governo",
            together = "opposizione", yashar = "opposizione", yb = "opposizione",
            dems = "opposizione", riservisti = "opposizione",
            blubianco = "opposizione", unity = "opposizione",
            raam = "arabi", lista_comune = "arabi", arabi_insieme = "arabi")

NOMI_COAL <- c(governo = "Coalizione di Netanyahu", opposizione = "Opposizione",
               arabi = "Partiti arabi")
NOMI_PART <- c(likud = "Likud", together = "Together (Bennett)",
               yashar = "Yashar (Eisenkot)", rzp = "Sionismo religioso",
               otzma = "Otzma Yehudit", shas = "Shas",
               utj = "Giudaismo unito", yb = "Yisrael Beiteinu",
               dems = "Democratici", raam = "Ra'am", lista_comune = "Lista comune",
               riservisti = "Riservisti", amcha = "Amcha Yisrael")
GRANDI <- c("likud", "together", "yashar")

# ── Sondaggi ─────────────────────────────────────────────────────────────────
g <- fread(file.path(INPUT, "sondaggi_grezzi.csv"), na.strings = c("", "NA"))
g[, data_media := as.Date(data_media)]
g[, istituto := fcase(istituto == "MP+TM+SN+A`", "MP+TM+SN+A",
                      istituto == "Lazar", "LRI+P4A",
                      istituto == "Filber", "SF+ND",
                      default = istituto)]
# Channel 14: Filber e SF+ND sono la stessa serie di Shlomo Filber
g[, id := .GRP, by = .(istituto, editore, data_fine, n)]
g[, chiave := gsub(" ", "", lista)]
g <- g[chiave %in% names(LISTE)]
g[, serie := LISTE[chiave]]
g[, blocco := BLOCCO[serie]]
n_mediano <- median(unique(g[, .(id, n)])$n, na.rm = TRUE)
g[is.na(n), n := n_mediano]

# coalizioni: seggi veri, riportati a 120
coal <- g[!is.na(seggi), .(y = sum(seggi)), by = .(id, istituto, data_media, n, blocco)]
coal[, y := y * 120 / sum(y), by = id]
coal[, serie := blocco]

# partiti: seggi o equivalente sotto soglia
g[, equiv := fifelse(!is.na(pct_sotto_soglia), pct_sotto_soglia * 1.2, seggi)]
part <- g[data_media >= INIZIO_PARTITI & serie %in% names(NOMI_PART) & !is.na(equiv),
          .(y = sum(equiv)), by = .(id, istituto, data_media, n, serie)]
# senza le righe in cui la voce non separa Ra'am dalla Lista comune
part <- part[!id %in% g[serie == "arabi_insieme", id] | !serie %in% c("raam", "lista_comune")]

prepara <- function(d, gruppo_di) {
  d <- copy(d)
  f_c <- d[, .(f = sqrt(mean(y) / 120 * (1 - mean(y) / 120)) / 0.5), by = serie]
  d <- merge(d, f_c, by = "serie")
  d[, s2_camp := DEFF * pmax(y, 1.2) / 120 * (1 - pmax(y, 1.2) / 120) / n * 120^2]
  d[, h := 0]
  d[, gruppo := gruppo_di(serie)]
  d
}
coal <- prepara(coal, function(s) rep("coalizioni", length(s)))
part <- prepara(part, function(s) fifelse(s %in% GRANDI, "grandi", "altri"))

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

stima <- function(d, nome) {
  giorni <- seq(min(d$data_media), OGGI, by = "day")
  d[, t := match(data_media, giorni)]
  setorder(d, serie, t)
  fit <- function(ds, sd_day, sd_extra) {
    f <- ds$f[1]
    kalman(ds$t, ds$y - ds$h, ds$s2_camp + (f * sd_extra)^2, (f * sd_day)^2,
           length(giorni))
  }
  ll_gruppo <- function(par, gruppo_nome) {
    dg <- d[gruppo == gruppo_nome]
    sum(vapply(split(dg, dg$serie), function(ds) fit(ds, exp(par[1]), exp(par[2]))$ll, 0))
  }
  gruppi <- unique(d$gruppo)
  par <- setNames(lapply(gruppi, function(x) log(c(0.2, 1.0))), gruppi)
  for (it in 1:20) {
    for (gr in gruppi)
      par[[gr]] <- optim(par[[gr]], ll_gruppo, gruppo_nome = gr, control = list(fnscale = -1))$par
    SD <- lapply(par, exp)
    h_vecchio <- copy(d$h)
    for (s in unique(d$serie)) {
      ds <- d[serie == s]
      sdd <- SD[[ds$gruppo[1]]]
      k <- fit(ds, sdd[1], sdd[2])
      ds[, r := y - k$m[t]]
      ds[, w := 1 / (s2_camp + (f * sdd[2])^2)]
      he <- ds[, .(h = sum(w * r) / (sum(w) + 1 / (f[1] * TAU_HOUSE)^2), np = .N),
               by = istituto]
      he[, h := h - weighted.mean(h, np)]       # somma zero, pesata per sondaggi
      d[serie == s, h := he$h[match(istituto, he$istituto)]]
    }
    delta <- max(abs(d$h - h_vecchio))
    cat(sprintf("%s giro %2d  %s | delta_h %.4f\n", nome, it,
                paste(sprintf("%s: day %.3f extra %.3f", names(SD),
                              sapply(SD, `[`, 1), sapply(SD, `[`, 2)), collapse = " | "),
                delta))
    if (delta < 1e-3) break
  }
  serie <- rbindlist(lapply(unique(d$serie), function(s) {
    ds <- d[serie == s]
    k <- fit(ds, SD[[ds$gruppo[1]]][1], SD[[ds$gruppo[1]]][2])
    data.table(data = giorni, serie = s, media = round(k$m, 2), sd = round(k$sd, 2))[
      data >= min(ds$data_media)]               # dal primo sondaggio della serie
  }))
  serie[, giorni_al_voto := as.integer(VOTO - data)]
  list(serie = serie, d = d)
}

rc <- stima(coal, "coalizioni")
rp <- stima(part, "partiti")
rc$serie[, nome := NOMI_COAL[serie]]
rp$serie[, nome := NOMI_PART[serie]]
rp$serie[, blocco := BLOCCO[serie]]

fwrite(rc$serie, file.path(OUTPUT, "medie_coalizioni.csv"))
fwrite(rp$serie, file.path(OUTPUT, "medie_partiti.csv"))
salva_sond <- function(d, nomi, file) {
  s <- d[, .(istituto, data = data_media, serie, nome = nomi[serie], seggi = round(y, 2),
             effetto_istituto = round(h, 2), corretto = round(y - h, 2))]
  fwrite(s[order(data, serie)], file.path(OUTPUT, file))
}
salva_sond(rc$d, NOMI_COAL, "sondaggi_coalizioni.csv")
salva_sond(rp$d, NOMI_PART, "sondaggi_partiti.csv")

he <- rbind(unique(rc$d[, .(istituto, serie, h = round(h, 2))]),
            unique(rp$d[, .(istituto, serie, h = round(h, 2))]))
he <- dcast(he, istituto ~ serie, value.var = "h")
he <- merge(he, unique(g[, .(id, istituto)])[, .(sondaggi = .N), by = istituto], by = "istituto")
setcolorder(he, c("istituto", "sondaggi", "governo", "opposizione", "arabi",
                  names(NOMI_PART)))
fwrite(he[order(-sondaggi)], file.path(OUTPUT, "effetti_istituto.csv"))

# ── Controlli ────────────────────────────────────────────────────────────────
cat("\n-- Effetti istituto (seggi; + = sovrastima la serie) --\n")
print(he[order(-sondaggi), .(istituto, sondaggi, governo, opposizione, arabi,
                             likud, together, yashar, shas, otzma)], row.names = FALSE)

banda <- function(s) {
  u <- s[data == OGGI][order(-media)]
  u[, `:=`(lo = round(media - 1.645 * sd, 1), hi = round(media + 1.645 * sd, 1))]
  u[, .(nome, media, sd, lo, hi)]
}
cat("\n-- Coalizioni al", format(OGGI), "(seggi, banda 90%) --\n")
print(banda(rc$serie), row.names = FALSE)
cat(sprintf("Somma: %.1f\n", sum(rc$serie[data == OGGI, media])))
cat("\n-- Partiti al", format(OGGI), "(seggi, banda 90%) --\n")
print(banda(rp$serie), row.names = FALSE)
cat(sprintf("Somma: %.1f\n", sum(rp$serie[data == OGGI, media])))

cat("\n-- Tappe coalizioni --\n")
tappe <- rc$serie[data %in% as.Date(c("2026-01-15", "2026-03-01", "2026-04-15", "2026-06-01",
                                      "2026-07-15", "2026-09-01", "2026-10-01", "2026-10-23"))]
print(dcast(tappe, data ~ serie, value.var = "media"), row.names = FALSE)

cat("\n-- Confronto: media semplice ultimi 7 giorni, per istituto l'ultimo --\n")
ult <- rc$d[data_media >= OGGI - 7][order(-data_media)][, .SD[1], by = .(istituto, serie)]
print(ult[, .(semplice = round(mean(y), 1), istituti = .N), by = serie], row.names = FALSE)
