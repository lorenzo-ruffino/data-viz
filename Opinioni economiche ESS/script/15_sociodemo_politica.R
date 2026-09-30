#!/usr/bin/env Rscript
# ==============================================================================
# 15_sociodemo_politica.R
#
# TEMA: sociodemo-politica — la mappa dei gruppi sociali dentro l'Italia
# (età, genere, istruzione, condizione occupazionale, settore, reddito
# percepito, decili di reddito, iscrizione sindacale, collocazione politica),
# incluso il voto alle politiche 2022, su 7 variabili-chiave di opinione
# economica ESS (round 8, 9, 11).
#
# Regole applicate (vedi input/DATA_MAP.md):
# - Dati: SOLO input/ess_slim.rds (data.table), MAI il CSV grezzo.
# - Codici missing (7/8/9, 77/88/99, 666...) sono numeri: si filtra sempre
#   sui valori validi elencati nel codebook JSON, mai su NA implicito.
# - Pesi: anweight (stime per singolo paese, qui sempre IT).
# - Percentuali con 1 decimale, media con 2 decimali; N sempre non ponderato.
# - Celle piccole: N<100 segnalato (resta visibile via n_validi), N<50 escluso
#   dall'output (riga non scritta).
# ==============================================================================

suppressPackageStartupMessages(library(data.table))

DIR     <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Opinioni economiche ESS"
OUT_DIR <- file.path(DIR, "output", "estrazioni")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

TEMA         <- "sociodemo-politica"
N_MIN_REPORT <- 50   # sotto questa soglia: riga scartata
N_FLAG       <- 100  # sotto questa soglia: da segnalare (visibile via n_validi)

# round -> anno fieldwork (da DATA_MAP.md)
anno_di <- c(`1` = 2002, `2` = 2004, `3` = 2006, `4` = 2008, `5` = 2010,
             `6` = 2012, `7` = 2014, `8` = 2016, `9` = 2018, `10` = 2021, `11` = 2023)

# ------------------------------------------------------------------------------
# dati
# ------------------------------------------------------------------------------
d <- readRDS(file.path(DIR, "input", "ess_slim.rds"))
setDT(d)
stopifnot(is.data.table(d))
stopifnot(d[, sum(is.na(anweight))] == 0)

it <- d[cntry == "IT"]
cat("Righe Italia:", nrow(it), " | round presenti:", paste(sort(unique(it$essround)), collapse = ","), "\n")

# ------------------------------------------------------------------------------
# helper di calcolo pesato (anweight)
# ------------------------------------------------------------------------------
wpct <- function(w, cond) {
  tot <- sum(w)
  if (tot <= 0 || length(w) == 0) return(NA_real_)
  round(100 * sum(w[cond]) / tot, 1)
}
wmean <- function(x, w) {
  if (length(x) == 0) return(NA_real_)
  round(stats::weighted.mean(x, w), 2)
}

# ------------------------------------------------------------------------------
# metadati delle 7 variabili-chiave: round, tipo di scala (da DATA_MAP.md),
# label italiano (tradotto dal codebook), valori validi (da codebook JSON)
# ------------------------------------------------------------------------------
vars_meta <- list(
  gincdif = list(round = 11, type = "agree5",   valid = 1:5,
                 label_it = "Il governo dovrebbe ridurre le differenze di reddito"),
  basinc  = list(round = 8,  type = "favore4",  valid = 1:4,
                 label_it = "Favorevole a un reddito di base universale"),
  gvslvue = list(round = 8,  type = "scale010", valid = 0:10,
                 label_it = "Responsabilità del governo per lo standard di vita dei disoccupati"),
  sblazy  = list(round = 8,  type = "agree5",   valid = 1:5,
                 label_it = "I sussidi sociali rendono le persone pigre"),
  topinfr = list(round = 9,  type = "fair44",   valid = -4:4,
                 label_it = "Equità del reddito del 10% di occupati più pagati"),
  sofrdst = list(round = 9,  type = "agree5",   valid = 1:5,
                 label_it = "Una società è giusta se redditi e ricchezza sono distribuiti equamente"),
  stfeco  = list(round = 11, type = "scale010", valid = 0:10,
                 label_it = "Soddisfazione per lo stato dell'economia")
)

# ------------------------------------------------------------------------------
# calcolo delle statistiche di una variabile-chiave su un vettore già
# filtrato sui valori validi (x, w stessa lunghezza). Restituisce un data.table
# con una riga per tipo_valore (convenzioni DATA_MAP.md per tipo di scala).
# ------------------------------------------------------------------------------
calc_stats <- function(x, w, type) {
  n <- length(x)
  rows <- list()
  if (type == "agree5") {
    rows[[1]] <- list(tipo_valore = "pct_accordo",   categoria = "d'accordo (1-2)",                       valore = wpct(w, x %in% 1:2))
    rows[[2]] <- list(tipo_valore = "pct_neutro",    categoria = "né d'accordo né in disaccordo (3)", valore = wpct(w, x == 3))
    rows[[3]] <- list(tipo_valore = "pct_contrario", categoria = "in disaccordo (4-5)",                   valore = wpct(w, x %in% 4:5))
  } else if (type == "favore4") {
    rows[[1]] <- list(tipo_valore = "pct_favorevoli", categoria = "favorevole (3-4)", valore = wpct(w, x %in% 3:4))
    rows[[2]] <- list(tipo_valore = "pct_contrario",  categoria = "contrario (1-2)",  valore = wpct(w, x %in% 1:2))
  } else if (type == "scale010") {
    rows[[1]] <- list(tipo_valore = "media",    categoria = "media (0-10)",       valore = wmean(x, w))
    rows[[2]] <- list(tipo_valore = "pct_7_10", categoria = "punteggio alto (7-10)", valore = wpct(w, x %in% 7:10))
  } else if (type == "fair44") {
    rows[[1]] <- list(tipo_valore = "media",        categoria = "media (-4..+4)",              valore = wmean(x, w))
    rows[[2]] <- list(tipo_valore = "pct_negativo", categoria = "ingiustamente basso (-4..-1)", valore = wpct(w, x < 0))
    rows[[3]] <- list(tipo_valore = "pct_zero",     categoria = "giusto (0)",                   valore = wpct(w, x == 0))
    rows[[4]] <- list(tipo_valore = "pct_positivo", categoria = "ingiustamente alto (1..4)",    valore = wpct(w, x > 0))
  } else {
    stop("tipo scala non gestito: ", type)
  }
  rbindlist(lapply(rows, function(r) data.table(tipo_valore = r$tipo_valore, categoria = r$categoria,
                                                 valore = r$valore, n_validi = n)))
}

COLONNE <- c("tema","variabile","label_it","essround","anno","aggregato","gruppo","categoria","valore","tipo_valore","n_validi")

# ------------------------------------------------------------------------------
# assi sociodemografici standard (DATA_MAP.md, per l'Italia)
# ------------------------------------------------------------------------------
axes <- list(
  eta = list(var = "agea", order = c("15-34","35-54","55+"),
    cat = function(x) fcase(x >= 15 & x <= 34, "15-34",
                             x >= 35 & x <= 54, "35-54",
                             x >= 55 & x <= 110, "55+")),
  genere = list(var = "gndr", order = c("uomini","donne"),
    cat = function(x) fcase(x == 1, "uomini", x == 2, "donne")),
  istruzione = list(var = "eisced", order = c("bassa","media","alta"),
    cat = function(x) fcase(x %in% 1:2, "bassa", x %in% 3:4, "media", x %in% 5:7, "alta")),
  condizione = list(var = "mnactic", order = c("occupati","disoccupati","pensionati","studenti","altro_inattivo"),
    cat = function(x) fcase(x == 1, "occupati",
                             x %in% c(3,4), "disoccupati",
                             x == 6, "pensionati",
                             x == 2, "studenti",
                             x %in% c(5,7,8,9), "altro_inattivo")),
  settore = list(var = "tporgwk", order = c("pubblico","privato","autonomi"),
    cat = function(x) fcase(x %in% 1:3, "pubblico", x == 4, "privato", x == 5, "autonomi")),
  reddito_percepito = list(var = "hincfel", order = c("comoda","adeguata","difficolta","grande_difficolta"),
    cat = function(x) fcase(x == 1, "comoda", x == 2, "adeguata", x == 3, "difficolta", x == 4, "grande_difficolta")),
  decile = list(var = "hinctnta", order = c("basso","medio","alto"),
    cat = function(x) fcase(x %in% 1:3, "basso", x %in% 4:7, "medio", x %in% 8:10, "alto")),
  sindacato = list(var = "mbtru", order = c("iscritto","ex_iscritto","mai_iscritto"),
    cat = function(x) fcase(x == 1, "iscritto", x == 2, "ex_iscritto", x == 3, "mai_iscritto")),
  ideologia = list(var = "lrscale", order = c("sinistra","centro","destra"),
    cat = function(x) fcase(x >= 0 & x <= 3, "sinistra", x >= 4 & x <= 6, "centro", x >= 7 & x <= 10, "destra"))
)

# ==============================================================================
# (a) TABELLA COMPLETA gruppo x variabile -> gruppi_quadro_completo.csv
# ==============================================================================
risultati <- list()
scartati_quadro <- list()

for (vn in names(vars_meta)) {
  meta <- vars_meta[[vn]]
  sub  <- it[essround == meta$round]
  raw  <- sub[[vn]]
  valid_mask <- raw %in% meta$valid

  # --- tutti (baseline Italia, round della variabile) ---
  x_all <- raw[valid_mask]; w_all <- sub$anweight[valid_mask]
  st <- calc_stats(x_all, w_all, meta$type)
  st[, `:=`(tema = TEMA, variabile = vn, label_it = meta$label_it,
            essround = meta$round, anno = anno_di[[as.character(meta$round)]],
            aggregato = "IT", gruppo = "tutti")]
  risultati[[length(risultati) + 1]] <- st

  # --- assi sociodemo standard ---
  for (an in names(axes)) {
    ax <- axes[[an]]
    grp_cat <- ax$cat(sub[[ax$var]])
    for (lvl in ax$order) {
      mask <- valid_mask & !is.na(grp_cat) & grp_cat == lvl
      n <- sum(mask)
      if (n < N_MIN_REPORT) {
        scartati_quadro[[length(scartati_quadro) + 1]] <- data.table(variabile = vn, gruppo = paste0(an, ":", lvl), n = n)
        next
      }
      x <- raw[mask]; w <- sub$anweight[mask]
      st <- calc_stats(x, w, meta$type)
      st[, `:=`(tema = TEMA, variabile = vn, label_it = meta$label_it,
                essround = meta$round, anno = anno_di[[as.character(meta$round)]],
                aggregato = "IT", gruppo = paste0(an, ":", lvl))]
      risultati[[length(risultati) + 1]] <- st
    }
  }
}

quadro <- rbindlist(risultati)
setcolorder(quadro, COLONNE)
setorder(quadro, variabile, gruppo, tipo_valore)
fwrite(quadro, file.path(OUT_DIR, "gruppi_quadro_completo.csv"))
cat("\nScritto gruppi_quadro_completo.csv:", nrow(quadro), "righe\n")
if (length(scartati_quadro)) {
  cat("Gruppi scartati per N<", N_MIN_REPORT, ":\n", sep = "")
  print(rbindlist(scartati_quadro))
}
cat("Gruppi con N<", N_FLAG, " (da segnalare, presenti nell'output con n_validi visibile):\n", sep = "")
print(unique(quadro[n_validi < N_FLAG, .(variabile, gruppo, n_validi)]))

# ==============================================================================
# (b) ELETTORATO round 11: gincdif e stfeco per partito votato alle politiche
#     2022 (prtvteit) -> gruppi_voto.csv
# ==============================================================================
# prtvteit (round 11, unica variante con FdI/PD/M5S/Lega/FI/Terzo Polo/AVS):
# 1 FdI, 2 PD, 3 M5S, 4 Lega, 5 FI, 6 Terzo Polo (Azione-Italia Viva),
# 7 Alleanza Verdi e Sinistra, 8 +Europa, 9 Italexit, 10 Unione Popolare,
# 11 Italia Sovrana e Popolare, 31 Altro; 66/77/88/99 missing.
# Nota: round 10 (prtvtdit, elezioni 2018) verificato nel codebook ma non
# utilizzato: la mappatura partiti richiesta (FdI/PD/M5S/Lega/FI/Azione/
# Italia Viva/AVS) corrisponde solo alle politiche 2022, quindi solo a
# prtvteit (round 11).
partiti_map <- function(x) fcase(
  x == 1, "FdI",
  x == 2, "PD",
  x == 3, "M5S",
  x == 4, "Lega",
  x == 5, "FI",
  x == 6, "Azione-IV",
  x == 7, "AVS",
  x %in% c(8, 9, 10, 11, 31), "Altri"
)
partiti_order <- c("FdI","PD","M5S","Lega","FI","Azione-IV","AVS","Altri")

r11 <- it[essround == 11]
voto_cat <- partiti_map(r11$prtvteit)

voto_risultati <- list()
scartati_voto <- list()

for (vn in c("gincdif","stfeco")) {
  meta <- vars_meta[[vn]]
  raw  <- r11[[vn]]
  valid_mask <- raw %in% meta$valid

  # tutti (baseline round 11, per confronto con le singole basi elettorali)
  x_all <- raw[valid_mask]; w_all <- r11$anweight[valid_mask]
  st <- calc_stats(x_all, w_all, meta$type)
  st[, `:=`(tema = TEMA, variabile = vn, label_it = meta$label_it,
            essround = 11, anno = anno_di[["11"]], aggregato = "IT", gruppo = "tutti")]
  voto_risultati[[length(voto_risultati) + 1]] <- st

  for (p in partiti_order) {
    mask <- valid_mask & !is.na(voto_cat) & voto_cat == p
    n <- sum(mask)
    if (n < N_MIN_REPORT) {
      scartati_voto[[length(scartati_voto) + 1]] <- data.table(variabile = vn, partito = p, n = n)
      next
    }
    x <- raw[mask]; w <- r11$anweight[mask]
    st <- calc_stats(x, w, meta$type)
    st[, `:=`(tema = TEMA, variabile = vn, label_it = meta$label_it,
              essround = 11, anno = anno_di[["11"]], aggregato = "IT", gruppo = paste0("voto:", p))]
    voto_risultati[[length(voto_risultati) + 1]] <- st
  }
}

voto <- rbindlist(voto_risultati)
setcolorder(voto, COLONNE)
setorder(voto, variabile, gruppo, tipo_valore)
fwrite(voto, file.path(OUT_DIR, "gruppi_voto.csv"))
cat("\nScritto gruppi_voto.csv:", nrow(voto), "righe\n")
cat("Partiti scartati per N<", N_MIN_REPORT, " (unweighted):\n", sep = "")
print(rbindlist(scartati_voto))
cat("N grezzo per partito, round 11 (prima del filtro):\n")
print(table(voto_cat, useNA = "ifany"))

# ==============================================================================
# (c) diagnostica per identificare i contrasti più forti e il consenso
#     trasversale (stampata a console, usata per il riepilogo finale)
# ==============================================================================
cat("\n\n=== DIAGNOSTICA CONTRASTI (pct_accordo / pct_favorevoli / media / pct_7_10, asse per asse) ===\n")
for (vn in names(vars_meta)) {
  meta <- vars_meta[[vn]]
  tv_principale <- switch(meta$type, agree5 = "pct_accordo", favore4 = "pct_favorevoli",
                           scale010 = "media", fair44 = "media")
  sub_q <- quadro[variabile == vn & tipo_valore == tv_principale & gruppo != "tutti"]
  if (nrow(sub_q) == 0) next
  baseline <- quadro[variabile == vn & tipo_valore == tv_principale & gruppo == "tutti", valore]
  cat(sprintf("\n--- %s (%s, round %d, tutti=%.1f) ---\n", vn, tv_principale, meta$round, baseline))
  sub_q[, asse := sub("^(.*):.*$", "\\1", gruppo)]
  for (asse_n in unique(sub_q$asse)) {
    rows <- sub_q[asse == asse_n][order(-valore)]
    cat(sprintf("  %-20s range=%.1f (min %s=%.1f, max %s=%.1f)\n",
                asse_n, max(rows$valore) - min(rows$valore),
                rows$gruppo[which.min(rows$valore)], min(rows$valore),
                rows$gruppo[which.max(rows$valore)], max(rows$valore)))
  }
}

cat("\n\n=== DIAGNOSTICA VOTO (gincdif pct_accordo, stfeco media, per partito) ===\n")
print(voto[gruppo != "tutti" & tipo_valore %in% c("pct_accordo","media")][order(variabile, -valore)])

cat("\nScript completato senza errori.\n")
