#!/usr/bin/env Rscript
# 12_sussidi.R
#
# Estrazione ESS: sussidi, welfare e presunti abusi, reddito di base.
# Variabili: sbstrec sbbsntx sbeqsoc sblazy sblwcoa sblwlka bennent lbenent
#            uentrjb basinc eudcnbf imsclbn
# Round 4 e 8 per i confronti internazionali (Italia presente solo al round 8).
#
# Regole vincolanti: vedi input/DATA_MAP.md
#  - dati SOLO da input/ess_slim.rds (data.table)
#  - filtrare sempre sui valori validi del codebook (i codici missing sono numeri)
#  - peso anweight sempre, anche per gli aggregati pooled ("Europa")
#  - percentuali a 1 decimale, N non ponderato riportato, N<100 segnalato (vedi
#    riepilogo a fine script), N<50 non riportato (riga scartata)

suppressPackageStartupMessages(library(data.table))

DIR <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Opinioni economiche ESS"
IN  <- file.path(DIR, "input", "ess_slim.rds")
OUT <- file.path(DIR, "output", "estrazioni")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

d <- readRDS(IN)
stopifnot(is.data.table(d))

TEMA <- "sussidi"

anno_map <- c(`1` = 2002, `2` = 2004, `3` = 2006, `4` = 2008, `5` = 2010, `6` = 2012,
              `7` = 2014, `8` = 2016, `9` = 2018, `10` = 2021, `11` = 2023)

# "Europa": UE27 + UK + Norvegia + Svizzera + Islanda (presenti nel round in questione)
EUROPA_LIST <- c("AT","BE","BG","CH","CY","CZ","DE","DK","EE","ES","FI","FR","GB","GR","HR","HU",
                  "IE","IS","IT","LT","LU","LV","NL","NO","PL","PT","RO","SE","SI","SK")
# Panel fisso di 15 paesi presenti in tutti gli 11 round, per le serie storiche
PANEL15 <- c("BE","CH","DE","ES","FI","FR","GB","HU","IE","NL","NO","PL","PT","SE","SI")

# --- Etichette italiane delle variabili (tradotte dal codebook) ---
label_it <- c(
  sbstrec = "I benefici sociali pesano troppo sull'economia",
  sbbsntx = "I benefici sociali costano troppo alle imprese in tasse",
  sbeqsoc = "I benefici sociali portano a una società più equa",
  sblazy  = "I benefici sociali rendono le persone pigre",
  sblwcoa = "I benefici sociali rendono le persone meno disposte a prendersi cura le une delle altre",
  sblwlka = "I benefici sociali rendono le persone meno disposte a badare a sé stesse/famiglia",
  bennent = "Molti riescono a ottenere benefici a cui non hanno diritto",
  lbenent = "Molti con redditi molto bassi ottengono meno benefici di quanto avrebbero legalmente diritto",
  uentrjb = "La maggior parte dei disoccupati non cerca davvero lavoro",
  basinc  = "Favorevole a uno schema di reddito di base",
  eudcnbf = "Se decidesse più l'UE, il livello dei benefici sociali sarebbe più alto o più basso",
  imsclbn = "Quando gli immigrati dovrebbero avere accesso ai benefici sociali"
)

# NOTA IMPORTANTE su bennent/lbenent: il codebook ESS (verificato in
# codebook_variabili.json) definisce bennent come "molti OTTENGONO benefici a
# cui NON hanno diritto" (sospetto di abuso/frode) e lbenent come "molti con
# redditi bassi ottengono MENO benefici di quanto spetterebbe loro per legge"
# (sospetto di sotto-erogazione). Sono le direzioni concettualmente opposte
# rispetto alla glossa nella consegna del task, che sembra aver scambiato le
# due variabili. Qui si segue il codebook (fonte vincolante).

# =========================================================
# Funzioni generiche
# =========================================================

# Distribuzione ponderata (anweight) per categoria intera di `var`.
# `sub` è già filtrato sulla popolazione di interesse (paese/i + round).
pct_by_cat <- function(sub, var, valid_codes) {
  x <- sub[[var]]
  w <- sub$anweight
  ok <- !is.na(x) & x %in% valid_codes
  x <- x[ok]; w <- w[ok]
  n <- length(x)
  if (n == 0) return(list(n = 0, tab = data.table(cat_val = integer(0), pct = numeric(0))))
  tot <- sum(w)
  agg <- data.table(cat_val = x, w = w)[, .(w = sum(w)), by = cat_val]
  agg[, pct := round(w / tot * 100, 1)]
  list(n = n, tab = agg[order(cat_val)][, .(cat_val, pct)])
}

mk_row_skeleton <- function(var, essround_val, aggregato, gruppo, categoria, valore, tipo_valore, n) {
  data.table(
    tema = TEMA, variabile = var, label_it = label_it[[var]],
    essround = as.integer(essround_val), anno = anno_map[[as.character(essround_val)]],
    aggregato = aggregato, gruppo = gruppo, categoria = categoria,
    valore = as.numeric(valore), tipo_valore = tipo_valore, n_validi = as.integer(n)
  )
}

# Scale 1-5 "accordo...disaccordo" (sb*, bennent, lbenent, uentrjb):
# 1-2 accordo, 3 neutro, 4-5 disaccordo. Riga scartata se N<50.
mk_agree_rows <- function(sub, var, essround_val, aggregato, gruppo = "tutti") {
  r <- pct_by_cat(sub, var, 1:5)
  if (r$n < 50) return(NULL)
  tab <- r$tab
  band <- function(codes) sum(tab[cat_val %in% codes, pct])
  mk_row_skeleton(var, essround_val, aggregato, gruppo, NA_character_,
                   c(band(1:2), band(3), band(4:5)),
                   c("pct_accordo","pct_neutro","pct_disaccordo"), r$n)
}

# basinc: 1-2 contrario, 3-4 favorevole.
mk_favor_rows <- function(sub, var, essround_val, aggregato, gruppo = "tutti") {
  r <- pct_by_cat(sub, var, 1:4)
  if (r$n < 50) return(NULL)
  tab <- r$tab
  band <- function(codes) sum(tab[cat_val %in% codes, pct])
  mk_row_skeleton(var, essround_val, aggregato, gruppo, NA_character_,
                   c(band(1:2), band(3:4)),
                   c("pct_contrario","pct_favorevoli"), r$n)
}

# Dettaglio per singola categoria (basinc a 4, imsclbn/eudcnbf a 5).
mk_category_rows <- function(sub, var, valid_codes, cat_labels, essround_val, aggregato, gruppo = "tutti") {
  r <- pct_by_cat(sub, var, valid_codes)
  if (r$n < 50) return(NULL)
  tab <- r$tab
  mk_row_skeleton(var, essround_val, aggregato, gruppo,
                   cat_labels[as.character(tab$cat_val)],
                   tab$pct, "pct", r$n)
}

# Bande di sintesi personalizzate (liste nominate codice->codici) per variabili
# categoriali che non sono scale accordo/disaccordo (imsclbn, eudcnbf).
mk_custom_band <- function(sub, var, valid_codes, band_defs, essround_val, aggregato, gruppo = "tutti") {
  r <- pct_by_cat(sub, var, valid_codes)
  if (r$n < 50) return(NULL)
  tab <- r$tab
  vals <- vapply(band_defs, function(codes) sum(tab[cat_val %in% codes, pct]), numeric(1))
  mk_row_skeleton(var, essround_val, aggregato, gruppo, NA_character_,
                   vals, names(band_defs), r$n)
}

collect <- function(lst) {
  lst <- Filter(Negate(is.null), lst)
  if (length(lst) == 0) return(data.table())
  rbindlist(lst)
}

# =========================================================
# (a) sussidi_atteggiamenti.csv
#   IT vs DE FR ES GB + Europa al round 8; variazione round 4->8 per gli
#   altri paesi (Italia assente al round 4).
# =========================================================

vars_a <- c("sbstrec","sbbsntx","sbeqsoc","sblazy","sblwcoa","sblwlka","bennent","lbenent","uentrjb")

slices_a <- list(
  list(aggregato = "IT", cntry = "IT", round = 8),
  list(aggregato = "DE", cntry = "DE", round = 8),
  list(aggregato = "DE", cntry = "DE", round = 4),
  list(aggregato = "FR", cntry = "FR", round = 8),
  list(aggregato = "FR", cntry = "FR", round = 4),
  list(aggregato = "ES", cntry = "ES", round = 8),
  list(aggregato = "ES", cntry = "ES", round = 4),
  list(aggregato = "GB", cntry = "GB", round = 8),
  list(aggregato = "GB", cntry = "GB", round = 4),
  list(aggregato = "Europa",          cntry = EUROPA_LIST, round = 8),
  list(aggregato = "Europa-panel15",  cntry = PANEL15,      round = 8),
  list(aggregato = "Europa-panel15",  cntry = PANEL15,      round = 4)
)

rows_a <- list()
for (v in vars_a) {
  for (s in slices_a) {
    # sblwlka: item di rotazione presente SOLO al round 4 (in nessun paese al
    # round 8): saltare tutte le slice di round 8 per questa variabile.
    if (v == "sblwlka" && s$round == 8) next
    sub <- d[essround == s$round & cntry %in% s$cntry]
    rows_a[[length(rows_a) + 1]] <- mk_agree_rows(sub, v, s$round, s$aggregato)
  }
}
out_a <- collect(rows_a)
fwrite(out_a, file.path(OUT, "sussidi_atteggiamenti.csv"))
cat("Scritto sussidi_atteggiamenti.csv:", nrow(out_a), "righe\n")

# =========================================================
# Gruppi socio-demografici standard (solo Italia, round 8)
# =========================================================

add_groups <- function(dt) {
  dt <- copy(dt)
  dt[, grp_eta := fcase(
    agea >= 15 & agea <= 34, "eta:15-34",
    agea >= 35 & agea <= 54, "eta:35-54",
    agea >= 55 & agea < 999, "eta:55+",
    default = NA_character_
  )]
  dt[, grp_genere := fcase(
    gndr == 1, "genere:uomini",
    gndr == 2, "genere:donne",
    default = NA_character_
  )]
  dt[, grp_istruzione := fcase(
    eisced %in% 1:2, "istruzione:bassa",
    eisced %in% 3:4, "istruzione:media",
    eisced %in% 5:7, "istruzione:alta",
    default = NA_character_
  )]
  dt[, grp_condizione := fcase(
    mnactic == 1,          "condizione:occupati",
    mnactic %in% 3:4,       "condizione:disoccupati",
    mnactic == 6,           "condizione:pensionati",
    mnactic == 2,           "condizione:studenti",
    mnactic %in% c(5,7,8,9),"condizione:altro_inattivo",
    default = NA_character_
  )]
  dt[, grp_settore := fcase(
    tporgwk %in% 1:3, "settore:pubblico",
    tporgwk == 4,     "settore:privato",
    tporgwk == 5,     "settore:autonomi",
    default = NA_character_
  )]
  dt[, grp_reddito_perc := fcase(
    hincfel == 1, "reddito_percepito:vive_comodamente",
    hincfel == 2, "reddito_percepito:se_la_cava",
    hincfel == 3, "reddito_percepito:difficolta",
    hincfel == 4, "reddito_percepito:grande_difficolta",
    default = NA_character_
  )]
  dt[, grp_decile := fcase(
    hinctnta %in% 1:3,  "decile_reddito:basso",
    hinctnta %in% 4:7,  "decile_reddito:medio",
    hinctnta %in% 8:10, "decile_reddito:alto",
    default = NA_character_
  )]
  dt[, grp_sindacato := fcase(
    mbtru == 1, "sindacato:iscritto_ora",
    mbtru == 2, "sindacato:in_passato",
    mbtru == 3, "sindacato:mai",
    default = NA_character_
  )]
  dt[, grp_lr := fcase(
    lrscale %in% 0:3,  "posizione_politica:sinistra",
    lrscale %in% 4:6,  "posizione_politica:centro",
    lrscale %in% 7:10, "posizione_politica:destra",
    default = NA_character_
  )]
  dt
}

it8 <- add_groups(d[cntry == "IT" & essround == 8])
group_cols <- c("grp_eta","grp_genere","grp_istruzione","grp_condizione","grp_settore",
                 "grp_reddito_perc","grp_decile","grp_sindacato","grp_lr")

# =========================================================
# (b) sussidi_basinc.csv
#   b1) classifica % favorevoli, tutti i paesi UE/EFTA+UK round 8 + Europa
#       pooled; dettaglio per le 4 categorie per IT/DE/FR/ES/GB/Europa
#   b2) Italia: breakdown sociodemografico standard
# =========================================================

r8 <- d[essround == 8]
rows_b <- list()

countries_r8 <- intersect(EUROPA_LIST, sort(unique(r8$cntry)))
for (cc in countries_r8) {
  rows_b[[length(rows_b) + 1]] <- mk_favor_rows(r8[cntry == cc], "basinc", 8, cc)
}
rows_b[[length(rows_b) + 1]] <- mk_favor_rows(r8[cntry %in% EUROPA_LIST], "basinc", 8, "Europa")

basinc_labels <- c(`1` = "Fortemente contrario", `2` = "Contrario",
                    `3` = "Favorevole", `4` = "Fortemente favorevole")
aggs_main <- list(IT = "IT", DE = "DE", FR = "FR", ES = "ES", GB = "GB", Europa = EUROPA_LIST)
for (nm in names(aggs_main)) {
  sub <- r8[cntry %in% aggs_main[[nm]]]
  rows_b[[length(rows_b) + 1]] <- mk_category_rows(sub, "basinc", 1:4, basinc_labels, 8, nm)
}

for (gc in group_cols) {
  livelli <- sort(unique(na.omit(it8[[gc]])))
  for (lv in livelli) {
    rows_b[[length(rows_b) + 1]] <- mk_favor_rows(it8[get(gc) == lv], "basinc", 8, "IT", gruppo = lv)
  }
}

out_b <- collect(rows_b)
fwrite(out_b, file.path(OUT, "sussidi_basinc.csv"))
cat("Scritto sussidi_basinc.csv:", nrow(out_b), "righe\n")

# =========================================================
# (c) sussidi_sociodemo_it.csv
#   sblazy e uentrjb, Italia round 8, per gruppi sociodemografici standard
#   (il sospetto verso i beneficiari, per gruppo)
# =========================================================

rows_c <- list()
for (v in c("sblazy","uentrjb")) {
  rows_c[[length(rows_c) + 1]] <- mk_agree_rows(it8, v, 8, "IT", gruppo = "tutti")
  for (gc in group_cols) {
    livelli <- sort(unique(na.omit(it8[[gc]])))
    for (lv in livelli) {
      rows_c[[length(rows_c) + 1]] <- mk_agree_rows(it8[get(gc) == lv], v, 8, "IT", gruppo = lv)
    }
  }
}
out_c <- collect(rows_c)
fwrite(out_c, file.path(OUT, "sussidi_sociodemo_it.csv"))
cat("Scritto sussidi_sociodemo_it.csv:", nrow(out_c), "righe\n")

# =========================================================
# (d) sussidi_confini.csv
#   imsclbn e eudcnbf, IT vs DE FR ES GB + Europa, round 8
#   (welfare e confini: accesso degli immigrati ai benefici; effetto di più
#   decisioni UE sul livello dei benefici)
# =========================================================

imsclbn_labels <- c(
  `1` = "Subito all'arrivo",
  `2` = "Dopo un anno, indipendentemente dal lavoro",
  `3` = "Dopo aver lavorato e pagato le tasse per almeno un anno",
  `4` = "Una volta ottenuta la cittadinanza",
  `5` = "Mai gli stessi diritti"
)
eudcnbf_labels <- c(
  `1` = "Molto più alti",
  `2` = "Più alti",
  `3` = "Né più alti né più bassi",
  `4` = "Più bassi",
  `5` = "Molto più bassi"
)

rows_d <- list()
for (nm in names(aggs_main)) {
  sub <- r8[cntry %in% aggs_main[[nm]]]

  rows_d[[length(rows_d) + 1]] <- mk_category_rows(sub, "imsclbn", 1:5, imsclbn_labels, 8, nm)
  rows_d[[length(rows_d) + 1]] <- mk_custom_band(
    sub, "imsclbn", 1:5,
    list(pct_mai_stessi_diritti = 5,
         pct_restrittivo_cittadinanza_o_mai = 4:5,
         pct_generoso_subito_o_dopo_un_anno = 1:2),
    8, nm
  )

  rows_d[[length(rows_d) + 1]] <- mk_category_rows(sub, "eudcnbf", 1:5, eudcnbf_labels, 8, nm)
  rows_d[[length(rows_d) + 1]] <- mk_custom_band(
    sub, "eudcnbf", 1:5,
    list(pct_piu_alti = 1:2, pct_piu_bassi = 4:5, pct_invariato = 3),
    8, nm
  )
}
out_d <- collect(rows_d)
fwrite(out_d, file.path(OUT, "sussidi_confini.csv"))
cat("Scritto sussidi_confini.csv:", nrow(out_d), "righe\n")

# =========================================================
# Riepilogo celle piccole (N non ponderato < 100) su tutti gli output
# =========================================================
tutti <- rbindlist(list(out_a, out_b, out_c, out_d), fill = TRUE)
piccole <- unique(tutti[n_validi < 100, .(variabile, essround, aggregato, gruppo, n_validi)])
if (nrow(piccole) > 0) {
  cat("\n--- Celle con N < 100 (segnalate, non scartate: N>=50 sempre riportato) ---\n")
  print(piccole[order(variabile, essround, aggregato, gruppo)])
} else {
  cat("\nNessuna cella con N < 100.\n")
}
cat("\nRighe totali per file: a =", nrow(out_a), " b =", nrow(out_b),
    " c =", nrow(out_c), " d =", nrow(out_d), "\n")
cat("Fatto.\n")
