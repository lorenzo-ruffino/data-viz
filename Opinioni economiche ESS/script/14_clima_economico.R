# 14_clima_economico.R
# ------------------------------------------------------------------
# Clima economico e politico di sfondo, ESS round 1-11 (2002-2023)
# Variabili: stfeco, stfgov, hincfel, trstprl, trstplt, euftf, lrscale, imbgeco
#
# Fonti obbligatorie (vedi input/DATA_MAP.md):
#   - input/ess_slim.rds (data.table, MAI il CSV da 1,7 GB)
#   - input/codebook_variabili.json (valori validi / missing per variabile)
#   - input/presenza_compatta_italia.csv (round in cui l'Italia ha dati)
#
# Convenzioni: pesi anweight sempre; filtro sui valori validi da codebook
# (i codici missing 7/8/9, 77/88/99 sono NUMERI nel dataset, non NA);
# percentuali a 1 decimale; N non ponderato riportato sempre; gruppi con
# N<100 segnalati (vedi commento finale), N<50 esclusi dal CSV.
#
# Output: clima_serie.csv, clima_lrscale.csv, clima_europa_immigrazione.csv,
#         clima_sociodemo_it.csv, clima_panel_copertura.csv (diagnostica sulla
#         composizione del panel europeo, vedi nota sotto).
# ------------------------------------------------------------------

suppressPackageStartupMessages(library(data.table))

DIR <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Opinioni economiche ESS"
OUT <- file.path(DIR, "output", "estrazioni")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

d <- readRDS(file.path(DIR, "input", "ess_slim.rds"))
cat("Caricate", nrow(d), "righe da ess_slim.rds\n")

# ---- Round -> anno (verifica coerenza con colonna anno gia' presente) ----
anni_map <- c(`1` = 2002, `2` = 2004, `3` = 2006, `4` = 2008, `5` = 2010,
              `6` = 2012, `7` = 2014, `8` = 2016, `9` = 2018, `10` = 2021, `11` = 2023)

# ---- Aggregati paese (da DATA_MAP.md) ----
panel15 <- c("BE","CH","DE","ES","FI","FR","GB","HU","IE","NL","NO","PL","PT","SE","SI")
europa_full <- c("AT","BE","BG","CH","CY","CZ","DE","DK","EE","ES","FI","FR","GB","GR","HR","HU",
                  "IE","IS","IT","LT","LU","LV","NL","NO","PL","PT","RO","SE","SI","SK")

# ---- ATTENZIONE: composizione costante delle serie storiche ---------------
# Il panel dei 15 paesi di DATA_MAP.md garantisce che il PAESE sia presente in
# tutti gli 11 round, NON che la VARIABILE sia stata somministrata (o abbia
# risposte valide) in tutti gli 11 round. Filtrare su `cntry %in% panel15`
# produce quindi serie a composizione variabile: due casi reali in questo
# script (verificati empiricamente sui dati, vedi clima_panel_copertura.csv)
#   - hincfel: FR ha 0 risposte valide nel round 1 (tutti i 1.503 rispondenti
#     hanno codice 9 = "No answer") e nel round 2 (colonna interamente NA).
#     Con il filtro sul paese i punti 2002 e 2004 stanno su 14 paesi e dal 2006
#     su 15: la Francia (~50 mln su ~330 del panel, e con quota di "difficolta"
#     sistematicamente sotto la media) rientra di colpo e crea un salto finto.
#   - stfgov: IE ha 0 risposte valide nel round 1 (colonna interamente NA).
# Regola applicata sotto: per OGNI variabile si tiene solo il sottoinsieme di
# panel15 che ha almeno una risposta valida in TUTTI gli 11 round, e lo stesso
# sottoinsieme viene usato per tutti i punti di quella serie.

# ---- Range validi (verificati empiricamente contro il codebook: i codici
#      missing compaiono come numeri nel dataset, es. stfeco IT ha 77/88/99) ----
valid0_10     <- 0:10   # stfeco, stfgov, trstprl, trstplt, euftf, lrscale, imbgeco (missing 77/88/99)
valid_hincfel <- 1:4    # hincfel (missing 7/8/9)

N_MIN <- 50   # non riportare stime con N < 50 (DATA_MAP.md)

lbl <- c(
  stfeco  = "Soddisfazione per lo stato dell'economia (0-10)",
  stfgov  = "Soddisfazione per l'operato del governo (0-10)",
  hincfel = "Percezione del reddito familiare (1-4)",
  trstprl = "Fiducia nel parlamento (0-10)",
  trstplt = "Fiducia nei politici (0-10)",
  euftf   = "Integrazione europea: gia' troppo oltre vs andare oltre (0-10, 10=piu' integrazione)",
  lrscale = "Collocazione sinistra-destra (0-10)",
  imbgeco = "Immigrazione, effetto sull'economia (0-10, 10=positivo)"
)

# ---- Helper pesati ----
wmean <- function(x, w) sum(as.numeric(x) * w) / sum(w)
wpct  <- function(mask, w) sum(w[mask]) / sum(w) * 100

# Nota: il parametro si chiama "ess_round" (non "round") per non mascherare
# la funzione base round() usata per l'arrotondamento.
row_media <- function(dt, var, aggregato, ess_round, gruppo = "tutti") {
  n <- nrow(dt)
  if (n < N_MIN) return(NULL)
  val <- wmean(dt[[var]], dt$anweight)
  data.table(tema = "clima-economico", variabile = var, label_it = lbl[[var]],
             essround = ess_round, anno = anni_map[[as.character(ess_round)]],
             aggregato = aggregato, gruppo = gruppo, categoria = NA_character_,
             valore = round(val, 2), tipo_valore = "media", n_validi = n)
}

row_pct <- function(dt, var, mask, cat_label, tipo, aggregato, ess_round, gruppo = "tutti") {
  n <- nrow(dt)
  if (n < N_MIN) return(NULL)
  val <- wpct(mask, dt$anweight)
  data.table(tema = "clima-economico", variabile = var, label_it = lbl[[var]],
             essround = ess_round, anno = anni_map[[as.character(ess_round)]],
             aggregato = aggregato, gruppo = gruppo, categoria = cat_label,
             valore = round(val, 1), tipo_valore = tipo, n_validi = n)
}

# ==================================================================
# a) Serie storiche: stfeco, stfgov, trstprl, trstplt (media ponderata)
#    e hincfel (% "difficolta"=3-4) per IT, DE, FR, ES, GB + aggregato europeo
#    a composizione costante (panel15 dove possibile, panel ridotto dove la
#    variabile non copre tutti e 15 i paesi in tutti gli 11 round).
#    Nota: trstplt aggiunta alla serie insieme a trstprl (nominata in coppia
#    nell'elenco variabili del tema, assente solo dall'elenco letterale del
#    punto a) — stesso trattamento, stessa scala 0-10, stesso costo nullo).
# ==================================================================
paesi_bilaterali <- c("IT", "DE", "FR", "ES", "GB")
serie_rows <- list()

# ---- Panel a composizione costante, variabile per variabile ---------------
serie_vars <- list(stfeco  = valid0_10, stfgov  = valid0_10, trstprl = valid0_10,
                   trstplt = valid0_10, hincfel = valid_hincfel)

# Copertura: N valide per variabile x round x paese del panel
cop <- rbindlist(lapply(names(serie_vars), function(v) {
  ok <- serie_vars[[v]]
  d[cntry %in% panel15, .(variabile = v, n_validi = sum(get(v) %in% ok, na.rm = TRUE)),
    by = .(essround, cntry)]
}))

# Un paese entra nel panel di una variabile solo se ha risposte valide in TUTTI
# gli 11 round. (n_validi == 0 = variabile mai somministrata o mai risposta.)
panel_var <- lapply(names(serie_vars), function(v) {
  per_paese <- cop[variabile == v, .(round_con_dati = sum(n_validi > 0)), by = cntry]
  sort(per_paese[round_con_dati == 11, cntry])
})
names(panel_var) <- names(serie_vars)

# Etichetta dell'aggregato: "Europa-panel15" solo se il panel resta completo,
# altrimenti nome esplicito con i paesi esclusi (es. "Europa-panel14-noFR").
nome_panel <- function(pp) {
  esclusi <- setdiff(panel15, pp)
  if (length(esclusi) == 0) "Europa-panel15"
  else paste0("Europa-panel", length(pp), "-no", paste(esclusi, collapse = ""))
}
agg_var <- vapply(panel_var, nome_panel, character(1))

cat("\n=== Composizione dei panel per la serie storica europea ===\n")
for (v in names(serie_vars)) {
  esclusi <- setdiff(panel15, panel_var[[v]])
  cat(sprintf("  %-8s -> %-22s (%2d paesi)%s\n", v, agg_var[[v]], length(panel_var[[v]]),
              if (length(esclusi)) paste0("  esclusi: ", paste(esclusi, collapse = ", ")) else ""))
  if (length(esclusi)) {
    buchi <- cop[variabile == v & cntry %in% esclusi & n_validi == 0]
    for (i in seq_len(nrow(buchi)))
      cat(sprintf("       %s: 0 risposte valide nel round %d (%d)\n",
                  buchi$cntry[i], buchi$essround[i], anni_map[[as.character(buchi$essround[i])]]))
  }
}
# Guardia: se il panel si svuota troppo la serie non e' piu' rappresentativa.
stopifnot(all(lengths(panel_var) >= 12))

# Diagnostica di copertura salvata su file (input della verifica successiva)
cop_out <- cop[, .(tema = "clima-economico", variabile, essround,
                   anno = anni_map[as.character(essround)], cntry, n_validi)][order(variabile, essround, cntry)]
cop_out[, paese_nel_panel := mapply(function(v, c) c %in% panel_var[[v]], variabile, cntry)]
cop_out[, panel_usato := agg_var[variabile]]
fwrite(cop_out, file.path(OUT, "clima_panel_copertura.csv"))
cat("clima_panel_copertura.csv:", nrow(cop_out), "righe\n\n")

for (r in 1:11) {
  for (p in paesi_bilaterali) {
    base <- d[cntry == p & essround == r]
    if (nrow(base) == 0) next  # es. IT nei round 2,3,4,5,7

    dt <- base[stfeco %in% valid0_10]
    row <- row_media(dt, "stfeco", p, r); if (!is.null(row)) serie_rows[[length(serie_rows)+1]] <- row

    dt <- base[stfgov %in% valid0_10]
    row <- row_media(dt, "stfgov", p, r); if (!is.null(row)) serie_rows[[length(serie_rows)+1]] <- row

    dt <- base[trstprl %in% valid0_10]
    row <- row_media(dt, "trstprl", p, r); if (!is.null(row)) serie_rows[[length(serie_rows)+1]] <- row

    dt <- base[trstplt %in% valid0_10]
    row <- row_media(dt, "trstplt", p, r); if (!is.null(row)) serie_rows[[length(serie_rows)+1]] <- row

    dt <- base[hincfel %in% valid_hincfel]
    row <- row_pct(dt, "hincfel", dt$hincfel %in% 3:4, "difficolta", "pct_difficolta", p, r)
    if (!is.null(row)) serie_rows[[length(serie_rows)+1]] <- row
  }

  # Aggregato europeo pooled (anweight pondera gia' per popolazione).
  # Il panel e' scelto PER VARIABILE, non per presenza del paese nel round:
  # cosi' ogni serie ha la stessa composizione in tutti gli 11 punti.
  for (v in c("stfeco", "stfgov", "trstprl", "trstplt")) {
    dt <- d[cntry %in% panel_var[[v]] & essround == r][get(v) %in% valid0_10]
    row <- row_media(dt, v, agg_var[[v]], r)
    if (!is.null(row)) serie_rows[[length(serie_rows)+1]] <- row
  }

  dt <- d[cntry %in% panel_var[["hincfel"]] & essround == r][hincfel %in% valid_hincfel]
  row <- row_pct(dt, "hincfel", dt$hincfel %in% 3:4, "difficolta", "pct_difficolta",
                 agg_var[["hincfel"]], r)
  if (!is.null(row)) serie_rows[[length(serie_rows)+1]] <- row
}

clima_serie <- rbindlist(serie_rows)
fwrite(clima_serie, file.path(OUT, "clima_serie.csv"))
cat("clima_serie.csv:", nrow(clima_serie), "righe\n")

# ---- Quanto pesava la composizione variabile? (solo diagnostica a schermo) --
# Confronto fra la serie corretta e quella "ingenua" su cntry %in% panel15.
cat("\n=== Effetto della correzione: panel per variabile vs filtro sul paese ===\n")
for (v in names(serie_vars)) {
  if (identical(sort(panel_var[[v]]), sort(panel15))) next
  cat(sprintf("-- %s: %s vs Europa-panel15 (ingenuo)\n", v, agg_var[[v]]))
  for (r in 1:11) {
    naive <- d[cntry %in% panel15 & essround == r][get(v) %in% serie_vars[[v]]]
    corr  <- d[cntry %in% panel_var[[v]] & essround == r][get(v) %in% serie_vars[[v]]]
    if (nrow(naive) < N_MIN || nrow(corr) < N_MIN) next
    if (v == "hincfel") {
      a <- wpct(naive$hincfel %in% 3:4, naive$anweight); b <- wpct(corr$hincfel %in% 3:4, corr$anweight)
      dec <- 1
    } else {
      a <- wmean(naive[[v]], naive$anweight); b <- wmean(corr[[v]], corr$anweight); dec <- 2
    }
    n_paesi_naive <- naive[, uniqueN(cntry)]
    cat(sprintf("   %d (%d): ingenuo %.*f su %2d paesi | corretto %.*f su %2d paesi | delta %+.*f\n",
                r, anni_map[[as.character(r)]], dec, a, n_paesi_naive, dec, b,
                corr[, uniqueN(cntry)], dec, b - a))
  }
}

# ==================================================================
# b) lrscale: distribuzione sinistra(0-3)/centro(4-6)/destra(7-10)
#    IT nel tempo (round 1,6,8,9,10,11) + confronto con Europa round 11
# ==================================================================
lr_rows <- list()

for (r in c(1, 6, 8, 9, 10, 11)) {
  base <- d[cntry == "IT" & essround == r & lrscale %in% valid0_10]
  if (nrow(base) < N_MIN) next
  grp <- ifelse(base$lrscale <= 3, "sinistra", ifelse(base$lrscale <= 6, "centro", "destra"))
  for (cat in c("sinistra", "centro", "destra")) {
    row <- row_pct(base, "lrscale", grp == cat, cat, paste0("pct_", cat), "IT", r)
    if (!is.null(row)) lr_rows[[length(lr_rows)+1]] <- row
  }
  row <- row_media(base, "lrscale", "IT", r)
  if (!is.null(row)) lr_rows[[length(lr_rows)+1]] <- row
}

# confronto Europa round 11 (aggregato pieno UE27+GB+NO+CH+IS presenti nel round)
base <- d[cntry %in% europa_full & essround == 11 & lrscale %in% valid0_10]
grp <- ifelse(base$lrscale <= 3, "sinistra", ifelse(base$lrscale <= 6, "centro", "destra"))
for (cat in c("sinistra", "centro", "destra")) {
  row <- row_pct(base, "lrscale", grp == cat, cat, paste0("pct_", cat), "Europa", 11)
  if (!is.null(row)) lr_rows[[length(lr_rows)+1]] <- row
}
row <- row_media(base, "lrscale", "Europa", 11)
if (!is.null(row)) lr_rows[[length(lr_rows)+1]] <- row

clima_lrscale <- rbindlist(lr_rows)
fwrite(clima_lrscale, file.path(OUT, "clima_lrscale.csv"))
cat("clima_lrscale.csv:", nrow(clima_lrscale), "righe\n")

# ==================================================================
# c) euftf e imbgeco: media IT vs paesi (DE, FR, ES, GB, Europa) round 11
#    + serie storica IT (euftf da round 6, imbgeco da round 1)
# ==================================================================
eu_imm_rows <- list()

# serie storica IT (include gia' il round 11, riusato per il confronto sotto)
for (r in c(6, 8, 9, 10, 11)) {
  dt <- d[cntry == "IT" & essround == r & euftf %in% valid0_10]
  row <- row_media(dt, "euftf", "IT", r); if (!is.null(row)) eu_imm_rows[[length(eu_imm_rows)+1]] <- row
}
for (r in c(1, 6, 8, 9, 10, 11)) {
  dt <- d[cntry == "IT" & essround == r & imbgeco %in% valid0_10]
  row <- row_media(dt, "imbgeco", "IT", r); if (!is.null(row)) eu_imm_rows[[length(eu_imm_rows)+1]] <- row
}

# confronto round 11: DE, FR, ES, GB, Europa (IT gia' incluso dalla serie sopra)
for (p in c("DE", "FR", "ES", "GB")) {
  base <- d[cntry == p & essround == 11]
  dt <- base[euftf %in% valid0_10]
  row <- row_media(dt, "euftf", p, 11); if (!is.null(row)) eu_imm_rows[[length(eu_imm_rows)+1]] <- row
  dt <- base[imbgeco %in% valid0_10]
  row <- row_media(dt, "imbgeco", p, 11); if (!is.null(row)) eu_imm_rows[[length(eu_imm_rows)+1]] <- row
}
base <- d[cntry %in% europa_full & essround == 11]
dt <- base[euftf %in% valid0_10]
row <- row_media(dt, "euftf", "Europa", 11); if (!is.null(row)) eu_imm_rows[[length(eu_imm_rows)+1]] <- row
dt <- base[imbgeco %in% valid0_10]
row <- row_media(dt, "imbgeco", "Europa", 11); if (!is.null(row)) eu_imm_rows[[length(eu_imm_rows)+1]] <- row

clima_europa_immigrazione <- rbindlist(eu_imm_rows)
fwrite(clima_europa_immigrazione, file.path(OUT, "clima_europa_immigrazione.csv"))
cat("clima_europa_immigrazione.csv:", nrow(clima_europa_immigrazione), "righe\n")

# ==================================================================
# d) stfeco e hincfel, round 11 Italia, per gruppi sociodemo standard
# ==================================================================
it11 <- d[cntry == "IT" & essround == 11]
sd_rows <- list()

# baseline "tutti"
dt <- it11[stfeco %in% valid0_10]
row <- row_media(dt, "stfeco", "IT", 11, gruppo = "tutti")
if (!is.null(row)) sd_rows[[length(sd_rows)+1]] <- row
dt <- it11[hincfel %in% valid_hincfel]
row <- row_pct(dt, "hincfel", dt$hincfel %in% 3:4, "difficolta", "pct_difficolta", "IT", 11, gruppo = "tutti")
if (!is.null(row)) sd_rows[[length(sd_rows)+1]] <- row

make_group_rows <- function(base_dt, group_vals, group_name, do_hincfel = TRUE) {
  rows <- list()
  cats <- na.omit(unique(group_vals))
  for (cat in cats) {
    sub <- base_dt[which(group_vals == cat)]

    dt_s <- sub[stfeco %in% valid0_10]
    row <- row_media(dt_s, "stfeco", "IT", 11, gruppo = paste0(group_name, ":", cat))
    if (!is.null(row)) rows[[length(rows)+1]] <- row

    if (do_hincfel) {
      dt_h <- sub[hincfel %in% valid_hincfel]
      row <- row_pct(dt_h, "hincfel", dt_h$hincfel %in% 3:4, "difficolta", "pct_difficolta",
                      "IT", 11, gruppo = paste0(group_name, ":", cat))
      if (!is.null(row)) rows[[length(rows)+1]] <- row
    }
  }
  rows
}

eta_grp    <- ifelse(it11$agea == 999, NA_character_,
                      ifelse(it11$agea < 35, "15-34", ifelse(it11$agea < 55, "35-54", "55+")))
genere_grp <- ifelse(it11$gndr == 1, "uomini", ifelse(it11$gndr == 2, "donne", NA_character_))
istr_grp   <- ifelse(it11$eisced %in% 1:2, "bassa",
                      ifelse(it11$eisced %in% 3:4, "media", ifelse(it11$eisced %in% 5:7, "alta", NA_character_)))
cond_grp   <- ifelse(it11$mnactic == 1, "occupati",
                      ifelse(it11$mnactic %in% 3:4, "disoccupati",
                             ifelse(it11$mnactic == 6, "pensionati",
                                    ifelse(it11$mnactic == 2, "studenti",
                                           ifelse(it11$mnactic %in% c(5,7,8,9), "altro_inattivo", NA_character_)))))
sett_grp   <- ifelse(it11$tporgwk %in% 1:3, "pubblico",
                      ifelse(it11$tporgwk == 4, "privato", ifelse(it11$tporgwk == 5, "autonomi", NA_character_)))
redperc_grp <- ifelse(it11$hincfel == 1, "comodamente",
                       ifelse(it11$hincfel == 2, "se_la_cava",
                              ifelse(it11$hincfel == 3, "difficolta",
                                     ifelse(it11$hincfel == 4, "grande_difficolta", NA_character_))))
dec_grp    <- ifelse(it11$hinctnta %in% 1:3, "basso",
                      ifelse(it11$hinctnta %in% 4:7, "medio", ifelse(it11$hinctnta %in% 8:10, "alto", NA_character_)))
sind_grp   <- ifelse(it11$mbtru == 1, "iscritto_ora",
                      ifelse(it11$mbtru == 2, "in_passato", ifelse(it11$mbtru == 3, "mai", NA_character_)))
lr_grp     <- ifelse(it11$lrscale %in% 0:3, "sinistra",
                      ifelse(it11$lrscale %in% 4:6, "centro", ifelse(it11$lrscale %in% 7:10, "destra", NA_character_)))

sd_rows <- c(sd_rows,
  make_group_rows(it11, eta_grp, "eta"),
  make_group_rows(it11, genere_grp, "genere"),
  make_group_rows(it11, istr_grp, "istruzione"),
  make_group_rows(it11, cond_grp, "condizione"),
  make_group_rows(it11, sett_grp, "settore"),
  make_group_rows(it11, redperc_grp, "reddito_percepito", do_hincfel = FALSE),  # evita tautologia hincfel-su-hincfel
  make_group_rows(it11, dec_grp, "decile_reddito"),
  make_group_rows(it11, sind_grp, "sindacato"),
  make_group_rows(it11, lr_grp, "collocazione_politica")
)

clima_sociodemo_it <- rbindlist(sd_rows)
fwrite(clima_sociodemo_it, file.path(OUT, "clima_sociodemo_it.csv"))
cat("clima_sociodemo_it.csv:", nrow(clima_sociodemo_it), "righe\n")

# ==================================================================
# Segnalazione celle piccole (N<100), come richiesto da DATA_MAP.md
# ==================================================================
tutti <- rbindlist(list(clima_serie, clima_lrscale, clima_europa_immigrazione, clima_sociodemo_it))
piccole <- tutti[n_validi < 100][order(n_validi)]
cat("\n=== Celle con N non ponderato < 100 (segnalate, non escluse) ===\n")
print(piccole[, .(variabile, essround, aggregato, gruppo, categoria, tipo_valore, valore, n_validi)])

cat("\nFatto.\n")
