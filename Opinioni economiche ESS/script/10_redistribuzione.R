## 10_redistribuzione.R
## Tema: redistribuzione (ESS) — gincdif (serie storica + classifica R11 + sociodemo IT R11)
##       dfincac / smdfslv (confronto R8, con R4 per il confronto temporale sugli altri paesi)
##
## CORREZIONE (revisione): una versione precedente della sintesi narrativa affermava che
## l'Italia è il paese più favorevole alla redistribuzione (gincdif) tra i cinque confrontati
## "in ogni round in cui è presente". È FALSO: l'Italia è prima solo nei round 6, 9 e 11
## (3 dei 6 round in cui è presente); è terza al round 1 (FR 85,1 e ES 79,4 davanti a IT 78,4)
## e seconda ai round 8 (ES 83,8 vs IT 80,5) e 10 (FR 75,8 vs IT 75,1).
## I numeri della serie erano già corretti: l'errore era solo nella prosa. Per impedirne il
## ritorno, il blocco (e) calcola e salva esplicitamente il rango dell'Italia round per round
## e la verifica formale è assertiva (stopifnot) sul fatto che l'Italia NON è prima ovunque.
##
## Regole applicate (v. input/DATA_MAP.md):
## - dati SOLO da input/ess_slim.rds (readRDS, data.table)
## - filtro sempre sui valori validi del codebook (mai fidarsi di is.na())
## - peso anweight sempre, sia per singolo paese sia per aggregati pooled
## - scale 1-5: % accordo (1-2), % né/né (3), % disaccordo (4-5), media pesata
## - percentuali arrotondate a 1 decimale; N<100 segnalato (flag_n_basso), N<50 non riportato
## - "Europa" = UE27+UK+NO+CH+IS presenti nel round corrente; "Europa-panel15" = panel fisso 15 paesi per le serie storiche

library(data.table)

DIR <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Opinioni economiche ESS"
IN  <- file.path(DIR, "input")
OUT <- file.path(DIR, "output/estrazioni")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

d <- readRDS(file.path(IN, "ess_slim.rds"))
stopifnot(is.data.table(d))
cat("Caricato ess_slim.rds:", nrow(d), "righe,", ncol(d), "colonne\n")

anni <- c(`1`=2002,`2`=2004,`3`=2006,`4`=2008,`5`=2010,`6`=2012,`7`=2014,`8`=2016,`9`=2018,`10`=2021,`11`=2023)

# "Europa" media round corrente: UE27 + UK + Norvegia + Svizzera + Islanda presenti nel round (DATA_MAP §Paesi e aggregati)
EU_EFTA_UK <- c("AT","BE","BG","CH","CY","CZ","DE","DK","EE","ES","FI","FR","GB","GR","HR","HU",
                "IE","IS","IT","LT","LU","LV","NL","NO","PL","PT","RO","SE","SI","SK")
# panel fisso 15 paesi presenti in tutti gli 11 round, per le serie storiche (DATA_MAP §Paesi e aggregati)
PANEL15 <- c("BE","CH","DE","ES","FI","FR","GB","HU","IE","NL","NO","PL","PT","SE","SI")

LBL <- list(
  gincdif = "Il governo dovrebbe ridurre le differenze di reddito",
  dfincac = "Grandi differenze di reddito sono accettabili per premiare talenti e impegno",
  smdfslv = "Per una società giusta le differenze nello standard di vita dovrebbero essere piccole"
)

## ---- helper: statistiche scala 1-5 (accordo/né-né/disaccordo + media), pesate anweight ----
calc_agree5 <- function(x, w) {
  ok <- !is.na(x) & x %in% 1:5 & !is.na(w)
  x <- x[ok]; w <- w[ok]
  n <- length(x)
  if (n == 0L) {
    return(list(pct_accordo = NA_real_, pct_neutro = NA_real_, pct_contrario = NA_real_,
                media = NA_real_, n_validi = 0L))
  }
  totw <- sum(w)
  list(
    pct_accordo   = round(100 * sum(w[x %in% 1:2]) / totw, 1),
    pct_neutro    = round(100 * sum(w[x == 3])     / totw, 1),
    pct_contrario = round(100 * sum(w[x %in% 4:5]) / totw, 1),
    media         = round(sum(x * w) / totw, 2),
    n_validi      = n
  )
}

## ---- helper: converte una stat in righe long nel formato DATA_MAP; NULL se N<50 ----
to_long <- function(st, tema, variabile, label_it, essround, anno, aggregato, gruppo, categoria) {
  if (is.na(st$n_validi) || st$n_validi < 50L) return(NULL)  # non riportare stime con N<50
  flag <- st$n_validi < 100L                                  # segnalare gruppi con N<100
  data.table(
    tema = tema, variabile = variabile, label_it = label_it,
    essround = essround, anno = anno, aggregato = aggregato, gruppo = gruppo, categoria = categoria,
    valore = c(st$pct_accordo, st$pct_neutro, st$pct_contrario, st$media),
    tipo_valore = c("pct_accordo", "pct_neutro", "pct_contrario", "media"),
    n_validi = st$n_validi, flag_n_basso = flag
  )
}

COLONNE <- c("tema","variabile","label_it","essround","anno","aggregato","gruppo","categoria",
             "valore","tipo_valore","n_validi","flag_n_basso")

# ============================================================================
# a) Serie storica gincdif — IT, DE, FR, ES, GB (round 1-11) + Europa-panel15
# ============================================================================
cat("\n--- (a) serie storica gincdif ---\n")
paesi5 <- c("IT","DE","FR","ES","GB")
rows_a <- list()

for (p in paesi5) {
  for (r in 1:11) {
    sub <- d[essround == r & cntry == p]
    st <- calc_agree5(sub$gincdif, sub$anweight)
    rows_a[[length(rows_a) + 1]] <- to_long(st, "redistribuzione", "gincdif", LBL$gincdif,
                                             r, anni[[as.character(r)]], p, "tutti", "")
  }
}
for (r in 1:11) {
  sub <- d[essround == r & cntry %in% PANEL15]
  st <- calc_agree5(sub$gincdif, sub$anweight)
  rows_a[[length(rows_a) + 1]] <- to_long(st, "redistribuzione", "gincdif", LBL$gincdif,
                                           r, anni[[as.character(r)]], "Europa-panel15", "tutti", "")
}
serie <- rbindlist(rows_a)
setcolorder(serie, COLONNE)
setorder(serie, aggregato, essround, tipo_valore)
cat("righe:", nrow(serie), "| celle paese-round-var mancanti per IT (round senza IT):",
    11L - uniqueN(serie[aggregato == "IT", essround]), "\n")

fwrite(serie, file.path(OUT, "redistribuzione_serie.csv"))

# ============================================================================
# b) Fotografia round 11 — classifica di tutti i paesi UE/EFTA+UK presenti
# ============================================================================
cat("\n--- (b) classifica round 11 ---\n")
r11_countries <- sort(unique(d[essround == 11 & cntry %in% EU_EFTA_UK, cntry]))
cat("paesi UE/EFTA+UK presenti al round 11:", paste(r11_countries, collapse = " "), "(n =", length(r11_countries), ")\n")

rows_b <- list()
for (p in r11_countries) {
  sub <- d[essround == 11 & cntry == p]
  st <- calc_agree5(sub$gincdif, sub$anweight)
  rows_b[[length(rows_b) + 1]] <- to_long(st, "redistribuzione", "gincdif", LBL$gincdif,
                                           11L, anni[["11"]], p, "tutti", "")
}
classifica <- rbindlist(rows_b)

# rank sulla base di % accordo decrescente (1 = paese più favorevole alla redistribuzione)
acc <- classifica[tipo_valore == "pct_accordo", .(aggregato, valore)]
acc[, rank := frank(-valore, ties.method = "min")]
classifica <- merge(classifica, acc[, .(aggregato, rank)], by = "aggregato", all.x = TRUE)
setcolorder(classifica, c(COLONNE, "rank"))
setorder(classifica, rank, aggregato, tipo_valore)

pos_it <- unique(classifica[aggregato == "IT", rank])
n_tot  <- uniqueN(classifica$aggregato)
cat("Italia: posizione", pos_it, "su", n_tot, "paesi (per % accordo gincdif)\n")

fwrite(classifica, file.path(OUT, "redistribuzione_classifica_r11.csv"))

# ============================================================================
# c) dfincac / smdfslv — IT vs DE FR ES GB + Europa, round 8 (+ round 4 per gli altri, senza IT)
#
# CORREZIONE COMPOSIZIONE AGGREGATO "Europa" (v. DATA_MAP.md §Paesi e aggregati):
#   L'aggregato "Europa" = UE27+UK+NO+CH+IS *presenti nel round corrente* ha una
#   composizione che cambia fra round 4 e round 8 (25 paesi vs 21: al round 4 ci sono
#   BG CY DK GR HR LV RO SK e NON c'e' l'Italia; al round 8 entrano AT IS IT LT ed
#   escono quegli otto). Poiche' anweight include pweight e quindi pondera i paesi per
#   popolazione, l'ingresso dell'Italia (~60 mln) e l'uscita di Romania, Grecia e
#   Bulgaria spostano l'aggregato di parecchi punti a prescindere dalle opinioni.
#   Usare "Europa" su entrambi i round per calcolare una variazione 2008->2016 mescola
#   quindi cambiamento vero e artefatto di composizione.
#   -> per il confronto temporale si usa SOLO "Europa-panel15" (composizione fissa,
#      15 paesi presenti in tutti gli 11 round), calcolato su round 4 e round 8;
#      "Europa" resta solo al round 8 come fotografia trasversale del round corrente.
#   Stesso schema gia' applicato in 12_sussidi.R (slices_a).
# ============================================================================
cat("\n--- (c) dfincac / smdfslv, round 8 e round 4 ---\n")
rows_c <- list()
vars_c <- c("dfincac", "smdfslv")

eu8_c <- sort(intersect(EU_EFTA_UK, unique(d[essround == 8, cntry])))
eu4_c <- sort(intersect(EU_EFTA_UK, unique(d[essround == 4, cntry])))
cat("composizione 'Europa' R4 (n=", length(eu4_c), "):", paste(eu4_c, collapse = " "), "\n")
cat("composizione 'Europa' R8 (n=", length(eu8_c), "):", paste(eu8_c, collapse = " "), "\n")
cat("  solo R4:", paste(setdiff(eu4_c, eu8_c), collapse = " "),
    "| solo R8:", paste(setdiff(eu8_c, eu4_c), collapse = " "), "\n")
cat("  -> composizione variabile: la variazione 2008-2016 si legge su Europa-panel15 (",
    length(PANEL15), "paesi fissi )\n")

# round 8: IT DE FR ES GB + Europa (fotografia round corrente) + Europa-panel15
for (vn in vars_c) {
  for (p in c("IT", "DE", "FR", "ES", "GB")) {
    sub <- d[essround == 8 & cntry == p]
    st <- calc_agree5(sub[[vn]], sub$anweight)
    rows_c[[length(rows_c) + 1]] <- to_long(st, "redistribuzione", vn, LBL[[vn]],
                                             8L, anni[["8"]], p, "tutti", "")
  }
  subE <- d[essround == 8 & cntry %in% EU_EFTA_UK]
  stE <- calc_agree5(subE[[vn]], subE$anweight)
  rows_c[[length(rows_c) + 1]] <- to_long(stE, "redistribuzione", vn, LBL[[vn]],
                                           8L, anni[["8"]], "Europa", "tutti", "")
  subP <- d[essround == 8 & cntry %in% PANEL15]
  stP <- calc_agree5(subP[[vn]], subP$anweight)
  rows_c[[length(rows_c) + 1]] <- to_long(stP, "redistribuzione", vn, LBL[[vn]],
                                           8L, anni[["8"]], "Europa-panel15", "tutti", "")
}

# round 4: DE FR ES GB + Europa-panel15 (l'Italia non è nel round 4 — v. presenza_compatta_italia.csv).
# NESSUNA riga "Europa" al round 4: sarebbe calcolata su 25 paesi diversi da quelli del
# round 8 e verrebbe inevitabilmente usata per una variazione temporale non valida.
for (vn in vars_c) {
  for (p in c("DE", "FR", "ES", "GB")) {
    sub <- d[essround == 4 & cntry == p]
    st <- calc_agree5(sub[[vn]], sub$anweight)
    rows_c[[length(rows_c) + 1]] <- to_long(st, "redistribuzione", vn, LBL[[vn]],
                                             4L, anni[["4"]], p, "tutti", "")
  }
  subP <- d[essround == 4 & cntry %in% PANEL15]
  stP <- calc_agree5(subP[[vn]], subP$anweight)
  rows_c[[length(rows_c) + 1]] <- to_long(stP, "redistribuzione", vn, LBL[[vn]],
                                           4L, anni[["4"]], "Europa-panel15", "tutti", "")
}

differenze <- rbindlist(rows_c)
setcolorder(differenze, COLONNE)
setorder(differenze, variabile, essround, aggregato, tipo_valore)
cat("righe:", nrow(differenze), "\n")

# --- guardia: nessun aggregato a composizione variabile puo' comparire su due round ---
agg_multi <- differenze[aggregato %in% c("Europa"), uniqueN(essround), by = aggregato]
stopifnot(nrow(agg_multi) == 0L || all(agg_multi$V1 == 1L))
stopifnot(differenze[aggregato == "Europa-panel15", uniqueN(essround)] == 2L)

# --- quantificazione dell'artefatto di composizione (solo a stampa, per il log) ---
cat("\n  variazione 2008-2016 dell'aggregato europeo, % accordo:\n")
for (vn in vars_c) {
  pv <- function(agg, rr) {
    subq <- if (agg == "Europa") d[essround == rr & cntry %in% EU_EFTA_UK]
            else d[essround == rr & cntry %in% PANEL15]
    calc_agree5(subq[[vn]], subq$anweight)$pct_accordo
  }
  e4 <- pv("Europa", 4); e8 <- pv("Europa", 8)
  p4 <- pv("panel15", 4); p8 <- pv("panel15", 8)
  cat(sprintf("   %-8s Europa (comp. variabile, NON pubblicata su R4): %.1f -> %.1f = %+.1f pp\n",
              vn, e4, e8, e8 - e4))
  cat(sprintf("   %-8s Europa-panel15 (comp. fissa, PUBBLICATA)      : %.1f -> %.1f = %+.1f pp",
              "", p4, p8, p8 - p4))
  cat(sprintf("   | artefatto di composizione: %+.1f pp\n", (e8 - e4) - (p8 - p4)))
}

fwrite(differenze, file.path(OUT, "redistribuzione_differenze.csv"))

# ============================================================================
# d) gincdif round 11 Italia per gruppi sociodemo standard (DATA_MAP §Gruppi socio-demografici)
# ============================================================================
cat("\n--- (d) sociodemo IT round 11 ---\n")
it11 <- d[essround == 11 & cntry == "IT"]
cat("N totale IT round 11:", nrow(it11), "\n")

rows_d <- list()

add_group <- function(gruppo_dim, categoria, sub) {
  st <- calc_agree5(sub$gincdif, sub$anweight)
  r <- to_long(st, "redistribuzione", "gincdif", LBL$gincdif, 11L, anni[["11"]], "IT",
               paste0(gruppo_dim, ":", categoria), categoria)
  if (!is.null(r)) rows_d[[length(rows_d) + 1]] <<- r
}

# Età: agea 15-34 / 35-54 / 55+ (esclude 999)
it11[, grp_eta := fifelse(agea == 999, NA_character_,
                    fifelse(agea <= 34, "15-34",
                    fifelse(agea <= 54, "35-54", "55+")))]
for (cat in c("15-34", "35-54", "55+")) add_group("eta", cat, it11[grp_eta == cat])

# Genere: gndr 1=uomini 2=donne
lbl_gndr <- c(`1` = "uomini", `2` = "donne")
for (code in c(1, 2)) add_group("genere", lbl_gndr[[as.character(code)]], it11[gndr == code])

# Istruzione: eisced 1-2 bassa / 3-4 media / 5-7 alta (esclude 0,55,77,88,99)
it11[, grp_edu := fifelse(eisced %in% 1:2, "bassa",
                    fifelse(eisced %in% 3:4, "media",
                    fifelse(eisced %in% 5:7, "alta", NA_character_)))]
for (cat in c("bassa", "media", "alta")) add_group("istruzione", cat, it11[grp_edu == cat])

# Condizione: mnactic 1=occupati 3-4=disoccupati 6=pensionati 2=studenti 5/7/8/9=altro inattivo
it11[, grp_cond := fifelse(mnactic == 1, "occupati",
                     fifelse(mnactic %in% c(3, 4), "disoccupati",
                     fifelse(mnactic == 6, "pensionati",
                     fifelse(mnactic == 2, "studenti",
                     fifelse(mnactic %in% c(5, 7, 8, 9), "altro inattivo", NA_character_)))))]
for (cat in c("occupati", "disoccupati", "pensionati", "studenti", "altro inattivo"))
  add_group("condizione", cat, it11[grp_cond == cat])

# Settore: tporgwk 1-3=pubblico 4=privato dipendente 5=autonomi
it11[, grp_set := fifelse(tporgwk %in% 1:3, "pubblico",
                    fifelse(tporgwk == 4, "privato",
                    fifelse(tporgwk == 5, "autonomi", NA_character_)))]
for (cat in c("pubblico", "privato", "autonomi")) add_group("settore", cat, it11[grp_set == cat])

# Reddito percepito: hincfel 1-4
lbl_hincfel <- c(`1` = "vive comodamente", `2` = "se la cava", `3` = "difficoltà", `4` = "grande difficoltà")
for (code in 1:4) add_group("reddito_percepito", lbl_hincfel[[as.character(code)]], it11[hincfel == code])

# Decile di reddito: hinctnta 1-3 basso / 4-7 medio / 8-10 alto
it11[, grp_dec := fifelse(hinctnta %in% 1:3, "basso",
                    fifelse(hinctnta %in% 4:7, "medio",
                    fifelse(hinctnta %in% 8:10, "alto", NA_character_)))]
for (cat in c("basso", "medio", "alto")) add_group("decile_reddito", cat, it11[grp_dec == cat])

# Sindacato: mbtru 1=iscritto ora 2=in passato 3=mai
lbl_mbtru <- c(`1` = "iscritto", `2` = "ex iscritto", `3` = "mai iscritto")
for (code in 1:3) add_group("sindacato", lbl_mbtru[[as.character(code)]], it11[mbtru == code])

# Collocazione politica: lrscale 0-3 sinistra / 4-6 centro / 7-10 destra
it11[, grp_lr := fifelse(lrscale %in% 0:3, "sinistra",
                   fifelse(lrscale %in% 4:6, "centro",
                   fifelse(lrscale %in% 7:10, "destra", NA_character_)))]
for (cat in c("sinistra", "centro", "destra")) add_group("politica", cat, it11[grp_lr == cat])

sociodemo <- rbindlist(rows_d)
setcolorder(sociodemo, COLONNE)
setorder(sociodemo, gruppo, tipo_valore)
cat("righe:", nrow(sociodemo), "| gruppi con N<100 segnalati:", uniqueN(sociodemo[flag_n_basso == TRUE, gruppo]), "\n")

fwrite(sociodemo, file.path(OUT, "redistribuzione_sociodemo_it.csv"))

# ============================================================================
# e) Rango dell'Italia sui 5 paesi confrontati (gincdif, % accordo), round per round
#    Blocco aggiunto in fase di correzione: rende verificabile dal CSV l'ordine del
#    confronto, che la prosa aveva riportato in modo errato ("prima in ogni round").
# ============================================================================
cat("\n--- (e) rango Italia sui 5 paesi, round per round ---\n")

acc5 <- serie[tipo_valore == "pct_accordo" & aggregato %in% paesi5,
              .(essround, anno, aggregato, valore, n_validi, flag_n_basso)]
round_it <- sort(unique(acc5[aggregato == "IT", essround]))   # round in cui l'Italia è presente
cat("round con Italia:", paste(round_it, collapse = ", "), "\n")

acc5 <- acc5[essround %in% round_it]
acc5[, `:=`(
  rank_su5     = frank(-valore, ties.method = "min"),
  paesi_round  = .N,
  valore_primo = max(valore),
  paese_primo  = aggregato[which.max(valore)]
), by = essround]
acc5[, gap_pp_vs_primo := round(valore - valore_primo, 1)]

rank_it <- data.table(
  tema = "redistribuzione", variabile = "gincdif", label_it = LBL$gincdif,
  essround = acc5$essround, anno = acc5$anno, aggregato = acc5$aggregato,
  gruppo = "tutti", categoria = "",
  valore = acc5$valore, tipo_valore = "pct_accordo",
  n_validi = acc5$n_validi, flag_n_basso = acc5$flag_n_basso,
  rank_su5 = acc5$rank_su5, paesi_confrontati = acc5$paesi_round,
  paese_primo = acc5$paese_primo, valore_primo = acc5$valore_primo,
  gap_pp_vs_primo = acc5$gap_pp_vs_primo
)
setorder(rank_it, essround, rank_su5)
fwrite(rank_it, file.path(OUT, "redistribuzione_rank_it_5paesi.csv"))

# quadro sintetico stampato: una riga per round, con il rango dell'Italia
sintesi_it <- rank_it[aggregato == "IT",
                      .(essround, anno, pct_it = valore, rank_it = rank_su5,
                        su = paesi_confrontati, paese_primo, pct_primo = valore_primo,
                        gap = gap_pp_vs_primo)]
print(sintesi_it)

primi  <- sintesi_it[rank_it == 1L, essround]
n_primi <- length(primi)
cat(sprintf("\nL'Italia è PRIMA sui 5 paesi in %d round su %d: round %s.\n",
            n_primi, nrow(sintesi_it), paste(primi, collapse = ", ")))
for (rr in setdiff(sintesi_it$essround, primi)) {
  s <- sintesi_it[essround == rr]
  cat(sprintf("  round %-2d (%d): Italia %s su %d — davanti %s (%.1f%% vs %.1f%%)\n",
              rr, s$anno, s$rank_it, s$su, s$paese_primo, s$pct_primo, s$pct_it))
}

# Guardia contro il ritorno dell'affermazione errata: se questa assertion fallisse,
# vorrebbe dire che l'Italia è davvero prima ovunque e la frase andrebbe riscritta.
stopifnot(
  "atteso: Italia NON prima in tutti i round" = n_primi < nrow(sintesi_it),
  "atteso: Italia prima ai round 6, 9 e 11"   = identical(as.integer(primi), c(6L, 9L, 11L))
)
cat("VERIFICATO: la formulazione corretta è \"il più favorevole al round 11 (2023-24)\"",
    "o \"nei round più recenti\", MAI \"in ogni round\".\n")

cat("\n=== FATTO ===\n")
cat("File salvati in", OUT, "\n")
