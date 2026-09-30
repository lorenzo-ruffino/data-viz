## 11_ruolo_stato.R
## Tema: ruolo dello Stato — responsabilita' del governo e intervento nell'economia (ESS)
##   Variabili: gvslvol gvslvue gvhlthc gvcldcr gvpdlwk gvjbevn (0-10, responsabilita' del governo, R4/R8)
##              ginveco (1-5, "meno lo Stato interviene nell'economia meglio e'", SOLO R1/2002)
##              slvpens slvuemp (0-10, valutazione standard di vita pensionati/disoccupati, R8)
##
## Regole applicate (v. input/DATA_MAP.md):
## - dati SOLO da input/ess_slim.rds (readRDS, data.table), MAI il CSV da 1,7 GB
## - filtro sempre sui valori validi del codebook (i codici missing 7/8/9, 77/88/99 sono NUMERI
##   nel dataset, mai fidarsi di is.na() da solo)
## - peso anweight sempre, sia per singolo paese sia per aggregati pooled ("Europa")
## - scale 0-10 (gv*, slvpens, slvuemp): media pesata + % 7-10 (per slvpens/slvuemp anche % 0-3)
## - scala 1-5 (ginveco): % accordo (1-2) / neutro (3) / disaccordo (4-5) + media
## - percentuali arrotondate a 1 decimale, medie a 2 decimali; N non ponderato sempre riportato;
##   gruppi con N<100 segnalati (colonna flag_n_basso), N<50 esclusi dal CSV
## - "Europa" = UE27 + UK + Norvegia + Svizzera + Islanda presenti nel round in questione
##   (pooled, anweight pondera gia' per popolazione)
##
## ANOMALIA IMPORTANTE nei dati, verificata empiricamente prima di scrivere qualunque calcolo
## (v. input/presenza_variabili_tutti.csv e i conteggi per valore stampati in fase di esplorazione):
##   Il round 8 NON contiene gvhlthc, gvpdlwk, gvjbevn per NESSUN paese del dataset (N=0 ovunque,
##   non solo per l'Italia): questi 3 item sono stati posti SOLO al round 4 (2008). Il round 8
##   (2016) ripete solo 3 dei 6 item originali: gvslvol, gvslvue, gvcldcr. Conseguenze:
##   - punto (a): il round 8 usa solo i 3 item disponibili (non 6); il round 4 (DE/FR/ES/GB/Europa,
##     l'Italia non ha il round 4) usa invece tutti e 6 gli item, ma non e' confrontabile con
##     l'Italia in quell'anno
##   - punto (b): la "classifica sulla media delle 6 responsabilita'" e' in realta' una classifica
##     sulla media dei 3 item disponibili al round 8 (variabile "indice_gv3")
##   - punto (d): gvjbevn, richiesto esplicitamente dal tema, non esiste al round 8: sostituito
##     con tutti e 3 gli item R8 disponibili (gvslvol, gvslvue, gvcldcr) per non perdere copertura

suppressPackageStartupMessages(library(data.table))

DIR <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Opinioni economiche ESS"
IN  <- file.path(DIR, "input")
OUT <- file.path(DIR, "output/estrazioni")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

d <- readRDS(file.path(IN, "ess_slim.rds"))
stopifnot(is.data.table(d))
cat("Caricato ess_slim.rds:", nrow(d), "righe,", ncol(d), "colonne\n")

anni <- c(`1`=2002,`2`=2004,`3`=2006,`4`=2008,`5`=2010,`6`=2012,`7`=2014,`8`=2016,`9`=2018,`10`=2021,`11`=2023)

# "Europa" media round corrente: UE27 + UK + Norvegia + Svizzera + Islanda presenti nel round
# (DATA_MAP.md §Paesi e aggregati)
EU_EFTA_UK <- c("AT","BE","BG","CH","CY","CZ","DE","DK","EE","ES","FI","FR","GB","GR","HR","HU",
                "IE","IS","IT","LT","LU","LV","NL","NO","PL","PT","RO","SE","SI","SK")
# panel fisso dei 15 paesi presenti in tutti gli 11 round: unico aggregato multi-paese
# ammesso dal DATA_MAP.md per i confronti temporali (composizione costante nel tempo)
PANEL15 <- c("BE","CH","DE","ES","FI","FR","GB","HU","IE","NL","NO","PL","PT","SE","SI")

NOME_PAESE <- c(AT="Austria", BE="Belgio", BG="Bulgaria", CH="Svizzera", CY="Cipro", CZ="Cechia",
  DE="Germania", DK="Danimarca", EE="Estonia", ES="Spagna", FI="Finlandia", FR="Francia", GB="Regno Unito",
  GR="Grecia", HR="Croazia", HU="Ungheria", IE="Irlanda", IS="Islanda", IT="Italia", LT="Lituania",
  LU="Lussemburgo", LV="Lettonia", NL="Paesi Bassi", NO="Norvegia", PL="Polonia", PT="Portogallo",
  RO="Romania", SE="Svezia", SI="Slovenia", SK="Slovacchia")

LBL <- list(
  gvslvol = "Tenore di vita degli anziani, responsabilita' del governo (0-10)",
  gvslvue = "Tenore di vita dei disoccupati, responsabilita' del governo (0-10)",
  gvhlthc = "Assistenza sanitaria per i malati, responsabilita' del governo (0-10)",
  gvcldcr = "Servizi per l'infanzia ai genitori lavoratori, responsabilita' del governo (0-10)",
  gvpdlwk = "Congedo retribuito per cura di familiari malati, responsabilita' del governo (0-10)",
  gvjbevn = "Lavoro per tutti, responsabilita' del governo (0-10)",
  ginveco = "Meno lo Stato interviene nell'economia, meglio e' per il Paese (1-5; accordo = anti-intervento)",
  slvpens = "Valutazione del tenore di vita dei pensionati (0-10)",
  slvuemp = "Valutazione del tenore di vita dei disoccupati (0-10)",
  indice_gv3 = "Indice medio responsabilita' del governo sui 3 item disponibili al round 8 (anziani, disoccupati, infanzia)"
)

N_MIN  <- 50   # non riportare stime con N<50 (DATA_MAP.md)
N_FLAG <- 100  # segnalare (flag_n_basso) stime con N<100

wmean <- function(x, w) sum(as.numeric(x) * w) / sum(w)
wpct  <- function(mask, w) sum(w[mask]) / sum(w) * 100

## ---- helper scala 0-10 (gv*, slvpens, slvuemp): media + %7-10 (+ opzionale %0-3) ----
calc_010 <- function(x, w, pct03 = FALSE) {
  ok <- !is.na(x) & x >= 0 & x <= 10 & !(x %in% c(77, 88, 99))
  x <- x[ok]; w <- w[ok]
  n <- length(x)
  if (n == 0L) return(list(n_validi = 0L))
  out <- list(media = round(wmean(x, w), 2),
              pct_7_10 = round(wpct(x >= 7, w), 1),
              n_validi = n)
  if (pct03) out$pct_0_3 <- round(wpct(x <= 3, w), 1)
  out
}

## ---- helper scala 1-5 (ginveco): media + accordo/neutro/disaccordo ----
calc_agree5 <- function(x, w) {
  ok <- !is.na(x) & x %in% 1:5
  x <- x[ok]; w <- w[ok]
  n <- length(x)
  if (n == 0L) return(list(n_validi = 0L))
  list(
    media = round(wmean(x, w), 2),
    pct_accordo = round(wpct(x %in% 1:2, w), 1),
    pct_neutro = round(wpct(x == 3, w), 1),
    pct_disaccordo = round(wpct(x %in% 4:5, w), 1),
    n_validi = n
  )
}

COLONNE <- c("tema", "variabile", "label_it", "essround", "anno", "aggregato", "gruppo", "categoria",
             "valore", "tipo_valore", "n_validi", "flag_n_basso")

## ---- converte una lista di statistiche (calc_010/calc_agree5) in righe long formato DATA_MAP ----
to_rows <- function(st, variabile, essround, aggregato, gruppo = "tutti", categoria = "") {
  if (st$n_validi < N_MIN) return(NULL)   # non riportare stime con N<50
  flag <- st$n_validi < N_FLAG            # segnalare gruppi con N<100
  tipi <- setdiff(names(st), "n_validi")
  data.table(
    tema = "ruolo-stato", variabile = variabile, label_it = LBL[[variabile]],
    essround = essround, anno = anni[[as.character(essround)]],
    aggregato = aggregato, gruppo = gruppo, categoria = categoria,
    valore = unlist(st[tipi]), tipo_valore = tipi,
    n_validi = st$n_validi, flag_n_basso = flag
  )
}

flag_log <- character(0)
log_small <- function(msg) flag_log <<- c(flag_log, msg)

# ============================================================================
# a) Le responsabilita' del governo — IT vs DE FR ES GB + Europa, round 8
#    (solo 3 item disponibili: gvslvol, gvslvue, gvcldcr — v. ANOMALIA in testa allo script)
#    + per gli altri paesi anche round 4, con tutti e 6 gli item (variazione 2008-2016
#    calcolabile solo sui 3 item comuni a entrambi i round)
#
# CORREZIONE COMPOSIZIONE AGGREGATO "Europa" (v. DATA_MAP.md §Paesi e aggregati):
#   "Europa" = UE27+UK+NO+CH+IS *presenti nel round corrente*: 25 paesi al round 4 (con
#   BG CY DK GR HR LV RO SK e SENZA l'Italia) contro 21 al round 8 (entrano AT IS IT LT,
#   escono quegli otto). anweight include pweight e pondera quindi i paesi per popolazione:
#   l'ingresso dell'Italia (~60 mln) e l'uscita di Romania, Grecia e Bulgaria spostano
#   l'aggregato a prescindere dalle opinioni. Confrontare "Europa" R4 con "Europa" R8
#   mescola cambiamento reale e artefatto di composizione.
#   -> il confronto temporale 2008-2016 si legge SOLO su "Europa-panel15" (15 paesi fissi),
#      calcolato su entrambi i round; "Europa" resta solo al round 8, come fotografia
#      trasversale. Stesso schema di 12_sussidi.R (slices_a).
# ============================================================================
cat("\n--- (a) responsabilita' del governo: IT vs DE FR ES GB + Europa, R8 (+R4 per gli altri) ---\n")

vars_r8 <- c("gvslvol", "gvslvue", "gvcldcr")
vars_r4 <- c("gvslvol", "gvslvue", "gvhlthc", "gvcldcr", "gvpdlwk", "gvjbevn")

rows_a <- list()

# Italia: solo round 8 (assente al round 4, v. DATA_MAP.md §Paesi e presenza_compatta_italia.csv)
it8 <- d[essround == 8 & cntry == "IT"]
for (v in vars_r8) {
  st <- calc_010(it8[[v]], it8$anweight)
  r <- to_rows(st, v, 8, "IT")
  if (!is.null(r)) rows_a[[length(rows_a) + 1]] <- r
}

# DE FR ES GB: round 8 (3 item) e round 4 (6 item)
for (p in c("DE", "FR", "ES", "GB")) {
  sub8 <- d[essround == 8 & cntry == p]
  for (v in vars_r8) {
    st <- calc_010(sub8[[v]], sub8$anweight)
    r <- to_rows(st, v, 8, p)
    if (!is.null(r)) rows_a[[length(rows_a) + 1]] <- r
  }
  sub4 <- d[essround == 4 & cntry == p]
  for (v in vars_r4) {
    st <- calc_010(sub4[[v]], sub4$anweight)
    r <- to_rows(st, v, 4, p)
    if (!is.null(r)) rows_a[[length(rows_a) + 1]] <- r
  }
}

# Europa (pooled). eu8/eu4 = paesi EU_EFTA_UK effettivamente presenti nei due round:
# servono a documentare quanto la composizione cambia (e eu8 e' riusato al punto (e)).
eu8 <- sort(intersect(EU_EFTA_UK, unique(d[essround == 8, cntry])))
eu4 <- sort(intersect(EU_EFTA_UK, unique(d[essround == 4, cntry])))
cat("Europa R8:", paste(eu8, collapse = " "), "(n=", length(eu8), "paesi )\n")
cat("Europa R4:", paste(eu4, collapse = " "), "(n=", length(eu4), "paesi )\n")
cat("  presenti solo al R4:", paste(setdiff(eu4, eu8), collapse = " "),
    "| solo al R8:", paste(setdiff(eu8, eu4), collapse = " "), "\n")
cat("  -> composizione variabile: nessuna riga 'Europa' al round 4; il confronto 2008-2016\n",
    "     si legge su Europa-panel15 (", length(PANEL15), "paesi fissi:",
    paste(PANEL15, collapse = " "), ")\n")

subE8 <- d[essround == 8 & cntry %in% eu8]
for (v in vars_r8) {
  st <- calc_010(subE8[[v]], subE8$anweight)
  r <- to_rows(st, v, 8, "Europa")
  if (!is.null(r)) rows_a[[length(rows_a) + 1]] <- r
}

# Europa-panel15: composizione fissa, calcolata su ENTRAMBI i round -> unica base valida
# per la variazione 2008-2016 dell'aggregato europeo.
subP8 <- d[essround == 8 & cntry %in% PANEL15]
subP4 <- d[essround == 4 & cntry %in% PANEL15]
stopifnot(length(intersect(PANEL15, unique(subP8$cntry))) == length(PANEL15),
          length(intersect(PANEL15, unique(subP4$cntry))) == length(PANEL15))
for (v in vars_r8) {
  st <- calc_010(subP8[[v]], subP8$anweight)
  r <- to_rows(st, v, 8, "Europa-panel15")
  if (!is.null(r)) rows_a[[length(rows_a) + 1]] <- r
}
for (v in vars_r4) {
  st <- calc_010(subP4[[v]], subP4$anweight)
  r <- to_rows(st, v, 4, "Europa-panel15")
  if (!is.null(r)) rows_a[[length(rows_a) + 1]] <- r
}

responsabilita <- rbindlist(rows_a)
setcolorder(responsabilita, COLONNE)
setorder(responsabilita, variabile, essround, aggregato, tipo_valore)
cat("righe:", nrow(responsabilita), "\n")

# --- guardia: "Europa" (composizione variabile) puo' comparire su un solo round ---
stopifnot(responsabilita[aggregato == "Europa", uniqueN(essround)] == 1L)
stopifnot(responsabilita[aggregato == "Europa", unique(essround)] == 8L)
# --- e Europa-panel15 deve esserci su entrambi i round per i 3 item comuni ---
stopifnot(all(responsabilita[aggregato == "Europa-panel15" & variabile %in% vars_r8,
                             uniqueN(essround), by = variabile]$V1 == 2L))

# --- quantificazione dell'artefatto di composizione (solo a stampa, per il log) ---
cat("\n  variazione 2008-2016, % 7-10 (item comuni ai due round):\n")
for (v in vars_r8) {
  pv <- function(paesi, rr) {
    subq <- d[essround == rr & cntry %in% paesi]
    calc_010(subq[[v]], subq$anweight)$pct_7_10
  }
  e4 <- pv(eu4, 4); e8 <- pv(eu8, 8)
  p4 <- pv(PANEL15, 4); p8 <- pv(PANEL15, 8)
  cat(sprintf("   %-8s Europa (comp. variabile, NON pubblicata su R4): %.1f -> %.1f = %+.1f pp\n",
              v, e4, e8, e8 - e4))
  cat(sprintf("   %-8s Europa-panel15 (comp. fissa, PUBBLICATA)      : %.1f -> %.1f = %+.1f pp",
              "", p4, p8, p8 - p4))
  cat(sprintf("   | artefatto di composizione: %+.1f pp\n", (e8 - e4) - (p8 - p4)))
}

fwrite(responsabilita, file.path(OUT, "ruolostato_responsabilita.csv"))

# ============================================================================
# b) Classifica round 8 dei paesi UE/EFTA+UK sull'indice medio dei 3 item
#    disponibili (gvslvol, gvslvue, gvcldcr) — posizione dell'Italia
# ============================================================================
cat("\n--- (b) classifica round 8: indice medio dei 3 item disponibili (gvslvol, gvslvue, gvcldcr) ---\n")

r8 <- copy(d[essround == 8])
ok3 <- !is.na(r8$gvslvol) & r8$gvslvol >= 0 & r8$gvslvol <= 10 & !(r8$gvslvol %in% c(77, 88, 99)) &
       !is.na(r8$gvslvue) & r8$gvslvue >= 0 & r8$gvslvue <= 10 & !(r8$gvslvue %in% c(77, 88, 99)) &
       !is.na(r8$gvcldcr) & r8$gvcldcr >= 0 & r8$gvcldcr <= 10 & !(r8$gvcldcr %in% c(77, 88, 99))
r8[, idx3 := NA_real_]
r8[ok3, idx3 := (gvslvol + gvslvue + gvcldcr) / 3]

paesi_r8 <- sort(intersect(EU_EFTA_UK, unique(r8$cntry)))
rank_dt <- data.table(aggregato = character(), media = numeric(), n = integer())
for (p in paesi_r8) {
  sub <- r8[cntry == p & !is.na(idx3)]
  n <- nrow(sub)
  if (n < N_MIN) { log_small(sprintf("classifica R8: %s escluso, N=%d<50", p, n)); next }
  if (n < N_FLAG) log_small(sprintf("classifica R8: %s N=%d<100 (segnalato)", p, n))
  rank_dt <- rbind(rank_dt, data.table(aggregato = p, media = wmean(sub$idx3, sub$anweight), n = n))
}
rank_dt[, rank := frank(-media, ties.method = "min")]
setorder(rank_dt, rank)

rows_b <- list()
for (i in seq_len(nrow(rank_dt))) {
  rw <- rank_dt[i]
  rows_b[[length(rows_b) + 1]] <- data.table(
    tema = "ruolo-stato", variabile = "indice_gv3", label_it = LBL$indice_gv3,
    essround = 8, anno = 2016, aggregato = rw$aggregato, gruppo = "tutti", categoria = NOME_PAESE[[rw$aggregato]],
    valore = round(rw$media, 2), tipo_valore = "media", n_validi = rw$n,
    flag_n_basso = rw$n < N_FLAG, rank = rw$rank
  )
}
# riga Europa di riferimento (pooled sugli stessi paesi; non entra nel ranking)
subEb <- r8[cntry %in% paesi_r8 & !is.na(idx3)]
rows_b[[length(rows_b) + 1]] <- data.table(
  tema = "ruolo-stato", variabile = "indice_gv3", label_it = LBL$indice_gv3,
  essround = 8, anno = 2016, aggregato = "Europa", gruppo = "tutti", categoria = "Europa",
  valore = round(wmean(subEb$idx3, subEb$anweight), 2), tipo_valore = "media",
  n_validi = nrow(subEb), flag_n_basso = nrow(subEb) < N_FLAG, rank = NA_integer_
)

classifica <- rbindlist(rows_b)
setcolorder(classifica, c(COLONNE, "rank"))
cat("righe:", nrow(classifica), "| paesi in classifica:", nrow(rank_dt), "\n")
pos_it <- rank_dt[aggregato == "IT", rank]
cat("Italia: posizione", pos_it, "su", nrow(rank_dt), "paesi (indice_gv3, round 8)\n")
fwrite(classifica, file.path(OUT, "ruolostato_classifica_r8.csv"))

# ============================================================================
# c) ginveco round 1 (2002) — IT vs DE FR ES GB + Europa
#    % accordo (1-2) = posizione anti-intervento ("meno Stato nell'economia, meglio e'")
# ============================================================================
cat("\n--- (c) ginveco round 1 (2002): IT vs DE FR ES GB + Europa ---\n")

r1 <- d[essround == 1]
rows_c <- list()
for (p in c("IT", "DE", "FR", "ES", "GB")) {
  sub <- r1[cntry == p]
  st <- calc_agree5(sub$ginveco, sub$anweight)
  r <- to_rows(st, "ginveco", 1, p)
  if (!is.null(r)) rows_c[[length(rows_c) + 1]] <- r
}
eu1 <- sort(intersect(EU_EFTA_UK, unique(r1$cntry)))
cat("Europa R1:", paste(eu1, collapse = " "), "(n=", length(eu1), "paesi )\n")
subE1 <- r1[cntry %in% eu1]
st <- calc_agree5(subE1$ginveco, subE1$anweight)
r <- to_rows(st, "ginveco", 1, "Europa")
if (!is.null(r)) rows_c[[length(rows_c) + 1]] <- r

ginveco_out <- rbindlist(rows_c)
setcolorder(ginveco_out, COLONNE)
setorder(ginveco_out, aggregato, tipo_valore)
cat("righe:", nrow(ginveco_out), "\n")
fwrite(ginveco_out, file.path(OUT, "ruolostato_ginveco_2002.csv"))

# ============================================================================
# d) gvslvol, gvslvue, gvcldcr (3 item disponibili al round 8) — Italia per
#    gruppi sociodemo standard. gvjbevn richiesto dal tema non esiste al round 8
#    (v. ANOMALIA in testa allo script): sostituito con gli altri 2 item disponibili.
# ============================================================================
cat("\n--- (d) gvslvol/gvslvue/gvcldcr round 8 Italia per gruppi sociodemo ---\n")

it8d <- copy(it8)  # gia' filtrato essround==8 & cntry=="IT"

it8d[, grp_eta := fifelse(agea == 999, NA_character_,
                    fifelse(agea < 35, "15-34", fifelse(agea < 55, "35-54", "55+")))]
it8d[, grp_genere := fifelse(gndr == 1, "uomini", fifelse(gndr == 2, "donne", NA_character_))]
it8d[, grp_istr := fifelse(eisced %in% 1:2, "bassa",
                     fifelse(eisced %in% 3:4, "media",
                     fifelse(eisced %in% 5:7, "alta", NA_character_)))]
it8d[, grp_cond := fifelse(mnactic == 1, "occupati",
                     fifelse(mnactic %in% c(3, 4), "disoccupati",
                     fifelse(mnactic == 6, "pensionati",
                     fifelse(mnactic == 2, "studenti",
                     fifelse(mnactic %in% c(5, 7, 8, 9), "altro inattivo", NA_character_)))))]
it8d[, grp_sett := fifelse(tporgwk %in% 1:3, "pubblico",
                     fifelse(tporgwk == 4, "privato",
                     fifelse(tporgwk == 5, "autonomi", NA_character_)))]
it8d[, grp_hincfel := fifelse(hincfel == 1, "vive comodamente",
                        fifelse(hincfel == 2, "se la cava",
                        fifelse(hincfel == 3, "difficolta",
                        fifelse(hincfel == 4, "grande difficolta", NA_character_))))]
it8d[, grp_dec := fifelse(hinctnta %in% 1:3, "basso",
                    fifelse(hinctnta %in% 4:7, "medio",
                    fifelse(hinctnta %in% 8:10, "alto", NA_character_)))]
it8d[, grp_sind := fifelse(mbtru == 1, "iscritto",
                     fifelse(mbtru == 2, "ex iscritto",
                     fifelse(mbtru == 3, "mai iscritto", NA_character_)))]
it8d[, grp_lr := fifelse(lrscale %in% 0:3, "sinistra",
                   fifelse(lrscale %in% 4:6, "centro",
                   fifelse(lrscale %in% 7:10, "destra", NA_character_)))]

vars_d <- c("gvslvol", "gvslvue", "gvcldcr")
rows_d <- list()

# baseline "tutti"
for (v in vars_d) {
  st <- calc_010(it8d[[v]], it8d$anweight)
  r <- to_rows(st, v, 8, "IT", gruppo = "tutti", categoria = "")
  if (!is.null(r)) rows_d[[length(rows_d) + 1]] <- r
}

add_dim <- function(colname, dimname, livelli) {
  for (lev in livelli) {
    sub <- it8d[get(colname) == lev]
    n_dim <- nrow(sub)
    if (n_dim < N_MIN) log_small(sprintf("sociodemo IT: %s N=%d<50 (nessuna riga)", paste0(dimname, ":", lev), n_dim))
    for (v in vars_d) {
      st <- calc_010(sub[[v]], sub$anweight)
      r <- to_rows(st, v, 8, "IT", gruppo = paste0(dimname, ":", lev), categoria = lev)
      if (!is.null(r)) rows_d[[length(rows_d) + 1]] <<- r
    }
  }
}

add_dim("grp_eta", "eta", c("15-34", "35-54", "55+"))
add_dim("grp_genere", "genere", c("uomini", "donne"))
add_dim("grp_istr", "istruzione", c("bassa", "media", "alta"))
add_dim("grp_cond", "condizione", c("occupati", "disoccupati", "pensionati", "studenti", "altro inattivo"))
add_dim("grp_sett", "settore", c("pubblico", "privato", "autonomi"))
add_dim("grp_hincfel", "reddito_percepito", c("vive comodamente", "se la cava", "difficolta", "grande difficolta"))
add_dim("grp_dec", "decile_reddito", c("basso", "medio", "alto"))
add_dim("grp_sind", "sindacato", c("iscritto", "ex iscritto", "mai iscritto"))
add_dim("grp_lr", "collocazione_politica", c("sinistra", "centro", "destra"))

sociodemo <- rbindlist(rows_d)
setcolorder(sociodemo, COLONNE)
setorder(sociodemo, variabile, gruppo, tipo_valore)
cat("righe:", nrow(sociodemo), "| celle con N<100 (segnalate):", sociodemo[flag_n_basso == TRUE, .N], "\n")
fwrite(sociodemo, file.path(OUT, "ruolostato_sociodemo_it.csv"))

# ============================================================================
# e) slvpens / slvuemp round 8 — IT vs DE FR ES GB + Europa
#    (valutazione dello standard di vita attuale; da leggere insieme a
#    gvslvol/gvslvue nel file (a): aspettativa sul dovere del governo vs
#    percezione di come stanno oggi pensionati e disoccupati)
# ============================================================================
cat("\n--- (e) slvpens/slvuemp round 8: IT vs DE FR ES GB + Europa ---\n")

r8v <- d[essround == 8]
rows_e <- list()
for (p in c("IT", "DE", "FR", "ES", "GB")) {
  sub <- r8v[cntry == p]
  for (v in c("slvpens", "slvuemp")) {
    st <- calc_010(sub[[v]], sub$anweight, pct03 = TRUE)
    r <- to_rows(st, v, 8, p)
    if (!is.null(r)) rows_e[[length(rows_e) + 1]] <- r
  }
}
subE8v <- r8v[cntry %in% eu8]
for (v in c("slvpens", "slvuemp")) {
  st <- calc_010(subE8v[[v]], subE8v$anweight, pct03 = TRUE)
  r <- to_rows(st, v, 8, "Europa")
  if (!is.null(r)) rows_e[[length(rows_e) + 1]] <- r
}

valutazioni <- rbindlist(rows_e)
setcolorder(valutazioni, COLONNE)
setorder(valutazioni, variabile, aggregato, tipo_valore)
cat("righe:", nrow(valutazioni), "\n")
fwrite(valutazioni, file.path(OUT, "ruolostato_valutazioni.csv"))

# ============================================================================
# Segnalazioni finali (N<100 flag, N<50 esclusioni)
# ============================================================================
cat("\n=== Segnalazioni N piccolo (raccolte durante l'esecuzione) ===\n")
if (length(flag_log)) cat(paste(flag_log, collapse = "\n"), "\n") else cat("nessuna\n")

tutti <- rbindlist(list(responsabilita, classifica[, -"rank"], ginveco_out, sociodemo, valutazioni))
piccole <- tutti[flag_n_basso == TRUE][order(n_validi)]
cat("\n=== Tutte le celle con N<100 nei 5 CSV (flag_n_basso, non escluse) ===\n")
print(unique(piccole[, .(variabile, essround, aggregato, gruppo, n_validi)]))

cat("\n=== FATTO ===\n")
cat("File salvati in", OUT, "\n")
cat(" - ruolostato_responsabilita.csv:", nrow(responsabilita), "righe\n")
cat(" - ruolostato_classifica_r8.csv:", nrow(classifica), "righe\n")
cat(" - ruolostato_ginveco_2002.csv:", nrow(ginveco_out), "righe\n")
cat(" - ruolostato_sociodemo_it.csv:", nrow(sociodemo), "righe\n")
cat(" - ruolostato_valutazioni.csv:", nrow(valutazioni), "righe\n")
