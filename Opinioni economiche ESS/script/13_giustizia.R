# 13_giustizia.R
# Tema: giustizia distributiva percepita (European Social Survey, modulo speciale "Justice")
# Round 9 (2018) — unico round in cui il modulo e' stato somministrato (vedi presenza_compatta_italia.csv)
#
# Legge SOLO input/ess_slim.rds (data.table gia' pronto, MAI il CSV da 1,7 GB).
# Filtra sempre sui valori validi del codebook_variabili.json prima di ogni calcolo
# (i codici missing 6/7/8/9, 55/77/88/99 sono NUMERI nel dataset, non NA).
# Pesi: anweight sempre, sia per singolo paese sia per l'aggregato pooled "Europa".

suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
})

DIR <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Opinioni economiche ESS"
IN  <- file.path(DIR, "input")
OUT <- file.path(DIR, "output/estrazioni")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

d  <- readRDS(file.path(IN, "ess_slim.rds"))
cb <- fromJSON(file.path(IN, "codebook_variabili.json"), simplifyVector = FALSE)
stopifnot(is.data.table(d))

# ------------------------------------------------------------------
# Round 9 (2018), unico round del modulo giustizia distributiva
# ------------------------------------------------------------------
d9 <- d[essround == 9]
cat("Round 9 (2018):", nrow(d9), "righe,", uniqueN(d9$cntry), "paesi\n")

# ------------------------------------------------------------------
# Valori validi da codebook (values con missing:false)
# ------------------------------------------------------------------
valid_codes <- function(var) {
  vals  <- cb[[var]]$values
  codes <- vapply(vals, function(v) v$value, character(1))
  miss  <- vapply(vals, function(v) isTRUE(v$missing), logical(1))
  as.numeric(codes[!miss])
}

# ifredu ha il codice 55 ("non ho ancora completato un livello di istruzione")
# marcato come missing:false nel codebook ma NON appartiene alla scala 0-10
# (extent to which the statement applies): va escluso dal calcolo di media/percentili
# sulla scala, altrimenti la distorce pesantemente. Segnalato in anomalie.
IFREDU_SCALA <- setdiff(valid_codes("ifredu"), 55)

filtra_validi <- function(dt, var) {
  vv <- if (var == "ifredu") IFREDU_SCALA else valid_codes(var)
  dt[get(var) %in% vv]
}

n_ifredu_55_it <- nrow(d9[cntry == "IT" & ifredu == 55])
cat("IT: N con ifredu=55 (non ancora completato un livello di istruzione), escluso dalla scala:",
    n_ifredu_55_it, "\n")

# ------------------------------------------------------------------
# Paesi "Europa" per il round corrente: UE27 + UK + NO + CH + IS presenti nel round 9
# ------------------------------------------------------------------
EUROPA_TARGET <- c("AT","BE","BG","CH","CY","CZ","DE","DK","EE","ES","FI","FR","GB","GR",
                    "HR","HU","IE","IS","IT","LT","LU","LV","NL","NO","PL","PT","RO","SE","SI","SK")
EUROPA9 <- sort(intersect(EUROPA_TARGET, unique(d9$cntry)))
cat("Paesi 'Europa' presenti nel round 9 (", length(EUROPA9), "): ",
    paste(EUROPA9, collapse = ", "), "\n", sep = "")
cat("Target 'Europa' NON presenti nel round 9 (esclusi automaticamente):",
    paste(setdiff(EUROPA_TARGET, EUROPA9), collapse = ", "), "\n")
cat("Paesi presenti nel round 9 ma fuori dal set 'Europa' (esclusi, es. Serbia/Montenegro):",
    paste(setdiff(unique(d9$cntry), EUROPA_TARGET), collapse = ", "), "\n\n")

PAESI_BILAT <- c("IT", "DE", "FR", "ES", "GB")

# ------------------------------------------------------------------
# Etichette italiane delle variabili (dal codebook, tradotte)
# ------------------------------------------------------------------
LAB <- c(
  netifr  = "Giustizia percepita della propria paga netta/pensione/sussidio (-4 ingiust. basso .. +4 ingiust. alto)",
  grspfr  = "Giustizia percepita della propria paga lorda (-4 ingiust. bassa .. +4 ingiust. alta)",
  topinfr = "Giustizia percepita dei redditi del 10% dei lavoratori piu' ricco",
  btminfr = "Giustizia percepita dei redditi del 10% dei lavoratori piu' povero",
  wltdffr = "Giustizia percepita delle differenze di ricchezza nel paese",
  sofrdst = "Societa' giusta se reddito e ricchezza sono distribuiti in modo uguale (principio egualitario)",
  sofrwrk = "Societa' giusta se chi lavora sodo guadagna piu' degli altri (principio meritocratico)",
  sofrpr  = "Societa' giusta se si prende cura di poveri e bisognosi a prescindere da quanto danno in cambio",
  sofrprv = "Societa' giusta se le famiglie di status sociale alto godono di privilegi",
  ppldsrv = "Le persone ottengono, nel complesso, cio' che meritano",
  ifredu  = "Ho avuto pari opportunita' di raggiungere il livello di istruzione che cercavo (percezione personale)",
  ifrjob  = "Avrei pari opportunita' di ottenere il lavoro che cerco (percezione personale, ipotetica)",
  evfredu = "Nel proprio paese, tutti hanno pari opportunita' di raggiungere il livello di istruzione che cercano",
  evfrjob = "Nel proprio paese, tutti hanno pari opportunita' di ottenere il lavoro che cercano",
  frprtpl = "Il sistema politico del paese garantisce a tutti una possibilita' equa di partecipare alla politica",
  recskil = "Conoscenze e competenze della persona: quanto influenzano le decisioni di assunzione",
  recexp  = "Esperienza lavorativa della persona: quanto influenza le decisioni di assunzione",
  recknow = "Conoscere qualcuno nell'organizzazione: quanto influenza le decisioni di assunzione",
  recimg  = "Avere un background migratorio: quanto influenza le decisioni di assunzione",
  recgndr = "Il genere della persona: quanto influenza le decisioni di assunzione"
)

# ------------------------------------------------------------------
# Statistiche ponderate (anweight)
# ------------------------------------------------------------------
wpct  <- function(x, w, set) 100 * sum(w[x %in% set]) / sum(w)
wmean <- function(x, w) sum(x * w) / sum(w)

# Log delle celle piccole (N<100 segnalate, N<50 omesse) per il report finale
LOG <- data.table(variabile = character(), aggregato = character(), gruppo = character(),
                   n = integer(), stato = character())
log_add <- function(var, aggregato, gruppo, n, stato) {
  LOG <<- rbind(LOG, data.table(variabile = var, aggregato = aggregato, gruppo = gruppo,
                                 n = n, stato = stato))
}

riga <- function(var, label, aggregato, gruppo, categoria, valore, tipo_valore, n) {
  data.table(tema = "giustizia", variabile = var, label_it = label, essround = 9L, anno = 2018L,
             aggregato = aggregato, gruppo = gruppo, categoria = categoria,
             valore = valore, tipo_valore = tipo_valore, n_validi = n)
}

# Ogni stat_* ritorna anche n (N non ponderato). Ogni emit_* applica la regola
# celle piccole (DATA_MAP: segnalare N<100, non riportare N<50) e poi costruisce le righe.

stat_fair4 <- function(dt, var) {
  x <- dt[[var]]; w <- dt$anweight
  list(pct_basso = round(wpct(x, w, -4:-1), 1),
       pct_giusto = round(wpct(x, w, 0), 1),
       pct_alto  = round(wpct(x, w, 1:4), 1),
       media = round(wmean(x, w), 2), n = length(x))
}
stat_agree5 <- function(dt, var) {
  x <- dt[[var]]; w <- dt$anweight
  list(pct_accordo = round(wpct(x, w, 1:2), 1),
       pct_neutro  = round(wpct(x, w, 3), 1),
       pct_contrario = round(wpct(x, w, 4:5), 1),
       media = round(wmean(x, w), 2), n = length(x))
}
stat_amount5 <- function(dt, var) {
  x <- dt[[var]]; w <- dt$anweight
  list(pct_basso = round(wpct(x, w, 1:2), 1),
       pct_medio = round(wpct(x, w, 3), 1),
       pct_alto  = round(wpct(x, w, 4:5), 1),
       media = round(wmean(x, w), 2), n = length(x))
}
stat_scale010 <- function(dt, var) {
  x <- dt[[var]]; w <- dt$anweight
  list(pct_0_3  = round(wpct(x, w, 0:3), 1),
       media    = round(wmean(x, w), 2),
       pct_7_10 = round(wpct(x, w, 7:10), 1), n = length(x))
}
stat_infl14 <- function(dt, var) {
  x <- dt[[var]]; w <- dt$anweight
  list(pct_poca  = round(wpct(x, w, 1:2), 1),
       pct_molta = round(wpct(x, w, 3:4), 1),
       media = round(wmean(x, w), 2), n = length(x))
}

check_n <- function(var, aggregato, gruppo, n) {
  if (n < 50)  { log_add(var, aggregato, gruppo, n, "OMESSO_N<50"); return(FALSE) }
  if (n < 100) log_add(var, aggregato, gruppo, n, "SEGNALATO_N<100")
  TRUE
}

emit_fair4 <- function(var, label, aggregato, gruppo, dt) {
  s <- stat_fair4(dt, var)
  if (!check_n(var, aggregato, gruppo, s$n)) return(NULL)
  if (var == "wltdffr") {
    cats <- c("ingiustamente piccole", "giuste", "ingiustamente grandi", "media")
  } else {
    cats <- c("ingiustamente basso", "giusto", "ingiustamente alto", "media")
  }
  riga(var, label, aggregato, gruppo, cats,
       c(s$pct_basso, s$pct_giusto, s$pct_alto, s$media),
       c("pct_ingiustamente_basso", "pct_giusto", "pct_ingiustamente_alto", "media"),
       s$n)
}
emit_agree5 <- function(var, label, aggregato, gruppo, dt) {
  s <- stat_agree5(dt, var)
  if (!check_n(var, aggregato, gruppo, s$n)) return(NULL)
  riga(var, label, aggregato, gruppo,
       c("d'accordo", "ne' d'accordo ne' in disaccordo", "in disaccordo", "media"),
       c(s$pct_accordo, s$pct_neutro, s$pct_contrario, s$media),
       c("pct_accordo", "pct_neutro", "pct_contrario", "media"),
       s$n)
}
emit_amount5 <- function(var, label, aggregato, gruppo, dt) {
  s <- stat_amount5(dt, var)
  if (!check_n(var, aggregato, gruppo, s$n)) return(NULL)
  riga(var, label, aggregato, gruppo,
       c("per niente/poco", "un po'", "molto/moltissimo", "media"),
       c(s$pct_basso, s$pct_medio, s$pct_alto, s$media),
       c("pct_basso", "pct_medio", "pct_alto", "media"),
       s$n)
}
emit_scale010 <- function(var, label, aggregato, gruppo, dt) {
  s <- stat_scale010(dt, var)
  if (!check_n(var, aggregato, gruppo, s$n)) return(NULL)
  riga(var, label, aggregato, gruppo,
       c("bassa (0-3)", "media", "alta (7-10)"),
       c(s$pct_0_3, s$media, s$pct_7_10),
       c("pct_0_3", "media", "pct_7_10"),
       s$n)
}
emit_infl14 <- function(var, label, aggregato, gruppo, dt) {
  s <- stat_infl14(dt, var)
  if (!check_n(var, aggregato, gruppo, s$n)) return(NULL)
  riga(var, label, aggregato, gruppo,
       c("poca o nessuna influenza (1-2)", "molta influenza (3-4)", "media"),
       c(s$pct_poca, s$pct_molta, s$media),
       c("pct_poca_influenza", "pct_molta_influenza", "media"),
       s$n)
}

add_bilat_europa <- function(emit_fn, var, dt9) {
  dv <- filtra_validi(dt9, var)
  rows <- vector("list", length(PAESI_BILAT) + 1)
  for (i in seq_along(PAESI_BILAT)) {
    p <- PAESI_BILAT[i]
    rows[[i]] <- emit_fn(var, LAB[[var]], p, "tutti", dv[cntry == p])
  }
  rows[[length(PAESI_BILAT) + 1]] <- emit_fn(var, LAB[[var]], "Europa", "tutti", dv[cntry %in% EUROPA9])
  rbindlist(rows)
}

# ====================================================================
# FILE 1 — giustizia_percezioni.csv
# Percezioni di giustizia su esiti specifici: propria paga, redditi top/bottom 10%,
# differenze di ricchezza, "le persone ottengono ciò che meritano", pari opportunità
# di istruzione/lavoro/partecipazione politica. IT vs DE FR ES GB + Europa.
# ====================================================================
percezioni_list <- list()
for (v in c("netifr", "grspfr", "topinfr", "btminfr", "wltdffr")) {
  percezioni_list[[v]] <- add_bilat_europa(emit_fair4, v, d9)
}
percezioni_list[["ppldsrv"]] <- add_bilat_europa(emit_agree5, "ppldsrv", d9)
for (v in c("ifredu", "ifrjob", "evfredu", "evfrjob")) {
  percezioni_list[[v]] <- add_bilat_europa(emit_scale010, v, d9)
}
percezioni_list[["frprtpl"]] <- add_bilat_europa(emit_amount5, "frprtpl", d9)

giustizia_percezioni <- rbindlist(percezioni_list)
fwrite(giustizia_percezioni, file.path(OUT, "giustizia_percezioni.csv"))
cat("Scritto giustizia_percezioni.csv:", nrow(giustizia_percezioni), "righe\n")

# ====================================================================
# FILE 2 — giustizia_principi.csv
# I due principi rivali di giustizia sociale (sofrdst egualitario vs sofrwrk
# meritocratico) + sofrpr (bisogno) e sofrprv (status, controllo "negativo").
# sofrdst: classifica completa UE/EFTA+UK (tutti i paesi 'Europa' presenti nel round 9).
# sofrwrk/sofrpr/sofrprv: IT vs DE FR ES GB + Europa.
# ====================================================================
principi_list <- list()

dv_dst <- filtra_validi(d9, "sofrdst")
rows_dst <- vector("list", length(EUROPA9) + 1)
for (i in seq_along(EUROPA9)) {
  p <- EUROPA9[i]
  rows_dst[[i]] <- emit_agree5("sofrdst", LAB[["sofrdst"]], p, "tutti", dv_dst[cntry == p])
}
rows_dst[[length(EUROPA9) + 1]] <- emit_agree5("sofrdst", LAB[["sofrdst"]], "Europa", "tutti",
                                                dv_dst[cntry %in% EUROPA9])
principi_list[["sofrdst"]] <- rbindlist(rows_dst)

for (v in c("sofrwrk", "sofrpr", "sofrprv")) {
  principi_list[[v]] <- add_bilat_europa(emit_agree5, v, d9)
}

giustizia_principi <- rbindlist(principi_list)
fwrite(giustizia_principi, file.path(OUT, "giustizia_principi.csv"))
cat("Scritto giustizia_principi.csv:", nrow(giustizia_principi), "righe\n")

# Classifica leggibile su console (per il report finale)
classifica_dst <- giustizia_principi[variabile == "sofrdst" & tipo_valore == "pct_accordo" &
                                        aggregato != "Europa"]
classifica_dst <- classifica_dst[order(-valore)]
classifica_dst[, rank := .I]
cat("\n=== Classifica sofrdst (% d'accordo 'societa' giusta se reddito/ricchezza equamente distribuiti'), UE/EFTA+UK round 9 ===\n")
print(classifica_dst[, .(rank, aggregato, valore, n_validi)], nrows = 100)
cat("Posizione Italia:", classifica_dst[aggregato == "IT", rank], "su", nrow(classifica_dst), "\n\n")

# ====================================================================
# FILE 3 — giustizia_sociodemo_it.csv
# topinfr e sofrdst per l'Italia, per gruppi sociodemografici standard (DATA_MAP).
# ====================================================================
it_d <- d9[cntry == "IT"]

it_d[, grp_eta := fcase(
  agea == 999, NA_character_,
  agea >= 15 & agea <= 34, "15-34",
  agea >= 35 & agea <= 54, "35-54",
  agea >= 55, "55+",
  default = NA_character_
)]
it_d[, grp_genere := fcase(
  gndr == 1, "uomini",
  gndr == 2, "donne",
  default = NA_character_
)]
it_d[, grp_istruzione := fcase(
  eisced %in% 1:2, "bassa",
  eisced %in% 3:4, "media",
  eisced %in% 5:7, "alta",
  default = NA_character_
)]
it_d[, grp_condizione := fcase(
  mnactic == 1, "occupati",
  mnactic %in% 3:4, "disoccupati",
  mnactic == 6, "pensionati",
  mnactic == 2, "studenti",
  mnactic %in% c(5, 7, 8, 9), "altro_inattivo",
  default = NA_character_
)]
it_d[, grp_settore := fcase(
  tporgwk %in% 1:3, "pubblico",
  tporgwk == 4, "privato",
  tporgwk == 5, "autonomi",
  default = NA_character_
)]
it_d[, grp_reddito_perc := fcase(
  hincfel == 1, "comodamente",
  hincfel == 2, "se_la_cava",
  hincfel == 3, "difficolta",
  hincfel == 4, "grande_difficolta",
  default = NA_character_
)]
it_d[, grp_decile := fcase(
  hinctnta %in% 1:3, "basso",
  hinctnta %in% 4:7, "medio",
  hinctnta %in% 8:10, "alto",
  default = NA_character_
)]
it_d[, grp_sindacato := fcase(
  mbtru == 1, "iscritto",
  mbtru == 2, "ex_iscritto",
  mbtru == 3, "mai_iscritto",
  default = NA_character_
)]
it_d[, grp_lr := fcase(
  lrscale %in% 0:3, "sinistra",
  lrscale %in% 4:6, "centro",
  lrscale %in% 7:10, "destra",
  default = NA_character_
)]

group_dims <- list(
  eta               = "grp_eta",
  genere            = "grp_genere",
  istruzione        = "grp_istruzione",
  condizione        = "grp_condizione",
  settore           = "grp_settore",
  reddito_percepito = "grp_reddito_perc",
  decile_reddito    = "grp_decile",
  sindacato         = "grp_sindacato",
  lr                = "grp_lr"
)
group_levels <- list(
  eta               = c("15-34", "35-54", "55+"),
  genere            = c("uomini", "donne"),
  istruzione        = c("bassa", "media", "alta"),
  condizione        = c("occupati", "disoccupati", "pensionati", "studenti", "altro_inattivo"),
  settore           = c("pubblico", "privato", "autonomi"),
  reddito_percepito = c("comodamente", "se_la_cava", "difficolta", "grande_difficolta"),
  decile_reddito    = c("basso", "medio", "alto"),
  sindacato         = c("iscritto", "ex_iscritto", "mai_iscritto"),
  lr                = c("sinistra", "centro", "destra")
)

vars_sociodemo <- list(topinfr = emit_fair4, sofrdst = emit_agree5)
sociodemo_list <- list()
for (v in names(vars_sociodemo)) {
  emit_fn <- vars_sociodemo[[v]]
  dv <- filtra_validi(it_d, v)
  for (dim_name in names(group_dims)) {
    col <- group_dims[[dim_name]]
    for (lvl in group_levels[[dim_name]]) {
      gruppo_lbl <- paste0(dim_name, ":", lvl)
      sub <- dv[get(col) == lvl]
      row <- emit_fn(v, LAB[[v]], "IT", gruppo_lbl, sub)
      if (!is.null(row)) sociodemo_list[[length(sociodemo_list) + 1]] <- row
    }
  }
}
giustizia_sociodemo_it <- rbindlist(sociodemo_list)
fwrite(giustizia_sociodemo_it, file.path(OUT, "giustizia_sociodemo_it.csv"))
cat("Scritto giustizia_sociodemo_it.csv:", nrow(giustizia_sociodemo_it), "righe\n")

# ====================================================================
# FILE 4 — giustizia_criteri_paga.csv
# ATTENZIONE (vedi anomalie nel report): le variabili rec* misurano, per testo del
# codebook, l'influenza di 5 fattori sulle DECISIONI DI ASSUNZIONE (recruitment),
# non criteri di determinazione della paga. IT vs Europa.
# ====================================================================
criteri_list <- list()
for (v in c("recskil", "recexp", "recknow", "recimg", "recgndr")) {
  dv <- filtra_validi(d9, v)
  rows <- list(
    emit_infl14(v, LAB[[v]], "IT", "tutti", dv[cntry == "IT"]),
    emit_infl14(v, LAB[[v]], "Europa", "tutti", dv[cntry %in% EUROPA9])
  )
  criteri_list[[v]] <- rbindlist(rows)
}
giustizia_criteri_paga <- rbindlist(criteri_list)
fwrite(giustizia_criteri_paga, file.path(OUT, "giustizia_criteri_paga.csv"))
cat("Scritto giustizia_criteri_paga.csv:", nrow(giustizia_criteri_paga), "righe\n")

# ====================================================================
# LOG celle piccole
# ====================================================================
cat("\n=== LOG celle piccole (N<100 segnalate, N<50 omesse) ===\n")
if (nrow(LOG) == 0) {
  cat("Nessuna cella sotto soglia 100.\n")
} else {
  print(LOG[order(stato, variabile, aggregato, gruppo)], nrows = 200)
}

# ====================================================================
# RIEPILOGO FINALE compatto (i numeri di dettaglio si leggono dai CSV in output/estrazioni/)
# ====================================================================
cat("\n########## RIEPILOGO ##########\n")
cat("File scritti in", OUT, "\n")
cat(" - giustizia_percezioni.csv   :", nrow(giustizia_percezioni), "righe\n")
cat(" - giustizia_principi.csv     :", nrow(giustizia_principi), "righe\n")
cat(" - giustizia_sociodemo_it.csv :", nrow(giustizia_sociodemo_it), "righe\n")
cat(" - giustizia_criteri_paga.csv :", nrow(giustizia_criteri_paga), "righe\n")
cat("########## FINE ##########\n")
