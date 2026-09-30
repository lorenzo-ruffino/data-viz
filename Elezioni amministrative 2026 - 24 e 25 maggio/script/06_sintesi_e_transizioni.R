# Sintesi pre/post per comuni superiori e tabella transizioni (Sankey)
# Elezioni amministrative 24-25 maggio 2026
#
# Input:
#   input/comuni_al_voto_2026.csv         → sindaco uscente + coalizione
#   output/risultati/{comuni,candidati,liste}.csv         → dati Ministero
#   output/risultati_sicilia/{comuni,candidati,liste}.csv → dati Sicilia
#
# Output:
#   output/sintesi_comuni.csv     → una riga per comune superiore con
#                                   sindaco uscente + esito 2026
#   output/transizioni_sankey.csv → aggregazione (coalizione_uscente,
#                                   coalizione_post) per Sankey
#
# Regola vincitore al primo turno:
#   • Resto d'Italia (legge nazionale): candidato con perc > 50%
#   • Sicilia (legge regionale):        candidato con perc >= 40%
# Altrimenti: ballottaggio.
#
# Lo script è idempotente: rilanciarlo dopo aggiornamento dei risultati
# rifà tutto da capo coi nuovi dati di scrutinio.

library(data.table)
library(stringr)

# Helper: normalizza codice ISTAT a 6 cifre con zero-padding.
# Necessario perché alcune sorgenti lo scrivono come integer (perdendo lo
# zero iniziale: "84001" anziché "084001").
pad_istat <- function(x) sprintf("%06d", as.integer(x))

PROJ_DIR <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Elezioni amministrative 2026 - 24 e 25 maggio"
INP_DIR  <- file.path(PROJ_DIR, "input")
OUT_DIR  <- file.path(PROJ_DIR, "output")

# ==============================================================================
# 1. CARICO COMUNI AL VOTO + SINDACO USCENTE
# ==============================================================================

comuni_voto <- fread(file.path(INP_DIR, "comuni_al_voto_2026.csv"))
comuni_sup  <- comuni_voto[tipologia == "SUP",
                           .(codice_istat = sprintf("%06d", as.integer(codice_istat)),
                             nome_istat,
                             regione,
                             provincia,
                             popolazione = popolazione_31_12_2021,
                             elettori    = elettori_31_12_2025,
                             sindaco_uscente_cognome = sindaco_cognome,
                             sindaco_uscente_nome    = sindaco_nome,
                             sindaco_uscente_lista   = sindaco_lista,
                             coalizione_uscente_raw  = coalizione)]

cat("Comuni superiori al voto:", nrow(comuni_sup), "\n")

# Normalizza la coalizione uscente alle stesse etichette che useremo per il post
norm_coal <- function(x) {
  fcase(
    x == "centrodestra",          "Centrodestra",
    x == "civico_centrodestra",   "Civico centrodestra",
    x == "centrosinistra",        "Centrosinistra",
    x == "civico_centrosinistra", "Civico centrosinistra",
    x == "M5S",                   "Movimento 5 Stelle",
    x == "M5S_CSX",               "Movimento 5 Stelle",
    x == "civico_altro",          "Civico altro",
    default = "Sconosciuta"
  )
}
comuni_sup[, coalizione_uscente := norm_coal(coalizione_uscente_raw)]

# ==============================================================================
# 2. CARICO RISULTATI MINISTERO + SICILIA (filtrati superiori)
# ==============================================================================

cand_min <- fread(file.path(OUT_DIR, "risultati", "candidati.csv"),
                  colClasses = list(character = c("perc", "perc_lis")))
liste_min <- fread(file.path(OUT_DIR, "risultati", "liste.csv"),
                   colClasses = list(character = c("perc")))
com_min  <- fread(file.path(OUT_DIR, "risultati", "comuni.csv"), na.strings = "")

cand_sic <- fread(file.path(OUT_DIR, "risultati_sicilia", "candidati.csv"))
liste_sic <- fread(file.path(OUT_DIR, "risultati_sicilia", "liste.csv"))
com_sic  <- fread(file.path(OUT_DIR, "risultati_sicilia", "comuni.csv"))

# Normalizza i codici ISTAT a 6 caratteri ovunque
for (dt in list(cand_min, liste_min, com_min, cand_sic, liste_sic, com_sic)) {
  dt[, codice_istat := pad_istat(codice_istat)]
}

# Helper conversione percentuale "54,61" -> 54.61
parse_perc <- function(x) suppressWarnings(as.numeric(gsub(",", ".", x)))

# Unifico schema candidati: codice_istat, cognome_nome, voti, perc, n_liste
cand_min_u <- cand_min[, .(codice_istat,
                            cognome_nome = trimws(paste(cognome, nome)),
                            voti         = as.integer(voti),
                            perc         = parse_perc(perc),
                            n_liste,
                            pos_cand)]
cand_sic_u <- cand_sic[, .(codice_istat,
                            cognome_nome,
                            voti         = as.integer(voti),
                            perc         = parse_perc(perc),
                            n_liste,
                            pos_cand)]

cand_all <- rbindlist(list(cand_min_u, cand_sic_u))

# Liste unificate per inferire coalizione del vincitore (solo descr_lista)
liste_min_u <- liste_min[, .(codice_istat, pos_cand, descr_lista)]
liste_sic_u <- liste_sic[, .(codice_istat, pos_cand, descr_lista)]
liste_all   <- rbindlist(list(liste_min_u, liste_sic_u))

# Sezioni scrutinate per ciascun comune (per diagnostica)
sez_min <- com_min[, .(codice_istat,
                       sz_perv = sz_p_sind, sz_tot,
                       perc_scrut = round(100 * sz_p_sind / sz_tot, 1),
                       fonte = "Ministero")]
sez_sic <- com_sic[, .(codice_istat,
                       sz_perv = NA_integer_, sz_tot = NA_integer_,
                       perc_scrut = NA_real_,
                       fonte = "Sicilia")]
sez_all <- rbindlist(list(sez_min, sez_sic))

# ==============================================================================
# 3. INFERENZA COALIZIONE DAL NOME DELLE LISTE
# ==============================================================================

# Dizionario keyword → categoria di partito.
# (case-insensitive; cerco substring nella descrizione lista)
#
# NB: ho escluso volutamente partiti ambigui che in alcuni contesti compaiono
# nel polo opposto rispetto a quello tradizionale:
#   - DEMOCRAZIA CRISTIANA, UDC, UNIONE DI CENTRO → spesso alleati CSX (es.
#     De Luca a Salerno con la DC di Rotondi)
#   - PRIMA L'ITALIA → talvolta lista Lega, talvolta civica
#   - AZIONE → centrista, oscilla
RX_CDX <- paste0(
  "FRATELLI D[' ]ITALIA|\\bFDI\\b|",
  "FORZA ITALIA|",
  "(?:^|[^A-Z])LEGA(?:$|[^A-Z])|LEGA SALVINI|LEGA NORD|LEGA SICILIA|",
  "NOI MODERATI|",
  "ALTERNATIVA POPOLARE|",
  # CDU = Cristiani Democratici Uniti (Nuovo CDU): partito centrista/CDX
  "NUOVO\\s+CDU|CRISTIANI\\s+DEMOCRATICI\\s+UNITI"
)
RX_CSX <- paste0(
  "PARTITO DEMOCRATICO|(?:^|[^A-Z])PD(?:$|[^A-Z])|",
  "DEMOCRATICI E PROGRESSISTI|",
  "ALLEANZA VERDI[ -]?E[ -]?SINISTRA|\\bAVS\\b|VERDI E SINISTRA|",
  "EUROPA VERDE|SINISTRA ITALIANA|\\+EUROPA|PIU[' ]?\\s*EUROPA|",
  "ITALIA VIVA|ARTICOLO UNO|",
  "PARTITO SOCIALISTA|\\bPSI\\b|\\bAVANTI[- ]?PSI\\b|",
  "RIFONDAZIONE|POTERE AL POPOLO|UNIONE POPOLARE|PROGRESSISTI"
)
RX_M5S <- "MOVIMENTO 5 STELLE|\\bM5S\\b|MOVIMENTO CINQUE STELLE"

detect_party_mask <- function(descr_vec) {
  txt <- toupper(descr_vec)
  list(
    cdx = grepl(RX_CDX, txt, perl = TRUE),
    csx = grepl(RX_CSX, txt, perl = TRUE),
    m5s = grepl(RX_M5S, txt, perl = TRUE)
  )
}

# Per ogni candidato (codice_istat, pos_cand) determina la coalizione.
# Logica:
#   - Ogni lista è "polo CDX" se contiene un partito CDX, "polo CSX/M5S"
#     se contiene un partito CSX o M5S (M5S accorpato a CSX in coalizioni
#     locali tipiche del campo largo); altrimenti "civica".
#   - Se ci sono partiti di entrambi i poli                       → Civico altro
#   - Se ≥2 liste del polo CDX, 0 CSX/M5S                         → Centrodestra
#   - Se 1 lista del polo CDX, 0 CSX/M5S                          → Civico centrodestra
#   - Se ≥2 liste del polo CSX/M5S, 0 CDX                         → Centrosinistra
#   - Se 1 lista del polo CSX/M5S, 0 CDX                          → Civico centrosinistra
#     Eccezione: se l'unica lista partitica è M5S (senza PD/AVS)  → Movimento 5 Stelle
#   - Se solo civiche                                              → Civico altro
infer_coalizione <- function(liste_dt) {
  liste_dt <- copy(liste_dt)
  mask <- detect_party_mask(liste_dt$descr_lista)
  liste_dt[, `:=`(is_cdx = mask$cdx, is_csx = mask$csx, is_m5s = mask$m5s)]
  # Un'unica lista può menzionare sia CSX che M5S (es. "M5S - AVS"): conta
  # come una sola lista del polo CSX/M5S.
  liste_dt[, polo_csx_m5s := is_csx | is_m5s]
  liste_dt[, polo_cdx     := is_cdx]
  liste_dt[, is_civica    := !is_cdx & !is_csx & !is_m5s]

  agg <- liste_dt[, .(
    n_liste       = .N,
    n_polo_cdx    = sum(polo_cdx),
    n_polo_csx    = sum(polo_csx_m5s),
    n_solo_m5s    = sum(is_m5s & !is_csx & !is_cdx),
    n_civiche     = sum(is_civica)
  ), by = .(codice_istat, pos_cand)]

  agg[, coalizione_post := fcase(
    n_polo_cdx > 0 & n_polo_csx > 0,                          "Civico altro",
    n_polo_cdx >= 2,                                          "Centrodestra",
    n_polo_cdx == 1,                                          "Civico centrodestra",
    # CSX puro / con M5S in coalizione
    n_polo_csx >= 2,                                          "Centrosinistra",
    n_polo_csx == 1 & n_solo_m5s == n_polo_csx,               "Movimento 5 Stelle",
    n_polo_csx == 1,                                          "Civico centrosinistra",
    default = "Civico altro"
  )]
  agg
}

coal_per_cand <- infer_coalizione(liste_all)
cat("Inferita coalizione per", nrow(coal_per_cand), "candidati totali\n")

# ==============================================================================
# 4. DETERMINA VINCITORE PER COMUNE
# ==============================================================================

# Soglia per primo turno: Sicilia 40%, resto Italia 50%
soglia_comune <- function(regione) ifelse(regione == "SICILIA", 40, 50)

# Top candidato per voti in ogni comune
cand_all[, rk := frank(-voti, ties.method = "first"), by = codice_istat]
top1 <- cand_all[rk == 1]
top2 <- cand_all[rk == 2]

# Merge coalizione del top1
top1 <- merge(top1, coal_per_cand[, .(codice_istat, pos_cand, coalizione_post,
                                       n_polo_cdx, n_polo_csx, n_solo_m5s,
                                       n_civiche)],
              by = c("codice_istat", "pos_cand"), all.x = TRUE)

# ==============================================================================
# 5. COSTRUISCO FILE 1: SINTESI PER COMUNE
# ==============================================================================

sintesi <- merge(comuni_sup,
                 top1[, .(codice_istat,
                          vincitore_cognome_nome = cognome_nome,
                          vincitore_voti = voti,
                          vincitore_perc = perc,
                          coalizione_post_raw = coalizione_post)],
                 by = "codice_istat", all.x = TRUE)

sintesi <- merge(sintesi,
                 top2[, .(codice_istat,
                          secondo_cognome_nome = cognome_nome,
                          secondo_perc = perc)],
                 by = "codice_istat", all.x = TRUE)

sintesi <- merge(sintesi,
                 sez_all[, .(codice_istat, sz_perv, sz_tot, perc_scrut, fonte)],
                 by = "codice_istat", all.x = TRUE)

# Applica soglia per definire esito
sintesi[, soglia := soglia_comune(regione)]

# Distinzione fra "dati assenti" (comune non estratto: Sardegna/altre autonomie)
# e "scrutinio in corso" (comune estratto ma niente voti/sezioni ancora):
#   - dati_assenti:           non c'è candidato per quel comune
#   - scrutinio_in_corso:     candidato presente ma voti tutti NA o 0
#   - vincitore_primo_turno:  perc primo >= soglia
#   - ballottaggio:           perc primo < soglia (sopra il rumore)
sintesi[, esito := fcase(
  is.na(vincitore_cognome_nome),                              "dati_assenti",
  is.na(vincitore_voti) | (vincitore_voti == 0 &
    (is.na(perc_scrut) | perc_scrut == 0)),                   "scrutinio_in_corso",
  is.na(vincitore_perc),                                      "scrutinio_in_corso",
  vincitore_perc >= soglia,                                   "vincitore_primo_turno",
  default = "ballottaggio"
)]

# La colonna "coalizione_post" finale dipende dall'esito
sintesi[, coalizione_post := fcase(
  esito == "vincitore_primo_turno", coalizione_post_raw,
  esito == "ballottaggio",          "Ballottaggio",
  esito == "scrutinio_in_corso",    "Scrutinio in corso",
  default = NA_character_
)]

# ==============================================================================
# 5b. OVERRIDE MANUALI (per i casi che il classifier non può inferire
#     automaticamente dalle sole liste — richiedono conoscenza del contesto
#     locale, es. Palmieri ad Alpignano)
# ==============================================================================

override_path <- file.path(INP_DIR, "override_coalizioni.csv")
if (file.exists(override_path)) {
  ovr <- fread(override_path)
  ovr[, codice_istat := pad_istat(codice_istat)]
  applied <- 0L
  for (i in seq_len(nrow(ovr))) {
    idx <- which(sintesi$codice_istat == ovr$codice_istat[i] &
                 sintesi$esito == "vincitore_primo_turno")
    if (length(idx) == 1) {
      sintesi$coalizione_post[idx] <- ovr$coalizione_post_override[i]
      applied <- applied + 1L
    }
  }
  cat("Override applicati:", applied, "/", nrow(ovr), "\n")
}

# Ordina le colonne finali
sintesi_out <- sintesi[, .(
  codice_istat,
  comune       = nome_istat,
  regione,
  provincia,
  popolazione,
  elettori,
  sindaco_uscente_cognome,
  sindaco_uscente_nome,
  sindaco_uscente_lista,
  coalizione_uscente,
  vincitore_cognome_nome,
  vincitore_voti,
  vincitore_perc,
  secondo_cognome_nome,
  secondo_perc,
  soglia_primo_turno = soglia,
  esito,
  coalizione_post,
  perc_scrut,
  sz_perv,
  sz_tot,
  fonte
)][order(regione, provincia, comune)]

fwrite(sintesi_out, file.path(OUT_DIR, "sintesi_comuni.csv"))
cat("Salvato:", file.path(OUT_DIR, "sintesi_comuni.csv"), "(", nrow(sintesi_out), "righe )\n")

# ==============================================================================
# 6. COSTRUISCO FILE 2: TRANSIZIONI PRE→POST per SANKEY
# ==============================================================================

transizioni <- sintesi_out[!is.na(coalizione_post),
  .(n_comuni    = .N,
    popolazione = sum(popolazione, na.rm = TRUE)),
  by = .(coalizione_uscente, coalizione_post)
][order(coalizione_uscente, coalizione_post)]

fwrite(transizioni, file.path(OUT_DIR, "transizioni_sankey.csv"))
cat("Salvato:", file.path(OUT_DIR, "transizioni_sankey.csv"),
    "(", nrow(transizioni), "transizioni )\n")

# ==============================================================================
# 7. DIAGNOSTICA
# ==============================================================================

cat("\n--- DIAGNOSTICA ---\n")
cat("\nDistribuzione esiti:\n")
print(sintesi_out[, .N, by = esito][order(-N)])

cat("\nMatrice transizioni (n. comuni):\n")
mat <- dcast(transizioni, coalizione_uscente ~ coalizione_post,
             value.var = "n_comuni", fill = 0)
print(mat)

cat("\nComuni con scrutinio incompleto / vincitore non determinato:\n")
print(sintesi_out[esito == "dati_assenti" | (esito == "ballottaggio" & perc_scrut < 50),
                  .(regione, comune, perc_scrut, vincitore_perc, esito)][1:15])
