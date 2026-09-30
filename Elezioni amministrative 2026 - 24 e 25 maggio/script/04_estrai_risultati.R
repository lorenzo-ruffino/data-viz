# Estrazione candidati sindaco, liste di sostegno e risultati
# Elezioni amministrative 24-25 maggio 2026 — primo turno
# Fonte: https://eleapi.interno.gov.it (API non documentata Ministero Interno)
#
# Endpoint usato:
#   GET /siel/PX/scrutiniG/DE/20260524/TE/08/PR/{prov}/CM/{com}
#     a livello di comune, RE NON va incluso nel path
#
# Output:
#   output/risultati/comuni.csv      → una riga per comune con totali e sezioni
#   output/risultati/candidati.csv   → una riga per candidato sindaco per comune
#   output/risultati/liste.csv       → una riga per lista di sostegno per candidato per comune
#
# Lo script è idempotente: può essere rilanciato durante lo spoglio
# per aggiornare i CSV con i voti man mano che arrivano.

library(httr)
library(jsonlite)
library(dplyr)
library(data.table)
library(parallel)
library(stringr)

# ==============================================================================
# CONFIGURAZIONE
# ==============================================================================

DATE          <- "20260524"
TIPO_ELEZIONE <- "08"      # 08 = comunali primo turno
PREFIX_ELEZ   <- "G"       # prefisso endpoint per comunali
N_WORKERS     <- 4L
MAX_RETRY     <- 3L

# Se TRUE estrae solo i comuni "superiori" (>15.000 ab.), cioè i comuni con
# sistema maggioritario a doppio turno e voto disgiunto + ripartizione liste
# proporzionale. L'API del Ministero li marca con tipo_comune = "M".
# Se FALSE estrae tutti i comuni al voto (101 superiori + 560 inferiori).
ONLY_SUPERIORI <- TRUE

PROJ_DIR   <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Elezioni amministrative 2026 - 24 e 25 maggio"
OUTPUT_DIR <- file.path(PROJ_DIR, "output", "risultati")
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# Mapping codice elettorale (Ministero) <-> codice ISTAT
# Anagrafica condivisa con il progetto referendum
CODICI_CSV <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Referendum_Giustizia_2026/utilities/codici_comuni_15-03-2026.csv"

headers <- c(
  'accept'            = 'application/json, text/plain, */*',
  'accept-language'   = 'it-IT,it;q=0.9,en;q=0.8',
  'origin'            = 'https://elezioni.interno.gov.it',
  'referer'           = 'https://elezioni.interno.gov.it/',
  'sec-fetch-dest'    = 'empty',
  'sec-fetch-mode'    = 'cors',
  'sec-fetch-site'    = 'same-site'
)

# ==============================================================================
# 1. LISTA COMUNI AL VOTO
# ==============================================================================

cat("Scarico lista enti dal Ministero...\n")
url_enti  <- paste0('https://eleapi.interno.gov.it/siel/PX/getenti', PREFIX_ELEZ,
                    '/DE/', DATE, '/TE/', TIPO_ELEZIONE)
resp <- GET(url_enti, add_headers(.headers = headers))
data_enti <- fromJSON(rawToChar(resp$content))$enti

regione_mapping <- setNames(
  data_enti$desc[data_enti$tipo == "RE"],
  substr(data_enti$cod[data_enti$tipo == "RE"], 1, 2)
)
provincia_mapping <- setNames(
  data_enti$desc[data_enti$tipo == "PR"],
  substr(data_enti$cod[data_enti$tipo == "PR"], 3, 5)
)

df_comuni <- data_enti[data_enti$tipo == "CM", ] %>%
  mutate(
    cod_regione    = substr(cod, 1, 2),
    cod_provincia  = substr(cod, 3, 5),
    cod_comune     = substr(cod, 6, 9),
    desc_regione   = regione_mapping[cod_regione],
    desc_provincia = provincia_mapping[cod_provincia]
  ) %>%
  select(cod_regione, desc_regione, cod_provincia, desc_provincia,
         cod_comune, desc_comune = desc, tipo_comune)

cat("Comuni al voto:", nrow(df_comuni),
    "  (M =", sum(df_comuni$tipo_comune == "M"),
    " >15k ab., N =", sum(df_comuni$tipo_comune == "N"),
    " <=15k ab.)\n")

if (isTRUE(ONLY_SUPERIORI)) {
  df_comuni <- df_comuni[df_comuni$tipo_comune == "M", ]
  cat("Filtro ONLY_SUPERIORI attivo: estratti solo", nrow(df_comuni),
      "comuni > 15k ab.\n")
}

# ==============================================================================
# 2. FUNZIONE FETCH SCRUTINIO COMUNE
# ==============================================================================

fetch_scrutinio <- function(row_list) {
  library(httr)
  library(jsonlite)

  headers <- c(
    'accept'   = 'application/json, text/plain, */*',
    'origin'   = 'https://elezioni.interno.gov.it',
    'referer'  = 'https://elezioni.interno.gov.it/'
  )

  url <- paste0(
    'https://eleapi.interno.gov.it/siel/PX/scrutini',
    row_list$PREFIX_ELEZ,
    '/DE/', row_list$DATE,
    '/TE/', row_list$TIPO_ELEZIONE,
    '/PR/', row_list$cod_provincia,
    '/CM/', row_list$cod_comune
  )

  for (attempt in 1:row_list$MAX_RETRY) {
    Sys.sleep(0.1 * attempt)

    res <- tryCatch({
      r    <- GET(url, add_headers(.headers = headers), timeout(20))
      body <- rawToChar(r$content)

      if (grepl("^<", trimws(body))) {
        if (attempt < row_list$MAX_RETRY) return(NULL)  # rate-limited, retry
        return(list(stato = "rate_limited"))
      }

      data <- fromJSON(body, simplifyVector = FALSE)

      if (!is.null(data$Error)) return(list(stato = "dati_non_trovati"))

      list(stato = "ok", data = data)
    }, error = function(e) {
      if (attempt < row_list$MAX_RETRY) return(NULL)
      list(stato = paste0("errore: ", conditionMessage(e)))
    })

    if (!is.null(res)) break
  }

  if (is.null(res)) res <- list(stato = "max_retry")

  base <- list(
    cod_regione    = row_list$cod_regione,
    desc_regione   = row_list$desc_regione,
    cod_provincia  = row_list$cod_provincia,
    desc_provincia = row_list$desc_provincia,
    cod_comune     = row_list$cod_comune,
    desc_comune    = row_list$desc_comune,
    tipo_comune    = row_list$tipo_comune,
    stato          = res$stato
  )

  if (res$stato != "ok") {
    return(list(
      comune    = list(c(base, list(
        ele_t = NA, vot_t = NA, perc_vot = NA,
        sz_tot = NA, sz_p_sind = NA, sz_p_cons = NA,
        sk_bianche = NA, sk_nulle = NA, sk_contestate = NA,
        tot_vot_cand = NA, tot_vot_lis = NA, sigla_prov = NA,
        data_prec_elez = NA, dt_agg = NA
      ))),
      candidati = list(),
      liste     = list()
    ))
  }

  info <- res$data$int
  cand <- res$data$cand

  perc_vot_num <- if (!is.null(info$perc_vot)) info$perc_vot else NA_character_

  comune_row <- c(base, list(
    ele_t          = info$ele_t,
    vot_t          = info$vot_t,
    perc_vot       = perc_vot_num,
    sz_tot         = info$sz_tot,
    sz_p_sind      = info$sz_p_sind,
    sz_p_cons      = info$sz_p_cons,
    sk_bianche     = info$sk_bianche,
    sk_nulle       = info$sk_nulle,
    sk_contestate  = info$sk_contestate,
    tot_vot_cand   = info$tot_vot_cand,
    tot_vot_lis    = info$tot_vot_lis,
    sigla_prov     = info$sigla_prov,
    data_prec_elez = if (!is.null(info$data_prec_elez)) sprintf("%.0f", info$data_prec_elez) else NA_character_,
    dt_agg         = if (!is.null(info$dt_agg)) sprintf("%.0f", info$dt_agg) else NA_character_
  ))

  cand_rows  <- list()
  liste_rows <- list()

  for (i in seq_along(cand)) {
    c_i <- cand[[i]]
    cand_rows[[length(cand_rows) + 1]] <- list(
      cod_regione   = row_list$cod_regione,
      desc_regione  = row_list$desc_regione,
      cod_provincia = row_list$cod_provincia,
      desc_provincia= row_list$desc_provincia,
      cod_comune    = row_list$cod_comune,
      desc_comune   = row_list$desc_comune,
      pos_cand      = c_i$pos,
      cognome       = c_i$cogn,
      nome          = c_i$nome,
      altro_nome    = if (!is.null(c_i$a_nome)) c_i$a_nome else NA_character_,
      data_nascita  = if (!is.null(c_i$d_nasc)) sprintf("%.0f", c_i$d_nasc) else NA_character_,
      luogo_nascita = if (!is.null(c_i$l_nasc)) c_i$l_nasc else NA_character_,
      voti          = c_i$voti,
      perc          = c_i$perc,
      eletto        = if (!is.null(c_i$eletto)) c_i$eletto else NA_character_,
      tot_vot_lis   = c_i$tot_vot_lis,
      perc_lis      = c_i$perc_lis,
      sg_ass        = c_i$sg_ass,
      n_liste       = length(c_i$liste)
    )

    for (lst in c_i$liste) {
      liste_rows[[length(liste_rows) + 1]] <- list(
        cod_regione   = row_list$cod_regione,
        desc_regione  = row_list$desc_regione,
        cod_provincia = row_list$cod_provincia,
        desc_provincia= row_list$desc_provincia,
        cod_comune    = row_list$cod_comune,
        desc_comune   = row_list$desc_comune,
        pos_cand      = c_i$pos,
        cognome_cand  = c_i$cogn,
        nome_cand     = c_i$nome,
        pos_lista     = lst$pos,
        descr_lista   = lst$descr_lista,
        img_lis       = if (!is.null(lst$img_lis)) lst$img_lis else NA_character_,
        voti          = lst$voti,
        perc          = lst$perc,
        seggi         = lst$seggi
      )
    }
  }

  list(comune = list(comune_row), candidati = cand_rows, liste = liste_rows)
}

# ==============================================================================
# 3. FETCH PARALLELO
# ==============================================================================

job_list <- lapply(seq_len(nrow(df_comuni)), function(i) {
  c(as.list(df_comuni[i, ]), list(
    DATE          = DATE,
    TIPO_ELEZIONE = TIPO_ELEZIONE,
    PREFIX_ELEZ   = PREFIX_ELEZ,
    MAX_RETRY     = MAX_RETRY
  ))
})

cat("Avvio fetch parallelo:", length(job_list), "comuni con", N_WORKERS, "worker...\n")
t0 <- proc.time()

cl <- makeCluster(N_WORKERS)
raw <- parLapply(cl, job_list, fetch_scrutinio)
stopCluster(cl)

elapsed <- round((proc.time() - t0)["elapsed"])
cat("Completato in", elapsed, "secondi\n")

# ==============================================================================
# 4. ASSEMBLA CSV
# ==============================================================================

bind_rows_list <- function(rows) {
  if (length(rows) == 0) return(data.frame())
  rbindlist(lapply(rows, as.data.frame, stringsAsFactors = FALSE), fill = TRUE)
}

comuni_df    <- bind_rows_list(unlist(lapply(raw, `[[`, "comune"),    recursive = FALSE))
candidati_df <- bind_rows_list(unlist(lapply(raw, `[[`, "candidati"), recursive = FALSE))
liste_df     <- bind_rows_list(unlist(lapply(raw, `[[`, "liste"),     recursive = FALSE))

cat("\nStato fetch:\n")
print(table(comuni_df$stato))
cat("Candidati totali:", nrow(candidati_df), "\n")
cat("Liste di sostegno totali:", nrow(liste_df), "\n")

# ==============================================================================
# 5. AGGIUNGE CODICE ISTAT (join via codice elettorale ministeriale)
# ==============================================================================
# Il codice ministeriale a 7 cifre = padding(cod_provincia, 3) + padding(cod_comune, 4)
# corrisponde alle ultime 7 cifre di "CODICE ELETTORALE" nel file anagrafico.

if (file.exists(CODICI_CSV)) {
  codici_istat <- fread(CODICI_CSV, sep = ";", encoding = "Latin-1",
                        na.strings = character(0)) %>%
    rename(cod_elettorale = `CODICE ELETTORALE`,
           codice_istat   = `CODICE ISTAT`) %>%
    mutate(
      codice_istat = gsub('="([^"]*)"', "\\1", codice_istat),
      cod_join     = str_sub(as.character(cod_elettorale), -7, -1)
    ) %>%
    select(cod_join, codice_istat)

  make_join_key <- function(prov, com) {
    paste0(sprintf("%03d", as.integer(prov)),
           sprintf("%04d", as.integer(com)))
  }

  comuni_df[, cod_join := make_join_key(cod_provincia, cod_comune)]
  candidati_df[, cod_join := make_join_key(cod_provincia, cod_comune)]
  liste_df[, cod_join := make_join_key(cod_provincia, cod_comune)]

  comuni_df    <- merge(comuni_df,    codici_istat, by = "cod_join",
                        all.x = TRUE, sort = FALSE)
  candidati_df <- merge(candidati_df, codici_istat, by = "cod_join",
                        all.x = TRUE, sort = FALSE)
  liste_df     <- merge(liste_df,     codici_istat, by = "cod_join",
                        all.x = TRUE, sort = FALSE)

  comuni_df[,    cod_join := NULL]
  candidati_df[, cod_join := NULL]
  liste_df[,     cod_join := NULL]

  # Fallback per comuni di nuova istituzione (fusioni) non ancora in anagrafica:
  # match per nome contro input/comuni_al_voto_2026.csv
  mapping_path <- file.path(PROJ_DIR, "input", "comuni_al_voto_2026.csv")
  if (any(is.na(comuni_df$codice_istat)) && file.exists(mapping_path)) {
    norm_name <- function(x) gsub("[^A-Z0-9]", "",
                                  iconv(toupper(x), to = "ASCII//TRANSLIT"))
    local_map <- fread(mapping_path)[, .(
      key = norm_name(comune),
      codice_istat_local = sprintf("%06d", as.integer(codice_istat))
    )]
    comuni_df[is.na(codice_istat),
              codice_istat := local_map[.(norm_name(desc_comune)),
                                        on = "key", codice_istat_local]]
    # propaga anche su candidati e liste
    refill <- comuni_df[, .(cod_regione, cod_provincia, cod_comune,
                            codice_istat_new = codice_istat)]
    candidati_df <- merge(candidati_df, refill,
                          by = c("cod_regione", "cod_provincia", "cod_comune"),
                          all.x = TRUE, sort = FALSE)
    candidati_df[is.na(codice_istat), codice_istat := codice_istat_new]
    candidati_df[, codice_istat_new := NULL]
    liste_df <- merge(liste_df, refill,
                      by = c("cod_regione", "cod_provincia", "cod_comune"),
                      all.x = TRUE, sort = FALSE)
    liste_df[is.na(codice_istat), codice_istat := codice_istat_new]
    liste_df[, codice_istat_new := NULL]
  }

  cat("Comuni con codice ISTAT mappato:",
      sum(!is.na(comuni_df$codice_istat)), "/", nrow(comuni_df), "\n")
} else {
  cat("ATTENZIONE: anagrafica codici non trovata in", CODICI_CSV, "\n")
}

fwrite(comuni_df,    file.path(OUTPUT_DIR, "comuni.csv"))
fwrite(candidati_df, file.path(OUTPUT_DIR, "candidati.csv"))
fwrite(liste_df,     file.path(OUTPUT_DIR, "liste.csv"))

cat("\nCSV salvati in", OUTPUT_DIR, "\n")
