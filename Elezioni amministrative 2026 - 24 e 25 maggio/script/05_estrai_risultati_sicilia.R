# Estrazione candidati sindaco, liste di sostegno e risultati — SICILIA
# Elezioni amministrative 24-25 maggio 2026
# Fonte: https://www.elezioni.regione.sicilia.it/comunali2026/{PROV}/...
#
# La Sicilia ha legge elettorale autonoma (regione a statuto speciale) e i
# risultati non passano dall'API del Ministero dell'Interno. Il sito della
# Regione pubblica report HTML statici, uno per comune, indicizzati per
# sigla provincia e codice numerico interno (1-3 cifre).
#
# Endpoint scoperti:
#   /comunali2026/{PROV}/ReportDatiLista{PROV}.html
#     pagina indice della provincia, contiene <select name="town"> con
#     l'elenco di (codice, nome) dei comuni al voto.
#
#   /comunali2026/{PROV}/ReportDatiLista{PROV}{N}.html
#     pre-voto: candidati sindaco + liste collegate (senza voti).
#
#   /comunali2026/{PROV}/ReportRisultati{PROV}{N}.html
#     post-voto: totali comune (elettori, votanti, sezioni, bianche, nulle)
#     + candidati sindaco con voti/% + liste collegate con voti/%.
#
#   /comunali2026/{PROV}/ReportCandidatiListe{PROV}{N}.html
#     post-voto: candidati e liste con voti (sottoinsieme di Risultati).
#
# Lo script tenta prima ReportRisultati: se dà "Impossibile fornire i dati"
# o "Nessun Dato disponibile" ripiega su ReportDatiLista (così funziona
# anche prima dello spoglio).
#
# Output (stessa struttura del CSV del Ministero, vedi 04_estrai_risultati.R):
#   output/risultati_sicilia/comuni.csv
#   output/risultati_sicilia/candidati.csv
#   output/risultati_sicilia/liste.csv

library(httr)
library(rvest)
library(xml2)
library(dplyr)
library(data.table)
library(stringr)

PROJ_DIR   <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Elezioni amministrative 2026 - 24 e 25 maggio"
OUTPUT_DIR <- file.path(PROJ_DIR, "output", "risultati_sicilia")
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

BASE <- "https://www.elezioni.regione.sicilia.it/comunali2026"
PROVINCE <- c("AG", "CL", "CT", "EN", "ME", "PA", "RG", "SR", "TP")
PROV_NOMI <- c(AG="AGRIGENTO", CL="CALTANISSETTA", CT="CATANIA", EN="ENNA",
               ME="MESSINA", PA="PALERMO", RG="RAGUSA", SR="SIRACUSA", TP="TRAPANI")

# Se TRUE estrae solo i comuni "superiori" (>15.000 ab.). In Sicilia
# l'informazione viene da input/comuni_al_voto_2026.csv (colonna tipologia:
# SUP = superiore, INF = inferiore).
# Se FALSE estrae tutti i 71 comuni siciliani al voto.
ONLY_SUPERIORI <- TRUE

UA <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36"

# ==============================================================================
# UTILITY: GET + decode ISO-8859-15 -> UTF-8
# ==============================================================================

fetch_html <- function(url) {
  for (attempt in 1:3) {
    r <- tryCatch(GET(url, user_agent(UA), timeout(20)), error = function(e) NULL)
    if (!is.null(r) && status_code(r) == 200) {
      txt <- iconv(rawToChar(r$content), from = "ISO-8859-15", to = "UTF-8", sub = "")
      return(txt)
    }
    Sys.sleep(0.5 * attempt)
  }
  NULL
}

clean_cell <- function(x) {
  x <- gsub("&nbsp;", " ", x, fixed = TRUE)
  x <- gsub("<[^>]+>", "", x)
  x <- gsub("[\r\n\t]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

# Converte "52,14%" -> 52.14 ; "2.612" -> 2612
to_num <- function(x) {
  if (is.null(x) || is.na(x) || x == "") return(NA_real_)
  x <- gsub("%", "", x, fixed = TRUE)
  x <- gsub("\\.", "", x)   # punto = separatore migliaia
  x <- gsub(",", ".", x)
  suppressWarnings(as.numeric(x))
}

# ==============================================================================
# 1. ELENCO COMUNI AL VOTO PER PROVINCIA
# ==============================================================================

discover_comuni <- function(prov) {
  url <- sprintf("%s/%s/ReportDatiLista%s.html", BASE, prov, prov)
  html <- fetch_html(url)
  if (is.null(html)) return(data.frame())

  # parse the <select name="town"> options
  m <- str_match_all(html, '<option value="ReportDatiLista([A-Z]{2})(\\d+)\\.html">([^<]+)</option>')[[1]]
  if (is.null(m) || nrow(m) == 0) return(data.frame())

  data.frame(
    sigla_prov  = as.character(m[, 2]),
    cod_comune  = as.character(m[, 3]),
    desc_comune = trimws(as.character(m[, 4])),
    desc_prov   = unname(PROV_NOMI[prov]),
    stringsAsFactors = FALSE
  )
}

cat("Scopro comuni al voto per provincia...\n")
comuni_list <- do.call(rbind, lapply(PROVINCE, discover_comuni))
cat("Comuni siciliani al voto:", nrow(comuni_list),
    "in", length(unique(comuni_list$sigla_prov)), "province\n")

# Annota tipologia (SUP / INF) e codice ISTAT via join per nome
mapping_path <- file.path(PROJ_DIR, "input", "comuni_al_voto_2026.csv")
if (file.exists(mapping_path)) {
  norm_name_disc <- function(x) gsub("[^A-Z0-9]", "",
                                     iconv(toupper(x), to = "ASCII//TRANSLIT"))
  map_sic <- as.data.table(fread(mapping_path))[regione == "SICILIA",
              .(key = norm_name_disc(nome_istat),
                tipologia,
                codice_istat = sprintf("%06d", as.integer(codice_istat)))]
  comuni_list$key       <- norm_name_disc(comuni_list$desc_comune)
  comuni_list           <- merge(comuni_list, map_sic, by = "key",
                                 all.x = TRUE, sort = FALSE)
  comuni_list$key       <- NULL
  cat("  Superiori (SUP):", sum(comuni_list$tipologia == "SUP", na.rm = TRUE),
      " Inferiori (INF):", sum(comuni_list$tipologia == "INF", na.rm = TRUE),
      " Non mappati:", sum(is.na(comuni_list$tipologia)), "\n")
}

if (isTRUE(ONLY_SUPERIORI)) {
  if (!"tipologia" %in% names(comuni_list)) {
    stop("ONLY_SUPERIORI = TRUE ma manca il file di mapping comuni_al_voto_2026.csv")
  }
  comuni_list <- comuni_list[!is.na(comuni_list$tipologia) &
                             comuni_list$tipologia == "SUP", ]
  cat("Filtro ONLY_SUPERIORI attivo: estratti solo", nrow(comuni_list),
      "comuni siciliani > 15k ab.\n")
}

# ==============================================================================
# 2. PARSER DEL REPORT COMUNE
# ==============================================================================

# Estrae candidati sindaco + liste collegate da una pagina HTML di Report.
# Funziona sia con ReportRisultati (post-voto, con voti e %) che con
# ReportDatiLista (pre-voto, solo candidati e liste).
parse_report <- function(html, prov, town_code, town_name) {
  is_empty <- grepl("Nessun Dato disponibile", html, fixed = TRUE) ||
              grepl("Impossibile fornire i dati", html, fixed = TRUE)

  comune_row <- list(
    sigla_prov     = prov,
    desc_prov      = unname(PROV_NOMI[prov]),
    cod_comune     = town_code,
    desc_comune    = town_name,
    fonte          = NA_character_,
    ele_t          = NA_integer_,
    vot_t          = NA_integer_,
    perc_vot       = NA_real_,
    sz_tot         = NA_integer_,
    sz_perv        = NA_integer_,
    seggi          = NA_integer_,
    sk_bianche     = NA_integer_,
    sk_nulle       = NA_integer_,
    tot_vot_cand   = NA_integer_,
    tot_vot_lis    = NA_integer_,
    popolazione    = NA_integer_,
    stato          = if (is_empty) "no_data" else "ok"
  )

  candidati_rows <- list()
  liste_rows     <- list()

  if (is_empty) {
    return(list(comune = list(comune_row),
                candidati = candidati_rows, liste = liste_rows))
  }

  # ------- TOTALI COMUNE (solo se presenti, cioè post-voto) -------
  # Cerco la TABELLA PIÙ INTERNA che contiene insieme "Sezioni", "Elettori",
  # "Votanti": il wrapper della pagina contiene tutto il testo, quindi
  # iterando su tutte le tabelle in ordine prenderei le celle sbagliate.
  # Filtro alle tabelle che contengono i 3 marker ma NESSUNA tabella nidificata
  # con gli stessi marker.
  doc <- tryCatch(read_html(html), error = function(e) NULL)
  if (!is.null(doc)) {
    tables <- html_elements(doc, "table")
    has_keys <- function(tb) {
      txt <- html_text(tb)
      grepl("Sezioni", txt) && grepl("Elettori", txt) && grepl("Votanti", txt)
    }
    keep_idx <- which(vapply(tables, has_keys, logical(1)))
    if (length(keep_idx) > 0) {
      # Tra le tabelle che matchano, prendo quella con MENO celle (la più
      # interna / specifica).
      ncells <- vapply(tables[keep_idx], function(tb) length(html_elements(tb, "td")),
                       integer(1))
      tb_totali <- tables[[keep_idx[which.min(ncells)]]]
      cells <- html_text(html_elements(tb_totali, "td"), trim = TRUE)
      num_cells <- cells[grepl("^[0-9.,%]+$", cells)]
      # Stesso parsing di to_num: % via, "." migliaia via, "," → "."
      num_clean <- gsub(",", ".", gsub("\\.", "",
                     gsub("%", "", num_cells)))
      # Per le percentuali abbiamo rimosso "." e poi messo "." al posto di ",":
      # ma "59,13" prima diventa "5913" e poi "5913" perché "," era già rimossa.
      # Devo fare l'ordine giusto: prima "," → "_", poi rimuovere ".", poi "_" → "."
      num_clean <- num_cells
      num_clean <- gsub("%", "", num_clean)
      num_clean <- gsub(",", "_DEC_", num_clean, fixed = TRUE)
      num_clean <- gsub(".", "",     num_clean, fixed = TRUE)
      num_clean <- gsub("_DEC_", ".", num_clean, fixed = TRUE)
      nums <- suppressWarnings(as.numeric(num_clean))
      nums <- nums[!is.na(nums)]
      # Struttura post-voto: 21 (sz_tot), 19924 (ele_t), 16 (seggi),
      # 11780 (vot_t), 59.13 (perc_vot), 11542 (tot_vot_cand),
      # 11191 (tot_vot_lis), 236 (sk_non_valide), 42 (sk_bianche)
      if (length(nums) >= 6) {
        comune_row$sz_tot       <- nums[1]
        comune_row$ele_t        <- nums[2]
        comune_row$seggi        <- nums[3]
        comune_row$vot_t        <- nums[4]
        comune_row$perc_vot     <- nums[5]
        comune_row$tot_vot_cand <- nums[6]
        if (length(nums) >= 7) comune_row$tot_vot_lis  <- nums[7]
        if (length(nums) >= 8) comune_row$sk_nulle     <- nums[8]
        if (length(nums) >= 9) comune_row$sk_bianche   <- nums[9]
      }
    }
    # Popolazione: cerca specificamente il td adiacente a "Pop.Legale"
    pop_match <- str_match(html,
      'Pop\\.Legale[^<]*</td>\\s*<td[^>]*>\\s*([0-9.,]+)\\s*</td>')
    if (!is.na(pop_match[1, 2])) {
      comune_row$popolazione <- as.integer(gsub("\\.", "", pop_match[1, 2]))
    }
  }

  # ------- CANDIDATI + LISTE -------
  # I blocchi candidato seguono uno schema riconoscibile via regex:
  # Una tabella che contiene "Candidato Sindaco" o "Sindaco Eletto" con il
  # nome accanto, eventualmente con VOTI e %. La tabella successiva contiene
  # le liste collegate, una per riga (img + nome + n_candidati [+ voti + %]).

  # Estraiamo il blocco fra i marker HTML "<!-- INSERIMENTO PARTE PERSONALIZZATA -->"
  # e "<!-- FINE INSERIMENTO PARTE PERSONALIZZATA -->" (entrambi presenti una sola volta).
  body <- html
  body <- sub("^.*?<!-- INSERIMENTO PARTE PERSONALIZZATA -->", "", body)
  body <- sub("<!-- FINE INSERIMENTO PARTE PERSONALIZZATA -->.*$", "", body)

  # Trova le tabelle dei candidati: contengono "Candidato Sindaco" o "Sindaco Eletto"
  # Pattern per la prima riga del candidato (con o senza voti):
  cand_pat <- paste0(
    '<td[^>]*>\\s*N\\s*&deg;|N\\s*°\\s*</td>',     # placeholder
    '|',
    'class="normalBold"[^>]*>\\s*N\\s*°'
  )

  # Approccio più semplice: split del body sulle occorrenze di "Sindaco Eletto"
  # / "Candidato Sindaco " usate come label nella prima tabella di un candidato.
  # Catturiamo tre cose nel pattern: il numero progressivo, l'etichetta e il nome.
  # E opzionalmente voti+%.

  # Etichette riconosciute per la riga "candidato":
  #   "Sindaco <br> Eletto"        → vincitore al primo turno
  #   "Candidato Sindaco"          → candidato non eletto
  #   "Candidato Sindaco II Turno" → ammesso al ballottaggio
  cand_block_re <- regex(
    paste0(
      '<td[^>]*class="normal(?:Bold)?"[^>]*>\\s*(\\d+)\\s*</td>\\s*',           # numero
      '<td[^>]*class="normalBold"[^>]*>\\s*',
      '(Sindaco\\s*<br\\s*/?>\\s*Eletto|Candidato\\s*Sindaco(?:\\s*II\\s*Turno)?)',
      '\\s*</td>\\s*',
      '<td[^>]*>\\s*([^<]+?)\\s*</td>',                                          # nome
      '(?:\\s*<td[^>]*class="normalBold"[^>]*>\\s*VOTI\\s*</td>\\s*',
      '<td[^>]*>\\s*([0-9.,]+)\\s*</td>\\s*',                                    # voti
      '<td[^>]*class="normalBold"[^>]*>\\s*%\\s*</td>\\s*',
      '<td[^>]*>\\s*([0-9,.%]+)\\s*</td>)?'                                      # %
    ),
    ignore_case = TRUE, dotall = TRUE
  )

  cand_matches <- str_match_all(body, cand_block_re)[[1]]

  # Per ogni candidato troviamo le liste collegate fino al prossimo candidato.
  positions <- str_locate_all(body, cand_block_re)[[1]]

  if (!is.null(cand_matches) && nrow(cand_matches) > 0) {
    for (i in seq_len(nrow(cand_matches))) {
      pos_cand    <- as.integer(cand_matches[i, 2])
      label       <- gsub("\\s+", " ", cand_matches[i, 3])
      nome_cand   <- trimws(cand_matches[i, 4])
      voti_cand   <- if (!is.na(cand_matches[i, 5])) to_num(cand_matches[i, 5]) else NA_real_
      perc_cand   <- if (!is.na(cand_matches[i, 6])) to_num(cand_matches[i, 6]) else NA_real_

      eletto <- fcase(
        grepl("Sindaco.*Eletto", label, ignore.case = TRUE), "S",
        grepl("II\\s*Turno",     label, ignore.case = TRUE), "B",  # ammesso al ballottaggio
        default = NA_character_
      )

      # finestra fino al prossimo candidato (o fine body)
      start_pos <- positions[i, 2] + 1
      end_pos   <- if (i < nrow(positions)) positions[i + 1, 1] - 1 else nchar(body)
      window    <- substr(body, start_pos, end_pos)

      # Pattern lista: due varianti, parsate insieme.
      # 1) ReportDatiLista (pre-voto): tutti i td hanno class="normal"
      # 2) ReportRisultati (post-voto): i td hanno align="..." ma NON sempre
      #    class="normal" (es. cella img senza class), e dopo "candidati"
      #    seguono voti e %.
      # Non vincolo class, vincolo l'ordine dei td e l'alternanza img/testo.
      lista_re <- regex(
        paste0(
          '<td[^>]*>\\s*(\\d+)\\s*</td>\\s*',                                              # pos lista
          '<td[^>]*>\\s*(?:<img[^>]*src="[^"]*?contrassegni/+(\\d+)\\.[Jj][Pp][Gg]"[^>]*>)?\\s*</td>\\s*',  # img id (opzionale)
          '<td[^>]*>\\s*([A-Za-z][^<]*?)\\s*</td>\\s*',                                    # desc lista (deve iniziare con lettera)
          '<td[^>]*>\\s*(\\d+)\\s*</td>',                                                  # candidati
          '(?:\\s*<td[^>]*>\\s*([0-9.,]+)\\s*</td>\\s*',                                   # voti
          '<td[^>]*>\\s*([0-9.,%]+)\\s*</td>)?'                                            # %
        ),
        ignore_case = TRUE, dotall = TRUE
      )

      lst_matches <- str_match_all(window, lista_re)[[1]]
      n_liste <- if (is.null(lst_matches)) 0 else nrow(lst_matches)

      candidati_rows[[length(candidati_rows) + 1]] <- list(
        sigla_prov   = prov,
        desc_prov    = unname(PROV_NOMI[prov]),
        cod_comune   = town_code,
        desc_comune  = town_name,
        pos_cand     = pos_cand,
        cognome_nome = nome_cand,
        voti         = voti_cand,
        perc         = perc_cand,
        eletto       = eletto,
        n_liste      = n_liste
      )

      if (n_liste > 0) {
        for (j in seq_len(n_liste)) {
          liste_rows[[length(liste_rows) + 1]] <- list(
            sigla_prov   = prov,
            desc_prov    = unname(PROV_NOMI[prov]),
            cod_comune   = town_code,
            desc_comune  = town_name,
            pos_cand     = pos_cand,
            cognome_nome_cand = nome_cand,
            pos_lista    = as.integer(lst_matches[j, 2]),
            id_simbolo   = if (!is.na(lst_matches[j, 3])) lst_matches[j, 3] else NA_character_,
            descr_lista  = trimws(lst_matches[j, 4]),
            n_candidati  = as.integer(lst_matches[j, 5]),
            voti         = if (!is.na(lst_matches[j, 6])) to_num(lst_matches[j, 6]) else NA_real_,
            perc         = if (!is.na(lst_matches[j, 7])) to_num(lst_matches[j, 7]) else NA_real_
          )
        }
      }
    }
  }

  list(comune = list(comune_row), candidati = candidati_rows, liste = liste_rows)
}

# ==============================================================================
# 3. FETCH PER COMUNE: prova prima Risultati, poi DatiLista
# ==============================================================================

fetch_comune <- function(prov, town_code, town_name) {
  # Fallback chain in ordine di "freschezza":
  #   1) ReportRisultati      → scrutinio completato (totali + candidati + liste + voti)
  #   2) ReportCandidatiListe → scrutinio parziale (es. "201 su 253 sezioni")
  #   3) ReportDatiLista      → pre-voto, solo candidati + liste senza voti
  endpoints <- c(ReportRisultati      = "ReportRisultati",
                 ReportCandidatiListe = "ReportCandidatiListe",
                 ReportDatiLista      = "ReportDatiLista")

  # Numero righe parsiali pervenute (per scrutinio parziale)
  extract_sez_info <- function(html) {
    # Cerca "n. 201 su n. 253 sezioni"
    m <- str_match(html, "n\\.\\s*(\\d+)\\s*su\\s*n\\.\\s*(\\d+)\\s*sezioni")
    if (!is.na(m[1, 2])) {
      list(sz_perv = as.integer(m[1, 2]), sz_tot = as.integer(m[1, 3]))
    } else {
      list(sz_perv = NA_integer_, sz_tot = NA_integer_)
    }
  }

  for (fonte_nm in names(endpoints)) {
    url  <- sprintf("%s/%s/%s%s%s.html", BASE, prov, endpoints[[fonte_nm]],
                    prov, town_code)
    html <- fetch_html(url)
    if (is.null(html)) next
    res <- parse_report(html, prov, town_code, town_name)
    if (is.null(res)) next

    ha_dati <- res$comune[[1]]$stato == "ok" && length(res$candidati) > 0
    if (!ha_dati) next

    res$comune[[1]]$fonte <- fonte_nm
    sez <- extract_sez_info(html)
    # Per Risultati i totali (sz_tot, ele_t…) sono già stati estratti dalla
    # tabella. Per CandidatiListe (scrutinio parziale) li leggiamo dal titolo.
    if (is.na(res$comune[[1]]$sz_perv)) res$comune[[1]]$sz_perv <- sez$sz_perv
    if (is.na(res$comune[[1]]$sz_tot))  res$comune[[1]]$sz_tot  <- sez$sz_tot
    return(res)
  }

  # Nessun endpoint ha restituito dati validi
  list(
    comune = list(list(
      sigla_prov = prov, desc_prov = unname(PROV_NOMI[prov]),
      cod_comune = town_code, desc_comune = town_name,
      fonte = NA_character_, ele_t = NA, vot_t = NA, perc_vot = NA,
      sz_tot = NA, sz_perv = NA, seggi = NA, sk_bianche = NA, sk_nulle = NA,
      tot_vot_cand = NA, tot_vot_lis = NA, popolazione = NA,
      stato = "errore"
    )),
    candidati = list(), liste = list()
  )
}

# ==============================================================================
# 4. SCRAPE TUTTI I COMUNI (sequenziale: server statico, poche pagine)
# ==============================================================================

cat("Scarico report per", nrow(comuni_list), "comuni siciliani...\n")
t0 <- proc.time()

all_results <- vector("list", nrow(comuni_list))
for (i in seq_len(nrow(comuni_list))) {
  row <- comuni_list[i, ]
  all_results[[i]] <- fetch_comune(row$sigla_prov, row$cod_comune, row$desc_comune)
  cat(sprintf("  [%d/%d] %s (%s/%s) — %s, %d candidati\n",
              i, nrow(comuni_list), row$desc_comune, row$sigla_prov, row$cod_comune,
              all_results[[i]]$comune[[1]]$stato,
              length(all_results[[i]]$candidati)))
}

cat("Completato in", round((proc.time() - t0)["elapsed"]), "secondi\n")

# ==============================================================================
# 5. ASSEMBLA E SALVA CSV
# ==============================================================================

bind_rows_list <- function(rows) {
  if (length(rows) == 0) return(data.frame())
  rbindlist(lapply(rows, as.data.frame, stringsAsFactors = FALSE), fill = TRUE)
}

comuni_df    <- bind_rows_list(unlist(lapply(all_results, `[[`, "comune"),    recursive = FALSE))
candidati_df <- bind_rows_list(unlist(lapply(all_results, `[[`, "candidati"), recursive = FALSE))
liste_df     <- bind_rows_list(unlist(lapply(all_results, `[[`, "liste"),     recursive = FALSE))

# Aggiunge codice ISTAT via join sul nome (case-insensitive) con
# comuni_al_voto_2026.csv che ha già il codice ISTAT.
mapping_path <- file.path(PROJ_DIR, "input", "comuni_al_voto_2026.csv")
if (file.exists(mapping_path)) {
  map <- fread(mapping_path)
  # Normalizza: rimuove diacritici, apostrofi e spazi -> chiave robusta
  norm_name <- function(x) {
    x <- toupper(x)
    x <- iconv(x, to = "ASCII//TRANSLIT")
    x <- gsub("[^A-Z0-9]", "", x)
    x
  }
  map_sicilia <- map[regione == "SICILIA",
                     .(key = norm_name(nome_istat),
                       codice_istat = codice_istat,
                       provincia_nome = provincia)]
  comuni_df[, key := norm_name(desc_comune)]
  comuni_df <- merge(comuni_df, map_sicilia, by = "key",
                     all.x = TRUE, sort = FALSE)
  comuni_df[, key := NULL]

  # Propaga codice ISTAT anche a candidati e liste via (sigla_prov, cod_comune)
  istat_lookup <- comuni_df[, .(sigla_prov, cod_comune, codice_istat)]
  if (nrow(candidati_df) > 0) {
    candidati_df <- merge(candidati_df, istat_lookup,
                          by = c("sigla_prov", "cod_comune"),
                          all.x = TRUE, sort = FALSE)
  }
  if (nrow(liste_df) > 0) {
    liste_df <- merge(liste_df, istat_lookup,
                      by = c("sigla_prov", "cod_comune"),
                      all.x = TRUE, sort = FALSE)
  }
  cat("Comuni con cod ISTAT mappato:", sum(!is.na(comuni_df$codice_istat)),
      "/", nrow(comuni_df), "\n")
}

cat("\nStato fetch:\n"); print(table(comuni_df$stato))
cat("Candidati totali:", nrow(candidati_df), "\n")
cat("Liste di sostegno totali:", nrow(liste_df), "\n")

fwrite(comuni_df,    file.path(OUTPUT_DIR, "comuni.csv"))
fwrite(candidati_df, file.path(OUTPUT_DIR, "candidati.csv"))
fwrite(liste_df,     file.path(OUTPUT_DIR, "liste.csv"))
cat("\nCSV salvati in", OUTPUT_DIR, "\n")
