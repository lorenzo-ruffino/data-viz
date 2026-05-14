# Estrazione affluenza Referendum Giustizia 2026
# Fonte: https://eleapi.interno.gov.it
# Output: elettori e votanti per comune per ogni comunicazione,
#         aggregati per provincia e regione

library(httr)
library(jsonlite)
library(stringr)
library(dplyr)
library(data.table)

DATE          <- "20260322"
TIPO_ELEZIONE <- "09"

headers <- c(
  'accept'            = 'application/json, text/plain, */*',
  'accept-language'   = 'en-US,en;q=0.6',
  'dnt'               = '1',
  'origin'            = 'https://elezioni.interno.gov.it',
  'priority'          = 'u=1, i',
  'referer'           = 'https://elezioni.interno.gov.it/',
  'sec-ch-ua-mobile'  = '?0',
  'sec-ch-ua-platform'= '"macOS"',
  'sec-fetch-dest'    = 'empty',
  'sec-fetch-mode'    = 'cors',
  'sec-fetch-site'    = 'same-site',
  'sec-gpc'           = '1'
)

# ==============================================================================
# 1. LISTA PROVINCE CON REGIONE
# ==============================================================================

url_enti  <- paste0('https://eleapi.interno.gov.it/siel/PX/getentiFI/DE/', DATE, '/TE/', TIPO_ELEZIONE)
data_enti <- fromJSON(rawToChar(GET(url_enti, add_headers(.headers = headers))$content))$enti

df_re <- data_enti %>% filter(tipo == "RE")
regione_mapping <- setNames(df_re$desc, substr(df_re$cod, 1, 2))

df_province <- data_enti %>%
  filter(tipo == "PR") %>%
  mutate(
    cod_regione   = substr(cod, 1, 2),
    desc_regione  = regione_mapping[cod_regione],
    cod_provincia = substr(cod, 3, 5)
  )

cat("Province trovate:", nrow(df_province), "\n")

# ==============================================================================
# 2. AFFLUENZA PER COMUNE (tutte le comunicazioni)
# ==============================================================================
# com_vot e' una lista di dataframe: una riga per comunicazione
# colonne: com, dt_com, scheda, enti_p, enti_t, perc, vot_m, vot_f, vot_t, perc_r

BASE_URL <- paste0('https://eleapi.interno.gov.it/siel/PX/votantiFI/DE/',
                   DATE, '/TE/', TIPO_ELEZIONE, '/SK/01/PR/')

all_comuni_long <- list()
all_comuni_wide <- list()

for (idx in 1:nrow(df_province)) {
  cod_prov <- df_province$cod_provincia[idx]

  tryCatch({
    r    <- GET(paste0(BASE_URL, cod_prov), add_headers(.headers = headers))
    data <- fromJSON(rawToChar(r$content))

    if (status_code(r) == 200 && !is.null(data$enti$enti_f) && nrow(data$enti$enti_f) > 0) {
      comuni <- data$enti$enti_f

      for (i in 1:nrow(comuni)) {
        c_row <- comuni[i, ]
        cv    <- c_row$com_vot[[1]]  # dataframe: 4 righe x {com, dt_com, perc, vot_m, vot_f, vot_t, ...}

        meta <- data.frame(
          cod_regione    = df_province$cod_regione[idx],
          desc_regione   = df_province$desc_regione[idx],
          cod_provincia  = df_province$cod_provincia[idx],
          desc_provincia = df_province$desc[idx],
          cod_comune     = c_row$cod,
          desc_comune    = c_row$desc,
          tipo_comune    = c_row$tipo_tras,
          ele_t          = c_row$ele_t,
          ele_m          = c_row$ele_m,
          ele_f          = c_row$ele_f,
          stringsAsFactors = FALSE
        )

        # Long: una riga per comunicazione
        rows_long <- cbind(
          meta[rep(1, nrow(cv)), ],
          data.frame(
            comunicazione = cv$com,
            dt_com        = cv$dt_com,
            vot_t         = cv$vot_t,
            vot_m         = cv$vot_m,
            vot_f         = cv$vot_f,
            perc          = cv$perc,
            stringsAsFactors = FALSE
          )
        )
        all_comuni_long[[length(all_comuni_long) + 1]] <- rows_long

        # Wide: una riga per comune
        row_wide <- meta
        for (j in 1:nrow(cv)) {
          row_wide[[paste0("dt_com",    j)]] <- cv$dt_com[j]
          row_wide[[paste0("vot_t_com", j)]] <- cv$vot_t[j]
          row_wide[[paste0("vot_m_com", j)]] <- cv$vot_m[j]
          row_wide[[paste0("vot_f_com", j)]] <- cv$vot_f[j]
          row_wide[[paste0("perc_com",  j)]] <- cv$perc[j]
        }
        all_comuni_wide[[length(all_comuni_wide) + 1]] <- row_wide
      }

      cat(idx, "-", df_province$desc[idx], "(", nrow(comuni), "comuni)\n")
    } else {
      cat(idx, "-", df_province$desc[idx], "- Nessun dato\n")
    }
  }, error = function(e) {
    cat(idx, "-", df_province$desc[idx], "- ERRORE:", e$message, "\n")
  })

  Sys.sleep(0.3)
}

affluenza_comuni_long <- do.call(rbind, all_comuni_long)
affluenza_comuni_wide <- do.call(rbind, all_comuni_wide)
rownames(affluenza_comuni_long) <- NULL
rownames(affluenza_comuni_wide) <- NULL
cat("Comuni (long):", nrow(affluenza_comuni_long), "righe | (wide):", nrow(affluenza_comuni_wide), "righe\n")

# ==============================================================================
# 3. JOIN CODICI ISTAT
# ==============================================================================

codici <- fread("/Users/lorenzoruffino/Documents/Progetti/data-viz/Referendum_Giustizia_2026/codici_comuni_15-03-2026.csv") %>%
  rename(cod_elettorale = `CODICE ELETTORALE`,
         cod_istat      = `CODICE ISTAT`,
         cod_catastale  = `CODICE BELFIORE`) %>%
  mutate(
    cod_istat = gsub('="([^"]*)"', '\\1', cod_istat),
    cod_join  = str_sub(as.character(cod_elettorale), -7, -1)
  ) %>%
  select(cod_join, cod_istat, cod_catastale)

join_istat <- function(df) {
  df %>%
    mutate(cod_join = paste0(cod_provincia, str_pad(cod_comune, 4, "left", "0"))) %>%
    left_join(codici, by = "cod_join") %>%
    rename(codice_istat = cod_istat) %>%
    mutate(codice_istat = as.numeric(codice_istat)) %>%
    select(-cod_join)
}

affluenza_comuni_long <- join_istat(affluenza_comuni_long)
affluenza_comuni_wide <- join_istat(affluenza_comuni_wide)

# ==============================================================================
# 4. AGGREGATI PER PROVINCIA E REGIONE (dal long)
# ==============================================================================

agg_cols <- function(df, ...) {
  df %>%
    group_by(..., comunicazione, dt_com) %>%
    summarise(
      ele_t = sum(ele_t, na.rm = TRUE),
      ele_m = sum(ele_m, na.rm = TRUE),
      ele_f = sum(ele_f, na.rm = TRUE),
      vot_t = sum(vot_t, na.rm = TRUE),
      vot_m = sum(vot_m, na.rm = TRUE),
      vot_f = sum(vot_f, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(perc = vot_t / ele_t)
}

affluenza_province  <- agg_cols(affluenza_comuni_long, cod_regione, desc_regione, cod_provincia, desc_provincia)
affluenza_regioni   <- agg_cols(affluenza_comuni_long, cod_regione, desc_regione)
affluenza_nazionale <- agg_cols(affluenza_comuni_long)

# ==============================================================================
# 5. SALVATAGGIO CSV
# ==============================================================================

script_dir <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Referendum_Giustizia_2026/affluenza"
fwrite(affluenza_comuni_long, file.path(script_dir, "affluenza_comuni_long.csv"))
fwrite(affluenza_comuni_wide, file.path(script_dir, "affluenza_comuni_wide.csv"))
cat("CSV salvati in affluenza/\n")
