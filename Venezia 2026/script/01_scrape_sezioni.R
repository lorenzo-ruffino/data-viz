# Scraping risultati sindaco per sezione — Venezia, comunali 24-25 maggio 2026
# Fonte: https://elezioni.comune.venezia.it
#
# Per ciascuno degli 8 candidati scarica la pagina /preferenze/.../<id> e
# parsa la tabella delle 256 sezioni (Sezione N -> Voti Validi).
# Output: output/risultati_2026_per_sezione.csv (formato wide, una riga per sezione).
#
# Lo script è idempotente: rilancialo durante lo spoglio per aggiornare il CSV.
# Per uso "live" usalo dentro un ciclo: while true; do Rscript 01_scrape_sezioni.R; sleep 60; done

suppressPackageStartupMessages({
  library(httr)
  library(stringr)
  library(dplyr)
  library(tidyr)
  library(purrr)
})

PROJ_DIR   <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Venezia 2026 - Proiezione sezioni"
OUTPUT_CSV <- file.path(PROJ_DIR, "output", "risultati_2026_per_sezione.csv")
LOG_FILE   <- file.path(PROJ_DIR, "output", "scrape_log.txt")
dir.create(dirname(OUTPUT_CSV), showWarnings = FALSE, recursive = TRUE)

BASE_URL <- "https://elezioni.comune.venezia.it"
# URL della pagina riepilogo, da cui estraggo il token cache "d" corrente
INDEX_URL <- paste0(BASE_URL, "/risultati/amministrative-sindaco-2026/567/A/30241")

# 8 candidati sindaco 2026 (id 466-473), slug come compaiono nell'URL
candidati <- tibble::tribble(
  ~id,   ~slug,                     ~cognome,
  466L,  "pierangelo-del-zotto",    "del_zotto",
  467L,  "luigi-coro%27",           "coro",
  468L,  "simone-venturini",        "venturini",
  469L,  "michele-boldrin",         "boldrin",
  470L,  "roberto-agirmo",          "agirmo",
  471L,  "claudio-vernier",         "vernier",
  472L,  "andrea-martella",         "martella",
  473L,  "giovanni-martini",        "martini_2026"
)

headers <- c(
  "accept"          = "text/html,application/xhtml+xml",
  "accept-language" = "it-IT,it;q=0.9,en;q=0.8",
  "user-agent"      = "Mozilla/5.0 (data-viz scraper, contact: lorenzo)"
)

# Genera un cache buster random (32 hex) — il server di Venezia ha cache
# basata sul valore di "d": usando un valore nuovo ad ogni run forziamo
# il miss e otteniamo dati freschi.
random_d <- function() {
  paste(sample(c(0:9, letters[1:6]), 32, replace = TRUE), collapse = "")
}

# ----------------------------------------------------------------------------
# Scarica e parsa una pagina candidato -> tibble (sezione, voti)
# Ogni candidato ottiene un d random diverso per evitare collisioni.
# ----------------------------------------------------------------------------
scrape_candidato <- function(id, slug, cognome, max_retry = 4L) {
  for (attempt in seq_len(max_retry)) {
    url <- sprintf("%s/preferenze/%s/30241/567/M/%d?d=%s",
                   BASE_URL, slug, id, random_d())
    resp <- tryCatch(
      GET(url, add_headers(.headers = headers), timeout(30)),
      error = function(e) NULL
    )
    if (!is.null(resp) && status_code(resp) == 200) break
    if (attempt < max_retry) {
      Sys.sleep(2 * attempt)  # backoff: 2s, 4s, 6s
      message(sprintf("  [%s] retry %d/%d", cognome, attempt, max_retry))
    }
  }
  if (is.null(resp) || status_code(resp) != 200) {
    message(sprintf("  [%s] HTTP error definitivo", cognome))
    return(tibble(sezione = integer(), voti = integer(), cognome = character()))
  }
  html <- rawToChar(resp$content)
  # ogni riga è: ...Sezione <b>N</b>...<div class="votivalidi">V</div>
  m <- str_match_all(
    html,
    regex("Sezione\\s*<b>(\\d+)</b>.*?<div class=\"votivalidi\">(\\d+)</div>",
          dotall = TRUE)
  )[[1]]
  if (nrow(m) == 0) {
    message(sprintf("  [%s] nessuna riga di sezione trovata", cognome))
    return(tibble(sezione = integer(), voti = integer(), cognome = character()))
  }
  tibble(
    sezione = as.integer(m[, 2]),
    voti    = as.integer(m[, 3]),
    cognome = cognome
  )
}

# ----------------------------------------------------------------------------
# Scarica tutti gli 8 candidati e combina in wide
# ----------------------------------------------------------------------------
ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
cat(sprintf("[%s] Scraping risultati per sezione, 8 candidati (cache buster random)...\n",
            ts))

long <- pmap_dfr(candidati, scrape_candidato)

if (nrow(long) == 0) {
  cat("Nessun dato scaricato — esco.\n")
  quit(status = 1)
}

wide <- long %>%
  pivot_wider(names_from = cognome, values_from = voti, values_fill = 0) %>%
  arrange(sezione)

# colonne nell'ordine dei candidati
ordered_cols <- candidati$cognome
ordered_cols <- ordered_cols[ordered_cols %in% names(wide)]
wide <- wide %>%
  select(sezione, all_of(ordered_cols)) %>%
  mutate(totale = rowSums(across(all_of(ordered_cols))))

# Flag "scrutinata": almeno un voto in qualunque candidato
# (le sezioni a 0 secchi sono "non ancora arrivate")
wide <- wide %>%
  mutate(scrutinata = as.integer(totale > 0))

write.csv(wide, OUTPUT_CSV, row.names = FALSE, fileEncoding = "UTF-8")

n_scrut <- sum(wide$scrutinata)
tot     <- sum(wide$totale)
log_line <- sprintf("[%s] sezioni scrutinate: %d/%d  voti totali: %d\n",
                    ts, n_scrut, nrow(wide), tot)
cat(log_line)
cat(log_line, file = LOG_FILE, append = TRUE)

# riepilogo per candidato sui dati attuali
cat("\nTotali correnti per candidato:\n")
tot_cand <- wide %>%
  summarise(across(all_of(ordered_cols), sum)) %>%
  pivot_longer(everything(), names_to = "candidato", values_to = "voti")
tot_voti <- sum(tot_cand$voti)
tot_cand$pct <- if (tot_voti > 0) round(100 * tot_cand$voti / tot_voti, 2) else 0
tot_cand <- tot_cand %>% arrange(desc(voti))
print(tot_cand, n = Inf)
