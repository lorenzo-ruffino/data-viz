# Proiezione in tempo reale — Sindaco Venezia 2026
#
# Modello di swing per coalizione:
#   - Brugnaro 2020              -> Venturini 2026
#   - Visman + Baretta 2020      -> Martella 2026
#   - resto (6 candidati 2020)   -> resto (6 candidati 2026)
#
# Logica:
# 1. Calcolo swing factor per ogni coalizione c:
#       swing_c = share_2026_c_su_scrutinate / share_2020_c_su_stesse_sezioni
# 2. Calcolo swing turnout (voti totali):
#       turnout_swing = voti_tot_2026_scrutinate / voti_tot_2020_scrutinate
# 3. Per ogni sezione NON scrutinata m, proietto i voti totali come
#       voti_2026_m = voti_2020_m * turnout_swing
#    e i voti della coalizione c come
#       voti_c_2026_m = (voti_c_2020_m / voti_tot_2020_m) * swing_c * voti_2026_m
#                     = voti_c_2020_m * swing_c * turnout_swing
# 4. Bootstrap a livello di sezione per intervallo di confidenza 95%.
#
# Input:
#   input/venezia_2020_sindaco_per_sezione.csv
#   output/risultati_2026_per_sezione.csv  (prodotto da 01_scrape_sezioni.R)
# Output:
#   output/proiezione_2026.csv

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
})

PROJ_DIR <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Venezia 2026 - Proiezione sezioni"
PATH_2020 <- file.path(PROJ_DIR, "input",  "venezia_2020_sindaco_per_sezione.csv")
PATH_2026 <- file.path(PROJ_DIR, "output", "risultati_2026_per_sezione.csv")
PATH_OUT  <- file.path(PROJ_DIR, "output", "proiezione_2026.csv")

N_BOOT <- 1000L

# ----------------------------------------------------------------------------
# Carica dati
# ----------------------------------------------------------------------------
d20 <- read_csv(PATH_2020, show_col_types = FALSE)
d26 <- read_csv(PATH_2026, show_col_types = FALSE)

# Mappatura coalizioni 2020 -> 2026
# coalition: "venturini" / "martella" / "altri"
coal_2020 <- list(
  venturini = "brugnaro",
  martella  = c("visman", "baretta"),
  altri     = c("zecchi", "busetto", "martini", "sitran", "gasparinetti", "callegari")
)
coal_2026 <- list(
  venturini = "venturini",
  martella  = "martella",
  altri     = c("del_zotto", "coro", "boldrin", "agirmo", "vernier", "martini_2026")
)

# Aggrega 2020 e 2026 per coalizione, riga = sezione
agg_coalition <- function(df, cols_map) {
  out <- tibble(sezione = df$sezione)
  for (c in names(cols_map)) {
    cols <- cols_map[[c]]
    cols <- cols[cols %in% names(df)]
    out[[c]] <- if (length(cols) == 1) df[[cols]] else rowSums(df[, cols, drop = FALSE])
  }
  out$totale <- rowSums(out[, names(cols_map), drop = FALSE])
  out
}

c20 <- agg_coalition(d20, coal_2020)
c26 <- agg_coalition(d26, coal_2026)

# Le sezioni 2020 erano 257, 2026 sono 256: tengo l'intersezione
sezioni_comuni <- intersect(c20$sezione, c26$sezione)
c20 <- c20 %>% filter(sezione %in% sezioni_comuni) %>% arrange(sezione)
c26 <- c26 %>% filter(sezione %in% sezioni_comuni) %>% arrange(sezione)

stopifnot(all(c20$sezione == c26$sezione))

# ----------------------------------------------------------------------------
# Identifica scrutinate (totale 2026 > 0)
# ----------------------------------------------------------------------------
scrut_mask <- c26$totale > 0
n_scrut <- sum(scrut_mask)
n_tot   <- nrow(c26)

cat(sprintf("Sezioni scrutinate: %d / %d (%.1f%%)\n",
            n_scrut, n_tot, 100 * n_scrut / n_tot))

if (n_scrut == 0) {
  cat("Nessuna sezione scrutinata: proiezione non disponibile.\n")
  cat("Mostro solo i totali baseline 2020 a confronto:\n\n")

  baseline <- tibble(
    coalizione = names(coal_2020),
    voti_2020  = sapply(names(coal_2020), function(c) sum(c20[[c]]))
  ) %>%
    mutate(pct_2020 = 100 * voti_2020 / sum(voti_2020))
  print(baseline)
  quit(save = "no")
}

# ----------------------------------------------------------------------------
# Stima centrale
# ----------------------------------------------------------------------------
calcola_proiezione <- function(c20_sub, c26_sub, mask) {
  scrut <- which(mask)
  miss  <- which(!mask)

  # turnout swing (rapporto voti tot 2026/2020 sulle scrutinate)
  voti_20_scr <- sum(c20_sub$totale[scrut])
  voti_26_scr <- sum(c26_sub$totale[scrut])
  turnout_swing <- if (voti_20_scr > 0) voti_26_scr / voti_20_scr else 1

  # swing per coalizione = share_2026 / share_2020 sulle scrutinate
  coal_names <- c("venturini", "martella", "altri")
  swing <- sapply(coal_names, function(c) {
    s20 <- sum(c20_sub[[c]][scrut]) / max(voti_20_scr, 1)
    s26 <- sum(c26_sub[[c]][scrut]) / max(voti_26_scr, 1)
    if (s20 > 0) s26 / s20 else 1
  })

  # proiezione voti coalizione c = già_arrivati + (2020_mancanti * swing_c * turnout_swing)
  out <- sapply(coal_names, function(c) {
    gia <- sum(c26_sub[[c]][scrut])
    proj <- sum(c20_sub[[c]][miss]) * swing[[c]] * turnout_swing
    gia + proj
  })
  out
}

proj_centrale <- calcola_proiezione(c20, c26, scrut_mask)

# ----------------------------------------------------------------------------
# Bootstrap su sezioni scrutinate per CI 95%
# ----------------------------------------------------------------------------
set.seed(42)
boot_mat <- matrix(NA_real_, nrow = N_BOOT, ncol = 3,
                   dimnames = list(NULL, c("venturini", "martella", "altri")))

scrut_idx <- which(scrut_mask)
miss_idx  <- which(!scrut_mask)

for (b in seq_len(N_BOOT)) {
  if (length(scrut_idx) < 2) {
    boot_mat[b, ] <- proj_centrale
    next
  }
  resamp <- sample(scrut_idx, length(scrut_idx), replace = TRUE)
  # mask "finta": scrutinate = resamp, mancanti = tutte le altre (inclusi missing veri)
  mask_b <- logical(nrow(c20))
  # uso resamp come "campione" delle scrutinate; per le mancanti uso tutte le sezioni non resamplate
  # in pratica: ricalcolo swing usando solo le sezioni resamp, e applico alle sezioni mancanti reali
  c20_b <- c20[resamp, ]
  c26_b <- c26[resamp, ]
  voti_20_scr <- sum(c20_b$totale)
  voti_26_scr <- sum(c26_b$totale)
  turnout_swing <- if (voti_20_scr > 0) voti_26_scr / voti_20_scr else 1
  for (j in seq_along(c("venturini", "martella", "altri"))) {
    c <- c("venturini", "martella", "altri")[j]
    s20 <- sum(c20_b[[c]]) / max(voti_20_scr, 1)
    s26 <- sum(c26_b[[c]]) / max(voti_26_scr, 1)
    sw  <- if (s20 > 0) s26 / s20 else 1
    gia <- sum(c26[[c]][scrut_idx])
    proj <- sum(c20[[c]][miss_idx]) * sw * turnout_swing
    boot_mat[b, j] <- gia + proj
  }
}

ci_lo <- apply(boot_mat, 2, quantile, 0.025, na.rm = TRUE)
ci_hi <- apply(boot_mat, 2, quantile, 0.975, na.rm = TRUE)

# Voti totali proiettati per normalizzare in percentuale
tot_proj    <- sum(proj_centrale)
tot_proj_b  <- rowSums(boot_mat)

pct_centrale <- 100 * proj_centrale / tot_proj
pct_boot     <- 100 * boot_mat / tot_proj_b
pct_lo <- apply(pct_boot, 2, quantile, 0.025, na.rm = TRUE)
pct_hi <- apply(pct_boot, 2, quantile, 0.975, na.rm = TRUE)

# ----------------------------------------------------------------------------
# Tabella di output
# ----------------------------------------------------------------------------
ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

# Quanti voti sono già arrivati per coalizione (sul partial)
gia_arrivati <- sapply(c("venturini", "martella", "altri"), function(c) sum(c26[[c]][scrut_idx]))

out <- tibble(
  timestamp    = ts,
  coalizione   = c("venturini", "martella", "altri"),
  voti_arrivati = as.integer(gia_arrivati),
  voti_proiettati = as.integer(round(proj_centrale)),
  voti_ic95_lo = as.integer(round(ci_lo)),
  voti_ic95_hi = as.integer(round(ci_hi)),
  pct_proiettata = round(pct_centrale, 2),
  pct_ic95_lo  = round(pct_lo, 2),
  pct_ic95_hi  = round(pct_hi, 2)
) %>% arrange(desc(voti_proiettati))

# Append al log storico (utile per vedere come la proiezione si stabilizza)
append_mode <- file.exists(PATH_OUT)
write_csv(out, PATH_OUT, append = append_mode)

cat(sprintf("\nProiezione [%s]  sezioni: %d/%d  voti arrivati: %d  voti proiettati: %d\n",
            ts, n_scrut, n_tot, sum(gia_arrivati), as.integer(round(tot_proj))))
print(out)

# avviso ballottaggio
v_pct <- pct_centrale[["venturini"]]
m_pct <- pct_centrale[["martella"]]
if (max(v_pct, m_pct) < 50) {
  cat(sprintf("\n>> Ballottaggio probabile: Venturini %.1f%% vs Martella %.1f%%\n", v_pct, m_pct))
} else {
  vinc <- if (v_pct > m_pct) "Venturini" else "Martella"
  cat(sprintf("\n>> Vittoria al primo turno proiettata: %s\n", vinc))
}
