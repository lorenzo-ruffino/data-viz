# =============================================================================
# Analisi salari nel settore privato italiano (Osservatorio Inps, 2014-2024)
#
# Obiettivo: capire perché i salari italiani non sono cresciuti negli ultimi
# 10 anni separando l'andamento "intra-classe" (cosa è successo nelle stesse
# categorie di lavoratori) dall'effetto composizione (chi lavora è cambiato).
#
# Dati di input (cartella del progetto):
#   - lavoratori_eta_sesso_2014_2024.csv
#   - lavoratori_attivita_economica_ateco_2014_2024.csv
#   - lavoratori_tempo_parziale_sesso_2014_2024.csv
#   - lavoratori_eta_tipologia_contrattuale_2014_2024.csv
#   - indice_prezzi_istat.csv  (NIC, base 1995=100)
#
# Misure salariali (calcolate tutte e tre, sia nominali sia reali a prezzi 2024):
#   1. Stipendio annuo per lavoratore  = somma_retribuzioni / lavoratori
#      (include part-year e part-time: è la "busta paga" media headline)
#   2. Stipendio settimanale            = somma_retribuzioni / settimane_retribuite
#      (toglie la durata del rapporto, ma il part-time conta come full-time)
#   3. Stipendio settimanale FTE        = somma_retribuzioni / settimane_utili
#      (settimane utili scontano l'intensita' part-time -> misura piu' pulita
#       del livello salariale)
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(purrr)
})

PROJ <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Stipendi privato Inps"
OUT  <- file.path(PROJ, "output")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

ANNO_BASE <- 2014L
ANNO_T    <- 2024L

# -----------------------------------------------------------------------------
# 1. Indice prezzi e deflattore a prezzi 2024
# -----------------------------------------------------------------------------
cpi <- read_csv(file.path(PROJ, "indice_prezzi_istat.csv"),
                show_col_types = FALSE) %>%
  select(anno, indice) %>%
  filter(anno >= 2014, anno <= 2024)

indice_2024 <- cpi$indice[cpi$anno == 2024]
cpi <- cpi %>% mutate(deflattore_2024 = indice_2024 / indice)

cat(sprintf("Inflazione cumulata 2014-2024: %+.1f%%\n",
            (indice_2024 / cpi$indice[cpi$anno == 2014] - 1) * 100))

# -----------------------------------------------------------------------------
# 2. Lettura dataset puliti
# -----------------------------------------------------------------------------
d_eta_sesso  <- read_csv(file.path(PROJ, "lavoratori_eta_sesso_2014_2024.csv"),
                         show_col_types = FALSE)
d_ateco      <- read_csv(file.path(PROJ, "lavoratori_attivita_economica_ateco_2014_2024.csv"),
                         show_col_types = FALSE)
d_tempo_parz <- read_csv(file.path(PROJ, "lavoratori_tempo_parziale_sesso_2014_2024.csv"),
                         show_col_types = FALSE)
d_eta_tipol  <- read_csv(file.path(PROJ, "lavoratori_eta_tipologia_contrattuale_2014_2024.csv"),
                         show_col_types = FALSE)

# Alias colonne misure (lunghissime nei file)
COL_LAV  <- "numero_lavoratori_con_almeno_una_giornata_retribuita_nell_anno"
COL_SR   <- "numero_settimane_retribuite_nell_anno"
COL_SU   <- "numero_settimane_utili_nell_anno"
COL_GG   <- "numero_giornate_retribuite_nell_anno"
COL_SOMM <- "somma_retribuzioni_nell_anno"
COL_PER  <- "periodo_retribuito_dal_datore_di_lavoro"

# -----------------------------------------------------------------------------
# 3. Mapping eta' decennali e settori macro
# -----------------------------------------------------------------------------
mappa_eta_decennale <- function(x) {
  case_when(
    x == "Fino a 19" ~ "<20",
    str_detect(x, "^20\\b|^25\\b") ~ "20-29",
    str_detect(x, "^30\\b|^35\\b") ~ "30-39",
    str_detect(x, "^40\\b|^45\\b") ~ "40-49",
    str_detect(x, "^50\\b|^55\\b") ~ "50-59",
    str_detect(x, "^60\\b|^65") ~ "60+",
    TRUE ~ NA_character_
  )
}

ordine_eta <- c("<20", "20-29", "30-39", "40-49", "50-59", "60+")

mappa_macro_settore <- function(x) {
  case_when(
    str_detect(x, "Estrazione|Fornitura di energia|Fornitura di acqua") ~
      "Industria estrattiva, energia, utilities",
    str_detect(x, "manifatturiere") ~ "Manifattura",
    str_detect(x, "Costruzioni") ~ "Costruzioni",
    str_detect(x, "Commercio") ~ "Commercio",
    str_detect(x, "Trasporto") ~ "Trasporti e magazzinaggio",
    str_detect(x, "alloggio e di ristorazione") ~ "Turismo e ristorazione",
    str_detect(x, "informazione e comunicazione") ~ "ICT",
    str_detect(x, "finanziarie") ~ "Finanza e assicurazioni",
    str_detect(x, "immobiliari") ~ "Immobiliari",
    str_detect(x, "professionali") ~ "Servizi professionali",
    str_detect(x, "Noleggio") ~ "Servizi alle imprese",
    str_detect(x, "Istruzione") ~ "Istruzione (privata)",
    str_detect(x, "Sanita") ~ "Sanita' e assistenza sociale (privata)",
    str_detect(x, "artistiche") ~ "Cultura e intrattenimento",
    str_detect(x, "Altre attivit") ~ "Altri servizi",
    str_detect(x, "famiglie e convivenze") ~ "Lavoro domestico",
    TRUE ~ NA_character_
  )
}

ordine_periodo <- c("Fino a 3 mesi", "Oltre 3 e fino a 6 mesi",
                    "Oltre 6 e meno di 12 mesi", "Anno intero")

# -----------------------------------------------------------------------------
# 4. Helper: aggrega un dataset su un set di colonne raggruppanti e calcola
#    le 3 misure salariali (nominali + reali a prezzi 2024).
# -----------------------------------------------------------------------------
calcola_salari <- function(df, gruppo) {
  df %>%
    group_by(across(all_of(gruppo))) %>%
    summarise(
      lavoratori           = sum(.data[[COL_LAV]], na.rm = TRUE),
      settimane_retribuite = sum(.data[[COL_SR]],  na.rm = TRUE),
      settimane_utili      = sum(.data[[COL_SU]],  na.rm = TRUE),
      giornate_retribuite  = sum(.data[[COL_GG]],  na.rm = TRUE),
      somma_retribuzioni   = sum(.data[[COL_SOMM]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      stipendio_annuo            = somma_retribuzioni / lavoratori,
      stipendio_settimanale      = somma_retribuzioni / settimane_retribuite,
      stipendio_settimanale_fte  = somma_retribuzioni / settimane_utili,
      # Annuo FTE = quello che guadagnerebbe in un anno chi lavora a tempo
      # pieno tutte le 52 settimane (misura pulita di livello salariale,
      # espressa nella scala "annua" che il lettore conosce).
      stipendio_annuo_fte        = stipendio_settimanale_fte * 52
    ) %>%
    left_join(cpi %>% select(anno, deflattore_2024), by = "anno") %>%
    mutate(
      somma_retribuzioni_reale         = somma_retribuzioni        * deflattore_2024,
      stipendio_annuo_reale            = stipendio_annuo           * deflattore_2024,
      stipendio_settimanale_reale      = stipendio_settimanale     * deflattore_2024,
      stipendio_settimanale_fte_reale  = stipendio_settimanale_fte * deflattore_2024,
      stipendio_annuo_fte_reale        = stipendio_annuo_fte       * deflattore_2024
    ) %>%
    select(-deflattore_2024)
}

# Applica il mapping eta' a un dataset che contiene `classe_di_eta`.
con_eta_decennale <- function(df) {
  df %>%
    mutate(eta_decennale = factor(mappa_eta_decennale(classe_di_eta),
                                  levels = ordine_eta))
}

# Filtro "anno intero".
solo_anno_intero <- function(df) {
  df %>% filter(.data[[COL_PER]] == "Anno intero")
}

# -----------------------------------------------------------------------------
# 5. Helper: shift-share su 2014 -> 2024
# -----------------------------------------------------------------------------
# Ritorna un data frame con: livello base, livello finale, delta totale,
# effetto salario puro, effetto composizione, interazione, contrafattuale.
#
# wage_col  = colonna con il salario per classe (es. stipendio_settimanale_fte)
# weight_col = colonna usata come peso per la share (es. lavoratori, settimane_utili)
# group_cols = la dim su cui calcolare composizione (es. "eta_decennale")
shift_share <- function(df, group_cols, wage_col, weight_col,
                        anno_base = ANNO_BASE, anno_t = ANNO_T,
                        label = "") {
  base <- df %>% filter(anno == anno_base) %>%
    select(all_of(c(group_cols, wage_col, weight_col))) %>%
    rename(wage_base = !!wage_col, weight_base = !!weight_col)
  fin  <- df %>% filter(anno == anno_t) %>%
    select(all_of(c(group_cols, wage_col, weight_col))) %>%
    rename(wage_t = !!wage_col, weight_t = !!weight_col)

  joined <- full_join(base, fin, by = group_cols) %>%
    mutate(
      wage_base   = coalesce(wage_base, 0),
      wage_t      = coalesce(wage_t, 0),
      weight_base = coalesce(weight_base, 0),
      weight_t    = coalesce(weight_t, 0)
    ) %>%
    mutate(
      share_base = weight_base / sum(weight_base),
      share_t    = weight_t    / sum(weight_t)
    )

  w_base <- sum(joined$wage_base * joined$share_base)
  w_t    <- sum(joined$wage_t    * joined$share_t)
  w_cf   <- sum(joined$wage_t    * joined$share_base) # 2024 con composizione 2014

  eff_salario_puro    <- sum((joined$wage_t - joined$wage_base) * joined$share_base)
  eff_composizione    <- sum(joined$wage_base * (joined$share_t - joined$share_base))
  eff_interazione     <- sum((joined$wage_t - joined$wage_base) *
                               (joined$share_t - joined$share_base))

  tibble(
    label                       = label,
    misura                      = wage_col,
    peso                        = weight_col,
    livello_2014                = w_base,
    livello_2024                = w_t,
    delta                       = w_t - w_base,
    delta_pct                   = (w_t / w_base - 1) * 100,
    effetto_salario_puro        = eff_salario_puro,
    effetto_composizione        = eff_composizione,
    effetto_interazione         = eff_interazione,
    contrafattuale_2024_comp_2014 = w_cf,
    salario_puro_pct_delta      = eff_salario_puro / (w_t - w_base) * 100,
    composizione_pct_delta      = eff_composizione / (w_t - w_base) * 100
  )
}

# -----------------------------------------------------------------------------
# 6. HEADLINE: salari aggregati totali
#     - vista (a) complessivo (4 periodi sommati)
#     - vista (b) solo "Anno intero"
# -----------------------------------------------------------------------------
headline_complessivo <- calcola_salari(d_eta_sesso, "anno") %>%
  mutate(vista = "complessivo")
headline_anno_intero <- calcola_salari(solo_anno_intero(d_eta_sesso), "anno") %>%
  mutate(vista = "anno_intero")

headline <- bind_rows(headline_complessivo, headline_anno_intero) %>%
  arrange(vista, anno)

write_csv(headline, file.path(OUT, "headline.csv"))

# -----------------------------------------------------------------------------
# 7. Periodo retribuito: come e' cambiata la frammentazione dei rapporti
# -----------------------------------------------------------------------------
periodo_anno <- calcola_salari(d_eta_sesso,
                               c("anno", COL_PER)) %>%
  rename(periodo = !!COL_PER) %>%
  group_by(anno) %>%
  mutate(quota_lavoratori = lavoratori / sum(lavoratori),
         quota_settimane_utili = settimane_utili / sum(settimane_utili)) %>%
  ungroup() %>%
  mutate(periodo = factor(periodo, levels = ordine_periodo)) %>%
  arrange(anno, periodo)

write_csv(periodo_anno, file.path(OUT, "periodo_retribuito.csv"))

# -----------------------------------------------------------------------------
# 8. Eta'
# -----------------------------------------------------------------------------
d_eta_dec <- con_eta_decennale(d_eta_sesso)

eta_complessivo <- calcola_salari(d_eta_dec, c("anno", "eta_decennale")) %>%
  mutate(vista = "complessivo")
eta_anno_intero <- calcola_salari(solo_anno_intero(d_eta_dec),
                                  c("anno", "eta_decennale")) %>%
  mutate(vista = "anno_intero")

eta <- bind_rows(eta_complessivo, eta_anno_intero) %>%
  arrange(vista, anno, eta_decennale)
write_csv(eta, file.path(OUT, "eta_decennale.csv"))

# Per sesso (utile per gap di genere x eta')
eta_sesso <- bind_rows(
  calcola_salari(d_eta_dec, c("anno", "eta_decennale", "sesso")) %>%
    mutate(vista = "complessivo"),
  calcola_salari(solo_anno_intero(d_eta_dec),
                 c("anno", "eta_decennale", "sesso")) %>%
    mutate(vista = "anno_intero")
) %>%
  arrange(vista, anno, eta_decennale, sesso)
write_csv(eta_sesso, file.path(OUT, "eta_sesso_decennale.csv"))

# -----------------------------------------------------------------------------
# 9. Settori (18 + macro)
# -----------------------------------------------------------------------------
d_ateco_macro <- d_ateco %>%
  mutate(macro_settore = mappa_macro_settore(attivita_economica_ateco_2007))

stopifnot(all(!is.na(d_ateco_macro$macro_settore)))

settore_18 <- bind_rows(
  calcola_salari(d_ateco, c("anno", "attivita_economica_ateco_2007")) %>%
    mutate(vista = "complessivo"),
  calcola_salari(solo_anno_intero(d_ateco),
                 c("anno", "attivita_economica_ateco_2007")) %>%
    mutate(vista = "anno_intero")
) %>%
  rename(settore = attivita_economica_ateco_2007) %>%
  arrange(vista, anno, settore)
write_csv(settore_18, file.path(OUT, "settore_18.csv"))

settore_macro <- bind_rows(
  calcola_salari(d_ateco_macro, c("anno", "macro_settore")) %>%
    mutate(vista = "complessivo"),
  calcola_salari(solo_anno_intero(d_ateco_macro),
                 c("anno", "macro_settore")) %>%
    mutate(vista = "anno_intero")
) %>%
  arrange(vista, anno, macro_settore)
write_csv(settore_macro, file.path(OUT, "settore_macro.csv"))

# -----------------------------------------------------------------------------
# 10. Tempo parziale x sesso
# -----------------------------------------------------------------------------
tempo_parz <- bind_rows(
  calcola_salari(d_tempo_parz,
                 c("anno", "presenza_tempo_parziale_nell_anno", "sesso")) %>%
    mutate(vista = "complessivo"),
  calcola_salari(solo_anno_intero(d_tempo_parz),
                 c("anno", "presenza_tempo_parziale_nell_anno", "sesso")) %>%
    mutate(vista = "anno_intero")
) %>%
  rename(tempo_parziale = presenza_tempo_parziale_nell_anno) %>%
  arrange(vista, anno, tempo_parziale, sesso)
write_csv(tempo_parz, file.path(OUT, "tempo_parziale.csv"))

# -----------------------------------------------------------------------------
# 11. Eta' x tipologia contrattuale
# -----------------------------------------------------------------------------
d_eta_tipol_dec <- con_eta_decennale(d_eta_tipol)

eta_tipologia <- bind_rows(
  calcola_salari(d_eta_tipol_dec,
                 c("anno", "eta_decennale", "tipologia_contrattuale")) %>%
    mutate(vista = "complessivo"),
  calcola_salari(solo_anno_intero(d_eta_tipol_dec),
                 c("anno", "eta_decennale", "tipologia_contrattuale")) %>%
    mutate(vista = "anno_intero")
) %>%
  arrange(vista, anno, eta_decennale, tipologia_contrattuale)
write_csv(eta_tipologia, file.path(OUT, "eta_tipologia.csv"))

tipologia <- bind_rows(
  calcola_salari(d_eta_tipol, c("anno", "tipologia_contrattuale")) %>%
    mutate(vista = "complessivo"),
  calcola_salari(solo_anno_intero(d_eta_tipol),
                 c("anno", "tipologia_contrattuale")) %>%
    mutate(vista = "anno_intero")
) %>%
  arrange(vista, anno, tipologia_contrattuale)
write_csv(tipologia, file.path(OUT, "tipologia_contrattuale.csv"))

# -----------------------------------------------------------------------------
# 12. Decomposizioni shift-share (2014 -> 2024)
#
# Per ogni dim e per ogni misura (annuo / settimanale / FTE) scompongo la
# variazione del salario medio nei termini "salario puro" + "composizione".
# Faccio sia su "complessivo" sia su "solo anno intero".
# -----------------------------------------------------------------------------
misure_pesi <- list(
  list(wage = "stipendio_annuo_reale",          weight = "lavoratori"),
  list(wage = "stipendio_settimanale_reale",    weight = "settimane_retribuite"),
  list(wage = "stipendio_settimanale_fte_reale",weight = "settimane_utili"),
  list(wage = "stipendio_annuo_fte_reale",      weight = "settimane_utili")
)

calcola_shift <- function(df_full, df_intero, group_cols, dim_label) {
  out <- list()
  for (mp in misure_pesi) {
    out[[length(out)+1]] <- shift_share(
      df_full, group_cols, mp$wage, mp$weight,
      label = sprintf("%s | complessivo", dim_label)
    )
    out[[length(out)+1]] <- shift_share(
      df_intero, group_cols, mp$wage, mp$weight,
      label = sprintf("%s | anno_intero", dim_label)
    )
  }
  bind_rows(out) %>%
    mutate(dim = dim_label,
           vista = if_else(str_detect(label, "anno_intero"),
                           "anno_intero", "complessivo")) %>%
    select(dim, vista, misura, peso, everything(), -label)
}

# Ricavo i livelli aggregati per dim (gia' fatti sopra) — riprendo le tabelle.
shift_eta <- calcola_shift(
  eta %>% filter(vista == "complessivo"),
  eta %>% filter(vista == "anno_intero"),
  group_cols = "eta_decennale",
  dim_label  = "eta_decennale"
)

shift_settore_macro <- calcola_shift(
  settore_macro %>% filter(vista == "complessivo"),
  settore_macro %>% filter(vista == "anno_intero"),
  group_cols = "macro_settore",
  dim_label  = "macro_settore"
)

shift_settore_18 <- calcola_shift(
  settore_18 %>% filter(vista == "complessivo"),
  settore_18 %>% filter(vista == "anno_intero"),
  group_cols = "settore",
  dim_label  = "settore_18"
)

shift_tempo_parz <- calcola_shift(
  tempo_parz %>% filter(vista == "complessivo") %>%
    unite("classe", tempo_parziale, sesso, sep = " | ", remove = TRUE),
  tempo_parz %>% filter(vista == "anno_intero") %>%
    unite("classe", tempo_parziale, sesso, sep = " | ", remove = TRUE),
  group_cols = "classe",
  dim_label  = "tempo_parziale_x_sesso"
)

shift_tipologia <- calcola_shift(
  tipologia %>% filter(vista == "complessivo"),
  tipologia %>% filter(vista == "anno_intero"),
  group_cols = "tipologia_contrattuale",
  dim_label  = "tipologia_contrattuale"
)

# Periodo retribuito: lo facciamo solo su "complessivo" perche' su anno_intero
# c'e' un solo periodo.
shift_periodo <- {
  out <- list()
  for (mp in misure_pesi) {
    out[[length(out)+1]] <- shift_share(
      periodo_anno, "periodo", mp$wage, mp$weight,
      label = "periodo_retribuito | complessivo"
    )
  }
  bind_rows(out) %>%
    mutate(dim = "periodo_retribuito", vista = "complessivo") %>%
    select(dim, vista, misura, peso, everything(), -label)
}

shift_all <- bind_rows(
  shift_eta,
  shift_settore_macro,
  shift_settore_18,
  shift_tempo_parz,
  shift_tipologia,
  shift_periodo
)
write_csv(shift_all, file.path(OUT, "shift_share.csv"))

# -----------------------------------------------------------------------------
# 13. Riepiloghi 2014 vs 2024 per dim (formato "wide" leggibile)
# -----------------------------------------------------------------------------
riepilogo_2014_2024 <- function(df, group_cols, dim_label) {
  vars_keep <- c("lavoratori", "settimane_utili",
                 "stipendio_annuo_reale", "stipendio_settimanale_reale",
                 "stipendio_settimanale_fte_reale", "stipendio_annuo_fte_reale")
  df %>%
    filter(anno %in% c(ANNO_BASE, ANNO_T)) %>%
    select(all_of(c("vista", "anno", group_cols, vars_keep))) %>%
    pivot_wider(
      names_from = anno,
      values_from = all_of(vars_keep),
      names_glue = "{.value}_{anno}"
    ) %>%
    mutate(
      lavoratori_var_pct                     = (lavoratori_2024 / lavoratori_2014 - 1) * 100,
      settimane_utili_var_pct                = (settimane_utili_2024 / settimane_utili_2014 - 1) * 100,
      stipendio_annuo_reale_var_pct          = (stipendio_annuo_reale_2024 / stipendio_annuo_reale_2014 - 1) * 100,
      stipendio_settimanale_reale_var_pct    = (stipendio_settimanale_reale_2024 / stipendio_settimanale_reale_2014 - 1) * 100,
      stipendio_settimanale_fte_reale_var_pct= (stipendio_settimanale_fte_reale_2024 / stipendio_settimanale_fte_reale_2014 - 1) * 100,
      stipendio_annuo_fte_reale_var_pct      = (stipendio_annuo_fte_reale_2024 / stipendio_annuo_fte_reale_2014 - 1) * 100,
      dim = dim_label
    ) %>%
    select(dim, everything())
}

riep_eta            <- riepilogo_2014_2024(eta, "eta_decennale", "eta_decennale")
riep_settore_macro  <- riepilogo_2014_2024(settore_macro, "macro_settore", "macro_settore")
riep_settore_18     <- riepilogo_2014_2024(settore_18, "settore", "settore_18")
riep_tempo_parz     <- riepilogo_2014_2024(tempo_parz, c("tempo_parziale","sesso"), "tempo_parziale_x_sesso")
riep_tipologia      <- riepilogo_2014_2024(tipologia, "tipologia_contrattuale", "tipologia_contrattuale")
riep_periodo <- periodo_anno %>%
  filter(anno %in% c(ANNO_BASE, ANNO_T)) %>%
  select(anno, periodo, lavoratori, quota_lavoratori,
         settimane_utili, quota_settimane_utili,
         stipendio_annuo_reale, stipendio_settimanale_reale,
         stipendio_settimanale_fte_reale, stipendio_annuo_fte_reale) %>%
  pivot_wider(
    names_from = anno,
    values_from = c(lavoratori, quota_lavoratori, settimane_utili, quota_settimane_utili,
                    stipendio_annuo_reale, stipendio_settimanale_reale,
                    stipendio_settimanale_fte_reale, stipendio_annuo_fte_reale),
    names_glue = "{.value}_{anno}"
  )

write_csv(riep_eta,            file.path(OUT, "riepilogo_eta.csv"))
write_csv(riep_settore_macro,  file.path(OUT, "riepilogo_settore_macro.csv"))
write_csv(riep_settore_18,     file.path(OUT, "riepilogo_settore_18.csv"))
write_csv(riep_tempo_parz,     file.path(OUT, "riepilogo_tempo_parziale.csv"))
write_csv(riep_tipologia,      file.path(OUT, "riepilogo_tipologia_contrattuale.csv"))
write_csv(riep_periodo,        file.path(OUT, "riepilogo_periodo_retribuito.csv"))

cat("\nFatto. CSV generati in:", OUT, "\n")
cat("File principali:\n")
cat(" - headline.csv (totale per anno, con e senza filtro anno_intero)\n")
cat(" - eta_decennale.csv, eta_sesso_decennale.csv\n")
cat(" - settore_18.csv, settore_macro.csv\n")
cat(" - tempo_parziale.csv, tipologia_contrattuale.csv, eta_tipologia.csv\n")
cat(" - periodo_retribuito.csv\n")
cat(" - shift_share.csv  (decomposizioni 2014->2024)\n")
cat(" - riepilogo_*.csv  (sintesi 2014 vs 2024 in wide)\n")
