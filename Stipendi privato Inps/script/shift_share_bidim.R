# =============================================================================
# Shift-share bidimensionale: settore (ATECO) x tipologia contrattuale
#
# Decompone la variazione del salario annuo FTE reale 2014 -> 2024 in:
#   1. effetto salario puro (dentro ciascuna cella settore x tipologia)
#   2. effetto composizione settoriale (a parità di mix tipologia entro settore)
#   3. effetto composizione contrattuale (a parità di mix settoriale)
#   4. interazione composizione settore x composizione tipologia
#
# I 4 termini sommano esattamente a Delta_w_medio (decomposizione di Das Gupta
# in versione "average weights" per garantire additività esatta).
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
})

PROJ <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Stipendi privato Inps"
OUT  <- file.path(PROJ, "output")

ANNO_BASE <- 2014L
ANNO_T    <- 2024L

# CPI per deflattore a prezzi 2024
cpi <- read_csv(file.path(PROJ, "indice_prezzi_istat.csv"),
                show_col_types = FALSE) %>%
  select(anno, indice) %>%
  filter(anno >= 2014, anno <= 2024) %>%
  mutate(deflattore_2024 = max(indice[anno == 2024]) / indice)

# Dataset bidimensionale
d <- read_csv(file.path(PROJ, "lavoratori_settore_tipologia_2014_2024.csv"),
              show_col_types = FALSE) %>%
  rename(
    settore   = attivita_economica_ateco_2007,
    tipologia = tipologia_contrattuale,
    lavoratori           = numero_lavoratori_con_almeno_una_giornata_retribuita_nell_anno,
    settimane_retribuite = numero_settimane_retribuite_nell_anno,
    settimane_utili      = numero_settimane_utili_nell_anno,
    giornate_retribuite  = numero_giornate_retribuite_nell_anno,
    somma_retribuzioni   = somma_retribuzioni_nell_anno
  )

# Calcola annuo FTE reale per ogni (anno, settore, tipologia)
d <- d %>%
  left_join(cpi %>% select(anno, deflattore_2024), by = "anno") %>%
  mutate(
    salario_annuo_fte = (somma_retribuzioni / settimane_utili) * 52,
    salario_annuo_fte_reale = salario_annuo_fte * deflattore_2024
  ) %>%
  select(-deflattore_2024)

# -----------------------------------------------------------------------------
# Filtra ANNO_BASE e ANNO_T, prepara matrice settore x tipologia
# -----------------------------------------------------------------------------
d_base <- d %>%
  filter(anno == ANNO_BASE) %>%
  select(settore, tipologia,
         w_base = salario_annuo_fte_reale,
         n_base = settimane_utili) %>%
  filter(n_base > 0, !is.na(w_base))

d_fin <- d %>%
  filter(anno == ANNO_T) %>%
  select(settore, tipologia,
         w_fin = salario_annuo_fte_reale,
         n_fin = settimane_utili) %>%
  filter(n_fin > 0, !is.na(w_fin))

celle <- full_join(d_base, d_fin, by = c("settore", "tipologia")) %>%
  mutate(
    w_base = coalesce(w_base, 0),
    w_fin  = coalesce(w_fin, 0),
    n_base = coalesce(n_base, 0),
    n_fin  = coalesce(n_fin, 0),
    s_base = n_base / sum(n_base),
    s_fin  = n_fin  / sum(n_fin),
    s_avg  = (s_base + s_fin) / 2,
    w_avg  = (w_base + w_fin) / 2
  )

# -----------------------------------------------------------------------------
# Verifica: media pesata 2014 e 2024
# -----------------------------------------------------------------------------
w_base_tot <- sum(celle$w_base * celle$s_base)
w_fin_tot  <- sum(celle$w_fin  * celle$s_fin)
delta_tot  <- w_fin_tot - w_base_tot

cat(sprintf("Salario annuo FTE reale (settore x tipologia):\n"))
cat(sprintf("  2014: %.0f euro\n", w_base_tot))
cat(sprintf("  2024: %.0f euro\n", w_fin_tot))
cat(sprintf("  Delta: %+.0f euro (%.2f%%)\n\n", delta_tot, delta_tot/w_base_tot*100))

# -----------------------------------------------------------------------------
# Decomposizione totale: composizione vs salario puro (additività esatta con
# pesi/salari medi tra i due anni)
# -----------------------------------------------------------------------------
celle <- celle %>%
  mutate(
    contrib_comp = (s_fin - s_base) * w_avg,
    contrib_wage = (w_fin - w_base) * s_avg
  )

eff_comp_tot <- sum(celle$contrib_comp)
eff_wage_tot <- sum(celle$contrib_wage)

cat(sprintf("Decomposizione di primo livello:\n"))
cat(sprintf("  Salario puro (intra-cella): %+.0f euro (%.1f%% del calo)\n",
            eff_wage_tot, eff_wage_tot/delta_tot*100))
cat(sprintf("  Composizione totale:        %+.0f euro (%.1f%% del calo)\n",
            eff_comp_tot, eff_comp_tot/delta_tot*100))
cat(sprintf("  Somma:                      %+.0f euro\n\n",
            eff_wage_tot + eff_comp_tot))

# -----------------------------------------------------------------------------
# Decomposizione della parte "composizione" in: settore + tipologia + interaz.
# -----------------------------------------------------------------------------
# Definisco:
#   p_i = peso del settore i sul totale = sum_j s_ij
#   q_j_given_i = peso della tipologia j entro settore i = s_ij / p_i
# Quindi s_ij = p_i * q_j_given_i

settore_tot <- celle %>%
  group_by(settore) %>%
  summarise(
    p_base = sum(s_base),
    p_fin  = sum(s_fin),
    .groups = "drop"
  ) %>%
  mutate(p_avg = (p_base + p_fin) / 2,
         delta_p = p_fin - p_base)

celle <- celle %>%
  left_join(settore_tot, by = "settore") %>%
  mutate(
    q_base = ifelse(p_base > 0, s_base / p_base, 0),
    q_fin  = ifelse(p_fin  > 0, s_fin  / p_fin , 0),
    q_avg  = (q_base + q_fin) / 2,
    delta_q = q_fin - q_base
  )

# Effetto settore puro (Δp con q fissato a media):
#   sum_ij Δp_i * q_ij_avg * w_ij_avg
eff_settore <- sum(celle$delta_p * celle$q_avg * celle$w_avg)

# Effetto tipologia entro-settore (Δq con p fissato a media):
#   sum_ij p_i_avg * Δq_ij * w_ij_avg
eff_tipologia <- sum(celle$p_avg * celle$delta_q * celle$w_avg)

# Interazione: residuo
eff_interaz_comp <- eff_comp_tot - eff_settore - eff_tipologia

cat(sprintf("Decomposizione del termine 'composizione':\n"))
cat(sprintf("  - Settore puro (Δ peso settoriale):    %+.0f euro (%.1f%% del calo)\n",
            eff_settore, eff_settore/delta_tot*100))
cat(sprintf("  - Tipologia entro settore (Δ mix det/ind/stag): %+.0f euro (%.1f%% del calo)\n",
            eff_tipologia, eff_tipologia/delta_tot*100))
cat(sprintf("  - Interazione settore x tipologia:     %+.0f euro (%.1f%% del calo)\n",
            eff_interaz_comp, eff_interaz_comp/delta_tot*100))
cat(sprintf("  Somma: %+.0f\n\n", eff_settore + eff_tipologia + eff_interaz_comp))

cat(sprintf("DECOMPOSIZIONE FINALE (somma = %.0f euro = %.1f%%):\n", delta_tot, delta_tot/w_base_tot*100))
cat(sprintf("  1. Salario puro (intra cella settore x tipologia): %+.0f euro (%.1f%%)\n",
            eff_wage_tot, eff_wage_tot/delta_tot*100))
cat(sprintf("  2. Composizione settoriale pura:                   %+.0f euro (%.1f%%)\n",
            eff_settore, eff_settore/delta_tot*100))
cat(sprintf("  3. Composizione contrattuale entro-settore:        %+.0f euro (%.1f%%)\n",
            eff_tipologia, eff_tipologia/delta_tot*100))
cat(sprintf("  4. Interazione settore x tipologia:                %+.0f euro (%.1f%%)\n",
            eff_interaz_comp, eff_interaz_comp/delta_tot*100))

# -----------------------------------------------------------------------------
# Salva tabella riepilogo
# -----------------------------------------------------------------------------
riepilogo <- tibble(
  componente = c(
    "Totale (Δ salario annuo FTE reale 2014→2024)",
    "1. Salario puro (intra-cella settore × tipologia)",
    "2. Composizione settoriale pura",
    "3. Composizione contrattuale entro-settore",
    "4. Interazione settore × tipologia"
  ),
  euro_per_lavoratore = c(delta_tot, eff_wage_tot, eff_settore,
                          eff_tipologia, eff_interaz_comp),
  pct_del_calo = c(100,
                   eff_wage_tot / delta_tot * 100,
                   eff_settore / delta_tot * 100,
                   eff_tipologia / delta_tot * 100,
                   eff_interaz_comp / delta_tot * 100)
)
write_csv(riepilogo, file.path(OUT, "shift_share_settore_tipologia.csv"))

# Salva anche il dettaglio per cella (chi ha contribuito di più)
dettaglio <- celle %>%
  select(settore, tipologia, w_base, w_fin, s_base, s_fin,
         contrib_comp, contrib_wage) %>%
  arrange(desc(abs(contrib_comp + contrib_wage)))
write_csv(dettaglio, file.path(OUT, "shift_share_settore_tipologia_dettaglio.csv"))

cat(sprintf("\nFile generati in %s\n", OUT))
cat(" - shift_share_settore_tipologia.csv (riepilogo)\n")
cat(" - shift_share_settore_tipologia_dettaglio.csv (per cella)\n")
