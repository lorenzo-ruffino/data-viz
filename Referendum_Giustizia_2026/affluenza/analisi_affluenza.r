# Analisi correlazione affluenza Referendum 2026 vs Politiche 2022
# - Y: ratio affluenza referendum (ultima com disponibile) / affluenza politiche 2022
# - X: % voti CDX / CSX / Centro alle politiche 2022 per comune
# - Esclude comuni senza dati aggiornati

library(stringr)
library(dplyr)
library(data.table)
library(ggplot2)
library(showtext)

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()

script_dir <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Referendum_Giustizia_2026/affluenza"

# ==============================================================================
# 1. CARICA DATI
# ==============================================================================

# Aggiornare i dati: eseguire prima Affluenza.r e Precedenti.r
affluenza_comuni_long <- fread(file.path(script_dir, "affluenza_comuni_long.csv"))
affluenza_nazionale <- affluenza_comuni_long %>%
  group_by(comunicazione, dt_com) %>%
  summarise(ele_t = sum(ele_t, na.rm = TRUE), vot_t = sum(vot_t, na.rm = TRUE), .groups = "drop") %>%
  mutate(perc = vot_t / ele_t)

pol_eur <- fread(file.path(script_dir, "pol_eur.csv")) %>%
  mutate(codice_istat = sprintf("%06d", as.integer(codice_istat)))

# ==============================================================================
# 2. INDIVIDUA ULTIMA COMUNICAZIONE DISPONIBILE
# ==============================================================================

# Ultima com con almeno alcuni comuni che hanno riportato dati (vot_t > 0)
ultima_com <- affluenza_comuni_long %>%
  filter(vot_t > 0) %>%
  pull(comunicazione) %>%
  max(na.rm = TRUE)

# Timestamp dell'ultima comunicazione (per il titolo)
dt_ultima <- affluenza_comuni_long %>%
  filter(comunicazione == ultima_com) %>%
  pull(dt_com) %>%
  first()

# Converti timestamp numerico (es. 20260322190000) in label leggibile
# "domenica alle 19:00", "lunedì alle 15:00" ecc.
dt_parsed <- as.POSIXct(as.character(dt_ultima), format = "%Y%m%d%H%M%S", tz = "Europe/Rome")
giorni_ita <- c("domenica", "lunedì", "martedì", "mercoledì", "giovedì", "venerdì", "sabato")
giorno_label <- giorni_ita[as.integer(format(dt_parsed, "%w")) + 1]
ora_label    <- format(dt_parsed, "%H:%M")
label_com    <- paste0(giorno_label, " alle ", ora_label)

cat("Ultima comunicazione disponibile: com", ultima_com, "->", label_com, "\n")

# ==============================================================================
# 3. DATI REFERENDUM 2026 ALL'ULTIMA COM
# ==============================================================================

# Prendi l'ultima com per ogni comune; escludi chi non ha ancora comunicato
affl_ultima <- affluenza_comuni_long %>%
  filter(comunicazione == ultima_com, vot_t > 0) %>%
  select(codice_istat, desc_comune, desc_provincia, desc_regione,
         ele_t, vot_t) %>%
  mutate(codice_istat = sprintf("%06d", as.integer(codice_istat)),
         perc_ref = vot_t / ele_t)

cat("Comuni con dati:", nrow(affl_ultima), "\n")

# ==============================================================================
# 4. JOIN CON POLITICHE 2022
# ==============================================================================

pol_base <- pol_eur %>%
  filter(coalizione %in% c("CDX", "CSX", "Centro")) %>%
  select(comune_norm, codice_istat, coalizione,
         pol_affluenza, pol_perc, pol_elettori) %>%
  filter(!is.na(pol_affluenza), !is.na(pol_perc)) %>%
  mutate(codice_istat = gsub('="(.*)"', "\\1", as.character(codice_istat)))

df_analisi <- inner_join(affl_ultima, pol_base, by = "codice_istat") %>%
  mutate(ratio_affl = perc_ref / pol_affluenza) %>%
  filter(!is.na(ratio_affl), is.finite(ratio_affl))

cat("Comuni dopo join:", length(unique(df_analisi$codice_istat)), "\n")

# ==============================================================================
# 5. CORRELAZIONI
# ==============================================================================

correlazioni <- df_analisi %>%
  group_by(coalizione) %>%
  summarise(
    r       = cor(pol_perc, ratio_affl, use = "complete.obs"),
    n       = n(),
    .groups = "drop"
  )

cat("\n--- Correlazioni (ratio affluenza 2026/2022 ~ % voti politiche 2022) ---\n")
print(correlazioni)

# ==============================================================================
# 6. TEMA GRAFICO (da Grafici.r)
# ==============================================================================

theme_linechart <- function(...) {
  theme_minimal() +
    theme(
      text             = element_text(family = "Source Sans Pro"),
      legend.position  = "top",
      axis.line        = element_line(linewidth = 0.3),
      axis.text        = element_text(size = 10, color = "#1C1C1C", hjust = 0.5,
                           margin = margin(b = 0, t = 0, l = 0, unit = "cm")),
      axis.ticks       = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background  = element_blank(),
      legend.background     = element_blank(),
      legend.box.background = element_blank(),
      legend.key       = element_blank(),
      panel.border     = element_blank(),
      legend.title     = element_blank(),
      plot.margin      = unit(c(b = 0.4, l = 0.4, t = 0.4, r = 0.4), "cm"),
      plot.title.position = "plot",
      legend.text  = element_text(size = 13, color = "#1C1C1C", hjust = 0,
                       margin = margin(b = 0, l = 0, t = 0, r = 0, unit = "cm")),
      plot.title   = element_text(size = 18, color = "#1C1C1C", hjust = 0,
                       margin = margin(b = 0, l = 0, t = 0, r = 0, unit = "cm")),
      plot.subtitle = element_text(size = 11, color = "#1C1C1C", hjust = 0,
                        lineheight = 1.1,
                        margin = margin(b = 0.2, l = 0, t = 0.15, r = 0, unit = "cm")),
      plot.caption = element_text(size = 11, color = "#1C1C1C", hjust = 1,
                       margin = margin(b = 0, l = 0, t = 0.5, r = 0, unit = "cm")),
      ...
    )
}

colori <- c("CDX" = "#0478EA", "CSX" = "#E8000D", "Centro" = "#FF9EC4")

# Subtitle con correlazioni
sub_corr <- correlazioni %>%
  mutate(txt = paste0(coalizione, ": r = ", round(r, 3))) %>%
  pull(txt) %>%
  paste(collapse = "   |   ")

# ==============================================================================
# 7. GRAFICO
# ==============================================================================

nome_file <- file.path(script_dir, paste0("analisi_affluenza_com", ultima_com, ".png"))
png(nome_file, width = 9, height = 7, units = "in", res = 300)

ggplot(df_analisi, aes(x = pol_perc * 100, y = ratio_affl,
                       color = coalizione, size = pol_elettori)) +
  geom_point(alpha = 0.15, show.legend = FALSE) +
  geom_smooth(aes(group = coalizione, weight = pol_elettori), method = "lm", se = FALSE,
              linewidth = 1.3, show.legend = TRUE) +
  #geom_hline(yintercept = 1, linetype = "dashed", color = "#888888") +
  scale_color_manual(values = colori,
                     labels = c("CDX" = "Centro-destra",
                                "CSX" = "Centro-sinistra + M5S",
                                "Centro" = "Centro")) +
  scale_size_continuous(guide = "none", range = c(0.3, 8)) +
  scale_x_continuous(
    expand = c(0.01, 0.01),
    labels = function(x) paste0(x, "%")
  ) +
  scale_y_continuous(
    expand = c(0.01, 0.01),
    labels = function(x) paste0(round(x * 100), "%")
  ) +
  theme_linechart() +
  labs(
    x        = "% voti alle Politiche 2022",
    y        = paste0("Affluenza referendum (", label_com, ") / Affluenza Politiche 2022"),
    title    = paste0("Affluenza al referendum — ", label_com),
    subtitle = paste0("Rapporto tra affluenza attuale e finale alle Politiche 2022 per comune\n", sub_corr),
    caption  = "Elaborazione di Lorenzo Ruffino | Fonte: Ministero dell'Interno"
  )

dev.off()
cat("Grafico salvato:", nome_file, "\n")

# ==============================================================================
# 8. GRAFICO PER AREA GEOGRAFICA (Nord / Centro / Mezzogiorno)
# ==============================================================================

area_geo <- c(
  "PIEMONTE"              = "Nord",
  "VALLE D'AOSTA"         = "Nord",
  "LOMBARDIA"             = "Nord",
  "TRENTINO-ALTO ADIGE"   = "Nord",
  "VENETO"                = "Nord",
  "FRIULI-VENEZIA GIULIA" = "Nord",
  "LIGURIA"               = "Nord",
  "EMILIA-ROMAGNA"        = "Nord",
  "TOSCANA"               = "Centro",
  "UMBRIA"                = "Centro",
  "MARCHE"                = "Centro",
  "LAZIO"                 = "Centro",
  "ABRUZZO"               = "Mezzogiorno",
  "MOLISE"                = "Mezzogiorno",
  "CAMPANIA"              = "Mezzogiorno",
  "PUGLIA"                = "Mezzogiorno",
  "BASILICATA"            = "Mezzogiorno",
  "CALABRIA"              = "Mezzogiorno",
  "SICILIA"               = "Mezzogiorno",
  "SARDEGNA"              = "Mezzogiorno"
)

df_geo <- df_analisi %>%
  mutate(area = factor(area_geo[desc_regione], levels = c("Nord", "Centro", "Mezzogiorno"))) %>%
  filter(!is.na(area))

nome_file_geo <- file.path(script_dir, paste0("analisi_affluenza_geo_com", ultima_com, ".png"))
png(nome_file_geo, width = 10, height = 8, units = "in", res = 300)
ggplot(df_geo, aes(x = pol_perc * 100, y = ratio_affl,
                   color = coalizione, size = pol_elettori)) +
  geom_point(alpha = 0.15, show.legend = FALSE) +
  geom_smooth(aes(group = coalizione, weight = pol_elettori), method = "lm", se = FALSE,
              linewidth = 1, show.legend = TRUE) +
  facet_wrap(~area, nrow = 1) +
  scale_color_manual(values = colori,
                     labels = c("CDX" = "Centro-destra",
                                "CSX" = "Centro-sinistra + M5S",
                                "Centro" = "Centro")) +
  scale_size_continuous(guide = "none", range = c(0.3, 8)) +
  scale_x_continuous(expand = c(0.01, 0.01), labels = function(x) paste0(x, "%")) +
  scale_y_continuous(expand = c(0.01, 0.01), labels = function(x) paste0(round(x * 100), "%")) +
  theme_linechart() +
  theme(strip.text = element_text(size = 13, face = "bold", color = "#1C1C1C")) +
  labs(
    x        = "% voti alle Politiche 2022",
    y        = paste0("Affluenza referendum (", label_com, ") / Affluenza Politiche 2022"),
    title    = paste0("Affluenza al referendum per area geografica — ", label_com),
    subtitle = "Rapporto tra affluenza attuale e finale alle Politiche 2022 per comune",
    caption  = "Elaborazione di Lorenzo Ruffino | Fonte: Ministero dell'Interno"
  )

dev.off()
cat("Grafico geo salvato:", nome_file_geo, "\n")

# ==============================================================================
# 9. GRAFICO VS EUROPEE 2024
# ==============================================================================

pol_base_eu <- pol_eur %>%
  filter(coalizione %in% c("CDX", "CSX", "Centro")) %>%
  select(comune_norm, codice_istat, coalizione,
         eur_affluenza, eur_perc, pol_elettori) %>%
  filter(!is.na(eur_affluenza), !is.na(eur_perc))

df_analisi_eu <- inner_join(affl_ultima, pol_base_eu, by = "codice_istat") %>%
  mutate(ratio_affl = perc_ref / eur_affluenza) %>%
  filter(!is.na(ratio_affl), is.finite(ratio_affl))

correlazioni_eu <- df_analisi_eu %>%
  group_by(coalizione) %>%
  summarise(r = cor(eur_perc, ratio_affl, use = "complete.obs"), .groups = "drop")

sub_corr_eu <- correlazioni_eu %>%
  mutate(txt = paste0(coalizione, ": r = ", round(r, 3))) %>%
  pull(txt) %>%
  paste(collapse = "   |   ")

nome_file_eu <- file.path(script_dir, paste0("analisi_affluenza_EU_com", ultima_com, ".png"))
png(nome_file_eu, width = 9, height = 7, units = "in", res = 300)

ggplot(df_analisi_eu, aes(x = eur_perc * 100, y = ratio_affl,
                          color = coalizione, size = pol_elettori)) +
  geom_point(alpha = 0.15, show.legend = FALSE) +
  geom_smooth(aes(group = coalizione, weight = pol_elettori), method = "lm", se = FALSE,
              linewidth = 1, show.legend = TRUE) +
  scale_color_manual(values = colori,
                     labels = c("CDX" = "Centro-destra",
                                "CSX" = "Centro-sinistra + M5S",
                                "Centro" = "Centro")) +
  scale_size_continuous(guide = "none", range = c(0.3, 8)) +
  scale_x_continuous(expand = c(0.01, 0.01), labels = function(x) paste0(x, "%")) +
  scale_y_continuous(expand = c(0.01, 0.01), labels = function(x) paste0(round(x * 100), "%")) +
  theme_linechart() +
  labs(
    x        = "% voti alle Europee 2024",
    y        = paste0("Affluenza referendum (", label_com, ") / Affluenza Europee 2024"),
    title    = paste0("Affluenza al referendum vs Europee 2024 — ", label_com),
    subtitle = paste0("Rapporto tra affluenza attuale e finale alle Europee 2024 per comune\n", sub_corr_eu),
    caption  = "Elaborazione di Lorenzo Ruffino | Fonte: Ministero dell'Interno"
  )

dev.off()
cat("Grafico EU salvato:", nome_file_eu, "\n")

# ==============================================================================
# 10. ANALISI PER GRADO DI URBANIZZAZIONE
# ==============================================================================

library(readxl)

urbaniz_label <- c("1" = "Città/Zone dense",
                   "2" = "Piccole città/Sobborghi",
                   "3" = "Zone rurali")

# Carica classificazione comuni
comuni_class <- read_excel(
  file.path(dirname(script_dir), "Classificazioni statistiche-e-dimensione-dei-comuni_01_01_2026.xlsx"),
  sheet = "Comuni 01-01-2026"
) %>%
  select(codice_istat = 2, urbaniz = `Grado di urbanizzazione 2021`) %>%
  mutate(
    codice_istat = sprintf("%06s", as.character(codice_istat)),
    urbaniz      = as.character(as.integer(urbaniz))
  ) %>%
  filter(!is.na(urbaniz))

# 1. Affluenza referendum per urbanizzazione (somma diretta)
affl_urb <- affl_ultima %>%
  inner_join(comuni_class, by = "codice_istat") %>%
  group_by(urbaniz) %>%
  summarise(
    ele_t_ref = sum(ele_t, na.rm = TRUE),
    vot_t_ref = sum(vot_t, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(affl_ref = vot_t_ref / ele_t_ref)

# 2. Affluenza e voti Politiche 2022 per urbanizzazione (somma diretta)
#    pol_votanti è uguale per tutti le coalizioni dello stesso comune → distinct
pol_affl_urb <- pol_eur %>%
  select(codice_istat, pol_votanti, pol_elettori) %>%
  distinct(codice_istat, .keep_all = TRUE) %>%
  inner_join(comuni_class, by = "codice_istat") %>%
  group_by(urbaniz) %>%
  summarise(
    ele_t_pol = sum(pol_elettori, na.rm = TRUE),
    vot_t_pol = sum(pol_votanti,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(affl_pol = vot_t_pol / ele_t_pol)

# 3. Voti per coalizione alle Politiche 2022 per urbanizzazione
pol_coal_urb <- pol_eur %>%
  filter(coalizione %in% c("CDX", "CSX", "Centro")) %>%
  select(codice_istat, coalizione, pol_voti) %>%
  inner_join(comuni_class, by = "codice_istat") %>%
  group_by(urbaniz, coalizione) %>%
  summarise(voti_coal = sum(pol_voti, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    pol_eur %>%
      select(codice_istat, pol_votanti) %>%
      distinct(codice_istat, .keep_all = TRUE) %>%
      inner_join(comuni_class, by = "codice_istat") %>%
      group_by(urbaniz) %>%
      summarise(tot_votanti = sum(pol_votanti, na.rm = TRUE), .groups = "drop"),
    by = "urbaniz"
  ) %>%
  mutate(perc_coal = voti_coal / tot_votanti)

# Unisci e stampa
risultati_urb <- affl_urb %>%
  left_join(pol_affl_urb, by = "urbaniz") %>%
  mutate(
    ratio_vs_pol22 = affl_ref / affl_pol,
    area           = urbaniz_label[urbaniz]
  ) %>%
  select(area, ele_t_pol, affl_ref, affl_pol, ratio_vs_pol22)

cat("\n--- Affluenza e voti per grado di urbanizzazione ---\n")
pol_coal_urb %>%
  mutate(area = urbaniz_label[urbaniz]) %>%
  select(area, coalizione, perc_coal) %>%
  tidyr::pivot_wider(names_from = coalizione, values_from = perc_coal) %>%
  left_join(risultati_urb, by = "area") %>%
  mutate(
    perc_elettorato = paste0(round(ele_t_pol / sum(ele_t_pol) * 100, 1), "%"),
    across(c(CDX, CSX, Centro), ~paste0(round(. * 100, 1), "%")),
    across(c(affl_ref, affl_pol), ~paste0(round(. * 100, 1), "%")),
    ratio_vs_pol22 = round(ratio_vs_pol22, 3)
  ) %>%
  select(area, perc_elettorato, affl_ref, affl_pol, ratio_vs_pol22, CDX, CSX, Centro) %>%
  print()
