# =============================================================================
# Quota di occupati part-time e di dipendenti a tempo determinato, Italia,
# 2004-2026 (dati trimestrali, media mobile su 4 trimestri)
# Fonti: Istat, Rilevazione sulle forze di lavoro (esploradati.istat.it)
#   - 150_938_DF_DCCV_OCCUPATIT1_6: dipendenti permanenti / a termine, 2004-oggi
#   - 150_938_DF_DCCV_OCCUPATIT1_4: occupati tempo pieno / parziale, 2018-oggi
#   - 150_1188_DF_DCCV_OCCUPATIT1_UNT2020_3: idem, regolamento precedente,
#     2004-2020 (usato per 2004-2017; nel 2018-2020 le due serie coincidono
#     entro 0,2 punti)
# Tutte le serie: Italia, totale sessi, 15-64 anni. Eseguire da script/.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(showtext)
})

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_ROSSO  <- "#F12938"
COL_GRIGIO <- "#9A9A9A"

theme_linechart <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro"),
      legend.position = "top",
      axis.line = element_line(linewidth = 0.3),
      axis.text = element_text(size = 9, color = "#1C1C1C", hjust = 0.5),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.key = element_blank(),
      panel.border = element_blank(),
      legend.title = element_blank(),
      plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), "cm"),
      plot.title.position = "plot",
      legend.text = element_text(size = 10, color = "#1C1C1C", hjust = 0),
      plot.title = element_text(family = "Source Sans Pro SemiBold",
                                size = 14, color = "#1C1C1C", hjust = 0,
                                margin = margin(b = 0.1, unit = "cm")),
      plot.subtitle = element_text(size = 9, color = "#1C1C1C", hjust = 0,
                                   lineheight = 1.35,
                                   margin = margin(b = 0.25, t = 0.1, unit = "cm")),
      plot.caption = element_text(size = 9, color = "#1C1C1C", hjust = 1,
                                  margin = margin(t = 0.5, unit = "cm")),
      ...
    )
}

input_dir  <- "../input"
output_dir <- "../output"

# --- Dati --------------------------------------------------------------------

leggi <- function(f) {
  read_csv(file.path(input_dir, f), show_col_types = FALSE,
           col_types = cols(.default = col_character())) %>%
    filter(REF_AREA == "IT", SEX == "9", AGE == "Y15-64") %>%
    mutate(valore = as.numeric(OBS_VALUE))
}

# Part-time: quota sul totale degli occupati
quota_pt <- function(f) {
  leggi(f) %>%
    filter(POSIZ_PROF == "9", FULL_PART_TIME %in% c("2", "9")) %>%
    select(trimestre = TIME_PERIOD, FULL_PART_TIME, valore) %>%
    pivot_wider(names_from = FULL_PART_TIME, values_from = valore) %>%
    transmute(trimestre, part_time_mila = `2`, occupati_mila = `9`) %>%
    filter(!is.na(part_time_mila))
}

pt_vecchio <- quota_pt("istat_150_1188_DF_DCCV_OCCUPATIT1_UNT2020_3.csv")
pt_nuovo   <- quota_pt("istat_150_938_DF_DCCV_OCCUPATIT1_4.csv")

# Raccordo: regolamento precedente fino al 2017, nuovo dal 2018
part_time <- bind_rows(
  filter(pt_vecchio, trimestre < "2018-Q1"),
  filter(pt_nuovo,   trimestre >= "2018-Q1")
)

# Tempo determinato: quota sul totale dei dipendenti
termine <- leggi("istat_150_938_DF_DCCV_OCCUPATIT1_6.csv") %>%
  filter(POSIZ_PROF == "1", FULL_PART_TIME == "9") %>%
  select(trimestre = TIME_PERIOD, PERM_TEMP_EMPLOYEES, valore) %>%
  pivot_wider(names_from = PERM_TEMP_EMPLOYEES, values_from = valore) %>%
  transmute(trimestre, termine_mila = `1`, permanenti_mila = `2`,
            dipendenti_mila = `9`)

stopifnot(all(abs(termine$termine_mila + termine$permanenti_mila -
                  termine$dipendenti_mila) < 0.01))

media4 <- function(x) as.numeric(stats::filter(x, rep(1 / 4, 4), sides = 1))

dati <- full_join(part_time, termine, by = "trimestre") %>%
  arrange(trimestre) %>%
  mutate(
    anno  = as.integer(substr(trimestre, 1, 4)),
    q     = as.integer(substr(trimestre, 7, 7)),
    x     = anno + (q - 0.5) / 4,
    quota_part_time = 100 * part_time_mila / occupati_mila,
    quota_termine   = 100 * termine_mila / dipendenti_mila,
    # Media degli ultimi quattro trimestri: toglie la stagionalita'
    # (i contratti a termine salgono ogni estate)
    part_time_m4 = media4(quota_part_time),
    termine_m4   = media4(quota_termine)
  )

stopifnot(!any(is.na(dati$quota_part_time)), !any(is.na(dati$quota_termine)))

dati %>%
  transmute(trimestre, occupati_mila, part_time_mila, dipendenti_mila,
            termine_mila,
            quota_part_time = round(quota_part_time, 2),
            quota_termine   = round(quota_termine, 2),
            quota_part_time_media_4_trimestri = round(part_time_m4, 2),
            quota_termine_media_4_trimestri   = round(termine_m4, 2)) %>%
  write_csv(file.path(output_dir, "tempo_determinato_part_time.csv"))

plot_data <- dati %>%
  select(x, trimestre, part_time_m4, termine_m4) %>%
  pivot_longer(c(part_time_m4, termine_m4), names_to = "serie",
               values_to = "valore") %>%
  filter(!is.na(valore)) %>%
  mutate(colore = if_else(serie == "part_time_m4", COL_ROSSO, COL_BLU))

# --- Etichette ---------------------------------------------------------------

fmt_pct <- function(v) {
  r <- round(v, 1)
  paste0(ifelse(r == round(r), formatC(r, format = "d"),
                formatC(r, format = "f", digits = 1, decimal.mark = ",")), "%")
}

# Primo punto, picco e ultimo punto di ciascuna serie
punti <- plot_data %>%
  group_by(serie) %>%
  filter(x == min(x) | x == max(x) | valore == max(valore)) %>%
  ungroup() %>%
  mutate(
    tipo  = case_when(x == min(x) ~ "inizio", x == max(x) ~ "fine",
                      TRUE ~ "picco"),
    testo = fmt_pct(valore),
    hjust = case_when(tipo == "inizio" ~ 0, tipo == "fine" ~ 0, TRUE ~ 0.5),
    # sopra la linea rossa, sotto la blu (che le sta sotto); l'ultimo valore
    # a destra del punto, perche' la linea rossa scende addosso all'etichetta
    vjust = case_when(tipo == "fine" ~ 0.5,
                      serie == "part_time_m4" ~ -1.1, TRUE ~ 2.0),
    x_lab = if_else(tipo == "fine", x + 0.3, x)
  )

label_serie <- tibble(
  x      = c(2005.4, 2008.6),
  valore = c(16.6, 10.4),
  colore = c(COL_ROSSO, COL_BLU),
  testo  = c("Occupati a tempo parziale", "Dipendenti a tempo determinato")
)

ultimo <- max(dati$trimestre)

# --- Grafico -----------------------------------------------------------------

p <- ggplot(plot_data, aes(x = x, y = valore, color = colore, group = serie)) +
  geom_line(linewidth = 0.9) +
  geom_point(data = punti, size = 2.0) +
  geom_text(data = punti,
            aes(x = x_lab, label = testo, hjust = hjust, vjust = vjust),
            family = "Source Sans Pro", fontface = "bold", size = 3.4,
            show.legend = FALSE) +
  geom_text(data = label_serie,
            aes(label = testo, group = 1), family = "Source Sans Pro",
            fontface = "bold", size = 3.6, hjust = 0, show.legend = FALSE) +
  scale_color_identity(guide = "none") +
  scale_x_continuous(breaks = seq(2005, 2025, 5),
                     limits = c(2004.6, 2027.9), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 22), breaks = seq(5, 20, 5),
                     labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Calano part-time e contratti a termine",
    subtitle = paste0("Quota di occupati a tempo parziale e di dipendenti a tempo determinato, ",
                      "media degli ultimi quattro trimestri,\n15-64 anni, Italia, 2004-",
                      substr(ultimo, 1, 4)),
    caption = "Elaborazione di Lorenzo Ruffino su dati Istat"
  ) +
  theme_linechart() +
  theme(legend.position = "none")

ggsave(file.path(output_dir, "tempo_determinato_part_time.png"), p,
       width = 9, height = 6.5, dpi = 220, bg = "white")

# --- Sanity check ------------------------------------------------------------

cat("\nTrimestri:", min(dati$trimestre), "-", ultimo, "| n =", nrow(dati), "\n")
print(punti %>% select(serie, trimestre, valore, tipo))
cat("\nMedie annue:\n")
print(dati %>% group_by(anno) %>%
        summarise(n = n(), part_time = mean(quota_part_time),
                  termine = mean(quota_termine),
                  termine_mila = mean(termine_mila),
                  part_time_mila = mean(part_time_mila)) %>%
        mutate(across(c(part_time, termine), ~ round(.x, 1)),
               across(ends_with("mila"), round)), n = 30)
