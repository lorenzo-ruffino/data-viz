library(tidyverse)
library(showtext)
library(jsonlite)

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_ROSSO  <- "#F12938"
COL_GRIGIO <- "#9A9A9A"
COL_GRIGIO_SCURO <- "#5A5A5A"

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

fmt_pct <- function(v) paste0(ifelse(v > 0, "+", ""),
                              formatC(v, format = "f", digits = 1, decimal.mark = ","), "%")

IN  <- "../input"
OUT <- "../output"

# --- Dati -------------------------------------------------------------------

# Prezzi al consumo (Nic), variazione tendenziale mensile
nic <- read_csv(file.path(IN, "istat_nic_tendenziale.csv"), show_col_types = FALSE,
                col_types = cols(.default = col_character())) |>
  transmute(mese = TIME_PERIOD, var = as.numeric(OBS_VALUE)) |>
  mutate(anno = as.integer(substr(mese, 1, 4)), m = as.integer(substr(mese, 6, 7)))

infl_gen_set <- nic |> filter(m <= 9) |> group_by(anno) |> summarise(infl = mean(var))
infl_gen_ago <- nic |> filter(m <= 8) |> group_by(anno) |> summarise(infl = mean(var))

# Spesa delle famiglie a prezzi correnti (conti trimestrali Istat via Eurostat)
fcs <- fromJSON(file.path(IN, "eurostat_consumi_famiglie_trimestrali.json"))
idx <- unlist(fcs$dimension$time$category$index)
consumi <- tibble(trim = names(idx), valore = unlist(fcs$value[as.character(idx)])) |>
  mutate(anno = as.integer(substr(trim, 1, 4)), q = as.integer(substr(trim, 7, 7)))
consumi_h1 <- consumi |> filter(q <= 2) |> group_by(anno) |> summarise(v = sum(valore)) |>
  mutate(var = 100 * (v / lag(v) - 1))

iva <- read_csv(file.path(IN, "mef_iva_gen_ago.csv"), show_col_types = FALSE)

# --- 01: crescita attesa della spesa e imponibile degli scontrini ---------

# Acquisti in quantità: spesa delle famiglie a prezzi costanti, primo semestre
vol <- fromJSON(file.path(IN, "eurostat_consumi_famiglie_volume.json"))
idv <- unlist(vol$dimension$time$category$index)
volume <- tibble(trim = names(idv), valore = unlist(vol$value[as.character(idv)])) |>
  mutate(anno = as.integer(substr(trim, 1, 4)), q = as.integer(substr(trim, 7, 7)))
vol_h1 <- volume |> filter(q <= 2) |> group_by(anno) |> summarise(v = sum(valore)) |>
  mutate(var = 100 * (v / lag(v) - 1))

QUANTITA <- round(vol_h1$var[vol_h1$anno == 2026], 1)          # +1,1%
PREZZI   <- round(infl_gen_set$infl[infl_gen_set$anno == 2026], 1)  # +2,6%
ATTESA   <- round(100 * ((1 + QUANTITA / 100) * (1 + PREZZI / 100) - 1), 1)
SCONTRINI <- 4.1
BASE_2025 <- 13.5 / 0.041                                      # mld, gen-set 2025
SCARTO_MLD <- BASE_2025 * (SCONTRINI - ATTESA) / 100

g1 <- tibble(
  x = c(1, 1, 2),
  ymin = c(0, QUANTITA, 0),
  ymax = c(QUANTITA, ATTESA, SCONTRINI),
  fill = c(COL_BLU, "#7FB6F2", COL_ROSSO)
)

p1 <- ggplot(g1) +
  geom_rect(aes(xmin = x - 0.275, xmax = x + 0.275, ymin = ymin, ymax = ymax, fill = fill)) +
  # etichette interne alla barra impilata
  annotate("text", x = 1, y = QUANTITA / 2, label = paste0("Più consumi\n(quantità)\n", fmt_pct(QUANTITA)),
           color = "white", family = "Source Sans Pro", fontface = "bold", size = 3.5, lineheight = 0.95) +
  annotate("text", x = 1, y = QUANTITA + (ATTESA - QUANTITA) / 2,
           label = paste0("Prezzi più alti\n", fmt_pct(PREZZI)),
           color = COL_NERO, family = "Source Sans Pro", fontface = "bold", size = 3.5, lineheight = 0.95) +
  # totali sopra le barre
  annotate("text", x = 1, y = ATTESA, label = fmt_pct(ATTESA), vjust = -0.6,
           color = COL_BLU, family = "Source Sans Pro", fontface = "bold", size = 4.4) +
  annotate("text", x = 2, y = SCONTRINI, label = fmt_pct(SCONTRINI), vjust = -0.6,
           color = COL_ROSSO, family = "Source Sans Pro", fontface = "bold", size = 4.4) +
  # scarto
  annotate("segment", x = 1.3, xend = 2.27, y = ATTESA, yend = ATTESA,
           linetype = "dashed", color = COL_GRIGIO, linewidth = 0.4) +
  annotate("segment", x = 2.36, xend = 2.36, y = ATTESA, yend = SCONTRINI,
           color = COL_NERO, linewidth = 0.5) +
  annotate("text", x = 2.41, y = (ATTESA + SCONTRINI) / 2, hjust = 0,
           label = paste0("Scarto: circa\n",
                          formatC(SCARTO_MLD, format = "f", digits = 1, decimal.mark = ","), " miliardi"),
           family = "Source Sans Pro", size = 3.3, color = COL_NERO, lineheight = 0.95) +
  scale_fill_identity() +
  scale_x_continuous(breaks = c(1, 2), limits = c(0.5, 2.9), expand = c(0, 0),
                     labels = c("Crescita attesa con più consumi\ne prezzi più alti",
                                "Crescita del valore\ndegli scontrini")) +
  scale_y_continuous(limits = c(0, 5), breaks = 1:5, expand = c(0, 0),
                     labels = function(x) paste0("+", x, "%")) +
  coord_cartesian(clip = "off") +
  labs(title = "Gli scontrini crescono poco più di consumi e prezzi",
       subtitle = "Variazione sul 2025: scontrini e prezzi tra gennaio e settembre, consumi nel primo semestre, Italia",
       caption = "Elaborazione di Lorenzo Ruffino su dati Agenzia delle entrate e Istat") +
  theme_linechart() +
  theme(axis.text.x = element_text(size = 10, lineheight = 1.1))

ggsave(file.path(OUT, "01_scontrini_prezzi_consumi.png"), p1,
       width = 9, height = 6.5, dpi = 220, bg = "white")

# --- 02: Iva scambi interni negli anni --------------------------------------

g2 <- iva |> filter(anno >= 2023) |>
  select(anno, var = iva_scambi_interni_var_pct) |>
  left_join(infl_gen_ago, by = "anno") |>
  mutate(colore = if_else(anno == 2026, COL_ROSSO, COL_BLU))

p2 <- ggplot(g2, aes(x = factor(anno))) +
  geom_col(aes(y = var, fill = colore), width = 0.6) +
  geom_text(aes(y = var, label = fmt_pct(var), color = colore), vjust = -0.7,
            family = "Source Sans Pro", fontface = "bold", size = 3.8) +
  geom_segment(aes(x = as.numeric(factor(anno)) - 0.3, xend = as.numeric(factor(anno)) + 0.3,
                   y = infl, yend = infl), linewidth = 1.3, color = COL_NERO) +
  geom_text(aes(x = as.numeric(factor(anno)) + 0.33, y = infl, label = fmt_pct(infl)),
            hjust = 0, family = "Source Sans Pro", size = 3.2, color = COL_NERO) +
  # legenda manuale
  annotate("rect", xmin = 0.55, xmax = 0.65, ymin = 9.35, ymax = 9.75, fill = COL_BLU) +
  annotate("rect", xmin = 0.67, xmax = 0.77, ymin = 9.35, ymax = 9.75, fill = COL_ROSSO) +
  annotate("text", x = 0.82, y = 9.55, hjust = 0, size = 3.4, family = "Source Sans Pro",
           color = COL_NERO, label = "Barre: crescita del gettito Iva sulle vendite in Italia (scambi interni, escluse le importazioni)") +
  annotate("segment", x = 0.55, xend = 0.77, y = 8.75, yend = 8.75, linewidth = 1.3, color = COL_NERO) +
  annotate("text", x = 0.82, y = 8.75, hjust = 0, size = 3.4, family = "Source Sans Pro",
           color = COL_NERO, label = "Linea: crescita dei prezzi al consumo (inflazione) nello stesso periodo") +
  annotate("text", x = 4, y = 5.2, label = "Primo anno con\nPos collegati alla cassa",
           family = "Source Sans Pro", fontface = "bold", size = 3.3,
           color = COL_ROSSO, lineheight = 0.95) +
  scale_fill_identity() + scale_color_identity() +
  scale_y_continuous(limits = c(0, 10), breaks = seq(2, 8, 2), expand = c(0, 0),
                     labels = function(x) paste0("+", x, "%")) +
  coord_cartesian(clip = "off") +
  labs(title = "Con il Pos collegato alla cassa l'Iva non cresce di più",
       subtitle = "Variazione tra gennaio e agosto rispetto allo stesso periodo dell'anno prima, Italia, 2023-2026",
       caption = "Elaborazione di Lorenzo Ruffino su dati Mef e Istat") +
  theme_linechart() +
  theme(axis.text.x = element_text(size = 10))

ggsave(file.path(OUT, "02_iva_scambi_interni.png"), p2,
       width = 9, height = 6.5, dpi = 220, bg = "white")

# --- 03: Iva per settore ----------------------------------------------------

g3 <- iva |> filter(anno >= 2024) |>
  select(anno, Commercio = commercio_pct, Industria = industria_pct,
         `Servizi privati` = servizi_privati_pct) |>
  pivot_longer(-anno, names_to = "settore", values_to = "var") |>
  mutate(anno = factor(anno),
         settore = factor(settore, levels = c("Commercio", "Industria", "Servizi privati")))

col_anni <- c("2024" = "#C9C9C9", "2025" = COL_GRIGIO_SCURO, "2026" = COL_ROSSO)
col_lab  <- c("2024" = COL_GRIGIO, "2025" = COL_GRIGIO_SCURO, "2026" = COL_ROSSO)

p3 <- ggplot(g3, aes(x = settore, y = var, fill = anno)) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = COL_NERO) +
  geom_col(position = position_dodge(width = 0.75), width = 0.68) +
  geom_text(aes(label = fmt_pct(var), color = anno,
                vjust = ifelse(var < 0, 1.6, -0.6)),
            position = position_dodge(width = 0.75),
            family = "Source Sans Pro", fontface = "bold", size = 3.4) +
  scale_fill_manual(values = col_anni) +
  scale_color_manual(values = col_lab, guide = "none") +
  scale_y_continuous(limits = c(-2, 8), breaks = seq(-2, 8, 2), expand = c(0, 0),
                     labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "%")) +
  coord_cartesian(clip = "off") +
  labs(title = "Sale l'Iva del commercio, rallentano gli altri settori",
       subtitle = "Variazione dell'Iva sugli scambi interni per settore tra gennaio e agosto sull'anno prima, Italia",
       caption = "Elaborazione di Lorenzo Ruffino su dati Mef") +
  theme_linechart() +
  theme(axis.text.x = element_text(size = 10), axis.line.x = element_blank(),
        legend.justification = "left")

ggsave(file.path(OUT, "03_iva_per_settore.png"), p3,
       width = 9, height = 6.5, dpi = 220, bg = "white")

# --- CSV e controlli ---------------------------------------------------------

write_csv(tibble(voce = c("Acquisti in quantità (primo semestre)", "Prezzi al consumo (gen-set)",
                          "Crescita attesa della spesa", "Imponibile degli scontrini (gen-set)",
                          "Scarto in miliardi"),
                  valore = c(QUANTITA, PREZZI, ATTESA, SCONTRINI, SCARTO_MLD)),
          file.path(OUT, "confronto_2026.csv"))
cat("Quantità", QUANTITA, "Prezzi", PREZZI, "Attesa", ATTESA, "Scarto mld", round(SCARTO_MLD, 2), "\n")
write_csv(g2 |> select(anno, iva_scambi_interni = var, inflazione_gen_ago = infl),
          file.path(OUT, "iva_scambi_interni.csv"))

cat("Inflazione gen-set:\n"); print(infl_gen_set)
cat("Inflazione gen-ago:\n"); print(infl_gen_ago)
cat("Consumi H1 nominali:\n"); print(consumi_h1)
