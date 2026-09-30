# script/bilancio_primo_semestre.R — eseguito da `cd script && Rscript bilancio_primo_semestre.R`
# Waterfall del bilancio demografico gennaio-giugno 2026, Italia.
# Dati: Istat, bilancio demografico mensile (demo.istat.it, tavola D7B),
# JSON restituito da RPCCerca.php e salvato in input/.

library(tidyverse)
library(jsonlite)
library(showtext)

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO  <- "#1C1C1C"
COL_BLU   <- "#0478EA"
COL_ROSSO <- "#F12938"
COL_GRIGIO_SCURO <- "#5A5A5A"
CAP_ISTAT <- "Elaborazione di Lorenzo Ruffino su dati Istat"

theme_linechart <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro"),
      legend.position = "none",
      axis.line = element_line(linewidth = 0.3),
      axis.text = element_text(size = 9, color = "#1C1C1C", hjust = 0.5),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      panel.border = element_blank(),
      plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), "cm"),
      plot.title.position = "plot",
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

source_dir <- ".."
input_dir  <- file.path(source_dir, "input")
output_dir <- file.path(source_dir, "output")

# 1) Dati ---------------------------------------------------------------------

raw <- fromJSON(file.path(input_dir, "istat_d7b_2026_italia.json"))$datatable$data

sem <- raw %>%
  filter(sesso == 9, mese <= 6)
stopifnot(nrow(sem) == 6)

tot <- sem %>%
  summarise(across(c(nati_vivi, morti, imm_estero, emi_estero), sum))

pop_ini <- sem$pop_iniziale[sem$mese == 1]
pop_fin <- sem$pop_finale[sem$mese == 6]
saldo   <- with(tot, nati_vivi - morti + imm_estero - emi_estero)
stopifnot(saldo == pop_fin - pop_ini)   # nel provvisorio contano solo le poste "reali"

voci <- tribble(
  ~voce,                     ~valore,          ~tipo,
  "Nascite",                 tot$nati_vivi,    "flusso",
  "Decessi",                 -tot$morti,       "flusso",
  "Arrivi\ndall'estero",     tot$imm_estero,   "flusso",
  "Partenze\nper l'estero",  -tot$emi_estero,  "flusso",
  "Saldo\ntotale",           NA,               "totale"
)

wf <- voci %>%
  mutate(
    cum_flussi = cumsum(replace_na(valore, 0)),
    fine   = cum_flussi,
    inizio = if_else(tipo == "flusso", lag(cum_flussi, default = 0), 0),
    valore = if_else(tipo == "flusso", valore, fine),
    id     = row_number(),
    colore = case_when(tipo != "flusso" ~ "tot",
                       valore > 0 ~ "pos",
                       TRUE ~ "neg")
  )

write_csv(wf %>% select(voce, valore, inizio, fine) %>%
            mutate(voce = str_replace_all(voce, "\n", " ")),
          file.path(output_dir, "bilancio_primo_semestre.csv"))

# 2) Grafico ------------------------------------------------------------------

# arrotondato alle migliaia; il saldo finale (sotto il migliaio) resta intero
fmt_num <- function(x) {
  s <- if_else(abs(x) >= 1000,
               paste0(formatC(round(abs(x) / 1000), format = "d",
                              big.mark = ".", decimal.mark = ","), " mila"),
               formatC(abs(x), format = "d", big.mark = ".", decimal.mark = ","))
  paste0(if_else(x > 0, "+", "−"), s)
}
fmt_asse <- function(x) {
  s <- formatC(abs(x) / 1000, format = "d", big.mark = ".", decimal.mark = ",")
  ifelse(x == 0, "0", paste0(if_else(x > 0, "+", "−"), s, " mila"))
}

w <- 0.62
wf <- wf %>%
  mutate(
    xmin = id - w / 2, xmax = id + w / 2,
    ymin = pmin(inizio, fine), ymax = pmax(inizio, fine),
    # etichetta sopra la barra se sale, sotto se scende
    y_lab = if_else(valore >= 0, ymax, ymin),
    vj    = if_else(valore >= 0, -0.6, 1.6)
  )

connettori <- wf %>%
  mutate(x = xmax, xend = lead(xmin), y = fine) %>%
  filter(!is.na(xend))

p <- ggplot(wf) +
  geom_hline(yintercept = 0, colour = COL_NERO, linewidth = 0.3) +
  geom_segment(data = connettori,
               aes(x = x, xend = xend, y = y, yend = y),
               colour = "#9A9A9A", linewidth = 0.35, linetype = "dashed") +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                fill = colore)) +
  # la variazione finale è minuscola: un trattino la rende visibile
  geom_segment(data = filter(wf, tipo == "totale"),
               aes(x = xmin, xend = xmax, y = fine, yend = fine),
               colour = COL_NERO, linewidth = 1.2) +
  geom_text(aes(x = id, y = y_lab, label = fmt_num(valore), vjust = vj),
            family = "Source Sans Pro", fontface = "bold", size = 3.6,
            colour = COL_NERO) +
  scale_fill_manual(values = c(pos = COL_BLU, neg = COL_ROSSO,
                               tot = COL_GRIGIO_SCURO)) +
  scale_x_continuous(breaks = wf$id, labels = wf$voce,
                     expand = expansion(add = 0.4)) +
  scale_y_continuous(breaks = seq(-150000, 150000, 50000),
                     labels = fmt_asse,
                     limits = c(-185000, 190000)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Come cambia la popolazione nei primi sei mesi del 2026",
    subtitle = "Nascite, decessi e trasferimenti di residenza con l'estero, dati provvisori, Italia, gennaio-giugno 2026",
    caption = CAP_ISTAT
  ) +
  theme_linechart() +
  theme(axis.line = element_blank(),
                  axis.text.x = element_text(size = 9, color = "#1C1C1C",
                                             lineheight = 1.1,
                                             margin = margin(t = 0.2, unit = "cm")))

ggsave(file.path(output_dir, "bilancio_primo_semestre.png"),
       plot = p, width = 8, height = 6.5, dpi = 220, bg = "white")

cat("Saldo:", saldo, "| popolazione 1 gen:", pop_ini, "| 30 giu:", pop_fin, "\n")
