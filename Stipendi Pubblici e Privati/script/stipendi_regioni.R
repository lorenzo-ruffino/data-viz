# script/stipendi_regioni.R — eseguito da `cd script && Rscript stipendi_regioni.R`

library(tidyverse)
library(showtext)

source_dir <- ".."
input_dir  <- file.path(source_dir, "input")
output_dir <- file.path(source_dir, "output")

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_ROSSO  <- "#F12938"
COL_GRIGIO <- "#9A9A9A"

CAP_INPS <- "Elaborazione di Lorenzo Ruffino su dati Inps"

# 1) INPS 2024 per regione ------------------------------------------------------
# Formato long: la posizione prevalente compare solo sulla prima riga del suo
# blocco, le successive hanno " " — va propagata. Campi: posizione; regione; "";
# lavoratori; settimane; redditi.
righe <- read_lines(file.path(input_dir, "inps_redditi_regioni_2024_raw.csv"))
righe <- righe[str_detect(righe, '^"')]

parse_num <- function(x) suppressWarnings(as.numeric(str_remove_all(x, '[."\\s]')))
pulisci   <- function(x) str_trim(str_remove_all(x, '"'))

dati <- map_dfr(righe, function(r) {
  campi <- str_split(r, ";")[[1]]
  if (length(campi) < 6) return(NULL)
  tibble(posizione = pulisci(campi[1]), regione = pulisci(campi[2]),
         lavoratori = parse_num(campi[4]), redditi = parse_num(campi[6]))
}) %>%
  mutate(posizione = ifelse(posizione == "", NA, posizione)) %>%
  fill(posizione) %>%
  filter(posizione %in% c("Dipendente privato", "Dipendente pubblico"),
         !regione %in% c("Totale", "Estero", "Regione", ""),
         !is.na(lavoratori)) %>%
  mutate(
    serie = ifelse(posizione == "Dipendente pubblico",
                   "Dipendenti pubblici", "Dipendenti privati"),
    regione = case_when(
      str_detect(regione, "Aosta")    ~ "Valle d'Aosta",
      str_detect(regione, "Trentino") ~ "Trentino-Alto Adige",
      str_detect(regione, "Friuli")   ~ "Friuli-Venezia Giulia",
      str_detect(regione, "Emilia")   ~ "Emilia-Romagna",
      TRUE ~ regione
    ),
    stipendio = redditi / lavoratori
  )

stopifnot(nrow(dati) == 40)

# Ordina per stipendio privato: in un asse y discreto il primo livello sta in
# basso, quindi ordine crescente mette la regione più ricca in alto.
ordine <- dati %>% filter(serie == "Dipendenti privati") %>%
  arrange(stipendio) %>% pull(regione)

plot_data <- dati %>%
  mutate(regione = factor(regione, levels = ordine),
         serie   = factor(serie, levels = c("Dipendenti privati",
                                            "Dipendenti pubblici")))

# Estremi del bilanciere: la linea grigia va da privati a pubblici.
segmenti <- plot_data %>%
  select(regione, serie, stipendio) %>%
  pivot_wider(names_from = serie, values_from = stipendio) %>%
  rename(pubblici = `Dipendenti pubblici`, privati = `Dipendenti privati`)

write_csv(segmenti %>% mutate(divario = pubblici - privati) %>%
            arrange(desc(privati)),
          file.path(output_dir, "stipendi_regioni.csv"))

# 2) Grafico --------------------------------------------------------------------
colori <- c("Dipendenti pubblici" = COL_ROSSO,
            "Dipendenti privati"  = COL_BLU)

fmt_eur <- function(x) paste0("€ ", formatC(round(x / 100) * 100, format = "d",
                                            big.mark = "."))

# Ogni valore sta all'esterno del proprio punto, così la linea grigia resta
# libera: i privati (punto di sinistra) a sinistra, i pubblici a destra.
label_data <- plot_data %>%
  mutate(hjust   = ifelse(serie == "Dipendenti privati", 1, 0),
         nudge_x = ifelse(serie == "Dipendenti privati", -700, 700))

p <- ggplot() +
  geom_segment(data = segmenti,
               aes(x = privati, xend = pubblici, y = regione, yend = regione),
               colour = COL_GRIGIO, linewidth = 0.7) +
  geom_point(data = plot_data,
             aes(x = stipendio, y = regione, colour = serie), size = 3.2) +
  geom_text(data = label_data,
            aes(x = stipendio + nudge_x, y = regione,
                label = fmt_eur(stipendio), colour = serie, hjust = hjust),
            size = 2.9, fontface = "bold", family = "Source Sans Pro",
            show.legend = FALSE) +
  scale_colour_manual(values = colori, name = NULL) +
  scale_x_continuous(limits = c(13000, 43200), expand = c(0, 0)) +
  guides(colour = guide_legend(override.aes = list(size = 3.6))) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    text = element_text(family = "Source Sans Pro"),
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 10, color = COL_NERO, hjust = 0),
    legend.margin = margin(b = 0.2, unit = "cm"),
    axis.text.y = element_text(size = 9.5, color = COL_NERO, hjust = 1),
    axis.text.x = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), "cm"),
    plot.title.position = "plot",
    plot.title = element_text(family = "Source Sans Pro SemiBold",
                              size = 14, color = COL_NERO, hjust = 0,
                              margin = margin(b = 0.1, unit = "cm")),
    plot.subtitle = element_text(size = 9, color = COL_NERO, hjust = 0,
                                 lineheight = 1.35,
                                 margin = margin(b = 0.25, t = 0.1, unit = "cm")),
    plot.caption = element_text(size = 9, color = COL_NERO, hjust = 1,
                                margin = margin(t = 0.5, unit = "cm"))
  ) +
  labs(
    title = "Al Sud lo stipendio pubblico vale quasi il doppio",
    subtitle = "Reddito medio annuo lordo dei lavoratori dipendenti per regione, Italia, 2024",
    caption = CAP_INPS
  )

ggsave(file.path(output_dir, "stipendi_regioni.png"),
       plot = p, width = 8, height = 8, units = "in", dpi = 220, bg = "white")

# Sanity check
segmenti %>% mutate(divario = pubblici - privati) %>%
  arrange(desc(privati)) %>% print(n = 20)
