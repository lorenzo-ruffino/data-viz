library(rvest)
library(httr)
library(tidyverse)
library(showtext)

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_ROSSO  <- "#F12938"

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
      plot.title = element_text(size = 14, color = "#1C1C1C", hjust = 0,
                                margin = margin(b = 0.1, unit = "cm")),
      plot.subtitle = element_text(size = 9, color = "#1C1C1C", hjust = 0,
                                   lineheight = 1.35,
                                   margin = margin(b = 0.25, t = 0.1, unit = "cm")),
      plot.caption = element_text(size = 9, color = "#1C1C1C", hjust = 1,
                                  margin = margin(t = 0.5, unit = "cm")),
      ...
    )
}

CAP <- "Elaborazione di Lorenzo Ruffino su dati dell'Osservatorio Meteorologico dell'Università di Torino"

# --- Dati (solo giugno) -----------------------------------------------------

estrai_mese <- function(anno, mese) {
  url <- paste0("https://www.meteo.dfg.unito.it/mese-", mese, "-", anno)
  pagina <- GET(url, config(ssl_verifypeer = 0), user_agent("Mozilla/5.0")) %>%
    content(as = "text", encoding = "ISO-8859-1") %>% read_html()
  tabella <- pagina %>% html_element(".divTable.mese")
  righe <- tabella %>% html_elements(".divTableRow")
  intestazioni <- righe[[1]] %>% html_elements(".divTableHead") %>%
    html_text(trim = TRUE) %>% str_replace_all("[\\r\\n]", " ") %>% str_squish()
  righe[-1] %>%
    map(function(riga) {
      celle <- riga %>% html_elements(".divTableCell")
      valori <- celle %>% map_chr(~ .x %>% html_text(trim = TRUE) %>% str_split("\n") %>% .[[1]] %>% .[1])
      if (length(valori) == 0 || !str_detect(valori[1], "^[0-9]+$")) return(NULL)
      valori
    }) %>% compact() %>%
    map_df(~ set_names(as.list(.x), intestazioni)) %>% mutate(anno = anno, mese = mese)
}

pulisci <- function(df) {
  df %>%
    mutate(
      Tmax = str_extract(`Tmax[°C]`, "^[0-9]+\\.?[0-9]*") %>% as.numeric(),
      Tmin = str_extract(`Tmin[°C]`, "^[0-9]+\\.?[0-9]*") %>% as.numeric(),
      Tmed = str_extract(`Tmed[°C]`, "^[0-9]+\\.?[0-9]*") %>% as.numeric(),
      giorno = as.numeric(giorno)
    ) %>%
    select(anno, mese, giorno, Tmax, Tmin, Tmed) %>%
    filter(!is.na(Tmed)) %>%
    mutate(data_generica = as.Date(paste(2024, mese, giorno, sep = "-")))
}

# 2003: giugno scaricato dal sito
d2003 <- estrai_mese(2003, 6) %>% pulisci()

# 2026: giugno dal CSV già scaricato
d2026 <- read_csv("../output/temperature_maggio_giugno_torino.csv",
                  show_col_types = FALSE) %>%
  mutate(data_generica = as.Date(data_generica)) %>%
  filter(anno == 2026, mese == 6)

# --- Etichette --------------------------------------------------------------

lab_2026 <- d2026 %>% slice_max(data_generica, n = 1) %>%
  transmute(data_generica, y = Tmed, label = "2026", color = COL_BLU)

lab_2003 <- d2003 %>% slice_max(data_generica, n = 1) %>%
  transmute(data_generica, y = Tmed, label = "2003", color = COL_ROSSO)

# --- Grafico ----------------------------------------------------------------

p <- ggplot() +
  # Fasce sfumate tra minima e massima
  geom_ribbon(data = d2003, aes(data_generica, ymin = Tmin, ymax = Tmax),
              fill = COL_ROSSO, alpha = 0.10) +
  geom_ribbon(data = d2026, aes(data_generica, ymin = Tmin, ymax = Tmax),
              fill = COL_BLU, alpha = 0.10) +
  # 2003 (rosso): media continua, min/max tratteggiate
  geom_line(data = d2003, aes(data_generica, Tmax),
            color = COL_ROSSO, linetype = "dashed", linewidth = 0.7) +
  geom_line(data = d2003, aes(data_generica, Tmin),
            color = COL_ROSSO, linetype = "dashed", linewidth = 0.7) +
  geom_line(data = d2003, aes(data_generica, Tmed),
            color = COL_ROSSO, linewidth = 1.1) +
  # 2026 (blu): media continua, min/max tratteggiate
  geom_line(data = d2026, aes(data_generica, Tmax),
            color = COL_BLU, linetype = "dashed", linewidth = 0.7) +
  geom_line(data = d2026, aes(data_generica, Tmin),
            color = COL_BLU, linetype = "dashed", linewidth = 0.7) +
  geom_line(data = d2026, aes(data_generica, Tmed),
            color = COL_BLU, linewidth = 1.1) +
  # Etichette
  geom_text(data = bind_rows(lab_2026, lab_2003),
            aes(data_generica, y, label = label, color = color),
            hjust = 0, nudge_x = 0.6, vjust = 0.4,
            size = 3.6, fontface = "bold", family = "Source Sans Pro") +
  scale_color_identity() +
  scale_x_date(
    breaks = as.Date(c("2024-06-01", "2024-06-08", "2024-06-15",
                       "2024-06-22", "2024-06-29")),
    date_labels = "%d %b",
    limits = as.Date(c("2024-06-01", "2024-07-04")),
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(breaks = seq(10, 35, 5),
                     labels = function(x) paste0(x, "°")) +
  theme_linechart() +
  labs(
    title = "A Torino giugno 2026 a confronto con giugno 2003",
    subtitle = "Temperatura giornaliera dell'aria a Torino: in blu il 2026, in rosso il 2003.\nLinea continua = temperatura media, tratteggiata = minima e massima",
    caption = CAP
  )

ggsave("../output/temperature_2026_vs_2003_giugno_torino.png", p,
       width = 8, height = 6.5, dpi = 300, bg = "white")
