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
COL_GRIGIO <- "#C8C8C8"

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

# --- Dati -------------------------------------------------------------------

estrai_mese <- function(anno, mese) {
  url <- paste0("https://www.meteo.dfg.unito.it/mese-", mese, "-", anno)

  pagina <- tryCatch({
    GET(url, config(ssl_verifypeer = 0), user_agent("Mozilla/5.0")) %>%
      content(as = "text", encoding = "ISO-8859-1") %>%
      read_html()
  }, error = function(e) {
    message("Errore ", anno, " mese ", mese, ": ", e$message)
    return(NULL)
  })
  if (is.null(pagina)) return(NULL)

  tabella <- pagina %>% html_element(".divTable.mese")
  if (is.na(tabella)) {
    message("Tabella non trovata ", anno, " mese ", mese)
    return(NULL)
  }

  righe <- tabella %>% html_elements(".divTableRow")
  intestazioni <- righe[[1]] %>%
    html_elements(".divTableHead") %>%
    html_text(trim = TRUE) %>%
    str_replace_all("[\\r\\n]", " ") %>%
    str_squish()

  righe[-1] %>%
    map(function(riga) {
      celle <- riga %>% html_elements(".divTableCell")
      valori <- celle %>%
        map_chr(~ .x %>% html_text(trim = TRUE) %>% str_split("\n") %>% .[[1]] %>% .[1])
      if (length(valori) == 0 || !str_detect(valori[1], "^[0-9]+$")) return(NULL)
      valori
    }) %>%
    compact() %>%
    map_df(~ set_names(as.list(.x), intestazioni)) %>%
    mutate(anno = anno, mese = mese)
}

# Maggio (mese = 5) dal 2005 al 2026
maggio <- 2005:2026 %>%
  map_df(~ estrai_mese(.x, 5)) %>%
  mutate(
    Tmax   = str_extract(`Tmax[°C]`, "^[0-9]+\\.?[0-9]*") %>% as.numeric(),
    Tmin   = str_extract(`Tmin[°C]`, "^[0-9]+\\.?[0-9]*") %>% as.numeric(),
    Tmed   = str_extract(`Tmed[°C]`, "^[0-9]+\\.?[0-9]*") %>% as.numeric(),
    giorno = as.numeric(giorno)
  ) %>%
  select(anno, mese, giorno, Tmax, Tmin, Tmed) %>%
  filter(!is.na(Tmed)) %>%
  mutate(data_generica = as.Date(paste(2024, mese, giorno, sep = "-")))

write_csv(maggio, "../output/temperature_maggio_torino.csv")

# Media giornaliera 2005-2025
media <- maggio %>%
  filter(anno >= 2005, anno <= 2025) %>%
  group_by(data_generica) %>%
  summarise(Tmed = mean(Tmed, na.rm = TRUE), .groups = "drop")

# --- Etichette inline (ultimo punto di ciascuna serie) ----------------------

lab_2026 <- maggio %>%
  filter(anno == 2026) %>%
  slice_max(data_generica, n = 1) %>%
  transmute(data_generica, Tmed, label = "2026", color = COL_ROSSO)

lab_media <- media %>%
  slice_max(data_generica, n = 1) %>%
  transmute(data_generica, Tmed, label = "Media\n2005-2025", color = COL_BLU)

# --- Grafico ----------------------------------------------------------------

p <- ggplot(maggio, aes(x = data_generica, y = Tmed)) +
  geom_line(data = filter(maggio, anno < 2026),
            aes(group = anno), color = COL_GRIGIO, linewidth = 0.3) +
  geom_line(data = media, color = COL_BLU, linewidth = 0.9) +
  geom_line(data = filter(maggio, anno == 2026),
            color = COL_ROSSO, linewidth = 1.1) +
  geom_text(data = bind_rows(lab_2026, lab_media),
            aes(label = label, color = color),
            hjust = 0, nudge_x = 1, vjust = 0.4,
            size = 3.6, fontface = "bold", lineheight = 0.9,
            family = "Source Sans Pro") +
  scale_color_identity() +
  scale_x_date(
    breaks = as.Date(c("2024-05-01", "2024-05-08", "2024-05-15",
                       "2024-05-22", "2024-05-29")),
    date_labels = "%d %b",
    limits = as.Date(c("2024-05-01", "2024-06-06")),
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(breaks = seq(10, 30, 5),
                     labels = function(x) paste0(x, "°")) +
  theme_linechart() +
  labs(
    title = "A Torino maggio 2026 è più caldo della media",
    subtitle = "Temperatura media giornaliera dell'aria a Torino, in grigio i singoli anni dal 2005 al 2025",
    caption = CAP
  )

ggsave("../output/temperature_maggio_torino.png", p,
       width = 8, height = 6.5, dpi = 300, bg = "white")

# Confronto sintetico sugli stessi giorni disponibili nel 2026
giorni_2026 <- maggio %>% filter(anno == 2026) %>% pull(giorno)
maggio %>%
  filter(giorno %in% giorni_2026) %>%
  mutate(periodo = if_else(anno == 2026, "2026", "2005-2025")) %>%
  group_by(periodo) %>%
  summarise(across(c(Tmax, Tmed, Tmin), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  print()
