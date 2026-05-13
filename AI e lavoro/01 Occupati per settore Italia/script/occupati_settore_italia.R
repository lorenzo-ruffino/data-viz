# Occupati per settore di attività economica in Italia, 1861-2025.
#
# Fonti concatenate:
#   - 1861-1991: Istat, "L'Italia in 150 anni. Sommario di statistiche storiche
#     1861-2010", Tavola 10.3 (popolazione attiva in condizione professionale
#     ai Censimenti, in migliaia). Decennali, M+F.
#     https://www.istat.it/wp-content/uploads/2019/03/cap_10.pdf#page=22
#
#   - 1993-2003: Istat, Tavola 10.9.1 — "Occupati per settore di attività
#     economica, posizione nella professione e sesso 1993-2015" (RFL).
#     Annuali, M+F (dipendenti + indipendenti).
#     https://seriestoriche.istat.it/fileadmin/documenti/Tavola_10.9.1.xls
#
#   - 2004-2025: Istat esploradati, Rilevazione sulle forze di lavoro,
#     Occupati Ateco 2007 - posizione professionale, regime orario.
#     Annuali, M+F, 15-89 anni, totale posizione e tempo pieno.
#     https://esploradati.istat.it/databrowser/#/it/dw/categories/IT1,Z0500LAB,1.0/LAB_OFFER/LAB_OFF_EMPLOY/DCCV_OCCUPATIT1/DCCV_OCCUPATIT1_SECTECOACT/IT1,150_938_DF_DCCV_OCCUPATIT1_10,1.0

library(tidyverse)
library(showtext)
library(readxl)
library(zoo)

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

# ---------------------------------------------------------------------------
# 1) Dati storici 1861-1991 (PDF Istat Tav. 10.3, popolazione attiva)
# ---------------------------------------------------------------------------

storici <- tibble(
  anno        = c(1861, 1871, 1881, 1901, 1911, 1921, 1931, 1936,
                  1951, 1961, 1971, 1981, 1991),
  agricoltura = c(10827, 10765, 10527, 10301, 10211,  9731,  9422,  9177,
                   8261,  5657,  3243,  2240,  1630),
  industria   = c( 2806,  3051,  3248,  3716,  4152,  4338,  4793,  5075,
                   6290,  7886,  8350,  8001,  7601),
  servizi     = c( 1902,  2125,  2315,  2678,  3134,  3399,  3997,  4331,
                   5026,  5976,  7238, 10004, 12092)
) %>%
  mutate(totale = agricoltura + industria + servizi,
         fonte  = "Censimenti")

# ---------------------------------------------------------------------------
# 2) Serie storica 1993-2003 (Istat Tav. 10.9.1, RFL)
# ---------------------------------------------------------------------------

xlsx_path <- "/Users/lorenzoruffino/Downloads/Tavola_10.9.1.xls"

raw_xls <- read_excel(xlsx_path, sheet = 1, col_names = FALSE) %>%
  mutate(anno_num = suppressWarnings(as.numeric(gsub(" .*", "", ...1)))) %>%
  filter(!is.na(anno_num), anno_num >= 1993, anno_num <= 2003) %>%
  # La sezione "MASCHI E FEMMINE" è la prima, quindi distinct() su anno tiene
  # la prima occorrenza.
  distinct(anno_num, .keep_all = TRUE) %>%
  transmute(
    anno        = anno_num,
    agricoltura = as.numeric(...2) + as.numeric(...3),    # dip + ind
    industria   = as.numeric(...5) + as.numeric(...6),
    servizi     = as.numeric(...11) + as.numeric(...12),
    totale      = as.numeric(...14) + as.numeric(...15),
    fonte       = "Rilevazione forze di lavoro"
  )

# ---------------------------------------------------------------------------
# 3) Dati recenti 2004-2025 (CSV Istat esploradati)
# ---------------------------------------------------------------------------

csv_path <- "/Users/lorenzoruffino/Downloads/Occupati (migliaia) - Ateco 2007 - posizione professionale, tempo pieno_parziale (IT1,150_938_DF_DCCV_OCCUPATIT1_10,1.0).csv"

# Il CSV Istat ha virgole all'interno di campi delimitati da apostrofi singoli
# (es. 'Agricoltura, silvicoltura e pesca'). Nessun parser standard di R le
# gestisce correttamente, quindi pre-processo le righe rimuovendo le virgole
# interne ai segmenti quotati con apostrofi prima di passarle a read.csv.
all_lines <- readLines(csv_path, encoding = "UTF-8")
header <- all_lines[1]
body <- all_lines[-1]
body_fixed <- vapply(body, function(line) {
  matches <- regmatches(line, gregexpr("'[^']*'", line))[[1]]
  for (m in matches) {
    line <- sub(m, gsub(",", "", m, fixed = TRUE), line, fixed = TRUE)
  }
  line
}, character(1))
tmp_csv <- tempfile(fileext = ".csv")
writeLines(c(header, unname(body_fixed)), tmp_csv, useBytes = TRUE)
recente_raw <- read.csv(tmp_csv, fileEncoding = "UTF-8-BOM",
                        stringsAsFactors = FALSE)
recente <- recente_raw[, c("SEX", "AGE", "POSIZ_PROF", "FULL_PART_TIME",
                           "ECON_ACTIVITY_NACE_2007", "TIME_PERIOD",
                           "Osservazione")] %>%
  as_tibble() %>%
  filter(SEX == 9, AGE == "Y15-89",
         POSIZ_PROF == 9, FULL_PART_TIME == 9,
         ECON_ACTIVITY_NACE_2007 %in% c("A", "0011", "0012", "0010")) %>%
  mutate(anno   = as.integer(TIME_PERIOD),
         valore = as.numeric(Osservazione)) %>%
  filter(!is.na(anno), !is.na(valore)) %>%
  select(anno, settore = ECON_ACTIVITY_NACE_2007, valore) %>%
  pivot_wider(names_from = settore, values_from = valore) %>%
  rename(agricoltura = A, industria = `0011`,
         servizi = `0012`, totale = `0010`) %>%
  mutate(fonte = "Rilevazione forze di lavoro") %>%
  filter(anno >= 2004) %>%
  arrange(anno)

# ---------------------------------------------------------------------------
# 4) Concatena e calcola percentuali
# ---------------------------------------------------------------------------

df <- bind_rows(storici, raw_xls, recente) %>%
  arrange(anno) %>%
  # Nel CSV Istat il 2004 manca della riga "0010 TOTALE": ricostruiamo il
  # totale come somma dei tre macro-settori per non avere NA nelle percentuali.
  mutate(totale = ifelse(is.na(totale),
                         agricoltura + industria + servizi, totale),
         pct_agricoltura = agricoltura / totale * 100,
         pct_industria   = industria   / totale * 100,
         pct_servizi     = servizi     / totale * 100)

write_csv(df, "../output/occupati_settore_italia.csv")

# Applichiamo una media mobile centrata a 3 anni SOLO ai dati annuali
# 1993-2025 per smussare il rumore campionario della RFL. I dati censuari
# (decennali) restano puntuali.
plot_data_cens <- df %>%
  filter(fonte == "Censimenti") %>%
  select(anno, fonte, Agricoltura = pct_agricoltura,
         Industria = pct_industria, Servizi = pct_servizi) %>%
  pivot_longer(c(Agricoltura, Industria, Servizi),
               names_to = "settore", values_to = "pct")

# Per smussare la giuntura fra l'ultimo punto censuario (1991) e il primo
# annuale (1993), prependiamo il valore del 1991 in input al rollapply.
# In questo modo il punto del 1993 è mediato con quello del 1991, e poi
# tolto dal risultato (resta solo nella serie censuaria).
plot_data_rfl <- df %>%
  filter(anno >= 1991) %>%
  select(anno, fonte, Agricoltura = pct_agricoltura,
         Industria = pct_industria, Servizi = pct_servizi) %>%
  pivot_longer(c(Agricoltura, Industria, Servizi),
               names_to = "settore", values_to = "pct") %>%
  arrange(settore, anno) %>%
  group_by(settore) %>%
  mutate(pct = zoo::rollapply(pct, width = 3, FUN = mean,
                              fill = NA, align = "center", partial = TRUE)) %>%
  ungroup() %>%
  filter(fonte != "Censimenti")

plot_data <- bind_rows(plot_data_cens, plot_data_rfl) %>%
  arrange(settore, anno)

# ---------------------------------------------------------------------------
# 5) Grafico
# ---------------------------------------------------------------------------

palette_settori <- c(
  "Agricoltura" = "#F12938",   # rosso
  "Industria"   = "#1C1C1C",   # nero
  "Servizi"     = "#0478EA"    # blu
)

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

# Etichette inline a destra (anno più recente)
labels_end <- plot_data %>%
  filter(anno == max(anno)) %>%
  mutate(label_text = sprintf("%s\n%d%%", settore, round(pct)))

# Punti decennali per i dati censuari (pre-1992), per evidenziare la natura
# diversa della serie storica.
punti_censuari <- plot_data %>% filter(anno <= 1991)

anno_max <- max(plot_data$anno)

p <- ggplot(plot_data, aes(x = anno, y = pct, color = settore, group = settore)) +
  geom_line(linewidth = 0.9) +
  geom_point(data = punti_censuari, size = 1.6) +
  geom_text(data = labels_end,
            aes(label = label_text),
            hjust = 0, nudge_x = 2, vjust = 0.5,
            fontface = "bold", size = 3.8,
            family = "Source Sans Pro",
            lineheight = 1) +
  scale_color_manual(values = palette_settori) +
  scale_x_continuous(limits = c(1860, anno_max + 30),
                     breaks = seq(1860, 2020, 20),
                     expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(0, 75),
                     breaks = seq(0, 70, 10),
                     labels = function(x) paste0(x, "%"),
                     expand = c(0.01, 0.01)) +
  labs(title = "Sette lavoratori italiani su dieci sono nei servizi",
       subtitle = "Quota di occupati per settore di attività economica, Italia, 1861-2025\nI punti sono i dati censuari decennali, la linea continua dal 1993 è la rilevazione annuale Istat",
       caption = "Elaborazione di Lorenzo Ruffino su dati Istat") +
  theme_linechart()

ggsave("../output/occupati_settore_italia.png", p,
       width = 8.5, height = 6.5, units = "in", dpi = 220, bg = "white")
