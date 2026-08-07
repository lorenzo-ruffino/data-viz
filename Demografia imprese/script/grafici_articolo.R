# Grafici per l'articolo "Perché in Italia chiudono sempre meno imprese"
# Dati: CSV master in ../input/ (tavole ISTAT Demografia d'impresa 2004-2024)

source("/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities/R/mappe.R")
library(tidyverse)
library(showtext)
library(sf)

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 220)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_VIOLA  <- "#A82DE3"
COL_ROSSO  <- "#F12938"
COL_GIALLO <- "#F2A900"
COL_GRIGIO <- "#9A9A9A"
COL_GRIGIO_SCURO <- "#5A5A5A"

CAP_ISTAT <- "Elaborazione di Lorenzo Ruffino su dati Istat"

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

INPUT  <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Demografia imprese/input"
OUTPUT <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Demografia imprese/output"

# ============================================================================
# 01 — Natalità vs mortalità con area tra le curve
# ============================================================================

serie <- read_csv(file.path(INPUT, "serie_nazionale_2004_2024.csv"), show_col_types = FALSE)

griglia <- tibble(anno = seq(2004, 2024, by = 0.02)) %>%
  mutate(natalita  = approx(serie$anno, serie$tasso_natalita,  xout = anno)$y,
         mortalita = approx(serie$anno, serie$tasso_mortalita, xout = anno)$y,
         sopra = natalita >= mortalita,
         grp = cumsum(c(1, diff(sopra) != 0)))

label_serie <- tibble(
  anno = 2024, valore = c(serie$tasso_natalita[serie$anno == 2024],
                          serie$tasso_mortalita[serie$anno == 2024]),
  nome = c("Natalità", "Mortalità"),
  colore = c(COL_BLU, COL_ROSSO))

annotazioni <- tibble(
  anno = c(2013, 2023),
  valore = c(serie$tasso_mortalita[serie$anno == 2013],
             serie$tasso_mortalita[serie$anno == 2023]),
  label = c("8,8%", "6,4%"),
  vjust = c(-1.1, 1.9))

p1 <- ggplot() +
  geom_ribbon(data = filter(griglia, !sopra),
              aes(x = anno, ymin = natalita, ymax = mortalita, group = grp),
              fill = COL_ROSSO, alpha = 0.14) +
  geom_ribbon(data = filter(griglia, sopra),
              aes(x = anno, ymin = mortalita, ymax = natalita, group = grp),
              fill = COL_BLU, alpha = 0.16) +
  geom_line(data = serie, aes(anno, tasso_natalita),  color = COL_BLU,   linewidth = 0.9) +
  geom_line(data = serie, aes(anno, tasso_mortalita), color = COL_ROSSO, linewidth = 0.9) +
  geom_point(data = filter(serie, anno %in% c(2013, 2023)),
             aes(anno, tasso_mortalita), color = COL_ROSSO, size = 1.7) +
  geom_text(data = annotazioni,
            aes(anno, valore, label = label, vjust = vjust),
            family = "Source Sans Pro", fontface = "bold",
            size = 3.4, color = COL_ROSSO) +
  geom_text(data = label_serie,
            aes(anno, valore, label = nome, color = colore),
            hjust = 0, nudge_x = 0.3, size = 3.6,
            fontface = "bold", family = "Source Sans Pro") +
  annotate("text", x = 2012, y = 7.62, label = "Più chiusure\nche aperture",
           family = "Source Sans Pro", size = 3.1, color = "#C2202D",
           lineheight = 0.95, hjust = 0.5) +
  annotate("text", x = 2022.7, y = 7.0, label = "Più aperture\nche chiusure",
           family = "Source Sans Pro", size = 3.1, color = "#0361BC",
           lineheight = 0.95, hjust = 0.5) +
  scale_color_identity() +
  scale_x_continuous(limits = c(2004, 2027), breaks = seq(2004, 2024, 4),
                     expand = c(0.01, 0)) +
  scale_y_continuous(limits = c(6, 9.1), breaks = 6:9,
                     labels = function(x) paste0(x, "%"), expand = c(0.01, 0)) +
  theme_linechart() +
  theme(legend.position = "none") +
  labs(title = "In Italia aprono più imprese di quante ne chiudano",
       subtitle = "Imprese nate e cessate ogni 100 attive, Italia, 2004-2024. La mortalità del 2024 è provvisoria",
       caption = CAP_ISTAT)

ggsave(file.path(OUTPUT, "01_natalita_mortalita.png"), p1,
       width = 8, height = 6.5, units = "in", dpi = 220, bg = "white")

serie %>% select(anno, tasso_natalita, tasso_mortalita, imprese_nate, imprese_cessate) %>%
  write_csv(file.path(OUTPUT, "01_natalita_mortalita.csv"))

# ============================================================================
# 02 — Composizione delle imprese nate per macrosettore (aree impilate 100%)
# ============================================================================

macro <- read_csv(file.path(INPUT, "demografia_macrosettori_serie.csv"), show_col_types = FALSE) %>%
  filter(macrosettore != "Totale") %>%
  group_by(anno) %>%
  mutate(quota = imprese_nate / sum(imprese_nate) * 100) %>%
  ungroup()

ordine_stack <- c("Altri servizi", "Commercio", "Costruzioni", "Industria in senso stretto")
col_stack <- c("Altri servizi" = COL_BLU, "Commercio" = COL_ROSSO,
               "Costruzioni" = COL_GIALLO, "Industria in senso stretto" = COL_GRIGIO_SCURO)

stack <- macro %>%
  mutate(macrosettore = factor(macrosettore, levels = ordine_stack)) %>%
  arrange(anno, macrosettore) %>%
  group_by(anno) %>%
  mutate(ymax = cumsum(quota), ymin = ymax - quota, ymid = (ymin + ymax) / 2) %>%
  ungroup() %>%
  # errori di virgola mobile: il cumulato puo superare 100 di 1e-14 e ggplot
  # scarterebbe i punti fuori dai limiti, spezzando la banda superiore
  mutate(ymax = pmin(ymax, 100), ymin = pmax(ymin, 0))

lab_dx <- stack %>% filter(anno == 2024) %>%
  mutate(nome = recode(as.character(macrosettore),
                       "Industria in senso stretto" = "Industria"),
         testo = ifelse(nome == "Industria",
                        paste0(nome, " ", round(quota), "%"),
                        paste0(nome, "\n", round(quota), "%")),
         colore = ifelse(as.character(macrosettore) == "Costruzioni", "#1C1C1C", "white"))

lab_sx <- stack %>% filter(anno == 2004) %>%
  mutate(testo = paste0(round(quota), "%"),
         colore = ifelse(macrosettore %in% c("Costruzioni"), "#1C1C1C", "white"))

p2 <- ggplot(stack) +
  geom_ribbon(aes(x = anno, ymin = ymin, ymax = ymax, fill = macrosettore),
              alpha = 0.92, color = "white", linewidth = 0.35) +
  geom_text(data = lab_dx,
            aes(x = 2023.6, y = ymid, label = testo, color = colore),
            hjust = 1, family = "Source Sans Pro", fontface = "bold",
            size = 3.4, lineheight = 0.95) +
  geom_text(data = lab_sx,
            aes(x = 2004.4, y = ymid, label = testo, color = colore),
            hjust = 0, family = "Source Sans Pro", fontface = "bold", size = 3.2) +
  scale_fill_manual(values = col_stack) +
  scale_color_identity() +
  scale_x_continuous(limits = c(2004, 2024), breaks = seq(2004, 2024, 4),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25),
                     labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  theme_linechart() +
  theme(legend.position = "none") +
  labs(title = "Sei nuove imprese su dieci nascono nei servizi",
       subtitle = "Composizione percentuale delle imprese nate, per macrosettore, Italia, 2004-2024",
       caption = CAP_ISTAT)

ggsave(file.path(OUTPUT, "02_composizione_nate.png"), p2,
       width = 8, height = 6.5, units = "in", dpi = 220, bg = "white")

stack %>% select(anno, macrosettore, imprese_nate, quota) %>%
  write_csv(file.path(OUTPUT, "02_composizione_nate.csv"))

# ============================================================================
# 03 — Mappa del saldo cumulato 2004-2024 per regione
# ============================================================================

regioni <- read_csv(file.path(INPUT, "demografia_regioni_serie.csv"), show_col_types = FALSE) %>%
  filter(tipo_territorio %in% c("regione", "provincia_autonoma")) %>%
  group_by(territorio) %>%
  summarise(saldo_cum = sum(tasso_natalita - tasso_mortalita), .groups = "drop")

nuts_map <- c(
  "Piemonte" = "ITC1", "Valle d'Aosta" = "ITC2", "Liguria" = "ITC3", "Lombardia" = "ITC4",
  "Bolzano" = "ITH1", "Trento" = "ITH2", "Veneto" = "ITH3",
  "Friuli-Venezia Giulia" = "ITH4", "Emilia-Romagna" = "ITH5",
  "Toscana" = "ITI1", "Umbria" = "ITI2", "Marche" = "ITI3", "Lazio" = "ITI4",
  "Abruzzo" = "ITF1", "Molise" = "ITF2", "Campania" = "ITF3", "Puglia" = "ITF4",
  "Basilicata" = "ITF5", "Calabria" = "ITF6", "Sicilia" = "ITG1", "Sardegna" = "ITG2")

regioni <- regioni %>% mutate(NUTS_ID = nuts_map[territorio])

bin_levels <- c("Meno di -15", "Da -15 a -10", "Da -10 a -5",
                "Da -5 a 0", "Da 0 a +5", "Più di +5")
bin_colours <- c(
  "Meno di -15"  = "#7A1220",
  "Da -15 a -10" = "#F12938",
  "Da -10 a -5"  = "#F79AA2",
  "Da -5 a 0"    = "#FDDCE0",
  "Da 0 a +5"    = "#A1C6EE",
  "Più di +5"    = "#0478EA")
bin_scuri <- c("Meno di -15", "Da -15 a -10", "Più di +5")

regioni <- regioni %>%
  mutate(bin = factor(case_when(
    saldo_cum < -15 ~ "Meno di -15",
    saldo_cum < -10 ~ "Da -15 a -10",
    saldo_cum <  -5 ~ "Da -10 a -5",
    saldo_cum <   0 ~ "Da -5 a 0",
    saldo_cum <   5 ~ "Da 0 a +5",
    TRUE            ~ "Più di +5"), levels = bin_levels))

geo_it <- load_geo_italia_regioni() %>% left_join(regioni, by = "NUTS_ID")

fmt_saldo <- function(v) paste0(ifelse(v > 0, "+", ""),
                                formatC(v, format = "f", digits = 1, decimal.mark = ","))

punti <- sf::st_point_on_surface(geo_it)
coords <- sf::st_coordinates(punti)
labels_map <- sf::st_drop_geometry(geo_it) %>%
  mutate(x = coords[, "X"], y = coords[, "Y"],
         label_value = fmt_saldo(saldo_cum),
         # Liguria: arco troppo stretto, etichetta spostata nel mare in nero
         label_color = case_when(territorio == "Liguria" ~ "#1C1C1C",
                                 bin %in% bin_scuri ~ "white",
                                 TRUE ~ "#1C1C1C"),
         x = case_when(territorio == "Liguria" ~ x - 20000, TRUE ~ x),
         y = case_when(territorio == "Liguria" ~ y - 62000,
                       territorio == "Calabria" ~ y + 10000, TRUE ~ y))

# Ritaglio: la mappa si ferma poco sotto la costa sud della Sicilia
# (le isole minori a sud, Pelagie e Pantelleria, restano fuori)
sic_poly <- geo_it %>% filter(territorio == "Sicilia") %>%
  sf::st_geometry() %>% sf::st_cast("POLYGON")
sic_main <- sic_poly[which.max(sf::st_area(sic_poly))]
bb_it  <- sf::st_bbox(geo_it)
y_min  <- as.numeric(sf::st_bbox(sic_main)["ymin"]) - 15000

p3 <- ggplot(geo_it) +
  geom_sf(aes(fill = bin), color = "white", linewidth = 0.3) +
  geom_text(data = labels_map,
            aes(x = x, y = y, label = label_value, color = label_color),
            family = "Source Sans Pro", fontface = "bold", size = 3.4) +
  scale_color_identity() +
  scale_fill_manual(values = bin_colours, drop = FALSE, name = NULL,
                    breaks = bin_levels) +
  guides(fill = guide_legend(
    reverse = TRUE,
    keyheight = unit(0.7, "cm"), keywidth = unit(0.45, "cm"),
    label.theme = element_text(family = "Source Sans Pro", size = 9.5,
                               color = "#1C1C1C", hjust = 0))) +
  coord_sf(xlim = c(as.numeric(bb_it["xmin"]) - 5000, as.numeric(bb_it["xmax"]) + 5000),
           ylim = c(y_min, as.numeric(bb_it["ymax"]) + 5000), expand = FALSE) +
  theme_map() +
  theme(legend.position = c(0.99, 0.90),
        legend.justification = c(1, 1)) +
  labs(title = "Dove il tessuto d'impresa si è ristretto",
       subtitle = "Somma dei saldi annui (natalità meno mortalità) delle imprese in punti percentuali, 2004-2024.\nIn rosso i territori dove il tessuto si è contratto, in blu dove è cresciuto",
       caption = CAP_ISTAT)

ggsave(file.path(OUTPUT, "03_mappa_saldo_cumulato.png"), p3,
       width = 8, height = 8.2, units = "in", dpi = 220, bg = "white")

regioni %>% select(territorio, saldo_cum) %>% arrange(saldo_cum) %>%
  write_csv(file.path(OUTPUT, "03_saldo_cumulato_regioni.csv"))

# ============================================================================
# 04 — Sopravvivenza: due versioni a curve (stile Tavola 5)
# ============================================================================

sopr <- read_csv(file.path(INPUT, "sopravvivenza_coorti.csv"), show_col_types = FALSE)

aggiungi_anno_zero <- function(df, chiavi) {
  bind_rows(df,
            df %>% distinct(across(all_of(chiavi))) %>%
              mutate(anni_dalla_nascita = 0, tasso_sopravvivenza = 100)) %>%
    arrange(across(all_of(chiavi)), anni_dalla_nascita)
}

tema_x_anni <- theme(
  axis.title.x = element_text(size = 10, color = "#1C1C1C", hjust = 0.5,
                              margin = margin(t = 0.3, unit = "cm")))

# --- 04a: coorte 2019, curve per macrosettore -------------------------------

c19 <- sopr %>% filter(coorte == 2019) %>%
  select(macrosettore, anni_dalla_nascita, tasso_sopravvivenza) %>%
  aggiungi_anno_zero("macrosettore")

col_settori <- c("Totale" = COL_NERO, "Industria in senso stretto" = COL_BLU,
                 "Costruzioni" = COL_GIALLO, "Commercio" = COL_ROSSO,
                 "Altri servizi" = COL_VIOLA)

lab_19 <- tibble(
  macrosettore = c("Industria in senso stretto", "Costruzioni", "Altri servizi",
                   "Totale", "Commercio"),
  nome = c("Industria 55%", "Costruzioni 52%", "Altri servizi 50%",
           "Totale 49%", "Commercio 46%"),
  y = c(55.9, 52.3, 50.2, 48.3, 46.0))

p4a <- ggplot(c19, aes(anni_dalla_nascita, tasso_sopravvivenza,
                       color = macrosettore, group = macrosettore)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.7) +
  geom_text(data = lab_19,
            aes(x = 5.15, y = y, label = nome, color = macrosettore),
            hjust = 0, family = "Source Sans Pro", fontface = "bold",
            size = 3.4, inherit.aes = FALSE) +
  scale_color_manual(values = col_settori) +
  scale_x_continuous(limits = c(0, 6.5), breaks = 0:5, expand = c(0.01, 0)) +
  scale_y_continuous(limits = c(43, 101), breaks = seq(50, 100, 10),
                     labels = function(x) paste0(x, "%"), expand = c(0.01, 0)) +
  theme_linechart() +
  theme(legend.position = "none") + tema_x_anni +
  labs(title = "Metà delle nuove imprese non arriva a cinque anni",
       subtitle = "Quota di imprese nate nel 2019 ancora attive, per macrosettore, Italia. Il 2024 è provvisorio",
       caption = CAP_ISTAT, x = "Anni dalla nascita")

ggsave(file.path(OUTPUT, "04a_sopravvivenza_coorte2019.png"), p4a,
       width = 8, height = 6.5, units = "in", dpi = 220, bg = "white")

# --- 04b: totale economia, curve per coorte ---------------------------------

ctot <- sopr %>% filter(macrosettore == "Totale", coorte %in% c(2004, 2009, 2014, 2019)) %>%
  select(coorte, anni_dalla_nascita, tasso_sopravvivenza) %>%
  aggiungi_anno_zero("coorte") %>%
  mutate(coorte = factor(coorte))

col_coorti <- c("2004" = "#9CC3EF", "2009" = "#5E9FE0",
                "2014" = "#2478CC", "2019" = "#0450A0")

lab_coorti <- tibble(
  coorte = factor(c(2004, 2019, 2009, 2014)),
  nome = c("Nate nel 2004 · 50%", "Nate nel 2019 · 49%",
           "Nate nel 2009 · 45%", "Nate nel 2014 · 44%"),
  y = c(51.6, 48.9, 45.4, 43.3))

p4b <- ggplot(ctot, aes(anni_dalla_nascita, tasso_sopravvivenza,
                        color = coorte, group = coorte)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.7) +
  geom_text(data = lab_coorti,
            aes(x = 5.15, y = y, label = nome, color = coorte),
            hjust = 0, family = "Source Sans Pro", fontface = "bold",
            size = 3.4, inherit.aes = FALSE) +
  scale_color_manual(values = col_coorti) +
  scale_x_continuous(limits = c(0, 7), breaks = 0:5, expand = c(0.01, 0)) +
  scale_y_continuous(limits = c(41, 101), breaks = seq(50, 100, 10),
                     labels = function(x) paste0(x, "%"), expand = c(0.01, 0)) +
  theme_linechart() +
  theme(legend.position = "none") + tema_x_anni +
  labs(title = "Le imprese nate nella crisi sono durate di meno",
       subtitle = "Quota di imprese ancora attive negli anni successivi alla nascita, per anno di nascita, Italia",
       caption = CAP_ISTAT, x = "Anni dalla nascita")

ggsave(file.path(OUTPUT, "04b_sopravvivenza_coorti_totale.png"), p4b,
       width = 8, height = 6.5, units = "in", dpi = 220, bg = "white")

sopr %>% filter(coorte %in% c(2004, 2009, 2014, 2019)) %>%
  select(macrosettore, coorte, anni_dalla_nascita, tasso_sopravvivenza) %>%
  write_csv(file.path(OUTPUT, "04_sopravvivenza_coorti.csv"))

cat("Fatto: PNG in output/\n")
