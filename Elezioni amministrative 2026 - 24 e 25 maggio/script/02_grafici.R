library(tidyverse)
library(sf)
library(showtext)

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

PROJ_DIR <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Elezioni amministrative 2026 - 24 e 25 maggio"
INPUT_DIR <- file.path(PROJ_DIR, "input")
OUTPUT_DIR <- file.path(PROJ_DIR, "output")
COM_JSON <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Variazione popolazione 2025-2020/Input/Com01012025_g_WGS84.json"
REG_JSON <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Densità Popolazione Italia/Reg01012025_g_WGS84.json"

# ---- DATI -----------------------------------------------------------------

voto <- read_csv(file.path(INPUT_DIR, "comuni_al_voto_2026.csv"),
                 show_col_types = FALSE) |>
  filter(data_elezione == "24-25 maggio 2026") |>
  mutate(codice_istat6 = sprintf("%06d", as.integer(codice_istat)))

cat("Comuni al voto 24-25 maggio:", nrow(voto), "\n")

# ---- CATEGORIE E COLORI ---------------------------------------------------

SPACER <- " "                                  # riga vuota nella legenda
COMUNI_MINORI <- "Comuni minori\n(sotto i 15.000)"

cat_levels <- c(
  "Centrosinistra",
  "Civico centrosinistra",
  "Movimento 5 Stelle",
  "Altro",
  "Civico centrodestra",
  "Centrodestra",
  SPACER,
  COMUNI_MINORI
)
cat_colours <- c(
  "Centrosinistra"            = "#F12938",  # rosso
  "Civico centrosinistra"     = "#F5A6B5",  # rosa
  "Movimento 5 Stelle"        = "#F2A900",  # giallo
  "Altro"                     = "#1B9E77",  # verde
  "Civico centrodestra"       = "#5C9CDE",  # azzurro
  "Centrodestra"              = "#0E5BAD",  # blu
  "#FFFFFF",                                # spacer invisibile
  "#4D4D4D"                                 # comuni minori, grigio scuro
)
names(cat_colours)[7:8] <- c(SPACER, COMUNI_MINORI)

voto <- voto |>
  mutate(cat = case_when(
    tipologia == "INF"                     ~ COMUNI_MINORI,
    coalizione == "centrosinistra"         ~ "Centrosinistra",
    coalizione == "civico_centrosinistra"  ~ "Civico centrosinistra",
    coalizione %in% c("M5S","M5S_CSX")     ~ "Movimento 5 Stelle",
    coalizione == "civico_altro"           ~ "Altro",
    coalizione == "civico_centrodestra"    ~ "Civico centrodestra",
    coalizione == "centrodestra"           ~ "Centrodestra",
    TRUE                                   ~ NA_character_
  ),
  cat = factor(cat, levels = cat_levels))

stopifnot(all(!is.na(voto$cat)))

# ---- GEOMETRIE -------------------------------------------------------------

cat("Carico geometrie comuni...\n")
geo_com <- st_read(COM_JSON, quiet = TRUE) |>
  st_transform(32632) |>
  st_make_valid()

cat("Carico geometrie regioni...\n")
geo_reg <- st_read(REG_JSON, quiet = TRUE) |>
  st_transform(32632) |>
  st_make_valid()

# Dissolvi i comuni per provincia (COD_UTS) per ottenere i bordi delle province
cat("Dissolvo i comuni per provincia...\n")
geo_prov_path <- file.path(OUTPUT_DIR, "geo_province_2025.rds")
if (file.exists(geo_prov_path)) {
  geo_prov <- readRDS(geo_prov_path)
} else {
  geo_prov <- geo_com |>
    group_by(COD_UTS) |>
    summarise(geometry = st_union(geometry), .groups = "drop") |>
    st_make_valid()
  saveRDS(geo_prov, geo_prov_path)
}

# ---- MAPPA -----------------------------------------------------------------

geo_voto <- geo_com |>
  inner_join(voto |> select(codice_istat6, cat),
             by = c("PRO_COM_T" = "codice_istat6"))

cat("Comuni geomatch:", nrow(geo_voto), "/ richiesti:", nrow(voto), "\n")

# Sfondo: tutti i comuni in grigio chiarissimo
mappa <- ggplot() +
  geom_sf(data = geo_com, fill = "#F7F7F7",
          color = NA) +
  geom_sf(data = geo_voto, aes(fill = cat),
          color = "#FFFFFF", linewidth = 0.04) +
  geom_sf(data = geo_prov, fill = NA,
          color = "#B5B5B5", linewidth = 0.12) +
  geom_sf(data = geo_reg, fill = NA,
          color = "#3A4448", linewidth = 0.3) +
  scale_fill_manual(values = cat_colours, drop = FALSE, name = NULL,
                    breaks = cat_levels) +
  guides(fill = guide_legend(
    ncol = 1, byrow = FALSE,
    override.aes = list(color = NA),
    keyheight = unit(0.5, "cm"),
    keywidth = unit(0.5, "cm")
  )) +
  coord_sf(datum = NA, expand = FALSE) +
  labs(
    title = "I comuni al voto del 24 e 25 maggio",
    subtitle = "I 118 comuni superiori (oltre 15.000 abitanti) sono colorati per coalizione del sindaco uscente.",
    caption = "Elaborazione di Lorenzo Ruffino su dati Ministero dell'Interno e Istat"
  ) +
  theme_minimal(base_family = "Source Sans Pro") +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.position = c(0.86, 0.78),
    legend.justification = c(0.5, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 10, color = "#1C1C1C", hjust = 0,
                               lineheight = 1.0),
    plot.title.position = "plot",
    plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), "cm"),
    plot.title = element_text(size = 16, color = "#1C1C1C", hjust = 0,
                              margin = margin(b = 0.1, unit = "cm")),
    plot.subtitle = element_text(size = 10, color = "#1C1C1C", hjust = 0,
                                 lineheight = 1.35,
                                 margin = margin(b = 0.3, t = 0.1, unit = "cm")),
    plot.caption = element_text(size = 9, color = "#1C1C1C", hjust = 1,
                                margin = margin(t = 0.4, unit = "cm"))
  )

ggsave(file.path(OUTPUT_DIR, "01_mappa_comuni_al_voto.png"),
       mappa, width = 9, height = 10, dpi = 220, bg = "white")
cat("Salvato:", file.path(OUTPUT_DIR, "01_mappa_comuni_al_voto.png"), "\n")

# ---- MAPPA V2: CERCHI PROPORZIONALI ALLA POPOLAZIONE ----------------------

geo_voto_pt <- geo_com |>
  inner_join(voto |> select(codice_istat6, cat, pop = popolazione_31_12_2021),
             by = c("PRO_COM_T" = "codice_istat6"))
geo_voto_pt <- suppressWarnings(st_centroid(geo_voto_pt)) |>
  arrange(desc(pop))

mappa2 <- ggplot() +
  geom_sf(data = geo_com, fill = "#F7F7F7", color = NA) +
  geom_sf(data = geo_prov, fill = NA, color = "#B5B5B5", linewidth = 0.12) +
  geom_sf(data = geo_reg, fill = NA, color = "#3A4448", linewidth = 0.3) +
  geom_sf(data = geo_voto_pt, aes(size = pop, fill = cat),
          shape = 21, color = "#FFFFFF", stroke = 0.18, alpha = 0.85) +
  scale_fill_manual(values = cat_colours, drop = FALSE, name = NULL,
                    breaks = cat_levels) +
  scale_size_area(max_size = 8.5, breaks = c(25000, 100000, 250000),
                  labels = c("25 mila", "100 mila", "250 mila"), name = NULL) +
  guides(
    fill = guide_legend(
      order = 1, ncol = 1,
      override.aes = list(size = 3.5, color = NA),
      keyheight = unit(0.5, "cm"), keywidth = unit(0.5, "cm")),
    size = guide_legend(
      order = 2,
      override.aes = list(fill = "#9A9A9A", color = "#FFFFFF", stroke = 0.18))
  ) +
  coord_sf(datum = NA, expand = FALSE) +
  labs(
    title = "I comuni al voto del 24 e 25 maggio",
    subtitle = paste0(
      "Ogni cerchio è un comune al voto: la dimensione è proporzionale alla popolazione,\n",
      "il colore indica la coalizione del sindaco uscente."
    ),
    caption = "Elaborazione di Lorenzo Ruffino su dati Ministero dell'Interno e Istat"
  ) +
  theme_minimal(base_family = "Source Sans Pro") +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.position = c(0.86, 0.93),
    legend.justification = c(0.5, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.spacing.y = unit(0.25, "cm"),
    legend.text = element_text(size = 10, color = "#1C1C1C", hjust = 0,
                               lineheight = 1.0),
    plot.title.position = "plot",
    plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), "cm"),
    plot.title = element_text(size = 16, color = "#1C1C1C", hjust = 0,
                              margin = margin(b = 0.1, unit = "cm")),
    plot.subtitle = element_text(size = 10, color = "#1C1C1C", hjust = 0,
                                 lineheight = 1.35,
                                 margin = margin(b = 0.3, t = 0.1, unit = "cm")),
    plot.caption = element_text(size = 9, color = "#1C1C1C", hjust = 1,
                                margin = margin(t = 0.4, unit = "cm"))
  )

ggsave(file.path(OUTPUT_DIR, "01_mappa_comuni_al_voto_v2.png"),
       mappa2, width = 9, height = 10, dpi = 220, bg = "white")
cat("Salvato:", file.path(OUTPUT_DIR, "01_mappa_comuni_al_voto_v2.png"), "\n")

# ---- BAR CHART POPOLAZIONE PER COALIZIONE (SOLO SUP) ----------------------

cat_levels_sup <- setdiff(cat_levels, c(SPACER, COMUNI_MINORI))

bar_data <- voto |>
  filter(tipologia == "SUP") |>
  group_by(cat) |>
  summarise(popolazione = sum(popolazione_31_12_2021, na.rm = TRUE),
            n_comuni    = n(),
            .groups = "drop") |>
  mutate(cat = factor(cat, levels = cat_levels_sup)) |>
  arrange(cat)

fmt_pop <- function(p) {
  ifelse(p >= 1e6,
    {
      v <- round(p / 1e6, 2)
      s <- formatC(v, format = "f", digits = 2, decimal.mark = ",")
      s <- sub(",([0-9])0$", ",\\1", s)
      paste0(s, " milioni")
    },
    paste0(format(round(p / 1000), big.mark = ".", decimal.mark = ","), " mila"))
}

bar_data <- bar_data |>
  mutate(label_pop = vapply(popolazione, fmt_pop, character(1)),
         label_n   = paste0(n_comuni, " comuni"),
         y_pop     = popolazione + max(popolazione) * 0.09,
         y_n       = popolazione + max(popolazione) * 0.035)

tot_pop <- sum(bar_data$popolazione)
tot_n   <- sum(bar_data$n_comuni)

cat("Popolazione totale SUP al voto:", format(tot_pop, big.mark = "."), "\n")
cat("Comuni SUP al voto:", tot_n, "\n")

x_label_breaks <- function(x) {
  case_when(
    x == "Movimento 5 Stelle"    ~ "Movimento 5 Stelle",
    x == "Civico centrosinistra" ~ "Civico\ncentrosinistra",
    x == "Civico centrodestra"   ~ "Civico\ncentrodestra",
    TRUE                         ~ x
  )
}

bar <- ggplot(bar_data, aes(x = cat, y = popolazione, fill = cat)) +
  geom_col(width = 0.7) +
  geom_text(aes(y = y_pop, label = label_pop),
            hjust = 0.5, family = "Source Sans Pro",
            fontface = "bold", size = 4.2, color = "#1C1C1C") +
  geom_text(aes(y = y_n, label = label_n),
            hjust = 0.5, family = "Source Sans Pro",
            size = 3.4, color = "#5A5A5A") +
  scale_fill_manual(values = cat_colours, guide = "none") +
  scale_x_discrete(labels = x_label_breaks) +
  scale_y_continuous(
    limits = c(0, max(bar_data$popolazione) * 1.22),
    expand = c(0, 0)
  ) +
  labs(
    title = "Il centrosinistra amministra più comuni superiori del centrodestra",
    subtitle = "Popolazione residente nei 118 comuni oltre 15.000 abitanti al voto il 24 e 25 maggio per coalizione del sindaco uscente.",
    caption = "Elaborazione di Lorenzo Ruffino su dati Ministero dell'Interno e Istat"
  ) +
  theme_minimal(base_family = "Source Sans Pro") +
  theme(
    axis.line.x = element_line(linewidth = 0.3, color = "#1C1C1C"),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 10, color = "#1C1C1C",
                               lineheight = 1.05, margin = margin(t = 0.2, unit = "cm")),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    plot.title.position = "plot",
    plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), "cm"),
    plot.title = element_text(size = 15, color = "#1C1C1C", hjust = 0,
                              margin = margin(b = 0.1, unit = "cm")),
    plot.subtitle = element_text(size = 10, color = "#1C1C1C", hjust = 0,
                                 lineheight = 1.35,
                                 margin = margin(b = 0.4, t = 0.1, unit = "cm")),
    plot.caption = element_text(size = 9, color = "#1C1C1C", hjust = 1,
                                margin = margin(t = 0.5, unit = "cm"))
  )

ggsave(file.path(OUTPUT_DIR, "02_popolazione_per_coalizione.png"),
       bar, width = 10, height = 6.5, dpi = 220, bg = "white")
cat("Salvato:", file.path(OUTPUT_DIR, "02_popolazione_per_coalizione.png"), "\n")

# ---- EXPORT DATI ----------------------------------------------------------

# Dati della mappa: un comune al voto per riga, con categoria e centroide.
coords_wgs <- st_coordinates(st_transform(geo_voto_pt, 4326))
coords_df <- tibble(codice_istat6 = geo_voto_pt$PRO_COM_T,
                    lon = round(coords_wgs[, "X"], 5),
                    lat = round(coords_wgs[, "Y"], 5))

mappa_dati <- voto |>
  transmute(regione, provincia, comune, codice_istat = codice_istat6,
            popolazione = popolazione_31_12_2021, tipologia, coalizione,
            categoria = ifelse(as.character(cat) == COMUNI_MINORI,
                               "Comuni minori", as.character(cat))) |>
  left_join(coords_df, by = c("codice_istat" = "codice_istat6")) |>
  arrange(desc(popolazione))

write_csv(mappa_dati, file.path(OUTPUT_DIR, "dati_mappa_comuni.csv"))
cat("Salvato:", file.path(OUTPUT_DIR, "dati_mappa_comuni.csv"),
    "(", nrow(mappa_dati), "righe )\n")

# Dati del grafico a barre: una riga per categoria, comuni e popolazione separati.
barre_dati <- bar_data |>
  transmute(categoria = as.character(cat),
            numero_comuni = n_comuni,
            popolazione)

write_csv(barre_dati, file.path(OUTPUT_DIR, "dati_barre_coalizioni.csv"))
cat("Salvato:", file.path(OUTPUT_DIR, "dati_barre_coalizioni.csv"),
    "(", nrow(barre_dati), "righe )\n")
