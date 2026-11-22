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
COL_VIOLA  <- "#A82DE3"
COL_ROSSO  <- "#F12938"
COL_GIALLO <- "#F2A900"
COL_GRIGIO <- "#9A9A9A"
COL_VERDE  <- "#1B9E77"

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

OUT <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Come funzionano i prezzi/output"
INP <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Come funzionano i prezzi/input"

# =============================================================================
# 01 - La conoscenza dispersa che confluisce in un prezzo (illustrazione)
# =============================================================================

set.seed(21)
pal <- c(COL_ROSSO, COL_BLU, COL_VIOLA, COL_GIALLO, COL_VERDE)

# Il prezzo: un filo unico tra NX1 e NX2
NX1 <- 5.35; NX2 <- 7.05; CX <- (NX1 + NX2) / 2; CY <- 0

# A sinistra: informazioni sparse, in disordine
N_L <- 230
sx <- tibble(
  id   = 1:N_L,
  x    = runif(N_L, 0, 3.7),
  y    = runif(N_L, -4.4, 4.4),
  col  = sample(pal, N_L, replace = TRUE),
  size = rexp(N_L, 1 / 1.6) + 0.9
) %>% mutate(size = pmin(size, 6.5))

# A destra: decisioni, in ordine
dx <- expand_grid(cx = seq(8.7, 12.4, length.out = 8),
                  cy = seq(-4.4, 4.4, length.out = 15)) %>%
  mutate(id = row_number(), x = cx, y = cy,
         col = sample(pal, n(), replace = TRUE))

# Curve che si stringono nel filo e poi si riaprono
curva <- function(x0, y0, x1, y1, n = 80) {
  t <- seq(0, 1, length.out = n)
  s <- t^2 * (3 - 2 * t)
  tibble(x = x0 + (x1 - x0) * t, y = y0 + (y1 - y0) * s)
}
curve_sx <- sx %>%
  mutate(p = pmap(list(x, y), ~ curva(..1, ..2, NX1, CY))) %>%
  select(id, col, p) %>% unnest(p)
curve_dx <- dx %>%
  mutate(p = pmap(list(x, y), ~ curva(NX2, CY, ..1, ..2))) %>%
  select(id, col, p) %>% unnest(p)

# Esempi: quattro informazioni e quattro decisioni
es_sx <- tibble(
  y     = c(3.3, 1.1, -1.1, -3.3),
  x     = c(0.9, 0.6, 0.8, 1.1),
  testo = c("Un raccolto\nandato male", "Una fabbrica\nche si ferma",
            "Un camion\nche torna vuoto", "Un cliente\nche cambia gusti"),
  col   = c(COL_ROSSO, COL_GIALLO, COL_BLU, COL_VIOLA)
)
es_dx <- tibble(
  y     = sort(unique(dx$cy))[c(13, 10, 6, 3)],
  x     = 12.4,
  testo = c("Chi compra\nne usa meno", "Chi produce\nne fa di più",
            "Qualcuno cerca\nun sostituto", "Qualcuno importa\nda più lontano"),
  col   = c(COL_ROSSO, COL_GIALLO, COL_BLU, COL_VIOLA)
)
curve_es_sx <- es_sx %>% mutate(id = row_number(),
                                p = pmap(list(x, y), ~ curva(..1, ..2, NX1, CY))) %>%
  select(id, col, p) %>% unnest(p)
curve_es_dx <- es_dx %>% mutate(id = row_number(),
                                p = pmap(list(x, y), ~ curva(NX2, CY, ..1, ..2))) %>%
  select(id, col, p) %>% unnest(p)

fig_conoscenza <- function(scuro = FALSE) {
  bg   <- if (scuro) "#101010" else "white"
  ink  <- if (scuro) "white"   else COL_NERO
  tenue <- if (scuro) "#B8B8B8" else "#5A5A5A"
  a_ln <- if (scuro) 0.45 else 0.32

  ggplot() +
    geom_path(data = curve_sx, aes(x, y, group = id, colour = col),
              linewidth = 0.28, alpha = a_ln) +
    geom_path(data = curve_dx, aes(x, y, group = id, colour = col),
              linewidth = 0.28, alpha = a_ln) +
    geom_path(data = curve_es_sx, aes(x, y, group = id, colour = col),
              linewidth = 1.15, alpha = 0.95, lineend = "round") +
    geom_path(data = curve_es_dx, aes(x, y, group = id, colour = col),
              linewidth = 1.15, alpha = 0.95, lineend = "round") +
    geom_point(data = sx, aes(x, y, colour = col, size = size), alpha = 0.88) +
    geom_point(data = dx, aes(x, y, colour = col), size = 2.5, alpha = 0.92) +
    # esempi evidenziati
    geom_point(data = es_sx, aes(x, y, colour = col), size = 7) +
    geom_point(data = es_sx, aes(x, y), colour = bg, size = 2.6) +
    geom_point(data = es_dx, aes(x, y, colour = col), size = 7) +
    geom_point(data = es_dx, aes(x, y), colour = bg, size = 2.6) +
    geom_text(data = es_sx, aes(x = -0.45, y = y, label = testo, colour = col),
              hjust = 1, family = "Source Sans Pro", fontface = "bold",
              size = 4.1, lineheight = 1.12) +
    geom_text(data = es_dx, aes(x = 12.85, y = y, label = testo, colour = col),
              hjust = 0, family = "Source Sans Pro", fontface = "bold",
              size = 4.1, lineheight = 1.12) +
    # il prezzo: un filo unico con il cartellino
    annotate("segment", x = NX1, xend = NX2, y = CY, yend = CY,
             colour = ink, linewidth = 2.6, lineend = "round") +
    annotate("label", x = CX, y = CY, label = "€ 2,40", colour = ink, fill = bg,
             family = "Source Sans Pro", fontface = "bold", size = 5.6,
             label.r = unit(0.55, "lines"), label.padding = unit(0.42, "lines"),
             linewidth = 0.9) +
    annotate("text", x = CX, y = -0.78, label = "un solo numero", colour = tenue,
             family = "Source Sans Pro", size = 3.4) +
    # intestazioni delle tre parti
    annotate("text", x = 1.85, y = 5.35, label = "Conoscenza dispersa", colour = ink,
             family = "Source Sans Pro", fontface = "bold", size = 4.8) +
    annotate("text", x = CX, y = 5.35, label = "Il prezzo", colour = ink,
             family = "Source Sans Pro", fontface = "bold", size = 4.8) +
    annotate("text", x = 10.55, y = 5.35, label = "Decisioni coordinate", colour = ink,
             family = "Source Sans Pro", fontface = "bold", size = 4.8) +
    scale_colour_identity() +
    scale_size_identity() +
    scale_x_continuous(limits = c(-3.7, 16.3), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-4.9, 5.8), expand = c(0, 0)) +
    coord_cartesian(clip = "off") +
    labs(
      title = "Un prezzo riassume quello che sanno milioni di persone",
      subtitle = "Le informazioni sparse tra chi compra e chi vende confluiscono in un solo numero, che orienta le decisioni di tutti",
      caption = "Illustrazione di Lorenzo Ruffino"
    ) +
    theme_linechart() +
    theme(
      axis.line = element_blank(),
      axis.text = element_blank(),
      legend.position = "none",
      plot.title = element_text(family = "Source Sans Pro SemiBold", size = 14,
                                color = ink, hjust = 0,
                                margin = margin(b = 0.1, unit = "cm")),
      plot.subtitle = element_text(size = 9, color = ink, hjust = 0, lineheight = 1.35,
                                   margin = margin(b = 0.25, t = 0.1, unit = "cm")),
      plot.caption = element_text(size = 9, color = ink, hjust = 1,
                                  margin = margin(t = 0.3, unit = "cm")),
      plot.background = element_rect(fill = bg, colour = NA)
    )
}

ggsave(file.path(OUT, "01_conoscenza_dispersa.png"), fig_conoscenza(FALSE),
       width = 11, height = 7.8, units = "in", dpi = 220, bg = "white")

# =============================================================================
# 02 - Prezzo delle uova negli Stati Uniti
# =============================================================================

bls <- fromJSON(file.path(INP, "uova_bls.json"))$Results$series$data[[1]]
uova <- as_tibble(bls) %>%
  filter(str_detect(period, "^M(0[1-9]|1[0-2])$")) %>%
  transmute(data = as.Date(paste0(year, "-", str_sub(period, 2, 3), "-01")),
            prezzo = suppressWarnings(as.numeric(value))) %>%
  filter(!is.na(prezzo), data >= as.Date("2019-01-01")) %>%
  arrange(data)
write_csv(uova, file.path(OUT, "uova_stati_uniti.csv"))

mesi_it <- c("gennaio", "febbraio", "marzo", "aprile", "maggio", "giugno", "luglio",
             "agosto", "settembre", "ottobre", "novembre", "dicembre")
fmt_usd <- function(x) paste0("$ ", formatC(x, format = "f", digits = 2, decimal.mark = ","))

punti <- uova %>%
  filter(data %in% as.Date(c("2023-01-01", "2025-03-01")) | data == max(data)) %>%
  mutate(etichetta = paste0(fmt_usd(prezzo), "\n", mesi_it[month(data)], " ", year(data)),
         hjust = c(0.5, 0.5, 1), vjust = c(-0.35, -0.35, 1.6))

# Che cosa è successo: fasce per le due ondate di influenza aviaria e note
fasce <- tibble(
  da = as.Date(c("2022-02-01", "2024-10-01")),
  a  = as.Date(c("2023-01-01", "2025-03-01"))
)
note <- tribble(
  ~x,           ~y,   ~hjust, ~testo,
  "2020-04-01", 2.75, 0.5,    "Corsa alle scorte\nnel primo lockdown",
  "2021-12-15", 5.55, 1,      "Prima ondata di influenza aviaria:\noltre 43 milioni di galline\nperse nel 2022",
  "2024-07-20", 5.95, 1,      "Seconda ondata:\noltre 30 milioni\ndi galline abbattute\nin due mesi",
  "2023-08-15", 1.45, 0.5,    "Gli allevamenti\nvengono ripopolati",
  "2025-11-01", 4.35, 0,      "Gli allevamenti\nvengono ripopolati\ndi nuovo"
) %>% mutate(x = as.Date(x))
frecce <- tribble(
  ~x,           ~y,   ~xend,        ~yend,
  "2020-04-01", 2.45, "2020-04-01", 2.12,
  "2022-01-01", 5.35, "2022-07-15", 4.55,
  "2024-08-01", 5.85, "2024-11-25", 5.45,
  "2023-08-15", 1.68, "2023-08-15", 1.98,
  "2025-10-20", 4.20, "2025-08-10", 3.72
) %>% mutate(x = as.Date(x), xend = as.Date(xend))

p2 <- ggplot(uova, aes(data, prezzo)) +
  geom_rect(data = fasce, aes(xmin = da, xmax = a, ymin = 0, ymax = 7.4),
            inherit.aes = FALSE, fill = COL_ROSSO, alpha = 0.07) +
  geom_line(colour = COL_ROSSO, linewidth = 1.0) +
  geom_segment(data = frecce, aes(x = x, y = y, xend = xend, yend = yend),
               inherit.aes = FALSE, colour = "#5A5A5A", linewidth = 0.3) +
  geom_text(data = note, aes(x = x, y = y, label = testo, hjust = hjust),
            inherit.aes = FALSE, family = "Source Sans Pro", size = 3.1,
            colour = "#5A5A5A", lineheight = 0.95) +
  geom_point(data = punti, colour = COL_ROSSO, size = 2.6) +
  geom_text(data = punti, aes(label = etichetta, hjust = hjust, vjust = vjust),
            family = "Source Sans Pro", fontface = "bold", size = 3.6,
            colour = COL_NERO, lineheight = 1.05) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               limits = as.Date(c("2019-01-01", "2027-03-01")), expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(0, 7.4), breaks = 1:7, expand = c(0, 0),
                     labels = function(x) paste0("$ ", x)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Negli Stati Uniti il prezzo delle uova è sceso di oltre il 60%",
    subtitle = "Prezzo medio al dettaglio di dodici uova, gennaio 2019-ottobre 2026. Le fasce indicano le ondate di influenza aviaria",
    caption = "Elaborazione di Lorenzo Ruffino su dati Bureau of Labor Statistics"
  ) +
  theme_linechart() +
  theme(legend.position = "none")

ggsave(file.path(OUT, "02_uova_stati_uniti.png"), p2,
       width = 10, height = 6.8, units = "in", dpi = 220, bg = "white")

# =============================================================================
# 03 - Distributori di benzina con il prezzo bloccato, 1973-74
# =============================================================================

benzina <- tibble(
  stato  = c("Rifornimenti regolari", "Rifornimenti limitati", "Senza benzina"),
  quota  = c(62.3, 27.6, 10.1),
  colore = c("#C9CED6", COL_GIALLO, COL_ROSSO),
  ordine = 1:3
) %>% mutate(celle = round(quota))
stopifnot(sum(benzina$celle) == 100)
write_csv(benzina, file.path(OUT, "benzina_1973.csv"))

griglia <- expand_grid(riga = 1:10, col = 1:10) %>%
  arrange(riga, col) %>%
  mutate(stato = rep(benzina$stato, benzina$celle)) %>%
  left_join(benzina, by = "stato")

etichette <- griglia %>%
  group_by(stato, colore, celle) %>%
  summarise(y = mean(riga), .groups = "drop") %>%
  mutate(colore_testo = if_else(stato == "Rifornimenti regolari", "#6B7280", colore),
         y = if_else(stato == "Rifornimenti limitati", y + 0.2, y))

p3 <- ggplot(griglia) +
  geom_tile(aes(col, -riga, fill = colore), width = 0.88, height = 0.88) +
  geom_text(data = etichette, aes(x = 11, y = -y + 0.22, label = paste0(celle, "%"),
                                  colour = colore_testo),
            hjust = 0, family = "Source Sans Pro", fontface = "bold", size = 9) +
  geom_text(data = etichette, aes(x = 11, y = -y - 0.42, label = stato,
                                  colour = colore_testo),
            hjust = 0, family = "Source Sans Pro", fontface = "bold", size = 3.9) +
  scale_fill_identity() +
  scale_colour_identity() +
  scale_x_continuous(limits = c(0.4, 15.6), expand = c(0, 0)) +
  coord_equal(clip = "off") +
  labs(
    title = "Con il prezzo bloccato un distributore su dieci era a secco",
    subtitle = "Situazione di cento distributori di benzina durante il controllo dei prezzi, Stati Uniti, 1973-1974",
    caption = "Elaborazione di Lorenzo Ruffino su dati Albrecht, Tabarrok e Whitmeyer (2026)"
  ) +
  theme_linechart() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
                  legend.position = "none")

ggsave(file.path(OUT, "03_benzina_1973.png"), p3,
       width = 9, height = 7, units = "in", dpi = 220, bg = "white")

# --- Controllo finale -------------------------------------------------------
cat("Uova: da", format(min(uova$data)), "a", format(max(uova$data)), "-", nrow(uova), "mesi\n")
cat("Picco:", max(uova$prezzo), "il", format(uova$data[which.max(uova$prezzo)]),
    "| ultimo:", tail(uova$prezzo, 1),
    "| calo dal picco:", round((tail(uova$prezzo, 1) / max(uova$prezzo) - 1) * 100, 1), "%\n")
print(punti)
cat("Benzina celle:", benzina$celle, "\n")
