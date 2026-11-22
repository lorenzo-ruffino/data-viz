library(eurostat)
library(tidyverse)
library(showtext)

# Replica per l'Italia del grafico di Jeremy Horpedahl sulla ricchezza netta
# mediana per generazione, a parità di età del capofamiglia.
# Fonte: Banca d'Italia, Indagine sui bilanci delle famiglie italiane,
# archivio storico (versione 12.2), ondate 1991-2022.

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

CAP_BDI <- "Elaborazione di Lorenzo Ruffino su dati Banca d'Italia"

# --- Parametri --------------------------------------------------------------

DIR_IN     <- "../input/storico/CSV"
ANNO_BASE  <- 2025      # euro di arrivo
ETA_MIN    <- 30        # sotto i 30 anni i capifamiglia sono pochi e selezionati
N_MIN      <- 30        # famiglie minime per singolo anno di età
CF_VAR     <- Sys.getenv("CF_VAR", "CFRED")   # capofamiglia = principale percettore di reddito

generazioni <- tribble(
  ~generazione,   ~da,  ~a,
  "Silenziosa",   1928, 1945,
  "Baby boomer",  1946, 1964,
  "Generazione X", 1965, 1980,
  "Millennial",   1981, 1996,
  "Generazione Z", 1997, 2012
)

palette_gen <- c(
  "Silenziosa"    = COL_BLU,
  "Baby boomer"   = COL_VERDE,
  "Generazione X" = COL_GIALLO,
  "Millennial"    = COL_VIOLA,
  "Generazione Z" = COL_ROSSO
)

# --- Dati -------------------------------------------------------------------

ricf <- read_csv(file.path(DIR_IN, "ricf.csv"), show_col_types = FALSE,
                 col_select = c(NQUEST, ANNO, W, AR, AF, PF))
comp <- read_csv(file.path(DIR_IN, "comp.csv"), show_col_types = FALSE,
                 col_select = c(NQUEST, ANNO, ETA, ANASC, all_of(CF_VAR)))
peso <- read_csv(file.path(DIR_IN, "peso.csv"), show_col_types = FALSE)
defl <- read_csv(file.path(DIR_IN, "defl.csv"), show_col_types = FALSE) |>
  rename(ANNO = Anno)

# RIVAL porta gli euro di ogni anno a euro 2022; da lì al 2025 con l'IPCA
ipca <- get_eurostat("prc_hicp_aind", time_format = "num",
                     filters = list(geo = "IT", coicop = "CP00", unit = "INX_A_AVG"))
write_csv(ipca, "../input/ipca_italia.csv")
ipca_val <- function(y) ipca$values[ipca$time == y]
coef_22_25 <- ipca_val(ANNO_BASE) / ipca_val(2022)

capi <- comp |>
  filter(.data[[CF_VAR]] == 1) |>
  mutate(anno_nascita = coalesce(ANASC, ANNO - ETA)) |>
  select(NQUEST, ANNO, eta = ETA, anno_nascita)

famiglie <- ricf |>
  filter(!is.na(W)) |>
  inner_join(capi, by = c("NQUEST", "ANNO")) |>
  inner_join(peso, by = c("NQUEST", "ANNO")) |>
  inner_join(defl, by = "ANNO") |>
  mutate(ricchezza = W * RIVAL * coef_22_25) |>
  cross_join(generazioni) |>
  filter(anno_nascita >= da, anno_nascita <= a) |>
  select(-da, -a)

# Restano fuori solo i capifamiglia nati prima del 1928 (Generazione più grande)
n_con_w <- ricf |> filter(!is.na(W)) |> semi_join(capi, by = c("NQUEST", "ANNO")) |> nrow()
n_prima_1928 <- ricf |> filter(!is.na(W)) |>
  inner_join(capi, by = c("NQUEST", "ANNO")) |> filter(anno_nascita < 1928) |> nrow()
stopifnot(nrow(famiglie) + n_prima_1928 == n_con_w)

# --- Mediane per età e generazione -------------------------------------------

mediana_pesata <- function(x, w) {
  o <- order(x); x <- x[o]; w <- w[o]
  cw <- cumsum(w) / sum(w)
  x[which(cw >= 0.5)[1]]
}

mediane <- famiglie |>
  group_by(generazione, eta) |>
  summarise(n = n(), mediana = mediana_pesata(ricchezza, PESO),
            onde = n_distinct(ANNO), .groups = "drop") |>
  filter(n >= N_MIN)

# Media mobile centrata su 5 anni di età, come nell'originale: solo dove ci
# sono tutti e cinque gli anni
serie <- mediane |>
  group_by(generazione) |>
  complete(eta = full_seq(eta, 1)) |>
  arrange(eta, .by_group = TRUE) |>
  mutate(valore = (lag(mediana, 2) + lag(mediana, 1) + mediana +
                   lead(mediana, 1) + lead(mediana, 2)) / 5,
         n5 = lag(n, 2) + lag(n, 1) + n + lead(n, 1) + lead(n, 2)) |>
  ungroup() |>
  filter(!is.na(valore), eta >= ETA_MIN) |>
  mutate(generazione = factor(generazione, levels = generazioni$generazione))

write_csv(serie |> select(generazione, eta, ricchezza_mediana = valore,
                          famiglie_finestra = n5),
          "../output/ricchezza_per_generazione.csv")

# --- Grafico ----------------------------------------------------------------

etichette <- serie |>
  group_by(generazione) |>
  slice_max(eta, n = 1) |>
  ungroup()

fmt_euro <- function(x) paste0("€ ", x / 1000, "k")

p <- ggplot(serie, aes(eta, valore, colour = generazione)) +
  geom_line(linewidth = 0.9) +
  geom_text(data = etichette, aes(label = generazione),
            hjust = 0, nudge_x = 0.6, size = 3.4, fontface = "bold",
            family = "Source Sans Pro") +
  scale_colour_manual(values = palette_gen) +
  scale_x_continuous(breaks = seq(30, 90, 5), limits = c(ETA_MIN, max(serie$eta) + 9),
                     expand = c(0.01, 0)) +
  scale_y_continuous(labels = fmt_euro, limits = c(0, 260000),
                     breaks = seq(50000, 250000, 50000),
                     expand = c(0.01, 0.01)) +
  coord_cartesian(clip = "off") +
  theme_linechart() + theme(legend.position = "none") +
  labs(title = "Dopo i baby boomer ogni generazione è più povera",
       subtitle = "Ricchezza netta mediana delle famiglie per età del capofamiglia e generazione, a prezzi 2025, Italia, 1991-2022",
       caption = CAP_BDI)

ggsave("../output/ricchezza_per_generazione.png", p,
       width = 9, height = 6.5, dpi = 220, bg = "white")

# --- Controlli --------------------------------------------------------------

cat("\nCoefficiente 2022 -> ", ANNO_BASE, ": ", round(coef_22_25, 4), "\n", sep = "")
cat("Ondate con ricchezza:", paste(sort(unique(famiglie$ANNO)), collapse = ", "), "\n")
serie |>
  filter(eta %in% c(25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80)) |>
  mutate(valore = round(valore / 1000)) |>
  select(generazione, eta, valore) |>
  pivot_wider(names_from = generazione, values_from = valore) |>
  print(n = 30)
serie |> group_by(generazione) |>
  summarise(eta_min = min(eta), eta_max = max(eta), n_min_finestra = min(n5)) |>
  print()
