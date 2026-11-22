# script/nuove_pensioni_tipologia.R — eseguito da `cd script && Rscript nuove_pensioni_tipologia.R`
# Nuove pensioni Inps per tipologia: importo medio, età media alla decorrenza
# e numero di pensioni, gennaio-settembre 2026.
# Fonte: Inps, Osservatorio "Monitoraggio dei flussi di pensionamento".

library(tidyverse)
library(showtext)

source_dir <- ".."
input_dir  <- file.path(source_dir, "input")
output_dir <- file.path(source_dir, "output")

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

CAP_INPS <- "Elaborazione di Lorenzo Ruffino su dati Inps"

# --- 1) Dati ----------------------------------------------------------------
# Il csv dell'osservatorio ha 3 righe di intestazione incrociata e un blocco
# html in un campo quotato: readr lo collassa in una colonna, serve read.csv.
# Righe di dato: 29 campi = categoria, vuoto, 9 periodi x 3 misure.
# Periodi: 2025 (I, II, III, IV, Totale), 2026 (I, II, III, Totale).

raw <- read.csv(file.path(input_dir, "inps_flussi_pensionamento_raw.csv"),
                sep = ";", header = FALSE, col.names = paste0("X", 1:47),
                fill = TRUE, fileEncoding = "latin1", quote = "\"",
                stringsAsFactors = FALSE)

PERIODI <- c("2025_T1", "2025_T2", "2025_T3", "2025_T4", "2025_Totale",
             "2026_T1", "2026_T2", "2026_T3", "2026_Totale")
MISURE  <- c("numero", "importo", "eta")

# Indice di colonna: categoria (1), vuoto (2), poi periodo x misura
col_idx <- function(periodo, misura) {
  2 + (match(periodo, PERIODI) - 1) * 3 + match(misura, MISURE)
}

num_it <- function(x) {
  as.numeric(str_replace(str_replace_all(str_trim(x), "\\.", ""), ",", "."))
}

categorie <- c("Vecchiaia", "Anticipata", "Invalidita'", "Superstiti",
               "Assegni Sociali", "Totale")

dati <- raw %>%
  mutate(categoria = str_trim(X1)) %>%
  filter(categoria %in% categorie) %>%
  transmute(
    categoria,
    numero  = num_it(.[[col_idx("2026_Totale", "numero")]]),
    importo = num_it(.[[col_idx("2026_Totale", "importo")]]),
    eta     = num_it(.[[col_idx("2026_Totale", "eta")]])
  )

# Controllo sugli indici: il totale 2026 (gen-set) dell'osservatorio è 665.689
# e deve coincidere con la somma delle cinque tipologie
totale <- dati %>% filter(categoria == "Totale")
tipi   <- dati %>% filter(categoria != "Totale")
stopifnot(nrow(tipi) == 5)
stopifnot(totale$numero == 665689)
stopifnot(sum(tipi$numero) == totale$numero)

nomi <- c(
  "Vecchiaia"       = "Pensioni di vecchiaia",
  "Anticipata"      = "Pensioni anticipate",
  "Invalidita'"     = "Pensioni di invalidità",
  "Superstiti"      = "Pensioni ai superstiti",
  "Assegni Sociali" = "Assegni sociali"
)

palette_tipi <- c(
  "Pensioni anticipate"    = COL_ROSSO,
  "Pensioni di vecchiaia"  = COL_BLU,
  "Pensioni ai superstiti" = COL_VIOLA,
  "Pensioni di invalidità" = COL_GIALLO,
  "Assegni sociali"        = COL_GRIGIO_SCURO
)

plot_data <- tipi %>%
  mutate(tipo = unname(nomi[categoria])) %>%
  select(tipo, numero, importo, eta)

write_csv(plot_data, file.path(output_dir, "nuove_pensioni_tipologia.csv"))

# --- 2) Grafico -------------------------------------------------------------

fmt_euro <- function(x) paste0("€ ", formatC(round(x), format = "d", big.mark = ".", decimal.mark = ","))
fmt_eta  <- function(x) formatC(x, format = "f", digits = 1, decimal.mark = ",")

# Posizione delle etichette (coordinate dati): a destra del cerchio, tranne
# i superstiti (al bordo destro, vanno sotto) e gli assegni sociali (a
# sinistra, per non finire sotto l'etichetta dei superstiti)
pos <- tribble(
  ~tipo,                    ~lx,   ~ly,  ~hj,
  "Pensioni anticipate",    64.6,  2106, 0,
  "Pensioni di vecchiaia",  70.2,  1400, 0,
  "Pensioni ai superstiti", 76.3,  640,  0.5,
  "Pensioni di invalidità", 57.6,  832,  0,
  "Assegni sociali",        66.4,  507,  1
)

label_data <- plot_data %>%
  left_join(pos, by = "tipo") %>%
  mutate(dettaglio = paste0(round(numero / 1000), " mila pensioni\n",
                            fmt_euro(importo), " al mese\n",
                            "a ", fmt_eta(eta), " anni"))

p <- ggplot(plot_data, aes(x = eta, y = importo)) +
  geom_point(aes(size = numero, color = tipo), shape = 16, alpha = 0.9) +
  geom_text(data = label_data,
            aes(x = lx, y = ly, label = tipo, color = tipo, hjust = hj),
            vjust = -0.35, family = "Source Sans Pro", fontface = "bold",
            size = 3.6) +
  geom_text(data = label_data,
            aes(x = lx, y = ly, label = dettaglio, hjust = hj),
            vjust = 1.08, family = "Source Sans Pro", size = 3.1,
            color = COL_NERO, lineheight = 1.05) +
  scale_size_area(max_size = 34) +
  scale_color_manual(values = palette_tipi) +
  scale_x_continuous(limits = c(53, 81), breaks = seq(55, 80, 5),
                     labels = function(x) paste0(x, " anni"),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 2500), breaks = seq(500, 2500, 500),
                     labels = function(x) paste0("€ ", format(x, big.mark = ".",
                                                               decimal.mark = ",",
                                                               trim = TRUE)),
                     expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Le pensioni anticipate sono le più alte",
    subtitle = paste0(
      "Importo medio mensile lordo ed età media alla decorrenza delle nuove pensioni Inps per tipologia.\n",
      "L'area dei cerchi è proporzionale al numero di pensioni, Italia, gennaio-settembre 2026"),
    x = "Età media alla decorrenza",
    y = "Importo medio mensile lordo",
    caption = CAP_INPS
  ) +
  theme_linechart() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 10, color = "#1C1C1C", hjust = 0.5,
                                margin = margin(t = 0.3, unit = "cm")),
    axis.title.y = element_text(size = 10, color = "#1C1C1C", hjust = 0.5,
                                angle = 90, margin = margin(r = 0.3, unit = "cm"))
  )

ggsave(file.path(output_dir, "nuove_pensioni_tipologia.png"),
       plot = p, width = 8, height = 7.5, units = "in", dpi = 220, bg = "white")

# --- Sanity check -----------------------------------------------------------
cat("\nNuove pensioni gennaio-settembre 2026\n")
print(as.data.frame(plot_data))
cat("Totale:", format(totale$numero, big.mark = ".", decimal.mark = ","),
    "| importo medio:", totale$importo, "| età media:", totale$eta, "\n")
