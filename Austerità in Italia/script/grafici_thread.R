library(tidyverse)
library(showtext)
library(ggrepel)

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

palette_paesi <- c(
  "Italia"     = "#F12938",
  "Francia"    = "#A82DE3",
  "Germania"   = "#1C1C1C",
  "Spagna"     = "#F2A900",
  "Area euro"  = "#0478EA"
)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_ROSSO  <- "#F12938"
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

CAP_EUROSTAT <- "Elaborazione di Lorenzo Ruffino su dati Eurostat"
CAP_AMECO    <- "Elaborazione di Lorenzo Ruffino su dati Commissione europea (Ameco)"

dir_in  <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Austerità in Italia/input"
dir_out <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Austerità in Italia/output"

num_it <- function(x, d = 1) formatC(round(x, d), format = "f", digits = d, decimal.mark = ",", big.mark = ".")

# --- 01. Spesa primaria reale, indice 2009 = 100 ----------------------------

spesa <- read_csv(file.path(dir_in, "A_spesa_reale.csv"), show_col_types = FALSE) %>%
  filter(variabile == "spesa_totale_reale_deflatore_pil_indice_2007=100",
         anno >= 2007) %>%
  mutate(paese = recode(paese, "Area euro (EA20)" = "Area euro")) %>%
  filter(paese %in% names(palette_paesi)) %>%
  select(paese, anno, indice = valore)

write_csv(spesa, file.path(dir_out, "01_spesa_totale_reale.csv"))

lab1 <- spesa %>% filter(anno == max(anno))

g1 <- ggplot(spesa, aes(anno, indice, colour = paese)) +
  geom_hline(yintercept = 100, colour = COL_GRIGIO, linewidth = 0.4, linetype = "dashed") +
  geom_line(aes(linewidth = paese == "Italia")) +
  geom_point(data = filter(spesa, anno == 2019), size = 1.8) +
  geom_text_repel(data = lab1, aes(label = paste0(paese, " ", round(indice))),
                  hjust = 0, nudge_x = 0.4, direction = "y", size = 3.4,
                  fontface = "bold", family = "Source Sans Pro",
                  segment.colour = COL_GRIGIO, min.segment.length = 0,
                  box.padding = 0.2, seed = 1) +
  scale_linewidth_manual(values = c(`TRUE` = 1.1, `FALSE` = 0.7), guide = "none") +
  scale_colour_manual(values = palette_paesi) +
  scale_x_continuous(breaks = seq(2007, 2025, 3), limits = c(2007, 2028.5),
                     expand = c(0.01, 0.01)) +
  scale_y_continuous(breaks = seq(95, 140, 5)) +
  coord_cartesian(clip = "off") +
  labs(title = "In Italia la spesa pubblica è rimasta ferma fino alla pandemia",
       subtitle = "Spesa pubblica totale al netto dell'inflazione (deflatore del Pil), 2007 = 100, 2007-2025",
       caption = CAP_EUROSTAT) +
  theme_linechart() + theme(legend.position = "none")

ggsave(file.path(dir_out, "01_spesa_totale_reale.png"), g1,
       width = 9, height = 6.5, units = "in", dpi = 220, bg = "white")

# --- 02. Aggiustamento strutturale 2010-2013 --------------------------------

agg <- read_csv(file.path(dir_in, "A_ameco_aggiustamento_cumulato.csv"), show_col_types = FALSE) %>%
  filter(anno == "2010-2013", variabile == "variazione_cumulata_saldo_primario_strutturale") %>%
  mutate(paese = recode(paese, "Area euro (EA20)" = "Area euro"),
         colore = case_when(paese == "Italia" ~ COL_ROSSO,
                            paese == "Area euro" ~ COL_BLU,
                            TRUE ~ COL_GRIGIO),
         paese = fct_reorder(paese, valore)) %>%
  select(paese, valore, colore)

write_csv(agg %>% select(-colore), file.path(dir_out, "02_aggiustamento_strutturale.csv"))

g2 <- ggplot(agg, aes(valore, paese)) +
  geom_col(aes(fill = colore), width = 0.65) +
  geom_text(aes(label = num_it(valore)), hjust = -0.25, size = 3.6,
            fontface = "bold", family = "Source Sans Pro") +
  scale_fill_identity() +
  scale_x_continuous(limits = c(0, 12), expand = c(0, 0)) +
  labs(title = "La stretta italiana è stata più piccola di quella dei paesi in crisi",
       subtitle = "Miglioramento del saldo primario strutturale tra il 2010 e il 2013, in punti di Pil potenziale",
       caption = CAP_AMECO) +
  theme_linechart() + theme(legend.position = "none",
                  axis.text.x = element_blank(),
                  axis.line.x = element_blank(),
                  axis.text.y = element_text(size = 11, hjust = 1))

ggsave(file.path(dir_out, "02_aggiustamento_strutturale.png"), g2,
       width = 9, height = 6.5, units = "in", dpi = 220, bg = "white")

# --- 03. Spesa per natura economica, variazione reale 2009-2019 -------------

nat <- read_csv(file.path(dir_in, "C_eurostat_spesa_natura_economica_S13.csv"), show_col_types = FALSE) %>%
  filter(geo == "IT")
defl_2025 <- nat %>% filter(year == 2025) %>% pull(deflatore_pil_2015) %>% first()

voci <- tribble(
  ~na_item,  ~etichetta,
  "D62PAY",  "Prestazioni sociali in denaro\n(pensioni, sussidi, bonus)",
  "D3PAY",   "Contributi alle imprese",
  "P2",      "Acquisti di beni e servizi",
  "D632PAY", "Prestazioni in natura\n(convenzioni sanitarie, farmaci)",
  "D1PAY",   "Stipendi dei dipendenti pubblici",
  "P51G",    "Investimenti"
)

nat3 <- nat %>%
  filter(na_item %in% voci$na_item, year %in% c(2009, 2019)) %>%
  mutate(mld_2025 = mln_euro_2015 * defl_2025 / 100 / 1000) %>%
  select(na_item, year, mld_2025) %>%
  pivot_wider(names_from = year, values_from = mld_2025, names_prefix = "a") %>%
  mutate(diff = a2019 - a2009, var_pct = (a2019 / a2009 - 1) * 100) %>%
  left_join(voci, by = "na_item") %>%
  mutate(etichetta = fct_reorder(etichetta, diff),
         colore = ifelse(diff > 0, COL_BLU, COL_ROSSO))

write_csv(nat3 %>% select(-colore), file.path(dir_out, "03_natura_economica_2009_2019.csv"))

g3 <- ggplot(nat3, aes(diff, etichetta)) +
  geom_vline(xintercept = 0, colour = COL_NERO, linewidth = 0.3) +
  geom_col(aes(fill = colore), width = 0.65) +
  geom_text(aes(label = paste0(ifelse(diff > 0, "+", "−"), "€ ", num_it(abs(diff), 0), " mld"),
                hjust = ifelse(diff > 0, -0.15, 1.15)),
            size = 3.6, fontface = "bold", family = "Source Sans Pro") +
  scale_fill_identity() +
  scale_x_continuous(limits = c(-42, 64), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(title = "Più trasferimenti, meno investimenti e stipendi pubblici",
       subtitle = "Variazione della spesa pubblica italiana tra il 2009 e il 2019, al netto dell'inflazione, a prezzi 2025",
       caption = CAP_EUROSTAT) +
  theme_linechart() + theme(legend.position = "none",
                  axis.text.x = element_blank(),
                  axis.line = element_blank(),
                  axis.text.y = element_text(size = 10.5, hjust = 1, lineheight = 0.95))

ggsave(file.path(dir_out, "03_natura_economica_2009_2019.png"), g3,
       width = 9, height = 6.5, units = "in", dpi = 220, bg = "white")

# --- 04. Italia meno area euro per funzione, 2024 ---------------------------

gap <- read_csv(file.path(dir_in, "B_gap_italia_area_euro.csv"), show_col_types = FALSE) %>%
  filter(paese == "Italia meno area euro (EA20, Eurostat)", anno == 2024,
         unita == "punti di PIL")
gv <- function(cod) gap %>% filter(funzione_codice %in% cod) %>% pull(valore) %>% sum()

gap4 <- tribble(
  ~etichetta,                              ~valore,
  "Pensioni di vecchiaia e ai superstiti", gv("PENS"),
  "Interessi sul debito",                  gv("GF0107"),
  "Ordine pubblico e sicurezza",           gv("GF03"),
  "Affari economici e trasporti",          gv("GF04"),
  "Cultura e sport",                       gv("GF08"),
  "Famiglie e figli",                      gv("GF1004"),
  "Disoccupazione",                        gv("GF1005"),
  "Casa e povertà",                        gv(c("GF1006", "GF1007")),
  "Amministrazione generale",              gv(c("GF0101", "GF0102", "GF0103", "GF0104", "GF0105", "GF0106", "GF0108")),
  "Istruzione",                            gv("GF09"),
  "Sanità",                                gv("GF07"),
  "Malattia e invalidità",                 gv("GF1001")
) %>%
  mutate(etichetta = fct_reorder(etichetta, valore),
         colore = case_when(etichetta == "Interessi sul debito" ~ COL_GRIGIO,
                            valore > 0 ~ COL_ROSSO, TRUE ~ COL_BLU))

write_csv(gap4 %>% select(-colore), file.path(dir_out, "04_gap_area_euro_2024.csv"))

g4 <- ggplot(gap4, aes(valore, etichetta)) +
  geom_vline(xintercept = 0, colour = COL_NERO, linewidth = 0.3) +
  geom_col(aes(fill = colore), width = 0.65) +
  geom_text(aes(label = paste0(ifelse(valore > 0, "+", "−"), num_it(abs(valore))),
                hjust = ifelse(valore > 0, -0.2, 1.2)),
            size = 3.5, fontface = "bold", family = "Source Sans Pro") +
  scale_fill_identity() +
  scale_x_continuous(limits = c(-1.6, 4.6), expand = c(0, 0)) +
  labs(title = "L'Italia spende di più per le pensioni e meno per quasi tutto il resto",
       subtitle = "Differenza tra la spesa pubblica italiana e la media dell'area euro, in punti di Pil, 2024",
       caption = CAP_EUROSTAT) +
  theme_linechart() + theme(legend.position = "none",
                  axis.text.x = element_blank(),
                  axis.line = element_blank(),
                  axis.text.y = element_text(size = 10.5, hjust = 1))

ggsave(file.path(dir_out, "04_gap_area_euro_2024.png"), g4,
       width = 9, height = 6.5, units = "in", dpi = 220, bg = "white")

# --- 05. Investimenti pubblici netti ----------------------------------------

inv <- read_csv(file.path(dir_in, "C_investimenti_pubblici_netti.csv"), show_col_types = FALSE) %>%
  filter(geo == "IT") %>%
  transmute(anno = year, netti_pct_pil = inv_netti_pct_pil, netti_mln = investimenti_netti) %>%
  mutate(colore = ifelse(netti_pct_pil < 0, COL_ROSSO, COL_BLU))

write_csv(inv %>% select(-colore), file.path(dir_out, "05_investimenti_netti.csv"))

g5 <- ggplot(inv, aes(anno, netti_pct_pil)) +
  geom_hline(yintercept = 0, colour = COL_NERO, linewidth = 0.3) +
  geom_col(aes(fill = colore), width = 0.75) +
  annotate("text", x = 2016.5, y = 0.35,
           label = "2011-2022: dodici anni in negativo",
           family = "Source Sans Pro", fontface = "bold", size = 3.6, colour = COL_ROSSO) +
  scale_fill_identity() +
  scale_x_continuous(breaks = seq(1995, 2025, 5), expand = c(0.01, 0.01)) +
  scale_y_continuous(breaks = seq(-1, 1.5, 0.5),
                     labels = function(x) paste0(format(x, decimal.mark = ","), "%")) +
  labs(title = "Per dodici anni gli investimenti pubblici netti sono stati negativi",
       subtitle = "Investimenti pubblici meno il valore delle infrastrutture che si consumano ogni anno, in % del Pil, Italia, 1995-2025",
       caption = CAP_EUROSTAT) +
  theme_linechart() + theme(legend.position = "none", axis.line.x = element_blank())

ggsave(file.path(dir_out, "05_investimenti_netti.png"), g5,
       width = 9, height = 6.5, units = "in", dpi = 220, bg = "white")

# --- Controllo ---------------------------------------------------------------

cat("\n01 indici 2009/2019/2025 Italia:",
    spesa %>% filter(paese == "Italia", anno %in% c(2009, 2019, 2025)) %>% pull(indice) %>% round(1), "\n")
cat("02 aggiustamento:\n"); print(agg %>% select(paese, valore))
cat("03 natura economica (mld 2025):\n"); print(nat3 %>% select(na_item, a2009, a2019, diff, var_pct))
cat("04 gap:\n"); print(gap4 %>% select(etichetta, valore)); cat("somma:", sum(gap4$valore), "\n")
cat("05 anni negativi:", inv %>% filter(netti_pct_pil < 0) %>% pull(anno), "\n")
cat("05 cumulato netti negativi 2011-2022 (mln correnti):",
    inv %>% filter(anno %in% 2011:2022) %>% pull(netti_mln) %>% sum(), "\n")
