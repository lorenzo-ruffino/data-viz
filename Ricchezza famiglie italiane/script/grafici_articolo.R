library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(showtext)
library(ggrepel)

# --- Font e tema ------------------------------------------------------------
font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_ROSSO  <- "#F12938"
COL_VIOLA  <- "#A82DE3"
COL_GIALLO <- "#F2A900"
COL_GRIGIO <- "#9A9A9A"

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
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.key = element_blank(),
      panel.border = element_blank(),
      legend.title = element_blank(),
      plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), "cm"),
      plot.title.position = "plot",
      legend.text = element_text(size = 10, color = "#1C1C1C", hjust = 0),
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

# --- Carica dati ------------------------------------------------------------
xls_path <- "../Conti-Distributivi-della-Ricchezza-2010q4-2025q4(1).xlsx"

ind   <- read_excel(xls_path, sheet = "1_indicatori")
tot   <- read_excel(xls_path, sheet = "2_totali")
dec   <- read_excel(xls_path, sheet = "3_per_decile_ricchezza")
hou   <- read_excel(xls_path, sheet = "4_per_cond_abitativa")
wor   <- read_excel(xls_path, sheet = "5_per_cond_lavorativa")

# Media annuale dai trimestri
ann_avg <- function(df, meta = 6) {
  r <- df[, 1:meta]
  for (yr in 2010:2025) {
    qc <- grep(sprintf("^%d Q", yr), colnames(df), value = TRUE)
    r[[as.character(yr)]] <- rowMeans(df[, qc, drop = FALSE], na.rm = TRUE)
  }
  r
}

ind_a  <- ann_avg(ind)
ph     <- tot %>% filter(Level == "Per household", Demographics == "Total")
ph_a   <- ann_avg(ph)
dec_nw <- dec %>% filter(Item == "Net Wealth", Level == "Per household")
dec_a  <- ann_avg(dec_nw)
hou_nw <- hou %>% filter(Item == "Net Wealth", Level == "Per household")
hou_a  <- ann_avg(hou_nw)
wor_nw <- wor %>% filter(Item == "Net Wealth", Level == "Per household")
wor_a  <- ann_avg(wor_nw)

# --- Deflatore FOI (2010=1), concatenato ---
defl <- c(1.0, 1.02725, 1.05775, 1.070167, 1.072417, 1.070583,
          1.069245, 1.081557, 1.092976, 1.097883, 1.095564,
          1.115815, 1.205209, 1.270693, 1.281845, 1.299599)
d25 <- defl[16]

to25 <- function(df) {
  r <- df
  for (i in seq_along(defl)) {
    yr <- as.character(2009 + i)
    r[[yr]] <- r[[yr]] * (d25 / defl[i])
  }
  r
}

ind25  <- to25(ind_a)
ph25   <- to25(ph_a)
dec25  <- to25(dec_a)
hou25  <- to25(hou_a)
wor25  <- to25(wor_a)

# --- 01 MEDIA vs MEDIANA ----------------------------------------------------
mw  <- ind25 %>% filter(Item == "Mean Wealth", Demographics == "Total")
med <- ind25 %>% filter(Item == "Median Wealth", Demographics == "Total")

plot01 <- bind_rows(
  mw %>% mutate(serie = "Media"),
  med %>% mutate(serie = "Mediana")
) %>%
  select(serie, as.character(2010:2025)) %>%
  pivot_longer(-serie, names_to = "anno", values_to = "valore") %>%
  mutate(anno = as.numeric(anno))

label01 <- plot01 %>%
  filter(anno == max(anno)) %>%
  mutate(label = serie)

# Valori solo sull'ultimo punto
pt_labels <- plot01 %>%
  filter(anno == 2025) %>%
  mutate(
    val_label = paste0(format(round(valore/1000), big.mark = ".", decimal.mark = ","), "k"),
    hjust = 1
  )

p01 <- ggplot(plot01, aes(x = anno, y = valore, color = serie)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  geom_text_repel(data = label01,
                  aes(label = label),
                  hjust = 0, nudge_x = 0.6, direction = "y",
                  size = 3.0, fontface = "bold", family = "Source Sans Pro",
                  segment.colour = "#9A9A9A", min.segment.length = 0,
                  box.padding = 0.2, seed = 1) +
  geom_text(data = pt_labels,
            aes(label = val_label, hjust = hjust),
            vjust = -1.4, size = 3.0, fontface = "bold",
            family = "Source Sans Pro", show.legend = FALSE) +
  scale_color_manual(values = c("Media" = COL_BLU, "Mediana" = COL_ROSSO)) +
  scale_x_continuous(breaks = seq(2010, 2025, 3), limits = c(2010, 2028.5), expand = c(0, 0)) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = c(0.01, 0.01),
    breaks = seq(100000, 500000, 100000),
    labels = function(x) paste0("€ ", format(x / 1000, big.mark = ".", decimal.mark = ","), "k")
  ) +
  theme_linechart() +
  labs(
    title = "La ricchezza mediana italiana scende",
    subtitle = "Ricchezza netta per famiglia aggiustata per l'inflazione, a prezzi 2025, Italia, 2010-2025",
    caption = "Elaborazione di Lorenzo Ruffino su dati Banca d'Italia e Istat"
  )

ggsave("../output/01_media_mediana.png", p01, width = 9, height = 6.5, dpi = 220, bg = "white")

# --- 02 VARIAZIONE PER GRUPPO SOCIALE ---------------------------------------
groups_nw <- list(
  "50% più povero"   = c("Bottom 50%"),
  "Classe medio-alta" = c("Decile D6-D9"),
  "10% più ricco"    = c("Decile D10")
)

get_var <- function(breakdown, df) {
  v <- df %>% filter(Breakdown == breakdown)
  c(v10 = as.numeric(v[["2010"]]), v25 = as.numeric(v[["2025"]]))
}

dec_vars <- bind_rows(lapply(names(groups_nw), function(nm) {
  bd <- groups_nw[[nm]]
  vals <- get_var(bd, dec25)
  tibble(group = nm, var_pct = (vals["v25"] / vals["v10"] - 1) * 100,
         v2010 = vals["v10"], v2025 = vals["v25"])
}))

dec_vars <- dec_vars %>%
  mutate(group = factor(group, levels = rev(c(
    "50% più povero", "Classe medio-alta", "10% più ricco"
  ))),
  direction = ifelse(var_pct >= 0, "positivo", "negativo"))

p02 <- ggplot(dec_vars, aes(x = var_pct, y = group, fill = direction)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = paste0(sprintf("%+.1f", var_pct), "%"),
                hjust = ifelse(var_pct >= 0, 1.1, -0.1)),
            size = 3.0, fontface = "bold", family = "Source Sans Pro",
            color = "white") +
  geom_vline(xintercept = 0, linewidth = 0.4, color = COL_GRIGIO) +
  scale_fill_manual(values = c("positivo" = COL_BLU, "negativo" = COL_ROSSO)) +
  scale_x_continuous(limits = c(-28, 10), expand = c(0, 0),
                     labels = function(x) paste0(x, "%")) +
  theme_linechart() +
  theme(
    axis.text.y = element_text(size = 10, hjust = 0),
    axis.line.x = element_blank(),
    axis.text.x = element_blank(),
    axis.line.y = element_blank()
  ) +
  labs(
    title = "La classe medio-alta è quella che si è impoverita di più",
    subtitle = "Variazione percentuale reale della ricchezza netta per famiglia, a prezzi 2025, Italia, 2010-2025",
    caption = "Elaborazione di Lorenzo Ruffino su dati Banca d'Italia e Istat"
  )

ggsave("../output/02_variazione_gruppi.png", p02, width = 9, height = 6.5, dpi = 220, bg = "white")

# --- 03 COMPOSIZIONE RICCHEZZA PER GRUPPO (2025) ----------------------------
# Mostriamo la composizione delle attività lorde (escludendo le passività)
# per evitare valori negativi nello stacked bar
dec_comp <- dec %>%
  filter(Level == "Per household", Breakdown %in% c("Bottom 50%", "Decile D6-D9", "Decile D10"))

asset_items <- c("Housing Wealth", "Deposits", "Financial Business Wealth",
                 "Investment Fund Shares", "Life insurance and annuity entitlements",
                 "Listed Shares", "Debt Securities", "Non Financial Business Wealth")

dec_comp_a <- ann_avg(dec_comp)
dec_comp25 <- to25(dec_comp_a)

comp_data <- dec_comp25 %>%
  filter(Item %in% asset_items) %>%
  select(Breakdown, Item, `2025`) %>%
  rename(valore = `2025`) %>%
  mutate(
    Item = case_when(
      Item == "Housing Wealth" ~ "Abitazione",
      Item == "Deposits" ~ "Depositi",
      Item == "Financial Business Wealth" ~ "Partecipazioni in imprese",
      Item == "Investment Fund Shares" ~ "Fondi comuni",
      Item == "Life insurance and annuity entitlements" ~ "Polizze vita",
      Item == "Non Financial Business Wealth" ~ "Altri beni reali",
      Item %in% c("Listed Shares", "Debt Securities") ~ "Azioni e titoli",
      TRUE ~ Item
    ),
      Breakdown = case_when(
      Breakdown == "Bottom 50%" ~ "50% più povero",
      Breakdown == "Decile D6-D9" ~ "Classe medio-alta",
      Breakdown == "Decile D10" ~ "10% più ricco"
    ),
    Breakdown = factor(Breakdown, levels = c("10% più ricco", "Classe medio-alta", "50% più povero"))
  ) %>%
  group_by(Breakdown, Item) %>%
  summarise(valore = sum(valore), .groups = "drop") %>%
  group_by(Breakdown) %>%
  mutate(pct = valore / sum(valore) * 100) %>%
  ungroup() %>%
  mutate(Item = factor(Item, levels = rev(c(
    "Abitazione", "Partecipazioni in imprese", "Depositi",
    "Fondi comuni", "Azioni e titoli", "Polizze vita", "Altri beni reali"
  ))))

comp_colors <- c(
  "Abitazione"               = COL_ROSSO,
  "Partecipazioni in imprese" = COL_BLU,
  "Depositi"                 = COL_GIALLO,
  "Fondi comuni"             = "#E06B9F",
  "Azioni e titoli"          = "#E07700",
  "Polizze vita"             = "#1B9E77",
  "Altri beni reali"         = "#D0D0D0"
)

comp_labels <- comp_data %>% filter(pct >= 5)

p03 <- ggplot(comp_data, aes(x = Breakdown, y = pct, fill = Item)) +
  geom_col(width = 0.6) +
  geom_text(data = comp_labels,
            aes(label = paste0(round(pct), "%")),
            position = position_stack(vjust = 0.5),
            size = 2.8, fontface = "bold", family = "Source Sans Pro",
            color = "white") +
  scale_fill_manual(values = comp_colors) +
  scale_y_continuous(expand = c(0, 0), labels = function(x) paste0(x, "%")) +
  guides(fill = guide_legend(reverse = TRUE, byrow = TRUE,
         keyheight = unit(0.35, "cm"), keywidth = unit(0.45, "cm"))) +
  theme_linechart() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 9, color = COL_NERO, lineheight = 1.2),
    legend.key.size = unit(0.4, "cm"),
    axis.text.x = element_text(size = 10, lineheight = 1.1),
    axis.text.y = element_blank(),
    axis.line = element_blank()
  ) +
  labs(
    title = "La casa è l'asset più importante, poi i depositi",
    subtitle = "Composizione delle attività lorde per gruppo sociale, a prezzi 2025, Italia, 2025",
    caption = "Elaborazione di Lorenzo Ruffino su dati Banca d'Italia e Istat"
  )

ggsave("../output/03_composizione_gruppi.png", p03, width = 9, height = 6.5, dpi = 220, bg = "white")

cat("✅ Tre grafici salvati in ../output/\n")
