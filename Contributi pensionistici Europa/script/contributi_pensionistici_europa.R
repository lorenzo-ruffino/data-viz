library(eurostat)
library(tidyverse)
library(showtext)

OUT <- "../output"

# ============================================================================
# DATI
# ----------------------------------------------------------------------------
# Numeratore: Eurostat nasa_10_pens1 (tavola 29 dei conti nazionali, triennale)
#   D6111 = contributi pensionistici effettivi dei datori di lavoro
#   D6131 = contributi pensionistici effettivi delle famiglie
#           (lavoratori dipendenti + autonomi)
#   penscheme S1P = tutti gli schemi pensionistici, pubblici e privati
# Denominatore: Eurostat nama_10_a10, D11 = retribuzioni lorde (monte salari)
# ============================================================================

ANNO <- 2021   # ultimo anno con copertura completa nella tavola 29

pens_raw <- get_eurostat(
  "nasa_10_pens1", time_format = "num",
  filters = list(unit = "MIO_EUR", na_item = c("D6111", "D6131"))
)

wag_raw <- get_eurostat(
  "nama_10_a10", time_format = "num",
  filters = list(nace_r2 = "TOTAL", unit = "CP_MEUR", na_item = "D11")
)

nomi <- c(
  AT = "Austria", BE = "Belgio", BG = "Bulgaria", HR = "Croazia", CY = "Cipro",
  CZ = "Cechia", DK = "Danimarca", EE = "Estonia", FI = "Finlandia",
  FR = "Francia", DE = "Germania", EL = "Grecia", HU = "Ungheria",
  IE = "Irlanda", IT = "Italia", LV = "Lettonia", LT = "Lituania",
  LU = "Lussemburgo", MT = "Malta", NL = "Paesi Bassi", PL = "Polonia",
  PT = "Portogallo", RO = "Romania", SK = "Slovacchia", SI = "Slovenia",
  ES = "Spagna", SE = "Svezia", CH = "Svizzera", NO = "Norvegia",
  IS = "Islanda"
)
pens <- pens_raw %>%
  filter(time == ANNO, geo %in% names(nomi)) %>%
  select(geo, penscheme, na_item, values)

# Totale di tutti gli schemi. S1P è l'aggregato pubblicato; dove manca (Romania)
# lo si ricostruisce sommando le componenti che non si sovrappongono.
tot_pubblicato <- pens %>%
  filter(penscheme == "S1P") %>%
  pivot_wider(names_from = na_item, values_from = values) %>%
  rename(dat_s1p = D6111, lav_s1p = D6131)

tot_ricostruito <- pens %>%
  filter(penscheme %in% c("S13PS", "S13PBI", "S13PBX", "S13PC",
                          "S12P", "S12PBI")) %>%
  group_by(geo, na_item) %>%
  summarise(v = sum(values, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = na_item, values_from = v) %>%
  rename(dat_ric = D6111, lav_ric = D6131)

contrib <- full_join(tot_pubblicato, tot_ricostruito, by = "geo") %>%
  mutate(
    ricostruito = is.na(dat_s1p) | is.na(lav_s1p),
    datore      = if_else(ricostruito, dat_ric, dat_s1p),
    lavoratore  = if_else(ricostruito, lav_ric, lav_s1p)
  )

# Controllo: dove S1P è pubblicato, la somma delle componenti deve coincidere
scarto <- contrib %>%
  filter(!ricostruito) %>%
  mutate(scarto = (dat_ric + lav_ric) / (dat_s1p + lav_s1p) - 1)

salari <- wag_raw %>%
  filter(time == ANNO, geo %in% names(nomi)) %>%
  select(geo, salari = values)

dati <- contrib %>%
  inner_join(salari, by = "geo") %>%
  filter(!is.na(datore), !is.na(lavoratore)) %>%
  transmute(
    geo,
    paese      = nomi[geo],
    datore_mln = datore,
    lavor_mln  = lavoratore,
    salari_mln = salari,
    datore     = datore / salari * 100,
    lavoratore = lavoratore / salari * 100,
    totale     = datore + lavoratore
  ) %>%
  arrange(desc(totale))

# Media dei paesi rappresentati, ponderata sul monte salari: qui si sommano
# importi, quindi l'aggregato è la media corretta.
media_eu <- dati %>%
  summarise(v = sum(datore_mln + lavor_mln) / sum(salari_mln) * 100) %>%
  pull(v)

write_csv(dati, file.path(OUT, "contributi_pensionistici_europa.csv"))

# ============================================================================
# TEMA
# ============================================================================

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_ROSSO  <- "#F12938"
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

fmt_it <- function(v, d = 1) {
  formatC(v, format = "f", digits = d, decimal.mark = ",")
}

# ============================================================================
# GRAFICO
# ============================================================================

plot_data <- dati %>%
  arrange(totale) %>%
  mutate(paese = factor(paese, levels = paese)) %>%
  select(paese, geo, totale, `Datore di lavoro` = datore,
         `Lavoratori e autonomi` = lavoratore) %>%
  pivot_longer(c(`Datore di lavoro`, `Lavoratori e autonomi`),
               names_to = "quota", values_to = "valore") %>%
  mutate(quota = factor(quota, levels = c("Datore di lavoro",
                                          "Lavoratori e autonomi")))

label_tot <- dati %>%
  arrange(totale) %>%
  mutate(paese = factor(paese, levels = paese), is_it = geo == "IT")

g <- ggplot(plot_data, aes(valore, paese, fill = quota)) +
  geom_col(width = 0.74, position = position_stack(reverse = TRUE)) +
  geom_vline(xintercept = media_eu, colour = COL_GRIGIO_SCURO,
             linewidth = 0.4, linetype = "dashed") +
  annotate("text", x = media_eu + 0.5, y = 2.4,
           label = paste0("media europea ", fmt_it(media_eu), "%"),
           hjust = 0, size = 2.9, colour = COL_GRIGIO_SCURO,
           family = "Source Sans Pro") +
  geom_label(data = label_tot,
             aes(x = totale, y = paese, label = paste0(fmt_it(totale), "%")),
             inherit.aes = FALSE, hjust = 0, nudge_x = 0.35, size = 2.9,
             family = "Source Sans Pro",
             fontface = ifelse(label_tot$is_it, "bold", "plain"),
             colour = COL_NERO, fill = "white", linewidth = 0,
             label.padding = unit(0.07, "lines")) +
  scale_fill_manual(values = c("Datore di lavoro" = COL_BLU,
                               "Lavoratori e autonomi" = COL_ROSSO)) +
  scale_x_continuous(limits = c(0, 41), expand = c(0, 0)) +
  labs(title = "In Italia i contributi per la pensione sono i più alti",
       subtitle = paste0("Contributi pensionistici versati, in percentuale delle retribuzioni lorde, ", ANNO),
       caption = CAP_EUROSTAT) +
  theme_linechart() +
  theme(axis.text.y = element_text(
          hjust = 0, size = 8.5,
          face = ifelse(levels(label_tot$paese) == "Italia", "bold", "plain")),
        axis.text.x = element_blank(),
        axis.line = element_blank(),
        legend.justification.top = "left",
        legend.location = "plot",
        legend.key.size = unit(0.42, "cm"))

ggsave(file.path(OUT, "contributi_pensionistici_europa.png"), g,
       width = 8.5, height = 9, dpi = 220, bg = "white")

# ============================================================================
# CONTROLLI
# ============================================================================

cat("\nAnno:", ANNO, "- paesi:", nrow(dati), "\n")
cat("Ricostruiti da componenti:",
    paste(contrib$geo[contrib$ricostruito], collapse = ", "), "\n")
cat("Scarto massimo somma componenti vs S1P pubblicato:",
    fmt_it(max(abs(scarto$scarto), na.rm = TRUE) * 100, 2), "%\n")
cat("Media europea (ponderata sul monte salari):", fmt_it(media_eu), "%\n\n")
print(as.data.frame(dati %>%
  transmute(paese, datore = round(datore, 1), lavoratore = round(lavoratore, 1),
            totale = round(totale, 1))))
