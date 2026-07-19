library(tidyverse)
library(showtext)

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO    <- "#1C1C1C"
COL_BLU     <- "#0478EA"
COL_ROSSO   <- "#F12938"
COL_GIALLO  <- "#F2A900"
COL_GRIGIO  <- "#9A9A9A"
COL_GRIGIO_SCURO <- "#5A5A5A"
COL_AZZURRO <- "#5C9CDE"

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

fmt_pct <- function(v) paste0(formatC(v, format = "f", digits = 1, decimal.mark = ","), "%")


# ============================================================================
# GRAFICO 1 — Composizione del gettito fiscale: Italia vs media OCSE
# Fonte: OCSE, Revenue Statistics 2025 (struttura del prelievo, anno 2023)
# ============================================================================

ordine_cat <- c(
  "Contributi sociali",
  "Redditi delle persone (IRPEF)",
  "IVA",
  "Altre imposte sui consumi",
  "Profitti delle imprese (IRES)",
  "Proprietà (IMU e altre)",
  "Altre imposte (IRAP, ecc.)"
)

# Etichette su due righe per l'asse
lab_wrap <- c(
  "Contributi sociali"            = "Contributi\nsociali",
  "Redditi delle persone (IRPEF)" = "Redditi delle persone\n(IRPEF)",
  "IVA"                           = "IVA",
  "Altre imposte sui consumi"     = "Altre imposte\nsui consumi",
  "Profitti delle imprese (IRES)" = "Profitti delle imprese\n(IRES)",
  "Proprietà (IMU e altre)"       = "Proprietà\n(IMU e altre)",
  "Altre imposte (IRAP, ecc.)"    = "Altre imposte\n(IRAP, ecc.)"
)

comp <- tribble(
  ~categoria,                        ~Italia, ~`Media OCSE`,
  "Contributi sociali",               29.6,   25.5,
  "Redditi delle persone (IRPEF)",    27.0,   23.7,
  "IVA",                              15.7,   20.5,
  "Altre imposte sui consumi",        11.6,   10.8,
  "Profitti delle imprese (IRES)",     6.6,   11.9,
  "Proprietà (IMU e altre)",           5.4,    5.1,
  "Altre imposte (IRAP, ecc.)",        4.1,    2.6
)

write.csv(comp, "../output/01_composizione_gettito.csv", row.names = FALSE)

comp_long <- comp %>%
  pivot_longer(-categoria, names_to = "serie", values_to = "valore") %>%
  mutate(
    categoria = factor(categoria, levels = rev(ordine_cat)),
    serie     = factor(serie, levels = c("Italia", "Media OCSE"))
  )

col_serie <- c("Italia" = COL_ROSSO, "Media OCSE" = COL_AZZURRO)

p1 <- ggplot(comp_long, aes(x = valore, y = categoria, fill = serie)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.62) +
  geom_text(aes(label = fmt_pct(valore), color = serie),
            position = position_dodge(width = 0.7),
            hjust = -0.18, size = 3.1, fontface = "bold",
            family = "Source Sans Pro", show.legend = FALSE) +
  scale_fill_manual(values = col_serie) +
  scale_color_manual(values = col_serie) +
  scale_x_continuous(limits = c(0, 34), expand = c(0, 0)) +
  scale_y_discrete(labels = lab_wrap) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(
    title = "L'Italia tassa più il lavoro, meno consumi e imprese",
    subtitle = "Entrate fiscali per tipo di imposta, in percentuale del totale del gettito, Italia e media OCSE, 2023",
    caption = "Elaborazione di Lorenzo Ruffino su dati OCSE, Revenue Statistics 2025"
  ) +
  theme_linechart() +
  theme(
    legend.position = "top",
    legend.justification = "left",
    axis.text.x = element_blank(),
    axis.line.x = element_blank(),
    axis.text.y = element_text(size = 9.5, hjust = 1, lineheight = 0.9)
  )

ggsave("../output/01_composizione_gettito.png", p1,
       width = 8, height = 6.5, units = "in", dpi = 220, bg = "white")


# ============================================================================
# GRAFICO 2 — Cuneo fiscale sul lavoro: classifica dei paesi europei OCSE 2025
# Fonte: OCSE, Taxing Wages 2026 (lavoratore medio single senza figli, 100% AW)
# ============================================================================

cuneo <- tribble(
  ~paese,          ~valore,
  "Belgio",          52.5,
  "Germania",        49.3,
  "Francia",         47.2,
  "Austria",         47.1,
  "Italia",          45.8,
  "Slovenia",        45.3,
  "Slovacchia",      42.7,
  "Estonia",         42.6,
  "Finlandia",       42.5,
  "Spagna",          41.4,
  "Ungheria",        41.2,
  "Cechia",          41.2,
  "Svezia",          41.1,
  "Lussemburgo",     40.2,
  "Lettonia",        40.1,
  "Lituania",        39.8,
  "Portogallo",      39.3,
  "Grecia",          39.3,
  "Norvegia",        36.4,
  "Paesi Bassi",     35.9,
  "Danimarca",       35.8,
  "Polonia",         35.0,
  "Irlanda",         32.6,
  "Regno Unito",     32.4,
  "Islanda",         31.5,
  "Svizzera",        23.0
)

media_ocse <- 35.1

write.csv(cuneo, "../output/02_cuneo_fiscale_ocse.csv", row.names = FALSE)

cuneo <- cuneo %>%
  mutate(
    paese  = fct_reorder(paese, valore),
    is_ita = paese == "Italia"
  )

lvl   <- levels(cuneo$paese)
ycol  <- ifelse(lvl == "Italia", COL_ROSSO, "#1C1C1C")
yface <- ifelse(lvl == "Italia", "bold", "plain")

p2 <- ggplot(cuneo, aes(x = valore, y = paese)) +
  geom_col(aes(fill = is_ita), width = 0.72) +
  geom_vline(xintercept = media_ocse, colour = COL_NERO,
             linewidth = 0.4, linetype = "dashed") +
  geom_text(aes(label = fmt_pct(valore), color = is_ita),
            hjust = -0.2, size = 2.6, family = "Source Sans Pro",
            show.legend = FALSE) +
  annotate("text", x = media_ocse + 1, y = 1.5, label = "Media OCSE: 35,1%",
           hjust = 0, vjust = 0.5, size = 3, fontface = "bold",
           family = "Source Sans Pro", color = COL_NERO) +
  scale_fill_manual(values = c("FALSE" = COL_AZZURRO, "TRUE" = COL_ROSSO), guide = "none") +
  scale_color_manual(values = c("FALSE" = COL_NERO, "TRUE" = COL_ROSSO), guide = "none") +
  scale_x_continuous(limits = c(0, 58), expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(add = c(0.6, 0.6))) +
  labs(
    title = "L'Italia ha il quinto cuneo fiscale più alto d'Europa",
    subtitle = "Cuneo fiscale, cioè la differenza tra il costo del lavoro per l'azienda e lo stipendio netto del lavoratore,\nper un single senza figli al salario medio, in percentuale del costo del lavoro, paesi europei dell'OCSE, 2025",
    caption = "Elaborazione di Lorenzo Ruffino su dati OCSE, Taxing Wages 2026"
  ) +
  theme_linechart() +
  theme(
    legend.position = "none",
    axis.text.x  = element_blank(),
    axis.line.x  = element_blank(),
    axis.text.y  = element_text(size = 9, hjust = 1, color = ycol, face = yface)
  )

ggsave("../output/02_cuneo_fiscale_ocse.png", p2,
       width = 8, height = 8, units = "in", dpi = 220, bg = "white")
