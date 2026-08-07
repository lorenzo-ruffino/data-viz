library(tidyverse)
library(readxl)
library(showtext)

IN  <- "../input"
OUT <- "../output"

# ============================================================================
# DATI
# ----------------------------------------------------------------------------
# OCSE, Pensions at a Glance 2025, tavola 8.1 "Mandatory contribution rates in
# 2024", scaricata dallo StatLink https://stat.link/orhufb.
# Aliquote obbligatorie per le pensioni di vecchiaia e ai superstiti, schemi
# pubblici e privati, lavoratori del settore privato.
#
# La tavola dà le aliquote nominali distinte fra lavoratore e datore di lavoro
# e, a parte, l'aliquota effettiva su un salario medio. Le due coincidono
# ovunque tranne dove ci sono massimali o aliquote che variano con la
# retribuzione (Paesi Bassi, Svizzera, Danimarca): lì l'aliquota nominale non
# è quella che paga chi guadagna lo stipendio medio. Si usa quindi come totale
# l'aliquota effettiva e si ripartisce fra le due parti in proporzione alle
# aliquote nominali. Il fattore di scala vale 1 in 22 paesi su 25.
# ============================================================================

nomi <- c(
  Austria = "Austria", Belgium = "Belgio", Czechia = "Cechia",
  Denmark = "Danimarca", Estonia = "Estonia", Finland = "Finlandia",
  France = "Francia", Germany = "Germania", Greece = "Grecia",
  Hungary = "Ungheria", Iceland = "Islanda", Ireland = "Irlanda",
  Italy = "Italia", Latvia = "Lettonia", Lithuania = "Lituania",
  Luxembourg = "Lussemburgo", Netherlands = "Paesi Bassi", Norway = "Norvegia",
  Poland = "Polonia", Portugal = "Portogallo", `Slovak Republic` = "Slovacchia",
  Slovenia = "Slovenia", Spain = "Spagna", Sweden = "Svezia",
  Switzerland = "Svizzera"
)

# le celle contengono marcatori tipo "7.47 [a]" o "11.3 [w]"
num <- function(x) as.numeric(str_remove_all(as.character(x), "\\[.*\\]|\\s"))

grezzi <- read_excel(
  file.path(IN, "oecd_paag2025_tab81_contribution_rates.xlsx"),
  sheet = "t8-1", skip = 4, col_names = FALSE
)

dati <- grezzi %>%
  transmute(
    paese_en  = str_remove_all(as.character(...1), "\\*"),
    lav_pub   = num(...2), dat_pub = num(...3),
    lav_priv  = num(...4), dat_priv = num(...5),
    nominale  = num(...6), effettiva = num(...8)
  ) %>%
  filter(paese_en %in% names(nomi)) %>%
  mutate(
    paese      = nomi[paese_en],
    lav_nom    = coalesce(lav_pub, 0) + coalesce(lav_priv, 0),
    dat_nom    = coalesce(dat_pub, 0) + coalesce(dat_priv, 0),
    scala      = effettiva / nominale,
    lavoratore = lav_nom * scala,
    datore     = dat_nom * scala,
    totale     = lavoratore + datore
  ) %>%
  arrange(desc(totale))

write_csv(
  dati %>% select(paese, paese_en, lav_nom, dat_nom, nominale,
                  effettiva, lavoratore, datore, totale),
  file.path(OUT, "aliquote_contributive_pensione_europa.csv")
)

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

CAP_OCSE <- "Elaborazione di Lorenzo Ruffino su dati Ocse"

fmt_it <- function(v, d = 1) formatC(v, format = "f", digits = d, decimal.mark = ",")

# ============================================================================
# GRAFICO
# ============================================================================

# Media semplice dei paesi rappresentati: sono aliquote, non aggregati, quindi
# non si ponderano. Non si usa la media UE perché la tavola Ocse non copre
# Bulgaria, Croazia, Cipro, Malta e Romania, che non sono paesi Ocse.
media_eu <- mean(dati$totale)

plot_data <- dati %>%
  arrange(totale) %>%
  mutate(paese = factor(paese, levels = paese)) %>%
  select(paese, `Datore di lavoro` = datore, Lavoratore = lavoratore) %>%
  pivot_longer(-paese, names_to = "quota", values_to = "valore") %>%
  mutate(quota = factor(quota, levels = c("Datore di lavoro", "Lavoratore")))

label_tot <- dati %>%
  arrange(totale) %>%
  mutate(paese = factor(paese, levels = paese), is_it = paese_en == "Italy")

g <- ggplot(plot_data, aes(valore, paese, fill = quota)) +
  geom_col(width = 0.74, position = position_stack(reverse = TRUE)) +
  geom_vline(xintercept = media_eu, colour = COL_GRIGIO_SCURO,
             linewidth = 0.4, linetype = "dashed") +
  annotate("text", x = media_eu + 0.45, y = 2.4,
           label = paste0("media europea ", fmt_it(media_eu), "%"),
           hjust = 0, size = 2.9, colour = COL_GRIGIO_SCURO,
           family = "Source Sans Pro") +
  geom_label(data = label_tot,
             aes(x = totale, y = paese, label = paste0(fmt_it(totale), "%")),
             inherit.aes = FALSE, hjust = 0, nudge_x = 0.3, size = 2.9,
             family = "Source Sans Pro",
             fontface = ifelse(label_tot$is_it, "bold", "plain"),
             colour = COL_NERO, fill = "white", linewidth = 0,
             label.padding = unit(0.07, "lines")) +
  scale_fill_manual(values = c("Datore di lavoro" = COL_BLU,
                               "Lavoratore" = COL_ROSSO)) +
  scale_x_continuous(limits = c(0, 36), expand = c(0, 0)) +
  labs(title = "In Italia i contributi per la pensione pesano di più",
       subtitle = paste0(
         "Contributi obbligatori per la pensione di un lavoratore dipendente con lo stipendio medio, in percentuale\n",
         "della retribuzione lorda, 2024"),
       caption = CAP_OCSE) +
  theme_linechart() +
  theme(axis.text.y = element_text(
          hjust = 0, size = 8.5,
          face = ifelse(levels(label_tot$paese) == "Italia", "bold", "plain")),
        axis.text.x = element_blank(),
        axis.line = element_blank(),
        legend.justification.top = "left",
        legend.location = "plot",
        legend.key.size = unit(0.42, "cm"))

ggsave(file.path(OUT, "aliquote_contributive_pensione_europa.png"), g,
       width = 8.5, height = 7.8, dpi = 220, bg = "white")

# ============================================================================
# CONTROLLI
# ============================================================================

cat("\nPaesi:", nrow(dati), "\n")
cat("Paesi con aliquota effettiva diversa dalla nominale:\n")
print(as.data.frame(dati %>% filter(abs(scala - 1) > 0.005) %>%
  transmute(paese, nominale, effettiva, scala = round(scala, 3))))
cat("\nMedia europea (semplice,", nrow(dati), "paesi):", fmt_it(media_eu), "%\n\n")
print(as.data.frame(dati %>% transmute(paese, lavoratore = round(lavoratore, 1),
                                       datore = round(datore, 1),
                                       totale = round(totale, 1))))
