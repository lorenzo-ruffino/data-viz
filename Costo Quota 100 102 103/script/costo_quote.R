library(tidyverse)
library(showtext)

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO <- "#1C1C1C"; COL_BLU <- "#0478EA"; COL_VIOLA <- "#A82DE3"
COL_ROSSO <- "#F12938"; COL_GIALLO <- "#F2A900"; COL_GRIGIO <- "#9A9A9A"

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

CAP <- "Elaborazione di Lorenzo Ruffino su dati Ragioneria generale dello Stato"

# Tabella A1.7, RGS "Le tendenze di medio-lungo periodo del sistema
# pensionistico e socio-sanitario - Aggiornamento 2026", p. 222.
# Bilanci consuntivi, valori in euro.
raw <- tribble(
  ~anno, ~art14_pens, ~art15_pens, ~tfr_14_15, ~art15_tfs, ~art16_pens, ~art16_tfs,
  2019, 1780315270, 387822695, 574661071,         0,  113590682,         0,
  2020, 5044924121, 953688521, 442392085,         0,  368057761,         0,
  2021, 5591803512, 802050964, 357091970, 306089690,  557228241,  85004898,
  2022, 5941569822, 759111084, 370986839,  82761631,  826377271, 145933163,
  2023, 4785845718, 603194355, 146929607,  88834517, 1024217666, 124183353,
  2024, 3813012277, 472199413, 111190852,         0,  937709264, 229400410,
  2025, 1655537825, 919227850,  20874603,         0,  787634894,  74883329
)

dati <- raw |>
  transmute(
    anno,
    `Quota 100, 102 e 103` = art14_pens,
    `Blocco dei requisiti per la pensione anticipata` = art15_pens,
    `Opzione donna` = art16_pens,
    `Liquidazioni (Tfr e Tfs) pagate in anticipo` = tfr_14_15 + art15_tfs + art16_tfs
  ) |>
  pivot_longer(-anno, names_to = "misura", values_to = "euro") |>
  mutate(mld = euro / 1e9)

livelli <- c("Liquidazioni (Tfr e Tfs) pagate in anticipo",
             "Opzione donna",
             "Blocco dei requisiti per la pensione anticipata",
             "Quota 100, 102 e 103")
dati <- dati |> mutate(misura = factor(misura, levels = livelli))

totali <- dati |> group_by(anno) |> summarise(mld = sum(mld), .groups = "drop")
write_csv(dati |> select(anno, misura, euro), "../output/costo_quote.csv")

fmt_mld <- function(x) paste0("€ ", formatC(x, format = "f", digits = 1, decimal.mark = ","), " mld")

colori <- c("Quota 100, 102 e 103" = COL_ROSSO,
            "Blocco dei requisiti per la pensione anticipata" = COL_BLU,
            "Opzione donna" = COL_VIOLA,
            "Liquidazioni (Tfr e Tfs) pagate in anticipo" = COL_GIALLO)

p <- ggplot(totali, aes(x = anno, y = mld)) +
  geom_col(width = 0.7, fill = COL_BLU) +
  geom_text(aes(label = fmt_mld(mld)), vjust = -0.7, family = "Source Sans Pro",
            fontface = "bold", size = 3.6, color = COL_NERO) +
  scale_x_continuous(breaks = 2019:2025, expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(0, 9), breaks = seq(2, 8, 2),
                     labels = function(x) paste0("€ ", x, " mld"),
                     expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_linechart(axis.line.y = element_blank()) +
  labs(title = "Quanto sono costate Quota 100, 102 e 103",
       subtitle = "Spesa per le misure di anticipo della pensione del decreto 4/2019 e proroghe, Italia, 2019-2025",
       caption = CAP)

ggsave("../output/costo_quote.png", p, width = 9, height = 6.5, dpi = 220, bg = "white")

cat("Totale 2019-2025:", sum(dati$euro) / 1e9, "mld\n")
print(totali)
print(dati |> group_by(misura) |> summarise(mld = sum(mld)))
