# Grafico 01 - Prezzi reali delle abitazioni (IPAB Istat deflazionato con il NIC),
# Italia e comuni di Milano, Roma e Torino, 2010-2025, indice 2010 = 100.
# Input: prezzi_istat_ipab_annuale.csv (indice nominale e reale, base 2025 = 100)

source("grafici_00_tema.R")

ANNO_BASE <- 2010

ipab <- read_csv(file.path(IN, "prezzi_istat_ipab_annuale.csv"), show_col_types = FALSE) %>%
  filter(abitazioni == "totale",
         area %in% c("Italia", "Milano", "Roma", "Torino"),
         anno >= 2010, anno <= 2025) %>%
  # Ricalcolo il reale dal nominale e dal NIC per controllo
  mutate(reale = ipab_2025_100 / nic_2025_100 * 100) %>%
  group_by(area) %>%
  mutate(indice = reale / reale[anno == ANNO_BASE] * 100,
         indice_2015 = reale / reale[anno == 2015] * 100,
         indice_2011 = reale / reale[anno == 2011] * 100) %>%
  ungroup()

stopifnot(max(abs(ipab$reale - ipab$ipab_reale_2025_100)) < 0.01)

write_csv(ipab %>% select(area, anno, ipab_nominale_2025_100 = ipab_2025_100,
                          nic_2025_100, indice_reale_2010_100 = indice,
                          indice_reale_2015_100 = indice_2015),
          file.path(OUT, "01_prezzi_reali_case_citta.csv"))

colori <- c("Milano" = COL_ROSSO, "Roma" = COL_BLU, "Torino" = COL_GIALLO,
            "Italia" = COL_NERO)

etichette <- ipab %>%
  filter(anno == 2025) %>%
  mutate(lab = paste0(area, " ", fmt_it(indice)))

p <- ggplot(ipab, aes(anno, indice, colour = area)) +
  geom_hline(yintercept = 100, colour = COL_GRIGIO, linewidth = 0.4, linetype = "dashed") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.6) +
  geom_text_repel(data = etichette, aes(label = lab),
                  hjust = 0, nudge_x = 0.3, direction = "y",
                  size = 3.6, fontface = "bold", family = "Source Sans Pro",
                  segment.colour = COL_GRIGIO, min.segment.length = 0,
                  box.padding = 0.2, seed = 1) +
  scale_colour_manual(values = colori) +
  scale_x_continuous(breaks = seq(2010, 2025, 3), limits = c(2010, 2028),
                     expand = c(0.01, 0)) +
  scale_y_continuous(breaks = seq(50, 110, 10), limits = c(50, 110),
                     expand = c(0.01, 0.01)) +
  coord_cartesian(clip = "off") +
  labs(title = "Le case costano meno che nel 2010, tranne a Milano",
       subtitle = "Prezzi delle case al netto dell'inflazione, 2010 = 100, Italia e comuni di Milano, Roma e Torino, 2010-2025",
       caption = "Elaborazione di Lorenzo Ruffino su dati Istat") +
  theme_linechart() + theme(legend.position = "none")

salva(p, "01_prezzi_reali_case_citta.png", width = 9, height = 6.5)

ipab %>% filter(anno %in% c(2015, 2025)) %>%
  select(area, anno, indice, indice_2015, indice_2011) %>%
  arrange(area, anno) %>% print()
