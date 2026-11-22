# Grafico 02 - Canoni dei nuovi contratti d'affitto (OMI, contratti ordinari di
# lungo periodo) al netto dell'inflazione: variazione 2015-2025 per città.
# Input: prezzi_omi_canoni_contratti.csv (Rapporti Immobiliari OMI 2016-2026)
#        prezzi_istat_nic_annuale.csv (indice NIC, base 2025 = 100)

source("grafici_00_tema.R")

CITTA <- c("Firenze", "Milano", "Bologna", "Torino", "Roma",
           "Genova", "Napoli", "Palermo", "Italia")

nic <- read_csv(file.path(IN, "prezzi_istat_nic_annuale.csv"), show_col_types = FALSE)
infl <- nic$nic_2025_100[nic$anno == 2025] / nic$nic_2025_100[nic$anno == 2015]

canoni <- read_csv(file.path(IN, "prezzi_omi_canoni_contratti.csv"), show_col_types = FALSE) %>%
  filter(segmento == "ordinario_lungo", area %in% CITTA, anno %in% c(2015, 2025)) %>%
  select(area, anno, canone = canone_annuo_medio_euro_m2) %>%
  pivot_wider(names_from = anno, values_from = canone, names_prefix = "c") %>%
  mutate(var_nominale = (c2025 / c2015 - 1) * 100,
         var_reale    = ((c2025 / c2015) / infl - 1) * 100,
         area = fct_reorder(area, var_reale))

write_csv(canoni, file.path(OUT, "02_canoni_nuovi_contratti_reali.csv"))

canoni <- canoni %>%
  mutate(colore = case_when(area == "Italia" ~ COL_NERO,
                            area %in% c("Firenze", "Milano", "Bologna") ~ COL_ROSSO,
                            TRUE ~ COL_BLU))

p <- ggplot(canoni, aes(var_reale, area)) +
  geom_col(aes(fill = colore), width = 0.68) +
  geom_text(aes(label = fmt_var(var_reale, 1)), hjust = -0.2,
            family = "Source Sans Pro", fontface = "bold", size = 3.6,
            colour = COL_NERO) +
  scale_fill_identity() +
  scale_x_continuous(limits = c(0, 35), breaks = seq(5, 30, 5),
                     labels = function(x) paste0("+", x, "%"),
                     expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(title = "I nuovi affitti crescono più dell'inflazione ovunque",
       subtitle = "Variazione del canone medio al metro quadro dei nuovi contratti d'affitto a lungo termine,\nal netto dell'inflazione, per città, 2015-2025",
       caption = "Elaborazione di Lorenzo Ruffino su dati Agenzia delle Entrate (Omi) e Istat") +
  theme_linechart() +
  theme(legend.position = "none",
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10, hjust = 1))

salva(p, "02_canoni_nuovi_contratti_reali.png", width = 8, height = 6.5)

cat(sprintf("Inflazione NIC 2015-2025: +%.1f%%\n", (infl - 1) * 100))
canoni %>% arrange(desc(var_reale)) %>%
  mutate(across(c(var_nominale, var_reale), ~ round(.x, 1))) %>%
  select(area, c2015, c2025, var_nominale, var_reale) %>% print()
