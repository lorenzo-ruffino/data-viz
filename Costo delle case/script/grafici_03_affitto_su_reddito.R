# Grafico 03 - Quanto pesa l'affitto: canone mensile di un'abitazione di 60 m²
# ai canoni medi dei nuovi contratti OMI, in percentuale del reddito netto medio
# mensile stimato dei contribuenti del comune. 2015 e 2025.
# Il reddito netto medio è stimato dai dati MEF: reddito complessivo meno imposta
# netta e addizionali regionale e comunale, diviso per il numero di contribuenti
# con reddito complessivo, diviso 12. Per il 2025 si usano i redditi 2024
# (ultimo anno d'imposta disponibile), per il 2015 i redditi 2015.
# Input: prezzi_omi_canoni_contratti.csv, redditi_comuni_irpef.csv

source("grafici_00_tema.R")

CITTA <- c("Milano", "Firenze", "Roma", "Bologna", "Torino",
           "Napoli", "Genova", "Palermo", "Italia")
MQ <- 60

redditi <- read_csv(file.path(IN, "redditi_comuni_irpef.csv"), show_col_types = FALSE,
                    col_types = cols(codice_istat = col_character())) %>%
  filter(comune %in% CITTA, anno_imposta %in% c(2015, 2024)) %>%
  mutate(netto_medio = (ammontare_reddito_complessivo - ammontare_imposta_netta -
                          ammontare_addizionale_regionale -
                          ammontare_addizionale_comunale) / freq_reddito_complessivo,
         anno = if_else(anno_imposta == 2024, 2025, 2015)) %>%
  select(area = comune, anno, anno_imposta, netto_medio, netto_mef = reddito_netto_stimato_medio)

# Controllo: il netto ricalcolato coincide con la colonna già stimata
stopifnot(max(abs(redditi$netto_medio - redditi$netto_mef)) < 1)

canoni <- read_csv(file.path(IN, "prezzi_omi_canoni_contratti.csv"), show_col_types = FALSE) %>%
  filter(segmento == "ordinario_lungo", area %in% CITTA, anno %in% c(2015, 2025)) %>%
  select(area, anno, canone_m2_anno = canone_annuo_medio_euro_m2)

dati <- canoni %>%
  left_join(redditi, by = c("area", "anno")) %>%
  mutate(canone_mese = canone_m2_anno * MQ / 12,
         netto_mese  = netto_medio / 12,
         quota       = canone_mese / netto_mese * 100)

write_csv(dati %>% select(-netto_mef), file.path(OUT, "03_affitto_su_reddito_netto.csv"))

larga <- dati %>%
  select(area, anno, quota) %>%
  pivot_wider(names_from = anno, values_from = quota, names_prefix = "q") %>%
  mutate(area = fct_reorder(area, q2025))

# Controllo con i valori del report di ricerca
attesi <- tibble(area = c("Milano", "Italia"), q2025 = c(40.4, 23.8), q2015 = c(32.8, 21.5))
chk <- larga %>% mutate(area = as.character(area)) %>% inner_join(attesi, by = "area", suffix = c("", "_atteso"))
stopifnot(all(abs(chk$q2025 - chk$q2025_atteso) < 0.06),
          all(abs(chk$q2015 - chk$q2015_atteso) < 0.06))

prima_riga <- levels(larga$area)[nlevels(larga$area)]

p <- ggplot(larga, aes(y = area)) +
  geom_vline(xintercept = 30, colour = COL_GRIGIO, linewidth = 0.4, linetype = "dashed") +
  annotate("text", x = 30, y = 0.35, label = "Soglia del 30%", hjust = -0.08,
           family = "Source Sans Pro", size = 3.2, colour = COL_GRIGIO_SCURO) +
  geom_segment(aes(x = q2015, xend = q2025, yend = area),
               colour = COL_ROSSO, linewidth = 0.9,
               arrow = arrow(length = unit(0.22, "cm"), type = "closed")) +
  geom_point(aes(x = q2015), colour = COL_GRIGIO_SCURO, size = 2) +
  geom_label(aes(x = q2015 - 0.5, label = paste0(fmt_it(q2015, 1), "%")), hjust = 1,
             family = "Source Sans Pro", fontface = "bold", size = 3.4,
             colour = COL_GRIGIO_SCURO, fill = "white", label.size = 0,
             label.padding = unit(0.08, "lines")) +
  geom_label(aes(x = q2025 + 0.6, label = paste0(fmt_it(q2025, 1), "%")), hjust = 0,
             family = "Source Sans Pro", fontface = "bold", size = 3.4,
             colour = COL_ROSSO, fill = "white", label.size = 0,
             label.padding = unit(0.08, "lines")) +
  annotate("text", x = larga$q2015[larga$area == prima_riga] - 0.5,
           y = nlevels(larga$area) + 0.55, label = "2015", hjust = 1,
           family = "Source Sans Pro", fontface = "bold", size = 3.4,
           colour = COL_GRIGIO_SCURO) +
  annotate("text", x = larga$q2025[larga$area == prima_riga] + 0.6,
           y = nlevels(larga$area) + 0.55, label = "2025", hjust = 0,
           family = "Source Sans Pro", fontface = "bold", size = 3.4,
           colour = COL_ROSSO) +
  scale_x_continuous(limits = c(15, 45), expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(add = c(0.7, 0.9))) +
  coord_cartesian(clip = "off") +
  labs(title = "In quattro città l'affitto supera il 30% del reddito",
       subtitle = "Canone mensile di una casa di 60 metri quadri ai prezzi dei nuovi contratti d'affitto, in percentuale\ndel reddito netto medio mensile, per città, 2015 e 2025",
       caption = "Elaborazione di Lorenzo Ruffino su dati Agenzia delle Entrate (Omi) e Mef") +
  theme_linechart() +
  theme(legend.position = "none",
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10, hjust = 1))

salva(p, "03_affitto_su_reddito_netto.png", width = 8, height = 6.5)

dati %>% arrange(area, anno) %>%
  transmute(area, anno, anno_imposta, canone_mese = round(canone_mese),
            netto_mese = round(netto_mese), quota = round(quota, 1)) %>%
  print(n = 30)
