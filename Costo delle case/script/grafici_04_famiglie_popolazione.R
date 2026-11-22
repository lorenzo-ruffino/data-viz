# Grafico 04 - Famiglie e popolazione in Italia, indice 1971 = 100, dai censimenti
# alle previsioni Istat al 2050 (base 1/1/2025, scenario mediano).
# Input: demo_famiglie_italia_1951_2050.csv (famiglie: censimenti e previsioni)
#        demo_censimenti_citta_1951_2024.csv (popolazione residente, riga Italia)
# La popolazione prevista (non presente nei CSV di input) è quella del comunicato
# Istat "Previsioni della popolazione residente e delle famiglie, base 1/1/2025"
# del 31/8/2026, scenario mediano: 58,9 milioni nel 2025 e 55,0 milioni nel 2050.

source("grafici_00_tema.R")

ANNO_BASE <- 1971

fam <- read_csv(file.path(IN, "demo_famiglie_italia_1951_2050.csv"), show_col_types = FALSE)

fam_oss <- fam %>%
  filter(str_detect(fonte_tipo, "^censimento"), anno >= ANNO_BASE) %>%
  transmute(anno, valore = famiglie, tipo = "osservato")
fam_prev <- fam %>%
  filter(str_detect(fonte_tipo, "^previsioni")) %>%
  transmute(anno, valore = famiglie, tipo = "previsione")

pop_oss <- read_csv(file.path(IN, "demo_censimenti_citta_1951_2024.csv"), show_col_types = FALSE) %>%
  filter(citta == "Italia", anno >= ANNO_BASE,
         riferimento == "censimento" | anno %in% c(2021, 2024)) %>%
  # il dato al 31 dicembre 2024 è la popolazione all'inizio del 2025
  transmute(anno = if_else(anno == 2024, 2025, anno), valore = comune, tipo = "osservato")
pop_prev <- tibble(anno = c(2025, 2050), valore = c(58.9e6, 55.0e6), tipo = "previsione")

base_fam <- fam_oss$valore[fam_oss$anno == ANNO_BASE]
base_pop <- pop_oss$valore[pop_oss$anno == ANNO_BASE]

serie <- bind_rows(
  bind_rows(fam_oss, fam_prev) %>% mutate(serie = "Famiglie", indice = valore / base_fam * 100),
  bind_rows(pop_oss, pop_prev) %>% mutate(serie = "Popolazione", indice = valore / base_pop * 100)
)

write_csv(serie, file.path(OUT, "04_famiglie_popolazione_italia.csv"))

# Tratto osservato e tratto previsto; il previsto parte dall'ultimo osservato
oss <- serie %>% filter(tipo == "osservato")
ultimo_oss <- oss %>% group_by(serie) %>% filter(anno == max(anno)) %>% ungroup()
prev <- bind_rows(ultimo_oss,
                  serie %>% filter(tipo == "previsione", !(serie == "Popolazione" & anno == 2025)))

colori <- c("Famiglie" = COL_ROSSO, "Popolazione" = COL_BLU)

etichette_fine <- serie %>% filter(anno == 2050) %>%
  mutate(lab = paste0(serie, " ", fmt_it(indice)))

valori_chiave <- serie %>%
  filter(tipo == "osservato", anno == 2021) %>%
  mutate(lab = fmt_it(indice))

p <- ggplot() +
  annotate("rect", xmin = 2025, xmax = 2050, ymin = -Inf, ymax = Inf,
           fill = COL_GRIGIO, alpha = 0.10) +
  annotate("text", x = 2037.5, y = 186, label = "Previsioni Istat",
           family = "Source Sans Pro", size = 3.3, colour = COL_GRIGIO_SCURO) +
  geom_hline(yintercept = 100, colour = COL_GRIGIO, linewidth = 0.4, linetype = "dashed") +
  geom_line(data = oss, aes(anno, indice, colour = serie), linewidth = 1) +
  geom_line(data = prev, aes(anno, indice, colour = serie), linewidth = 1, linetype = "22") +
  geom_point(data = serie %>% filter(!(serie == "Popolazione" & tipo == "previsione" & anno == 2025)),
             aes(anno, indice, colour = serie), size = 1.8) +
  geom_text(data = valori_chiave, aes(anno, indice, label = lab, colour = serie),
            vjust = -1.1, family = "Source Sans Pro", fontface = "bold", size = 3.4) +
  geom_text(data = etichette_fine, aes(anno, indice, label = lab, colour = serie),
            hjust = 0, nudge_x = 0.8, family = "Source Sans Pro", fontface = "bold",
            size = 3.6) +
  scale_colour_manual(values = colori) +
  scale_x_continuous(breaks = c(1971, 1981, 1991, 2001, 2011, 2021, 2030, 2040, 2050),
                     limits = c(1971, 2067), expand = c(0.01, 0)) +
  scale_y_continuous(breaks = seq(90, 190, 10), limits = c(90, 190),
                     expand = c(0.01, 0.01)) +
  coord_cartesian(clip = "off") +
  labs(title = "Le famiglie crescono anche se la popolazione cala",
       subtitle = "Numero di famiglie e di residenti, 1971 = 100, Italia, censimenti 1971-2021 e previsioni al 2050",
       caption = "Elaborazione di Lorenzo Ruffino su dati Istat") +
  theme_linechart() +
  theme(legend.position = "none")

salva(p, "04_famiglie_popolazione_italia.png", width = 9, height = 6.5)

serie %>% mutate(indice = round(indice, 1), valore = round(valore / 1e6, 2)) %>%
  arrange(serie, anno) %>% print(n = 40)
