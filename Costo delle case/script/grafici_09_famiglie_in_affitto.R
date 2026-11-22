# Grafico 09 - Quota di famiglie in affitto e in proprietà ai censimenti, Italia,
# 1951-2023. Fino al 2001 l'unità sono le abitazioni occupate da residenti
# (Sommario di statistiche storiche Istat, tav. 15.2), dal 2011 le famiglie
# (censimento 2011 e censimento permanente 2021 e 2023).
# Input: rentcontrol_italia_titolo_godimento_1951_2023.csv (1951-2001, 2011, 2023)
#        demo_censimenti_titolo_godimento_1951_2023.csv (2021, solo affitto)

source("grafici_00_tema.R")

base <- read_csv(file.path(IN, "rentcontrol_italia_titolo_godimento_1951_2023.csv"),
                 show_col_types = FALSE) %>%
  transmute(anno, Affitto = affitto_pct, `Proprietà` = proprieta_pct)

c2021 <- read_csv(file.path(IN, "demo_censimenti_titolo_godimento_1951_2023.csv"),
                  show_col_types = FALSE) %>%
  filter(anno == 2021) %>%
  transmute(anno, Affitto = quota_affitto_pct, `Proprietà` = NA_real_)

# Controllo di coerenza tra i due file sugli anni in comune
chk <- read_csv(file.path(IN, "demo_censimenti_titolo_godimento_1951_2023.csv"), show_col_types = FALSE) %>%
  inner_join(base, by = "anno") %>% filter(!is.na(quota_affitto_pct))
stopifnot(all(abs(chk$quota_affitto_pct - chk$Affitto) < 0.05))

serie <- bind_rows(base, c2021) %>%
  arrange(anno) %>%
  pivot_longer(-anno, names_to = "titolo", values_to = "quota") %>%
  filter(!is.na(quota))

write_csv(serie, file.path(OUT, "09_famiglie_in_affitto_1951_2023.csv"))

colori <- c("Affitto" = COL_ROSSO, "Proprietà" = COL_BLU)

etichette_valori <- serie %>%
  filter(anno %in% c(1951, 2023)) %>%
  mutate(lab = paste0(fmt_it(quota, 1), "%"),
         hj = if_else(anno == 1951, 0.5, 0.5),
         vj = case_when(titolo == "Affitto" & anno == 1951 ~ -1.1,
                        titolo == "Proprietà" & anno == 1951 ~ 2.0,
                        titolo == "Affitto" ~ 2.0,
                        TRUE ~ -1.1))

etichette_serie <- tibble(
  titolo = c("Affitto", "Proprietà"),
  anno = c(1984, 1951),
  quota = c(41, 62),
  lab = c("Famiglie in affitto", "Famiglie proprietarie")
)

p <- ggplot(serie, aes(anno, quota, colour = titolo)) +
  geom_vline(xintercept = 2006, colour = COL_GRIGIO, linewidth = 0.4, linetype = "dashed") +
  annotate("text", x = 2006, y = 84, label = "Fino al 2001 abitazioni occupate,\ndal 2011 famiglie",
           hjust = 1.04, vjust = 1, family = "Source Sans Pro", size = 3.1,
           colour = COL_GRIGIO_SCURO, lineheight = 0.95) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_text(data = etichette_valori, aes(label = lab, vjust = vj),
            family = "Source Sans Pro", fontface = "bold", size = 3.5) +
  geom_text(data = etichette_serie, aes(label = lab), hjust = 0,
            family = "Source Sans Pro", fontface = "bold", size = 3.6) +
  scale_colour_manual(values = colori) +
  scale_x_continuous(breaks = c(1951, 1961, 1971, 1981, 1991, 2001, 2011, 2023),
                     limits = c(1949, 2025), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 85), breaks = seq(10, 80, 10),
                     labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(title = "In settant'anni l'affitto è passato da metà a un quinto",
       subtitle = "Quota di famiglie in affitto e in proprietà, Italia, censimenti 1951-2023",
       caption = "Elaborazione di Lorenzo Ruffino su dati Istat") +
  theme_linechart() +
  theme(legend.position = "none")

salva(p, "09_famiglie_in_affitto_1951_2023.png", width = 8, height = 6.5)

serie %>% pivot_wider(names_from = titolo, values_from = quota) %>% print()
