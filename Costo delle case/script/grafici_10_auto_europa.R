# Grafico 10 - Automobili ogni 1.000 abitanti nei paesi Ue, 2025
# (Eurostat road_eqs_carhab).
# Input: trasporti_auto_per_1000_paesi.csv

source("grafici_00_tema.R")

ANNO <- 2025

nomi_paesi <- c(
  AT = "Austria", BE = "Belgio", BG = "Bulgaria", CY = "Cipro", CZ = "Cechia",
  DE = "Germania", DK = "Danimarca", EE = "Estonia", EL = "Grecia", ES = "Spagna",
  FI = "Finlandia", FR = "Francia", HR = "Croazia", HU = "Ungheria", IE = "Irlanda",
  IT = "Italia", LT = "Lituania", LU = "Lussemburgo", LV = "Lettonia", MT = "Malta",
  NL = "Paesi Bassi", PL = "Polonia", PT = "Portogallo", RO = "Romania", SE = "Svezia",
  SI = "Slovenia", SK = "Slovacchia", EU27_2020 = "Unione europea"
)

auto_all <- read_csv(file.path(IN, "trasporti_auto_per_1000_paesi.csv"), show_col_types = FALSE)

auto <- auto_all %>%
  filter(anno == ANNO, geo %in% names(nomi_paesi), !is.na(auto_per_1000)) %>%
  mutate(paese = nomi_paesi[geo],
         paese = fct_reorder(paese, auto_per_1000),
         colore = case_when(geo == "IT" ~ COL_ROSSO,
                            geo == "EU27_2020" ~ COL_NERO,
                            TRUE ~ COL_BLU))

stopifnot(sum(auto$geo != "EU27_2020") == 27)
write_csv(auto %>% select(geo, paese, anno, auto_per_1000),
          file.path(OUT, "10_auto_per_1000_abitanti_europa_2025.csv"))

p <- ggplot(auto, aes(auto_per_1000, paese)) +
  geom_col(aes(fill = colore), width = 0.7) +
  geom_text(aes(label = fmt_it(auto_per_1000), colour = colore), hjust = -0.25,
            family = "Source Sans Pro", fontface = "bold", size = 3.2) +
  scale_fill_identity() +
  scale_colour_identity() +
  scale_x_continuous(limits = c(0, 780), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(title = "L'Italia è il paese Ue con più auto per abitante",
       subtitle = "Automobili ogni 1.000 abitanti, paesi dell'Unione europea, 2025",
       caption = "Elaborazione di Lorenzo Ruffino su dati Eurostat") +
  theme_linechart() +
  theme(legend.position = "none",
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 9.5, hjust = 1,
                                   colour = ifelse(levels(auto$paese) == "Italia", COL_ROSSO, COL_NERO),
                                   face = ifelse(levels(auto$paese) %in% c("Italia", "Unione europea"), "bold", "plain")))

salva(p, "10_auto_per_1000_abitanti_europa_2025.png", width = 8, height = 8)

var15 <- auto_all %>% filter(geo %in% c("IT", "EU27_2020"), anno %in% c(2015, ANNO)) %>%
  pivot_wider(names_from = anno, values_from = auto_per_1000)
print(var15)
auto %>% arrange(desc(auto_per_1000)) %>% select(paese, auto_per_1000) %>% print(n = 30)
