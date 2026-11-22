# Grafico 05 - Abitazioni autorizzate in nuovi edifici residenziali ogni 1.000
# abitanti nei paesi Ue, 2025 (Eurostat sts_cobp_a e popolazione al 1° gennaio).
# Grafico 05b - La serie italiana 2000-2025 (Istat, permessi di costruire).
# Input: costruzioni_permessi_europa_per_1000_ab.csv
#        costruzioni_permessi_italia_2000_2025.csv

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

eu <- read_csv(file.path(IN, "costruzioni_permessi_europa_per_1000_ab.csv"),
               show_col_types = FALSE) %>%
  filter(anno == ANNO, geo %in% names(nomi_paesi), !is.na(abitazioni_per_1000_ab)) %>%
  mutate(paese = nomi_paesi[geo],
         valore = abitazioni_migliaia * 1000 / popolazione * 1000,
         paese = fct_reorder(paese, valore),
         colore = case_when(geo == "IT" ~ COL_ROSSO,
                            geo == "EU27_2020" ~ COL_NERO,
                            TRUE ~ COL_BLU))

stopifnot(sum(eu$geo != "EU27_2020") == 27)
write_csv(eu %>% select(geo, paese, anno, abitazioni_migliaia, popolazione, valore),
          file.path(OUT, "05_permessi_abitazioni_europa_2025.csv"))

p <- ggplot(eu, aes(valore, paese)) +
  geom_col(aes(fill = colore), width = 0.7) +
  geom_text(aes(label = fmt_it(valore, 1), colour = colore), hjust = -0.25,
            family = "Source Sans Pro", fontface = "bold", size = 3.2) +
  scale_fill_identity() +
  scale_colour_identity() +
  scale_x_continuous(limits = c(0, 23), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(title = "L'Italia è il paese Ue che autorizza meno case nuove",
       subtitle = "Abitazioni autorizzate in nuovi edifici residenziali ogni 1.000 abitanti, paesi dell'Unione europea, 2025",
       caption = "Elaborazione di Lorenzo Ruffino su dati Eurostat") +
  theme_linechart() +
  theme(legend.position = "none",
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 9.5, hjust = 1,
                                   colour = ifelse(levels(eu$paese) == "Italia", COL_ROSSO, COL_NERO),
                                   face = ifelse(levels(eu$paese) %in% c("Italia", "Unione europea"), "bold", "plain")))

salva(p, "05_permessi_abitazioni_europa_2025.png", width = 8, height = 8)

# --- 05b: la serie italiana -------------------------------------------------

ita <- read_csv(file.path(IN, "costruzioni_permessi_italia_2000_2025.csv"), show_col_types = FALSE) %>%
  mutate(valore = abitazioni / popolazione * 1000)

write_csv(ita %>% select(anno, abitazioni, popolazione, valore),
          file.path(OUT, "05b_permessi_abitazioni_italia_2000_2025.csv"))

punti <- ita %>% filter(anno %in% c(2000, 2005, 2015, 2025)) %>%
  mutate(lab = paste0(fmt_it(abitazioni / 1000), " mila"),
         hj = case_when(anno == 2000 ~ 0, anno == 2025 ~ 1, TRUE ~ 0.5),
         vj = if_else(anno %in% c(2000, 2015), 2.0, -1.2))

p2 <- ggplot(ita, aes(anno, abitazioni)) +
  geom_line(colour = COL_ROSSO, linewidth = 1) +
  geom_point(colour = COL_ROSSO, size = 1.8) +
  geom_text(data = punti, aes(label = lab, hjust = hj, vjust = vj),
            family = "Source Sans Pro", fontface = "bold", size = 3.6, colour = COL_ROSSO) +
  scale_x_continuous(breaks = seq(2000, 2025, 5), expand = c(0.02, 0.02)) +
  scale_y_continuous(limits = c(0, 320000), breaks = seq(50000, 300000, 50000),
                     labels = function(x) paste0(fmt_it(x / 1000), " mila"),
                     expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(title = "Dal 2005 i permessi per nuove case sono crollati",
       subtitle = "Abitazioni autorizzate in nuovi edifici residenziali, Italia, 2000-2025",
       caption = "Elaborazione di Lorenzo Ruffino su dati Istat") +
  theme_linechart() +
  theme(legend.position = "none")

salva(p2, "05b_permessi_abitazioni_italia_2000_2025.png", width = 8, height = 6.5)

cat("Italia 2025:", round(eu$valore[eu$geo == "IT"], 2),
    "| Ue:", round(eu$valore[eu$geo == "EU27_2020"], 2),
    "| penultimo:", as.character(eu$paese[order(eu$valore)][2]),
    round(sort(eu$valore)[2], 2), "\n")
eu %>% arrange(desc(valore)) %>% select(paese, valore) %>% mutate(valore = round(valore, 2)) %>% print(n = 30)
ita %>% filter(anno %in% c(2000, 2005, 2015, 2025)) %>% print()
