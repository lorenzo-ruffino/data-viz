# Grafico 07 - Annunci Airbnb ogni 100 abitazioni nel centro storico e nel resto
# della città (Inside Airbnb, giugno 2026; abitazioni dal censimento Istat 2021).
# Definizioni del centro: Firenze Quartiere 1; Roma 22 rioni storici; Napoli
# quartieri del centro antico; Milano Municipio 1; Bologna zone dentro i viali;
# Venezia i sei sestieri (San Marco, Castello, Cannaregio, Dorsoduro, San Polo,
# Santa Croce), ricalcolati dal file per quartiere al posto della Municipalità
# Venezia-Murano-Burano usata nel file dei centri storici.
# Input: affittibrevi_centri_storici.csv, affittibrevi_venezia_sestieri.csv

source("grafici_00_tema.R")

SESTIERI <- c("San Marco", "Castello", "Cannaregio", "Dorsoduro", "San Polo", "Santa Croce")

centri <- read_csv(file.path(IN, "affittibrevi_centri_storici.csv"), show_col_types = FALSE) %>%
  transmute(comune,
            annunci_centro, annunci_resto, abit_centro = abitazioni_2021_centro,
            abit_resto = abitazioni_2021_resto)

# Venezia: centro = sei sestieri, resto = tutto il resto del comune
sest <- read_csv(file.path(IN, "affittibrevi_venezia_sestieri.csv"), show_col_types = FALSE) %>%
  filter(quartiere %in% SESTIERI) %>%
  summarise(annunci = sum(annunci), abitazioni = sum(abitazioni_2021))
stopifnot(nrow(read_csv(file.path(IN, "affittibrevi_venezia_sestieri.csv"), show_col_types = FALSE) %>%
                 filter(quartiere %in% SESTIERI)) == 6)

ven <- centri %>% filter(comune == "Venezia")
ven_tot_annunci <- ven$annunci_centro + ven$annunci_resto
ven_tot_abit    <- ven$abit_centro + ven$abit_resto
centri <- centri %>%
  mutate(annunci_centro = if_else(comune == "Venezia", sest$annunci, annunci_centro),
         abit_centro    = if_else(comune == "Venezia", sest$abitazioni, abit_centro),
         annunci_resto  = if_else(comune == "Venezia", ven_tot_annunci - sest$annunci, annunci_resto),
         abit_resto     = if_else(comune == "Venezia", ven_tot_abit - sest$abitazioni, abit_resto)) %>%
  mutate(centro = annunci_centro / abit_centro * 100,
         resto  = annunci_resto / abit_resto * 100,
         comune = fct_reorder(comune, centro))

write_csv(centri, file.path(OUT, "07_airbnb_centri_storici.csv"))

lunga <- centri %>%
  select(comune, centro, resto) %>%
  pivot_longer(c(centro, resto), names_to = "zona", values_to = "valore") %>%
  mutate(zona = factor(zona, levels = c("resto", "centro")))

colori <- c(centro = COL_ROSSO, resto = COL_GRIGIO)

prima <- levels(centri$comune)[nlevels(centri$comune)]
legenda <- tibble(
  comune = prima, zona = c("centro", "resto"),
  x = c(centri$centro[centri$comune == prima] + 3.6, 4.5),
  y = nlevels(centri$comune) + c(0.18, -0.18),
  lab = c("Centro storico", "Resto della città")
)

p <- ggplot(lunga, aes(valore, comune, fill = zona)) +
  geom_col(position = position_dodge(width = 0.78), width = 0.72) +
  geom_text(aes(label = fmt_it(valore, 1), colour = zona),
            position = position_dodge(width = 0.78), hjust = -0.25,
            family = "Source Sans Pro", fontface = "bold", size = 3.4) +
  geom_text(data = legenda, aes(x = x, y = y, label = lab, colour = zona),
            inherit.aes = FALSE, hjust = 0, family = "Source Sans Pro",
            fontface = "bold", size = 3.4) +
  scale_fill_manual(values = colori) +
  scale_colour_manual(values = c(centro = COL_ROSSO, resto = COL_GRIGIO_SCURO)) +
  scale_x_continuous(limits = c(0, 32), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(title = "Gli affitti brevi si concentrano nei centri storici",
       subtitle = "Annunci Airbnb ogni 100 abitazioni nel centro storico e nel resto del comune, per città, giugno 2026",
       caption = "Elaborazione di Lorenzo Ruffino su dati Inside Airbnb (CC BY 4.0) e Istat") +
  theme_linechart() +
  theme(legend.position = "none",
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10, hjust = 1))

salva(p, "07_airbnb_centri_storici.png", width = 8, height = 6.5)

centri %>% arrange(desc(centro)) %>%
  transmute(comune, centro = round(centro, 1), resto = round(resto, 1),
            rapporto = round(centro / resto, 1),
            quota_annunci_centro = round(annunci_centro / (annunci_centro + annunci_resto) * 100),
            quota_abit_centro = round(abit_centro / (abit_centro + abit_resto) * 100)) %>%
  print()
