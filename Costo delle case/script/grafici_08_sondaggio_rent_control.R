# Grafico 08 - Sondaggio del Clark Center (Chicago Booth, panel di economisti
# statunitensi, 7 febbraio 2012) sul controllo degli affitti: distribuzione delle
# risposte ponderate per la confidenza dichiarata, con il numero di economisti.
# Statement: "Local ordinances that limit rent increases for some rental housing
# units, such as in New York and San Francisco, have had a positive impact over
# the past three decades on the amount and quality of broadly affordable rental
# housing in cities that have used them."
# Input: rentcontrol_clark_sondaggi.csv

source("grafici_00_tema.R")

LIVELLI <- c("Molto d'accordo", "D'accordo", "Incerto", "In disaccordo", "Molto in disaccordo")

clark <- read_csv(file.path(IN, "rentcontrol_clark_sondaggi.csv"), show_col_types = FALSE) %>%
  filter(sondaggio == "rent-control", domanda == "A", risposta_it %in% LIVELLI) %>%
  transmute(risposta = factor(risposta_it, levels = rev(LIVELLI)),
            n, ponderata = pct_ponderata_confidenza) %>%
  mutate(colore = case_when(str_detect(risposta, "disaccordo") ~ COL_ROSSO,
                            risposta == "Incerto" ~ COL_GRIGIO,
                            TRUE ~ COL_BLU),
         lab_n = case_when(n == 0 ~ "nessun economista",
                           n == 1 ~ "1 economista",
                           TRUE ~ paste(n, "economisti")))

stopifnot(sum(clark$ponderata) == 100, sum(clark$n) == 37)
write_csv(clark %>% select(risposta, n, ponderata), file.path(OUT, "08_sondaggio_clark_rent_control.csv"))

p <- ggplot(clark, aes(ponderata, risposta)) +
  geom_col(aes(fill = colore), width = 0.68) +
  geom_text(aes(label = paste0(ponderata, "%"), colour = colore), hjust = -0.25,
            family = "Source Sans Pro", fontface = "bold", size = 4.2) +
  geom_text(aes(x = ponderata + 1.6 + nchar(paste0(ponderata, "%")) * 1.6, label = lab_n), hjust = 0,
            family = "Source Sans Pro", size = 3.4, colour = COL_GRIGIO_SCURO) +
  scale_fill_identity() +
  scale_colour_identity() +
  scale_x_continuous(limits = c(0, 75), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(title = "Gli economisti bocciano il tetto agli affitti",
       subtitle = "Risposte degli economisti del panel Clark Center alla domanda se i limiti agli aumenti degli affitti di New York\ne San Francisco abbiano reso più accessibili le case in affitto, ponderate per la confidenza, febbraio 2012",
       caption = "Elaborazione di Lorenzo Ruffino su dati Clark Center Forum (Chicago Booth)") +
  theme_linechart() +
  theme(legend.position = "none",
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10.5, hjust = 1))

salva(p, "08_sondaggio_clark_rent_control.png", width = 9, height = 6.5)

print(clark %>% select(risposta, n, ponderata))
cat("In disaccordo (ponderato):", sum(clark$ponderata[str_detect(clark$risposta, "disaccordo")]), "%\n")
