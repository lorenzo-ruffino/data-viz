# Grafico 06 - Studenti universitari residenti fuori regione ogni 100 famiglie
# in affitto, per città universitaria, a.a. 2024/25.
# Studenti: iscritti alle sedi della provincia (USTAT-MUR, Anagrafe nazionale
# studenti); famiglie in affitto: comune capoluogo, Censimento permanente 2021.
# Escluse Urbino e Rende: il comune è molto più piccolo del bacino di sedi
# della provincia (Pesaro-Urbino) o gli studenti si dividono con Cosenza, e il
# rapporto risulta gonfiato.
# Input: studenti_fuori_sede_citta.csv

source("grafici_00_tema.R")

CITTA <- c("Ferrara", "Pisa", "Siena", "Macerata", "Pavia", "Padova", "Perugia",
           "Bologna", "Trento", "Parma", "Torino", "Milano", "Firenze", "Roma", "Napoli")

stud <- read_csv(file.path(IN, "studenti_fuori_sede_citta.csv"), show_col_types = FALSE) %>%
  filter(citta %in% CITTA) %>%
  mutate(valore = fuori_regione_residenza / famiglie_affitto_comune_2021 * 100,
         citta = fct_reorder(citta, valore),
         colore = if_else(citta %in% c("Milano", "Roma", "Torino", "Napoli", "Firenze"),
                          COL_BLU, COL_ROSSO))

stopifnot(nrow(stud) == length(CITTA))
write_csv(stud %>% select(citta, anno_accademico, fuori_regione_residenza,
                          famiglie_affitto_comune_2021, valore),
          file.path(OUT, "06_studenti_fuori_regione_famiglie_affitto.csv"))

p <- ggplot(stud, aes(valore, citta)) +
  geom_vline(xintercept = 100, colour = COL_GRIGIO, linewidth = 0.4, linetype = "dashed") +
  geom_col(aes(fill = colore), width = 0.7) +
  geom_label(aes(x = valore + 0.8, label = fmt_it(valore)), hjust = 0,
             family = "Source Sans Pro", fontface = "bold", size = 3.4, colour = COL_NERO,
             fill = "white", label.size = 0, label.padding = unit(0.08, "lines")) +
  annotate("text", x = 100, y = 0.2, label = "Uno studente per famiglia in affitto",
           hjust = 1.03, family = "Source Sans Pro", size = 3.1, colour = COL_GRIGIO_SCURO) +
  scale_fill_identity() +
  scale_x_continuous(limits = c(0, 112), expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(add = c(0.9, 0.6))) +
  coord_cartesian(clip = "off") +
  labs(title = "Gli studenti fuori sede pesano di più nelle città medie",
       subtitle = "Studenti universitari residenti fuori regione ogni 100 famiglie in affitto, per città,\nanno accademico 2024/25",
       caption = "Elaborazione di Lorenzo Ruffino su dati Mur e Istat") +
  theme_linechart() +
  theme(legend.position = "none",
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10, hjust = 1))

salva(p, "06_studenti_fuori_regione_famiglie_affitto.png", width = 8, height = 7)

stud %>% arrange(desc(valore)) %>%
  transmute(citta, fuori_regione_residenza, famiglie_affitto_comune_2021, valore = round(valore, 1)) %>%
  print(n = 20)
