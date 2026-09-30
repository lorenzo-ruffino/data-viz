# script/popolazione_per_ripartizione.R — eseguito da `cd script && Rscript popolazione_per_ripartizione.R`
#
# Popolazione residente per ripartizione (Nord-ovest, Nord-est, Centro,
# Mezzogiorno): ai censimenti 1901-1951, al 1° gennaio di ogni anno dal 1952 al
# 2026, e previsioni Istat (base 2024, scenario mediano) fino al 2080.
#
# Fonti (tutte Istat):
#  - input/istat_serie_storiche_tavola_2_1_1.xls — serie storiche, tav. 2.1.1:
#      popolazione residente per regione e ripartizione ai censimenti 1861-2011
#  - input/istat_serie_storiche_tavola_2_1.xls   — serie storiche, tav. 2.1:
#      popolazione residente nazionale ai confini dell'epoca e a quelli attuali
#  - input/istat_164_346.csv — ricostruzione intercensuaria 1952-1971  (SDMX)
#  - input/istat_164_347.csv — ricostruzione intercensuaria 1972-1981  (SDMX)
#  - input/istat_164_279.csv — ricostruzione intercensuaria 1982-1991  (SDMX)
#  - input/istat_164_305.csv — ricostruzione intercensuaria 1991-2001  (SDMX)
#  - input/istat_164_164.csv — ricostruzione intercensuaria 2002-2019  (SDMX)
#  - input/istat_22_289.csv  — popolazione residente al 1° gennaio 2019-2026
#  - input/istat_165_889.csv — previsioni della popolazione 2024-2080, mediana
#
# Confini. La tav. 2.1.1 è ricostruita sui comuni ai confini attuali (nota b),
# quindi i territori ceduti nel 1947 (Istria, Fiume, Zara) sono già esclusi da
# tutti gli anni: il Friuli-Venezia Giulia vale 1.178 mila nel 1921, 1.176 nel
# 1931 e 1.226 nel 1951, cioè sempre il suo territorio odierno. Restano invece
# fuori, nel 1901 e nel 1911, i comuni non ancora italiani — Trentino-Alto
# Adige, Trieste e Gorizia. Il correttivo è quello ufficiale Istat: la tav. 2.1
# dà la popolazione nazionale sia ai confini dell'epoca sia a quelli attuali, e
# la differenza (+813 mila nel 1901, +1.076 mila nel 1911) è per intero
# territorio di Nord-est, a cui viene quindi riattribuita.

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(showtext)
})

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_VIOLA  <- "#A82DE3"
COL_ROSSO  <- "#F12938"
COL_GIALLO <- "#F2A900"
COL_GRIGIO <- "#9A9A9A"
CAP_ISTAT  <- "Elaborazione di Lorenzo Ruffino su dati Istat"

theme_linechart <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro"),
      legend.position = "top",
      axis.line = element_line(linewidth = 0.3),
      axis.text = element_text(size = 9, color = COL_NERO, hjust = 0.5),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.key = element_blank(),
      panel.border = element_blank(),
      legend.title = element_blank(),
      plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), "cm"),
      plot.title.position = "plot",
      legend.text = element_text(size = 10, color = COL_NERO, hjust = 0),
      plot.title = element_text(family = "Source Sans Pro SemiBold",
                                size = 14, color = COL_NERO, hjust = 0,
                                margin = margin(b = 0.1, unit = "cm")),
      plot.subtitle = element_text(size = 9, color = COL_NERO, hjust = 0,
                                   lineheight = 1.35,
                                   margin = margin(b = 0.25, t = 0.1, unit = "cm")),
      plot.caption = element_text(size = 9, color = COL_NERO, hjust = 1,
                                  margin = margin(t = 0.5, unit = "cm")),
      ...
    )
}

input_dir  <- "../input"
output_dir <- "../output"

RIPARTIZIONI <- c("ITC"  = "Nord-ovest",
                  "ITD"  = "Nord-est",
                  "ITE"  = "Centro",
                  "ITFG" = "Mezzogiorno")

# Il 1921 è escluso: è l'unico censimento della tavola su una base diversa dagli
# altri. Sommando le ripartizioni dà 39,4 milioni contro i 37,9 del totale
# nazionale ai confini attuali (+4,1%, gli stessi 1,5 milioni della nota c della
# tavola), e a livello regionale produce artefatti evidenti — la Sicilia passa
# da 4,2 milioni nel 1921 a 3,9 nel 1931. Il 1891 e il 1941 mancano perché quei
# censimenti non furono mai svolti.
CENSIMENTI <- c(1901, 1911, 1931, 1936, 1951)

# 1) Censimenti 1901-1951 (serie storiche, valori in migliaia) ------------------

tav_2_1_1 <- read_excel(file.path(input_dir, "istat_serie_storiche_tavola_2_1_1.xls"),
                        sheet = "Tavola 2.1.1 (segue 2)",
                        col_names = FALSE, .name_repair = "minimal")[6:23, 1:8]
names(tav_2_1_1) <- c("anno", "Nord-ovest", "Nord-est", "Centro",
                      "Sud", "Isole", "italia_somma", "italia")

censimenti <- tav_2_1_1 %>%
  filter(grepl("^[0-9]{4}", anno)) %>%
  mutate(anno = as.integer(substr(anno, 1, 4)),
         across(-anno, ~ as.numeric(.x) * 1000)) %>%
  filter(anno %in% CENSIMENTI) %>%
  transmute(anno, `Nord-ovest`, `Nord-est`, Centro,
            Mezzogiorno = Sud + Isole)

# Correttivo confini attuali per il Nord-est nel 1901 e nel 1911: differenza fra
# popolazione nazionale ai confini attuali e ai confini dell'epoca (tav. 2.1).
tav_2_1 <- read_excel(file.path(input_dir, "istat_serie_storiche_tavola_2_1.xls"),
                      sheet = "Tavola 2.1",
                      col_names = FALSE, .name_repair = "minimal")[8:25, c(1, 4, 8)]
names(tav_2_1) <- c("anno", "epoca", "attuali")

confini <- tav_2_1 %>%
  mutate(across(everything(), ~ suppressWarnings(as.numeric(.x)))) %>%  # righe "[…]" -> NA
  filter(!is.na(anno)) %>%
  mutate(anno = as.integer(anno), epoca = epoca * 1000,
         attuali = attuali * 1000, delta = attuali - epoca)

correttivo <- confini %>% filter(anno %in% c(1901, 1911)) %>% select(anno, delta)

# il correttivo esiste solo prima dell'annessione e si azzera dal 1951
stopifnot(nrow(correttivo) == 2,
          all(correttivo$delta > 700e3 & correttivo$delta < 1.2e6),
          all(confini$delta[confini$anno >= 1951] == 0))

storico <- censimenti %>%
  pivot_longer(-anno, names_to = "ripartizione", values_to = "popolazione") %>%
  left_join(correttivo, by = "anno") %>%
  mutate(popolazione = popolazione +
           if_else(ripartizione == "Nord-est" & !is.na(delta), delta, 0)) %>%
  select(-delta) %>%
  mutate(tipo = "osservato", fonte = "censimento")

stopifnot(nrow(storico) == 4 * length(CENSIMENTI))

# controllo decisivo: dopo il correttivo la somma delle quattro ripartizioni deve
# ricadere sul totale nazionale ai confini attuali della tav. 2.1 (entro lo 0,2%)
verifica_nazionale <- storico %>%
  group_by(anno) %>% summarise(somma = sum(popolazione), .groups = "drop") %>%
  left_join(confini %>% select(anno, attuali), by = "anno") %>%
  mutate(scarto = somma / attuali - 1)
print(verifica_nazionale)
stopifnot(all(abs(verifica_nazionale$scarto) < 0.002))

# 2) Serie annuale 1952-2026 (SDMX) --------------------------------------------

leggi <- function(file, anni) {
  read_csv(file.path(input_dir, file), show_col_types = FALSE,
           col_types = cols(.default = col_character())) %>%
    filter(DATA_TYPE == "JAN", REF_AREA %in% names(RIPARTIZIONI)) %>%
    transmute(anno = as.integer(substr(TIME_PERIOD, 1, 4)),   # "1952-01-01" -> 1952
              ripartizione = unname(RIPARTIZIONI[REF_AREA]),
              popolazione = as.numeric(OBS_VALUE)) %>%
    filter(anno %in% anni)
}

annuale <- bind_rows(
  leggi("istat_164_346.csv", 1952:1971),
  leggi("istat_164_347.csv", 1972:1981),
  leggi("istat_164_279.csv", 1982:1990),
  leggi("istat_164_305.csv", 1991:2001),
  leggi("istat_164_164.csv", 2002:2018),
  leggi("istat_22_289.csv",  2019:2026)
) %>%
  arrange(ripartizione, anno) %>%
  mutate(tipo = "osservato", fonte = "annuale")

# la serie annuale deve essere completa e senza buchi per ogni ripartizione
stopifnot(
  nrow(annuale) == 4 * length(1952:2026),
  !any(duplicated(annuale[c("anno", "ripartizione")])),
  all(sort(unique(annuale$anno)) == 1952:2026)
)

# il censimento 1951 e il 1° gennaio 1952 devono coincidere entro l'1%
raccordo_51_52 <- storico %>% filter(anno == 1951) %>%
  select(ripartizione, cens = popolazione) %>%
  left_join(annuale %>% filter(anno == 1952) %>%
              select(ripartizione, ann = popolazione), by = "ripartizione")
stopifnot(all(abs(raccordo_51_52$ann / raccordo_51_52$cens - 1) < 0.01))

# 3) Previsioni ----------------------------------------------------------------

previsione <- leggi("istat_165_889.csv", 2027:2080) %>%
  mutate(tipo = "previsione", fonte = "previsione")

stopifnot(nrow(previsione) == 4 * length(2027:2080))

serie <- bind_rows(storico, annuale, previsione) %>%
  arrange(ripartizione, anno)

write_csv(serie, file.path(output_dir, "popolazione_per_ripartizione.csv"))

# 4) Controlli e numeri per il testo -------------------------------------------

fmt_mln <- function(x) paste0(formatC(x / 1e6, format = "f", digits = 1,
                                      decimal.mark = ","), " mln")

riepilogo <- serie %>%
  filter(anno %in% c(1901, 1951, 2026, 2080)) %>%
  select(ripartizione, anno, popolazione) %>%
  pivot_wider(names_from = anno, values_from = popolazione, names_prefix = "a") %>%
  mutate(var_01_26 = a2026 / a1901 - 1,
         var_26_80 = a2080 / a2026 - 1,
         var_01_80 = a2080 / a1901 - 1)

print(riepilogo %>% mutate(across(starts_with("a"), fmt_mln),
                           across(starts_with("var"),
                                  ~ paste0(round(.x * 100, 1), "%"))))

picchi <- serie %>% filter(tipo == "osservato") %>%
  group_by(ripartizione) %>% slice_max(popolazione, n = 1)
cat("\nPicco per ripartizione:\n"); print(picchi)

cat("\nTotale Italia 1901:", fmt_mln(sum(riepilogo$a1901)),
    "| 2026:", fmt_mln(sum(riepilogo$a2026)),
    "| 2080:", fmt_mln(sum(riepilogo$a2080)), "\n")

# anno in cui il Nord-ovest supera il Mezzogiorno
sorpasso <- serie %>%
  filter(ripartizione %in% c("Nord-ovest", "Mezzogiorno")) %>%
  pivot_wider(id_cols = anno, names_from = ripartizione, values_from = popolazione) %>%
  filter(`Nord-ovest` > Mezzogiorno) %>%
  slice_min(anno, n = 1)
cat("Nord-ovest supera il Mezzogiorno nel", sorpasso$anno,
    "(", fmt_mln(sorpasso$`Nord-ovest`), "contro", fmt_mln(sorpasso$Mezzogiorno), ")\n")

# ultimo anno in cui il Mezzogiorno è sopra il proprio livello del 1901
soglia_1901 <- serie %>%
  filter(ripartizione == "Mezzogiorno") %>%
  mutate(sopra_1901 = popolazione > popolazione[anno == 1901]) %>%
  filter(!sopra_1901, anno > 1901) %>%
  slice_min(anno, n = 1)
cat("Il Mezzogiorno torna sotto il livello del 1901 nel", soglia_1901$anno,
    "(", fmt_mln(soglia_1901$popolazione), ")\n")

# 5) Grafico -------------------------------------------------------------------

# Nessun punto di raccordo: se il 2026 entrasse anche nella serie tratteggiata,
# il primo trattino partirebbe dalla verticale e si leggerebbe come una
# prosecuzione della linea continua. Meglio lo stacco di un anno fra l'ultimo
# osservato (1° gennaio 2026) e la prima previsione (1° gennaio 2027), con la
# verticale a metà.
STACCO <- 2026.5

plot_data <- serie %>%
  mutate(pop_mln = popolazione / 1e6,
         ripartizione = factor(ripartizione, levels = RIPARTIZIONI))

colori <- c("Nord-ovest"  = COL_BLU,
            "Nord-est"    = COL_VIOLA,
            "Centro"      = COL_GIALLO,
            "Mezzogiorno" = COL_ROSSO)

# Etichette inline su due righe. Sono due geom_text distinti e non un unico
# testo con "\n": ggrepel e geom_text centrano le righe di un testo multiriga
# anche con hjust = 0, mentre qui il nome e il valore vanno allineati a sinistra
# sullo stesso margine. Le posizioni sono fisse (niente repel): i quattro
# arrivi al 2080 distano abbastanza da non collidere.
X_ETICHETTA  <- 2083
SIZE_ETICH   <- 2.9
DELTA_RIGA   <- 0.40   # milioni: mezza interlinea sopra e sotto il centro
# I tre arrivi bassi (11,9 / 10,5 / 9,3) distano meno di due interlinee, quindi
# le etichette vanno scostate un po' l'una dall'altra o si toccano.
SCOSTAMENTO  <- c("Nord-ovest" = 0, "Mezzogiorno" = 0.15,
                  "Nord-est" = 0, "Centro" = -0.30)

etichette <- plot_data %>%
  filter(anno == 2080) %>%
  mutate(centro = pop_mln + unname(SCOSTAMENTO[as.character(ripartizione)]),
         y_nome = centro + DELTA_RIGA,
         y_valore = centro - DELTA_RIGA,
         valore = fmt_mln(popolazione))

p <- ggplot(plot_data,
            aes(x = anno, y = pop_mln,
                colour = ripartizione, group = interaction(ripartizione, tipo))) +
  geom_vline(xintercept = STACCO, colour = COL_GRIGIO,
             linewidth = 0.4, linetype = "dashed") +
  geom_line(aes(linetype = tipo), linewidth = 0.9) +
  geom_text(data = etichette, aes(x = X_ETICHETTA, y = y_nome,
                                  label = as.character(ripartizione)),
            hjust = 0, size = SIZE_ETICH, fontface = "bold",
            family = "Source Sans Pro", show.legend = FALSE) +
  geom_text(data = etichette, aes(x = X_ETICHETTA, y = y_valore, label = valore),
            hjust = 0, size = SIZE_ETICH, fontface = "bold",
            family = "Source Sans Pro", show.legend = FALSE) +
  annotate("text", x = STACCO - 4, y = 24.5, label = "Dato osservato",
           hjust = 1, family = "Source Sans Pro", fontface = "bold",
           size = 3.4, colour = COL_GRIGIO) +
  annotate("text", x = STACCO + 4, y = 24.5, label = "Previsioni Istat",
           hjust = 0, family = "Source Sans Pro", fontface = "bold",
           size = 3.4, colour = COL_GRIGIO) +
  scale_colour_manual(values = colori) +
  scale_linetype_manual(values = c("osservato" = "solid", "previsione" = "22")) +
  scale_x_continuous(limits = c(1898, 2105), breaks = seq(1900, 2080, 20),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 26), breaks = seq(5, 25, 5),
                     labels = function(x) paste0(x, " mln"),
                     expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  theme(legend.position = "none") +
  labs(
    title = "Il Mezzogiorno perderà quasi 8 milioni di persone",
    subtitle = "Popolazione residente per ripartizione dal 1901 al 2026 e, dal 2027, previsione mediana Istat.",
    caption = CAP_ISTAT
  )

ggsave(file.path(output_dir, "popolazione_per_ripartizione.png"),
       plot = p, width = 9, height = 6.5, units = "in", dpi = 220, bg = "white")

cat("\nGrafico salvato in ../output/popolazione_per_ripartizione.png\n")
