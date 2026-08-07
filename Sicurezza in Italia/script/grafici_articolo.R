# Grafici per l'articolo "L'Italia è un paese sempre più sicuro"
# 01 - serie lunga tasso delitti per 100k (1960-2024)
# 02 - serie lunga tasso omicidi per 100k (1960-2024)
# 03 - variazione per reato (cosa cala, cosa cresce)
# 04 - omicidi in Europa 2024, per 100k

library(tidyverse)
library(showtext)

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_ROSSO  <- "#F12938"
COL_GIALLO <- "#F2A900"
COL_GRIGIO <- "#9A9A9A"
COL_GRIGIO_SCURO <- "#5A5A5A"

CAP_ISTAT   <- "Elaborazione di Lorenzo Ruffino su dati Istat"
CAP_MISTO   <- "Elaborazione di Lorenzo Ruffino su dati Istat ed Eurostat"
CAP_EUROSTAT<- "Elaborazione di Lorenzo Ruffino su dati Eurostat"

IN  <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Sicurezza in Italia/input"
OUT <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Sicurezza in Italia/output"

theme_linechart <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro"),
      legend.position = "top",
      axis.line = element_line(linewidth = 0.3),
      axis.text = element_text(size = 9, color = "#1C1C1C", hjust = 0.5),
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
      legend.text = element_text(size = 10, color = "#1C1C1C", hjust = 0),
      plot.title = element_text(family = "Source Sans Pro SemiBold",
                                size = 14, color = "#1C1C1C", hjust = 0,
                                margin = margin(b = 0.1, unit = "cm")),
      plot.subtitle = element_text(size = 9, color = "#1C1C1C", hjust = 0,
                                   lineheight = 1.35,
                                   margin = margin(b = 0.25, t = 0.1, unit = "cm")),
      plot.caption = element_text(size = 9, color = "#1C1C1C", hjust = 1,
                                  margin = margin(t = 0.5, unit = "cm")),
      ...
    )
}

fmt_it <- function(x, dec = 0)
  format(round(x, dec), big.mark = ".", decimal.mark = ",", nsmall = dec, trim = TRUE)

# --- DATI DI BASE -----------------------------------------------------------

# Popolazione residente Italia (Eurostat demo_pjan, 1 gennaio), 1960-2025
pop <- read_csv(file.path(IN, "eurostat_demo_pjan.csv"), show_col_types = FALSE,
                col_types = cols(sex = col_character(), age = col_character())) %>%
  filter(geo == "IT", age == "TOTAL", sex == "T") %>%
  transmute(anno = as.integer(TIME_PERIOD), pop = as.numeric(popolazione)) %>%
  filter(!is.na(pop))

# Serie storiche polizia (1955-2014, long)
storiche <- read_csv(file.path(IN, "serie_storiche_delitti.csv"),
                     show_col_types = FALSE,
                     col_names = c("anno", "indicatore", "valore", "fonte"),
                     col_types = "icic", skip = 1)

# SDMX moderno (2006-2024)
sdmx <- read_csv(file.path(IN, "istat_delitti_denunciati.csv"), show_col_types = FALSE) %>%
  transmute(tipo = TYPE_CRIME, anno = as.integer(TIME_PERIOD), valore = as.numeric(OBS_VALUE))

get_sdmx <- function(code, yr) sdmx %>% filter(tipo == code, anno == yr) %>% pull(valore)

# ============================================================================
# GRAFICO 01 - Serie lunga: tasso di delitti denunciati per 100.000 ab.
# ============================================================================

tot_storico <- storiche %>%
  filter(indicatore == "totale_delitti_polizia", anno >= 1960, anno <= 2005) %>%
  select(anno, valore)
tot_moderno <- sdmx %>% filter(tipo == "TOT") %>% select(anno, valore)

tot <- bind_rows(tot_storico, tot_moderno) %>%
  distinct(anno, .keep_all = TRUE) %>%
  left_join(pop, by = "anno") %>%
  mutate(tasso = valore / pop * 1e5) %>%
  arrange(anno)

ann1 <- tot %>% filter(anno %in% c(1960, 2007, 2020, 2024)) %>%
  mutate(
    lab = paste0(fmt_it(tasso), "\n(", ifelse(anno == 1960, "1960",
          ifelse(anno == 2007, "2007", ifelse(anno == 2020, "2020", "2024"))), ")"),
    vj  = c(1.7, -0.9, 1.7, -0.9)[match(anno, c(1960, 2007, 2020, 2024))],
    hj  = c(0, 0.5, 0.5, 1)[match(anno, c(1960, 2007, 2020, 2024))]
  )

g1 <- ggplot(tot, aes(anno, tasso)) +
  geom_vline(xintercept = 2004, colour = COL_GRIGIO, linewidth = 0.4, linetype = "dashed") +
  annotate("text", x = 2003.3, y = 1050, label = "dal 2004\nnuovo sistema\ndi rilevazione",
           hjust = 1, vjust = 0, size = 2.7, colour = COL_GRIGIO_SCURO,
           family = "Source Sans Pro", lineheight = 0.95) +
  geom_line(linewidth = 1.0, colour = COL_ROSSO) +
  geom_point(data = ann1, size = 2.2, colour = COL_ROSSO) +
  geom_text(data = ann1, aes(label = lab, vjust = vj, hjust = hj),
            size = 3.1, fontface = "bold", family = "Source Sans Pro",
            colour = COL_NERO, lineheight = 0.9) +
  scale_x_continuous(breaks = seq(1960, 2020, 10), limits = c(1960, 2028),
                     expand = c(0.01, 0.01)) +
  scale_y_continuous(breaks = seq(0, 5000, 1000), limits = c(0, 6000),
                     labels = function(x) fmt_it(x), expand = c(0.01, 0.01)) +
  coord_cartesian(clip = "off") +
  labs(title = "In sessant'anni i crimini denunciati sono prima esplosi, poi calati",
       subtitle = "Crimini denunciati dalle forze di polizia all'autorità giudiziaria ogni 100.000 abitanti, Italia, 1960-2024",
       caption = CAP_MISTO) +
  theme_linechart()

ggsave(file.path(OUT, "01_delitti_tasso_lungo_periodo.png"), g1,
       width = 8, height = 6.5, dpi = 220, bg = "white")

# ============================================================================
# GRAFICO 02 - Serie lunga: tasso di omicidi volontari per 100.000 ab.
# ============================================================================

# La serie polizia degli omicidi "consumati" ha due basi non confrontabili:
# fino al 1974 e dal 1983 sono i soli consumati; tra il 1975 e il 1982 la
# fonte accorpa consumati e tentati (nota (a), Tav. 6.18). Escludo quel
# segmento e traccio due tratti distinti (seg a: 1960-1974, seg b: 1983-2024).
om_pre  <- storiche %>%
  filter(indicatore == "omicidi_volontari_consumati_polizia", anno >= 1960, anno <= 1974) %>%
  select(anno, valore) %>% mutate(seg = "a")
om_post <- storiche %>%
  filter(indicatore == "omicidi_volontari_consumati_polizia", anno >= 1983, anno <= 2005) %>%
  select(anno, valore) %>% mutate(seg = "b")
om_sdmx <- sdmx %>% filter(tipo == "INTENHOM") %>% select(anno, valore) %>% mutate(seg = "b")

om <- bind_rows(om_pre, om_post, om_sdmx) %>%
  distinct(anno, .keep_all = TRUE) %>%
  left_join(pop, by = "anno") %>%
  mutate(tasso = valore / pop * 1e5) %>%
  arrange(anno)

ann2 <- om %>% filter(anno %in% c(1991, 2024)) %>%
  mutate(lab = c(
    "1991: 1.916 omicidi\n(3,4 ogni 100 mila)",
    "2024: 326\n(0,55)"
  )[match(anno, c(1991, 2024))],
  vj = c(-0.7, -0.8)[match(anno, c(1991, 2024))],
  hj = c(0, 1)[match(anno, c(1991, 2024))])

g2 <- ggplot(om, aes(anno, tasso, group = seg)) +
  annotate("rect", xmin = 1974.5, xmax = 1982.5, ymin = 0, ymax = 4,
           fill = COL_GRIGIO, alpha = 0.13) +
  annotate("text", x = 1978.5, y = 1.15, label = "1975-1982\ndati non\nconfrontabili",
           hjust = 0.5, vjust = 1, size = 2.5, colour = COL_GRIGIO_SCURO,
           family = "Source Sans Pro", lineheight = 0.95) +
  geom_line(linewidth = 1.0, colour = COL_ROSSO) +
  geom_point(data = ann2, size = 2.2, colour = COL_ROSSO) +
  geom_text(data = ann2, aes(label = lab, vjust = vj, hjust = hj),
            size = 3.1, fontface = "bold", family = "Source Sans Pro",
            colour = COL_NERO, lineheight = 0.9) +
  scale_x_continuous(breaks = seq(1960, 2020, 10), limits = c(1960, 2026),
                     expand = c(0.01, 0.01)) +
  scale_y_continuous(breaks = seq(0, 4, 1), limits = c(0, 4),
                     labels = function(x) fmt_it(x, 1), expand = c(0.01, 0.01)) +
  coord_cartesian(clip = "off") +
  labs(title = "Gli omicidi sono ai minimi storici",
       subtitle = "Omicidi volontari consumati denunciati dalle forze di polizia ogni 100.000 abitanti, Italia, 1960-2024.\nGli anni 1975-1982 sono esclusi: la fonte non distingue i consumati dai tentati",
       caption = CAP_MISTO) +
  theme_linechart()

ggsave(file.path(OUT, "02_omicidi_tasso_lungo_periodo.png"), g2,
       width = 8, height = 6.5, dpi = 220, bg = "white")

# ============================================================================
# GRAFICO 03 - Cosa cala e cosa cresce (variazione per reato)
# ============================================================================
# Reati in calo: variazione dal picco del 2007. Reati in crescita: dal 2006,
# primo anno della serie moderna (quando erano ai minimi).

reati <- tribble(
  ~reato,                              ~code,       ~base,
  "Rapine in banca",                   "BANKROB",   2007,
  "Furti di ciclomotori",              "MOPETHEF",  2007,
  "Omicidi volontari",                 "INTENHOM",  2007,
  "Rapine",                            "ROBBER",    2007,
  "Furti di automobili",               "CARTHEF",   2007,
  "Furti (totale)",                    "THEFT",     2007,
  "Borseggi",                          "PICKTHEF",  2007,
  "Furti in abitazione",               "BURGTHEF",  2007,
  "Percosse",                          "BLOWS",     2006,
  "Violenze sessuali",                 "RAPE",      2006,
  "Estorsioni",                        "EXTORT",    2006,
  "Truffe e frodi informatiche",       "SWINCYB",   2006
) %>%
  mutate(
    v_base = map2_dbl(code, base, get_sdmx),
    v_2024 = map_dbl(code, ~ get_sdmx(.x, 2024)),
    var    = (v_2024 / v_base - 1) * 100,
    segno  = ifelse(var >= 0, "cresce", "cala")
  ) %>%
  arrange(var) %>%
  mutate(reato = factor(reato, levels = reato),
         lab = paste0(ifelse(var >= 0, "+", "−"), fmt_it(abs(var)), "%"),
         hj  = ifelse(var >= 0, -0.12, 1.12))

g3 <- ggplot(reati, aes(var, reato, fill = segno)) +
  geom_col(width = 0.72) +
  geom_vline(xintercept = 0, colour = COL_NERO, linewidth = 0.3) +
  geom_text(aes(label = lab, hjust = hj), fontface = "bold",
            family = "Source Sans Pro", size = 3.2, colour = COL_NERO) +
  scale_fill_manual(values = c("cala" = COL_BLU, "cresce" = COL_ROSSO), guide = "none") +
  scale_x_continuous(limits = c(-125, 200), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(title = "Crollano furti e rapine, crescono le truffe online",
       subtitle = "Variazione dei crimini denunciati fino al 2024, misurata dal picco del 2007 per i reati in calo e dal 2006\nper quelli in crescita. In blu i reati che diminuiscono, in rosso quelli che aumentano",
       caption = CAP_ISTAT) +
  theme_linechart() +
  theme(axis.text.y = element_text(hjust = 0, size = 10),
        axis.text.x = element_blank(),
        axis.line = element_blank())

ggsave(file.path(OUT, "03_variazione_per_reato.png"), g3,
       width = 8, height = 7.8, dpi = 220, bg = "white")

# ============================================================================
# GRAFICO 04 - Omicidi in Europa 2024 (per 100.000 ab., Eurostat ICCS)
# ============================================================================

eu27 <- c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","EL","HU",
          "IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE")
nomi <- c(AT="Austria", BE="Belgio", BG="Bulgaria", HR="Croazia", CY="Cipro",
          CZ="Cechia", DK="Danimarca", EE="Estonia", FI="Finlandia", FR="Francia",
          DE="Germania", EL="Grecia", HU="Ungheria", IE="Irlanda", IT="Italia",
          LV="Lettonia", LT="Lituania", LU="Lussemburgo", MT="Malta", NL="Paesi Bassi",
          PL="Polonia", PT="Portogallo", RO="Romania", SK="Slovacchia", SI="Slovenia",
          ES="Spagna", SE="Svezia")

eu <- read_csv(file.path(IN, "eurostat_crim_off_cat.csv"), show_col_types = FALSE) %>%
  filter(iccs == "ICCS0101", unit == "P_HTHAB", TIME_PERIOD == 2024,
         geo %in% eu27, !is.na(values)) %>%
  transmute(geo, paese = nomi[geo], tasso = as.numeric(values)) %>%
  arrange(tasso) %>%
  mutate(paese = factor(paese, levels = paese),
         is_it = geo == "IT")

media_ue <- mean(eu$tasso)

g4 <- ggplot(eu, aes(tasso, paese, fill = is_it)) +
  geom_col(width = 0.74) +
  geom_vline(xintercept = media_ue, colour = COL_GRIGIO_SCURO,
             linewidth = 0.4, linetype = "dashed") +
  annotate("text", x = media_ue + 0.03, y = 2.2,
           label = paste0("media UE ", fmt_it(media_ue, 2)),
           hjust = 0, size = 2.9, colour = COL_GRIGIO_SCURO, family = "Source Sans Pro") +
  geom_text(aes(label = fmt_it(tasso, 2), hjust = -0.15,
                colour = is_it, fontface = ifelse(eu$is_it, "bold", "plain")),
            family = "Source Sans Pro", size = 2.9) +
  scale_fill_manual(values = c(`TRUE` = COL_ROSSO, `FALSE` = COL_BLU), guide = "none") +
  scale_colour_manual(values = c(`TRUE` = COL_ROSSO, `FALSE` = COL_NERO), guide = "none") +
  scale_x_continuous(limits = c(0, 2.95), expand = c(0, 0)) +
  labs(title = "L'Italia è tra i paesi più sicuri d'Europa per gli omicidi",
       subtitle = "Omicidi volontari ogni 100.000 abitanti nei paesi dell'Unione europea, 2024",
       caption = CAP_EUROSTAT) +
  theme_linechart() +
  theme(axis.text.y = element_text(hjust = 0, size = 8.5),
        axis.text.x = element_blank(),
        axis.line = element_blank())

ggsave(file.path(OUT, "04_omicidi_europa_2024.png"), g4,
       width = 8, height = 7, dpi = 220, bg = "white")

# ============================================================================
# GRAFICO 05 - Vittimizzazione: indagine Istat 2015-16 vs 2022-23
# ============================================================================
# Fonte: Istat, "Reati contro la persona e la proprietà: vittime ed eventi.
# 2022-2023" (giugno 2025). Quota di persone vittime nei 12 mesi precedenti;
# per i furti nell'abitazione principale la quota è di famiglie.

vitt <- tribble(
  ~reato,                              ~v1516, ~v2223,
  "Reati predatori nel complesso",        3.7,    2.3,
  "Furti nell'abitazione principale*",    1.8,    0.6,
  "Borseggi",                             1.6,    1.0,
  "Furti di oggetti personali",           1.5,    1.0,
  "Rapine",                               0.5,    0.2
) %>%
  arrange(v1516) %>%
  mutate(reato = factor(reato, levels = reato))

fmt_pct5 <- function(v) ifelse(v %% 1 == 0, paste0(as.integer(v), "%"),
                               paste0(formatC(v, format = "f", digits = 1, decimal.mark = ","), "%"))

top_row <- vitt %>% filter(v1516 == max(v1516))

g5 <- ggplot(vitt) +
  geom_segment(aes(x = v1516, xend = v2223, y = reato, yend = reato),
               colour = COL_BLU, linewidth = 0.9,
               arrow = arrow(length = unit(0.22, "cm"), type = "closed")) +
  geom_text(aes(v1516, reato, label = fmt_pct5(v1516)), hjust = -0.45,
            colour = COL_GRIGIO_SCURO, fontface = "bold",
            family = "Source Sans Pro", size = 3.2) +
  geom_text(aes(v2223, reato, label = fmt_pct5(v2223)), hjust = 1.6,
            colour = COL_BLU, fontface = "bold",
            family = "Source Sans Pro", size = 3.2) +
  geom_text(data = top_row, aes(v1516, reato, label = "2015-2016"),
            vjust = -1.6, colour = COL_GRIGIO_SCURO, fontface = "bold",
            family = "Source Sans Pro", size = 3.1) +
  geom_text(data = top_row, aes(v2223, reato, label = "2022-2023"),
            vjust = -1.6, colour = COL_BLU, fontface = "bold",
            family = "Source Sans Pro", size = 3.1) +
  scale_x_continuous(limits = c(-0.15, 4.35), expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(add = c(0.4, 0.9))) +
  coord_cartesian(clip = "off") +
  labs(title = "Il calo dei reati si vede anche tra le vittime",
       subtitle = "Quota di persone che hanno subìto il reato nei 12 mesi precedenti l'intervista (*per i furti in casa:\nquota di famiglie), indagine Istat sulla sicurezza dei cittadini, edizioni 2015-2016 e 2022-2023",
       caption = CAP_ISTAT) +
  theme_linechart() +
  theme(axis.text.y = element_text(hjust = 0, size = 10),
        axis.text.x = element_blank(),
        axis.line = element_blank())

ggsave(file.path(OUT, "05_vittimizzazione.png"), g5,
       width = 8, height = 5.2, dpi = 220, bg = "white")

# --- esporta i dati puliti --------------------------------------------------
write_csv(tot   %>% select(anno, delitti = valore, pop, tasso), file.path(OUT, "tasso_delitti_1960_2024.csv"))
write_csv(om    %>% select(anno, omicidi = valore, pop, tasso), file.path(OUT, "tasso_omicidi_1960_2024.csv"))
write_csv(reati %>% select(reato, base, v_base, v_2024, var),   file.path(OUT, "variazione_per_reato.csv"))
write_csv(eu    %>% select(geo, paese, tasso),                  file.path(OUT, "omicidi_europa_2024.csv"))
write_csv(vitt  %>% select(reato, v1516, v2223),                file.path(OUT, "vittimizzazione_2015_2023.csv"))

cat("Fatto. Tasso 2007:", round(tot$tasso[tot$anno==2007]),
    "| 2024:", round(tot$tasso[tot$anno==2024]),
    "| omicidi 1991:", round(om$tasso[om$anno==1991],2),
    "2024:", round(om$tasso[om$anno==2024],2),
    "| media UE omicidi:", round(media_ue,2), "\n")
