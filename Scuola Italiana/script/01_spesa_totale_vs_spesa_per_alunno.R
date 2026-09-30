# Scuola Italiana — 01
# Spesa pubblica per istruzione in quota del bilancio (Eurostat COFOG GF09, PC_TOT,
# S13, 2024) contro spesa annua per studente nella primaria in PPS (Eurostat
# educ_uoe_fine09, ISCED ED1, 2023). Due pannelli affiancati.
#
# NOTA sull'Unione europea nel pannello di destra: in `educ_uoe_fine09` l'aggregato
# EU27_2020 NON esiste in nessun anno per ISCED 1 in PPS (verificato scaricando il
# dataflow completo). La barra "Unione europea" è quindi una media ponderata per il
# numero di studenti ISCED 1 equivalenti a tempo pieno (Eurostat `educ_uoe_enra01`,
# unit NR, sex T, worktime TOT_FTE, sector TOT_SEC, 2023), calcolata sui 25 paesi
# UE27 con il dato disponibile (mancano Irlanda e Croazia). Il dato di appoggio è
# in `input/eurostat_educ_uoe_fine09_ED1_PPS_e_studenti_2023_EU27.csv`.

suppressMessages({
  library(tidyverse)
  library(showtext)
  library(patchwork)
})

BASE   <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Scuola Italiana"
INPUT  <- file.path(BASE, "input")
OUTPUT <- file.path(BASE, "output")

FIG_W <- 10
FIG_H <- 6.8
wrap_titolo <- function(x) str_wrap(x, width = floor(FIG_W * 9.2))
wrap_sub    <- function(x) str_wrap(x, width = floor(FIG_W * 12.3))

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_VIOLA  <- "#A82DE3"
COL_ROSSO  <- "#F12938"
COL_GIALLO <- "#F2A900"
COL_VERDE  <- "#1B9E77"

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
                                lineheight = 1.2,
                                margin = margin(b = 0.1, unit = "cm")),
      plot.subtitle = element_text(size = 9, color = COL_NERO, hjust = 0,
                                   lineheight = 1.35,
                                   margin = margin(b = 0.25, t = 0.1, unit = "cm")),
      plot.caption = element_text(size = 9, color = COL_NERO, hjust = 1,
                                  margin = margin(t = 0.5, unit = "cm")),
      ...
    )
}

CAP <- paste0("Elaborazione di Lorenzo Ruffino su dati Eurostat. ",
              "Media UE ponderata per gli studenti iscritti, 25 paesi su 27")

# Un colore distinto della palette per ogni paese, Italia in rosso.
colori_paesi <- c("Italia"         = COL_ROSSO,
                  "Unione europea" = COL_BLU,
                  "Francia"        = COL_VIOLA,
                  "Germania"       = COL_NERO,
                  "Spagna"         = COL_GIALLO,
                  "Svezia"         = COL_VERDE)

nomi <- c("IT" = "Italia", "FR" = "Francia", "DE" = "Germania", "ES" = "Spagna",
          "EU27_2020" = "Unione europea", "SE" = "Svezia")

EU27 <- c("BE", "BG", "CZ", "DK", "DE", "EE", "IE", "EL", "ES", "FR", "HR", "IT",
          "CY", "LV", "LT", "LU", "HU", "MT", "NL", "AT", "PL", "PT", "RO", "SI",
          "SK", "FI", "SE")

# --- Dati: quota della spesa pubblica (2024) --------------------------------

quota <- read_csv(file.path(INPUT, "eurostat_gov_10a_exp_cofog_GF09_istruzione.csv"),
                  show_col_types = FALSE) %>%
  filter(freq == "A", unit == "PC_TOT", sector == "S13",
         cofog99 == "GF09", na_item == "TE", TIME_PERIOD == 2024,
         geo %in% names(nomi)) %>%
  transmute(paese = nomi[geo], valore = OBS_VALUE) %>%
  mutate(paese = fct_reorder(paese, valore))

# --- Dati: spesa per studente nella primaria (2023, PPS) --------------------

spesa_studente <- read_csv(file.path(INPUT, "eurostat_educ_uoe_fine09_spesa_per_studente.csv"),
                           show_col_types = FALSE) %>%
  filter(freq == "A", unit == "PPS", isced11 == "ED1", TIME_PERIOD == 2023,
         geo %in% names(nomi)) %>%
  transmute(paese = nomi[geo], valore = OBS_VALUE)

ue_ponderata <- read_csv(
  file.path(INPUT, "eurostat_educ_uoe_fine09_ED1_PPS_e_studenti_2023_EU27.csv"),
  show_col_types = FALSE) %>%
  filter(geo %in% EU27) %>%
  summarise(paese = "Unione europea",
            valore = sum(pps * studenti) / sum(studenti),
            n_paesi = n())

studente <- bind_rows(spesa_studente, ue_ponderata %>% select(paese, valore)) %>%
  mutate(paese = fct_reorder(paese, valore))

fmt_pct <- function(v) paste0(format(round(v, 1), nsmall = 1, decimal.mark = ","), "%")
fmt_num <- function(v) format(round(v), big.mark = ".", decimal.mark = ",")

# I titoli dei pannelli sono allineati al pannello (cioè all'asse e all'inizio
# delle barre), non al bordo della figura: plot.title.position = "panel".
etichetta_paese <- function(x) str_replace(x, "Unione europea", "Unione\neuropea")

tema_pannello <- theme_linechart() +
  theme(legend.position = "none",
        plot.title.position = "panel",
        axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 10, hjust = 1),
        plot.title = element_text(family = "Source Sans Pro SemiBold", size = 11,
                                  color = COL_NERO, hjust = 0, lineheight = 1.3,
                                  margin = margin(b = 0.3, unit = "cm")),
        plot.margin = unit(c(0.2, 0.7, 0.2, 0.1), "cm"))

p1 <- ggplot(quota, aes(x = paese, y = valore, fill = paese)) +
  geom_col(width = 0.68) +
  geom_text(aes(label = fmt_pct(valore), colour = paese),
            hjust = -0.18, family = "Source Sans Pro", fontface = "bold", size = 3.4) +
  scale_fill_manual(values = colori_paesi) +
  scale_colour_manual(values = colori_paesi) +
  scale_x_discrete(labels = etichetta_paese) +
  scale_y_continuous(limits = c(0, 17), expand = c(0, 0)) +
  coord_flip(clip = "off") +
  tema_pannello +
  labs(title = "Quota della spesa pubblica\ndestinata all'istruzione")

p2 <- ggplot(studente, aes(x = paese, y = valore, fill = paese)) +
  geom_col(width = 0.68) +
  geom_text(aes(label = fmt_num(valore), colour = paese),
            hjust = -0.15, family = "Source Sans Pro", fontface = "bold", size = 3.4) +
  scale_fill_manual(values = colori_paesi) +
  scale_colour_manual(values = colori_paesi) +
  scale_x_discrete(labels = etichetta_paese) +
  scale_y_continuous(limits = c(0, 12800), expand = c(0, 0)) +
  coord_flip(clip = "off") +
  tema_pannello +
  labs(title = "Spesa annua per studente\nnella scuola primaria")

p <- (p1 | p2) +
  plot_annotation(
    title = wrap_titolo("L'Italia spende poco per l'istruzione ma tanto per ogni studente"),
    subtitle = wrap_sub(paste0(
      "A sinistra il peso dell'istruzione sulla spesa pubblica totale nel 2024, a destra la spesa ",
      "annua per ogni studente della scuola primaria a parità di potere d'acquisto nel 2023")),
    caption = CAP,
    theme = theme_linechart()
  )

ggsave(file.path(OUTPUT, "01_spesa_totale_vs_spesa_per_alunno.png"), p,
       width = FIG_W, height = FIG_H, dpi = 220, bg = "white")

write_csv(
  bind_rows(
    quota %>% mutate(indicatore = "quota della spesa pubblica (%), 2024"),
    studente %>% mutate(indicatore = "spesa per studente primaria (PPS), 2023")
  ) %>% select(indicatore, paese, valore),
  file.path(OUTPUT, "01_spesa_totale_vs_spesa_per_alunno.csv"))

cat("--- Quota spesa pubblica 2024 (%) ---\n")
print(quota %>% arrange(desc(valore)) %>% as.data.frame())
cat("--- Spesa per studente primaria 2023 (PPS) ---\n")
print(studente %>% arrange(desc(valore)) %>% as.data.frame())
cat("Media UE ponderata su", ue_ponderata$n_paesi, "paesi:",
    round(ue_ponderata$valore, 1), "PPS\n")
