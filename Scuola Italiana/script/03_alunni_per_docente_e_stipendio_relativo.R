# Scuola Italiana — 03
# Scatter: alunni per docente nella scuola primaria (asse x) contro stipendio
# previsto dal contratto a 15 anni di carriera in rapporto al salario medio (asse y).
#
# Asse x: Eurostat `educ_uoe_perp04` (ISCED ED1, unità RT), anno 2023. Il file
#   `eurostat_educ_uoe_perp04_rapporto_studenti_docenti.csv` presente in input
#   copre solo 9 paesi, quindi la serie è stata riscaricata per tutti i paesi con
#   `eurostat::get_eurostat()` e salvata in
#   `input/eurostat_educ_uoe_perp04_ED1_tutti_i_paesi.csv`.
#   (La fonte OCSE `oecd_EAG_UOE_PERS_rapporto_studenti_docenti_2013_2024.csv` ha
#   solo 11 paesi: dà per l'Italia 2023 un valore quasi identico, 10,71.)
# Asse y: `calc_stipendio_docenti_su_salario_medio.csv`, edizione EAG 2025
#   (rif. 2022), tipo_stipendio = statutario, livello ISCED11_1, fase EXP15.

suppressMessages({
  library(tidyverse)
  library(showtext)
  library(ggrepel)
})

BASE   <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Scuola Italiana"
INPUT  <- file.path(BASE, "input")
OUTPUT <- file.path(BASE, "output")

FIG_W <- 9.5
FIG_H <- 8
wrap_titolo <- function(x) str_wrap(x, width = floor(FIG_W * 9.2))
wrap_sub    <- function(x) str_wrap(x, width = floor(FIG_W * 12.3))

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO         <- "#1C1C1C"
COL_ROSSO        <- "#F12938"
COL_GRIGIO       <- "#9A9A9A"
COL_BLU          <- "#0478EA"
COL_AZZURRO      <- "#3686D6"
COL_GRIGIO_LABEL <- "#7E7E7E"

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

CAP <- "Elaborazione di Lorenzo Ruffino su dati Eurostat e OCSE"

# --- Dati -------------------------------------------------------------------

ANNO_RATIO <- 2023

# codice ISO3 (OCSE) -> codice Eurostat
iso3_geo <- c(AUT = "AT", BEL = "BE", BGR = "BG", HRV = "HR", CYP = "CY",
              CZE = "CZ", DNK = "DK", EST = "EE", FIN = "FI", FRA = "FR",
              DEU = "DE", GRC = "EL", HUN = "HU", IRL = "IE", ITA = "IT",
              LVA = "LV", LTU = "LT", LUX = "LU", MLT = "MT", NLD = "NL",
              POL = "PL", PRT = "PT", ROU = "RO", SVK = "SK", SVN = "SI",
              ESP = "ES", SWE = "SE", ISL = "IS", NOR = "NO", TUR = "TR",
              GBR = "UK", CHE = "CH")

nomi_it <- c(AT = "Austria", BE = "Belgio", BG = "Bulgaria", HR = "Croazia",
             CY = "Cipro", CZ = "Cechia", DK = "Danimarca", EE = "Estonia",
             FI = "Finlandia", FR = "Francia", DE = "Germania", EL = "Grecia",
             HU = "Ungheria", IE = "Irlanda", IT = "Italia", LV = "Lettonia",
             LT = "Lituania", LU = "Lussemburgo", MT = "Malta",
             NL = "Paesi Bassi", PL = "Polonia", PT = "Portogallo",
             RO = "Romania", SK = "Slovacchia", SI = "Slovenia", ES = "Spagna",
             SE = "Svezia", IS = "Islanda", NO = "Norvegia", TR = "Turchia",
             UK = "Regno Unito", CH = "Svizzera")

alunni <- read_csv(file.path(INPUT, "eurostat_educ_uoe_perp04_ED1_tutti_i_paesi.csv"),
                   show_col_types = FALSE) %>%
  filter(TIME_PERIOD == ANNO_RATIO, !is.na(values)) %>%
  transmute(geo, alunni_per_docente = values)

stipendi <- read_csv(file.path(INPUT, "calc_stipendio_docenti_su_salario_medio.csv"),
                     show_col_types = FALSE) %>%
  filter(edizione == "EAG 2025 (rif. 2022)", tipo_stipendio == "statutario",
         livello == "ISCED11_1", fase == "EXP15", !is.na(rapporto)) %>%
  transmute(geo = iso3_geo[codice], rapporto) %>%
  filter(!is.na(geo))

dati <- inner_join(stipendi, alunni, by = "geo") %>%
  mutate(paese = nomi_it[geo],
         italia = geo == "IT")

media_x <- mean(dati$alunni_per_docente)
media_y <- mean(dati$rapporto)

fmt_dec <- function(v) format(round(v, 1), nsmall = 1, decimal.mark = ",")
fmt_rap <- function(v) format(round(v, 2), nsmall = 2, decimal.mark = ",")

p <- ggplot(dati, aes(x = alunni_per_docente, y = rapporto)) +
  geom_hline(yintercept = media_y, colour = COL_BLU,
             linewidth = 0.4, linetype = "dashed") +
  geom_vline(xintercept = media_x, colour = COL_BLU,
             linewidth = 0.4, linetype = "dashed") +
  annotate("text", x = max(dati$alunni_per_docente), y = media_y,
           label = paste0("media dei paesi  ", fmt_rap(media_y)),
           hjust = 1, vjust = -0.7, size = 3, colour = COL_BLU,
           fontface = "bold", family = "Source Sans Pro") +
  annotate("text", x = media_x, y = min(dati$rapporto),
           label = paste0("media dei paesi  ", fmt_dec(media_x)),
           hjust = -0.05, vjust = 0, size = 3, colour = COL_BLU,
           fontface = "bold", family = "Source Sans Pro") +
  geom_point(aes(colour = italia, size = italia)) +
  geom_text_repel(aes(label = paese,
                      colour = ifelse(italia, "italia_lab", "altri_lab"),
                      fontface = ifelse(italia, "bold", "plain")),
                  size = 3.1, family = "Source Sans Pro",
                  segment.colour = COL_GRIGIO, min.segment.length = 0,
                  box.padding = 0.3, max.overlaps = 30, seed = 1) +
  scale_colour_manual(values = c("FALSE" = COL_AZZURRO, "TRUE" = COL_ROSSO,
                                 "altri_lab" = COL_GRIGIO_LABEL,
                                 "italia_lab" = COL_ROSSO)) +
  scale_size_manual(values = c("FALSE" = 2.2, "TRUE" = 3.4)) +
  scale_x_continuous(limits = c(7.5, 19.5), breaks = seq(8, 19, 2),
                     labels = function(x) format(x, decimal.mark = ",")) +
  scale_y_continuous(limits = c(0.56, 1.50), breaks = seq(0.6, 1.5, 0.15),
                     labels = function(x) format(round(x, 2), nsmall = 2,
                                                 decimal.mark = ",")) +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 10, color = COL_NERO, hjust = 0.5,
                                    margin = margin(t = 0.3, unit = "cm")),
        axis.title.y = element_text(size = 10, color = COL_NERO, hjust = 0.5,
                                    angle = 90, margin = margin(r = 0.3, unit = "cm"))) +
  labs(x = "Alunni per docente nella scuola primaria",
       y = "Stipendio contrattuale del docente rispetto al salario medio del paese",
       title = wrap_titolo("L'Italia ha pochi alunni per insegnante e stipendi bassi"),
       subtitle = wrap_sub(paste0(
         "Alunni per docente nella scuola primaria nel 2023 e stipendio previsto dal contratto per un ",
         "docente con 15 anni di carriera in rapporto al salario medio nazionale nel 2022, paesi europei")),
       caption = CAP)

ggsave(file.path(OUTPUT, "03_alunni_per_docente_e_stipendio_relativo.png"), p,
       width = FIG_W, height = FIG_H, dpi = 220, bg = "white")

write_csv(dati %>% select(paese, geo, alunni_per_docente, rapporto) %>%
            arrange(alunni_per_docente),
          file.path(OUTPUT, "03_alunni_per_docente_e_stipendio_relativo.csv"))

cat("--- n paesi:", nrow(dati), "---\n")
cat("Italia: alunni per docente", round(dati$alunni_per_docente[dati$italia], 2),
    "| rapporto", round(dati$rapporto[dati$italia], 3), "\n")
cat("Medie: alunni per docente", round(media_x, 2), "| rapporto", round(media_y, 3), "\n")
print(dati %>% select(paese, alunni_per_docente, rapporto) %>%
        arrange(alunni_per_docente) %>% as.data.frame())
