# Grafico 3 — Dumbbell: età effettiva di uscita dal lavoro (2024, uomini)
# vs età piena di pensionamento futura per chi entra oggi a 22 anni.
# Fonte: OCSE, Pensions at a Glance 2025 (stat.link).

library(tidyverse)
library(showtext)

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_ROSSO  <- "#F12938"
COL_GRIGIO <- "#9A9A9A"

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

BASE <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Pensioni in Europa"

# --- Parsing ----------------------------------------------------------------
# eta_uscita_effettiva_2024.csv: header sporco su 4 righe; le colonne dati
# sono: 1 = paese, 2 = anno, 3 = età EFFETTIVA uomini ("Men/Hommes" +
# "Effective" nelle righe 3-4 dell'header). Salto le prime 4 righe.
effettiva_raw <- read_csv(file.path(BASE, "input/eta_uscita_effettiva_2024.csv"),
                          skip = 4, col_names = FALSE, show_col_types = FALSE)
effettiva <- effettiva_raw %>%
  transmute(paese = X1, eta_effettiva = as.numeric(X3)) %>%
  filter(!is.na(paese), !is.na(eta_effettiva))

# eta_normale_futura.csv: header sporco su 3 righe; colonne: 1 = paese,
# 2 = Current, 3 = Future; 4-5 = media OCSE current/future (ripetuta su ogni
# riga). Salto le prime 3 righe.
futura_raw <- read_csv(file.path(BASE, "input/eta_normale_futura.csv"),
                       skip = 3, col_names = FALSE, show_col_types = FALSE)
futura <- futura_raw %>%
  transmute(paese = X1, eta_futura = as.numeric(X3),
            ocse_futura = as.numeric(X5)) %>%
  filter(!is.na(paese), !is.na(eta_futura))

OCSE_FUTURA <- unique(round(futura$ocse_futura, 6))
stopifnot(length(OCSE_FUTURA) == 1)

paesi_sel <- c("France"  = "Francia", "Spain" = "Spagna",
               "Germany" = "Germania", "Italy" = "Italia",
               "Sweden"  = "Svezia", "Netherlands" = "Paesi Bassi",
               "Denmark" = "Danimarca")

dati <- effettiva %>%
  inner_join(futura %>% select(paese, eta_futura), by = "paese") %>%
  filter(paese %in% names(paesi_sel)) %>%
  mutate(paese_it = paesi_sel[paese]) %>%
  bind_rows(tibble(
    paese = "OECD", paese_it = "Media OCSE",
    eta_effettiva = effettiva$eta_effettiva[effettiva$paese == "OECD"],
    eta_futura = OCSE_FUTURA
  )) %>%
  select(paese_it, eta_effettiva, eta_futura)

# --- Controlli --------------------------------------------------------------
check <- c(Francia = 61.9, Spagna = 62.4, Italia = 64.0, Germania = 64.2,
           `Media OCSE` = 64.7)
eff_round <- round(dati$eta_effettiva[match(names(check), dati$paese_it)], 1)
stopifnot(all(eff_round == check))
check_fut <- c(Francia = 65, Spagna = 65, Germania = 67, Italia = 70,
               Svezia = 70, `Paesi Bassi` = 70, Danimarca = 74)
stopifnot(all(dati$eta_futura[match(names(check_fut), dati$paese_it)] == check_fut))

# --- Preparazione plot ------------------------------------------------------
# Ordine per età futura crescente (a parità, per età effettiva): la più bassa
# in alto, la più alta in basso.
dati <- dati %>%
  arrange(eta_futura, eta_effettiva) %>%
  mutate(paese_it = factor(paese_it, levels = rev(paese_it)),
         ypos = as.numeric(paese_it))

fmt_eta <- function(v, decimale) ifelse(
  decimale | v %% 1 != 0,
  formatC(v, format = "f", digits = 1, decimal.mark = ","),
  as.character(as.integer(round(v)))
)

dati <- dati %>%
  mutate(
    lab_eff = fmt_eta(eta_effettiva, decimale = TRUE),
    # Età legali intere; la media OCSE (66,4) non è intera → una cifra decimale
    lab_fut = fmt_eta(eta_futura, decimale = FALSE)
  )

N <- nrow(dati)

# Etichette dirette delle due serie sopra la prima riga
serie_lab <- tibble(
  x = c(dati$eta_effettiva[dati$ypos == N], dati$eta_futura[dati$ypos == N]),
  y = N + 0.72,
  label = c("Uscita effettiva nel 2024", "Età piena per chi entra oggi"),
  colore = c(COL_BLU, COL_ROSSO),
  hjust = c(0.8, 0.2)
)

p <- ggplot(dati, aes(y = ypos)) +
  geom_segment(aes(x = eta_effettiva, xend = eta_futura,
                   y = ypos, yend = ypos),
               colour = COL_GRIGIO, linewidth = 0.8) +
  geom_point(aes(x = eta_effettiva), colour = COL_BLU, size = 3.2) +
  geom_point(aes(x = eta_futura), colour = COL_ROSSO, size = 3.2) +
  geom_text(aes(x = eta_effettiva, label = lab_eff),
            hjust = 1, nudge_x = -0.28, family = "Source Sans Pro",
            fontface = "bold", size = 3.3, colour = COL_BLU) +
  geom_text(aes(x = eta_futura, label = lab_fut),
            hjust = 0, nudge_x = 0.28, family = "Source Sans Pro",
            fontface = "bold", size = 3.3, colour = COL_ROSSO) +
  geom_text(data = serie_lab,
            aes(x = x, y = y, label = label, colour = colore, hjust = hjust),
            family = "Source Sans Pro", fontface = "bold", size = 3.4) +
  scale_colour_identity() +
  scale_x_continuous(limits = c(60.4, 75.4), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq_len(N), labels = levels(dati$paese_it),
                     limits = c(0.6, N + 1.05), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  theme(legend.position = "none",
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10, hjust = 1)) +
  labs(
    title = "A che età si va in pensione",
    subtitle = "Età effettiva di uscita dal lavoro e età piena di pensionamento prevista per chi entra oggi a 22 anni, uomini",
    caption = "Fonte: OCSE, Pensions at a Glance 2025"
  )

ggsave(file.path(BASE, "output/03_eta_uscita.png"), p,
       width = 9, height = 6.5, units = "in", dpi = 220, bg = "white")

write_csv(dati %>% select(paese_it, eta_effettiva, eta_futura),
          file.path(BASE, "output/03_eta_uscita.csv"))

# --- Sanity check -----------------------------------------------------------
cat("Valori usati (ordine dal basso verso l'alto del grafico invertito):\n")
dati %>%
  arrange(desc(ypos)) %>%
  mutate(riga = sprintf("%-12s effettiva 2024 = %s | futura = %s",
                        as.character(paese_it), lab_eff, lab_fut)) %>%
  pull(riga) %>% walk(~cat(.x, "\n"))
