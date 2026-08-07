# script/stipendi_eta_piramide.R — eseguito da `cd script && Rscript stipendi_eta_piramide.R`

library(tidyverse)
library(showtext)
library(patchwork)

source_dir <- ".."
input_dir  <- file.path(source_dir, "input")
output_dir <- file.path(source_dir, "output")

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO  <- "#1C1C1C"
COL_BLU   <- "#0478EA"
COL_ROSSO <- "#F12938"

CAP_INPS <- "Elaborazione di Lorenzo Ruffino su dati Inps"

# 1) INPS 2024: lavoratori e redditi per classe di età -------------------------
# Righe "Dipendente privato" / "Dipendente pubblico"; per ciascuna delle 11
# classi di età (poi Totale) tre colonne: Lavoratori, Settimane, Redditi.
righe <- read_lines(file.path(input_dir, "inps_redditi_eta_2024_raw.csv"))

parse_num <- function(x) as.numeric(str_remove_all(x, '[."\\s]'))

classi <- c("Fino a 19", "20-24", "25-29", "30-34", "35-39", "40-44",
            "45-49", "50-54", "55-59", "60-64", "65 e oltre")

estrai <- function(pattern, nome) {
  campi <- str_split(righe[str_detect(righe, pattern)], ";")[[1]]
  map_dfr(seq_along(classi), function(i) {
    tibble(
      classe     = classi[i],
      serie      = nome,
      lavoratori = parse_num(campi[3 * i]),
      redditi    = parse_num(campi[3 * i + 2])
    )
  })
}

dati <- bind_rows(
  estrai('^"Dipendente privato"',  "Dipendenti privati"),
  estrai('^"Dipendente pubblico"', "Dipendenti pubblici")
) %>%
  mutate(classe = factor(classe, levels = classi),
         stipendio = redditi / lavoratori)

write_csv(dati %>% select(classe, serie, lavoratori, stipendio),
          file.path(output_dir, "stipendi_eta_piramide.csv"))

# 2) Piramide a specchio: helper ----------------------------------------------
# Pubblici (rosso) a sinistra, privati (blu) a destra, classi di età al centro.
# Le barre sono normalizzate sul massimo del pannello (stessa scala sui due
# lati); i valori stanno alle estremità delle barre.
GAP <- 0.38   # mezzo-corridoio centrale per le etichette delle classi

piramide <- function(df, valore, fmt, titolo) {
  df <- df %>%
    mutate(v = {{ valore }},
           y = as.integer(classe),
           scaled = v / max(v),
           lato = ifelse(serie == "Dipendenti pubblici", -1, 1),
           xmin = ifelse(lato < 0, -GAP - scaled, GAP),
           xmax = ifelse(lato < 0, -GAP, GAP + scaled),
           label_x = ifelse(lato < 0, xmin - 0.03, xmax + 0.03),
           hjust = ifelse(lato < 0, 1, 0),
           colore = ifelse(lato < 0, COL_ROSSO, COL_BLU))

  # Intestazioni allineate all'inizio interno delle barre (bordo del corridoio)
  intestazioni <- tibble(
    x = c(-GAP, GAP),
    label = c("Pubblici", "Privati"),
    colore = c(COL_ROSSO, COL_BLU),
    hjust = c(1, 0)
  )

  ggplot(df) +
    geom_rect(aes(xmin = xmin, xmax = xmax,
                  ymin = y - 0.36, ymax = y + 0.36, fill = colore)) +
    geom_text(aes(x = 0, y = y, label = classe),
              family = "Source Sans Pro", size = 3, color = COL_NERO) +
    geom_text(aes(x = label_x, y = y, label = fmt(v), hjust = hjust,
                  color = colore),
              family = "Source Sans Pro", fontface = "bold", size = 2.9) +
    geom_text(data = intestazioni,
              aes(x = x, y = 12, label = label, color = colore, hjust = hjust),
              family = "Source Sans Pro", fontface = "bold", size = 3.3,
              vjust = 0.5) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_x_continuous(limits = c(-1.85, 1.85), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0.4, 12.3), expand = c(0, 0)) +
    labs(title = titolo) +
    theme_void() +
    theme(plot.title = element_text(family = "Source Sans Pro SemiBold",
                                    size = 11, color = COL_NERO, hjust = 0.5,
                                    margin = margin(b = 0.15, unit = "cm")))
}

# formatC e NON format(): format() pads a larghezza comune nel vettore e lo
# spazio leading rende illeggibili le etichette (vedi grafici-r).
fmt_eur <- function(x) paste0("€ ", formatC(round(x / 100) * 100, format = "d",
                                            big.mark = "."))
fmt_lav <- function(x) ifelse(
  x >= 1e6,
  paste0(formatC(x / 1e6, format = "f", digits = 2, decimal.mark = ","), " mln"),
  paste0(formatC(round(x / 1e3), format = "d", big.mark = "."), " mila")
)

p_stip <- piramide(dati, stipendio,  fmt_eur, "Stipendio medio annuo lordo")
p_lav  <- piramide(dati, lavoratori, fmt_lav, "Numero di lavoratori")

p <- p_stip + p_lav +
  plot_annotation(
    title = "Lo stipendio nel pubblico è più alto che nel privato",
    subtitle = "Reddito medio annuo lordo e numero di lavoratori dipendenti per classe di età, Italia, 2024",
    caption = CAP_INPS,
    theme = theme(
      text = element_text(family = "Source Sans Pro"),
      plot.title = element_text(family = "Source Sans Pro SemiBold",
                                size = 14, color = COL_NERO, hjust = 0,
                                margin = margin(b = 0.1, unit = "cm")),
      plot.subtitle = element_text(size = 9, color = COL_NERO, hjust = 0,
                                   lineheight = 1.35,
                                   margin = margin(b = 0.25, t = 0.1, unit = "cm")),
      plot.caption = element_text(size = 9, color = COL_NERO, hjust = 1,
                                  margin = margin(t = 0.4, unit = "cm")),
      plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), "cm")
    )
  )

ggsave(file.path(output_dir, "stipendi_eta_piramide.png"),
       plot = p, width = 10, height = 6.5, units = "in", dpi = 220, bg = "white")

# Sanity check
dati %>% select(classe, serie, lavoratori, stipendio) %>%
  pivot_wider(names_from = serie, values_from = c(lavoratori, stipendio)) %>%
  print(n = 11)
