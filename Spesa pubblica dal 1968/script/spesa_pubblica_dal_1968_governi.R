# =============================================================================
# Spesa pubblica italiana dal 1968 — variante con i governi
# Come spesa_pubblica_dal_1968.R, piu' una linea tratteggiata all'insediamento
# di ogni presidente del Consiglio (i governi consecutivi dello stesso premier
# sono raggruppati: Rumor I-III = una linea sola) con il cognome in verticale
# alla destra della linea, in una fascia sopra le barre (ylim esteso a 1400).
# Fonte governi: Wikipedia "Governi italiani per durata"
#                (input/governi_wikipedia_raw.csv)
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(showtext)
  library(grid)
  library(gridExtra)
})

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO  <- "#1C1C1C"
COL_BLU   <- "#0478EA"
COL_ROSSO <- "#F12938"
COL_GOV   <- "#4D4D4D"

theme_linechart <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro"),
      legend.position = "none",
      axis.line = element_line(linewidth = 0.3),
      axis.text = element_text(size = 9, color = "#1C1C1C", hjust = 0.5),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      panel.border = element_blank(),
      plot.margin = unit(c(0.15, 0.4, 0.4, 0.4), "cm"),
      plot.title.position = "plot",
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

ANNO_MIN <- 1968
ANNO_MAX <- 2025
K <- 20     # fattore asse destro: 1 punto di PIL = 20 unita' dell'asse sinistro
Y_TOP <- 1400   # limite y: la fascia 1250-1400 ospita le etichette dei governi

# --- Dati spesa --------------------------------------------------------------

ameco <- read_csv("../input/ameco_italia.csv", show_col_types = FALSE)

fmi <- read_csv("../input/owid_gov_spending_raw.csv", show_col_types = FALSE) %>%
  filter(entity == "Italy") %>%
  transmute(anno = year, quota_fmi = expenditure)

quota_ameco_1995 <- ameco$spesa_pct_pil[ameco$anno == 1995]
quota_fmi_1995   <- fmi$quota_fmi[fmi$anno == 1995]
stopifnot(length(quota_ameco_1995) == 1, length(quota_fmi_1995) == 1)
raccordo <- quota_ameco_1995 / quota_fmi_1995

pil_2025 <- ameco %>% filter(anno == 2025)
prezzi_2025 <- pil_2025$pil_nominale_mrd / pil_2025$pil_reale_2020_mrd

dati <- ameco %>%
  left_join(fmi, by = "anno") %>%
  filter(anno >= ANNO_MIN, anno <= ANNO_MAX) %>%
  mutate(
    quota = if_else(anno >= 1995, spesa_pct_pil, quota_fmi * raccordo),
    reale = quota / 100 * pil_reale_2020_mrd * prezzi_2025
  )
stopifnot(!any(is.na(dati$quota)), !any(is.na(dati$reale)),
          nrow(dati) == ANNO_MAX - ANNO_MIN + 1)

# --- Governi: un segno per presidente del Consiglio --------------------------

mesi_it <- c(gennaio = 1, febbraio = 2, marzo = 3, aprile = 4, maggio = 5,
             giugno = 6, luglio = 7, agosto = 8, settembre = 9, ottobre = 10,
             novembre = 11, dicembre = 12)

parse_data_it <- function(x) {
  x <- str_squish(str_replace_all(x, "[º ]", " "))
  m <- str_match(x, "^(\\d{1,2})\\s+(\\w+)\\s+(\\d{4})")
  as.Date(sprintf("%s-%02d-%02d", m[, 4], mesi_it[m[, 3]], as.integer(m[, 2])))
}

governi <- read_csv("../input/governi_wikipedia_raw.csv", show_col_types = FALSE) %>%
  transmute(
    governo = str_squish(governo),
    inizio  = parse_data_it(str_split_i(periodo_in_carica, "–", 1)),
    premier = str_remove(governo, "\\s+(I|II|III|IV|V|VI|VII)$")
  ) %>%
  filter(!is.na(inizio)) %>%
  arrange(inizio) %>%
  # governi consecutivi dello stesso premier -> un unico mandato
  mutate(nuovo_premier = premier != lag(premier, default = "")) %>%
  filter(nuovo_premier) %>%
  transmute(
    premier,
    frazione = as.numeric(format(inizio, "%j")) / 366,
    # la barra dell'anno Y copre [Y-0,5, Y+0,5]: una data cade in
    # anno + frazione - 0,5
    x = as.integer(format(inizio, "%Y")) + frazione - 0.5
  ) %>%
  filter(x >= ANNO_MIN - 0.5, x <= ANNO_MAX + 0.4)

stopifnot(nrow(governi) > 25, !any(duplicated(governi$x)))

# Etichette su due corsie: se due insediamenti sono a meno di 0,8 anni,
# il secondo scende nella corsia bassa per non sovrapporsi
governi <- governi %>%
  mutate(y_label = NA_real_)
ultimo_top <- -Inf
ultimo_basso <- -Inf
for (i in seq_len(nrow(governi))) {
  if (governi$x[i] - ultimo_top >= 0.8) {
    governi$y_label[i] <- Y_TOP - 10
    ultimo_top <- governi$x[i]
  } else {
    governi$y_label[i] <- Y_TOP - 122
    ultimo_basso <- governi$x[i]
  }
}

write_csv(governi %>% select(premier, x, y_label),
          "../output/spesa_pubblica_dal_1968_governi_marcatori.csv")

# --- Grafico -----------------------------------------------------------------

p <- ggplot(dati, aes(x = anno)) +
  geom_col(aes(y = reale), fill = COL_BLU, width = 0.8) +
  # ogni linea si ferma appena sopra la propria etichetta: cosi' nelle coppie
  # ravvicinate la linea della corsia bassa non attraversa l'etichetta alta
  geom_segment(data = governi, aes(x = x, xend = x, yend = y_label + 4),
               y = 0, color = COL_GOV, linetype = "22", linewidth = 0.3,
               alpha = 0.55) +
  # Bordo bianco sotto la linea rossa, per staccarla dalle barre blu
  geom_line(aes(y = quota * K), color = "white", linewidth = 2.3) +
  geom_line(aes(y = quota * K), color = COL_ROSSO, linewidth = 1.1) +
  geom_text(data = governi,
            aes(x = x + 0.28, y = y_label, label = premier),
            angle = 90, hjust = 1, vjust = 0.5, size = 2.6, color = COL_GOV,
            family = "Source Sans Pro") +
  scale_x_continuous(breaks = seq(1970, 2020, 10),
                     limits = c(ANNO_MIN - 0.6, ANNO_MAX + 0.6),
                     expand = c(0, 0)) +
  scale_y_continuous(
    limits = c(0, Y_TOP), breaks = seq(200, 1200, 200),
    labels = function(x) paste0("€ ", formatC(x, format = "d", big.mark = ".",
                                              decimal.mark = ","), " mld"),
    expand = c(0, 0),
    sec.axis = sec_axis(~ . / K, breaks = seq(10, 60, 10),
                        labels = function(x) paste0(x, "%"))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    caption = "Elaborazione di Lorenzo Ruffino su dati Fmi, Our World in Data, Commissione europea e Wikipedia"
  ) +
  theme_linechart() +
  theme(plot.margin = unit(c(0.15, 0.4, 0.4, 0.4), "cm"),
        axis.text.y       = element_text(size = 9, color = COL_BLU),
        # hjust 0 + margine: con l'hjust 0.5 del tema le label finivano
        # sopra la linea dell'asse destro
        axis.text.y.right = element_text(size = 9, color = COL_ROSSO,
                                         hjust = 0,
                                         margin = margin(l = 4)),
        axis.line.y.right = element_line(linewidth = 0.3, color = COL_ROSSO),
        axis.line.y       = element_line(linewidth = 0.3, color = COL_BLU))

# --- Titolo e sottotitolo (textGrob puri, come nella versione base) ----------

seg_testo <- function(testo, colore, face = "plain", size = 9,
                      family = "Source Sans Pro") {
  textGrob(testo, hjust = 0,
           gp = gpar(fontfamily = family, fontsize = size,
                     col = colore, fontface = face))
}

spazio_w <- function(size = 9) {
  gp <- gpar(fontfamily = "Source Sans Pro", fontsize = size)
  grobWidth(textGrob("| |", gp = gp)) - grobWidth(textGrob("||", gp = gp))
}

# corr_pt: aggiustamenti per giunzione calibrati sul PNG renderizzato (vedi
# versione base): le metriche strwidth di showtext sbagliano in modo
# deterministico ma diverso da stringa a stringa
riga_segmenti <- function(segmenti, spazi_prima, corr_pt = 0) {
  corr_pt <- rep_len(corr_pt, length(segmenti))
  x <- unit(0.4, "cm")
  posizionati <- list()
  for (i in seq_along(segmenti)) {
    if (spazi_prima[i]) x <- x + spazio_w()
    x <- x + unit(corr_pt[i], "pt")
    posizionati[[i]] <- editGrob(segmenti[[i]], x = x)
    x <- x + grobWidth(segmenti[[i]])
  }
  gTree(children = do.call(gList, posizionati))
}

tavola <- arrangeGrob(
  riga_segmenti(list(seg_testo("Come è cambiata la spesa pubblica italiana",
                               COL_NERO, size = 14,
                               family = "Source Sans Pro SemiBold")),
                spazi_prima = FALSE),
  riga_segmenti(
    list(seg_testo("Spesa totale delle amministrazioni pubbliche", COL_NERO),
         seg_testo("in miliardi di euro aggiustati per l'inflazione (barre, asse sinistro)",
                   COL_BLU, "bold")),
    spazi_prima = c(FALSE, TRUE), corr_pt = c(0, 3.6)
  ),
  riga_segmenti(
    list(seg_testo("e", COL_NERO),
         seg_testo("in percentuale del Pil (linea, asse destro)", COL_ROSSO, "bold"),
         seg_testo(", Italia, 1968-2025", COL_NERO)),
    spazi_prima = c(FALSE, TRUE, FALSE), corr_pt = c(0, 0, 5)
  ),
  p,
  ncol = 1,
  heights = unit(c(0.95, 0.52, 0.52, 1), c("cm", "cm", "cm", "null"))
)

# Bordo bianco attorno a tutta la tavola: viewport ristretto di 0,45 cm per
# lato (il plot.margin passato a wrap_elements(full=) viene ignorato)
tavola_pad <- gTree(children = gList(tavola),
                    vp = viewport(width  = unit(1, "npc") - unit(0.9, "cm"),
                                  height = unit(1, "npc") - unit(0.9, "cm")))
tavola_gg <- patchwork::wrap_elements(full = tavola_pad)
ggsave("../output/spesa_pubblica_dal_1968_governi.png", tavola_gg,
       width = 9, height = 6.5, dpi = 220, bg = "white")

# --- Sanity check ------------------------------------------------------------

cat("\nPremier segnati:", nrow(governi), "\n")
cat(paste(sprintf("%s (%.1f%s)", governi$premier, governi$x,
                  if_else(governi$y_label < Y_TOP - 50, " basso", "")),
          collapse = ", "), "\n")
