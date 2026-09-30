# =============================================================================
# Spesa pubblica italiana dal 1968: valore reale (barre, asse sinistro)
# e % del PIL (linea, asse destro)
# Fonti: FMI "Public Finances in Modern History" via Our World in Data
#        (1968-1994, raccordata al livello AMECO sul 1995)
#        AMECO Spring 2026, tavole 6 e 16 (1995-2025): UUTG % PIL,
#        PIL nominale UVGD e PIL reale OVGD (prezzi 2020)
# Spesa reale = quota sul PIL x PIL reale, riportata a prezzi 2025 col
# rapporto UVGD/OVGD del 2025 (equivale a deflazionare col deflatore del PIL).
# Dati gia' scaricati in ../input/
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
      plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), "cm"),
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
K <- 20   # fattore asse destro: 1 punto di PIL = 20 unita' dell'asse sinistro

# --- Dati --------------------------------------------------------------------

ameco <- read_csv("../input/ameco_italia.csv", show_col_types = FALSE)

fmi <- read_csv("../input/owid_gov_spending_raw.csv", show_col_types = FALSE) %>%
  filter(entity == "Italy") %>%
  transmute(anno = year, quota_fmi = expenditure)

# Raccordo: la serie FMI (1968-1994) viene riproporzionata al livello AMECO
# sull'anno comune 1995, cosi' i tassi di variazione restano quelli FMI
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
    reale = quota / 100 * pil_reale_2020_mrd * prezzi_2025,
    fonte_segmento = if_else(anno >= 1995, "ameco", "fmi_raccordato")
  )
stopifnot(!any(is.na(dati$quota)), !any(is.na(dati$reale)),
          nrow(dati) == ANNO_MAX - ANNO_MIN + 1)

dati %>%
  transmute(anno,
            spesa_pct_pil = round(quota, 1),
            spesa_reale_mld_prezzi_2025 = round(reale, 1),
            fonte_segmento) %>%
  write_csv("../output/spesa_pubblica_dal_1968.csv")

# --- Grafico -----------------------------------------------------------------

p <- ggplot(dati, aes(x = anno)) +
  geom_col(aes(y = reale), fill = COL_BLU, width = 0.8) +
  # Bordo bianco sotto la linea rossa, per staccarla dalle barre blu
  geom_line(aes(y = quota * K), color = "white", linewidth = 2.3) +
  geom_line(aes(y = quota * K), color = COL_ROSSO, linewidth = 1.1) +
  scale_x_continuous(breaks = seq(1970, 2020, 10),
                     limits = c(ANNO_MIN - 0.6, ANNO_MAX + 0.6),
                     expand = c(0, 0)) +
  scale_y_continuous(
    limits = c(0, 1250), breaks = seq(200, 1200, 200),
    labels = function(x) paste0("€ ", formatC(x, format = "d", big.mark = ".",
                                              decimal.mark = ","), " mld"),
    expand = c(0, 0),
    sec.axis = sec_axis(~ . / K, breaks = seq(10, 60, 10),
                        labels = function(x) paste0(x, "%"))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    caption = "Elaborazione di Lorenzo Ruffino su dati Fmi, Our World in Data e Commissione europea"
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

# Titolo e sottotitolo come textGrob puri: il renderer rich-text di ggtext
# (element_markdown) con showtext attivo collassa parte degli spazi tra le
# parole, il testo semplice no. Il sottotitolo scorre come una riga unica
# (a capo solo dove serve): i segmenti colorati vengono accodati sulla stessa
# riga con grobWidth, che si risolve al momento del disegno.

seg_testo <- function(testo, colore, face = "plain", size = 9,
                      family = "Source Sans Pro") {
  textGrob(testo, hjust = 0,
           gp = gpar(fontfamily = family, fontsize = size,
                     col = colore, fontface = face))
}

# Larghezza di un singolo spazio, per differenza: le metriche dei textGrob
# scartano gli spazi ai bordi, quindi lo spazio tra due segmenti va inserito
# esplicitamente nella posizione, non nel testo
spazio_w <- function(size = 9) {
  gp <- gpar(fontfamily = "Source Sans Pro", fontsize = size)
  grobWidth(textGrob("| |", gp = gp)) - grobWidth(textGrob("||", gp = gp))
}

# corr_pt: aggiustamenti per giunzione calibrati sul PNG renderizzato — le
# metriche strwidth di showtext sbagliano in modo deterministico ma diverso
# da stringa a stringa (il segmento lungo e' sotto-misurato di ~11px, la "e"
# sovra-misurata), quindi si corregge a mano. Se cambiano i testi del
# sottotitolo, ricalibrare misurando i gap sul raster.
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

# Salvataggio con ggsave via patchwork: e' il percorso di rendering di tutti
# i grafici del repo (device manuale o ggsave su grob puro rendono i testi
# piu' piccoli / con font di ripiego)
# Bordo bianco attorno a tutta la tavola: viewport ristretto di 0,45 cm per
# lato (il plot.margin passato a wrap_elements(full=) viene ignorato)
tavola_pad <- gTree(children = gList(tavola),
                    vp = viewport(width  = unit(1, "npc") - unit(0.9, "cm"),
                                  height = unit(1, "npc") - unit(0.9, "cm")))
tavola_gg <- patchwork::wrap_elements(full = tavola_pad)
ggsave("../output/spesa_pubblica_dal_1968.png", tavola_gg,
       width = 9, height = 6.5, dpi = 220, bg = "white")

# --- Sanity check ------------------------------------------------------------

r <- function(a) dati %>% filter(anno == a)
cat("\nAnni:", min(dati$anno), "-", max(dati$anno), "| n =", nrow(dati), "\n")
cat(sprintf("Raccordo FMI->AMECO 1995: %.4f\n", raccordo))
cat(sprintf("Reale  %d: %.0f mld -> %d: %.0f mld  (x%.2f)\n",
            ANNO_MIN, r(ANNO_MIN)$reale, ANNO_MAX, r(ANNO_MAX)$reale,
            r(ANNO_MAX)$reale / r(ANNO_MIN)$reale))
cat(sprintf("Quota  %d: %.1f%% -> %d: %.1f%%\n",
            ANNO_MIN, r(ANNO_MIN)$quota, ANNO_MAX, r(ANNO_MAX)$quota))
cat(sprintf("Picco quota: %d (%.1f%%) | picco pre-Covid: 1993 (%.1f%%)\n",
            dati$anno[which.max(dati$quota)], max(dati$quota), r(1993)$quota))
cat(sprintf("Controllo 2025 vs AMECO nominale: %.0f mld (attesi ~1155)\n",
            r(2025)$reale))
cat(sprintf("Picco spesa reale: %d (%.0f mld) | reale 2019: %.0f mld\n",
            dati$anno[which.max(dati$reale)], max(dati$reale), r(2019)$reale))
cat(sprintf("Reale 1995: %.0f mld | var. reale 1995-2019: %+.1f%% | quota: %+.1f punti\n",
            r(1995)$reale, (r(2019)$reale / r(1995)$reale - 1) * 100,
            r(2019)$quota - r(1995)$quota))
