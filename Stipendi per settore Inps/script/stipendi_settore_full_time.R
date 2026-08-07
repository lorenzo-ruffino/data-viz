# stipendi_settore_full_time.R — eseguito da `cd script && Rscript stipendi_settore_full_time.R`
#
# Fonte: Inps, Osservatorio sui Lavoratori Dipendenti, anno 2024
# (attività economica ATECO 2007 × presenza di tempo parziale × periodo retribuito).
# Perimetro: dipendenti del settore privato non agricolo.

library(tidyverse)
library(showtext)
library(patchwork)

source_dir <- ".."
input_dir  <- file.path(source_dir, "input")
output_dir <- file.path(source_dir, "output")

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_ROSSO  <- "#F12938"    # serie "tutti i dipendenti" nel pannello degli stipendi
COL_GRIGIO <- "#9A9A9A"
COL_GRIGIO_SCURO <- "#5A5A5A"
COL_TRACCIA <- "#E6E6E6"   # fondo scala del pannello "quota nel settore"

CAP_INPS <- "Elaborazione di Lorenzo Ruffino su dati Inps"

theme_linechart <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro"),
      legend.position = "none",
      axis.line = element_line(linewidth = 0.3),
      axis.text = element_text(size = 10.5, color = COL_NERO, hjust = 0.5),
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
                                size = 17.5, color = COL_NERO, hjust = 0,
                                margin = margin(b = 0.1, unit = "cm")),
      plot.subtitle = element_text(size = 11, color = COL_NERO, hjust = 0,
                                   lineheight = 1.35,
                                   margin = margin(b = 0.25, t = 0.1, unit = "cm")),
      plot.caption = element_text(size = 10.5, color = COL_NERO, hjust = 1,
                                  margin = margin(t = 0.5, unit = "cm")),
      ...
    )
}

# --- 1) Lettura del tracciato Inps -------------------------------------------
# Il csv scaricato dall'osservatorio ha 3 righe di intestazione incrociata
# (periodo retribuito × presenza di tempo parziale × misura) e un blocco di
# markup html dentro un unico record quotato. Le 75 colonne di misura seguono
# sempre lo stesso ordine: 5 periodi × 3 modalità di tempo parziale × 5 misure.

PERIODI <- c("Fino a 3 mesi", "Oltre 3 e fino a 6 mesi",
             "Oltre 6 e meno di 12 mesi", "Anno intero", "Totale")
TEMPO_PARZIALE <- c("No", "Si", "Totale")
MISURE <- c("lavoratori", "giornate", "retribuzioni", "sett_utili", "sett_retribuite")

col_idx <- function(periodo, tempo_parziale, misura) {
  # +1 perché in R le colonne partono da 1; le prime due sono settore e separatore
  3 +
    (match(periodo, PERIODI) - 1) * 15 +
    (match(tempo_parziale, TEMPO_PARZIALE) - 1) * 5 +
    (match(misura, MISURE) - 1)
}

parse_num <- function(x) {
  as.numeric(str_replace_all(str_trim(x), "\\.", ""))
}

# Le righe di intestazione hanno 137 campi, quelle di dato 77. Si usa read.csv
# di base e non readr: vroom non regge i ritorni a capo dentro il campo quotato
# del blocco html iniziale e collassa tutto in una colonna sola.
raw <- read.csv(
  file.path(input_dir, "inps_ateco_tempoparziale_2024.csv"),
  sep = ";", header = FALSE, quote = "\"", colClasses = "character",
  fill = TRUE, col.names = paste0("X", 1:137),
  fileEncoding = "latin1", blank.lines.skip = FALSE
)

righe <- raw %>%
  filter(str_trim(X1) != "",
         str_trim(X1) != "Attivita' economica ATECO 2007",
         str_trim(X77) != "")

estrai <- function(periodo, tempo_parziale, misura) {
  parse_num(righe[[col_idx(periodo, tempo_parziale, misura)]])
}

dati <- tibble(
  settore   = str_trim(righe$X1),
  occupati  = estrai("Totale", "Totale", "lavoratori"),
  monte     = estrai("Totale", "Totale", "retribuzioni"),
  ftfy      = estrai("Anno intero", "No", "lavoratori"),
  monte_ftfy = estrai("Anno intero", "No", "retribuzioni")
)

totale <- dati %>% filter(settore == "Totale")
stopifnot(nrow(totale) == 1, totale$occupati == 17731002)

# --- 2) Indicatori per settore ----------------------------------------------

NOMI_BREVI <- c(
  "Estrazione di minerali da cave e miniere"                          = "Estrazione di minerali",
  "Attivita' manifatturiere"                                          = "Manifattura",
  "Fornitura di energia elettrica, gas, vapore e aria condizionata"   = "Energia elettrica e gas",
  "Fornitura di acqua, reti fognarie, attivita' di gestione dei rifiuti e risanamento" = "Acqua e rifiuti",
  "Costruzioni"                                                       = "Costruzioni",
  "Commercio all'ingrosso e al dettaglio, riparazione di autoveicoli e motocicli" = "Commercio",
  "Trasporto e magazzinaggio"                                         = "Trasporti e magazzini",
  "Attivita' dei servizi di alloggio e di ristorazione"               = "Alberghi e ristoranti",
  "Servizi di informazione e comunicazione"                           = "Informatica e comunicazione",
  "Attivita' finanziarie e assicurative"                              = "Banche e assicurazioni",
  "Attivita' immobiliari"                                             = "Attività immobiliari",
  "Attivita' professionali, scientifiche e tecniche"                  = "Attività professionali",
  "Noleggio, agenzie di viaggio, servizi di supporto alle imprese"    = "Servizi alle imprese",
  "Istruzione"                                                        = "Istruzione",
  "Sanita' e assistenza sociale"                                      = "Sanità e assistenza sociale",
  "Attivita' artistiche, sportive, di intrattenimento e divertimento" = "Sport e intrattenimento",
  "Altre attivita' di servizi"                                        = "Altri servizi",
  "Attivita' di famiglie e convivenze come datori di lavoro per personale domestico, produzione di beni e servizi indifferenziati per uso proprio da parte di famiglie e convivenze" = "Lavoro domestico"
)

TOT_OCCUPATI <- totale$occupati

settori <- dati %>%
  filter(settore != "Totale") %>%
  mutate(
    nome            = unname(NOMI_BREVI[settore]),
    stipendio_medio = monte / occupati,
    stipendio_ftfy  = monte_ftfy / ftfy,
    quota_settore   = occupati / TOT_OCCUPATI * 100,   # peso del settore sul totale
    quota_ftfy_tot  = ftfy / TOT_OCCUPATI * 100,       # di cui a tempo pieno tutto l'anno
    quota_ftfy_int  = ftfy / occupati * 100            # tempo pieno tutto l'anno nel settore
  )

stopifnot(!any(is.na(settori$nome)))

write_csv(
  settori %>%
    select(settore, nome, occupati, ftfy, stipendio_medio, stipendio_ftfy,
           quota_settore, quota_ftfy_tot, quota_ftfy_int),
  file.path(output_dir, "stipendi_settore_full_time.csv")
)

# --- 3) Preparazione del grafico --------------------------------------------

HEADER <- " "   # riga fittizia in cima, ospita le intestazioni di colonna
livelli <- c(settori$nome[order(settori$stipendio_ftfy)], HEADER)

settori <- settori %>% mutate(nome = factor(nome, levels = livelli))

fmt_euro <- function(x) paste0("€ ", formatC(round(x / 100) * 100, format = "d",
                                             big.mark = ".", decimal.mark = ","))
fmt_pct1 <- function(x) ifelse(round(x, 1) %% 1 == 0,
                               paste0(as.integer(round(x)), "%"),
                               paste0(formatC(x, format = "f", digits = 1,
                                              decimal.mark = ","), "%"))

riga_header <- tibble(nome = factor(HEADER, levels = livelli))

# Pannello 1 — stipendio medio: tutti i dipendenti vs solo tempo pieno annuale
barre <- settori %>%
  select(nome, `Tutti i dipendenti` = stipendio_medio,
         `Solo a tempo pieno per tutto l'anno` = stipendio_ftfy) %>%
  pivot_longer(-nome, names_to = "gruppo", values_to = "stipendio") %>%
  mutate(gruppo = factor(gruppo, levels = c("Solo a tempo pieno per tutto l'anno",
                                            "Tutti i dipendenti")))

# Le tre righe in cima hanno le barre più lunghe: con l'etichetta fuori
# servirebbero ~20.000 euro di scala in più solo per farcela stare. Le etichette
# vanno dentro la barra, in bianco, e la scala si accorcia da 84k a 67k.
top3 <- livelli[(length(livelli) - 3):(length(livelli) - 1)]
barre <- barre %>% mutate(dentro = nome %in% top3)

X_MAX <- 67000

p_stipendi <- ggplot(barre, aes(x = stipendio, y = nome, fill = gruppo)) +
  geom_col(position = position_dodge(width = 0.78), width = 0.72) +
  # position_dodge ignora nudge_x: lo scarto dell'etichetta va nell'estetica x
  geom_text(data = filter(barre, !dentro),
            aes(x = stipendio + 1100, label = fmt_euro(stipendio), color = gruppo),
            position = position_dodge(width = 0.78),
            hjust = 0, size = 2.8,
            fontface = "bold", family = "Source Sans Pro") +
  geom_text(data = filter(barre, dentro),
            aes(x = stipendio - 1100, label = fmt_euro(stipendio)),
            position = position_dodge(width = 0.78),
            hjust = 1, size = 2.8, color = "white",
            fontface = "bold", family = "Source Sans Pro") +
  geom_text(data = riga_header, inherit.aes = FALSE,
            aes(x = 0, y = nome), label = "Tutti i\ndipendenti",
            hjust = 0, vjust = 0, size = 3.2, fontface = "bold",
            color = COL_ROSSO, family = "Source Sans Pro") +
  geom_text(data = riga_header, inherit.aes = FALSE,
            aes(x = 24000, y = nome), label = "Solo a tempo pieno\nper tutto l'anno",
            hjust = 0, vjust = 0, size = 3.2, fontface = "bold",
            color = COL_BLU, family = "Source Sans Pro") +
  scale_fill_manual(values = c("Tutti i dipendenti" = COL_ROSSO,
                               "Solo a tempo pieno per tutto l'anno" = COL_BLU)) +
  scale_color_manual(values = c("Tutti i dipendenti" = COL_ROSSO,
                                "Solo a tempo pieno per tutto l'anno" = COL_BLU)) +
  scale_x_continuous(limits = c(0, X_MAX), expand = c(0, 0)) +
  scale_y_discrete(limits = livelli, expand = expansion(add = c(0.5, 1.4))) +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  theme(axis.text.x = element_blank(),
        axis.line = element_blank(),
        axis.text.y = element_text(size = 10.5, hjust = 1),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))

# Pannello 2 — quanti, nel settore, lavorano a tempo pieno per tutto l'anno.
# La traccia chiara è lunga uguale su ogni riga: si legge come scala 0-100%,
# non come una categoria (era l'ambiguità della versione con la barra impilata).
p_quota <- ggplot(settori, aes(y = nome)) +
  geom_col(aes(x = 100), fill = COL_TRACCIA, width = 0.72) +
  geom_col(aes(x = quota_ftfy_int), fill = COL_BLU, width = 0.72) +
  # sotto il 22% la barra blu è più corta dell'etichetta: label fuori, in blu
  geom_text(data = filter(settori, quota_ftfy_int >= 22),
            aes(x = quota_ftfy_int - 2, label = fmt_pct1(round(quota_ftfy_int))),
            hjust = 1, size = 2.8, fontface = "bold",
            color = "white", family = "Source Sans Pro") +
  geom_text(data = filter(settori, quota_ftfy_int < 22),
            aes(x = quota_ftfy_int + 2, label = fmt_pct1(round(quota_ftfy_int))),
            hjust = 0, size = 2.8, fontface = "bold",
            color = COL_BLU, family = "Source Sans Pro") +
  geom_text(data = riga_header, inherit.aes = FALSE,
            aes(x = 0, y = nome), label = "Quota a tempo pieno\ntutto l'anno\nnel settore",
            hjust = 0, vjust = 0, size = 3.2, fontface = "bold",
            color = COL_BLU, family = "Source Sans Pro") +
  scale_x_continuous(limits = c(0, 104), expand = c(0, 0)) +
  scale_y_discrete(limits = livelli, expand = expansion(add = c(0.5, 1.4))) +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))

# Pannello 3 — quanto pesa il settore sull'occupazione complessiva.
# Resta in grigio: è un conteggio di teste, non una delle due serie di stipendio.
p_peso <- ggplot(settori, aes(x = quota_settore, y = nome)) +
  geom_col(fill = COL_GRIGIO, width = 0.72) +
  geom_text(data = filter(settori, quota_settore > 10),
            aes(x = quota_settore - 0.6, label = fmt_pct1(quota_settore)),
            hjust = 1, size = 2.8, fontface = "bold",
            color = "white", family = "Source Sans Pro") +
  geom_text(data = filter(settori, quota_settore <= 10),
            aes(x = quota_settore + 0.6, label = fmt_pct1(quota_settore)),
            hjust = 0, size = 2.8, fontface = "bold",
            color = COL_GRIGIO_SCURO, family = "Source Sans Pro") +
  geom_text(data = riga_header, inherit.aes = FALSE,
            aes(x = 0, y = nome), label = "Peso del settore\nsull'occupazione",
            hjust = 0, vjust = 0, size = 3.2, fontface = "bold",
            color = COL_GRIGIO_SCURO, family = "Source Sans Pro") +
  scale_x_continuous(limits = c(0, 24), expand = c(0, 0)) +
  scale_y_discrete(limits = livelli, expand = expansion(add = c(0.5, 1.4))) +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))

p <- p_stipendi + p_quota + p_peso +
  plot_layout(widths = c(0.54, 0.23, 0.23)) +
  plot_annotation(
    title = "Come cambiano gli stipendi privati tra i settori",
    subtitle = paste0(
      "Stipendio lordo medio annuo di tutti i dipendenti e dei soli occupati a tempo pieno per l'intero anno.\n",
      "Settore privato non agricolo, Italia, 2024"),
    caption = CAP_INPS,
    theme = theme_linechart()
  )

ggsave(file.path(output_dir, "stipendi_settore_full_time.png"),
       plot = p, width = 10, height = 10.3, dpi = 220, bg = "white")

# --- 4) Sanity check ---------------------------------------------------------

cat(sprintf("Totale dipendenti: %s | a tempo pieno tutto l'anno: %s (%.1f%%)\n",
            formatC(TOT_OCCUPATI, format = "d", big.mark = ".", decimal.mark = ","),
            formatC(totale$ftfy, format = "d", big.mark = ".", decimal.mark = ","),
            totale$ftfy / TOT_OCCUPATI * 100))
cat(sprintf("Stipendio medio: %.0f | solo tempo pieno annuale: %.0f | rapporto %.2f\n",
            totale$monte / TOT_OCCUPATI, totale$monte_ftfy / totale$ftfy,
            (totale$monte_ftfy / totale$ftfy) / (totale$monte / TOT_OCCUPATI)))
cat(sprintf("Somma quote settore: %.1f%% | somma quote tempo pieno: %.1f%%\n",
            sum(settori$quota_settore), sum(settori$quota_ftfy_tot)))
