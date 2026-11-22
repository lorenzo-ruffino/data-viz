library(tidyverse)
library(lubridate)
library(showtext)
library(ggrepel)

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
COL_GRIGIO <- "#9A9A9A"
COL_VERDE  <- "#1B9E77"

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

CAP <- "Elaborazione di Lorenzo Ruffino su dati Istat ed Eurostat"

fmt_mila <- function(x) {
  ifelse(abs(x) < 1e6,
         paste0(format(round(x / 1000), big.mark = ".", decimal.mark = ",", trim = TRUE), " mila"),
         paste0(formatC(x / 1e6, format = "f", digits = 2, decimal.mark = ","), " mln"))
}

# --- Dati -------------------------------------------------------------------

# Mensile 2019-oggi: bilancio demografico mensile Istat (iscrizioni dall'estero
# REGOTHC, cancellazioni per l'estero DEREGOTHC, totale maschi e femmine)
mensile <- read_csv("../input/istat_22_315_DF_DCIS_POPORESBIL1_2.csv",
                    col_types = cols(.default = col_character())) |>
  filter(REF_AREA == "IT", SEX == "9", DATA_TYPE %in% c("REGOTHC", "DEREGOTHC")) |>
  transmute(mese = ym(TIME_PERIOD), DATA_TYPE, valore = as.numeric(OBS_VALUE)) |>
  pivot_wider(names_from = DATA_TYPE, values_from = valore) |>
  transmute(mese, immigrati = REGOTHC, emigrati = DEREGOTHC,
            saldo = immigrati - emigrati, fonte = "Istat, mensile") |>
  arrange(mese)

ULTIMO_MESE <- max(mensile$mese)

# Annuale 2001-2018: flussi anagrafici con l'estero (Eurostat migr_imm8 e
# migr_emi2, che riprendono i dati Istat; coincidono con le somme mensili Istat
# dal 2019). Prima del 2019 non esiste un dato mensile con l'estero separato:
# il saldo annuo viene ripartito in parti uguali sui giorni dell'anno.
annuale <- read_csv("../input/eurostat_immigrazione_emigrazione_IT.csv",
                    col_types = cols(.default = col_double())) |>
  mutate(saldo = immigrati - emigrati)

controllo <- mensile |>
  mutate(anno = year(mese)) |>
  group_by(anno) |>
  summarise(imm_istat = sum(immigrati), emi_istat = sum(emigrati), n = n()) |>
  filter(n == 12) |>
  inner_join(annuale, by = "anno")
stopifnot(all(controllo$imm_istat == controllo$immigrati),
          all(controllo$emi_istat == controllo$emigrati))

governi <- read_csv("../input/governi.csv", col_types = cols(.default = col_character())) |>
  mutate(inizio = ymd(inizio),
         fine = coalesce(ymd(fine), ceiling_date(ULTIMO_MESE, "month")),
         in_carica = governo == "Meloni")

# Serie giornaliera: ogni giorno riceve la quota del suo mese (dal 2019) o del
# suo anno (fino al 2018). Serve ad attribuire i mesi a cavallo di due governi.
giorni <- tibble(giorno = seq(min(governi$inizio), ceiling_date(ULTIMO_MESE, "month") - 1, by = "day")) |>
  mutate(anno = year(giorno), mese = floor_date(giorno, "month")) |>
  left_join(annuale |> select(anno, saldo_anno = saldo), by = "anno") |>
  left_join(mensile |> select(mese, saldo_mese = saldo), by = "mese") |>
  mutate(saldo_giorno = if_else(anno >= 2019,
                                saldo_mese / days_in_month(giorno),
                                saldo_anno / if_else(leap_year(anno), 366, 365)))
stopifnot(!anyNA(giorni$saldo_giorno))

giorni <- giorni |>
  cross_join(governi |> select(governo, inizio, fine)) |>
  filter(giorno >= inizio, giorno < fine) |>
  select(-inizio, -fine)

# Saldo mensile stimato per tutto il periodo (mese solare)
saldo_mensile <- giorni |>
  group_by(mese) |>
  summarise(saldo = sum(saldo_giorno), .groups = "drop") |>
  mutate(dato = if_else(year(mese) >= 2019, "mensile Istat", "annuale ripartito sui mesi"))
write_csv(saldo_mensile |> mutate(saldo = round(saldo)), "../output/saldo_migratorio_estero_mensile.csv")

# Totali per governo
per_governo <- giorni |>
  group_by(governo) |>
  summarise(saldo = sum(saldo_giorno), giorni = n(), .groups = "drop") |>
  left_join(governi, by = "governo") |>
  mutate(mesi = giorni / (365.25 / 12),
         media_mensile = saldo / mesi) |>
  arrange(inizio)
write_csv(per_governo |> transmute(governo, inizio, fine, mesi = round(mesi, 1),
                                   saldo = round(saldo), media_mensile = round(media_mensile)),
          "../output/saldo_migratorio_estero_per_governo.csv")

# Cumulato per mese di governo
cumulato <- giorni |>
  left_join(governi |> select(governo, inizio), by = "governo") |>
  arrange(governo, giorno) |>
  group_by(governo) |>
  mutate(mesi = as.numeric(giorno - inizio + 1) / (365.25 / 12),
         cumulato = cumsum(saldo_giorno)) |>
  ungroup()

# --- Grafico 1: linee cumulate ------------------------------------------------

colori_governi <- c(
  "Meloni"        = COL_ROSSO,
  "Prodi II"      = COL_VIOLA,
  "Berlusconi IV" = COL_NERO,
  "Berlusconi II" = COL_GIALLO,
  "Renzi"         = COL_BLU,
  "Draghi"        = COL_VERDE
)
col_gov <- function(g) coalesce(colori_governi[g], "#BDBDBD")

cum_plot <- cumulato |>
  group_by(governo) |>
  filter(row_number() %% 7 == 1 | row_number() == n()) |>
  ungroup() |>
  mutate(colore = col_gov(governo),
         evidenza = governo %in% names(colori_governi))

etichette <- cum_plot |>
  group_by(governo) |>
  slice_max(mesi, n = 1) |>
  ungroup() |>
  left_join(per_governo |> select(governo, saldo), by = "governo") |>
  mutate(label = if_else(evidenza, paste0(governo, " ", fmt_mila(saldo)), governo),
         label = if_else(governo == "Meloni", paste0(label, "*"), label),
         dx = case_when(governo == "Prodi II" ~ -0.6, TRUE ~ 0.9),
         dy = case_when(governo == "Prodi II" ~ 45000,
                        governo == "Meloni" ~ -35000,
                        governo == "Berlusconi II" ~ 15000,
                        TRUE ~ 0),
         hj = if_else(governo == "Prodi II", 1, 0))

COL_X <- 29
colonna <- etichette |>
  filter(!evidenza | governo == "Draghi") |>
  mutate(label = paste0(governo, " ", fmt_mila(saldo))) |>
  arrange(desc(cumulato)) |>
  mutate(label_y = 300000 - (row_number() - 1) * 44000)
etichette <- filter(etichette, !governo %in% colonna$governo)

p1 <- ggplot(cum_plot, aes(mesi, cumulato, group = governo, colour = colore)) +
  geom_line(data = filter(cum_plot, !evidenza), linewidth = 0.6) +
  geom_line(data = filter(cum_plot, evidenza), linewidth = 0.95) +
  geom_point(data = bind_rows(etichette, colonna), size = 1.6) +
  # Governi evidenziati: etichetta a destra del punto finale (Prodi II sopra a
  # sinistra, per non finire sulla linea di Berlusconi IV)
  geom_text(data = filter(etichette, evidenza),
            aes(x = mesi + dx, y = cumulato + dy, label = label, hjust = hj),
            size = 3.3, fontface = "bold", family = "Source Sans Pro") +
  # Governi brevi: finiscono tutti nello stesso punto del grafico, quindi le
  # etichette vanno in colonna nello spazio libero sotto la linea di Renzi
  geom_segment(data = colonna,
               aes(x = mesi + 0.3, xend = COL_X - 0.3, y = cumulato, yend = label_y),
               colour = "#D0D0D0", linewidth = 0.3) +
  geom_text(data = colonna, aes(x = COL_X, y = label_y, label = label,
                                colour = if_else(governo == "Draghi", COL_VERDE, "#7A7A7A")),
            hjust = 0, size = 3.0, fontface = "bold", family = "Source Sans Pro") +
  scale_colour_identity() +
  scale_x_continuous(breaks = seq(0, 48, 12),
                     labels = c("Insediamento", "1 anno", "2 anni", "3 anni", "4 anni"),
                     limits = c(0, 62), expand = c(0.01, 0)) +
  scale_y_continuous(breaks = seq(200000, 1200000, 200000), labels = fmt_mila,
                     limits = c(0, 1400000), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_linechart() + theme(legend.position = "none") +
  labs(title = "Con quali governi c'è stata più immigrazione netta",
       subtitle = paste0("Saldo cumulato tra iscrizioni anagrafiche dall'estero e cancellazioni per l'estero per tempo in carica,\n",
                         "Italia, giugno 2001-", tolower(format(ULTIMO_MESE, "%B %Y")),
                         " (*governo ancora in carica)"),
       caption = CAP)

ggsave("../output/01_immigrazione_netta_cumulata_governi.png", p1,
       width = 9, height = 6.5, dpi = 220, bg = "white")

# --- Grafico 2: totale e media mensile per governo ----------------------------

barre <- per_governo |>
  mutate(governo_lab = if_else(in_carica, paste0(governo, "*"), governo),
         governo_lab = fct_rev(fct_inorder(governo_lab)),
         colore = col_gov(governo),
         colore = if_else(colore == "#BDBDBD", COL_GRIGIO, colore))

p2 <- ggplot(barre, aes(saldo, governo_lab, fill = colore)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = fmt_mila(saldo)), hjust = -0.15, size = 3.2,
            fontface = "bold", family = "Source Sans Pro", colour = COL_NERO) +
  scale_fill_identity() +
  scale_x_continuous(limits = c(0, 1550000), expand = c(0, 0),
                     breaks = seq(200000, 1200000, 200000), labels = fmt_mila) +
  coord_cartesian(clip = "off") +
  theme_linechart(axis.line.y = element_blank()) + theme(axis.text.y = element_text(hjust = 1)) +
  labs(title = "Il saldo più alto è del governo Berlusconi IV",
       subtitle = paste0("Iscrizioni anagrafiche dall'estero meno cancellazioni per l'estero durante ogni governo,\n",
                         "Italia, giugno 2001-", tolower(format(ULTIMO_MESE, "%B %Y")),
                         " (*governo ancora in carica)"),
       caption = CAP)

ggsave("../output/02_immigrazione_netta_totale_governi.png", p2,
       width = 8, height = 6.5, dpi = 220, bg = "white")

p3 <- ggplot(barre, aes(media_mensile, governo_lab, fill = colore)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = fmt_mila(media_mensile)), hjust = -0.15, size = 3.2,
            fontface = "bold", family = "Source Sans Pro", colour = COL_NERO) +
  scale_fill_identity() +
  scale_x_continuous(limits = c(0, 38000), expand = c(0, 0),
                     breaks = seq(5000, 35000, 5000), labels = fmt_mila) +
  coord_cartesian(clip = "off") +
  theme_linechart(axis.line.y = element_blank()) + theme(axis.text.y = element_text(hjust = 1)) +
  labs(title = "In media al mese, il primato è del governo Prodi II",
       subtitle = paste0("Saldo medio mensile tra iscrizioni anagrafiche dall'estero e cancellazioni per l'estero,\n",
                         "Italia, giugno 2001-", tolower(format(ULTIMO_MESE, "%B %Y")),
                         " (*governo ancora in carica)"),
       caption = CAP)

ggsave("../output/03_immigrazione_netta_media_mensile_governi.png", p3,
       width = 8, height = 6.5, dpi = 220, bg = "white")

# --- Controllo ------------------------------------------------------------------

cat("\nUltimo mese disponibile:", format(ULTIMO_MESE, "%Y-%m"), "\n\n")
print(per_governo |> transmute(governo, mesi = round(mesi, 1),
                               saldo = round(saldo), media_mensile = round(media_mensile)),
      n = Inf)
cat("\nPrimi 12 mesi di ogni governo (saldo cumulato):\n")
print(cumulato |> filter(mesi <= 12) |> group_by(governo) |>
        slice_max(mesi, n = 1) |> ungroup() |> arrange(desc(cumulato)) |>
        transmute(governo, cumulato_12_mesi = round(cumulato)), n = Inf)
