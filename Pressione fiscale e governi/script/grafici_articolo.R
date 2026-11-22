library(tidyverse)
library(showtext)

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
COL_GRIGIO_SCURO <- "#5A5A5A"

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
CAP_ISTAT    <- "Elaborazione di Lorenzo Ruffino su dati Istat"
CAP_EUROSTAT <- "Elaborazione di Lorenzo Ruffino su dati Eurostat"
CAP_MISTO    <- "Elaborazione di Lorenzo Ruffino su dati Istat, MEF e UPB"

pct_it <- function(x, d = 1) paste0(formatC(x, format = "f", digits = d, decimal.mark = ","), "%")
pp_it  <- function(x) paste0(ifelse(x > 0, "+", "−"), formatC(abs(x), format = "f", digits = 2, decimal.mark = ","))

# --- 1. Serie storica con i governi ------------------------------------------
serie <- read_csv("../input/serie_pressione_1995_2025.csv", show_col_types = FALSE) |>
  rename(anno = TIME_PERIOD)

governi <- tribble(
  ~nome,              ~da,  ~a,
  "Centrosinistra",   1996, 2001,
  "Berlusconi",       2002, 2006,
  "Prodi",            2007, 2008,
  "Berlusconi",       2009, 2011,
  "Monti",            2012, 2013,
  "Letta e Renzi",    2014, 2016,
  "Gentiloni",        2017, 2018,
  "Conte",            2019, 2020,
  "Draghi",           2021, 2022,
  "Meloni",           2023, 2025
) |>
  mutate(i = row_number(),
         fill = ifelse(i %% 2 == 0, "#EFEFEF", "#F8F8F8"),
         fill = ifelse(nome == "Meloni", "#FDE3E5", fill),
         xmin = da - 0.5, xmax = a + 0.5, xmid = (da + a) / 2,
         ylab = ifelse(i %% 2 == 0, 44.55, 44.15))

etichette <- serie |> filter(anno %in% c(2005, 2013, 2022, 2025)) |>
  mutate(vj = case_when(anno == 2005 ~ 1.9, anno == 2022 ~ 0.5, TRUE ~ -1.1),
         hj = case_when(anno == 2025 ~ 0.8, anno == 2022 ~ 1.25, TRUE ~ 0.5))

p1 <- ggplot(serie, aes(anno, pressione)) +
  geom_rect(data = governi, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = 37.6, ymax = 44.85, fill = fill)) +
  scale_fill_identity() +
  geom_text(data = governi, inherit.aes = FALSE,
            aes(x = xmid, y = ylab, label = nome),
            family = "Source Sans Pro", size = 2.55, color = COL_GRIGIO_SCURO) +
  geom_line(color = COL_ROSSO, linewidth = 1.0) +
  geom_point(data = etichette, color = COL_ROSSO, size = 2.2) +
  geom_text(data = etichette, aes(label = pct_it(pressione), vjust = vj, hjust = hj),
            family = "Source Sans Pro", fontface = "bold", size = 3.4, color = COL_NERO) +
  scale_x_continuous(limits = c(1994.5, 2025.5), breaks = seq(1995, 2025, 5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(37.6, 44.85), breaks = seq(38, 44, 1),
                     labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_linechart() + theme(legend.position = "none") +
  labs(title = "La pressione fiscale è la più alta dal 2015",
       subtitle = "Imposte e contributi sociali in percentuale del PIL. Ogni fascia indica gli anni attribuiti a un governo,\na partire dal primo bilancio che ha scritto, Italia, 1995-2025",
       caption = CAP_ISTAT)
ggsave("../output/01_pressione_fiscale_governi.png", p1, width = 9, height = 6.5, dpi = 220, bg = "white")

# --- 2. Confronto europeo -----------------------------------------------------
nomi <- c(FR = "Francia", BE = "Belgio", DK = "Danimarca", AT = "Austria", IT = "Italia",
          FI = "Finlandia", LU = "Lussemburgo", DE = "Germania", EL = "Grecia", SE = "Svezia",
          NL = "Paesi Bassi", SI = "Slovenia", HR = "Croazia", ES = "Spagna", PL = "Polonia",
          PT = "Portogallo", EE = "Estonia", CY = "Cipro", SK = "Slovacchia", HU = "Ungheria",
          CZ = "Cechia", LV = "Lettonia", LT = "Lituania", BG = "Bulgaria", MT = "Malta",
          RO = "Romania", IE = "Irlanda")
ue <- read_csv("../input/eurostat_pressione_2025.csv", show_col_types = FALSE,
               col_types = cols(geo = col_character()))
media_ue <- ue |> filter(geo == "EU27_2020") |> pull(valore)
nomi <- c(nomi, EU27_2020 = "Media UE")
paesi <- ue |> filter(geo %in% names(nomi)) |>
  mutate(paese = nomi[geo], paese = fct_reorder(paese, valore),
         col = case_when(geo == "IT" ~ COL_ROSSO, geo == "EU27_2020" ~ COL_GRIGIO_SCURO, TRUE ~ COL_BLU))
stopifnot(nrow(paesi) == 28)

p2 <- ggplot(paesi, aes(valore, paese)) +
  geom_col(aes(fill = col), width = 0.72) +
  scale_fill_identity() +
  geom_text(aes(label = pct_it(valore), color = col), hjust = -0.15,
            family = "Source Sans Pro", fontface = "bold", size = 2.9) +
  scale_color_identity() +
  scale_x_continuous(limits = c(0, 52), breaks = seq(10, 50, 10),
                     labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  theme_linechart() +
  theme(legend.position = "none", axis.line = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_text(hjust = 1, size = 9,
                                   face = ifelse(levels(paesi$paese) %in% c("Italia", "Media UE"), "bold", "plain"))) +
  labs(title = "L'Italia ha la quinta pressione fiscale più alta dell'UE",
       subtitle = "Imposte e contributi sociali in percentuale del PIL, paesi dell'Unione europea, 2025",
       caption = CAP_EUROSTAT)
ggsave("../output/02_pressione_fiscale_europa.png", p2, width = 8, height = 8, dpi = 220, bg = "white")

# --- 3. Cascata 2022-2025 ----------------------------------------------------
casc <- read_csv("../input/cascata_2022_2025.csv", show_col_types = FALSE)
P0 <- serie |> filter(anno == 2022) |> pull(pressione)
P1 <- serie |> filter(anno == 2025) |> pull(pressione)
stopifnot(abs(P0 + sum(casc$valore) - P1) < 0.005)

righe <- casc |>
  mutate(fine = P0 + cumsum(valore), inizio = fine - valore) |>
  bind_rows(tibble(voce = "Pressione fiscale 2022", gruppo = "Totale", valore = P0, inizio = 41, fine = P0), .x = _) |>
  bind_rows(tibble(voce = "Pressione fiscale 2025", gruppo = "Totale", valore = P1, inizio = 41, fine = P1)) |>
  mutate(ord = row_number(), voce = fct_reorder(voce, -ord),
         etichetta = ifelse(gruppo == "Totale", pct_it(valore), pp_it(valore)),
         xlab = pmax(inizio, fine), y = as.numeric(voce))

col_gruppi <- c("Composizione dei redditi" = COL_BLU, "Base finanziaria" = COL_VIOLA,
                "Scelte del governo" = COL_ROSSO, "Automatismi" = COL_GIALLO,
                "Altro" = COL_GRIGIO, "Totale" = COL_NERO)
connettori <- righe |> filter(ord < max(ord)) |>
  transmute(x = fine, y = y, yend = y - 1)

p3 <- ggplot(righe) +
  geom_segment(data = connettori, aes(x = x, xend = x, y = y - 0.36, yend = yend + 0.36),
               colour = COL_GRIGIO, linewidth = 0.3) +
  geom_rect(aes(xmin = inizio, xmax = fine, ymin = y - 0.36, ymax = y + 0.36, fill = gruppo)) +
  geom_text(aes(x = xlab, y = y, label = etichetta, color = gruppo), hjust = -0.2,
            family = "Source Sans Pro", fontface = "bold", size = 3.2, show.legend = FALSE) +
  scale_fill_manual(values = col_gruppi, breaks = names(col_gruppi)[1:4]) +
  scale_color_manual(values = col_gruppi, guide = "none") +
  scale_y_continuous(breaks = righe$y, labels = as.character(righe$voce), expand = c(0.02, 0.02)) +
  scale_x_continuous(limits = c(41, 43.6), breaks = seq(41, 43.5, 0.5),
                     labels = function(x) paste0(formatC(x, format = "f", digits = 1, decimal.mark = ","), "%"),
                     expand = c(0, 0)) +
  guides(fill = guide_legend(nrow = 1)) +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  theme(axis.line.y = element_blank(), axis.text.y = element_text(hjust = 1, size = 9.5),
        legend.location = "plot", legend.justification.top = "left", legend.text = element_text(size = 9.5)) +
  labs(title = "Da dove viene l'aumento della pressione fiscale",
       subtitle = "Contributo di ciascun fattore alla variazione della pressione fiscale, in punti di PIL, Italia, 2022-2025",
       caption = CAP_MISTO)
ggsave("../output/03_cascata_pressione_fiscale.png", p3, width = 9, height = 6.8, dpi = 220, bg = "white")

cat("2022:", P0, " 2025:", P1, " somma voci:", sum(casc$valore), " media UE:", media_ue,
    " Italia:", paesi$valore[paesi$geo == "IT"], " posto Italia:",
    rank(-paesi$valore[paesi$geo != "EU27_2020"])[paesi$geo[paesi$geo != "EU27_2020"] == "IT"], "\n")

# --- 3b. Cascata con frecce al posto delle barre --------------------------------
fr <- righe |> mutate(col = col_gruppi[gruppo])
voci_fr <- fr |> filter(gruppo != "Totale")
tot_fr  <- fr |> filter(gruppo == "Totale")

p3b <- ggplot(fr) +
  geom_segment(data = connettori, aes(x = x, xend = x, y = y, yend = yend),
               colour = COL_GRIGIO, linewidth = 0.3, linetype = "dotted") +
  geom_segment(data = voci_fr, aes(x = inizio, xend = fine, y = y, yend = y, colour = gruppo),
               linewidth = 1.6, linejoin = "mitre",
               arrow = arrow(length = unit(0.22, "cm"), type = "closed")) +
  geom_point(data = tot_fr, aes(x = fine, y = y), colour = COL_NERO, size = 3) +
  geom_text(data = voci_fr, aes(x = pmax(inizio, fine), y = y, label = etichetta, colour = gruppo),
            hjust = -0.25, family = "Source Sans Pro", fontface = "bold", size = 3.3, show.legend = FALSE) +
  geom_text(data = tot_fr, aes(x = fine, y = y, label = etichetta), hjust = -0.35,
            family = "Source Sans Pro", fontface = "bold", size = 3.5, colour = COL_NERO) +
  scale_colour_manual(values = col_gruppi, breaks = names(col_gruppi)[1:4]) +
  scale_y_continuous(breaks = righe$y, labels = as.character(righe$voce), expand = c(0.03, 0.03)) +
  scale_x_continuous(limits = c(41.4, 43.4), breaks = seq(41.5, 43, 0.5),
                     labels = function(x) paste0(formatC(x, format = "f", digits = 1, decimal.mark = ","), "%"),
                     expand = c(0, 0)) +
  guides(colour = guide_legend(nrow = 1, override.aes = list(linewidth = 2.5))) +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  theme(axis.line.y = element_blank(), axis.text.y = element_text(hjust = 1, size = 9.5),
        legend.location = "plot", legend.justification.top = "left", legend.text = element_text(size = 9.5)) +
  labs(title = "Da dove viene l'aumento della pressione fiscale",
       subtitle = "Contributo di ciascun fattore alla variazione della pressione fiscale, in punti di PIL, Italia, 2022-2025",
       caption = CAP_MISTO)
ggsave("../output/03b_cascata_frecce_pressione_fiscale.png", p3b, width = 9, height = 6.8, dpi = 220, bg = "white")

# --- 4. Tabella dei governi, nello stesso stile dei grafici --------------------
gov <- read_csv("../input/governi_pressione.csv", show_col_types = FALSE) |>
  mutate(governo = recode(governo, "Prodi I / D'Alema / Amato" = "Prodi I, D'Alema e Amato",
                          "Berlusconi II-III" = "Berlusconi II e III", "Letta / Renzi" = "Letta e Renzi",
                          "Conte I-II" = "Conte I e II"),
         riga = row_number(), y = -riga,
         meloni = governo == "Meloni")
colonne <- tibble(
  x     = c(0, 3.55, 5.15, 6.65, 8.15, 9.6),
  hj    = c(0, 0.5, 0.5, 0.5, 0.5, 0.5),
  titolo = c("Governo", "Anni\nattribuiti", "Anno prima\ndel governo", "Ultimo anno\ndel governo",
             "Variazione\nin punti", "Media\ndel periodo"))
celle <- gov |>
  transmute(y, meloni,
            c1 = governo, c2 = str_replace(anni, "-", "–"),
            c3 = pct_it(inizio), c4 = pct_it(fine),
            c5 = paste0(ifelse(delta > 0, "+", "−"), formatC(abs(delta), format = "f", digits = 1, decimal.mark = ",")),
            c6 = pct_it(media)) |>
  pivot_longer(c1:c6, names_to = "col", values_to = "testo") |>
  mutate(k = as.integer(str_sub(col, 2)), x = colonne$x[k], hj = colonne$hj[k],
         bold = meloni | k == 5, colore = ifelse(meloni, COL_ROSSO, COL_NERO))

p4 <- ggplot() +
  annotate("rect", xmin = -0.15, xmax = 10.35, ymin = -10.5, ymax = -9.5, fill = "#FDE3E5") +
  geom_segment(data = tibble(y = -(1:9) - 0.5), aes(x = -0.15, xend = 10.35, y = y, yend = y),
               colour = "#E3E3E3", linewidth = 0.3) +
  annotate("segment", x = -0.15, xend = 10.35, y = -0.45, yend = -0.45, colour = COL_NERO, linewidth = 0.3) +
  geom_text(data = colonne, aes(x = x, y = 0.05, label = titolo, hjust = hj),
            family = "Source Sans Pro", fontface = "bold", size = 3.3, lineheight = 0.9,
            vjust = 0, colour = COL_NERO) +
  geom_text(data = celle, aes(x = x, y = y, label = testo, hjust = hj, colour = colore,
                              fontface = ifelse(bold, "bold", "plain")),
            family = "Source Sans Pro", size = 3.5) +
  scale_colour_identity() +
  scale_x_continuous(limits = c(-0.2, 10.4), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-10.6, 1.1), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  theme(axis.line = element_blank(), axis.text = element_blank()) +
  labs(title = "Con il governo Meloni il secondo aumento in trent'anni",
       subtitle = "Pressione fiscale (imposte e contributi sociali in percentuale del PIL) per governo. A ogni governo sono attribuiti\ngli anni a partire dal primo bilancio che ha scritto, Italia, 1996-2025",
       caption = CAP_ISTAT)
ggsave("../output/04_tabella_governi.png", p4, width = 9, height = 6.5, dpi = 220, bg = "white")
