# Grafici per l'articolo "No, non si cresce solo con più spesa pubblica"
# 01 - spesa pubblica 2025 e crescita del PIL per abitante 2000-2025, nove paesi
# 02 - interessi e istruzione in percentuale del PIL, Italia, 2001-2024
# 03 - effetto del PNRR sul livello del PIL secondo l'UPB, 2021-2030
# 04 - sondaggi del Clark Center tra gli economisti

library(eurostat)
library(tidyverse)
library(showtext)
library(ggrepel)

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_ROSSO  <- "#F12938"
COL_GRIGIO <- "#9A9A9A"
COL_GRIGIO_SCURO <- "#5A5A5A"

CAP_EUROSTAT <- "Elaborazione di Lorenzo Ruffino su dati Eurostat"
CAP_UPB      <- "Elaborazione di Lorenzo Ruffino su dati Ufficio parlamentare di bilancio e Governo"
CAP_CLARK    <- "Elaborazione di Lorenzo Ruffino su dati Clark Center for Global Markets"

OUT <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Spesa pubblica e crescita/output"

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

fmt_num <- function(x, d = 1) formatC(x, format = "f", digits = d, decimal.mark = ",")

# Europa occidentale: UE a 15 senza l'Irlanda (PIL gonfiato dalle multinazionali)
# più Norvegia, Svizzera e Islanda. Esclusi i paesi dell'Est, che crescono per convergenza.
PAESI <- c(IT = "Italia", DE = "Germania", FR = "Francia", ES = "Spagna",
           NL = "Paesi Bassi", SE = "Svezia", DK = "Danimarca", EL = "Grecia",
           PT = "Portogallo", BE = "Belgio", AT = "Austria", FI = "Finlandia",
           LU = "Lussemburgo", NO = "Norvegia", CH = "Svizzera", IS = "Islanda")

# --- 01 Spesa pubblica e crescita -------------------------------------------

spesa <- get_eurostat("gov_10a_main", time_format = "num",
                      filters = list(na_item = "TE", sector = "S13",
                                     unit = "PC_GDP", geo = names(PAESI))) |>
  filter(time %in% c(2000, 2025)) |>
  select(geo, time, values) |>
  pivot_wider(names_from = time, values_from = values, names_prefix = "s") |>
  transmute(geo, spesa_2000 = s2000, spesa_2025 = s2025,
            var_spesa = s2025 - s2000)

pil_pc <- get_eurostat("nama_10_pc", time_format = "num",
                       filters = list(na_item = "B1GQ", unit = "CLV20_EUR_HAB",
                                      geo = names(PAESI))) |>
  filter(time %in% c(2000, 2025)) |>
  select(geo, time, values) |>
  pivot_wider(names_from = time, values_from = values, names_prefix = "a") |>
  transmute(geo, crescita = (a2025 / a2000 - 1) * 100)

d1 <- spesa |>
  inner_join(pil_pc, by = "geo") |>
  mutate(paese = PAESI[geo],
         colore = if_else(geo == "IT", COL_ROSSO, COL_NERO))

write_csv(d1, file.path(OUT, "01_spesa_crescita.csv"))

fmt_punti <- function(x) ifelse(x > 0, paste0("+", fmt_num(x, 0)),
                                ifelse(x < 0, paste0("−", fmt_num(abs(x), 0)), "0"))

trend <- lm(crescita ~ var_spesa, data = d1)
x_inizio <- -6.4
y_inizio <- predict(trend, newdata = tibble(var_spesa = x_inizio))

p1 <- ggplot(d1, aes(var_spesa, crescita)) +
  geom_vline(xintercept = 0, colour = COL_GRIGIO, linewidth = 0.4, linetype = "dashed") +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, fullrange = TRUE,
              color = COL_GRIGIO_SCURO, linewidth = 0.7, linetype = "dashed") +
  annotate("text", x = x_inizio, y = y_inizio + 2.3, label = "Tendenza", hjust = 0,
           family = "Source Sans Pro", fontface = "bold", size = 3.3, color = COL_GRIGIO_SCURO) +
  geom_point(aes(color = colore), size = 2.6) +
  geom_text_repel(aes(label = paese, color = colore),
                  family = "Source Sans Pro", fontface = "bold", size = 3.4,
                  box.padding = 0.4, min.segment.length = 0.3, segment.colour = COL_GRIGIO,
                  seed = 1) +
  annotate("text", x = -0.3, y = 44, label = "Spesa scesa", hjust = 1,
           family = "Source Sans Pro", size = 3.3, color = COL_GRIGIO_SCURO) +
  annotate("text", x = 0.3, y = 44, label = "Spesa salita", hjust = 0,
           family = "Source Sans Pro", size = 3.3, color = COL_GRIGIO_SCURO) +
  scale_color_identity() +
  scale_x_continuous(labels = fmt_punti, limits = c(-6.5, 11.5), breaks = seq(-6, 10, 2)) +
  scale_y_continuous(labels = function(x) paste0("+", x, "%"),
                     limits = c(0, 46), breaks = seq(10, 40, 10)) +
  labs(title = "Chi ha aumentato la spesa non è cresciuto di più",
       subtitle = "Variazione della spesa pubblica e crescita del PIL reale per abitante, 16 paesi dell'Europa occidentale, 2000-2025",
       x = "Variazione della spesa pubblica, in punti di PIL",
       y = "Crescita del PIL per abitante",
       caption = CAP_EUROSTAT) +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 10, color = "#1C1C1C", hjust = 0.5,
                                    margin = margin(t = 0.3, unit = "cm")),
        axis.title.y = element_text(size = 10, color = "#1C1C1C", hjust = 0.5,
                                    angle = 90, margin = margin(r = 0.3, unit = "cm")))

ggsave(file.path(OUT, "01_spesa_crescita.png"), p1,
       width = 9, height = 7.5, units = "in", dpi = 220, bg = "white")

# --- 02 Interessi e istruzione ----------------------------------------------

d2 <- get_eurostat("gov_10a_exp", time_format = "num",
                   filters = list(na_item = "TE", sector = "S13", unit = "PC_GDP",
                                  geo = "IT", cofog99 = c("GF09", "GF0107"))) |>
  filter(!is.na(values)) |>
  transmute(anno = time,
            serie = if_else(cofog99 == "GF09", "Spesa per l'istruzione",
                            "Spesa per interessi sul debito"),
            valore = values) |>
  group_by(anno) |> filter(n() == 2) |> ungroup()

write_csv(d2, file.path(OUT, "02_interessi_istruzione.csv"))

anno_min2 <- min(d2$anno); anno_max2 <- max(d2$anno)
col2 <- c("Spesa per l'istruzione" = COL_BLU, "Spesa per interessi sul debito" = COL_ROSSO)

lab2_fine <- d2 |> filter(anno == anno_max2)
lab2_val  <- d2 |> filter(anno == anno_min2)
lab2_ult  <- d2 |> filter(anno == anno_max2)

p2 <- ggplot(d2, aes(anno, valore, color = serie)) +
  geom_line(linewidth = 0.9) +
  geom_point(data = bind_rows(lab2_val, lab2_ult), size = 1.8) +
  geom_text(data = lab2_val,
            aes(label = paste0(fmt_num(valore), "%")),
            hjust = 0, vjust = -1.2, family = "Source Sans Pro", fontface = "bold", size = 3.4) +
  annotate("text", x = anno_max2 - 0.2, y = max(lab2_ult$valore) + 0.5,
           label = paste0(fmt_num(max(lab2_ult$valore)), "%"), hjust = 1,
           family = "Source Sans Pro", fontface = "bold", size = 3.4, color = COL_NERO) +
  annotate("text", x = anno_max2 + 0.8, y = 4.3, label = "Spesa per\nl'istruzione",
           hjust = 0, family = "Source Sans Pro", fontface = "bold", size = 3.6, color = COL_BLU,
           lineheight = 0.95) +
  annotate("text", x = anno_max2 + 0.8, y = 3.55, label = "Spesa per interessi\nsul debito",
           hjust = 0, family = "Source Sans Pro", fontface = "bold", size = 3.6, color = COL_ROSSO,
           lineheight = 0.95) +
  scale_color_manual(values = col2) +
  scale_x_continuous(limits = c(anno_min2, anno_max2 + 6.5),
                     breaks = seq(2005, 2020, 5) |> c(anno_min2, anno_max2) |> sort(),
                     expand = c(0.01, 0.01)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 7), breaks = 1:7, expand = c(0, 0)) +
  labs(title = "In Italia gli interessi costano quanto l'istruzione",
       subtitle = paste0("Spesa pubblica per interessi sul debito e per l'istruzione, dalla scuola all'università,\n",
                         "in percentuale del PIL, Italia, ", anno_min2, "-", anno_max2),
       caption = CAP_EUROSTAT) +
  coord_cartesian(clip = "off") +
  theme_linechart() + theme(legend.position = "none")

ggsave(file.path(OUT, "02_interessi_istruzione.png"), p2,
       width = 9, height = 6.5, units = "in", dpi = 220, bg = "white")

# --- 03 PNRR ------------------------------------------------------------------
# UPB, Rapporto sulla politica di bilancio 2026, Riquadro 1.1, Tab. R1.1.1 (MeMo-It)
# Governo, PNRR aprile 2021: +3,6 punti nel 2026 con investimenti ad alta efficienza,
# +1,8 punti con bassa efficienza

d3 <- tibble(anno = 2021:2030,
             livello = c(0.2, 0.6, 1.0, 0.8, 1.3, 1.8, 1.4, 1.2, 1.1, 1.1))
gov_testo <- paste0("Stime del governo nel 2021 per il 2026:\n",
                    "+3,6 con investimenti molto efficienti,\n",
                    "+1,8 con investimenti poco efficienti")

write_csv(d3, file.path(OUT, "03_pnrr_upb.csv"))

p3 <- ggplot(d3, aes(anno, livello)) +
  geom_col(fill = COL_ROSSO, width = 0.65) +
  geom_text(aes(label = paste0("+", fmt_num(livello))), vjust = -0.6,
            color = COL_ROSSO, family = "Source Sans Pro", fontface = "bold", size = 3.6) +
  annotate("segment", x = 2025.6, xend = 2026.4, y = 3.6, yend = 3.6,
           colour = COL_GRIGIO_SCURO, linewidth = 0.6, linetype = "dashed") +
  annotate("text", x = 2026.55, y = 3.6, label = gov_testo, hjust = 0, vjust = 0.75,
           family = "Source Sans Pro", size = 3.3, color = COL_GRIGIO_SCURO, lineheight = 1.05) +
  scale_x_continuous(breaks = 2021:2030, expand = c(0.02, 0.02)) +
  scale_y_continuous(labels = function(x) paste0("+", fmt_num(x, 0)),
                     limits = c(0, 4), breaks = 1:4, expand = c(0, 0)) +
  labs(title = "Il PNRR ha alzato il PIL meno del previsto",
       subtitle = paste0("Effetto cumulato del PNRR sul livello del PIL: di quanto il PIL di ciascun anno è più alto rispetto a\n",
                         "uno scenario senza piano, in punti percentuali, secondo le stime dell'UPB di giugno 2026, Italia, 2021-2030"),
       caption = CAP_UPB) +
  coord_cartesian(clip = "off") +
  theme_linechart() + theme(legend.position = "none")

ggsave(file.path(OUT, "03_pnrr_upb.png"), p3,
       width = 9, height = 6.5, units = "in", dpi = 220, bg = "white")

# --- 04 Sondaggi tra gli economisti -----------------------------------------
# Clark Center (Chicago Booth), panel US Economic Experts. Quota d'accordo
# (Agree + Strongly agree) su chi ha espresso un'opinione.
# economic-stimulus-revisited (2014): A 36/37, B 25/37
# infrastructure-revisited (2014): A 36/38, B 18/38
# science-funding (2026): B 37/40

d4 <- tibble(
  tema = c("Stimolo di Obama del 2009", "Stimolo di Obama del 2009",
           "Infrastrutture", "Infrastrutture", "Ricerca pubblica"),
  affermazione = c("Lo stimolo del 2009 ha ridotto\nla disoccupazione (2014)",
                   "I benefici dello stimolo del 2009\nsuperano i costi (2014)",
                   "Più spesa in strade, ferrovie, ponti e\naeroporti può alzare i redditi medi (2014)",
                   "Molti progetti avrebbero rendimenti\nbassi o negativi (2014)",
                   "Il finanziamento federale alla ricerca si è\nripagato con più produttività (2026)"),
  accordo = c(36 / 37, 25 / 37, 36 / 38, 18 / 38, 37 / 40) * 100
) |>
  mutate(affermazione = factor(affermazione, levels = rev(affermazione)),
         etichetta = paste0(floor(accordo + 0.5), "%"))

write_csv(d4, file.path(OUT, "04_sondaggi_economisti.csv"))

p4 <- ggplot(d4, aes(accordo, affermazione)) +
  geom_col(fill = COL_BLU, width = 0.6) +
  geom_text(aes(label = etichetta), hjust = -0.2, family = "Source Sans Pro",
            fontface = "bold", size = 3.6, color = COL_BLU) +
  scale_x_continuous(limits = c(0, 110), breaks = seq(25, 100, 25),
                     labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  labs(title = "Gli economisti distinguono il breve dal lungo periodo",
       subtitle = paste0("Quota di economisti d'accordo con ciascuna affermazione sul totale di chi ha espresso un'opinione,\n",
                         "panel di economisti accademici del Clark Center, Stati Uniti, 2014 e 2026"),
       caption = CAP_CLARK) +
  coord_cartesian(clip = "off") +
  theme_linechart() + theme(legend.position = "none",
                  axis.text.y = element_text(size = 9.5, color = "#1C1C1C", hjust = 1, lineheight = 1.1),
                  axis.line.y = element_blank(),
                  axis.line.x = element_blank(),
                  axis.text.x = element_blank())

ggsave(file.path(OUT, "04_sondaggi_economisti.png"), p4,
       width = 9, height = 6.5, units = "in", dpi = 220, bg = "white")

# --- Controlli ------------------------------------------------------------------

cat("\n01: cor", round(cor(d1$var_spesa, d1$crescita), 2), "pendenza", round(coef(trend)[2], 2), "n", nrow(d1))
cat("\n01:", paste0(d1$paese, " var spesa ", round(d1$var_spesa, 1), " crescita ", round(d1$crescita, 1), collapse = " | "))
cat("\n02:", paste0(lab2_val$serie, " ", lab2_val$anno, " ", lab2_val$valore, collapse = " | "))
cat("\n03: picco UPB", max(d3$livello), "nel", d3$anno[which.max(d3$livello)])
cat("\n04:", paste0(round(d4$accordo, 1), collapse = " | "), "\n")
