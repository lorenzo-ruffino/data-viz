library(tidyverse)
library(jsonlite)
library(showtext)
library(ggrepel)

# --- Tema -------------------------------------------------------------------
font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"; COL_BLU <- "#0478EA"; COL_VIOLA <- "#A82DE3"
COL_ROSSO  <- "#F12938"; COL_GIALLO <- "#F2A900"; COL_GRIGIO <- "#9A9A9A"
COL_GRIGIO_SCURO <- "#5A5A5A"

theme_linechart <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro"),
      legend.position = "top",
      axis.line = element_line(linewidth = 0.3),
      axis.text = element_text(size = 9, color = "#1C1C1C", hjust = 0.5),
      axis.ticks = element_blank(), axis.title = element_blank(),
      panel.background = element_blank(), panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), plot.background = element_blank(),
      legend.background = element_blank(), legend.box.background = element_blank(),
      legend.key = element_blank(), panel.border = element_blank(),
      legend.title = element_blank(),
      plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), "cm"),
      plot.title.position = "plot",
      legend.text = element_text(size = 10, color = "#1C1C1C", hjust = 0),
      plot.title = element_text(family = "Source Sans Pro SemiBold", size = 14,
                                color = "#1C1C1C", hjust = 0,
                                margin = margin(b = 0.1, unit = "cm")),
      plot.subtitle = element_text(size = 9, color = "#1C1C1C", hjust = 0,
                                   lineheight = 1.35,
                                   margin = margin(b = 0.25, t = 0.1, unit = "cm")),
      plot.caption = element_text(size = 9, color = "#1C1C1C", hjust = 1,
                                  margin = margin(t = 0.5, unit = "cm")),
      ...
    )
}
CAP_WB <- "Elaborazione di Lorenzo Ruffino su dati Banca Mondiale"
fmt_it <- function(x, d = 1) format(round(x, d), nsmall = d, decimal.mark = ",")

IN  <- "../input/"; OUT <- "../output/"
read_wb <- function(f) {
  j <- fromJSON(paste0(IN, f), simplifyVector = TRUE)[[2]]
  tibble(iso3 = j$countryiso3code, paese = j$country$value,
         anno = as.integer(j$date), valore = j$value)
}

# ============================================================================
# 01 — Figli per donna per gruppo di reddito, 1960-2024
# ============================================================================
gruppi <- c(XD = "Paesi ad alto reddito", XT = "Reddito medio-alto",
            XN = "Reddito medio-basso", XM = "Basso reddito")
tfr_gruppi <- map_dfr(names(gruppi), function(c)
  read_wb(paste0("tfr_", c, ".json")) |> mutate(gruppo = gruppi[[c]])) |>
  filter(!is.na(valore)) |> select(gruppo, anno, valore) |> arrange(gruppo, anno)
write_csv(tfr_gruppi, paste0(OUT, "01_fecondita_gruppi_reddito.csv"))

col_gruppi <- c("Paesi ad alto reddito" = COL_NERO, "Reddito medio-alto" = COL_ROSSO,
                "Reddito medio-basso" = COL_BLU, "Basso reddito" = COL_GRIGIO_SCURO)
ultimo <- tfr_gruppi |> group_by(gruppo) |> slice_max(anno, n = 1) |> ungroup() |>
  mutate(lab = paste0(gruppo, "  ", fmt_it(valore, 2)))

p1 <- ggplot(tfr_gruppi, aes(anno, valore, colour = gruppo)) +
  geom_hline(yintercept = 2.1, colour = COL_GRIGIO, linewidth = 0.4, linetype = "dashed") +
  annotate("text", x = 1961, y = 2.1, label = "Soglia di sostituzione: 2,1",
           vjust = -0.5, hjust = 0, size = 3, colour = COL_GRIGIO_SCURO,
           family = "Source Sans Pro") +
  geom_line(linewidth = 0.9) +
  geom_text_repel(data = ultimo, aes(label = lab), hjust = 0, nudge_x = 0.8,
                  direction = "y", size = 3.3, fontface = "bold",
                  family = "Source Sans Pro", segment.colour = NA, seed = 1) +
  scale_colour_manual(values = col_gruppi) +
  scale_x_continuous(limits = c(1960, 2043), breaks = seq(1960, 2020, 10), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 7.2), breaks = 1:7,
                     labels = function(x) format(x, decimal.mark = ","), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_linechart() + theme(legend.position = "none") +
  labs(title = "Il calo della natalità si è spostato ai paesi a reddito medio",
       subtitle = "Numero medio di figli per donna per gruppo di reddito della Banca Mondiale, 1960-2024",
       caption = CAP_WB)
ggsave(paste0(OUT, "01_fecondita_gruppi_reddito.png"), p1, width = 9, height = 6.5,
       units = "in", dpi = 220, bg = "white")

# ============================================================================
# 02 — Correlazione fra reddito e figli, 30 paesi OCSE, 1960-2023
# ============================================================================
ocse30 <- c("AUS","AUT","BEL","CAN","CHL","COL","CRI","DNK","FIN","FRA","DEU","GRC",
            "ISL","IRL","ISR","ITA","JPN","KOR","LUX","MEX","NLD","NZL","NOR","PRT",
            "ESP","SWE","CHE","TUR","GBR","USA")
tfr_all <- read_wb("tfr_all.json") |> filter(iso3 %in% ocse30) |> rename(tfr = valore)
gdp_all <- read_wb("gdp_all.json") |> filter(iso3 %in% ocse30) |> rename(gdp = valore)
corr <- inner_join(tfr_all, gdp_all, by = c("iso3", "paese", "anno")) |>
  filter(!is.na(tfr), !is.na(gdp)) |>
  group_by(anno) |>
  summarise(n = n(), rho = cor(tfr, gdp, method = "spearman"), .groups = "drop") |>
  filter(n >= 24)
write_csv(corr, paste0(OUT, "02_correlazione_reddito_fecondita_ocse.csv"))
minimo <- corr |> slice_min(rho, n = 1)
positivo <- corr |> filter(rho > 0) |> slice_min(anno, n = 1)
cat("Correlazione: minimo", fmt_it(minimo$rho, 2), "nel", minimo$anno,
    "| primo anno positivo:", positivo$anno, "| ultimo:", fmt_it(tail(corr$rho, 1), 2),
    "nel", max(corr$anno), "| paesi per anno:", min(corr$n), "-", max(corr$n), "\n")

p2 <- ggplot(corr, aes(anno, rho)) +
  geom_hline(yintercept = 0, colour = COL_GRIGIO, linewidth = 0.4, linetype = "dashed") +
  geom_line(colour = COL_ROSSO, linewidth = 1.0) +
  geom_point(data = bind_rows(minimo, positivo, tail(corr, 1)), colour = COL_ROSSO, size = 2.4) +
  geom_text(data = minimo, aes(label = paste0(fmt_it(rho, 2), " nel ", anno)),
            vjust = 1.8, size = 3.4, fontface = "bold", family = "Source Sans Pro", colour = COL_NERO) +
  geom_text(data = positivo, aes(label = paste0("Positiva dal ", anno)),
            vjust = -1.2, hjust = 1, size = 3.4, fontface = "bold", family = "Source Sans Pro", colour = COL_NERO) +
  geom_text(data = tail(corr, 1), aes(label = paste0(fmt_it(rho, 2), " nel ", anno)),
            vjust = -1.2, hjust = 1, size = 3.4, fontface = "bold", family = "Source Sans Pro", colour = COL_NERO) +
  annotate("text", x = 1961, y = 0.02, label = "Sopra lo zero i paesi più ricchi fanno più figli",
           hjust = 0, vjust = 0, size = 3, colour = COL_GRIGIO_SCURO, family = "Source Sans Pro") +
  annotate("text", x = 1961, y = -0.02, label = "Sotto lo zero i paesi più ricchi fanno meno figli",
           hjust = 0, vjust = 1, size = 3, colour = COL_GRIGIO_SCURO, family = "Source Sans Pro") +
  scale_x_continuous(limits = c(1960, 2025), breaks = seq(1960, 2020, 10), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-1, 0.6), breaks = seq(-1, 0.5, 0.25),
                     labels = function(x) format(x, decimal.mark = ","), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_linechart() + theme(legend.position = "none") +
  labs(title = "Oggi i paesi più ricchi fanno più figli degli altri",
       subtitle = "Correlazione tra figli per donna e PIL per abitante in 30 paesi OCSE, calcolata anno per anno, 1960-2023",
       caption = CAP_WB)
ggsave(paste0(OUT, "02_correlazione_reddito_fecondita_ocse.png"), p2, width = 9, height = 6.5,
       units = "in", dpi = 220, bg = "white")

# ============================================================================
# 03 — Quattrocento donne: da 1,8 a 1,3 figli per donna
# ============================================================================
cat_fig <- c("Senza figli", "Un figlio", "Due figli", "Tre o più figli")
scen <- tribble(
  ~scenario, ~categoria, ~n,
  "1,8 figli per donna", "Senza figli", 60, "1,8 figli per donna", "Un figlio", 40,
  "1,8 figli per donna", "Due figli", 240, "1,8 figli per donna", "Tre o più figli", 60,
  "1,3 figli per donna", "Senza figli", 104, "1,3 figli per donna", "Un figlio", 92,
  "1,3 figli per donna", "Due figli", 184, "1,3 figli per donna", "Tre o più figli", 20) |>
  mutate(categoria = factor(categoria, levels = cat_fig),
         scenario = factor(scenario, levels = c("1,8 figli per donna", "1,3 figli per donna")))
write_csv(scen, paste0(OUT, "03_quattrocento_donne.csv"))
NCOL3 <- 20
grid <- scen |> group_by(scenario) |> arrange(scenario, categoria) |>
  mutate(end = cumsum(n), start = lag(end, default = 0) + 1) |>
  filter(n > 0) |> rowwise() |> mutate(idx = list(start:end)) |> ungroup() |>
  unnest(idx) |> mutate(col = (idx - 1) %% NCOL3 + 1, row = (idx - 1) %/% NCOL3 + 1)
col_fig <- c("Senza figli" = COL_ROSSO, "Un figlio" = COL_GIALLO,
             "Due figli" = COL_BLU, "Tre o più figli" = COL_VIOLA)
p3 <- ggplot(grid, aes(col, -row, colour = categoria)) +
  geom_point(shape = 16, size = 5.6) +
  facet_wrap(~scenario, nrow = 1) +
  scale_colour_manual(values = col_fig, drop = FALSE) +
  scale_x_continuous(expand = expansion(add = 0.6)) +
  scale_y_continuous(expand = expansion(add = 0.6)) +
  guides(colour = guide_legend(nrow = 1, override.aes = list(size = 4))) +
  coord_equal() +
  theme_linechart() +
  theme(legend.position = "top", legend.justification = "left",
        axis.text = element_blank(), axis.line = element_blank(),
        strip.text = element_text(family = "Source Sans Pro", face = "bold",
                                  size = 11, colour = COL_NERO, hjust = 0),
        panel.spacing = unit(0.9, "cm")) +
  labs(title = "Cosa cambia quando la fecondità scende da 1,8 a 1,3",
       subtitle = "Quattrocento donne per numero di figli. 1,8 era la media dei paesi avanzati fino agli anni Novanta,\n1,3 è la soglia che i demografi chiamano fecondità bassissima",
       caption = "Elaborazione di Lorenzo Ruffino sull'esempio di Fernández-Villaverde e Norrick (2026)")
ggsave(paste0(OUT, "03_quattrocento_donne.png"), p3, width = 9, height = 6,
       units = "in", dpi = 220, bg = "white")

# ============================================================================
# 04 — Crescita G7 + Spagna, tre misure, 1991-2023 (Tabella 1 dello studio)
# ============================================================================
g7 <- tribble(
  ~paese, ~pil, ~adulto, ~ora,
  "Canada", 2.40, 1.38, 1.09, "Francia", 1.56, 1.28, 1.07, "Germania", 1.27, 1.41, 1.37,
  "Italia", 0.79, 0.92, 0.67, "Giappone", 0.79, 1.33, 1.22, "Spagna", 1.92, 1.28, 0.58,
  "Regno Unito", 2.13, 1.63, 1.45, "Stati Uniti", 2.60, 1.73, 1.62)
write_csv(g7, paste0(OUT, "04_crescita_g7_spagna.csv"))
misure <- c(pil = "Crescita del PIL", adulto = "PIL per adulto in età lavorativa",
            ora = "PIL per ora lavorata")
g7l <- g7 |> pivot_longer(-paese, names_to = "misura", values_to = "valore") |>
  mutate(misura = factor(misure[misura], levels = misure),
         paese = fct_reorder(paese, g7$pil[match(paese, g7$paese)]),
         evid = paese %in% c("Italia", "Giappone"))
col_mis <- c("Crescita del PIL" = COL_GRIGIO, "PIL per adulto in età lavorativa" = COL_BLU,
             "PIL per ora lavorata" = COL_ROSSO)
p4 <- ggplot(g7l, aes(valore, paese)) +
  geom_line(aes(group = paese), colour = "#DDDDDD", linewidth = 0.6) +
  geom_point(aes(colour = misura), size = 3.2) +
  geom_text_repel(aes(label = fmt_it(valore, 2), colour = misura),
                  direction = "x", nudge_y = 0.28, size = 2.9, min.segment.length = Inf,
                  family = "Source Sans Pro", fontface = "bold", seed = 1,
                  box.padding = 0.08, point.padding = 0, show.legend = FALSE) +
  scale_colour_manual(values = col_mis) +
  scale_x_continuous(limits = c(0.35, 2.85), breaks = seq(0.5, 2.5, 0.5),
                     labels = function(x) paste0(format(x, decimal.mark = ","), "%"), expand = c(0, 0)) +
  guides(colour = guide_legend(nrow = 1, override.aes = list(size = 3.5))) +
  theme_linechart() + theme(legend.position = "top", legend.justification = "left",
                  axis.text.y = element_text(size = 10, hjust = 1,
                                             face = ifelse(levels(g7l$paese) %in% c("Italia", "Giappone"), "bold", "plain")),
                  axis.line.y = element_blank()) +
  labs(title = "L'Italia cresce poco anche al netto della demografia",
       subtitle = "Crescita media annua del PIL, del PIL per adulto in età lavorativa (15-64 anni) e per ora lavorata, 1991-2023",
       caption = "Elaborazione di Lorenzo Ruffino su dati Fernández-Villaverde e Norrick (2026)")
ggsave(paste0(OUT, "04_crescita_g7_spagna.png"), p4, width = 9, height = 6.5,
       units = "in", dpi = 220, bg = "white")

cat("Gruppi 2024:", paste(ultimo$gruppo, fmt_it(ultimo$valore, 2), collapse = " | "), "\n")
cat("Quattrocento donne — figli totali: 1,8 ->", 40 + 240*2 + 40*3 + 20*4,
    "| 1,3 ->", 92 + 184*2 + 20*3, "| medie:", fmt_it((40 + 240*2 + 40*3 + 20*4)/400, 2),
    fmt_it((92 + 184*2 + 20*3)/400, 2), "\n")
