# Occupazione nelle professioni informatiche per fascia d'età, USA,
# indice ottobre 2022 = 100.
#
# Fonte: Brynjolfsson, Chandar, Chen (2025), "Canaries in the Coal Mine?
# Six Facts about the Recent Employment Effects of Artificial Intelligence",
# Stanford Digital Economy Lab, Figura A1 (Computer Occupations, in alto).
# https://digitaleconomy.stanford.edu/app/uploads/2025/11/CanariesintheCoalMine_Nov25.pdf
# Dati sottostanti: ADP payroll, panel bilanciato 2021-2025.
# Il file di replicazione non è pubblico: i valori sono stati ricostruiti
# leggendo la Figura A1 del paper.

library(tidyverse)
library(showtext)

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

# ---------------------------------------------------------------------------
# Punti di ancoraggio (letti dalla Figura A1) e interpolazione spline
# ---------------------------------------------------------------------------

ancore <- tribble(
  ~mese,        ~giovani, ~adulti,
  "2022-10-01", 1.000,    1.000,
  "2023-01-01", 1.000,    1.010,
  "2023-04-01", 0.990,    1.020,
  "2023-07-01", 0.980,    1.030,
  "2023-10-01", 0.970,    1.040,
  "2024-01-01", 0.960,    1.050,
  "2024-04-01", 0.940,    1.060,
  "2024-07-01", 0.920,    1.070,
  "2024-10-01", 0.910,    1.080,
  "2025-01-01", 0.905,    1.085,
  "2025-04-01", 0.900,    1.090,
  "2025-07-01", 0.890,    1.095,
  "2025-09-01", 0.880,    1.100
) %>%
  mutate(mese = as.Date(mese),
         mese_num = as.numeric(mese))

# Interpolazione spline mensile per avere una linea fluida
mesi <- seq(as.Date("2022-10-01"), as.Date("2025-09-01"), by = "month")
g_smooth <- spline(ancore$mese_num, ancore$giovani, xout = as.numeric(mesi))$y
a_smooth <- spline(ancore$mese_num, ancore$adulti,  xout = as.numeric(mesi))$y

df <- tibble(
  mese = mesi,
  `22-25 anni` = g_smooth * 100,
  `35-40 anni` = a_smooth * 100
) %>%
  pivot_longer(-mese, names_to = "eta", values_to = "indice")

write_csv(df, "../output/primo_gradino_eroso.csv")

# ---------------------------------------------------------------------------
# Tema e palette
# ---------------------------------------------------------------------------

COL_BLU    <- "#0478EA"
COL_ROSSO  <- "#F12938"
COL_GRIGIO <- "#9A9A9A"

palette_eta <- c(
  "22-25 anni" = COL_ROSSO,
  "35-40 anni" = COL_BLU
)

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
      plot.title = element_text(size = 14, color = "#1C1C1C", hjust = 0,
                                margin = margin(b = 0.1, unit = "cm")),
      plot.subtitle = element_text(size = 9, color = "#1C1C1C", hjust = 0,
                                   lineheight = 1.35,
                                   margin = margin(b = 0.25, t = 0.1, unit = "cm")),
      plot.caption = element_text(size = 9, color = "#1C1C1C", hjust = 1,
                                  margin = margin(t = 0.5, unit = "cm")),
      ...
    )
}

# ---------------------------------------------------------------------------
# Grafico
# ---------------------------------------------------------------------------

labels_end <- df %>%
  filter(mese == max(mese)) %>%
  mutate(label_text = sprintf("%s\n%d", eta, round(indice)))

# Linea di riferimento a 100
ref_100 <- tibble(yint = 100)

p <- ggplot(df, aes(x = mese, y = indice, color = eta, group = eta)) +
  geom_hline(yintercept = 100, color = COL_GRIGIO,
             linetype = "dashed", linewidth = 0.4) +
  geom_line(linewidth = 1.0) +
  geom_text(data = labels_end,
            aes(label = label_text),
            hjust = 0, nudge_x = 30, vjust = 0.5,
            fontface = "bold", size = 3.8,
            family = "Source Sans Pro",
            lineheight = 1) +
  scale_color_manual(values = palette_eta) +
  scale_x_date(limits = c(as.Date("2022-10-01"), as.Date("2026-04-01")),
               breaks = seq(as.Date("2023-01-01"), as.Date("2025-09-01"),
                            by = "6 months"),
               date_labels = "%Y-%m",
               expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(85, 113),
                     breaks = seq(85, 110, 5),
                     expand = c(0.01, 0.01)) +
  labs(title = "Tra i programmatori USA cala solo l'occupazione dei 22-25enni",
       subtitle = "Indice dell'occupazione nelle professioni informatiche per fascia d'età, Stati Uniti, ottobre 2022 = 100",
       caption = "Elaborazione di Lorenzo Ruffino su Brynjolfsson, Chandar, Chen (2025)") +
  theme_linechart()

ggsave("../output/primo_gradino_eroso.png", p,
       width = 8.5, height = 6.5, units = "in", dpi = 220, bg = "white")
