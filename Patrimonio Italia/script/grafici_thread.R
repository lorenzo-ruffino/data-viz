library(tidyverse)
library(readxl)
library(showtext)

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

palette_paesi <- c(
  "Italia"     = "#F12938",
  "Francia"    = "#A82DE3",
  "Germania"   = "#1C1C1C",
  "Spagna"     = "#F2A900",
  "Area euro"  = "#0478EA"
)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
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

CAP_BCE   <- "Elaborazione di Lorenzo Ruffino su dati Bce e Banca d'Italia"
CAP_BDI   <- "Elaborazione di Lorenzo Ruffino su dati Banca d'Italia"
CAP_OCSE  <- "Elaborazione di Lorenzo Ruffino su dati Ocse"

fmt0 <- function(x) formatC(x, format = "f", digits = 0, decimal.mark = ",", big.mark = ".")
fmt1 <- function(x) formatC(x, format = "f", digits = 1, decimal.mark = ",")
fmt_pct_segno <- function(x) paste0(ifelse(x > 0, "+", ""), fmt0(x), "%")

# --- 01. Quota del 10% più ricco, Italia e altri paesi (Bce) -----------------

dwa <- read_csv("../input/dwa_serie_q4.csv", show_col_types = FALSE,
                col_types = cols(paese = col_character(), serie = col_character()))

quota <- dwa %>%
  filter(serie == "share_T10", anno >= 2010, anno <= 2025) %>%
  mutate(paese = recode(paese, IT = "Italia", FR = "Francia", DE = "Germania",
                        ES = "Spagna", I9 = "Area euro")) %>%
  filter(!is.na(valore))

etich_q <- quota %>% filter(anno == 2025)
estremi_it <- quota %>% filter(paese == "Italia", anno %in% c(2010, 2025))

p1 <- ggplot(quota, aes(anno, valore, colour = paese)) +
  geom_line(aes(linewidth = paese == "Italia")) +
  geom_point(data = filter(quota, paese == "Italia"), size = 1.6) +
  scale_linewidth_manual(values = c(`TRUE` = 1.1, `FALSE` = 0.6), guide = "none") +
  geom_text(data = etich_q, aes(label = paese), hjust = 0, nudge_x = 0.3,
            size = 3.4, fontface = "bold", family = "Source Sans Pro",
            position = position_nudge(y = c(Italia = 0.45, Germania = -0.35, Francia = -0.2,
                                            Spagna = 0.35, `Area euro` = 0)[etich_q$paese])) +
  geom_text(data = estremi_it, aes(label = paste0(fmt1(valore), "%")),
            vjust = c(1.9, -1.1), hjust = c(0.2, 0.8), size = 3.5, fontface = "bold",
            family = "Source Sans Pro", show.legend = FALSE) +
  scale_colour_manual(values = palette_paesi) +
  scale_x_continuous(limits = c(2010, 2028), breaks = seq(2010, 2025, 5), expand = c(0.01, 0)) +
  scale_y_continuous(limits = c(46, 63), breaks = seq(46, 62, 4),
                     labels = function(x) paste0(x, "%")) +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  theme(legend.position = "none") +
  labs(title = "Cresce la quota di ricchezza del 10% più ricco",
       subtitle = "Quota della ricchezza netta delle famiglie posseduta dal 10% più ricco, 2010-2025",
       caption = CAP_BCE)

ggsave("../output/01_quota_top10_paesi.png", p1, width = 9, height = 6.5, dpi = 220, bg = "white")
write_csv(quota, "../output/01_quota_top10_paesi.csv")

# --- 02. Variazione reale della ricchezza per gruppo (Bce) --------------------

var <- read_csv("../input/dwa_variazioni_reali_2010_2025.csv", show_col_types = FALSE)

gruppi <- var %>%
  filter(voce == "Ricchezza netta") %>%
  mutate(nome = recode(gruppo, Top10 = "10% più ricco", Medio40 = "40% centrale",
                       Bottom50 = "50% più povero", Tot = "Tutte le famiglie"),
         nome = factor(nome, levels = c("10% più ricco", "40% centrale", "50% più povero", "Tutte le famiglie")),
         colore = ifelse(variazione_pct > 0, COL_BLU, COL_ROSSO),
         colore = ifelse(gruppo == "Tot", COL_GRIGIO_SCURO, colore))

p2 <- ggplot(gruppi, aes(nome, variazione_pct)) +
  geom_col(aes(fill = colore), width = 0.6) +
  geom_hline(yintercept = 0, colour = COL_NERO, linewidth = 0.3) +
  geom_text(aes(label = fmt_pct_segno(variazione_pct), colour = colore,
                vjust = ifelse(variazione_pct > 0, -0.6, 1.6)),
            size = 4, fontface = "bold", family = "Source Sans Pro") +
  scale_fill_identity() + scale_colour_identity() +
  scale_y_continuous(limits = c(-28, 15), breaks = seq(-25, 10, 5),
                     labels = function(x) paste0(x, "%")) +
  theme_linechart() +
  theme(axis.line.x = element_blank(), axis.text.x = element_text(size = 10, face = "bold")) +
  labs(title = "Cala la ricchezza reale del ceto medio e dei più poveri",
       subtitle = "Variazione della ricchezza netta al netto dell'inflazione per gruppo di famiglie, Italia, 2010-2025",
       caption = CAP_BCE)

ggsave("../output/02_variazione_reale_gruppi.png", p2, width = 9, height = 6.5, dpi = 220, bg = "white")

# --- 03. Cosa è cambiato: variazione reale per tipo di bene (Bce) -------------

beni <- var %>%
  filter(gruppo == "Tot", !voce %in% c("Ricchezza netta", "Debiti")) %>%
  mutate(voce = recode(voce, "Beni delle imprese familiari" = "Beni delle piccole imprese familiari",
                       "Partecipazioni non quotate" = "Quote di società non quotate"),
         voce = fct_reorder(voce, variazione_mld),
         colore = ifelse(variazione_mld > 0, COL_BLU, COL_ROSSO))

p3 <- ggplot(beni, aes(variazione_mld, voce)) +
  geom_col(aes(fill = colore), width = 0.65) +
  geom_vline(xintercept = 0, colour = COL_NERO, linewidth = 0.3) +
  geom_text(aes(label = paste0(ifelse(variazione_mld > 0, "+", "−"), "€ ", fmt0(abs(variazione_mld)), " mld"),
                colour = colore, hjust = ifelse(variazione_mld > 0, -0.1, 1.1)),
            size = 3.4, fontface = "bold", family = "Source Sans Pro") +
  scale_fill_identity() + scale_colour_identity() +
  scale_x_continuous(limits = c(-2500, 1750), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  theme(axis.text.x = element_blank(), axis.line = element_blank(),
        axis.text.y = element_text(size = 10, hjust = 1)) +
  labs(title = "Le case perdono valore, crescono imprese e fondi",
       subtitle = "Variazione della ricchezza delle famiglie al netto dell'inflazione, prezzi 2025, Italia, 2010-2025",
       caption = CAP_BCE)

ggsave("../output/03_variazione_reale_beni.png", p3, width = 9, height = 6.5, dpi = 220, bg = "white")
write_csv(var, "../output/02_03_variazioni_reali.csv")

# --- 04. Giovani e anziani (Banca d'Italia, indagine sui bilanci) -------------

s40 <- read_excel("../input/bankitalia_shiw_tavole_storiche.xlsx", sheet = "tav.S40", col_names = FALSE)
anni <- unlist(s40[4, -1])
riga <- function(nome) as.numeric(unlist(s40[which(s40[[1]] == nome), -1]))
eta <- tibble(anno = anni,
              `Fino a 30 anni` = riga("fino a 30 anni"),
              `Da 31 a 40 anni` = riga("da 31 a 40 anni"),
              `Oltre 65 anni` = riga("oltre 65 anni"),
              Totale = riga("Totale")) %>%
  filter(!is.na(Totale), anno != "2022 (2)") %>%
  mutate(anno = as.numeric(substr(anno, 1, 4))) %>%
  pivot_longer(-c(anno, Totale), names_to = "classe", values_to = "ricchezza") %>%
  mutate(rel = ricchezza / Totale * 100) %>%
  filter(classe != "Fino a 30 anni")   # campione piccolo e molto volatile

col_eta <- c("Da 31 a 40 anni" = COL_ROSSO, "Oltre 65 anni" = COL_BLU)
estremi_eta <- eta %>% filter(anno %in% c(1991, 2022))

p4 <- ggplot(eta, aes(anno, rel, colour = classe)) +
  geom_hline(yintercept = 100, colour = COL_GRIGIO, linewidth = 0.4, linetype = "dashed") +
  annotate("text", x = 1991, y = 102.5, label = "Media delle famiglie", hjust = 0,
           size = 3, colour = COL_GRIGIO_SCURO, family = "Source Sans Pro") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.6) +
  geom_text(data = estremi_eta, aes(label = paste0(fmt0(rel), "%"),
                                    vjust = ifelse(anno == 2022 & classe == "Da 31 a 40 anni", 2.1, -1.1)), size = 3.5, fontface = "bold", family = "Source Sans Pro") +
  geom_text(data = filter(eta, anno == 2022), aes(label = classe), hjust = 0, nudge_x = 0.8,
            size = 3.5, fontface = "bold", family = "Source Sans Pro") +
  scale_colour_manual(values = col_eta) +
  scale_x_continuous(limits = c(1990, 2030), breaks = seq(1990, 2020, 5)) +
  scale_y_continuous(limits = c(40, 125), breaks = seq(40, 120, 20),
                     labels = function(x) paste0(x, "%")) +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  theme(legend.position = "none") +
  labs(title = "Gli anziani sono sempre più ricchi dei giovani",
       subtitle = "Ricchezza netta media per età del capofamiglia in rapporto alla media di tutte le famiglie, Italia, 1991-2022",
       caption = CAP_BDI)

ggsave("../output/04_ricchezza_eta.png", p4, width = 9, height = 6.5, dpi = 220, bg = "white")
write_csv(eta, "../output/04_ricchezza_eta.csv")

# --- 05. Imposta di successione in percentuale del Pil (Ocse) ----------------

rev <- read_csv("../input/ocse_tasse_patrimonio_pil.csv", show_col_types = FALSE)
succ <- rev %>%
  filter(voce == "T_4300", paese %in% c("ITA", "FRA", "DEU", "ESP", "GBR", "OECD_REP")) %>%
  group_by(paese) %>% filter(anno == max(anno)) %>% ungroup() %>%
  mutate(nome = recode(paese, ITA = "Italia", FRA = "Francia", DEU = "Germania", ESP = "Spagna",
                       GBR = "Regno Unito", OECD_REP = "Media Ocse"),
         nome = fct_reorder(nome, pct_pil),
         colore = case_when(paese == "ITA" ~ COL_ROSSO, paese == "OECD_REP" ~ COL_BLU, TRUE ~ COL_GRIGIO))

p5 <- ggplot(succ, aes(pct_pil, nome)) +
  geom_col(aes(fill = colore), width = 0.6) +
  geom_text(aes(label = paste0(formatC(pct_pil, format = "f", digits = 2, decimal.mark = ","), "%"),
                colour = colore), hjust = -0.2, size = 3.6, fontface = "bold", family = "Source Sans Pro") +
  scale_fill_identity() + scale_colour_identity() +
  scale_x_continuous(limits = c(0, 0.85), expand = c(0, 0)) +
  theme_linechart() +
  theme(axis.text.x = element_blank(), axis.line = element_blank(),
        axis.text.y = element_text(size = 10, hjust = 1)) +
  labs(title = "In Italia l'imposta di successione quasi non c'è",
       subtitle = "Gettito delle imposte su successioni e donazioni in percentuale del Pil, 2024 (media Ocse 2023)",
       caption = CAP_OCSE)

ggsave("../output/05_successioni_pil.png", p5, width = 9, height = 6.5, dpi = 220, bg = "white")
write_csv(succ, "../output/05_successioni_pil.csv")

# --- Controlli ----------------------------------------------------------------

cat("\nQuota top 10% Italia 2010/2025:", estremi_it$valore, "\n")
print(gruppi %>% select(nome, variazione_pct))
print(eta %>% filter(anno %in% c(1991, 2022)))
print(succ %>% select(nome, anno, pct_pil))
