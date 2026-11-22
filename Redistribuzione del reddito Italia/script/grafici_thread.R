library(tidyverse)
library(showtext)

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

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

CAP_ISTAT    <- "Elaborazione di Lorenzo Ruffino su dati Istat"
CAP_OCSE     <- "Elaborazione di Lorenzo Ruffino su dati Ocse"
CAP_EUROSTAT <- "Elaborazione di Lorenzo Ruffino su dati Eurostat"

fmt1 <- function(x) formatC(x, format = "f", digits = 1, decimal.mark = ",")

# --- 01. Gini prima e dopo l'intervento pubblico, per area (Istat 2025) -----

aree <- read_csv("../input/istat_famimod_2025_gini_aree.csv", show_col_types = FALSE) %>%
  mutate(area = factor(area, levels = rev(c("Italia", "Nord", "Centro", "Mezzogiorno"))))

punti <- aree %>%
  pivot_longer(c(primario, lordo, disponibile), names_to = "fase", values_to = "gini") %>%
  mutate(fase = factor(fase, levels = c("primario", "lordo", "disponibile")))

COL_ARANCIONE <- "#E07700"
col_fasi <- c(primario = COL_ROSSO, lordo = COL_ARANCIONE, disponibile = COL_BLU)

etichette_fasi <- tibble(
  fase  = factor(c("primario", "lordo", "disponibile"), levels = levels(punti$fase)),
  gini  = c(47.28, 35.83, 31.17),
  testo = c("Reddito\ndi mercato", "Dopo pensioni\ne trasferimenti", "Dopo imposte\ne contributi")
)

p1 <- ggplot(punti, aes(y = area)) +
  geom_segment(data = aree, aes(x = primario, xend = lordo + 0.35, yend = area),
               colour = COL_ARANCIONE, linewidth = 0.9,
               arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  geom_segment(data = aree, aes(x = lordo, xend = disponibile + 0.35, yend = area),
               colour = COL_BLU, linewidth = 0.9,
               arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  geom_point(aes(x = gini, colour = fase), size = 2.6) +
  geom_text(aes(x = gini, label = fmt1(gini), colour = fase),
            vjust = -1.1, size = 3.5, fontface = "bold", family = "Source Sans Pro") +
  geom_text(data = etichette_fasi, aes(x = gini, y = 4.75, label = testo, colour = fase),
            size = 3.3, fontface = "bold", family = "Source Sans Pro",
            lineheight = 0.9, vjust = 0) +
  scale_colour_manual(values = col_fasi) +
  scale_x_continuous(limits = c(25, 52), expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(add = c(0.5, 1.4))) +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  theme(legend.position = "none",
        axis.text.x = element_blank(), axis.line = element_blank(),
        axis.text.y = element_text(size = 10, face = "bold", hjust = 0)) +
  labs(title = "Lo Stato riduce di un terzo la disuguaglianza",
       subtitle = "Indice di Gini del reddito equivalente prima e dopo l'intervento pubblico, per area geografica, 2025",
       caption = CAP_ISTAT)

ggsave("../output/01_gini_prima_dopo_stato.png", p1, width = 9, height = 6.5, dpi = 220, bg = "white")
write_csv(aree, "../output/01_gini_prima_dopo_stato.csv")

# --- 02. Contributi e imposte per quinto (Istat 2025) -----------------------

quinti <- read_csv("../input/istat_famimod_2025_prelievo_quinti.csv", show_col_types = FALSE) %>%
  mutate(contributi = contributi_datori + contributi_lavoratori,
         nome = factor(c("20% più povero", "Secondo quinto", "Terzo quinto",
                         "Quarto quinto", "20% più ricco")[quinto],
                       levels = c("20% più povero", "Secondo quinto", "Terzo quinto",
                                  "Quarto quinto", "20% più ricco")))

voci <- quinti %>%
  select(nome, contributi, irpef, altre_imposte) %>%
  pivot_longer(-nome, names_to = "voce", values_to = "quota") %>%
  mutate(voce = factor(voce, levels = c("altre_imposte", "irpef", "contributi"),
                       labels = c("Altre imposte dirette", "Irpef", "Contributi sociali")))

totali <- voci %>% group_by(nome) %>% summarise(tot = sum(quota))

p2 <- ggplot(voci, aes(x = nome, y = quota, fill = voce)) +
  geom_col(width = 0.65) +
  geom_text(data = filter(voci, quota >= 3),
            aes(label = paste0(fmt1(quota), "%")), position = position_stack(vjust = 0.5),
            colour = "white", size = 3.3, fontface = "bold", family = "Source Sans Pro") +
  geom_text(data = totali, aes(x = nome, y = tot, label = paste0(fmt1(tot), "%"), fill = NULL),
            vjust = -0.6, size = 3.6, fontface = "bold", family = "Source Sans Pro", colour = COL_NERO) +
  scale_fill_manual(values = c("Contributi sociali" = COL_BLU,
                               "Irpef" = COL_ROSSO,
                               "Altre imposte dirette" = COL_GIALLO),
                    breaks = c("Contributi sociali", "Irpef", "Altre imposte dirette")) +
  scale_y_continuous(limits = c(0, 43), breaks = seq(10, 40, 10),
                     labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  theme_linechart() +
  theme(legend.position = "top", legend.justification = "left",
        axis.line.y = element_blank()) +
  labs(title = "Il prelievo cresce con il reddito grazie all'Irpef",
       subtitle = "Contributi sociali e imposte dirette in percentuale del reddito lordo familiare, per quinto di reddito\ndisponibile equivalente, Italia, 2025",
       caption = CAP_ISTAT)

ggsave("../output/02_prelievo_per_quinto.png", p2, width = 9, height = 6.5, dpi = 220, bg = "white")
write_csv(quinti, "../output/02_prelievo_per_quinto.csv")

# --- 03. Gini di mercato e disponibile nel lungo periodo (Ocse) --------------

oecd <- read_csv("../input/oecd_idd.csv", show_col_types = FALSE) %>%
  filter(REF_AREA == "ITA", AGE == "_T", DEFINITION == "D_CUR",
         MEASURE %in% c("INC_MRKT_GINI", "INC_DISP_GINI")) %>%
  filter((METHODOLOGY == "METH2011" & TIME_PERIOD <= 2010) |
         (METHODOLOGY == "METH2012" & TIME_PERIOD >= 2011)) %>%
  transmute(anno = TIME_PERIOD, misura = MEASURE, gini = OBS_VALUE * 100) %>%
  filter(!is.na(gini)) %>%
  mutate(serie = if_else(misura == "INC_MRKT_GINI", "Reddito di mercato", "Reddito disponibile"))

larga <- oecd %>% select(anno, serie, gini) %>% pivot_wider(names_from = serie, values_from = gini)

estremi <- oecd %>% group_by(serie) %>% filter(anno %in% c(min(anno), max(anno))) %>% ungroup()

p3 <- ggplot(oecd, aes(x = anno, y = gini)) +
  geom_ribbon(data = larga, aes(x = anno, ymin = `Reddito disponibile`, ymax = `Reddito di mercato`),
              inherit.aes = FALSE, fill = COL_GRIGIO, alpha = 0.15) +
  geom_vline(xintercept = 2010.5, colour = COL_GRIGIO, linewidth = 0.4, linetype = "dashed") +
  annotate("text", x = 2010.8, y = 25.5, label = "Dal 2011 cambia\nla definizione di reddito",
           hjust = 0, size = 3, colour = COL_GRIGIO_SCURO, family = "Source Sans Pro", lineheight = 0.9) +
  geom_line(aes(colour = serie), linewidth = 0.9) +
  geom_point(aes(colour = serie), size = 1.7) +
  geom_text(data = estremi,
            aes(label = fmt1(gini), colour = serie, hjust = if_else(anno == min(anno), 0.5, 0.5)),
            vjust = -1.1, size = 3.5, fontface = "bold", family = "Source Sans Pro") +
  annotate("text", x = 2024, y = 50.9, label = "Reddito di mercato", hjust = 0,
           size = 3.6, fontface = "bold", colour = COL_ROSSO, family = "Source Sans Pro") +
  annotate("text", x = 2024, y = 32.5, label = "Reddito disponibile", hjust = 0,
           size = 3.6, fontface = "bold", colour = COL_BLU, family = "Source Sans Pro") +
  annotate("text", x = 2004, y = 42, label = "Effetto di imposte\ne trasferimenti",
           size = 3.3, colour = COL_GRIGIO_SCURO, fontface = "bold", family = "Source Sans Pro", lineheight = 0.9) +
  scale_colour_manual(values = c("Reddito di mercato" = COL_ROSSO, "Reddito disponibile" = COL_BLU)) +
  scale_x_continuous(limits = c(1983, 2035), breaks = seq(1985, 2020, 5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(20, 56), breaks = seq(20, 55, 5), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  theme(legend.position = "none") +
  labs(title = "Cresce la disuguaglianza di mercato, non quella finale",
       subtitle = "Indice di Gini del reddito di mercato e del reddito disponibile equivalente, Italia, 1984-2023",
       caption = CAP_OCSE)

ggsave("../output/03_gini_mercato_disponibile_ocse.png", p3, width = 9, height = 6.5, dpi = 220, bg = "white")
write_csv(larga, "../output/03_gini_mercato_disponibile_ocse.csv")

# --- 04. Effetto dei trasferimenti esclusi le pensioni, paesi Ue -------------

nomi_ue <- c(AT = "Austria", BE = "Belgio", BG = "Bulgaria", CY = "Cipro", CZ = "Cechia",
             DE = "Germania", DK = "Danimarca", EE = "Estonia", EL = "Grecia", ES = "Spagna",
             FI = "Finlandia", FR = "Francia", HR = "Croazia", HU = "Ungheria", IE = "Irlanda",
             IT = "Italia", LT = "Lituania", LU = "Lussemburgo", LV = "Lettonia", MT = "Malta",
             NL = "Paesi Bassi", PL = "Polonia", PT = "Portogallo", RO = "Romania", SE = "Svezia",
             SI = "Slovenia", SK = "Slovacchia", EU27_2020 = "Unione europea")

disp <- read_csv("../input/ilc_di12.csv", show_col_types = FALSE) %>%
  filter(age == "TOTAL") %>% select(geo, anno = TIME_PERIOD, disponibile = values)
pre_tutti <- read_csv("../input/ilc_di12b.csv", show_col_types = FALSE) %>%
  select(geo, anno = TIME_PERIOD, prima_di_tutto = values)
pre_no_pens <- read_csv("../input/ilc_di12c.csv", show_col_types = FALSE) %>%
  select(geo, anno = TIME_PERIOD, prima_dei_trasferimenti = values)

europa <- disp %>%
  inner_join(pre_tutti, by = c("geo", "anno")) %>%
  inner_join(pre_no_pens, by = c("geo", "anno")) %>%
  filter(anno == 2025, geo %in% names(nomi_ue)) %>%
  mutate(paese = nomi_ue[geo],
         rid_trasferimenti = prima_dei_trasferimenti - disponibile,
         rid_pensioni = prima_di_tutto - prima_dei_trasferimenti,
         colore = case_when(geo == "IT" ~ COL_ROSSO, geo == "EU27_2020" ~ COL_BLU, TRUE ~ COL_GRIGIO),
         paese = fct_reorder(paese, rid_trasferimenti))

p4 <- ggplot(europa, aes(x = rid_trasferimenti, y = paese)) +
  geom_col(aes(fill = colore), width = 0.7) +
  geom_text(aes(label = fmt1(rid_trasferimenti), colour = colore), hjust = -0.25,
            size = 3.2, fontface = "bold", family = "Source Sans Pro") +
  scale_fill_identity() + scale_colour_identity() +
  scale_x_continuous(limits = c(0, 8.6), expand = c(0, 0)) +
  theme_linechart() +
  theme(axis.text.x = element_blank(), axis.line = element_blank(),
        axis.text.y = element_text(size = 9.5, hjust = 1)) +
  labs(title = "Senza pensioni l'Italia redistribuisce meno della media",
       subtitle = "Punti di indice di Gini tolti dai trasferimenti sociali escluse le pensioni (sostegni a famiglie,\ndisoccupati, poveri, disabili), paesi dell'Unione europea, 2025",
       caption = CAP_EUROSTAT)

ggsave("../output/04_trasferimenti_senza_pensioni_ue.png", p4, width = 8, height = 9, dpi = 220, bg = "white")
write_csv(europa %>% select(-colore), "../output/04_trasferimenti_senza_pensioni_ue.csv")

# --- Controlli ----------------------------------------------------------------

cat("\nIstat 2025 Italia:", unlist(aree[aree$area == "Italia", -1]), "\n")
cat("Ocse primo/ultimo:\n"); print(estremi %>% select(anno, serie, gini))
cat("Eurostat 2025 Italia e Ue:\n")
print(europa %>% filter(geo %in% c("IT", "EU27_2020", "FR", "DE", "ES")) %>%
        select(paese, prima_di_tutto, prima_dei_trasferimenti, disponibile, rid_pensioni, rid_trasferimenti))
cat("Posizione Italia (su 27 paesi):",
    which(levels(droplevels(filter(europa, geo != "EU27_2020")$paese)) %>% rev() == "Italia"), "\n")
