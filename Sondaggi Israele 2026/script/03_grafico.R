# script/03_grafico.R — eseguito da `cd script && Rscript 03_grafico.R`
# Legge le medie di 02_medie.R e gli errori di 04_errori_storici.py e disegna
# i tre grafici: coalizioni, partiti, errori storici.

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
COL_VERDE  <- "#1B9E77"
COL_VIOLA  <- "#A82DE3"
COL_ARANCIO <- "#E07700"
COL_GRIGIO <- "#9A9A9A"
COL_GRIGIO_SCURO <- "#5A5A5A"

theme_linechart <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro"),
      legend.position = "none",
      axis.line = element_line(linewidth = 0.3),
      axis.text = element_text(size = 9, color = COL_NERO, hjust = 0.5),
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
                                size = 14, color = COL_NERO, hjust = 0,
                                margin = margin(b = 0.1, unit = "cm")),
      plot.subtitle = element_text(size = 9, color = COL_NERO, hjust = 0,
                                   lineheight = 1.35,
                                   margin = margin(b = 0.25, t = 0.1, unit = "cm")),
      plot.caption = element_text(size = 9, color = COL_NERO, hjust = 1,
                                  margin = margin(t = 0.5, unit = "cm")),
      ...
    )
}

CAP <- "Elaborazione di Lorenzo Ruffino su sondaggi raccolti da Wikipedia"

output_dir <- file.path("..", "output")

# seggi interi: con la virgola si leggerebbero come percentuali
fmt_seggi <- function(x) as.character(round(x))

# Colore per blocco: blu la coalizione di Netanyahu, verde l'opposizione,
# rosso i partiti arabi.
colori_blocco <- c(governo = COL_BLU, opposizione = COL_VERDE, arabi = COL_ROSSO)

mesi_it <- c("Gen", "Feb", "Mar", "Apr", "Mag", "Giu", "Lug", "Ago", "Set",
             "Ott", "Nov", "Dic")

# ── 1. Coalizioni ────────────────────────────────────────────────────────────
coal  <- read_csv(file.path(output_dir, "medie_coalizioni.csv"), show_col_types = FALSE)
scoal <- read_csv(file.path(output_dir, "sondaggi_coalizioni.csv"), show_col_types = FALSE)

# La banda somma all'incertezza della media l'errore che i sondaggi hanno
# fatto davvero sui blocchi alle elezioni 2015-2022 (scarto quadratico medio
# fra risultato e ultimi sondaggi): circa 2,5 seggi per il blocco di Netanyahu,
# 2,9 per l'opposizione, 1,4 per i partiti arabi. L'errore dell'opposizione e'
# quello che resta: 120 meno gli altri due blocchi.
err_blocchi <- read_csv(file.path(output_dir, "errori_storici.csv"), show_col_types = FALSE) %>%
  filter(famiglia %in% c("blocco_netanyahu", "arabi")) %>%
  select(elezione, famiglia, errore) %>%
  pivot_wider(names_from = famiglia, values_from = errore) %>%
  mutate(opposizione = -(blocco_netanyahu + arabi))
sd_storico <- c(governo = sqrt(mean(err_blocchi$blocco_netanyahu^2)),
                opposizione = sqrt(mean(err_blocchi$opposizione^2)),
                arabi = sqrt(mean(err_blocchi$arabi^2)))
coal <- coal %>% mutate(sd = sqrt(sd^2 + sd_storico[serie]^2))

ultimo <- max(coal$data)
inizio <- min(coal$data)
et_coal <- coal %>%
  filter(data == ultimo) %>%
  mutate(nome = recode(nome, "Coalizione di Netanyahu" = "Coalizione\ndi Netanyahu"),
         label = paste0(nome, " ", fmt_seggi(media)),
         y_lab = case_when(serie == "opposizione" ~ media + 1.8,
                           serie == "governo" ~ media - 2.4, TRUE ~ media))

mesi <- seq(as.Date("2026-01-01"), as.Date("2026-10-01"), by = "month")

p1 <- ggplot() +
  geom_hline(yintercept = 61, colour = COL_GRIGIO, linewidth = 0.4, linetype = "dashed") +
  annotate("text", x = inizio + 2, y = 61, label = "Maggioranza alla Knesset, 61 seggi",
           hjust = 0, vjust = -0.6, size = 3.0, colour = COL_GRIGIO_SCURO,
           family = "Source Sans Pro") +
  geom_ribbon(data = coal,
              aes(x = data, ymin = media - 1.645 * sd, ymax = media + 1.645 * sd,
                  fill = serie), alpha = 0.15) +
  geom_point(data = scoal, aes(x = data, y = seggi, colour = serie),
             size = 1.0, alpha = 0.3) +
  geom_line(data = coal, aes(x = data, y = media, colour = serie), linewidth = 1.0) +
  geom_text(data = et_coal,
            aes(x = data, y = y_lab, label = label, colour = serie),
            hjust = 0, nudge_x = 3, size = 3.6, fontface = "bold",
            lineheight = 0.9, family = "Source Sans Pro") +
  scale_colour_manual(values = colori_blocco) +
  scale_fill_manual(values = colori_blocco) +
  scale_x_date(limits = c(inizio, ultimo + 70), breaks = mesi,
               labels = c("Gen\n2026", mesi_it[2:10]), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 70), breaks = seq(10, 60, 10), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(title = "In Israele né Netanyahu né l'opposizione hanno la maggioranza",
       subtitle = "Media dei sondaggi per le elezioni della Knesset del 27 ottobre, in seggi su 120, Israele, gennaio-ottobre 2026",
       caption = CAP) +
  theme_linechart()

ggsave(file.path(output_dir, "01_sondaggi_israele_coalizioni.png"),
       plot = p1, width = 9, height = 6.5, dpi = 220, bg = "white")

# ── 2. Partiti, un pannello per lista ────────────────────────────────────────
part  <- read_csv(file.path(output_dir, "medie_partiti.csv"), show_col_types = FALSE)
spart <- read_csv(file.path(output_dir, "sondaggi_partiti.csv"), show_col_types = FALSE)

# Come per le coalizioni, la banda somma all'incertezza della media l'errore
# storico, qui quello di una singola lista della stessa famiglia (scarto
# quadratico medio fra risultato e ultimi sondaggi, 2015-2022): l'errore della
# famiglia intera e' piu' piccolo, perche' gli spostamenti fra liste dello
# stesso campo si compensano.
FAMIGLIA_2026 <- c(likud = "likud", shas = "shas", utj = "utj", yb = "yb",
                   rzp = "destra_rel", otzma = "destra_rel", amcha = "destra_rel",
                   raam = "arabi", lista_comune = "arabi",
                   together = "centro_sin", yashar = "centro_sin",
                   dems = "centro_sin", riservisti = "centro_sin")
sd_liste <- read_csv(file.path(output_dir, "errori_liste.csv"), show_col_types = FALSE) %>%
  group_by(famiglia) %>%
  summarise(sd_storico = sqrt(mean(errore^2)), .groups = "drop")
part <- part %>%
  mutate(famiglia = FAMIGLIA_2026[serie]) %>%
  left_join(sd_liste, by = "famiglia") %>%
  mutate(sd = sqrt(sd^2 + sd_storico^2))

ordine <- part %>% filter(data == ultimo) %>% arrange(desc(media))
titoli <- ordine %>%
  # sotto la soglia (3,9 seggi teorici) i seggi sono zero: si scrive la percentuale
  mutate(titolo = ifelse(media < 3.9,
                         paste0(nome, "\nsotto soglia, ",
                                formatC(media / 1.2, format = "f", digits = 1,
                                        decimal.mark = ","), "%"),
                         paste0(nome, "\n", fmt_seggi(media), " seggi"))) %>%
  select(serie, titolo, blocco)
part  <- part %>% left_join(select(titoli, serie, titolo), by = "serie") %>%
  mutate(titolo = factor(titolo, levels = titoli$titolo))
spart <- spart %>% left_join(titoli, by = "serie") %>%
  mutate(titolo = factor(titolo, levels = titoli$titolo))

# soglia di sbarramento: 3,25 per cento = 3,9 seggi teorici
p2 <- ggplot() +
  geom_hline(yintercept = 3.9, colour = COL_GRIGIO, linewidth = 0.3, linetype = "dashed") +
  geom_ribbon(data = part,
              aes(x = data, ymin = pmax(media - 1.645 * sd, 0), ymax = pmin(media + 1.645 * sd, 35),
                  fill = blocco), alpha = 0.15) +
  geom_point(data = spart, aes(x = data, y = seggi, colour = blocco),
             size = 0.6, alpha = 0.3) +
  geom_line(data = part, aes(x = data, y = media, colour = blocco), linewidth = 0.8) +
  facet_wrap(~ titolo, ncol = 5) +
  scale_colour_manual(values = colori_blocco) +
  scale_fill_manual(values = colori_blocco) +
  scale_x_date(breaks = as.Date(c("2026-05-01", "2026-07-01", "2026-09-01")),
               labels = c("Mag", "Lug", "Set"), expand = c(0.02, 0.02)) +
  scale_y_continuous(limits = c(0, 35), breaks = seq(0, 30, 10), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(title = "In Israele il Likud e Yashar di Eisenkot sono appaiati in testa",
       subtitle = "Media dei sondaggi per le elezioni della Knesset del 27 ottobre, in seggi su 120, per lista, Israele, aprile-ottobre 2026.\nIn blu la coalizione di Netanyahu, in verde l'opposizione, in rosso i partiti arabi. Tratteggiata la soglia di sbarramento",
       caption = CAP) +
  theme_linechart() +
  theme(strip.text = element_text(family = "Source Sans Pro", face = "bold",
                                  size = 9, color = COL_NERO, hjust = 0,
                                  lineheight = 1.1),
        panel.spacing.x = unit(0.9, "cm"),
        panel.spacing.y = unit(0.7, "cm"),
        plot.margin = unit(c(0.5, 0.6, 0.5, 0.5), "cm"),
        axis.text = element_text(size = 8, color = COL_NERO))

ggsave(file.path(output_dir, "02_sondaggi_israele_partiti.png"),
       plot = p2, width = 10, height = 7.5, dpi = 220, bg = "white")

# ── 3. Errori storici ────────────────────────────────────────────────────────
err <- read_csv(file.path(output_dir, "errori_storici.csv"), show_col_types = FALSE)

NOMI_FAM <- c(likud = "Likud", shas = "Shas (ultraortodossi sefarditi)",
              utj = "Giudaismo unito della Torah\n(ultraortodossi askenaziti)",
              destra_rel = "Destra religiosa e radicale", yb = "Yisrael Beiteinu",
              arabi = "Partiti arabi", centro_sin = "Centro e sinistra",
              blocco_netanyahu = "Blocco di Netanyahu\n(Likud, Shas, Giudaismo unito\ne destra religiosa insieme)")
medie_err <- err %>% group_by(famiglia) %>%
  summarise(errore = mean(errore), .groups = "drop") %>%
  mutate(nome = NOMI_FAM[famiglia])
livelli <- medie_err %>% arrange(errore) %>% pull(nome)
err <- err %>% mutate(nome = factor(NOMI_FAM[famiglia], levels = livelli))
medie_err <- medie_err %>%
  mutate(nome = factor(nome, levels = livelli),
         segno = case_when(round(errore) > 0 ~ "su", round(errore) < 0 ~ "giu",
                           TRUE ~ "zero"))
# le elezioni con l'errore piu' grande per ogni famiglia, da nominare
estremi <- err %>% group_by(nome) %>% slice_max(abs(errore), n = 1) %>%
  ungroup() %>% filter(abs(errore) >= 3.3)

fmt_err <- function(x) {
  x <- round(x)
  paste0(ifelse(x > 0, "+", ""), x)
}

p3 <- ggplot() +
  geom_vline(xintercept = 0, colour = COL_GRIGIO, linewidth = 0.4, linetype = "dashed") +
  geom_point(data = err, aes(x = errore, y = nome), colour = COL_GRIGIO,
             size = 2.0, alpha = 0.7) +
  geom_text(data = estremi, aes(x = errore, y = nome, label = elezione),
            vjust = -1.0, size = 2.8, colour = COL_GRIGIO_SCURO,
            family = "Source Sans Pro") +
  geom_point(data = medie_err, aes(x = errore, y = nome,
                                   colour = segno), size = 3.6) +
  geom_text(data = medie_err, aes(x = errore, y = nome, label = fmt_err(errore),
                                  colour = segno),
            vjust = 2.0, size = 3.2, fontface = "bold", family = "Source Sans Pro") +
  annotate("text", x = 0.3, y = length(livelli) + 0.75, hjust = 0, size = 3.0,
           label = "Sottovalutati dai sondaggi →", colour = COL_ARANCIO,
           family = "Source Sans Pro", fontface = "bold") +
  annotate("text", x = -0.3, y = length(livelli) + 0.75, hjust = 1, size = 3.0,
           label = "← Sopravvalutati", colour = COL_VIOLA,
           family = "Source Sans Pro", fontface = "bold") +
  scale_colour_manual(values = c(su = COL_ARANCIO, giu = COL_VIOLA, zero = COL_GRIGIO_SCURO)) +
  scale_x_continuous(limits = c(-15, 10), breaks = seq(-15, 10, 5),
                     labels = function(x) paste0(ifelse(x > 0, "+", ""), x)) +
  scale_y_discrete(expand = expansion(add = c(0.6, 1.1))) +
  coord_cartesian(clip = "off") +
  labs(title = "In Israele i sondaggi di solito sottovalutano Likud e Shas",
       subtitle = "Differenza in seggi fra il risultato e la media degli ultimi sondaggi prima del voto, per famiglia politica, Israele, 2015-2022.\nIn grigio le singole elezioni, il punto grande è la media",
       caption = CAP) +
  theme_linechart() +
  theme(axis.line.y = element_blank(),
        axis.text.y = element_text(size = 9.5, hjust = 1, lineheight = 1.2))

ggsave(file.path(output_dir, "03_errori_storici_sondaggi_israele.png"),
       plot = p3, width = 9, height = 6.5, dpi = 220, bg = "white")

# ── Controlli ────────────────────────────────────────────────────────────────
cat("Ultimo giorno:", format(ultimo), "\n")
print(select(et_coal, nome, media, sd))
print(select(ordine, nome, media))
print(arrange(medie_err, desc(errore)))
