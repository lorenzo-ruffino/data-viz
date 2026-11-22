library(tidyverse)
library(showtext)
library(jsonlite)
Sys.setlocale("LC_TIME", "it_IT.UTF-8")

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_GRIGIO <- "#9A9A9A"
COL_GRIGIO_SCURO <- "#5A5A5A"
COL_DEM    <- "#3963A2"
COL_REP    <- "#E35D5D"
COL_IND    <- "#7A7A7A"

theme_linechart <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro"),
      legend.position = "top",
      axis.line = element_line(linewidth = 0.3),
      axis.text = element_text(size = 9, color = COL_NERO, hjust = 0.5),
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

CAP_FA <- "Elaborazione di Lorenzo Ruffino su dati Focus America"
CAP_MODELLI <- "Elaborazione di Lorenzo Ruffino su dati dei singoli modelli"

fmt_pct <- function(x) paste0(format(round(x, 1), nsmall = 1, decimal.mark = ","), "%")

base <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Midterm 2026"

# --- 1. Generic ballot --------------------------------------------------------

gen <- fromJSON(file.path(base, "input/generic.json")) |>
  as_tibble() |>
  mutate(date = as.Date(date)) |>
  filter(date >= as.Date("2026-01-01"))

gen_long <- bind_rows(
  gen |> transmute(date, partito = "Democratici", val = dem, lo = dem_lo, hi = dem_hi),
  gen |> transmute(date, partito = "Repubblicani", val = rep, lo = rep_lo, hi = rep_hi)
)
write_csv(gen_long, file.path(base, "output/generic_ballot_2026.csv"))

sond <- fromJSON(file.path(base, "input/sondaggi_generic.json")) |>
  as_tibble() |>
  mutate(date = as.Date(date_mid)) |>
  filter(date >= as.Date("2026-01-01"))
sond_long <- bind_rows(
  sond |> transmute(date, partito = "Democratici", val = dem),
  sond |> transmute(date, partito = "Repubblicani", val = rep)
)

ultimo <- gen_long |> filter(date == max(date))
d_fine <- max(gen_long$date)
y_dem <- ultimo$val[ultimo$partito == "Democratici"]
y_rep <- ultimo$val[ultimo$partito == "Repubblicani"]
etichette <- ultimo |>
  mutate(lab = paste0(if_else(partito == "Democratici", "Dem ", "Rep "), fmt_pct(val)))

g1 <- ggplot(gen_long, aes(date, val, color = partito, fill = partito)) +
  annotate("segment", x = as.Date("2026-08-01"), xend = as.Date("2026-08-01"), y = 35, yend = 56.3,
           color = COL_GRIGIO, linewidth = 0.3, linetype = "22") +
  annotate("text", x = as.Date("2026-07-29"), y = 56.6,
           label = "Da agosto il vantaggio Dem\npassa da circa 5 a 8,4 punti",
           hjust = 1, vjust = 1, family = "Source Sans Pro", size = 2.9,
           color = COL_GRIGIO_SCURO, lineheight = 0.95) +
  geom_point(data = sond_long, size = 1.5, alpha = 0.2, stroke = 0) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.08, color = NA) +
  geom_line(linewidth = 1.1, lineend = "round") +
  geom_point(data = ultimo, size = 2.8, shape = 21, color = "white", stroke = 0.8) +
  # graffa con il distacco fra i due partiti all'ultimo giorno
  annotate("segment", x = d_fine + 7, xend = d_fine + 7, y = y_rep, yend = y_dem,
           color = COL_GRIGIO_SCURO, linewidth = 0.35) +
  annotate("segment", x = d_fine + 4, xend = d_fine + 7, y = c(y_rep, y_dem), yend = c(y_rep, y_dem),
           color = COL_GRIGIO_SCURO, linewidth = 0.35) +
  annotate("text", x = d_fine + 11, y = (y_dem + y_rep) / 2,
           label = paste0("+", sub("\\.", ",", sprintf("%.1f", gen$margin[gen$date == d_fine])), "\npunti"),
           hjust = 0, family = "Source Sans Pro", fontface = "bold", size = 3.2,
           color = COL_NERO, lineheight = 0.9) +
  geom_label(data = etichette, aes(x = d_fine + 11, label = lab), hjust = 0, color = "white",
             family = "Source Sans Pro", fontface = "bold", size = 3.2,
             label.size = 0, label.r = unit(0.25, "lines"),
             label.padding = unit(0.28, "lines")) +
  scale_color_manual(values = c("Democratici" = COL_DEM, "Repubblicani" = COL_REP)) +
  scale_fill_manual(values = c("Democratici" = COL_DEM, "Repubblicani" = COL_REP)) +
  scale_x_date(labels = function(x) str_to_title(format(x, "%b")),
               limits = c(as.Date("2026-01-01"), as.Date("2027-01-05")),
               breaks = seq(as.Date("2026-01-01"), as.Date("2026-11-01"), by = "1 month"),
               expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(34, 57), breaks = seq(35, 55, 5),
                     labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(title = "I Dem avanti di otto punti nei sondaggi per la Camera",
       subtitle = "Sondaggi nazionali sul partito che si voterà alla Camera (generic ballot): i punti sono i singoli sondaggi, la linea la media\ne la fascia il margine di errore, Stati Uniti, gennaio-novembre 2026",
       caption = CAP_FA) +
  theme_linechart() +
  theme(legend.position = "none",
        axis.line.y = element_blank(),
        axis.line.x = element_line(linewidth = 0.3, color = "#BDBDBD"),
        panel.grid.major.y = element_line(linewidth = 0.3, color = "#EEF1F6"),
        axis.text.y = element_text(color = COL_GRIGIO_SCURO, margin = margin(r = 0.15, unit = "cm")),
        axis.text.x = element_text(color = COL_GRIGIO_SCURO))

ggsave(file.path(base, "output/1_generic_ballot.png"), g1,
       width = 9, height = 6.5, units = "in", dpi = 220, bg = "white")

# --- 2. Senato, le gare in bilico ---------------------------------------------

sen <- fromJSON(file.path(base, "input/senato.json")) |> as_tibble()

stati_it <- c(GA = "Georgia", NC = "North Carolina", NH = "New Hampshire", MN = "Minnesota",
              MI = "Michigan", OH = "Ohio", TX = "Texas", AK = "Alaska",
              ME = "Maine", IA = "Iowa", KS = "Kansas", SC = "South Carolina",
              NE = "Nebraska", FL = "Florida")

cognome <- function(x) word(str_remove(x, ",? (Jr\\.|III|II)$"), -1)
fmt_dist <- function(x) sub("\\.", ",", sub("\\.0$", "", sprintf("%.1f", abs(x))))

# margin > 0: avanti il candidato non repubblicano (in Nebraska l'indipendente Osborn)
sel <- sen |>
  filter(state %in% names(stati_it)) |>
  mutate(
    nome_stato = stati_it[state],
    sfidante = if_else(cand_d == "" | is.na(cand_d), cand_i, cand_d),
    davanti = if_else(margin > 0, cognome(sfidante), cognome(cand_r)),
    col = case_when(margin < 0 ~ COL_REP, cand_d == "" | is.na(cand_d) ~ COL_IND, TRUE ~ COL_DEM),
    lab = paste0(davanti, " +", fmt_dist(margin))
  ) |>
  arrange(desc(margin)) |>
  mutate(ordine = row_number())
write_csv(sel |> select(state, nome_stato, sfidante, cand_r, margin, margin_lo, margin_hi),
          file.path(base, "output/senato_gare_in_bilico.csv"))

# L'intervallo sfuma dalla stima verso i due estremi; ogni pezzo prende il colore
# del partito che sarebbe avanti in quel punto
n_step <- 40
h <- 0.42
sfuma <- sel |>
  rowwise() |>
  reframe(ordine, cand_d, cand_i,
          bind_rows(
            tibble(x0 = margin - (margin - margin_lo) * (1:n_step) / n_step,
                   x1 = margin - (margin - margin_lo) * (0:(n_step - 1)) / n_step),
            tibble(x0 = margin + (margin_hi - margin) * (0:(n_step - 1)) / n_step,
                   x1 = margin + (margin_hi - margin) * (1:n_step) / n_step)
          ) |>
            mutate(a = 0.12 + 0.5 * (1 - rep((0:(n_step - 1)) / n_step, 2))^0.8)) |>
  mutate(mid = (x0 + x1) / 2,
         col = case_when(mid < 0 ~ COL_REP, cand_d == "" | is.na(cand_d) ~ COL_IND, TRUE ~ COL_DEM))

g2 <- ggplot() +
  geom_vline(xintercept = 0, color = COL_GRIGIO_SCURO, linewidth = 0.35) +
  geom_rect(data = sfuma, aes(xmin = x0, xmax = x1, ymin = ordine - h / 2, ymax = ordine + h / 2,
                              fill = col, alpha = a), color = NA) +
  geom_point(data = sel, aes(x = margin, y = ordine, fill = col), shape = 21,
             color = "white", size = 3.6, stroke = 0.7) +
  geom_text(data = sel |> filter(margin > 0),
            aes(x = margin_hi + 0.7, y = ordine, label = lab, color = col),
            hjust = 0, family = "Source Sans Pro", fontface = "bold", size = 3) +
  geom_text(data = sel |> filter(margin < 0),
            aes(x = margin_lo - 0.7, y = ordine, label = lab, color = col),
            hjust = 1, family = "Source Sans Pro", fontface = "bold", size = 3) +
  annotate("text", x = c(-1, 1), y = 0.25, label = c("← avanti i repubblicani", "avanti i Dem →"),
           hjust = c(1, 0), family = "Source Sans Pro", size = 2.8, color = COL_GRIGIO_SCURO) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_alpha_identity() +
  scale_y_reverse(breaks = sel$ordine, labels = sel$nome_stato, expand = expansion(add = c(0.9, 0.6))) +
  scale_x_continuous(limits = c(-24, 26), breaks = seq(-20, 20, 10),
                     labels = function(x) case_when(x < 0 ~ paste0("R+", -x), x > 0 ~ paste0("D+", x), TRUE ~ "Pari"),
                     expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(title = "I Dem avanti in 11 delle 14 sfide più combattute al Senato",
       subtitle = "Distacco previsto dal modello tra i due principali candidati, in punti: il pallino è la stima, la barra l'intervallo\nin cui cade il risultato nove volte su dieci, Stati Uniti, 2 novembre 2026",
       caption = paste0(CAP_FA, ". In Nebraska lo sfidante è l'indipendente Dan Osborn, in grigio")) +
  theme_linechart() +
  theme(axis.line = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.3, color = "#EEF1F6"),
        axis.text.x = element_text(color = COL_GRIGIO_SCURO),
        axis.text.y = element_text(size = 10, hjust = 1, color = COL_NERO))

ggsave(file.path(base, "output/2_senato_gare_in_bilico.png"), g2,
       width = 9, height = 8.5, units = "in", dpi = 220, bg = "white")

# --- 3. I modelli a confronto ---------------------------------------------------

mod <- fromJSON(file.path(base, "input/modelli_2026-11-02.json"), simplifyVector = FALSE)$models

righe <- map_dfr(mod, function(m) {
  map_dfr(c("house", "senate"), function(ch) {
    if (is.null(m[[ch]]) || is.null(m[[ch]]$p_dem)) return(NULL)
    tibble(modello = m$name, id = m$id, camera = ch, p = 100 * m[[ch]]$p_dem)
  })
})
# Decision Desk HQ blocca lo scaricamento automatico: valori letti a mano dalla
# pagina pubblica, aggiornamento del 2 novembre
righe <- bind_rows(righe,
                   tibble(modello = "Decision Desk HQ", id = "ddhq",
                          camera = c("house", "senate"), p = c(84, 64)))

righe <- righe |>
  mutate(modello = recode(modello, "The Economist" = "Economist"),
         camera = factor(if_else(camera == "house", "Camera", "Senato"), levels = c("Camera", "Senato")),
         fa = id == "fa")
write_csv(righe, file.path(base, "output/modelli_confronto.csv"))

# La media è quella dei modelli esterni, come nell'articolo
medie <- righe |> filter(!fa) |> group_by(camera) |> summarise(media = mean(p))

# Focus America in fondo e separato dagli altri, che vanno dal più prudente
# al più favorevole ai democratici al Senato
ordine_mod <- c("Focus America",
                righe |> filter(camera == "Senato", !fa) |> arrange(p) |> pull(modello))
righe <- righe |> mutate(modello = factor(modello, levels = ordine_mod))

COL_TRACCIA <- "#EEF1F6"
COL_DEM_CHIARO <- "#B9C8E1"
righe <- righe |> mutate(col_pt = if_else(fa, COL_GRIGIO_SCURO, COL_DEM),
                         col_bar = if_else(fa, "#D3D3D3", COL_DEM_CHIARO))
n_mod <- length(ordine_mod)

g3 <- ggplot(righe, aes(y = modello)) +
  # binario da 0 a 100, poi il tratto colorato dal testa a testa (50) al valore
  geom_segment(aes(x = 0, xend = 100, yend = modello), color = COL_TRACCIA,
               linewidth = 5.5, lineend = "round") +
  geom_segment(aes(x = 50, xend = p, yend = modello, color = col_bar),
               linewidth = 5.5, lineend = "round") +
  geom_segment(data = medie, aes(x = media, xend = media, y = 0.45, yend = n_mod + 0.55),
               inherit.aes = FALSE, color = COL_DEM, linewidth = 0.35, linetype = "22") +
  geom_text(data = medie, aes(x = media, y = n_mod + 0.85, label = paste0("Media ", round(media))),
            inherit.aes = FALSE, family = "Source Sans Pro", fontface = "bold",
            size = 2.9, color = COL_DEM, hjust = 0.5) +
  geom_text(data = expand_grid(camera = factor(c("Camera", "Senato"), levels = c("Camera", "Senato")),
                               tibble(x = c(0, 50, 100), lab = c("0", "50 · testa a testa", "100"))),
            aes(x = x, y = 0.35, label = lab), inherit.aes = FALSE,
            family = "Source Sans Pro", size = 2.7, color = COL_GRIGIO_SCURO, vjust = 1) +
  geom_point(aes(x = p, fill = col_pt), shape = 21, color = "white", stroke = 0.8, size = 8.5) +
  geom_text(aes(x = p, label = round(p)), color = "white", family = "Source Sans Pro",
            fontface = "bold", size = 3) +
  facet_wrap(~camera) +
  scale_color_identity() +
  scale_fill_identity() +
  scale_x_continuous(limits = c(-3, 104), breaks = c(0, 50, 100), expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(add = c(0.9, 1.1))) +
  coord_cartesian(clip = "off") +
  labs(title = "Tutti i modelli danno favoriti i Dem alla Camera e al Senato",
       subtitle = "Possibilità su 100 che i Dem conquistino la maggioranza secondo i principali modelli di previsione; la media\nesclude il modello di Focus America, Stati Uniti, 2 novembre 2026",
       caption = CAP_MODELLI) +
  theme_linechart() +
  theme(axis.line = element_blank(),
                  axis.text.x = element_blank(),
                  strip.text = element_text(family = "Source Sans Pro SemiBold",
                                            size = 12, hjust = 0, color = COL_NERO,
                                            margin = margin(b = 0.2, unit = "cm")),
                  axis.text.y = element_text(size = 10, hjust = 1, color = COL_NERO,
                                             face = c("italic", rep("plain", n_mod - 1)),
                                             margin = margin(r = 0.2, unit = "cm")),
                  panel.spacing = unit(1.2, "cm"))

ggsave(file.path(base, "output/3_modelli_confronto.png"), g3,
       width = 9, height = 6.5, units = "in", dpi = 220, bg = "white")
