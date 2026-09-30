# ---------------------------------------------------------------------------
# Decomposizione della crescita del reddito disponibile reale pro capite
# delle famiglie spagnole tra 1995 e 2024.
#
# Replica del grafico di @pablogguz_ per la Spagna, con la stessa metodologia
# usata in "Reddito disponibile famiglie Italia/script/reddito_disponibile_italia.R"
# (lavoro spezzato in dipendente e autonomo, decomposizione Bennet su w*e).
# Calcola anche l'Italia sullo stesso periodo 1995-2024 per il confronto nel
# testo (solo CSV, senza grafico).
#
# Fonti:
#   - Eurostat nasa_10_nf_tr (conti non finanziari settore famiglie)
#   - Eurostat nama_10_a10_e (occupati per branca)
#   - Eurostat nama_10_gdp  (deflatore consumi famiglie)
#   - Eurostat demo_pjan    (popolazione)
#
# Note metodologiche: identiche allo script Italia (vedi header di quello).
# ---------------------------------------------------------------------------

library(eurostat)
library(tidyverse)
library(showtext)

# --- Tema --------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_ROSSO  <- "#F12938"
COL_GRIGIO <- "#9A9A9A"
COL_NAVY   <- "#0E2E5C"

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

# --- Parametri ---------------------------------------------------------------

ANNO_INIZIO <- 1995
ANNO_FINE   <- 2024
OUTPUT_DIR  <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Modello Spagnolo/output"

# --- Download unico dei quattro dataset per entrambi i paesi ------------------

GEOS <- c("ES", "IT")

message("Scarico nasa_10_nf_tr...")
nasa_all <- get_eurostat(
  "nasa_10_nf_tr",
  time_format = "num",
  filters = list(geo = GEOS, sector = "S14_S15", unit = "CP_MEUR")
)

message("Scarico nama_10_gdp...")
gdp_all <- get_eurostat(
  "nama_10_gdp",
  time_format = "num",
  filters = list(geo = GEOS, na_item = "P31_S14")
)

message("Scarico demo_pjan...")
pop_all <- get_eurostat(
  "demo_pjan",
  time_format = "num",
  filters = list(geo = GEOS, sex = "T", age = "TOTAL")
)

message("Scarico nama_10_a10_e...")
emp_all <- get_eurostat(
  "nama_10_a10_e",
  time_format = "num",
  filters = list(geo = GEOS, nace_r2 = "TOTAL", unit = "THS_PER")
)

# --- Funzione di decomposizione ----------------------------------------------

decomponi <- function(geo, anno_inizio, anno_fine) {

  nasa <- nasa_all %>% filter(geo == !!geo)

  flow <- function(item, dir, name) {
    nasa %>%
      filter(na_item == item, direct == dir) %>%
      transmute(anno = as.integer(time), !!name := values)
  }

  conti <- reduce(
    list(
      flow("D1",  "RECV", "d1_recv"),
      flow("B2G", "RECV", "b2g_recv"),
      flow("D4",  "RECV", "d4_recv"),
      flow("D4",  "PAID", "d4_paid"),
      flow("B3G", "RECV", "b3g_recv"),
      flow("D51", "PAID", "d51_paid"),
      flow("D59", "PAID", "d59_paid"),
      flow("D61", "PAID", "d61_paid"),
      flow("D62", "RECV", "d62_recv"),
      flow("D7",  "RECV", "d7_recv"),
      flow("D7",  "PAID", "d7_paid"),
      flow("B6G", "RECV", "b6g_recv")
    ),
    full_join, by = "anno"
  ) %>%
    arrange(anno) %>%
    filter(anno %in% c(anno_inizio, anno_fine))

  if (nrow(conti) != 2 || any(is.na(conti))) {
    message("ATTENZIONE [", geo, "]: dati settoriali incompleti per ",
            anno_inizio, "/", anno_fine)
    print(conti)
    return(NULL)
  }

  # Deflatore consumi famiglie, base = anno_fine
  gdp <- gdp_all %>% filter(geo == !!geo)
  defl_curr <- gdp %>% filter(unit == "CP_MEUR") %>%
    transmute(anno = as.integer(time), p31_curr = values)
  defl_real <- gdp %>% filter(unit == "CLV20_MEUR") %>%
    transmute(anno = as.integer(time), p31_real = values)
  deflatore <- inner_join(defl_curr, defl_real, by = "anno") %>%
    mutate(indice = p31_curr / p31_real)
  base <- deflatore$indice[deflatore$anno == anno_fine]
  deflatore <- deflatore %>%
    mutate(fattore = base / indice) %>%
    select(anno, fattore) %>%
    filter(anno %in% c(anno_inizio, anno_fine))

  pop <- pop_all %>% filter(geo == !!geo) %>%
    transmute(anno = as.integer(time), pop = values) %>%
    filter(anno %in% c(anno_inizio, anno_fine))

  emp_raw <- emp_all %>% filter(geo == !!geo)
  emp <- inner_join(
    emp_raw %>% filter(na_item == "EMP_DC") %>%
      transmute(anno = as.integer(time), emp_tot_thous = values),
    emp_raw %>% filter(na_item == "SAL_DC") %>%
      transmute(anno = as.integer(time), emp_sal_thous = values),
    by = "anno"
  ) %>%
    mutate(emp_self_thous = emp_tot_thous - emp_sal_thous) %>%
    filter(anno %in% c(anno_inizio, anno_fine))

  panel <- conti %>%
    left_join(deflatore, by = "anno") %>%
    left_join(pop,       by = "anno") %>%
    left_join(emp,       by = "anno")

  if (any(is.na(panel))) {
    message("ATTENZIONE [", geo, "]: panel incompleto")
    print(panel)
    return(NULL)
  }

  panel <- panel %>%
    mutate(across(where(is.integer), as.numeric)) %>%
    mutate(
      lav_dip_lordo  = d1_recv,
      lav_aut_lordo  = b3g_recv,
      capitale_lordo = (d4_recv - d4_paid) + b2g_recv,
      base_alloc = lav_dip_lordo + lav_aut_lordo + capitale_lordo + d62_recv,
      q_dip       = lav_dip_lordo  / base_alloc,
      q_aut       = lav_aut_lordo  / base_alloc,
      q_capitale  = capitale_lordo / base_alloc,
      q_benefits  = d62_recv       / base_alloc,
      d51_dip      = q_dip      * d51_paid,
      d51_aut      = q_aut      * d51_paid,
      d51_capitale = q_capitale * d51_paid,
      d51_benefits = q_benefits * d51_paid,
      d61_dip = d61_paid * lav_dip_lordo / (lav_dip_lordo + lav_aut_lordo),
      d61_aut = d61_paid * lav_aut_lordo / (lav_dip_lordo + lav_aut_lordo),
      lav_dip_netto   = lav_dip_lordo - d51_dip - d61_dip,
      lav_aut_netto   = lav_aut_lordo - d51_aut - d61_aut,
      capitale_netto  = capitale_lordo - d51_capitale,
      benefits_netto  = d62_recv - d51_benefits + (d7_recv - d7_paid),
      residuo         = -d59_paid,
      check = lav_dip_netto + lav_aut_netto + capitale_netto + benefits_netto +
              residuo - b6g_recv
    )

  message("[", geo, "] check bilancio (deve essere ~0): ",
          paste(round(panel$check, 1), collapse = " / "))

  panel <- panel %>%
    mutate(
      pop_persons     = pop,
      emp_dip_persons = emp_sal_thous  * 1000,
      emp_aut_persons = emp_self_thous * 1000,
      lav_dip_netto_r   = lav_dip_netto   * 1e6 * fattore,
      lav_aut_netto_r   = lav_aut_netto   * 1e6 * fattore,
      capitale_netto_r  = capitale_netto  * 1e6 * fattore,
      benefits_netto_r  = benefits_netto  * 1e6 * fattore,
      residuo_r         = residuo         * 1e6 * fattore,
      b6g_r             = b6g_recv        * 1e6 * fattore,
      lav_dip_pc   = lav_dip_netto_r  / pop_persons,
      lav_aut_pc   = lav_aut_netto_r  / pop_persons,
      capitale_pc  = capitale_netto_r / pop_persons,
      benefits_pc  = benefits_netto_r / pop_persons,
      residuo_pc   = residuo_r        / pop_persons,
      b6g_pc       = b6g_r            / pop_persons,
      w_dip = lav_dip_netto_r / emp_dip_persons,
      e_dip = emp_dip_persons / pop_persons,
      w_aut = lav_aut_netto_r / emp_aut_persons,
      e_aut = emp_aut_persons / pop_persons
    )

  v0 <- panel %>% filter(anno == anno_inizio)
  v1 <- panel %>% filter(anno == anno_fine)

  contrib_salario_dip <- (v1$w_dip - v0$w_dip) * (v0$e_dip + v1$e_dip) / 2
  contrib_numero_dip  <- (v1$e_dip - v0$e_dip) * (v0$w_dip + v1$w_dip) / 2
  contrib_reddito_aut <- (v1$w_aut - v0$w_aut) * (v0$e_aut + v1$e_aut) / 2
  contrib_numero_aut  <- (v1$e_aut - v0$e_aut) * (v0$w_aut + v1$w_aut) / 2
  contrib_capitale <- v1$capitale_pc - v0$capitale_pc
  contrib_benefits <- v1$benefits_pc - v0$benefits_pc
  contrib_residuo  <- v1$residuo_pc  - v0$residuo_pc
  delta_b6g_pc     <- v1$b6g_pc - v0$b6g_pc

  message("[", geo, "] salario dip: ", round(contrib_salario_dip),
          " | n. dip: ",       round(contrib_numero_dip),
          " | reddito aut: ",  round(contrib_reddito_aut),
          " | n. aut: ",       round(contrib_numero_aut),
          " | capitale: ",     round(contrib_capitale),
          " | benefits: ",     round(contrib_benefits),
          " | residuo: ",      round(contrib_residuo),
          " | TOTALE B6G pc: ", round(delta_b6g_pc))
  message("[", geo, "] livelli B6G pc reali: ", round(v0$b6g_pc), " (",
          anno_inizio, ") -> ", round(v1$b6g_pc), " (", anno_fine, "), ",
          sprintf("%+.1f%%", (v1$b6g_pc / v0$b6g_pc - 1) * 100))
  message("[", geo, "] salario dip netto reale per lavoratore: ",
          round(v0$w_dip), " -> ", round(v1$w_dip), " (",
          sprintf("%+.1f%%", (v1$w_dip / v0$w_dip - 1) * 100), ")")

  tibble(
    geo = geo,
    ord = 1:7,
    categoria = c(
      "Salario netto\nper lavoratore\ndipendente",
      "Più lavoratori\ndipendenti\npro capite",
      "Reddito netto\nper lavoratore\nNON dipendente",
      "Meno lavoratori\nNON dipendenti\npro capite",
      "Reddito da capitale\npro capite",
      "Prestazioni sociali\ne trasferimenti\npro capite",
      "Reddito disponibile\nreale pro capite"
    ),
    valore = c(contrib_salario_dip, contrib_numero_dip,
               contrib_reddito_aut, contrib_numero_aut,
               contrib_capitale, contrib_benefits, delta_b6g_pc),
    tipo = c("contrib", "contrib", "contrib", "contrib",
             "contrib", "contrib", "totale")
  )
}

# --- Esecuzione per Spagna e Italia -----------------------------------------

res_es <- decomponi("ES", ANNO_INIZIO, ANNO_FINE)
res_it <- decomponi("IT", ANNO_INIZIO, ANNO_FINE)

stopifnot(!is.null(res_es))

write_csv(bind_rows(res_es, res_it),
          file.path(OUTPUT_DIR, "dati", "reddito_disponibile_es_it_1995_2024.csv"))

# --- Grafico waterfall Spagna -------------------------------------------------

contributi <- res_es

# La seconda barra "non dipendenti" in Spagna può essere positiva o negativa:
# adegua l'etichetta di categoria al segno.
if (contributi$valore[4] >= 0) {
  contributi$categoria[4] <- "Più lavoratori\nNON dipendenti\npro capite"
}

contributi <- contributi %>%
  mutate(
    cum_prev = lag(cumsum(if_else(tipo == "totale", 0, valore)), default = 0),
    ymin = case_when(
      tipo == "totale" ~ 0,
      valore >= 0      ~ cum_prev,
      TRUE             ~ cum_prev + valore
    ),
    ymax = case_when(
      tipo == "totale" ~ valore,
      valore >= 0      ~ cum_prev + valore,
      TRUE             ~ cum_prev
    ),
    colore = case_when(
      tipo == "totale" ~ "totale",
      valore < 0       ~ "negativo",
      TRUE             ~ "positivo"
    ),
    label = paste0(
      if_else(valore < 0, "−", "+"),
      "€ ",
      format(abs(round(valore)), big.mark = ".", decimal.mark = ",",
             trim = TRUE)
    ),
    label_y = case_when(
      tipo == "totale" & valore < 0 ~ ymax,
      tipo == "totale"              ~ ymax,
      valore >= 0                   ~ ymax,
      TRUE                          ~ ymin
    )
  )

print(contributi)

connettori <- contributi %>%
  filter(tipo == "contrib") %>%
  mutate(
    xstart = ord + 0.4,
    xend   = ord + 1 - 0.4,
    y      = cum_prev + valore
  ) %>%
  filter(ord < max(contributi$ord) - 1)

ultimo_cum <- contributi %>%
  filter(tipo == "contrib") %>%
  summarise(y = sum(valore)) %>% pull(y)

connettore_finale <- tibble(
  xstart = max(contributi$ord) - 1 + 0.4,
  xend   = max(contributi$ord) - 0.4,
  y      = ultimo_cum
)

contributi <- contributi %>%
  mutate(categoria = factor(categoria, levels = categoria))

y_range_data <- max(contributi$ymax) - min(contributi$ymin)
y_max <- max(contributi$ymax) + 0.35 * y_range_data
y_min <- min(0, min(contributi$ymin) - 0.10 * y_range_data)

y_max_data <- max(contributi$ymax)
y_group_line <- y_max_data + 0.45 * (y_max - y_max_data)
y_group_text <- y_max_data + 0.72 * (y_max - y_max_data)

group_lines <- tibble(
  xstart = c(0.6, 2.6),
  xend   = c(2.4, 4.4),
  y      = y_group_line
)
group_labels <- tibble(
  x_center = c(1.5, 3.5),
  label    = c("Lavoro dipendente", "Lavoro non dipendente"),
  y        = y_group_text
)

p <- ggplot() +
  geom_segment(
    data = group_lines,
    aes(x = xstart, xend = xend, y = y, yend = y),
    color = "#5A5A5A", linewidth = 0.4
  ) +
  geom_text(
    data = group_labels,
    aes(x = x_center, y = y, label = label),
    family = "Source Sans Pro", fontface = "bold", size = 4,
    color = "#5A5A5A"
  ) +
  geom_rect(
    data = contributi,
    aes(xmin = ord - 0.4, xmax = ord + 0.4, ymin = ymin, ymax = ymax,
        fill = colore),
    color = NA
  ) +
  geom_segment(
    data = bind_rows(connettori %>% select(xstart, xend, y), connettore_finale),
    aes(x = xstart, xend = xend, y = y, yend = y),
    linetype = "dotted", color = COL_NERO, linewidth = 0.35
  ) +
  geom_text(
    data = contributi %>% filter(tipo == "contrib"),
    aes(x = ord, y = label_y, label = label, color = colore,
        vjust = if_else(valore >= 0, -0.6, 1.4)),
    family = "Source Sans Pro", fontface = "bold", size = 4.4
  ) +
  geom_text(
    data = contributi %>% filter(tipo == "totale"),
    aes(x = ord, y = label_y, label = label, color = colore,
        vjust = if_else(valore >= 0, -0.5, 1.4)),
    family = "Source Sans Pro", fontface = "bold", size = 5.4
  ) +
  geom_hline(yintercept = 0, color = COL_NERO, linewidth = 0.3) +
  scale_x_continuous(
    breaks = contributi$ord,
    labels = contributi$categoria,
    expand = c(0.04, 0.04)
  ) +
  # niente asse verticale: ogni barra porta già il proprio valore
  scale_y_continuous(breaks = NULL, limits = c(y_min, y_max), expand = c(0, 0)) +
  scale_fill_manual(values = c("positivo" = COL_BLU,
                               "negativo" = COL_ROSSO,
                               "totale"   = COL_NAVY)) +
  scale_color_manual(values = c("positivo" = COL_BLU,
                                "negativo" = COL_ROSSO,
                                "totale"   = COL_NAVY)) +
  labs(
    title = "Da dove viene la crescita del reddito delle famiglie spagnole",
    subtitle = sprintf(paste0(
      "Contributi alla variazione del reddito disponibile reale per persona, ",
      "a prezzi %d, Spagna, %d-%d"
    ), ANNO_FINE, ANNO_INIZIO, ANNO_FINE),
    caption = "Elaborazione di Lorenzo Ruffino su dati Eurostat"
  ) +
  theme_linechart() +
  theme(
    axis.text.x = element_text(size = 9, lineheight = 1.3, color = COL_NERO,
                               margin = margin(t = 0.3, unit = "cm")),
    axis.line.y = element_blank()
  )

ggsave(
  filename = file.path(OUTPUT_DIR, "reddito_disponibile_spagna.png"),
  plot = p,
  width = 12, height = 7, units = "in", dpi = 220, bg = "white"
)

message("Fatto: ", file.path(OUTPUT_DIR, "reddito_disponibile_spagna.png"))
