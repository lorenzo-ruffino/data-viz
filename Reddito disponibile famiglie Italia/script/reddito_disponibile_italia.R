# ---------------------------------------------------------------------------
# Decomposizione della crescita del reddito disponibile reale pro capite
# delle famiglie italiane tra 1995 e 2024.
#
# Replica del grafico di @pablogguz_ per la Spagna, applicato all'Italia.
#
# Fonti:
#   - Eurostat nasa_10_nf_tr (conti non finanziari settore famiglie)
#   - Eurostat nama_10_a10_e (occupati per branca)
#   - Eurostat nama_10_gdp  (deflatore consumi famiglie)
#   - Eurostat demo_pjan    (popolazione)
#
# Note metodologiche:
#   1. I flussi monetari nominali sono deflazionati con il deflatore
#      implicito dei consumi finali delle famiglie (P31_S14) base 2024.
#   2. Il "Lavoro" è scomposto in due blocchi separati:
#        a) Lavoro dipendente: D.1 (= D.11 + D.12) al netto della quota
#           di D.51 e di D.61 attribuiti, diviso per i dipendenti SAL_DC.
#        b) Lavoro autonomo: B.3G al netto della quota di D.51 e D.61
#           attribuiti, diviso per gli autonomi (EMP_DC − SAL_DC).
#      Ciascun blocco è poi scomposto con Bennet su w*e, dove w è il
#      reddito netto medio e e è il numero di lavoratori pro capite.
#   3. Il "Capitale" comprende redditi da proprietà netti (D.4) e affitti
#      imputati (B.2G).
#   4. L'imposta D.51 è allocata proporzionalmente tra lavoro dipendente
#      (D.1), lavoro autonomo (B.3G), capitale (D.4 + B.2G) e
#      prestazioni sociali (D.62), in base alle loro quote sul reddito
#      primario lordo più prestazioni. È un'approssimazione delle quote
#      NTL.
#   5. D.61 è ripartita tra lavoro dipendente e autonomo proporzionalmente
#      a D.1 e B.3G.
# ---------------------------------------------------------------------------

library(eurostat)
library(tidyverse)
library(showtext)

# --- Tema --------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
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
      plot.title = element_text(size = 14, color = COL_NERO, hjust = 0,
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

GEO <- "IT"
ANNO_INIZIO <- 2001
ANNO_FINE <- 2024
OUTPUT_DIR <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Reddito disponibile famiglie Italia/output"

# --- 1. Conti settoriali famiglie (S.14_S.15) -------------------------------

message("Scarico nasa_10_nf_tr...")
nasa <- get_eurostat(
  "nasa_10_nf_tr",
  time_format = "num",
  filters = list(
    geo = GEO,
    sector = "S14_S15",
    unit = "CP_MEUR"
  )
)

flow <- function(item, dir, name) {
  nasa %>%
    filter(na_item == item, direct == dir) %>%
    transmute(anno = as.integer(time), !!name := values)
}

d1_recv  <- flow("D1",  "RECV", "d1_recv")    # comp. lavoratori dip. = D11 + D12
d11_recv <- flow("D11", "RECV", "d11_recv")   # salari monetari + in-kind
d12_recv <- flow("D12", "RECV", "d12_recv")   # contributi sociali a carico DDL
b2g_recv <- flow("B2G", "RECV", "b2g_recv")   # ecc. operativa lorda (affitti imputati)
d4_recv  <- flow("D4",  "RECV", "d4_recv")    # redditi da capitale ricevuti
d4_paid  <- flow("D4",  "PAID", "d4_paid")    # redditi da capitale pagati
b3g_recv <- flow("B3G", "RECV", "b3g_recv")   # reddito misto lavoro autonomo
d51_paid <- flow("D51", "PAID", "d51_paid")   # imposte correnti su reddito
d59_paid <- flow("D59", "PAID", "d59_paid")   # altre imposte correnti
d61_paid <- flow("D61", "PAID", "d61_paid")   # contributi sociali pagati
d62_recv <- flow("D62", "RECV", "d62_recv")   # prestazioni sociali ricevute
d7_recv  <- flow("D7",  "RECV", "d7_recv")    # altri trasferimenti ricevuti
d7_paid  <- flow("D7",  "PAID", "d7_paid")    # altri trasferimenti pagati
b6g_recv <- flow("B6G", "RECV", "b6g_recv")   # reddito disponibile lordo

conti <- reduce(
  list(d1_recv, d11_recv, d12_recv, b2g_recv, d4_recv, d4_paid, b3g_recv,
       d51_paid, d59_paid, d61_paid, d62_recv, d7_recv, d7_paid, b6g_recv),
  full_join, by = "anno"
) %>%
  arrange(anno) %>%
  filter(anno %in% c(ANNO_INIZIO, ANNO_FINE))

stopifnot(nrow(conti) == 2)
print(conti)

# --- 2. Deflatore consumi famiglie ------------------------------------------

message("Scarico nama_10_gdp...")
gdp <- get_eurostat(
  "nama_10_gdp",
  time_format = "num",
  filters = list(geo = GEO, na_item = "P31_S14")
)

defl_curr <- gdp %>%
  filter(unit == "CP_MEUR") %>%
  transmute(anno = as.integer(time), p31_curr = values)
defl_real <- gdp %>%
  filter(unit == "CLV20_MEUR") %>%
  transmute(anno = as.integer(time), p31_real = values)

deflatore <- inner_join(defl_curr, defl_real, by = "anno") %>%
  mutate(indice = p31_curr / p31_real) %>%
  select(anno, indice)

base <- deflatore$indice[deflatore$anno == ANNO_FINE]
deflatore <- deflatore %>%
  mutate(fattore = base / indice) %>%
  filter(anno %in% c(ANNO_INIZIO, ANNO_FINE))
print(deflatore)

# --- 3. Popolazione (1 gennaio) ---------------------------------------------

message("Scarico demo_pjan...")
pop <- get_eurostat(
  "demo_pjan",
  time_format = "num",
  filters = list(geo = GEO, sex = "T", age = "TOTAL")
)
pop <- pop %>%
  transmute(anno = as.integer(time), pop = values) %>%
  arrange(anno) %>%
  filter(anno %in% c(ANNO_INIZIO, ANNO_FINE))
print(pop)

# --- 4. Dipendenti (SAL_DC) -------------------------------------------------

message("Scarico nama_10_a10_e...")
emp_raw <- get_eurostat(
  "nama_10_a10_e",
  time_format = "num",
  filters = list(geo = GEO, nace_r2 = "TOTAL", unit = "THS_PER")
)
emp_tot <- emp_raw %>% filter(na_item == "EMP_DC") %>%
  transmute(anno = as.integer(time), emp_tot_thous = values)
emp_sal <- emp_raw %>% filter(na_item == "SAL_DC") %>%
  transmute(anno = as.integer(time), emp_sal_thous = values)

emp <- inner_join(emp_tot, emp_sal, by = "anno") %>%
  mutate(emp_self_thous = emp_tot_thous - emp_sal_thous) %>%
  arrange(anno) %>%
  filter(anno %in% c(ANNO_INIZIO, ANNO_FINE))
print(emp)

# --- 5. Composizione del panel finale per i due anni -----------------------

panel <- conti %>%
  left_join(deflatore, by = "anno") %>%
  left_join(pop,       by = "anno") %>%
  left_join(emp,       by = "anno")

if (any(is.na(panel))) {
  print(panel)
  stop("Mancano dati per uno degli anni richiesti.")
}

# --- 6. Calcolo delle componenti --------------------------------------------
#
# Tutte in € reali base 2024, pro capite.

panel <- panel %>%
  mutate(across(where(is.integer), as.numeric)) %>%
  mutate(
    # Lordi per blocco
    lav_dip_lordo  = d1_recv,
    lav_aut_lordo  = b3g_recv,
    capitale_lordo = (d4_recv - d4_paid) + b2g_recv,
    # Denominatore per allocare D.51 (proxy delle quote NTL)
    base_alloc = lav_dip_lordo + lav_aut_lordo + capitale_lordo + d62_recv,
    q_dip       = lav_dip_lordo  / base_alloc,
    q_aut       = lav_aut_lordo  / base_alloc,
    q_capitale  = capitale_lordo / base_alloc,
    q_benefits  = d62_recv       / base_alloc,
    d51_dip      = q_dip      * d51_paid,
    d51_aut      = q_aut      * d51_paid,
    d51_capitale = q_capitale * d51_paid,
    d51_benefits = q_benefits * d51_paid,
    # D.61 ripartita tra dip/aut proporzionalmente a D.1 e B.3G
    d61_dip = d61_paid * lav_dip_lordo / (lav_dip_lordo + lav_aut_lordo),
    d61_aut = d61_paid * lav_aut_lordo / (lav_dip_lordo + lav_aut_lordo),
    # Componenti del reddito disponibile (nominali, milioni di euro)
    lav_dip_netto   = lav_dip_lordo - d51_dip - d61_dip,
    lav_aut_netto   = lav_aut_lordo - d51_aut - d61_aut,
    capitale_netto  = capitale_lordo - d51_capitale,
    benefits_netto  = d62_recv - d51_benefits + (d7_recv - d7_paid),
    residuo         = -d59_paid,
    # Check di bilancio
    check = lav_dip_netto + lav_aut_netto + capitale_netto + benefits_netto +
            residuo - b6g_recv
  )

message("Check (deve essere ~0 in milioni di euro): ")
print(panel %>% select(anno, lav_dip_netto, lav_aut_netto, capitale_netto,
                       benefits_netto, residuo, b6g_recv, check))

# In € reali 2024 e pro capite
panel <- panel %>%
  mutate(
    pop_persons     = pop,
    emp_dip_persons = emp_sal_thous  * 1000,
    emp_aut_persons = emp_self_thous * 1000,
    # Tutto in EUR reali 2024
    lav_dip_netto_r   = lav_dip_netto   * 1e6 * fattore,
    lav_aut_netto_r   = lav_aut_netto   * 1e6 * fattore,
    capitale_netto_r  = capitale_netto  * 1e6 * fattore,
    benefits_netto_r  = benefits_netto  * 1e6 * fattore,
    residuo_r         = residuo         * 1e6 * fattore,
    b6g_r             = b6g_recv        * 1e6 * fattore,
    # pro capite (su popolazione totale)
    lav_dip_pc   = lav_dip_netto_r  / pop_persons,
    lav_aut_pc   = lav_aut_netto_r  / pop_persons,
    capitale_pc  = capitale_netto_r / pop_persons,
    benefits_pc  = benefits_netto_r / pop_persons,
    residuo_pc   = residuo_r        / pop_persons,
    b6g_pc       = b6g_r            / pop_persons,
    # Per la decomposizione Bennet di ciascun blocco lavoro
    w_dip = lav_dip_netto_r / emp_dip_persons,
    e_dip = emp_dip_persons / pop_persons,
    w_aut = lav_aut_netto_r / emp_aut_persons,
    e_aut = emp_aut_persons / pop_persons
  )

print(panel %>% select(anno, lav_dip_pc, lav_aut_pc, capitale_pc,
                       benefits_pc, residuo_pc, b6g_pc,
                       w_dip, e_dip, w_aut, e_aut))

# --- 7. Decomposizione Bennet del contributo Lavoro ------------------------

v0 <- panel %>% filter(anno == ANNO_INIZIO)
v1 <- panel %>% filter(anno == ANNO_FINE)

# Bennet su lavoro dipendente
contrib_salario_dip <- (v1$w_dip - v0$w_dip) * (v0$e_dip + v1$e_dip) / 2
contrib_numero_dip  <- (v1$e_dip - v0$e_dip) * (v0$w_dip + v1$w_dip) / 2

# Bennet su lavoro autonomo
contrib_reddito_aut <- (v1$w_aut - v0$w_aut) * (v0$e_aut + v1$e_aut) / 2
contrib_numero_aut  <- (v1$e_aut - v0$e_aut) * (v0$w_aut + v1$w_aut) / 2

# Check coerenza per ciascun blocco
delta_lav_dip_pc <- v1$lav_dip_pc - v0$lav_dip_pc
delta_lav_aut_pc <- v1$lav_aut_pc - v0$lav_aut_pc
message("Δ lavoro dip pc: ", round(delta_lav_dip_pc, 2),
        " | Somma Bennet dip: ", round(contrib_salario_dip + contrib_numero_dip, 2))
message("Δ lavoro aut pc: ", round(delta_lav_aut_pc, 2),
        " | Somma Bennet aut: ", round(contrib_reddito_aut + contrib_numero_aut, 2))

contrib_capitale <- v1$capitale_pc - v0$capitale_pc
contrib_benefits <- v1$benefits_pc - v0$benefits_pc
contrib_residuo  <- v1$residuo_pc  - v0$residuo_pc
delta_b6g_pc     <- v1$b6g_pc - v0$b6g_pc

# Sotto-split del capitale: D.4 netto vs B.2G (affitti imputati)
split_capitale <- panel %>%
  mutate(
    d4_net_lordo  = d4_recv - d4_paid,
    d51_su_d4     = d4_net_lordo / base_alloc * d51_paid,
    d51_su_b2g    = b2g_recv     / base_alloc * d51_paid,
    cap_d4_pc     = (d4_net_lordo - d51_su_d4) * 1e6 * fattore / pop_persons,
    cap_b2g_pc    = (b2g_recv     - d51_su_b2g) * 1e6 * fattore / pop_persons
  ) %>%
  select(anno, cap_d4_pc, cap_b2g_pc)

print(split_capitale)
message("Δ capitale D.4 (interessi + dividendi netti) pc: ",
        round(split_capitale$cap_d4_pc[2] - split_capitale$cap_d4_pc[1], 0))
message("Δ capitale B.2G (affitti imputati) pc:           ",
        round(split_capitale$cap_b2g_pc[2] - split_capitale$cap_b2g_pc[1], 0))
message("Somma = contrib_capitale: ", round(contrib_capitale, 0))

# --- Verifica reddito autonomi vs dati IRPEF -------------------------------
verifica_aut <- panel %>%
  mutate(
    # Tutto in nominale (€ correnti per autonomo, anno)
    b3g_lordo_nom_per_aut = lav_aut_lordo * 1e6 / emp_aut_persons,
    b3g_netto_nom_per_aut = lav_aut_netto * 1e6 / emp_aut_persons,
    # Reale 2024 per autonomo
    b3g_netto_real_per_aut = lav_aut_netto_r / emp_aut_persons,
    # Numero di autonomi (migliaia)
    n_aut_migliaia = emp_self_thous,
    # B.3G totale lordo nominale (miliardi €)
    b3g_lordo_tot_mld = lav_aut_lordo / 1000
  ) %>%
  select(anno, n_aut_migliaia, b3g_lordo_tot_mld,
         b3g_lordo_nom_per_aut, b3g_netto_nom_per_aut, b3g_netto_real_per_aut)

print(verifica_aut)

infl_cum <- v0$fattore  # = defl_2024 / defl_2001
message("Inflazione cumulata 2001-2024 (deflatore HFCE): +",
        round((infl_cum - 1) * 100, 1), "%")
message("Δ% B.3G lordo per autonomo, nominale: ",
        round((verifica_aut$b3g_lordo_nom_per_aut[2] /
               verifica_aut$b3g_lordo_nom_per_aut[1] - 1) * 100, 1), "%")
message("Δ% B.3G netto per autonomo, nominale: ",
        round((verifica_aut$b3g_netto_nom_per_aut[2] /
               verifica_aut$b3g_netto_nom_per_aut[1] - 1) * 100, 1), "%")
message("Δ% B.3G netto per autonomo, reale:    ",
        round((verifica_aut$b3g_netto_real_per_aut[2] /
               verifica_aut$b3g_netto_real_per_aut[1] - 1) * 100, 1), "%")

message("Somma contributi: ",
        round(contrib_salario_dip + contrib_numero_dip +
              contrib_reddito_aut + contrib_numero_aut +
              contrib_capitale + contrib_benefits + contrib_residuo, 2))
message("Δ B6G pc: ", round(delta_b6g_pc, 2))

# --- 8. Costruzione del data frame per il waterfall ------------------------

contributi <- tibble(
  ord = 1:7,
  categoria = c(
    "Salario netto\nper lavoratore\ndipendente",
    "Più lavoratori\ndipendenti\npro capite",
    "Reddito netto\nper lavoratore\nNON dipendente",
    "Meno lavoratori\nNON dipendenti\npro capite",
    "Reddito da capitale\npro capite",
    "Prestazioni sociali\ne trasferimenti\npro capite",
    "Reddito disponibile\nreale pro capite*"
  ),
  valore = c(contrib_salario_dip, contrib_numero_dip,
             contrib_reddito_aut, contrib_numero_aut,
             contrib_capitale, contrib_benefits, delta_b6g_pc),
  tipo = c("contrib", "contrib", "contrib", "contrib",
           "contrib", "contrib", "totale")
)

# Cumulativa parziale per posizionare le barre intermedie
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
      tipo == "totale" & valore < 0 ~ ymax,    # totale negativo: sotto la barra
      tipo == "totale"              ~ ymax,    # totale positivo: sopra
      valore >= 0                   ~ ymax,    # contrib positivo: sopra
      TRUE                          ~ ymin     # contrib negativo: sotto
    )
  )

print(contributi)

# Salva CSV
write_csv(
  contributi %>% select(ord, categoria, valore, tipo),
  file.path(OUTPUT_DIR, "reddito_disponibile_italia.csv")
)

# --- 9. Grafico waterfall ---------------------------------------------------

# Linee tratteggiate che connettono il top di una barra all'inizio della successiva
connettori <- contributi %>%
  filter(tipo == "contrib") %>%
  mutate(
    xstart = ord + 0.4,
    xend   = ord + 1 - 0.4,
    y      = cum_prev + valore   # punto di partenza della barra successiva
  ) %>%
  filter(ord < max(contributi$ord) - 1)

# Connettore tra l'ultimo contributo e la barra totale: dal top cumulato fino al totale
ultimo_cum <- contributi %>%
  filter(tipo == "contrib") %>%
  summarise(y = sum(valore)) %>% pull(y)

connettore_finale <- tibble(
  xstart = max(contributi$ord) - 1 + 0.4,
  xend   = max(contributi$ord) - 0.4,
  y      = ultimo_cum
)

# Etichette delle categorie sotto l'asse x: solo numeri sull'asse, label tra le barre
contributi <- contributi %>%
  mutate(categoria = factor(categoria, levels = categoria))

# Range Y per l'asse
y_range_data <- max(contributi$ymax) - min(contributi$ymin)
y_max <- max(contributi$ymax) + 0.35 * y_range_data
y_min <- min(0, min(contributi$ymin) - 0.10 * y_range_data)

# Annotazioni di raggruppamento sopra le barre lavoro dipendente / autonomo
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
  # Annotazioni di gruppo sopra le barre
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
  # Barre
  geom_rect(
    data = contributi,
    aes(xmin = ord - 0.4, xmax = ord + 0.4, ymin = ymin, ymax = ymax,
        fill = colore),
    color = NA
  ) +
  # Connettori tra barre
  geom_segment(
    data = bind_rows(connettori %>% select(xstart, xend, y), connettore_finale),
    aes(x = xstart, xend = xend, y = y, yend = y),
    linetype = "dotted", color = COL_NERO, linewidth = 0.35
  ) +
  # Etichette dei valori sopra/sotto le barre (totale più grande)
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
  # Linea zero
  geom_hline(yintercept = 0, color = COL_NERO, linewidth = 0.3) +
  scale_x_continuous(
    breaks = contributi$ord,
    labels = contributi$categoria,
    expand = c(0.04, 0.04)
  ) +
  scale_y_continuous(
    labels = function(x) {
      prefix <- case_when(x > 0 ~ "+", x < 0 ~ "−", TRUE ~ "")
      paste0(prefix, "€ ",
             format(abs(x) / 1000, big.mark = ".", decimal.mark = ",",
                    trim = TRUE, drop0trailing = TRUE),
             "k")
    },
    breaks = function(lim) pretty(lim, n = 6),
    limits = c(y_min, y_max),
    expand = c(0, 0)
  ) +
  scale_fill_manual(values = c("positivo" = COL_BLU,
                               "negativo" = COL_ROSSO,
                               "totale"   = COL_NAVY)) +
  scale_color_manual(values = c("positivo" = COL_BLU,
                                "negativo" = COL_ROSSO,
                                "totale"   = COL_NAVY)) +
  labs(
    title = sprintf("Il reddito delle famiglie italiane scende dal %d", ANNO_INIZIO),
    subtitle = sprintf(paste0(
      "Contributi per fonte alla variazione del reddito disponibile reale lordo ",
      "per persona, in € a prezzi %d, Italia, %d-%d"
    ), ANNO_FINE, ANNO_INIZIO, ANNO_FINE),
    caption = paste0(
      "Lavoro dipendente = salari e contributi datore di lavoro per occupato dipendente. Lavoro non dipendente = reddito misto per occupato non dipendente,\n",
      "include imprenditori individuali, lavoratori in proprio, professionisti, coadiuvanti familiari e soci di cooperative. Reddito da capitale = interessi e\n",
      "dividendi netti e affitti imputati delle abitazioni di proprietà. Prestazioni sociali e trasferimenti = pensioni, sussidi e altri trasferimenti netti. Tutte le\n",
      "componenti sono al netto di IRPEF e contributi sociali. *Piccolo residuo da altre imposte correnti.\n",
      "Dataset Eurostat: nasa_10_nf_tr, nama_10_a10_e, nama_10_gdp, demo_pjan."
    ),
    tag = "Elaborazione di Lorenzo Ruffino su dati Eurostat"
  ) +
  theme_linechart() +
  theme(
    axis.text.x = element_text(size = 9, lineheight = 1.3, color = COL_NERO,
                               margin = margin(t = 0.3, unit = "cm")),
    plot.caption = element_text(size = 8, color = COL_NERO, hjust = 0,
                                lineheight = 1.35,
                                margin = margin(t = 0.6, b = 0.4, unit = "cm")),
    plot.tag.position = c(0.99, 0.005),
    plot.tag = element_text(size = 8, color = COL_NERO, hjust = 1, vjust = 0,
                            family = "Source Sans Pro")
  )

ggsave(
  filename = file.path(OUTPUT_DIR, "reddito_disponibile_italia.png"),
  plot = p,
  width = 12, height = 8, units = "in", dpi = 220, bg = "white"
)

message("Fatto: ", file.path(OUTPUT_DIR, "reddito_disponibile_italia.png"))
