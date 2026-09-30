# Due mappe provinciali dai dati INPS (Osservatorio sui lavoratori dipendenti,
# anno 2024): 1) retribuzione media annua dei dipendenti privati occupati a
# tempo pieno per 52 settimane; 2) quota di questi lavoratori sul totale dei
# dipendenti privati della provincia.
# Fonte: estrazioni dall'osservatorio INPS in input/ (tracciato con header
# incrociato e markup html: si legge con read.csv base, non con readr).
# Eseguito da `cd script && Rscript stipendi_provincia_inps.R`.

source("/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities/R/mappe.R")
suppressPackageStartupMessages({
  library(tidyverse)
  library(giscoR)
  library(showtext)
  library(sf)
})

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

input_dir  <- file.path("..", "input")
output_dir <- file.path("..", "output")

CAP_INPS <- "Elaborazione di Lorenzo Ruffino su dati Inps"

parse_num <- function(x) suppressWarnings(as.numeric(gsub("\\.", "", trimws(x))))

# --- File 1: dipendenti senza part-time, 52 settimane retribuite -----------

raw_ft <- read.csv(file.path(input_dir, "inps_province_full_time_52sett.csv"),
                   sep = ";", header = FALSE, col.names = paste0("X", 1:7),
                   fill = TRUE, fileEncoding = "latin1", quote = "\"",
                   stringsAsFactors = FALSE)

ft <- raw_ft %>%
  mutate(provincia = trimws(X1),
         lavoratori = parse_num(X3),
         retribuzione = parse_num(X4)) %>%
  filter(!is.na(lavoratori), provincia != "") %>%
  select(provincia, lavoratori, retribuzione)

tot_riga_ft <- ft %>% filter(provincia == "Totale")
estero_ft   <- ft %>% filter(provincia == "Estero")
ft <- ft %>% filter(!provincia %in% c("Totale", "Estero"))

stopifnot(nrow(ft) == 107)
stopifnot(sum(ft$lavoratori) + estero_ft$lavoratori == tot_riga_ft$lavoratori)
stopifnot(tot_riga_ft$lavoratori == 8630967)

# --- File 2: tutti i dipendenti (riga di chiusura blocco = totale provincia) --

raw_tot <- read.csv(file.path(input_dir, "inps_province_totale.csv"),
                    sep = ";", header = FALSE, col.names = paste0("X", 1:9),
                    fill = TRUE, fileEncoding = "latin1", quote = "\"",
                    stringsAsFactors = FALSE)

# La riga che chiude ogni blocco provincia ripete il nome in X1, con X2 vuota
# e X3 = "Totale": è il totale provincia (tutti gli incroci part-time × settimane).
tot <- raw_tot %>%
  mutate(provincia = trimws(X1),
         dipendenti_totali = parse_num(X5)) %>%
  filter(provincia != "", trimws(X2) == "", trimws(X3) == "Totale",
         !is.na(dipendenti_totali)) %>%
  select(provincia, dipendenti_totali)

tot_riga_tot <- tot %>% filter(provincia == "Totale")
estero_tot   <- tot %>% filter(provincia == "Estero")
tot <- tot %>% filter(!provincia %in% c("Totale", "Estero"))

stopifnot(nrow(tot) == 107)
stopifnot(sum(tot$dipendenti_totali) + estero_tot$dipendenti_totali ==
            tot_riga_tot$dipendenti_totali)
stopifnot(tot_riga_tot$dipendenti_totali == 17731002)

# --- Unione e misure -------------------------------------------------------

dati <- ft %>%
  inner_join(tot, by = "provincia") %>%
  mutate(salario = retribuzione / lavoratori,
         quota = lavoratori / dipendenti_totali * 100)

stopifnot(nrow(dati) == 107)
stopifnot(all(dati$lavoratori < dati$dipendenti_totali))
stopifnot(abs(dati$salario[dati$provincia == "Milano"] - 50442) < 1)

# Valori Italia (senza Estero)
ita_salario <- (tot_riga_ft$retribuzione - estero_ft$retribuzione) /
               (tot_riga_ft$lavoratori - estero_ft$lavoratori)
ita_quota   <- (tot_riga_ft$lavoratori - estero_ft$lavoratori) /
               (tot_riga_tot$dipendenti_totali - estero_tot$dipendenti_totali) * 100

cat("Italia: salario medio", round(ita_salario), "- quota",
    round(ita_quota, 1), "%\n")
cat("Range salario:", paste(round(range(dati$salario)), collapse = " - "), "\n")
cat("Range quota:", paste(round(range(dati$quota), 1), collapse = " - "), "\n")

# --- Aggancio ai NUTS-3 GISCO per nome normalizzato ------------------------

norm_nome <- function(x) {
  gsub("[^A-Z]", "", toupper(iconv(x, to = "ASCII//TRANSLIT")))
}

eccezioni <- c(
  "PROVINCIAAUTONOMADIBOLZANOBOZEN" = "ITH10",
  "PROVINCIAAUTONOMADITRENTO"       = "ITH20",
  "AOSTA"                           = "ITC20",
  "REGGIOEMILIA"                    = "ITH53",
  "REGGIOCALABRIA"                  = "ITF65"
)

geo <- gisco_get_nuts(country = "IT", nuts_level = 3,
                      resolution = "03", year = "2024") %>%
  st_transform(3035) %>%
  select(NUTS_ID, NAME_LATN) %>%
  mutate(chiave = norm_nome(NAME_LATN))

dati <- dati %>%
  mutate(chiave = norm_nome(provincia),
         NUTS_ID = if_else(chiave %in% names(eccezioni),
                           eccezioni[chiave], NA_character_))

match_nome <- dati %>%
  filter(is.na(NUTS_ID)) %>%
  select(-NUTS_ID) %>%
  inner_join(geo %>% st_drop_geometry() %>% select(NUTS_ID, chiave),
             by = "chiave")

dati <- bind_rows(dati %>% filter(!is.na(NUTS_ID)), match_nome) %>%
  select(NUTS_ID, provincia, lavoratori, retribuzione, salario,
         dipendenti_totali, quota)

geo_dati <- geo %>% left_join(dati, by = "NUTS_ID")

mancanti <- geo_dati %>% filter(is.na(salario)) %>% pull(NAME_LATN)
if (length(mancanti) > 0) stop("Province GISCO senza dato: ",
                               paste(mancanti, collapse = ", "))
orfani <- setdiff(dati$NUTS_ID, geo$NUTS_ID)
if (length(orfani) > 0) stop("Codici dati senza geometria: ",
                             paste(orfani, collapse = ", "))

# --- Contorni regionali (dissolvenza province → NUTS-2) --------------------

geo_reg <- geo_dati %>%
  mutate(reg = substr(NUTS_ID, 1, 4)) %>%
  group_by(reg) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# --- Bbox: taglio poco sotto la Sicilia (Pelagie fuori) --------------------

bbox_ita <- st_bbox(geo_dati)
y_cut <- st_coordinates(st_transform(
  st_sfc(st_point(c(12.5, 36.55)), crs = 4326), 3035))[2]
xlim_ita <- bbox_ita[c("xmin", "xmax")]
ylim_ita <- c(max(bbox_ita["ymin"], y_cut), bbox_ita["ymax"])

# --- Palette mako (viridisLite): chiaro = basso, scuro = alto --------------
# Scala di bin chiusi che copre tutto il range: le classi vuote restano in
# legenda (drop = FALSE) e mostrano quanto l'outlier stacca il resto.

pal_mako <- function(n) rev(viridisLite::mako(n, begin = 0.12, end = 0.94))

disegna_mappa <- function(df, bin_levels, palette, titolo, sottotitolo) {
  ggplot(df) +
    # show.legend = TRUE: senza, i bin vuoti (drop = FALSE) restano in
    # legenda senza quadratino perché il layer sf non disegna il glifo
    # per i livelli assenti dai dati
    geom_sf(aes(fill = bin), color = "white", linewidth = 0.15,
            show.legend = TRUE) +
    geom_sf(data = geo_reg, fill = NA, color = "#1C1C1C", linewidth = 0.5) +
    scale_fill_manual(
      values = setNames(palette, bin_levels),
      drop = FALSE,
      na.value = COL_NA_MAPPA,
      name = NULL,
      breaks = bin_levels
    ) +
    guides(fill = guide_legend(
      reverse = TRUE,             # valori alti in cima
      keyheight = unit(0.65, "cm"), keywidth = unit(0.5, "cm"),
      label.theme = element_text(family = "Source Sans Pro", size = 9.5,
                                 color = "#1C1C1C", hjust = 0)
    )) +
    coord_sf(xlim = xlim_ita, ylim = ylim_ita, crs = 3035, expand = FALSE) +
    theme_map() +
    theme(legend.position = c(0.99, 0.95),
          legend.justification = c(1, 1),
          legend.spacing.y = unit(0, "cm"),
          # element_markdown per il <b> nel sottotitolo (a capo con <br>)
          plot.subtitle = ggtext::element_markdown(
            size = 9, color = "#1C1C1C", hjust = 0, lineheight = 1.35,
            margin = margin(b = 0.25, t = 0.1, unit = "cm"))) +
    labs(title = titolo, subtitle = sottotitolo, caption = CAP_INPS)
}

# --- Mappa 1: salario medio (bin costanti da 3.000 euro fino a Milano) -----

bin_sal <- c("Meno di € 30.000", "€ 30.000–33.000", "€ 33.000–36.000",
             "€ 36.000–39.000", "€ 39.000–42.000", "€ 42.000–45.000",
             "€ 45.000–48.000", "€ 48.000–51.000")

geo_sal <- geo_dati %>%
  mutate(bin = factor(case_when(
    salario <  30000                    ~ bin_sal[1],
    salario >= 30000 & salario < 33000  ~ bin_sal[2],
    salario >= 33000 & salario < 36000  ~ bin_sal[3],
    salario >= 36000 & salario < 39000  ~ bin_sal[4],
    salario >= 39000 & salario < 42000  ~ bin_sal[5],
    salario >= 42000 & salario < 45000  ~ bin_sal[6],
    salario >= 45000 & salario < 48000  ~ bin_sal[7],
    TRUE                                ~ bin_sal[8]
  ), levels = bin_sal))

cat("\nProvince per bin (salario):\n")
print(table(geo_sal$bin))

p_sal <- disegna_mappa(
  geo_sal, bin_sal, pal_mako(length(bin_sal)),
  "Milano è la provincia con gli stipendi più alti",
  paste0("Retribuzione media annua dei dipendenti del settore privato <b>occupati a tempo<br>",
         "pieno per tutto l'anno</b>, per provincia, 2024. Italia € ",
         formatC(round(ita_salario), format = "d", big.mark = "."))
)

ggsave(file.path(output_dir, "salario_medio_provincia.png"),
       p_sal, width = 8.5, height = 9.1, dpi = 220, bg = "white")
cat("Salvata salario_medio_provincia.png\n")

# --- Mappa 2: quota sul totale dei dipendenti (bin costanti da 5 punti) ----
# Range reale 24,7-59,8: scala chiusa 25-60. Il minimo (Vibo Valentia, 24,7)
# arrotonda a 25 e sta nel primo bin.

bin_quo <- c("25–30%", "30–35%", "35–40%", "40–45%", "45–50%", "50–55%",
             "55–60%")

geo_quo <- geo_dati %>%
  mutate(bin = factor(case_when(
    quota <  30              ~ bin_quo[1],
    quota >= 30 & quota < 35 ~ bin_quo[2],
    quota >= 35 & quota < 40 ~ bin_quo[3],
    quota >= 40 & quota < 45 ~ bin_quo[4],
    quota >= 45 & quota < 50 ~ bin_quo[5],
    quota >= 50 & quota < 55 ~ bin_quo[6],
    TRUE                     ~ bin_quo[7]
  ), levels = bin_quo))

cat("\nProvince per bin (quota):\n")
print(table(geo_quo$bin))

p_quo <- disegna_mappa(
  geo_quo, bin_quo, pal_mako(length(bin_quo)),
  "Meno di metà dei dipendenti è a tempo pieno tutto l'anno",
  paste0("Quota dei dipendenti del settore privato <b>occupati a tempo pieno per tutto<br>",
         "l'anno</b> sul totale dei dipendenti privati, per provincia, 2024. Italia ",
         formatC(ita_quota, format = "f", digits = 1, decimal.mark = ","), "%")
)

ggsave(file.path(output_dir, "quota_tempo_pieno_provincia.png"),
       p_quo, width = 8.5, height = 9.1, dpi = 220, bg = "white")
cat("Salvata quota_tempo_pieno_provincia.png\n")

# --- CSV -------------------------------------------------------------------

export <- dati %>%
  mutate(salario = round(salario), quota = round(quota, 1)) %>%
  arrange(desc(salario)) %>%
  select(provincia, NUTS_ID, lavoratori_tempo_pieno_52sett = lavoratori,
         retribuzione_totale = retribuzione, salario_medio = salario,
         dipendenti_totali, quota_tempo_pieno = quota)

write_csv(export, file.path(output_dir, "stipendi_provincia_inps.csv"))

cat("\nTop 5 salario:\n")
print(head(export %>% select(provincia, salario_medio, quota_tempo_pieno), 5))
cat("\nBottom 5 salario:\n")
print(tail(export %>% select(provincia, salario_medio, quota_tempo_pieno), 5))
cat("\nTop 5 quota:\n")
print(head(export %>% arrange(desc(quota_tempo_pieno)) %>%
             select(provincia, quota_tempo_pieno, salario_medio), 5))
cat("\nBottom 5 quota:\n")
print(head(export %>% arrange(quota_tempo_pieno) %>%
             select(provincia, quota_tempo_pieno, salario_medio), 5))
