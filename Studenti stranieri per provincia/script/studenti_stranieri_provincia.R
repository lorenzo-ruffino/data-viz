# script/studenti_stranieri_provincia.R
# Quota di alunni con cittadinanza non italiana sul totale, per provincia,
# anno scolastico 2024/25. Fonte: open data del Ministero dell'istruzione
# e del merito (dati.istruzione.it), dato scuola per scuola aggregato a
# provincia tramite l'anagrafica delle scuole.
#
# Eseguito da: cd script && Rscript studenti_stranieri_provincia.R

source("/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities/R/mappe.R")

suppressPackageStartupMessages({
  library(tidyverse)
  library(showtext)
  library(sf)
  library(giscoR)
  library(scales)
})

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

CAP_MIM <- "Elaborazione di Lorenzo Ruffino su dati Ministero dell'istruzione e del merito"

source_dir <- ".."
input_dir  <- file.path(source_dir, "input")
output_dir <- file.path(source_dir, "output")

# --- 1) DATI ----------------------------------------------------------------

# Anagrafica: CODICESCUOLA -> provincia. Statali e paritarie hanno due file
# con tracciati diversi, ma le colonne che servono sono le stesse.
anagrafe <- bind_rows(
  read_csv(file.path(input_dir, "SCUANAGRAFESTAT20242520250831.csv"),
           show_col_types = FALSE) |>
    select(CODICESCUOLA, REGIONE, PROVINCIA),
  read_csv(file.path(input_dir, "SCUANAGRAFEPAR20242520250831.csv"),
           show_col_types = FALSE) |>
    select(CODICESCUOLA, REGIONE, PROVINCIA)
) |>
  distinct(CODICESCUOLA, .keep_all = TRUE)

# Primaria e secondarie: una riga per scuola x anno di corso.
alunni <- bind_rows(
  read_csv(file.path(input_dir, "ALUITASTRACITSTA20242520250831.csv"),
           show_col_types = FALSE),
  read_csv(file.path(input_dir, "ALUITASTRACITPAR20242520250831.csv"),
           show_col_types = FALSE)
) |>
  transmute(CODICESCUOLA,
            alunni    = ALUNNI,
            stranieri = ALUNNICITTADINANZANONITALIANA)

# Infanzia: file separato, senza anno di corso e con il totale da ricostruire.
infanzia <- bind_rows(
  read_csv(file.path(input_dir, "INFANZIASTRACITSTA20242520250831.csv"),
           show_col_types = FALSE),
  read_csv(file.path(input_dir, "INFANZIASTRACITPAR20242520250831.csv"),
           show_col_types = FALSE)
) |>
  transmute(CODICESCUOLA,
            alunni    = BAMBINICITTADINANZAITALIANA + BAMBINICITTADINANZANONITALIANA,
            stranieri = BAMBINICITTADINANZANONITALIANA)

per_provincia <- bind_rows(alunni, infanzia) |>
  left_join(anagrafe, by = "CODICESCUOLA") |>
  group_by(REGIONE, PROVINCIA) |>
  summarise(alunni = sum(alunni), stranieri = sum(stranieri), .groups = "drop") |>
  mutate(quota = stranieri / alunni * 100)

# Ogni scuola deve trovare la sua provincia nell'anagrafica.
stopifnot(!any(is.na(per_provincia$PROVINCIA)))
cat("Province coperte:", nrow(per_provincia), "\n")
cat(sprintf("Italia (province MIM): %s su %s = %.2f%%\n",
            format(sum(per_provincia$stranieri), big.mark = "."),
            format(sum(per_provincia$alunni), big.mark = "."),
            sum(per_provincia$stranieri) / sum(per_provincia$alunni) * 100))

write_csv(per_provincia |> arrange(desc(quota)),
          file.path(output_dir, "studenti_stranieri_provincia.csv"))

# --- 2) GEOMETRIE -----------------------------------------------------------

# Risoluzione 1:3 milioni invece di 1:10: i confini provinciali e la linea di
# costa restano leggibili anche sulle province piccole.
geo <- gisco_get_nuts(country = "IT", nuts_level = 3,
                      resolution = "03", year = "2021") |>
  st_transform(3035)

# NUTS-2 = regioni, disegnate sopra con un tratto più marcato per orientarsi.
geo_regioni <- gisco_get_nuts(country = "IT", nuts_level = 2,
                              resolution = "03", year = "2021") |>
  st_transform(3035)

# Le denominazioni MIM sono maiuscole e senza accenti: normalizzo entrambi i
# lati e correggo a mano i tre casi in cui i due nomi divergono davvero.
normalizza <- function(x) {
  x |>
    str_replace_all("’", "'") |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_to_upper() |>
    str_trim()
}

rinomina <- c(
  "FORLI'-CESENA"   = "FORLI-CESENA",
  "REGGIO CALABRIA" = "REGGIO DI CALABRIA",
  "REGGIO EMILIA"   = "REGGIO NELL'EMILIA"
)

dati <- per_provincia |>
  mutate(chiave = normalizza(coalesce(rinomina[PROVINCIA], PROVINCIA)))

geo_dati <- geo |>
  mutate(chiave = normalizza(NAME_LATN)) |>
  left_join(dati, by = "chiave")

# Le uniche province senza dato devono essere le tre a statuto speciale che
# non confluiscono nelle rilevazioni del ministero.
senza_dato <- geo_dati$NAME_LATN[is.na(geo_dati$quota)]
cat("Senza dato:", paste(senza_dato, collapse = ", "), "\n")
stopifnot(length(senza_dato) == 3)
stopifnot(nrow(dati) == sum(!is.na(geo_dati$quota)))

# --- 3) BINNING DISCRETO ----------------------------------------------------

# Bin di ampiezza costante (5 punti), primo e ultimo aperti.
# "dato non disponibile" è il PRIMO livello perche' la legenda gira con
# reverse = TRUE: cosi' in alto resta il bin piu' intenso e il grigio finisce
# in fondo, sotto "meno del 5%".
bin_levels <- c("dato non disponibile",
                "meno del 5%", "dal 5 al 10%", "dal 10 al 15%",
                "dal 15 al 20%", "dal 20 al 25%", "25% e oltre")

# Rampa monocromatica costruita attorno al rosso di repertorio (#F12938).
bin_colours <- c(
  "meno del 5%"          = "#FDEDED",
  "dal 5 al 10%"         = "#FBCFCF",
  "dal 10 al 15%"        = "#F79C9C",
  "dal 15 al 20%"        = "#EF5A5A",
  "dal 20 al 25%"        = "#CB1C2B",
  "25% e oltre"          = "#800D17",
  "dato non disponibile" = COL_NA_MAPPA
)

geo_dati <- geo_dati |>
  mutate(bin = factor(case_when(
    is.na(quota) ~ "dato non disponibile",
    quota <  5   ~ "meno del 5%",
    quota < 10   ~ "dal 5 al 10%",
    quota < 15   ~ "dal 10 al 15%",
    quota < 20   ~ "dal 15 al 20%",
    quota < 25   ~ "dal 20 al 25%",
    TRUE         ~ "25% e oltre"
  ), levels = bin_levels))

print(count(st_drop_geometry(geo_dati), bin))

# --- 4) MAPPA ---------------------------------------------------------------

p <- ggplot(geo_dati) +
  geom_sf(aes(fill = bin), colour = "white", linewidth = 0.15) +
  geom_sf(data = geo_regioni, fill = NA,
          colour = "#3A3A3A", linewidth = 0.42) +
  scale_fill_manual(values = bin_colours, drop = FALSE, na.translate = FALSE) +
  # Lampedusa (ymin 1.386 mln) allungherebbe la tela di 130 km di mare vuoto
  # sotto la Sicilia: taglio sotto Pantelleria, che resta dentro.
  coord_sf(xlim = c(4040000, 5065000), ylim = c(1515000, 2672000),
           expand = FALSE) +
  guides(fill = guide_legend(ncol = 1, reverse = TRUE,
                             keywidth = unit(0.5, "cm"),
                             keyheight = unit(0.42, "cm"))) +
  labs(
    title = "Un alunno su otto non ha la cittadinanza italiana",
    subtitle = paste0(
      "Alunni con cittadinanza non italiana in percentuale sul totale degli iscritti,\n",
      "dall'infanzia alle superiori, per provincia, anno scolastico 2024/25"
    ),
    caption = CAP_MIM
  ) +
  theme_map() +
  theme(
    legend.position = c(0.99, 0.88),
    legend.justification = c(1, 1),
    legend.text = element_text(size = 9, color = "#1C1C1C", hjust = 0)
  )

ggsave(file.path(output_dir, "studenti_stranieri_provincia.png"),
       plot = p, width = 8, height = 9, dpi = 220, bg = "white")

cat("Fatto.\n")
