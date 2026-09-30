# Suolo consumato in percentuale della superficie per provincia, 2024.
# Fonte: ISPRA, "Consumo di suolo, dinamiche territoriali e servizi
# ecosistemici" - estratto dati 2025 (input/ispra_consumo_suolo_2025.xlsx).
# Eseguito da `cd script && Rscript consumo_suolo_italia.R`.

source("/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities/R/mappe.R")
suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(giscoR)
  library(showtext)
  library(sf)
})

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

output_dir <- file.path("..", "output")
input_dir  <- file.path("..", "input")
xlsx <- file.path(input_dir, "ispra_consumo_suolo_2025.xlsx")

# --- Province: suolo consumato 2024 ---------------------------------------

prov <- read_excel(xlsx, sheet = "Province_2006_2024") %>%
  select(provincia = Nome_Provincia, regione = Nome_Regione,
         ettari = `Suolo consumato 2024 [ettari]`,
         quota  = `Suolo consumato 2024 [%]`)

cat("Province:", nrow(prov), "\n")
cat("Range:", round(min(prov$quota), 2), "-", round(max(prov$quota), 2), "\n")

# --- Dato Italia (dalle regioni: ettari / superficie implicita) -----------

reg <- read_excel(xlsx, sheet = "Regioni_2006_2024") %>%
  select(regione = Nome_Regione,
         ettari = `Suolo consumato 2024 [ettari]`,
         quota  = `Suolo consumato 2024 [%]`) %>%
  mutate(superficie = ettari / quota * 100)

media_ita <- sum(reg$ettari) / sum(reg$superficie) * 100
cat("Italia 2024:", round(media_ita, 2), "%\n")

# --- Aggancio ai NUTS-3 GISCO per nome normalizzato -----------------------

norm_nome <- function(x) {
  gsub("[^A-Z]", "", toupper(iconv(x, to = "ASCII//TRANSLIT")))
}

eccezioni <- c(
  "AOSTA"            = "ITC20",
  "BOLZANO"          = "ITH10",
  "REGGIODICALABRIA" = "ITF65"
)

geo <- gisco_get_nuts(country = "IT", nuts_level = 3,
                      resolution = "03", year = "2024") %>%
  st_transform(3035) %>%
  select(NUTS_ID, NAME_LATN) %>%
  mutate(chiave = norm_nome(NAME_LATN))

dati_prov <- prov %>%
  mutate(chiave = norm_nome(provincia),
         NUTS_ID = if_else(chiave %in% names(eccezioni),
                           eccezioni[chiave], NA_character_))

match_nome <- dati_prov %>%
  filter(is.na(NUTS_ID)) %>%
  select(-NUTS_ID) %>%
  inner_join(geo %>% st_drop_geometry() %>% select(NUTS_ID, chiave),
             by = "chiave")

dati_prov <- bind_rows(dati_prov %>% filter(!is.na(NUTS_ID)), match_nome) %>%
  select(NUTS_ID, provincia, regione, ettari, quota)

geo_dati <- geo %>% left_join(dati_prov, by = "NUTS_ID")

mancanti <- geo_dati %>% filter(is.na(quota)) %>% pull(NAME_LATN)
if (length(mancanti) > 0) {
  cat("Province GISCO senza dato:", paste(mancanti, collapse = ", "), "\n")
}
orfani <- setdiff(dati_prov$NUTS_ID, geo$NUTS_ID)
if (length(orfani) > 0) {
  cat("Codici dati senza geometria:", paste(orfani, collapse = ", "), "\n")
}

# --- Binning (2 punti percentuali costanti) -------------------------------

bin_levels <- c("Meno del 4%", "4–6%", "6–8%", "8–10%",
                "10–12%", "12–14%", "14–16%", "16% e oltre")

bin_colours <- c(
  "Meno del 4%" = "#EDF3FC",
  "4–6%"   = "#CDE0F6",
  "6–8%"   = "#9ABFEA",
  "8–10%"  = "#5C9CDE",
  "10–12%" = "#2279C3",
  "12–14%" = "#0E5BAD",
  "14–16%" = "#083C7A",
  "16% e oltre" = "#041C3B"
)

geo_dati <- geo_dati %>%
  mutate(bin = factor(case_when(
    is.na(quota)               ~ NA_character_,
    quota <  4                 ~ bin_levels[1],
    quota >= 4  & quota < 6    ~ bin_levels[2],
    quota >= 6  & quota < 8    ~ bin_levels[3],
    quota >= 8  & quota < 10   ~ bin_levels[4],
    quota >= 10 & quota < 12   ~ bin_levels[5],
    quota >= 12 & quota < 14   ~ bin_levels[6],
    quota >= 14 & quota < 16   ~ bin_levels[7],
    TRUE                       ~ bin_levels[8]
  ), levels = bin_levels))

# --- Contorni regionali ---------------------------------------------------

geo_reg <- geo_dati %>%
  mutate(reg = substr(NUTS_ID, 1, 4)) %>%
  group_by(reg) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# --- Mappa ----------------------------------------------------------------

# Taglio del bbox poco sotto la Sicilia: le Pelagie (Lampedusa, Linosa)
# allungherebbero la mappa a sud lasciando una fascia bianca sopra la caption.
bbox_ita <- st_bbox(geo_dati)
y_cut <- st_coordinates(st_transform(
  st_sfc(st_point(c(12.5, 36.55)), crs = 4326), 3035))[2]

p <- ggplot(geo_dati) +
  geom_sf(aes(fill = bin), color = "white", linewidth = 0.15) +
  geom_sf(data = geo_reg, fill = NA, color = "#1C1C1C", linewidth = 0.5) +
  scale_fill_manual(
    values = bin_colours,
    drop = FALSE,
    na.value = COL_NA_MAPPA,
    name = NULL,
    breaks = bin_levels
  ) +
  guides(fill = guide_legend(
    reverse = TRUE,
    keyheight = unit(0.65, "cm"), keywidth = unit(0.5, "cm"),
    label.theme = element_text(family = "Source Sans Pro", size = 9.5,
                               color = "#1C1C1C", hjust = 0)
  )) +
  coord_sf(xlim = bbox_ita[c("xmin", "xmax")],
           ylim = c(max(bbox_ita["ymin"], y_cut), bbox_ita["ymax"]),
           crs = 3035, expand = FALSE) +
  theme_map() +
  theme(legend.position = c(0.99, 0.95),
        legend.justification = c(1, 1),
        legend.spacing.y = unit(0, "cm")) +
  labs(
    title = "Dove è stato consumato più suolo in Italia?",
    subtitle = paste0(
      "Suolo consumato in percentuale della superficie per provincia, 2024. ",
      "Italia ", formatC(media_ita, format = "f", digits = 1,
                         decimal.mark = ","), "%"
    ),
    caption = "Elaborazione di Lorenzo Ruffino su dati ISPRA"
  )

ggsave(file.path(output_dir, "consumo_suolo_italia.png"),
       p, width = 8.5, height = 9.1, dpi = 220, bg = "white")

cat("Mappa salvata.\n")

# --- CSV -------------------------------------------------------------------

export <- geo_dati %>%
  st_drop_geometry() %>%
  filter(!is.na(quota)) %>%
  arrange(desc(quota)) %>%
  select(provincia, regione, NUTS_ID, ettari, quota)

write_csv(export, file.path(output_dir, "consumo_suolo_italia.csv"))

cat("\nTop 5:\n")
print(head(export, 5))
cat("\nBottom 5:\n")
print(tail(export, 5))
