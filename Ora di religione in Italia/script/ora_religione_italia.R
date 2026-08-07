# Quota di alunni che non si avvalgono dell'insegnamento della religione
# cattolica per provincia, anno scolastico 2024/25.
# Dati per scuola: MIM via istanza di accesso civico UAAR
# (input/mim_irc_2024_25.xlsx, repo dati-no-irc).
# Provincia di ogni scuola: anagrafiche MIM open data 2024/25
# (input/SCUANAGRAFE*.csv, input/SCUANAAUT*.csv).
# Eseguito da `cd script && Rscript ora_religione_italia.R`.

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

# --- Dati IRC per scuola (2024/25) ----------------------------------------
# Privacy MIM: i conteggi pari o inferiori a 3 sono riportati come "<=3",
# li fissiamo a 3 (stessa convenzione delle elaborazioni precedenti).

irc <- read_excel(file.path(input_dir, "mim_irc_2024_25.xlsx"), skip = 2,
                  col_names = c("sigla", "codice_scuola", "n_tot", "n_irc"),
                  col_types = "text") %>%
  filter(!is.na(codice_scuola)) %>%
  mutate(
    n_tot = as.numeric(if_else(n_tot == "<=3", "3", n_tot)),
    n_irc = as.numeric(if_else(n_irc == "<=3", "3", n_irc))
  )

cat("Scuole nel file IRC:", nrow(irc), "\n")

# --- Anagrafiche scuole 2024/25 (statali, paritarie, autonome) ------------

anagrafiche <- c("SCUANAGRAFESTAT20242520240901.csv",
                 "SCUANAGRAFEPAR20242520240901.csv",
                 "SCUANAAUTSTAT20242520240901.csv",
                 "SCUANAAUTPAR20242520240901.csv")

registro <- map_dfr(anagrafiche, function(f) {
  read_csv(file.path(input_dir, f), show_col_types = FALSE,
           col_types = cols(.default = col_character())) %>%
    select(CODICESCUOLA, PROVINCIA)
}) %>%
  distinct(CODICESCUOLA, .keep_all = TRUE)

dati <- irc %>%
  left_join(registro, by = c("codice_scuola" = "CODICESCUOLA"))

n_senza <- sum(is.na(dati$PROVINCIA))
cat("Scuole senza match in anagrafica:", n_senza, "di", nrow(dati), "\n")

# Fallback per i codici non in anagrafica: sigla del file MIM.
# Attenzione: le sigle MIM sono storiche (CA copre anche il Sud Sardegna,
# FO = Forlì-Cesena, PS = Pesaro e Urbino), quindi il fallback su CA
# attribuisce a Cagliari: stampiamo quante scuole ricadono nel caso ambiguo.
sigla_ambigue <- dati %>% filter(is.na(PROVINCIA), sigla == "CA") %>% nrow()
cat("  di cui con sigla CA (ambigua Cagliari/Sud Sardegna):", sigla_ambigue, "\n")

sigla_to_prov <- registro %>%
  mutate(sigla = substr(CODICESCUOLA, 1, 2)) %>%
  count(sigla, PROVINCIA) %>%
  group_by(sigla) %>%
  slice_max(n, n = 1) %>%
  ungroup() %>%
  select(sigla, prov_fallback = PROVINCIA)

dati <- dati %>%
  left_join(sigla_to_prov, by = "sigla") %>%
  mutate(PROVINCIA = coalesce(PROVINCIA, prov_fallback)) %>%
  select(-prov_fallback)

cat("Scuole ancora senza provincia:",
    sum(is.na(dati$PROVINCIA)), "\n")

# --- Aggregazione provinciale ---------------------------------------------

dati_prov <- dati %>%
  filter(!is.na(PROVINCIA)) %>%
  group_by(PROVINCIA) %>%
  summarise(
    alunni  = sum(n_tot, na.rm = TRUE),
    irc     = sum(n_irc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    no_irc = alunni - irc,
    quota  = round(no_irc / alunni * 100, 1)
  )

italia_tot  <- sum(dati_prov$alunni)
italia_no   <- sum(dati_prov$no_irc)
media_ita   <- round(italia_no / italia_tot * 100, 1)
cat("Italia 2024/25:", italia_no, "/", italia_tot, "=", media_ita, "%\n")
cat("Province:", nrow(dati_prov), "\n")
cat("Range:", min(dati_prov$quota), "-", max(dati_prov$quota), "\n")

# --- Aggancio ai NUTS-3 GISCO per nome ------------------------------------
# Normalizzazione: maiuscole, senza accenti/apostrofi/trattini/spazi.
# Eccezioni esplicite dove il nome MIM e quello GISCO divergono davvero.

normalizza <- function(x) {
  x %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
    toupper() %>%
    gsub("[^A-Z]", "", .)
}

eccezioni <- c(
  "AOSTA"           = "ITC20",  # GISCO: Valle d'Aosta/Vallée d'Aoste
  "REGGIOEMILIA"    = "ITH53",  # GISCO: Reggio nell'Emilia
  "REGGIOCALABRIA"  = "ITF65"   # GISCO: Reggio di Calabria
)

geo <- gisco_get_nuts(country = "IT", nuts_level = 3,
                      resolution = "03", year = "2024") %>%
  st_transform(3035) %>%
  select(NUTS_ID, NAME_LATN) %>%
  mutate(nome_norm = normalizza(NAME_LATN))

dati_prov <- dati_prov %>%
  mutate(
    nome_norm = normalizza(PROVINCIA),
    NUTS_ID   = if_else(nome_norm %in% names(eccezioni),
                        eccezioni[nome_norm], NA_character_)
  )

match_nome <- geo %>% st_drop_geometry() %>% select(nome_norm, NUTS_ID_geo = NUTS_ID)

dati_prov <- dati_prov %>%
  left_join(match_nome, by = "nome_norm") %>%
  mutate(NUTS_ID = coalesce(NUTS_ID, NUTS_ID_geo)) %>%
  select(-NUTS_ID_geo)

non_agganciate <- dati_prov %>% filter(is.na(NUTS_ID)) %>% pull(PROVINCIA)
if (length(non_agganciate) > 0) {
  cat("PROVINCE NON AGGANCIATE:", paste(non_agganciate, collapse = ", "), "\n")
}

geo_dati <- geo %>% left_join(dati_prov, by = "NUTS_ID")

mancanti <- geo_dati %>% filter(is.na(quota)) %>% pull(NAME_LATN)
cat("Geometrie senza dato (attese Bolzano e Trento):",
    paste(mancanti, collapse = ", "), "\n")

# --- Binning (5 punti percentuali costanti) -------------------------------

bin_levels <- c("Meno del 10%", "10–15%", "15–20%", "20–25%",
                "25–30%", "30–35%", "35% e oltre")

bin_colours <- c(
  "Meno del 10%" = "#EDF3FC",
  "10–15%"  = "#C6DCF5",
  "15–20%"  = "#8FB9E7",
  "20–25%"  = "#4A90D9",
  "25–30%"  = "#1D6DB8",
  "30–35%"  = "#0A4A8F",
  "35% e oltre" = "#04264F"
)

geo_dati <- geo_dati %>%
  mutate(bin = factor(case_when(
    is.na(quota)             ~ NA_character_,
    quota <  10              ~ "Meno del 10%",
    quota >= 10 & quota < 15 ~ "10–15%",
    quota >= 15 & quota < 20 ~ "15–20%",
    quota >= 20 & quota < 25 ~ "20–25%",
    quota >= 25 & quota < 30 ~ "25–30%",
    quota >= 30 & quota < 35 ~ "30–35%",
    TRUE                     ~ "35% e oltre"
  ), levels = bin_levels))

# --- Contorni regionali ---------------------------------------------------

geo_reg <- geo_dati %>%
  mutate(reg = substr(NUTS_ID, 1, 4)) %>%
  group_by(reg) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# --- Mappa ----------------------------------------------------------------

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
  coord_sf(crs = 3035, expand = FALSE) +
  theme_map() +
  theme(legend.position = c(0.99, 0.95),
        legend.justification = c(1, 1),
        legend.spacing.y = unit(0, "cm")) +
  labs(
    title = "Dove non si fa l'ora di religione",
    subtitle = paste0(
      "Quota di alunni che non si avvalgono dell'insegnamento della religione cattolica\n",
      "per provincia, anno scolastico 2024/25. Italia ",
      formatC(media_ita, format = "f", digits = 1, decimal.mark = ","),
      "%. Bolzano e Trento non comunicano il dato"
    ),
    caption = "Elaborazione di Lorenzo Ruffino su dati MIM e UAAR"
  )

ggsave(file.path(output_dir, "ora_religione_italia.png"),
       p, width = 8.5, height = 9.5, dpi = 220, bg = "white")

cat("Mappa salvata.\n")

# --- CSV -------------------------------------------------------------------

export <- geo_dati %>%
  st_drop_geometry() %>%
  filter(!is.na(quota)) %>%
  arrange(desc(quota)) %>%
  select(provincia = NAME_LATN, NUTS_ID, alunni, frequentanti_irc = irc,
         non_avvalentisi = no_irc, quota)

write_csv(export, file.path(output_dir, "ora_religione_italia.csv"))

cat("\nTop 5:\n")
print(head(export, 5))
cat("\nBottom 5:\n")
print(tail(export, 5))
