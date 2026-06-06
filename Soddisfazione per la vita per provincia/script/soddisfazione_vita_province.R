# Quota di persone di 14 anni e più soddisfatte della propria vita (voto da 8
# a 10), per provincia italiana, ultimo anno disponibile.
# Eseguito da `cd script && Rscript soddisfazione_vita_province.R`.
#
# Mappa choropleth provinciale (NUTS-3) con scala DIVERGENTE blu-rossa spezzata
# sulla media nazionale (~54%): blu sopra la media, rosso sotto, bin da 2 punti,
# nessuna classe bianca.
# Fonte: Istat, Bes delle province, dataflow DF_DCSS_BEST_PPC_2_GC (persone di
# 14 anni e più per livello di soddisfazione della vita - province). I dati
# grezzi (conteggi di popolazione) sono in input/istat_*.tsv: la quota è già
# calcolata come (voti 8 + 9 + 10) / totale.

source("/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities/R/mappe.R")
suppressPackageStartupMessages({
  library(tidyverse)
  library(giscoR)
  library(showtext)
  library(sf)
})

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

CAP_ISTAT <- "Elaborazione di Lorenzo Ruffino su dati Istat"

# Media nazionale (BES province): vedi riga IT del dataflow, 53,8% nel 2024.
ITALIA_VAL  <- 53.8
SPARTIACQUE <- 54     # soglia blu/rosso (≈ media nazionale)

# --- Dati: quota voti 8-10 per provincia -----------------------------------

raw <- read_tsv("../input/istat_DF_DCSS_BEST_PPC_2_GC.tsv", show_col_types = FALSE)

anno_label <- max(raw$TIME_PERIOD, na.rm = TRUE)

# Crosswalk codici Istat ITTER107 → codici NUTS 2021 delle geometrie GISCO.
# Regola generale: Nord-est ITD*→ITH*, Centro ITE*→ITI*. Più alcune eccezioni
# (Milano, Foggia, Bari, Sardegna riorganizzata, province nuove IT108-IT111).
nuts_da_istat <- function(code) {
  special <- c(ITC45 = "ITC4C", IT108 = "ITC4D",
               ITF41 = "ITF46", ITF42 = "ITF47", IT110 = "ITF48",
               ITG25 = "ITG2D", ITG26 = "ITG2E", ITG27 = "ITG2F",
               ITG28 = "ITG2G", IT111 = "ITG2H", IT109 = "ITI35")
  out <- ifelse(code %in% names(special), unname(special[code]), code)
  out <- ifelse(startsWith(out, "ITD"), sub("^ITD", "ITH", out), out)
  out <- ifelse(startsWith(out, "ITE"), sub("^ITE", "ITI", out), out)
  out
}

dati_prov <- raw %>%
  transmute(NUTS_ID = nuts_da_istat(REF_AREA), valore = quota_8_10)

stopifnot(nrow(dati_prov) == 107)

# --- Geometrie province (NUTS-3, GISCO 2021) -------------------------------

geo <- gisco_get_nuts(country = "IT", nuts_level = 3,
                      resolution = "03", year = "2021") %>%
  st_transform(3035) %>%
  select(NUTS_ID, NAME_LATN)

geo_dati <- geo %>% left_join(dati_prov, by = "NUTS_ID")

# Verifica che tutte le 107 province abbiano il dato (crosswalk corretto)
mancanti <- geo_dati %>% filter(is.na(valore)) %>% pull(NAME_LATN)
if (length(mancanti) > 0) stop("Province senza dato: ", paste(mancanti, collapse = ", "))

cat("Anno:", anno_label, "\n")
cat("Italia:", ITALIA_VAL, "\n")
cat("Range province:", paste(round(range(geo_dati$valore), 1), collapse = " - "), "\n")

write_csv(
  st_drop_geometry(geo_dati) %>%
    select(NUTS_ID, provincia = NAME_LATN, soddisfatti_8_10 = valore) %>%
    arrange(desc(soddisfatti_8_10)),
  "../output/soddisfazione_vita_province.csv"
)

# --- Binning divergente (bin da 2, spezzato sulla media 54) ----------------
#
# Niente classe bianca: il rosso più chiaro ("da 52 a 54%") confina con il blu
# più chiaro ("da 54 a 56%") proprio sulla soglia 54.
bin_levels <- c("< 48%", "da 48 a 50%", "da 50 a 52%", "da 52 a 54%",
                "da 54 a 56%", "da 56 a 58%", "da 58 a 60%", "da 60 a 62%",
                "da 62 a 64%", "≥ 64%")
# 4 classi sotto 54 (rosso, scuro→chiaro) + 6 sopra 54 (blu, chiaro→scuro).
rosso <- colorRampPalette(c("#B2182B", "#D6604D", "#F4A582", "#FDDBC7"))(4)
blu   <- colorRampPalette(c("#D1E5F0", "#92C5DE", "#4393C3", "#2166AC",
                            "#0B3D6B"))(6)
bin_colours <- setNames(c(rosso, blu), bin_levels)

# Etichetta legenda: colore del testo non serve (no label sulle province).
geo_dati <- geo_dati %>%
  mutate(bin = factor(case_when(
    valore <  48 ~ "< 48%",
    valore <  50 ~ "da 48 a 50%",
    valore <  52 ~ "da 50 a 52%",
    valore <  54 ~ "da 52 a 54%",
    valore <  56 ~ "da 54 a 56%",
    valore <  58 ~ "da 56 a 58%",
    valore <  60 ~ "da 58 a 60%",
    valore <  62 ~ "da 60 a 62%",
    valore <  64 ~ "da 62 a 64%",
    TRUE         ~ "≥ 64%"
  ), levels = bin_levels))

# --- Contorni regionali (dissolvenza province → NUTS-2) --------------------

geo_reg <- geo_dati %>%
  mutate(reg = substr(NUTS_ID, 1, 4)) %>%
  group_by(reg) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# --- Mappa ------------------------------------------------------------------

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
    reverse = TRUE,             # valori alti (blu scuro) in cima
    keyheight = unit(0.55, "cm"), keywidth = unit(0.45, "cm"),
    label.theme = element_text(family = "Source Sans Pro", size = 9,
                               color = "#1C1C1C", hjust = 0)
  )) +
  coord_sf(crs = 3035, expand = FALSE) +
  theme_map() +
  theme(legend.position = c(0.99, 0.95),
        legend.justification = c(1, 1),
        legend.spacing.y = unit(0, "cm")) +
  labs(
    title = "Massima soddisfazione per la vita a Bolzano",
    subtitle = paste0(
      "Quota di persone di 14 anni e più che danno un voto da 8 a 10 alla\n",
      "soddisfazione per la propria vita, province italiane, ", anno_label,
      ". Italia ", formatC(ITALIA_VAL, format = "f", digits = 1,
                           decimal.mark = ","), "%\n",
      "In blu le province sopra la media italiana, in rosso quelle sotto"
    ),
    caption = CAP_ISTAT
  )

# NB: dpi allineato a showtext_opts(dpi) e al resto del repo (220). Alzarlo
# senza alzare anche showtext_opts rimpicciolisce titolo/sottotitolo/legenda.
# La nitidezza dei confini dipende dalla geometria (1:3M), non dal dpi.
ggsave("../output/soddisfazione_vita_province.png",
       plot = p, width = 8.5, height = 9.5, units = "in", dpi = 220, bg = "white")

cat("Mappa salvata in ../output/soddisfazione_vita_province.png\n")
