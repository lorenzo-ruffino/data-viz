# script/aiuti_di_stato_crisi.R — eseguito da `cd script && Rscript aiuti_di_stato_crisi.R`
#
# Aiuti di Stato erogati sotto i quadri d'emergenza (Covid e Temporary Crisis
# and Transition Framework per l'industria pulita) tra il 2020 e il 2024:
# quota di ogni paese sul totale dell'Unione, confrontata con la quota dello
# stesso paese sul Pil dell'Unione.
#
# Fonte: Commissione europea, State Aid Scoreboard 2025 (edizione del 15
# gennaio 2026, dati fino al 2024), estratto dal dashboard ufficiale
# https://competition-policy.ec.europa.eu/state-aid/scoreboard/scoreboard-state-aid-data_en
# Il perimetro dello Scoreboard esclude ferrovie e istituzioni finanziarie.
#
# La quota di Pil di ciascun paese si ricava dal file stesso: il rapporto fra
# aiuti in percentuale del Pil dell'Unione e aiuti in percentuale del Pil
# nazionale è il peso del paese sull'economia europea.

suppressPackageStartupMessages({
  library(tidyverse)
  library(showtext)
})

source_dir <- ".."
input_dir  <- file.path(source_dir, "input")
output_dir <- file.path(source_dir, "output")

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO  <- "#1C1C1C"
COL_ROSSO <- "#F12938"

theme_barchart <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro"),
      legend.position = "none",
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 9.5, color = COL_NERO, hjust = 0),
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
                                   margin = margin(b = 0.35, t = 0.1, unit = "cm")),
      plot.caption = element_text(size = 9, color = COL_NERO, hjust = 1,
                                  margin = margin(t = 0.5, unit = "cm")),
      ...
    )
}

CAP_COMM <- "Elaborazione di Lorenzo Ruffino su dati Commissione europea, State Aid Scoreboard 2025"

# Vedi prezzi_telefonia.R: una riga di sottotitolo tiene circa 124 caratteri e
# l'a capo va messo solo a riga piena, su una virgola o un punto.

ANNO_DA <- 2020
ANNO_A  <- 2024
N_PAESI <- 10

# --- 1) Dati ----------------------------------------------------------------

nomi_it <- c(
  Germany = "Germania", Italy = "Italia", France = "Francia",
  Poland = "Polonia", Spain = "Spagna", Austria = "Austria",
  Netherlands = "Paesi Bassi", Greece = "Grecia", Hungary = "Ungheria",
  Czechia = "Repubblica Ceca", Denmark = "Danimarca", Romania = "Romania",
  Belgium = "Belgio", Sweden = "Svezia", Portugal = "Portogallo",
  Finland = "Finlandia", Ireland = "Irlanda", Slovakia = "Slovacchia",
  Bulgaria = "Bulgaria", Croatia = "Croazia", Slovenia = "Slovenia",
  Lithuania = "Lituania", Latvia = "Lettonia", Estonia = "Estonia",
  Luxembourg = "Lussemburgo", Cyprus = "Cipro", Malta = "Malta"
)

grezzi <- read_csv(file.path(input_dir,
                             "state_aid_scoreboard_2025_panel_ms_year_measure.csv"),
                   col_types = cols()) %>%
  filter(expenditure_year >= ANNO_DA, expenditure_year <= ANNO_A)

# Peso di ogni paese sul Pil dell'Unione, medio sul periodo.
pil <- grezzi %>%
  filter(aid_pct_nat_gdp > 0) %>%
  mutate(quota_pil = aid_pct_eu_gdp / aid_pct_nat_gdp * 100) %>%
  group_by(member_state_name) %>%
  summarise(quota_pil = mean(quota_pil), .groups = "drop")

crisi <- grezzi %>%
  filter(aid_measure != "Non-crisis aid") %>%
  group_by(member_state_name) %>%
  summarise(aiuti = sum(aid_eur_mln_current), .groups = "drop") %>%
  mutate(quota_aiuti = aiuti / sum(aiuti) * 100) %>%
  left_join(pil, by = "member_state_name") %>%
  arrange(desc(aiuti))

TOTALE_MLD <- sum(crisi$aiuti) / 1000

# I paesi oltre i primi dieci finiscono in una riga sola.
resto <- crisi %>% slice((N_PAESI + 1):n())

dati <- crisi %>%
  slice(1:N_PAESI) %>%
  transmute(paese = recode(member_state_name, !!!nomi_it),
            aiuti, quota_aiuti, quota_pil) %>%
  bind_rows(tibble(paese = paste("Altri", nrow(resto), "paesi"),
                   aiuti = sum(resto$aiuti),
                   quota_aiuti = sum(resto$quota_aiuti),
                   quota_pil = sum(resto$quota_pil))) %>%
  mutate(paese = factor(paese, levels = rev(paese)))

write_csv(dati %>% mutate(across(where(is.numeric), ~ round(.x, 1))),
          file.path(output_dir, "aiuti_di_stato_crisi.csv"))

# --- 2) Grafico -------------------------------------------------------------

fmt_pct <- function(v) paste0(formatC(v, format = "f", digits = 1,
                                      decimal.mark = ","), "%")

# Etichette di lettura appoggiate alla prima barra, al posto della legenda.
prima <- dati %>% slice(1)
legenda <- tibble(
  paese  = factor(rep(prima$paese, 2), levels = levels(dati$paese)),
  x      = c(prima$quota_aiuti - 0.6, prima$quota_pil - 0.6),
  testo  = c("Quota degli aiuti", "Quota del Pil"),
  colore = c(COL_ROSSO, COL_NERO)
)

# Il valore sta dentro la barra solo quando c'è spazio, altrimenti va di
# fianco, oltre il punto che segna il peso economico del paese.
etichette <- dati %>%
  mutate(dentro = quota_aiuti >= 8,
         x = if_else(dentro, quota_aiuti - 0.6,
                     pmax(quota_aiuti, quota_pil) + 0.7),
         testo = fmt_pct(quota_aiuti))

p <- ggplot(dati, aes(y = paese)) +
  geom_col(aes(x = quota_aiuti), fill = COL_ROSSO, width = 0.62) +
  geom_point(aes(x = quota_pil), size = 2.4, colour = COL_NERO) +
  geom_text(data = filter(etichette, dentro),
            aes(x = x, label = testo), hjust = 1, colour = "white",
            fontface = "bold", size = 3.1, family = "Source Sans Pro") +
  geom_text(data = filter(etichette, !dentro),
            aes(x = x, label = testo), hjust = 0, colour = COL_NERO,
            fontface = "bold", size = 3.1, family = "Source Sans Pro") +
  geom_text(data = legenda, aes(x = x, label = testo),
            colour = legenda$colore, fontface = "bold", size = 3.2,
            family = "Source Sans Pro", hjust = 1, vjust = -1.5) +
  scale_x_continuous(limits = c(0, 37), expand = c(0.005, 0.005)) +
  scale_y_discrete(drop = FALSE) +
  coord_cartesian(clip = "off") +
  theme_barchart() +
  labs(
    title = "Dove sono andati gli aiuti di Stato europei per la crisi",
    subtitle = paste0(
      "Quota di ogni paese sui ", round(TOTALE_MLD), " miliardi di aiuti di ",
      "Stato d’emergenza per la pandemia e per l’industria pulita,\n",
      "erogati fra il ", ANNO_DA, " e il ", ANNO_A, ", e peso dello stesso ",
      "paese sul Pil dell’Unione"
    ),
    caption = CAP_COMM
  )

ggsave(file.path(output_dir, "aiuti_di_stato_crisi.png"), plot = p,
       width = 9, height = 5.9, dpi = 220, bg = "white")

# --- 3) Sanity check --------------------------------------------------------

cat("\nAiuti per la crisi", ANNO_DA, "-", ANNO_A, ":",
    round(TOTALE_MLD), "miliardi\n\n")
print(as.data.frame(dati %>%
  mutate(miliardi = round(aiuti / 1000),
         across(c(quota_aiuti, quota_pil), ~ round(.x, 1))) %>%
  select(paese, miliardi, quota_aiuti, quota_pil)))

quota_germania <- dati$quota_aiuti[dati$paese == "Germania"]
cat("\nQuota tedesca:", round(quota_germania, 1),
    "% degli aiuti contro il", round(dati$quota_pil[dati$paese == "Germania"], 1),
    "% del Pil\n")

# Le quote devono chiudere a cento e la Germania deve stare sopra il suo peso.
stopifnot(abs(sum(dati$quota_aiuti) - 100) < 0.01)
stopifnot(abs(sum(dati$quota_pil) - 100) < 0.5)
stopifnot(quota_germania > 33)
