# script/prep_markup.R — eseguito da `cd script && Rscript prep_markup.R`
#
# Ricostruisce la serie storica dei markup delle imprese quotate americane di
# De Loecker, Eeckhout e Unger, "The Rise of Market Power and the Macroeconomic
# Implications", Quarterly Journal of Economics 2020 (doi 10.1093/qje/qjz041).
#
# Input: i due file originali della replication package distribuita da Jan
# Eeckhout (https://www.janeeckhout.com/wp-content/uploads/Replication_DLEU.zip),
# cartella 02_indata/QJE_files:
#   data_main_upd_trim_1.dta   dati di bilancio Compustat già puliti (166 MB)
#   theta_W_s_window.dta       elasticità di output stimate per settore e anno
# Sono troppo pesanti per il repository: vanno scaricati e scompattati a parte,
# poi si indica la cartella con la variabile d'ambiente DLEU_DIR.
#
# La pipeline replica 01_code/01_Steps/03_Create_Temp.do:
#   costshare1 = cogs/(cogs+costo del capitale), trim all'1° e 99° percentile
#   annuale (prima su costshare1, poi su costshare2 nel campione già tagliato)
#   mu_5 = theta_WI1_ct * (vendite/costo del venduto)
#   aggregato = somma ponderata per quote di vendite (MARKUP5_AGG)
#   percentili = markup dell'impresa che chiude il 50°, 75° e 90° percentile
#   della distribuzione ordinata per markup e pesata per vendite, come nel
#   blocco "Markup distribution by market share" dello stesso do-file.
#
# Output: input/markup_dle_us_serie.csv (una riga per anno).

suppressPackageStartupMessages({
  library(tidyverse)
  library(haven)
})

input_dir <- file.path("..", "input")

dleu_dir <- Sys.getenv("DLEU_DIR", unset = "")
if (dleu_dir == "") {
  candidati <- c(
    file.path(input_dir, "Replication_DLEU", "02_indata", "QJE_files")
  )
  trovati <- candidati[file.exists(file.path(candidati, "data_main_upd_trim_1.dta"))]
  if (length(trovati) == 0) {
    stop("Dati DLEU non trovati. Scarica Replication_DLEU.zip da janeeckhout.com, ",
         "scompattalo e indica la cartella QJE_files con DLEU_DIR.")
  }
  dleu_dir <- trovati[1]
}
cat("Dati letti da:", dleu_dir, "\n")

# --- 1) Campione ------------------------------------------------------------

grezzi <- read_dta(file.path(dleu_dir, "data_main_upd_trim_1.dta"),
                   col_select = c(gvkey, year, ind2d, sale_D, cogs_D,
                                  xsga_D, kexp))
cat("Osservazioni lette:", nrow(grezzi), "\n")

# Percentili alla maniera di Stata (egen pctile): quantile di tipo 2.
pctile_stata <- function(x, p) {
  as.numeric(quantile(x, probs = p, type = 2, na.rm = TRUE))
}

campione <- grezzi %>%
  filter(!is.na(gvkey)) %>%
  mutate(costshare1 = cogs_D / (cogs_D + kexp),
         costshare2 = cogs_D / (cogs_D + xsga_D + kexp))

# Il do-file taglia prima su costshare1 e poi su costshare2, e i percentili
# della seconda variabile sono calcolati sul campione già ridotto.
for (v in c("costshare1", "costshare2")) {
  campione <- campione %>%
    filter(!is.na(.data[[v]]), .data[[v]] != 0) %>%
    group_by(year) %>%
    filter(.data[[v]] <= pctile_stata(.data[[v]], 0.99),
           .data[[v]] >= pctile_stata(.data[[v]], 0.01)) %>%
    ungroup()
}
cat("Osservazioni dopo il trim:", nrow(campione), "\n")

theta <- read_dta(file.path(dleu_dir, "theta_W_s_window.dta")) %>%
  select(year, ind2d, theta_WI1_ct)

imprese <- campione %>%
  inner_join(theta, by = c("year", "ind2d")) %>%
  filter(!is.na(sale_D), !is.na(cogs_D), cogs_D > 0) %>%
  mutate(mu = theta_WI1_ct * (sale_D / cogs_D)) %>%
  filter(!is.na(mu)) %>%
  group_by(year) %>%
  mutate(quota = sale_D / sum(sale_D)) %>%
  ungroup()

# --- 2) Aggregato, percentili pesati, mediana semplice ----------------------

# Percentile "per quota di mercato": si ordinano le imprese per markup
# crescente e si prende il markup dell'ultima impresa prima che le vendite
# cumulate superino la soglia.
pctile_vendite <- function(mu, quota, soglia) {
  ord <- order(mu)
  cum <- cumsum(quota[ord])
  sotto <- cum < soglia
  if (!any(sotto)) return(min(mu))
  max(mu[ord][sotto])
}

serie <- imprese %>%
  group_by(year) %>%
  summarise(
    markup_aggregato = sum(quota * mu),
    p50_vendite = pctile_vendite(mu, quota, 0.50),
    p75_vendite = pctile_vendite(mu, quota, 0.75),
    p90_vendite = pctile_vendite(mu, quota, 0.90),
    p50_imprese = median(mu),
    p75_imprese = pctile_stata(mu, 0.75),
    p90_imprese = pctile_stata(mu, 0.90),
    n_imprese = n(),
    .groups = "drop"
  ) %>%
  arrange(year)

write_csv(serie %>% mutate(across(-c(year, n_imprese), ~ round(.x, 4))),
          file.path(input_dir, "markup_dle_us_serie.csv"))

# --- 3) Controlli -----------------------------------------------------------

cat("\nSerie ricostruita, anni chiave:\n")
print(as.data.frame(serie %>%
  filter(year %in% c(1955, 1960, 1980, 2000, 2016)) %>%
  mutate(across(-c(year, n_imprese), ~ round(.x, 3)))))

# Confronto con la serie aggregata già verificata sui valori pubblicati
# (1980 = 1,21 e 2016 = 1,61 nel paper).
riferimento <- read_csv(file.path(input_dir, "markup_dle_us_qje_fig1.csv"),
                        col_types = cols()) %>%
  select(year, markup_rif = markup)

confronto <- serie %>%
  inner_join(riferimento, by = "year") %>%
  mutate(scarto = markup_aggregato - markup_rif)

cat("\nScarto massimo dall'aggregato di riferimento:",
    round(max(abs(confronto$scarto)), 4), "\n")
cat("Anni coperti:", min(serie$year), "-", max(serie$year), "\n")

stopifnot(nrow(serie) > 55)
stopifnot(max(abs(confronto$scarto)) < 0.01)
stopifnot(all(serie$p90_vendite > serie$p50_vendite))
