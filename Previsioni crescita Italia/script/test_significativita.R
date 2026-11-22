# Test di significatività sugli errori di previsione del Pil (anni bersaglio 1999-2025 senza 2008-2009, 2012-2013 e 2020-2021)
# 1. Distorsione: l'errore medio di ciascun ente è diverso da zero? (t-test e test dei segni)
# 2. Ottimismo relativo: la previsione del governo è sistematicamente più alta? (differenza tra previsioni)
# 3. Accuratezza relativa: Diebold-Mariano con correzione per piccoli campioni (Harvey, Leybourne, Newbold 1997)

library(tidyverse)

BASE <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Previsioni crescita Italia"
OUT  <- file.path(BASE, "output")

ANNI <- setdiff(1999:2025, c(2008, 2009, 2012, 2013, 2020, 2021))
ORIZ <- c("t, primavera", "t, autunno", "t+1, primavera", "t+1, autunno", "t+2, primavera", "t+2, autunno")

err <- read_csv(file.path(OUT, "errori_previsioni.csv"), show_col_types = FALSE) |>
  filter(variabile == "pil_reale", anno %in% ANNI, orizzonte %in% ORIZ)

p_segni <- function(x) { x <- x[x != 0]; if (length(x) == 0) NA else binom.test(sum(x > 0), length(x))$p.value }

# 1. Distorsione
distorsione <- err |>
  group_by(vintage, orizzonte, serie) |>
  summarise(n = n(), errore_medio = mean(errore),
            p_ttest = if (n() > 2 && sd(errore) > 0) t.test(errore)$p.value else NA,
            p_segni = p_segni(errore), .groups = "drop")

# Diebold-Mariano con correzione HLN, perdita quadratica e assoluta, orizzonte h = 1 (una previsione per anno)
dm_hln <- function(e1, e2, perdita = c("quadratica", "assoluta")) {
  perdita <- match.arg(perdita)
  d <- if (perdita == "quadratica") e1^2 - e2^2 else abs(e1) - abs(e2)
  n <- length(d)
  if (n < 4 || sd(d) == 0) return(NA)
  stat <- mean(d) / sqrt(var(d) / n)
  2 * pt(-abs(stat), df = n - 1)
}

gov <- c("Governo (programmatico)", "Governo (tendenziale)")
coppie <- err |>
  filter(serie %in% gov) |>
  inner_join(err |> filter(!serie %in% gov),
             by = c("vintage", "orizzonte", "anno", "effettivo"), suffix = c("_gov", "_altro"))

confronti <- coppie |>
  group_by(vintage, orizzonte, governo = serie_gov, ente = serie_altro) |>
  summarise(
    n = n(),
    anni = paste(min(anno), max(anno), sep = "-"),
    # 2. ottimismo relativo
    scarto_previsioni = mean(previsione_gov - previsione_altro),
    gov_sopra = sum(previsione_gov > previsione_altro),
    gov_sotto = sum(previsione_gov < previsione_altro),
    p_scarto_ttest = if (n() > 2 && sd(previsione_gov - previsione_altro) > 0) t.test(previsione_gov - previsione_altro)$p.value else NA,
    p_scarto_segni = p_segni(previsione_gov - previsione_altro),
    # 3. accuratezza relativa
    mae_gov = mean(abs(errore_gov)), mae_altro = mean(abs(errore_altro)),
    p_dm_quadratica = dm_hln(errore_gov, errore_altro, "quadratica"),
    p_dm_assoluta = dm_hln(errore_gov, errore_altro, "assoluta"),
    .groups = "drop")

write_csv(distorsione, file.path(OUT, "test_distorsione.csv"))
write_csv(confronti, file.path(OUT, "test_governo_contro_enti.csv"))
