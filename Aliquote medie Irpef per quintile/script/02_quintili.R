library(tidyverse)

# Aliquota media = imposte / reddito complessivo per gruppi di contribuenti
# ordinati per reddito complessivo (quintili, 5% e 1% più ricchi).
# Imposte = Irpef netta + addizionali regionale e comunale + cedolare secca
#           - bonus 80 euro - trattamento integrativo.
# I gruppi si ricavano dalle classi di reddito: quando un confine cade dentro
# una classe, la classe si divide assumendo i contribuenti distribuiti in modo
# uniforme tra i suoi estremi (con la media vera della classe); le imposte
# della parte seguono il reddito della parte. Il risultato si confronta con i
# ventili pubblicati dal MEF dal 2011, che danno quintili e 5% esatti.

OUT <- "../output"

classi  <- read_csv(file.path(OUT, "classi_reddito_2002_2024.csv"), show_col_types = FALSE)
ventili <- read_csv(file.path(OUT, "ventili_2011_2024.csv"), show_col_types = FALSE)

gruppi <- tribble(
  ~gruppo,           ~p0,  ~p1,
  "Primo quintile",   0.0,  0.2,
  "Secondo quintile", 0.2,  0.4,
  "Terzo quintile",   0.4,  0.6,
  "Quarto quintile",  0.6,  0.8,
  "Quinto quintile",  0.8,  1.0,
  "5% più ricco",     0.95, 1.0,
  "1% più ricco",     0.99, 1.0
)

# Somma di reddito e imposte per i contribuenti tra i percentili p0 e p1
somma_gruppo <- function(cl, p0, p1) {
  cl <- arrange(cl, lo, hi)
  N <- sum(cl$n)
  cum1 <- cumsum(cl$n) / N
  cum0 <- lag(cum1, default = 0)
  # quota di ciascuna classe (in frazione della classe, dal basso) nel gruppo
  u0 <- pmin(pmax((p0 - cum0) / (cum1 - cum0), 0), 1)
  u1 <- pmin(pmax((p1 - cum0) / (cum1 - cum0), 0), 1)
  u0[!is.finite(u0)] <- 0; u1[!is.finite(u1)] <- 0
  quota_n <- u1 - u0
  media <- cl$reddito / cl$n
  larg <- ifelse(is.finite(cl$lo) & is.finite(cl$hi), (cl$hi - cl$lo) / 1000, 0)
  media_tra <- function(a, b) media + ((a + b) / 2 - 0.5) * larg
  red_parte <- cl$n * quota_n * media_tra(u0, u1)
  # Imposta pro capite in funzione del reddito: spezzata che passa per le
  # medie (reddito, imposta) delle classi. Dentro la classe le imposte si
  # ripartiscono tra la parte nel gruppo e il resto in proporzione a n * T(media)
  ok <- cl$n > 0
  T_red <- function(x) approx(media[ok], (cl$imposte / cl$n)[ok], x, rule = 2, ties = mean)$y
  w_parte <- quota_n * pmax(T_red(media_tra(u0, u1)), 0)
  w_sotto <- u0 * pmax(T_red(media_tra(0, u0)), 0)
  w_sopra <- (1 - u1) * pmax(T_red(media_tra(u1, 1)), 0)
  quota_imp <- ifelse(larg > 0 & (w_parte + w_sotto + w_sopra) > 0,
                      w_parte / (w_parte + w_sotto + w_sopra), quota_n)
  imp_parte <- cl$imposte * quota_imp
  tibble(reddito = sum(red_parte), imposte = sum(imp_parte))
}

stima_classi <- function(cl) {
  cl |>
    group_by(anno) |>
    group_modify(\(d, key) {
      pmap(gruppi, \(gruppo, p0, p1) somma_gruppo(d, p0, p1) |> mutate(gruppo = gruppo)) |>
        list_rbind()
    }) |>
    ungroup()
}

# Come fa il CBO, chi dichiara un reddito negativo o nullo conta per fissare i
# confini dei quintili ma resta fuori dal calcolo dell'aliquota del primo
# quintile: le perdite (enormi nel 2017, col passaggio al regime di cassa per
# le imprese in contabilità semplificata) schiaccerebbero il denominatore.
non_positivi <- function(var) {
  classi |>
    filter(hi <= 0) |>
    group_by(anno) |>
    summarise(red_np = sum(reddito), imp_np = sum(.data[[var]]))
}

escludi_non_positivi <- function(df, var) {
  df |>
    left_join(non_positivi(var), by = "anno") |>
    mutate(reddito = if_else(gruppo == "Primo quintile", reddito - red_np, reddito),
           imposte = if_else(gruppo == "Primo quintile", imposte - imp_np, imposte)) |>
    select(-red_np, -imp_np)
}

somme_ventili <- function(v) {
  v |>
    mutate(q = ceiling(ventile / 4)) |>
    group_by(anno, q) |>
    summarise(reddito = sum(reddito), imposte = sum(imposte), .groups = "drop") |>
    mutate(gruppo = gruppi$gruppo[q]) |>
    select(-q) |>
    bind_rows(v |> filter(ventile == 20) |> mutate(gruppo = "5% più ricco") |>
                select(anno, gruppo, reddito, imposte))
}

calcola <- function(var) {
  stima <- stima_classi(mutate(classi, imposte = .data[[var]])) |>
    escludi_non_positivi(var) |>
    transmute(anno, gruppo, reddito, imposte, stima = 100 * imposte / reddito)
  esatti <- somme_ventili(mutate(ventili, imposte = .data[[var]])) |>
    escludi_non_positivi(var) |>
    transmute(anno, gruppo, esatto = 100 * imposte / reddito)
  left_join(stima, esatti, by = c("anno", "gruppo"))
}

tutte <- calcola("imposte")
irpef <- calcola("irpef")

# --- Controllo con i ventili (valori esatti) ----------------------------------

cat("\nScarto stima da classi - ventili esatti (punti percentuali):\n")
tutte |>
  filter(!is.na(esatto)) |>
  group_by(gruppo) |>
  summarise(max_abs = max(abs(stima - esatto)), medio = mean(stima - esatto)) |>
  print()

# --- Output ------------------------------------------------------------------

# Valore finale: ventili esatti quando ci sono (quintili e 5%, dal 2011),
# altrimenti la stima dalle classi (2002-2010 e sempre per l'1%)
aliquote <- tutte |>
  left_join(irpef |> transmute(anno, gruppo,
                               aliquota_solo_irpef = coalesce(esatto, stima)),
            by = c("anno", "gruppo")) |>
  mutate(aliquota = coalesce(esatto, stima),
         fonte = if_else(is.na(esatto), "classi", "ventili"),
         gruppo = factor(gruppo, levels = gruppi$gruppo)) |>
  select(anno, gruppo, aliquota, fonte, aliquota_stima_classi = stima,
         aliquota_ventili = esatto, aliquota_solo_irpef) |>
  arrange(anno, gruppo)

write_csv(aliquote, file.path(OUT, "aliquote_medie_2002_2024.csv"))

cat("\nAliquote medie (imposte / reddito complessivo), %:\n")
aliquote |>
  filter(anno %in% c(2002, 2007, 2010, 2011, 2013, 2019, 2024)) |>
  select(anno, gruppo, aliquota) |>
  pivot_wider(names_from = anno, values_from = aliquota) |>
  mutate(across(where(is.numeric), ~ round(.x, 1))) |>
  print()
