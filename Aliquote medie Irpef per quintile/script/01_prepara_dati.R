library(tidyverse)
library(readxl)

# Aliquote medie effettive per quintile di reddito complessivo, anni d'imposta
# 2002-2024, dalle statistiche MEF sulle dichiarazioni delle persone fisiche.
#
# Fonti (Dipartimento delle Finanze):
# - a.i. 2002-2007: Excel per classi di reddito, archivio studi_statNewDf
#   (tab_1_6_2 = calcolo Irpef, tab_1_7_2 = addizionali)
# - a.i. 2008-2010: open data, tree=<dich>AAPFTOT0302 (calcolo Irpef) e 0802
#   (addizionali), classi di reddito complessivo
# - a.i. 2011-2024: open data, tree=<dich>AAPFTOT0202 (analisi personalizzata
#   per classi) e 0203 (stessa analisi per ventili, usata come controllo)
#
# Nei file open data l'anno nel nome è quello di dichiarazione (= a.i. + 1).
# Ammontari in migliaia di euro.

IN_OD <- "../input/opendata"
IN_XL <- "../input/excel_2002_2007"
OUT   <- "../output"

# --- Utility -----------------------------------------------------------------

num_it <- function(x) {
  x <- str_trim(as.character(x))
  x[x %in% c("", "-", "***")] <- NA
  as.numeric(str_replace_all(str_replace_all(x, "\\.", ""), ",", "."))
}

# Estremi della classe dall'etichetta ("da 1.000 a 1.500", "oltre 300.000"...)
estremi_classe <- function(lab) {
  lab <- str_to_lower(str_squish(lab))
  n <- str_extract_all(str_replace(lab, "a zero$", "a 0"), "-?[0-9][0-9.]*") |>
    map(~ as.numeric(str_remove_all(.x, "\\.")))
  tibble(
    lo = map2_dbl(lab, n, \(l, v) case_when(
      l == "zero" ~ 0,
      str_starts(l, "fino a|minore di") ~ -Inf,
      str_starts(l, "oltre") ~ v[1],
      TRUE ~ v[1])),
    hi = map2_dbl(lab, n, \(l, v) case_when(
      l == "zero" ~ 0,
      str_starts(l, "fino a|minore di") ~ v[1],
      str_starts(l, "oltre") ~ Inf,
      TRUE ~ v[2]))
  )
}

leggi_csv_mef <- function(f) {
  raw <- readBin(f, "raw", file.size(f))
  txt <- rawToChar(raw)
  Encoding(txt) <- "UTF-8"
  if (!validUTF8(txt)) txt <- iconv(rawToChar(raw), "CP1252", "UTF-8")
  txt <- sub("^﻿", "", txt)
  righe <- str_split(txt, "\r?\n")[[1]]
  hr <- which(str_detect(righe, "^(Classi di reddito|Ventili di reddito)"))[1]
  corpo <- righe[hr:length(righe)]
  corpo <- corpo[seq_len(which(str_detect(corpo, "^TOTALE"))[1])]
  df <- read_delim(I(paste(corpo, collapse = "\n")), delim = ";",
                   col_types = cols(.default = col_character()),
                   name_repair = "minimal", progress = FALSE)
  df[, names(df) != ""]
}

# Colonna "Variabile - Ammontare" (NA se la variabile non esiste nel file)
amm <- function(df, var) {
  col <- paste0(var, " - Ammontare")
  if (col %in% names(df)) num_it(df[[col]]) else rep(NA_real_, nrow(df))
}

# --- Open data per classi (a.i. 2008-2024) -----------------------------------

classi_od <- function(dich) {
  if (dich >= 2012) {
    a <- leggi_csv_mef(file.path(IN_OD, sprintf("t0202_%d.csv", dich)))
    b <- a
  } else {
    a <- leggi_csv_mef(file.path(IN_OD, sprintf("t0302_%d.csv", dich)))
    b <- leggi_csv_mef(file.path(IN_OD, sprintf("t0802_%d.csv", dich)))
    stopifnot(identical(a[[1]], b[[1]]))
  }
  tibble(
    anno       = dich - 1,
    classe     = a[[1]],
    n          = num_it(a[["Numero contribuenti"]]),
    reddito    = amm(a, "Reddito complessivo"),
    irpef      = amm(a, "Imposta netta"),
    add_reg    = amm(b, "Addizionale regionale dovuta"),
    add_com    = amm(b, "Addizionale comunale dovuta"),
    cedolare   = amm(a, "Totale imposta cedolare secca"),
    bonus      = amm(a, "Bonus spettante"),          # bonus 80 euro, a.i. 2014-2020
    trattamento = amm(a, "Trattamento spettante")    # trattamento integrativo, dal 2020
  )
}

# --- Excel per classi (a.i. 2002-2007) ---------------------------------------

# Ogni foglio: riga "CLASSI DI REDDITO..." con i nomi delle variabili (celle
# unite, quindi da propagare a destra) e sotto la riga Frequenza/Ammontare/Media.
leggi_foglio_xls <- function(f, sheet) {
  d <- read_excel(f, sheet = sheet, col_names = FALSE, .name_repair = "minimal")
  d <- as.data.frame(d)
  hr <- which(str_detect(as.character(d[[1]]), "^CLASSI"))[1]
  var <- as.character(unlist(d[hr, ]))
  sub <- as.character(unlist(d[hr + 1, ]))
  var <- str_squish(zoo_fill(var))
  nomi <- ifelse(is.na(sub), var, paste(var, sub, sep = " - "))
  nomi[1] <- "classe"
  corpo <- d[(hr + 2):nrow(d), , drop = FALSE]
  names(corpo) <- make.unique(nomi)
  corpo <- corpo[!is.na(corpo$classe), ]
  corpo <- corpo[seq_len(which(corpo$classe == "TOTALE")[1]), ]
  corpo$classe <- str_squish(corpo$classe)
  corpo
}

zoo_fill <- function(x) {
  for (i in seq_along(x)) if (i > 1 && is.na(x[i])) x[i] <- x[i - 1]
  x
}

leggi_xls <- function(f) {
  fogli <- excel_sheets(f)
  tab <- map(fogli, ~ leggi_foglio_xls(f, .x))
  classi <- tab[[1]]$classe
  walk(tab, ~ stopifnot(identical(.x$classe, classi)))
  # unisce i fogli tenendo la prima occorrenza di ogni colonna
  out <- tab[[1]]
  for (t in tab[-1]) out <- bind_cols(out, t[, setdiff(names(t), names(out)), drop = FALSE])
  out
}

val_xls <- function(df, var, sub = "Ammontare") {
  col <- names(df)[str_detect(str_to_lower(names(df)),
                              fixed(str_to_lower(paste(var, sub, sep = " - "))))]
  if (length(col) == 0) return(rep(NA_real_, nrow(df)))
  as.numeric(df[[col[1]]])
}

classi_xls <- function(anno) {
  f_calc <- list.files(IN_XL, pattern = sprintf("^%d_.*tab_1_6_2", anno), full.names = TRUE)
  f_add  <- list.files(IN_XL, pattern = sprintf("^%d_.*tab_1_7_2", anno), full.names = TRUE)
  a <- leggi_xls(f_calc)
  b <- leggi_xls(f_add)
  stopifnot(identical(a$classe, b$classe))
  n_col <- names(a)[str_detect(names(a), "^Numero di contribuenti")][1]
  tibble(
    anno       = anno,
    classe     = a$classe,
    n          = as.numeric(a[[n_col]]),
    reddito    = val_xls(a, "Reddito complessivo"),
    irpef      = val_xls(a, "Imposta netta"),
    add_reg    = val_xls(b, "Addizionale regionale dovuta"),
    add_com    = val_xls(b, "Addizionale comunale dovuta"),
    cedolare   = NA_real_, bonus = NA_real_, trattamento = NA_real_
  )
}

# --- Unione ------------------------------------------------------------------

classi <- bind_rows(
  map(2002:2007, classi_xls),
  map(2009:2025, classi_od)
) |>
  mutate(across(c(n, reddito, irpef, add_reg, add_com, cedolare, bonus, trattamento),
                ~ if_else(is.na(.x) & classe != "TOTALE", 0, .x)))

# Controllo: la somma delle classi coincide con la riga TOTALE. Il segreto
# statistico ("***") oscura a volte l'imposta della classe "zero" e quindi il
# totale: lì il confronto salta (NA) e l'imposta della classe è posta a 0.
scarti <- classi |>
  group_by(anno) |>
  summarise(across(c(n, reddito, irpef, add_reg, add_com),
                   ~ sum(.x[classe != "TOTALE"]) / .x[classe == "TOTALE"] - 1))
cat("Scarto massimo classi vs totale:",
    max(abs(as.matrix(scarti[-1])[is.finite(as.matrix(scarti[-1]))])), "\n")

classi <- classi |>
  filter(classe != "TOTALE") |>
  bind_cols(estremi_classe(classi$classe[classi$classe != "TOTALE"])) |>
  mutate(imposte = irpef + add_reg + add_com + cedolare - bonus - trattamento)

stopifnot(!any(is.na(classi$lo)), !any(is.na(classi$hi)))

# --- Ventili (a.i. 2011-2024), per il controllo ------------------------------

ventili <- map(2012:2025, \(dich) {
  a <- leggi_csv_mef(file.path(IN_OD, sprintf("t0203_%d.csv", dich)))
  tibble(
    anno = dich - 1,
    ventile = seq_len(nrow(a) - 1),
    soglia = num_it(a[["Reddito complessivo all'estremo del ventile in euro"]])[-nrow(a)],
    n = num_it(a[["Reddito complessivo - Frequenza"]])[-nrow(a)],
    reddito = amm(a, "Reddito complessivo")[-nrow(a)],
    irpef = amm(a, "Imposta netta")[-nrow(a)],
    add_reg = amm(a, "Addizionale regionale dovuta")[-nrow(a)],
    add_com = amm(a, "Addizionale comunale dovuta")[-nrow(a)],
    cedolare = amm(a, "Totale imposta cedolare secca")[-nrow(a)],
    bonus = amm(a, "Bonus spettante")[-nrow(a)],
    trattamento = amm(a, "Trattamento spettante")[-nrow(a)]
  )
}) |>
  list_rbind() |>
  mutate(across(reddito:trattamento, ~ replace_na(.x, 0)),
         imposte = irpef + add_reg + add_com + cedolare - bonus - trattamento)

write_csv(classi, file.path(OUT, "classi_reddito_2002_2024.csv"))
write_csv(ventili, file.path(OUT, "ventili_2011_2024.csv"))

cat("Anni:", paste(range(classi$anno), collapse = "-"),
    "| mancanti:", setdiff(2002:2024, unique(classi$anno)), "\n")
