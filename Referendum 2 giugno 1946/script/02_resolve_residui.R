suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(stringi)
  library(tidyr)
  library(purrr)
})

BASE <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Referendum 2 giugno 1946"
w <- readRDS(file.path(BASE, "output", "step1_workspace.rds"))

referendum    <- w$referendum
com91         <- w$com91
com91_lookup  <- w$com91_lookup
soppressioni  <- w$soppressioni
denominazioni <- w$denominazioni
variazioni91  <- w$variazioni91
step1         <- w$step1
residui       <- w$residui
prov_map      <- w$prov_map

norm <- function(x) {
  x %>%
    str_to_upper() %>%
    str_replace_all("[’`‘′]", "'") %>%
    str_replace_all("\\bA'(?=\\s|$|-)", "À") %>%
    str_replace_all("\\bE'(?=\\s|$|-)", "È") %>%
    str_replace_all("\\bI'(?=\\s|$|-)", "Ì") %>%
    str_replace_all("\\bO'(?=\\s|$|-)", "Ò") %>%
    str_replace_all("\\bU'(?=\\s|$|-)", "Ù") %>%
    stri_trans_general("Latin-ASCII") %>%
    str_replace_all("J", "I") %>%
    str_replace_all("[^A-Z0-9 ]+", " ") %>%
    str_squish()
}
nospc <- function(x) str_replace_all(x, " ", "")

# Province 1991 candidate per ogni provincia 1946 (vettore numerico)
prov_candidati <- prov_map %>%
  group_by(PROVINCIA_1946) %>%
  summarise(COD_LIST = list(COD_PROV_1991), .groups = "drop")

# ---- Mappa codice_corrente → vettore di codici 1991 --------------------------

codici_1991 <- com91$PRO_COM_T

var <- variazioni91 %>%
  rename(TIPO = `Tipo variazione`,
         COD = `Codice Comune`,
         COD_ASS = `Codice Comune associato`,
         ANNO = `Anno evento`) %>%
  mutate(across(c(COD, COD_ASS), as.character)) %>%
  arrange(ANNO)

cache <- new.env(parent = emptyenv())

expand_to_1991 <- function(c, depth = 0) {
  if (is.null(c) || is.na(c) || c == "") return(character(0))
  if (depth > 10) return(character(0))
  if (!is.null(cache[[c]])) return(cache[[c]])
  if (c %in% codici_1991) { cache[[c]] <- c; return(c) }
  rows <- var %>% filter(COD == c,
                         TIPO %in% c("CS-Costituzione",
                                     "CECS-Cessione territorio per costituzione nuova unità",
                                     "RN-Rinumerazione del codice statistico",
                                     "AP-Cambio appartenenza Provincia"))
  if (nrow(rows) == 0) { cache[[c]] <- character(0); return(character(0)) }
  out <- unique(unlist(lapply(rows$COD_ASS, expand_to_1991, depth = depth + 1)))
  cache[[c]] <- out
  out
}

# ---- Indici delle tabelle storiche ------------------------------------------

denominazioni <- denominazioni %>%
  rename(DEN_PREC = `Denominazione precedente`,
         COD_PREC = `Codice Comune`,
         COD_CORR = `Codice Comune corrente`,
         DEN_CORR = `Denominazione corrente`,
         COD_PROV_PREC = `Codice Provincia/Uts`,
         COD_PROV_CORR = `Codice Provincia/Uts corrente`,
         SIGLA_PREC = `Sigla provincia/uts`,
         SIGLA_CORR = `Sigla provincia/uts corrente`) %>%
  mutate(across(c(COD_PREC, COD_CORR), as.character),
         NOME_NORM = norm(DEN_PREC))

soppressioni <- soppressioni %>%
  rename(NOME_SOPP = Comune,
         COD_SOPP = `Codice Comune`,
         COD_ASS = `Codice Comune associato`,
         NOME_ASS = `Comune associato`,
         ANNO = `Anno evento`,
         TIPO = `Tipo variazione`,
         FLAG_SCORPORO = `Flag Scorporo`,
         SIGLA = `Sigla Provincia/Uts`,
         COD_PROV_SOPP = `Codice Provincia/Uts`) %>%
  mutate(across(c(COD_SOPP, COD_ASS), as.character),
         NOME_NORM = norm(NOME_SOPP))

# Indici per ricerca veloce nel "fusione fascista":
# - "nome esatto" per (cod_prov, nome_norm)
# - "nome che inizia con" per cod_prov (lo cerco con dplyr a runtime)

# Per ogni residuo: provo a vedere se il nome 1946 è la concatenazione di
# due nomi 1991 nella stessa provincia. Tipo: OLGIATE CALCO -> Olgiate Molgora + Calco.
resolve_fascista <- function(parts, cod_prov_list) {
  if (length(parts) < 2) return(NULL)
  com_prov <- com91 %>%
    filter(COD_PROV %in% cod_prov_list) %>%
    select(COD_PROV, COMUNE, COMUNE_NORM, PRO_COM_T)
  if (nrow(com_prov) == 0) return(NULL)

  # Per ogni split del nome in 2 pezzi, cerco un comune che matcha p1 (esatto
  # o come prima parola) E un comune che matcha p2 (esatto o come ultima parola).
  best <- NULL
  for (i in seq_len(length(parts) - 1)) {
    p1 <- paste(parts[1:i], collapse = " ")
    p2 <- paste(parts[(i+1):length(parts)], collapse = " ")
    # candidato per p1: esatto o "p1 *"
    c1 <- com_prov %>% filter(COMUNE_NORM == p1 |
                              str_starts(COMUNE_NORM, paste0(p1, " ")))
    c2 <- com_prov %>% filter(COMUNE_NORM == p2 |
                              str_ends(COMUNE_NORM, paste0(" ", p2)))
    if (nrow(c1) >= 1 && nrow(c2) >= 1) {
      pick <- bind_rows(c1, c2) %>% distinct(PRO_COM_T, COMUNE)
      if (is.null(best) || nrow(pick) > nrow(best)) best <- pick
    }
  }
  best
}

# ---- Funzione di risoluzione di un singolo residuo --------------------------

resolve_one <- function(prov_46, com_46_norm, cod_prov_list) {
  # 1) Denominazione storica (cerco nello stesso nome storico)
  hits <- denominazioni %>% filter(NOME_NORM == com_46_norm)
  if (nrow(hits) > 0) {
    # disambiguo per provincia (storica o corrente)
    h_match <- hits %>%
      filter(as.integer(COD_PROV_PREC) %in% cod_prov_list |
             as.integer(COD_PROV_CORR) %in% cod_prov_list)
    if (nrow(h_match) > 0) hits <- h_match
    cod_corr <- unique(hits$COD_CORR)
    pro_com <- unique(unlist(lapply(cod_corr, expand_to_1991)))
    pro_com <- intersect(pro_com,
                         com91 %>% filter(COD_PROV %in% cod_prov_list) %>% pull(PRO_COM_T))
    if (length(pro_com) > 0) {
      return(list(metodo = "denominazione_storica",
                  pro_com_1991 = pro_com,
                  note = paste0("nome storico '", hits$DEN_PREC[1],
                                "' -> codice corrente ", paste(cod_corr, collapse=","))))
    }
  }

  # 2) Soppressioni (cerco per nome del soppresso, preferendo anno >=1946)
  hits <- soppressioni %>% filter(NOME_NORM == com_46_norm)
  if (nrow(hits) > 0) {
    h_match <- hits %>%
      filter(as.integer(COD_PROV_SOPP) %in% cod_prov_list)
    if (nrow(h_match) > 0) hits <- h_match
    hits_post <- hits %>% filter(ANNO >= 1946)
    if (nrow(hits_post) > 0) hits <- hits_post
    cod_ass <- unique(hits$COD_ASS)
    pro_com <- unique(unlist(lapply(cod_ass, expand_to_1991)))
    pro_com <- intersect(pro_com,
                         com91 %>% filter(COD_PROV %in% cod_prov_list) %>% pull(PRO_COM_T))
    if (length(pro_com) > 0) {
      return(list(metodo = "soppressione",
                  pro_com_1991 = pro_com,
                  note = paste0("soppresso ", paste(unique(hits$ANNO), collapse="/"),
                                " -> assorbente corrente ", paste(cod_ass, collapse=","))))
    }
  }

  # 3) Variante grafica senza spazi (Sanremo, ecc.)
  com_compact <- nospc(com_46_norm)
  hits <- com91 %>% filter(COD_PROV %in% cod_prov_list,
                           nospc(COMUNE_NORM) == com_compact)
  if (nrow(hits) > 0) {
    return(list(metodo = "variante_grafica",
                pro_com_1991 = hits$PRO_COM_T,
                note = paste("match senza spazi con", paste(hits$COMUNE, collapse=","))))
  }

  # 4) Fusione fascista scissa: nome 1946 = concatenazione di 2+ nomi 1991
  parts <- str_split(com_46_norm, " ")[[1]]
  parts <- parts[nchar(parts) >= 2]
  fasc <- resolve_fascista(parts, cod_prov_list)
  if (!is.null(fasc) && nrow(fasc) >= 2) {
    return(list(metodo = "fusione_fascista_scissa",
                pro_com_1991 = fasc$PRO_COM_T,
                note = paste("aggregazione di", paste(fasc$COMUNE, collapse=" + "))))
  }

  # 5) Approssimato (Jaro-Winkler) sui comuni delle province candidate
  cand <- com91 %>% filter(COD_PROV %in% cod_prov_list)
  if (nrow(cand) > 0) {
    d <- stringdist::stringdist(com_46_norm, cand$COMUNE_NORM, method = "jw")
    best <- which.min(d)
    if (length(best) && d[best] < 0.10) {
      return(list(metodo = "approssimato",
                  pro_com_1991 = cand$PRO_COM_T[best],
                  note = paste0("JW=", round(d[best],3), " -> ", cand$COMUNE[best])))
    }
  }

  list(metodo = "irrisolto", pro_com_1991 = character(0), note = NA_character_)
}

# ---- Itero sui residui --------------------------------------------------------

residui_with_cands <- residui %>%
  select(CIRCOSCRIZIONE, PROVINCIA, COMUNE, COMUNE_NORM) %>%
  distinct() %>%
  left_join(prov_candidati, by = c("PROVINCIA" = "PROVINCIA_1946"))

cat("Risoluzione residui:", nrow(residui_with_cands), "\n")

residui_res <- residui_with_cands %>%
  rowwise() %>%
  mutate(
    .res = list(resolve_one(PROVINCIA, COMUNE_NORM, COD_LIST))
  ) %>%
  ungroup() %>%
  mutate(
    METODO = map_chr(.res, "metodo"),
    PRO_COM_1991_LIST = map(.res, "pro_com_1991"),
    NOTE = map_chr(.res, "note")
  ) %>%
  select(-.res, -COD_LIST)

cat("\nDistribuzione metodi step2:\n")
print(residui_res %>% count(METODO))

cat("\nIrrisolti:\n")
print(residui_res %>% filter(METODO == "irrisolto") %>%
      select(PROVINCIA, COMUNE), n = 100)

cat("\nApprossimati (controllare):\n")
print(residui_res %>% filter(METODO == "approssimato") %>%
      select(PROVINCIA, COMUNE, NOTE), n = 50)

cat("\nFusione fascista scissa (controllare):\n")
print(residui_res %>% filter(METODO == "fusione_fascista_scissa") %>%
      select(PROVINCIA, COMUNE, NOTE), n = 50)

cat("\nSoppressione (controllare):\n")
print(residui_res %>% filter(METODO == "soppressione") %>%
      select(PROVINCIA, COMUNE, NOTE), n = 50)

cat("\nDenominazione storica (controllare):\n")
print(residui_res %>% filter(METODO == "denominazione_storica") %>%
      select(PROVINCIA, COMUNE, NOTE), n = 50)

cat("\nVariante grafica (controllare):\n")
print(residui_res %>% filter(METODO == "variante_grafica") %>%
      select(PROVINCIA, COMUNE, NOTE), n = 50)

saveRDS(residui_res, file.path(BASE, "output", "step2_residui_res.rds"))
