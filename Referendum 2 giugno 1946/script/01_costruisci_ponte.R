#' Costruisce il CSV ponte fra i comuni del referendum 1946 (Min. Interno)
#' e i comuni dei confini ISTAT 1991.
#'
#' Strategia di matching, in ordine:
#'   1) Esatto su (provincia 1991, nome normalizzato)  +  variante tedesca
#'   2) Senza spazi (Sanremo / San Remo, Montecompatri, ecc.)
#'   3) Prefisso: il nome 1946 corrisponde all'inizio di un comune 1991
#'      (BORGO -> Borgo Valsugana, MONTICELLO -> Monticello Brianza)
#'   4) Denominazione storica ISTAT (con risoluzione del codice corrente -> 1991
#'      tramite le variazioni dal 1991, incluse AP/RN/CS)
#'   5) Soppressione ISTAT (anno >= 1946 preferito)
#'   6) Fusione fascista scissa: nome 1946 = concatenazione di 2+ nomi 1991
#'   7) Approssimato (Jaro-Winkler) sui comuni delle province candidate
#'   8) Override manuale (tabella esplicita in fondo)
#'   9) Irrisolto

suppressPackageStartupMessages({
  library(sf)
  library(readr)
  library(readxl)
  library(dplyr)
  library(stringr)
  library(stringi)
  library(tidyr)
  library(purrr)
})

BASE <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Referendum 2 giugno 1946"

# ---- 1) Lettura dati ---------------------------------------------------------

referendum <- read_delim(
  file.path(BASE, "referendum-19460602.txt"),
  delim = ";", show_col_types = FALSE,
  locale = locale(encoding = "Latin1")
)

com91_shp <- st_read(
  file.path(BASE, "Limiti1991_g", "Com1991_g", "Com1991_g_WGS84.shp"),
  quiet = TRUE
)
com91 <- st_drop_geometry(com91_shp)

elenco91 <- read_excel(
  file.path(BASE, "Limiti1991_g", "ElencoUnitaAmministrative1991.xls"),
  sheet = "RipRegProv1991"
)

soppressioni  <- read_excel(file.path(BASE, "Elenco comuni soppressi e non ricostituiti Data Indagine 17-03-1861 Stampa 11052026112716.xlsx"))
denominazioni <- read_excel(file.path(BASE, "Elenco delle denominazioni precedenti Data Indagine 17-03-1861 Stampa 11052026112650.xlsx"))
variazioni91  <- read_excel(file.path(BASE, "Variazioni amministrative e territoriali dei comuni dal 1991 Data Indagine 01-01-1991 Stampa 11052026112736.xlsx"))

# ---- 2) Normalizzazione ------------------------------------------------------

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

# Variante senza preposizioni (per Costa DI Monticelli, Belvedere DI Spinello,
# Cassano ALLO Ionio, Castronovo DI Sicilia, ecc.)
.PREP <- c("DI","DE","DA","DEL","DELLO","DELLA","DELLE","DEI","DEGLI",
           "DAL","DALLO","DALLA","DALLE","DAGLI","DAI",
           "NEL","NELLO","NELLA","NELLE","NEI","NEGLI","NE",
           "AL","ALLO","ALLA","ALLE","AI","AGLI","A",
           "SUL","SULLO","SULLA","SULLE","SUI","SUGLI","SU",
           "CON","IN","E","ED","D")
norm_nopre <- function(x) {
  x %>%
    norm() %>%
    str_split(" ") %>%
    map_chr(~ paste(.x[!(.x %in% .PREP)], collapse = " ")) %>%
    str_squish()
}

referendum <- referendum %>%
  mutate(COMUNE_NORM = norm(COMUNE), PROVINCIA_NORM = norm(PROVINCIA))

com91 <- com91 %>%
  left_join(elenco91 %>% select(COD_PROV, DEN_PROV, SIGLA), by = "COD_PROV") %>%
  mutate(
    COMUNE_NORM    = norm(COMUNE),
    COMUNE_NORM_NP = norm_nopre(COMUNE),
    COMUNE_A_NORM  = ifelse(is.na(COMUNE_A), NA_character_, norm(COMUNE_A)),
    DEN_PROV_NORM  = norm(DEN_PROV)
  )

referendum <- referendum %>%
  mutate(COMUNE_NORM_NP = norm_nopre(COMUNE))

# ---- 3) Mapping province 1946 -> 1991 ---------------------------------------

prov_map <- tribble(
  ~PROVINCIA_1946,   ~COD_PROV_1991, ~DEN_PROV_1991,
  "TORINO",          1L,  "Torino",
  "VERCELLI",        2L,  "Vercelli",
  "NOVARA",          3L,  "Novara",
  "CUNEO",           4L,  "Cuneo",
  "ASTI",            5L,  "Asti",
  "ALESSANDRIA",     6L,  "Alessandria",
  "AOSTA",           7L,  "Valle d'Aosta",
  "IMPERIA",         8L,  "Imperia",
  "SAVONA",          9L,  "Savona",
  "GENOVA",          10L, "Genova",
  "LA SPEZIA",       11L, "La Spezia",
  "VARESE",          12L, "Varese",
  "COMO",            13L, "Como",
  "SONDRIO",         14L, "Sondrio",
  "MILANO",          15L, "Milano",
  "BERGAMO",         16L, "Bergamo",
  "BRESCIA",         17L, "Brescia",
  "PAVIA",           18L, "Pavia",
  "CREMONA",         19L, "Cremona",
  "MANTOVA",         20L, "Mantova",
  "TRENTO",          22L, "Trento",
  "VERONA",          23L, "Verona",
  "VICENZA",         24L, "Vicenza",
  "BELLUNO",         25L, "Belluno",
  "TREVISO",         26L, "Treviso",
  "VENEZIA",         27L, "Venezia",
  "PADOVA",          28L, "Padova",
  "ROVIGO",          29L, "Rovigo",
  "UDINE",           30L, "Udine",
  "UDINE",           93L, "Pordenone",
  "PIACENZA",        33L, "Piacenza",
  "PARMA",           34L, "Parma",
  "REGGIO EMILIA",   35L, "Reggio nell'Emilia",
  "MODENA",          36L, "Modena",
  "BOLOGNA",         37L, "Bologna",
  "FERRARA",         38L, "Ferrara",
  "RAVENNA",         39L, "Ravenna",
  "FORLI'",          40L, "Forli'",
  "PESARO ",         41L, "Pesaro e Urbino",
  "ANCONA",          42L, "Ancona",
  "MACERATA",        43L, "Macerata",
  "ASCOLI PICENO",   44L, "Ascoli Piceno",
  "MASSA CARRARA",   45L, "Massa-Carrara",
  "LUCCA",           46L, "Lucca",
  "PISTOIA",         47L, "Pistoia",
  "FIRENZE",         48L, "Firenze",
  "LIVORNO",         49L, "Livorno",
  "PISA",            50L, "Pisa",
  "AREZZO",          51L, "Arezzo",
  "SIENA",           52L, "Siena",
  "GROSSETO",        53L, "Grosseto",
  "PERUGIA",         54L, "Perugia",
  "TERNI",           55L, "Terni",
  "VITERBO",         56L, "Viterbo",
  "RIETI",           57L, "Rieti",
  "ROMA",            58L, "Roma",
  "LATINA",          59L, "Latina",
  "FROSINONE",       60L, "Frosinone",
  "CASERTA",         61L, "Caserta",
  "BENEVENTO",       62L, "Benevento",
  "NAPOLI",          63L, "Napoli",
  "AVELLINO",        64L, "Avellino",
  "AVELLINO",        62L, "Benevento (per S.Arcangelo Trimonte)",
  "SALERNO",         65L, "Salerno",
  "L'AQUILA",        66L, "L'Aquila",
  "TERAMO",          67L, "Teramo",
  "PESCARA",         68L, "Pescara",
  "CHIETI",          69L, "Chieti",
  "CAMPOBASSO",      70L, "Campobasso",
  "CAMPOBASSO",      94L, "Isernia",
  "FOGGIA",          71L, "Foggia",
  "BARI",            72L, "Bari",
  "TARANTO",         73L, "Taranto",
  "BRINDISI",        74L, "Brindisi",
  "LECCE",           75L, "Lecce",
  "POTENZA",         76L, "Potenza",
  "MATERA",          77L, "Matera",
  "COSENZA",         78L, "Cosenza",
  "CATANZARO",       79L, "Catanzaro",
  "REGGIO CALABRIA", 80L, "Reggio di Calabria",
  "TRAPANI",         81L, "Trapani",
  "TRAPANI",         82L, "Palermo (per Camporeale)",
  "PALERMO",         82L, "Palermo",
  "MESSINA",         83L, "Messina",
  "AGRIGENTO",       84L, "Agrigento",
  "CALTANISSETTA",   85L, "Caltanissetta",
  "ENNA",            86L, "Enna",
  "CATANIA",         87L, "Catania",
  "RAGUSA",          88L, "Ragusa",
  "SIRACUSA",        89L, "Siracusa",
  "SASSARI",         90L, "Sassari",
  "NUORO",           91L, "Nuoro",
  "NUORO",           95L, "Oristano",
  "CAGLIARI",        92L, "Cagliari",
  "CAGLIARI",        95L, "Oristano"
)

prov_candidati <- prov_map %>%
  group_by(PROVINCIA_1946) %>%
  summarise(COD_LIST = list(COD_PROV_1991), .groups = "drop")

# ---- 4) Mappa codice corrente -> codici 1991 (gestisce CS, AP, RN) ---------

codici_1991 <- com91$PRO_COM_T

var <- variazioni91 %>%
  rename(TIPO = `Tipo variazione`,
         COD = `Codice Comune`,
         COD_ASS = `Codice Comune associato`,
         ANNO = `Anno evento`) %>%
  mutate(across(c(COD, COD_ASS), as.character))

# Costruisco preventivamente due index per look-up O(1):
# direct_map[c_post] = lista dei "predecessori" (codici precedenti, potenzialmente 1991)
direct_map <- new.env(parent = emptyenv())

add_pred <- function(c_post, c_pre) {
  if (is.na(c_post) || is.na(c_pre) || c_post == c_pre) return(invisible(NULL))
  key <- c_post
  prev <- direct_map[[key]]
  direct_map[[key]] <- unique(c(prev, c_pre))
}

# CS / CECS: COD = nuovo, COD_ASS = vecchio (predecessore)
csrows <- var %>% filter(TIPO %in% c("CS-Costituzione",
                                      "CECS-Cessione territorio per costituzione nuova unità"))
for (i in seq_len(nrow(csrows))) add_pred(csrows$COD[i], csrows$COD_ASS[i])

# AP: COD = vecchio (1991), COD_ASS = nuovo. Predecessore di COD_ASS = COD
aprows <- var %>% filter(TIPO == "AP-Cambio appartenenza Provincia")
for (i in seq_len(nrow(aprows))) add_pred(aprows$COD_ASS[i], aprows$COD[i])

# RN: COD = vecchio, COD_ASS = nuovo (idem AP)
rnrows <- var %>% filter(TIPO == "RN-Rinumerazione del codice statistico")
for (i in seq_len(nrow(rnrows))) add_pred(rnrows$COD_ASS[i], rnrows$COD[i])

# Memoizzazione
cache <- new.env(parent = emptyenv())

expand_to_1991 <- function(c, depth = 0) {
  if (is.null(c) || is.na(c) || c == "") return(character(0))
  if (depth > 15) return(character(0))
  if (!is.null(cache[[c]])) return(cache[[c]])
  if (c %in% codici_1991) { cache[[c]] <- c; return(c) }
  preds <- direct_map[[c]]
  if (is.null(preds)) { cache[[c]] <- character(0); return(character(0)) }
  out <- unique(unlist(lapply(preds, expand_to_1991, depth = depth + 1)))
  cache[[c]] <- out
  out
}

# ---- 5) Indici tabelle storiche ---------------------------------------------

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
         NOME_NORM    = norm(DEN_PREC),
         NOME_NORM_NP = norm_nopre(DEN_PREC))

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
         NOME_NORM    = norm(NOME_SOPP),
         NOME_NORM_NP = norm_nopre(NOME_SOPP))

# ---- 6) Override manuali per casi noti -------------------------------------

# Casi che le euristiche non risolvono pulitamente: li dichiaro a mano.
# PRO_COM = "" significa irrisolvibile (territorio non più italiano).
overrides <- tribble(
  ~PROVINCIA_1946, ~COMUNE_1946,        ~PRO_COM_1991_OVR,        ~NOTE_OVR,
  "CUNEO",         "BRIGA MARITTIMA",   "004027",                  "ceduto alla Francia 1947; resta solo Briga Alta come frazione italiana - approssimazione",
  "CUNEO",         "TENDA",             "",                        "ceduto integralmente alla Francia col Trattato di Parigi 1947",
  "CUNEO",         "VINADIO",           NA_character_,             NA_character_, # sentinel, non usato
  "TRENTO",        "BORGO",             "022022",                  "Borgo Valsugana",
  "BELLUNO",       "CASTELLO LAVAZZO",  "025009",                  "Castellavazzo (poi fusa con Longarone nel 2014)",
  "AOSTA",         "VILLA SANT ANSELMO","007020;007021",           "scisso in Challand-Saint-Anselme + Challand-Saint-Victor"
) %>% filter(!is.na(PRO_COM_1991_OVR))

# Trasformo in tabella lunga
overrides_long <- overrides %>%
  separate_rows(PRO_COM_1991_OVR, sep = ";") %>%
  filter(PRO_COM_1991_OVR != "")

# ---- 7) Funzione di risoluzione di un singolo residuo ----------------------

resolve_one <- function(prov_46, com_46, com_46_norm, com_46_norm_np, cod_prov_list) {

  # 0) Override manuale
  ovr <- overrides_long %>%
    filter(PROVINCIA_1946 == prov_46, COMUNE_1946 == com_46)
  if (nrow(ovr) > 0) {
    return(list(metodo = "manuale",
                pro_com_1991 = ovr$PRO_COM_1991_OVR,
                note = ovr$NOTE_OVR[1]))
  }

  # Vincolo: i match devono ricadere in cod_prov_list
  com_filt <- com91 %>% filter(COD_PROV %in% cod_prov_list)

  # 1) Variante senza spazi (Sanremo, Massalubrense, Pontboset, ecc.)
  com_compact <- nospc(com_46_norm)
  hits <- com_filt %>% filter(nospc(COMUNE_NORM) == com_compact)
  if (nrow(hits) > 0) {
    return(list(metodo = "variante_grafica",
                pro_com_1991 = hits$PRO_COM_T,
                note = paste("match senza spazi con", paste(hits$COMUNE, collapse = ", "))))
  }

  # 2a) Senza preposizioni: matcha "COSTA MONTICELLI" con "Costa di Monticelli",
  # "BELVEDERE SPINELLO" con "Belvedere di Spinello", ecc.
  if (com_46_norm_np != "" && com_46_norm_np != com_46_norm) {
    hits <- com_filt %>% filter(COMUNE_NORM_NP == com_46_norm_np)
    if (nrow(hits) > 0) {
      return(list(metodo = "no_preposizioni",
                  pro_com_1991 = hits$PRO_COM_T,
                  note = paste("match senza preposizioni con", paste(hits$COMUNE, collapse = ", "))))
    }
  }

  # 2b) Prefisso: nome 1946 == prima parola(/e) di un unico comune 1991
  hits <- com_filt %>%
    filter(str_starts(COMUNE_NORM, paste0(com_46_norm, " ")))
  if (nrow(hits) == 1) {
    return(list(metodo = "prefisso",
                pro_com_1991 = hits$PRO_COM_T,
                note = paste("prefisso di", hits$COMUNE)))
  }

  # 3) Denominazione storica (esatto, poi senza preposizioni) — PRIORITÀ
  # ALTA: se ISTAT dice "X 1946 = Y 1991" (es. Castello d'Alife = Castello
  # del Matese, Piana di Caiazzo = Piana di Monte Verna) lo prendiamo come
  # verità, anche se le parole sembrano comporre una fusione fascista.
  hits <- denominazioni %>% filter(NOME_NORM == com_46_norm)
  if (nrow(hits) == 0 && com_46_norm_np != "") {
    hits <- denominazioni %>% filter(NOME_NORM_NP == com_46_norm_np)
  }
  if (nrow(hits) > 0) {
    h_match <- hits %>%
      filter(as.integer(COD_PROV_PREC) %in% cod_prov_list |
             as.integer(COD_PROV_CORR) %in% cod_prov_list)
    if (nrow(h_match) > 0) hits <- h_match
    cod_corr <- unique(hits$COD_CORR)
    pro_com <- unique(unlist(lapply(cod_corr, expand_to_1991)))
    pro_com <- intersect(pro_com, com_filt$PRO_COM_T)
    if (length(pro_com) > 0) {
      # Tentativo di aggregazione "_plus": solo per fusioni storiche reali
      # (RENATE VEDUGGIO, OLGIATE CALCO, LESA BELGIRATE, ecc.).
      # Esclude i toponimi qualificatori tipo "Bassano DI Sutri" / "Piana DI
      # Caiazzo" applicando due filtri sulla denominazione storica originale:
      #   (a) la PRIMA parola della denominazione storica è il comune
      #       principale e va sempre tolta dal residuo;
      #   (b) ogni parola residua preceduta da una preposizione (di/d'/del/
      #       della/da/...) nella denominazione storica è un qualificatore
      #       geografico, non un comune coincidente.
      den_norm <- norm(hits$DEN_PREC[1])
      den_words <- str_split(den_norm, " ")[[1]]
      first_dom <- den_words[1]
      # Parole "scartate": prima parola della denominazione + parole precedute
      # da preposizione nel testo originale (controllando in den_words).
      preceduta <- function(w) {
        idx <- which(den_words == w)
        if (length(idx) == 0) return(FALSE)
        any(map_lgl(idx, ~ .x > 1 && den_words[.x - 1] %in% .PREP))
      }
      parts <- str_split(com_46_norm, " ")[[1]]
      parts <- parts[nchar(parts) >= 2 & !(parts %in% .PREP)]
      nomi_pro <- com_filt %>% filter(PRO_COM_T %in% pro_com) %>% pull(COMUNE_NORM)
      already <- unique(unlist(str_split(nomi_pro, " ")))
      rest <- setdiff(parts, already)
      rest <- setdiff(rest, first_dom)
      rest <- rest[!map_lgl(rest, preceduta)]
      if (length(rest) > 0) {
        extra <- com_filt %>% filter(COMUNE_NORM %in% rest)
        for (w in setdiff(rest, extra$COMUNE_NORM)) {
          uniq <- com_filt %>% filter(str_starts(COMUNE_NORM, paste0(w, " ")))
          if (nrow(uniq) == 1) extra <- bind_rows(extra, uniq)
        }
        if (nrow(extra) > 0) {
          pro_com <- unique(c(pro_com, extra$PRO_COM_T))
          return(list(metodo = "denominazione_storica_plus",
                      pro_com_1991 = pro_com,
                      note = paste0("nome storico '", hits$DEN_PREC[1],
                                    "' -> ", paste(cod_corr, collapse=","),
                                    " + aggregati ", paste(extra$COMUNE, collapse=", "))))
        }
      }
      return(list(metodo = "denominazione_storica",
                  pro_com_1991 = pro_com,
                  note = paste0("nome storico '", hits$DEN_PREC[1],
                                "' -> codice corrente ", paste(cod_corr, collapse = ","))))
    }
  }

  # 4) Soppressione (esatto, poi senza preposizioni)
  hits <- soppressioni %>% filter(NOME_NORM == com_46_norm)
  if (nrow(hits) == 0 && com_46_norm_np != "") {
    hits <- soppressioni %>% filter(NOME_NORM_NP == com_46_norm_np)
  }
  if (nrow(hits) > 0) {
    h_match <- hits %>% filter(as.integer(COD_PROV_SOPP) %in% cod_prov_list)
    if (nrow(h_match) > 0) hits <- h_match
    hits_post <- hits %>% filter(ANNO >= 1946)
    if (nrow(hits_post) > 0) hits <- hits_post
    cod_ass <- unique(hits$COD_ASS)
    pro_com <- unique(unlist(lapply(cod_ass, expand_to_1991)))
    pro_com <- intersect(pro_com, com_filt$PRO_COM_T)
    if (length(pro_com) > 0) {
      parts <- str_split(com_46_norm, " ")[[1]]
      parts <- parts[nchar(parts) >= 2 & !(parts %in% .PREP)]
      nomi_pro <- com_filt %>% filter(PRO_COM_T %in% pro_com) %>% pull(COMUNE_NORM)
      already <- unique(unlist(str_split(nomi_pro, " ")))
      rest <- setdiff(parts, already)
      if (length(rest) > 0) {
        extra <- com_filt %>% filter(COMUNE_NORM %in% rest)
        for (w in setdiff(rest, extra$COMUNE_NORM)) {
          uniq <- com_filt %>% filter(str_starts(COMUNE_NORM, paste0(w, " ")))
          if (nrow(uniq) == 1) extra <- bind_rows(extra, uniq)
        }
        if (nrow(extra) > 0) {
          pro_com <- unique(c(pro_com, extra$PRO_COM_T))
          return(list(metodo = "soppressione_plus",
                      pro_com_1991 = pro_com,
                      note = paste0("soppresso ", paste(unique(hits$ANNO), collapse = "/"),
                                    " -> ", paste(cod_ass, collapse = ","),
                                    " + aggregati ", paste(extra$COMUNE, collapse = ", "))))
        }
      }
      return(list(metodo = "soppressione",
                  pro_com_1991 = pro_com,
                  note = paste0("soppresso ", paste(unique(hits$ANNO), collapse = "/"),
                                " -> assorbente corrente ", paste(cod_ass, collapse = ","))))
    }
  }

  # 5) Fusione fascista: il nome 1946 è la concatenazione di 2+ comuni 1991
  # (es. FAVRIA OGLIANICO -> Favria + Oglianico). Si esegue dopo denominazione
  # e soppressione per evitare di sovrascrivere mapping ISTAT espliciti.
  parts <- str_split(com_46_norm, " ")[[1]]
  parts <- parts[nchar(parts) >= 2 & !(parts %in% .PREP)]
  if (length(parts) >= 2) {
    for (mode in c("strict", "loose")) {
      for (i in seq_len(length(parts) - 1)) {
        p1 <- paste(parts[1:i], collapse = " ")
        p2 <- paste(parts[(i + 1):length(parts)], collapse = " ")
        if (mode == "strict") {
          c1 <- com_filt %>% filter(COMUNE_NORM == p1)
          c2 <- com_filt %>% filter(COMUNE_NORM == p2)
        } else {
          c1 <- com_filt %>% filter(COMUNE_NORM == p1 |
                                    str_starts(COMUNE_NORM, paste0(p1, " ")))
          c2 <- com_filt %>% filter(COMUNE_NORM == p2 |
                                    str_starts(COMUNE_NORM, paste0(p2, " ")))
          if (nrow(c1) > 1) c1 <- c1 %>% filter(COMUNE_NORM == p1)
          if (nrow(c2) > 1) c2 <- c2 %>% filter(COMUNE_NORM == p2)
        }
        if (nrow(c1) >= 1 && nrow(c2) >= 1) {
          pick <- bind_rows(c1, c2) %>% distinct(PRO_COM_T, .keep_all = TRUE)
          if (nrow(pick) >= 2) {
            return(list(metodo = paste0("fusione_fascista_", mode),
                        pro_com_1991 = pick$PRO_COM_T,
                        note = paste("aggregazione di", paste(pick$COMUNE, collapse = " + "))))
          }
        }
      }
    }
  }

  # 5b) Suffisso geografico: solo la prima parola è un comune 1991 unico,
  # le altre parole NON corrispondono ad altri comuni (es. ZEME LOMELLINA).
  if (str_count(com_46_norm, " ") >= 1) {
    first_word <- word(com_46_norm, 1)
    hits <- com_filt %>% filter(COMUNE_NORM == first_word)
    if (nrow(hits) == 1) {
      return(list(metodo = "suffisso_geografico",
                  pro_com_1991 = hits$PRO_COM_T,
                  note = paste0("'", com_46_norm, "' -> ", hits$COMUNE,
                                " (suffisso geografico caduto)")))
    }
  }

  # 6) Approssimato JW
  if (nrow(com_filt) > 0) {
    d <- stringdist::stringdist(com_46_norm, com_filt$COMUNE_NORM, method = "jw")
    best <- which.min(d)
    if (length(best) && d[best] < 0.12) {
      return(list(metodo = "approssimato",
                  pro_com_1991 = com_filt$PRO_COM_T[best],
                  note = paste0("JW=", round(d[best], 3), " -> ", com_filt$COMUNE[best])))
    }
  }

  list(metodo = "irrisolto", pro_com_1991 = character(0), note = NA_character_)
}

# ---- 8) Loop su tutti i 7180 comuni ----------------------------------------

cat("Inizio risoluzione di", nrow(referendum), "comuni 1946...\n")

ref_with_cands <- referendum %>%
  left_join(prov_candidati, by = c("PROVINCIA" = "PROVINCIA_1946"))

res <- ref_with_cands %>%
  rowwise() %>%
  mutate(.r = list(resolve_one(PROVINCIA, COMUNE, COMUNE_NORM, COMUNE_NORM_NP, COD_LIST))) %>%
  ungroup() %>%
  mutate(
    METODO = map_chr(.r, "metodo"),
    PRO_COM_1991_LIST = map(.r, "pro_com_1991"),
    NOTE_MATCH = map_chr(.r, "note")
  ) %>%
  select(-.r, -COD_LIST)

cat("\nDistribuzione metodi:\n")
print(res %>% count(METODO, sort = TRUE))

# ---- 9) Espando in tabella ponte ------------------------------------------

# Lookup nomi 1991
com91_idx <- com91 %>%
  select(PRO_COM_T, COMUNE, COD_PROV, DEN_PROV, SIGLA, POP_1991)

ponte <- res %>%
  select(CIRCOSCRIZIONE, PROVINCIA, COMUNE, ELETTORI, VOTANTI,
         NUMVOTISI, NUMVOTINO, SCHEDE_BIANCHE,
         METODO, PRO_COM_1991_LIST, NOTE_MATCH) %>%
  mutate(.id = row_number()) %>%
  unnest(PRO_COM_1991_LIST, keep_empty = TRUE) %>%
  rename(PRO_COM_1991 = PRO_COM_1991_LIST) %>%
  left_join(com91_idx, by = c("PRO_COM_1991" = "PRO_COM_T")) %>%
  rename(COMUNE_1991 = COMUNE.y, COMUNE_1946 = COMUNE.x,
         DEN_PROV_1991 = DEN_PROV, SIGLA_1991 = SIGLA, COD_PROV_1991 = COD_PROV,
         POP_1991 = POP_1991) %>%
  arrange(.id) %>%
  select(-.id)

# Conteggi finali
cat("\n--- Sommario finale ---\n")
cat("Comuni 1946 risolti:    ",
    n_distinct(paste(res$PROVINCIA, res$COMUNE)[res$METODO != "irrisolto"]), "\n")
cat("Comuni 1946 irrisolti:  ",
    n_distinct(paste(res$PROVINCIA, res$COMUNE)[res$METODO == "irrisolto"]), "\n")
cat("Righe ponte (one row per 1946x1991 mapping):", nrow(ponte), "\n")

cat("\nElettori non assegnati (somma irrisolti):",
    res %>% filter(METODO == "irrisolto") %>% pull(ELETTORI) %>% sum(), "\n")

# ---- 10) Salva CSV ponte e RDS workspace ------------------------------------

write_csv(ponte, file.path(BASE, "output", "ponte_1946_1991.csv"))

# Versione "compatta": una riga per comune 1946, codici 1991 separati da ";"
ponte_compatto <- res %>%
  select(CIRCOSCRIZIONE, PROVINCIA, COMUNE, ELETTORI, VOTANTI,
         NUMVOTISI, NUMVOTINO, SCHEDE_BIANCHE,
         METODO, PRO_COM_1991_LIST, NOTE_MATCH) %>%
  mutate(
    PRO_COM_1991 = map_chr(PRO_COM_1991_LIST, ~ paste(.x, collapse = ";")),
    N_COMUNI_1991 = map_int(PRO_COM_1991_LIST, length)
  ) %>%
  select(-PRO_COM_1991_LIST) %>%
  rename(COMUNE_1946 = COMUNE)

write_csv(ponte_compatto, file.path(BASE, "output", "ponte_1946_1991_compatto.csv"))

saveRDS(list(res = res, ponte = ponte, com91 = com91, com91_shp = com91_shp,
             referendum = referendum, prov_map = prov_map),
        file.path(BASE, "output", "ponte_workspace.rds"))

cat("\nFile salvati:\n")
cat(" - output/ponte_1946_1991.csv          (long: una riga per (1946 x 1991))\n")
cat(" - output/ponte_1946_1991_compatto.csv (wide: una riga per comune 1946)\n")
cat(" - output/ponte_workspace.rds          (RDS per ricostruire la geometria)\n")

# ---- 11) Diagnostica: stampa irrisolti e casi approssimati ------------------

cat("\n=== Comuni IRRISOLTI ===\n")
print(res %>% filter(METODO == "irrisolto") %>% select(PROVINCIA, COMUNE, ELETTORI), n = 50)

cat("\n=== Match APPROSSIMATI (controllare) ===\n")
print(res %>% filter(METODO == "approssimato") %>% select(PROVINCIA, COMUNE, NOTE_MATCH), n = 50)

cat("\n=== Match FUSIONE FASCISTA SCISSA ===\n")
print(res %>% filter(METODO == "fusione_fascista_scissa") %>%
        select(PROVINCIA, COMUNE, NOTE_MATCH), n = 50)
