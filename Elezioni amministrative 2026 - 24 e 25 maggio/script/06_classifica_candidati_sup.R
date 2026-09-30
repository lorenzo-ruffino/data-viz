# Classifica i candidati sindaco dei comuni SUPERIORI (>15.000 abitanti)
# secondo lo schema:
#   centrosinistra | centrodestra | civico_centrosinistra | civico_centrodestra
#   m5s | corre_da_solo | civico_altro
#
# Logica:
#   - guarda le liste a sostegno di ogni candidato (risultati/liste.csv +
#     risultati_sicilia/liste.csv)
#   - identifica per ogni lista la possibile etichetta di partito tramite
#     regex sul nome della lista
#   - aggrega per candidato e applica le regole di classificazione

library(data.table)
library(dplyr)
library(stringr)

PROJ_DIR <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Elezioni amministrative 2026 - 24 e 25 maggio"

# ------------------------------------------------------------------
# 1. Carica le liste delle due fonti e uniformale
# ------------------------------------------------------------------
liste_ita <- fread(file.path(PROJ_DIR, "output/risultati/liste.csv"))
cand_ita  <- fread(file.path(PROJ_DIR, "output/risultati/candidati.csv"))
com_ita   <- fread(file.path(PROJ_DIR, "output/risultati/comuni.csv"))

liste_sic <- fread(file.path(PROJ_DIR, "output/risultati_sicilia/liste.csv"))
cand_sic  <- fread(file.path(PROJ_DIR, "output/risultati_sicilia/candidati.csv"))

# Sicilia: il candidato è già in formato "COGNOME NOME"; uniformiamo le chiavi
liste_sic[, candidato := cognome_nome_cand]
liste_ita[, candidato := paste(cognome_cand, nome_cand)]
cand_ita[,  candidato := paste(cognome, nome)]
cand_sic[,  candidato := cognome_nome]

# ------------------------------------------------------------------
# 1b. Tieni solo primo e secondo per voti in ogni comune.
# I candidati senza voti vengono comunque rankati (n_liste come spareggio),
# così sui comuni non ancora scrutinati teniamo i due con più liste.
# ------------------------------------------------------------------
cand_ita[, voti_num := as.numeric(voti)]
cand_ita[is.na(voti_num), voti_num := 0]
cand_ita[, rank_voti := frank(-voti_num, ties.method = "first"),
         by = .(desc_regione, desc_provincia, desc_comune)]
top_ita <- cand_ita[rank_voti <= 2,
                    .(desc_regione, desc_provincia, desc_comune, candidato)]

cand_sic[, voti_num := suppressWarnings(as.numeric(voti))]
cand_sic[is.na(voti_num), voti_num := 0]
cand_sic[, rank_voti := frank(-voti_num - n_liste/100, ties.method = "first"),
         by = .(sigla_prov, cod_comune)]
top_sic <- cand_sic[rank_voti <= 2, .(desc_comune, candidato)]

# Filtro ai soli comuni SUP
sup_ita <- com_ita[tipo_comune == "M",
                   .(desc_regione, desc_provincia, desc_comune)]
sup_ita[, key := paste(desc_regione, desc_provincia, desc_comune, sep = "|")]

liste_ita[, key := paste(desc_regione, desc_provincia, desc_comune, sep = "|")]
liste_ita_sup <- liste_ita[key %in% sup_ita$key]

# applica filtro top-2
top_ita_key <- paste(top_ita$desc_regione, top_ita$desc_provincia,
                     top_ita$desc_comune, top_ita$candidato, sep = "|")
liste_ita_sup[, kc := paste(desc_regione, desc_provincia, desc_comune,
                            candidato, sep = "|")]
liste_ita_sup <- liste_ita_sup[kc %in% top_ita_key]

# Sicilia: tutti i comuni nel file sono superiori? verifichiamo con input
input_comuni <- fread(file.path(PROJ_DIR, "input/comuni_al_voto_2026.csv"))
sup_sic <- input_comuni[tipologia == "SUP" & toupper(regione) == "SICILIA",
                        toupper(comune)]
liste_sic[, key := toupper(desc_comune)]
liste_sic_sup <- liste_sic[key %in% sup_sic]

# applica filtro top-2 (Sicilia: per ora voti=0, fallback su numero liste)
top_sic_key <- paste(top_sic$desc_comune, top_sic$candidato, sep = "|")
liste_sic_sup[, kc := paste(desc_comune, candidato, sep = "|")]
liste_sic_sup <- liste_sic_sup[kc %in% top_sic_key]

# Uniformo le colonne in un unico data.table
ita <- liste_ita_sup[, .(
  regione  = desc_regione,
  comune   = desc_comune,
  candidato,
  lista    = descr_lista
)]
sic <- liste_sic_sup[, .(
  regione  = "SICILIA",
  comune   = toupper(desc_comune),
  candidato,
  lista    = descr_lista
)]
all_liste <- rbind(ita, sic)
all_liste[, lista_up := toupper(lista)]

# ------------------------------------------------------------------
# 2. Classificatore per lista: assegna un'etichetta partito
# ------------------------------------------------------------------
classifica_lista <- function(x) {
  # partiti centrodestra
  if (str_detect(x, "FRATELLI D'ITALIA|FRATELLI D.ITALIA|\\bFDI\\b|\\bFD\\.I\\.|MELONI")) return("FDI")
  if (str_detect(x, "\\bLEGA\\b|LEGA SALVINI|LEGA - SALVINI|PRIMA L'ITALIA|PRIMA L.ITALIA"))     return("LEGA")
  if (str_detect(x, "FORZA ITALIA|FORZA AZZURRI|BERLUSCONI"))                                      return("FI")
  if (str_detect(x, "NOI MODERATI|NOI CON L'ITALIA|NOI CON L.ITALIA"))                             return("NM")
  if (str_detect(x, "DEMOCRAZIA CRISTIANA|\\bDC\\b"))                                              return("DC")
  if (str_detect(x, "UNIONE DI CENTRO|\\bUDC\\b"))                                                 return("UDC")
  if (str_detect(x, "MOVIMENTO PER L'AUTONOMIA|\\bMPA\\b"))                                        return("MPA")
  if (str_detect(x, "GRANDE SUD"))                                                                  return("GRANDE_SUD")
  if (str_detect(x, "SUD CHIAMA NORD|\\bSCN\\b"))                                                  return("SCN")
  if (str_detect(x, "\\bUDEUR\\b"))                                                                 return("UDEUR")
  # partiti centrosinistra
  if (str_detect(x, "PARTITO DEMOCRATICO|\\bPD\\b|\\bP\\.D\\."))                                   return("PD")
  if (str_detect(x, "ALLEANZA VERDI E SINISTRA|VERDI E SINISTRA|\\bAVS\\b|SINISTRA ITALIANA|EUROPA VERDE|\\bVERDI\\b|ECOLOGISTI E CIVICI|RIFORMISTI ED ECOLOGISTI")) return("AVS")
  if (str_detect(x, "ITALIA VIVA|\\bIV\\b - RENZI|MATTEO RENZI"))                                  return("IV")
  if (str_detect(x, "\\bAZIONE\\b|CALENDA"))                                                        return("AZIONE")
  if (str_detect(x, "\\+EUROPA|PI\\u00d9 EUROPA|PIU' EUROPA"))                                     return("PIUEUROPA")
  if (str_detect(x, "CASA RIFORMISTA"))                                                             return("CASARIF")
  if (str_detect(x, "RIFONDAZIONE COMUNISTA|\\bPRC\\b"))                                            return("PRC")
  if (str_detect(x, "POTERE AL POPOLO"))                                                            return("POPOLO")
  if (str_detect(x, "ARTICOLO 1|ART\\.1"))                                                           return("ART1")
  if (str_detect(x, "PARTITO SOCIALISTA|\\bPSI\\b|AVANTI - PSI"))                                  return("PSI")
  # M5S
  if (str_detect(x, "MOVIMENTO 5 STELLE|\\bM5S\\b|\\b5 STELLE\\b|CINQUE STELLE"))                  return("M5S")
  # default: civica
  return("CIVICA")
}

all_liste[, lista_label := vapply(lista_up, classifica_lista, character(1))]

# Mapping label → area. Distinguiamo tra partiti "core" (identificano
# univocamente la coalizione) e partiti "ponte" che da soli non bastano
# (Azione, PSI, UDC, MPA, SCN spesso aderiscono a coalizioni di entrambe
# le aree a livello locale).
area_map <- c(
  # core centrodestra
  FDI = "CDX_CORE", LEGA = "CDX_CORE", FI = "CDX_CORE", NM = "CDX_CORE",
  # core centrosinistra
  PD = "CSX_CORE", AVS = "CSX_CORE", IV = "CSX_CORE", PIUEUROPA = "CSX_CORE",
  CASARIF = "CSX_CORE", PRC = "CSX_CORE", POPOLO = "CSX_CORE", ART1 = "CSX_CORE",
  # ponte / cespiti locali
  DC = "AMBIG_CDX", UDC = "AMBIG_CDX", MPA = "AMBIG_CDX",
  GRANDE_SUD = "AMBIG_CDX", SCN = "AMBIG_CDX", UDEUR = "AMBIG_CDX",
  AZIONE = "AMBIG_CSX", PSI = "AMBIG_CSX",
  # M5S (gestito a parte)
  M5S = "M5S",
  CIVICA = "CIVICA"
)
all_liste[, area := area_map[lista_label]]

# ------------------------------------------------------------------
# 3. Aggrega per candidato
# ------------------------------------------------------------------
agg <- all_liste[, .(
  n_liste     = .N,
  n_cdx_core  = sum(area == "CDX_CORE"),
  n_csx_core  = sum(area == "CSX_CORE"),
  n_amb_cdx   = sum(area == "AMBIG_CDX"),
  n_amb_csx   = sum(area == "AMBIG_CSX"),
  n_m5s       = sum(area == "M5S"),
  n_civ       = sum(area == "CIVICA"),
  liste_str   = paste(lista, collapse = " | "),
  labels_str  = paste(unique(lista_label[area != "CIVICA"]), collapse = ",")
), by = .(regione, comune, candidato)]

# Per i casi "tutto civico", proviamo a inferire un lean dalla denominazione
# delle liste tramite parole chiave.
re_csx_civ <- "PROGRESSIST|ECOLOGIST|\\bSINISTRA\\b|RIFORMIST|DEMOCRATIC|COMUNIST|AMBIENTAL|ROSSO|ROSSA"
re_cdx_civ <- "PATRIOT|TRADIZION|\\bDESTRA\\b|IDENTIT|POPOLARE|CONSERVATOR|MODERATI|AZZURR|LIBERI E FORTI|CENTRODESTRA"

agg[, lean_civ_csx := str_detect(toupper(liste_str), re_csx_civ)]
agg[, lean_civ_cdx := str_detect(toupper(liste_str), re_cdx_civ)]

classifica_candidato <- function(n_liste, n_cdx_c, n_csx_c, n_amb_cdx,
                                 n_amb_csx, n_m5s, n_civ,
                                 lean_csx, lean_cdx) {
  # 1) lista unica civica → corre da solo
  if (n_liste == 1 && n_civ == 1) return("corre_da_solo")

  # 2) coalizione con partiti core di una sola area → centrodestra / centrosinistra
  if (n_cdx_c > 0 && n_csx_c == 0 && n_m5s == 0) return("centrodestra")
  if (n_csx_c > 0 && n_cdx_c == 0)               return("centrosinistra")
  if (n_csx_c > 0 && n_cdx_c > 0)                return("civico_altro")  # coalizione mista

  # 3) M5S in solitaria (con eventuali civiche) → m5s
  if (n_m5s > 0 && n_csx_c == 0 && n_cdx_c == 0 && n_amb_csx == 0 && n_amb_cdx == 0)
    return("m5s")

  # 4) solo partiti "ponte"
  if (n_amb_cdx > 0 && n_amb_csx == 0) return("civico_centrodestra")
  if (n_amb_csx > 0 && n_amb_cdx == 0) return("civico_centrosinistra")
  if (n_amb_cdx > 0 && n_amb_csx > 0)  return("civico_altro")

  # 5) tutto civico → guardo le keyword
  if (lean_csx && !lean_cdx) return("civico_centrosinistra")
  if (lean_cdx && !lean_csx) return("civico_centrodestra")
  return("civico_altro")
}

agg[, classificazione := mapply(classifica_candidato,
                                n_liste, n_cdx_core, n_csx_core,
                                n_amb_cdx, n_amb_csx, n_m5s, n_civ,
                                lean_civ_csx, lean_civ_cdx)]

# ------------------------------------------------------------------
# 3b. Override manuali per casi dove i dati delle liste ufficiali
# non riflettono la coalizione politica reale.
# ------------------------------------------------------------------
override <- data.table(
  comune    = c("SALERNO",          "MESSINA",           "BARCELLONA POZZO DI GOTTO"),
  cognome   = c("DE LUCA",          "BASILE",            "SCOLARO"),
  new_class = c("centrosinistra",   "civico_altro",      "civico_altro")
)
agg[, comune_up := toupper(comune)]
agg[, cogn_up   := toupper(stringr::word(candidato, 1))]
for (i in seq_len(nrow(override))) {
  agg[comune_up == override$comune[i] &
      cogn_up   == toupper(stringr::word(override$cognome[i], 1)),
      classificazione := override$new_class[i]]
}
agg[, c("comune_up","cogn_up") := NULL]

# ------------------------------------------------------------------
# 4. Salva l'output
# ------------------------------------------------------------------
setorder(agg, regione, comune, candidato)
out_path <- file.path(PROJ_DIR, "output/classificazione_candidati_sup.csv")
fwrite(agg[, .(regione, comune, candidato, classificazione,
               n_liste, n_csx_core, n_cdx_core, n_amb_csx, n_amb_cdx,
               n_m5s, n_civ,
               partiti = labels_str, liste = liste_str)],
       out_path)

cat("Salvato:", out_path, "\n")
cat("Totale candidati classificati:", nrow(agg), "\n\n")
print(agg[, .N, by = classificazione][order(-N)])
