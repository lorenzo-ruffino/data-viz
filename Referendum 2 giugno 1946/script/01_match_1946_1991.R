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

# ---- 2) Normalizzazione nomi -------------------------------------------------

# Apostrofo terminale del referendum = accento sulla vocale finale.
# J ↔ I: nel 1946 si scrive "JESOLO/BAJARDO/JOLANDA"; nel 1991 ISTAT usa
# inconsistentemente J e I. Per il match riduco entrambi alla forma con I.
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

referendum <- referendum %>%
  mutate(
    COMUNE_NORM = norm(COMUNE),
    PROVINCIA_NORM = norm(PROVINCIA)
  )

com91 <- com91 %>%
  left_join(elenco91 %>% select(COD_PROV, DEN_PROV, SIGLA), by = "COD_PROV") %>%
  mutate(
    COMUNE_NORM   = norm(COMUNE),
    COMUNE_A_NORM = ifelse(is.na(COMUNE_A), NA_character_, norm(COMUNE_A)),
    DEN_PROV_NORM = norm(DEN_PROV)
  )

# Mapping province 1946 -> codice provincia 1991.
# Le seguenti province erano 1 nel 1946 e sono diventate 2 nel 1991:
#   UDINE -> Udine (30) + Pordenone (93)        Pordenone creata nel 1968
#   CAMPOBASSO -> Campobasso (70) + Isernia (94)   Isernia creata nel 1970
#   CAGLIARI -> Cagliari (92) + Oristano (95)      Oristano creata nel 1974
#   NUORO    -> Nuoro (91) + Oristano (95)         (territori da entrambe)
prov_map_46_to_91 <- tribble(
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

# Verifica copertura province
prov_unmapped <- referendum %>%
  anti_join(prov_map_46_to_91, by = c("PROVINCIA" = "PROVINCIA_1946")) %>%
  distinct(PROVINCIA)
if (nrow(prov_unmapped) > 0) {
  cat("ATTENZIONE: province 1946 non mappate:\n"); print(prov_unmapped)
}

# ---- 3) Match diretto: (cod_prov_1991, nome_normalizzato) --------------------

# Inner join molti-a-molti su (sigla provincia possibile, nome normalizzato).
com91_lookup <- com91 %>%
  select(COD_PROV, DEN_PROV, PRO_COM_T, COMUNE, COMUNE_NORM, COMUNE_A_NORM) %>%
  rename(COMUNE_1991 = COMUNE)

# Espando referendum su tutte le possibili province 1991:
ref_x <- referendum %>%
  inner_join(prov_map_46_to_91, by = c("PROVINCIA" = "PROVINCIA_1946"),
             relationship = "many-to-many")

m_direct <- ref_x %>%
  inner_join(
    com91_lookup,
    by = c("COD_PROV_1991" = "COD_PROV", "COMUNE_NORM" = "COMUNE_NORM"),
    relationship = "many-to-many"
  ) %>%
  mutate(METODO = "esatto_nome")

# Match sul nome tedesco (per Alto Adige... ma A.A. non vota; per qualche
# comune trilingue di Valle d'Aosta o Trentino).
m_direct_de <- ref_x %>%
  anti_join(m_direct %>% distinct(PROVINCIA, COMUNE),
            by = c("PROVINCIA", "COMUNE")) %>%
  inner_join(
    com91_lookup %>% filter(!is.na(COMUNE_A_NORM)),
    by = c("COD_PROV_1991" = "COD_PROV", "COMUNE_NORM" = "COMUNE_A_NORM"),
    relationship = "many-to-many"
  ) %>%
  mutate(METODO = "esatto_nome_tedesco")

step1 <- bind_rows(m_direct, m_direct_de)

cat("\n--- Step 1: match diretto ---\n")
cat("Referendum totali:    ", nrow(referendum), "\n")
cat("Comuni 1946 risolti:  ",
    n_distinct(paste(step1$PROVINCIA, step1$COMUNE)), "\n")
cat("Comuni 1946 residui:  ",
    nrow(referendum) - n_distinct(paste(step1$PROVINCIA, step1$COMUNE)), "\n")

residui <- referendum %>%
  anti_join(step1 %>% distinct(PROVINCIA, COMUNE), by = c("PROVINCIA", "COMUNE"))

saveRDS(list(
  referendum = referendum,
  com91 = com91,
  com91_lookup = com91_lookup,
  soppressioni = soppressioni,
  denominazioni = denominazioni,
  variazioni91 = variazioni91,
  step1 = step1,
  residui = residui,
  prov_map = prov_map_46_to_91
), file.path(BASE, "output", "step1_workspace.rds"))
