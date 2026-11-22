# Affitti brevi nelle città italiane: indicatori da Inside Airbnb (snapshot giugno 2026)
# rapportati alle abitazioni del censimento permanente ISTAT.
#
# Fonti
# - Inside Airbnb, listings dettagliati (insideairbnb.com/get-the-data, CC BY 4.0).
#   Scaricati con affittibrevi_01_scarica_insideairbnb.py
# - ISTAT, Censimento permanente: abitazioni per comune al 31/12/2021 e 2023
#   (dataflow SDMX DF_DCSS_ABITAZIONI_TV_1, esploradati.istat.it)
# - ISTAT, Basi territoriali 2021: sezioni di censimento con abitazioni (ABI21) e
#   popolazione (POP21) e codici delle aree sub-comunali (municipi, quartieri, NIL, zone)
#   https://www.istat.it/storage/cartografia/basi_territoriali/2021/Rxx_21.zip
# - ISTAT, Elenco dei comuni italiani (per abbinare i nomi dei comuni di Puglia, Sicilia,
#   provincia di Bergamo)
#
# Da lanciare dalla radice del repo data-viz. I file grezzi pesanti stanno fuori dal repo.
#
# Definizioni
# - annuncio "attivo": almeno una recensione negli ultimi 12 mesi (number_of_reviews_ltm > 0)
# - host multi-annuncio: calculated_host_listings_count > 1 nello stesso snapshot
# - CIN presente: il campo license contiene un codice nel formato del Codice identificativo
#   nazionale (IT + codice ISTAT del comune a 6 cifre + lettere/cifre)

library(tidyverse)
library(sf)

dir_priv <- file.path(path.expand("~"), "Desktop", "Costo delle case", "dati_privati")
dir_iab  <- file.path(dir_priv, "insideairbnb")
dir_sez  <- file.path(dir_priv, "istat_sezioni")
dir_out  <- "Costo delle case/input"

# --- 1. Abitazioni ISTAT per comune -------------------------------------------

f_abi <- file.path(dir_priv, "istat_abitazioni_comuni.csv")
if (!file.exists(f_abi)) {
  url <- paste0("https://esploradati.istat.it/SDMXWS/rest/data/IT1,DF_DCSS_ABITAZIONI_TV_1,1.0/",
                "A...ALL.TOT.ALL.ALL.TOTAL.TOTAL")
  download.file(url, f_abi, headers = c(Accept = "application/vnd.sdmx.data+csv;version=1.0.0"))
}
abitazioni <- read_csv(f_abi, col_types = cols(REF_AREA = "c", .default = "c")) |>
  transmute(pro_com = REF_AREA, indicatore = INDICATOR, anno = TIME_PERIOD,
            valore = as.numeric(OBS_VALUE)) |>
  filter(anno %in% c("2021", "2023")) |>
  mutate(var = paste0(recode(indicatore, NUM_DW_AV = "abitazioni",
                             NUM_OCC_DW_AV = "abitazioni_occupate",
                             NUM_UNOCC_DW_AV = "abitazioni_non_occupate"), "_", anno)) |>
  select(pro_com, var, valore) |>
  pivot_wider(names_from = var, values_from = valore)

f_com <- file.path(dir_priv, "istat_elenco_comuni.csv")
if (!file.exists(f_com)) {
  download.file("https://www.istat.it/storage/codici-unita-amministrative/Elenco-comuni-italiani.csv", f_com)
}
comuni <- read_delim(f_com, delim = ";", locale = locale(encoding = "latin1"),
                     col_types = cols(.default = "c")) |>
  transmute(pro_com = `Codice Comune formato alfanumerico`,
            comune = `Denominazione in italiano`,
            cod_reg = `Codice Regione`,
            sigla = `Sigla automobilistica`)

# normalizza i nomi per l'abbinamento: minuscolo, solo lettere ASCII
# (Inside Airbnb a volte perde le lettere accentate: "Nard" per "Nardò")
norm <- function(x) {
  x |> str_to_lower() |> str_replace_all("[^a-z]", "")
}
comuni <- comuni |> mutate(chiave = norm(comune))

# --- 2. Annunci Inside Airbnb --------------------------------------------------

file_iab <- list.files(dir_iab, pattern = "_listings\\.csv\\.gz$", full.names = TRUE)

leggi <- function(f) {
  area <- str_extract(basename(f), "^[a-z]+")
  data <- str_extract(basename(f), "\\d{4}-\\d{2}-\\d{2}")
  read_csv(f, col_types = cols(.default = "c"), progress = FALSE) |>
    transmute(area, data_snapshot = data, id, host_id, room_type,
              quartiere = neighbourhood_cleansed,
              gruppo = neighbourhood_group_cleansed,
              lat = as.numeric(latitude), lon = as.numeric(longitude),
              prezzo = parse_number(price),
              camere = as.numeric(bedrooms),
              recensioni_12m = as.numeric(number_of_reviews_ltm),
              occupazione_stimata = as.numeric(estimated_occupancy_l365d),
              ricavo_stimato = as.numeric(estimated_revenue_l365d),
              n_host = as.numeric(calculated_host_listings_count),
              licenza = license)
}
annunci <- map(file_iab, leggi) |> list_rbind() |>
  mutate(intero = room_type == "Entire home/apt",
         attivo = recensioni_12m > 0,
         cin = str_detect(str_to_upper(str_remove_all(coalesce(licenza, ""), "\\s")),
                          "^IT\\d{6}[A-Z0-9]{5,}"))

# Comune di ciascun annuncio
citta <- c(milan = "015146", rome = "058091", florence = "048017", venice = "027042",
           naples = "063049", bologna = "037006")
reg_area <- c(puglia = "16", sicily = "19")

abbina <- annunci |> distinct(area, quartiere) |>
  mutate(chiave = norm(quartiere))
abbina_reg <- abbina |> filter(area %in% names(reg_area)) |>
  mutate(cod_reg = reg_area[area]) |>
  left_join(comuni |> select(pro_com, chiave, cod_reg), by = c("chiave", "cod_reg"))
abbina_bg <- abbina |> filter(area == "bergamo") |>
  left_join(comuni |> filter(sigla == "BG") |> select(pro_com, chiave), by = "chiave")
abbina_tn <- abbina |> filter(area == "trentino") |>
  mutate(pro_com = if_else(quartiere == "Trento, Trentino", "022205", NA_character_))
mappa_com <- bind_rows(abbina_reg, abbina_bg, abbina_tn) |> select(area, quartiere, pro_com)

annunci <- annunci |>
  left_join(mappa_com, by = c("area", "quartiere")) |>
  mutate(pro_com = coalesce(unname(citta[area]), pro_com))

cat("Annunci senza comune abbinato:\n")
print(annunci |> filter(is.na(pro_com)) |> count(area, quartiere, sort = TRUE) |> head(15))

# --- 3. Indicatori ------------------------------------------------------------

indicatori <- function(d) {
  d |> summarise(
    annunci = n(),
    annunci_attivi = sum(attivo),
    interi = sum(intero),
    interi_attivi = sum(intero & attivo),
    quota_interi = interi / annunci,
    host = n_distinct(host_id),
    quota_annunci_host_multi = mean(n_host > 1, na.rm = TRUE),
    quota_annunci_host_10piu = mean(n_host >= 10, na.rm = TRUE),
    quota_host_multi = n_distinct(host_id[n_host > 1]) / n_distinct(host_id),
    quota_con_cin = mean(cin),
    .groups = "drop")
}

# Ricavi stimati da Inside Airbnb (occupazione stimata dalle recensioni) per alloggi
# interi attivi con una camera da letto: utile per il confronto con l'affitto lungo
ricavi <- function(d) {
  d |> filter(intero, attivo, camere == 1) |>
    summarise(n_bilocali_attivi = n(),
              prezzo_mediano_notte = median(prezzo, na.rm = TRUE),
              notti_stimate_mediane = median(occupazione_stimata, na.rm = TRUE),
              ricavo_annuo_mediano = median(ricavo_stimato, na.rm = TRUE),
              ricavo_annuo_p75 = quantile(ricavo_stimato, 0.75, na.rm = TRUE),
              .groups = "drop")
}

# 3a. Comuni: le sei città, Bergamo, Trento e i comuni con almeno 500 annunci in Puglia e Sicilia
per_comune <- annunci |> filter(!is.na(pro_com)) |>
  group_by(area, data_snapshot, pro_com) |> indicatori() |>
  left_join(annunci |> filter(!is.na(pro_com)) |> group_by(area, pro_com) |> ricavi(),
            by = c("area", "pro_com")) |>
  left_join(comuni |> select(pro_com, comune), by = "pro_com") |>
  left_join(abitazioni, by = "pro_com") |>
  mutate(annunci_per_100_abitazioni = 100 * annunci / abitazioni_2021,
         interi_attivi_per_100_abitazioni = 100 * interi_attivi / abitazioni_2021,
         quota_abitazioni_non_occupate_2021 = abitazioni_non_occupate_2021 / abitazioni_2021) |>
  filter(pro_com %in% c(citta, "016024", "022205") | annunci >= 500) |>
  relocate(comune, .after = pro_com) |>
  arrange(desc(annunci))

write_csv(per_comune, file.path(dir_out, "affittibrevi_comuni.csv"))

# 3b. Aree intere coperte da Inside Airbnb (regioni e province)
abi_reg <- abitazioni |> left_join(comuni, by = "pro_com")
den_area <- tribble(
  ~area, ~territorio, ~abitazioni_2021,
  "puglia",   "Regione Puglia",        sum(abi_reg$abitazioni_2021[abi_reg$cod_reg == "16"], na.rm = TRUE),
  "sicily",   "Regione Siciliana",     sum(abi_reg$abitazioni_2021[abi_reg$cod_reg == "19"], na.rm = TRUE),
  "bergamo",  "Provincia di Bergamo",  sum(abi_reg$abitazioni_2021[abi_reg$sigla == "BG"], na.rm = TRUE),
  "trentino", "Provincia di Trento",   sum(abi_reg$abitazioni_2021[abi_reg$sigla == "TN"], na.rm = TRUE)
)
per_area <- annunci |> filter(area %in% den_area$area) |>
  group_by(area, data_snapshot) |> indicatori() |>
  left_join(den_area, by = "area") |>
  mutate(annunci_per_100_abitazioni = 100 * annunci / abitazioni_2021,
         interi_attivi_per_100_abitazioni = 100 * interi_attivi / abitazioni_2021) |>
  relocate(territorio, .after = area)
write_csv(per_area, file.path(dir_out, "affittibrevi_aree_regionali.csv"))

# --- 4. Zone sub-comunali e centri storici ------------------------------------

leggi_asc <- function(reg, livello) {
  f <- file.path(dir_sez, sprintf("R%s_21/TAB/ASC%d_R%s_21.csv", reg, livello, reg))
  if (!file.exists(f)) return(NULL)
  read.delim(f, fileEncoding = "UTF-16", colClasses = "character") |>
    transmute(codice = .data[[paste0("COM_ASC", livello)]],
              nome = .data[[paste0("DEN_ASC", livello)]],
              tipo = .data[[paste0("TIPO_ASC", livello)]])
}

# regione ISTAT, livello sub-comunale usato e definizione di centro storico
conf <- tribble(
  ~area,      ~reg, ~livello, ~centro,
  "florence", "09", 1, list("Centro Storico"),
  "rome",     "12", 2, list(sprintf("Rione %s", c("Monti", "Trevi", "Colonna", "Campo Marzio", "Ponte",
                                                  "Parione", "Regola", "Sant'Eustachio", "Pigna",
                                                  "Campitelli", "Sant'Angelo", "Ripa", "Trastevere",
                                                  "Borgo", "Esquilino", "Ludovisi", "Sallustiano",
                                                  "Castro Pretorio", "Celio", "Testaccio",
                                                  "San Saba", "Prati"))),
  "milan",    "03", 1, list("Municipio 1"),
  "venice",   "05", 1, list("Venezia-Murano-Burano (Venezia Insulare)"),
  "naples",   "15", 2, list(c("San Ferdinando", "San Giuseppe", "Montecalvario", "Avvocata",
                              "Pendino", "Porto", "Mercato", "San Lorenzo", "Stella")),
  "bologna",  "08", 2, list(c("Galvani", "Irnerio", "Malpighi", "Marconi"))
)

zone_citta <- function(i) {
  a <- conf$area[i]; reg <- conf$reg[i]; liv <- conf$livello[i]; centro <- conf$centro[[i]][[1]]
  pc <- as.integer(citta[a])
  sez <- st_read(file.path(dir_sez, sprintf("R%s_21/SHP/R%s_21_WGS84.shp", reg, reg)), quiet = TRUE) |>
    filter(PRO_COM == pc) |>
    mutate(codice = as.character(.data[[paste0("COM_ASC", liv)]]),
           codice_asc1 = as.character(COM_ASC1))
  pt <- annunci |> filter(area == a, !is.na(lat)) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    st_transform(st_crs(sez)) |>
    st_join(sez |> select(codice), join = st_within)
  nomi <- leggi_asc(reg, liv)
  # Roma: le zone toponomastiche (rioni, quartieri, suburbi) non coprono tutto il comune;
  # le sezioni fuori sono raggruppate come "altre zone"
  den <- sez |> st_drop_geometry() |> group_by(codice) |>
    summarise(abitazioni_2021 = sum(ABI21), residenti_2021 = sum(POP21),
              superficie_km2 = sum(SHAPE_Area) / 1e6, .groups = "drop")
  num <- pt |> st_drop_geometry() |> group_by(codice) |> indicatori()
  full_join(den, num, by = "codice") |>
    left_join(nomi, by = "codice") |>
    mutate(area = a, livello_asc = liv,
           nome = if_else(is.na(nome) | codice == "0", "Fuori dalle zone / non attribuito", nome),
           centro_storico = nome %in% centro,
           across(c(annunci, annunci_attivi, interi, interi_attivi), ~ replace_na(.x, 0)),
           annunci_per_100_abitazioni = 100 * annunci / abitazioni_2021,
           interi_attivi_per_100_abitazioni = 100 * interi_attivi / abitazioni_2021,
           annunci_per_km2 = annunci / superficie_km2)
}

zone <- map(seq_len(nrow(conf)), zone_citta) |> list_rbind() |>
  left_join(tibble(area = names(citta), pro_com = citta), by = "area") |>
  left_join(comuni |> select(pro_com, comune), by = "pro_com") |>
  select(comune, area, livello_asc, codice, nome, tipo, centro_storico, everything(), -pro_com) |>
  arrange(comune, desc(annunci_per_100_abitazioni))
write_csv(zone, file.path(dir_out, "affittibrevi_zone_subcomunali.csv"))

# Sintesi centro storico vs resto della città
centri <- zone |>
  mutate(parte = if_else(centro_storico, "centro", "resto")) |>
  group_by(comune, parte) |>
  summarise(across(c(abitazioni_2021, residenti_2021, annunci, interi_attivi), \(x) sum(x, na.rm = TRUE)),
            .groups = "drop") |>
  pivot_wider(names_from = parte, values_from = c(abitazioni_2021, residenti_2021, annunci, interi_attivi)) |>
  mutate(definizione_centro = c(
           Bologna = "Zone Galvani, Irnerio, Malpighi, Marconi (centro dentro i viali)",
           Firenze = "Quartiere 1 Centro Storico",
           Milano = "Municipio 1",
           Napoli = "Quartieri San Ferdinando, San Giuseppe, Montecalvario, Avvocata, Pendino, Porto, Mercato, San Lorenzo, Stella",
           Roma = "22 rioni storici (Monti ... Prati)",
           Venezia = "Municipalità Venezia-Murano-Burano (Venezia insulare)")[comune],
         quota_annunci_citta_nel_centro = annunci_centro / (annunci_centro + annunci_resto),
         quota_abitazioni_citta_nel_centro = abitazioni_2021_centro / (abitazioni_2021_centro + abitazioni_2021_resto),
         annunci_per_100_abitazioni_centro = 100 * annunci_centro / abitazioni_2021_centro,
         annunci_per_100_abitazioni_resto = 100 * annunci_resto / abitazioni_2021_resto,
         interi_attivi_per_100_abitazioni_centro = 100 * interi_attivi_centro / abitazioni_2021_centro,
         interi_attivi_per_100_abitazioni_resto = 100 * interi_attivi_resto / abitazioni_2021_resto,
         annunci_per_100_residenti_centro = 100 * annunci_centro / residenti_2021_centro)
write_csv(centri, file.path(dir_out, "affittibrevi_centri_storici.csv"))

# Ricavi stimati nei centri storici (alloggi interi attivi con una camera)
ricavi_centri <- map(seq_len(nrow(conf)), function(i) {
  z <- zone |> filter(area == conf$area[i], centro_storico) |> pull(codice)
  sez <- st_read(file.path(dir_sez, sprintf("R%s_21/SHP/R%s_21_WGS84.shp", conf$reg[i], conf$reg[i])),
                 quiet = TRUE) |>
    filter(PRO_COM == as.integer(citta[conf$area[i]])) |>
    mutate(codice = as.character(.data[[paste0("COM_ASC", conf$livello[i])]])) |>
    filter(codice %in% z) |> st_union()
  annunci |> filter(area == conf$area[i], !is.na(lat)) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    st_transform(st_crs(sez)) |>
    filter(st_within(geometry, sez, sparse = FALSE)[, 1]) |>
    st_drop_geometry() |> mutate(area = conf$area[i]) |> group_by(area) |> ricavi()
}) |> list_rbind() |>
  left_join(tibble(area = names(citta), pro_com = citta), by = "area") |>
  left_join(comuni |> select(pro_com, comune), by = "pro_com") |>
  select(comune, everything(), -pro_com)
write_csv(ricavi_centri, file.path(dir_out, "affittibrevi_ricavi_centri_storici.csv"))

print(per_comune |> select(comune, annunci, annunci_attivi, quota_interi, quota_annunci_host_multi,
                           annunci_per_100_abitazioni, interi_attivi_per_100_abitazioni), n = 40)
print(per_area)
print(centri |> select(comune, quota_annunci_citta_nel_centro, quota_abitazioni_citta_nel_centro,
                       annunci_per_100_abitazioni_centro, annunci_per_100_abitazioni_resto,
                       interi_attivi_per_100_abitazioni_centro))
print(ricavi_centri)

# --- 5. Venezia per sestiere ----------------------------------------------------
# Le municipalità ISTAT sono troppo grandi per la Venezia insulare: si usano i quartieri
# di Inside Airbnb (sestieri e isole) e si attribuiscono le abitazioni 2021 delle sezioni
# di censimento in base al punto interno di ciascuna sezione.

f_geo <- list.files(dir_iab, pattern = "^venice_.*_neighbourhoods\\.geojson$", full.names = TRUE)
quart_ve <- st_read(f_geo, quiet = TRUE) |> st_make_valid()
sez_ve <- st_read(file.path(dir_sez, "R05_21/SHP/R05_21_WGS84.shp"), quiet = TRUE) |>
  filter(PRO_COM == 27042)
quart_ve <- st_transform(quart_ve, st_crs(sez_ve))
abi_ve <- sez_ve |> st_point_on_surface() |>
  st_join(quart_ve |> select(quartiere = neighbourhood, gruppo = neighbourhood_group)) |>
  st_drop_geometry() |>
  group_by(quartiere, gruppo) |>
  summarise(abitazioni_2021 = sum(ABI21), residenti_2021 = sum(POP21), .groups = "drop")
venezia <- annunci |> filter(area == "venice") |> group_by(quartiere) |> indicatori() |>
  full_join(abi_ve, by = "quartiere") |>
  mutate(across(c(annunci, annunci_attivi, interi, interi_attivi), ~ replace_na(.x, 0)),
         annunci_per_100_abitazioni = 100 * annunci / abitazioni_2021,
         interi_attivi_per_100_abitazioni = 100 * interi_attivi / abitazioni_2021,
         annunci_per_100_residenti = 100 * annunci / residenti_2021) |>
  relocate(gruppo, .after = quartiere) |>
  arrange(desc(annunci))
write_csv(venezia, file.path(dir_out, "affittibrevi_venezia_sestieri.csv"))
print(venezia |> select(quartiere, gruppo, annunci, abitazioni_2021, residenti_2021,
                        annunci_per_100_abitazioni, annunci_per_100_residenti), n = 15)
