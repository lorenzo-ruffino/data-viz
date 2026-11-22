# Popolazione residente al 1° gennaio per provincia, sesso ed età, 2011-2026.
#
# Fonte: Istat, demo.istat.it
#   - 2011-2018: ricostruzione intercensuaria 2002-2019 (comuni, età singola)
#   - 2019-2026: POSAS, popolazione al 1° gennaio (comuni, età singola);
#                il 2026 è una stima provvisoria (rilascio marzo 2026)
# Le province sono quelle dei dati di mortalità (input/derivati/comuni.csv,
# classificazione al 31/3/2026, Sardegna con le 8 nuove unità 112-119): i comuni
# di ogni anno sono ricondotti alla provincia attuale per codice, poi per nome
# (comuni sardi ricodificati, comuni passati ad altra provincia), infine per
# codice della provincia di allora (comuni poi fusi).
#
# Output (input/derivati/, parquet):
#   - popolazione_provincia.parquet           cod_prov x anno x sesso x eta (4 classi) -> pop
#   - popolazione_provincia_eta_singola.parquet cod_prov x anno x sesso x eta (0-100, 100 = 100+) -> pop

library(data.table)
library(arrow)

setwd("/Users/lorenzoruffino/Documents/Progetti/data-viz/Mortalità caldo 2026")
dir_pop <- "input/istat_popolazione"
dir.create(dir_pop, showWarnings = FALSE)

base <- "https://demo.istat.it/data/"
file_url <- c(sprintf("posas/POSAS_%d_it_Comuni.zip", 2019:2026),
              "ricostruzione/PopolazioneEta-SingolaArea-Comuni.zip")
for (f in file_url) {
  dest <- file.path(dir_pop, basename(f))
  if (!file.exists(dest)) download.file(paste0(base, f), dest, mode = "wb")
}

# POSAS 2019-2026 ---------------------------------------------------------------
leggi_posas <- function(anno) {
  zf <- file.path(dir_pop, sprintf("POSAS_%d_it_Comuni.zip", anno))
  x <- fread(cmd = sprintf("unzip -p '%s'", zf), skip = 1, encoding = "UTF-8",
             colClasses = list(character = 1:2))
  setnames(x, 1:3, c("cod_comune", "comune", "eta"))
  x <- x[eta != 999 & !is.na(eta)]
  x <- melt(x[, .(cod_comune, comune, eta, M = `Totale maschi`, F = `Totale femmine`)],
            id.vars = c("cod_comune", "comune", "eta"),
            variable.name = "sesso", value.name = "pop", variable.factor = FALSE)
  x[, anno := anno]
}
posas <- rbindlist(lapply(2019:2026, leggi_posas))

# Ricostruzione intercensuaria 2002-2019 (un file per comune) -----------------------
dir_ric <- file.path(dir_pop, "ric_comuni")
if (!dir.exists(dir_ric))
  unzip(file.path(dir_pop, "PopolazioneEta-SingolaArea-Comuni.zip"), exdir = dir_ric)

leggi_ric <- function(f) {
  l <- readLines(f, encoding = "UTF-8", warn = FALSE)
  cod <- sub(".*Comune: (\\d{6}) - .*", "\\1", l[1])
  nome <- sub(".*Comune: \\d{6} - (.*)\"$", "\\1", l[1])
  # primo blocco (tutte le cittadinanze): maschi righe 111-211, femmine 215-315
  blocco <- function(righe, sesso) {
    m <- do.call(rbind, strsplit(l[righe], ";", fixed = TRUE))
    stopifnot(m[1, 1] == "0", m[101, 1] == "100 e oltre")
    d <- as.data.table(m[, -1])
    setnames(d, as.character(2002:2019))
    d[, eta := 0:100]
    d <- melt(d, id.vars = "eta", variable.name = "anno", value.name = "pop",
              variable.factor = FALSE)
    d[, `:=`(anno = as.integer(anno), pop = as.integer(pop), sesso = sesso)]
  }
  stopifnot(grepl("Maschi", l[110]), grepl("Femmine", l[214]))
  r <- rbind(blocco(111:211, "M"), blocco(215:315, "F"))
  r[, `:=`(cod_comune = cod, comune = nome)]
}
ric <- rbindlist(lapply(list.files(dir_ric, full.names = TRUE), leggi_ric))
ric <- ric[anno %between% c(2011, 2019)]

# Raccordo ricostruzione / POSAS al 1° gennaio 2019
racc <- merge(ric[anno == 2019, .(ric = sum(pop)), by = sesso],
              posas[anno == 2019, .(posas = sum(pop)), by = sesso], by = "sesso")
print(racc)
ric <- ric[anno <= 2018]

pop <- rbind(ric, posas, use.names = TRUE)
pop[eta > 100, eta := 100L]

# Comuni -> provincia dei dati di mortalità -------------------------------------
comuni <- fread("input/derivati/comuni.csv", encoding = "Latin-1",
                colClasses = list(character = "cod_comune"))
norm <- function(s) gsub("[^a-z]", "", tolower(iconv(sub("/.*", "", s), "UTF-8", "ASCII//TRANSLIT")))
comuni[, chiave := paste(cod_reg, norm(enc2utf8(comune)))]
stopifnot(!anyDuplicated(comuni$chiave))

ana <- unique(pop[, .(cod_comune, comune)])
ana[, cod_prov_old := as.integer(substr(cod_comune, 1, 3))]
# regione dalla provincia di allora (province sarde storiche: 90-92, 95, 104-107, 111)
prov_reg <- unique(comuni[, .(cod_prov, cod_reg)])
prov_reg <- rbind(prov_reg, data.table(cod_prov = c(90:92, 95, 104:107, 111), cod_reg = 20L))
ana <- prov_reg[ana, on = .(cod_prov = cod_prov_old)]
setnames(ana, "cod_prov", "cod_prov_old")
ana[, chiave := paste(cod_reg, norm(comune))]

ana[comuni, on = "cod_comune", `:=`(cod_prov = i.cod_prov, metodo = "codice")]
ana[is.na(cod_prov), cod_prov := comuni$cod_prov[match(chiave, comuni$chiave)]]
ana[is.na(metodo) & !is.na(cod_prov), metodo := "nome"]
# comuni passati ad altra regione (Montecopiolo e Sassofeltrio, 2021): nome unico in Italia
comuni[, nome_n := norm(enc2utf8(comune))]
nomi_unici <- comuni[, .N, by = nome_n][N == 1, nome_n]
ana[is.na(cod_prov) & norm(comune) %in% nomi_unici,
    `:=`(cod_prov = comuni$cod_prov[match(norm(comune), comuni$nome_n)], metodo = "nome (altra regione)")]
ana[is.na(cod_prov) & cod_reg != 20, `:=`(cod_prov = cod_prov_old, metodo = "provincia di allora")]
cat("Raccordo comuni -> provincia:\n")
print(ana[, .N, by = metodo])
print(ana[metodo != "codice" & (cod_reg != 20 | is.na(cod_prov))][order(cod_comune)], nrows = 200)
stopifnot(!anyNA(ana$cod_prov), all(ana$cod_prov %in% comuni$cod_prov))

pop <- ana[, .(cod_comune, comune, cod_prov)][pop, on = .(cod_comune, comune)]
stopifnot(!anyNA(pop$cod_prov))

pop_sing <- pop[, .(pop = sum(pop)), by = .(cod_prov, anno, sesso, eta)]
setorder(pop_sing, cod_prov, anno, sesso, eta)
write_parquet(pop_sing, "input/derivati/popolazione_provincia_eta_singola.parquet")

pop_cl <- pop_sing[, .(pop = sum(pop)),
                   by = .(cod_prov, anno, sesso,
                          eta = fcase(eta <= 64, "0-64", eta <= 74, "65-74",
                                      eta <= 84, "75-84", default = "85+"))]
setorder(pop_cl, cod_prov, anno, sesso, eta)
write_parquet(pop_cl, "input/derivati/popolazione_provincia.parquet")

# Controlli
stopifnot(setequal(unique(pop_cl$cod_prov), unique(comuni$cod_prov)),
          pop_cl[, .N, by = anno][, all(N == uniqueN(comuni$cod_prov) * 8)])
print(dcast(pop_cl[, .(pop = sum(pop)), by = .(anno, eta)], anno ~ eta, value.var = "pop")[
  , totale := `0-64` + `65-74` + `75-84` + `85+`][])
