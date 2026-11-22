# Decessi giornalieri Istat (comuni, 2011 - 30 giugno 2026) in formato lungo.
#
# Input:  input/istat/comuni_giornaliero_30giugno26.csv (Istat, dati provvisori 2025-2026)
# Output (input/derivati/, parquet):
#   - decessi_comune.parquet    comune x data x classe d'età (4) x sesso, solo righe > 0
#   - decessi_provincia.parquet provincia x data x classe d'età x sesso, griglia completa
#   - comuni.csv                anagrafica comuni (codici, nomi, provincia, regione)
#
# Classi d'età: 0-64, 65-74, 75-84, 85+ (CL_ETA Istat 0-13, 14-15, 16-17, 18-21).

library(data.table)
library(arrow)

setwd("/Users/lorenzoruffino/Documents/Progetti/data-viz/Mortalità caldo 2026")
dir.create("input/derivati", showWarnings = FALSE)

d <- fread("input/istat/comuni_giornaliero_30giugno26.csv",
           encoding = "Latin-1", colClasses = "character")

comuni <- unique(d[, .(cod_comune = COD_PROVCOM, comune = NOME_COMUNE,
                       cod_prov = as.integer(PROV), provincia = NOME_PROVINCIA,
                       cod_reg = as.integer(REG), regione = NOME_REGIONE)])
stopifnot(!anyDuplicated(comuni$cod_comune))
fwrite(comuni, "input/derivati/comuni.csv")

d[, cl := as.integer(CL_ETA)]
d[, eta := fcase(cl <= 13, "0-64", cl <= 15, "65-74", cl <= 17, "75-84", default = "85+")]

val_cols <- grep("^[MF]_\\d{2}$", names(d), value = TRUE)
long <- melt(d[, c("COD_PROVCOM", "PROV", "GE", "eta", val_cols), with = FALSE],
             id.vars = c("COD_PROVCOM", "PROV", "GE", "eta"),
             variable.name = "var", value.name = "n", variable.factor = FALSE)
rm(d); gc()

long <- long[n != "n.d." & n != "0"]
long[, `:=`(sesso = substr(var, 1, 1),
            anno  = 2000L + as.integer(substr(var, 3, 4)),
            n     = as.integer(n))]
long[, data := as.IDate(sprintf("%d-%s-%s", anno, substr(GE, 1, 2), substr(GE, 3, 4)))]
# 29 febbraio negli anni non bisestili: deve essere sempre zero
stopifnot(long[is.na(data), sum(n)] == 0)
long <- long[!is.na(data) & data <= as.IDate("2026-06-30")]

dec_com <- long[, .(n = sum(n)), by = .(cod_comune = COD_PROVCOM, data, eta, sesso)]
write_parquet(dec_com, "input/derivati/decessi_comune.parquet")

dec_prov <- long[, .(n = sum(n)), by = .(cod_prov = as.integer(PROV), data, eta, sesso)]
griglia <- CJ(cod_prov = sort(unique(comuni$cod_prov)),
              data = seq(as.IDate("2011-01-01"), as.IDate("2026-06-30"), by = 1),
              eta = c("0-64", "65-74", "75-84", "85+"), sesso = c("M", "F"))
dec_prov <- dec_prov[griglia, on = .(cod_prov, data, eta, sesso)]
dec_prov[is.na(n), n := 0L]
write_parquet(dec_prov, "input/derivati/decessi_provincia.parquet")

# Controlli
tot <- dec_prov[, .(n = sum(n)), by = .(anno = year(data))]
print(tot)
cat("Giugno per anno:\n")
print(dec_prov[month(data) == 6, .(n = sum(n)), by = .(anno = year(data))])
