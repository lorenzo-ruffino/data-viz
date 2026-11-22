# Completezza dei dati provvisori di giugno 2026 e contesto della mortalità 2026.
#
# Input:  input/derivati/decessi_provincia.parquet, popolazione_provincia.parquet
# Output: output/qualita_*.csv
#
# 1. Fine giugno: rapporto tra media giornaliera 24-30 e 1-23 giugno, per anno;
#    rapporto tra ultimi 7 giorni e i 7 precedenti per ogni mese del 2026.
# 2. Attesi standardizzati: tassi per età e sesso di un periodo di riferimento
#    (2015-2019, 2023-2025) applicati alla popolazione al 1° gennaio dell'anno.
#    Rapporto osservati/attesi per mese (gennaio-giugno) e per settimana di giugno.

library(data.table)
library(arrow)

setwd("/Users/lorenzoruffino/Documents/Progetti/data-viz/Mortalità caldo 2026")

dec <- as.data.table(read_parquet("input/derivati/decessi_provincia.parquet"))
pop <- as.data.table(read_parquet("input/derivati/popolazione_provincia.parquet"))
dec[, `:=`(anno = year(data), mese = month(data), giorno = mday(data))]

naz <- dec[, .(n = sum(n)), by = .(data, anno, mese, giorno, eta, sesso)]
pop_naz <- pop[, .(pop = sum(pop)), by = .(anno, eta, sesso)]

# 1. Fine giugno ------------------------------------------------------------------
giu <- naz[mese == 6, .(n = sum(n)), by = .(anno, giorno)]
fine_giugno <- giu[, .(media_1_23 = mean(n[giorno <= 23]),
                       media_24_30 = mean(n[giorno >= 24])), by = anno]
fine_giugno[, rapporto := media_24_30 / media_1_23]
print(fine_giugno)

# ultimi 7 giorni / 7 giorni precedenti per mese, 2026 e media 2011-2025
gm <- naz[, .(n = sum(n)), by = .(anno, mese, giorno)]
gm[, ultimo := max(giorno), by = .(anno, mese)]
coda <- gm[, .(rap_coda = sum(n[giorno > ultimo - 7]) /
                 sum(n[giorno > ultimo - 14 & giorno <= ultimo - 7])), by = .(anno, mese)]
coda_conf <- merge(coda[anno == 2026, .(mese, rap_2026 = rap_coda)],
                   coda[anno <= 2025, .(rap_med = mean(rap_coda), rap_min = min(rap_coda),
                                        rap_max = max(rap_coda)), by = mese], by = "mese")
print(coda_conf)

# 2. Attesi standardizzati --------------------------------------------------------
# periodo: mesi 1-6 e settimane di giugno (giorni 1-7, 8-14, 15-21, 22-28, 29-30)
naz[, periodo := fifelse(mese < 6, sprintf("m%02d", mese),
                         sprintf("giu %s", fcase(giorno <= 7, "01-07", giorno <= 14, "08-14",
                                                 giorno <= 21, "15-21", giorno <= 28, "22-28",
                                                 default = "29-30")))]
per <- naz[mese <= 6, .(n = as.numeric(sum(n))), by = .(anno, periodo, eta, sesso)]
# 29 febbraio: rapporta febbraio a 28 giorni negli anni bisestili
per[periodo == "m02" & anno %% 4 == 0, n := n * 28 / 29]
per <- pop_naz[per, on = .(anno, eta, sesso)]
per[, tasso := n / pop]

attesi <- function(rif, nome) {
  t <- per[anno %in% rif, .(tasso = mean(tasso)), by = .(periodo, eta, sesso)]
  a <- t[pop_naz, on = .(eta, sesso), allow.cartesian = TRUE]
  a[, .(attesi = sum(tasso * pop)), by = .(anno, periodo)][, rif := nome]
}
att <- rbind(attesi(2015:2019, "2015-2019"), attesi(2023:2025, "2023-2025"))
oss <- per[, .(osservati = sum(n)), by = .(anno, periodo)]
oa <- att[oss, on = .(anno, periodo), nomatch = 0]
oa[, `:=`(rapporto = osservati / attesi, eccesso = osservati - attesi)]
setorder(oa, rif, periodo, anno)

cat("\nOsservati/attesi 2026 per periodo:\n")
print(dcast(oa[anno == 2026], periodo + osservati ~ rif, value.var = "rapporto"))
cat("\nOsservati/attesi (rif. 2015-2019) per anno e periodo:\n")
print(dcast(oa[rif == "2015-2019" & anno >= 2015], periodo ~ anno, value.var = "rapporto"),
      digits = 3)

# gennaio-maggio standardizzato per anno
genmag <- oa[grepl("^m0[1-5]", periodo), .(osservati = sum(osservati), attesi = sum(attesi)),
             by = .(anno, rif)][, rapporto := osservati / attesi]
print(dcast(genmag, anno ~ rif, value.var = "rapporto"), digits = 3)

# inverno: dicembre-marzo, osservati/attesi rif. 2015-2019
dic <- naz[mese == 12, .(n = sum(n)), by = .(anno = anno + 1L, eta, sesso)]
dic <- pop_naz[dic, on = .(anno, eta, sesso), nomatch = 0][, tasso := n / pop]
t_dic <- dic[anno %in% 2016:2019, .(tasso = mean(tasso)), by = .(eta, sesso)]
att_dic <- t_dic[pop_naz, on = .(eta, sesso)][, .(attesi = sum(tasso * pop)), by = anno]
inverno <- merge(dic[, .(osservati = sum(n)), by = anno], att_dic, by = "anno")
gm3 <- oa[rif == "2015-2019" & periodo %in% c("m01", "m02", "m03"),
          .(o = sum(osservati), a = sum(attesi)), by = anno]
inverno <- merge(inverno, gm3, by = "anno")[, .(anno, dic_mar_oss = osservati + o,
                                               dic_mar_att = attesi + a)]
inverno[, rapporto := dic_mar_oss / dic_mar_att]
print(inverno, digits = 3)

dir.create("output", showWarnings = FALSE)
fwrite(fine_giugno, "output/qualita_fine_giugno.csv")
fwrite(coda_conf, "output/qualita_coda_mesi_2026.csv")
fwrite(oa, "output/qualita_osservati_attesi.csv")
fwrite(inverno, "output/qualita_inverno.csv")
