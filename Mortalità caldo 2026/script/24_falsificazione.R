# Test di falsificazione della baseline.
#
# Se l'eccesso del 2026 è dovuto al caldo, la stessa procedura applicata a periodi
# senza caldo anomalo deve dare un eccesso vicino a zero. Si usano le previsioni
# leave-one-year-out di 21_baseline_attesi.R (ogni anno previsto da un modello
# stimato sugli altri anni) per:
#   a) le stesse finestre di calendario del 2026 negli anni 2014-2019 e 2023-2025:
#      O/E nazionale e anomalia di temperatura della finestra;
#   b) la relazione O/E - anomalia tra province in quegli anni (stesso modello
#      quasi-Poisson di 23_geografia.R con effetti fissi d'anno): l'O/E atteso ad
#      anomalia zero deve essere ~1 e la pendenza confrontabile con il 2026;
#   c) il 2026 prima del caldo (1 aprile-14 maggio) e le province meno calde.
#
# Output (output/): falsificazione_anni.csv, falsificazione_province.csv,
#                   falsificazione_sintesi.csv

library(data.table)
library(arrow)

setwd("/Users/lorenzoruffino/Documents/Progetti/data-viz/Mortalità caldo 2026")

FIN <- data.table(periodo = c("21-29 maggio", "1-16 giugno", "17-30 giugno", "giugno",
                              "15 maggio-30 giugno"),
                  md0 = c("05-21", "06-01", "06-17", "06-01", "05-15"),
                  md1 = c("05-29", "06-16", "06-30", "06-30", "06-30"))

bt <- as.data.table(read_parquet("input/derivati/backtest_strato_giorno.parquet"))
bt <- bt[startsWith(prova, "loyo")]
bt[, md := format(data, "%m-%d")]

tp <- as.data.table(read_parquet("input/derivati/giorni_caldi_provincia.parquet"))
tp[, `:=`(anno = year(data), md = format(data, "%m-%d"))]
popw <- as.data.table(read_parquet("input/derivati/popolazione_provincia.parquet"))[
  anno == 2023, .(pop = sum(pop)), by = cod_prov]

# anomalie per provincia, anno e finestra (rispetto alla media 2011-2025)
anom <- rbindlist(lapply(seq_len(nrow(FIN)), function(f) {
  x <- tp[md >= FIN$md0[f] & md <= FIN$md1[f] & anno <= 2026,
          .(tmean = mean(tmean), caldi = sum(caldo), giorni = .N), by = .(cod_prov, anno)]
  x[, an_tmean := tmean - mean(tmean[anno <= 2025]), by = cod_prov]
  x[, periodo := FIN$periodo[f]]
}))

oe_prov <- rbindlist(lapply(seq_len(nrow(FIN)), function(f) {
  bt[md >= FIN$md0[f] & md <= FIN$md1[f],
     .(oss = sum(oss), atteso = sum(atteso)), by = .(cod_prov, anno)][, periodo := FIN$periodo[f]]
}))
oe_prov <- anom[oe_prov, on = .(cod_prov, anno, periodo)]

# ---- a) Italia, stesse finestre negli anni passati ------------------------------------

anni <- popw[oe_prov, on = "cod_prov"][
  , .(oss = sum(oss), atteso = sum(atteso), an_tmean = weighted.mean(an_tmean, pop),
      quota_giorni_caldi = sum(caldi * pop) / sum(giorni * pop)), by = .(periodo, anno)]
anni[, `:=`(oe = oss / atteso, eccesso = oss - atteso)]

# 2026 dalle tabelle di 22_eccesso.R (osservati corretti) per confronto
ita <- fread("output/eccesso_italia_periodi.csv")[baseline == "trend"]
a26 <- popw[anom[anno == 2026], on = "cod_prov"][
  , .(an_tmean = weighted.mean(an_tmean, pop), quota_giorni_caldi = sum(caldi * pop) / sum(giorni * pop)),
  by = periodo]
a26 <- ita[, .(periodo, oss, atteso)][a26, on = "periodo"][
  , `:=`(anno = 2026L, oe = oss / atteso, eccesso = oss - atteso)]
anni <- rbind(anni, a26, use.names = TRUE)
fwrite(anni[order(periodo, anno), lapply(.SD, function(v) if (is.double(v)) round(v, 3) else v)],
       "output/falsificazione_anni.csv")
print(dcast(anni, anno ~ periodo, value.var = "oe"), digits = 3)
print(dcast(anni, anno ~ periodo, value.var = "an_tmean"), digits = 2)

# finestre "fredde" (anomalia nazionale < 1 °C) negli anni passati
fredde <- anni[anno < 2026 & an_tmean < 1]
cat("\nFinestre degli anni passati con anomalia < 1 °C:", nrow(fredde),
    " O/E medio:", round(fredde[, sum(oss) / sum(atteso)], 3),
    " (min", round(min(fredde$oe), 3), "max", round(max(fredde$oe), 3), ")\n")
calde <- anni[anno < 2026 & an_tmean >= 2]
cat("Finestre degli anni passati con anomalia >= 2 °C:", nrow(calde),
    " O/E medio:", round(calde[, sum(oss) / sum(atteso)], 3), "\n")

# relazione tra anni: O/E nazionale e anomalia (17-30 giugno e 21-29 maggio insieme)
m_anni <- glm(oss ~ an_tmean + periodo, offset = log(atteso), family = quasipoisson(),
              data = anni[anno < 2026 & periodo %in% c("21-29 maggio", "17-30 giugno", "1-16 giugno")])
cat("Tra anni (2014-2025): variazione O/E per °C =",
    round(100 * (exp(coef(m_anni)[["an_tmean"]]) - 1), 2), "%\n")

# ---- b) Province negli anni passati ----------------------------------------------------

stima <- function(d) {
  m <- suppressWarnings(glm(oss ~ factor(anno) + an_tmean, offset = log(atteso),
                            family = quasipoisson(), data = d))
  cf <- summary(m)$coefficients["an_tmean", ]
  # O/E ad anomalia zero, media degli effetti d'anno
  nd <- data.table(anno = unique(d$anno), an_tmean = 0, atteso = 1)
  oe0 <- mean(predict(m, newdata = nd, type = "response"))
  data.table(var_perc = 100 * (exp(cf[1]) - 1), inf = 100 * (exp(cf[1] - 1.96 * cf[2]) - 1),
             sup = 100 * (exp(cf[1] + 1.96 * cf[2]) - 1), oe_a_zero = oe0,
             dispersione = summary(m)$dispersion, n = nrow(d))
}
prov_bt <- oe_prov[anno < 2026, stima(.SD), by = periodo]
prov_bt[, campione := "2014-2019, 2023-2025 (fuori campione)"]
geo26 <- fread("output/geografia_modelli.csv")[variabile == "an_tmean" & !effetti_fissi_ripartizione &
                                                 osservati == "corretti"]
prov_26 <- geo26[, .(periodo, var_perc, inf, sup, oe_a_zero, dispersione, n = 110L,
                     campione = "2026")]
fp <- rbind(prov_bt, prov_26, fill = TRUE)
fwrite(fp[, lapply(.SD, function(v) if (is.double(v)) round(v, 3) else v)],
       "output/falsificazione_province.csv")
cat("\nDose-risposta tra province, anni passati vs 2026:\n")
print(fp[order(periodo, campione)], digits = 3)

# province-anno con anomalia vicina a zero (|an| < 0,5 °C) nelle finestre passate
cat("\nProvince-anno con anomalia tra -0,5 e +0,5 °C, O/E:\n")
print(oe_prov[anno < 2026 & abs(an_tmean) < 0.5,
              .(n = .N, oe = round(sum(oss) / sum(atteso), 3)), by = periodo])

# ---- c) 2026: prima del caldo e province meno calde -----------------------------------

pre <- ita[periodo == "1 aprile-14 maggio"]
geo <- fread("output/geografia_province.csv")
fredde26 <- geo[, .SD[an_tmean <= quantile(an_tmean, 1 / 3)], by = periodo][
  , .(province = .N, oe = sum(oss) / sum(atteso)), by = periodo]
sintesi <- rbind(
  data.table(test = "2026, 1 aprile-14 maggio (prima del caldo, fuori campione)",
             oe = pre$oss / pre$atteso, inf = 1 + pre$ecc_perc_inf / 100, sup = 1 + pre$ecc_perc_sup / 100),
  fredde26[, .(test = paste0("2026, ", periodo, ", terzo di province meno calde"), oe, inf = NA, sup = NA)],
  data.table(test = "Anni passati, finestre con anomalia nazionale < 1 °C",
             oe = fredde[, sum(oss) / sum(atteso)], inf = min(fredde$oe), sup = max(fredde$oe)),
  data.table(test = "Anni passati, finestre con anomalia nazionale >= 2 °C",
             oe = calde[, sum(oss) / sum(atteso)], inf = min(calde$oe), sup = max(calde$oe)),
  fill = TRUE)
fwrite(sintesi[, lapply(.SD, function(v) if (is.double(v)) round(v, 3) else v)],
       "output/falsificazione_sintesi.csv")
print(sintesi, digits = 3)
