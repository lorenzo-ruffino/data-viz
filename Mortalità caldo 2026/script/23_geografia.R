# Dose-risposta geografica: l'eccesso relativo per provincia è più alto dove
# ha fatto più caldo?
#
# Per ogni provincia e periodo (21-29 maggio, 17-30 giugno 2026):
#   - O/E da 22_eccesso.R (osservati corretti per completezza);
#   - anomalia di tmean, tmax e tmin rispetto alla media degli stessi giorni
#     2011-2025 (ERA5-Land, media pesata per popolazione);
#   - notti tropicali (tmin >= 20 °C) e giorni sopra la soglia di caldo
#     provinciale (90° percentile, da 21_baseline_attesi.R) nel periodo.
# Modello: quasi-Poisson  O ~ offset(log E) + anomalia, una osservazione per
# provincia (la sovradispersione tra province è stimata dal modello). Si riporta
# la variazione percentuale del rapporto O/E per 1 °C di anomalia (o per notte
# tropicale / giorno caldo in più), anche con effetti fissi di ripartizione
# (confronto solo tra province della stessa area) e con gli osservati senza
# correzione di completezza.
#
# Output (output/): geografia_province.csv, geografia_modelli.csv,
#                   grafico_dose_risposta_province.png

library(data.table)
library(arrow)
library(ggplot2)
library(showtext)
library(ggrepel)

setwd("/Users/lorenzoruffino/Documents/Progetti/data-viz/Mortalità caldo 2026")

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

PER <- data.table(periodo = c("21-29 maggio", "17-30 giugno", "1-16 giugno"),
                  inizio = as.IDate(c("2026-05-21", "2026-06-17", "2026-06-01")),
                  fine = as.IDate(c("2026-05-29", "2026-06-30", "2026-06-16")))

ecc <- fread("output/eccesso_province.csv")
tp <- as.data.table(read_parquet("input/derivati/giorni_caldi_provincia.parquet"))
tp[, `:=`(data = as.IDate(data), md = format(data, "%m-%d"), anno = year(data))]
comuni <- fread("input/derivati/comuni.csv", encoding = "Latin-1")
prov <- unique(comuni[, .(cod_prov, cod_reg)])
prov[, rip := fcase(cod_reg %in% c(1, 2, 3, 7), "Nord-Ovest", cod_reg %in% c(4, 5, 6, 8), "Nord-Est",
                    cod_reg %in% 9:12, "Centro", cod_reg %in% 13:18, "Sud", default = "Isole")]

temp <- rbindlist(lapply(seq_len(nrow(PER)), function(p) {
  md0 <- format(PER$inizio[p], "%m-%d"); md1 <- format(PER$fine[p], "%m-%d")
  x <- tp[md >= md0 & md <= md1 & anno %between% c(2011, 2026)]
  x[, .(tmean = mean(tmean[anno == 2026]), tmax = mean(tmax[anno == 2026]),
        tmin = mean(tmin[anno == 2026]),
        an_tmean = mean(tmean[anno == 2026]) - mean(tmean[anno < 2026]),
        an_tmax = mean(tmax[anno == 2026]) - mean(tmax[anno < 2026]),
        an_tmin = mean(tmin[anno == 2026]) - mean(tmin[anno < 2026]),
        notti_tropicali = sum(tmin[anno == 2026] >= 20),
        notti_tropicali_clim = sum(tmin[anno < 2026] >= 20) / 15,
        giorni_caldi = sum(caldo[anno == 2026])),
    by = cod_prov][, periodo := PER$periodo[p]]
}))

geo <- merge(ecc[periodo %in% PER$periodo], temp, by = c("cod_prov", "periodo"))
geo <- prov[geo, on = "cod_prov"]
geo[, oe := oss / atteso]
stopifnot(geo[, .N, by = periodo]$N == 110)
fwrite(geo[, lapply(.SD, function(v) if (is.double(v)) round(v, 3) else v)],
       "output/geografia_province.csv")

stima <- function(d, var, effetti_fissi = FALSE, grezzi = FALSE) {
  d <- copy(d)
  d[, y := if (grezzi) oss_grezzi else oss]
  # la variabile dipendente corretta non è intera: quasi-Poisson lo ammette
  f <- if (effetti_fissi) as.formula(paste("y ~ rip +", var)) else as.formula(paste("y ~", var))
  m <- suppressWarnings(glm(f, offset = log(atteso), family = quasipoisson(), data = d))
  cf <- summary(m)$coefficients
  b <- cf[var, "Estimate"]; se <- cf[var, "Std. Error"]
  data.table(variabile = var, effetti_fissi_ripartizione = effetti_fissi,
             osservati = if (grezzi) "grezzi" else "corretti",
             var_perc = 100 * (exp(b) - 1), inf = 100 * (exp(b - 1.96 * se) - 1),
             sup = 100 * (exp(b + 1.96 * se) - 1), p_valore = cf[var, 4],
             dispersione = summary(m)$dispersion,
             oe_a_zero = if (!effetti_fissi) exp(coef(m)[[1]]) else NA_real_)
}

modelli <- rbindlist(lapply(PER$periodo, function(pp) {
  d <- geo[periodo == pp]
  rbindlist(list(
    stima(d, "an_tmean"), stima(d, "an_tmax"), stima(d, "an_tmin"),
    stima(d, "notti_tropicali"), stima(d, "giorni_caldi"),
    stima(d, "an_tmean", effetti_fissi = TRUE), stima(d, "an_tmax", effetti_fissi = TRUE),
    stima(d, "an_tmin", effetti_fissi = TRUE),
    stima(d, "an_tmean", grezzi = TRUE)))[, periodo := pp]
}))
setcolorder(modelli, "periodo")
fwrite(modelli[, lapply(.SD, function(v) if (is.double(v)) signif(v, 4) else v)],
       "output/geografia_modelli.csv")
print(modelli, digits = 3)

cat("\nCorrelazioni (pesate per l'atteso) tra O/E e anomalie:\n")
print(geo[, .(r_tmean = cov.wt(cbind(oe, an_tmean), wt = atteso, cor = TRUE)$cor[1, 2],
              r_tmax = cov.wt(cbind(oe, an_tmax), wt = atteso, cor = TRUE)$cor[1, 2],
              r_tmin = cov.wt(cbind(oe, an_tmin), wt = atteso, cor = TRUE)$cor[1, 2]),
          by = periodo], digits = 2)

cat("\nO/E per terzili di anomalia di tmean:\n")
geo[, terzile := cut(an_tmean, quantile(an_tmean, 0:3 / 3), include.lowest = TRUE,
                     labels = c("meno calde", "intermedie", "più calde")), by = periodo]
print(geo[, .(province = .N, an_media = round(weighted.mean(an_tmean, atteso), 2),
              oss = round(sum(oss)), atteso = round(sum(atteso)),
              oe = round(sum(oss) / sum(atteso), 3)), keyby = .(periodo, terzile)])

cat("\nPer ripartizione:\n")
print(geo[, .(an_media = round(weighted.mean(an_tmean, atteso), 2),
              oe = round(sum(oss) / sum(atteso), 3)), keyby = .(periodo, rip)])

# ---- Grafico ---------------------------------------------------------------------

g <- geo[periodo %in% c("21-29 maggio", "17-30 giugno")]
g[, periodo := factor(periodo, levels = c("21-29 maggio", "17-30 giugno"))]
rette <- g[, {
  m <- glm(oss ~ an_tmean, offset = log(atteso), family = quasipoisson())
  xs <- seq(min(an_tmean), max(an_tmean), length.out = 50)
  list(x = xs, y = 100 * (exp(coef(m)[1] + coef(m)[2] * xs) - 1))
}, by = periodo]
etich <- g[order(-atteso), head(.SD, 6), by = periodo]

p <- ggplot(g, aes(an_tmean, 100 * (oe - 1))) +
  geom_hline(yintercept = 0, color = "#1C1C1C", linewidth = 0.3) +
  geom_point(aes(size = atteso), shape = 21, fill = "#F12938", color = "white",
             alpha = 0.75, stroke = 0.3) +
  geom_line(data = rette, aes(x, y), color = "#1C1C1C", linewidth = 0.6) +
  geom_text_repel(data = etich, aes(label = provincia), size = 3, family = "Source Sans Pro",
                  color = "#1C1C1C", min.segment.length = 0, segment.size = 0.2,
                  box.padding = 0.5, seed = 1) +
  facet_wrap(~periodo, scales = "free_x") +
  scale_size_area(max_size = 9, guide = "none") +
  scale_y_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "%")) +
  scale_x_continuous(labels = scales::label_number(decimal.mark = ",", style_positive = "plus",
                                                    suffix = "°")) +
  labs(title = "Dove ha fatto più caldo i morti in più sono stati di più",
       subtitle = paste0("Scarto tra decessi osservati e attesi per provincia e anomalia della temperatura media\n",
                         "rispetto agli stessi giorni 2011-2025; area dei cerchi proporzionale ai decessi attesi"),
       x = "Anomalia della temperatura media", y = NULL,
       caption = "Elaborazione di Lorenzo Ruffino su dati Istat e Copernicus ERA5-Land") +
  theme_minimal(base_family = "Source Sans Pro") +
  theme(text = element_text(color = "#1C1C1C"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#E5E5E5", linewidth = 0.3),
        strip.text = element_text(size = 12, hjust = 0),
        axis.text = element_text(size = 10, color = "#1C1C1C"),
        axis.title.x = element_text(size = 10, margin = margin(t = 6)),
        plot.title.position = "plot",
        plot.title = element_text(size = 17.5),
        plot.subtitle = element_text(size = 11, lineheight = 1.3, margin = margin(b = 10)),
        plot.caption = element_text(size = 10, hjust = 1, margin = margin(t = 10)),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(12, 14, 10, 12))

ggsave("output/grafico_dose_risposta_province.png", p, width = 9, height = 5.6, dpi = 220)
