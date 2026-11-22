#!/usr/bin/env Rscript
# ============================================================================
# Media dei sondaggi per le elezioni generali spagnole del 29 novembre 2026.
#
# Il modello e' in utilities/R/media_sondaggi.R: media bayesiana sul modello
# di doppiacifra (passeggiata casuale giornaliera, effetti istituto robusti
# ai fuori scala, rumore extra stimato, peso ridotto a chi pubblica molto).
#
# Sondaggi: tutti quelli nazionali dal gennaio 2025 raccolti sulla voce di
# Wikipedia, tranne
#   - il CIS, l'istituto pubblico, che da' il PP sette punti sotto e il PSOE
#     cinque sopra tutti gli altri, e le ristime di altri istituti sui suoi
#     microdati ("CIS (Target Point)" e simili);
#   - KeyData/Publico, che e' una media di sondaggi altrui;
#   - i sondaggi interni diffusi dai partiti (istituto "PSOE", "Vox", "Sumar").
#
# Serie: PP, PSOE, Vox, Frente Amplio, Podemos e Se Acabo' La Fiesta. Il
# Frente Amplio e' l'alleanza nata a ottobre 2026 dai partiti di Sumar: la
# serie e' Sumar fino a ottobre, poi Frente Amplio insieme a Compromis-Front
# Valencia' (che nel 2023 era dentro Sumar e che meta' degli istituti somma
# gia' al Frente Amplio). In piu' una serie per i sei partiti nazionalisti e
# regionalisti che nel 2023 hanno votato l'investitura di Sanchez (ERC, Junts,
# EH Bildu, PNV, BNG, Coalicion Canaria): da soli valgono fra lo 0,4 e il 2,5
# per cento, insieme oltre il 7. Entra un sondaggio solo se li da' tutti e sei.
#
# Seggi: gli istituti spagnoli pubblicano quasi sempre anche la stima dei
# seggi (350 al Congresso, maggioranza a 176). La seconda media e' sulla somma
# dei seggi dei due blocchi nazionali, sondaggio per sondaggio:
#   destra   = PP + Vox (con UPN, alleato del PP in Navarra)
#   sinistra = PSOE e alleati, cioe' la maggioranza che ha eletto Sanchez nel
#              2023 e che i giornali spagnoli (El Confidencial, El Pais)
#              contrappongono a PP e Vox: PSOE + Frente Amplio (Sumar fino a
#              ottobre, poi con Compromis-Front Valencia') + Podemos + ERC +
#              Junts + EH Bildu + PNV + BNG + Coalicion Canaria
# Restano fuori dai due blocchi Se Acabo' La Fiesta, Adelante Andalucia e
# Alianca Catalana. Entra un sondaggio solo se da' i seggi di tutti i partiti
# del blocco (Podemos e UPN, se mancano, contano zero). Il modello e' lo
# stesso, applicato alla quota di seggi su 350.
#
# Output: output/medie_giornaliere.csv, output/sondaggi_validi.csv,
#         output/effetti_istituto.csv, output/parametri_modello.csv,
#         output/medie_seggi.csv, output/sondaggi_seggi.csv
# ============================================================================

suppressPackageStartupMessages({ library(data.table) })
source("/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities/R/media_sondaggi.R")

BASE   <- dirname(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))))
INPUT  <- file.path(BASE, "input")
OUTPUT <- file.path(BASE, "output")

VOTO   <- as.Date("2026-11-29")
INIZIO <- as.Date("2025-01-01")

NOMI <- c(pp = "PP", psoe = "PSOE", vox = "Vox", fa = "Frente Amplio",
          podemos = "Podemos", salf = "Se Acabó La Fiesta",
          naz = "Nazionalisti e regionalisti")
NON_SONDAGGI <- c("CIS", "KeyData", "PSOE", "Vox", "Sumar")
SEGGI <- 350
NOMI_BLOCCHI <- c(destra = "PP e Vox", sinistra = "PSOE e alleati")

# ── Sondaggi ─────────────────────────────────────────────────────────────────
g <- fread(file.path(INPUT, "sondaggi_grezzi.csv"), na.strings = c("", "NA"),
           encoding = "UTF-8")
g[, data := as.Date(data_media)]
g <- g[ristima_cis == 0 & !istituto %in% NON_SONDAGGI]
largo <- dcast(g, id + istituto + committente + data + data_fine + n ~ lista,
               value.var = "pct")
largo[, fa := fcase(!is.na(Sumar), Sumar,
                    !is.na(`FA+FV`), `FA+FV`,
                    !is.na(FA) & !is.na(FV), FA + FV)]
largo[, naz := ERC + Junts + `EH Bildu` + PNV + BNG + CCa]
d <- melt(largo[, .(id, istituto, data, n, pp = PP, psoe = PSOE, vox = Vox, fa,
                    podemos = Podemos, salf = SALF, naz)],
          id.vars = c("id", "istituto", "data", "n"),
          variable.name = "serie", value.name = "y", na.rm = TRUE)
d[, serie := as.character(serie)]
FINE <- max(as.Date(g$data_fine))
cat(sprintf("%d sondaggi di %d istituti, dal %s al %s\n", uniqueN(d$id),
            uniqueN(d$istituto), format(min(d$data)), format(FINE)))

# ── Modello ──────────────────────────────────────────────────────────────────
fit <- media_sondaggi(d, fine = FINE, inizio = INIZIO)

media <- fit$media[, lapply(.SD, round, 3), by = .(data, serie),
                   .SDcols = c("media", "lo90", "hi90", "lo50", "hi50", "sd")]
media[, nome := NOMI[serie]]
media[, giorni_al_voto := as.integer(VOTO - data)]
fwrite(media, file.path(OUTPUT, "medie_giornaliere.csv"))

sond <- fit$sondaggi[, .(serie, nome = NOMI[serie], istituto, data, pct = y, n,
                         peso = round(peso, 3), effetto_istituto = round(effetto, 2),
                         corretto = round(corretto, 2))]
fwrite(sond[order(data, serie)], file.path(OUTPUT, "sondaggi_validi.csv"))

he <- dcast(fit$effetti, istituto ~ serie, value.var = "effetto")
he <- merge(unique(d[, .(id, istituto)])[, .(sondaggi = .N), by = istituto], he,
            by = "istituto")
setcolorder(he, c("istituto", "sondaggi", names(NOMI)))
he <- he[order(-sondaggi)][, (names(NOMI)) := lapply(.SD, round, 2), .SDcols = names(NOMI)]
fwrite(he, file.path(OUTPUT, "effetti_istituto.csv"))
fwrite(fit$parametri[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)],
       file.path(OUTPUT, "parametri_modello.csv"))

# ── Seggi per blocco ─────────────────────────────────────────────────────────
zero <- function(x) fifelse(is.na(x), 0, x)
ls <- dcast(g[!is.na(seggi)], id + istituto + data + n ~ lista, value.var = "seggi")
ls[, fa := fcase(!is.na(Sumar), Sumar,
                 !is.na(`FA+FV`), `FA+FV`,
                 !is.na(FA), FA + zero(FV))]
ls[, destra := PP + Vox + zero(UPN)]
ls[, sinistra := PSOE + fa + zero(Podemos) + ERC + Junts + `EH Bildu` + PNV + BNG + CCa]
ds <- melt(ls[, .(id, istituto, data, n, destra, sinistra)],
           id.vars = c("id", "istituto", "data", "n"),
           variable.name = "serie", value.name = "seggi", na.rm = TRUE)
ds[, serie := as.character(serie)]
ds[, y := seggi / SEGGI * 100]
cat(sprintf("\nSeggi: %d sondaggi di %d istituti\n", uniqueN(ds$id), uniqueN(ds$istituto)))

fit_s <- media_sondaggi(ds, fine = FINE, inizio = INIZIO)
col_s <- c("media", "lo90", "hi90", "lo50", "hi50", "sd")
media_s <- copy(fit_s$media)[, (col_s) := lapply(.SD, function(x) round(x * SEGGI / 100, 2)),
                             .SDcols = col_s]
media_s[, nome := NOMI_BLOCCHI[serie]]
fwrite(media_s, file.path(OUTPUT, "medie_seggi.csv"))
sond_s <- fit_s$sondaggi[, .(serie, nome = NOMI_BLOCCHI[serie], istituto, data,
                             seggi = round(y * SEGGI / 100, 1), peso = round(peso, 3),
                             effetto_istituto = round(effetto * SEGGI / 100, 1))]
fwrite(sond_s[order(data, serie)], file.path(OUTPUT, "sondaggi_seggi.csv"))

# ── Controlli ────────────────────────────────────────────────────────────────
cat("\n-- Parametri (tau: passo giornaliero; sigma: rumore extra; omega: sd effetti istituto) --\n")
print(fit$parametri[, .(serie, tau = round(tau, 3), sigma = round(sigma, 2),
                        omega = round(omega, 2), rhat_tau = round(tau_rhat, 3),
                        rhat_sigma = round(sigma_rhat, 3), rhat_omega = round(omega_rhat, 3),
                        rhat_finale = round(rhat_finale, 3))], row.names = FALSE)

cat("\n-- Media al", format(FINE), "(banda 90%) --\n")
u <- media[data == FINE][order(-media)]
print(u[, .(nome, media = round(media, 1), lo90 = round(lo90, 1), hi90 = round(hi90, 1))],
      row.names = FALSE)
cat(sprintf("Somma delle serie: %.1f\n", sum(u$media)))

f <- fit$finale
cat(sprintf("\nP(PP > PSOE) = %.0f%%   vantaggio PP %.1f (90%%: %.1f, %.1f)\n",
            100 * mean(f[, "pp"] > f[, "psoe"]), mean(f[, "pp"] - f[, "psoe"]),
            quantile(f[, "pp"] - f[, "psoe"], 0.05), quantile(f[, "pp"] - f[, "psoe"], 0.95)))
cat(sprintf("PP + Vox = %.1f (90%%: %.1f, %.1f)\n", mean(f[, "pp"] + f[, "vox"]),
            quantile(f[, "pp"] + f[, "vox"], 0.05), quantile(f[, "pp"] + f[, "vox"], 0.95)))

cat("\n-- Effetti istituto (punti; + = l'istituto sovrastima la lista) --\n")
print(he[sondaggi >= 5], row.names = FALSE)

cat("\n-- Tappe --\n")
tappe <- media[data %in% c(as.Date(c("2025-01-15", "2025-07-01", "2026-01-01", "2026-07-01",
                                     "2026-10-01", "2026-11-01")), FINE)]
print(dcast(tappe, data ~ serie, value.var = "media")[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 1) else x)],
      row.names = FALSE)

cat("\n-- Confronto: media semplice degli ultimi 10 giorni, un sondaggio per istituto --\n")
ult <- d[data >= FINE - 10][order(-data)][, .SD[1], by = .(istituto, serie)]
print(ult[, .(semplice = round(mean(y), 1), istituti = .N), by = serie], row.names = FALSE)

cat("\n-- Seggi per blocco al", format(FINE), "(banda 90%) --\n")
print(media_s[data == FINE, .(nome, media = round(media), lo90 = round(lo90), hi90 = round(hi90))],
      row.names = FALSE)
fs <- fit_s$finale * SEGGI / 100
cat(sprintf("P(PP + Vox >= 176 seggi) = %.0f%%\n", 100 * mean(fs[, "destra"] >= 176)))
print(fit_s$parametri[, .(serie, tau = round(tau * 3.5, 2), sigma = round(sigma * 3.5, 1),
                          omega = round(omega * 3.5, 1), rhat_finale = round(rhat_finale, 3))],
      row.names = FALSE)
cat("Effetti istituto sui seggi (istituti con almeno 8 sondaggi):\n")
print(dcast(fit_s$effetti[sondaggi >= 8], istituto ~ serie, value.var = "effetto")[
  , .(istituto, destra = round(destra * 3.5, 1), sinistra = round(sinistra * 3.5, 1))],
  row.names = FALSE)
ult_s <- ds[data >= FINE - 10][order(-data)][, .SD[1], by = .(istituto, serie)]
cat("Media semplice degli ultimi 10 giorni, un sondaggio per istituto:\n")
print(ult_s[, .(semplice = round(mean(seggi), 1), istituti = .N), by = serie], row.names = FALSE)
