#!/usr/bin/env Rscript
# ============================================================================
# Media dei sondaggi sul primo turno delle presidenziali francesi del
# 18 aprile 2027.
#
# Il modello e' in utilities/R/media_sondaggi.R: media bayesiana sul modello
# di doppiacifra (passeggiata casuale giornaliera, effetti istituto robusti
# ai fuori scala, rumore extra stimato, peso ridotto a chi pubblica molto).
#
# Sondaggi: per ogni rilevazione lo scenario con il campo di oggi, scelto da
# 01_scarica_sondaggi.py (colonna "scelto"). Le ipotesi (senza Attal, con
# Hollande, con Lagarde...) restano fuori. Si parte dal 7 luglio 2026, quando
# la Corte d'appello di Parigi ha riaperto la strada alla candidatura di
# Marine Le Pen: prima il candidato del Rassemblement National nei sondaggi
# era Jordan Bardella (fase "bardella" di sondaggi_grezzi.csv), con soli
# cinque sondaggi a campo completo fra febbraio e giugno, e resta fuori.
#
# Lisnard e Riner entrano nella media dal primo sondaggio che li prova nella
# lista completa.
#
# Output: output/medie_giornaliere.csv, output/sondaggi_validi.csv,
#         output/effetti_istituto.csv, output/parametri_modello.csv
# ============================================================================

suppressPackageStartupMessages({ library(data.table) })
source("/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities/R/media_sondaggi.R")

BASE   <- dirname(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))))
INPUT  <- file.path(BASE, "input")
OUTPUT <- file.path(BASE, "output")

VOTO   <- as.Date("2027-04-18")
INIZIO <- as.Date("2026-07-07")

CHIAVI <- c("Marine Le Pen" = "lepen", "Jean-Luc Mélenchon" = "melenchon",
            "Raphaël Glucksmann" = "glucksmann", "Édouard Philippe" = "philippe",
            "David Lisnard" = "lisnard", "Bruno Retailleau" = "retailleau",
            "Gabriel Attal" = "attal", "Éric Zemmour" = "zemmour",
            "Marine Tondelier" = "tondelier", "Teddy Riner" = "riner",
            "Fabien Roussel" = "roussel", "Nicolas Dupont-Aignan" = "dupontaignan")
NOMI <- setNames(sub("^.* (?=[^ ]+$)", "", names(CHIAVI), perl = TRUE), CHIAVI)
NOMI[c("lepen", "dupontaignan")] <- c("Le Pen", "Dupont-Aignan")

# ── Sondaggi ─────────────────────────────────────────────────────────────────
g <- fread(file.path(INPUT, "sondaggi_grezzi.csv"), na.strings = c("", "NA"),
           encoding = "UTF-8")
cat(sprintf("%d sondaggi sulla voce dal 7 luglio, %d con lo scenario del campo di oggi\n",
            uniqueN(g[fase == "lepen", id]), uniqueN(g[scelto == 1 & fase == "lepen", id])))
g <- g[scelto == 1 & fase == "lepen" & candidato %in% names(CHIAVI)]
d <- g[, .(id, istituto, data = as.Date(data_media), n, serie = CHIAVI[candidato],
           y = pct)]
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
col <- intersect(CHIAVI, names(he))
setcolorder(he, c("istituto", "sondaggi", col))
he <- he[order(-sondaggi)][, (col) := lapply(.SD, round, 2), .SDcols = col]
fwrite(he, file.path(OUTPUT, "effetti_istituto.csv"))
fwrite(fit$parametri[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)],
       file.path(OUTPUT, "parametri_modello.csv"))

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
cat(sprintf("Somma: %.1f\n", sum(u$media)))

f <- fit$finale
secondo <- apply(f, 1, function(x) names(x)[order(-x)[2]])
cat("\n-- Chi e' secondo dietro Le Pen (quota delle estrazioni) --\n")
print(round(100 * prop.table(table(secondo))))
cat(sprintf("Vantaggio di Le Pen su Mélenchon: %.1f (90%%: %.1f, %.1f)\n",
            mean(f[, "lepen"] - f[, "melenchon"]),
            quantile(f[, "lepen"] - f[, "melenchon"], 0.05),
            quantile(f[, "lepen"] - f[, "melenchon"], 0.95)))

cat("\n-- Effetti istituto (punti; + = l'istituto sovrastima il candidato) --\n")
print(he[, 1:10], row.names = FALSE)

cat("\n-- Tappe --\n")
tappe <- media[data %in% c(as.Date(c("2026-07-15", "2026-08-15", "2026-09-15", "2026-10-15",
                                     "2026-11-01")), FINE)]
print(dcast(tappe, data ~ serie, value.var = "media")[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 1) else x)],
      row.names = FALSE)

cat("\n-- Confronto: media semplice degli ultimi 14 giorni, un sondaggio per istituto --\n")
ult <- d[data >= FINE - 14][order(-data)][, .SD[1], by = .(istituto, serie)]
print(ult[, .(semplice = round(mean(y), 1), istituti = .N), by = serie][order(-semplice)],
      row.names = FALSE)
