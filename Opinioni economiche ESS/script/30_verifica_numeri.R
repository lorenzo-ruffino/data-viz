# 30_verifica_numeri.R
# Verifica adversariale INDIPENDENTE dei numeri dichiarati dagli analisti.
# Ricalcola da zero un campione di stime sui microdati ESS, senza leggere gli
# script 10_-20_. Unico riferimento metodologico: DATA_MAP.md + codebook JSON.

suppressPackageStartupMessages(library(data.table))

BASE <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Opinioni economiche ESS"
d <- readRDS(file.path(BASE, "input", "ess_slim.rds"))
setDT(d)

cat("Righe:", nrow(d), " Colonne:", ncol(d), "\n")

# ---------------------------------------------------------------- helper ----
# Filtro sui valori validi (codici missing = numeri nel dataset)
vv <- function(x, valid) { y <- as.numeric(x); ifelse(y %in% valid, y, NA_real_) }

S15 <- c("BE","CH","DE","ES","FI","FR","GB","HU","IE","NL","NO","PL","PT","SE","SI")
EU  <- c("AT","BE","BG","CH","CY","CZ","DE","DK","EE","ES","FI","FR","GB","GR",
         "HR","HU","IE","IS","IT","LT","LU","LV","NL","NO","PL","PT","RO","SE","SI","SK")

# percentuale ponderata: quota di casi validi che soddisfano `cond`
wpct <- function(x, w, cond_fun) {
  ok <- !is.na(x)
  if (sum(ok) == 0) return(c(pct = NA, n = 0))
  c(pct = 100 * sum(w[ok][cond_fun(x[ok])]) / sum(w[ok]), n = sum(ok))
}
wmean <- function(x, w) {
  ok <- !is.na(x); c(m = sum(w[ok] * x[ok]) / sum(w[ok]), n = sum(ok))
}
wsd <- function(x, w) {
  ok <- !is.na(x); m <- sum(w[ok]*x[ok])/sum(w[ok])
  sqrt(sum(w[ok]*(x[ok]-m)^2)/sum(w[ok]))
}

RES <- list()
add <- function(id, mio, dich, tol, unita = "pp") {
  RES[[length(RES)+1]] <<- data.table(id = id, mio = mio, dichiarato = dich,
                                      scarto = mio - dich, tol = tol, unita = unita)
}

# stima generica su un sottoinsieme
stima_pct <- function(dt, var, valid, cond, paesi, round) {
  s <- dt[cntry %in% paesi & essround == round]
  x <- vv(s[[var]], valid)
  wpct(x, s$anweight, cond)
}
stima_mean <- function(dt, var, valid, paesi, round) {
  s <- dt[cntry %in% paesi & essround == round]
  x <- vv(s[[var]], valid)
  wmean(x, s$anweight)
}
paesi_round <- function(dt, round) sort(intersect(EU, unique(dt[essround == round, cntry])))

L5 <- 1:5; L010 <- 0:10

cat("\n=========================================================\n")
cat("TEMA 1 — REDISTRIBUZIONE (gincdif, dfincac, smdfslv)\n")
cat("=========================================================\n")

# 1a gincdif % accordo (1-2) Italia round 11
r <- stima_pct(d, "gincdif", L5, function(x) x <= 2, "IT", 11)
cat(sprintf("gincdif %%accordo IT r11 : %.2f%%  (N=%d)  [dich. 80,7]\n", r["pct"], r["n"]))
add("redistr: gincdif %acc IT r11", r["pct"], 80.7, 0.3)

# 1b gincdif media Italia round 11
r <- stima_mean(d, "gincdif", L5, "IT", 11)
cat(sprintf("gincdif media IT r11    : %.3f    (N=%d)  [dich. 1,85]\n", r["m"], r["n"]))
add("redistr: gincdif media IT r11", r["m"], 1.85, 0.05, "punti")

# 1c gincdif Europa-panel15 round 11
r <- stima_pct(d, "gincdif", L5, function(x) x <= 2, S15, 11)
cat(sprintf("gincdif %%acc Eu-panel15 r11: %.2f%% (N=%d) [dich. 68,8]\n", r["pct"], r["n"]))
add("redistr: gincdif %acc Eu-panel15 r11", r["pct"], 68.8, 0.3)

# 1c-bis panel15 round 1
r <- stima_pct(d, "gincdif", L5, function(x) x <= 2, S15, 1)
cat(sprintf("gincdif %%acc Eu-panel15 r1 : %.2f%% (N=%d) [dich. 69,8]\n", r["pct"], r["n"]))
add("redistr: gincdif %acc Eu-panel15 r1", r["pct"], 69.8, 0.3)

# 1d confronto bilaterale round 11
dich_b <- c(DE = 67.8, FR = 71.7, ES = 68.1, GB = 70.2)
for (p in names(dich_b)) {
  r <- stima_pct(d, "gincdif", L5, function(x) x <= 2, p, 11)
  cat(sprintf("gincdif %%acc %s r11      : %.2f%%  (N=%d)  [dich. %.1f]\n", p, r["pct"], r["n"], dich_b[p]))
  add(paste0("redistr: gincdif %acc ", p, " r11"), r["pct"], dich_b[p], 0.3)
}

# 1e serie storica Italia
cat("\nSerie storica gincdif Italia (tutti i round con dati):\n")
for (rr in 1:11) {
  s <- d[cntry == "IT" & essround == rr]
  if (nrow(s) == 0) { cat(sprintf("  r%-2d : ITALIA ASSENTE dal round\n", rr)); next }
  x <- vv(s$gincdif, L5); if (sum(!is.na(x)) == 0) { cat(sprintf("  r%-2d : nessuna risposta valida (N righe=%d)\n", rr, nrow(s))); next }
  q <- wpct(x, s$anweight, function(z) z <= 2)
  cat(sprintf("  r%-2d (%d): %.2f%%  N=%d\n", rr, s$anno[1], q["pct"], q["n"]))
}
for (spec in list(c(1, 78.4), c(9, 86.4), c(10, 75.1))) {
  rr <- spec[1]; dv <- spec[2]
  r <- stima_pct(d, "gincdif", L5, function(x) x <= 2, "IT", rr)
  add(sprintf("redistr: gincdif %%acc IT r%d", rr), r["pct"], dv, 0.3)
}

# 1f classifica round 11 (26 paesi UE/EFTA+UK)
pr11 <- paesi_round(d, 11)
cl <- rbindlist(lapply(pr11, function(p) {
  r <- stima_pct(d, "gincdif", L5, function(x) x <= 2, p, 11)
  data.table(cntry = p, pct = r["pct"], n = r["n"])
}))
cl <- cl[n > 0][order(-pct)][, rank := .I]
cat(sprintf("\nClassifica gincdif r11: %d paesi. 1° %s %.2f%% | ultimo %s %.2f%% | Italia rank %d (%.2f%%)\n",
            nrow(cl), cl$cntry[1], cl$pct[1], cl$cntry[nrow(cl)], cl$pct[nrow(cl)],
            cl[cntry == "IT", rank], cl[cntry == "IT", pct]))
print(cl[1:8])
add("redistr: n paesi classifica r11", nrow(cl), 26, 0, "paesi")
add("redistr: rank IT classifica r11", cl[cntry == "IT", rank], 6, 0, "posizioni")
add("redistr: 1° classifica (PT) %acc", cl[cntry == "PT", pct], 89.3, 0.3)
add("redistr: ultimo classifica (PL) %acc", cl[cntry == "PL", pct], 58.7, 0.3)

# 1g dfincac / smdfslv round 8
r <- stima_pct(d, "dfincac", L5, function(x) x <= 2, "IT", 8)
cat(sprintf("\ndfincac %%acc IT r8      : %.2f%%  (N=%d)  [dich. 28,6]\n", r["pct"], r["n"]))
add("redistr: dfincac %acc IT r8", r["pct"], 28.6, 0.3)
r <- stima_pct(d, "smdfslv", L5, function(x) x <= 2, "IT", 8)
cat(sprintf("smdfslv %%acc IT r8      : %.2f%%  (N=%d)  [dich. 68,1]\n", r["pct"], r["n"]))
add("redistr: smdfslv %acc IT r8", r["pct"], 68.1, 0.3)
r <- stima_pct(d, "smdfslv", L5, function(x) x <= 2, "ES", 8)
cat(sprintf("smdfslv %%acc ES r8      : %.2f%%  (N=%d)  [dich. 78,6]\n", r["pct"], r["n"]))
add("redistr: smdfslv %acc ES r8", r["pct"], 78.6, 0.3)
p8 <- paesi_round(d, 8)
r <- stima_pct(d, "dfincac", L5, function(x) x <= 2, p8, 8)
cat(sprintf("dfincac %%acc Europa r8  : %.2f%%  (N=%d)  [dich. 44,3]  (paesi: %d)\n", r["pct"], r["n"], length(p8)))
add("redistr: dfincac %acc Europa r8", r["pct"], 44.3, 0.3)
r <- stima_pct(d, "smdfslv", L5, function(x) x <= 2, p8, 8)
cat(sprintf("smdfslv %%acc Europa r8  : %.2f%%  (N=%d)  [dich. 63,1]\n", r["pct"], r["n"]))
add("redistr: smdfslv %acc Europa r8", r["pct"], 63.1, 0.3)
# Variazione 2008-2016 dell'aggregato europeo: la composizione di "Europa" cambia fra i due
# round (25 paesi al r4, con BG CY DK GR HR LV RO SK e senza IT; 21 al r8, con AT IS IT LT),
# e anweight pondera per popolazione. Il confronto temporale va quindi letto SOLO sul panel
# a composizione fissa (S15), che e' l'unico pubblicato su entrambi i round in
# redistribuzione_differenze.csv. Qui si ricalcolano entrambe le versioni per misurare
# quanto della variazione e' artefatto di composizione.
p4 <- paesi_round(d, 4)
r_e4 <- stima_pct(d, "dfincac", L5, function(x) x <= 2, p4, 4)
r_e8 <- stima_pct(d, "dfincac", L5, function(x) x <= 2, p8, 8)
cat(sprintf("dfincac %%acc Europa r4  : %.2f%%  (N=%d)  (paesi: %d) -- comp. VARIABILE, non pubblicata\n",
            r_e4["pct"], r_e4["n"], length(p4)))
for (spec in list(list("dfincac", 4, 57.0), list("dfincac", 8, 46.6),
                  list("smdfslv", 4, 61.7), list("smdfslv", 8, 62.7))) {
  v <- spec[[1]]; rr <- spec[[2]]; dich <- spec[[3]]
  q <- stima_pct(d, v, L5, function(x) x <= 2, S15, rr)
  cat(sprintf("%s %%acc Europa-panel15 r%d: %.2f%%  (N=%d)  [dich. %.1f]\n", v, rr, q["pct"], q["n"], dich))
  add(sprintf("redistr: %s %%acc Europa-panel15 r%d", v, rr), q["pct"], dich, 0.3)
}
d_var <- r_e8["pct"] - r_e4["pct"]
d_fix <- stima_pct(d, "dfincac", L5, function(x) x <= 2, S15, 8)["pct"] -
         stima_pct(d, "dfincac", L5, function(x) x <= 2, S15, 4)["pct"]
cat(sprintf("dfincac var. 2008-2016: Europa comp. variabile %+.2f pp | panel15 %+.2f pp | artefatto %+.2f pp\n",
            d_var, d_fix, d_var - d_fix))
add("redistr: dfincac variazione 2008-2016 panel15", d_fix, -10.4, 0.3)

# 1h sociodemo IT r11: lrscale
it11 <- d[cntry == "IT" & essround == 11]
lr <- vv(it11$lrscale, L010); g <- vv(it11$gincdif, L5)
for (lab in c("sinistra", "destra")) {
  sel <- if (lab == "sinistra") which(lr <= 3) else which(lr >= 7)
  q <- wpct(g[sel], it11$anweight[sel], function(z) z <= 2)
  cat(sprintf("gincdif %%acc IT r11 %-8s: %.2f%%  (N=%d)\n", lab, q["pct"], q["n"]))
  add(paste0("redistr: gincdif %acc IT r11 ", lab), q["pct"], if (lab == "sinistra") 90.0 else 70.0, 0.3)
}
hf <- vv(it11$hincfel, 1:4); sel <- which(hf == 4)
q <- wpct(g[sel], it11$anweight[sel], function(z) z <= 2)
cat(sprintf("gincdif %%acc IT r11 grande difficolta: %.2f%% (N=%d) [dich. 56,9 N=86]\n", q["pct"], q["n"]))
add("redistr: gincdif %acc IT r11 hincfel=4", q["pct"], 56.9, 0.3)
add("redistr: N cella hincfel=4", q["n"], 86, 0, "casi")

cat("\n=========================================================\n")
cat("TEMA 2 — RUOLO DELLO STATO\n")
cat("=========================================================\n")

it8 <- d[cntry == "IT" & essround == 8]
for (spec in list(c("gvslvol", 8.46, 88.6), c("gvslvue", 7.69, 72.2), c("gvcldcr", 8.14, NA))) {
  v <- spec[1]; dm <- as.numeric(spec[2]); dp <- as.numeric(spec[3])
  x <- vv(it8[[v]], L010)
  m <- wmean(x, it8$anweight); p <- wpct(x, it8$anweight, function(z) z >= 7)
  cat(sprintf("%s IT r8: media %.3f  %%7-10 %.2f%%  (N=%d)  [dich. media %.2f / %%%s]\n",
              v, m["m"], p["pct"], m["n"], dm, ifelse(is.na(dp), "-", sprintf("%.1f", dp))))
  add(paste0("ruolostato: ", v, " media IT r8"), m["m"], dm, 0.05, "punti")
  if (!is.na(dp)) add(paste0("ruolostato: ", v, " %7-10 IT r8"), p["pct"], dp, 0.3)
}

# indice_gv3: media individuale dei 3 item (almeno 2 su 3 validi)
gv3_index <- function(dt) {
  m <- cbind(vv(dt$gvslvol, L010), vv(dt$gvslvue, L010), vv(dt$gvcldcr, L010))
  nv <- rowSums(!is.na(m))
  idx <- ifelse(nv >= 2, rowMeans(m, na.rm = TRUE), NA_real_)
  idx
}
p8 <- paesi_round(d, 8)
d8 <- d[essround == 8 & cntry %in% p8]
d8[, idx3 := gv3_index(d8)]
cl2 <- d8[!is.na(idx3), .(idx = sum(anweight * idx3) / sum(anweight), n = .N), by = cntry][order(-idx)][, rank := .I]
euro3 <- d8[!is.na(idx3), sum(anweight * idx3) / sum(anweight)]
cat(sprintf("\nindice_gv3: paesi=%d | Europa pooled %.4f (N=%d) [dich. 7,44]\n",
            nrow(cl2), euro3, d8[!is.na(idx3), .N]))
print(cl2[1:6]); print(cl2[(nrow(cl2)-2):nrow(cl2)])
cat(sprintf("Italia: %.4f  rank %d  [dich. 8,1 rank 4]\n", cl2[cntry == "IT", idx], cl2[cntry == "IT", rank]))
add("ruolostato: indice_gv3 IT r8", cl2[cntry == "IT", idx], 8.1, 0.05, "punti")
add("ruolostato: indice_gv3 Europa r8", euro3, 7.44, 0.05, "punti")
add("ruolostato: rank IT indice_gv3", cl2[cntry == "IT", rank], 4, 0, "posizioni")
add("ruolostato: n paesi indice_gv3", nrow(cl2), 21, 0, "paesi")
add("ruolostato: 1° indice_gv3 (IS)", cl2[cntry == "IS", idx], 8.24, 0.05, "punti")
add("ruolostato: ultimo indice_gv3 (CH)", cl2[cntry == "CH", idx], 6.61, 0.05, "punti")

# slvpens / slvuemp IT r8
for (spec in list(c("slvpens", 4.00), c("slvuemp", 2.66))) {
  v <- spec[1]; dm <- as.numeric(spec[2])
  x <- vv(it8[[v]], L010); m <- wmean(x, it8$anweight)
  cat(sprintf("%s media IT r8: %.3f (N=%d) [dich. %.2f]\n", v, m["m"], m["n"], dm))
  add(paste0("ruolostato: ", v, " media IT r8"), m["m"], dm, 0.05, "punti")
}
x <- vv(it8$slvuemp, L010)
p03 <- wpct(x, it8$anweight, function(z) z <= 3); p710 <- wpct(x, it8$anweight, function(z) z >= 7)
cat(sprintf("slvuemp IT r8: %%0-3 %.2f%% [dich. 66,6] | %%7-10 %.2f%% [dich. 2,8]\n", p03["pct"], p710["pct"]))
add("ruolostato: slvuemp %0-3 IT r8", p03["pct"], 66.6, 0.3)
add("ruolostato: slvuemp %7-10 IT r8", p710["pct"], 2.8, 0.3)
gap <- wpct(vv(it8$gvslvue, L010), it8$anweight, function(z) z >= 7)["pct"] - p710["pct"]
cat(sprintf("Scarto gvslvue(7-10) - slvuemp(7-10): %.2f pp [dich. 69,4]\n", gap))
add("ruolostato: scarto aspettativa-realta disoccupati", gap, 69.4, 0.3)

# ginveco 2002
it1 <- d[cntry == "IT" & essround == 1]
x <- vv(it1$ginveco, L5)
pa <- wpct(x, it1$anweight, function(z) z <= 2); pd <- wpct(x, it1$anweight, function(z) z >= 4)
cat(sprintf("\nginveco IT r1: %%accordo %.2f%% [dich. 31,0] | %%disaccordo %.2f%% [dich. 39,2] (N=%d)\n",
            pa["pct"], pd["pct"], pa["n"]))
add("ruolostato: ginveco %acc IT r1", pa["pct"], 31.0, 0.3)
add("ruolostato: ginveco %disacc IT r1", pd["pct"], 39.2, 0.3)
p1 <- paesi_round(d, 1)
r <- stima_pct(d, "ginveco", L5, function(x) x <= 2, p1, 1)
cat(sprintf("ginveco %%accordo Europa r1: %.2f%% (N=%d) [dich. 32,0]\n", r["pct"], r["n"]))
add("ruolostato: ginveco %acc Europa r1", r["pct"], 32.0, 0.3)
r <- stima_pct(d, "ginveco", L5, function(x) x <= 2, "DE", 1)
cat(sprintf("ginveco %%accordo DE r1: %.2f%% [dich. 46,8]\n", r["pct"]))
add("ruolostato: ginveco %acc DE r1", r["pct"], 46.8, 0.3)

# gvslvue per istruzione, IT r8
ei <- vv(it8$eisced, c(1:7)); xg <- vv(it8$gvslvue, L010)
for (lab in c("bassa", "alta")) {
  sel <- if (lab == "bassa") which(ei %in% 1:2) else which(ei %in% 5:7)
  q <- wpct(xg[sel], it8$anweight[sel], function(z) z >= 7)
  cat(sprintf("gvslvue %%7-10 IT r8 istruzione %-6s: %.2f%% (N=%d)\n", lab, q["pct"], q["n"]))
  add(paste0("ruolostato: gvslvue %7-10 IT istr ", lab), q["pct"], if (lab == "bassa") 75.8 else 68.2, 0.3)
}

cat("\n=========================================================\n")
cat("TEMA 3 — SUSSIDI\n")
cat("=========================================================\n")

for (spec in list(list("sblazy", "IT", 28.2), list("sblazy", "GB", 57.0),
                  list("bennent", "IT", 77.4), list("lbenent", "IT", 66.7),
                  list("uentrjb", "IT", 38.4), list("sbeqsoc", "IT", 51.7),
                  list("sbstrec", "IT", 34.2))) {
  v <- spec[[1]]; p <- spec[[2]]; dv <- spec[[3]]
  r <- stima_pct(d, v, L5, function(x) x <= 2, p, 8)
  cat(sprintf("%-8s %%acc %s r8: %.2f%% (N=%d) [dich. %.1f]\n", v, p, r["pct"], r["n"], dv))
  add(paste0("sussidi: ", v, " %acc ", p, " r8"), r["pct"], dv, 0.3)
}
for (spec in list(c("sblazy", 45.5), c("bennent", 65.0))) {
  v <- spec[1]; dv <- as.numeric(spec[2])
  r <- stima_pct(d, v, L5, function(x) x <= 2, p8, 8)
  cat(sprintf("%-8s %%acc Europa r8: %.2f%% (N=%d) [dich. %.1f]\n", v, r["pct"], r["n"], dv))
  add(paste0("sussidi: ", v, " %acc Europa r8"), r["pct"], dv, 0.3)
}

# basinc classifica r8
clb <- rbindlist(lapply(p8, function(p) {
  r <- stima_pct(d, "basinc", 1:4, function(x) x >= 3, p, 8)
  data.table(cntry = p, pct = r["pct"], n = r["n"])
}))[n > 0][order(-pct)][, rank := .I]
cat(sprintf("\nbasinc r8: %d paesi | 1° %s %.2f%% | ultimo %s %.2f%% | IT rank %d (%.2f%%, N=%d)\n",
            nrow(clb), clb$cntry[1], clb$pct[1], clb$cntry[nrow(clb)], clb$pct[nrow(clb)],
            clb[cntry == "IT", rank], clb[cntry == "IT", pct], clb[cntry == "IT", n]))
print(clb[1:7])
add("sussidi: basinc %fav IT r8", clb[cntry == "IT", pct], 58.9, 0.3)
add("sussidi: rank IT basinc", clb[cntry == "IT", rank], 6, 0, "posizioni")
add("sussidi: basinc LT (1°)", clb[cntry == "LT", pct], 80.4, 0.3)
add("sussidi: basinc NO (ultimo)", clb[cntry == "NO", pct], 33.8, 0.3)

# imsclbn % restrittivo (4-5)
r <- stima_pct(d, "imsclbn", L5, function(x) x >= 4, "IT", 8)
cat(sprintf("imsclbn %%restrittivo IT r8: %.2f%% (N=%d) [dich. 46,8]\n", r["pct"], r["n"]))
add("sussidi: imsclbn %restrittivo IT r8", r["pct"], 46.8, 0.3)
r <- stima_pct(d, "imsclbn", L5, function(x) x >= 4, p8, 8)
cat(sprintf("imsclbn %%restrittivo Europa r8: %.2f%% (N=%d) [dich. 33,3]\n", r["pct"], r["n"]))
add("sussidi: imsclbn %restrittivo Europa r8", r["pct"], 33.3, 0.3)

# eudcnbf
r <- stima_pct(d, "eudcnbf", L5, function(x) x <= 2, "IT", 8)
cat(sprintf("eudcnbf %%piu alti IT r8: %.2f%% (N=%d) [dich. 37,3]\n", r["pct"], r["n"]))
add("sussidi: eudcnbf %piu-alti IT r8", r["pct"], 37.3, 0.3)
r <- stima_pct(d, "eudcnbf", L5, function(x) x >= 4, "IT", 8)
cat(sprintf("eudcnbf %%piu bassi IT r8: %.2f%% [dich. 18,3]\n", r["pct"]))
add("sussidi: eudcnbf %piu-bassi IT r8", r["pct"], 18.3, 0.3)
r <- stima_pct(d, "eudcnbf", L5, function(x) x >= 4, "DE", 8)
cat(sprintf("eudcnbf %%piu bassi DE r8: %.2f%% (N=%d) [dich. 49,6]\n", r["pct"], r["n"]))
add("sussidi: eudcnbf %piu-bassi DE r8", r["pct"], 49.6, 0.3)

# basinc per sindacato IT r8
mb <- vv(it8$mbtru, 1:3); xb <- vv(it8$basinc, 1:4)
for (k in c(1, 3)) {
  sel <- which(mb == k)
  q <- wpct(xb[sel], it8$anweight[sel], function(z) z >= 3)
  cat(sprintf("basinc %%fav IT r8 mbtru=%d: %.2f%% (N=%d)\n", k, q["pct"], q["n"]))
  add(paste0("sussidi: basinc %fav IT mbtru=", k), q["pct"], if (k == 1) 45.2 else 59.7, 0.3)
}

cat("\n=========================================================\n")
cat("TEMA 4 — GIUSTIZIA (round 9)\n")
cat("=========================================================\n")

FR9 <- -4:4
p9 <- paesi_round(d, 9)
it9 <- d[cntry == "IT" & essround == 9]
x <- vv(it9$topinfr, FR9)
q <- wpct(x, it9$anweight, function(z) z > 0); m <- wmean(x, it9$anweight)
cat(sprintf("topinfr IT r9: %%>0 %.2f%% [dich. 69,9] | media %.3f [dich. 1,53] (N=%d)\n", q["pct"], m["m"], q["n"]))
add("giustizia: topinfr %>0 IT r9", q["pct"], 69.9, 0.3)
add("giustizia: topinfr media IT r9", m["m"], 1.53, 0.05, "punti")
r <- stima_pct(d, "topinfr", FR9, function(z) z > 0, p9, 9)
cat(sprintf("topinfr %%>0 Europa r9: %.2f%% (N=%d, paesi=%d) [dich. 45,7]\n", r["pct"], r["n"], length(p9)))
add("giustizia: topinfr %>0 Europa r9", r["pct"], 45.7, 0.3)

r <- stima_pct(d, "netifr", FR9, function(z) z < 0, "IT", 9)
cat(sprintf("netifr %%<0 IT r9: %.2f%% (N=%d) [dich. 67,9]\n", r["pct"], r["n"]))
add("giustizia: netifr %<0 IT r9", r["pct"], 67.9, 0.3)
r <- stima_pct(d, "netifr", FR9, function(z) z < 0, "GB", 9)
cat(sprintf("netifr %%<0 GB r9: %.2f%% (N=%d) [dich. 41,0]\n", r["pct"], r["n"]))
add("giustizia: netifr %<0 GB r9", r["pct"], 41.0, 0.3)
r <- stima_pct(d, "btminfr", FR9, function(z) z < 0, "IT", 9)
cat(sprintf("btminfr %%<0 IT r9: %.2f%% (N=%d) [dich. 91,2]\n", r["pct"], r["n"]))
add("giustizia: btminfr %<0 IT r9", r["pct"], 91.2, 0.3)

# sofrdst / sofrwrk
for (spec in list(list("sofrdst", "IT", 76.1), list("sofrwrk", "IT", 82.0))) {
  v <- spec[[1]]; dv <- spec[[3]]
  r <- stima_pct(d, v, L5, function(x) x <= 2, "IT", 9)
  cat(sprintf("%s %%acc IT r9: %.2f%% (N=%d) [dich. %.1f]\n", v, r["pct"], r["n"], dv))
  add(paste0("giustizia: ", v, " %acc IT r9"), r["pct"], dv, 0.3)
}
r <- stima_pct(d, "sofrdst", L5, function(x) x <= 2, p9, 9)
cat(sprintf("sofrdst %%acc Europa r9: %.2f%% (N=%d) [dich. 53,9]\n", r["pct"], r["n"]))
add("giustizia: sofrdst %acc Europa r9", r["pct"], 53.9, 0.3)
r <- stima_pct(d, "sofrwrk", L5, function(x) x <= 2, p9, 9)
cat(sprintf("sofrwrk %%acc Europa r9: %.2f%% [dich. 80,7]\n", r["pct"]))
add("giustizia: sofrwrk %acc Europa r9", r["pct"], 80.7, 0.3)

cls <- rbindlist(lapply(p9, function(p) {
  r <- stima_pct(d, "sofrdst", L5, function(x) x <= 2, p, 9)
  data.table(cntry = p, pct = r["pct"], n = r["n"])
}))[n > 0][order(-pct)][, rank := .I]
cat(sprintf("Classifica sofrdst r9: %d paesi | 1° %s %.2f%% | ultimo %s %.2f%% | IT rank %d (%.2f%%)\n",
            nrow(cls), cls$cntry[1], cls$pct[1], cls$cntry[nrow(cls)], cls$pct[nrow(cls)],
            cls[cntry == "IT", rank], cls[cntry == "IT", pct]))
print(cls[1:4]); print(cls[(nrow(cls)-2):nrow(cls)])
add("giustizia: rank IT sofrdst", cls[cntry == "IT", rank], 2, 0, "posizioni")
add("giustizia: n paesi sofrdst", nrow(cls), 27, 0, "paesi")
add("giustizia: sofrdst PT (1°)", cls[cntry == "PT", pct], 77.9, 0.3)
add("giustizia: sofrdst NO (ultimo)", cls[cntry == "NO", pct], 23.0, 0.3)

# evfrjob %7-10 IT
r <- stima_pct(d, "evfrjob", L010, function(z) z >= 7, "IT", 9)
cat(sprintf("evfrjob %%7-10 IT r9: %.2f%% (N=%d) [dich. 8,7]\n", r["pct"], r["n"]))
add("giustizia: evfrjob %7-10 IT r9", r["pct"], 8.7, 0.3)
r <- stima_pct(d, "evfrjob", L010, function(z) z >= 7, "DE", 9)
cat(sprintf("evfrjob %%7-10 DE r9: %.2f%% [dich. 32,5]\n", r["pct"]))
add("giustizia: evfrjob %7-10 DE r9", r["pct"], 32.5, 0.3)
# frprtpl % 1-2
r <- stima_pct(d, "frprtpl", L5, function(z) z <= 2, "IT", 9)
cat(sprintf("frprtpl %%1-2 (per niente/poco) IT r9: %.2f%% (N=%d) [dich. 75,1]\n", r["pct"], r["n"]))
add("giustizia: frprtpl %per-niente/poco IT r9", r["pct"], 75.1, 0.3)
# ifredu media (esclude 55)
r <- stima_mean(d, "ifredu", L010, "IT", 9)
cat(sprintf("ifredu media IT r9: %.3f (N=%d) [dich. 5,80]\n", r["m"], r["n"]))
add("giustizia: ifredu media IT r9", r["m"], 5.80, 0.05, "punti")
r <- stima_mean(d, "ifredu", L010, "DE", 9)
cat(sprintf("ifredu media DE r9: %.3f [dich. 7,77]\n", r["m"]))
add("giustizia: ifredu media DE r9", r["m"], 7.77, 0.05, "punti")
# recskil / recknow %3-4
for (spec in list(list("recskil", "IT", 56.2), list("recskil", "Europa", 78.1),
                  list("recknow", "IT", 61.1), list("recknow", "Europa", 58.1))) {
  v <- spec[[1]]; p <- spec[[2]]; dv <- spec[[3]]
  pp <- if (p == "Europa") p9 else p
  r <- stima_pct(d, v, 1:4, function(z) z >= 3, pp, 9)
  cat(sprintf("%s %%3-4 %s r9: %.2f%% (N=%d) [dich. %.1f]\n", v, p, r["pct"], r["n"], dv))
  add(paste0("giustizia: ", v, " %molta-influenza ", p), r["pct"], dv, 0.3)
}
# sofrdst per decile e istruzione IT r9
dec <- vv(it9$hinctnta, 1:10); ei9 <- vv(it9$eisced, 1:7)
xs <- vv(it9$sofrdst, L5); xt <- vv(it9$topinfr, FR9)
for (lab in c("basso", "alto")) {
  sel <- if (lab == "basso") which(dec %in% 1:3) else which(dec %in% 8:10)
  q <- wpct(xt[sel], it9$anweight[sel], function(z) z > 0)
  cat(sprintf("topinfr %%>0 IT r9 decile %-6s: %.2f%% (N=%d)\n", lab, q["pct"], q["n"]))
  add(paste0("giustizia: topinfr %>0 IT decile ", lab), q["pct"], if (lab == "basso") 76.1 else 54.9, 0.3)
}
for (lab in c("bassa", "alta")) {
  sel <- if (lab == "bassa") which(ei9 %in% 1:2) else which(ei9 %in% 5:7)
  q <- wpct(xs[sel], it9$anweight[sel], function(z) z <= 2)
  cat(sprintf("sofrdst %%acc IT r9 istruzione %-6s: %.2f%% (N=%d)\n", lab, q["pct"], q["n"]))
  add(paste0("giustizia: sofrdst %acc IT istr ", lab), q["pct"], if (lab == "bassa") 81.2 else 63.7, 0.3)
}

cat("\n=========================================================\n")
cat("TEMA 5 — CLIMA ECONOMICO\n")
cat("=========================================================\n")

for (spec in list(c(1, 4.13), c(6, 2.62), c(11, 4.00))) {
  rr <- spec[1]; dv <- spec[2]
  r <- stima_mean(d, "stfeco", L010, "IT", rr)
  cat(sprintf("stfeco media IT r%-2d: %.3f (N=%d) [dich. %.2f]\n", rr, r["m"], r["n"], dv))
  add(sprintf("clima: stfeco media IT r%d", rr), r["m"], dv, 0.05, "punti")
}
r <- stima_mean(d, "stfeco", L010, S15, 11)
cat(sprintf("stfeco media Eu-panel15 r11: %.3f (N=%d) [dich. 4,36]\n", r["m"], r["n"]))
add("clima: stfeco media Eu-panel15 r11", r["m"], 4.36, 0.05, "punti")
for (spec in list(c("stfgov", 2.81), c("trstprl", 3.05), c("trstplt", 1.88))) {
  v <- spec[1]; dv <- as.numeric(spec[2])
  r <- stima_mean(d, v, L010, "IT", 6)
  cat(sprintf("%s media IT r6: %.3f (N=%d) [dich. %.2f]\n", v, r["m"], r["n"], dv))
  add(paste0("clima: ", v, " media IT r6"), r["m"], dv, 0.05, "punti")
}
for (spec in list(c(1, 16.2), c(8, 30.7), c(6, 29.4), c(11, 21.3))) {
  rr <- spec[1]; dv <- spec[2]
  r <- stima_pct(d, "hincfel", 1:4, function(z) z >= 3, "IT", rr)
  cat(sprintf("hincfel %%3-4 IT r%-2d: %.2f%% (N=%d) [dich. %.1f]\n", rr, r["pct"], r["n"], dv))
  add(sprintf("clima: hincfel %%diff IT r%d", rr), r["pct"], dv, 0.3)
}
r <- stima_pct(d, "hincfel", 1:4, function(z) z >= 3, S15, 11)
cat(sprintf("hincfel %%3-4 Eu-panel15 r11: %.2f%% [dich. 14,2]\n", r["pct"]))
add("clima: hincfel %diff Eu-panel15 r11", r["pct"], 14.2, 0.3)

pr11 <- paesi_round(d, 11)
for (spec in list(list("lrscale", "IT", 11, "pct_destra", 28.2), list("lrscale", "Europa", 11, "pct_destra", 22.6),
                  list("lrscale", "IT", 1, "pct_destra", 22.4))) {
  v <- spec[[1]]; p <- spec[[2]]; rr <- spec[[3]]; dv <- spec[[5]]
  pp <- if (p == "Europa") pr11 else p
  r <- stima_pct(d, v, L010, function(z) z >= 7, pp, rr)
  cat(sprintf("lrscale %%destra %s r%d: %.2f%% (N=%d) [dich. %.1f]\n", p, rr, r["pct"], r["n"], dv))
  add(sprintf("clima: lrscale %%destra %s r%d", p, rr), r["pct"], dv, 0.3)
}
r <- stima_mean(d, "lrscale", L010, "IT", 11)
cat(sprintf("lrscale media IT r11: %.3f [dich. 5,16]\n", r["m"]))
add("clima: lrscale media IT r11", r["m"], 5.16, 0.05, "punti")
r <- stima_mean(d, "lrscale", L010, pr11, 11)
cat(sprintf("lrscale media Europa r11: %.3f [dich. 4,91] (paesi=%d)\n", r["m"], length(pr11)))
add("clima: lrscale media Europa r11", r["m"], 4.91, 0.05, "punti")
for (spec in list(list("euftf", "IT", 4.75), list("euftf", "Europa", 5.47),
                  list("imbgeco", "IT", 5.04), list("imbgeco", "Europa", 5.63))) {
  v <- spec[[1]]; p <- spec[[2]]; dv <- spec[[3]]
  pp <- if (p == "Europa") pr11 else p
  r <- stima_mean(d, v, L010, pp, 11)
  cat(sprintf("%s media %s r11: %.3f (N=%d) [dich. %.2f]\n", v, p, r["m"], r["n"], dv))
  add(paste0("clima: ", v, " media ", p, " r11"), r["m"], dv, 0.05, "punti")
}
# stfeco per lrscale e mnactic, IT r11
xe <- vv(it11$stfeco, L010); lr <- vv(it11$lrscale, L010); mn <- vv(it11$mnactic, 1:9)
for (lab in c("sinistra", "destra")) {
  sel <- if (lab == "sinistra") which(lr <= 3) else which(lr >= 7)
  q <- wmean(xe[sel], it11$anweight[sel])
  cat(sprintf("stfeco media IT r11 %-8s: %.3f (N=%d)\n", lab, q["m"], q["n"]))
  add(paste0("clima: stfeco media IT r11 ", lab), q["m"], if (lab == "sinistra") 3.57 else 4.86, 0.05, "punti")
}
sel <- which(mn == 1); q <- wmean(xe[sel], it11$anweight[sel])
cat(sprintf("stfeco media IT r11 occupati: %.3f (N=%d) [dich. 4,15]\n", q["m"], q["n"]))
add("clima: stfeco media IT r11 occupati", q["m"], 4.15, 0.05, "punti")
sel <- which(mn %in% 3:4); q <- wmean(xe[sel], it11$anweight[sel])
cat(sprintf("stfeco media IT r11 disoccupati: %.3f (N=%d) [dich. 3,56]\n", q["m"], q["n"]))
add("clima: stfeco media IT r11 disoccupati", q["m"], 3.56, 0.05, "punti")
dec11 <- vv(it11$hinctnta, 1:10); hf11 <- vv(it11$hincfel, 1:4)
for (lab in c("basso", "alto")) {
  sel <- if (lab == "basso") which(dec11 %in% 1:3) else which(dec11 %in% 8:10)
  q <- wpct(hf11[sel], it11$anweight[sel], function(z) z >= 3)
  cat(sprintf("hincfel %%diff IT r11 decile %-6s: %.2f%% (N=%d)\n", lab, q["pct"], q["n"]))
  add(paste0("clima: hincfel %diff IT r11 decile ", lab), q["pct"], if (lab == "basso") 40.9 else 3.7, 0.3)
}

cat("\n=========================================================\n")
cat("TEMA 6 — SOCIODEMO / POLITICA (voto 2022)\n")
cat("=========================================================\n")

pv <- vv(it11$prtvteit, c(1:11, 31))
gg <- vv(it11$gincdif, L5); ee <- vv(it11$stfeco, L010)
lab_part <- c("1" = "FdI", "2" = "PD", "3" = "M5S", "4" = "Lega", "5" = "FI",
              "6" = "Azione-IV", "7" = "AVS", "31" = "Altri")
for (k in names(lab_part)) {
  sel <- which(pv == as.numeric(k))
  if (length(sel) == 0) next
  qg <- wpct(gg[sel], it11$anweight[sel], function(z) z <= 2)
  qe <- wmean(ee[sel], it11$anweight[sel])
  cat(sprintf("%-10s: gincdif %%acc %.2f%% (N=%d) | stfeco media %.3f (N=%d)\n",
              lab_part[k], qg["pct"], qg["n"], qe["m"], qe["n"]))
}
dich_voto <- list(c(3, 87.9), c(2, 86.5), c(1, 72.1), c(4, 66.7))
for (s in dich_voto) {
  sel <- which(pv == s[1]); q <- wpct(gg[sel], it11$anweight[sel], function(z) z <= 2)
  add(paste0("sociodemo: gincdif %acc voto ", lab_part[as.character(s[1])]), q["pct"], s[2], 0.3)
}
sel <- which(pv == 5); q <- wmean(ee[sel], it11$anweight[sel])
add("sociodemo: stfeco media voto FI", q["m"], 5.23, 0.05, "punti")
sel <- which(pv == 3); q <- wmean(ee[sel], it11$anweight[sel])
add("sociodemo: stfeco media voto M5S", q["m"], 3.57, 0.05, "punti")

# genere
gn <- vv(it11$gndr, 1:2)
for (k in 1:2) {
  sel <- which(gn == k); q <- wpct(gg[sel], it11$anweight[sel], function(z) z <= 2)
  cat(sprintf("gincdif %%acc IT r11 %s: %.2f%% (N=%d)\n", ifelse(k == 1, "uomini", "donne"), q["pct"], q["n"]))
  add(paste0("sociodemo: gincdif %acc ", ifelse(k == 1, "uomini", "donne")), q["pct"], if (k == 1) 79.3 else 82.0, 0.3)
}
# sofrdst per decile r9
for (lab in c("basso", "alto")) {
  sel <- if (lab == "basso") which(dec %in% 1:3) else which(dec %in% 8:10)
  q <- wpct(xs[sel], it9$anweight[sel], function(z) z <= 2)
  cat(sprintf("sofrdst %%acc IT r9 decile %-6s: %.2f%% (N=%d)\n", lab, q["pct"], q["n"]))
  add(paste0("sociodemo: sofrdst %acc decile ", lab), q["pct"], if (lab == "basso") 84.5 else 61.7, 0.3)
}
# sblazy per eta r8
ag8 <- vv(it8$agea, 15:130); xl <- vv(it8$sblazy, L5); mn8 <- vv(it8$mnactic, 1:9)
for (lab in c("15-34", "55+")) {
  sel <- if (lab == "15-34") which(ag8 >= 15 & ag8 <= 34) else which(ag8 >= 55)
  q <- wpct(xl[sel], it8$anweight[sel], function(z) z <= 2)
  cat(sprintf("sblazy %%acc IT r8 eta %-6s: %.2f%% (N=%d)\n", lab, q["pct"], q["n"]))
  add(paste0("sociodemo: sblazy %acc eta ", lab), q["pct"], if (lab == "15-34") 23.9 else 30.1, 0.3)
}
sel <- which(mn8 == 6); q <- wpct(xl[sel], it8$anweight[sel], function(z) z <= 2)
cat(sprintf("sblazy %%acc IT r8 pensionati: %.2f%% (N=%d) [dich. 32,6]\n", q["pct"], q["n"]))
add("sociodemo: sblazy %acc pensionati", q["pct"], 32.6, 0.3)

cat("\n=========================================================\n")
cat("TEMA 7 — INDICE SINTETICO (round 8)\n")
cat("=========================================================\n")

# INDICE 1 già calcolato sopra (idx3). Ricontrollo con 3 decimali.
cat(sprintf("Indice1 IT: %.4f rank %d | Europa %.4f\n", cl2[cntry == "IT", idx], cl2[cntry == "IT", rank], euro3))
add("indice: indice1 IT", cl2[cntry == "IT", idx], 8.099, 0.05, "punti")
add("indice: indice1 Europa", euro3, 7.445, 0.05, "punti")

# INDICE 2: 9 item, z-score ponderato su pooled 21 paesi
items <- c("gincdif", "gvslvol", "gvslvue", "gvcldcr", "sbeqsoc", "sbstrec", "sbbsntx", "sblazy", "basinc")
Z <- matrix(NA_real_, nrow = nrow(d8), ncol = length(items), dimnames = list(NULL, items))
for (v in items) {
  raw <- if (v %in% c("gvslvol", "gvslvue", "gvcldcr")) vv(d8[[v]], L010)
         else if (v == "basinc") vv(d8[[v]], 1:4)
         else vv(d8[[v]], L5)
  # orientamento pro-Stato
  orient <- if (v %in% c("gincdif", "sbeqsoc")) 6 - raw else raw
  m <- sum(d8$anweight[!is.na(orient)] * orient[!is.na(orient)]) / sum(d8$anweight[!is.na(orient)])
  s <- wsd(orient, d8$anweight)
  Z[, v] <- (orient - m) / s
}
nv <- rowSums(!is.na(Z))
idx2 <- ifelse(nv >= 6, rowMeans(Z, na.rm = TRUE), NA_real_)
d8[, idx2 := idx2]
cl3 <- d8[!is.na(idx2), .(idx = sum(anweight * idx2) / sum(anweight), n = .N), by = cntry][order(-idx)][, rank := .I]
euro2 <- d8[!is.na(idx2), sum(anweight * idx2) / sum(anweight)]
cat(sprintf("Indice2: paesi=%d | Europa pooled %.4f\n", nrow(cl3), euro2))
print(cl3[1:6]); print(cl3[(nrow(cl3)-4):nrow(cl3)])
cat(sprintf("Italia indice2: %.4f rank %d (N=%d) [dich. 0,1751 rank 4 N=2466]\n",
            cl3[cntry == "IT", idx], cl3[cntry == "IT", rank], cl3[cntry == "IT", n]))
add("indice: indice2 IT", cl3[cntry == "IT", idx], 0.1751, 0.05, "z")
add("indice: rank IT indice2", cl3[cntry == "IT", rank], 4, 0, "posizioni")
add("indice: indice2 IS (1°)", cl3[cntry == "IS", idx], 0.3004, 0.05, "z")
add("indice: indice2 GB (ultimo)", cl3[cntry == "GB", idx], -0.1967, 0.05, "z")
add("indice: indice2 ES (2°)", cl3[cntry == "ES", idx], 0.2332, 0.05, "z")

# correlazioni item-resto e alpha
cat("\nCorrelazioni item-resto della scala (casi completi):\n")
Zc <- Z[complete.cases(Z), , drop = FALSE]
for (v in items) {
  rest <- rowMeans(Zc[, setdiff(items, v), drop = FALSE])
  cat(sprintf("  %-8s r = %+.3f\n", v, cor(Zc[, v], rest)))
}
k <- ncol(Zc); Cv <- cov(Zc)
alpha <- (k / (k - 1)) * (1 - sum(diag(Cv)) / sum(Cv))
cat(sprintf("Alpha di Cronbach (9 item, N=%d casi completi): %.4f [dich. 0,633]\n", nrow(Zc), alpha))
add("indice: alpha Cronbach", alpha, 0.633, 0.02, "alpha")

# gruppi Italia sull'indice2
it8i <- d8[cntry == "IT"]
lr8 <- vv(it8i$lrscale, L010); tp8 <- vv(it8i$tporgwk, 1:6)
for (lab in c("sinistra", "centro", "destra")) {
  sel <- switch(lab, sinistra = which(lr8 <= 3), centro = which(lr8 >= 4 & lr8 <= 6), destra = which(lr8 >= 7))
  sub <- it8i[sel]; ok <- !is.na(sub$idx2)
  cat(sprintf("indice2 IT %-8s: %.4f (N=%d)\n", lab, sum(sub$anweight[ok]*sub$idx2[ok])/sum(sub$anweight[ok]), sum(ok)))
}
for (lab in c("pubblico", "privato", "autonomi")) {
  sel <- switch(lab, pubblico = which(tp8 %in% 1:3), privato = which(tp8 == 4), autonomi = which(tp8 == 5))
  sub <- it8i[sel]; ok <- !is.na(sub$idx2)
  cat(sprintf("indice2 IT %-9s: %.4f (N=%d)\n", lab, sum(sub$anweight[ok]*sub$idx2[ok])/sum(sub$anweight[ok]), sum(ok)))
}

cat("\n=========================================================\n")
cat("QUADRO SCARTI\n")
cat("=========================================================\n")
R <- rbindlist(RES)
R[, esito := fifelse(abs(scarto) <= tol, "ok",
             fifelse(unita == "pp" & abs(scarto) > 1, "GRAVE",
             fifelse(unita %in% c("posizioni","paesi","casi") & abs(scarto) > 0, "GRAVE",
             fifelse(unita %in% c("punti","z","alpha") & abs(scarto) > tol*3, "GRAVE", "lieve"))))]
print(R[order(esito != "ok", -abs(scarto))], nrows = 200)
cat("\nRIEPILOGO:", R[esito == "ok", .N], "ok /", R[esito == "lieve", .N], "lievi /",
    R[esito == "GRAVE", .N], "gravi  su", nrow(R), "controlli\n")
if (R[esito != "ok", .N] > 0) { cat("\nNON CONFORMI:\n"); print(R[esito != "ok"][order(-abs(scarto))]) }

# ============================================================================
# PARTE B — verifica delle AFFERMAZIONI NARRATIVE (ordini, primati, confronti)
# ============================================================================

cat("\n\n#########################################################\n")
cat("PARTE B — AFFERMAZIONI NARRATIVE\n")
cat("#########################################################\n")
B <- list()
addB <- function(claim, esito, nota) B[[length(B)+1]] <<- data.table(claim=claim, esito=esito, nota=nota)

# B1 "Italia il piu' favorevole a gincdif tra i 5 paesi, in OGNI round in cui e' presente"
cat("\n[B1] gincdif %accordo IT vs DE/FR/ES/GB, per round\n")
ok <- TRUE
for (rr in c(1,6,8,9,10,11)) {
  v <- sapply(c("IT","DE","FR","ES","GB"), function(p) stima_pct(d,"gincdif",L5,function(x)x<=2,p,rr)["pct"])
  names(v) <- c("IT","DE","FR","ES","GB")
  cat(sprintf("  r%-2d: %s   -> max = %s\n", rr,
              paste(sprintf("%s %.1f", names(v), v), collapse="  "), names(which.max(v))))
  if (names(which.max(v)) != "IT") ok <- FALSE
}
addB("redistr: IT prima sui 5 paesi su gincdif in ogni round", ifelse(ok,"OK","FALSO"), "")

# B2 dfincac r8: ES piu' basso di IT? valori DE/FR/GB
cat("\n[B2] dfincac %accordo r8\n")
v <- sapply(c("IT","ES","DE","FR","GB"), function(p) stima_pct(d,"dfincac",L5,function(x)x<=2,p,8)["pct"])
print(round(v,2))
addB("redistr: dfincac DE 51,8 FR 44,8 GB 53,6 ES 27,9",
     ifelse(all(abs(v[c("DE","FR","GB","ES")] - c(51.8,44.8,53.6,27.9)) <= 0.3),"OK","SCARTO"),
     paste(sprintf("%s=%.1f",names(v),v),collapse=" "))

cat("\n[B3] smdfslv %accordo r8\n")
v <- sapply(c("IT","ES","DE","FR","GB"), function(p) stima_pct(d,"smdfslv",L5,function(x)x<=2,p,8)["pct"])
print(round(v,2))
addB("redistr: smdfslv DE/FR ~61%, GB 54,9%",
     ifelse(abs(v["GB"]-54.9)<=0.3,"OK","SCARTO"), paste(sprintf("%s=%.1f",names(v),v),collapse=" "))
# dfincac ES round4 -> round8
v4 <- stima_pct(d,"dfincac",L5,function(x)x<=2,"ES",4)["pct"]
cat(sprintf("dfincac ES r4 %.2f -> r8 %.2f  [dich. 52,7 -> 27,9]\n", v4, v["ES"]))
addB("redistr: dfincac ES r4=52,7", ifelse(abs(v4-52.7)<=0.3,"OK","SCARTO"), sprintf("%.2f",v4))

# B4 ruolo stato: rank DE/FR nell'indice_gv3; GB gvslvol r4->r8
cat("\n[B4] indice_gv3: rank dei 5 paesi\n")
print(cl2[cntry %in% c("IT","ES","DE","FR","GB")][order(rank)])
addB("ruolostato: DE 14a, FR 18a, GB 19o su indice_gv3",
     ifelse(cl2[cntry=="DE",rank]==14 && cl2[cntry=="FR",rank]==18 && cl2[cntry=="GB",rank]==19,"OK","DIVERGE"),
     paste(sprintf("%s=%d",cl2[cntry %in% c("DE","FR","GB"),cntry],cl2[cntry %in% c("DE","FR","GB"),rank]),collapse=" "))
g4 <- stima_mean(d,"gvslvol",L010,"GB",4)["m"]; g8 <- stima_mean(d,"gvslvol",L010,"GB",8)["m"]
cat(sprintf("gvslvol GB r4 %.3f -> r8 %.3f  delta %.3f  [dich. 8,50 -> 7,79 = -0,71]\n", g4, g8, g8-g4))
addB("ruolostato: gvslvol GB 8,50->7,79 (-0,71)",
     ifelse(abs(g4-8.50)<=0.05 && abs(g8-7.79)<=0.05,"OK","SCARTO"), sprintf("%.2f -> %.2f", g4, g8))
# slvpens/slvuemp media Europa r8
for (v in c("slvpens","slvuemp")) {
  m <- stima_mean(d,v,L010,p8,8)["m"]
  cat(sprintf("%s media Europa r8: %.3f  [dich. %s]\n", v, m, ifelse(v=="slvpens","4,65","3,97")))
  addB(paste0("ruolostato: ",v," Europa r8"),
       ifelse(abs(m - ifelse(v=="slvpens",4.65,3.97))<=0.05,"OK","SCARTO"), sprintf("%.3f",m))
}
# slvpens/slvuemp: IT la piu' bassa tra i 5?
for (v in c("slvpens","slvuemp")) {
  vv5 <- sapply(c("IT","DE","FR","ES","GB"), function(p) stima_mean(d,v,L010,p,8)["m"])
  names(vv5) <- c("IT","DE","FR","ES","GB")
  cat(sprintf("%s r8 5 paesi: %s -> min = %s\n", v, paste(sprintf("%s %.2f",names(vv5),vv5),collapse="  "),
              names(which.min(vv5))))
  addB(paste0("ruolostato: IT la piu' bassa su ",v), ifelse(names(which.min(vv5))=="IT","OK","FALSO"), "")
}
# gvslvue per hincfel IT r8
hf8 <- vv(it8$hincfel,1:4); xu <- vv(it8$gvslvue,L010)
for (k in c(4,1)) {
  sel <- which(hf8==k); q <- wpct(xu[sel], it8$anweight[sel], function(z) z>=7)
  cat(sprintf("gvslvue %%7-10 IT r8 hincfel=%d: %.2f%% (N=%d) [dich. %s]\n", k, q["pct"], q["n"],
              ifelse(k==4,"83,7","68,3")))
  addB(sprintf("ruolostato: gvslvue %%7-10 hincfel=%d",k),
       ifelse(abs(q["pct"] - ifelse(k==4,83.7,68.3))<=0.3,"OK","SCARTO"), sprintf("%.2f (N=%d)",q["pct"],q["n"]))
}

# B5 sussidi
cat("\n[B5] sussidi\n")
for (v in c("sblazy","sblwcoa","bennent","lbenent","uentrjb")) {
  vv5 <- sapply(c("IT","DE","FR","ES","GB"), function(p) stima_pct(d,v,L5,function(x)x<=2,p,8)["pct"])
  eur <- stima_pct(d,v,L5,function(x)x<=2,p8,8)["pct"]
  cat(sprintf("%-8s r8: %s | Europa %.1f\n", v, paste(sprintf("%s %.1f",names(vv5),vv5),collapse="  "), eur))
}
vsl <- sapply(c("IT","DE","FR","ES","GB"), function(p) stima_pct(d,"sblazy",L5,function(x)x<=2,p,8)["pct"]); names(vsl) <- c("IT","DE","FR","ES","GB")
vbn <- sapply(c("IT","DE","FR","ES","GB"), function(p) stima_pct(d,"bennent",L5,function(x)x<=2,p,8)["pct"]); names(vbn) <- c("IT","DE","FR","ES","GB")
addB("sussidi: IT il MENO sospettoso su sblazy tra i 5", ifelse(names(which.min(vsl))=="IT","OK","FALSO"), names(which.min(vsl)))
addB("sussidi: IT il PIU' sospettoso su bennent tra i 5", ifelse(names(which.max(vbn))=="IT","OK","FALSO"), names(which.max(vbn)))
b4 <- stima_pct(d,"sbbsntx",L5,function(x)x<=2,"DE",4)["pct"]; b8 <- stima_pct(d,"sbbsntx",L5,function(x)x<=2,"DE",8)["pct"]
cat(sprintf("sbbsntx DE r4 %.2f -> r8 %.2f (%.1f pp) [dich. 45,4 -> 22,9 = -22,5]\n", b4, b8, b8-b4))
addB("sussidi: sbbsntx DE 45,4->22,9", ifelse(abs(b4-45.4)<=0.3 && abs(b8-22.9)<=0.3,"OK","SCARTO"), sprintf("%.1f->%.1f",b4,b8))
n4 <- stima_pct(d,"bennent",L5,function(x)x<=2,"GB",4)["pct"]
cat(sprintf("bennent GB r4 %.2f -> r8 63,7 [dich. 76,5]\n", n4))
addB("sussidi: bennent GB r4=76,5", ifelse(abs(n4-76.5)<=0.3,"OK","SCARTO"), sprintf("%.1f",n4))
# sblwlka esiste al round 8?
n_sblwlka_r8 <- d[essround==8, sum(!is.na(vv(sblwlka,L5)))]
cat(sprintf("sblwlka risposte valide round 8 (tutti i paesi): %d [dich. 0]\n", n_sblwlka_r8))
addB("sussidi: sblwlka assente al round 8", ifelse(n_sblwlka_r8==0,"OK","FALSO"), sprintf("N=%d",n_sblwlka_r8))
# gvhlthc/gvpdlwk/gvjbevn al round 8
for (v in c("gvhlthc","gvpdlwk","gvjbevn")) {
  nn <- d[essround==8, sum(!is.na(vv(get(v),L010)))]
  cat(sprintf("%s risposte valide round 8: %d [dich. 0]\n", v, nn))
  addB(paste0("ruolostato/indice: ",v," assente al round 8"), ifelse(nn==0,"OK","FALSO"), sprintf("N=%d",nn))
}
# dfincac/smdfslv solo round 4 e 8?
for (v in c("dfincac","smdfslv")) {
  rr <- d[, .(n=sum(!is.na(vv(get(v),L5)))), by=essround][n>0][order(essround)]
  cat(sprintf("%s presente nei round: %s\n", v, paste(rr$essround, collapse=",")))
  addB(paste0("redistr: ",v," solo round 4 e 8"),
       ifelse(identical(sort(rr$essround), c(4L,8L)),"OK","FALSO"), paste(rr$essround,collapse=","))
}
# uentrjb ES
cat(sprintf("uentrjb ES r8: %.2f%% [dich. 23,1]\n", stima_pct(d,"uentrjb",L5,function(x)x<=2,"ES",8)["pct"]))

# B6 giustizia
cat("\n[B6] giustizia r9\n")
for (v in c("sofrdst","sofrwrk")) {
  vv5 <- sapply(c("IT","DE","FR","ES","GB"), function(p) stima_pct(d,v,L5,function(x)x<=2,p,9)["pct"])
  cat(sprintf("%-8s: %s\n", v, paste(sprintf("%s %.1f",names(vv5),vv5),collapse="  ")))
}
addB("giustizia: DE sofrdst 42,4 / sofrwrk 86,2",
     ifelse(abs(stima_pct(d,"sofrdst",L5,function(x)x<=2,"DE",9)["pct"]-42.4)<=0.3 &&
            abs(stima_pct(d,"sofrwrk",L5,function(x)x<=2,"DE",9)["pct"]-86.2)<=0.3,"OK","SCARTO"),"")
addB("giustizia: GB sofrdst 45,3 / sofrwrk 76,3",
     ifelse(abs(stima_pct(d,"sofrdst",L5,function(x)x<=2,"GB",9)["pct"]-45.3)<=0.3 &&
            abs(stima_pct(d,"sofrwrk",L5,function(x)x<=2,"GB",9)["pct"]-76.3)<=0.3,"OK","SCARTO"),"")
# netifr: %giusto (=0) IT/GB/DE/Europa; %<0 Europa e DE
for (p in list(list("IT","IT"), list("GB","GB"), list("DE","DE"), list("Europa",p9))) {
  pp <- p[[2]]
  lo <- stima_pct(d,"netifr",FR9,function(z)z<0,pp,9)["pct"]; ju <- stima_pct(d,"netifr",FR9,function(z)z==0,pp,9)["pct"]
  cat(sprintf("netifr %-7s: %%<0 %.2f  %%=0 %.2f\n", p[[1]], lo, ju))
}
addB("giustizia: netifr Europa 57,9 basso / 37,4 giusto",
     ifelse(abs(stima_pct(d,"netifr",FR9,function(z)z<0,p9,9)["pct"]-57.9)<=0.3 &&
            abs(stima_pct(d,"netifr",FR9,function(z)z==0,p9,9)["pct"]-37.4)<=0.3,"OK","SCARTO"),"")
addB("giustizia: netifr IT 28,5 giusto",
     ifelse(abs(stima_pct(d,"netifr",FR9,function(z)z==0,"IT",9)["pct"]-28.5)<=0.3,"OK","SCARTO"),"")
# frprtpl %4-5
for (p in c("IT","DE")) {
  hi <- stima_pct(d,"frprtpl",L5,function(z)z>=4,p,9)["pct"]; lo <- stima_pct(d,"frprtpl",L5,function(z)z<=2,p,9)["pct"]
  cat(sprintf("frprtpl %s: %%molta/moltissima %.2f  %%per-niente/poco %.2f\n", p, hi, lo))
}
addB("giustizia: frprtpl IT 2,2 molta/moltissima",
     ifelse(abs(stima_pct(d,"frprtpl",L5,function(z)z>=4,"IT",9)["pct"]-2.2)<=0.3,"OK","SCARTO"),"")
addB("giustizia: frprtpl DE 37,4 molta / 22,6 poco",
     ifelse(abs(stima_pct(d,"frprtpl",L5,function(z)z>=4,"DE",9)["pct"]-37.4)<=0.3 &&
            abs(stima_pct(d,"frprtpl",L5,function(z)z<=2,"DE",9)["pct"]-22.6)<=0.3,"OK","SCARTO"),"")
cat(sprintf("wltdffr %%>0 IT r9: %.2f%% [dich. 75,0] | ppldsrv %%acc IT r9: %.2f%% [dich. 34,2]\n",
            stima_pct(d,"wltdffr",FR9,function(z)z>0,"IT",9)["pct"],
            stima_pct(d,"ppldsrv",L5,function(x)x<=2,"IT",9)["pct"]))
addB("giustizia: wltdffr 75,0 / ppldsrv 34,2",
     ifelse(abs(stima_pct(d,"wltdffr",FR9,function(z)z>0,"IT",9)["pct"]-75.0)<=0.3 &&
            abs(stima_pct(d,"ppldsrv",L5,function(x)x<=2,"IT",9)["pct"]-34.2)<=0.3,"OK","SCARTO"),"")
cat(sprintf("sofrdst SE r9: %.2f%% [dich. 27,8]  topinfr DE %.2f [dich. 41,3] ES %.2f [dich. 45,8]\n",
            stima_pct(d,"sofrdst",L5,function(x)x<=2,"SE",9)["pct"],
            stima_pct(d,"topinfr",FR9,function(z)z>0,"DE",9)["pct"],
            stima_pct(d,"topinfr",FR9,function(z)z>0,"ES",9)["pct"]))
cat(sprintf("topinfr media Europa r9: %.3f [dich. 0,71]\n", stima_mean(d,"topinfr",FR9,p9,9)["m"]))
addB("giustizia: topinfr media Europa 0,71",
     ifelse(abs(stima_mean(d,"topinfr",FR9,p9,9)["m"]-0.71)<=0.05,"OK","SCARTO"),"")
# ifredu: quanti IT con codice 55
cat(sprintf("ifredu IT r9: codice 55 = %d casi, validi 0-10 = %d [dich. 38 su 2571]\n",
            d[cntry=="IT"&essround==9, sum(ifredu==55, na.rm=TRUE)],
            d[cntry=="IT"&essround==9, sum(!is.na(vv(ifredu,L010)))]))

# B7 clima
cat("\n[B7] clima\n")
cat(sprintf("trstplt ES r6 %.3f [dich. 1,91] | stfeco ES r6 %.3f [dich. 2,16]\n",
            stima_mean(d,"trstplt",L010,"ES",6)["m"], stima_mean(d,"stfeco",L010,"ES",6)["m"]))
addB("clima: ES r6 trstplt 1,91 / stfeco 2,16",
     ifelse(abs(stima_mean(d,"trstplt",L010,"ES",6)["m"]-1.91)<=0.05 &&
            abs(stima_mean(d,"stfeco",L010,"ES",6)["m"]-2.16)<=0.05,"OK","SCARTO"),"")
cat(sprintf("lrscale %%sinistra IT r11 %.2f [dich. 22,7] | Europa %.2f [dich. 25,5]\n",
            stima_pct(d,"lrscale",L010,function(z)z<=3,"IT",11)["pct"],
            stima_pct(d,"lrscale",L010,function(z)z<=3,pr11,11)["pct"]))
addB("clima: lrscale sinistra IT 22,7 / Eu 25,5",
     ifelse(abs(stima_pct(d,"lrscale",L010,function(z)z<=3,"IT",11)["pct"]-22.7)<=0.3 &&
            abs(stima_pct(d,"lrscale",L010,function(z)z<=3,pr11,11)["pct"]-25.5)<=0.3,"OK","SCARTO"),"")
cat(sprintf("euftf ES r11 %.3f [dich. 6,24] DE %.3f [dich. 5,91]\n",
            stima_mean(d,"euftf",L010,"ES",11)["m"], stima_mean(d,"euftf",L010,"DE",11)["m"]))
v5 <- sapply(c("IT","DE","FR","ES","GB"), function(p) stima_mean(d,"imbgeco",L010,p,11)["m"])
names(v5) <- c("IT","DE","FR","ES","GB")
cat(sprintf("imbgeco r11: %s -> min %s [dich. IT la piu' bassa; GB 6,39 ES 6,20]\n",
            paste(sprintf("%s %.2f",names(v5),v5),collapse="  "), names(which.min(v5))))
addB("clima: IT la piu' bassa su imbgeco tra i 5", ifelse(names(which.min(v5))=="IT","OK","FALSO"),"")
addB("clima: euftf ES 6,24 DE 5,91 | imbgeco GB 6,39 ES 6,20",
     ifelse(abs(stima_mean(d,"euftf",L010,"ES",11)["m"]-6.24)<=0.05 &&
            abs(stima_mean(d,"euftf",L010,"DE",11)["m"]-5.91)<=0.05 &&
            abs(v5["GB"]-6.39)<=0.05 && abs(v5["ES"]-6.20)<=0.05,"OK","SCARTO"),"")
sel <- which(vv(it11$mnactic,1:9) %in% 3:4)
cat(sprintf("hincfel %%diff IT r11 disoccupati: %.2f%% (N=%d) [dich. 60,2]\n",
            wpct(vv(it11$hincfel,1:4)[sel], it11$anweight[sel], function(z)z>=3)["pct"],
            wpct(vv(it11$hincfel,1:4)[sel], it11$anweight[sel], function(z)z>=3)["n"]))
ei11 <- vv(it11$eisced,1:7)
for (lab in c("bassa","alta")) {
  s2 <- if (lab=="bassa") which(ei11 %in% 1:2) else which(ei11 %in% 5:7)
  q <- wpct(vv(it11$hincfel,1:4)[s2], it11$anweight[s2], function(z)z>=3)
  cat(sprintf("hincfel %%diff IT r11 istruzione %-6s: %.2f%% (N=%d) [dich. %s]\n", lab, q["pct"], q["n"],
              ifelse(lab=="bassa","32,8","5,0")))
}
# stfeco per reddito percepito r11 (cella segnalata)
for (k in 1:4) {
  s2 <- which(vv(it11$hincfel,1:4)==k); q <- wmean(vv(it11$stfeco,L010)[s2], it11$anweight[s2])
  cat(sprintf("stfeco IT r11 hincfel=%d: %.3f (N=%d)\n", k, q["m"], q["n"]))
}

# B8 sociodemo/politica
cat("\n[B8] sociodemo\n")
lr8b <- vv(it8$lrscale,L010); xb8 <- vv(it8$basinc,1:4)
for (lab in c("sinistra","centro","destra")) {
  s2 <- switch(lab, sinistra=which(lr8b<=3), centro=which(lr8b>=4&lr8b<=6), destra=which(lr8b>=7))
  q <- wpct(xb8[s2], it8$anweight[s2], function(z)z>=3)
  cat(sprintf("basinc %%fav IT r8 %-8s: %.2f%% (N=%d)\n", lab, q["pct"], q["n"]))
}
addB("sociodemo: basinc destra 52,1 centro 55,5 sinistra 55,5",
     ifelse(abs(wpct(xb8[which(lr8b>=7)], it8$anweight[which(lr8b>=7)], function(z)z>=3)["pct"]-52.1)<=0.3,"OK","SCARTO"),"")
s2 <- which(vv(it8$mbtru,1:3)==2); q <- wpct(xb8[s2], it8$anweight[s2], function(z)z>=3)
cat(sprintf("basinc %%fav IT r8 ex iscritti: %.2f%% (N=%d) [dich. 58,4]\n", q["pct"], q["n"]))
ag8b <- vv(it8$agea,15:130)
for (lab in c("15-34","55+")) {
  s2 <- if (lab=="15-34") which(ag8b>=15&ag8b<=34) else which(ag8b>=55)
  q <- wpct(xb8[s2], it8$anweight[s2], function(z)z>=3)
  cat(sprintf("basinc %%fav IT r8 eta %-6s: %.2f%% (N=%d) [dich. %s]\n", lab, q["pct"], q["n"],
              ifelse(lab=="15-34","65,0 (N=603)","56,1 (N=859)")))
}
lr11 <- vv(it11$lrscale,L010); tp11 <- vv(it11$tporgwk,1:6); gg11 <- vv(it11$gincdif,L5)
for (lab in c("pubblico","autonomi")) {
  s2 <- if (lab=="pubblico") which(tp11 %in% 1:3) else which(tp11==5)
  q <- wpct(gg11[s2], it11$anweight[s2], function(z)z<=2)
  cat(sprintf("gincdif %%acc IT r11 settore %-9s: %.2f%% (N=%d) [dich. %s]\n", lab, q["pct"], q["n"],
              ifelse(lab=="pubblico","78,1 (N=379)","82,4 (N=253)")))
}
# uentrjb tra disoccupati vs occupati r8
mn8b <- vv(it8$mnactic,1:9); xu8 <- vv(it8$uentrjb,L5)
for (lab in c("disoccupati","occupati")) {
  s2 <- if (lab=="disoccupati") which(mn8b %in% 3:4) else which(mn8b==1)
  q <- wpct(xu8[s2], it8$anweight[s2], function(z)z<=2)
  cat(sprintf("uentrjb %%acc IT r8 %-12s: %.2f%% (N=%d) [dich. %s]\n", lab, q["pct"], q["n"],
              ifelse(lab=="disoccupati","29,9 (N=246)","39,5 (N=1197)")))
}
# distribuzione voto 2022 grezza
cat("\nDistribuzione prtvteit IT r11 (conteggi non ponderati):\n")
print(it11[, .N, by=.(prtvteit)][order(prtvteit)])

cat("\n----- ESITI PARTE B -----\n")
BB <- rbindlist(B); print(BB, nrows=100)
cat("\nOK:", BB[esito=="OK",.N], "| NON OK:", BB[esito!="OK",.N], "\n")
if (BB[esito!="OK",.N]>0) print(BB[esito!="OK"])

# ============================================================================
# PARTE C — coerenza interna dei CSV
# ============================================================================
cat("\n\n#########################################################\n")
cat("PARTE C — COERENZA INTERNA CSV\n")
cat("#########################################################\n")
EST <- file.path(BASE,"output","estrazioni")
for (f in c("redistribuzione_serie.csv","sussidi_atteggiamenti.csv","clima_serie.csv",
            "gruppi_voto.csv","indice_paesi.csv","giustizia_principi.csv","ruolostato_responsabilita.csv")) {
  cat("\n===", f, "===\n")
  x <- fread(file.path(EST,f))
  cat("righe:", nrow(x), "| colonne:", paste(names(x),collapse=","), "\n")
  if ("n_validi" %in% names(x)) {
    cat(sprintf("n_validi: min %d, max %d | righe con N<50: %d | righe con N<100: %d\n",
                min(x$n_validi,na.rm=TRUE), max(x$n_validi,na.rm=TRUE),
                sum(x$n_validi<50,na.rm=TRUE), sum(x$n_validi<100,na.rm=TRUE)))
    if (sum(x$n_validi<100,na.rm=TRUE)>0) print(x[n_validi<100])
  }
  if ("tipo_valore" %in% names(x)) {
    print(x[, .N, by=tipo_valore])
    # somma accordo+neutro+contrario ~ 100
    w <- x[tipo_valore %in% c("pct_accordo","pct_neutro","pct_contrario","pct_disaccordo")]
    if (nrow(w)>0) {
      key <- w[, .(tot=sum(valore), k=.N), by=.(variabile,essround,aggregato,gruppo)]
      bad <- key[k==3 & abs(tot-100)>0.35]
      cat(sprintf("triplette accordo/neutro/contrario complete: %d | fuori da 100+-0,35: %d\n",
                  key[k==3,.N], nrow(bad)))
      if (nrow(bad)>0) print(head(bad,20))
      if (key[k!=3,.N]>0) { cat("gruppi incompleti (k!=3):\n"); print(head(key[k!=3],10)) }
    }
    if (any(grepl("^pct", x$tipo_valore))) {
      out <- x[grepl("^pct", tipo_valore) & (valore < 0 | valore > 100)]
      cat("valori % fuori [0,100]:", nrow(out), "\n")
    }
    if (any(x$tipo_valore=="media")) {
      md <- x[tipo_valore=="media"]
      cat("medie fuori range plausibile (<-4 o >10):", md[valore < -4 | valore > 10, .N], "\n")
    }
  }
  if (f=="indice_paesi.csv") {
    cat("rank1 e' una permutazione 1..21:", identical(sort(x$rank_indice1), 1:21), "\n")
    cat("rank2 e' una permutazione 1..21:", identical(sort(x$rank_indice2), 1:21), "\n")
    cat("coerenza rank1 vs ordine indice1:", identical(x[order(-indice1), rank_indice1], 1:21), "\n")
    cat("coerenza rank2 vs ordine indice2:", identical(x[order(-indice2), rank_indice2], 1:21), "\n")
    cat("indice1 in [0,10]:", all(x$indice1>=0 & x$indice1<=10), "| indice2 in [-1,1]:", all(abs(x$indice2)<=1), "\n")
    cat("Spearman indice1 vs indice2:", round(cor(x$indice1,x$indice2,method="spearman"),4), "[dich. 0,878]\n")
  }
}

# ============================================================================
# PARTE D — ricalcolo RIGA PER RIGA di 4 CSV, confronto col valore pubblicato
# ============================================================================
cat("\n\n#########################################################\n")
cat("PARTE D — RICALCOLO RIGA PER RIGA DEI CSV\n")
cat("#########################################################\n")

paesi_agg <- function(agg, round) {
  if (agg == "Europa") paesi_round(d, round)
  else if (agg == "Europa-panel15") S15
  # Panel ristretti a composizione costante: alcune variabili non hanno risposte
  # valide in tutti gli 11 round per tutti e 15 i paesi (hincfel: FR a zero nei
  # round 1-2; stfgov: IE a zero nel round 1), quindi la serie usa il panel
  # ridotto in TUTTI i round. L'etichetta e' "Europa-panel<N>-no<XX><YY>...".
  else if (grepl("^Europa-panel[0-9]+-no", agg))
    setdiff(S15, regmatches(sub("^Europa-panel[0-9]+-no", "", agg),
                            gregexpr("[A-Z]{2}", sub("^Europa-panel[0-9]+-no", "", agg)))[[1]])
  else agg
}
valid_for <- function(v) {
  if (v %in% c("gvslvol","gvslvue","gvcldcr","gvhlthc","gvpdlwk","gvjbevn",
               "stfeco","stfgov","trstprl","trstplt","lrscale","euftf","imbgeco","evfrjob")) 0:10
  else if (v %in% c("topinfr","netifr","btminfr","grspfr","wltdffr")) -4:4
  else if (v == "hincfel") 1:4
  else if (v == "basinc") 1:4
  else if (v %in% c("recskil","recexp","recknow","recimg","recgndr")) 1:4
  else 1:5
}

verifica_csv <- function(f, tol_pct = 0.15, tol_media = 0.02) {
  cat("\n---", f, "---\n")
  x <- fread(file.path(EST, f))
  x <- x[gruppo == "tutti"]
  bad <- 0; tot <- 0
  x[, mio := NA_real_]; x[, mio_n := NA_integer_]
  for (i in seq_len(nrow(x))) {
    v <- x$variabile[i]; rr <- x$essround[i]; agg <- x$aggregato[i]; tv <- x$tipo_valore[i]
    pp <- paesi_agg(agg, rr); val <- valid_for(v)
    s <- d[cntry %in% pp & essround == rr]
    z <- vv(s[[v]], val)
    if (sum(!is.na(z)) == 0) next
    m <- switch(tv,
      "media"          = wmean(z, s$anweight)["m"],
      "pct_accordo"    = wpct(z, s$anweight, function(q) q <= 2)["pct"],
      "pct_neutro"     = wpct(z, s$anweight, function(q) q == 3)["pct"],
      "pct_contrario"  = wpct(z, s$anweight, function(q) q >= 4)["pct"],
      "pct_disaccordo" = wpct(z, s$anweight, function(q) q >= 4)["pct"],
      "pct_7_10"       = wpct(z, s$anweight, function(q) q >= 7)["pct"],
      "pct_difficolta" = wpct(z, s$anweight, function(q) q >= 3)["pct"],
      NA_real_)
    if (is.na(m)) next
    set(x, i, "mio", as.numeric(m)); set(x, i, "mio_n", sum(!is.na(z)))
    tot <- tot + 1
    tl <- if (tv == "media") tol_media else tol_pct
    if (abs(m - x$valore[i]) > tl) bad <- bad + 1
  }
  x[, delta := mio - valore]; x[, delta_n := mio_n - n_validi]
  cat(sprintf("righe verificate: %d | scarti > tolleranza: %d | max |delta valore|: %.4f | max |delta N|: %d\n",
              tot, bad, max(abs(x$delta), na.rm = TRUE), max(abs(x$delta_n), na.rm = TRUE)))
  if (bad > 0) print(x[abs(delta) > pmax(tol_pct, tol_media)][order(-abs(delta))][1:min(15,.N),
                       .(variabile, essround, aggregato, tipo_valore, valore, mio, delta, n_validi, mio_n)])
  if (x[abs(delta_n) > 0, .N] > 0) {
    cat("righe con N diverso:\n")
    print(x[abs(delta_n) > 0][1:min(15,.N), .(variabile, essround, aggregato, tipo_valore, n_validi, mio_n, delta_n)])
  }
  invisible(x)
}
for (f in c("redistribuzione_serie.csv", "clima_serie.csv",
            "sussidi_atteggiamenti.csv", "giustizia_principi.csv",
            "ruolostato_responsabilita.csv")) verifica_csv(f)

# Controllo N<50 / N<100 su TUTTI i CSV della cartella
cat("\n--- N minimi su tutti i CSV della cartella ---\n")
for (f in list.files(EST, pattern = "\\.csv$")) {
  x <- fread(file.path(EST, f))
  nc <- intersect(c("n_validi","n_validi_indice1","n_validi_indice2","n"), names(x))
  if (length(nc) == 0) { cat(sprintf("%-38s (nessuna colonna N)\n", f)); next }
  mn <- min(unlist(x[, ..nc]), na.rm = TRUE)
  cat(sprintf("%-38s N min %6d | righe N<100: %3d | righe N<50: %d\n", f, mn,
              sum(unlist(x[, ..nc]) < 100, na.rm = TRUE), sum(unlist(x[, ..nc]) < 50, na.rm = TRUE)))
}

# ---- controllo arrotondamento: l'ultimo decimale pubblicato e' corretto? ----
cat("\n--- Arrotondamento all'ultimo decimale ---\n")
for (f in c("redistribuzione_serie.csv","clima_serie.csv","sussidi_atteggiamenti.csv",
            "giustizia_principi.csv","ruolostato_responsabilita.csv")) {
  x <- verifica_csv_quiet <- NULL
  x <- fread(file.path(EST, f)); x <- x[gruppo == "tutti"]; x[, mio := NA_real_]
  for (i in seq_len(nrow(x))) {
    pp <- paesi_agg(x$aggregato[i], x$essround[i]); s <- d[cntry %in% pp & essround == x$essround[i]]
    z <- vv(s[[x$variabile[i]]], valid_for(x$variabile[i])); if (sum(!is.na(z)) == 0) next
    m <- switch(x$tipo_valore[i],
      "media" = wmean(z, s$anweight)["m"],
      "pct_accordo" = wpct(z, s$anweight, function(q) q <= 2)["pct"],
      "pct_neutro" = wpct(z, s$anweight, function(q) q == 3)["pct"],
      "pct_contrario" = , "pct_disaccordo" = wpct(z, s$anweight, function(q) q >= 4)["pct"],
      "pct_7_10" = wpct(z, s$anweight, function(q) q >= 7)["pct"],
      "pct_difficolta" = wpct(z, s$anweight, function(q) q >= 3)["pct"], NA_real_)
    if (!is.na(m)) set(x, i, "mio", as.numeric(m))
  }
  x <- x[!is.na(mio)]
  nd <- x[, sum(abs(round(mio, ifelse(tipo_valore == "media", 2, 1)) - valore) > 1e-9)]
  cat(sprintf("%-34s righe %4d | ultimo decimale diverso: %3d (%.1f%%) | max |delta| %.4f\n",
              f, nrow(x), nd, 100 * nd / nrow(x), max(abs(x$mio - x$valore))))
}
cat("\nFINE VERIFICA\n")
