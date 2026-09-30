# ============================================================================
# 20_indice_sintetico.R
# Indicatore sintetico "pro-Stato" — European Social Survey round 8 (2016)
#
# 1. INDICE RUOLO DELLO STATO: media degli item gv* "responsabilita' del
#    governo" (0-10), media ponderata anweight per paese.
# 2. INDICE COMPOSITO PRO-STATO: media di z-score (standardizzati sulla
#    popolazione pooled europea, ponderata anweight) di 9 item orientati.
# 3. ROBUSTEZZA: leave-one-out sulla classifica, Spearman indice1 vs indice2,
#    validazione round 4 della riduzione della batteria gv*.
#
# NOTA METODOLOGICA VINCOLANTE (verificata sui dati, non assunta):
#   nel round 8 la batteria "governments' responsibility" comprende SOLO
#   gvslvol, gvslvue, gvcldcr. Gli item gvhlthc, gvpdlwk, gvjbevn sono
#   interamente NA nel round 8 (esistono solo nel round 4, dove pero' l'Italia
#   non e' presente). L'indice 1 usa quindi i 3 item effettivamente rilevati,
#   con soglia di validita' proporzionale (>=2 su 3 invece di >=4 su 6).
#   La sezione di robustezza quantifica sul round 4 quanto la riduzione da 6 a
#   3 item sposti la classifica dei paesi.
#
# Riproducibile da solo: legge input/ess_slim.rds, scrive output/estrazioni/.
# ============================================================================

library(data.table)

DIR <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Opinioni economiche ESS"
OUT <- file.path(DIR, "output", "estrazioni")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

ROUND <- 8L
ANNO  <- 2016L
TEMA  <- "indice_sintetico"

# --- paesi UE27 + Regno Unito + Norvegia + Svizzera + Islanda (DATA_MAP) -----
EUROPA <- c("AT","BE","BG","CH","CY","CZ","DE","DK","EE","ES","FI","FR","GB",
            "GR","HR","HU","IE","IS","IT","LT","LU","LV","NL","NO","PL","PT",
            "RO","SE","SI","SK")

NOMI <- c(AT="Austria", BE="Belgio", BG="Bulgaria", CH="Svizzera", CY="Cipro",
          CZ="Cechia", DE="Germania", DK="Danimarca", EE="Estonia",
          ES="Spagna", FI="Finlandia", FR="Francia", GB="Regno Unito",
          GR="Grecia", HR="Croazia", HU="Ungheria", IE="Irlanda",
          IS="Islanda", IT="Italia", LT="Lituania", LU="Lussemburgo",
          LV="Lettonia", NL="Paesi Bassi", NO="Norvegia", PL="Polonia",
          PT="Portogallo", RO="Romania", SE="Svezia", SI="Slovenia",
          SK="Slovacchia")

# --- helper -----------------------------------------------------------------
# filtro sui valori validi del codebook: i codici missing (7/8/9, 77/88/99,
# 999) sono NUMERI nel dataset, vanno esclusi esplicitamente
val <- function(x, lo, hi) {
  y <- as.numeric(x)
  y[is.na(y) | y < lo | y > hi] <- NA_real_
  y
}
wmean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}
wsd <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (sum(ok) < 2L) return(NA_real_)
  m <- sum(x[ok] * w[ok]) / sum(w[ok])
  sqrt(sum(w[ok] * (x[ok] - m)^2) / sum(w[ok]))
}
# media riga per riga sugli item disponibili, NA se meno di `minval` validi
row_mean_min <- function(M, minval) {
  nv <- rowSums(!is.na(M))
  out <- rowMeans(M, na.rm = TRUE)
  out[nv < minval] <- NA_real_
  out
}

# ============================================================================
# 0. DATI
# ============================================================================
d <- readRDS(file.path(DIR, "input", "ess_slim.rds"))
stopifnot(is.data.table(d), d[is.na(anweight), .N] == 0)

# verifica empirica della disponibilita' degli item gv* nel round 8
gv_full <- c("gvslvol","gvslvue","gvhlthc","gvcldcr","gvpdlwk","gvjbevn")
disp_r8 <- sapply(gv_full, function(v) sum(!is.na(d[essround == ROUND][[v]])))
cat("--- item gv* nel round 8 (record non-NA) ---\n"); print(disp_r8)
GV_R8 <- names(disp_r8)[disp_r8 > 0]
cat("Item gv* utilizzabili nel round 8:", paste(GV_R8, collapse = ", "), "\n\n")
stopifnot(length(GV_R8) >= 2L)

r8 <- d[essround == ROUND & cntry %in% EUROPA]
cat("Round 8 — paesi europei:", uniqueN(r8$cntry), "| N non ponderato:", nrow(r8), "\n")
cat(paste(sort(unique(r8$cntry)), collapse = " "), "\n\n")

# ============================================================================
# 1. INDICE RUOLO DELLO STATO (0-10)
# ============================================================================
for (v in GV_R8) set(r8, j = paste0("v_", v), value = val(r8[[v]], 0, 10))
M1 <- as.matrix(r8[, paste0("v_", GV_R8), with = FALSE])
MIN1 <- max(2L, ceiling(length(GV_R8) * 4 / 6))   # soglia proporzionale a 4/6
r8[, indice1 := row_mean_min(M1, MIN1)]
cat("Indice 1: item =", paste(GV_R8, collapse = ", "),
    "| soglia validi >=", MIN1, "su", length(GV_R8), "\n")
cat("N con indice1 valido:", r8[!is.na(indice1), .N], "\n\n")

paesi1 <- r8[!is.na(indice1), .(indice1 = wmean(indice1, anweight),
                                n1 = .N), by = cntry]
europa1 <- r8[!is.na(indice1), .(v = wmean(indice1, anweight), n = .N)]

# ============================================================================
# 2. INDICE COMPOSITO PRO-STATO (z-score)
# ============================================================================
# orientamento: valore alto = piu' pro-Stato / pro-welfare
#   gincdif  1=molto d'accordo ("il governo deve ridurre le differenze di
#            reddito") -> invertito, l'accordo diventa il polo pro-Stato
#   gv*      0-10 gia' orientati (10 = interamente responsabilita' del governo)
#   sbeqsoc  1=molto d'accordo ("i servizi sociali rendono la societa' piu'
#            equa") -> invertito, l'accordo e' il polo pro-welfare
#   sbstrec / sbbsntx / sblazy  affermazioni ANTI-welfare (pesano sull'economia,
#            costano troppo alle imprese, rendono pigri): il disaccordo (4-5) e'
#            il polo pro-welfare, quindi la scala grezza e' gia' orientata
#   basinc   1=fortemente contrario ... 4=fortemente favorevole -> gia' orientato
spec <- data.table(
  item = c("gincdif", GV_R8, "sbeqsoc", "sbstrec", "sbbsntx", "sblazy", "basinc"),
  lo   = c(1,  rep(0, length(GV_R8)), 1, 1, 1, 1, 1),
  hi   = c(5,  rep(10, length(GV_R8)), 5, 5, 5, 5, 4),
  rev  = c(TRUE, rep(FALSE, length(GV_R8)), TRUE, FALSE, FALSE, FALSE, FALSE)
)
ITEMS <- spec$item
cat("Indice 2: ", length(ITEMS), " item ->", paste(ITEMS, collapse = ", "), "\n")

for (i in seq_len(nrow(spec))) {
  it <- spec$item[i]
  x <- val(r8[[it]], spec$lo[i], spec$hi[i])
  if (spec$rev[i]) x <- (spec$lo[i] + spec$hi[i]) - x
  set(r8, j = paste0("o_", it), value = x)
}

# z-score sulla popolazione pooled europea, pesata anweight
zpar <- rbindlist(lapply(ITEMS, function(it) {
  x <- r8[[paste0("o_", it)]]
  data.table(item = it, media_eu = wmean(x, r8$anweight),
             sd_eu = wsd(x, r8$anweight), n_validi = sum(!is.na(x)))
}))
print(zpar)
stopifnot(all(zpar$sd_eu > 0), all(zpar$n_validi > 0))

for (i in seq_len(nrow(zpar))) {
  it <- zpar$item[i]
  set(r8, j = paste0("z_", it),
      value = (r8[[paste0("o_", it)]] - zpar$media_eu[i]) / zpar$sd_eu[i])
}

Z <- as.matrix(r8[, paste0("z_", ITEMS), with = FALSE])
MIN2 <- 6L                       # regola letterale: almeno 6 item validi
r8[, indice2 := row_mean_min(Z, MIN2)]
cat("Soglia validi indice2 >=", MIN2, "su", length(ITEMS),
    "| N valido:", r8[!is.na(indice2), .N],
    "| (con soglia >=5:", sum(rowSums(!is.na(Z)) >= 5L), ")\n\n")

paesi2 <- r8[!is.na(indice2), .(indice2 = wmean(indice2, anweight),
                                n2 = .N), by = cntry]
europa2 <- r8[!is.na(indice2), .(v = wmean(indice2, anweight), n = .N)]

# --- tabella paesi ----------------------------------------------------------
paesi <- merge(paesi1, paesi2, by = "cntry", all = TRUE)
paesi[, paese := NOMI[cntry]]
paesi[, rank_indice1 := frank(-indice1, ties.method = "min")]
paesi[, rank_indice2 := frank(-indice2, ties.method = "min")]
setorder(paesi, rank_indice2)
paesi_out <- paesi[, .(paese, cntry, essround = ROUND, anno = ANNO,
                       indice1 = round(indice1, 3), rank_indice1,
                       indice2 = round(indice2, 4), rank_indice2,
                       n_validi_indice1 = n1, n_validi_indice2 = n2)]
fwrite(paesi_out, file.path(OUT, "indice_paesi.csv"))
cat("--- CLASSIFICA (ordinata per indice composito) ---\n")
print(paesi_out)
cat("\nMedia Europa indice1:", round(europa1$v, 3), "(N", europa1$n, ")",
    "| indice2:", round(europa2$v, 4), "(N", europa2$n, ")\n\n")

# formato lungo DATA_MAP
lungo <- rbindlist(list(
  paesi[, .(tema = TEMA, variabile = paste(GV_R8, collapse = "+"),
            label_it = "Indice ruolo dello Stato (media item responsabilita' del governo, 0-10)",
            essround = ROUND, anno = ANNO, aggregato = cntry, gruppo = "tutti",
            categoria = "indice", valore = round(indice1, 3),
            tipo_valore = "media_indice_0_10", n_validi = n1)],
  paesi[, .(tema = TEMA, variabile = paste(GV_R8, collapse = "+"),
            label_it = "Posizione in classifica, indice ruolo dello Stato",
            essround = ROUND, anno = ANNO, aggregato = cntry, gruppo = "tutti",
            categoria = "indice", valore = rank_indice1,
            tipo_valore = "rank", n_validi = n1)],
  paesi[, .(tema = TEMA, variabile = paste(ITEMS, collapse = "+"),
            label_it = "Indice composito pro-Stato (media z-score, 0 = media europea)",
            essround = ROUND, anno = ANNO, aggregato = cntry, gruppo = "tutti",
            categoria = "indice", valore = round(indice2, 4),
            tipo_valore = "media_zscore", n_validi = n2)],
  paesi[, .(tema = TEMA, variabile = paste(ITEMS, collapse = "+"),
            label_it = "Posizione in classifica, indice composito pro-Stato",
            essround = ROUND, anno = ANNO, aggregato = cntry, gruppo = "tutti",
            categoria = "indice", valore = rank_indice2,
            tipo_valore = "rank", n_validi = n2)],
  data.table(tema = TEMA, variabile = paste(GV_R8, collapse = "+"),
             label_it = "Indice ruolo dello Stato (media item responsabilita' del governo, 0-10)",
             essround = ROUND, anno = ANNO, aggregato = "Europa", gruppo = "tutti",
             categoria = "indice", valore = round(europa1$v, 3),
             tipo_valore = "media_indice_0_10", n_validi = europa1$n),
  data.table(tema = TEMA, variabile = paste(ITEMS, collapse = "+"),
             label_it = "Indice composito pro-Stato (media z-score, 0 = media europea)",
             essround = ROUND, anno = ANNO, aggregato = "Europa", gruppo = "tutti",
             categoria = "indice", valore = round(europa2$v, 4),
             tipo_valore = "media_zscore", n_validi = europa2$n)
))
fwrite(lungo, file.path(OUT, "indice_paesi_long.csv"))

# ============================================================================
# 3. GRUPPI SOCIODEMOGRAFICI — ITALIA
# ============================================================================
it <- r8[cntry == "IT"]
cat("Italia round 8 — N:", nrow(it),
    "| indice1 valido:", it[!is.na(indice1), .N],
    "| indice2 valido:", it[!is.na(indice2), .N], "\n\n")

g <- function(x) factor(x)
it[, `:=`(
  g_eta = {a <- val(agea, 15, 130)
           fifelse(is.na(a), NA_character_,
           fifelse(a < 35, "15-34", fifelse(a < 55, "35-54", "55+")))},
  g_genere = {x <- val(gndr, 1, 2)
              fifelse(is.na(x), NA_character_, fifelse(x == 1, "uomini", "donne"))},
  g_istruzione = {x <- val(eisced, 1, 7)
                  fifelse(is.na(x), NA_character_,
                  fifelse(x <= 2, "bassa", fifelse(x <= 4, "media", "alta")))},
  g_condizione = {x <- val(mnactic, 1, 9)
                  fifelse(is.na(x), NA_character_,
                  fifelse(x == 1, "occupati",
                  fifelse(x %in% c(3, 4), "disoccupati",
                  fifelse(x == 6, "pensionati",
                  fifelse(x == 2, "studenti", "altro inattivo")))))},
  g_settore = {x <- val(tporgwk, 1, 5)
               fifelse(is.na(x), NA_character_,
               fifelse(x <= 3, "pubblico",
               fifelse(x == 4, "privato dipendente", "autonomi")))},
  g_reddito_percepito = {x <- val(hincfel, 1, 4)
                         fifelse(is.na(x), NA_character_,
                         fifelse(x == 1, "vive comodamente",
                         fifelse(x == 2, "se la cava",
                         fifelse(x == 3, "difficolta'", "grande difficolta'"))))},
  g_decile_reddito = {x <- val(hinctnta, 1, 10)
                      fifelse(is.na(x), NA_character_,
                      fifelse(x <= 3, "basso (1-3)",
                      fifelse(x <= 7, "medio (4-7)", "alto (8-10)")))},
  g_sindacato = {x <- val(mbtru, 1, 3)
                 fifelse(is.na(x), NA_character_,
                 fifelse(x == 1, "iscritto ora",
                 fifelse(x == 2, "iscritto in passato", "mai iscritto")))},
  g_politica = {x <- val(lrscale, 0, 10)
                fifelse(is.na(x), NA_character_,
                fifelse(x <= 3, "sinistra (0-3)",
                fifelse(x <= 6, "centro (4-6)", "destra (7-10)")))}
)]

GRUPPI <- c(eta = "g_eta", genere = "g_genere", istruzione = "g_istruzione",
            condizione = "g_condizione", settore = "g_settore",
            reddito_percepito = "g_reddito_percepito",
            decile_reddito = "g_decile_reddito", sindacato = "g_sindacato",
            politica = "g_politica")

calc_gruppi <- function(dt, ind, tipo, label, variabile) {
  res <- rbindlist(lapply(names(GRUPPI), function(nm) {
    col <- GRUPPI[[nm]]
    sub <- dt[!is.na(get(ind)) & !is.na(get(col))]
    if (!nrow(sub)) return(NULL)
    z <- sub[, .(valore = wmean(get(ind), anweight), n_validi = .N), by = col]
    setnames(z, col, "categoria")
    z[, gruppo := nm]
    z[, .(gruppo, categoria, valore, n_validi)]
  }))
  tot <- dt[!is.na(get(ind)), .(gruppo = "tutti", categoria = "totale",
                                valore = wmean(get(ind), anweight), n_validi = .N)]
  res <- rbind(tot, res)
  res[, `:=`(tema = TEMA, variabile = variabile, label_it = label,
             essround = ROUND, anno = ANNO, aggregato = "IT",
             tipo_valore = tipo)]
  res[]
}

gr <- rbind(
  calc_gruppi(it, "indice1", "media_indice_0_10",
              "Indice ruolo dello Stato (media item responsabilita' del governo, 0-10)",
              paste(GV_R8, collapse = "+")),
  calc_gruppi(it, "indice2", "media_zscore",
              "Indice composito pro-Stato (media z-score, 0 = media europea)",
              paste(ITEMS, collapse = "+"))
)
# regola celle piccole: N<50 non riportato, N<100 segnalato
gr[, nota := fifelse(n_validi < 50, "N<50 — stima non riportata",
             fifelse(n_validi < 100, "N<100 — stima poco affidabile", ""))]
gr[n_validi < 50, valore := NA_real_]
gr[, valore := fifelse(tipo_valore == "media_indice_0_10",
                       round(valore, 3), round(valore, 4))]
gruppi_out <- gr[, .(tema, variabile, label_it, essround, anno, aggregato,
                     gruppo, categoria, valore, tipo_valore, n_validi, nota)]
setorder(gruppi_out, tipo_valore, gruppo, categoria)
fwrite(gruppi_out, file.path(OUT, "indice_gruppi_italia.csv"))
cat("--- GRUPPI ITALIA ---\n")
print(gruppi_out[tipo_valore == "media_zscore", .(gruppo, categoria, valore, n_validi, nota)])
cat("\n")
print(gruppi_out[tipo_valore == "media_indice_0_10", .(gruppo, categoria, valore, n_validi)])
cat("\n")

# ============================================================================
# 4. ROBUSTEZZA
# ============================================================================
rob <- list()

# il campione analitico resta quello della specifica di base: cosi' l'effetto
# misurato e' quello della rimozione dell'item, non della diversa selezione
loo <- function(dt, cols, base_paesi, nome_indice) {
  B <- dt[!is.na(get(nome_indice))]
  M <- as.matrix(B[, cols, with = FALSE])
  items <- sub("^[vz]_", "", cols)
  ref <- base_paesi[match(unique(B$cntry), cntry)]
  out <- lapply(seq_along(items), function(k) {
    B[, tmp := rowMeans(M[, -k, drop = FALSE], na.rm = TRUE)]
    B[is.nan(tmp), tmp := NA_real_]
    p <- B[!is.na(tmp), .(v = wmean(tmp, anweight), n = .N), by = cntry]
    p[, rk := frank(-v, ties.method = "min")]
    m <- base_paesi[match(p$cntry, cntry)]
    data.table(
      analisi = "leave_one_out", indice = nome_indice, item = items[k],
      n_item = length(items) - 1L, n_paesi = nrow(p),
      valore_it = round(p[cntry == "IT", v], 4), rank_it = p[cntry == "IT", rk],
      n_validi_it = p[cntry == "IT", n],
      statistica = round(cor(p$v, m[[nome_indice]], method = "spearman"), 4),
      max_shift_rank = max(abs(p$rk - m[[paste0("rank_", nome_indice)]])),
      nota = "rho di Spearman della classifica senza l'item vs classifica di base")
  })
  rbindlist(out)
}

rob[[length(rob) + 1]] <- loo(r8, paste0("v_", GV_R8), paesi, "indice1")
rob[[length(rob) + 1]] <- data.table(
  analisi = "specifica_base", indice = "indice1", item = "(nessuno)",
  n_item = length(GV_R8), n_paesi = nrow(paesi1),
  valore_it = round(paesi[cntry == "IT", indice1], 4),
  rank_it = paesi[cntry == "IT", rank_indice1],
  n_validi_it = paesi[cntry == "IT", n1], statistica = 1, max_shift_rank = 0L,
  nota = paste("item:", paste(GV_R8, collapse = "+")))

rob[[length(rob) + 1]] <- loo(r8, paste0("z_", ITEMS), paesi, "indice2")
rob[[length(rob) + 1]] <- data.table(
  analisi = "specifica_base", indice = "indice2", item = "(nessuno)",
  n_item = length(ITEMS), n_paesi = nrow(paesi2),
  valore_it = round(paesi[cntry == "IT", indice2], 4),
  rank_it = paesi[cntry == "IT", rank_indice2],
  n_validi_it = paesi[cntry == "IT", n2], statistica = 1, max_shift_rank = 0L,
  nota = paste("item:", paste(ITEMS, collapse = "+")))

rob <- rbindlist(rob, fill = TRUE)

# --- 4c. correlazione di Spearman indice1 vs indice2 a livello paese ---------
cmp <- paesi[!is.na(indice1) & !is.na(indice2)]
rho12 <- cor(cmp$indice1, cmp$indice2, method = "spearman")
rob <- rbind(rob, data.table(
  analisi = "spearman_indice1_vs_indice2", indice = "indice1~indice2",
  n_paesi = nrow(cmp), statistica = round(rho12, 4),
  max_shift_rank = max(abs(cmp$rank_indice1 - cmp$rank_indice2)),
  nota = "correlazione di rango tra i due indici sui paesi europei"), fill = TRUE)

# --- 4c-bis. diagnostica di coerenza interna e orientamento -----------------
# ogni item deve correlare POSITIVAMENTE con il resto della scala: se cosi' non
# fosse, l'orientamento sarebbe sbagliato
for (k in seq_along(ITEMS)) {
  r_it <- cor(Z[, k], rowMeans(Z[, -k, drop = FALSE], na.rm = TRUE),
              use = "complete.obs")
  rob <- rbind(rob, data.table(
    analisi = "correlazione_item_totale", indice = "indice2", item = ITEMS[k],
    statistica = round(r_it, 4),
    nota = "r item-resto della scala; positiva = orientamento corretto"),
    fill = TRUE)
}
Zc <- Z[complete.cases(Z), , drop = FALSE]
kI <- ncol(Zc)
alpha <- (kI / (kI - 1)) * (1 - sum(apply(Zc, 2, var)) / var(rowSums(Zc)))
rob <- rbind(rob, data.table(
  analisi = "alpha_cronbach", indice = "indice2", item = "(tutti)",
  n_item = kI, statistica = round(alpha, 4), n_validi_it = nrow(Zc),
  nota = "coerenza interna, non ponderata, su casi completi"), fill = TRUE)

# --- 4d. validazione round 4: batteria 6 item vs 3 item ---------------------
# l'Italia non e' nel round 4, ma il confronto misura quanto la riduzione della
# batteria (imposta dal questionario del round 8) sposti la classifica
r4 <- d[essround == 4 & cntry %in% EUROPA]
if (nrow(r4) > 0) {
  for (v in gv_full) set(r4, j = paste0("v_", v), value = val(r4[[v]], 0, 10))
  M6 <- as.matrix(r4[, paste0("v_", gv_full), with = FALSE])
  M3 <- as.matrix(r4[, paste0("v_", GV_R8), with = FALSE])
  r4[, i6 := row_mean_min(M6, 4L)]
  r4[, i3 := row_mean_min(M3, 2L)]
  p6 <- r4[!is.na(i6), .(v6 = wmean(i6, anweight)), by = cntry]
  p3 <- r4[!is.na(i3), .(v3 = wmean(i3, anweight)), by = cntry]
  pc <- merge(p6, p3, by = "cntry")
  pc[, `:=`(r6 = frank(-v6, ties.method = "min"), r3 = frank(-v3, ties.method = "min"))]
  rho63 <- cor(pc$v6, pc$v3, method = "spearman")
  maxshift <- max(abs(pc$r6 - pc$r3))
  cat("--- Validazione round 4: indice a 6 item vs 3 item ---\n")
  print(pc[order(r6)])
  cat("Spearman 6 vs 3 item (round 4,", nrow(pc), "paesi):", round(rho63, 4),
      "| massimo spostamento di posizione:", maxshift, "\n\n")
  rob <- rbind(rob, data.table(
    analisi = "validazione_r4_6item_vs_3item", indice = "indice1",
    item = paste(setdiff(gv_full, GV_R8), collapse = "+"),
    n_item = 3L, n_paesi = nrow(pc), statistica = round(rho63, 4),
    max_shift_rank = maxshift,
    nota = paste("round 4 (2008), unico round con tutti e 6 gli item;",
                 "l'Italia non e' nel round 4. rho tra indice a 6 item e",
                 "indice ai soli 3 item disponibili nel round 8")), fill = TRUE)
}

rob[, `:=`(tema = TEMA, essround = ROUND, anno = ANNO)]
setcolorder(rob, c("tema", "essround", "anno", "analisi", "indice", "item",
                   "n_item", "n_paesi", "valore_it", "rank_it", "n_validi_it",
                   "statistica", "max_shift_rank", "nota"))
fwrite(rob, file.path(OUT, "indice_robustezza.csv"))

cat("--- ROBUSTEZZA ---\n")
print(rob[, .(analisi, indice, item, valore_it, rank_it, statistica, max_shift_rank)])
loo1 <- rob[analisi == "leave_one_out" & indice == "indice1", rank_it]
loo2 <- rob[analisi == "leave_one_out" & indice == "indice2", rank_it]
cat("\nRange posizione Italia — indice1 LOO:", min(loo1), "-", max(loo1),
    "(base:", paesi[cntry == "IT", rank_indice1], ")\n")
cat("Range posizione Italia — indice2 LOO:", min(loo2), "-", max(loo2),
    "(base:", paesi[cntry == "IT", rank_indice2], ")\n")
cat("Spearman indice1 vs indice2 (", nrow(cmp), "paesi):", round(rho12, 4), "\n")
cat("Alpha di Cronbach indice composito (", kI, "item, N", nrow(Zc), "):",
    round(alpha, 3), "\n")
cat("Correlazioni item-totale (tutte positive = orientamento corretto):\n")
print(rob[analisi == "correlazione_item_totale", .(item, r = statistica)])
cat("\nFile scritti in", OUT, "\n")
print(list.files(OUT))
