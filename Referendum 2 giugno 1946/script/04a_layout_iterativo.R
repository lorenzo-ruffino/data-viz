#' Calcola il layout repel A BLOCCHI con snapshot e report di overlap dopo
#' ogni blocco. Permette di:
#'   - Salvare progressi intermedi (output/repel_layout_partial.rds)
#'   - Riprendere da uno snapshot se rilanciato
#'   - Vedere SE la repulsione sta convergendo prima di aspettare un'ora
#'
#' I top 50 cerchi (citta' grandi) sono FISSI dall'inizio con weight=1e10:
#'   nessuna "fase 1" preliminare in cui si spostano. Cosi' evitiamo il
#'   buco vuoto attorno a Milano/Napoli/Roma.

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(packcircles)
  library(digest)
})
sf_use_s2(FALSE)

PROJ_DIR <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Referendum 2 giugno 1946"
OUT      <- file.path(PROJ_DIR, "output")

DIAM_MIN_MM <- 0.50    # raggio min ~1670 m: cerchi che si "toccano" sotto
                       # i 3,3 km di distanza fra centroidi -> attiva il
                       # repel reale nelle aree dense (Brianza, hinterland
                       # Napoli, valli alpine)
DIAM_MAX_MM <- 7.0
N_ANCHOR    <- 50
BLOCK_ITER  <- 250
MAX_ITER    <- 2000
MIN_FILTER  <- 100    # diff_abs minimo per essere in mappa

# ---- 1) Carico dati e calcolo raggi ---------------------------------------

g <- st_read(file.path(OUT, "referendum_1946_su_confini_1991.geojson"), quiet = TRUE) %>%
  filter(!st_is_empty(geometry)) %>%
  mutate(NUMVOTISI = as.numeric(NUMVOTISI),
         NUMVOTINO = as.numeric(NUMVOTINO),
         diff_si_no = NUMVOTISI - NUMVOTINO,
         diff_abs   = abs(diff_si_no),
         vincitore  = ifelse(diff_si_no > 0, "SI", "NO"))

gc <- st_centroid(g) %>% st_transform(32632)
coords <- st_coordinates(gc)
df <- gc %>% st_drop_geometry() %>%
  mutate(x = coords[, 1], y = coords[, 2]) %>%
  filter(diff_abs >= MIN_FILTER)

cat("Cerchi totali:", nrow(df), "\n")

italy_height_m  <- diff(range(df$y))
panel_height_mm <- 8 * 25.4 * 0.87
m_per_mm        <- italy_height_m / panel_height_mm

diff_max <- max(df$diff_abs)
df$radius_visual_m <- (DIAM_MIN_MM + (DIAM_MAX_MM - DIAM_MIN_MM) *
                       df$diff_abs / diff_max) / 2 * m_per_mm
df$radius <- df$radius_visual_m

# Top N: fissi DALL'INIZIO (weight = 1e10), raggio repulsivo ridotto al
# minimo dei piccoli (~1670m). Visivamente sono cerchi grandi (raggio visivo
# 17-24 km), ma durante la repulsione non spingono via l'hinterland.
idx_top <- order(df$diff_abs, decreasing = TRUE)[1:N_ANCHOR]
df$is_top <- FALSE; df$is_top[idx_top] <- TRUE
df$weight <- 1 + 99 * (df$diff_abs / diff_max)
df$weight[idx_top] <- 1e10
df$radius[idx_top] <- min(df$radius_visual_m)

# Forza di richiamo: ad ogni blocco, ogni nodo viene tirato indietro verso
# il suo centroide originale di PULL_STRENGTH della distanza percorsa. E'
# l'equivalente di forceX/forceY in d3-force.simulation. Con PULL=0.15 e
# repel attivo, l'equilibrio finale e' una mappa dove i cerchi sono separati
# (per la repulsione) ma "trattenuti" vicino alla loro posizione geografica
# vera (per la pull-back). Piu' alta = piu' fedeli geograficamente, ma con
# piu' overlap residuo. Piu' bassa = nodi piu' separati ma piu' "dispersi".
PULL_STRENGTH <- 0.15

cat("Raggi (m): min=", round(min(df$radius)), " max=", round(max(df$radius)), "\n", sep="")
cat("Top fissi (weight=1e10):", sum(df$is_top), "\n")
cat("\n")


# ---- 2) Funzione di metric: overlap ----------------------------------------

# Per ogni coppia di cerchi vicini, calcola se c'e' overlap e quanto.
# Usa un approccio basato su griglia per evitare O(N^2): suddivide il piano
# in celle di lato 2*max(radius), e confronta solo le coppie all'interno
# della stessa cella + 8 celle vicine.

compute_overlaps <- function(x, y, r) {
  n <- length(x)
  cell <- 2 * max(r)  # cella un po' piu' grande del cerchio max
  ci <- floor(x / cell)
  cj <- floor(y / cell)
  key <- paste(ci, cj, sep = "_")
  by_cell <- split(seq_len(n), key)

  count <- 0
  area_overlap <- 0
  max_overlap_frac <- 0

  # Costruisco per ogni cella l'elenco delle celle vicine (3x3)
  cells <- unique(data.frame(ci = ci, cj = cj))
  for (k in seq_len(nrow(cells))) {
    cci <- cells$ci[k]; ccj <- cells$cj[k]
    # punti nella cella e nelle 8 vicine
    local <- c()
    for (di in -1:1) for (dj in -1:1) {
      kk <- paste(cci + di, ccj + dj, sep = "_")
      if (!is.null(by_cell[[kk]])) local <- c(local, by_cell[[kk]])
    }
    me <- by_cell[[paste(cci, ccj, sep = "_")]]
    if (length(local) < 2 || length(me) == 0) next
    # confronto ogni punto della cella corrente con TUTTI quelli "local"
    for (i in me) {
      for (j in local) {
        if (j <= i) next  # evita doppi conteggi
        dx <- x[i] - x[j]; dy <- y[i] - y[j]
        d <- sqrt(dx*dx + dy*dy)
        ov <- (r[i] + r[j]) - d
        if (ov > 0) {
          count <- count + 1
          # Area di intersezione tra due cerchi
          ri <- r[i]; rj <- r[j]
          if (d < abs(ri - rj)) {
            a <- pi * min(ri, rj)^2  # uno contiene l'altro
          } else {
            a1 <- ri^2 * acos((d^2 + ri^2 - rj^2) / (2*d*ri))
            a2 <- rj^2 * acos((d^2 + rj^2 - ri^2) / (2*d*rj))
            a3 <- 0.5 * sqrt((-d+ri+rj)*(d+ri-rj)*(d-ri+rj)*(d+ri+rj))
            a <- a1 + a2 - a3
          }
          area_overlap <- area_overlap + a
          frac <- ov / (ri + rj)  # 0..1 (0 = appena tangenti)
          if (frac > max_overlap_frac) max_overlap_frac <- frac
        }
      }
    }
  }
  list(pairs = count, area_m2 = area_overlap, max_frac = max_overlap_frac)
}


# ---- 3) Loop iterativo a blocchi -----------------------------------------

snapshot_file <- file.path(OUT, "repel_layout_partial.rds")
x_cur <- df$x; y_cur <- df$y
iter_done <- 0

if (file.exists(snapshot_file)) {
  sn <- readRDS(snapshot_file)
  if (length(sn$x) == nrow(df)) {
    x_cur <- sn$x; y_cur <- sn$y
    # Forzo l'anchor dei top sulle posizioni geografiche corrette anche in
    # bootstrap (lo snapshot potrebbe averli leggermente derivati)
    x_cur[idx_top] <- df$x[idx_top]; y_cur[idx_top] <- df$y[idx_top]
    cat("Snapshot trovato: bootstrap dalle posizioni di iter", sn$iter,
        "(ricomincio da 0 con i nuovi raggi).\n")
  } else {
    cat("Snapshot esistente ma N diverso: riparto da zero.\n")
  }
}

# Metric iniziale
o <- compute_overlaps(x_cur, y_cur, df$radius)
cat(sprintf("iter %4d: overlap pairs = %5d  area = %.0f km²  max_frac = %.2f\n",
            iter_done, o$pairs, o$area_m2/1e6, o$max_frac))

while (iter_done < MAX_ITER) {
  block <- min(BLOCK_ITER, MAX_ITER - iter_done)
  t0 <- Sys.time()

  # Step 1: REPEL -- circleRepelLayout separa i cerchi in collisione
  lay <- circleRepelLayout(
    data.frame(x = x_cur, y = y_cur, radius = df$radius),
    xysizecols = c("x", "y", "radius"),
    sizetype   = "radius",
    maxiter    = block,
    weights    = df$weight
  )
  x_cur <- lay$layout$x; y_cur <- lay$layout$y

  # Step 2: PULL-BACK verso le posizioni geografiche originali (forceX/Y di
  # d3). Solo per i non-top -- i top sono gia' riancorati dopo.
  not_top <- !df$is_top
  x_cur[not_top] <- x_cur[not_top] + PULL_STRENGTH * (df$x[not_top] - x_cur[not_top])
  y_cur[not_top] <- y_cur[not_top] + PULL_STRENGTH * (df$y[not_top] - y_cur[not_top])

  # Step 3: re-anchor top
  x_cur[idx_top] <- df$x[idx_top]; y_cur[idx_top] <- df$y[idx_top]

  iter_done <- iter_done + block
  dt <- as.numeric(Sys.time() - t0, units = "secs")
  o <- compute_overlaps(x_cur, y_cur, df$radius)
  # Statistica spostamento medio
  d_mean <- mean(sqrt((x_cur[not_top]-df$x[not_top])^2 + (y_cur[not_top]-df$y[not_top])^2))
  cat(sprintf("iter %4d: overlap pairs = %5d  area = %.0f km²  max_frac = %.2f  spost_avg = %.0f m  (block %.0fs)\n",
              iter_done, o$pairs, o$area_m2/1e6, o$max_frac, d_mean, dt))
  saveRDS(list(key = list(n = nrow(df), r = sum(df$radius)),
               x = x_cur, y = y_cur, iter = iter_done),
          snapshot_file)
  if (o$pairs < 50) {
    cat("Convergenza raggiunta (< 50 overlaps).\n")
    break
  }
}

# Salvo anche il layout finale "buono" sotto un nome stabile
df$x_new <- x_cur; df$y_new <- y_cur
saveRDS(df, file.path(OUT, "repel_layout_final.rds"))
cat("\nSalvato: output/repel_layout_final.rds (puo' essere usato da 04b_plot.R)\n")
