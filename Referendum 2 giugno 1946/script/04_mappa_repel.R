#' Mappa "a cerchi che si auto-respingono" dei risultati del referendum
#' istituzionale del 2 giugno 1946, sullo stile di
#'   Referendum_Giustizia_2026/mappa_risultati_repel.r
#'
#' Differenze chiave rispetto al 2026:
#'   - Sì = Repubblica (rosso), No = Monarchia (blu) -- colori invertiti
#'   - I comuni 1946 sono mappati sui confini Istat 1991 (vedi
#'     output/referendum_1946_su_confini_1991.geojson) tramite il ponte
#'     costruito da 01_costruisci_ponte.R
#'   - Le coordinate sono il centroide del poligono aggregato 1991

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(data.table)
  library(sf)
  library(packcircles)
  library(showtext)
  library(digest)
})
font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
sf_use_s2(FALSE)

PROJ_DIR <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Referendum 2 giugno 1946"

theme_map <- function() {
  theme_minimal() +
    theme(
      text             = element_text(family = "Source Sans Pro"),
      axis.text        = element_blank(),
      axis.title       = element_blank(),
      axis.line        = element_blank(),
      axis.ticks       = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background  = element_blank(),
      panel.border     = element_blank(),
      legend.position  = "bottom",
      legend.box       = "vertical",
      legend.background     = element_blank(),
      legend.box.background = element_blank(),
      legend.key            = element_blank(),
      legend.title          = element_text(size = 11, color = "#1C1C1C"),
      legend.text           = element_text(size = 11, color = "#1C1C1C"),
      plot.margin           = unit(c(b = 0.4, l = 0.4, t = 0.4, r = 0.4), "cm"),
      plot.title.position   = "plot",
      plot.title    = element_text(size = 19, color = "#1C1C1C", hjust = 0,
                                   margin = margin(b = 0, l = 0, t = 0, r = 0, unit = "cm")),
      plot.subtitle = element_text(size = 11, color = "#1C1C1C", hjust = 0,
                                   lineheight = 1.1,
                                   margin = margin(b = 0.2, l = 0, t = 0.35, r = 0, unit = "cm")),
      plot.caption  = element_text(size = 11, color = "#1C1C1C", hjust = 1,
                                   margin = margin(b = 0, l = 0, t = -0.5, r = 0, unit = "cm"))
    )
}


# ==============================================================================
# 1. Lettura dati: GeoJSON aggregato 1946 -> 1991 + centroidi
# ==============================================================================

g <- st_read(file.path(PROJ_DIR, "output", "referendum_1946_su_confini_1991.geojson"),
             quiet = TRUE) %>%
  filter(!st_is_empty(geometry))

g <- g %>%
  mutate(NUMVOTISI = as.numeric(NUMVOTISI),
         NUMVOTINO = as.numeric(NUMVOTINO),
         diff_si_no = NUMVOTISI - NUMVOTINO,
         diff_abs   = abs(diff_si_no),
         vincitore  = factor(ifelse(diff_si_no > 0, "SI", "NO"),
                             levels = c("SI", "NO")))

# Centroide del poligono aggregato (in WGS84) -> UTM 32N
g_centro <- st_centroid(g) %>% st_transform(32632)
coords <- st_coordinates(g_centro)

df_circles <- g_centro %>%
  st_drop_geometry() %>%
  mutate(x = coords[, 1], y = coords[, 2]) %>%
  # Filtro quasi-pareggi: stessa soglia dello script 2026 (>=100 voti
  # di vantaggio). Toglie circa il 14% dei comuni piu' piccoli/in pareggio.
  filter(diff_abs >= 100)

cat("Comuni totali nel referendum:", nrow(g), "\n")
cat("Dopo filtro diff_abs >= 100:", nrow(df_circles), "\n")
cat("Sì (Repubblica):", sum(df_circles$vincitore == "SI"), "\n")
cat("No (Monarchia):", sum(df_circles$vincitore == "NO"), "\n")


# ==============================================================================
# 2. Raggio in metri e repulsione (identico al 2026)
# ==============================================================================

DIAM_MIN_MM <- 0.45
DIAM_MAX_MM <- 9.0

italy_height_m  <- diff(range(df_circles$y))
panel_height_mm <- 8 * 25.4 * 0.87
mm_per_m        <- panel_height_mm / italy_height_m
m_per_mm        <- 1 / mm_per_m

diff_max     <- max(df_circles$diff_abs)
radius_max_m <- (DIAM_MAX_MM / 2) * m_per_mm

# Scala visiva LINEARE in diff_abs
radius_visual_m <- (DIAM_MIN_MM + (DIAM_MAX_MM - DIAM_MIN_MM) *
                    df_circles$diff_abs / diff_max) / 2 * m_per_mm

# Il raggio repulsivo COINCIDE col raggio visivo: qualsiasi sovrapposizione
# visiva viene effettivamente respinta dalla simulazione. Niente "alone"
# attorno ai cerchi grandi (safety_g > 1) e niente sotto-raggio sui piccoli
# (safety_g < 1) che permetterebbe sovrapposizioni visive.
df_circles$radius <- radius_visual_m

# Pesi: i grandi devono essere praticamente fissi. Una città come Milano e'
# in collisione con centinaia di vicini in un dato cluster denso; anche con
# weight ~ raggio (0..1) la somma cumulativa delle spinte la sposta di km.
# Userò weight molto piu' alto per i grandi (scala 1..1e5) -- cosi anche con
# 200 vicini la spinta cumulativa per iterazione resta sotto i 10 m.
df_circles$weight <- 1 + 99999 * (df_circles$diff_abs / diff_max)

cat("\nCalibrazione: cerchio visivo min =", round(min(radius_visual_m)), "m,",
    "max =", round(max(radius_visual_m)), "m\n")
cat("Pesi: min =", round(min(df_circles$weight), 4),
    "(piccoli, liberi), max =", round(max(df_circles$weight), 2),
    "(grandi, ancore)\n")

# Cache del layout: la repulsione su 6k+ cerchi richiede ~70 minuti, quindi
# la salvo. La cache viene invalidata se cambiano numero di cerchi, raggi
# o pesi (dipendono dai parametri DIAM_*).
cache_file <- file.path(PROJ_DIR, "output", "repel_layout_cache.rds")
cache_key <- list(
  n      = nrow(df_circles),
  radius = sum(df_circles$radius),
  weight = sum(df_circles$weight),
  x_md5  = digest::digest(df_circles$x),
  y_md5  = digest::digest(df_circles$y)
)

use_cache <- FALSE
if (file.exists(cache_file)) {
  cached <- readRDS(cache_file)
  if (identical(cached$key, cache_key)) {
    cat("\nLayout in cache valido: salto la repulsione.\n")
    df_circles$x_new <- cached$x_new
    df_circles$y_new <- cached$y_new
    use_cache <- TRUE
  } else {
    cat("\nCache esiste ma chiave diversa: rifaccio la repulsione.\n")
  }
}

if (!use_cache) {
  cat("\nRepulsione su", nrow(df_circles), "cerchi (puo' richiedere ~1 ora)...\n")
  t0 <- Sys.time()
  layout <- circleRepelLayout(
    df_circles[, c("x", "y", "radius")],
    xysizecols = c("x", "y", "radius"),
    sizetype   = "radius",
    maxiter    = 1500,
    weights    = df_circles$weight
  )
  cat("Iterazioni completate in",
      round(as.numeric(Sys.time() - t0, units = "secs"), 1), "sec\n")
  cat("Cerchi spostati con successo:",
      sum(layout$layout$x != df_circles$x | layout$layout$y != df_circles$y),
      "/", nrow(df_circles), "\n")
  df_circles$x_new <- layout$layout$x
  df_circles$y_new <- layout$layout$y

  # Ancoraggio finale: i 50 cerchi piu' grandi vengono RIPORTATI alla loro
  # posizione geografica originale. Anche con pesi alti, l'effetto cumulativo
  # di centinaia di collisioni puo' spostarli di km e renderli irriconoscibili.
  # Forzare la posizione originale e' didatticamente piu' corretto: chi guarda
  # la mappa si aspetta di trovare Milano, Roma, Napoli al posto giusto.
  N_ANCHOR <- 50
  idx_anchor <- order(df_circles$diff_abs, decreasing = TRUE)[1:N_ANCHOR]
  delta_anchor <- sqrt(
    (df_circles$x_new[idx_anchor] - df_circles$x[idx_anchor])^2 +
    (df_circles$y_new[idx_anchor] - df_circles$y[idx_anchor])^2
  )
  cat("Top", N_ANCHOR, "cerchi: spostamento medio prima dell'anchor =",
      round(mean(delta_anchor)), "m, max =", round(max(delta_anchor)), "m\n")
  df_circles$x_new[idx_anchor] <- df_circles$x[idx_anchor]
  df_circles$y_new[idx_anchor] <- df_circles$y[idx_anchor]
  cat("Anchor applicato ai top", N_ANCHOR, "cerchi.\n")

  # ---- Seconda fase di repulsione ----------------------------------------
  # Dopo l'anchor, i piccoli che si erano spostati seguendo i grandi (poi
  # ricondotti in posizione) possono ritrovarsi SOPRA i grandi o sovrapposti
  # tra loro. Eseguo una passata aggiuntiva con i top fissati come muri
  # (weight enorme = praticamente immobili) per separare i piccoli.
  cat("\nFase 2: repulsione con top", N_ANCHOR, "fissati...\n")
  weight2 <- df_circles$weight
  weight2[idx_anchor] <- 1e10
  t0 <- Sys.time()
  layout2 <- circleRepelLayout(
    data.frame(x = df_circles$x_new, y = df_circles$y_new, radius = df_circles$radius),
    xysizecols = c("x", "y", "radius"),
    sizetype   = "radius",
    maxiter    = 800,
    weights    = weight2
  )
  cat("Fase 2 completata in",
      round(as.numeric(Sys.time() - t0, units = "secs"), 1), "sec\n")
  df_circles$x_new <- layout2$layout$x
  df_circles$y_new <- layout2$layout$y
  # Riassicuro l'anchor (anche con weight 1e10 ci puo' essere uno scostamento
  # di pochi metri cumulativi)
  df_circles$x_new[idx_anchor] <- df_circles$x[idx_anchor]
  df_circles$y_new[idx_anchor] <- df_circles$y[idx_anchor]

  saveRDS(list(key = cache_key, x_new = df_circles$x_new, y_new = df_circles$y_new),
          cache_file)
  cat("Cache salvata in:", cache_file, "\n")
}


# ==============================================================================
# 3. Sfondo: regioni Istat 1991 in UTM 32N
# ==============================================================================

regioni <- st_read(
  file.path(PROJ_DIR, "Limiti1991_g", "Reg1991_g", "Reg1991_g_WGS84.shp"),
  quiet = TRUE
) %>%
  st_make_valid() %>%
  st_transform(32632)


# ==============================================================================
# 4. Plot
# ==============================================================================

# Sì = Repubblica = BLU. No = Monarchia = ROSSO. (Come la mappa del 2026.)
colori_vincitore <- c("SI" = "#0478EA", "NO" = "#E74C3C")

png(file.path(PROJ_DIR, "output", "mappa_referendum_1946_repel.png"),
    width = 8, height = 8, units = "in", res = 300)
ggplot() +
  geom_sf(data = regioni, fill = "#F0F0F0", color = "#BBBBBB", linewidth = 0.35) +
  geom_point(
    data  = df_circles,
    aes(x = x_new, y = y_new, size = diff_abs, fill = vincitore),
    alpha = 0.92, shape = 21, color = "white", stroke = 0.25
  ) +
  scale_fill_manual(
    values = colori_vincitore,
    labels = c("SI" = "Ha vinto la Repubblica", "NO" = "Ha vinto la Monarchia"),
    name   = NULL
  ) +
  scale_size_continuous(
    name   = "Vantaggio (voti)",
    range  = c(DIAM_MIN_MM, DIAM_MAX_MM),
    breaks = c(1000, 10000, 50000, 150000, 250000),
    labels = function(x) formatC(x, format = "d", big.mark = ".")
  ) +
  guides(
    fill = guide_legend(order = 1,
                        override.aes = list(size = 4, color = "white", stroke = 0.25)),
    size = guide_legend(order = 2,
                        override.aes = list(fill = "#888888", color = "white", stroke = 0.25))
  ) +
  coord_sf(crs = 32632, expand = FALSE) +
  theme_map() +
  theme(
    legend.position        = "inside",
    legend.position.inside = c(0.87, 0.80),
    legend.box             = "vertical",
    legend.key.size        = unit(0.5, "cm")
  ) +
  labs(
    title    = "La mappa del referendum del 2 giugno 1946",
    subtitle = "Vantaggio in voti assoluti della Repubblica o della Monarchia a livello comunale",
    caption  = "Elaborazione di Lorenzo Ruffino | Fonte: Ministero dell'Interno, Istat (confini 1991)",
    x = NULL, y = NULL
  )
dev.off()
cat("\nSalvata: output/mappa_referendum_1946_repel.png\n")
