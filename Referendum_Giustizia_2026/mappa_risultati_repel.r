library(ggplot2)
library(dplyr)
library(data.table)
library(sf)
library(packcircles)
library(showtext)
font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()

sf_use_s2(FALSE)

PROJ_DIR <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Referendum_Giustizia_2026"

# theme identico a quello di Grafici.r
theme_map = function() {
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
            legend.background      = element_blank(),
            legend.box.background  = element_blank(),
            legend.key             = element_blank(),
            legend.title           = element_text(size = 11, color = "#1C1C1C"),
            legend.text            = element_text(size = 11, color = "#1C1C1C"),
            plot.margin        = unit(c(b = 0.4, l = 0.4, t = 0.4, r = 0.4), "cm"),
            plot.title.position = "plot",
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
# 1. Lettura dati e riproiezione in UTM 32N (metri)
# ==============================================================================

df_map <- fread(file.path(PROJ_DIR, "risultati", "risultati_comuni_grafico.csv")) %>%
    filter(!is.na(lat), !is.na(lon)) %>%
    mutate(
        diff_abs  = abs(diff_si_no),
        vincitore = factor(vincitore, levels = c("SI", "NO"))
    ) %>%
    # Filtro i comuni con vantaggio < 100 voti: quasi-pareggi che aggiungono
    # solo rumore visivo e fanno densita' inutile.
    filter(diff_abs >= 100)

# Da WGS84 a UTM 32N (EPSG:32632) per lavorare in metri
df_sf <- st_as_sf(df_map, coords = c("lon", "lat"), crs = 4326) %>%
    st_transform(32632)

coords <- st_coordinates(df_sf)
df_circles <- df_sf %>%
    st_drop_geometry() %>%
    mutate(
        x = coords[, 1],
        y = coords[, 2]
    )


# ==============================================================================
# 2. Calcolo raggio in metri e repulsione
# ==============================================================================

# Scala visiva LINEARE in diff_abs (come la mappa originale).
# I cerchi piccoli risultano davvero piccoli (un comune da 1.000 voti e'
# ~80x piu' piccolo in area di uno da 250.000 voti, anche se il rapporto
# "vero" per area sarebbe 1:250). E' la scala dell'originale.
DIAM_MIN_MM <- 0.25  # diametro minimo sul plot in mm (poco piu' dell'originale)
DIAM_MAX_MM <- 12.0  # diametro massimo sul plot in mm (come originale)

# Floor minimo sul raggio repulsivo: previene sovrapposizioni tra micro
# nelle zone densissime (ma quasi mai attivo con DIAM_MIN = 0.25).
MIN_REPULSE_MM <- 0.2

# Conversione mm -> metri sul plot (UTM 32N).
italy_height_m   <- diff(range(df_circles$y))
# 0.87 stimato dai margini effettivi del plot 8x8 inch:
# title (~19pt + margin) + subtitle (~11pt) + caption (~11pt) + padding 0.4cm/lato
panel_height_mm  <- 8 * 25.4 * 0.87
mm_per_m         <- panel_height_mm / italy_height_m
m_per_mm         <- 1 / mm_per_m

diff_max         <- max(df_circles$diff_abs)
radius_max_m     <- (DIAM_MAX_MM / 2) * m_per_mm
min_repulse_m    <- (MIN_REPULSE_MM / 2) * m_per_mm

# Raggio visivo LINEARE in diff_abs
radius_visual_m <- (DIAM_MIN_MM + (DIAM_MAX_MM - DIAM_MIN_MM) *
    df_circles$diff_abs / diff_max) / 2 * m_per_mm

# Safety crescente: 0.7 sui piccoli (meno repulsione, restano vicini),
# 1.15 sui grandi (piu' alone attorno per evitare sovrapposizioni).
# Sui piccoli, 0.7 e' sufficiente perche' il visivo nominale e' ~30% piu'
# grande del raggio reale che serve per evitare sovrapposizione (lo stroke
# 0.1mm bianco si "fonde" col background grigio chiaro).
df_circles$safety_g <- 0.7 + 0.45 * sqrt(df_circles$diff_abs / diff_max)

# Raggio repulsivo: visivo * safety_g, con floor sui micro
df_circles$radius <- pmax(radius_visual_m * df_circles$safety_g, min_repulse_m)

cat("Calibrazione: cerchio visivo min =", round(min(radius_visual_m)), "m,",
    "max =", round(max(radius_visual_m)), "m\n")
cat("Floor repulsivo:", round(min_repulse_m), "m\n")
cat("Cerchi al floor:", sum(radius_visual_m * df_circles$safety_g < min_repulse_m), "/",
    nrow(df_circles), "\n")
cat("Safety: piccoli =", round(min(df_circles$safety_g), 2),
    ", grandi =", round(max(df_circles$safety_g), 2), "\n")

# Pesi: i cerchi grandi (Napoli, Roma...) restano quasi fissi, i piccoli
# liberi di spostarsi attorno. Basato sul radius VISIVO (non quello con floor)
# perche' vogliamo ancorare i cerchi VISIVAMENTE grandi.
df_circles$weight <- exp(-radius_visual_m / (radius_max_m / 6))

cat("Repulsione su", nrow(df_circles), "cerchi (puo' richiedere qualche minuto)...\n")
t0 <- Sys.time()
layout <- circleRepelLayout(
    df_circles[, c("x", "y", "radius")],
    xysizecols = c("x", "y", "radius"),
    sizetype   = "radius",
    maxiter    = 1500,
    weights    = df_circles$weight
)
cat("Iterazioni completate in", round(as.numeric(Sys.time() - t0, units = "secs"), 1), "sec\n")
cat("Cerchi spostati con successo:",
    sum(layout$layout$x != df_circles$x | layout$layout$y != df_circles$y),
    "/", nrow(df_circles), "\n")

df_circles$x_new <- layout$layout$x
df_circles$y_new <- layout$layout$y


# ==============================================================================
# 3. Confini regioni in UTM 32N
# ==============================================================================

geo <- st_read(file.path(PROJ_DIR, "utilities", "Com01012025_g_WGS84(1).json"), quiet = TRUE) %>%
    st_make_valid()
regioni <- geo %>%
    group_by(COD_REG) %>%
    summarise(geometry = st_union(geometry), .groups = "drop") %>%
    st_transform(32632)


# ==============================================================================
# 4. Plot
# ==============================================================================

colori_vincitore <- c("SI" = "#0478EA", "NO" = "#E74C3C")

png(file.path(PROJ_DIR, "grafici", "mappa_risultati_repel.png"),
    width = 8, height = 8, units = "in", res = 300)
ggplot() +
    geom_sf(data = regioni, fill = "#F0F0F0", color = "#BBBBBB", linewidth = 0.35) +
    geom_point(
        data  = df_circles,
        aes(x = x_new, y = y_new, size = diff_abs, fill = vincitore),
        alpha = 0.7, shape = 21, color = "white", stroke = 0.1
    ) +
    scale_fill_manual(
        values = colori_vincitore,
        labels = c("SI" = "Ha vinto il Sì", "NO" = "Ha vinto il No"),
        name   = NULL
    ) +
    # Scala LINEARE come la mappa originale: diametro proporzionale a diff_abs.
    scale_size_continuous(
        name   = "Vantaggio (voti)",
        range  = c(DIAM_MIN_MM, DIAM_MAX_MM),
        breaks = c(1000, 10000, 50000, 150000, 250000),
        labels = function(x) formatC(x, format = "d", big.mark = ".")
    ) +
    guides(
        fill = guide_legend(order = 1, override.aes = list(size = 4, color = "white", stroke = 0.25)),
        size = guide_legend(order = 2, override.aes = list(fill = "#888888", color = "white", stroke = 0.25))
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
        title    = "La mappa dei risultati del referendum",
        subtitle = "Vantaggio in voti assoluti del Sì o del No a livello comunale",
        caption  = "Elaborazione di Lorenzo Ruffino | Fonte: Ministero dell'Interno",
        x = NULL,
        y = NULL
    )
dev.off()
cat("Salvata: grafici/mappa_risultati_repel.png\n")
