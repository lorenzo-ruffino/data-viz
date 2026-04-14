library(jsonlite)
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(showtext)

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
sf_use_s2(FALSE)

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
            legend.background      = element_blank(),
            legend.box.background  = element_blank(),
            legend.key             = element_blank(),
            legend.title           = element_text(size = 14, color = "#1C1C1C"),
            legend.text            = element_text(size = 14, color = "#1C1C1C"),
            legend.spacing.y       = unit(-0.35, "cm"),
            legend.box.spacing     = unit(0.1, "cm"),
            plot.margin        = unit(c(0.4, 0.4, 0.4, 0.4), "cm"),
            plot.title.position = "plot",
            plot.title    = element_text(size = 26, color = "#1C1C1C", hjust = 0),
            plot.subtitle = element_text(size = 15, color = "#1C1C1C", hjust = 0,
                                         lineheight = 1.1,
                                         margin = margin(b = 0.2, t = 0.35, unit = "cm")),
            plot.caption  = element_text(size = 14, color = "#1C1C1C", hjust = 1,
                                         margin = margin(t = -0.5, unit = "cm"))
        )
}

# === 1) Poligoni OEVK ===
poli <- fromJSON("https://vtr.valasztas.hu/ogy2026/data/04112100/ver/OevkPoligonok.json")$list
poli$oevk_id <- paste0(poli$maz, "-", poli$evk)

parse_centrum <- function(s) {
    v <- as.numeric(strsplit(s, " ")[[1]])
    list(lat = v[1], lon = v[2])
}
cen <- do.call(rbind, lapply(poli$centrum, function(s) {
    v <- as.numeric(strsplit(s, " ")[[1]])
    data.frame(lat = v[1], lon = v[2])
}))
poli$lat <- cen$lat; poli$lon <- cen$lon

parse_poligon <- function(s) {
    pts <- strsplit(s, ",")[[1]]
    coords <- do.call(rbind, lapply(pts, function(p) {
        v <- as.numeric(strsplit(trimws(p), " ")[[1]])
        c(v[2], v[1])  # lon, lat
    }))
    if (!all(coords[1, ] == coords[nrow(coords), ])) {
        coords <- rbind(coords, coords[1, ])
    }
    st_polygon(list(coords))
}
polys <- lapply(poli$poligon, parse_poligon)
oevk_sf <- st_sf(oevk_id = poli$oevk_id, maz = poli$maz, evk = poli$evk,
                 geometry = st_sfc(polys, crs = 4326)) %>% st_make_valid()

# === 2) Candidati con partito ===
jel <- fromJSON("https://vtr.valasztas.hu/ogy2026/data/04112100/ver/EgyeniJeloltek.json")$list
jel <- jel %>% select(ej_id, maz, evk, neve, partito = jlcs_nev) %>%
    mutate(oevk_id = paste0(maz, "-", evk))

# === 3) Risultati per candidato ===
jkv <- fromJSON("https://vtr.valasztas.hu/ogy2026/data/04130800/szavossz/OevkJkv.json")$list
risultati <- bind_rows(lapply(seq_len(nrow(jkv)), function(i) {
    tet <- jkv$egyeni_jkv$tetelek[[i]]
    if (is.null(tet) || nrow(tet) == 0) return(NULL)
    tet$oevk_id <- paste0(jkv$maz[i], "-", jkv$evk[i])
    tet
})) %>% left_join(jel %>% select(ej_id, partito), by = "ej_id")

cat("Distribuzione partiti nei risultati:\n")
print(table(risultati$partito))

# === 4) Blocchi governo vs opposizione ===
# Governo: FIDESZ-KDNP
# Opposizione principale: TISZA (Péter Magyar)
# Altro: tutto il resto
risultati <- risultati %>%
    mutate(blocco = case_when(
        partito == "FIDESZ-KDNP" ~ "Governo",
        partito == "TISZA"        ~ "Opposizione",
        TRUE                      ~ "Altro"
    ))

blocchi <- risultati %>%
    group_by(oevk_id, blocco) %>%
    summarise(voti = sum(szavazat, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = blocco, values_from = voti, values_fill = 0) %>%
    mutate(
        diff = Opposizione - Governo,
        diff_abs = abs(diff),
        vincitore = ifelse(diff >= 0, "Opposizione", "Governo")
    )

centers <- poli %>% select(oevk_id, maz, lon, lat) %>%
    inner_join(blocchi, by = "oevk_id")

# "Esplodi" i collegi di Budapest (maz=01) verso l'esterno così i cerchi non si sovrappongono
bp <- centers$maz == "01"
if (any(bp)) {
    bp_lon_c <- mean(centers$lon[bp])
    bp_lat_c <- mean(centers$lat[bp])
    scale_factor <- 4.5  # quanto "esplodere" verso l'esterno
    centers$lon[bp] <- bp_lon_c + (centers$lon[bp] - bp_lon_c) * scale_factor
    centers$lat[bp] <- bp_lat_c + (centers$lat[bp] - bp_lat_c) * scale_factor
}

cat("\nRiepilogo vincitori:\n")
print(table(centers$vincitore))
cat("Range differenza voti:\n"); print(summary(centers$diff_abs))

# === 5) Confini macrozone (maz) ===
maz_sf <- oevk_sf %>% group_by(maz) %>%
    summarise(geometry = st_union(geometry), .groups = "drop")

# === 6) Mappa ===
colori <- c("Governo" = "#FF6B1A", "Opposizione" = "#0478EA")

if (!dir.exists("grafici")) dir.create("grafici")

png("grafici/mappa_oevk_ungheria.png", width = 11, height = 8, units = "in", res = 300)
ggplot() +
    geom_sf(data = maz_sf, fill = "#F0F0F0", color = "#BBBBBB", linewidth = 0.35) +
    geom_point(
        data = centers,
        aes(x = lon, y = lat, size = diff_abs, fill = vincitore),
        alpha = 0.75, shape = 21, color = "white", stroke = 0.3
    ) +
    scale_fill_manual(
        values = colori,
        labels = c("Governo" = "Ha vinto Fidesz-KDNP", "Opposizione" = "Ha vinto TISZA"),
        name   = NULL
    ) +
    scale_size_area(
        name     = "Vantaggio",
        max_size = 16,
        breaks   = c(500, 5000, 15000, 25000),
        labels   = function(x) formatC(x, format = "d", big.mark = ".")
    ) +
    guides(
        fill = guide_legend(order = 1, direction = "horizontal",
                             override.aes = list(size = 5, color = "white", stroke = 0.3)),
        size = guide_legend(order = 2, direction = "horizontal", nrow = 1,
                             override.aes = list(fill = "#888888", color = "white", stroke = 0.3))
    ) +
    coord_sf(expand = FALSE) +
    theme_map() +
    theme(
        legend.position   = "inside",
        legend.position.inside = c(0.01, 0.99),
        legend.justification = c(0, 1),
        legend.box        = "vertical",
        legend.box.just   = "left",
        legend.key.size   = unit(0.5, "cm")
    ) +
    labs(
        title    = "La mappa dei risultati delle elezioni in Ungheria",
        subtitle = "Vantaggio in voti assoluti di TISZA o Fidesz-KDNP a livello di collegio (OEVK)",
        caption  = "Elaborazione di Lorenzo Ruffino | Fonte: Valasztas.hu"
    )
dev.off()
cat("\nSalvata: grafici/mappa_oevk_ungheria.png\n")
