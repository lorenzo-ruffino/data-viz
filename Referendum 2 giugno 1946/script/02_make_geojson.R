#' Aggrega i confini comunali 1991 per ottenere il GeoJSON del referendum 1946.
#' Per ogni record del referendum 1946:
#'   - se ha 1 codice 1991: prendo direttamente il poligono
#'   - se ne ha 2+: faccio l'union (st_union) e li fondo in un poligono unico
#'   - se ne ha 0 (TENDA): salto, ma includo una geometry vuota e i dati nel CSV
#' Output: output/referendum_1946.geojson  (geometria + voti + provincia 1946)

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(purrr)
  library(tidyr)
})

BASE <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Referendum 2 giugno 1946"
w <- readRDS(file.path(BASE, "output", "ponte_workspace.rds"))

res <- w$res            # uno per record del referendum
com91_shp <- w$com91_shp

# Riproietto in WGS84 (lat/lon) per facilitare l'uso in web/data-viz
com91_shp <- st_transform(com91_shp, 4326)

# Per ciascuna riga referendum, unisco i poligoni dei PRO_COM_T elencati.
build_geom <- function(prot_list) {
  if (length(prot_list) == 0) return(st_sfc(st_polygon(), crs = 4326))
  g <- com91_shp %>% filter(PRO_COM_T %in% prot_list)
  if (nrow(g) == 0) return(st_sfc(st_polygon(), crs = 4326))
  if (nrow(g) == 1) return(g$geometry)
  st_union(g)
}

cat("Genero geometrie per", nrow(res), "comuni 1946...\n")
ti <- Sys.time()
geoms <- map(res$PRO_COM_1991_LIST, build_geom)
cat("Fatto in", round(as.numeric(Sys.time() - ti, units = "secs"), 1), "s\n")

# Costruisco l'sf
ref_sf <- res %>%
  mutate(
    PRO_COM_1991_STR = map_chr(PRO_COM_1991_LIST, ~ paste(.x, collapse = ";")),
    N_COMUNI_1991 = map_int(PRO_COM_1991_LIST, length)
  ) %>%
  select(CIRCOSCRIZIONE, PROVINCIA, COMUNE, NUM_REFERENDUM, QUESITO,
         ELETTORI, VOTANTI, NUMVOTISI, NUMVOTINO, SCHEDE_BIANCHE,
         METODO, PRO_COM_1991_STR, N_COMUNI_1991, NOTE_MATCH) %>%
  mutate(
    PERC_SI = ifelse(VOTANTI > 0,
                     as.numeric(NUMVOTISI) / (as.numeric(NUMVOTISI) + as.numeric(NUMVOTINO)) * 100,
                     NA_real_)
  )

ref_sf <- st_sf(ref_sf, geometry = do.call(c, geoms))

# Sanity check
cat("\nGeometrie vuote:", sum(st_is_empty(ref_sf)), "\n")
cat("Bounding box totale:\n"); print(st_bbox(ref_sf))

# Salvo GeoJSON
out_geo <- file.path(BASE, "output", "referendum_1946_su_confini_1991.geojson")
if (file.exists(out_geo)) file.remove(out_geo)
st_write(ref_sf, out_geo, delete_dsn = TRUE, quiet = TRUE)
cat("Scritto:", out_geo, "\n")
cat("Dimensione file:", round(file.info(out_geo)$size / 1024 / 1024, 1), "MB\n")

# Versione semplificata per debug rapido (geometrie più leggere)
# s2 ha problemi con geometrie multipoligono non orientate: disabilito
# durante la semplificazione e riproietto in WGS84/UTM32N.
sf_use_s2(FALSE)
ref_sf_utm <- st_transform(ref_sf, 32632)
ref_sf_simp <- st_simplify(ref_sf_utm, dTolerance = 200, preserveTopology = TRUE) %>%
  st_transform(4326)
sf_use_s2(TRUE)
out_geo_simp <- file.path(BASE, "output", "referendum_1946_su_confini_1991_simplified.geojson")
if (file.exists(out_geo_simp)) file.remove(out_geo_simp)
st_write(ref_sf_simp, out_geo_simp, delete_dsn = TRUE, quiet = TRUE)
cat("Scritto (semplificato):", out_geo_simp, "\n")
cat("Dimensione file:", round(file.info(out_geo_simp)$size / 1024 / 1024, 1), "MB\n")
