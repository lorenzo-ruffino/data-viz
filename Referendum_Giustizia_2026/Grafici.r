library(ggplot2)
library(dplyr)
library(data.table)
library(showtext)
font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()

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

theme_linechart = function(...) {
    theme_minimal() +
        theme(
            text = element_text(family = "Source Sans Pro"),
            legend.position = "top",
            axis.line = element_line(linewidth = 0.3),
            axis.text = element_text(size = 10, color = "#1C1C1C", hjust = 0.5,
                margin = margin(b = 0, t = 0, l = 0, unit = "cm")),
            axis.ticks = element_blank(),
            panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.background = element_blank(),
            legend.background = element_blank(),
            legend.box.background = element_blank(),
            legend.key = element_blank(),
            panel.border = element_blank(),
            legend.title = element_blank(),
            plot.margin = unit(c(b = 0.4, l = 0.4, t = 0.4, r = 0.4), "cm"),
            plot.title.position = "plot",
            legend.text = element_text(size = 13, color = "#1C1C1C", hjust = 0,
                margin = margin(b = 0, l = 0, t = 0, r = 0, unit = "cm")),
            plot.title = element_text(size = 18, color = "#1C1C1C", hjust = 0,
                margin = margin(b = 0, l = 0, t = 0, r = 0, unit = "cm")),
            plot.subtitle = element_text(size = 11, color = "#1C1C1C", hjust = 0,
                lineheight = 1.1,
                margin = margin(b = 0.2, l = 0, t = 0.15, r = 0, unit = "cm")),
            plot.caption = element_text(size = 11, color = "#1C1C1C", hjust = 1,
                margin = margin(b = 0, l = 0, t = 0.5, r = 0, unit = "cm")),
            ...
        )
}


# CDX europee vs politiche per comune

pol_eur <- fread("affluenza/pol_eur.csv")

cdx = pol_eur %>%
    filter(coalizione == "CDX") %>%
    select(comune_norm, regione,
           pol_perc, eur_perc,
           pol_elettori, eur_elettori) %>%
    filter(!is.na(pol_perc), !is.na(eur_perc))

png("grafici/cdx_eur_vs_pol.png", width = 8, height = 7, units = "in", res = 300)
ggplot(cdx, aes(x = pol_perc * 100, y = eur_perc * 100, size = pol_elettori)) +
    geom_point(alpha = 0.4, color = "#0478EA", show.legend = FALSE) +
    geom_smooth(aes(x = pol_perc * 100, y = eur_perc * 100),
                inherit.aes = FALSE,
                method = lm, linewidth = 1.5, se = FALSE,
                color = "#0478EA", show.legend = FALSE) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#888888") +
    scale_size_continuous(guide = "none", range = c(0.5, 6)) +
    scale_x_continuous(
        expand = c(0.01, 0.01),
        limits = c(0, 100),
        breaks = seq(0, 100, by = 10),
        labels = function(x) paste0(x, "%")
    ) +
    scale_y_continuous(
        expand = c(0.01, 0.01),
        limits = c(0, 100),
        breaks = seq(0, 100, by = 10),
        labels = function(x) paste0(x, "%")
    ) +
    theme_linechart() +
    labs(
        x = "% CDX alle Politiche 2022",
        y = "% CDX alle Europee 2024",
        title = "CDX: Europee 2024 vs Politiche 2022",
        subtitle = "Ogni punto è un comune. La diagonale tratteggiata indica parità tra le due elezioni.",
        caption = "Elaborazione di Lorenzo Ruffino | Fonte: Ministero dell'Interno"
    )
dev.off()


# Affluenza europee vs politiche per comune

affl = pol_eur %>%
    distinct(comune_norm, regione, pol_affluenza, eur_affluenza, pol_elettori) %>%
    filter(!is.na(pol_affluenza), !is.na(eur_affluenza))

png("grafici/affl_eur_vs_pol.png", width = 8, height = 7, units = "in", res = 300)
ggplot(affl, aes(x = pol_affluenza * 100, y = eur_affluenza * 100, size = pol_elettori)) +
    geom_point(alpha = 0.4, color = "#555555", show.legend = FALSE) +
    #geom_smooth(method = lm, linewidth = 1.5, se = FALSE,
    #            color = "#555555", show.legend = FALSE) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#888888") +
    scale_size_continuous(guide = "none", range = c(0.5, 6)) +
    scale_x_continuous(
        expand = c(0.01, 0.01),
        limits = c(0, 100),
        breaks = seq(0, 100, by = 10),
        labels = function(x) paste0(x, "%")
    ) +
    scale_y_continuous(
        expand = c(0.01, 0.01),
        limits = c(0, 100),
        breaks = seq(0, 100, by = 10),
        labels = function(x) paste0(x, "%")
    ) +
    theme_linechart() +
    labs(
        x = "Affluenza Politiche 2022",
        y = "Affluenza Europee 2024",
        title = "Affluenza: Europee 2024 vs Politiche 2022",
        subtitle = "Ogni punto è un comune. La diagonale tratteggiata indica parità tra le due elezioni.",
        caption = "Elaborazione di Lorenzo Ruffino | Fonte: Ministero dell'Interno"
    )
dev.off()

# ==============================================================================
# Evoluzione SI/NO nel tempo (da polling ogni 5 minuti)
# ==============================================================================

library(data.table)

storico_dir <- "risultati/storico"
files <- list.files(storico_dir, pattern = "^nazionale_.*\\.csv$", full.names = TRUE)

storico <- rbindlist(lapply(files, fread)) %>%
    filter(voti_si > 0 | voti_no > 0) %>%
    mutate(ora = as.POSIXct(timestamp, format = "%Y%m%d_%H%M%S", tz = "Europe/Rome")) %>%
    arrange(ora)

ultimo_aggiornamento <- format(max(storico$ora), "%H:%M")

df_long <- storico %>%
    select(ora, SI = perc_si, NO = perc_no) %>%
    tidyr::pivot_longer(cols = c(SI, NO), names_to = "voto", values_to = "perc")

colori_voto <- c("SI" = "#0478EA", "NO" = "#E74C3C")

png("grafici/evoluzione_si_no.png", width = 7, height = 7, units = "in", res = 300)
ggplot(df_long, aes(x = ora, y = perc, color = voto)) +
    geom_hline(yintercept = 50, linetype = "dashed", color = "#888888", linewidth = 0.5) +
    geom_line(linewidth = 1.5) +
    geom_point(size = 1.5) +
    scale_color_manual(values = colori_voto) +
    scale_x_datetime(
        limits = c(
            as.POSIXct("2026-03-23 15:00:00", tz = "Europe/Rome"),
            as.POSIXct("2026-03-23 19:00:00", tz = "Europe/Rome")
        ),
        date_breaks = "1 hour",
        date_labels = "%H:%M",
        expand = c(0.01, 0.01)
    ) +
    scale_y_continuous(
        limits = c(35, 65),
        breaks = seq(0, 100, by = 5),
        labels = function(x) paste0(x, "%"),
        expand = c(0.01, 0.01)
    ) +
    theme_linechart() +
    labs(
        x        = NULL,
        y        = "% voti validi",
        title    = "Referendum Giustizia: l'evoluzione del Sì e No",
        subtitle = "Evoluzione del voto durante lo spoglio",
        caption  = paste0(
            "Ultimo aggiornamento: ", ultimo_aggiornamento,
            " | Elaborazione di Lorenzo Ruffino | Fonte: Ministero dell'Interno"
        )
    )
dev.off()


# ==============================================================================
# Mappa risultati per comune
# ==============================================================================

library(sf)
sf_use_s2(FALSE)

PROJ_DIR <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Referendum_Giustizia_2026"

# Leggo dati comuni con coordinate
df_map <- fread(file.path(PROJ_DIR, "risultati", "risultati_comuni_grafico.csv")) %>%
    filter(!is.na(lat), !is.na(lon)) %>%
    mutate(
        diff_abs  = abs(diff_si_no),
        vincitore = factor(vincitore, levels = c("SI", "NO"))
    )

# Confini regioni aggregati dal GeoJSON dei comuni
geo     <- st_read(file.path(PROJ_DIR, "utilities", "Com01012025_g_WGS84(1).json"), quiet = TRUE) %>%
    st_make_valid()
regioni <- geo %>%
    group_by(COD_REG) %>%
    summarise(geometry = st_union(geometry), .groups = "drop")

colori_vincitore <- c("SI" = "#0478EA", "NO" = "#E74C3C")

png(file.path(PROJ_DIR, "grafici", "mappa_risultati.png"), width = 8, height = 8, units = "in", res = 300)
ggplot() +
    geom_sf(data = regioni, fill = "#F0F0F0", color = "#BBBBBB", linewidth = 0.35) +
    geom_point(
        data  = df_map,
        aes(x = lon, y = lat, size = diff_abs, fill = vincitore),
        alpha = 0.7, shape = 21, color = "white", stroke = 0.25
    ) +
    scale_fill_manual(
        values = colori_vincitore,
        labels = c("SI" = "Ha vinto il Sì", "NO" = "Ha vinto il No"),
        name   = NULL
    ) +
    scale_size_continuous(
        name   = "Vantaggio (voti)",
        range = c(0.1, 12),
        breaks = c(1000, 10000, 50000, 150000, 250000),
        labels = function(x) formatC(x, format = "d", big.mark = ".")
    ) +
    guides(
        fill = guide_legend(order = 1, override.aes = list(size = 4, color = "white", stroke = 0.25)),
        size = guide_legend(order = 2, override.aes = list(fill = "#888888", color = "white", stroke = 0.25))
    ) +
    coord_sf(expand = FALSE) +
    theme_map() +
    theme(
        legend.position   = "inside",
        legend.position.inside = c(0.87, 0.80),
        legend.box        = "vertical",
        legend.key.size   = unit(0.5, "cm")
    ) +
    labs(
        title    = "La mappa dei risultati del referendum",
        subtitle = "Vantaggio in voti assoluti del Sì o del No a livello comunale",
        caption  = "Elaborazione di Lorenzo Ruffino | Fonte: Ministero dell'Interno",
        x = NULL,
        y = NULL
    )
dev.off()
cat("Salvata: grafici/mappa_risultati.png\n")


# ==============================================================================
# Coalizione vs Sì referendum 2026 per comune
# ==============================================================================

ref <- fread(file.path(PROJ_DIR, "risultati", "risultati_comuni_grafico.csv"))

coalizione_voto <- pol_eur %>%
    filter(!is.na(codice_istat)) %>%
    inner_join(ref %>% select(cod_istat, perc_si, perc_no), by = c("codice_istat" = "cod_istat")) %>%
    filter(!is.na(pol_perc))

grafico_coalizione <- function(df, coalizione, var_y, colore, label_x, label_y, titolo, nome_file) {
    dfc <- df %>% filter(coalizione == !!coalizione) %>% filter(!is.na(.data[[var_y]]))
    png(paste0("grafici/", nome_file, ".png"), width = 8, height = 7, units = "in", res = 300)
    p <- ggplot(dfc, aes(x = pol_perc * 100, y = .data[[var_y]], size = pol_elettori)) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#888888") +
        geom_point(alpha = 0.4, color = colore, show.legend = FALSE) +
        geom_smooth(aes(weight = pol_elettori),
                    method = lm, linewidth = 1.5, se = FALSE,
                    color = colore, show.legend = FALSE) +
        scale_size_continuous(guide = "none", range = c(0.5, 6)) +
        scale_x_continuous(
            expand = c(0.01, 0.01),
            limits = c(0, 100),
            breaks = seq(0, 100, by = 10),
            labels = function(x) paste0(x, "%")
        ) +
        scale_y_continuous(
            expand = c(0.01, 0.01),
            limits = c(0, 100),
            breaks = seq(0, 100, by = 10),
            labels = function(x) paste0(x, "%")
        ) +
        theme_linechart() +
        labs(
            x        = label_x,
            y        = label_y,
            title    = titolo,
            subtitle = "Relazione tra il voto alla coalizione nel 2022 e il risultato del referendum 2026 a livello comunale.",
            caption  = "Elaborazione di Lorenzo Ruffino | Fonte: Ministero dell'Interno"
        )
    print(p)
    dev.off()
    cat("Salvata: grafici/", nome_file, ".png\n", sep = "")
}

grafico_coalizione(coalizione_voto, "CDX",    "perc_si", "#0478EA", "% CDX alle Politiche 2022",    "% Sì al referendum 2026", "Il centrodestra e il voto a favore al referendum",    "cdx_vs_si_referendum")
grafico_coalizione(coalizione_voto, "CSX",    "perc_no", "#E74C3C", "% CSX alle Politiche 2022",    "% No al referendum 2026", "Il centrosinistra e il voto contrario al referendum", "csx_vs_no_referendum")
grafico_coalizione(coalizione_voto, "Centro", "perc_si", "#F39C12", "% Centro alle Politiche 2022", "% Sì al referendum 2026", "Il centro e il voto a favore al referendum",          "centro_vs_si_referendum")


# ==============================================================================
# Mappa risultati voto estero per paese
# ==============================================================================

library(sf)
sf_use_s2(FALSE)

estero <- fread(file.path(PROJ_DIR, "risultati", "risultati_estero.csv")) %>%
    filter(stato == "ok", !is.na(voti_si), !is.na(voti_no)) %>%
    mutate(
        diff_abs  = abs(voti_si - voti_no),
        vincitore = factor(ifelse(voti_si > voti_no, "SI", "NO"), levels = c("SI", "NO")),
        desc_join = toupper(desc_naz)
    )

nazioni_geo <- st_read(file.path(PROJ_DIR, "utilities", "nazioni.geojson"), quiet = TRUE) %>%
    st_make_valid() %>%
    filter(NAME != "Antarctica") %>%
    mutate(desc_join = toupper(NAME_IT))

# Centroidi per i cerchi
centroids <- nazioni_geo %>%
    st_centroid() %>%
    mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2]) %>%
    st_drop_geometry() %>%
    select(desc_join, lon, lat)

df_estero_map <- centroids %>%
    inner_join(estero %>% select(desc_join, vincitore, diff_abs), by = "desc_join")

colori_vincitore <- c("SI" = "#0478EA", "NO" = "#E74C3C")

png(file.path(PROJ_DIR, "grafici", "mappa_estero.png"), width = 10, height = 7, units = "in", res = 300)
ggplot() +
    geom_sf(data = nazioni_geo, fill = "#F0F0F0", color = "#BBBBBB", linewidth = 0.15) +
    geom_point(
        data  = df_estero_map,
        aes(x = lon, y = lat, size = diff_abs, fill = vincitore),
        alpha = 0.75, shape = 21, color = "white", stroke = 0.25
    ) +
    scale_fill_manual(
        values = colori_vincitore,
        labels = c("SI" = "Ha vinto il Sì", "NO" = "Ha vinto il No"),
        name   = NULL
    ) +
    scale_size_continuous(
        name   = "Vantaggio (voti)",
        range  = c(1, 14),
        breaks = c(5000, 20000, 60000, 115000),
        labels = function(x) formatC(x, format = "d", big.mark = "\u2009")
    ) +
    guides(
        fill = guide_legend(order = 1, override.aes = list(size = 4, color = "white", stroke = 0.25),
                            direction = "horizontal"),
        size = guide_legend(order = 2, override.aes = list(fill = "#888888", color = "white", stroke = 0.25),
                            direction = "horizontal")
    ) +
    coord_sf(ylim = c(-57, 83), expand = FALSE) +
    theme_map() +
    theme(
        aspect.ratio     = 0.55,
        legend.position  = "bottom",
        legend.box       = "vertical",
        legend.box.just  = "left",
        legend.spacing.y = unit(0.05, "cm"),
        plot.caption     = element_text(margin = margin(t = 2))
    ) +
    labs(
        title    = "La mappa del voto degli italiani all'estero",
        subtitle = "Vantaggio in voti assoluti del Sì o del No per paese di residenza",
        caption  = "Elaborazione di Lorenzo Ruffino | Fonte: Ministero dell'Interno",
        x = NULL,
        y = NULL
    )
dev.off()
cat("Salvata: grafici/mappa_estero.png\n")
