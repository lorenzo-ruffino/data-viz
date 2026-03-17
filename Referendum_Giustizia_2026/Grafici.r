source("Precedenti.r")
library(showtext)
font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()

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

cdx = pol_eur %>%
    filter(coalizione == "CDX") %>%
    select(comune_norm, regione,
           pol_perc, eur_perc,
           pol_elettori, eur_elettori) %>%
    filter(!is.na(pol_perc), !is.na(eur_perc))

png("cdx_eur_vs_pol.png", width = 8, height = 7, units = "in", res = 300)
ggplot(cdx, aes(x = pol_perc * 100, y = eur_perc * 100, size = pol_elettori)) +
    geom_point(alpha = 0.4, color = "#0478EA", show.legend = FALSE) +
    geom_smooth(method = lm, linewidth = 1.5, se = FALSE,
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

png("affl_eur_vs_pol.png", width = 8, height = 7, units = "in", res = 300)
ggplot(affl, aes(x = pol_affluenza * 100, y = eur_affluenza * 100, size = pol_elettori)) +
    geom_point(alpha = 0.4, color = "#555555", show.legend = FALSE) +
    geom_smooth(method = lm, linewidth = 1.5, se = FALSE,
                color = "#555555", show.legend = FALSE) +
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
