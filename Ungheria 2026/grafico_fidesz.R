library(ggplot2)
library(dplyr)
library(openxlsx)
library(showtext)
font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()

theme_linechart <- function(...) {
    theme_minimal() +
        theme(
            text = element_text(family = "Source Sans Pro"),
            legend.position = "top",
            axis.line = element_line(linewidth = 0.3),
            axis.text = element_text(size = 10, color = "#1C1C1C", hjust = 0.5),
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
            plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), "cm"),
            plot.title.position = "plot",
            legend.text = element_text(size = 13, color = "#1C1C1C", hjust = 0),
            plot.title = element_text(size = 18, color = "#1C1C1C", hjust = 0),
            plot.subtitle = element_text(size = 11, color = "#1C1C1C", hjust = 0, lineheight = 1.1,
                margin = margin(b = 0.2, t = 0.15, unit = "cm")),
            plot.caption = element_text(size = 11, color = "#1C1C1C", hjust = 1,
                margin = margin(t = 0.5, unit = "cm")),
            ...
        )
}

d <- read.xlsx("elezioni_ungheria_oevk_2022_2026.xlsx", sheet = "Dati completi") %>%
    filter(!is.na(h17_2022), !is.na(h17_2026), !is.na(pct_voti), !is.na(partito)) %>%
    mutate(
        fidesz_pct_2022 = ifelse(grepl("FIDESZ", partito, ignore.case = TRUE),
                                 pct_voti, 100 - pct_voti),
        ratio_h17 = h17_2026 / h17_2022
    )

if (!dir.exists("grafici")) dir.create("grafici")

png("grafici/fidesz_vs_variazione_h17.png", width = 8, height = 7, units = "in", res = 300)
ggplot(d, aes(x = fidesz_pct_2022, y = ratio_h17, size = valp)) +
    geom_point(alpha = 0.5, color = "#FF6B1A", show.legend = FALSE) +
    geom_smooth(aes(x = fidesz_pct_2022, y = ratio_h17),
                inherit.aes = FALSE,
                method = lm, linewidth = 1.5, se = FALSE,
                color = "#FF6B1A", show.legend = FALSE) +
    scale_size_continuous(guide = "none", range = c(1, 6)) +
    scale_x_continuous(
        limits = c(35, 75),
        breaks = seq(35, 75, by = 5),
        labels = function(x) paste0(x, "%"),
        expand = c(0.01, 0.01)
    ) +
    scale_y_continuous(
        breaks = seq(1.0, 1.5, by = 0.05),
        labels = function(x) sprintf("%.2f", x),
        expand = c(0.02, 0.02)
    ) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "#888888") +
    theme_linechart() +
    labs(
        x = "% Fidesz alle Politiche 2022",
        y = "Rapporto affluenza alle 17 (2026 / 2022)",
        title = "Ungheria: l'affluenza alle 17 e il voto a Orban nel 2022",
        subtitle = "Ogni punto è un collegio uninominale (OEVK). La dimensione è proporzionale agli elettori.",
        caption = "Elaborazione di Lorenzo Ruffino | Fonte: Valasztas.hu"
    )
dev.off()
cat("Salvata: grafici/fidesz_vs_variazione_h17.png\n")

# Stampa r e R2
m <- lm(ratio_h17 ~ fidesz_pct_2022, data = d)
cat("r  =", round(cor(d$fidesz_pct_2022, d$ratio_h17), 4), "\n")
cat("R2 =", round(summary(m)$r.squared, 4), "\n")
cat("slope =", round(coef(m)[2], 4), "\n")
