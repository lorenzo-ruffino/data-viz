library(tidyverse)
library(showtext)
library(scales)

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_VIOLA  <- "#A82DE3"
COL_ROSSO  <- "#F12938"
COL_GIALLO <- "#F2A900"
COL_GRIGIO <- "#C9C9C9"
COL_GRIGIO_SCURO <- "#5A5A5A"

theme_linechart <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro"),
      legend.position = "none",
      axis.line = element_line(linewidth = 0.3),
      axis.text = element_text(size = 9, color = "#1C1C1C", hjust = 0.5),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      panel.border = element_blank(),
      plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), "cm"),
      plot.title.position = "plot",
      plot.title = element_text(family = "Source Sans Pro SemiBold",
                                size = 14, color = "#1C1C1C", hjust = 0,
                                margin = margin(b = 0.1, unit = "cm")),
      plot.subtitle = element_text(size = 9, color = "#1C1C1C", hjust = 0,
                                   lineheight = 1.35,
                                   margin = margin(b = 0.25, t = 0.1, unit = "cm")),
      plot.caption = element_text(size = 9, color = "#1C1C1C", hjust = 1,
                                  margin = margin(t = 0.5, unit = "cm")),
      ...
    )
}

d <- read.csv("../stima_lavoratori_in_ferie_per_settimana.csv", check.names = FALSE)

dl <- d %>%
  pivot_longer(-week_number, names_to = "paese", values_to = "val")

top_to_bottom <- rev(colnames(d)[colnames(d) != "week_number"])

evidenziati <- c("Italia" = COL_ROSSO, "Francia" = COL_VIOLA,
                 "Germania" = COL_NERO, "Spagna" = COL_GIALLO,
                 "UE-27" = COL_BLU)

K <- 2.5 / 45

ridges <- dl %>%
  mutate(pos_alto = match(paese, top_to_bottom),
         base = 26 - pos_alto,
         ymax = base + ifelse(is.na(val), 0, val) * K,
         fill = ifelse(paese %in% names(evidenziati),
                       evidenziati[paese], COL_GRIGIO)) %>%
  mutate(paese_draw = factor(paese,
                             levels = c(setdiff(top_to_bottom, names(evidenziati)),
                                        intersect(top_to_bottom, names(evidenziati)))))

axis_cols <- map_chr(top_to_bottom, ~ ifelse(.x %in% names(evidenziati),
                                             evidenziati[[.x]], COL_GRIGIO_SCURO))

date0 <- as.Date("2024-12-30")
settimane <- date0 + weeks(0:52)
mesi <- c("gen", "feb", "mar", "apr", "mag", "giu",
          "lug", "ago", "set", "ott", "nov", "dic")
break_mesi <- map_dbl(1:12, function(m) {
  inizio <- as.Date(paste0("2025-", sprintf("%02d", m), "-01"))
  which(settimane >= inizio)[1]
})

ggplot(ridges) +
  geom_ribbon(aes(x = week_number, ymin = base, ymax = ymax,
                  group = paese_draw, fill = fill),
              alpha = 0.9) +
  geom_line(aes(x = week_number, y = ymax, group = paese_draw),
            color = COL_NERO, linewidth = 0.25) +
  scale_fill_identity() +
  scale_x_continuous(limits = c(1, 53), expand = c(0, 0),
                     breaks = break_mesi, labels = mesi) +
  scale_y_continuous(breaks = 1:25, labels = rev(top_to_bottom)) +
  theme_linechart() +
  theme(axis.text.y = element_text(size = 8.5, hjust = 1, color = rev(axis_cols)),
        axis.line.y = element_blank()) +
  labs(title = "Quando si va in vacanza in Europa",
       subtitle = "Quota stimata di lavoratori in ferie per settimana, paesi ordinati per latitudine",
       caption = "Elaborazione di Lorenzo Ruffino su dati Eurostat")

ggsave("../output/Vacanze_Europa_ridgeline.png",
       width = 8, height = 10, units = "in", dpi = 220, bg = "white")

cat("max val:", max(dl$val, na.rm = TRUE), "\n")
