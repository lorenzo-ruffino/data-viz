library(tidyverse)
library(showtext)

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto(); showtext_opts(dpi = 220)

COL_ROSSO <- "#F12938"
COL_SCUOLA <- "#ECECEC"
COL_HL     <- "#FBE3E5"

base <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Vacanze scolastiche Europa"
df <- read_csv(file.path(base, "input/blocchi_vacanze.csv"), show_col_types = FALSE)

W0 <- as.Date("2023-09-01"); W1 <- as.Date("2024-08-31")
ndays <- as.integer(W1 - W0) + 1   # 366

# solo paesi UE (Belgio = comunità fiamminga + francese)
eu <- c("Austria","Belgio (fiamminga)","Belgio (francese)","Bulgaria","Cechia","Cipro",
        "Croazia","Danimarca","Estonia","Finlandia","Francia","Germania","Grecia","Irlanda",
        "Italia","Lettonia","Lituania","Lussemburgo","Malta","Paesi Bassi","Polonia",
        "Portogallo","Romania","Slovacchia","Slovenia","Spagna","Svezia","Ungheria")
df <- df %>% filter(paese %in% eu)

paesi <- sort(unique(df$paese))
ylev  <- rev(paesi)                # alfabetico dall'alto
yidx  <- function(p) match(p, ylev)

df  <- df %>% mutate(y = yidx(paese))
bg  <- tibble(paese = paesi, y = yidx(paese),
              fill = ifelse(paese == "Italia", COL_HL, COL_SCUOLA))

# mesi
mstart <- seq(as.Date("2023-09-01"), as.Date("2024-08-01"), by = "month")
mstart_off <- as.integer(mstart - W0)
mend_off <- c(mstart_off[-1], ndays)
mmid <- (mstart_off + mend_off) / 2
mlab <- c("set","ott","nov","dic","gen","feb","mar","apr","mag","giu","lug","ago")

lab <- tibble(paese = paesi, y = yidx(paese)) %>%
  mutate(face = ifelse(paese == "Italia", "bold", "plain"),
         col  = ifelse(paese == "Italia", COL_ROSSO, "#1C1C1C"))

h <- 0.42
p <- ggplot() +
  # cornice anno scolastico
  geom_rect(data = bg, aes(xmin = 0, xmax = ndays, ymin = y - h, ymax = y + h, fill = fill)) +
  scale_fill_identity() +
  # linee dei mesi
  geom_vline(xintercept = mstart_off, colour = "white", linewidth = 0.6) +
  # blocchi di vacanza
  geom_rect(data = df, aes(xmin = start_off, xmax = end_off + 1, ymin = y - h, ymax = y + h),
            fill = COL_ROSSO) +
  # etichette paesi a sinistra
  geom_text(data = lab, aes(x = -4, y = y, label = paese, fontface = face, colour = col),
            hjust = 1, size = 3.4, family = "Source Sans Pro") +
  scale_colour_identity() +
  scale_x_continuous(breaks = mmid, labels = mlab,
                     limits = c(-46, ndays), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.3, length(paesi) + 0.7), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Quando chiudono le scuole in Europa",
    subtitle = "In rosso i periodi di vacanza nelle scuole secondarie (ISCED 2-3), anno scolastico 2023/2024\nDove i calendari variano per regione o land, la durata delle pause è indicativa",
    caption = "Elaborazione di Lorenzo Ruffino su dati Eurydice"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Source Sans Pro"),
    axis.text.x = element_text(size = 12, color = "#1C1C1C"),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 19, color = "#1C1C1C", face = "bold",
                              margin = margin(b = 0.15, unit = "cm")),
    plot.subtitle = element_text(size = 12, color = "#1C1C1C", lineheight = 1.3,
                                 margin = margin(b = 0.3, t = 0.1, unit = "cm")),
    plot.caption = element_text(size = 11, color = "#5A5A5A", hjust = 1,
                                margin = margin(t = 0.4, unit = "cm")),
    plot.title.position = "plot",
    plot.margin = unit(c(0.4, 0.5, 0.4, 0.4), "cm")
  )

ggsave(file.path(base, "output/calendario_vacanze.png"), p,
       width = 10, height = 9, units = "in", dpi = 220, bg = "white")
cat("OK\n")
