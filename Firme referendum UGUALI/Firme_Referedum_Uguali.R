library(tidyverse)
library(data.table)
library(showtext)

data = fread("https://raw.githubusercontent.com/ondata/liberiamoli-tutti/refs/heads/main/referendum_iniziative_popolare/data/referendum_iniziative_popolare_log.csv")

result = data %>%
  mutate(date = as.Date(datetime))%>%
  group_by(id, date)%>%
  filter(sostenitori == max(sostenitori)
         & id == 3600000)


latest = tibble(
  id = 1100000,
  sostenitori = 617715  ,
  datetime = Sys.time(),
  date = today()
)

result = bind_rows(result, latest)

result$diff_sostenitori <- c(NA, diff(result$sostenitori))


font_add_google("Source Sans Pro")
showtext_auto()



theme_barchart <- function(...) {
  theme_minimal() +
    theme(
      text=element_text(family="Source Sans Pro"),
      legend.position="right",
      axis.line = element_line(linewidth = 0.3),
      axis.text = element_text(size = 45,  color = "#1C1C1C", hjust = 0.5,  margin = margin(b = 0, t = 0, l = 0, unit = "cm")),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank() ,
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      plot.background = element_blank(),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.key = element_blank(),
      panel.border = element_blank(),
      legend.title=element_blank(),
      plot.margin = unit(c(b = 1, l = 1, t = 1, r=1), "cm"),
      plot.title.position = "plot",
      legend.text =    element_text(size = 50,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.title =     element_text(size = 90,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.subtitle =  element_text(size = 50,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0.5, l = 0, t = 0.2, r=0, unit = "cm")),
      plot.caption =   element_text(size = 50,  color = "#1C1C1C", hjust = 1, margin = margin(b = 0, l = 0, t = 0.5, r=0, unit = "cm")),
      ...
    )
}


png("Referendum.png", width = 9.5, height = 10, units="in", res=300)
ggplot(result, aes(x = date, y = sostenitori)) +
  geom_bar(stat = "identity", fill = "#0478EA") +
  geom_text(aes(label = paste0(round(sostenitori/1000, 0), "k")), vjust = -0.5, size=12) +
  geom_hline(yintercept = 500000, color = "#F12938", linetype = "dashed") +
  scale_x_date(date_labels = "%d %b", date_breaks = "3 days") +
  scale_y_continuous(
    limits = c(0, 600000),
    breaks = seq(100000, 620000, by = 100000),
    labels = function(x) paste0(x/1000, "k"),
    expand = expansion(mult = c(0, 0.1))
  ) +
  theme_barchart()+
  labs(x = NULL,
       y = NULL,
       title = "Le firme per il referendum UGUALI!", 
       subtitle = "Andamento della raccolta firme online per il referendum al 16 maggio 2025", 
       caption = paste("Elaborazione di Lorenzo Ruffino | Fonte dati: Referendum e iniziative popolari (via OnData)"))
dev.off()
