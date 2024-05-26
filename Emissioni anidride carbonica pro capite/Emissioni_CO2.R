library(tidyverse)
library(data.table)
library(svglite)
library(showtext)
library(ggrepel)
library(slider)

data = fread("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")%>%
  filter(iso_code %in% c("ITA", "FRA", "DEU", "ESP", "GBR"))%>%
  select(country, iso_code, year, co2_per_capita)%>%
  mutate(
    country = case_when(
      country == "France" ~ "Francia",
      country == "Germany" ~ "Germania",
      country == "Italy" ~ "Italia", 
      country == "Spain" ~ "Spagna",
      country == "United Kingdom" ~ "Regno Unito",
      TRUE ~ country
    )
  )%>%
  group_by(country, iso_code)%>%
  mutate(co2_per_capita_ma = slide_dbl(co2_per_capita, mean, .before = 2, .after = 2, .complete = TRUE),
         label = ifelse(year == max(year)-2, country, NA))


font_add_google("Source Sans Pro")
showtext_auto()


theme_linechart <- function(...) {
  theme_minimal() +
    theme(
      text=element_text(family="Source Sans Pro"),
      legend.position="right",
      axis.line = element_line(linewidth = 0.5),
      axis.text = element_text(size = 50,  color = "#1C1C1C", hjust = 0.5,  margin = margin(b = 0, t = 0, l = 0, unit = "cm")),
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
      plot.margin = unit(c(b = 0.4, l = 0.4, t = 0.4, r=0.4), "cm"),
      plot.title.position = "plot",
      legend.text =    element_text(size = 50,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.title =     element_text(size = 90,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.subtitle = element_text(size = 50, color = "#1C1C1C", hjust = 0, lineheight = 0.3, margin = margin(b = 1, l = 0, t = 0.05, r = 0, unit = "cm")),
      plot.caption =   element_text(size = 50,  color = "#1C1C1C", hjust = 1, margin = margin(b = 0, l = 0, t = .5, r=0, unit = "cm")),
      ...
    )
}


png("Emissioni_CO2.png", width = 9.5, height = 10, units="in", res=300)
ggplot(data, aes(x=year, y=co2_per_capita_ma, group=iso_code, colour=iso_code)) +
  theme_linechart()+
  geom_line(linewidth = 1, show.legend = F) + 
  scale_color_manual(
    values = c("ITA" = "#F12938", "FRA" = "#0478EA", "DEU" = "#02B875", "ESP" = "#FF7F00", "GBR" = "#9B59B6")) +
  geom_text_repel(aes(label = label), size=15, 
                  direction = "y",
                  hjust = 0,
                  xlim = c(2021, NA),
                  segment.size = .7,
                  segment.alpha = .5,
                  segment.linetype = "dotted",
                  box.padding = .4,
                  segment.curvature = -0.1,
                  segment.ncp = 3,
                  segment.angle = 20,
                  fontface="bold", show.legend = FALSE, family="Source Sans Pro")+
  scale_y_continuous(limits=c(0, 15), breaks=seq(3, 15, by=3), labels = function(x) paste0(x, " t"), expand = c(0, 0))+
  scale_x_continuous(limits=c(1800, 2055), breaks=seq(1800, 2020, by=50), labels = function(x) paste0(x, ""), expand = c(0, 0))+
  labs(x = NULL,
       y = NULL,
       title = expression("Emissioni di CO"[2]*" pro capite"),
       subtitle = "Media mobile centrata a 5 anni delle emissioni pro capite di anidride carbonica da combustibili\nfossili e industria, anni 1800-2022. Dati in tonnellata per persona.",
       caption = paste("Elaborazione di Lorenzo Ruffino | Fonte dati: Our World in Data"))
dev.off()
