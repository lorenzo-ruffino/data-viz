library(eurostat)
library(tidyverse)
library(showtext)
library(ggrepel)
library(sf)
library(scales)

font_add_google("Source Sans Pro")
showtext_auto()

data_eurostat = get_eurostat("tour_occ_nim")


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
      plot.margin = unit(c(b = 0.7, l = 0.7, t = 0.7, r=0.7), "cm"),
      plot.title.position = "plot",
      legend.text =    element_text(size = 50,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.title =     element_text(size = 90,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.subtitle = element_text(size = 55, color = "#1C1C1C", hjust = 0, margin = margin(b = 0.5, l = 0, t = -0.2, r = 0, unit = "cm")),
      plot.caption =   element_text(size = 50,  color = "#1C1C1C", hjust = 1, margin = margin(b = 0, l = 0, t = .2, r=0, unit = "cm")),
      ...
    )
}



data = data_eurostat%>%
  filter(c_resid != "TOTAL"
         & geo == "IT"
         & unit == "NR"
         & nace_r2 == "I551-I553")%>%
  mutate(year = lubridate::year(TIME_PERIOD),
         month = lubridate::month(TIME_PERIOD))%>%
  filter(month %in% c(6, 7, 8)
         & year >= 2011)%>%
  group_by(c_resid, year)%>%
  summarise(turisti = ( sum(values, na.rm=T)))%>%
  mutate(categoria = case_when(c_resid == "DOM" & year == 2024 ~ "Italiani",
                               c_resid == "FOR"  & year == 2024 ~ "Stranieri"))%>%
  filter(!(c_resid == "DOM" & year == 2001))


png("Turisti_Stranieri_Italiani.png", width = 9.5, height = 10, units="in", res=300)
ggplot(data, aes(x=year, y=turisti/1e6, label = categoria, color=c_resid, group=c_resid)) +
  theme_linechart()+
  geom_line(linewidth=1.5, show.legend = F) + 
  scale_color_manual(values = c("FOR" = "#F12938",
                                  "DOM" = "#3399FF")) +
  geom_text(size=18, vjust=0.5, hjust=-0.1, , family="Source Sans Pro", show.legend = F)+
  scale_y_continuous(breaks=seq(20, 150, by=10), labels = function(x) paste0(x, " mln"), expand = c(0, 0))+
  scale_x_continuous(limits=c(2010, 2026), breaks=seq(2000, 2024, by=4), labels = function(x) paste0(x, ""), expand = c(0, 0))+
  coord_cartesian(ylim=c(40, 120), clip = "off")+
  labs(x = NULL,
       y = NULL,
       title = "Turisti italiani e stranieri in Italia",
       subtitle = "Pernottamenti tra giugno e agosto in Italia, anni 2011-2024",
       caption = "Elaborazione di Lorenzo Ruffino | Fonte dati: Eurostat")
dev.off()




theme_barchart <- function(...) {
  theme_minimal() +
    theme(
      text=element_text(family="Source Sans Pro"),
      legend.position="right",
      axis.line = element_blank(),
      axis.text.x = element_text(size = 50,  color = "#1C1C1C", hjust = 0.5,  margin = margin(b = 0, t = 0, l = 0, unit = "cm")),
      axis.text  = element_blank(),
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
      plot.margin = unit(c(b = 0.7, l = 0.7, t = 0.7, r=0.7), "cm"),
      plot.title.position = "plot",
      legend.text =    element_text(size = 50,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.title =     element_text(size = 90,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.subtitle = element_text(size = 55, color = "#1C1C1C", hjust = 0, margin = margin(b = 0.5, l = 0, t = -0.2, r = 0, unit = "cm")),
      plot.caption =   element_text(size = 50,  color = "#1C1C1C", hjust = 1, margin = margin(b = 0, l = 0, t = .2, r=0, unit = "cm")),
      ...
    )
}

data = data_eurostat%>%
  filter(c_resid == "TOTAL"
         & geo == "IT"
         & unit == "NR"
         & nace_r2 == "I551-I553")%>%
  mutate(year = lubridate::year(TIME_PERIOD),
         month = lubridate::month(TIME_PERIOD))%>%
  filter(month %in% c(6, 7, 8)
         & year >= 2000)%>%
  group_by(c_resid, year)%>%
  summarise(turisti = ( sum(values, na.rm=T)))%>%
  mutate(colore = year == 2024)


png("Turisti_Italia_Totale.png", width = 9.5, height = 10, units="in", res=300)
ggplot(data, aes(x=year, y=turisti/1e6, label = round(turisti/1e6, 0),fill=colore)) +
  theme_barchart()+
  geom_col(show.legend = F) + 
  scale_color_manual(values = c("TRUE" = "#F12938",
                                "FALSE" = "#3399FF")) +
  scale_fill_manual(values = c("TRUE" = "#F12938",
                                "FALSE" = "#3399FF")) +
  geom_text(size=15, vjust=1.6, hjust=0.5, color="white", family="Source Sans Pro", show.legend = F)+
  scale_y_continuous(breaks=seq(0, 150, by=25), labels = function(x) paste0(x, " mln"), expand = c(0, 0))+
  scale_x_continuous(limits=c(1999, 2025), breaks=seq(2000, 2024, by=4), labels = function(x) paste0(x, ""), expand = c(0, 0))+
  coord_cartesian(ylim=c(0,215), clip = "off")+
  labs(x = NULL,
       y = NULL,
       title = "I turisti estivi in Italia",
       subtitle = "Pernottamenti tra giugno e agosto in Italia, anni 2000-2024. Dati in milioni",
       caption = "Elaborazione di Lorenzo Ruffino | Fonte dati: Eurostat")
dev.off()






data = data_eurostat%>%
  filter(c_resid == "TOTAL"
         #& geo == "IT"
         & unit == "NR"
         & nace_r2 == "I551-I553")%>%
  mutate(year = lubridate::year(TIME_PERIOD),
         month = lubridate::month(TIME_PERIOD))%>%
  filter(month %in% c(6, 7, 8)
         & year >= 2000)%>%
  group_by(geo, year)%>%
  summarise(turisti = ( sum(values, na.rm=T)))%>%
  group_by(geo)%>%
  mutate(var = ((turisti / lag(turisti, 1)-1))*100)%>%
  filter(year == 2024)


geo = read_sf("/home/lorenzo/Documenti/NUTS_RG_10M_2024_3035.json")%>%
  filter(LEVL_CODE == 0)
        # &  CNTR_CODE %in% c('AT',	'BE',	'BG',	'HR',	'CY',	'CZ',	'DK',	'EE',	'FI',	'FR',	'DE',	'EL',		'HU',	'IE',	'IT',		'LV',	'LT',	'LU',	'MT',	'NL',	'PL',	'PT',	'RO',	'SK',	'SI',	'ES',	'SE'))

data = inner_join(geo,data, by = c("NUTS_ID"="geo"))

theme_map2 <- function(...) {
    theme_minimal() +
      theme(
        text=element_text(family="Source Sans Pro"),
        axis.line = element_blank(),
        axis.text  = element_blank(),
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
        plot.margin = unit(c(0.8, 0.8, 0.8, 0.8), "cm"),
        legend.key.height = unit(2.2, 'cm'), 
        legend.key.width = unit(0.7, 'cm'),
        plot.title.position = "plot",
        legend.position = c(0.9, 0.8),  # Posizione della legenda (x, y)
        legend.justification = c(0, 1),  # Punto di ancoraggio della legenda
        legend.text =    element_text(size = 50,  color = "#1C1C1C", hjust = 0.5, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
        plot.title =     element_text(size = 90,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
        plot.subtitle = element_text(size = 55, color = "#1C1C1C", hjust = 0, margin = margin(b = 0.5, l = 0, t = -0.2, r = 0, unit = "cm")),
        plot.caption =   element_text(size = 50,  color = "#1C1C1C", hjust = 1, margin = margin(b = 0, l = 0, t = .2, r=0, unit = "cm")),
        ...
      )
  }

png("Turisti_Europa.png", width = 9.5, height = 10, units="in", res=300)
ggplot()  +
  geom_sf(data, mapping=aes(geometry = geometry, fill=var),
          color = "#404040",  alpha=1,
          size = 1)+
  scale_fill_stepsn(
    breaks= c(-2, -1, 0, 2, 4, 6, 8, 10, 12),
    values=rescale(c(-2, 0, 14)),
    limits = c(-2, 14),
    colours = c("#F12938", "white", "#0478EA"),
    space = "Lab",
    na.value = "grey50",
    guide = "coloursteps",
    aesthetics = "fill",
    oob = scales::squish,
    labels = function(x) ifelse(x==12, ">12", x),
    guide_colourbar( label.position = "left", direction = "vertical", barheight = 100, barwidth = 40, frame.colour = "black", ticks.colour = "black"))+
  coord_sf(xlim = c(-944017, 3077441), ylim = c(3636430,  7437934),
           expand = FALSE,
           crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") +
  theme_map2()+
  labs(x = NULL,
       y = NULL,
       title = "La crescita dei turisti estivi in Europa",
       subtitle = "Variazione percentuale tra 2024 e 2023 dei pernottamenti tra giugno e agosto",
       caption = "Elaborazione di Lorenzo Ruffino | Fonte dati: Eurostat")
dev.off()

