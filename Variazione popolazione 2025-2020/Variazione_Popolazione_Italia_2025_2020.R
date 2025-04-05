library(tidyverse)
library(data.table)
library(janitor)
library(sf)
library(scales)
library(showtext)
library(readxl)



# Sources

# POSAS_2025_it_Comuni.csv: https://demo.istat.it/data/posas/POSAS_2025_it_Comuni.zip
# POSAS_2020_it_Comuni.csv: https://demo.istat.it/data/posas/POSAS_2020_it_Comuni.zip
# Elenco comuni soppressi.csv: https://www.istat.it/wp-content/uploads/2024/09/Elenco-comuni-soppressi.zip
# Aree interne: https://www.istat.it/wp-content/uploads/2022/07/20220715_Elenco_Comuni_Classi_di_Aree_Interne.xlsx


comuni_2025 = as.data.frame(fread("Input/POSAS_2025_it_Comuni.csv"))%>%
  clean_names()%>%
  filter(eta == 999)%>%
  select(codice_comune, comune, totale)%>%
  rename(pop_2025 = totale)

comuni_2020 = as.data.frame(fread("Input/POSAS_2020_it_Comuni.csv"))%>%
  clean_names()%>%
  filter(eta == 999)%>%
  select(codice_comune, totale)%>%
  rename(pop_2020 = totale)

comuni_soppressi = as.data.frame(fread("Input/Elenco comuni soppressi.csv"))%>%
  clean_names()%>%
  select(codice_comune, codice_del_comune_associato_alla_variazione)

comuni_2020_all = left_join(comuni_2020, comuni_soppressi, by="codice_comune")%>%
  mutate(nuovo_codice = ifelse(!is.na(codice_del_comune_associato_alla_variazione), codice_del_comune_associato_alla_variazione, codice_comune))%>%
  group_by(nuovo_codice)%>%
  summarise(pop_2020 = sum(pop_2020))%>%
  rename(codice_comune = nuovo_codice)

sum(comuni_2020_all$pop_2020)  == sum(comuni_2020$pop_2020)

pop_all_years = left_join(comuni_2025, comuni_2020_all, by="codice_comune")%>%
  mutate(abs_var = pop_2025 - pop_2020,
         rel_var = ((pop_2025 / pop_2020) -1)*100)



# Aree interne

comuni_aree_interne <- read_excel(
  "Input/20220715_Elenco_Comuni_Classi_di_Aree_Interne.xlsx", 
  skip = 2,
  col_names = TRUE  
)%>%
  clean_names()%>%
  select(procom_n,
         descrizione_aree_interne_2021_2027 )

pop_all_years_classification = left_join(pop_all_years, comuni_aree_interne, by = c("codice_comune"="procom_n"))%>%
  group_by(descrizione_aree_interne_2021_2027)%>%
  summarise(pop_2025 = sum(pop_2025, na.rn=T),
            pop_2020 = sum(pop_2020, na.rn=T))%>%
mutate(abs_var = pop_2025 - pop_2020,
       rel_var = ((pop_2025 / pop_2020) -1)*100)


# Mappa

comuni_geo = read_sf("Input/Com01012025_g_WGS84.json")

pop_all_years_geo = left_join(comuni_geo, pop_all_years, by=c("PRO_COM" = "codice_comune"))

sf_use_s2(FALSE)

regioni_geo = st_make_valid(comuni_geo) %>%
  group_by(COD_REG) %>%
  summarise(
    NOME_REGIONE = first(COD_REG),
    .groups = "drop"
  )


province_geo = st_make_valid(comuni_geo) %>%
  group_by(COD_PROV) %>%
  summarise(
    NOME_REGIONE = first(COD_PROV),
    .groups = "drop"
  )

font_add_google("Source Sans Pro")
showtext_auto()

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text=element_text(family="Source Sans Pro"),
      legend.position="right",
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
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
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      legend.key.height = unit(2.2, 'cm'), 
      legend.key.width = unit(0.8, 'cm'),
      plot.title.position = "plot",
      plot.caption.position =  "plot",
      legend.text =    element_text(size = 45,  color = "#1C1C1C", hjust = 0,  margin = margin(b = 0, t = 0, l = 0.2, unit = "cm")),
      plot.caption =   element_text(size = 50,  color = "#1C1C1C", hjust = 1, margin = margin(b = 0.5, t = -1, l = 0, r=0, unit = "cm")),
      plot.title =     element_text(size = 90,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, t = 1, l = 0, r = 1, unit = "cm")),
      plot.subtitle =  element_text(size = 50,  color = "#1C1C1C", hjust = 0, margin = margin(b = -0.2, t = 0.25, l = 0, unit = "cm")),
      ...
    )
}




min = min(pop_all_years_geo$rel_var, na.rm=T)
max = max(pop_all_years_geo$rel_var, na.rm=T)

min
max


png("Variazione_Popolazione_Italia_2025_2020.png", width = 9.5, height = 10, units="in", res=300)
ggplot()  +
  theme_map()+
  geom_sf(data = pop_all_years_geo, mapping=aes(geometry = geometry, fill=rel_var), color = "#a6a6a6",  lwd = 0)+
  geom_sf(data = province_geo, mapping=aes(geometry = geometry), color = "#1C1C1C",  lwd = 0.2, fill="white", alpha=0)+
  geom_sf(data = regioni_geo, mapping=aes(geometry = geometry), color = "#1C1C1C",  lwd = 0.5, fill="white", alpha=0)+
  scale_fill_stepsn(
    breaks= seq(-10, 10, by=2),
    values=rescale(c(-10, 0, 10)),
    limits = c(-10, 10),
    colours = c("#F12938", "white", "#0478EA"),
    space = "Lab",
    na.value = "grey50",
    guide = "coloursteps",
    aesthetics = "fill",
    oob = scales::squish,
    labels = function(x) {
      ifelse(x == 10, ">10%",
             ifelse(x == -10, "<-10%",
                    paste0(" ", round(x, 1), "%")))
    },
    guide_colourbar( label.position = "left", direction = "vertical", barheight = 100, barwidth = 40, frame.colour = "black", ticks.colour = "black"))+
  labs(x = NULL, 
       y = NULL, 
       title = "Com'è cambiata la popolazione in cinque anni", 
       subtitle = "Variazione percentuale della popolazione tra il 1 gennaio 2025 e il 1 gennaio 2020", 
       caption =  "Elaborazione di Lorenzo Ruffino | Fonte dati: Istat")
dev.off()



pop_all_years%>%
  mutate(pop_down = rel_var < 0)%>%
  group_by(pop_down)%>%
  count()
  
  

theme_barchart <- function(...) {
  theme_minimal() +
    theme(
      text=element_text(family="Source Sans Pro"),
      legend.position="right",
      axis.line = element_line(linewidth = 0.5),
      axis.text = element_text(size = 40,  color = "#1C1C1C", hjust = 0.5,  margin = margin(b = 0, t = 0, l = 0, unit = "cm")),
      axis.title = element_text(size = 40,  color = "#1C1C1C", hjust = 0.5,  margin = margin(b = 0, t = 0, l = 0, unit = "cm")),
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
      legend.text =    element_text(size = 45,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.title =     element_text(size = 90,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.subtitle = element_text(size = 50, color = "#1C1C1C", hjust = 0, margin = margin(b = 0.5, l = 0, t = 0.05, r = 0, unit = "cm")),
      plot.caption =   element_text(size = 50,  color = "#1C1C1C", hjust = 1, margin = margin(b = 0, l = 0, t = .2, r=0, unit = "cm")),
      ...
    )
}


png("Comuni_Variazione_Popolazione_Italia_2025_2020.png", width = 9.5, height = 10, units="in", res=300)
ggplot(pop_all_years, aes(x = rel_var)) +
  geom_histogram(
    aes(fill = ..x..), 
    breaks = seq(-20, 20, by = 2), 
    closed = "left",
    show.legend = F
  ) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95"),
        legend.position = "none") +
  scale_fill_stepsn(
    breaks = seq(-10, 10, by = 2),
    values = rescale(c(-10, 0, 10)),
    limits = c(-10, 10),
    colours = c("#F12938", "white", "#0478EA"),
    space = "Lab",
    na.value = "grey50",
    guide = "coloursteps",
    aesthetics = "fill",
    oob = scales::squish,
    labels = function(x) {
      ifelse(x == 10, ">10%",
             ifelse(x == -10, "<-10%",
                    paste0(" ", round(x, 1), "%")))
    }
  ) +
  scale_x_continuous(
    limits = c(-20, 20), 
    expand = expansion(mult = 0.05),
    breaks = seq(-20, 20, by = 5),
    labels = function(x) paste0(x, "%")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
  theme_barchart()+
  labs(x = "Variazione % popolazione", 
       y = "Numero di comuni", 
       title = "Nel 70% dei comuni è diminuita la popolazione", 
       subtitle = "Comuni in base alla variazione di popolazione il 1 gennaio 2025 e il 1 gennaio 2020", 
       caption =  "Elaborazione di Lorenzo Ruffino | Fonte dati: Istat")
dev.off()


pop_all_years = pop_all_years %>%
  filter(!is.na(rel_var))

min_var = min(pop_all_years$rel_var)
max_var = max(pop_all_years$rel_var)

min_break = floor(min_var/2) * 2 - 2  
max_break = ceiling(max_var/2) * 2 + 2  

hist_data = hist(pop_all_years$rel_var, 
                  breaks = seq(min_break, max_break, by = 2), 
                  plot = FALSE)


histogram_df = data.frame(
  bin_start = hist_data$breaks[-length(hist_data$breaks)],
  bin_end = hist_data$breaks[-1],
  bin_center = (hist_data$breaks[-length(hist_data$breaks)] + hist_data$breaks[-1])/2,
  count = hist_data$counts
)%>%
  mutate(perc =  100*(count / sum(count)))


top_20 = pop_all_years%>%
  arrange(desc(pop_2025))%>%
  slice(1:20)