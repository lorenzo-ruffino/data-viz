library(tidyverse)
library(data.table)
library(janitor)
library(sf)
library(scales)
library(showtext)

# Reddito medio per provincia

link = "https://www1.finanze.gov.it/finanze/analisi_stat/public/v_4_0_0/contenuti/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2022.zip?d=1615465800"

download(link, dest="mef.zip", mode="wb") 
unzip ("mef.zip", exdir = "Input")

data = as.data.frame(fread("Input/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2022.csv"))%>%
  mutate(across(8:ncol(.), ~ as.numeric(gsub("\\.", "", .))))%>%
  clean_names() %>%
  mutate(sigla_provincia = ifelse(is.na(sigla_provincia), "NA", sigla_provincia))%>%
  group_by(sigla_provincia)%>%
  summarise(reddito = sum(reddito_imponibile_ammontare_in_euro, na.rm = T),
            contribuenti = sum(reddito_imponibile_frequenza , na.rm = T))%>%
  ungroup()%>%
  mutate(media = reddito / contribuenti,
         media_italia = sum(reddito)/sum(contribuenti))


province = read_sf("province.json")
regioni = read_sf("regioni.json")


data = left_join(province, data, by=c("SIGLA" = "sigla_provincia"))


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
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
      legend.key.height = unit(1.9, 'cm'), 
      legend.key.width = unit(0.8, 'cm'),
      plot.title.position = "plot",
      legend.text =    element_text(size = 40,  color = "#1C1C1C", hjust = 0,  margin = margin(b = 0, t = 0, l = 0, unit = "cm")),
      plot.caption =   element_text(size = 50,  color = "#1C1C1C", hjust = 1, margin = margin(b = 0.5, t = -1, l = 0, r=0, unit = "cm")),
      plot.title =     element_text(size = 90,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, t = 0.25, l = 0, r = 0, unit = "cm")),
      plot.subtitle =  element_text(size = 60,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, t = 0.25, l = 0, unit = "cm")),
      ...
    )
}


min = min(data$media)
max = max(data$media)


png("Reddito_Imponibile_Provincia.png", width = 9.5, height = 10, units="in", res=300)
ggplot()  +
  theme_map()+
  geom_sf(data = data, mapping=aes(geometry = geometry, fill=media), color = "#a6a6a6",  lwd = 0.4 )+
  geom_sf(data = regioni, mapping=aes(geometry = geometry), color = "#1C1C1C",  lwd = 10, fill="white", alpha=0)+
  scale_fill_stepsn(
    breaks= seq(16000, 32000, by=2000),
    values=rescale(c(16000, 22800, 32000)),
    limits = c(16000, 32000),
    colours = c("#F12938", "white", "#0478EA"),
    space = "Lab",
    na.value = "grey50",
    guide = "coloursteps",
    aesthetics = "fill",
    oob = scales::squish,
    labels = function(x) paste0(round(x/1000, 0), " mila"),
    guide_colourbar( label.position = "left", direction = "vertical", barheight = 100, barwidth = 40, frame.colour = "black", ticks.colour = "black"))+
  annotate(geom = "richtext", y = 39.49931, x = 12.67170, label = "In <span style='color:#0478EA'>blu</span> le aree con un<br>dato maggiore della <b>media italiana</b>,<br>in <span style='color:#F12938'>rosso</span> quelle con un dato minore<br>e in bianco quelle simili alla media.",
           size = 14, fill = NA, label.color = NA, lineheight = 0.5)+
  labs(x = NULL, 
       y = NULL, 
       title = "Il reddito medio nel 2022", 
       subtitle = "Reddito medio imponibile dichiarato nel 2023 per il 2022", 
       caption =  "Elaborazione di Lorenzo Ruffino | Fonte dati: Ministero dell'Economia")
dev.off()

# Peso delle imposte

data = as.data.frame(fread("https://www1.finanze.gov.it/finanze/analisi_stat/public/v_4_0_0/contenuti/REG_calcolo_irpef_2023.csv?d=1615465800"))%>%
  mutate(across(3:ncol(.), ~ as.numeric(gsub("\\.", "", .))))%>%
  clean_names()%>%
  mutate(categoria = case_when(classi_di_reddito_complessivo_in_euro=='minore di -1.000' ~ '0-15.000',
                               classi_di_reddito_complessivo_in_euro=='da -1.000 a 0' ~ '0-15.000',
                               classi_di_reddito_complessivo_in_euro=='zero' ~ '0-15.000',
                               classi_di_reddito_complessivo_in_euro=='da 0 a 1.000' ~ '0-15.000',
                               classi_di_reddito_complessivo_in_euro=='da 1.000 a 1.500' ~ '0-15.000',
                               classi_di_reddito_complessivo_in_euro=='da 1.500 a 2.000' ~ '0-15.000',
                               classi_di_reddito_complessivo_in_euro=='da 2.000 a 2.500' ~ '0-15.000',
                               classi_di_reddito_complessivo_in_euro=='da 2.500 a 3.000' ~ '0-15.000',
                               classi_di_reddito_complessivo_in_euro=='da 3.000 a 3.500' ~ '0-15.000',
                               classi_di_reddito_complessivo_in_euro=='da 3.500 a 4.000' ~ '0-15.000',
                               classi_di_reddito_complessivo_in_euro=='da 4.000 a 5.000' ~ '0-15.000',
                               classi_di_reddito_complessivo_in_euro=='da 5.000 a 6.000' ~ '0-15.000',
                               classi_di_reddito_complessivo_in_euro=='da 6.000 a 7.500' ~ '0-15.000',
                               classi_di_reddito_complessivo_in_euro=='da 7.500 a 10.000' ~ '0-15.000',
                               classi_di_reddito_complessivo_in_euro=='da 10.000 a 12.000' ~ '0-15.000',
                               classi_di_reddito_complessivo_in_euro=='da 12.000 a 15.000' ~ '0-15.000',
                               classi_di_reddito_complessivo_in_euro=='da 15.000 a 20.000' ~ '15-26.000',
                               classi_di_reddito_complessivo_in_euro=='da 20.000 a 26.000' ~ '15-26.000',
                               classi_di_reddito_complessivo_in_euro=='da 26.000 a 29.000' ~ '26-35.000',
                               classi_di_reddito_complessivo_in_euro=='da 29.000 a 35.000' ~ '26-35.000',
                               classi_di_reddito_complessivo_in_euro=='da 35.000 a 40.000' ~ '35-55.000',
                               classi_di_reddito_complessivo_in_euro=='da 40.000 a 50.000' ~ '35-55.000',
                               classi_di_reddito_complessivo_in_euro=='da 50.000 a 55.000' ~ '35-55.000',
                               classi_di_reddito_complessivo_in_euro=='da 55.000 a 60.000' ~ '55-75.000',
                               classi_di_reddito_complessivo_in_euro=='da 60.000 a 70.000' ~ '55-75.000',
                               classi_di_reddito_complessivo_in_euro=='da 70.000 a 75.000' ~ '55-75.000',
                               classi_di_reddito_complessivo_in_euro=='da 75.000 a 80.000' ~ '75-120.000',
                               classi_di_reddito_complessivo_in_euro=='da 80.000 a 90.000' ~ '75-120.000',
                               classi_di_reddito_complessivo_in_euro=='da 90.000 a 100.000' ~ '75-120.000',
                               classi_di_reddito_complessivo_in_euro=='da 100.000 a 120.000' ~ '>120.000',
                               classi_di_reddito_complessivo_in_euro=='da 120.000 a 150.000' ~ '>120.000',
                               classi_di_reddito_complessivo_in_euro=='da 150.000 a 200.000' ~ '>120.000',
                               classi_di_reddito_complessivo_in_euro=='da 200.000 a 300.000' ~ '>120.000',
                               classi_di_reddito_complessivo_in_euro=='oltre 300.000' ~ '>120.000'))%>%
  group_by(categoria)%>%
  summarise(reddito = sum(reddito_imponibile_ammontare_in_euro, na.rm = T),
            imposta = sum(imposta_netta_ammontare_in_euro, na.rm = T),
            contribuenti = sum(reddito_imponibile_frequenza , na.rm = T),
            contribuenti_tot = sum(numero_contribuenti ))%>%
  mutate(peso_imposta = (imposta / reddito )*100,
         imposta_media = (imposta / contribuenti ),
         contribuenti = (contribuenti / sum(contribuenti ))*100,
         ordine = as.numeric(sub(">", "", sub("-.+$", "", categoria))))%>%
  select(categoria, contribuenti, peso_imposta, imposta_media, ordine)%>%
  gather(tipologia, valore, contribuenti:imposta_media)%>%
  mutate(label = case_when(tipologia == "contribuenti" ~ paste0(round(valore, 1), "%"),
                           tipologia == "peso_imposta" ~ paste0(round(valore, 1), "%"),
                           tipologia == "imposta_media" ~ paste0(format(round(valore, 0), big.mark = ".", decimal.mark = ",", nsmall = 0), "€")),
        tipologia = case_when(tipologia == "contribuenti" ~ "% contribuenti",
                               tipologia == "peso_imposta" ~ "Peso imposta sul reddito",
                               tipologia == "imposta_media" ~ "Imposta media",
                               ))%>%
  group_by(tipologia)%>%
  mutate(massimo = max(valore)*1.3)

data$categoria <- reorder(data$categoria, -data$ordine)
data$tipologia <- factor(data$tipologia, levels=c( "% contribuenti", "Peso imposta sul reddito", "Imposta media"))

theme_barchart <- function(...) {
  theme_minimal() +
    theme(
      text=element_text(family="Source Sans Pro"),
      legend.position="right",
      axis.line = element_blank(),
      axis.text.y = element_text(size = 50,  color = "#1C1C1C", hjust = 0,  margin = margin(b = 0, t = 0, l = 0, unit = "cm"), lineheight = 0.35),
      axis.text.x = element_blank(),
      axis.title = element_blank(),
      strip.text = element_text(size = 50,  color = "#1C1C1C", hjust = 0.1),
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
      plot.margin = unit(c(b = 1, l = 1.5, t = 1, r=1.5), "cm"),
      plot.title.position = "plot",
      legend.text =    element_text(size = 50,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.title =     element_text(size = 90,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 1, t = 0, r=0, unit = "cm")),
      plot.subtitle =  element_text(size = 50,  color = "#1C1C1C", hjust = 0, margin = margin(b = 1, l = 1, t = 0.2, r=0, unit = "cm")),
      plot.caption =   element_text(size = 50,  color = "#1C1C1C", hjust = 1, margin = margin(b = 0, l = 0, t = 1, r=0, unit = "cm")),
      ...
    )
}


png("Imposte.png", width = 9.5, height = 10, units="in", res=300)
ggplot(data, aes(x=valore, y=categoria, group=tipologia, fill=tipologia)) +
  theme_barchart()+
  facet_wrap(~tipologia, nrow=1, scales = "free_x")+
  geom_col(show.legend = F)+
  geom_text(data, mapping=aes(x=valore, y=categoria, group=tipologia, colour=tipologia, label = label),
            size=14, hjust=-0.1,  family="Source Sans Pro", show.legend = F)+
  scale_fill_manual(values =c("Imposta media"="#F12938", "% contribuenti"="#0478EA", "Peso imposta sul reddito"="#FB7103" ))+
  scale_colour_manual(values =c("Imposta media"="#F12938", "% contribuenti"="#0478EA", "Peso imposta sul reddito"="#FB7103" ))+
  geom_blank(data, mapping=aes(x=massimo, y=categoria, group=tipologia, fill=tipologia)) + 
  labs(x = NULL,
       y = NULL,
       title = "Il peso delle imposte",
       subtitle = "Dichiarazione dei redditi del 2023 sul 2022 per fascia di reddito", 
       caption = paste("Elaborazione di Lorenzo Ruffino | Fonte dati: Ministero dell'Economia"))
dev.off()


