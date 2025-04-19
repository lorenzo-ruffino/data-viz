library(tidyverse)
library(data.table)
library(janitor)
library(svglite)
library(showtext)



data = fread("https://www1.finanze.gov.it/finanze/analisi_stat/public/v_4_0_0/contenuti/REG_calcolo_irpef_2024.csv?d=1615465800")%>%
  mutate(across(3:ncol(.), ~ as.numeric(gsub("\\.", "", .))))%>%
  clean_names()%>%
  mutate(categoria = case_when(
    classi_di_reddito_complessivo_in_euro=='minore di -1.000' ~ '0-10 mila',
    classi_di_reddito_complessivo_in_euro=='da -1.000 a 0' ~ '0-10 mila',
    classi_di_reddito_complessivo_in_euro=='zero' ~ '0-10 mila',
    classi_di_reddito_complessivo_in_euro=='da 0 a 1.000' ~ '0-10 mila',
    classi_di_reddito_complessivo_in_euro=='da 1.000 a 1.500' ~ '0-10 mila',
    classi_di_reddito_complessivo_in_euro=='da 1.500 a 2.000' ~ '0-10 mila',
    classi_di_reddito_complessivo_in_euro=='da 2.000 a 2.500' ~ '0-10 mila',
    classi_di_reddito_complessivo_in_euro=='da 2.500 a 3.000' ~ '0-10 mila',
    classi_di_reddito_complessivo_in_euro=='da 3.000 a 3.500' ~ '0-10 mila',
    classi_di_reddito_complessivo_in_euro=='da 3.500 a 4.000' ~ '0-10 mila',
    classi_di_reddito_complessivo_in_euro=='da 4.000 a 5.000' ~ '0-10 mila',
    classi_di_reddito_complessivo_in_euro=='da 5.000 a 6.000' ~ '0-10 mila',
    classi_di_reddito_complessivo_in_euro=='da 6.000 a 7.500' ~ '0-10 mila',
    classi_di_reddito_complessivo_in_euro=='da 7.500 a 10.000' ~ '0-10 mila',
    classi_di_reddito_complessivo_in_euro=='da 10.000 a 12.000' ~ '10-20 mila',
    classi_di_reddito_complessivo_in_euro=='da 12.000 a 15.000' ~ '10-20 mila',
    classi_di_reddito_complessivo_in_euro=='da 15.000 a 20.000' ~ '10-20 mila',
    classi_di_reddito_complessivo_in_euro=='da 20.000 a 26.000' ~ '20-30 mila',
    classi_di_reddito_complessivo_in_euro=='da 26.000 a 29.000' ~ '20-30 mila',
    classi_di_reddito_complessivo_in_euro=='da 29.000 a 35.000' ~ '30-40 mila',
    classi_di_reddito_complessivo_in_euro=='da 35.000 a 40.000' ~ '30-40 mila',
    classi_di_reddito_complessivo_in_euro=='da 40.000 a 50.000' ~ '40-50 mila',
    classi_di_reddito_complessivo_in_euro=='da 50.000 a 55.000' ~ '50-60 mila',
    classi_di_reddito_complessivo_in_euro=='da 55.000 a 60.000' ~ '50-60 mila',
    classi_di_reddito_complessivo_in_euro=='da 60.000 a 70.000' ~ '60-70 mila',
    classi_di_reddito_complessivo_in_euro=='da 70.000 a 75.000' ~ '70-80 mila',
    classi_di_reddito_complessivo_in_euro=='da 75.000 a 80.000' ~ '70-80 mila',
    classi_di_reddito_complessivo_in_euro=='da 80.000 a 90.000' ~ '80-90 mila',
    classi_di_reddito_complessivo_in_euro=='da 90.000 a 100.000' ~ '90-100 mila',
    classi_di_reddito_complessivo_in_euro=='da 100.000 a 120.000' ~ '100-110 mila',
    classi_di_reddito_complessivo_in_euro=='da 120.000 a 150.000' ~ '>120 mila',
    classi_di_reddito_complessivo_in_euro=='da 150.000 a 200.000' ~ '>120 mila',
    classi_di_reddito_complessivo_in_euro=='da 200.000 a 300.000' ~ '>120 mila',
    classi_di_reddito_complessivo_in_euro=='oltre 300.000' ~ '>120 mila'
  )) %>%
  group_by(categoria)%>%
  summarise(reddito = sum(reddito_imponibile_ammontare_in_euro, na.rm = T),
            imposta = sum(imposta_netta_ammontare_in_euro, na.rm = T),
            contribuenti = sum(reddito_imponibile_frequenza , na.rm = T))%>%
  mutate(peso_imposta = (imposta / reddito )*100,
         imposta_media = (imposta / contribuenti ),
         contribuenti = (contribuenti / sum(contribuenti ))*100,
         ordine = case_when(
           grepl("^>", categoria) ~ 999999,  # Categoria ">120 mila" va alla fine
           TRUE ~ as.numeric(gsub(" mila", "", gsub("-.*$", "", categoria)))
         ))%>%
  select(categoria, contribuenti, peso_imposta, imposta_media, ordine)%>%
  gather(tipologia, valore, contribuenti:imposta_media)%>%
  mutate(label = case_when(tipologia == "contribuenti" ~ paste0(round(valore, 1), "%"),
                           tipologia == "peso_imposta" ~ paste0(round(valore, 1), "%"),
                           tipologia == "imposta_media" ~ paste0(format(round(valore, -1), big.mark = ".", decimal.mark = ",", nsmall = 0), "€")),
         tipologia = case_when(tipologia == "contribuenti" ~ "% contribuenti",
                               tipologia == "peso_imposta" ~ "Peso imposta sul reddito",
                               tipologia == "imposta_media" ~ "Imposta media",
         ))%>%
  group_by(tipologia)%>%
  mutate(massimo = max(valore)*1.3)

data$categoria <- reorder(data$categoria, -data$ordine)
data$tipologia <- factor(data$tipologia, levels=c( "% contribuenti", "Peso imposta sul reddito", "Imposta media"))



# Tema e grafico

font_add_google("Source Sans Pro")
showtext_auto()


theme_barchart <- function(...) {
  theme_minimal() +
    theme(
      text=element_text(family="Source Sans Pro"),
      legend.position="right",
      axis.line = element_blank(),
      axis.text.y = element_text(size = 50,  color = "#1C1C1C", hjust = 0,  margin = margin(b = 0, t = 0, l = 0, unit = "cm"), lineheight = 0.35),
      axis.text.x = element_blank(),
      axis.title = element_blank(),
      strip.text = element_text(size = 45,  color = "#1C1C1C", hjust = 0.1),
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
      plot.title =     element_text(size = 90,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.subtitle =  element_text(size = 50,  color = "#1C1C1C", hjust = 0, margin = margin(b = 1, l = 0, t = 0.4, r=0, unit = "cm")),
      plot.caption =   element_text(size = 50,  color = "#1C1C1C", hjust = 1, margin = margin(b = 0, l = 0, t = 1, r=0, unit = "cm")),
      ...
    )
}


png("Peso_Imposte.png", width = 9.5, height = 10, units="in", res=300)
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
       title = "Quante tasse sul reddito di pagano",
       subtitle = "Dichiarazione dei redditi del 2024 sul 2023 per fascia di reddito imponibile", 
       caption = paste("Elaborazione di Lorenzo Ruffino | Fonte dati: Ministero dell'Economia"))
dev.off()




fread("https://www1.finanze.gov.it/finanze/analisi_stat/public/v_4_0_0/contenuti/REG_calcolo_irpef_2024.csv?d=1615465800")%>%
  mutate(across(3:ncol(.), ~ as.numeric(gsub("\\.", "", .))))%>%
  clean_names()%>%
  summarise(numero_contribuenti = sum(numero_contribuenti, na.rm = T),
            reddito_complessivo_ammontare_in_euro = sum(reddito_complessivo_ammontare_in_euro, na.rm = T),
            reddito_imponibile_frequenza = sum(reddito_imponibile_frequenza, na.rm = T),
            reddito_imponibile_ammontare_in_euro = sum(reddito_imponibile_ammontare_in_euro, na.rm = T),
            imposta_lorda_ammontare_in_euro = sum(imposta_lorda_ammontare_in_euro, na.rm = T),
            imposta_netta_ammontare_in_euro = sum(imposta_netta_ammontare_in_euro, na.rm = T),
            irpef_a_credito_ammontare_in_euro = sum(irpef_a_credito_ammontare_in_euro, na.rm = T),
            irpef_a_debito_ammontare_in_euro = sum(irpef_a_debito_ammontare_in_euro, na.rm = T))
  
