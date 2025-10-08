library(tidyverse)
library(showtext)
library(data.table)
library(readxl)

comuni_comunali_2021 =
  c('ARGUSTO', 'BADOLATO', 'BORGIA', 'CENADI', 'CHIARAVALLE CENTRALE', 'CICALA', 'CONFLENTI', 'DECOLLATURA', 'FALERNA', 'FEROLETO ANTICO',
    'JACURSO', 'MARTIRANO', 'SAN SOSTENE', 'SERRASTRETTA', 'SOVERIA MANNELLI', 'SOVERIA SIMERI', 'TIRIOLO', 'TORRE DI RUGGIERO', 'VALLEFIORITA',
    'AIETA', 'ALBIDONA', 'ALTILIA', 'AMENDOLARA', 'BELMONTE CALABRO', 'BISIGNANO', 'CALOPEZZATI', 'CELICO', 'CERISANO', 'CLETO', 'COSENZA',
    'FALCONARA ALBANESE', 'FRANCAVILLA MARITTIMA', 'FUSCALDO', 'GUARDIA PIEMONTESE', 'LAGO', 'MANGONE', 'MARANO PRINCIPATO', 'PIETRAFITTA',
    'ROGLIANO', 'ROVITO', 'SAN COSMO ALBANESE', 'SAN DEMETRIO CORONE', 'SAN LORENZO BELLIZZI', 'SAN NICOLA ARCELLA', 'SAN PIETRO IN AMANTEA',
    'SANGINETO', 'SCALA COELI', 'SPEZZANO DELLA SILA', 'ZUMPANO', 'CASABONA', 'COTRONEI', 'CRUCOLI', 'PALLAGORIO', 'PETILIA POLICASTRO', 'BOVA',
    'CAMINI', 'CARERI', 'DELIANUOVA', 'FERRUZZANO', 'GALATRO', 'GERACE', 'MAMMOLA', "MELICUCCA'", 'MELICUCCO', 'MELITO DI PORTO SALVO', 'PALIZZI',
    'POLISTENA', 'ROGHUDI', 'SAN LORENZO', "SANT'AGATA DEL BIANCO", 'SEMINARA', 'SIDERNO', 'STILO', 'BROGNATURO', "DASA'",
    'FILADELFIA', 'FRANCICA', 'JOPPOLO', 'POLIA', "SANT'ONOFRIO", 'ZAMBRONE')

setwd("~/Documents/data-viz/Elezioni Italia/2025_10_05_Calabria_Regionali")


filtra_ora <- function() {
  h <- as.numeric(format(Sys.time(), "%H"))
  giorno <- format(Sys.time(), "%A")
  
  if (h >= 23)  return(3)
  if (h >= 19) return(2)
  if (h >= 15 && giorno != "domenica") return(4)
  if (h >= 12) return(1)
  1
}

orario = function() {
  h <- as.numeric(format(Sys.time(), "%H"))
  giorno <- format(Sys.time(), "%A")
  
  if (h >= 23)  return("23.00")
  if (h >= 19) return("19.00")
  if (h >= 15 && giorno != "domenica") return("15.00")
  if (h >= 12) return("12.00")
  "12.00"
}

ora_com = orario()

com_ora =  filtra_ora


df_affluenza_2025_tot = fread("Calabria_2025_Affluenza.csv")%>%
         mutate(affluenza_2025 = perc)

df_affluenza_2025 = df_affluenza_2025_tot %>%
  filter(com == filtra_ora())

# Dati del 2021

affluenza_2021 <- read_excel("2021/Calabria_2021_Affluenze.xlsx")%>%
  pivot_longer(
    cols = starts_with("Ore_"),
    names_to = "Ora",
    values_to = "Affluenza"
  ) %>%
  mutate(
    com = case_when(
      Ora == "Ore_12" ~ 1,
      Ora == "Ore_15" ~ 4,
      Ora == "Ore_19" ~ 2,
      Ora == "Ore_23" ~ 3
    )
  )%>%
  rename(affluenza_2021 = Affluenza)



risultati_2021 <- read_excel("2021/Calabria_2021_Risultati.xlsx")%>%
  select(PRO_COM, COD_PROV, CSX, CDX)%>%
  mutate(CDX = CDX * 100,
         CSX = CSX * 100)%>%
  pivot_longer(
    cols = c(CSX, CDX),
    names_to = "coalizione",
    values_to = "perc_2021"
  )



df_final = left_join(df_affluenza_2025, affluenza_2021, by = c('Comune', 'com'))%>%
  left_join(risultati_2021, by = 'PRO_COM', relationship = "many-to-many")%>%
  mutate(variazione_perc = (affluenza_2025 - affluenza_2021))%>% 
  filter(coalizione %in% c("CDX", "CSX"))%>%
  filter(!is.na(affluenza_2025)
         & affluenza_2025 != 0)%>%
  mutate(coalizione = ifelse(coalizione == 'CSX', 'CSX/M5S', coalizione))


df_final_no_comunali = left_join(df_affluenza_2025, affluenza_2021, by = c('Comune', 'com'))%>%
  left_join(risultati_2021, by = 'PRO_COM', relationship = "many-to-many")%>%
  mutate(variazione_perc = (affluenza_2025 - affluenza_2021))%>% 
  filter(coalizione %in% c("CDX", "CSX"))%>%
  filter(!is.na(affluenza_2025)
         & affluenza_2025 != 0)%>%
  mutate(coalizione = ifelse(coalizione == 'CSX', 'CSX/M5S', coalizione))%>%
  filter(!Comune %in% comuni_comunali_2021)




font_add_google("Source Sans Pro")
showtext_auto()


theme_linechart <- function(...) {
  theme_minimal() +
    theme(
      text=element_text(family="Source Sans Pro"),
      legend.position="top",
      axis.line = element_line(linewidth = 0.3),
      axis.text = element_text(size = 10,  color = "#1C1C1C", hjust = 0.5,  margin = margin(b = 0, t = 0, l = 0, unit = "cm")),
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
      legend.text =    element_text(size = 13,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.title =     element_text(size = 18,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.subtitle = element_text(size = 11, color = "#1C1C1C", hjust = 0, lineheight = 1.1, margin = margin(b = 0.2, l = 0, t = 0.15, r = 0, unit = "cm")),
      plot.caption =   element_text(size = 11,  color = "#1C1C1C", hjust = 1, margin = margin(b = 0, l = 0, t = .5, r=0, unit = "cm")),
      ...
    )
}




png(paste0("Calabria_",ora_com,".png"), width = 8, height = 7, units="in", res=300)
ggplot(df_final, aes(x = perc_2021, y = variazione_perc , 
                     size = ele_t, color = coalizione)) + 
  geom_point(alpha = 0.7, show.legend = TRUE) +
  geom_smooth(method = lm, size = 1.5, se = FALSE, show.legend = FALSE) +
  scale_color_manual(values = c("CDX" = "#0478EA", "CSX/M5S" = "#E0301E")) +
  scale_size_continuous(guide = "none") +   
  scale_x_continuous(expand = c(0.01, 0.01), limits = c(0, 100), 
                     breaks = seq(0, 100, by = 10), labels = function(x) paste0(x, "%")) +
  scale_y_continuous(expand = c(0.01, 0.01), 
                    # limits = c(-20, 9), 
                     breaks = seq(-30, 30, by = 2),
                     labels = function(x) paste0(x, "pp")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_linechart() +
  labs(
    x = '% di voti a CDX e CSX/M5S/SX per comune nel 2021',
    y = 'Variazione affluenza tra 2025 e 2021 in punti percentuali',
    title = paste0("L'affluenza in Calabria alle ", ora_com), 
    subtitle = "Relazione per comune tra la variazione dell'affluenza alla stessa ora tra 2025 e 2021 e i voti per coalizione del 2021",
    caption = "Elaborazione di Lorenzo Ruffino | Fonte dati: Eligendo, Ministero dell'Interno",
  )+
  guides(color = guide_legend(override.aes = list(size = 4))) 
dev.off()



modello_csx <- lm(variazione_perc ~ perc_2021, 
                  data = subset(df_final, coalizione == "CSX/M5S"), 
                  weights = ele_t)

summary(modello_csx)

modello_cdx <- lm(variazione_perc ~ perc_2021, 
                  data = subset(df_final, coalizione == "CDX"), 
                  weights = ele_t)

summary(modello_cdx)



png(paste0("Calabria_",ora_com,"_No_Comunali.png"), width = 8, height = 7, units="in", res=300)
ggplot(df_final_no_comunali, aes(x = perc_2021, y = variazione_perc , 
                     size = ele_t, color = coalizione)) + 
  geom_point(alpha = 0.7, show.legend = TRUE) +
  geom_smooth(method = lm, size = 1.5, se = FALSE, show.legend = FALSE) +
  scale_color_manual(values = c("CDX" = "#0478EA", "CSX/M5S" = "#E0301E")) +
  scale_size_continuous(guide = "none") +   
  scale_x_continuous(expand = c(0.01, 0.01), limits = c(0, 100), 
                     breaks = seq(0, 100, by = 10), labels = function(x) paste0(x, "%")) +
  scale_y_continuous(expand = c(0.01, 0.01), 
                     # limits = c(-20, 9), 
                     breaks = seq(-30, 30, by = 2),
                     labels = function(x) paste0(x, "pp")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_linechart() +
  labs(
    x = '% di voti a CDX e CSX/M5S/SX per comune nel 2021',
    y = 'Variazione affluenza tra 2025 e 2021 in punti percentuali',
    title = paste0("L'affluenza in Calabria alle ", ora_com), 
    subtitle = "Relazione per comune tra la variazione dell'affluenza alla stessa ora tra 2025 e 2021 e i voti per coalizione del 2021\nSono esclusi gli 81 comuni per cui si tennero le elezioni comunali nel 2021 in contemporanea alle regionali",
    caption = "Elaborazione di Lorenzo Ruffino | Fonte dati: Eligendo, Ministero dell'Interno",
  )+
  guides(color = guide_legend(override.aes = list(size = 4))) 
dev.off()



modello_csx <- lm(variazione_perc ~ perc_2021, 
                  data = subset(df_final_no_comunali, coalizione == "CSX/M5S"), 
                  weights = ele_t)

summary(modello_csx)

modello_cdx <- lm(variazione_perc ~ perc_2021, 
                  data = subset(df_final_no_comunali, coalizione == "CDX"), 
                  weights = ele_t)

summary(modello_cdx)
