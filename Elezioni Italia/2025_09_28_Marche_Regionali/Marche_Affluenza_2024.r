library(jsonlite)
library(purrr)
library(tidyverse)
library(rlang)
library(readr)
library(showtext)

setwd("~/Documents/data-viz/Elezioni Italia/2025_09_28_Marche_Regionali")

estrai_affluenze <- function(url, provincia_id) {
  data <- fromJSON(url, simplifyDataFrame = TRUE)
  
  # anagrafica
  enti <- as_tibble(data$enti) %>%
    mutate(provincia_id = provincia_id)
  
  # affluenze
  affluenze_df <- map_dfr(names(data$affluenze), function(id) {
    affluenze <- data$affluenze[[id]]$votantiAffluenze
    
    if (is.null(affluenze) || length(affluenze) == 0) {
      return(tibble())
    }
    
    map_dfr(names(affluenze), function(orario) {
      riga <- affluenze[[orario]]
      if (is.null(riga)) return(tibble())
      
      tibble(
        idente = as.integer(id),
        orario = as.integer(orario),
        tot_sezio    = riga$tot_sezio %||% NA,
        iscri_maschi = riga$iscri_maschi %||% NA,
        iscri_femm   = riga$iscri_femm %||% NA,
        iscri_totale = riga$iscri_totale %||% NA,
        tvmaschi     = riga$tvmaschi %||% NA,
        tvfemm       = riga$tvfemm %||% NA,
        tvotanti     = riga$tvotanti %||% NA
      )
    })
  })
  
  affluenze_df %>%
    left_join(enti, by = "idente")
}

urls <- paste0("https://dati.elezioni.marche.it/static_json/afflu_", 1:5, ".json")

df_affluenza_2024_tot <- map2_dfr(urls, 1:5, estrai_affluenze)


write.csv(df_affluenza_2024_tot, file='marche_affluenza_2025.csv', row.names = F)
  
  df_affluenza_2024 = df_affluenza_2024_tot %>%  
  mutate(affluenza_2024 = tvotanti / iscri_totale,
         iscri_totale_2024 = iscri_totale)%>%
  select(nome_breve, orario, idente, iscri_totale, affluenza_2024, iscri_totale_2024)

# Dati del 2020

marche_affluenza_2020 <- read_csv("2020/marche_affluenza_2020.csv")%>%
  mutate(affluenza = tvotanti / iscri_totale)%>%
  select(orario, idente, iscri_totale, affluenza)%>%
  rename(iscri_totale_2020 = iscri_totale,
         affluenza_2020 = affluenza)


marche_risultati_2020 <- read_csv("2020/marche_risultati_2020.csv")%>%
  mutate(coalizione = ifelse(cognome_cand == 'Acquaroli', 'CDX', ifelse(cognome_cand %in% c('Mangialardi', 'Mangialardi'), 'CSX', 'Altri')))%>%
  group_by(ente_id, coalizione)%>%
  summarise(voti = sum(voti, na.rm = T))%>%
  group_by(ente_id)%>%
  mutate(perc_2020 = voti / sum(voti))%>%
  rename(idente = ente_id)



df_final = left_join(df_affluenza_2024, marche_affluenza_2020, by = c('orario', 'idente'))%>%
  left_join(marche_risultati_2020, by = 'idente', relationship = "many-to-many")%>%
  mutate(variazione_perc = (affluenza_2024 - affluenza_2020)*100,
         perc_2020 = perc_2020*100)%>% 
  filter(coalizione %in% c("CDX", "CSX"))%>%
  filter(!is.na(affluenza_2024)
         & affluenza_2020 != 0)%>%
  filter(orario == 8)%>%
  mutate(coalizione = ifelse(coalizione == 'CSX', 'CSX/M5S', coalizione))




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
      legend.text =    element_text(size = 15,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.title =     element_text(size = 18,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.subtitle = element_text(size = 11, color = "#1C1C1C", hjust = 0, lineheight = 0.3, margin = margin(b = 0.2, l = 0, t = 0.15, r = 0, unit = "cm")),
      plot.caption =   element_text(size = 11,  color = "#1C1C1C", hjust = 1, margin = margin(b = 0, l = 0, t = .5, r=0, unit = "cm")),
      ...
    )
}


png("Marche_Affluenza_15.png", width = 8, height = 7, units="in", res=300)
ggplot(df_final, aes(x = perc_2020, y = variazione_perc , 
                     size = iscri_totale_2024, color = coalizione)) + 
  geom_point(alpha = 0.7, show.legend = TRUE) +
  geom_smooth(method = lm, size = 1.5, se = FALSE, show.legend = FALSE) +
  scale_color_manual(values = c("CDX" = "#0478EA", "CSX/M5S" = "#E0301E")) +
  scale_size_continuous(guide = "none") +   # 🔹 nasconde la legenda della dimensione
  scale_x_continuous(expand = c(0.01, 0.01), limits = c(0, 100), 
                     breaks = seq(0, 100, by = 10), labels = function(x) paste0(x, "%")) +
  scale_y_continuous(expand = c(0.01, 0.01), 
                     limits = c(-20, 9), 
                     breaks = seq(-20, 8, by = 2), labels = function(x) paste0(x, "pp")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_linechart() +
  labs(
    x = '% di voti a CDX e CSX/M5S per comune nel 2020',
    y = 'Variazione affluenza tra 2025 e 2020 in punti percentuali',
    title = "L'affluenza nelle Marche alle 15.00",
    subtitle = "Relazione per comune tra la variazione dell'affluenza alla stessa ora tra 2025 e 2020 e i voti per coalizione del 2020",
    caption = "Elaborazione di Lorenzo Ruffino | Fonte dati: Regione Marche",
  )+
  guides(color = guide_legend(override.aes = list(size = 6))) 
dev.off()



modello_csx <- lm(variazione_perc ~ perc_2020, 
                  data = subset(df_final, coalizione == "CSX/M5S"), 
                  weights = iscri_totale_2020)

summary(modello_csx)

modello_cdx <- lm(variazione_perc ~ perc_2020, 
                  data = subset(df_final, coalizione == "CDX"), 
                  weights = iscri_totale_2020)

summary(modello_cdx)