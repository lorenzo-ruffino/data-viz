library(ggplot2)
library(tidyr)
library(dplyr)
library(showtext)

df <- data.frame(
  Sondaggista = c("Harris Interactive", "Ipsos", "Ifop", "Elabe", "Odoxa", "OpinionWay", "Cluster17"),
  Data = c("28/06/2024", "28/06/2024", "28/06/2024", "27/06/2024", "27/06/2024", "27/06/2024", "27/06/2024"),
  Campione = c(2182, 10286, 2824, 1871, 1896, 1058, 2465),
  NFP = c(28, 29, 29, 27.5, 27.5, 28, 29.5),
  ENS = c(20, 20, 20.5, 20, 21, 20, 20),
  RN = c(37, 36, 36.5, 36.5, 36.5, 36.5, 36.5),
  LR = c(8, 8, 7, 9, 9, 8, 9),
  Altri = c(7, 7, 7, 7, 6, 7.5, 5)
)

medie <- colMeans(df[, 4:8])

df_medie <- data.frame(
  Partito = names(medie),
  Media = medie
)

# Mappa dei nomi completi dei partiti
nomi_partiti <- c(
  "RN" = "Rassemblement National",
  "NFP" = "Nouveau Front Populaire",
  "ENS" = "Ensemble",
  "LR" = "Les Républicains",
  "Altri" = "Altri"
)


df_medie$NomeCompleto <- nomi_partiti[as.character(df_medie$Partito)]
df_medie$NomeCompleto <- factor(df_medie$NomeCompleto, levels = c("Altri", "Les Républicains", "Ensemble", "Nouveau Front Populaire", "Rassemblement National"))
df_medie <- df_medie[order(df_medie$Partito), ]

# MoE
calcola_moe <- function(p) {
  moe <- 3 * sqrt((p/100) * (1-p/100) / 50)
  return(moe * 100)
}

df_medie$Margine_Errore <- sapply(df_medie$Media, calcola_moe)

df_medie$Min <- pmax(0, df_medie$Media - df_medie$Margine_Errore)
df_medie$Max <- pmin(100, df_medie$Media + df_medie$Margine_Errore)

# Colori
colori_partiti <- c("RN" = "#40435B", "NFP" = "#D90858", "ENS" = "#FBDD49", "LR" = "#5FBEF1", "Altri" = "gray")

# Frafico

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
      plot.subtitle =  element_text(size = 55,  color = "#1C1C1C", hjust = 0, margin = margin(b = 1, l = 1, t = 0.4, r=0, unit = "cm")),
      plot.caption =   element_text(size = 50,  color = "#1C1C1C", hjust = 1, lineheight = 0.3, margin = margin(b = 0, l = 0, t = 0.5, r=0, unit = "cm")),
      ...
    )
}


png("Francia_2024_Sondaggi_Primo_Turno.png", width = 9.5, height = 10, units="in", res=300)
ggplot(df_medie, aes(x=Media, y=NomeCompleto, group=Partito, fill=Partito, label = paste0(round(Media, 1), "%"))) +
  theme_barchart()+
  geom_col(show.legend = F)+
  geom_text(size=14, hjust=-0.2,  family="Source Sans Pro", show.legend = F)+
  scale_fill_manual(values = colori_partiti) +
  scale_x_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 40)) +
  labs(x = NULL,
       y = NULL,
       title = "Francia: la media dei sondaggi",
       subtitle = "Media ponderata degli ultimi sondaggi per le elezioni legislative - 30 giugno 2024", 
       caption = paste("Elaborazione di Lorenzo Ruffino\nSondaggi: Ipsos, Ifop, Elabe, Odoxa, OpinionWay, Cluster17"))
dev.off()
