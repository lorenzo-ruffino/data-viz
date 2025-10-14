library(tidyverse)
library(showtext)
library(data.table)
library(readxl)

data_pres = fread("Toscana_2025_Risultati_Presidente.csv")


setwd("~/Documents/Progetti/data-viz/Elezioni Italia/2025_10_12_Toscana_Regionali")


pulisci_testo <- function(testo) {
  testo <- iconv(testo, to = "ASCII//TRANSLIT")
  testo <- gsub("[^[:alnum:]]", "", testo)
  testo <- toupper(testo)
  return(testo)
}


# Prov

data_pres %>%
  group_by(Provincia, Cognome)%>%
  summarise(voti = sum(Voti, na.rm = T))%>%
  group_by(Provincia)%>%
  mutate(voti = 100*(voti / sum(voti)))%>%
  spread(Cognome, voti)%>%
  arrange(desc(GIANI))


# Grafici

risultati_2020 = read_excel("2020/RegionaliToscana_Scrutini.xlsx")%>%
  select(prov, comune, votantitot, cognome, VOTICANDLEADER )%>%
  mutate(coalizione = case_when(cognome == "CECCARDI" ~ "CDX",
                                cognome == "GIANI" ~ "CSX",
                                cognome == "GALLETTI" ~ "CSX",
                                T ~ "Altro"))%>%
  group_by(prov, comune, coalizione)%>%
  summarise(voti = sum(VOTICANDLEADER, na.rm = T))%>%
  group_by(prov, comune)%>%
  mutate(perc_2021 = 100 * (voti / sum(voti)))%>%
  mutate(Comune = pulisci_testo(comune))%>%
  filter(coalizione == "CSX")%>%
  ungroup()%>%
  select(Comune, CSX_2020 = perc_2021, voti_2020 = voti)

centroidi = fread("2020/toscana_centrodi.csv")%>%
  select(Comune = COMUNE, X, Y)%>%
  mutate(Comune = pulisci_testo(Comune))


risultati_grafici = data_pres%>%
  select(Provincia, Comune , Cognome , Voti)%>%
  spread(Cognome, Voti)%>%
  mutate(voti_totali = coalesce(GIANI, 0) + coalesce(TOMASI, 0) + coalesce(`MORO BUNDU`, 0),
         Giani_Pct = (coalesce(GIANI, 0) / voti_totali)*100,
         Tomasi_Pct = (coalesce(TOMASI, 0) / voti_totali)*100,
         vincitore = ifelse(GIANI >= TOMASI, "Giani", "Tomasi"),
         differenza = abs(coalesce(GIANI, 0) - coalesce(TOMASI, 0)))%>%
  mutate(comune = Comune,
         Comune = pulisci_testo(Comune))%>%
  left_join(risultati_2020, by = "Comune")%>%
  mutate(variazione = Giani_Pct - CSX_2020)%>%
  left_join(centroidi, by = "Comune")

write.csv(risultati_grafici, file="toscana_risultati_grafici.csv", row.names = F)




library(ggrepel)

top_10_comuni = risultati_grafici %>%
  arrange(desc(GIANI)) %>%
  head(13) %>%
  mutate(Comune_label = str_to_title(comune))

top_5_differenze = risultati_grafici %>%
  mutate(differenza = abs(Giani_Pct - CSX_2020)) %>%
  arrange(desc(differenza)) %>%
  head(7) %>%
  mutate(Comune_label = str_to_title(comune))

comuni_da_etichettare = bind_rows(top_10_comuni, top_5_differenze) %>%
  distinct(comune, .keep_all = TRUE)



png(paste0("Toscana_2020_2025.png"), width = 7.3, height = 7.3, units="in", res=300)
ggplot(risultati_grafici, aes(y = Giani_Pct, x = CSX_2020, 
                              size = GIANI, color = "#E0301E")) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = lm, size = 1.5, se = FALSE, show.legend = FALSE, color = "#961609") +
  geom_label_repel(
    data = comuni_da_etichettare,
    aes(label = Comune_label),
    size = 2.5,
    color = "gray20",
    fill = "white",
    max.overlaps = 20,
    segment.color = "gray50",
    segment.size = 0.5,
    box.padding = 1,
    point.padding = 0,
    min.segment.length = 0,
    direction = "both",
    force = 15,
    force_pull = 1,
    seed = 42
  ) +
  annotate("text", x = 23, y = 25.5, label = "2025 meglio del 2020", 
           angle = 42.7, vjust = 1, hjust = 0, size = 3.5, color = "gray30") +
  annotate("text", x = 25, y = 22.5, label = "2025 peggio del 2020", 
           angle = 42.7, vjust = 0,  hjust = 0, size = 3.5, color = "gray30") +
  scale_size_continuous(guide = "none") +   
  scale_color_identity() +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(20, 80), 
                     breaks = seq(0, 100, by = 10),
                     labels = function(x) paste0(x, "%")) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(20, 80), 
                     breaks = seq(0, 100, by = 10),
                     labels = function(x) paste0(x, "%")) +
  theme_linechart() +
  labs(
    x = '% voti Giani e Galletti nel 2020',
    y = '% voti Giani nel 2025',
    title = "Toscana: come sono cambiati i voti al centrosinistra tra 2020 e 2025",
    subtitle = "Relazione tra i risultati per comune di Giani nel 2025 e quelli di Giani e Galletti nel 2020",
    caption = "Elaborazione di Lorenzo Ruffino | Fonte dati: Eligendo, Ministero dell'Interno"
  ) +
  guides(color = "none") 
dev.off()