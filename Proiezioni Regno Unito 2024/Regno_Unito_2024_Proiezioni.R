library(tidyverse)
library(data.table)
library(showtext)

data = fread("https://raw.githubusercontent.com/inglesp/apogee/main/outputs/data/predictions-all.csv")

data%>%
  group_by(model)%>%
  filter(scrape_date == max(scrape_date))%>%
  group_by(model, code)%>%
  arrange(desc(prediction))%>%
  slice(1)%>%
  group_by(model, party)%>%
  count()%>%
  spread(model, n)%>%
  arrange(desc(yougov))


df = data%>%
  group_by(model)%>%
  filter(scrape_date == max(scrape_date))%>%
  group_by(model, code)%>%
  arrange(desc(prediction))%>%
  slice(1)%>%
  group_by(model, party)%>%
  count()%>%
  group_by(party)%>%
  filter(model != "2019")%>%
  summarise(mean = mean(n, na.rm = T),
            min = min(n, na.rm = T),
            max = max(n, na.rm = T))%>%
  mutate(party = case_when (party == "lab" ~ "Labour",
                           party == "con" ~ "Conservatives",
                           party == "lib" ~ "Lib Dems",
                           party == "snp" ~ "SNP",
                           party ==  "pc" ~ "Plaid Cymru",
                           party ==  "ref" ~ "Reform UK",
                           party == "grn" ~ "Green",
                           T ~ party))%>%
  filter(party != "oth")



party_colors <- c(
  "Labour" = "#DC241F",
  "Conservatives" = "#0087DC",
  "Lib Dems" = "#FDBB30",
  "SNP" = "#FDF38E",
  "Plaid Cymru" = "#005B54",
  "Reform UK" = "#12B6CF",
  "Green" = "#6AB023"
)

font_add_google("Source Sans Pro")
showtext_auto()


theme_barchart <- function(...) {
  theme_minimal() +
    theme(
      text=element_text(family="Source Sans Pro"),
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
      plot.margin = unit(c(b = 1, l = 1, t = 1, r=1), "cm"),
      plot.title.position = "plot",
      legend.text =    element_text(size = 50,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.title =     element_text(size = 90,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 1, t = 0, r=0, unit = "cm")),
      plot.subtitle =  element_text(size = 55,  color = "#1C1C1C", hjust = 0, margin = margin(b = 1, l = 1, t = 0.2, r=0, unit = "cm")),
      plot.caption =   element_text(size = 50,  color = "#1C1C1C", hjust = 1, lineheight = 0.3, margin = margin(b = 0, l = 0, t = 0.5, r=0, unit = "cm")),
      ...
    )
}


png("Regno_Unito_2024.png", width = 9.5, height = 10, units="in", res=300)
ggplot(df, aes(y = reorder(party, mean))) +
  geom_col(aes(x = max, fill = party), alpha = 0.4, width = 0.7, show.legend = F) +
  geom_col(aes(x = min, fill = party), alpha = 0.9, width = 0.7, show.legend = F) +
  geom_segment(aes(x = mean, xend = mean, 
                   y = as.numeric(reorder(party, mean)) - 0.35, 
                   yend = as.numeric(reorder(party, mean)) + 0.35, 
                   color = party), 
               alpha = 0.9, 
               size = 1.5, show.legend = F) +
  geom_text(data = df %>% filter(party %in% c("Labour", "Conservatives", "Lib Dems")),
            aes(x = max, label = round(max, 0)), hjust = 0, color = "black",
            size = 14,  family="Source Sans Pro", show.legend = F) +
  geom_text(aes(x = mean + ifelse(party %in% c("Plaid Cymru", "Reform UK", "Green"), 6, 0),
              label = round(mean, 0)), hjust = 0.5, vjust=.5, color = "black",
            size = 18,  family="Source Sans Pro", show.legend = F) +
  geom_text(data = df %>% filter(party %in% c("Labour", "Conservatives", "Lib Dems")),
            aes(x = min, label = round(min, 0)), hjust = 1, color = "black",
            size = 14,  family="Source Sans Pro", show.legend = F) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  scale_fill_manual(values = party_colors) +
  scale_color_manual(values = party_colors) +
  theme_barchart()+
  labs(x = NULL,
       y = NULL,
       title = "Regno Unito: le proiezione dei seggi",
       subtitle = "Media delle proiezione dei seggi e intervallo di incertezza - 4 luglio 2024", 
       caption = paste("Elaborazione di Lorenzo Ruffino | Fonte dati: Peter Inglesby\nPrevisioni considerate: Britain Predicts, Economist, electionmaps, Electoral Calculus,\nFocaldata, FT, Ipsos, JLP, More in Common, Savanta, Survation, WeThink, YouGov"))
dev.off()