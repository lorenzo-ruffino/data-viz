library(tidyverse)
library(data.table)
library(showtext)

link = "https://projects.fivethirtyeight.com/polls/data/presidential_general_averages.csv"

data = as.data.frame(fread(link))%>%
  filter(cycle == 2024)%>%
  mutate(date = as.Date(date))%>%
  group_by(state)%>%
  filter(date %in% c(max(date), as.Date("2024-06-27"))
         & candidate %in% c("Trump", "Biden")
         & state %in% c("Arizona" , "Georgia", "Nevada",
                        "Pennsylvania", "Wisconsin", "Minnesota", "Michigan", "National"))%>%
  mutate(periodo = ifelse(date == as.Date("2024-06-27"), "prima", "dopo"))%>%
  select(-party, -hi, -lo, -date, -cycle)%>%
  pivot_wider(names_from = c(candidate, periodo),
              values_from = c(pct_estimate),
              names_glue = "{candidate}_{periodo}_{.value}")%>%
  mutate(prima = Trump_prima_pct_estimate - Biden_prima_pct_estimate,
         dopo = Trump_dopo_pct_estimate - Biden_dopo_pct_estimate,
         colore = prima < dopo)


ordered_data <- data %>%
  arrange(ifelse(state == "National", +Inf, -dopo))

ordered_state <- factor(ordered_data$state, levels = ordered_data$state)

data <- data %>%
  mutate(state_ordered = factor(state, levels = levels(ordered_state)))


font_add_google("Source Sans Pro")
showtext_auto()


theme_barchart <- function(...) {
  theme_minimal() +
    theme(
      text=element_text(family="Source Sans Pro"),
      axis.line = element_blank(),
      axis.text.y = element_text(size = 55,  color = "#1C1C1C", hjust = 0,  margin = margin(b = 0, t = 0, l = 0, unit = "cm"), lineheight = 0.35),
      axis.text.x = element_blank(),
      axis.title = element_blank(),
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
      plot.subtitle =  element_text(size = 55,  color = "#1C1C1C", hjust = 0, margin = margin(b = 1, l = 0, t = 0.4, r=0, unit = "cm")),
      plot.caption =   element_text(size = 50,  color = "#1C1C1C", hjust = 1, margin = margin(b = 0, l = 0, t = 0.5, r=0, unit = "cm")),
      ...
    )
}


png("Media_Sondaggi_Dibattito_USA.png", width = 9.5, height = 10, units="in", res=300)
ggplot(data) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey")+
  geom_segment(aes(x = prima, y = state_ordered, xend = dopo, yend = state_ordered, color = colore),
               arrow = arrow(type = "closed", length = unit(0.4, "cm")), show.legend = F)+
  scale_color_manual(values = c("TRUE" = "#E35D5D",
                                "FALSE" = "#3963A2"))+
  geom_text(aes(y=state_ordered, x=prima, label=paste0(round(prima, 1), ""), hjust=ifelse(colore == "TRUE", 1.2, -0.2)), size=17, family="Source Sans Pro")+
  geom_text(aes(y=state_ordered, x=dopo, label=paste0(round(dopo, 1), ""), hjust=ifelse(colore != "TRUE", 1.2, -0.2)), size=17, family="Source Sans Pro")+
  theme_barchart()+
  scale_x_continuous(limits=c(-2.5,6))+
  labs(x = NULL,
     y = NULL,
     title = "Stati Uniti: i sondaggi dopo il dibattito",
     subtitle = "Vantaggio di Donald Trump negli Stati chiave il 27 giugno e l'11 luglio", 
     caption = paste("Elaborazione di Lorenzo Ruffino | Fonte: FiveThirtyEight"))
dev.off()
