library(quantmod)
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(showtext)
library(ggrepel)

getSymbols("^GSPC", from = "1950-01-01")
sp500_df <- data.frame(date = index(GSPC), coredata(GSPC))


latest <- data.frame(
  date = Sys.Date(),
  GSPC.Open = NA,
  GSPC.High = NA,
  GSPC.Low = NA, 
  GSPC.Close = NA,
  GSPC.Volume = NA,
  GSPC.Adjusted = 4982
)

sp500_df <- rbind(sp500_df, latest)

presidents <- data.frame(
  name = c(
    "Eisenhower", "Kennedy", "Nixon", "Carter", "Reagan", 
    "Bush Sr.", "Clinton", "Bush Jr.", "Obama", "Trump (2017)", "Biden", "Trump (2025)"
  ),
  inauguration = as.Date(c(
    "1953-01-20", "1961-01-20", "1969-01-20", "1977-01-20", "1981-01-20",
    "1989-01-20", "1993-01-20", "2001-01-20", "2009-01-20", "2017-01-20", "2021-01-20", "2025-01-20"
  ))
)

calculate_returns <- function(start_date, president_name) {
  closest_start_idx <- which.min(abs(sp500_df$date - start_date))
  start_actual_date <- sp500_df$date[closest_start_idx]
  
  end_calendar_date <- start_date + 100
  
  end_idx <- which.min(abs(sp500_df$date - end_calendar_date))
  if (end_idx > nrow(sp500_df)) {
    end_idx <- nrow(sp500_df)
  }
  
  period_data <- sp500_df[closest_start_idx:end_idx, ]
  
  start_price <- period_data$GSPC.Adjusted[1]
  
  period_data$calendar_day <- as.numeric(period_data$date - start_actual_date)
  period_data$return_cumulative <- (period_data$GSPC.Adjusted / start_price - 1) * 100
  
  period_data$president <- president_name
  
  return(period_data)
}

all_returns <- data.frame()

for (i in 1:nrow(presidents)) {
  president_returns <- calculate_returns(presidents$inauguration[i], presidents$name[i])
  all_returns <- rbind(all_returns, president_returns)
}

custom_colors <- c(
  "Eisenhower" = "#95B8D1",   
  "Kennedy" = "#B8E0D2",      
  "Nixon" = "#D6BCFA",         
  "Carter" = "#EAD2AC",        
  "Reagan" = "#B5D99C",       
  "Bush Sr." = "#9CADCE",     
  "Clinton" = "#E6C79C",       
  "Bush Jr." = "#A7C5BD",      
  "Obama" = "#27907A",        
  "Trump (2017)" = "#C2C2C2",  
  "Biden" = "#CBC3E3",         
  "Trump (2025)" = "#FF6B6B"   
)

all_returns$president <- factor(all_returns$president, 
                                levels = c("Eisenhower", "Kennedy", "Nixon", "Carter", "Reagan", 
                                           "Bush Sr.", "Clinton", "Bush Jr.", "Obama", "Trump (2017)", "Biden", "Trump (2025)"))

font_add_google("Source Sans Pro")
showtext_auto()

theme_linechart <- function(...) {
  theme_minimal() +
    theme(
      text=element_text(family="Source Sans Pro"),
      legend.position="none",
      axis.line = element_line(linewidth = 0.5),
      axis.title = element_text(size = 40,  color = "#1C1C1C", hjust = 0.5,  margin = margin(b = 0, t = 0, l = 0, unit = "cm")),
      axis.text = element_text(size = 50,  color = "#1C1C1C", hjust = 0.5,  margin = margin(b = 0, t = 0, l = 0, unit = "cm")),
      axis.ticks = element_blank(),
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
      plot.subtitle = element_text(size = 55, color = "#1C1C1C", hjust = 0, margin = margin(b = 0.5, l = 0, t = 0.05, r = 0, unit = "cm")),
      plot.caption =   element_text(size = 50,  color = "#1C1C1C", hjust = 1, margin = margin(b = 0, l = 0, t = .8, r=0, unit = "cm")),
      ...
    )
}

png("Rendimento_S&P500_Presidenti_Cento_Giorni.png", width = 9.5, height = 10, units="in", res=300)
ggplot(all_returns, aes(x = calendar_day, y = return_cumulative, color = president)) +
  geom_line(aes(size = president == "Trump (2025)")) +
  scale_size_manual(values = c("FALSE" = 0.4, "TRUE" = 1)) +
  geom_text_repel(
    data = all_returns %>% 
      group_by(president) %>% 
      filter(calendar_day == max(calendar_day)),
    aes(label = president),
    nudge_x = 2,
    direction = "y",
    hjust = 0,
    segment.size = 0.3,
    segment.color = "grey50",
    segment.linetype = "dotted",
    box.padding = 0.7,
    force = 1,
    min.segment.length = 0,
    family = "Source Sans Pro",
    size = 15
  ) +
  labs(
    title = "Il S&P 500 nei primi 100 giorni di mandato",
    subtitle = "Rendimenti cumulativi S&P 500 nei primi 100 giorni di mandato presidenziale",
    x = "Giorni di calendario dall'insediamento",
    y = "Rendimento cumulativo (%)",
    caption = "Elaborazione di Lorenzo Ruffino | Fonte dati: quantmod"
  ) +
  scale_x_continuous(breaks = seq(0, 100, 10), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-20, 15), breaks = seq(-100, 100, 5), labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  scale_color_manual(values = custom_colors) +
  coord_cartesian(xlim = c(0, 110))+
  theme_linechart()
dev.off()



summary_table <- all_returns %>%
  group_by(president) %>%
  filter(calendar_day ==77) %>%
  select(president, return_cumulative) %>%
  arrange(desc(return_cumulative))

print(summary_table)