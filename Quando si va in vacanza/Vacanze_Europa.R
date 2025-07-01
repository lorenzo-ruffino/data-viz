library(eurostat)
library(tidyverse)
library(sf)
library(data.table)
library(scales)
library(svglite)
library(showtext)
library(ggtext)

setwd("~/Documenti/data-viz/Quando si va in vacanza")

# Assenze settimanali 

weekly_absences = get_eurostat("lfsi_abs_w")%>%
  filter(sex == 'T')%>%
  mutate(quarter = floor_date(TIME_PERIOD, unit = "quarter"))

# Motivazioni per quarter

quaterly_reasons = get_eurostat("lfsi_abs_q_h")%>%
  filter(sex == 'T'
         & reason != 'TOTAL'
         & s_adj == 'NSA')%>%
  group_by(age, geo, TIME_PERIOD)%>%
  mutate(pct_reason = values / sum(values, na.rm = T))%>%
  filter(reason == 'HOL')%>%
  ungroup()%>%
  rename(quarter = TIME_PERIOD)%>%
  select(geo, quarter, pct_reason)

# Aggiungiamo dati post 2019

data_2019 = quaterly_reasons %>%
  filter(year(quarter) == 2019)

data_extended =map_dfr(2020:2022, function(year) {
  data_2019 %>%
    mutate(quarter = quarter + years(year - 2019))
})

quaterly_reasons_extended = bind_rows(quaterly_reasons, data_extended) %>%
  arrange(geo, quarter)

# Occupazione per quarter

employment_quarter = get_eurostat("lfsi_emp_q")%>%
  filter(sex == 'T'
         & age == 'Y20-64'
         & indic_em == 'EMP_LFS'
         & unit == 'THS_PER'
         & s_adj == 'NSA')%>%
  rename(quarter = TIME_PERIOD,
         employee = values)%>%
  select(geo, quarter, employee)

# Unisci tutto

data = left_join(weekly_absences, quaterly_reasons_extended, by = c('geo', 'quarter'), relationship = "many-to-many")%>%
  mutate(holiday = values * pct_reason)%>%
  left_join(employment_quarter, by = c('geo', 'quarter'), relationship = "many-to-many")%>%
  mutate(pct_holiday = 100 * (holiday / employee))%>%
  filter(!lubridate::year(TIME_PERIOD) %in% c(2020, 2021)
         & lubridate::year(TIME_PERIOD) >= 2015 ) %>%
  mutate(
    week_number = isoweek(TIME_PERIOD)
  ) %>%
  group_by(geo, week_number) %>%
  summarise(
    mean_pct_holiday = mean(pct_holiday, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(geo, week_number)%>%
  mutate(
    date = lubridate::ymd("2024-12-30") + weeks(week_number - 1)
  )%>%
  # Togliamo alcuni paesi con diversi dati mancanti
  filter(!geo %in% c("UK", "BG", "EE", "LU", "ME", "MK", "MT"))

data_wide = data %>%
  spread(geo, mean_pct_holiday)


# Tema e grafico

font_add_google("Source Sans Pro")
showtext_auto()


theme_linechart <- function(...) {
  theme_minimal() +
    theme(
      text=element_text(family="Source Sans Pro"),
      legend.position="top",
      legend.key.width = unit(1.8, "cm"),   
      legend.key.height = unit(0.5, "cm"),  
      axis.line = element_line(linewidth = 0.3),
      axis.text = element_text(size = 50,  color = "#1C1C1C", hjust = 0.5,  margin = margin(b = 0.2, t = 0.2, l = 0.2, r=.2, unit = "cm")),
      axis.title = element_text(size = 45,  color = "#1C1C1C", hjust = 0.5,  margin = margin(b = 0, t = 0, l = 0.5, unit = "cm")),
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
      plot.margin = unit(c(b = .75, l = .57, t = .75, r=.75), "cm"),
      plot.title.position = "plot",
      legend.text = element_text(size = 40,  color = "#1C1C1C", hjust = 0.5, 
                                 margin = margin(b = 0.2, l = 0, t = 0.2, r=0, unit = "cm")),  # Aumenta t e b
      plot.title =     element_text(size = 90,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.subtitle = ggtext::element_markdown(size = 55,  color = "#1C1C1C", hjust = 0, 
                                               margin = margin(b = 0.5, l = 0, t = 0.2, r = 0, unit = "cm")),
      plot.caption = element_text(size = 50, color = "#1C1C1C", hjust = 1, lineheight = 0.35,
                                  margin = margin(b = 0, l = 0, t = 0.5, r = 0, unit = "cm")),
      ...
    )
}


png("Vacanze_Italia.png", width = 9.5, height = 10, units="in", res=300)
ggplot(data, aes(x = date, y = mean_pct_holiday)) +
  geom_line(data = filter(data, !geo %in% c('IT', 'EU27_2020')),
            aes(group = geo),
            color = "grey80", alpha = 0.6) +
  geom_line(data = filter(data, geo == 'IT'),
            aes(group = geo),
            color = "#F12938", size = 1.2) +
  geom_line(data = filter(data, geo == 'EU27_2020'),
            aes(group = geo),
            color = "#0478EA", size = 1) +
 # scale_x_continuous(breaks = seq(1, 30, by = 3)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0, 0)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  theme_linechart()+
  labs(x = NULL, 
       y = "% lavoratori in ferie", 
       title = "Quando si va in vacanza",
       subtitle = "Stima della ercentuale di occupati in ferie in <span style='color:#F12938;'>Italia</span>, <span style='color:#0478EA;'>Unione Europa</span> e <span style='color:grey40;'>singoli Stati</span>",
       caption = "Elaborazione di Lorenzo Ruffino su dati Eurostat")
dev.off()


country_names <- c(
  "EU27_2020" = "UE-27",
  "AT" = "Austria", 
  "BE" = "Belgio", 
  "BG" = "Bulgaria", 
  "CY" = "Cipro", 
  "CZ" = "Cechia", 
  "DE" = "Germania", 
  "DK" = "Danimarca", 
  "EL" = "Grecia", 
  "ES" = "Spagna", 
  "FI" = "Finlandia", 
  "FR" = "Francia", 
  "HR" = "Croazia", 
  "HU" = "Ungheria",
  "IE" = "Irlanda", 
  "IS" = "Islanda", 
  "IT" = "Italia", 
  "LT" = "Lituania", 
  "LV" = "Lettonia", 
  "MK" = "Macedonia del Nord", 
  "NL" = "Paesi Bassi", 
  "NO" = "Norvegia", 
  "PL" = "Polonia", 
  "PT" = "Portogallo", 
  "RO" = "Romania", 
  "RS" = "Serbia",
  "SI" = "Slovenia", 
  "SK" = "Slovacchia", 
  "TR" = "Turchia"
)

latitude_order <- c(
  "EU27_2020",  # Prima
  "IS",  # Islanda (64°N)
  "NO",  # Norvegia (62°N)
  "FI",  # Finlandia (61°N)
  "LV",  # Lettonia (57°N)
  "LT",  # Lituania (56°N)
  "DK",  # Danimarca (56°N)
  "IE",  # Irlanda (53°N)
  "NL",  # Paesi Bassi (52°N)
  "PL",  # Polonia (52°N)
  "BE",  # Belgio (51°N)
  "DE",  # Germania (51°N)
  "CZ",  # Cechia (50°N)
  "SK",  # Slovacchia (49°N)
  "AT",  # Austria (47°N)
  "HU",  # Ungheria (47°N)
  "RO",  # Romania (46°N)
  "SI",  # Slovenia (46°N)
  "FR",  # Francia (46°N)
  "HR",  # Croazia (45°N)
  "RS",  # Serbia (44°N)
  "BG",  # Bulgaria (43°N)
  "MK",  # Macedonia del Nord (42°N)
  "IT",  # Italia (42°N)
  "PT",  # Portogallo (40°N)
  "ES",  # Spagna (40°N)
  "TR",  # Turchia (39°N)
  "EL",  # Grecia (39°N)
  "CY"   # Cipro (35°N)
)

heatmap_data <- data %>%
  mutate(
    country_name = country_names[geo]
  ) %>%
  # Paesi per la latitudine
  mutate(country_name = factor(country_name, 
                               levels = rev(country_names[latitude_order])))



data_wide = heatmap_data %>%
  select(-geo, -date)%>%
  mutate(mean_pct_holiday = round(mean_pct_holiday, 1))%>%
  spread(country_name, mean_pct_holiday)


write.csv(data_wide, file="stima_lavoratori_in_ferie_per_settimana.csv", row.names = F)



png("Vacanze_Europa.png", width = 10, height = 10, units="in", res=300)
ggplot(heatmap_data, aes(x = date, y = country_name, fill = mean_pct_holiday)) +
  geom_tile(color = "white", size = 0.1) +
  scale_fill_viridis_b(name = "% in ferie", 
                       limits = c(0, 50),
                       breaks = seq(0, 50, by = 5),
                       labels = function(x) paste0(x, "%"),
                       na.value = "#440154",
                       option = "viridis") +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b", 
               expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme_linechart() +
  labs(
    x = NULL,
    y = NULL,
    title = "Quando si va in vacanza in Europa",
    subtitle = "Stima della percentuale di lavoratori in ferie per settimana",
    caption = "Elaborazione di Lorenzo Ruffino su dati Eurostat")

dev.off()