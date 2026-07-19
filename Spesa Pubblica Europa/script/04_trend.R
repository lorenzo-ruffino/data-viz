suppressMessages({ library(dplyr); library(tidyr) })
options(width = 200)
dir_in  <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Spesa Pubblica Europa/input"
dir_out <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Spesa Pubblica Europa/output"
raw <- readRDS(file.path(dir_in, "gov_10a_exp_raw.rds"))
fmt <- function(x) formatC(x, format="f", digits=1, decimal.mark=",")

anni <- c(2007, 2010, 2014, 2019, 2024)
voci <- c(TOTAL="Spesa totale", GF07="Sanità", GF09="Istruzione",
          GF10="Protezione sociale", GF1002="Pensioni vecchiaia",
          GF0107="Interessi debito", GF1004="Famiglia e infanzia")

## --- Italia: composizione nel tempo, % PIL ---
it_trend <- raw %>%
  filter(geo=="IT", na_item=="TE", cofog99 %in% names(voci),
         unit=="PC_GDP", TIME_PERIOD %in% anni) %>%
  mutate(voce=voci[cofog99]) %>%
  select(voce, cofog99, TIME_PERIOD, values) %>%
  pivot_wider(names_from=TIME_PERIOD, values_from=values) %>%
  arrange(match(cofog99, names(voci))) %>%
  mutate(`var 07->24` = .data[["2024"]] - .data[["2007"]])

cat("==================================================================\n")
cat("ITALIA - spesa per funzione in % PIL, nel tempo\n")
cat("==================================================================\n")
print(it_trend %>% mutate(across(where(is.numeric), fmt)), row.names=FALSE)

## --- confronto sanità+istruzione vs pensioni+interessi (Italia) ---
blocchi <- raw %>%
  filter(geo=="IT", na_item=="TE", unit=="PC_GDP", TIME_PERIOD %in% anni) %>%
  mutate(blocco=case_when(
    cofog99 %in% c("GF07","GF09") ~ "Sanità + Istruzione",
    cofog99 %in% c("GF1002","GF0107") ~ "Pensioni vecchiaia + Interessi",
    TRUE ~ NA_character_)) %>%
  filter(!is.na(blocco)) %>%
  group_by(blocco, TIME_PERIOD) %>% summarise(v=sum(values), .groups="drop") %>%
  pivot_wider(names_from=TIME_PERIOD, values_from=v)
cat("\nITALIA - due blocchi a confronto (% PIL):\n")
print(blocchi %>% mutate(across(where(is.numeric), fmt)), row.names=FALSE)

## --- quanta parte della spesa va a pensioni vecchiaia+superstiti+interessi ---
geo_lab <- c(IT="Italia", FR="Francia", DE="Germania", ES="Spagna", EU27_2020="Media UE")
rigidi <- raw %>%
  filter(TIME_PERIOD==2024, na_item=="TE", unit=="PC_TOT",
         cofog99 %in% c("GF1002","GF1003","GF0107")) %>%
  group_by(geo) %>% summarise(quota=sum(values), .groups="drop") %>%
  mutate(paese=geo_lab[geo]) %>% select(paese, `quota su spesa tot (%)`=quota) %>%
  arrange(desc(`quota su spesa tot (%)`))
cat("\nQuota di spesa assorbita da Pensioni(vecchiaia+superstiti)+Interessi, 2024 (% spesa totale):\n")
print(rigidi %>% mutate(across(where(is.numeric), fmt)), row.names=FALSE)

write.csv(it_trend, file.path(dir_out,"09_italia_trend_pil.csv"), row.names=FALSE)
write.csv(rigidi,   file.path(dir_out,"10_spesa_rigida_quota.csv"), row.names=FALSE)
cat("\n[ok] salvati\n")
