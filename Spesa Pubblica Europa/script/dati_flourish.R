suppressMessages({ library(dplyr); library(tidyr) })
dir_in  <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Spesa Pubblica Europa/input"
dir_out <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Spesa Pubblica Europa/output"
raw <- readRDS(file.path(dir_in,"gov_10a_exp_raw.rds"))
geo_lab <- c(IT="Italia", FR="Francia", DE="Germania", ES="Spagna", EU27_2020="Media UE")

## --- G1: treemap, formato gerarchico (macro_categoria, categoria, quota_pct) ---
it <- raw %>% filter(geo=="IT", na_item=="TE", TIME_PERIOD==2024, unit=="MIO_EUR")
mio <- function(c){ v <- it$values[it$cofog99==c]; if(length(v)==0) 0 else v }
tot <- mio("TOTAL")
g1 <- tribble(
  ~macro_categoria,        ~categoria,                  ~mio,
  "Protezione sociale",    "Pensioni di vecchiaia",     mio("GF1002"),
  "Protezione sociale",    "Reversibilità",             mio("GF1003"),
  "Protezione sociale",    "Malattia e invalidità",     mio("GF1001"),
  "Protezione sociale",    "Famiglia",                  mio("GF1004"),
  "Protezione sociale",    "Disoccupazione",            mio("GF1005"),
  "Protezione sociale",    "Altra protezione sociale",  mio("GF10")-mio("GF1002")-mio("GF1003")-mio("GF1001")-mio("GF1004")-mio("GF1005"),
  "Interessi sul debito",  "Interessi sul debito",      mio("GF0107"),
  "Sanità e istruzione",   "Sanità",                    mio("GF07"),
  "Sanità e istruzione",   "Istruzione",                mio("GF09"),
  "Funzioni dello Stato",  "Funzionamento Stato",       mio("GF01")-mio("GF0107"),
  "Funzioni dello Stato",  "Ordine pubblico",           mio("GF03"),
  "Funzioni dello Stato",  "Difesa",                    mio("GF02"),
  "Economia e territorio", "Affari economici",          mio("GF04"),
  "Economia e territorio", "Ambiente cultura e case",   mio("GF05")+mio("GF08")+mio("GF06")
) %>%
  mutate(quota_pct = round(mio/tot*100, 1)) %>%
  select(macro_categoria, categoria, quota_pct)
write.csv(g1, file.path(dir_out,"flourish_01_treemap.csv"), row.names=FALSE, fileEncoding="UTF-8")

## --- G2: stacked bar, wide (Paese, Pensioni, Interessi, Resto) in % spesa totale ---
g2 <- raw %>% filter(TIME_PERIOD==2024, na_item=="TE", unit=="PC_TOT",
                     cofog99 %in% c("GF1002","GF1003","GF0107")) %>%
  mutate(voce=ifelse(cofog99=="GF0107","Interessi","Pensioni")) %>%
  group_by(geo, voce) %>% summarise(v=sum(values), .groups="drop") %>%
  pivot_wider(names_from=voce, values_from=v) %>%
  mutate(Paese=geo_lab[geo], Resto=100-Pensioni-Interessi, rig=Pensioni+Interessi) %>%
  arrange(desc(rig)) %>%
  transmute(Paese, Pensioni=round(Pensioni,1), Interessi=round(Interessi,1), Resto=round(Resto,1))
write.csv(g2, file.path(dir_out,"flourish_02_pensioni_interessi.csv"), row.names=FALSE, fileEncoding="UTF-8")

## --- G3: barre raggruppate, wide (Voce, Italia, Francia, Germania, Spagna, Media UE) in PPS ---
pop <- readRDS(file.path(dir_in,"demo_gind_raw.rds")) %>%
  filter(indic_de=="AVG", TIME_PERIOD==2024) %>% select(geo, pop=values)
ppp <- readRDS(file.path(dir_in,"prc_ppp_ind_raw.rds")) %>%
  filter(na_item=="PPP_EU27_2020", ppp_cat=="GDP", TIME_PERIOD==2024) %>% select(geo, ppp=values)
voci3 <- c(GF07="Sanità", GF09="Istruzione", GF1002="Pensioni di vecchiaia")
g3 <- raw %>% filter(TIME_PERIOD==2024, na_item=="TE", unit=="MIO_EUR", cofog99 %in% names(voci3)) %>%
  left_join(pop, by="geo") %>% left_join(ppp, by="geo") %>%
  mutate(pps=round(values*1e6/pop/ppp), Voce=voci3[cofog99], paese=geo_lab[geo]) %>%
  select(Voce, paese, pps) %>%
  pivot_wider(names_from=Voce, values_from=pps) %>%
  mutate(paese=factor(paese, levels=c("Italia","Francia","Germania","Spagna","Media UE"))) %>%
  arrange(paese) %>%
  transmute(Paese=paese, `Sanità`, `Istruzione`, `Pensioni di vecchiaia`)
write.csv(g3, file.path(dir_out,"flourish_03_procapite_pps.csv"), row.names=FALSE, fileEncoding="UTF-8")

cat("== flourish_01_treemap.csv ==\n"); print(g1, row.names=FALSE)
cat("\n== flourish_02_pensioni_interessi.csv ==\n"); print(as.data.frame(g2), row.names=FALSE)
cat("\n== flourish_03_procapite_pps.csv ==\n"); print(as.data.frame(g3), row.names=FALSE)
