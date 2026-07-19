suppressMessages({ library(dplyr); library(tidyr) })
options(width = 220)
dir_in  <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Spesa Pubblica Europa/input"
dir_out <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Spesa Pubblica Europa/output"
raw <- readRDS(file.path(dir_in, "gov_10a_exp_raw.rds"))
YR <- 2024

## verifica: esistono codici COFOG a 6 cifre (livello 3)? E sotto GF1002?
allc <- sort(unique(raw$cofog99))
cat("Codici COFOG piu lunghi di GF+4cifre (livello 3):",
    paste(allc[nchar(allc) > 6], collapse=", "), "\n")
cat("=> il dataset si ferma al gruppo a 4 cifre: GF1002 NON e' ulteriormente scomponibile.\n\n")

## parametri Italia
pop <- readRDS(file.path(dir_in,"demo_gind_raw.rds")) %>%
  filter(geo=="IT", indic_de=="AVG", TIME_PERIOD==YR) %>% pull(values)
ppp <- readRDS(file.path(dir_in,"prc_ppp_ind_raw.rds")) %>%
  filter(geo=="IT", na_item=="PPP_EU27_2020", ppp_cat=="GDP", TIME_PERIOD==YR) %>% pull(values)
it <- raw %>% filter(geo=="IT", na_item=="TE", TIME_PERIOD==YR)
ue <- raw %>% filter(geo=="EU27_2020", na_item=="TE", TIME_PERIOD==YR)
tot_mio <- it %>% filter(cofog99=="TOTAL", unit=="MIO_EUR") %>% pull(values)
pil_tot <- it %>% filter(cofog99=="TOTAL", unit=="PC_GDP") %>% pull(values)
PIL <- tot_mio/(pil_tot/100)

tabella <- function(codes, labels, titolo){
  df <- data.frame(cod=codes, voce=labels) %>%
    rowwise() %>%
    mutate(
      mio = it %>% filter(cofog99==cod, unit=="MIO_EUR") %>% pull(values) %>% (\(x) if(length(x)==0) 0 else x)(),
      ue_pil = ue %>% filter(cofog99==cod, unit=="PC_GDP") %>% pull(values) %>% (\(x) if(length(x)==0) NA else x)()
    ) %>% ungroup() %>%
    mutate(pc_pil=mio/PIL*100, pc_tot=mio/tot_mio*100, pps=mio*1e6/pop/ppp, mld=mio/1000)
  f1 <- function(x) formatC(x, format="f", digits=1, decimal.mark=",")
  fi <- function(x) formatC(round(x), format="d", big.mark=".")
  cat("==============================================================================================\n")
  cat(titolo, "\n")
  cat("==============================================================================================\n")
  disp <- df %>% transmute(Voce=voce, `% PIL`=f1(pc_pil), `% spesa`=f1(pc_tot),
                           `PPS pc`=fi(pps), `mld €`=f1(mld), `[UE %PIL]`=f1(ue_pil))
  print(disp, row.names=FALSE, right=FALSE)
  cat(sprintf("   somma sottovoci %% PIL = %.1f\n\n", sum(df$pc_pil)))
  df %>% mutate(blocco=titolo)
}

g01 <- tabella(
  paste0("GF010", 1:8),
  c("Organi governo/legisl., fisco, esteri","Aiuti economici internazionali",
    "Servizi generali (personale, pianif.)","Ricerca di base","R&S servizi generali",
    "Servizi generali n.e.c.","Interessi sul debito","Trasferimenti tra livelli di governo"),
  "GF01 SERVIZI GENERALI — disaggregato (Italia 2024)")

g04 <- tabella(
  paste0("GF040", 1:9),
  c("Affari econ./commercio e lavoro","Agricoltura, foreste, pesca","Combustibili ed energia",
    "Estrattiva, manifattura, costruzioni","Trasporti","Comunicazioni","Altri settori",
    "R&S affari economici","Affari economici n.e.c."),
  "GF04 AFFARI ECONOMICI — disaggregato (Italia 2024)")

g09 <- tabella(
  paste0("GF090", 1:8),
  c("Pre-primaria e primaria","Secondaria","Post-secondaria non terziaria",
    "Terziaria (UNIVERSITÀ)","Non definibile per livello","Servizi ausiliari",
    "R&S istruzione","Istruzione n.e.c."),
  "GF09 ISTRUZIONE — disaggregato (Italia 2024)")

## aggregato istruzione: universita vs resto
istr <- it %>% filter(cofog99 %in% paste0("GF090",1:8), unit=="MIO_EUR")
uni <- istr %>% filter(cofog99=="GF0904") %>% pull(values)
resto <- sum(istr$values) - uni
cat("ISTRUZIONE: università", round(uni/1000,1),"mld (",round(100*uni/sum(istr$values)),"%) | resto",
    round(resto/1000,1),"mld (",round(100*resto/sum(istr$values)),"%)\n")
ue_uni <- ue %>% filter(cofog99=="GF0904", unit=="PC_GDP") %>% pull(values)
ue_istr <- ue %>% filter(cofog99=="GF09", unit=="PC_GDP") %>% pull(values)
cat(sprintf("Università in %% PIL: IT %.1f vs UE %.1f | quota università su istruzione: IT %.0f%% vs UE %.0f%%\n",
            uni/PIL*100, ue_uni, 100*uni/sum(istr$values), 100*ue_uni/ue_istr))

bind_rows(g01,g04,g09) %>%
  select(blocco, cod, voce, pc_pil, pc_tot, pps, mld, ue_pil) %>%
  write.csv(file.path(dir_out,"12_disaggregazioni.csv"), row.names=FALSE)
cat("\n[ok] salvato output/12_disaggregazioni.csv\n")
