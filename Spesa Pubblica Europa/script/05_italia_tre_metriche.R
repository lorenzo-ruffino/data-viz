suppressMessages({ library(dplyr); library(tidyr) })
options(width = 200)
dir_in  <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Spesa Pubblica Europa/input"
dir_out <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Spesa Pubblica Europa/output"
raw <- readRDS(file.path(dir_in, "gov_10a_exp_raw.rds"))
YR <- 2024

## parametri Italia per il PPS pro-capite reale
pop <- readRDS(file.path(dir_in,"demo_gind_raw.rds")) %>%
  filter(geo=="IT", indic_de=="AVG", TIME_PERIOD==YR) %>% pull(values)
ppp <- readRDS(file.path(dir_in,"prc_ppp_ind_raw.rds")) %>%
  filter(geo=="IT", na_item=="PPP_EU27_2020", ppp_cat=="GDP", TIME_PERIOD==YR) %>% pull(values)
cat(sprintf("Italia 2024: pop media = %.0f  |  PPP(GDP) = %.3f euro/PPS\n\n", pop, ppp))

it <- raw %>% filter(geo=="IT", na_item=="TE", TIME_PERIOD==YR)
mio <- function(code) { v <- it %>% filter(cofog99==code, unit=="MIO_EUR") %>% pull(values); if(length(v)==0) NA_real_ else v }
pps <- function(mioeur) mioeur*1e6/pop/ppp   # PPS pro-capite

tot_mio <- mio("TOTAL")
pil_tot <- it %>% filter(cofog99=="TOTAL", unit=="PC_GDP") %>% pull(values)  # spesa tot in % PIL
PIL <- tot_mio / (pil_tot/100)                                              # PIL nominale (mio EUR)
cat(sprintf("PIL nominale 2024 (derivato) = %.0f mld EUR\n\n", PIL/1000))

# elenco voci: (codice o vettore di codici da sommare, etichetta, livello)
voci <- list(
  list("TOTAL","SPESA TOTALE","tot"),
  list("GF01","Servizi generali","f"),
  list("GF02","Difesa","f"),
  list("GF03","Ordine pubblico e sicurezza","f"),
  list("GF04","Affari economici","f"),
  list("GF05","Protezione dell'ambiente","f"),
  list("GF06","Abitazioni e territorio","f"),
  list("GF07","Sanità","f"),
  list("GF08","Cultura, sport e culto","f"),
  list("GF09","Istruzione","f"),
  list("GF10","Protezione sociale","f"),
  list("GF0107","  · di cui Interessi sul debito","s"),
  list(c("GF01_NET"),"  · Servizi gen. netto interessi","s"),
  list("GF1001","  · Malattia e invalidità","s"),
  list("GF1002","  · Pensioni di vecchiaia","s"),
  list("GF1003","  · Superstiti (reversibilità)","s"),
  list("GF1004","  · Famiglia e infanzia","s"),
  list("GF1005","  · Disoccupazione","s"),
  list("GF1006","  · Edilizia sociale","s"),
  list("GF1007","  · Esclusione sociale n.e.c.","s"),
  list(c("GF1002","GF1003","GF0107"),">> Spesa ipotecata (pensioni vecch.+superstiti+interessi)","agg"),
  list(c("GF07","GF09"),">> Sanità + Istruzione","agg")
)

rows <- lapply(voci, function(v){
  codes <- v[[1]]; lab <- v[[2]]; lev <- v[[3]]
  if (identical(codes,"GF01_NET")) m <- mio("GF01")-mio("GF0107")
  else m <- sum(sapply(codes, mio))
  data.frame(voce=lab, livello=lev,
             pc_pil = m/PIL*100, pc_tot = m/tot_mio*100, pps_pc = pps(m), mio_eur = m)
})
out <- bind_rows(rows)

f1 <- function(x) formatC(x, format="f", digits=1, decimal.mark=",")
fi <- function(x) formatC(round(x), format="d", big.mark=".")
disp <- out %>% transmute(
  Voce=voce,
  `% PIL`=f1(pc_pil),
  `% spesa tot`=f1(pc_tot),
  `PPS pro-cap`=fi(pps_pc),
  `mld €`=f1(mio_eur/1000)
)
cat("=========================================================================================\n")
cat("ITALIA 2024 — scomposizioni nelle tre metriche\n")
cat("(funzioni in MAIUSC sommano al totale; '·' = sottovoci interne; '>>' = aggregati narrativi)\n")
cat("=========================================================================================\n")
print(disp, row.names=FALSE, right=FALSE)

# check additività
f10 <- out %>% filter(livello=="f")
cat(sprintf("\n[check] somma 10 funzioni: %%PIL=%.1f  %%tot=%.1f  PPS=%.0f  | TOTALE: %%PIL=%.1f %%tot=%.1f PPS=%.0f\n",
            sum(f10$pc_pil), sum(f10$pc_tot), sum(f10$pps_pc),
            out$pc_pil[1], out$pc_tot[1], out$pps_pc[1]))

write.csv(out, file.path(dir_out,"11_italia_tre_metriche.csv"), row.names=FALSE)
cat("[ok] salvato output/11_italia_tre_metriche.csv\n")
