# =====================================================================
# Demografia d'impresa: confronto europeo (Eurostat Business Demography)
# Scarica 4 dataset e li salva in input/eurostat/
# Serie storica coerente: bd_9bd_sz_cl_r2 (SBS classico, 2004-2020)
# Snapshot recente: bd_size (EBS/SBS 2021+, rottura metodologica)
# =====================================================================
suppressMessages(library(eurostat))
suppressMessages(library(dplyr))

outdir <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Demografia imprese/input/eurostat"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

GG <- c("EU27_2020","IT","DE","FR","ES")
geo_lab <- c(EU27_2020="Unione europea (27)", IT="Italia", DE="Germania",
             FR="Francia", ES="Spagna")

add_labels <- function(df){
  df$geo_label <- geo_lab[df$geo]
  df
}

# ---------------------------------------------------------------------
# 1) TASSO DI NATALITA' e MORTALITA' - tutte le imprese - business economy
#    Dataset: bd_9bd_sz_cl_r2 (NACE Rev.2, 2004-2020)
#    nace B-N_X_K642 = industria, costruzioni e servizi (escl. holding e PA)
#    V97020 = birth rate %, V97030 = death rate %
# ---------------------------------------------------------------------
d1 <- get_eurostat("bd_9bd_sz_cl_r2",
        filters = list(geo=GG, indic_sb=c("V97020","V97030"),
                       sizeclas="TOTAL", nace_r2="B-N_X_K642"),
        time_format = "num") %>%
  transmute(dataset="bd_9bd_sz_cl_r2",
            popolazione="all_enterprises",
            indic_code=indic_sb,
            indicatore=recode(indic_sb, V97020="Tasso di natalita (%)",
                                        V97030="Tasso di mortalita (%)"),
            geo, nace_r2, sizeclas,
            anno=time, valore=values, unita="percentuale") %>%
  add_labels() %>% arrange(indic_code, geo, anno)
write.csv(d1, file.path(outdir,"01_tasso_natalita_mortalita_all_2004_2020.csv"),
          row.names=FALSE, na="")
cat("File 1:", nrow(d1), "righe\n")

# ---------------------------------------------------------------------
# 2) TASSO DI SOPRAVVIVENZA a 3 e 5 anni - tutte le imprese
#    V97043 = survival rate 3 (%), V97045 = survival rate 5 (%)
# ---------------------------------------------------------------------
d2 <- get_eurostat("bd_9bd_sz_cl_r2",
        filters = list(geo=GG, indic_sb=c("V97043","V97045"),
                       sizeclas="TOTAL", nace_r2="B-N_X_K642"),
        time_format = "num") %>%
  transmute(dataset="bd_9bd_sz_cl_r2",
            popolazione="all_enterprises",
            indic_code=indic_sb,
            indicatore=recode(indic_sb,
                V97043="Sopravvivenza a 3 anni (%)",
                V97045="Sopravvivenza a 5 anni (%)"),
            geo, nace_r2, sizeclas,
            anno=time, valore=values, unita="percentuale") %>%
  add_labels() %>% arrange(indic_code, geo, anno)
write.csv(d2, file.path(outdir,"02_sopravvivenza_3_5_anni_all_2004_2020.csv"),
          row.names=FALSE, na="")
cat("File 2:", nrow(d2), "righe\n")

# ---------------------------------------------------------------------
# 3) NATE PER CLASSE DIMENSIONALE (numero) -> quota nate SENZA dipendenti
#    V11920 = number of enterprise births, per sizeclas
# ---------------------------------------------------------------------
d3 <- get_eurostat("bd_9bd_sz_cl_r2",
        filters = list(geo=GG, indic_sb="V11920",
                       sizeclas=c("TOTAL","0","1-4","5-9","GE10"),
                       nace_r2="B-N_X_K642"),
        time_format = "num") %>%
  transmute(dataset="bd_9bd_sz_cl_r2",
            popolazione="all_enterprises",
            indic_code=indic_sb,
            indicatore="Imprese nate (numero)",
            geo, nace_r2, sizeclas,
            anno=time, valore=values, unita="numero") %>%
  add_labels() %>% arrange(geo, anno, sizeclas)
write.csv(d3, file.path(outdir,"03_nate_per_classe_dimensionale_all_2004_2020.csv"),
          row.names=FALSE, na="")
cat("File 3:", nrow(d3), "righe\n")

# ---------------------------------------------------------------------
# 4) SNAPSHOT RECENTE (bd_size, EBS 2021-2023) - rottura metodologica
#    nace B-S_X_O_S94 = business economy (escl. PA e org. associative)
#    natalita/mortalita/sopravvivenza Y3-Y5 + nate 0 dipendenti
# ---------------------------------------------------------------------
raw4 <- get_eurostat("bd_size",
        filters = list(geo=GG, nace_r2="B-S_X_O_S94"),
        time_format = "num")

r4a <- raw4 %>% filter(indic_sbs %in% c("ENT_BRTHR_PC","ENT_DTHR_PC"),
                       sizeclas=="TOTAL", age=="TOTAL") %>%
  transmute(indic_code=indic_sbs,
            indicatore=recode(indic_sbs, ENT_BRTHR_PC="Tasso di natalita (%)",
                              ENT_DTHR_PC="Tasso di mortalita (%)"),
            sizeclas, age, geo, anno=time, valore=values, unita="percentuale")
# NB: la serie EBS parte dal 2021, quindi al 2023 si osserva solo la
# sopravvivenza a 1 e 2 anni (le coorti non hanno ancora 3/5 anni di vita).
# Sopravvivenza a 3 e 5 anni: usare il file 02 (serie storica bd_9bd_sz_cl_r2).
# ENT_SRVLR_BRTH_PC = imprese sopravvissute / imprese nate nella coorte (%).
r4b <- raw4 %>% filter(indic_sbs=="ENT_SRVLR_BRTH_PC", age %in% c("Y1","Y2"),
                       sizeclas=="TOTAL") %>%
  transmute(indic_code=indic_sbs,
            indicatore=ifelse(age=="Y1","Sopravvivenza a 1 anno (%)","Sopravvivenza a 2 anni (%)"),
            sizeclas, age, geo, anno=time, valore=values, unita="percentuale")
r4c <- raw4 %>% filter(indic_sbs=="ENT_BRTH_NR", sizeclas %in% c("TOTAL","0"),
                       age=="TOTAL") %>%
  transmute(indic_code=indic_sbs, indicatore="Imprese nate (numero)",
            sizeclas, age, geo, anno=time, valore=values, unita="numero")
d4 <- bind_rows(r4a,r4b,r4c) %>%
  mutate(dataset="bd_size", popolazione="all_enterprises",
         nace_r2="B-S_X_O_S94") %>%
  add_labels() %>%
  select(dataset,popolazione,indic_code,indicatore,geo,geo_label,
         nace_r2,sizeclas,age,anno,valore,unita) %>%
  arrange(indic_code, geo, anno, sizeclas)
write.csv(d4, file.path(outdir,"04_snapshot_recente_bd_size_2021_2023.csv"),
          row.names=FALSE, na="")
cat("File 4:", nrow(d4), "righe\n")

cat("\nFATTO. File in:", outdir, "\n")
