library(data.table)

SRC <- "/Users/lorenzoruffino/Downloads/Datafile-subset/Datafile-subset.csv"
DIR <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Opinioni economiche ESS"

cols <- c(
  # id / design / pesi
  "name","essround","edition","proddate","idno","cntry","dweight","pspwght","pweight","anweight",
  # atteggiamenti core
  "gincdif","ginveco","dfincac","smdfslv","lrscale","polintr","stfeco","stfgov","stfdem","stfedu","stfhlth",
  "trstprl","trstplt","trstprt","trstlgl","trstplc","trstep","trstun","euftf","vote","clsprty","prtdgcl",
  "freehms","ecohenv","needtru","lawobey","imbgeco",
  # welfare / benefici (R4/R8)
  "gvslvol","gvslvue","gvhlthc","gvcldcr","gvpdlwk","gvjbevn",
  "sbstrec","sbbsntx","sbeqsoc","sblazy","sblwcoa","sblwlka",
  "bennent","lbenent","uentrjb","slvpens","slvuemp","basinc","eudcnbf","imsclbn",
  # giustizia distributiva (R9)
  "netifr","grspfr","frprtpl","ifredu","ifrjob","evfredu","evfrjob",
  "sofrdst","sofrwrk","sofrpr","sofrprv","ppldsrv","wltdffr","topinfr","btminfr",
  "recskil","recexp","recknow","recimg","recgndr",
  # lavoro
  "mbtru","wkdcorga","iorgact","stfmjob","tporgwk","wrkctra","emplrel","emplno",
  "uemp3m","uemp5yr","uempla","uempli","mnactic","isco08","iscoco","nacer2",
  # sociodemo
  "agea","yrbrn","gndr","eisced","edulvlb","eduyrs","domicil","region",
  "hincfel","hinctnta","hinctnt","hhmmb","maritalb","chldhm","rlgdgr",
  "brncntr","ctzcntr","blgetmg","health","happy",
  # politica Italia
  "prtvtit","prtvtait","prtvtbit","prtvtcit","prtvtdit","prtvteit",
  "prtclit","prtclait","prtclbit","prtclcit","prtcldit","prtcleit","prtclfit"
)

header <- names(fread(SRC, nrows = 0))
missing_cols <- setdiff(cols, header)
if (length(missing_cols)) cat("COLONNE NON TROVATE:", paste(missing_cols, collapse=", "), "\n")
cols <- intersect(cols, header)

d <- fread(SRC, select = cols, showProgress = FALSE)
cat("Caricate", nrow(d), "righe,", ncol(d), "colonne\n")

# anweight mancante nei round 2 e 3: anweight = pspwght * pweight (definizione ESS)
d[is.na(anweight), anweight := pspwght * pweight]
stopifnot(d[is.na(anweight), .N] == 0)

# anno di riferimento del round (inizio fieldwork)
anni <- c(`1`=2002,`2`=2004,`3`=2006,`4`=2008,`5`=2010,`6`=2012,`7`=2014,`8`=2016,`9`=2018,`10`=2021,`11`=2023)
d[, anno := anni[as.character(essround)]]

saveRDS(d, file.path(DIR, "input", "ess_slim.rds"), compress = "xz")
cat("Salvato ess_slim.rds\n")

# matrice di presenza: N risposte valide (non-NA) per variabile x round, totale e Italia
vars <- setdiff(names(d), c("name","essround","edition","proddate","idno","cntry","dweight","pspwght","pweight","anweight","anno"))
pres_all <- d[, lapply(.SD, function(x) sum(!is.na(x))), by = essround, .SDcols = vars]
pres_it  <- d[cntry=="IT", lapply(.SD, function(x) sum(!is.na(x))), by = essround, .SDcols = vars]
fwrite(melt(pres_all, id.vars="essround", variable.name="variabile", value.name="n_validi"),
       file.path(DIR, "input", "presenza_variabili_tutti.csv"))
fwrite(melt(pres_it, id.vars="essround", variable.name="variabile", value.name="n_validi"),
       file.path(DIR, "input", "presenza_variabili_italia.csv"))
cat("Salvate matrici di presenza\n")

# vista compatta: per le variabili di atteggiamento, in quali round l'Italia ha dati
pit <- melt(pres_it, id.vars="essround", variable.name="variabile", value.name="n")
compact <- pit[n > 100, .(round_italia = paste(sort(unique(essround)), collapse=",")), by = variabile]
fwrite(compact[order(variabile)], file.path(DIR, "input", "presenza_compatta_italia.csv"))
print(compact[variabile %in% c("gincdif","ginveco","gvslvue","gvslvol","basinc","topinfr","sofrdst","needtru","sbstrec","sblazy","dfincac","smdfslv","slvpens","bennent","stfeco","lrscale","hincfel","wltdffr","imsclbn","eudcnbf")])
