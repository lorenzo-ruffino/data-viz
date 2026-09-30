library(data.table)
DIR <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Opinioni economiche ESS"
OUT <- file.path(DIR, "output", "grafici")
dir.create(OUT, showWarnings = FALSE)

nomi <- c(AT="Austria", BE="Belgio", BG="Bulgaria", CH="Svizzera", CY="Cipro", CZ="Cechia",
  DE="Germania", DK="Danimarca", EE="Estonia", ES="Spagna", FI="Finlandia", FR="Francia",
  GB="Regno Unito", GR="Grecia", HR="Croazia", HU="Ungheria", IE="Irlanda", IS="Islanda",
  IT="Italia", LT="Lituania", LV="Lettonia", NL="Paesi Bassi", NO="Norvegia", PL="Polonia",
  PT="Portogallo", SE="Svezia", SI="Slovenia", SK="Slovacchia", Europa="Media europea")

# G1 - classifica gincdif ultimo dato (2023-24), barre
cl <- fread(file.path(DIR, "output/estrazioni/redistribuzione_classifica_r11.csv"))
g1 <- cl[tipo_valore == "pct_accordo", .(Paese = nomi[aggregato], `D'accordo` = round(valore))][order(-`D'accordo`)]
fwrite(g1, file.path(OUT, "g1_redistribuzione_classifica.csv"))

# G2 - quota "responsabilita' dello Stato" (7-10), barre raggruppate
resp <- fread(file.path(DIR, "output/estrazioni/ruolostato_responsabilita.csv"))
r8 <- resp[essround == 8 & tipo_valore == "pct_7_10" & aggregato %in% c("IT","DE","FR","ES","GB","Europa")]
g2 <- dcast(r8[variabile %in% c("gvslvol","gvslvue")], aggregato ~ variabile, value.var = "valore")
g2 <- g2[, .(Paese = nomi[aggregato], `Tenore di vita degli anziani` = round(gvslvol),
             `Tenore di vita dei disoccupati` = round(gvslvue))][order(-`Tenore di vita dei disoccupati`)]
fwrite(g2, file.path(OUT, "g2_responsabilita_stato.csv"))

# G3 - indice sintetico pro-Stato, barre
ind <- fread(file.path(DIR, "output/estrazioni/indice_paesi.csv"))
g3 <- ind[, .(Paese = paese, Indice = round(indice2, 2))][order(-Indice)]
fwrite(g3, file.path(OUT, "g3_indice_prostato.csv"))

# G4 - scatter uguaglianza x merito
pr <- fread(file.path(DIR, "output/estrazioni/giustizia_principi.csv"))
p9 <- pr[tipo_valore == "pct_accordo" & aggregato != "Europa"]
g4 <- dcast(p9[variabile %in% c("sofrdst","sofrwrk")], aggregato ~ variabile, value.var = "valore")
g4 <- g4[!is.na(sofrdst) & !is.na(sofrwrk),
         .(Paese = nomi[aggregato], `Merito` = round(sofrwrk), `Uguaglianza` = round(sofrdst))]
fwrite(g4, file.path(OUT, "g4_uguaglianza_merito.csv"))

# G5 - sospetto sussidi, tre item (arrotondati a intero per il difetto di arrotondamento noto)
sus <- fread(file.path(DIR, "output/estrazioni/sussidi_atteggiamenti.csv"))
s8 <- sus[essround == 8 & tipo_valore == "pct_accordo" & aggregato %in% c("IT","DE","FR","ES","GB","Europa")]
g5 <- dcast(s8[variabile %in% c("bennent","lbenent","sblazy")], aggregato ~ variabile, value.var = "valore")
g5 <- g5[, .(Paese = nomi[aggregato],
             `Molti ottengono sussidi a cui non hanno diritto` = round(bennent),
             `Chi ha redditi bassi riceve meno del dovuto` = round(lbenent),
             `I sussidi rendono le persone pigre` = round(sblazy))][order(-`Molti ottengono sussidi a cui non hanno diritto`)]
fwrite(g5, file.path(OUT, "g5_sospetto_sussidi.csv"))

for (f in list.files(OUT, full.names = TRUE)) { cat("\n====", basename(f), "====\n"); print(fread(f)) }
