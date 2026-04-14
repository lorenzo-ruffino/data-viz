# Installa pacchetti se necessario
if (!require(jsonlite)) install.packages("jsonlite")
if (!require(openxlsx)) install.packages("openxlsx")

library(jsonlite)
library(openxlsx)

# === 1) SCARICA DATI 2026 PER OEVK ===
oevk2026 <- fromJSON("https://vtr.valasztas.hu/ogy2026/data/04121731/napkozi/ReszvetelOevk.json")
df26 <- oevk2026$list
df26$jelido <- as.integer(df26$jelido)
df26$pct <- round(df26$megj / df26$valp * 100, 2)
df26$oevk_id <- paste0(df26$maz, "-", df26$evk)

# Pivot: una riga per OEVK, colonne per ogni ora
library(reshape2)
wide26 <- dcast(df26, oevk_id + maz + evk + valp ~ jelido, value.var = "pct")
ore_map <- c("1" = "h07_2026", "2" = "h09_2026", "3" = "h11_2026", "4" = "h13_2026", "5" = "h15_2026", "6" = "h17_2026")
names(wide26)[5:ncol(wide26)] <- ore_map[names(wide26)[5:ncol(wide26)]]

# === 2) DATI 2022 PER OEVK (dalla pagina HTML ufficiale) ===
library(xml2)
library(rvest)
page <- read_html("https://vtr.valasztas.hu/ogy2022/data/04161400/szavossz/html/reszvetel.html", encoding = "UTF-8")
tbl <- html_table(page, fill = TRUE)
# La tabella di affluenza è la più grande
tab22 <- tbl[[which.max(sapply(tbl, nrow))]]
names(tab22) <- c("nome", "h07_2022", "h09_2022", "h11_2022", "h13_2022", "h15_2022", "h17_2022", "h1830_2022", "h19_2022")

# Rimuovi riga totale nazionale e pulisci
tab22 <- tab22[-1, ]  # prima riga è header o totale
# Rimuovi "%" e converti
for (col in names(tab22)[-1]) {
  tab22[[col]] <- as.numeric(gsub("[^0-9.]", "", tab22[[col]]))
}

# Estrai codice OEVK dal nome (es. "BUDAPEST - 01" -> "01-01")
county_map <- c(
  "BUDAPEST" = "01", "BARANYA" = "02", "B.CS-KISKUN" = "03", "B.K.S" = "04",
  "BORSOD" = "05", "CSONGR" = "06", "FEJ.R" = "07", "GY.R" = "08",
  "HAJD" = "09", "HEVES" = "10", "J.SZ" = "11", "KOM.ROM" = "12",
  "N.GR.D" = "13", "PEST" = "14", "SOMOGY" = "15", "SZABOLCS" = "16",
  "TOLNA" = "17", "VAS" = "18", "VESZPR" = "19", "ZALA" = "20"
)

get_oevk_id <- function(nome) {
  parts <- strsplit(nome, " - ")[[1]]
  county_name <- trimws(parts[1])
  evk_num <- trimws(parts[2])
  # Match county
  for (pattern in names(county_map)) {
    if (grepl(pattern, county_name, ignore.case = TRUE)) {
      return(paste0(county_map[pattern], "-", evk_num))
    }
  }
  return(NA)
}

tab22$oevk_id <- sapply(tab22$nome, get_oevk_id)
tab22 <- tab22[!is.na(tab22$oevk_id), ]

# === 3) RISULTATI 2022 (vincitore OEVK) ===
page_er <- read_html("https://vtr.valasztas.hu/ogy2022/data/04161400/szavossz/html/eredmenyek.html", encoding = "UTF-8")
tbl_er <- html_table(page_er, fill = TRUE)
# Cerca la tabella OEVK vincitori (quella con più righe e colonna "OEVK")
tab_ris <- tbl_er[[which.max(sapply(tbl_er, nrow))]]
names(tab_ris) <- c("oevk_nome", "candidato", "partito", "voti", "pct_voti")
tab_ris$voti <- as.numeric(gsub("[^0-9]", "", tab_ris$voti))
tab_ris$pct_voti <- as.numeric(gsub("[^0-9.]", "", tab_ris$pct_voti))
tab_ris$oevk_id <- sapply(tab_ris$oevk_nome, get_oevk_id)
tab_ris <- tab_ris[!is.na(tab_ris$oevk_id), ]

# === 4) MERGE TUTTO ===
merged <- merge(wide26, tab22[, c("oevk_id", "h07_2022", "h09_2022", "h11_2022", "h13_2022", "h15_2022", "h17_2022", "h1830_2022", "h19_2022")], by = "oevk_id", all.x = TRUE)
merged <- merge(merged, tab_ris[, c("oevk_id", "candidato", "partito", "voti", "pct_voti")], by = "oevk_id", all.x = TRUE)

# Delta e rapporto (solo ore comuni: 07, 09, 11, 13, 15)
merged$delta_07 <- merged$h07_2026 - merged$h07_2022
merged$delta_09 <- merged$h09_2026 - merged$h09_2022
merged$delta_11 <- merged$h11_2026 - merged$h11_2022
merged$delta_13 <- merged$h13_2026 - merged$h13_2022
merged$delta_15 <- merged$h15_2026 - merged$h15_2022
merged$delta_17 <- merged$h17_2026 - merged$h17_2022

merged$ratio_07 <- round(merged$h07_2026 / merged$h07_2022, 3)
merged$ratio_09 <- round(merged$h09_2026 / merged$h09_2022, 3)
merged$ratio_11 <- round(merged$h11_2026 / merged$h11_2022, 3)
merged$ratio_13 <- round(merged$h13_2026 / merged$h13_2022, 3)
merged$ratio_15 <- round(merged$h15_2026 / merged$h15_2022, 3)
merged$ratio_17 <- round(merged$h17_2026 / merged$h17_2022, 3)

# Ordina
merged <- merged[order(merged$oevk_id), ]

# === 5) SALVA XLSX ===
wb <- createWorkbook()

addWorksheet(wb, "Dati completi")
writeDataTable(wb, "Dati completi", merged)

addWorksheet(wb, "Solo confronto")
confronto <- merged[, c("oevk_id", "maz", "evk", "partito",
  "h07_2022", "h07_2026", "delta_07", "ratio_07",
  "h09_2022", "h09_2026", "delta_09", "ratio_09",
  "h11_2022", "h11_2026", "delta_11", "ratio_11",
  "h13_2022", "h13_2026", "delta_13", "ratio_13",
  "h15_2022", "h15_2026", "delta_15", "ratio_15",
  "h17_2022", "h17_2026", "delta_17", "ratio_17")]
writeDataTable(wb, "Solo confronto", confronto)

saveWorkbook(wb, "elezioni_ungheria_oevk_2022_2026.xlsx", overwrite = TRUE)
cat("Salvato: elezioni_ungheria_oevk_2022_2026.xlsx\n")
cat(nrow(merged), "OEVK trovati\n")
