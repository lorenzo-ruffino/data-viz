library(openxlsx)

d <- read.xlsx("elezioni_ungheria_oevk_2022_2026.xlsx", sheet = "Dati completi")
d <- d[!is.na(d$h07_2022) & !is.na(d$pct_voti) & !is.na(d$partito), ]

d$blocco <- ifelse(grepl("FIDESZ", d$partito, ignore.case = TRUE), "Fidesz", "Opposizione")
d$fidesz_pct_2022 <- ifelse(d$blocco == "Fidesz", d$pct_voti, 100 - d$pct_voti)
d$vincitore_pct_2022 <- d$pct_voti

# === FORMATO WIDE: una riga per OEVK, tutte le variabili come colonne ===
wide <- d[, c("oevk_id", "vincitore_pct_2022", "fidesz_pct_2022",
              "h07_2022","h09_2022","h11_2022","h13_2022","h15_2022",
              "h07_2026","h09_2026","h11_2026","h13_2026","h15_2026",
              "delta_07","delta_09","delta_11","delta_13","delta_15",
              "ratio_07","ratio_09","ratio_11","ratio_13","ratio_15")]

cat("Dimensioni dataset wide:", nrow(wide), "righe ×", ncol(wide), "colonne\n\n")

# === MATRICE DI CORRELAZIONE COMPLETA ===
num <- wide[, -1]
M <- cor(num, use = "pairwise.complete.obs")

cat("=== Correlazioni con % vincitore 2022 ===\n")
print(round(M[, "vincitore_pct_2022", drop = FALSE], 3))

cat("\n=== Correlazioni con % Fidesz 2022 ===\n")
print(round(M[, "fidesz_pct_2022", drop = FALSE], 3))

# === Tabella riassuntiva wide: una riga, colonne per ora ===
ore <- c("07","09","11","13","15")
tab <- data.frame(
  ora = ore,
  cor_delta_vs_vincitore2022  = sapply(ore, function(h) cor(wide[[paste0("delta_",h)]], wide$vincitore_pct_2022)),
  p_delta_vs_vincitore2022    = sapply(ore, function(h) cor.test(wide[[paste0("delta_",h)]], wide$vincitore_pct_2022)$p.value),
  cor_ratio_vs_vincitore2022  = sapply(ore, function(h) cor(wide[[paste0("ratio_",h)]], wide$vincitore_pct_2022)),
  p_ratio_vs_vincitore2022    = sapply(ore, function(h) cor.test(wide[[paste0("ratio_",h)]], wide$vincitore_pct_2022)$p.value),
  cor_delta_vs_fidesz2022     = sapply(ore, function(h) cor(wide[[paste0("delta_",h)]], wide$fidesz_pct_2022)),
  p_delta_vs_fidesz2022       = sapply(ore, function(h) cor.test(wide[[paste0("delta_",h)]], wide$fidesz_pct_2022)$p.value),
  cor_ratio_vs_fidesz2022     = sapply(ore, function(h) cor(wide[[paste0("ratio_",h)]], wide$fidesz_pct_2022)),
  p_ratio_vs_fidesz2022       = sapply(ore, function(h) cor.test(wide[[paste0("ratio_",h)]], wide$fidesz_pct_2022)$p.value)
)
tab[,-1] <- round(tab[,-1], 4)

cat("\n=== Tabella correlazioni (formato wide per ora) ===\n")
print(tab, row.names = FALSE)

# === Salva wide + matrice ===
wb <- createWorkbook()
addWorksheet(wb, "Wide")
writeDataTable(wb, "Wide", wide)
addWorksheet(wb, "Matrice correlazione")
writeData(wb, "Matrice correlazione", round(M, 3), rowNames = TRUE)
addWorksheet(wb, "Tabella correlazioni")
writeDataTable(wb, "Tabella correlazioni", tab)
saveWorkbook(wb, "correlazioni_wide.xlsx", overwrite = TRUE)
cat("\nSalvato: correlazioni_wide.xlsx\n")
