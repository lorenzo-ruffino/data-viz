library(openxlsx)

d <- read.xlsx("elezioni_ungheria_oevk_2022_2026.xlsx", sheet = "Dati completi")

# Tieni solo OEVK con dati completi
d <- d[!is.na(d$h07_2022) & !is.na(d$pct_voti) & !is.na(d$partito), ]

# Normalizza partito: Fidesz vs opposizione unita (DK-JOBBIK-...) vs altro
d$blocco <- ifelse(grepl("FIDESZ", d$partito, ignore.case = TRUE), "Fidesz",
             ifelse(grepl("DK|JOBBIK|MOMENTUM|MSZP", d$partito, ignore.case = TRUE), "Opposizione",
                    "Altro"))

cat("Distribuzione vincitori 2022:\n")
print(table(d$blocco))

# pct_voti = % del vincitore nel suo OEVK nel 2022
# Usiamo come indicatore anche: pct_voti quando Fidesz vince (forza Fidesz)
d$fidesz_strength <- ifelse(d$blocco == "Fidesz", d$pct_voti, 100 - d$pct_voti)

ore <- c("07", "09", "11", "13", "15")

cat("\n=== Correlazioni: variazione affluenza (2026-2022) vs % vincitore 2022 ===\n")
cat(sprintf("%-6s %10s %10s %10s %10s\n", "Ora", "cor_delta", "p_delta", "cor_ratio", "p_ratio"))
for (h in ore) {
  delta_col <- paste0("delta_", h)
  ratio_col <- paste0("ratio_", h)
  c1 <- cor.test(d[[delta_col]], d$pct_voti)
  c2 <- cor.test(d[[ratio_col]], d$pct_voti)
  cat(sprintf("h%s   %10.4f %10.4g %10.4f %10.4g\n", h, c1$estimate, c1$p.value, c2$estimate, c2$p.value))
}

cat("\n=== Correlazioni vs forza Fidesz 2022 (pct Fidesz o 100-pct se vinse opposizione) ===\n")
cat(sprintf("%-6s %10s %10s %10s %10s\n", "Ora", "cor_delta", "p_delta", "cor_ratio", "p_ratio"))
for (h in ore) {
  delta_col <- paste0("delta_", h)
  ratio_col <- paste0("ratio_", h)
  c1 <- cor.test(d[[delta_col]], d$fidesz_strength)
  c2 <- cor.test(d[[ratio_col]], d$fidesz_strength)
  cat(sprintf("h%s   %10.4f %10.4g %10.4f %10.4g\n", h, c1$estimate, c1$p.value, c2$estimate, c2$p.value))
}

cat("\n=== Media variazione per blocco vincitore 2022 ===\n")
for (h in ore) {
  delta_col <- paste0("delta_", h)
  agg <- aggregate(d[[delta_col]], list(blocco = d$blocco), mean, na.rm = TRUE)
  cat(sprintf("h%s: ", h))
  for (i in seq_len(nrow(agg))) cat(sprintf("%s=%.2f  ", agg$blocco[i], agg$x[i]))
  cat("\n")
}

# t-test Fidesz vs Opposizione su delta h15
cat("\n=== t-test: delta_15 Fidesz vs Opposizione ===\n")
print(t.test(delta_15 ~ blocco, data = d[d$blocco %in% c("Fidesz", "Opposizione"), ]))
