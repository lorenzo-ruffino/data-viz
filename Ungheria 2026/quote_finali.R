library(openxlsx)

d <- read.xlsx("elezioni_ungheria_oevk_2022_2026.xlsx", sheet = "Dati completi")
d <- d[!is.na(d$h17_2022) & !is.na(d$h17_2026) & !is.na(d$pct_voti) & !is.na(d$partito), ]
d$fidesz_pct_2022 <- ifelse(grepl("FIDESZ", d$partito, ignore.case = TRUE),
                            d$pct_voti, 100 - d$pct_voti)

# Quota di affluenza raggiunta a ogni ora rispetto al "finale disponibile"
# 2022: finale = h19_2022 (chiusura reale)
# 2026: finale = h17_2026 (ultimo dato disponibile)
ore <- c("07","09","11","13","15","17")
for (h in ore) {
  d[[paste0("q_",h,"_2022")]] <- d[[paste0("h",h,"_2022")]] / d$h19_2022
  d[[paste0("q_",h,"_2026")]] <- d[[paste0("h",h,"_2026")]] / d$h17_2026
  d[[paste0("dq_",h)]]        <- d[[paste0("q_",h,"_2026")]] - d[[paste0("q_",h,"_2022")]]
}
# Per confronto pulito: usa 2022 anche con denominatore h17 (stesso riferimento)
for (h in ore) {
  d[[paste0("q17_",h,"_2022")]] <- d[[paste0("h",h,"_2022")]] / d$h17_2022
  d[[paste0("dq17_",h)]] <- d[[paste0("q_",h,"_2026")]] - d[[paste0("q17_",h,"_2022")]]
}

cat("=== Quota media di affluenza raggiunta (share del riferimento) ===\n")
cat(sprintf("%-4s %12s %12s %12s\n", "Ora", "q_2022(/h19)", "q_2022(/h17)", "q_2026(/h17)"))
for (h in ore) {
  cat(sprintf("h%s  %12.3f %12.3f %12.3f\n", h,
      mean(d[[paste0("q_",h,"_2022")]]),
      mean(d[[paste0("q17_",h,"_2022")]]),
      mean(d[[paste0("q_",h,"_2026")]])))
}

cat("\n=== Correlazione: quota raggiunta a ogni ora vs forza Fidesz 2022 ===\n")
cat("(denominatore comune h17 in entrambi gli anni — controlla per il 'quanto' totale)\n\n")
cat(sprintf("%-4s %12s %12s %12s %10s %10s\n",
            "Ora","cor_q2022","cor_q2026","cor_dq","p_dq","n"))
for (h in ore) {
  q22 <- d[[paste0("q17_",h,"_2022")]]
  q26 <- d[[paste0("q_",h,"_2026")]]
  dq  <- d[[paste0("dq17_",h)]]
  c22 <- cor(q22, d$fidesz_pct_2022)
  c26 <- cor(q26, d$fidesz_pct_2022)
  ct  <- cor.test(dq, d$fidesz_pct_2022)
  cat(sprintf("h%s  %12.4f %12.4f %12.4f %10.4f %10d\n",
              h, c22, c26, ct$estimate, ct$p.value, nrow(d)))
}

cat("\nInterpretazione: se cor_dq < 0 a ora X, significa che nelle aree Fidesz\n")
cat("la quota di voto arrivata entro X è CALATA dal 2022 al 2026 (voto posticipato).\n")
cat("Se cor_dq > 0, nelle aree Fidesz il voto è stato ANTICIPATO.\n")
