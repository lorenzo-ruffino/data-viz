library(dplyr)
library(readr)

dat <- read_csv("/Users/lorenzoruffino/Documents/Progetti/data-viz/Variazione PIL reale 2000-2025/input/pil_reale_2000_2025.csv")

nomi_it <- c(
  AT = "Austria", BE = "Belgio", BG = "Bulgaria", CY = "Cipro", CZ = "Cechia",
  DE = "Germania", DK = "Danimarca", EE = "Estonia", EL = "Grecia", ES = "Spagna",
  FI = "Finlandia", FR = "Francia", HR = "Croazia", HU = "Ungheria", IE = "Irlanda",
  IT = "Italia", LT = "Lituania", LU = "Lussemburgo", LV = "Lettonia", MT = "Malta",
  NL = "Paesi Bassi", PL = "Polonia", PT = "Portogallo", RO = "Romania",
  SE = "Svezia", SI = "Slovenia", SK = "Slovacchia",
  CH = "Svizzera", IS = "Islanda", NO = "Norvegia", LI = "Liechtenstein",
  UK = "Regno Unito", TR = "Turchia", RS = "Serbia", MK = "Macedonia del Nord",
  ME = "Montenegro", AL = "Albania", BA = "Bosnia ed Erzegovina", XK = "Kosovo",
  MD = "Moldavia", UA = "Ucraina",
  EU27_2020 = "Unione europea (27)",
  EA = "Area euro", EA12 = "Area euro (12)", EA19 = "Area euro (19)",
  EA20 = "Area euro (20)", EA21 = "Area euro (21)"
)

out <- dat %>%
  mutate(paese = nomi_it[geo]) %>%
  select(
    codice_geo = geo,
    paese,
    indice_2000 = y2000,
    indice_2025 = y2025,
    variazione_pct = var_pct
  ) %>%
  arrange(desc(variazione_pct))

write_csv(out, "/Users/lorenzoruffino/Documents/Progetti/data-viz/Variazione PIL reale 2000-2025/output/pil_reale_2000_2025.csv")

cat("Salvato:", nrow(out), "righe\n")
print(out, n = Inf)
