library(tidyverse)
library(data.table)
library(janitor)


classifica_coalizione = function(lista) {
    case_when(
        lista %in% c(
            "FRATELLI D'ITALIA CON GIORGIA MELONI",
            "FRATELLI D'ITALIA",
            "LEGA PER SALVINI PREMIER",
            "LEGA SALVINI PREMIER",
            "FORZA ITALIA",
            "NOI MODERATI/LUPI - TOTI - BRUGNARO - UDC",
            "LEGA PER SALVINI PREM.-FORZA IT.-NOI MODERATI-FRAT. D'ITALIA",
            "FORZA ITALIA - NOI MODERATI - PPE"
        ) ~ "CDX",
        lista %in% c(
            "PARTITO DEMOCRATICO - ITALIA DEMOCRATICA E PROGRESSISTA",
            "PARTITO DEMOCRATICO",
            "ALLEANZA VERDI E SINISTRA",
            "MOVIMENTO 5 STELLE",
            "IMPEGNO CIVICO LUIGI DI MAIO - CENTRO DEMOCRATICO"
        ) ~ "CSX",
        lista %in% c(
            "AZIONE - ITALIA VIVA - CALENDA",
            "AZIONE - SIAMO EUROPEI",
            "STATI UNITI D'EUROPA",
            "+EUROPA"
        ) ~ "Centro",
        TRUE ~ "Altro"
    )
}

clean_circ_reg = function(x) {
    x |>
        sub("/.*", "", x = _) |>
        trimws() |>
        sub("\\s+\\d+$", "", x = _)
}

# Normalizza nomi: uppercase, accenti, apostrofi, trattini, bilingui (ALDINO/ALDEIN)
normalize_comune = function(x) {
    x |>
        toupper() |>
        trimws() |>
        stringi::stri_trans_general("Latin-ASCII") |>
        gsub("/.*", "", x = _) |>
        gsub("'", " ", x = _) |>
        gsub("-", " ", x = _) |>
        gsub("\\s+", " ", x = _) |>
        trimws()
}


# Politiche 2022

pol_2022_ita = fread("Precedenti/camera-20220925/Camera_Italia_LivComune.csv") %>%
    clean_names()

pol_2022_ita_affl = pol_2022_ita %>%
    select(circ_reg, comune, elettoritot, votantitot) %>%
    distinct() %>%
    mutate(affluenza = votantitot / elettoritot)

pol_2022_ita_risultati = pol_2022_ita %>%
    mutate(
        coalizione = classifica_coalizione(descrlista),
        regione    = clean_circ_reg(circ_reg)
    ) %>%
    group_by(regione, comune, coalizione) %>%
    summarise(votilista = sum(votilista, na.rm = T), .groups = "drop")

pol_2022_vda = fread("Precedenti/camera-20220925/Camera_VAosta_LivComune.csv") %>%
    clean_names()


# Europee 2024

elettori_2024 = fread("Precedenti/elettori_residenti_estero_15-03-2026.csv") %>%
    clean_names() %>%
    select(comune, paesi_extra_ue_totale) %>%
    mutate(comune = clean_circ_reg(comune))

eur_2024 = fread("Precedenti/europee-20240609/EUROPEE_ITALIA_LivComune.csv",
    encoding = "Latin-1") %>%
    clean_names() %>%
    mutate(desccomune = clean_circ_reg(desccomune)) %>%
    left_join(elettori_2024, by = c("desccomune" = "comune"), relationship = "many-to-many")

eur_2024_affl = eur_2024 %>%
    select(descregione, desccomune, elettori, votanti, paesi_extra_ue_totale) %>%
    distinct() %>%
    mutate(
        elettori_res = elettori - coalesce(paesi_extra_ue_totale, 0),
        affl         = votanti / elettori_res
    )

eur_2024_risultati = eur_2024 %>%
    mutate(coalizione = classifica_coalizione(desclista)) %>%
    group_by(descregione, desccomune, coalizione) %>%
    summarise(votilista = sum(numvoti, na.rm = T), .groups = "drop")


# Lookup fusioni/rinominazioni comunali 2022→2024
# Fonte: Wikipedia "Fusione di comuni italiani"
merger_lookup = tribble(
    ~regione,             ~pol_norm,            ~eur_norm,
    "ABRUZZO",            "POPOLI",             "POPOLI TERME",
    "CALABRIA",           "IONADI",             "JONADI",
    "LOMBARDIA",          "BARDELLO",           "BARDELLO CON MALGESSO E BREGANO",
    "LOMBARDIA",          "MALGESSO",           "BARDELLO CON MALGESSO E BREGANO",
    "LOMBARDIA",          "BREGANO",            "BARDELLO CON MALGESSO E BREGANO",
    "LOMBARDIA",          "ALBAREDO ARNABOLDI", "CAMPOSPINOSO ALBAREDO",
    "LOMBARDIA",          "CAMPOSPINOSO",       "CAMPOSPINOSO ALBAREDO",
    "LOMBARDIA",          "UGGIATE TREVANO",    "UGGIATE CON RONAGO",
    "LOMBARDIA",          "RONAGO",             "UGGIATE CON RONAGO",
    "PIEMONTE",           "CALLIANO",           "CALLIANO MONFERRATO",
    "PIEMONTE",           "GRANA",              "GRANA MONFERRATO",
    "PIEMONTE",           "MONTEMAGNO",         "MONTEMAGNO MONFERRATO",
    "PIEMONTE",           "MORANSENGO",         "MORANSENGO TONENGO",
    "PIEMONTE",           "TONENGO",            "MORANSENGO TONENGO",
    "VENETO",             "CARCERI",            "SANTA CATERINA D ESTE",
    "VENETO",             "VIGHIZZOLO D ESTE",  "SANTA CATERINA D ESTE",
    "VENETO",             "ALANO DI PIAVE",     "SETTEVILLE",
    "VENETO",             "QUERO VAS",          "SETTEVILLE",
    "VENETO",             "GAMBUGLIANO",        "SOVIZZO",
    "TRENTINO ALTO ADIGE","MONTAGNA",           "MONTAGNA SULLA STRADA DEL VINO"
    # Fusioni 2026 NON incluse: avvengono dopo le europee 2024
    # Veneto: Castegnero + Nanto → Castegnero Nanto
    # Abruzzo 2027: Montesilvano + Pescara + Spoltore → Pescara
)

apply_merger = function(df) {
    df %>%
        left_join(merger_lookup, by = c("regione", "comune_norm" = "pol_norm")) %>%
        mutate(comune_norm = coalesce(eur_norm, comune_norm)) %>%
        select(-eur_norm)
}


# Prep pol

pol_affl_prep = pol_2022_ita_affl %>%
    mutate(
        regione     = normalize_comune(clean_circ_reg(circ_reg)),
        comune_norm = normalize_comune(comune)
    ) %>%
    apply_merger() %>%
    group_by(regione, comune_norm) %>%
    summarise(
        pol_elettori = sum(elettoritot, na.rm = T),
        pol_votanti  = sum(votantitot,  na.rm = T),
        .groups = "drop"
    ) %>%
    mutate(pol_affluenza = pol_votanti / pol_elettori)

pol_ris_prep = pol_2022_ita_risultati %>%
    mutate(
        regione     = normalize_comune(regione),
        comune_norm = normalize_comune(comune)
    ) %>%
    apply_merger() %>%
    group_by(regione, comune_norm, coalizione) %>%
    summarise(pol_voti = sum(votilista, na.rm = T), .groups = "drop") %>%
    group_by(regione, comune_norm) %>%
    mutate(pol_perc = pol_voti / sum(pol_voti)) %>%
    ungroup()

# Valle d'Aosta: file separato per candidato, nessun dato affluenza
pol_vda_ris_prep = pol_2022_vda %>%
    clean_names() %>%
    mutate(
        regione     = normalize_comune(regione),
        comune_norm = normalize_comune(comune),
        coalizione  = classifica_coalizione(contrassegno)
    ) %>%
    group_by(regione, comune_norm, coalizione) %>%
    summarise(pol_voti = sum(totvoti, na.rm = T), .groups = "drop") %>%
    group_by(regione, comune_norm) %>%
    mutate(pol_perc = pol_voti / sum(pol_voti)) %>%
    ungroup()

pol_prep = bind_rows(pol_ris_prep, pol_vda_ris_prep) %>%
    left_join(pol_affl_prep, by = c("regione", "comune_norm"))


# Prep eur

eur_affl_prep = eur_2024_affl %>%
    rename(regione = descregione, comune = desccomune) %>%
    mutate(
        regione     = normalize_comune(regione),
        comune_norm = normalize_comune(comune)
    ) %>%
    group_by(regione, comune_norm) %>%
    slice_max(order_by = elettori, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(
        regione, comune_norm,
        eur_elettori          = elettori_res,
        eur_elettori_extra_ue = paesi_extra_ue_totale,
        eur_votanti           = votanti,
        eur_affluenza         = affl
    )

eur_prep = eur_2024_risultati %>%
    rename(regione = descregione, comune = desccomune) %>%
    mutate(
        regione     = normalize_comune(regione),
        comune_norm = normalize_comune(comune)
    ) %>%
    group_by(regione, comune_norm) %>%
    mutate(eur_perc = votilista / sum(votilista)) %>%
    ungroup() %>%
    select(regione, comune_norm, coalizione, eur_voti = votilista, eur_perc) %>%
    left_join(eur_affl_prep, by = c("regione", "comune_norm"))


# Codici comuni (ISTAT, Belfiore, codice elettorale)

# sigla → regione per i soli 6 comuni omonimi
omonimi_regione = c(
    "BG" = "LOMBARDIA",        "LE" = "PUGLIA",
    "TN" = "TRENTINO ALTO ADIGE", "CO" = "LOMBARDIA",
    "PZ" = "BASILICATA",       "CT" = "SICILIA",
    "PU" = "MARCHE",           "TO" = "PIEMONTE",
    "OT" = "SARDEGNA",         "ME" = "SICILIA"
)

codici_comuni = fread("codici_comuni_15-03-2026.csv", sep = ";",
    encoding = "Latin-1", na.strings = character(0)) %>%
    clean_names() %>%
    mutate(
        comune_norm = normalize_comune(descrizione_comune),
        comune_norm = case_when(
            comune_norm == "MOJO ALCANTARA"       ~ "MOIO ALCANTARA",
            comune_norm == "TRIPI ABAKAINON"      ~ "TRIPI",
            comune_norm == "MURISENGO MONFERRATO" ~ "MURISENGO",
            TRUE ~ comune_norm
        ),
        codice_istat = gsub('="(.*)"', "\\1", codice_istat),
        regione      = omonimi_regione[sigla]
    ) %>%
    select(comune_norm, regione, codice_istat, codice_belfiore, codice_elettorale)


# Join finale

pol_eur = full_join(pol_prep, eur_prep, by = c("regione", "comune_norm", "coalizione")) %>%
    left_join(
        codici_comuni %>% filter(!is.na(regione)),
        by = c("regione", "comune_norm")
    ) %>%
    left_join(
        codici_comuni %>% filter(is.na(regione)) %>% select(-regione),
        by = "comune_norm"
    ) %>%
    mutate(
        codice_istat      = coalesce(codice_istat.x,      codice_istat.y),
        codice_belfiore   = coalesce(codice_belfiore.x,   codice_belfiore.y),
        codice_elettorale = coalesce(codice_elettorale.x, codice_elettorale.y)
    ) %>%
    select(-ends_with(".x"), -ends_with(".y"))


pol_eur %>%
    select(comune_norm, pol_elettori, eur_elettori, eur_elettori_extra_ue)%>%
    distinct()%>%
    summarise(pol_elettori = sum(pol_elettori, na.rm = T),
              eur_elettori = sum(eur_elettori, na.rm = T),
              eur_elettori_extra_ue = sum(eur_elettori_extra_ue, na.rm = T))