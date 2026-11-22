"""
Costo delle case - Tabelle trascritte da rapporti e pubblicazioni.

Numeri che non esistono come dataset scaricabile e sono stati trascritti a
mano dai PDF o dalle pagine indicate nella colonna "fonte" (verificati sulla
fonte al 1° ottobre 2026). Lo script si limita a scriverli in CSV in input/.
"""

import csv
from pathlib import Path

OUT = Path("/Users/lorenzoruffino/Documents/Progetti/data-viz/Costo delle case/input")

PENDOLARIA_2024 = ("Legambiente, Rapporto Pendolaria (dic. 2024), pp. 50-52, "
                   "elaborazione su urbanrail.net - https://www.pendolaria.it/wp-content/"
                   "uploads/2024/12/Report_Pendolaria_2025_digital-1.pdf")
PENDOLARIA_2025 = ("Legambiente, Rapporto Pendolaria 20a ed. (12/12/2025), p. 16 - "
                   "https://www.legambiente.it/wp-content/uploads/2025/12/"
                   "Rapporto-Pendolaria-20esima-edizione-1-2.pdf")
PENDOLARIA_URB = ("Legambiente, Pendolaria 2024 - Speciale aree urbane (mar. 2024) - "
                  "https://www.pendolaria.it/wp-content/uploads/2024/03/"
                  "Pendolaria-2024-speciale-aree-urbane.pdf")


def scrivi(nome, intestazione, righe):
    with open(OUT / nome, "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(intestazione)
        w.writerows(righe)


# 1. Reti su ferro urbane e suburbane per paese (km)
scrivi(
    "trasporti_reti_ferro_paesi.csv",
    ["paese", "anno_dati", "km_metro", "km_tram", "km_ferrovie_suburbane", "fonte"],
    [
        ["Regno Unito", 2024, 680.4, 256.6, 1817.3, PENDOLARIA_2024],
        ["Germania", 2024, 657.2, 2044.5, 2041.3, PENDOLARIA_2024],
        ["Spagna", 2024, 615.6, 302.8, 1442.7, PENDOLARIA_2024],
        ["Francia", 2024, 405.2, 878.2, 698.4, PENDOLARIA_2024],
        ["Italia", 2024, 269.8, 397.4, 721.9, PENDOLARIA_2024],
        ["Italia", 2025, 271.7, 404.4, 721.9, PENDOLARIA_2025],
        ["Spagna", 2025, 620.5, None, 1442.7, PENDOLARIA_2025],
        ["Francia", 2025, None, 889.5, None, PENDOLARIA_2025],
    ],
)

# 2. Metropolitane nelle città (km, stazioni). Le basi di popolazione per i
#    km per abitante di Legambiente non sono omogenee: non le riporto.
scrivi(
    "trasporti_metro_citta_europee.csv",
    ["citta", "anno_dati", "km_metro", "stazioni", "nota", "fonte"],
    [
        ["Londra", 2023, 470.6, 387, "include reti oltre la Underground (402 km TfL)", PENDOLARIA_URB],
        ["Madrid", 2023, 291.3, 301, "CRTM 2024 conta 269,5 km escludendo le concessioni", PENDOLARIA_URB],
        ["Parigi", 2023, 225.2, 309, "solo metro, RER esclusa", PENDOLARIA_URB],
        ["Berlino", 2023, 156.1, 175, "U-Bahn", PENDOLARIA_URB],
        ["Barcellona", 2023, 148.6, 185, "TMB + FGC", PENDOLARIA_URB],
        ["Milano", 2023, 103.8, 121, "prima del completamento della M4", PENDOLARIA_URB],
        ["Monaco di Baviera", 2023, 94.7, 96, "U-Bahn", PENDOLARIA_URB],
        ["Roma", 2023, 60.6, 73, "", PENDOLARIA_URB],
        ["Napoli", 2023, 47.0, 37, "", PENDOLARIA_URB],
        ["Torino", 2023, 14.9, 23, "", PENDOLARIA_URB],
        ["Milano", 2024, 111.8, None, "M4 completa", PENDOLARIA_2024],
        ["Roma", 2024, 60.0, None, "esclusa la tratta Colosseo aperta il 16/12/2025", PENDOLARIA_2024],
        ["Napoli", 2024, 53.4, None, "", PENDOLARIA_2024],
        ["Torino", 2024, 15.1, None, "", PENDOLARIA_2024],
        ["Madrid", 2024, 293.0, None, "", PENDOLARIA_2024],
    ],
)

# 3. Congestione stradale 2025
TOMTOM = "TomTom Traffic Index 2025 - https://www.tomtom.com/traffic-index/"
INRIX = ("INRIX 2025 Global Traffic Scorecard - https://www.sipotra.it/wp-content/"
         "uploads/2026/04/2025-INRIX-Global-Traffic-Scorecard.pdf")
scrivi(
    "trasporti_congestione_citta_2025.csv",
    ["citta", "tomtom_ore_perse_anno_ora_punta", "tomtom_livello_congestione_perc",
     "inrix_ore_perse_per_guidatore", "fonte"],
    [
        ["Milano", 136, 49.4, 67, f"{TOMTOM}; {INRIX}"],
        ["Roma", 115, 45.2, 76, f"{TOMTOM}; {INRIX}"],
        ["Torino", 111, 37.6, None, TOMTOM],
        ["Napoli", 90, 44.5, None, TOMTOM],
        ["Bologna", 74, 39.9, None, TOMTOM],
        ["Londra", 141, 51.6, 91, f"{TOMTOM}; {INRIX}"],
        ["Parigi", 109, 40.0, 90, f"{TOMTOM}; {INRIX}"],
        ["Berlino", 100, 44.6, 60, f"{TOMTOM}; {INRIX}"],
        ["Madrid", 98, 38.0, None, TOMTOM],
    ],
)

# 4. Lavoratori da remoto, Osservatorio Smart Working Politecnico di Milano
POLIMI = ("Osservatorio Smart Working, Politecnico di Milano, comunicato ott. 2025 - "
          "https://www.osservatori.net/comunicato/smart-working/smart-working-italia-numeri-trend/")
POLIMI_SEC = "valori 2024 da stampa (ItaliaOggi, BusinessCommunity) coerenti con le variazioni del comunicato 2025"
scrivi(
    "trasporti_smart_working_polimi.csv",
    ["comparto", "lavoratori_2024", "lavoratori_2025", "var_perc", "nota", "fonte"],
    [
        ["Grandi imprese", 1910000, 1945000, 1.8, "53% del personale", POLIMI],
        ["PMI", 520000, 480000, -7.7, "", POLIMI],
        ["Microimprese", 625000, 595000, -4.8, "", POLIMI],
        ["Pubblica amministrazione", 500000, 555000, 11.0, "17% dei dipendenti", POLIMI],
        ["Totale", 3555000, 3575000, 0.6, POLIMI_SEC, POLIMI],
    ],
)

# 5. Ripartizione modale degli spostamenti degli italiani (Isfort Audimob)
ISFORT = ("Isfort, 22° Rapporto sulla mobilità degli italiani (nov. 2025) - "
          "https://www.isfort.it/wp-content/uploads/2025/11/22-RapportoMobilita_Sintesi.pdf")
scrivi(
    "trasporti_ripartizione_modale_audimob.csv",
    ["periodo", "auto", "mezzi_pubblici", "bici_micromobilita", "piedi", "moto", "fonte"],
    [
        ["2019", 62.5, 10.8, None, None, None, ISFORT],
        ["2024", 61.1, None, None, None, None, ISFORT],
        ["2025 (1° semestre)", 60.8, 8.9, 5.2, 20.6, 4.5, ISFORT],
    ],
)

# 6. Infrastrutture ciclabili nei capoluoghi (Legambiente, dati 2024)
ECO = ("Legambiente, Ecosistema Urbano 2025 (ott. 2025), indicatore infrastrutture "
       "per la ciclabilità - https://www.legambiente.it/wp-content/uploads/2025/10/"
       "Ecosistema-Urbano_2025.pdf")
scrivi(
    "trasporti_ciclabilita_ecosistema_urbano_2025.csv",
    ["comune", "metri_equivalenti_per_100_ab", "fonte"],
    [[c, v, ECO] for c, v in [
        ("Reggio Emilia", 44.33), ("Cosenza", 37.35), ("Cremona", 35.54),
        ("Modena", 23.54), ("Bologna", 15.64), ("Firenze", 11.48),
        ("Media capoluoghi", 10.39), ("Venezia", 10.11), ("Torino", 8.48),
        ("Milano", 5.64), ("Bari", 4.95), ("Palermo", 3.00), ("Roma", 2.52),
        ("Genova", 1.11), ("Napoli", 0.38),
    ]],
)

# 7. Sharing mobility in Italia
SHARING = ("Osservatorio Nazionale Sharing Mobility, 9° Rapporto (ott. 2025) - "
           "https://osservatoriosharingmobility.it/wp-content/uploads/2025/10/"
           "9-Rapporto-sharing-mobility_2025.pdf")
scrivi(
    "trasporti_sharing_mobility_italia.csv",
    ["indicatore", "valore", "unita", "anno", "fonte"],
    [
        ["noleggi totali (car, bike, scooter, monopattini)", 50, "milioni", "2024", SHARING],
        ["noleggi totali, previsione", 60, "milioni", "2025", SHARING],
        ["quota noleggi monopattini", 50, "%", "2024", SHARING],
        ["quota noleggi bike sharing", 32, "%", "2024", SHARING],
        ["quota noleggi car sharing", 12, "%", "2024", SHARING],
        ["quota noleggi scooter sharing", 7, "%", "2024", SHARING],
        ["noleggi a Roma", 13.2, "milioni", "2024", SHARING],
        ["noleggi a Milano", 12.6, "milioni", "2024", SHARING],
        ["veicoli in sharing", 95.6, "migliaia", "2024", SHARING],
        ["veicoli in sharing, variazione sul picco 2022", -15, "%", "2024", SHARING],
        ["servizi di sharing attivi", 170, "numero", "2024", SHARING],
        ["percorrenza media noleggio bici o monopattino", 2, "km", "2024", SHARING],
    ],
)

# 8. Auto per 1.000 abitanti nelle città: perimetri diversi, vedi nota
UA = "Eurostat, Urban Audit urb_ctran, indicatore TT1057I"
ISTAT_AU = ("ISTAT, Dati ambientali nelle città 2024 (dataflow 609_1_DF_DCCV_URBANENV_12, "
            "CPASCAR1000) - https://www.istat.it/comunicato-stampa/ambiente-urbano-anno-2024/")
scrivi(
    "trasporti_motorizzazione_citta.csv",
    ["citta", "auto_per_1000_ab", "anno", "perimetro", "fonte"],
    [
        ["Catania", 824, 2024, "comune", ISTAT_AU],
        ["Roma", 646, 2024, "comune", ISTAT_AU],
        ["Torino", 650, 2024, "comune", ISTAT_AU],
        ["Napoli", 629, 2024, "comune", ISTAT_AU],
        ["Bologna", 557, 2024, "comune", ISTAT_AU],
        ["Milano", 519, 2024, "comune", ISTAT_AU],
        ["Genova", 489, 2024, "comune", ISTAT_AU],
        ["Media capoluoghi metropolitani", 612, 2024, "comuni capoluogo", ISTAT_AU],
        ["Madrid", 397, 2024, "greater city", UA],
        ["Parigi", 352, 2023, "greater city", UA],
        ["Monaco di Baviera", 346, 2023, "città", UA],
        ["Barcellona", 336, 2023, "greater city (2024: 279, serie discontinua)", UA],
        ["Berlino", 283, 2023, "città", UA],
        ["Berlino", 275, 2025, "città, solo auto private (334 con aziendali)",
         "Amt für Statistik Berlin-Brandenburg via Berliner Morgenpost 2026 - "
         "https://www.morgenpost.de/berlin/article412306798/"],
        ["Londra", 283, 2018, "città (Greater London)", UA],
    ],
)
