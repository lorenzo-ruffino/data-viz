"""
Popolazione ai censimenti 1951-2011 e al 31 dicembre 2018-2024 (censimento permanente)
per le grandi città: comune capoluogo, città metropolitana (confini attuali) e resto.

Fonte: ISTAT, dataflow SDMX DF_DCSS_POPRES_SERIES_TV_1 "Popolazione residente - Serie storica"
https://esploradati.istat.it/databrowser/#/it/dw/categories/IT1,POP,1.0/POP_POPULATION/DCSS_POP_DEMCITMIG_TV/DF_DCSS_POPRES_SERIES_TV_1
I dati sono ricostruiti da ISTAT ai confini attuali.

Output: ../input/demo_censimenti_citta_1951_2024.csv
"""

import csv
import io
import os
import urllib.request

CARTELLA = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "input")

# comune capoluogo -> provincia/città metropolitana (codici NUTS3 2006 usati dal dataflow; i valori sono ai confini attuali)
CITTA = {
    "Torino": ("001272", "ITC11"), "Milano": ("015146", "ITC45"), "Genova": ("010025", "ITC33"),
    "Venezia": ("027042", "ITD35"), "Padova": ("028060", "ITD36"), "Bologna": ("037006", "ITD55"),
    "Firenze": ("048017", "ITE14"), "Roma": ("058091", "ITE43"), "Napoli": ("063049", "ITF33"),
    "Bari": ("072006", "ITF42"), "Palermo": ("082053", "ITG12"),
}
codici = ["IT"] + [c for coppia in CITTA.values() for c in coppia]
url = ("https://esploradati.istat.it/SDMXWS/rest/data/IT1,DF_DCSS_POPRES_SERIES_TV_1,1.0/A."
       + "+".join(codici) + ".RESPOP_AV.T.TOTAL.ALL.TOTAL.ALL.ALL/")
req = urllib.request.Request(url, headers={"Accept": "application/vnd.sdmx.data+csv;version=1.0.0"})
testo = urllib.request.urlopen(req, timeout=300).read().decode("utf-8")
dati = {}
for r in csv.DictReader(io.StringIO(testo)):
    dati[(r["REF_AREA"], int(r["TIME_PERIOD"]))] = int(float(r["OBS_VALUE"]))
anni = sorted({a for (_, a) in dati})

with open(os.path.join(CARTELLA, "demo_censimenti_citta_1951_2024.csv"), "w", newline="") as f:
    w = csv.writer(f)
    # per i censimenti 1951-2011 il dato è alla data del censimento; dal 2018 al 31 dicembre
    w.writerow(["citta", "anno", "riferimento", "comune", "citta_metropolitana", "resto_citta_metropolitana", "quota_comune_pct"])
    for a in anni:
        rif = "censimento" if a <= 2011 else "31 dicembre (censimento permanente)"
        w.writerow(["Italia", a, rif, dati[("IT", a)], "", "", ""])
    for nome, (com, cm) in CITTA.items():
        for a in anni:
            c, p = dati[(com, a)], dati[(cm, a)]
            w.writerow([nome, a, "censimento" if a <= 2011 else "31 dicembre (censimento permanente)", c, p, p - c, round(100 * c / p, 1)])
print("anni:", anni)
