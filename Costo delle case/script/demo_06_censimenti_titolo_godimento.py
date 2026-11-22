"""
Abitazioni occupate da persone residenti per titolo di godimento ai censimenti 1951-2001.

Fonte: ISTAT, "L'Italia in 150 anni. Sommario di statistiche storiche 1861-2010",
capitolo 15 "Abitazioni e costruzioni", Tavola 15.2 (fonte originale: censimenti generali della popolazione)
https://www.istat.it/wp-content/uploads/2019/03/cap_15.pdf
Note della tavola: "In proprietà" comprende usufrutto, abitazioni di proprietà ma affittate
(o subaffittate) e a riscatto; "Altro titolo" comprende uso gratuito, enfiteusi, prestazione di servizi.

Per gli anni recenti (stessa unità: abitazioni occupate = famiglie che vi abitano) si aggiungono:
- 2021: ISTAT, censimento permanente, famiglie per titolo di godimento (dataflow SDMX DF_DCSS_HUDW_1_COM),
  vedi script sfratti_01_viminale_istat.py e demo_07_istat_famiglie_abitazioni.py
- 2023: ISTAT, "Abitazioni occupate - Anni 2021-2023", 11 febbraio 2026, Prospetto 2 (percentuali di famiglie)
  https://www.istat.it/wp-content/uploads/2026/02/Abitazioni-occupate-Anni-2021-2023.pdf

Richiede pdftotext (poppler).
Output: ../input/demo_censimenti_titolo_godimento_1951_2023.csv
"""

import csv
import os
import re
import subprocess
import tempfile
import urllib.request

CARTELLA = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "input")
URL = "https://www.istat.it/wp-content/uploads/2019/03/cap_15.pdf"

with tempfile.TemporaryDirectory() as tmp:
    pdf = os.path.join(tmp, "cap_15.pdf")
    req = urllib.request.Request(URL, headers={"User-Agent": "Mozilla/5.0"})
    with urllib.request.urlopen(req, timeout=120) as r, open(pdf, "wb") as f:
        f.write(r.read())
    testo = subprocess.run(["pdftotext", "-layout", pdf, "-"], capture_output=True, text=True).stdout

blocco = testo[testo.index("Tavola 15.2"):testo.index("Tavola 15.3")]
assoluti = blocco[blocco.index("VALORI ASSOLUTI"):blocco.index("COMPOSIZIONI PERCENTUALI")]
righe = []
for riga in assoluti.splitlines():
    m = re.match(r"\s*(19\d\d|2001)\s+(.*)", riga)
    if not m:
        continue
    n = [int(x.replace(".", "")) for x in m.group(2).split()]
    # colonne: proprietà (abitazioni, stanze), affitto (abitazioni, stanze), altro (abitazioni, stanze), totale (abitazioni, stanze)
    prop, aff, alt, tot = n[0], n[2], n[4], n[6]
    assert prop + aff + alt == tot, riga
    righe.append([int(m.group(1)), "censimento (abitazioni occupate da residenti)", prop, aff, alt, tot,
                  round(100 * prop / tot, 1), round(100 * aff / tot, 1), round(100 * alt / tot, 1),
                  "ISTAT, Sommario di statistiche storiche, Tav. 15.2"])

# anni recenti: quote di famiglie per titolo di godimento (censimento permanente)
righe.append([2021, "censimento permanente (famiglie)", "", 5073753, "", 26133640, "", round(100 * 5073753 / 26133640, 1), "",
              "ISTAT, censimento permanente 2021, DF_DCSS_HUDW_1_COM"])
righe.append([2023, "censimento permanente (famiglie)", "", "", "", "", 73.9, 19.6, 6.5,
              "ISTAT, Abitazioni occupate - Anni 2021-2023 (11/2/2026), Prospetto 2"])

with open(os.path.join(CARTELLA, "demo_censimenti_titolo_godimento_1951_2023.csv"), "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["anno", "unita", "proprieta", "affitto", "altro_titolo", "totale",
                "quota_proprieta_pct", "quota_affitto_pct", "quota_altro_pct", "fonte"])
    w.writerows(righe)
for r in righe:
    print(r)
