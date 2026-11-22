"""
Scarica gli ultimi snapshot di Inside Airbnb (insideairbnb.com, licenza CC BY 4.0)
per le città e le aree italiane disponibili: listings dettagliati, listings sintetici
e confini dei quartieri.

I file grezzi sono pesanti e restano fuori dal repo, nella cartella DATI_PRIVATI.
"""
import re
import urllib.parse
import urllib.request
from pathlib import Path

DATI_PRIVATI = Path.home() / "Desktop" / "Costo delle case" / "dati_privati" / "insideairbnb"
DATI_PRIVATI.mkdir(parents=True, exist_ok=True)

PAGINA = "https://insideairbnb.com/get-the-data/"
html = urllib.request.urlopen(PAGINA).read().decode("utf-8")

# Per ogni area italiana prende lo snapshot più recente elencato nella pagina
base = re.findall(r"https://data\.insideairbnb\.com/italy/([^/]+)/([^/]+)/(\d{4}-\d{2}-\d{2})/", html)
ultimi = {}
for regione, citta, data in base:
    chiave = (regione, citta)
    if chiave not in ultimi or data > ultimi[chiave]:
        ultimi[chiave] = data

FILE = ["data/listings.csv.gz", "visualisations/neighbourhoods.geojson"]

for (regione, citta), data in sorted(ultimi.items()):
    for f in FILE:
        url = f"https://data.insideairbnb.com/italy/{regione}/{citta}/{data}/{f}"
        nome = f"{citta}_{data}_{f.split('/')[-1]}"
        dest = DATI_PRIVATI / nome
        if dest.exists():
            continue
        print("scarico", url)
        urllib.request.urlretrieve(urllib.parse.quote(url, safe=":/"), dest)
