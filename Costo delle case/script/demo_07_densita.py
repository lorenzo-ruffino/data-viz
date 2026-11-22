"""
Densità abitativa: centro e periferia nelle grandi città italiane e confronto europeo.

Fonti:
- Milano, 88 NIL (Nuclei d'identità locale): Comune di Milano, open data
  ds205 "Territorio: caratteristiche demografiche e territoriali dei quartieri (2011-2024)" (popolazione e area)
  https://dati.comune.milano.it/dataset/ds205-sociale-caratteristiche-demografiche-territoriali-quartiere
  ds964 "NIL vigenti - PGT 2030" (centroidi) https://dati.comune.milano.it/dataset/ds964-nil-vigenti-pgt-2030
  Distanza calcolata dal Duomo (45.4642 N, 9.1900 E). Densità lorda (area totale del NIL, parchi e scali inclusi).
- Roma, 15 municipi: Roma Capitale, Annuario statistico, cap. 1 Territorio, Tab. 1.2 (anagrafe al 31/12/2023)
  https://www.comune.roma.it/web-resources/cms/documents/01_Territorio_Annuario_2024.pdf  (richiede pdftotext)
- Comuni italiani: popolazione ISTAT al 1/1/2026 (../input/demo_popolazione_comuni_2002_2026.csv)
  e superficie ISTAT SITUAS (pfun 74)
- Parigi e Lione: INSEE, populations légales, via https://geo.api.gouv.fr (superficie in ettari)
- Barcellona: Idescat, El municipi en xifres (API https://api.idescat.cat/emex/v1)
- Confronto armonizzato: OCSE, "Population and built-up density - Cities and FUAs"
  (DSD_FUA_TERR@DF_DENSITY), ultimo anno disponibile. "City" è la città Eurostat-OCSE
  (per alcune capitali la "greater city", es. Parigi e Milano), FUA = città + area di pendolarismo.

Output in ../input/:
- demo_densita_milano_nil_2024.csv
- demo_densita_milano_anelli_2024.csv
- demo_densita_roma_municipi_2023.csv
- demo_densita_citta_amministrative.csv
- demo_densita_ocse_citta_fua.csv
"""

import csv
import io
import json
import math
import os
import re
import subprocess
import tempfile
import urllib.request
from datetime import date

CARTELLA = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "input")
UA = {"User-Agent": "Mozilla/5.0"}


def get(url, headers=None):
    req = urllib.request.Request(url, headers={**UA, **(headers or {})})
    return urllib.request.urlopen(req, timeout=300).read()


# --- 1. Milano: densità per NIL e per anelli di distanza dal Duomo
nil = list(csv.DictReader(io.StringIO(get(
    "https://dati.comune.milano.it/dataset/fcddb7d2-b106-48f5-9a2a-64d3136621dc/resource/"
    "084457a7-ec4b-4a6b-b463-d8ab53c64fbb/download/ds205_dati_quartieri_2011_2024_.csv").decode("utf-8-sig")), delimiter=";"))
centroidi = {r["ID_NIL"]: (float(r["LAT_Y_4326_CENTROID"]), float(r["LONG_X_4326_CENTROID"]))
             for r in csv.DictReader(io.StringIO(get(
                 "https://dati.comune.milano.it/dataset/e8e765fc-d882-40b8-95d8-16ff3d39eb7c/resource/"
                 "3fce7202-0076-4a7b-ac2c-d2ab9b5dc658/download/ds964_nil_wm_4326.csv").decode("utf-8-sig")), delimiter=";")}
DUOMO = (45.4642, 9.1900)


def km(a, b):
    # distanza approssimata (equirettangolare), sufficiente alla scala urbana
    x = math.radians(b[1] - a[1]) * math.cos(math.radians((a[0] + b[0]) / 2))
    y = math.radians(b[0] - a[0])
    return 6371 * math.hypot(x, y)


righe_nil = []
for r in nil:
    if r["Anno"] != "2024" or not r["Area (metri quadrati)"] or not r["Totale"]:
        continue  # esclusa la riga "N.D." (residenti non localizzati)
    area = float(r["Area (metri quadrati)"].replace(",", ".")) / 1e6
    pop = int(r["Totale"])
    c = centroidi.get(r["NIL"])
    d = round(km(DUOMO, c), 2) if c else ""
    righe_nil.append([r["NIL"], r["Quartiere"], pop, round(area, 3), round(pop / area) if area else "", d,
                      r["Famiglie registrate in anagrafe"], r["Famiglie unipersonali registrate in anagrafe"]])
with open(os.path.join(CARTELLA, "demo_densita_milano_nil_2024.csv"), "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["id_nil", "nil", "popolazione_2024", "superficie_kmq", "densita_ab_kmq", "distanza_duomo_km",
                "famiglie_anagrafiche", "famiglie_unipersonali"])
    w.writerows(sorted(righe_nil, key=lambda r: -(r[4] or 0)))

anelli = [(0, 2), (2, 4), (4, 6), (6, 8), (8, 99)]
with open(os.path.join(CARTELLA, "demo_densita_milano_anelli_2024.csv"), "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["anello_km_dal_duomo", "n_nil", "popolazione", "superficie_kmq", "densita_ab_kmq"])
    for a, b in anelli:
        sel = [r for r in righe_nil if r[5] != "" and a <= r[5] < b]
        p, s = sum(r[2] for r in sel), sum(r[3] for r in sel)
        w.writerow([f"{a}-{b}" if b < 99 else f"oltre {a}", len(sel), p, round(s, 1), round(p / s)])
    p, s = sum(r[2] for r in righe_nil), sum(r[3] for r in righe_nil)
    w.writerow(["totale comune", len(righe_nil), p, round(s, 1), round(p / s)])

# --- 2. Roma: municipi (Annuario statistico di Roma Capitale, Tab. 1.2)
with tempfile.TemporaryDirectory() as tmp:
    pdf = os.path.join(tmp, "roma.pdf")
    open(pdf, "wb").write(get("https://www.comune.roma.it/web-resources/cms/documents/01_Territorio_Annuario_2024.pdf"))
    testo = subprocess.run(["pdftotext", "-layout", pdf, "-"], capture_output=True, text=True).stdout
# l'ultima occorrenza del titolo è la tavola (la prima è nell'indice)
blocco = testo[testo.rindex("Tab.1.2 - Superficie, popolazione, densità della popolazione per municipio di Roma al"):]
blocco = blocco[blocco.index("Municipio"):blocco.index("Fonte:")]
with open(os.path.join(CARTELLA, "demo_densita_roma_municipi_2023.csv"), "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["municipio", "superficie_kmq", "popolazione_anagrafe_31_12_2023", "densita_ab_kmq"])
    for riga in blocco.splitlines():
        m = re.match(r"\s*([IVX]+|Totale)\b[^\d]*([\d.]+,\d)\s+([\d.]+)\s+([\d.]+,\d)", riga)
        if m:
            w.writerow([m.group(1), m.group(2).replace(".", "").replace(",", "."), m.group(3).replace(".", ""),
                        m.group(4).replace(".", "").replace(",", ".")])

# --- 3. Densità dei comuni amministrativi (Italia ed estero)
oggi = date.today().strftime("%d/%m/%Y")
situas = json.loads(get(f"https://situas-servizi.istat.it/publish/reportspooljson?pfun=74&pdata={oggi}",
                        {"Referer": "https://situas.istat.it/web/"}))["resultset"]
area = {r["PRO_COM_T"]: float(r["AREA_KMQ"]) for r in situas}
pop26 = {}
with open(os.path.join(CARTELLA, "demo_popolazione_comuni_2002_2026.csv")) as f:
    for r in csv.DictReader(f):
        pop26[r["codice"]] = int(r["2026"])
ITALIA = {"Milano": "015146", "Torino": "001272", "Napoli": "063049", "Roma": "058091", "Bologna": "037006",
          "Firenze": "048017", "Genova": "010025", "Palermo": "082053", "Bari": "072006", "Padova": "028060",
          "Venezia": "027042"}
righe = [[n, "Italia", pop26[c], round(area[c], 1), round(pop26[c] / area[c]), "2026 (1/1, stima)",
          "ISTAT POSAS e SITUAS"] for n, c in ITALIA.items()]
for nome, codice in [("Parigi", "75056"), ("Lione", "69123")]:
    d = json.loads(get(f"https://geo.api.gouv.fr/communes/{codice}?fields=nom,population,surface"))
    sup = d["surface"] / 100
    righe.append([nome, "Francia", d["population"], round(sup, 1), round(d["population"] / sup),
                  "popolazione legale più recente", "INSEE via geo.api.gouv.fr"])
emex = json.loads(get("https://api.idescat.cat/emex/v1/dades.json?id=080193&i=f171,f261,f262&lang=es"))
valori = {}
def cerca(o):
    if isinstance(o, dict):
        if "c" in o and "v" in o:
            valori[o["c"]] = (o.get("r"), o["v"].split(",")[0])
        for v in o.values():
            cerca(v)
    elif isinstance(o, list):
        for v in o:
            cerca(v)
cerca(emex)
righe.append(["Barcellona", "Spagna", int(valori["Población"][1]), float(valori["Superficie"][1]),
              round(float(valori["Densidad"][1])), valori["Población"][0], "Idescat, El municipi en xifres"])
with open(os.path.join(CARTELLA, "demo_densita_citta_amministrative.csv"), "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["citta", "paese", "popolazione", "superficie_kmq", "densita_ab_kmq", "anno", "fonte"])
    w.writerows(sorted(righe, key=lambda r: -r[4]))

# --- 4. OCSE: densità armonizzata di città e FUA (ultimo anno)
testo = get("https://sdmx.oecd.org/public/rest/data/OECD.CFE.EDS,DSD_FUA_TERR@DF_DENSITY,/all?lastNObservations=1",
            {"Accept": "application/vnd.sdmx.data+csv; charset=utf-8; labels=both"}).decode("utf-8")
with open(os.path.join(CARTELLA, "demo_densita_ocse_citta_fua.csv"), "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["codice", "nome", "livello", "anno", "densita_ab_kmq"])
    for r in csv.DictReader(io.StringIO(testo)):
        if not r["MEASURE: Measure"].startswith("POP_DEN"):
            continue
        cod, nome = r["REF_AREA: Reference area"].split(": ", 1)
        if cod[:2] not in {"IT", "FR", "ES", "DE", "AT", "BE", "NL", "PT", "EL", "PL", "SE", "DK", "CZ", "UK", "IE", "FI", "HU"}:
            continue
        w.writerow([cod, nome, r["TERRITORIAL_LEVEL: Territorial level"].split(": ")[1], r["TIME_PERIOD: Time period"],
                    r["OBS_VALUE"]])
print("ok")
