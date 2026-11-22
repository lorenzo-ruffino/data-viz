"""
Titolo di godimento dell'abitazione (proprietari / affittuari) in Italia e nei paesi UE.

Fonte: Eurostat, EU-SILC, ilc_lvho02 "Distribution of population by tenure status,
type of household and income group" (quota di PERSONE, non di famiglie).
https://ec.europa.eu/eurostat/databrowser/view/ilc_lvho02/default/table

Nota: Eurostat non pubblica tavole standard del titolo di godimento per età o per grado
di urbanizzazione; per i giovani si usano qui i tipi familiari disponibili (es. adulti soli
sotto i 65 anni) e per l'età della persona di riferimento si rimanda a Banca d'Italia/ISTAT.

Output in ../input/:
- demo_eurostat_titolo_godimento_paesi.csv   ultimo anno, tutti i paesi
- demo_eurostat_titolo_godimento_italia.csv  serie Italia e UE per tipo familiare
"""

import csv
import json
import os
import urllib.request

CARTELLA = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "input")
BASE = "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/ilc_lvho02?format=JSON&lang=en"


def scarica(filtri):
    url = BASE + "".join(f"&{k}={v}" for k, vs in filtri.items() for v in (vs if isinstance(vs, list) else [vs]))
    d = json.loads(urllib.request.urlopen(url, timeout=300).read().decode("utf-8"))
    ids, size = d["id"], d["size"]
    idx = {k: {v: c for c, v in d["dimension"][k]["category"]["index"].items()} for k in ids}
    lab = d["dimension"]["geo"]["category"]["label"]
    out = []
    for k, v in d["value"].items():
        k = int(k)
        coord = {}
        for dim, n in zip(reversed(ids), reversed(size)):
            coord[dim] = idx[dim][k % n]
            k //= n
        coord["geo_label"] = lab[coord["geo"]]
        coord["valore"] = v
        out.append(coord)
    return out


# 1. Tutti i paesi, ultimo anno disponibile, popolazione totale
paesi = scarica({"hhcomp": "TOTAL", "rskpovth": "TOTAL", "unit": "PC",
                 "tenure": ["OWN", "OWN_L", "OWN_NL", "RENT", "RENT_MKT", "RENT_FR"], "lastTimePeriod": 2})
with open(os.path.join(CARTELLA, "demo_eurostat_titolo_godimento_paesi.csv"), "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["geo", "paese", "anno", "titolo", "quota_popolazione_pct"])
    for r in sorted(paesi, key=lambda r: (r["geo"], r["time"], r["tenure"])):
        w.writerow([r["geo"], r["geo_label"], r["time"], r["tenure"], r["valore"]])

# 2. Serie Italia e UE per tipo familiare
serie = scarica({"geo": ["IT", "EU27_2020", "FR", "DE", "ES"], "rskpovth": "TOTAL", "unit": "PC",
                 "tenure": ["OWN", "RENT", "RENT_MKT", "RENT_FR"],
                 "hhcomp": ["TOTAL", "A1", "A1_LT65", "A1_GE65", "A2_2LT65", "DCH", "A2_DCH1", "A_GE3"]})
with open(os.path.join(CARTELLA, "demo_eurostat_titolo_godimento_italia.csv"), "w", newline="") as f:
    w = csv.writer(f)
    # per i tipi familiari diversi da TOTAL Eurostat dà la quota sul totale della popolazione:
    # si ricalcola anche la quota interna al tipo familiare (titolo / (proprietari + affittuari))
    tot_tipo = {}
    for r in serie:
        if r["tenure"] in ("OWN", "RENT"):
            k = (r["geo"], r["time"], r["hhcomp"])
            tot_tipo[k] = tot_tipo.get(k, 0) + r["valore"]
    w.writerow(["geo", "paese", "anno", "tipo_familiare", "titolo", "quota_su_popolazione_totale_pct", "quota_entro_tipo_familiare_pct"])
    for r in sorted(serie, key=lambda r: (r["geo"], r["hhcomp"], r["tenure"], r["time"])):
        t = tot_tipo.get((r["geo"], r["time"], r["hhcomp"]))
        w.writerow([r["geo"], r["geo_label"], r["time"], r["hhcomp"], r["tenure"], r["valore"],
                    round(100 * r["valore"] / t, 1) if t else ""])

print(len(paesi), len(serie))
