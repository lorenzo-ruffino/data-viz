"""
Quanta parte delle aree urbane funzionali (FUA) e dei sistemi locali del lavoro (SLL)
sta fuori dal comune centrale, e come sono cresciuti centro e cintura dal 2002.

Fonti:
- ISTAT SITUAS, composizione FUA 2021 (pfun 446) e SLL 2021 (pfun 408); superfici dei comuni (pfun 74)
  https://situas.istat.it/web/  (API: https://situas-servizi.istat.it/publish/reportspooljson?pfun=<n>&pdata=<gg/mm/aaaa>)
  Le FUA sono la definizione armonizzata Eurostat-OCSE (città + area di pendolarismo, censimento 2021);
  i SLL sono la geografia ISTAT dei bacini di pendolarismo casa-lavoro (censimento 2021).
- Popolazione 2002-2026 per comune: ../input/demo_popolazione_comuni_2002_2026.csv (script demo_01)

Output: ../input/demo_fua_sll_centro_cintura.csv
"""

import csv
import json
import os
import urllib.request
from datetime import date

CARTELLA = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "input")
OGGI = date.today().strftime("%d/%m/%Y")


def situas(pfun):
    url = f"https://situas-servizi.istat.it/publish/reportspooljson?pfun={pfun}&pdata={OGGI}"
    req = urllib.request.Request(url, headers={"Referer": "https://situas.istat.it/web/", "User-Agent": "Mozilla/5.0"})
    return json.loads(urllib.request.urlopen(req, timeout=300).read().decode("utf-8"))["resultset"]


fua = situas(446)
sll = situas(408)
area = {r["PRO_COM_T"]: float(r["AREA_KMQ"]) for r in situas(74)}

# serie di popolazione per gruppo di comuni (confini 2026); mappa i codici confluiti sul gruppo
pop, gruppo_di = {}, {}
with open(os.path.join(CARTELLA, "demo_popolazione_comuni_2002_2026.csv")) as f:
    for r in csv.DictReader(f):
        pop[r["codice"]] = {a: int(r[str(a)]) for a in range(2002, 2027)}
        gruppo_di[r["codice"]] = r["codice"]
        for c in filter(None, r["codici_confluiti"].split("|")):
            gruppo_di[c] = r["codice"]

CITTA = {"Roma": "058091", "Milano": "015146", "Napoli": "063049", "Torino": "001272",
         "Palermo": "082053", "Genova": "010025", "Firenze": "048017", "Bari": "072006",
         "Bologna": "037006", "Venezia": "027042", "Padova": "028060"}

righe = []
for nome, core in CITTA.items():
    for geo, elenco, chiave_nome in [("FUA 2021", fua, "DEN_FUA"), ("SLL 2021", sll, "DEN_SLL")]:
        if geo == "FUA 2021":
            membri = [r["PRO_COM_T"] for r in elenco if r["DEN_FUA"].upper() == nome.upper()]
        else:
            cod_sll = [r["COD_SLL"] for r in elenco if r["PRO_COM_T"] == core][0]
            membri = [r["PRO_COM_T"] for r in elenco if r["COD_SLL"] == cod_sll]
        gruppi = sorted({gruppo_di[c] for c in membri if c in gruppo_di})
        mancanti = [c for c in membri if c not in gruppo_di]
        if mancanti:
            print(nome, geo, "comuni senza serie:", mancanti)
        sup_tot = sum(area.get(c, 0) for c in membri)
        sup_core = area[core]
        p = {a: sum(pop[g][a] for g in gruppi) for a in (2002, 2011, 2026)}
        pc = {a: pop[core][a] for a in (2002, 2011, 2026)}
        resto = {a: p[a] - pc[a] for a in p}
        righe.append([
            nome, geo, len(membri), round(sup_tot, 1), round(sup_core, 1),
            p[2026], pc[2026], round(100 * pc[2026] / p[2026], 1), round(100 * resto[2026] / p[2026], 1),
            round(pc[2026] / sup_core), round(resto[2026] / (sup_tot - sup_core)),
            round(100 * (pc[2026] / pc[2002] - 1), 1), round(100 * (resto[2026] / resto[2002] - 1), 1),
            p[2026] - p[2002], pc[2026] - pc[2002], resto[2026] - resto[2002],
        ])

with open(os.path.join(CARTELLA, "demo_fua_sll_centro_cintura.csv"), "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["citta", "geografia", "n_comuni", "superficie_kmq", "superficie_comune_centrale_kmq",
                "pop_2026", "pop_comune_centrale_2026", "quota_comune_centrale_pct", "quota_fuori_comune_pct",
                "densita_comune_centrale_ab_kmq", "densita_resto_ab_kmq",
                "var_pct_comune_centrale_2002_2026", "var_pct_resto_2002_2026",
                "var_ass_totale_2002_2026", "var_ass_comune_centrale_2002_2026", "var_ass_resto_2002_2026"])
    w.writerows(righe)
for r in righe:
    print(r)
