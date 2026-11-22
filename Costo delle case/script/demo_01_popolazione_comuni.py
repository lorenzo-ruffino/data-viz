"""
Popolazione residente al 1° gennaio 2002-2026 per comune.

Fonti (ISTAT, demo.istat.it):
- 2002-2018: Ricostruzione intercensuaria della popolazione residente 2002-2019
  https://demo.istat.it/app/?i=RIC&l=it  (file PopolazioneEta-Territorio-Comuni.zip)
- 2019-2026: Popolazione residente al 1° gennaio per età e sesso (POSAS)
  https://demo.istat.it/app/?i=POS&l=it  (file POSAS_<anno>_it_Comuni.zip)
  Il dato al 1/1/2026 è una stima provvisoria ISTAT.
- Elenco comuni italiani (capoluoghi, province, ripartizioni)
  https://www.istat.it/storage/codici-unita-amministrative/Elenco-comuni-italiani.csv

Il 1/1/2019 è presente in entrambe le fonti e coincide (totale Italia identico):
si usa la ricostruzione fino al 2018 e POSAS dal 2019.

I comuni fusi o scissi tra la geografia della ricostruzione e quella del 2026
vengono aggregati in gruppi stabili (es. Trapani + Misiliscemi, Pesaro + Monteciccardo),
così che le variazioni non dipendano da cambi di confine (file ISTAT delle
variazioni amministrative dal 1991).

Output in ../input/:
- demo_popolazione_citta_2002_2026.csv      serie per le grandi città (comune, città metropolitana, resto CM)
- demo_capoluoghi_popolazione_2002_2026.csv variazione 2002-2026 dei capoluoghi di provincia
- demo_quota_popolazione_comuni_crescita.csv quota di popolazione nei comuni/province in crescita
- demo_popolazione_comuni_2002_2026.csv     serie per comune (confini 2026, gruppi stabili)
"""

import csv
import io
import os
import re
import shutil
import tempfile
import urllib.request
import zipfile
from collections import defaultdict

CARTELLA = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "input")
TMP = tempfile.mkdtemp()

URL_RIC = "https://demo.istat.it/data/ricostruzione/PopolazioneEta-Territorio-Comuni.zip"
URL_POSAS = "https://demo.istat.it/data/posas/POSAS_{a}_it_Comuni.zip"
URL_ELENCO = "https://www.istat.it/storage/codici-unita-amministrative/Elenco-comuni-italiani.csv"


def scarica(url):
    dest = os.path.join(TMP, os.path.basename(url))
    if not os.path.exists(dest):
        req = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0"})
        with urllib.request.urlopen(req, timeout=600) as r, open(dest, "wb") as f:
            f.write(r.read())
    return dest


# --- 1. Ricostruzione intercensuaria 2002-2019 (tutte le cittadinanze, totale sessi)
pop = {}      # (codice, anno) -> popolazione
nomi = {}
with zipfile.ZipFile(scarica(URL_RIC)) as z:
    for nome_file in z.namelist():
        if not nome_file.endswith(".csv"):
            continue
        anno = citt = sesso = None
        testo = z.read(nome_file).decode("utf-8-sig")
        for riga in testo.splitlines():
            m = re.match(r'^"(Tutte le cittadinanze|Cittadinanza italiana|Cittadinanza straniera) - Anno: (\d{4})', riga)
            if m:
                citt, anno = m.group(1), int(m.group(2))
                continue
            if riga.startswith("Codice comune;Comune;"):
                sesso = riga.split(";")[2]
                continue
            if re.match(r"^\d{6};", riga) and citt == "Tutte le cittadinanze" and sesso == "Totale" and anno <= 2018:
                p = riga.split(";")
                pop[(p[0], anno)] = sum(int(x) for x in p[2:] if x != "")
                nomi[p[0]] = p[1]

# --- 2. POSAS 2019-2026 (riga età 999 = totale)
for anno in range(2019, 2027):
    with zipfile.ZipFile(scarica(URL_POSAS.format(a=anno))) as z:
        f = [n for n in z.namelist() if n.endswith(".csv")][0]
        testo = z.read(f).decode("utf-8-sig")
    for r in csv.reader(io.StringIO(testo), delimiter=";"):
        if len(r) > 3 and r[2] == "999":
            pop[(r[0], anno)] = int(r[-1])
            nomi[r[0]] = r[1]

# --- 3. Gruppi di comuni stabili nel tempo (fusioni, scissioni, cambi di provincia)
# Dal file ISTAT delle variazioni amministrative dal 1991 si uniscono (union-find) i codici
# coinvolti in estinzioni/fusioni (ES, CS, AQES), scorpori che creano nuovi comuni (CECS)
# e cambi di provincia (AP). Le semplici cessioni di territorio (CE/AQ) non creano gruppi.
URL_VAR = "https://www.istat.it/storage/codici-unita-amministrative/Variazioni-amministrative-e-territoriali-dal-1991.zip"
with zipfile.ZipFile(scarica(URL_VAR)) as z:
    f = [n for n in z.namelist() if n.endswith(".csv")][0]
    var = list(csv.reader(io.StringIO(z.read(f).decode("latin1")), delimiter=";"))[1:]
padre = {}
def radice(c):
    padre.setdefault(c, c)
    while padre[c] != c:
        padre[c] = padre[padre[c]]
        c = padre[c]
    return c
for r in var:
    if len(r) > 8 and r[0].isdigit() and int(r[0]) >= 2002 and r[1].strip() in {"ES", "CS", "AQES", "CECS", "AP"}:
        padre[radice(r[4])] = radice(r[8])
codici_2026 = {c for (c, a) in pop if a == 2026}
# il codice del gruppo è quello del comune più popoloso esistente nel 2026
membri = defaultdict(list)
for c in {c for (c, a) in pop}:
    membri[radice(c)].append(c)
gruppo = {}
for r, lista in membri.items():
    vivi = [c for c in lista if c in codici_2026] or lista
    rif = max(vivi, key=lambda c: pop.get((c, 2026), 0))
    for c in lista:
        gruppo[c] = rif

serie = defaultdict(int)   # (codice gruppo, anno) -> popolazione
for (c, anno), v in pop.items():
    serie[(gruppo.get(c, c), anno)] += v

anni = list(range(2002, 2027))
codici = sorted({c for (c, a) in serie})
incompleti = [c for c in codici if any((c, a) not in serie for a in anni)]
if incompleti:
    print("Attenzione, serie incomplete:", [(c, nomi.get(c)) for c in incompleti])

# --- 4. Anagrafica: provincia storica, capoluogo, ripartizione
testo = urllib.request.urlopen(urllib.request.Request(URL_ELENCO, headers={"User-Agent": "Mozilla/5.0"}), timeout=120).read().decode("latin1")
righe = list(csv.reader(io.StringIO(testo), delimiter=";"))[1:]
anag = {r[4]: {"nome": r[6], "prov": r[2], "uts": r[11], "rip": r[9], "regione": r[10], "capoluogo": r[13] == "1"} for r in righe}

def prov_di(c):
    return anag[c]["prov"] if c in anag else c[:3]

totale_italia = {a: sum(serie[(c, a)] for c in codici if (c, a) in serie) for a in anni}

# --- 4b. Serie completa per comune (formato largo), usata dagli altri script
with open(os.path.join(CARTELLA, "demo_popolazione_comuni_2002_2026.csv"), "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["codice", "comune", "codici_confluiti"] + [str(a) for a in anni])
    confluiti = defaultdict(list)
    for c, rif in gruppo.items():
        if c != rif:
            confluiti[rif].append(c)
    for c in codici:
        if c in incompleti:
            continue
        w.writerow([c, anag.get(c, {}).get("nome", nomi.get(c)), "|".join(sorted(confluiti.get(c, [])))] + [serie[(c, a)] for a in anni])

# --- 5. Serie delle grandi città: comune, città metropolitana/provincia, resto
CITTA = {"Milano": "015146", "Torino": "001272", "Roma": "058091", "Bologna": "037006",
         "Firenze": "048017", "Napoli": "063049", "Venezia": "027042", "Padova": "028060",
         "Bari": "072006", "Palermo": "082053", "Genova": "010025"}
with open(os.path.join(CARTELLA, "demo_popolazione_citta_2002_2026.csv"), "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["citta", "ambito", "anno", "popolazione", "fonte"])
    for a in anni:
        fonte = "ISTAT ricostruzione intercensuaria" if a <= 2018 else ("ISTAT POSAS (stima)" if a == 2026 else "ISTAT POSAS")
        w.writerow(["Italia", "Italia", a, totale_italia[a], fonte])
    for nome, cod in CITTA.items():
        p = prov_di(cod)
        for a in anni:
            fonte = "ISTAT ricostruzione intercensuaria" if a <= 2018 else ("ISTAT POSAS (stima)" if a == 2026 else "ISTAT POSAS")
            com = serie[(cod, a)]
            prov = sum(serie[(c, a)] for c in codici if prov_di(c) == p and (c, a) in serie)
            w.writerow([nome, "comune", a, com, fonte])
            w.writerow([nome, "citta metropolitana/provincia", a, prov, fonte])
            w.writerow([nome, "resto della provincia", a, prov - com, fonte])

# --- 6. Capoluoghi di provincia: variazione 2002-2026 e 2019-2026
capoluoghi = [c for c in codici if anag.get(c, {}).get("capoluogo")]
righe_cap = []
for c in capoluoghi:
    p02, p11, p19, p26 = serie[(c, 2002)], serie[(c, 2011)], serie[(c, 2019)], serie[(c, 2026)]
    righe_cap.append([c, anag[c]["nome"], anag[c]["regione"], anag[c]["rip"], p02, p11, p19, p26,
                      p26 - p02, round(100 * (p26 / p02 - 1), 2), round(100 * (p26 / p19 - 1), 2)])
righe_cap.sort(key=lambda r: -r[9])
with open(os.path.join(CARTELLA, "demo_capoluoghi_popolazione_2002_2026.csv"), "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["codice", "comune", "regione", "ripartizione", "pop_2002", "pop_2011", "pop_2019", "pop_2026",
                "var_ass_2002_2026", "var_pct_2002_2026", "var_pct_2019_2026"])
    w.writerows(righe_cap)

# --- 7. Quota di popolazione nei comuni e nelle province che crescono
def quota(unita, a0, a1):
    cresc = [u for u in unita if unita[u][a1] > unita[u][a0]]
    tot1 = sum(unita[u][a1] for u in unita)
    return len(cresc), len(unita), sum(unita[u][a1] for u in cresc) / tot1, sum(unita[u][a0] for u in cresc) / sum(unita[u][a0] for u in unita)

comuni_u = {c: {a: serie[(c, a)] for a in anni} for c in codici if c not in incompleti}
prov_u = defaultdict(lambda: defaultdict(int))
for c in comuni_u:
    for a in anni:
        prov_u[prov_di(c)][a] += comuni_u[c][a]
cap_u = {c: comuni_u[c] for c in capoluoghi}

with open(os.path.join(CARTELLA, "demo_quota_popolazione_comuni_crescita.csv"), "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["unita", "periodo", "n_in_crescita", "n_totale", "quota_pop_fine_periodo_in_crescita", "quota_pop_inizio_periodo_in_crescita"])
    for etichetta, u in [("comuni", comuni_u), ("province storiche", prov_u), ("capoluoghi di provincia", cap_u)]:
        for a0, a1 in [(2002, 2026), (2002, 2014), (2014, 2026), (2019, 2026)]:
            n, tot, q1, q0 = quota(u, a0, a1)
            w.writerow([etichetta, f"{a0}-{a1}", n, tot, round(q1, 4), round(q0, 4)])

print("Italia 2002:", totale_italia[2002], "2026:", totale_italia[2026])

# pulizia dei file scaricati
shutil.rmtree(TMP, ignore_errors=True)
