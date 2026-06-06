#!/usr/bin/env python3
# Sezione 12 - Energia: identifica i settori energivori e verifica se hanno
# avuto cali di produzione maggiori, in Italia e negli altri Paesi.
import csv, statistics
from collections import defaultdict

INP = "../input"
def load(f): return list(csv.DictReader(open(f"{INP}/{f}")))
def num(x):
    try: return float(x)
    except: return None
def chg(a,b): return 100*(a/b-1) if b else None

NAMES = {"C10":"Alimentari","C11":"Bevande","C13":"Tessile","C14":"Abbigliamento",
 "C15":"Pelle e calzature","C16":"Legno","C17":"Carta","C18":"Stampa",
 "C19":"Raffinazione petrolio","C20":"Chimica","C21":"Farmaceutica",
 "C22":"Gomma e plastica","C23":"Minerali non metalliferi","C24":"Metallurgia",
 "C25":"Prodotti in metallo","C26":"Elettronica e ottica","C27":"App. elettriche",
 "C28":"Macchinari","C29":"Autoveicoli","C30":"Altri mezzi trasporto",
 "C31":"Mobili","C32":"Altre manifatturiere","C33":"Riparaz./installaz."}
EII = ["C17","C19","C20","C23","C24"]  # energivori: carta, raffinazione, chimica,
                                       # minerali non metalliferi, metallurgia

print("="*70)
print("A. IDENTIFICAZIONE DEI SETTORI ENERGIVORI (Italia)")
print("="*70)
# uso di energia per settore (TJ)
EN = defaultdict(dict)
for r in load("eurostat_energia_per_settore_italia.csv"):
    v = num(r["energia_tj"])
    if v is not None: EN[r["nace_r2"]][int(float(r["anno"]))] = v
# valore aggiunto a64 (prezzi correnti) per divisione
VA = defaultdict(dict)
for r in load("eurostat_valore_aggiunto_a64_italia.csv"):
    if r["unit"]=="CP_MEUR":
        v = num(r["valore"])
        if v is not None: VA[r["nace_r2"]][int(float(r["anno"]))] = v
Y = 2022  # anno per la classificazione (energia + VA disponibili)
print(f"Intensità energetica per settore manifatturiero, {Y}")
print(f"(terajoule di energia usata per milione di euro di valore aggiunto):")
rows = []
for c in NAMES:
    e = EN.get(c,{}).get(Y); va = VA.get(c,{}).get(Y)
    if e and va: rows.append((c, NAMES[c], e/va, e, va))
rows.sort(key=lambda x:-x[2])
for c,nm,inten,e,va in rows:
    flag = "  <-- ENERGIVORO" if c in EII else ""
    print(f"  {nm:26s} {inten:7.2f} TJ/mln€{flag}")
# peso degli energivori
etot = EN["C"][Y]
e_eii = sum(EN[c][Y] for c in EII if Y in EN.get(c,{}))
va_man = VA.get("C",{}).get(Y) or sum(VA[c][Y] for c in NAMES if Y in VA.get(c,{}))
va_eii = sum(VA[c][Y] for c in EII if Y in VA.get(c,{}))
print(f"\nI 5 settori energivori (carta, raffinazione, chimica, minerali non")
print(f"metalliferi, metallurgia) nel {Y}:")
print(f"  usano il {100*e_eii/etot:.0f}% di tutta l'energia della manifattura")
print(f"  ma producono solo il {100*va_eii/va_man:.0f}% del valore aggiunto manifatturiero")

print()
print("="*70)
print("B. I SETTORI ENERGIVORI HANNO AVUTO CALI MAGGIORI? - ITALIA")
print("="*70)
# indice produzione per divisione e Paese
IP = defaultdict(dict)  # (geo,nace) -> {anno: indice}
for r in load("eurostat_produzione_settore_paesi.csv"):
    v = num(r["indice"])
    if v is not None: IP[(r["geo"],r["nace_r2"])][int(float(r["anno"]))] = v

def var(geo, nace, y0, y1):
    d = IP.get((geo,nace),{})
    return chg(d[y1], d[y0]) if y0 in d and y1 in d else None

print("Variazione della produzione dei settori energivori - Italia:")
print(f"  {'settore':26s}{'2019->2024':>13}{'2021->2024':>13}")
for c in EII:
    v1 = var("IT",c,2019,2024); v2 = var("IT",c,2021,2024)
    print(f"  {NAMES[c]:26s}{v1:+12.1f}%{v2:+12.1f}%")
allc = [c for c in NAMES if ("IT",c) in IP]
non = [c for c in allc if c not in EII]
def grpmean(geo, codes, y0, y1):
    vs = [var(geo,c,y0,y1) for c in codes]
    vs = [v for v in vs if v is not None]
    return statistics.mean(vs) if vs else None
print(f"\n  {'GRUPPO ENERGIVORI (media)':26s}"
      f"{grpmean('IT',EII,2019,2024):+12.1f}%{grpmean('IT',EII,2021,2024):+12.1f}%")
print(f"  {'GRUPPO NON ENERGIVORI (media)':26s}"
      f"{grpmean('IT',non,2019,2024):+12.1f}%{grpmean('IT',non,2021,2024):+12.1f}%")
print(f"  {'Totale manifattura':26s}"
      f"{var('IT','C',2019,2024):+12.1f}%{var('IT','C',2021,2024):+12.1f}%")

print()
print("="*70)
print("C. E' UN PROBLEMA ITALIANO O EUROPEO? (confronto fra Paesi)")
print("="*70)
geos = ["IT","DE","FR","ES","EU27_2020"]
print("Variazione della produzione dei settori ENERGIVORI 2021->2024:")
print(f"  {'Paese':12}{'energivori':>13}{'non energiv.':>14}{'divario':>10}")
for g in geos:
    e = grpmean(g,EII,2021,2024); n = grpmean(g,non,2021,2024)
    if e is not None and n is not None:
        print(f"  {g:12}{e:+12.1f}%{n:+13.1f}%{e-n:+9.1f}pp")
print("\nVariazione 2019->2024 (orizzonte piu' lungo, include pre-shock):")
print(f"  {'Paese':12}{'energivori':>13}{'non energiv.':>14}")
for g in geos:
    e = grpmean(g,EII,2019,2024); n = grpmean(g,non,2019,2024)
    if e is not None and n is not None:
        print(f"  {g:12}{e:+12.1f}%{n:+13.1f}%")
print("\nDettaglio per i 5 settori energivori, var. 2021->2024 per Paese:")
print(f"  {'settore':26}" + "".join(f"{g:>9}" for g in geos))
for c in EII:
    line = f"  {NAMES[c]:26}"
    for g in geos:
        v = var(g,c,2021,2024)
        line += f"{v:+8.0f}%" if v is not None else f"{'-':>9}"
    print(line)

print()
print("="*70)
print("D. I PREZZI DELL'ENERGIA PER L'INDUSTRIA")
print("="*70)
def price(fn, lab):
    P = defaultdict(dict)
    for r in load(fn):
        if r["tassazione"]=="X_TAX":
            v = num(r["prezzo_eur_kwh"])
            if v is not None:
                y = float(r["anno"])
                P[r["geo"]].setdefault(int(y), []).append(v)
    print(f"\n{lab} - prezzo per l'industria, al netto delle imposte (€/kWh, media annua):")
    print(f"  {'anno':>6}" + "".join(f"{g:>10}" for g in ["IT","DE","FR","ES"]))
    for y in [2019,2021,2022,2023,2024]:
        line = f"  {y:>6}"
        for g in ["IT","DE","FR","ES"]:
            vs = P.get(g,{}).get(y)
            line += f"{statistics.mean(vs):9.4f}" if vs else f"{'-':>10}"
        print(line)
    # divario Italia vs Germania
    for y in [2019,2024]:
        it = P.get("IT",{}).get(y); de = P.get("DE",{}).get(y)
        if it and de:
            print(f"  Italia vs Germania {y}: {chg(statistics.mean(it),statistics.mean(de)):+.0f}%")
price("eurostat_prezzo_elettricita_industria.csv","ELETTRICITA'")
price("eurostat_prezzo_gas_industria.csv","GAS NATURALE")
print("\nFINE.")
