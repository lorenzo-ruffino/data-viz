#!/usr/bin/env python3
# Analisi: produttivita' del lavoro, costo del lavoro per unita' di prodotto
# (CLUP), salari reali e investimenti nella manifattura.
import csv
from collections import defaultdict

INP = "../input"
def load(f): return list(csv.DictReader(open(f"{INP}/{f}")))
def num(x):
    try: return float(x)
    except: return None
def chg(a,b): return 100*(a/b-1) if b else None

GEOS = ["IT","DE","FR","ES","EU27_2020"]

# valore aggiunto reale (chained 2015) e occupati, manifattura, per Paese
VA = defaultdict(dict); EMP = defaultdict(dict)
for r in load("eurostat_valore_aggiunto_a10.csv"):
    if r["nace_r2"]=="C" and r["unit"]=="CLV15_MEUR":
        v=num(r["valore"])
        if v is not None: VA[r["geo"]][int(float(r["anno"]))]=v
for r in load("eurostat_occupazione_a10.csv"):
    if r["nace_r2"]=="C":
        v=num(r["occupati_migliaia"])
        if v is not None: EMP[r["geo"]][int(float(r["anno"]))]=v

print("="*70)
print("A. PRODUTTIVITA' DEL LAVORO NELLA MANIFATTURA (valore aggiunto reale")
print("   per occupato; indice 2000=100)")
print("="*70)
prod = {}
for g in GEOS:
    prod[g] = {y: VA[g][y]/EMP[g][y] for y in VA[g] if y in EMP[g]}
print(f"{'anno':>6}" + "".join(f"{g:>11}" for g in GEOS))
for y in [2000,2007,2015,2019,2024]:
    line=f"{y:>6}"
    for g in GEOS:
        d=prod[g]
        line += f"{100*d[y]/d[2000]:11.1f}" if 2000 in d and y in d else f"{'-':>11}"
    print(line)
print("\nCrescita della produttivita' manifatturiera 2000->2024:")
for g in GEOS:
    d=prod[g]
    if 2000 in d and 2024 in d: print(f"  {g:10s} {chg(d[2024],d[2000]):+.1f}%")
print("Livello del valore aggiunto reale per occupato, 2024 (migliaia di euro 2015):")
for g in GEOS:
    d=prod[g]
    if 2024 in d: print(f"  {g:10s} {d[2024]*1000:.1f}")

print()
print("="*70)
print("B. COSTO DEL LAVORO PER UNITA' DI PRODOTTO - CLUP (manifattura)")
print("   CLUP = redditi da lavoro / valore aggiunto reale; indice 2000=100")
print("="*70)
D1 = defaultdict(dict)
for r in load("eurostat_redditi_lavoro_a10.csv"):
    if r["nace_r2"]=="C" and r["voce"]=="D1":
        v=num(r["valore_meur"])
        if v is not None: D1[r["geo"]][int(float(r["anno"]))]=v
ulc = {}
for g in GEOS:
    ulc[g] = {y: D1[g][y]/VA[g][y] for y in D1[g] if y in VA[g]}
print(f"{'anno':>6}" + "".join(f"{g:>11}" for g in GEOS))
for y in [2000,2007,2015,2019,2024]:
    line=f"{y:>6}"
    for g in GEOS:
        d=ulc[g]
        line += f"{100*d[y]/d[2000]:11.1f}" if 2000 in d and y in d else f"{'-':>11}"
    print(line)
print("\nVariazione del CLUP nominale 2000->2024 (manifattura):")
for g in GEOS:
    d=ulc[g]
    if 2000 in d and 2024 in d: print(f"  {g:10s} {chg(d[2024],d[2000]):+.1f}%")

print()
print("="*70)
print("C. SALARI NELLA MANIFATTURA - ITALIA (retribuzioni per dipendente)")
print("="*70)
HICP={int(float(r["anno"])):num(r["hicp_indice"]) for r in load("eurostat_hicp_italia.csv")}
def real(v,y): return v*HICP[2024]/HICP[y] if y in HICP and v is not None else None
D11=defaultdict(dict)
for r in load("eurostat_redditi_lavoro_a10.csv"):
    if r["nace_r2"]=="C" and r["voce"]=="D11":
        v=num(r["valore_meur"])
        if v is not None: D11[r["geo"]][int(float(r["anno"]))]=v
SAL=defaultdict(dict)
for r in load("eurostat_dipendenti_a10.csv"):
    if r["nace_r2"]=="C":
        v=num(r["dipendenti_migliaia"])
        if v is not None: SAL[r["geo"]][int(float(r["anno"]))]=v
# retribuzione lorda per dipendente (D11 mln / dipendenti mila = mille eur)
print("Retribuzione lorda media per dipendente, manifattura Italia (mila € l'anno):")
print(f"  {'anno':>6}{'nominale':>11}{'reale (€2024)':>16}")
wi = {y: D11['IT'][y]/SAL['IT'][y] for y in D11['IT'] if y in SAL['IT']}
for y in [2000,2008,2015,2019,2024]:
    if y in wi: print(f"  {y:>6}{wi[y]:10.1f}{real(wi[y],y):14.1f}")
print(f"Variazione retribuzione reale 2000->2024: {chg(wi[2024],real(wi[2000],2000)):+.1f}%")
print(f"Variazione retribuzione reale 2008->2024: {chg(wi[2024],real(wi[2008],2008)):+.1f}%")
print("\nCosto del lavoro per dipendente (redditi D1) - confronto, 2024 (mila €):")
for g in GEOS:
    if 2024 in D1[g] and 2024 in SAL[g]:
        print(f"  {g:10s} {D1[g][2024]/SAL[g][2024]:.1f}")

print()
print("="*70)
print("D. INVESTIMENTI NELLA MANIFATTURA - ITALIA")
print("="*70)
INV=defaultdict(dict)  # (codice, valutazione) -> {anno: mln}
lab={}
for r in load("investimenti_per_branca.csv"):
    v=num(r["investimenti_mln_eur"])
    if v is not None:
        INV[(r["codice"],r["valutazione"])][int(r["anno"])]=v
        lab[r["codice"]]=r["branca"]
ic = INV[("C","valori concatenati 2020")]   # investimenti reali manifattura
it_ = INV[("_T","valori concatenati 2020")]
print("Investimenti fissi lordi, manifattura (valori concatenati 2020, mln €):")
for y in [1995,2000,2007,2008,2013,2019,2023]:
    if y in ic: print(f"  {y}: {ic[y]:,.0f}   (vs 2007: {chg(ic[y],ic[2007]):+.1f}%)")
print(f"Investimenti manifattura reali: 2023 vs 2000 {chg(ic[2023],ic[2000]):+.1f}% | "
      f"2023 vs 2007 {chg(ic[2023],ic[2007]):+.1f}%")
print(f"Investimenti totale economia reali: 2023 vs 2007 {chg(it_[2023],it_[2007]):+.1f}%")
# tasso di investimento: investimenti / valore aggiunto (prezzi correnti)
icv=INV[("C","prezzi correnti")]
VAcur=defaultdict(dict)
for r in load("valore_aggiunto_per_branca.csv"):
    if r["valutazione"]=="prezzi correnti" and r["codice"]=="C":
        v=num(r["valore_aggiunto_mln_eur"])
        if v is not None: VAcur[r["codice"]][int(r["anno"])]=v
print("\nTasso di investimento manifattura (investimenti / valore aggiunto, %):")
for y in [1995,2000,2007,2013,2019,2023]:
    if y in icv and y in VAcur["C"]:
        print(f"  {y}: {100*icv[y]/VAcur['C'][y]:.1f}%")
print("\nInvestimenti reali per sotto-settore manifatturiero, var. 2023 vs 2007:")
subs=["C10T12","C13T15","C16T18","C19T21","C22_23","C24_25","C26T28","C29_30","C31T33"]
res=[]
for c in subs:
    d=INV[(c,"valori concatenati 2020")]
    if 2007 in d and 2023 in d: res.append((lab[c], chg(d[2023],d[2007])))
res.sort(key=lambda x:x[1])
for nm,g in res: print(f"  {nm:34s} {g:+.1f}%")
print("\nFINE.")
