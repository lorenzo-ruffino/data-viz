#!/usr/bin/env python3
# Analisi dei dati di commercio estero: Coeweb (Italia, settore e regione) +
# Eurostat Comext (confronto internazionale).
import csv
from collections import defaultdict

INP = "../input"
def load(f): return list(csv.DictReader(open(f"{INP}/{f}")))
def num(x):
    try: return float(x)
    except: return None
def chg(a,b): return 100*(a/b-1) if b else None

print("="*70)
print("A. COMMERCIO ESTERO PER SETTORE MANIFATTURIERO (Coeweb, valori in euro)")
print("="*70)
rows = load("coeweb_commercio_estero_per_settore.csv")
T = defaultdict(dict)  # (codice, flusso) -> {anno: valore}
labels = {}
for r in rows:
    v = num(r["valore_euro"])
    if v is not None:
        T[(r["codice_cpa"], r["flusso"])][int(r["anno"])] = v
        labels[r["codice_cpa"]] = r["settore"]

exC = T[("C","esportazioni")]; imC = T[("C","importazioni")]
print("Esportazioni manifatturiere totali (CPA C):")
for y in [1991,2000,2007,2009,2019,2020,2024]:
    if y in exC: print(f"  {y}: {exC[y]/1e9:7.1f} mld €   (saldo {(exC[y]-imC[y])/1e9:+.1f} mld)")
print(f"Export 2024 vs 1991: {chg(exC[2024],exC[1991]):+.0f}%  | 2024 vs 2000: {chg(exC[2024],exC[2000]):+.0f}%")
print(f"Saldo commerciale manifatturiero 2024: {(exC[2024]-imC[2024])/1e9:+.1f} mld €  (1991: {(exC[1991]-imC[1991])/1e9:+.1f})")

print("\nEsportazioni per settore: valore 2024 e crescita")
subs = ["CA","CB","CC","CD","CE","CF","CG","CH","CI","CJ","CK","CL","CM"]
res = []
for c in subs:
    e = T[(c,"esportazioni")]; im = T[(c,"importazioni")]
    if 2024 in e:
        res.append((labels[c], c, e[2024], e.get(2000), chg(e[2024],e.get(2000)),
                    e[2024]-im[2024], 100*e[2024]/exC[2024]))
res.sort(key=lambda x:-x[2])
print(f"{'Settore':34s}{'export24':>11s}{'quota':>7s}{'v.24/00':>9s}{'saldo24':>11s}")
for nm,c,e24,e00,g,saldo,q in res:
    g = f"{g:+.0f}%" if g is not None else "-"
    print(f"{nm:34s}{e24/1e9:9.1f}M€{q:6.1f}%{g:>9s}{saldo/1e9:+9.1f}M€")

print()
print("="*70)
print("B. PROPENSIONE A ESPORTARE (export / valore aggiunto, prezzi correnti)")
print("="*70)
# valore aggiunto per branca ISTAT (prezzi correnti)
va = load("valore_aggiunto_per_branca.csv")
VA = defaultdict(dict)
for r in va:
    if r["valutazione"]=="prezzi correnti":
        v = num(r["valore_aggiunto_mln_eur"])
        if v is not None: VA[r["codice"]][int(r["anno"])] = v   # mln €
cpa2va = {"CA":"C10T12","CB":"C13T15","CC":"C16T18","CD":"C19","CE":"C20",
 "CF":"C21","CG":"C22_23","CH":"C24_25","CI":"C26","CJ":"C27","CK":"C28",
 "CL":"C29_30","CM":"C31T33"}
print("Export in % del valore aggiunto del settore (manifattura totale e comparti):")
for y in [2000,2007,2024]:
    e = exC.get(y); v = VA["C"].get(y)
    if e and v: print(f"  Totale manifattura {y}: export {e/1e9:.0f} mld / VA {v/1e3:.0f} mld = {100*e/(v*1e6):.0f}%")
print("  Per comparto (2000 -> 2024):")
pr=[]
for c in subs:
    vc = cpa2va.get(c)
    e0=T[(c,"esportazioni")].get(2000); e1=T[(c,"esportazioni")].get(2024)
    v0=VA.get(vc,{}).get(2000); v1=VA.get(vc,{}).get(2024)
    if e0 and e1 and v0 and v1:
        r0=100*e0/(v0*1e6); r1=100*e1/(v1*1e6)
        pr.append((labels[c], r0, r1))
pr.sort(key=lambda x:-x[2])
for nm,r0,r1 in pr:
    print(f"    {nm:34s} 2000: {r0:5.0f}%   2024: {r1:5.0f}%   ({r1-r0:+.0f} pp)")

print()
print("="*70)
print("C. EXPORT MANIFATTURIERO PER REGIONE (Coeweb)")
print("="*70)
reg = load("coeweb_export_manifattura_per_regione.csv")
R = defaultdict(dict); rlab = {}
for r in reg:
    v = num(r["export_manifattura_euro"])
    code = r["codice"]
    # solo regioni NUTS2: IT + lettera + cifra, lunghezza 4
    if v is not None and len(code)==4 and code[2].isalpha() and code[3].isdigit():
        R[code][int(r["anno"])] = v
        rlab[code] = r["territorio"]
print(f"Export manifatturiero per regione, 2024 ({len(R)} regioni):")
tot24 = sum(d.get(2024,0) for d in R.values())
out = []
for code,d in R.items():
    if 2024 in d and 2000 in d:
        out.append((rlab[code], d[2024], 100*d[2024]/tot24, chg(d[2024],d[2000])))
out.sort(key=lambda x:-x[1])
for nm,e24,q,g in out:
    print(f"  {nm:22s} {e24/1e9:8.1f} mld €  quota {q:5.1f}%  cresc.00-24 {g:+6.0f}%")
# concentrazione nel tempo
print("\nConcentrazione dell'export manifatturiero tra le regioni:")
for y in [2000,2008,2015,2024]:
    vals = sorted((d[y] for d in R.values() if y in d), reverse=True)
    tot = sum(vals)
    print(f"  {y}: prime 4 regioni = {100*sum(vals[:4])/tot:.1f}%   prima = {100*vals[0]/tot:.1f}%")

print()
print("="*70)
print("D. CONFRONTO INTERNAZIONALE (Eurostat Comext, ext_lt_intratrd)")
print("="*70)
cx = load("eurostat_comext_export_sitc.csv")
X = defaultdict(dict)  # (geo, indic, sitc, partner) -> {anno: mln}
for r in cx:
    v = num(r["valore_mln_eur"])
    if v is not None:
        X[(r["geo"],r["indicatore"],r["sitc"],r["partner"])][int(float(r["anno"]))] = v
geos = ["IT","DE","FR","ES","EU27_2020"]
print("Esportazioni di beni (totale, mld €, partner mondo):")
print(f"{'anno':>6}" + "".join(f"{g:>10}" for g in geos))
for y in [2002,2008,2019,2024]:
    line=f"{y:>6}"
    for g in geos:
        v=X.get((g,"MIO_EXP_VAL","TOTAL","WORLD"),{}).get(y)
        line+=f"{v/1e3:9.0f}" if v else f"{'-':>10}"
    print(line)
print("\nCrescita delle esportazioni di beni 2024 vs 2002:")
for g in geos:
    a=X.get((g,"MIO_EXP_VAL","TOTAL","WORLD"),{}).get(2002)
    b=X.get((g,"MIO_EXP_VAL","TOTAL","WORLD"),{}).get(2024)
    if a and b: print(f"  {g:10s} {chg(b,a):+.0f}%")
print("\nQuota dell'Italia sull'export di beni UE-27:")
for y in [2002,2008,2015,2024]:
    it=X.get(("IT","MIO_EXP_VAL","TOTAL","WORLD"),{}).get(y)
    eu=X.get(("EU27_2020","MIO_EXP_VAL","TOTAL","WORLD"),{}).get(y)
    if it and eu: print(f"  {y}: {100*it/eu:.1f}%")
print("\nSaldo commerciale di beni (mld €, 2024, partner mondo):")
for g in geos:
    b=X.get((g,"MIO_BAL_VAL","TOTAL","WORLD"),{}).get(2024)
    if b is not None: print(f"  {g:10s} {b/1e3:+.0f} mld")
print("\nComposizione export italiano per gruppo SITC (quota %, partner mondo):")
sitc_lab={"SITC5":"Chimica","SITC6_8":"Manufatti di base","SITC7":"Macchine e mezzi di trasporto",
 "SITC0_1":"Alimentari","SITC2_4":"Materie prime","SITC3":"Energia","SITC9":"Altro"}
for y in [2002,2024]:
    tot=X.get(("IT","MIO_EXP_VAL","TOTAL","WORLD"),{}).get(y)
    print(f"  {y}:")
    for s,lab in sitc_lab.items():
        v=X.get(("IT","MIO_EXP_VAL",s,"WORLD"),{}).get(y)
        if v and tot: print(f"    {lab:32s} {100*v/tot:5.1f}%")
print("\nFINE.")
