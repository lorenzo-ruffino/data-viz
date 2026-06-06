#!/usr/bin/env python3
# Analisi con valori reali (deflazionati HICP), export per destinazione e
# demografia d'impresa (selezione).
import csv
from collections import defaultdict

INP = "../input"
def load(f): return list(csv.DictReader(open(f"{INP}/{f}")))
def num(x):
    try: return float(x)
    except: return None
def chg(a,b): return 100*(a/b-1) if b else None

# --- deflatore HICP Italia (base 2015=100) ---------------------------------
HICP = {}
for r in load("eurostat_hicp_italia.csv"):
    y = int(float(r["anno"])); v = num(r["hicp_indice"])
    if v: HICP[y] = v
BASE = 2024  # esprimo tutto in euro costanti 2024
def real(value, year):
    """Converte un valore nominale dell'anno 'year' in euro costanti 2024."""
    if year in HICP and value is not None:
        return value * HICP[BASE] / HICP[year]
    return None
print(f"Deflatore HICP Italia: anni {min(HICP)}-{max(HICP)} | "
      f"2000={HICP.get(2000)} 2008={HICP.get(2008)} 2015=100 2024={HICP[2024]}")
print(f"Inflazione cumulata 2000->2024: +{HICP[2024]/HICP[2000]*100-100:.0f}% | "
      f"2008->2023: +{HICP[2024]/HICP[2008]*100-100:.0f}% circa")

print()
print("="*70)
print("A. EXPORT MANIFATTURIERO: NOMINALE vs REALE (euro costanti 2024)")
print("="*70)
T = defaultdict(dict)
for r in load("coeweb_commercio_estero_per_settore.csv"):
    v = num(r["valore_euro"])
    if v is not None: T[(r["codice_cpa"], r["flusso"])][int(r["anno"])] = v
exC = T[("C","esportazioni")]
print(f"{'anno':>6}{'nominale':>14}{'reale (€2024)':>17}")
for y in [2000,2007,2013,2019,2024]:
    if y in exC and y in HICP:
        print(f"{y:>6}{exC[y]/1e9:11.0f} mld{real(exC[y],y)/1e9:13.0f} mld")
g_nom = chg(exC[2024], exC[2000])
g_real = chg(exC[2024], real(exC[2000],2000)) if 2000 in HICP else None
print(f"Crescita export manifatturiero 2000->2024:")
print(f"  nominale: {g_nom:+.0f}%   |   REALE (al netto inflazione): {g_real:+.0f}%")

print("\nCrescita REALE per settore, 2000->2024 (export, euro costanti 2024):")
subs = ["CA","CB","CC","CD","CE","CF","CG","CH","CI","CJ","CK","CL","CM"]
lab = {r["codice_cpa"]:r["settore"] for r in load("coeweb_commercio_estero_per_settore.csv")}
res=[]
for c in subs:
    e = T[(c,"esportazioni")]
    if 2000 in e and 2024 in e:
        res.append((lab[c], chg(e[2024],e[2000]), chg(e[2024], real(e[2000],2000))))
res.sort(key=lambda x:-x[2])
print(f"  {'settore':34s}{'nominale':>11}{'reale':>10}")
for nm,n,rr in res:
    print(f"  {nm:34s}{n:+9.0f}%{rr:+9.0f}%")

print()
print("="*70)
print("B. EXPORT MANIFATTURIERO PER DESTINAZIONE (Coeweb)")
print("="*70)
D = defaultdict(dict)
for r in load("coeweb_export_manifattura_per_destinazione.csv"):
    v = num(r["export_manifattura_euro"])
    if v is not None: D[r["destinazione"]][int(r["anno"])] = v
mondo = D["Mondo"]; ue = D["Unione Europea (27)"]
extra = {y: mondo[y]-ue.get(y,0) for y in mondo if y in ue}
def line(nm, d):
    for y in (2000,2024): pass
    q00 = 100*d.get(2000,0)/mondo[2000]; q24 = 100*d.get(2024,0)/mondo[2024]
    gr  = chg(d.get(2024), real(d.get(2000),2000))
    print(f"  {nm:22s} 2000:{d.get(2000,0)/1e9:7.1f} ({q00:4.1f}%)  "
          f"2024:{d.get(2024,0)/1e9:7.1f} ({q24:4.1f}%)  cresc.reale {gr:+.0f}%")
print("Export manifatturiero per area (mld € correnti e quota sul totale):")
line("Unione Europea (27)", ue)
line("Extra-UE", extra)
for nm in ["Germania","Francia","Stati Uniti","Spagna","Regno Unito","Svizzera","Cina"]:
    if nm in D: line(nm, D[nm])
print(f"\nQuota UE sul totale export: 2000 {100*ue[2000]/mondo[2000]:.1f}% -> "
      f"2024 {100*ue[2024]/mondo[2024]:.1f}%  (Extra-UE: "
      f"{100*extra[2000]/mondo[2000]:.1f}% -> {100*extra[2024]/mondo[2024]:.1f}%)")

print()
print("="*70)
print("C. STRUTTURA D'IMPRESA: VALORE AGGIUNTO REALE per classe dimensionale")
print("="*70)
S = defaultdict(dict)
for r in load("imprese_manifattura_per_dimensione.csv"):
    v = num(r["valore"])
    if v is not None: S[(r["variabile"], r["classe_codice"])][int(r["anno"])] = v
classi = [("W0_9","micro 0-9"),("W10_19","piccole 10-19"),("W20_49","piccole 20-49"),
          ("W50_249","medie 50-249"),("W_GE250","grandi 250+"),("TOTAL","TOTALE")]
Y0,Y1 = 2008,2023
print(f"Valore aggiunto per addetto (migliaia di euro COSTANTI 2024):")
print(f"  {'classe':18s}{'2008 nom':>10}{'2008 reale':>12}{'2023 reale':>12}{'var.reale':>11}")
for code,nm in classi:
    va0=S[("valore_aggiunto_mgl_eur",code)].get(Y0); ad0=S[("addetti",code)].get(Y0)
    va1=S[("valore_aggiunto_mgl_eur",code)].get(Y1); ad1=S[("addetti",code)].get(Y1)
    if va0 and ad0 and va1 and ad1:
        n0=va0/ad0; r0=real(va0,Y0)/ad0; r1=real(va1,Y1)/ad1
        print(f"  {nm:18s}{n0:9.1f}{r0:11.1f}{r1:11.1f}{chg(r1,r0):+10.1f}%")
print(f"\nValore aggiunto per impresa (migliaia di euro COSTANTI 2024):")
print(f"  {'classe':18s}{'2008 reale':>12}{'2023 reale':>12}{'var.reale':>11}")
for code,nm in classi:
    va0=S[("valore_aggiunto_mgl_eur",code)].get(Y0); im0=S[("imprese",code)].get(Y0)
    va1=S[("valore_aggiunto_mgl_eur",code)].get(Y1); im1=S[("imprese",code)].get(Y1)
    if va0 and im0 and va1 and im1:
        r0=real(va0,Y0)/im0; r1=real(va1,Y1)/im1
        print(f"  {nm:18s}{r0:11,.0f}{r1:11,.0f}{chg(r1,r0):+10.1f}%")
# quota VA reale
print(f"\nValore aggiunto manifatturiero totale: {Y0} reale "
      f"{real(S[('valore_aggiunto_mgl_eur','TOTAL')][Y0],Y0)/1e6:,.0f} mld -> "
      f"{Y1} reale {real(S[('valore_aggiunto_mgl_eur','TOTAL')][Y1],Y1)/1e6:,.0f} mld "
      f"({chg(real(S[('valore_aggiunto_mgl_eur','TOTAL')][Y1],Y1),real(S[('valore_aggiunto_mgl_eur','TOTAL')][Y0],Y0)):+.1f}%)")

print()
print("="*70)
print("D. DEMOGRAFIA D'IMPRESA: NASCITE, CESSAZIONI, SELEZIONE (Eurostat)")
print("="*70)
BD = defaultdict(dict)
for r in load("eurostat_demografia_impresa.csv"):
    v = num(r["valore"])
    if v is not None: BD[(r["nace_r2"], r["indic_sb"])][int(float(r["anno"]))] = v
active = BD[("C","V11910")]; born = BD[("C","V11920")]; dead = BD[("C","V11930")]
print("Manifattura (C) - imprese attive, nate (iscritte), cessate:")
print(f"  {'anno':>6}{'attive':>10}{'nate':>9}{'cessate':>10}{'saldo':>9}"
      f"{'tasso nat.':>12}{'tasso mort.':>13}")
yrs = sorted(y for y in active if y in born and y in dead)
for y in yrs:
    a,b,d = active[y], born[y], dead[y]
    print(f"  {y:>6}{a:10,.0f}{b:9,.0f}{d:10,.0f}{b-d:+9,.0f}"
          f"{100*b/a:11.1f}%{100*d/a:12.1f}%")
tb = sum(born[y] for y in yrs); td = sum(dead[y] for y in yrs)
print(f"  TOTALE {yrs[0]}-{yrs[-1]}: nate {tb:,.0f}  cessate {td:,.0f}  "
      f"saldo {tb-td:+,.0f}")
print("\nNel periodo coperto le cessazioni hanno superato le nascite negli anni:")
neg = [y for y in yrs if born[y]<dead[y]]
print("  ", ", ".join(str(y) for y in neg))
print("\nFINE.")
