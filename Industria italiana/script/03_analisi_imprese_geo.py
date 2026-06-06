#!/usr/bin/env python3
# Analisi struttura d'impresa della manifattura e concentrazione geografica
import csv
from collections import defaultdict

INP = "../input"
def load(f): return list(csv.DictReader(open(f"{INP}/{f}")))
def num(x):
    try: return float(x)
    except: return None
def chg(a,b): return 100*(a/b-1) if b else None

print("="*70)
print("A. CONCENTRAZIONE GEOGRAFICA DELLA MANIFATTURA NEL TEMPO")
print("="*70)
rg = load("eurostat_valore_aggiunto_regionale.csv")
re = load("eurostat_occupazione_regionale.csv")
macro = {  # NUTS2 -> macroarea
 "ITC1":"Nord","ITC2":"Nord","ITC3":"Nord","ITC4":"Nord",
 "ITH1":"Nord","ITH2":"Nord","ITH3":"Nord","ITH4":"Nord","ITH5":"Nord",
 "ITI1":"Centro","ITI2":"Centro","ITI3":"Centro","ITI4":"Centro",
 "ITF1":"Mezzogiorno","ITF2":"Mezzogiorno","ITF3":"Mezzogiorno","ITF4":"Mezzogiorno",
 "ITF5":"Mezzogiorno","ITF6":"Mezzogiorno","ITG1":"Mezzogiorno","ITG2":"Mezzogiorno"}

def conc(rows, valcol, nace="C"):
    d = defaultdict(dict)
    for x in rows:
        v = num(x[valcol])
        if v is not None and x["nace_r2"]==nace and x["geo"] in macro:
            d[x["geo"]][int(float(x["anno"]))] = v
    return d

vd = conc(rg, "va_meur")
print("Valore aggiunto manifatturiero - indici di concentrazione tra le 21 regioni:")
print(f"{'anno':>6}{'top4 %':>9}{'top1 %':>9}{'HHI':>8}{'Nord %':>9}{'Mezzog. %':>11}")
for y in [2000,2003,2007,2011,2015,2019,2023]:
    vals = {g:d[y] for g,d in vd.items() if y in d}
    tot = sum(vals.values())
    sh = sorted((v/tot for v in vals.values()), reverse=True)
    hhi = sum(s*s for s in sh)*10000
    nord = sum(v for g,v in vals.items() if macro[g]=="Nord")/tot*100
    mezz = sum(v for g,v in vals.items() if macro[g]=="Mezzogiorno")/tot*100
    print(f"{y:>6}{sum(sh[:4])*100:>8.1f}%{sh[0]*100:>8.1f}%{hhi:>8.0f}{nord:>8.1f}%{mezz:>10.1f}%")

ed = conc(re, "occupati_migliaia")
print("\nOccupati manifatturieri - quota per macroarea:")
print(f"{'anno':>6}{'Nord %':>9}{'Centro %':>10}{'Mezzog. %':>11}")
for y in [2000,2007,2015,2023]:
    vals = {g:d[y] for g,d in ed.items() if y in d}
    tot = sum(vals.values())
    for lab in ["Nord","Centro","Mezzogiorno"]:
        pass
    nord=sum(v for g,v in vals.items() if macro[g]=="Nord")/tot*100
    cen=sum(v for g,v in vals.items() if macro[g]=="Centro")/tot*100
    mez=sum(v for g,v in vals.items() if macro[g]=="Mezzogiorno")/tot*100
    print(f"{y:>6}{nord:>8.1f}%{cen:>9.1f}%{mez:>10.1f}%")

print()
print("="*70)
print("B. STRUTTURA DIMENSIONALE DELLE IMPRESE MANIFATTURIERE (ISTAT SBS)")
print("="*70)
sbs = load("imprese_manifattura_per_dimensione.csv")
D = defaultdict(dict)  # (variabile, classe_codice) -> {anno: valore}
for x in sbs:
    v = num(x["valore"])
    if v is not None: D[(x["variabile"], x["classe_codice"])][int(x["anno"])] = v
classi = [("W0_9","0-9 addetti (micro)"),("W10_19","10-19 (piccole)"),
          ("W20_49","20-49 (piccole)"),("W50_249","50-249 (medie)"),
          ("W_GE250","250+ (grandi)")]
Y0, Y1 = 2008, 2023

def row(var, label, unit=1, dec=0):
    tot0 = D[(var,"TOTAL")][Y0]; tot1 = D[(var,"TOTAL")][Y1]
    print(f"\n{label} — {Y0}: {tot0/unit:,.{dec}f}  |  {Y1}: {tot1/unit:,.{dec}f}  |  var. {chg(tot1,tot0):+.1f}%")
    for code,nm in classi:
        a = D[(var,code)].get(Y0); b = D[(var,code)].get(Y1)
        if a and b:
            print(f"  {nm:24s} {a/unit:>13,.{dec}f} -> {b/unit:>13,.{dec}f}  ({chg(b,a):+6.1f}%)  "
                  f"quota {100*a/tot0:5.1f}% -> {100*b/tot1:5.1f}%")

row("imprese", "NUMERO DI IMPRESE")
row("addetti", "ADDETTI")
row("valore_aggiunto_mgl_eur", "VALORE AGGIUNTO (migliaia di euro, prezzi correnti)", unit=1000, dec=0)
row("fatturato_mgl_eur", "FATTURATO (migliaia di euro, prezzi correnti)", unit=1000, dec=0)

print("\n" + "-"*70)
print("DIMENSIONE MEDIA D'IMPRESA (addetti per impresa)")
for y in [2008,2012,2016,2020,2023]:
    tot = D[("addetti","TOTAL")][y]/D[("imprese","TOTAL")][y]
    print(f"  {y}: {tot:.2f} addetti per impresa")
print("  per classe (addetti/impresa):")
for code,nm in classi:
    for y in [Y0,Y1]:
        pass
    a = D[("addetti",code)][Y0]/D[("imprese",code)][Y0]
    b = D[("addetti",code)][Y1]/D[("imprese",code)][Y1]
    print(f"    {nm:24s} {Y0}: {a:6.1f}   {Y1}: {b:6.1f}")

print("\n" + "-"*70)
print("VALORE AGGIUNTO PER ADDETTO per classe dimensionale (migliaia di euro)")
print("(misura della produttività; prezzi correnti)")
for y in [Y0,Y1]:
    print(f"  Anno {y}:")
    for code,nm in [("TOTAL","TOTALE")]+classi:
        va = D[("valore_aggiunto_mgl_eur",code)][y]
        ad = D[("addetti",code)][y]
        print(f"    {nm:24s} {va/ad:6.1f}")

print("\n" + "-"*70)
print("VALORE AGGIUNTO PER IMPRESA (migliaia di euro)")
for code,nm in [("TOTAL","TOTALE")]+classi:
    a = D[("valore_aggiunto_mgl_eur",code)][Y0]/D[("imprese",code)][Y0]
    b = D[("valore_aggiunto_mgl_eur",code)][Y1]/D[("imprese",code)][Y1]
    print(f"  {nm:24s} {Y0}: {a:10,.0f}   {Y1}: {b:10,.0f}")

print("\n" + "-"*70)
print("COSTO DEL LAVORO PER DIPENDENTE (proxy: costi personale / addetti, migliaia €)")
print("NB: per le micro il dato è distorto perché i titolari non sono 'personale'")
for code,nm in [("TOTAL","TOTALE")]+classi:
    b = D[("costi_personale_mgl_eur",code)][Y1]/D[("addetti",code)][Y1]
    print(f"  {nm:24s} {Y1}: {b:6.1f}")

print("\nFINE.")
