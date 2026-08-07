#!/usr/bin/env python3
# Analisi dati per l'articolo "Che fine ha fatto l'industria italiana"
import csv, statistics
from collections import defaultdict

INP = "../input"
def load(f):
    return list(csv.DictReader(open(f"{INP}/{f}")))
def num(x):
    try: return float(x)
    except: return None
def pct(a, b): return 100*a/b if b else None
def chg(a, b): return 100*(a/b-1) if b else None

print("="*70)
print("1. PRODUZIONE INDUSTRIALE - ITALIA (indice base 2021=100, ISTAT)")
print("="*70)
rows = load("produzione_industriale_indice_mensile.csv")
# media annua per categoria
annu = defaultdict(lambda: defaultdict(list))
for r in rows:
    y = int(r["mese"][:4]); v = num(r["indice"])
    if v is not None: annu[r["categoria"]][y].append(v)
tot = {y: statistics.mean(vs) for y, vs in annu["Totale industria escl. costruzioni"].items() if len(vs)==12}
yrs = sorted(tot)
print(f"Media annua indice (anni completi {yrs[0]}-{yrs[-1]}):")
for y in [2000,2007,2008,2009,2013,2015,2019,2020,2021,2023,2024,2025]:
    if y in tot: print(f"  {y}: {tot[y]:.1f}")
peak = max(tot, key=tot.get); trough = min((y for y in tot if y>=2008), key=tot.get)
print(f"Picco: {peak} ({tot[peak]:.1f}) | minimo post-2008: {trough} ({tot[trough]:.1f})")
print(f"Caduta picco->minimo: {chg(tot[trough],tot[peak]):+.1f}%")
print(f"2024 vs 2007: {chg(tot[2024],tot[2007]):+.1f}%   2024 vs 2000: {chg(tot[2024],tot[2000]):+.1f}%")
print(f"2025 vs 2021 (base): {chg(tot[2025],tot[2021]):+.1f}%   2025 vs 2019: {chg(tot[2025],tot[2019]):+.1f}%")
print("Per raggruppamento (media annua, var. 2024 vs 2007):")
for cat in ["Beni intermedi","Beni strumentali","Beni di consumo","Energia"]:
    a = {y: statistics.mean(vs) for y,vs in annu[cat].items() if len(vs)==12}
    if 2007 in a and 2024 in a:
        print(f"  {cat:24s}: 2007={a[2007]:.0f} 2024={a[2024]:.0f}  {chg(a[2024],a[2007]):+.1f}%")

print()
print("="*70)
print("2. PRODUZIONE PER SETTORE MANIFATTURIERO (indice 2021=100, ISTAT)")
print("="*70)
rows = load("produzione_industriale_per_settore_mensile.csv")
sett = defaultdict(lambda: defaultdict(list))
for r in rows:
    y = int(r["mese"][:4]); v = num(r["indice"])
    if v is not None: sett[r["settore"]][y].append(v)
res = []
for s, d in sett.items():
    a = {y: statistics.mean(vs) for y,vs in d.items() if len(vs)==12}
    if 2007 in a and 2024 in a and 2000 in a:
        res.append((s, a[2000], a[2007], a[2024], a.get(2025), chg(a[2024],a[2007]), chg(a[2024],a[2000])))
res.sort(key=lambda x: x[5])
print(f"{'Settore':35s}{'2000':>8s}{'2007':>8s}{'2024':>8s}{'v24/07':>9s}{'v24/00':>9s}")
for s,v00,v07,v24,v25,c0724,c0024 in res:
    print(f"{s:35s}{v00:8.0f}{v07:8.0f}{v24:8.0f}{c0724:8.1f}%{c0024:8.1f}%")

print()
print("="*70)
print("3. VALORE AGGIUNTO PER BRANCA - ITALIA (ISTAT, conti nazionali)")
print("="*70)
rows = load("valore_aggiunto_per_branca.csv")
va = defaultdict(dict)  # (val, codice) -> {anno: valore}
for r in rows:
    v = num(r["valore_aggiunto_mln_eur"])
    if v is not None: va[(r["valutazione"], r["codice"])][int(r["anno"])] = v
cur = lambda c: va[("prezzi correnti", c)]
chn = lambda c: va[("valori concatenati 2020", c)]
print("Quota dell'industria sul valore aggiunto totale (prezzi correnti):")
print(f"{'anno':>6s}{'manif.C':>10s}{'industria':>11s}{'  (C+B+D+E share su _T)':<10s}")
for y in [1995,2000,2007,2008,2009,2015,2019,2020,2024]:
    t = cur("_T").get(y)
    c = cur("C").get(y)
    ind = sum(cur(x).get(y,0) for x in ["B","C","D","E"])
    if t: print(f"{y:6d}{pct(c,t):9.1f}%{pct(ind,t):10.1f}%")
print("Valore aggiunto manifattura (C) - valori concatenati 2020 (volume reale):")
cc = chn("C")
for y in [1995,2000,2007,2008,2009,2015,2019,2020,2024]:
    if y in cc: print(f"  {y}: {cc[y]:,.0f} mln  (vs 2007: {chg(cc[y],cc[2007]):+.1f}%)")
print(f"Manifattura reale: 2024 vs 2000 {chg(cc[2024],cc[2000]):+.1f}% | 2024 vs 2007 {chg(cc[2024],cc[2007]):+.1f}%")
tt = chn("_T")
print(f"Totale economia reale: 2024 vs 2007 {chg(tt[2024],tt[2007]):+.1f}%")
print()
print("Sotto-settori manifattura: VA reale (concatenati 2020), var. 2024 vs 2007:")
subs = ["C10T12","C13T15","C16T18","C19","C20","C21","C22_23","C24_25","C26","C27","C28","C29_30","C31T33"]
namemap={r["codice"]:r["branca"] for r in rows}
sr=[]
for c in subs:
    d = chn(c)
    if 2007 in d and 2024 in d: sr.append((namemap[c], c, d[2000], d[2007], d[2024], chg(d[2024],d[2007])))
sr.sort(key=lambda x:x[5])
for nm,c,v00,v07,v24,ch in sr:
    print(f"  {nm:34s} 2007={v07:9,.0f} 2024={v24:9,.0f}  {ch:+6.1f}%")
print()
print("Quota di ogni sotto-settore sul VA manifatturiero (prezzi correnti) 2000 vs 2024:")
for c in subs:
    d=cur(c); ct=cur("C")
    if 2000 in d and 2024 in d:
        print(f"  {namemap[c]:34s} 2000={pct(d[2000],ct[2000]):5.1f}%  2024={pct(d[2024],ct[2024]):5.1f}%  ({pct(d[2024],ct[2024])-pct(d[2000],ct[2000]):+.1f}pp)")

print()
print("="*70)
print("4. OCCUPAZIONE PER BRANCA - ITALIA (ISTAT, migliaia di occupati)")
print("="*70)
rows = load("occupazione_per_branca.csv")
oc = defaultdict(dict)
for r in rows:
    if r["misura"]=="occupati":
        v=num(r["valore_migliaia"])
        if v is not None: oc[r["codice"]][int(r["anno"])]=v
print("Occupati nella manifattura (C) e quota sul totale:")
for y in [1995,2000,2007,2008,2009,2015,2019,2020,2024]:
    c=oc["C"].get(y); t=oc["_T"].get(y)
    if c and t: print(f"  {y}: {c:,.0f} mila  ({pct(c,t):.1f}% del totale occupati)")
c=oc["C"]
print(f"Manifattura: 2024 vs 2000 {chg(c[2024],c[2000]):+.1f}% ({c[2024]-c[2000]:+,.0f} mila) | 2024 vs 2007 {chg(c[2024],c[2007]):+.1f}% ({c[2024]-c[2007]:+,.0f} mila)")
ind={y:sum(oc[x].get(y,0) for x in ['B','C','D','E']) for y in c}
print(f"Industria in s.s. (B+C+D+E): 2000={ind[2000]:,.0f} 2024={ind[2024]:,.0f} quota su tot 2000={pct(ind[2000],oc['_T'][2000]):.1f}% 2024={pct(ind[2024],oc['_T'][2024]):.1f}%")
print("Occupati per sotto-settore manifattura, var. 2024 vs 2007:")
er=[]
for cc2 in subs:
    d=oc[cc2]
    if 2007 in d and 2024 in d: er.append((namemap[cc2],d[2000],d[2007],d[2024],chg(d[2024],d[2007])))
er.sort(key=lambda x:x[4])
for nm,v00,v07,v24,ch in er:
    print(f"  {nm:34s} 2007={v07:8,.0f} 2024={v24:8,.0f}  {ch:+6.1f}%  ({v24-v07:+,.0f} mila)")

print()
print("="*70)
print("5. VALORE AGGIUNTO PER ADDETTO (settori ad alto VA, Eurostat a64 IT)")
print("="*70)
va64=load("eurostat_valore_aggiunto_a64_italia.csv")
e64=load("eurostat_occupazione_a64_italia.csv")
vac=defaultdict(dict)  # codice -> {anno: VA corrente}
for r in va64:
    if r["unit"]=="CP_MEUR":
        v=num(r["valore"]);
        if v is not None: vac[r["nace_r2"]][int(float(r["anno"]))]=v
emp64=defaultdict(dict)
for r in e64:
    v=num(r["occupati_migliaia"])
    if v is not None: emp64[r["nace_r2"]][int(float(r["anno"]))]=v
# settori manifatturieri a64 (codici Eurostat)
mans={"C10-C12":"Alimentari bevande tabacco","C13-C15":"Tessile abbigl. pelle","C16":"Legno",
"C17":"Carta","C18":"Stampa","C19":"Raffinazione petrolio","C20":"Chimica","C21":"Farmaceutica",
"C22":"Gomma e plastica","C23":"Minerali non metalliferi","C24":"Metallurgia","C25":"Prodotti in metallo",
"C26":"Elettronica e ottica","C27":"Apparecchiature elettriche","C28":"Macchinari","C29":"Autoveicoli",
"C30":"Altri mezzi trasporto","C31_C32":"Mobili e altre manif.","C33":"Riparaz. installaz. macchine"}
LY=2024
print(f"VA per addetto, anno {LY} (migliaia di euro per occupato; VA corrente / occupati):")
prod=[]
for c,nm in mans.items():
    if LY in vac.get(c,{}) and LY in emp64.get(c,{}) and emp64[c][LY]>0:
        vpa=vac[c][LY]/emp64[c][LY]  # mln eur / mila persone = migliaia eur per occupato
        prod.append((nm,vpa,vac[c][LY],emp64[c][LY]))
prod.sort(key=lambda x:-x[1])
manC_vpa=vac["C"][LY]/emp64["C"][LY]
print(f"  {'MEDIA MANIFATTURA':30s} {manC_vpa:8.1f}")
for nm,vpa,v,e in prod:
    print(f"  {nm:30s} {vpa:8.1f}  (VA={v:,.0f} mln, occ={e:,.0f} mila)")
print()
print("VA per addetto 2024 - 13 sotto-settori manifattura (ISTAT: VA corrente / occupati):")
isr=[]
for c in subs:
    v=cur(c).get(2024); e=oc[c].get(2024)
    if v and e: isr.append((namemap[c], v/e, v, e))
isr.sort(key=lambda x:-x[1])
mm=cur("C")[2024]/oc["C"][2024]
print(f"  {'MEDIA MANIFATTURA':34s} {mm:7.1f}")
for nm,vpa,v,e in isr:
    print(f"  {nm:34s} {vpa:7.1f}  (VA={v:,.0f} mln, occ={e:,.0f} mila)")

print()
print("="*70)
print("6. CONFRONTO INTERNAZIONALE (Eurostat)")
print("="*70)
print("--- 6a. Quota del manifatturiero sul valore aggiunto totale (prezzi correnti) ---")
a10=load("eurostat_valore_aggiunto_a10.csv")
vq=defaultdict(dict)  # (geo,nace) -> {anno:val}
for r in a10:
    if r["unit"]=="CP_MEUR":
        v=num(r["valore"])
        if v is not None: vq[(r["geo"],r["nace_r2"])][int(float(r["anno"]))]=v
geos=["IT","DE","FR","ES","EU27_2020"]
print(f"{'anno':>6s}" + "".join(f"{g:>9s}" for g in geos) + "   (quota C su TOTAL)")
for y in [2000,2007,2019,2024]:
    line=f"{y:6d}"
    for g in geos:
        c=vq[(g,"C")].get(y); t=vq[(g,"TOTAL")].get(y)
        line+=f"{pct(c,t):8.1f}%" if c and t else f"{'-':>9s}"
    print(line)
print()
print("--- 6b. Quota dell'industria (B-E) sul valore aggiunto totale ---")
for y in [2000,2007,2019,2024]:
    line=f"{y:6d}"
    for g in geos:
        c=vq[(g,"B-E")].get(y); t=vq[(g,"TOTAL")].get(y)
        line+=f"{pct(c,t):8.1f}%" if c and t else f"{'-':>9s}"
    print(line)
print()
print("--- 6c. Indice produzione industriale (B-D), rebase 2000=100 ---")
ip=load("eurostat_produzione_industriale.csv")
ipd=defaultdict(dict)
for r in ip:
    if r["nace_r2"]=="B-D":
        v=num(r["indice"])
        if v is not None: ipd[r["geo"]][int(float(r["anno"]))]=v
print(f"{'anno':>6s}" + "".join(f"{g:>9s}" for g in geos))
for y in [2000,2007,2009,2019,2024]:
    line=f"{y:6d}"
    for g in geos:
        d=ipd.get(g,{})
        if 2000 in d and y in d: line+=f"{100*d[y]/d[2000]:9.1f}"
        else: line+=f"{'-':>9s}"
    print(line)
print()
print("--- 6d. Occupazione manifattura: quota su occupati totali ---")
e10=load("eurostat_occupazione_a10.csv")
eq=defaultdict(dict)
for r in e10:
    v=num(r["occupati_migliaia"])
    if v is not None: eq[(r["geo"],r["nace_r2"])][int(float(r["anno"]))]=v
print(f"{'anno':>6s}" + "".join(f"{g:>9s}" for g in geos))
for y in [2000,2007,2019,2024]:
    line=f"{y:6d}"
    for g in geos:
        c=eq[(g,"C")].get(y); t=eq[(g,"TOTAL")].get(y)
        line+=f"{pct(c,t):8.1f}%" if c and t else f"{'-':>9s}"
    print(line)
print()
print("--- 6e. Esportazioni di beni e servizi in % del PIL ---")
gdp=load("eurostat_pil_export.csv")
gd=defaultdict(dict)
for r in gdp:
    v=num(r["valore_meur"])
    if v is not None: gd[(r["geo"],r["na_item"])][int(float(r["anno"]))]=v
print(f"{'anno':>6s}" + "".join(f"{g:>9s}" for g in geos) + "   (P6/B1GQ)")
for y in [1995,2000,2007,2019,2024]:
    line=f"{y:6d}"
    for g in geos:
        p6=gd[(g,"P6")].get(y); pil=gd[(g,"B1GQ")].get(y)
        line+=f"{pct(p6,pil):8.1f}%" if p6 and pil else f"{'-':>9s}"
    print(line)
print("Esportazioni di soli BENI (P61) in % del PIL:")
for y in [2000,2007,2024]:
    line=f"{y:6d}"
    for g in geos:
        p6=gd[(g,"P61")].get(y); pil=gd[(g,"B1GQ")].get(y)
        line+=f"{pct(p6,pil):8.1f}%" if p6 and pil else f"{'-':>9s}"
    print(line)

print()
print("="*70)
print("7. DISTRIBUZIONE GEOGRAFICA (Eurostat conti regionali)")
print("="*70)
rg=load("eurostat_valore_aggiunto_regionale.csv")
re=load("eurostat_occupazione_regionale.csv")
regnames={"ITC1":"Piemonte","ITC2":"Valle d'Aosta","ITC3":"Liguria","ITC4":"Lombardia",
"ITH1":"Bolzano","ITH2":"Trento","ITH3":"Veneto","ITH4":"Friuli-V.G.","ITH5":"Emilia-Romagna",
"ITI1":"Toscana","ITI2":"Umbria","ITI3":"Marche","ITI4":"Lazio","ITF1":"Abruzzo","ITF2":"Molise",
"ITF3":"Campania","ITF4":"Puglia","ITF5":"Basilicata","ITF6":"Calabria","ITG1":"Sicilia","ITG2":"Sardegna"}
rgv=defaultdict(dict)
for r in rg:
    v=num(r["va_meur"])
    if v is not None: rgv[(r["geo"],r["nace_r2"])][int(float(r["anno"]))]=v
LASTY=max(y for y in rgv[("ITC4","C")])
print(f"(ultimo anno con dati regionali disponibili: {LASTY})")
rows_out=[]
for g,nm in regnames.items():
    c=rgv[(g,"C")].get(LASTY); t=rgv[(g,"TOTAL")].get(LASTY)
    c0=rgv[(g,"C")].get(2000); t0=rgv[(g,"TOTAL")].get(2000)
    if c and t:
        rows_out.append((nm,c,pct(c,t), pct(c0,t0) if c0 and t0 else None))
rows_out.sort(key=lambda x:-x[2])
tot_c=sum(x[1] for x in rows_out)
print(f"Peso manifattura su VA regionale ({LASTY}) e quota su VA manifatt. nazionale:")
print(f"{'Regione':18s}{'VA manif.':>12s}{'%suVAreg':>10s}{'%2000':>9s}{'%suManIT':>10s}")
for nm,c,sh,sh0 in rows_out:
    s0=f"{sh0:.1f}%" if sh0 else "-"
    print(f"{nm:18s}{c:11,.0f}M{sh:9.1f}%{s0:>9s}{pct(c,tot_c):9.1f}%")
# top 4 concentrazione
top4=sorted(rows_out,key=lambda x:-x[1])[:4]
print(f"Prime 4 regioni = {sum(pct(x[1],tot_c) for x in top4):.1f}% del VA manifatturiero italiano")
print()
reg_e=defaultdict(dict)
for r in re:
    v=num(r["occupati_migliaia"])
    if v is not None: reg_e[(r["geo"],r["nace_r2"])][int(float(r["anno"]))]=v
LASTYE=max(y for y in reg_e[("ITC4","C")])
print(f"Occupati manifattura per regione: var. {LASTYE} vs 2000")
er2=[]
for g,nm in regnames.items():
    a=reg_e[(g,"C")].get(2000); b=reg_e[(g,"C")].get(LASTYE)
    if a and b: er2.append((nm,a,b,chg(b,a)))
er2.sort(key=lambda x:x[3])
for nm,a,b,ch in er2:
    print(f"  {nm:18s} 2000={a:7,.0f} {LASTY}={b:7,.0f}  {ch:+6.1f}%")
print("\nFINE ANALISI.")
