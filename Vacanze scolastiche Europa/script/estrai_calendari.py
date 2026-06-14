import openpyxl, warnings, csv, re
from datetime import datetime, date, timedelta
warnings.filterwarnings('ignore')

SRC='/Users/lorenzoruffino/Downloads/Summer Loss/Calendars_Country_Sheets_2023_2024_0.xlsx'
OUT='/Users/lorenzoruffino/Documents/Progetti/data-viz/Vacanze scolastiche Europa/input'
W0=date(2023,9,1); W1=date(2024,8,31); JUN15=date(2024,6,15)
wb=openpyxl.load_workbook(SRC, data_only=True)

NAMES={'Albania':'Albania','Austria':'Austria','Belgium - \nFlemish Community':'Belgio (fiamminga)',
 'French Community\nof Belgium':'Belgio (francese)','German-speaking Community of Belgium':'Belgio (tedesca)',
 'Bulgaria':'Bulgaria','Croatia':'Croazia','Cyprus':'Cipro','Czechia':'Cechia','Denmark':'Danimarca',
 'Estonia':'Estonia','Finland':'Finlandia','France':'Francia','Germany':'Germania','Greece':'Grecia',
 'Hungary':'Ungheria','Iceland':'Islanda','Ireland':'Irlanda','Italy':'Italia','Latvia':'Lettonia',
 'Liechtenstein':'Liechtenstein','Lithuania':'Lituania','Luxembourg':'Lussemburgo','Malta':'Malta',
 'Montenegro':'Montenegro','North Macedonia':'Macedonia del Nord','Norway':'Norvegia','Poland':'Polonia',
 'Portugal':'Portogallo','Romania':'Romania','Serbia':'Serbia','Slovakia':'Slovacchia','Slovenia':'Slovenia',
 'Spain':'Spagna','Sweden':'Svezia','Switzerland':'Svizzera','The Netherlands':'Paesi Bassi','Türkiye':'Turchia'}

NATIONAL={'overview','national overview'}
PT_CAL='3.º ciclo do ensino básico e ensino secundário'

def pick(levels):
    for L in ('ISCED 2-3','ISCED 3','ISCED 2','ISCED 1-3','ISCED 1-2','ISCED 1'):
        if L in levels: return L
def ad(x): return x.date() if isinstance(x,datetime) else None
def minint(v):
    if isinstance(v,(int,float)): return int(v)
    if isinstance(v,str):
        m=re.search(r'\d+', v); return int(m.group()) if m else None
def clip(a,b):
    a=max(a,W0); b=min(b,W1)
    return (a,b) if a<=b else None

# righe nazionali per paese
from collections import defaultdict
rows_by=defaultdict(list)
for r in wb['All_data'].iter_rows(values_only=True):
    if not r or r[0] is None: continue
    cal=str(r[2]).strip().lower()
    if r[0]=='Portugal':
        if str(r[2]).strip()==PT_CAL: rows_by['Portugal'].append(r)
    elif cal in NATIONAL:
        rows_by[r[0]].append(r)

blocks=[]; beg={}; end={}
for country, rs in rows_by.items():
    it=NAMES.get(country)
    if not it: continue
    L=pick({str(r[1]) for r in rs})
    for r in rs:
        if str(r[1])!=L: continue
        pn=str(r[3])
        if 'TEACHER' in pn: continue
        if pn.startswith('BEGINNING of the school year'): beg[it]=ad(r[4]) or ad(r[7]); continue
        if pn.startswith('END of the school year'): end[it]=ad(r[4]) or ad(r[7]); continue
        b=ad(r[7]) or ad(r[4]); e=ad(r[8]) or b
        if not b: continue
        if e<b: e=b
        md=minint(r[5])
        if e < JUN15:                       # pausa infra-anno: durata minima
            if md and md>=1: e=b+timedelta(days=md-1)
        else:                               # estate: piena, salvo paesi molto sfalsati
            span=(e-b).days+1
            if md and md>=1 and span > 1.5*md: e=b+timedelta(days=md-1)
        if e<b: e=b
        c2=clip(b,e)
        if c2: blocks.append((it,c2[0],c2[1]))

paesi=sorted(set(NAMES.values()))
AUG=date(2024,8,1)
for it in paesi:                            # estate derivata dove nessun blocco copre agosto
    if not any(b<=AUG<=e for (p,b,e) in blocks if p==it):
        if end.get(it) and beg.get(it):
            c2=clip(end[it], beg[it]+timedelta(days=363))
            if c2: blocks.append((it,c2[0],c2[1]))
for it in paesi:                            # coda estate a sinistra
    if beg.get(it) and beg[it]>W0:
        c2=clip(W0, beg[it]-timedelta(days=1))
        if c2: blocks.append((it,c2[0],c2[1]))

def off(d): return (d-W0).days
with open(f'{OUT}/blocchi_vacanze.csv','w',newline='') as f:
    w=csv.writer(f); w.writerow(['paese','start_off','end_off'])
    for it,b,e in sorted(blocks): w.writerow([it,off(b),off(e)])

print('paesi con dati:', sorted({b[0] for b in blocks}).__len__())
for it in ['Italia','Francia','Germania','Spagna','Danimarca','Paesi Bassi','Portogallo','Svezia']:
    bl=sorted((b,e) for (p,b,e) in blocks if p==it)
    print(f'=== {it} ===')
    for b,e in bl: print(f'   {b} -> {e} ({(e-b).days+1}gg)')
