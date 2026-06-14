import openpyxl, warnings, csv, re
from datetime import datetime, date, timedelta
warnings.filterwarnings('ignore')
SRC='/Users/lorenzoruffino/Downloads/Summer Loss/Calendars_Country_Sheets_2023_2024_0.xlsx'
OUT='/Users/lorenzoruffino/Documents/Progetti/data-viz/Vacanze scolastiche Europa/input'
JUN15=date(2024,7,1)
wb=openpyxl.load_workbook(SRC, data_only=True)

# nome paese -> codice mappa (CNTR_ID). Belgio = media delle 3 comunità.
CODE={'Austria':'AT','Belgio (fiamminga)':'BE','Belgio (francese)':'BE','Belgio (tedesca)':'BE',
 'Bulgaria':'BG','Croazia':'HR','Cipro':'CY','Cechia':'CZ','Danimarca':'DK','Estonia':'EE',
 'Finlandia':'FI','Francia':'FR','Germania':'DE','Grecia':'EL','Ungheria':'HU','Irlanda':'IE',
 'Italia':'IT','Lettonia':'LV','Lituania':'LT','Lussemburgo':'LU','Malta':'MT','Paesi Bassi':'NL',
 'Polonia':'PL','Portogallo':'PT','Romania':'RO','Slovacchia':'SK','Slovenia':'SI','Spagna':'ES',
 'Svezia':'SE','Albania':'AL','Islanda':'IS','Liechtenstein':'LI','Montenegro':'ME',
 'Macedonia del Nord':'MK','Norvegia':'NO','Serbia':'RS','Svizzera':'CH','Turchia':'TR'}
NAMES={'Albania':'Albania','Austria':'Austria','Belgium - \nFlemish Community':'Belgio (fiamminga)',
 'French Community\nof Belgium':'Belgio (francese)','German-speaking Community of Belgium':'Belgio (tedesca)',
 'Bulgaria':'Bulgaria','Croatia':'Croazia','Cyprus':'Cipro','Czechia':'Cechia','Denmark':'Danimarca',
 'Estonia':'Estonia','Finland':'Finlandia','France':'Francia','Germany':'Germania','Greece':'Grecia',
 'Hungary':'Ungheria','Iceland':'Islanda','Ireland':'Irlanda','Italy':'Italia','Latvia':'Lettonia',
 'Liechtenstein':'Liechtenstein','Lithuania':'Lituania','Luxembourg':'Lussemburgo','Malta':'Malta',
 'Montenegro':'Montenegro','North Macedonia':'Macedonia del Nord','Norway':'Norvegia','Poland':'Polonia',
 'Portugal':'Portogallo','Romania':'Romania','Serbia':'Serbia','Slovakia':'Slovacchia','Slovenia':'Slovenia',
 'Spain':'Spagna','Sweden':'Svezia','Switzerland':'Svizzera','The Netherlands':'Paesi Bassi','Türkiye':'Turchia'}
NATIONAL={'overview','national overview'}; PT_CAL='3.º ciclo do ensino básico e ensino secundário'
def pick(levels):
    for L in ('ISCED 2-3','ISCED 3','ISCED 2','ISCED 1-3','ISCED 1-2','ISCED 1'):
        if L in levels: return L
def ad(x): return x.date() if isinstance(x,datetime) else None
def minint(v):
    if isinstance(v,(int,float)): return int(v)
    if isinstance(v,str):
        m=re.search(r'\d+',v); return int(m.group()) if m else None

from collections import defaultdict
rows_by=defaultdict(list)
for r in wb['All_data'].iter_rows(values_only=True):
    if not r or r[0] is None: continue
    cal=str(r[2]).strip().lower()
    if r[0]=='Portugal':
        if str(r[2]).strip()==PT_CAL: rows_by['Portugal'].append(r)
    elif cal in NATIONAL: rows_by[r[0]].append(r)

res={}  # it -> (summer, midyear)
for country,rs in rows_by.items():
    it=NAMES.get(country)
    if not it: continue
    L=pick({str(r[1]) for r in rs})
    beg=end=None; periods=[]
    for r in rs:
        if str(r[1])!=L: continue
        pn=str(r[3])
        if 'TEACHER' in pn: continue
        if pn.startswith('BEGINNING of the school year'): beg=ad(r[4]) or ad(r[7]); continue
        if pn.startswith('END of the school year'): end=ad(r[4]) or ad(r[7]); continue
        b=ad(r[7]) or ad(r[4]); e=ad(r[8]) or b
        if not b: continue
        if e<b: e=b
        md=minint(r[5])
        if e<JUN15:
            if md and md>=1: e=b+timedelta(days=md-1)
        else:
            span=(e-b).days+1
            if md and md>=1 and span>1.5*md: e=b+timedelta(days=md-1)
        if e<b: e=b
        periods.append((b,e))
    summer=[(b,e) for (b,e) in periods if e>=JUN15]
    midy=[(b,e) for (b,e) in periods if e<JUN15]
    if summer:
        sd=max((e-b).days+1 for b,e in summer)
    elif beg and end:
        sd=((beg+timedelta(days=364))-end).days
    else: sd=0
    md_tot=sum((e-b).days+1 for b,e in midy)
    res[it]=(sd,md_tot)

# Belgio: media delle 3 comunità
be=[res[k] for k in ('Belgio (fiamminga)','Belgio (francese)','Belgio (tedesca)') if k in res]
rows=[]
seen=set()
for it,code in CODE.items():
    if code=='BE':
        if 'BE' in seen: continue
        seen.add('BE')
        s=sum(x[0] for x in be)/len(be); m=sum(x[1] for x in be)/len(be)
    else:
        if it not in res: continue
        s,m=res[it]
    tot=s+m
    rows.append((code, it.replace(' (fiamminga)','').replace(' (francese)','').replace(' (tedesca)',''), s, m, tot, round(100*s/tot,1) if tot else ''))

rows.sort(key=lambda x:-x[5] if x[5]!='' else 0)
with open(f'{OUT}/quota_estate.csv','w',newline='') as f:
    w=csv.writer(f); w.writerow(['CNTR_ID','paese','estate_gg','altre_gg','totale_gg','quota_estate'])
    for r in rows: w.writerow(r)
print(f"{'paese':20} {'estate':>6} {'altre':>6} {'tot':>5} {'quota%':>6}")
for code,p,s,m,t,q in rows:
    print(f"  {p:20} {s:6.0f} {m:6.0f} {t:5.0f} {q:6}")
