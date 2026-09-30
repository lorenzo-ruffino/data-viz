# -*- coding: utf-8 -*-
"""
Deriva salariale (wage drift) come proxy dei superminimi e degli altri
elementi di retribuzione decisi fuori dal CCNL.

drift = retribuzioni di fatto per ULA (Oros, INPS)  -  retribuzioni contrattuali
        di CASSA per dipendente (indagine retribuzioni contrattuali)

Entrambe le serie sono trimestrali, per settore Ateco, universo confrontabile
(imprese con dipendenti, industria e servizi B-N).
Fonti: API SDMX Istat (esploradati.istat.it), query salvate in ../input/.
"""
import csv, collections, statistics, os

IN = os.path.join(os.path.dirname(__file__), "..", "input")
OUT = os.path.join(os.path.dirname(__file__), "..", "output")

NOMI = {'0011':'Industria (B-F)','0013':'Servizi di mercato (G-N)',
        '0015':'Industria e servizi (B-N)','0020':'Industria escl. costruzioni',
        'B':'Estrattive','C':'Manifattura','D':'Energia','E':'Acqua e rifiuti',
        'F':'Costruzioni','G':'Commercio','H':'Trasporti','I':'Alberghi e ristoranti',
        'J':'Informazione e comunicazione','K':'Finanza e assicurazioni',
        'L':'Immobiliare','M':'Professionali','N':'Servizi alle imprese',
        'P':'Istruzione','Q':'Sanità','R':'Arte e sport','S':'Altri servizi'}


def leggi(f, **filtri):
    d = collections.defaultdict(dict)
    for r in csv.DictReader(open(os.path.join(IN, f))):
        if any(r[k] != v for k, v in filtri.items()):
            continue
        d[r['ECON_ACTIVITY_NACE_2007']][r['TIME_PERIOD']] = float(r['OBS_VALUE'])
    return d


def media_annua(serie, anno):
    v = [x for t, x in serie.items() if t[:4] == anno]
    return sum(v) / 4 if len(v) == 4 else None


# --- contrattuali di cassa: raccordo base 2015 (2015-2023) e base 2021 (2021-) ---
c21 = leggi('istat_contrattuali_cassa_155_358_8.csv', PROF_STATUS_EMP='10')
c15 = leggi('istat_contrattuali_cassa_155_358_2_base2015.csv', PROF_STATUS_EMP='10')
contr = {}
for s in c21:
    ov = sorted(set(c21[s]) & set(c15.get(s, {})))
    if not ov:
        contr[s] = dict(c21[s]); continue
    k = statistics.mean(c21[s][t] / c15[s][t] for t in ov)
    contr[s] = {**{t: v * k for t, v in c15[s].items()}, **c21[s]}

oros = leggi('istat_oros_retrib_ula_155_374_4.csv')

# --- serie annuale del drift sull'aggregato B-N ---
os.makedirs(OUT, exist_ok=True)
S = '0015'
anni = [str(a) for a in range(2015, 2026)]
idx = 100.0
with open(os.path.join(OUT, 'deriva_salariale_annua.csv'), 'w', newline='') as fh:
    w = csv.writer(fh)
    w.writerow(['anno', 'var_contrattuale_pct', 'var_di_fatto_pct', 'drift_pp', 'indice_drift_2015_100'])
    w.writerow([anni[0], '', '', '', f'{idx:.1f}'])
    for prec, anno in zip(anni, anni[1:]):
        gc = (media_annua(contr[S], anno) / media_annua(contr[S], prec) - 1) * 100
        go = (media_annua(oros[S], anno) / media_annua(oros[S], prec) - 1) * 100
        idx *= (1 + go / 100) / (1 + gc / 100)
        w.writerow([anno, f'{gc:.2f}', f'{go:.2f}', f'{go - gc:.2f}', f'{idx:.1f}'])

# --- drift per settore, cumulato 2021-2025 (dentro il settore = niente
#     effetto di composizione fra settori) ---
righe = []
for s in sorted(set(contr) & set(oros)):
    a, b = media_annua(contr[s], '2021'), media_annua(contr[s], '2025')
    x, y = media_annua(oros[s], '2021'), media_annua(oros[s], '2025')
    if None in (a, b, x, y):
        continue
    gc, go = (b / a - 1) * 100, (y / x - 1) * 100
    righe.append([NOMI.get(s, s), round(gc, 1), round(go, 1), round(go - gc, 1)])
righe.sort(key=lambda r: -r[3])
with open(os.path.join(OUT, 'deriva_salariale_settori_2021_2025.csv'), 'w', newline='') as fh:
    w = csv.writer(fh)
    w.writerow(['settore', 'var_contrattuale_pct', 'var_di_fatto_pct', 'drift_pp'])
    w.writerows(righe)

print('scritti output/deriva_salariale_annua.csv e output/deriva_salariale_settori_2021_2025.csv')
