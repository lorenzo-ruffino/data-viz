#!/opt/homebrew/bin/python3
# -*- coding: utf-8 -*-
"""
Estrazione appendice statistica ISTAT "Demografia d'impresa" - edizione 2014-2019.
Sorgente: /Users/lorenzoruffino/Desktop/IMPRESE/TAVOLE_DEMOGRAFIA2021.xlsx
Output CSV tidy/long in input/raw/ prefissati ed1419_.
Rieseguibile. Engine openpyxl. NON modifica l'originale.
"""
import os
import re
import openpyxl
import pandas as pd

EDIZIONE = "2014-2019"
PREFIX = "ed1419_"
SRC = "/Users/lorenzoruffino/Desktop/IMPRESE/TAVOLE_DEMOGRAFIA2021.xlsx"
OUTDIR = "/Users/lorenzoruffino/Documents/Progetti/data-viz/Demografia imprese/input/raw"

os.makedirs(OUTDIR, exist_ok=True)
wb = openpyxl.load_workbook(SRC, data_only=True, read_only=True)

# ----------------------------------------------------------------------------- helpers
def get_ws(name):
    """Match foglio per nome stripped (i nomi possono avere spazi finali)."""
    for w in wb.worksheets:
        if w.title.strip() == name.strip():
            return w
    raise KeyError(f"Foglio non trovato: {name!r}")

def grid(ws):
    """Ritorna una matrice 1-based [r][c] dei valori del foglio."""
    rows = list(ws.iter_rows(values_only=True))
    nrow = len(rows)
    ncol = max((len(r) for r in rows), default=0)
    g = {}
    for i, row in enumerate(rows, start=1):
        for j, v in enumerate(row, start=1):
            g[(i, j)] = v
    return g, nrow, ncol

def C(g, r, c):
    return g.get((r, c))

def sstrip(v):
    return v.strip() if isinstance(v, str) else v

YEAR_RE = re.compile(r"(\d{4})")

def parse_year(v):
    """(anno:int|None, provvisorio:bool). provvisorio se '*' o '(a)'."""
    if v is None or isinstance(v, bool):
        return None, False
    if isinstance(v, int):
        return v, False
    if isinstance(v, float):
        if v != v:  # NaN
            return None, False
        return int(v), False
    st = str(v).strip()
    prov = ("*" in st) or ("(a)" in st.lower())
    m = YEAR_RE.search(st)
    return (int(m.group(1)) if m else None), prov

def is_year(v, lo=2000, hi=2100):
    """Cella-anno: numero in range, o stringa che INIZIA con 4 cifre (es. '2019(a)').
    Cosi titoli ('Tavola 1 - ... 2014-2019') e note ('(a) ... 2019') non matchano."""
    if isinstance(v, bool):
        return False
    if isinstance(v, (int, float)):
        if isinstance(v, float) and v != v:
            return False
        return lo <= int(v) <= hi
    if isinstance(v, str):
        m = re.match(r"^(\d{4})", v.strip())
        return bool(m) and (lo <= int(m.group(1)) <= hi)
    return False

def is_note(v):
    return isinstance(v, str) and v.strip().startswith("(")

def is_title(v):
    return isinstance(v, str) and v.strip().lower().startswith("tavola")

written = []  # (path, df, descrizione, anni)

def write_csv(fname, df, descrizione, anni):
    path = os.path.join(OUTDIR, fname)
    df.to_csv(path, index=False, encoding="utf-8")
    written.append((path, df, descrizione, anni))
    return path

# ----------------------------------------------------------------------------- Tavola 1
def tavola1():
    g, nrow, ncol = grid(get_ws("Tavola 1"))
    recs = []
    for r in range(1, nrow + 1):
        v1 = C(g, r, 1)
        if not is_year(v1):
            continue
        anno, prov = parse_year(v1)
        recs.append({
            "edizione": EDIZIONE, "anno": anno, "provvisorio": prov,
            "tasso_natalita": C(g, r, 2), "imprese_nate": C(g, r, 3),
            "tasso_mortalita": C(g, r, 4), "imprese_cessate": C(g, r, 5),
            "turnover_netto": C(g, r, 6),
        })
    df = pd.DataFrame(recs, columns=["edizione", "anno", "provvisorio",
        "tasso_natalita", "imprese_nate", "tasso_mortalita", "imprese_cessate", "turnover_netto"])
    return write_csv(f"{PREFIX}totale_annuale.csv", df, "TOTALE economia per anno", "2014-2019")

# ----------------------------------------------------------------------------- Tavola 2
def tavola2():
    g, nrow, ncol = grid(get_ws("Tavola 2"))
    recs = []
    macro = None
    for r in range(1, nrow + 1):
        v1 = C(g, r, 1)
        if v1 is None:
            continue
        if is_title(v1) or is_note(v1):
            continue
        if isinstance(v1, str) and v1.strip() == "Anni":
            continue  # riga intestazione colonne
        if is_year(v1):
            anno, prov = parse_year(v1)
            recs.append({
                "edizione": EDIZIONE, "macrosettore": macro, "anno": anno, "provvisorio": prov,
                "tasso_natalita": C(g, r, 2), "imprese_nate": C(g, r, 3),
                "tasso_mortalita": C(g, r, 4), "imprese_cessate": C(g, r, 5),
                "turnover_netto": C(g, r, 6),
            })
        else:
            macro = sstrip(v1)  # intestazione blocco macrosettore
    df = pd.DataFrame(recs, columns=["edizione", "macrosettore", "anno", "provvisorio",
        "tasso_natalita", "imprese_nate", "tasso_mortalita", "imprese_cessate", "turnover_netto"])
    return write_csv(f"{PREFIX}macrosettori.csv", df, "Macrosettori per anno", "2014-2019")

# ----------------------------------------------------------------------------- Tavola 3
def tavola3():
    g, nrow, ncol = grid(get_ws("Tavola 3"))
    # riga intestazione anni: c1 == 'SETTORI ECONOMICI'
    hdr = None
    for r in range(1, nrow + 1):
        if isinstance(C(g, r, 1), str) and C(g, r, 1).strip().upper() == "SETTORI ECONOMICI":
            hdr = r
            break
    # anni: ogni anno occupa 4 colonne, base = 2 + j*4
    years = []
    j = 0
    while True:
        col = 2 + j * 4
        v = C(g, hdr, col)
        if not is_year(v):
            break
        years.append((col,) + parse_year(v))  # (col_base, anno, prov)
        j += 1
    data_start = hdr + 2  # hdr, sottointestazioni, poi dati
    recs = []
    for r in range(data_start, nrow + 1):
        name = C(g, r, 1)
        if name is None or is_note(name) or is_title(name):
            continue
        settore = sstrip(name)
        for base, anno, prov in years:
            recs.append({
                "edizione": EDIZIONE, "settore": settore, "anno": anno, "provvisorio": prov,
                "tasso_natalita": C(g, r, base), "imprese_nate": C(g, r, base + 1),
                "tasso_mortalita": C(g, r, base + 2), "imprese_cessate": C(g, r, base + 3),
            })
    df = pd.DataFrame(recs, columns=["edizione", "settore", "anno", "provvisorio",
        "tasso_natalita", "imprese_nate", "tasso_mortalita", "imprese_cessate"])
    return write_csv(f"{PREFIX}settori_tecnologia.csv", df, "Settori per intensita tecnologica/conoscenza", "2014-2019")

# ----------------------------------------------------------------------------- Tavola 4
def tavola4():
    g, nrow, ncol = grid(get_ws("Tavola 4"))
    hdr = None
    for r in range(1, nrow + 1):
        if isinstance(C(g, r, 1), str) and C(g, r, 1).strip().lower().startswith("aree geografiche"):
            hdr = r
            break
    # anni: 3 colonne per anno, base = 2 + j*3
    years = []
    j = 0
    while True:
        col = 2 + j * 3
        v = C(g, hdr, col)
        if not is_year(v):
            break
        years.append((col,) + parse_year(v))
        j += 1
    data_start = hdr + 2
    recs = []
    for r in range(data_start, nrow + 1):
        name = C(g, r, 1)
        if name is None or is_note(name) or is_title(name):
            continue
        territorio = sstrip(name)
        for base, anno, prov in years:
            recs.append({
                "edizione": EDIZIONE, "territorio": territorio, "anno": anno, "provvisorio": prov,
                "tasso_natalita": C(g, r, base), "tasso_mortalita": C(g, r, base + 1),
                "turnover_netto": C(g, r, base + 2),
            })
    df = pd.DataFrame(recs, columns=["edizione", "territorio", "anno", "provvisorio",
        "tasso_natalita", "tasso_mortalita", "turnover_netto"])
    return write_csv(f"{PREFIX}regioni.csv", df, "Regioni e ripartizioni per anno", "2014-2019")

# ----------------------------------------------------------------------------- Tavola 5
def tavola5():
    g, nrow, ncol = grid(get_ws("Tavola 5"))
    hdr = None
    for r in range(1, nrow + 1):
        if isinstance(C(g, r, 1), str) and C(g, r, 1).strip().lower() == "macrosettori":
            hdr = r
            break
    # colonne anno di osservazione: da col3 in poi, quelle con un anno
    obs_cols = []
    for c in range(3, ncol + 1):
        v = C(g, hdr, c)
        if is_year(v):
            obs_cols.append((c, parse_year(v)[0]))
    recs = []
    macro = None
    for r in range(hdr + 1, nrow + 1):
        v1 = C(g, r, 1)
        if isinstance(v1, str) and (is_note(v1) or is_title(v1)):
            continue
        if v1 is not None and not is_note(v1):
            macro = sstrip(v1)
        coorte, _ = parse_year(C(g, r, 2))
        if coorte is None:
            continue  # riga vuota tra blocchi
        for c, obs_year in obs_cols:
            val = C(g, r, c)
            if val is None:
                continue  # cella vuota: struttura triangolare, non inventare
            recs.append({
                "edizione": EDIZIONE, "macrosettore": macro, "coorte": coorte,
                "anno_osservazione": obs_year, "anni_dalla_nascita": obs_year - coorte,
                "tasso_sopravvivenza": val,
            })
    df = pd.DataFrame(recs, columns=["edizione", "macrosettore", "coorte",
        "anno_osservazione", "anni_dalla_nascita", "tasso_sopravvivenza"])
    return write_csv(f"{PREFIX}sopravvivenza.csv", df, "Sopravvivenza coorti 2014-2018", "2014-2018 (oss. 2015-2019)")

# ----------------------------------------------------------------------------- Tavola 6
def tavola6():
    g, nrow, ncol = grid(get_ws("Tavola 6"))
    # riga sottointestazione formule: c2 == '( a )'
    sub = None
    for r in range(1, nrow + 1):
        v2 = C(g, r, 2)
        if isinstance(v2, str) and v2.replace(" ", "").lower() == "(a)":
            sub = r
            break
    recs = []
    for r in range(sub + 1, nrow + 1):
        name = C(g, r, 1)
        if name is None or is_note(name) or is_title(name):
            continue
        recs.append({
            "edizione": EDIZIONE, "coorte": 2014, "macrosettore": sstrip(name),
            "addetti_t0_nate": C(g, r, 2),
            "addetti_t0_sopravviventi": C(g, r, 3),
            "addetti_t5_sopravviventi": C(g, r, 4),
            "perdita_pct_da_cessazioni": C(g, r, 5),
            "crescita_pct_sopravviventi": C(g, r, 6),
            "variazione_pct_netta": C(g, r, 7),
        })
    df = pd.DataFrame(recs, columns=["edizione", "coorte", "macrosettore",
        "addetti_t0_nate", "addetti_t0_sopravviventi", "addetti_t5_sopravviventi",
        "perdita_pct_da_cessazioni", "crescita_pct_sopravviventi", "variazione_pct_netta"])
    return write_csv(f"{PREFIX}addetti_coorte2014.csv", df, "Addetti coorte 2014 (t0 2014 -> t5 2019)", "2014-2019")

# ----------------------------------------------------------------------------- grafico web
def grafico_web():
    g, nrow, ncol = grid(get_ws("grafico web"))
    recs = []
    for r in range(1, nrow + 1):
        v1 = C(g, r, 1)
        if not is_year(v1):
            continue
        anno, _ = parse_year(v1)
        nat = C(g, r, 2)
        mor = C(g, r, 3)
        recs.append({
            "edizione": EDIZIONE, "anno": anno,
            # frazioni nel foglio -> *100 per portarli in percentuale
            "tasso_natalita": (nat * 100) if nat is not None else None,
            "tasso_mortalita": (mor * 100) if mor is not None else None,
        })
    df = pd.DataFrame(recs, columns=["edizione", "anno", "tasso_natalita", "tasso_mortalita"])
    return write_csv(f"{PREFIX}serie_tassi_2006_2019.csv", df,
                     "Serie nazionale tassi 2006-2019 (frazioni x100 -> percentuale)", "2006-2019")

# ----------------------------------------------------------------------------- run
tavola1(); tavola2(); tavola3(); tavola4(); tavola5(); tavola6(); grafico_web()

# ----------------------------------------------------------------------------- verifica
print("=" * 70)
for path, df, desc, anni in written:
    print(f"{os.path.basename(path)}  shape={df.shape}  [{anni}]  {desc}")

def rb(name):
    return pd.read_csv(os.path.join(OUTDIR, name))

checks = []
def chk(label, got, exp):
    ok = (got == exp) if not isinstance(exp, float) else (float(got) == exp)
    checks.append(ok)
    print(f"[{'OK' if ok else 'FAIL'}] {label}: got={got!r} exp={exp!r}")

t1 = rb(f"{PREFIX}totale_annuale.csv")
chk("T1 2014 imprese_nate", int(t1.loc[t1.anno == 2014, "imprese_nate"].iloc[0]), 274489)
chk("T1 2019 tasso_natalita", t1.loc[t1.anno == 2019, "tasso_natalita"].iloc[0], 7.390716226679621)
chk("T1 2019 imprese_cessate", int(t1.loc[t1.anno == 2019, "imprese_cessate"].iloc[0]), 296917)
chk("T1 2019 provvisorio", bool(t1.loc[t1.anno == 2019, "provvisorio"].iloc[0]), True)

t2 = rb(f"{PREFIX}macrosettori.csv")
chk("T2 Industria 2014 tasso_natalita",
    t2[(t2.macrosettore == "Industria in senso stretto") & (t2.anno == 2014)]["tasso_natalita"].iloc[0], 4.7)
chk("T2 Costruzioni 2019 tasso_natalita",
    t2[(t2.macrosettore == "Costruzioni") & (t2.anno == 2019)]["tasso_natalita"].iloc[0], 8.205367091932535)

t3 = rb(f"{PREFIX}settori_tecnologia.csv")
chk("T3 HITS 2014 tasso_natalita",
    t3[(t3.settore == "Servizi tecnologici ad Alto contenuto di conoscenza (HITS)") & (t3.anno == 2014)]["tasso_natalita"].iloc[0],
    10.638773151520448)
chk("T3 HT 2014 imprese_nate",
    int(t3[(t3.settore == "Manifatture ad  Alta tecnologia (HT)") & (t3.anno == 2014)]["imprese_nate"].iloc[0]), 712)

t4 = rb(f"{PREFIX}regioni.csv")
chk("T4 Lazio 2014 tasso_natalita",
    t4[(t4.territorio == "Lazio") & (t4.anno == 2014)]["tasso_natalita"].iloc[0], 9.2)
chk("T4 Piemonte 2014 tasso_natalita",
    t4[(t4.territorio == "Piemonte") & (t4.anno == 2014)]["tasso_natalita"].iloc[0], 6.3)

t5 = rb(f"{PREFIX}sopravvivenza.csv")
chk("T5 Industria coorte2014 oss2019",
    t5[(t5.macrosettore == "Industria in s.s.") & (t5.coorte == 2014) & (t5.anno_osservazione == 2019)]["tasso_sopravvivenza"].iloc[0],
    51.7)

t6 = rb(f"{PREFIX}addetti_coorte2014.csv")
chk("T6 Industria addetti_t0_nate (a)",
    t6[t6.macrosettore == "Industria in senso stretto"]["addetti_t0_nate"].iloc[0], 34357.0300000013)

t7 = rb(f"{PREFIX}serie_tassi_2006_2019.csv")
chk("T7 2006 tasso_natalita x100",
    t7[t7.anno == 2006]["tasso_natalita"].iloc[0], 0.0713846075033129 * 100)

print("=" * 70)
print("TUTTI I CONTROLLI OK" if all(checks) else "!!! CONTROLLI FALLITI !!!")
