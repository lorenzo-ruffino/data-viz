#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Estrazione dati appendice ISTAT "Demografia d'impresa" (report 2011, dati 2004-2009).
File sorgente: /Users/lorenzoruffino/Desktop/IMPRESE/tavole_SB.xls  (engine xlrd)
Edizione: 2004-2009  -> prefisso CSV: ed0409

Output tidy/long in .../Demografia imprese/input/raw/ , UTF-8, sep=",", decimale ".".
Rieseguibile: sovrascrive i CSV a ogni run.
"""
import os
import re
import pandas as pd

SRC = "/Users/lorenzoruffino/Desktop/IMPRESE/tavole_SB.xls"
OUTDIR = "/Users/lorenzoruffino/Documents/Progetti/data-viz/Demografia imprese/input/raw"
EDIZIONE = "2004-2009"
PFX = "ed0409"

os.makedirs(OUTDIR, exist_ok=True)
xls = pd.ExcelFile(SRC, engine="xlrd")
# mappa nome-stripped -> nome-reale (i fogli possono avere spazi finali)
SHEETMAP = {s.strip(): s for s in xls.sheet_names}


def sheet(name):
    return pd.read_excel(xls, sheet_name=SHEETMAP[name.strip()], header=None)


def isna(v):
    return pd.isna(v)


def parse_year(v):
    """Ritorna (anno_int|None, provvisorio_bool). '2009*' o '2024(a)' -> provvisorio=True."""
    if isna(v):
        return None, False
    s = str(v).strip()
    prov = ("*" in s) or ("(a)" in s)
    m = re.search(r"(\d{4})", s)
    return (int(m.group(1)) if m else None), prov


def split_settore(label):
    """('B','Estrazione...'), ('10-12','Industrie...'), ('','Totale')."""
    s = str(label).strip()
    m = re.match(r"^([0-9]+(?:\s*-\s*[0-9]+)?|[A-Za-z]+)\s*-\s*(.+)$", s)
    if m:
        return m.group(1).strip(), m.group(2).strip()
    return "", s


def norm_class(v):
    """Normalizza etichetta classe di addetti: 0/0.0 -> '0', altrimenti stringa strip."""
    if isinstance(v, bool):
        return str(v)
    if isinstance(v, (int,)):
        return str(v)
    if isinstance(v, float) and float(v).is_integer():
        return str(int(v))
    return str(v).strip()


def fnum(v):
    """float o None (per celle vuote)."""
    return None if isna(v) else float(v)


def inum(v):
    """int o None (per conteggi interi)."""
    if isna(v):
        return None
    f = float(v)
    return int(round(f)) if f == int(f) else f


def write_csv(df, fname, int_cols=None):
    path = os.path.join(OUTDIR, fname)
    if int_cols:
        for c in int_cols:
            if c in df.columns:
                df[c] = df[c].astype("Int64")
    df.to_csv(path, index=False, encoding="utf-8")
    return path


manifest = []      # (path, righe, descrizione, anni)
anomalie = []
notes = []


# ---------------------------------------------------------------------------
# 1) Tavola 1 -> ed0409_macrosettori.csv
# ---------------------------------------------------------------------------
def t1():
    df = sheet("Tavola 1")
    rows = []
    macro = None
    for r in range(df.shape[0]):
        c0 = df.iat[r, 0]
        if isna(c0):
            continue
        year, prov = parse_year(c0)
        if year is not None and not isna(df.iat[r, 1]):
            rows.append({
                "edizione": EDIZIONE,
                "macrosettore": macro,
                "anno": year,
                "provvisorio": prov,
                "tasso_natalita": fnum(df.iat[r, 1]),
                "imprese_nate": inum(df.iat[r, 2]),
                "tasso_mortalita": fnum(df.iat[r, 3]),
                "imprese_cessate": inum(df.iat[r, 4]),
                "turnover_netto": fnum(df.iat[r, 5]),
            })
        else:
            # header di blocco / nota: aggiorna macro solo se le colonne valore sono vuote
            if all(isna(df.iat[r, c]) for c in range(1, 6)):
                macro = str(c0).strip()
    out = pd.DataFrame(rows)
    p = write_csv(out, f"{PFX}_macrosettori.csv",
                  int_cols=["imprese_nate", "imprese_cessate"])
    manifest.append((p, len(out), "Tavola 1: tassi natalita/mortalita per macrosettore", "2004-2009"))
    # note in fondo al foglio (r40, r41)
    anomalie.append("Tavola 1: nota nel foglio - 'Stima per le cessate al 2009'; "
                    "'Natalita e mortalita 2007-2009 classificate secondo la nuova classificazione NACE Rev.2' "
                    "(dati 2007-2009 in ATECO 2007 / NACE Rev.2). Nota informativa.")
    return out


# ---------------------------------------------------------------------------
# 2) Tavola 2 -> ed0409_settori_nace.csv
# ---------------------------------------------------------------------------
def t2():
    df = sheet("Tavola 2")
    # header: r2 col1=2008, col4='2009*' ; r3 sottointestazioni nat/mor
    blocks = []  # (anno, prov, col_nat, col_mor)
    for c in range(1, df.shape[1]):
        y, prov = parse_year(df.iat[2, c])
        if y is not None:
            blocks.append((y, prov, c, c + 1))
    rows = []
    for r in range(5, 35):
        lab = df.iat[r, 0]
        if isna(lab):
            continue
        code, name = split_settore(lab)
        for (anno, prov, cn, cm) in blocks:
            rows.append({
                "edizione": EDIZIONE,
                "settore_codice": code,
                "settore_nome": name,
                "anno": anno,
                "provvisorio": prov,
                "tasso_natalita": fnum(df.iat[r, cn]),
                "tasso_mortalita": fnum(df.iat[r, cm]),
            })
    out = pd.DataFrame(rows)
    p = write_csv(out, f"{PFX}_settori_nace.csv")
    manifest.append((p, len(out), "Tavola 2: natalita/mortalita per settore NACE Rev.2", "2008-2009"))
    return out


# ---------------------------------------------------------------------------
# 3) Tavola 3 -> ed0409_regioni.csv
# ---------------------------------------------------------------------------
def t3():
    df = sheet("Tavola 3")
    blocks = []  # (anno, prov, col_nat, col_mor)
    for c in range(1, df.shape[1]):
        y, prov = parse_year(df.iat[2, c])
        if y is not None:
            blocks.append((y, prov, c, c + 1))
    rows = []
    for r in range(4, 30):
        terr = df.iat[r, 0]
        if isna(terr):
            continue
        terr = str(terr).strip()
        for (anno, prov, cn, cm) in blocks:
            rows.append({
                "edizione": EDIZIONE,
                "territorio": terr,
                "anno": anno,
                "provvisorio": prov,
                "tasso_natalita": fnum(df.iat[r, cn]),
                "tasso_mortalita": fnum(df.iat[r, cm]),
            })
    out = pd.DataFrame(rows)
    p = write_csv(out, f"{PFX}_regioni.csv")
    manifest.append((p, len(out), "Tavola 3: natalita/mortalita per regione e ripartizione", "2004-2009"))
    return out


# ---------------------------------------------------------------------------
# 4) Tavola 4 -> ed0409_sopravvivenza.csv
# ---------------------------------------------------------------------------
def t4():
    df = sheet("Tavola 4")
    # header r3: cols 2..6 -> anni di osservazione 2005..2009
    obs_cols = {}
    for c in range(2, 7):
        y, _ = parse_year(df.iat[3, c])
        if y is not None:
            obs_cols[c] = y
    rows = []
    macro = None
    for r in range(4, df.shape[0]):
        c0 = df.iat[r, 0]
        if not isna(c0):
            macro = str(c0).strip()
        coorte, _ = parse_year(df.iat[r, 1])
        if coorte is None:
            continue
        for c, obs_year in obs_cols.items():
            v = df.iat[r, c]
            if isna(v):
                continue
            rows.append({
                "edizione": EDIZIONE,
                "macrosettore": macro,
                "coorte": coorte,
                "anno_osservazione": obs_year,
                "anni_dalla_nascita": obs_year - coorte,
                "tasso_sopravvivenza": fnum(v),
            })
    out = pd.DataFrame(rows)
    p = write_csv(out, f"{PFX}_sopravvivenza.csv")
    manifest.append((p, len(out), "Tavola 4: tassi di sopravvivenza per macrosettore e coorte", "2005-2009"))
    return out


# ---------------------------------------------------------------------------
# 5) Tavola 5 -> ed0409_dimensione_media_macrosettori.csv
# 6) Tavola 6 -> ed0409_dimensione_media_ripartizioni.csv
# ---------------------------------------------------------------------------
def t5_t6(name, key, fname, desc):
    df = sheet(name)
    # trova header (riga con anni) e titolo (riga con 'Tavola')
    title = None
    hdr_row = None
    for r in range(df.shape[0]):
        c0 = df.iat[r, 0]
        if not isna(c0) and str(c0).strip().lower().startswith("tavola"):
            title = str(c0).strip()
        yrs = [parse_year(df.iat[r, c])[0] for c in range(1, df.shape[1])]
        if hdr_row is None and sum(1 for y in yrs if y is not None) >= 5 and (isna(c0) or not str(c0).strip().lower().startswith("tavola")):
            # riga con >=5 anni e col0 e' un'intestazione testuale (non 'Tavola')
            if not isna(c0):
                hdr_row = r
    year_cols = {}
    for c in range(1, df.shape[1]):
        y, _ = parse_year(df.iat[hdr_row, c])
        if y is not None:
            year_cols[c] = y
    rows = []
    for r in range(hdr_row + 1, df.shape[0]):
        lab = df.iat[r, 0]
        if isna(lab):
            continue
        lab = str(lab).strip()
        for c, y in year_cols.items():
            v = df.iat[r, c]
            if isna(v):
                continue
            rows.append({
                "edizione": EDIZIONE,
                key: lab,
                "anno": y,
                "addetti_medi": fnum(v),
            })
    out = pd.DataFrame(rows)
    p = write_csv(out, fname)
    manifest.append((p, len(out), desc, "2004-2009"))
    if title:
        notes.append(f"{name} titolo: {title}")
    return out


# ---------------------------------------------------------------------------
# 7) Tavola 7 -> ed0409_addetti_coorte2004.csv
# ---------------------------------------------------------------------------
def t7():
    df = sheet("Tavola 7")
    rows = []
    for r in range(6, 11):
        lab = df.iat[r, 0]
        if isna(lab):
            continue
        rows.append({
            "edizione": EDIZIONE,
            "coorte": 2004,
            "macrosettore": str(lab).strip(),
            "addetti_t0_nate": inum(df.iat[r, 1]),
            "addetti_t0_sopravviventi": inum(df.iat[r, 2]),
            "addetti_t5_sopravviventi": inum(df.iat[r, 3]),
            "perdita_pct_da_cessazioni": fnum(df.iat[r, 4]),
            "crescita_pct_sopravviventi": fnum(df.iat[r, 5]),
            "variazione_pct_netta": fnum(df.iat[r, 6]),
        })
    out = pd.DataFrame(rows)
    p = write_csv(out, f"{PFX}_addetti_coorte2004.csv",
                  int_cols=["addetti_t0_nate", "addetti_t0_sopravviventi", "addetti_t5_sopravviventi"])
    manifest.append((p, len(out), "Tavola 7: addetti coorte 2004 (nate, sopravviventi t0 e t5)", "2004 e 2009"))
    return out


# ---------------------------------------------------------------------------
# 8) Figura3 -> ed0409_stock_settori_2009.csv (blocco IMPRESE cols 0-6)
#             -> ed0409_addetti_demografia_settori_2009.csv (blocco ADDETTI cols 9-15)
#             -> ed0409_addetti_settori_2009.csv (blocco r43-r72 cols 0-2, tassi turnover)
# ---------------------------------------------------------------------------
def fig3():
    df = sheet("Figura3")

    # blocco IMPRESE (cols 0-6, r6-r35)
    rows = []
    for r in range(6, 36):
        code = df.iat[r, 0]
        if isna(code):
            continue
        rows.append({
            "edizione": EDIZIONE,
            "settore_codice": str(code).strip(),
            "stock_2009": inum(df.iat[r, 1]),
            "nate_2009": inum(df.iat[r, 2]),
            "morte_2009_stima": inum(df.iat[r, 3]),
            "tasso_natalita": fnum(df.iat[r, 4]),
            "tasso_mortalita": fnum(df.iat[r, 5]),
            "turnover": fnum(df.iat[r, 6]),
        })
    out1 = pd.DataFrame(rows)
    p1 = write_csv(out1, f"{PFX}_stock_settori_2009.csv",
                   int_cols=["stock_2009", "nate_2009", "morte_2009_stima"])
    manifest.append((p1, len(out1), "Figura3 blocco IMPRESE: stock/nate/morte/tassi/turnover imprese 2009", "2009"))

    # blocco ADDETTI demografia (cols 9-15, r6-r35) - dato unico non presente altrove
    rows = []
    for r in range(6, 36):
        code = df.iat[r, 9]
        if isna(code):
            continue
        rows.append({
            "edizione": EDIZIONE,
            "settore_codice": str(code).strip(),
            "stock_2009": inum(df.iat[r, 10]),
            "nate_2009": inum(df.iat[r, 11]),
            "morte_2009_stima": inum(df.iat[r, 12]),
            "tasso_natalita": fnum(df.iat[r, 13]),
            "tasso_mortalita": fnum(df.iat[r, 14]),
            "turnover": fnum(df.iat[r, 15]),
        })
    out2 = pd.DataFrame(rows)
    p2 = write_csv(out2, f"{PFX}_addetti_demografia_settori_2009.csv",
                   int_cols=["stock_2009", "nate_2009", "morte_2009_stima"])
    manifest.append((p2, len(out2),
                     "Figura3 blocco ADDETTI (cols 9-15): stock/nate/morte/tassi/turnover ADDETTI 2009 "
                     "(struttura identica al blocco IMPRESE ma riferita agli addetti; dato unico)", "2009"))

    # blocco r43-r72 (cols 0-2): Nace rev2 | Addetti | Imprese  (tassi di turnover)
    rows = []
    for r in range(43, 73):
        lab = df.iat[r, 0]
        if isna(lab):
            continue
        code, name = split_settore(lab)
        rows.append({
            "edizione": EDIZIONE,
            "settore_codice": code,
            "settore_nome": name,
            "addetti": fnum(df.iat[r, 1]),
            "imprese": fnum(df.iat[r, 2]),
        })
    out3 = pd.DataFrame(rows)
    p3 = write_csv(out3, f"{PFX}_addetti_settori_2009.csv")
    manifest.append((p3, len(out3),
                     "Figura3 blocco r43-r72: intestazione 'Nace rev2 | Addetti | Imprese'. "
                     "'addetti'=tasso di turnover degli addetti, 'imprese'=tasso di turnover delle imprese (2009)", "2009"))

    # intestazione esatta blocco r41 + verifica semantica colonne
    notes.append("Figura3 blocco r41-r72 intestazione esatta: 'Nace rev2' | 'Addetti' | 'Imprese'. "
                 "Le due colonne sono i tassi di turnover (natalita+mortalita) 2009: la colonna 'Addetti' "
                 "coincide col turnover del blocco ADDETTI (col.15), la colonna 'Imprese' col turnover del "
                 "blocco IMPRESE (col.6). Sono percentuali.")
    # valore orfano r36 col11
    v_orph = df.iat[36, 11]
    if not isna(v_orph):
        anomalie.append(f"Figura3: valore orfano {inum(v_orph)} in r36 col11 (sotto la riga 'Totale', "
                        f"colonna nate09 addetti), privo di etichetta -> escluso.")
    return out1, out2, out3


# ---------------------------------------------------------------------------
# 9) Figura4 -> ed0409_sopravvivenza_ripartizioni_coorte2004.csv
# ---------------------------------------------------------------------------
def fig4():
    df = sheet("Figura4")
    # header r5: col1..col6 -> 2004..2009
    hdr = 5
    year_cols = {}
    for c in range(1, df.shape[1]):
        y, _ = parse_year(df.iat[hdr, c])
        if y is not None:
            year_cols[c] = y
    rows = []
    for r in range(hdr + 1, df.shape[0]):
        lab = df.iat[r, 0]
        if isna(lab):
            continue
        lab = str(lab).strip()
        if lab.lower().startswith("figura"):
            continue
        for c, y in year_cols.items():
            v = df.iat[r, c]
            if isna(v):
                continue
            rows.append({
                "edizione": EDIZIONE,
                "ripartizione": lab,
                "anno": y,
                "quota_sopravviventi": fnum(v),
            })
    out = pd.DataFrame(rows)
    p = write_csv(out, f"{PFX}_sopravvivenza_ripartizioni_coorte2004.csv")
    manifest.append((p, len(out), "Figura4: quota sopravviventi coorte 2004 per ripartizione (indice base 2004=1)", "2004-2009"))
    return out


# ---------------------------------------------------------------------------
# 10) Figura5 -> ed0409_sopravvivenza_macrosettori_coorte2004_indice.csv
# ---------------------------------------------------------------------------
def fig5():
    df = sheet("Figura5")
    hdr = 3
    year_cols = {}
    for c in range(1, 7):
        y, _ = parse_year(df.iat[hdr, c])
        if y is not None:
            year_cols[c] = y
    rows = []
    for r in range(4, 9):
        lab = df.iat[r, 0]
        if isna(lab):
            continue
        lab = str(lab).strip()
        for c, y in year_cols.items():
            v = df.iat[r, c]
            if isna(v):
                continue
            rows.append({
                "edizione": EDIZIONE,
                "macrosettore": lab,
                "anno": y,
                "quota_sopravviventi": fnum(v),
            })
    out = pd.DataFrame(rows)
    p = write_csv(out, f"{PFX}_sopravvivenza_macrosettori_coorte2004_indice.csv")
    manifest.append((p, len(out), "Figura5: indice di sopravvivenza coorte 2004 per macrosettore (base 2004=1)", "2004-2009"))
    # coefficienti di correlazione ausiliari (anno in col11, coeff in col16) - documentati, non su CSV
    corr = {}
    for r in range(4, 25):
        y, _ = parse_year(df.iat[r, 11])
        v = df.iat[r, 16]
        if y is not None and not isna(v):
            corr[y] = float(v)
    extra = df.iat[27, 15]
    notes.append("Figura5: oltre all'indice principale il foglio contiene un blocco ausiliario "
                 "'COEFFICIENTE DI CORRELAZIONE' (dimensione media vs sopravvivenza) con i coefficienti per anno: "
                 + ", ".join(f"{y}={corr[y]}" for y in sorted(corr))
                 + (f"; coeff. aggiuntivo={float(extra)}" if not isna(extra) else "")
                 + ". Piu un blocco di valori arrotondati per il grafico (duplicati di Tavola 5 e dell'indice). Non esportati (derivati).")
    return out


# ---------------------------------------------------------------------------
# 11) Figura6 -> ed0409_addetti_coorte2004_percorso.csv
# ---------------------------------------------------------------------------
def fig6():
    df = sheet("Figura6")
    # macrosettori in r2 alle col 2,4,6,8 ; per ciascuno 2 colonne (nascita, creati dopo)
    macro_cols = {}
    for c in range(2, df.shape[1], 2):
        m = df.iat[2, c]
        if not isna(m):
            macro_cols[str(m).strip()] = (c, c + 1)
    rows = []
    for r in range(5, df.shape[0]):
        y, _ = parse_year(df.iat[r, 1])
        if y is None:
            continue
        for macro, (cn, cd) in macro_cols.items():
            qn = df.iat[r, cn]
            qd = df.iat[r, cd]
            if isna(qn) and isna(qd):
                continue
            rows.append({
                "edizione": EDIZIONE,
                "macrosettore": macro,
                "anno": y,
                "quota_addetti_alla_nascita": fnum(qn),
                "quota_addetti_creati_dopo": fnum(qd),
            })
    out = pd.DataFrame(rows)
    p = write_csv(out, f"{PFX}_addetti_coorte2004_percorso.csv")
    manifest.append((p, len(out), "Figura6: percorso addetti coorte 2004 per macrosettore (quote alla nascita / creati dopo)", "2004-2009"))
    return out


# ---------------------------------------------------------------------------
# 12) Figura1 e 2 -> ed0409_figura1e2.csv
# ---------------------------------------------------------------------------
def fig12():
    df = sheet("Figura1 e 2")
    # Blocco natalita: header r30 anni col1-6, righe r31-r35 classe in col0
    nat = {}
    nat_years = {}
    for c in range(1, 7):
        y, _ = parse_year(df.iat[30, c])
        if y is not None:
            nat_years[c] = y
    for r in range(31, 36):
        classe = norm_class(df.iat[r, 0])
        for c, y in nat_years.items():
            v = df.iat[r, c]
            if not isna(v):
                nat[(classe, y)] = float(v)
    # Blocco mortalita: header r44 classi col9-13, righe r45-r50 anno in col8
    mor = {}
    mor_classes = {}
    for c in range(9, 14):
        mor_classes[c] = norm_class(df.iat[44, c])
    for r in range(45, 51):
        y, _ = parse_year(df.iat[r, 8])
        if y is None:
            continue
        for c, classe in mor_classes.items():
            v = df.iat[r, c]
            if not isna(v):
                mor[(classe, y)] = float(v)

    order = ["0", "1-4", "5-9", "10+", "Totale"]
    years = sorted({k[1] for k in list(nat) + list(mor)})
    rows = []
    for classe in order:
        for y in years:
            if (classe, y) not in nat and (classe, y) not in mor:
                continue
            rows.append({
                "edizione": EDIZIONE,
                "classe_addetti": classe,
                "anno": y,
                "tasso_natalita": nat.get((classe, y)),
                "tasso_mortalita": mor.get((classe, y)),
            })
    out = pd.DataFrame(rows)
    p = write_csv(out, f"{PFX}_figura1e2.csv")
    manifest.append((p, len(out),
                     "Figura1 e 2: tassi di natalita (Fig.1) e mortalita (Fig.2) per classe di addetti", "2004-2009"))
    notes.append("Figura1 e 2: il foglio (apparentemente vuoto in testa) contiene due blocchi di dati. "
                 "Blocco natalita (r31-r35): classi di addetti 0, 1-4, 5-9, 10+, Totale x anni 2004-2009. "
                 "Blocco mortalita (r45-r50): stessi assi. I valori sono espressi come FRAZIONI (tasso/100, es. "
                 "0.0772=7.72%), a differenza delle Tavole 1-3 in percentuale; conservati come nel foglio. "
                 "Il 2009 e provvisorio per convenzione del report (non marcato nel foglio).")
    return out


# ===========================================================================
t1_df = t1()
t2_df = t2()
t3_df = t3()
t4_df = t4()
t5_df = t5_t6("Tavola 5", "macrosettore", f"{PFX}_dimensione_media_macrosettori.csv",
              "Tavola 5: dimensione media (addetti/impresa) coorte 2004 per macrosettore")
t6_df = t5_t6("Tavola 6", "ripartizione", f"{PFX}_dimensione_media_ripartizioni.csv",
              "Tavola 6: dimensione media (addetti/impresa) coorte 2004 per ripartizione")
t7_df = t7()
f3a, f3b, f3c = fig3()
f4_df = fig4()
f5_df = fig5()
f6_df = fig6()
f12_df = fig12()

# ---------------------------------------------------------------------------
# Anomalie cross-foglio (verificate) utili all'armonizzazione a valle
# ---------------------------------------------------------------------------
anomalie.append(
    "Incoerenza di fonte settore '35' (energia): tasso_natalita 2009 = 17.3 in Tavola 2 "
    "(settori_nace) vs 17.255297679112008 in Figura3 (stock_settori_2009); la mortalita coincide "
    "(6.559031281533804). In Tavola 2 il valore risulta arrotondato/forzato (anche 2008 = 15.5).")
anomalie.append(
    "Denominazioni macrosettore non uniformi tra i fogli: 'Industria in senso stretto' (Tav.1, Tav.7) "
    "vs 'Industria in s.s.' (Tav.4, Tav.5, Fig.5, Fig.6); 'Altri servizi' (Tav.1, Tav.5) vs 'Altri Servizi' "
    "(Tav.4, Fig.5, Fig.6). Mantenute fedeli alla fonte; da armonizzare a valle.")
anomalie.append(
    "Tavola 2 e Figura3 usano livelli di aggregazione NACE diversi per alcuni settori: Tavola 2 usa codici "
    "lettera (E, G, H, I, K, L, N), Figura3 usa range numerici (36-39, 45-47, 49-53, 55-56, 64-66, 68, 77-82); "
    "22 codici coincidono. La riga 'Totale' ha settore_codice vuoto in settori_nace e addetti_settori_2009.")
anomalie.append(
    "Figura4: nel foglio e presente una didascalia in r13 ('Figura 2 - Tassi di sopravvivenza...') che "
    "etichetta erroneamente il contenuto; ignorata (non e un dato).")

# ===========================================================================
# VERIFICA CONTROLLI
# ===========================================================================
print("\n===== VERIFICA CSV E CONTROLLI =====")


def readback(fname):
    return pd.read_csv(os.path.join(OUTDIR, fname))


def check(cond, msg):
    print(("OK  " if cond else "FAIL") + "  " + msg)
    if not cond:
        raise SystemExit("CONTROLLO FALLITO: " + msg)


# shape di ogni CSV
for (p, n, d, a) in manifest:
    rb = pd.read_csv(p)
    print(f"  {os.path.basename(p):55s} shape={rb.shape}  anni={a}")

print("\n-- controlli --")
m = readback(f"{PFX}_macrosettori.csv")
row = m[(m.macrosettore == "Industria in senso stretto") & (m.anno == 2004)].iloc[0]
check(abs(row.tasso_natalita - 4.5785204208310635) < 1e-12, f"macro Industria s.s. 2004 tasso_natalita={row.tasso_natalita}")
check(int(row.imprese_nate) == 24710, f"macro Industria s.s. 2004 imprese_nate={row.imprese_nate}")
check(int(row.imprese_cessate) == 33169, f"macro Industria s.s. 2004 imprese_cessate={row.imprese_cessate}")
row = m[(m.macrosettore == "Totale") & (m.anno == 2006)].iloc[0]
check(abs(row.tasso_natalita - 7.13846075033129) < 1e-12, f"macro Totale 2006 tasso_natalita={row.tasso_natalita}")

reg = readback(f"{PFX}_regioni.csv")
row = reg[(reg.territorio == "Piemonte") & (reg.anno == 2004)].iloc[0]
check(abs(row.tasso_natalita - 7.269865310401159) < 1e-12, f"Piemonte 2004 tasso_natalita={row.tasso_natalita}")

sop = readback(f"{PFX}_sopravvivenza.csv")
row = sop[(sop.macrosettore == "Industria in s.s.") & (sop.coorte == 2004) & (sop.anno_osservazione == 2009)].iloc[0]
check(abs(row.tasso_sopravvivenza - 53.03589831672964) < 1e-12, f"sopravv Industria s.s. coorte2004 oss2009={row.tasso_sopravvivenza}")
check(int(row.anni_dalla_nascita) == 5, f"anni_dalla_nascita={row.anni_dalla_nascita}")

st = readback(f"{PFX}_stock_settori_2009.csv")
check(int(st[st.settore_codice == "F"].iloc[0].stock_2009) == 633945, "stock F=633945")
check(int(st[st.settore_codice == "Totale"].iloc[0].stock_2009) == 3998096, "stock Totale=3998096")

ad = readback(f"{PFX}_addetti_coorte2004.csv")
row = ad[ad.macrosettore == "Industria in senso stretto"].iloc[0]
check(int(row.addetti_t0_nate) == 46686, f"addetti a={row.addetti_t0_nate}")
check(int(row.addetti_t0_sopravviventi) == 25082, f"addetti b={row.addetti_t0_sopravviventi}")
check(int(row.addetti_t5_sopravviventi) == 53279, f"addetti c={row.addetti_t5_sopravviventi}")

# controlli extra utili
adem = readback(f"{PFX}_addetti_demografia_settori_2009.csv")
check(int(adem[adem.settore_codice == "Totale"].iloc[0].stock_2009) == 16235000, "addetti demografia Totale stock=16235000")
check(int(adem[adem.settore_codice == "B"].iloc[0].stock_2009) == 36039, "addetti demografia B stock=36039")

f12 = readback(f"{PFX}_figura1e2.csv")
rowt = f12[(f12.classe_addetti == "Totale") & (f12.anno == 2004)].iloc[0]
check(abs(rowt.tasso_natalita - 0.07719657244514624) < 1e-15, f"figura1e2 Totale 2004 natalita={rowt.tasso_natalita}")
check(abs(rowt.tasso_mortalita - 0.07251646582815399) < 1e-15, f"figura1e2 Totale 2004 mortalita={rowt.tasso_mortalita}")
check(int(f12.tasso_natalita.notna().sum()) == 30 and int(f12.tasso_mortalita.notna().sum()) == 30,
      f"figura1e2 nat/mor popolati: nat={int(f12.tasso_natalita.notna().sum())} mor={int(f12.tasso_mortalita.notna().sum())}")

print("\nTUTTI I CONTROLLI OK")
print(f"\nCSV scritti: {len(manifest)}")
for (p, n, d, a) in manifest:
    print(f"  {p}  ({n} righe)")
print(f"\nNOTE: {len(notes)}")
for x in notes:
    print("  - " + x)
print(f"\nANOMALIE: {len(anomalie)}")
for x in anomalie:
    print("  - " + x)
