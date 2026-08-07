#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Consolidamento dati ISTAT "Demografia d'impresa" 2004-2024.

Unisce i CSV grezzi estratti dalle 4 edizioni (ed0409_, ed0914_, ed1419_, ed1924_)
in input/raw/ e produce i CSV MASTER puliti in input/.

Rieseguibile / idempotente: sovrascrive i master e ristampa le verifiche.
Interprete: /opt/homebrew/bin/python3
"""

import sys
import io
import csv
import pandas as pd

BASE = "/Users/lorenzoruffino/Documents/Progetti/data-viz/Demografia imprese"
RAW = f"{BASE}/input/raw"
OUT = f"{BASE}/input"

# ----------------------------------------------------------------------------
# Armonizzazione etichette
# ----------------------------------------------------------------------------
MACRO = {
    "Industria in s.s.": "Industria in senso stretto",
    "INDUSTRIA in senso stretto": "Industria in senso stretto",
    "Industria in S.S.": "Industria in senso stretto",
    "Altri Servizi": "Altri servizi",
    "Totale ": "Totale",
}
SETT_TEC = {
    "Atra Industria (B,D,E)": "Altra industria (B,D,E)",
}
TERR = {
    "Friuli-V.G.": "Friuli-Venezia Giulia",
    "Sud-Isole": "Sud e Isole",
    "Sud-isole": "Sud e Isole",
    "Nord-ovest": "Nord-Ovest",
    "Nord-est": "Nord-Est",
}

RIPART = {"Nord-Ovest", "Nord-Est", "Centro", "Sud e Isole"}
PROV_AUT = {"Trento", "Bolzano"}

# preferenza edizione (anno di inizio): per gli overlap tieni la piu recente
PREF = {"2004-2009": 2004, "2009-2014": 2009, "2014-2019": 2014, "2019-2024": 2019}


def strip_obj(df):
    """Trim spazi su tutte le colonne stringa."""
    for c in df.columns:
        if df[c].dtype == object:
            df[c] = df[c].map(lambda x: x.strip() if isinstance(x, str) else x)
    return df


def read_raw(name):
    """Legge un CSV grezzo come stringhe (fedelta byte-exact), '' per celle vuote."""
    df = pd.read_csv(f"{RAW}/{name}.csv", dtype=str, keep_default_na=False)
    return strip_obj(df)


def harmonize_macro(s):
    return s.map(lambda x: MACRO.get(x, x))


def harmonize_terr(s):
    return s.map(lambda x: TERR.get(x, x))


def tipo_territorio(t):
    if t == "Italia":
        return "paese"
    if t in PROV_AUT:
        return "provincia_autonoma"
    if t in RIPART:
        return "ripartizione"
    return "regione"


def pref_dedup(df, key_cols):
    """Serie preferita: per ogni (key, anno) tieni la riga dell'edizione piu recente."""
    df = df.copy()
    df["_pref"] = df["edizione"].map(PREF)
    df = df.sort_values("_pref", kind="stable")
    df = df.drop_duplicates(subset=key_cols + ["anno"], keep="last")
    df = df.drop(columns="_pref")
    return df


def saldo_col(df):
    """saldo = imprese_nate - imprese_cessate (interi esatti)."""
    return (df["imprese_nate"].astype("int64") - df["imprese_cessate"].astype("int64")).astype("int64")


def write_master(df, name):
    path = f"{OUT}/{name}"
    df.to_csv(path, index=False)
    return path


PROBLEMI = []
OVERLAP = []
report_rows = []


def note_file(path, righe, descrizione, copertura):
    report_rows.append({"path": path, "righe": righe, "descrizione": descrizione, "copertura": copertura})


# ============================================================================
# 1) demografia_macrosettori.csv
# ============================================================================
cols_macro = ["edizione", "macrosettore", "anno", "provvisorio", "tasso_natalita",
              "imprese_nate", "tasso_mortalita", "imprese_cessate", "turnover_netto"]
macro_parts = []
for ed in ["ed0409", "ed0914", "ed1419", "ed1924"]:
    d = read_raw(f"{ed}_macrosettori")
    d["macrosettore"] = harmonize_macro(d["macrosettore"])
    macro_parts.append(d[cols_macro])
macro = pd.concat(macro_parts, ignore_index=True)
p = write_master(macro, "demografia_macrosettori.csv")
note_file(p, len(macro), "Unione 4 edizioni: tassi natalita/mortalita, imprese nate/cessate, turnover netto per 5 macrosettori.", "2004-2024 (con overlap 2009/2014/2019)")

# ============================================================================
# 2) demografia_macrosettori_serie.csv (serie preferita + saldo)
# ============================================================================
macro_serie = pref_dedup(macro, ["macrosettore"]).copy()
macro_serie["saldo"] = saldo_col(macro_serie)
macro_serie = macro_serie.sort_values(["macrosettore", "anno"], kind="stable").reset_index(drop=True)
p = write_master(macro_serie, "demografia_macrosettori_serie.csv")
note_file(p, len(macro_serie), "Serie preferita 2004-2024 senza duplicati (overlap -> edizione piu recente) + colonna saldo.", "2004-2024")

# ============================================================================
# 3) demografia_regioni.csv (union, turnover calcolato per ed0409)
# ============================================================================
reg_parts = []
for ed in ["ed0409", "ed0914", "ed1419", "ed1924"]:
    d = read_raw(f"{ed}_regioni")
    d["territorio"] = harmonize_terr(d["territorio"])
    d["tipo_territorio"] = d["territorio"].map(tipo_territorio)
    if "turnover_netto" not in d.columns:
        # ed0409: calcola turnover = natalita - mortalita
        d["turnover_netto"] = (d["tasso_natalita"].astype(float) - d["tasso_mortalita"].astype(float))
        d["turnover_netto"] = d["turnover_netto"].map(lambda x: repr(float(x)))
        d["turnover_calcolato"] = True
    else:
        d["turnover_calcolato"] = False
    reg_parts.append(d[["edizione", "territorio", "tipo_territorio", "anno", "provvisorio",
                        "tasso_natalita", "tasso_mortalita", "turnover_netto", "turnover_calcolato"]])
regioni = pd.concat(reg_parts, ignore_index=True)
p = write_master(regioni, "demografia_regioni.csv")
note_file(p, len(regioni), "Unione 4 edizioni: natalita/mortalita/turnover per 26 territori. turnover_calcolato=True per ed0409 (turnover ricavato = nat-mort).", "2004-2024 (con overlap)")

# ============================================================================
# 4) demografia_regioni_serie.csv
# ============================================================================
regioni_serie = pref_dedup(regioni, ["territorio"]).copy()
regioni_serie = regioni_serie.sort_values(["territorio", "anno"], kind="stable").reset_index(drop=True)
p = write_master(regioni_serie, "demografia_regioni_serie.csv")
note_file(p, len(regioni_serie), "Serie preferita 2004-2024 senza duplicati per 26 territori.", "2004-2024")

# ============================================================================
# 5) settori_nace_2008_2014.csv
# ============================================================================
nace_cols = ["edizione", "settore_codice", "settore_nome", "anno", "provvisorio",
             "tasso_natalita", "tasso_mortalita", "turnover_netto"]
nace_parts = []
for ed in ["ed0409", "ed0914"]:
    d = read_raw(f"{ed}_settori_nace")
    if "turnover_netto" not in d.columns:
        d["turnover_netto"] = ""
    nace_parts.append(d[nace_cols])
nace = pd.concat(nace_parts, ignore_index=True)
p = write_master(nace, "settori_nace_2008_2014.csv")
note_file(p, len(nace), "Settori NACE Rev.2 (ed0409 2008-2009 + ed0914 2009-2014). turnover vuoto dove assente (ed0409). Overlap 2009 mantenuto (2 edizioni).", "2008-2014")

# ============================================================================
# 6) settori_tecnologia_2014_2024.csv + settori_tecnologia_serie.csv
# ============================================================================
tec_cols = ["edizione", "settore", "anno", "provvisorio", "tasso_natalita",
            "imprese_nate", "tasso_mortalita", "imprese_cessate"]
tec_parts = []
for ed in ["ed1419", "ed1924"]:
    d = read_raw(f"{ed}_settori_tecnologia")
    d["settore"] = d["settore"].map(lambda x: SETT_TEC.get(x, MACRO.get(x, x)))
    tec_parts.append(d[tec_cols])
tec = pd.concat(tec_parts, ignore_index=True)
p = write_master(tec, "settori_tecnologia_2014_2024.csv")
note_file(p, len(tec), "Settori per intensita tecnologica/di conoscenza (13 settori) ed1419 + ed1924. Overlap 2019 mantenuto.", "2014-2024 (con overlap)")

tec_serie = pref_dedup(tec, ["settore"]).copy()
tec_serie = tec_serie.sort_values(["settore", "anno"], kind="stable").reset_index(drop=True)
p = write_master(tec_serie, "settori_tecnologia_serie.csv")
note_file(p, len(tec_serie), "Serie preferita 2014-2024 per 13 settori tecnologici (2019 da ed1924).", "2014-2024")

# ============================================================================
# 7) sopravvivenza_coorti.csv
# ============================================================================
sop_cols = ["edizione", "macrosettore", "coorte", "anno_osservazione",
            "anni_dalla_nascita", "tasso_sopravvivenza"]
sop_parts = []
for ed in ["ed0409", "ed0914", "ed1419", "ed1924"]:
    d = read_raw(f"{ed}_sopravvivenza")
    d["macrosettore"] = harmonize_macro(d["macrosettore"])
    sop_parts.append(d[sop_cols])
sop = pd.concat(sop_parts, ignore_index=True)
p = write_master(sop, "sopravvivenza_coorti.csv")
note_file(p, len(sop), "Tassi di sopravvivenza per macrosettore, coorte di nascita e anni dalla nascita (matrice triangolare), 4 edizioni.", "coorti 2004-2023")

# ============================================================================
# 8) addetti_coorti.csv
# ============================================================================
ORIZ = {"ed0409": 5, "ed0914": 4, "ed1419": 5, "ed1924": 5}
addetti_cols = ["edizione", "coorte", "orizzonte_anni", "macrosettore",
                "addetti_t0_nate", "addetti_t0_sopravviventi", "addetti_tfin_sopravviventi",
                "perdita_pct_da_cessazioni", "crescita_pct_sopravviventi", "variazione_pct_netta"]
add_parts = []
for ed, fn in [("ed0409", "ed0409_addetti_coorte2004"), ("ed0914", "ed0914_addetti_coorte2010"),
               ("ed1419", "ed1419_addetti_coorte2014"), ("ed1924", "ed1924_addetti_coorte2019")]:
    d = read_raw(fn)
    d = d.rename(columns={"addetti_t5_sopravviventi": "addetti_tfin_sopravviventi"})
    d["macrosettore"] = harmonize_macro(d["macrosettore"])
    d["orizzonte_anni"] = ORIZ[ed]
    add_parts.append(d[addetti_cols])
addetti = pd.concat(add_parts, ignore_index=True)
p = write_master(addetti, "addetti_coorti.csv")
note_file(p, len(addetti), "Addetti delle coorti 2004/2010/2014/2019 per macrosettore: nate, sopravviventi t0 e tfin, variazioni pct. orizzonte 5/4/5/5.", "coorti 2004,2010,2014,2019")

# ============================================================================
# 9) classi_dipendenti_2009_2014.csv (copia)
# ============================================================================
classi = read_raw("ed0914_classi_dipendenti")
p = write_master(classi, "classi_dipendenti_2009_2014.csv")
note_file(p, len(classi), "Tassi natalita/mortalita per classe di dipendenti (0,1-4,5-9,10+,Totale), edizione 2009-2014.", "2009-2014")

# ============================================================================
# 10) dimensione_media_2004_2009.csv
# ============================================================================
dm_macro = read_raw("ed0409_dimensione_media_macrosettori").rename(columns={"macrosettore": "gruppo"})
dm_macro["tipo_gruppo"] = "macrosettore"
dm_rip = read_raw("ed0409_dimensione_media_ripartizioni").rename(columns={"ripartizione": "gruppo"})
dm_rip["tipo_gruppo"] = "ripartizione"
dim = pd.concat([dm_macro, dm_rip], ignore_index=True)
dim["gruppo"] = dim["gruppo"].map(lambda x: MACRO.get(x, TERR.get(x, x)))
dim = dim[["edizione", "gruppo", "tipo_gruppo", "anno", "addetti_medi"]]
p = write_master(dim, "dimensione_media_2004_2009.csv")
note_file(p, len(dim), "Dimensione media (addetti/impresa) coorte 2004 sopravvivente, per macrosettore e ripartizione (tipo_gruppo).", "2004-2009")

# ============================================================================
# 11) stock_settori_2009.csv (copia)
# ============================================================================
stock = read_raw("ed0409_stock_settori_2009")
p = write_master(stock, "stock_settori_2009.csv")
note_file(p, len(stock), "Demografia imprese 2009 per settore NACE Rev.2: stock, nate, morte (stima), tassi, turnover.", "2009")

# ============================================================================
# 12) serie_nazionale_2004_2024.csv (dalle righe Totale della serie preferita)
# ============================================================================
tot = macro_serie[macro_serie["macrosettore"] == "Totale"].copy()
serie_naz = pd.DataFrame({
    "anno": tot["anno"].values,
    "tasso_natalita": tot["tasso_natalita"].values,
    "tasso_mortalita": tot["tasso_mortalita"].values,
    "turnover_netto": tot["turnover_netto"].values,
    "imprese_nate": tot["imprese_nate"].values,
    "imprese_cessate": tot["imprese_cessate"].values,
    "saldo": tot["saldo"].values,
    "provvisorio": tot["provvisorio"].values,
    "fonte_anno": tot["edizione"].values,
})
serie_naz = serie_naz.sort_values("anno", kind="stable").reset_index(drop=True)
p = write_master(serie_naz, "serie_nazionale_2004_2024.csv")
note_file(p, len(serie_naz), "Serie nazionale (righe Totale della serie preferita): tassi, nate/cessate, saldo, turnover, provvisorio, fonte_anno.", "2004-2024")

# ============================================================================
# VERIFICHE
# ============================================================================
print("=" * 70)
print("VERIFICHE CONTEGGI RIGHE")
print("=" * 70)
expect = {
    "demografia_macrosettori.csv": 120,
    "demografia_macrosettori_serie.csv": 105,
    "demografia_regioni.csv": 624,
    "demografia_regioni_serie.csv": 546,
    "settori_nace_2008_2014.csv": 240,
    "settori_tecnologia_2014_2024.csv": 156,
    "settori_tecnologia_serie.csv": 143,
    "sopravvivenza_coorti.csv": 300,
    "addetti_coorti.csv": 20,
    "classi_dipendenti_2009_2014.csv": 30,
    "dimensione_media_2004_2009.csv": 60,
    "stock_settori_2009.csv": 30,
    "serie_nazionale_2004_2024.csv": 21,
}
actual = {r["path"].split("/")[-1]: r["righe"] for r in report_rows}
for name, exp in expect.items():
    act = actual.get(name)
    ok = "OK" if act == exp else "!!! MISMATCH"
    print(f"  {name:42s} atteso={exp:4d} reale={act} {ok}")
    if act != exp:
        PROBLEMI.append(f"{name}: righe attese {exp}, ottenute {act}")

print()
print("=" * 70)
print("CONTINUITA ANNI")
print("=" * 70)
def check_years(df, keycol, name, years):
    bad = []
    for k, g in df.groupby(keycol):
        got = set(g["anno"].astype(int))
        miss = set(years) - got
        if miss:
            bad.append((k, sorted(miss)))
    if bad:
        PROBLEMI.append(f"{name}: buchi anni {bad[:3]}")
        print(f"  {name}: BUCHI {bad[:3]}")
    else:
        print(f"  {name}: tutti i {keycol} coprono {min(years)}-{max(years)} senza buchi  OK")

check_years(macro_serie, "macrosettore", "macrosettori_serie", range(2004, 2025))
check_years(regioni_serie, "territorio", "regioni_serie", range(2004, 2025))
# sopravvivenza coorti 2004-2023
sop_coorti = set(sop["coorte"].astype(int))
miss_c = set(range(2004, 2024)) - sop_coorti
if miss_c:
    PROBLEMI.append(f"sopravvivenza: coorti mancanti {sorted(miss_c)}")
    print(f"  sopravvivenza: COORTI MANCANTI {sorted(miss_c)}")
else:
    print(f"  sopravvivenza: coorti 2004-2023 tutte presenti  OK  ({sorted(sop_coorti)})")

print()
print("=" * 70)
print("SPOT CHECK")
print("=" * 70)
# serie_nazionale 2024 imprese_nate 292787
v = serie_naz[serie_naz["anno"] == "2024"]["imprese_nate"].iloc[0]
ok = (str(v) == "292787")
print(f"  serie_nazionale 2024 imprese_nate = {v}  {'OK' if ok else '!!!'}")
if not ok: PROBLEMI.append(f"spot: serie_naz 2024 nate={v} != 292787")

# regioni_serie Piemonte 2004 natalita 7.269865310401159
v = regioni_serie[(regioni_serie["territorio"] == "Piemonte") & (regioni_serie["anno"] == "2004")]["tasso_natalita"].iloc[0]
ok = (v == "7.269865310401159")
print(f"  regioni_serie Piemonte 2004 natalita = {v}  {'OK' if ok else '!!!'}")
if not ok: PROBLEMI.append(f"spot: Piemonte 2004 nat={v}")

# sopravvivenza Totale coorte 2019 anni_dalla_nascita 5 presente
sub = sop[(sop["macrosettore"] == "Totale") & (sop["coorte"] == "2019") & (sop["anni_dalla_nascita"] == "5")]
ok = len(sub) == 1
print(f"  sopravvivenza Totale coorte 2019 d5 presente = {len(sub)} riga  {'OK' if ok else '!!!'}  (val={sub['tasso_sopravvivenza'].iloc[0] if ok else 'NA'})")
if not ok: PROBLEMI.append("spot: sopravvivenza Totale coorte 2019 d5 assente")

# tipo_territorio counts
print()
tt = regioni_serie.drop_duplicates("territorio")["tipo_territorio"].value_counts().to_dict()
print(f"  tipo_territorio (distinti): {tt}  -> tot {sum(tt.values())} territori")
if tt.get("regione") != 19 or tt.get("provincia_autonoma") != 2 or tt.get("ripartizione") != 4 or tt.get("paese") != 1:
    PROBLEMI.append(f"tipo_territorio inatteso: {tt}")

print()
print("=" * 70)
print("OVERLAP (differenze tra edizione vecchia e nuova)")
print("=" * 70)

def overlap_diffs(df, keycol, metrics):
    """Ritorna lista di dict con differenze assolute per gli anni doppi."""
    out = []
    pairs = [(2009, "2004-2009", "2009-2014"),
             (2014, "2009-2014", "2014-2019"),
             (2019, "2014-2019", "2019-2024")]
    for anno, old_ed, new_ed in pairs:
        a = str(anno)
        old = df[(df["anno"] == a) & (df["edizione"] == old_ed)]
        new = df[(df["anno"] == a) & (df["edizione"] == new_ed)]
        for k in old[keycol].unique():
            ro = old[old[keycol] == k]
            rn = new[new[keycol] == k]
            if len(ro) == 0 or len(rn) == 0:
                continue
            for m in metrics:
                try:
                    vo = float(ro[m].iloc[0]); vn = float(rn[m].iloc[0])
                except (ValueError, IndexError):
                    continue
                out.append({"gruppo": k, "anno": anno, "metrica": m,
                            "vecchio": vo, "nuovo": vn, "diff_abs": abs(vo - vn)})
    return out

diffs = []
diffs += [dict(d, dominio="macrosettori") for d in overlap_diffs(macro, "macrosettore", ["tasso_natalita", "tasso_mortalita"])]
diffs += [dict(d, dominio="regioni") for d in overlap_diffs(regioni, "territorio", ["tasso_natalita", "tasso_mortalita"])]
diffs.sort(key=lambda x: -x["diff_abs"])
print("  Top 5 differenze assolute sui tassi (vecchio vs nuovo):")
for d in diffs[:5]:
    line = f"    {d['dominio']:12s} {d['gruppo']:22s} {d['anno']} {d['metrica']:16s} vecchio={d['vecchio']:.4f} nuovo={d['nuovo']:.4f} |diff|={d['diff_abs']:.4f}"
    print(line)
    OVERLAP.append(f"{d['dominio']} {d['gruppo']} {d['anno']} {d['metrica']}: {d['vecchio']:.4f} (vecchia) vs {d['nuovo']:.4f} (nuova), diff {d['diff_abs']:.4f} pp")

# conteggi nazionali nate/cessate agli anni doppi
print("  Differenza conteggio nazionale nate/cessate agli anni doppi:")
for anno, old_ed, new_ed in [(2009, "2004-2009", "2009-2014"), (2014, "2009-2014", "2014-2019"), (2019, "2014-2019", "2019-2024")]:
    a = str(anno)
    ro = macro[(macro["macrosettore"] == "Totale") & (macro["anno"] == a) & (macro["edizione"] == old_ed)]
    rn = macro[(macro["macrosettore"] == "Totale") & (macro["anno"] == a) & (macro["edizione"] == new_ed)]
    no, cn = int(ro["imprese_nate"].iloc[0]), int(ro["imprese_cessate"].iloc[0])
    nn, cc = int(rn["imprese_nate"].iloc[0]), int(rn["imprese_cessate"].iloc[0])
    print(f"    {anno}: nate {no}->{nn} (diff {nn-no}); cessate {cn}->{cc} (diff {cc-cn})")
    OVERLAP.append(f"Conteggio nazionale {anno}: nate {no} (vecchia) vs {nn} (nuova) diff {nn-no}; cessate {cn} (vecchia) vs {cc} (nuova) diff {cc-cn}")

print()
print("=" * 70)
print("COERENZA serie_nazionale vs ed1924_serie_tassi_2006_2024 (soglia 0.05 pp)")
print("=" * 70)
web = read_raw("ed1924_serie_tassi_2006_2024")
web_nat = dict(zip(web["anno"], web["tasso_natalita"]))
web_mor = dict(zip(web["anno"], web["tasso_mortalita"]))
scost = []
for _, r in serie_naz.iterrows():
    a = r["anno"]
    if a not in web_nat:
        continue
    dn = abs(float(r["tasso_natalita"]) - float(web_nat[a]))
    dm = abs(float(r["tasso_mortalita"]) - float(web_mor[a]))
    if dn > 0.05:
        scost.append((a, "natalita", float(r["tasso_natalita"]), float(web_nat[a]), dn))
    if dm > 0.05:
        scost.append((a, "mortalita", float(r["tasso_mortalita"]), float(web_mor[a]), dm))
if scost:
    for a, met, v1, v2, dd in scost:
        print(f"    {a} {met}: serie_naz={v1:.4f} vs web={v2:.4f} |diff|={dd:.4f}  >0.05")
        OVERLAP.append(f"Coerenza web {a} {met}: serie_nazionale {v1:.4f} vs grafico web {v2:.4f}, scostamento {dd:.4f} pp")
else:
    print("    Nessuno scostamento > 0.05 pp: serie_nazionale coerente col grafico web 2006-2024  OK")

print()
print("=" * 70)
print("RIEPILOGO FILE PRODOTTI")
print("=" * 70)
for r in report_rows:
    print(f"  {r['righe']:4d}  {r['path'].split('/')[-1]}")
print()
print(f"PROBLEMI: {len(PROBLEMI)}")
for x in PROBLEMI:
    print("  -", x)
print(f"\nOVERLAP items: {len(OVERLAP)}")

# esporta un piccolo json di stato per debug
import json
with open(f"{BASE}/script/_stato_consolida.json", "w") as fh:
    json.dump({"problemi": PROBLEMI, "overlap": OVERLAP,
               "file": [{"path": r["path"], "righe": r["righe"]} for r in report_rows]},
              fh, ensure_ascii=False, indent=2)
print("\nOK" if not PROBLEMI else "\nCI SONO PROBLEMI")
