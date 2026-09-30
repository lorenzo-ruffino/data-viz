#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Modello Spagnolo — evidenza quantitativa demografica e pensionistica (ES vs IT, con FR/DE/EU27).

Input : Modello Spagnolo/input/eurostat_*.csv (gia' scaricati, formato SDMX-CSV Eurostat)
Output: Modello Spagnolo/output/dati/demo_*.csv e pens_*.csv (tidy/long, decimale con punto)

Blocchi:
  a) crescita popolazione 1995-2025 scomposta in saldo naturale e saldo migratorio (DEMO_GIND)
  b) old-age dependency storico 1990-2025 (DEMO_PJANIND) + proiezione 2026-2070 (PROJ_23NP)
  c) popolazione 15-64 storica (DEMO_PJAN) e proiettata (PROJ_23NP)
  d) spesa pensionistica in % Pil 1995-2024 (SPR_EXP_PENS)
  e) beneficiari di pensione / occupati 20-64 (SPR_PNS_BEN / LFSI_EMP_A[_H])
  f) scenario meccanico "aritmetica pensionistica" (tasso di occupazione 2025 costante)
  g) TFR (DEMO_FRATE) e reddito disponibile reale pro capite (TEPSR_WC310)

Uso: /opt/homebrew/bin/python3 "script/11_demografia_pensioni.py"
"""

from pathlib import Path
import pandas as pd

BASE = Path(__file__).resolve().parent.parent
INP = BASE / "input"
OUT = BASE / "output" / "dati"
OUT.mkdir(parents=True, exist_ok=True)

PAESI = {
    "ES": "Spagna",
    "IT": "Italia",
    "FR": "Francia",
    "DE": "Germania",
    "EU27_2020": "UE27",
}
ORDINE = ["ES", "IT", "FR", "DE", "EU27_2020"]


def load(nome):
    """Carica un CSV SDMX Eurostat e aggiunge la colonna anno (int)."""
    d = pd.read_csv(INP / nome)
    d["anno"] = pd.to_datetime(d["TIME_PERIOD"]).dt.year
    return d


def salva(df, nome, col_ordine=None):
    """Salva un CSV tidy: decimale con punto, niente indice, valori arrotondati."""
    if col_ordine:
        df = df[col_ordine]
    df = df.copy()
    for c in df.columns:
        if pd.api.types.is_float_dtype(df[c]):
            df[c] = df[c].round(4)
    df.to_csv(OUT / nome, index=False, float_format="%.10g")
    print(f"  -> {nome}  ({len(df)} righe)")


def paese(df, col="geo"):
    df = df.copy()
    df.insert(df.columns.get_loc(col) + 1, "paese", df[col].map(PAESI))
    df[col] = pd.Categorical(df[col], categories=ORDINE, ordered=True)
    return df.sort_values([col, "anno"]) if "anno" in df.columns else df.sort_values(col)


ris = {}  # numeri chiave per i findings

# =====================================================================
# a) Crescita della popolazione scomposta: saldo naturale vs migratorio
# =====================================================================
print("\n[a] Crescita popolazione: saldo naturale vs saldo migratorio")

gind = load("eurostat_demo_gind_demografia.csv")
gw = gind.pivot_table(index=["geo", "anno"], columns="indic_de",
                      values="OBS_VALUE").reset_index()

# Verifica identita' contabile GROW = NATGROW + CNMIGRAT (e = JAN(t+1) - JAN(t))
chk = gw.dropna(subset=["GROW", "NATGROW", "CNMIGRAT"])
scarto = (chk["GROW"] - chk["NATGROW"] - chk["CNMIGRAT"]).abs().max()
print(f"  identita' GROW = NATGROW + CNMIGRAT: scarto max = {scarto:.1f} persone")

# rotture di serie: anni in cui JAN(t+1) - JAN(t) != GROW(t) (revisioni censuarie)
for g in ORDINE:
    d = gw[gw.geo == g].set_index("anno")
    dif = d["JAN"].shift(-1) - d["JAN"] - d["GROW"]
    rotture = dif[dif.abs() > 1].dropna()
    print(f"  rotture di serie {g}: " +
          (", ".join(f"{int(k)} ({int(v):+,})" for k, v in rotture.items()) or "nessuna"))

annuale = gw[["geo", "anno", "JAN", "NATGROW", "CNMIGRAT", "GROW"]].copy()
annuale = annuale.melt(id_vars=["geo", "anno"], var_name="indicatore",
                       value_name="valore").dropna(subset=["valore"])
annuale["indicatore"] = annuale["indicatore"].map({
    "JAN": "popolazione_1gen",
    "NATGROW": "saldo_naturale",
    "CNMIGRAT": "saldo_migratorio",
    "GROW": "variazione_totale",
})
annuale = paese(annuale)
salva(annuale, "demo_crescita_pop_componenti.csv",
      ["geo", "paese", "anno", "indicatore", "valore"])

# Cumulati per periodo
PERIODI = [
    ("1995-2007", 1995, 2007),   # boom migratorio pre-crisi
    ("2008-2014", 2008, 2014),   # crisi finanziaria e austerita'
    ("2015-2025", 2015, 2025),   # nuova ondata migratoria
    ("1995-2025", 1995, 2025),   # intero periodo
    ("2015-2019", 2015, 2019),
    ("2020-2025", 2020, 2025),
]
righe = []
for g in ORDINE:
    d = gw[gw.geo == g].set_index("anno")
    for nome, y0, y1 in PERIODI:
        sel = d.loc[y0:y1]
        if sel[["NATGROW", "CNMIGRAT"]].isna().any().any() or len(sel) < (y1 - y0 + 1):
            continue
        nat, mig = sel["NATGROW"].sum(), sel["CNMIGRAT"].sum()
        tot = nat + mig
        pop0 = d.loc[y0, "JAN"]
        pop1 = d.loc[y1 + 1, "JAN"] if (y1 + 1) in d.index else float("nan")
        righe.append({
            "geo": g, "periodo": nome, "anno_inizio": y0, "anno_fine": y1,
            "pop_iniziale": pop0, "pop_finale": pop1,
            "saldo_naturale": nat, "saldo_migratorio": mig, "variazione_totale": tot,
            "var_pct": 100 * tot / pop0,
            "quota_migratoria_pct": 100 * mig / tot if tot != 0 else float("nan"),
        })
periodi = paese(pd.DataFrame(righe))
salva(periodi, "demo_crescita_pop_periodi.csv",
      ["geo", "paese", "periodo", "anno_inizio", "anno_fine", "pop_iniziale",
       "pop_finale", "saldo_naturale", "saldo_migratorio", "variazione_totale",
       "var_pct", "quota_migratoria_pct"])

p = periodi.set_index(["geo", "periodo"])
for g in ["ES", "IT"]:
    r = p.loc[(g, "1995-2025")]
    ris[f"{g}_1995_2025"] = r
    print(f"  {PAESI[g]} 1995-2025: pop {r.pop_iniziale/1e6:.2f}M -> {r.pop_finale/1e6:.2f}M "
          f"({r.var_pct:+.1f}%) | naturale {r.saldo_naturale/1e6:+.2f}M | "
          f"migratorio {r.saldo_migratorio/1e6:+.2f}M | quota migratoria {r.quota_migratoria_pct:.0f}%")
for g in ["ES", "IT"]:
    r = p.loc[(g, "2015-2025")]
    ris[f"{g}_2015_2025"] = r
    print(f"  {PAESI[g]} 2015-2025: variazione {r.variazione_totale/1e6:+.2f}M "
          f"(naturale {r.saldo_naturale/1e6:+.2f}M, migratorio {r.saldo_migratorio/1e6:+.2f}M)")

# anni in cui il saldo naturale spagnolo e' negativo
es_nat = gw[(gw.geo == "ES")].set_index("anno")["NATGROW"].dropna()
primo_neg = es_nat[es_nat < 0].index.min()
ris["ES_primo_anno_saldo_nat_negativo"] = int(primo_neg)
print(f"  Spagna: saldo naturale negativo dal {int(primo_neg)}")

# =====================================================================
# b) Old-age dependency ratio: storico + proiezioni Europop2023
# =====================================================================
print("\n[b] Old-age dependency ratio (65+/15-64): storico 1990-2025 + Europop2023")

dip = load("eurostat_demo_pjanind_dipendenza.csv")
old_st = dip[dip.indic_de == "OLDDEP1"][["geo", "anno", "OBS_VALUE"]].rename(
    columns={"OBS_VALUE": "valore"})
old_st["tipo"] = "storico"

proj = load("eurostat_proj_23np_proiezioni.csv")
pw = proj.pivot_table(index=["geo", "anno"], columns="age", values="OBS_VALUE").reset_index()
pw["olddep"] = 100 * pw["Y_GE65"] / pw["Y15-64"]

old_pr = pw[["geo", "anno", "olddep"]].rename(columns={"olddep": "valore"})
old_pr["tipo"] = "proiezione"

# confronto sull'anno di raccordo (2025: c'e' sia il dato reale sia la proiezione)
conf = old_st.merge(old_pr, on=["geo", "anno"], suffixes=("_st", "_pr"))
conf = conf[conf.anno == 2025][["geo", "valore_st", "valore_pr"]]
print("  scarto Europop2023 vs dato reale nel 2025 (punti):")
for _, r in conf.iterrows():
    print(f"    {PAESI[r.geo]:<9} reale {r.valore_st:.1f}  Europop {r.valore_pr:.1f}  "
          f"({r.valore_pr - r.valore_st:+.1f})")

# serie unica: storico fino al 2025, proiezione dal 2026 al 2070
olddep = pd.concat([
    old_st[old_st.anno <= 2025],
    old_pr[(old_pr.anno >= 2026) & (old_pr.anno <= 2070)],
], ignore_index=True)
olddep = paese(olddep)
salva(olddep, "demo_olddep_storico_proiezioni.csv",
      ["geo", "paese", "anno", "tipo", "valore"])

od = olddep.set_index(["geo", "anno"])["valore"]
for g in ["ES", "IT", "FR", "DE", "EU27_2020"]:
    vals = {y: od.loc[(g, y)] for y in [1995, 2025, 2040, 2050, 2070] if (g, y) in od.index}
    ris[f"olddep_{g}"] = vals
    print("  " + PAESI[g].ljust(9) + "  " +
          "  ".join(f"{y}: {v:.1f}" for y, v in vals.items()))

# anno in cui la Spagna raggiunge l'old-dep italiano del 2025
it25 = od.loc[("IT", 2025)]
es_serie = od.loc["ES"]
anno_es_raggiunge = int(es_serie[es_serie >= it25].index.min())
ris["ES_anno_olddep_pari_IT2025"] = anno_es_raggiunge
ris["IT_olddep_2025"] = it25
print(f"  la Spagna raggiunge l'old-dep italiano del 2025 ({it25:.1f}%) nel {anno_es_raggiunge}")

# picco proiettato
for g in ["ES", "IT"]:
    s = od.loc[g]
    print(f"  {PAESI[g]}: massimo old-dep entro il 2070 = {s.max():.1f}% nel {int(s.idxmax())}")
    ris[f"olddep_max_{g}"] = (float(s.max()), int(s.idxmax()))

# =====================================================================
# c) Popolazione in eta' da lavoro (15-64): storico e proiezioni
# =====================================================================
print("\n[c] Popolazione 15-64: storico (DEMO_PJAN) e proiezioni (Europop2023)")

pjan = load("eurostat_demo_pjan_demografia.csv")
eta_1564 = [f"Y{i}" for i in range(15, 65)]
st_1564 = (pjan[pjan.age.isin(eta_1564)]
           .groupby(["geo", "anno"])["OBS_VALUE"].sum().reset_index()
           .rename(columns={"OBS_VALUE": "valore"}))
# controllo: numero di eta' presenti per ogni geo/anno (deve essere 50)
n_eta = (pjan[pjan.age.isin(eta_1564)].groupby(["geo", "anno"])["age"].nunique())
print(f"  eta' singole aggregate per geo/anno: min {n_eta.min()}, max {n_eta.max()} (atteso 50)")
st_1564["tipo"] = "storico"

pr_1564 = pw[["geo", "anno", "Y15-64"]].rename(columns={"Y15-64": "valore"})
pr_1564["tipo"] = "proiezione"

conf2 = st_1564.merge(pr_1564, on=["geo", "anno"], suffixes=("_st", "_pr"))
conf2 = conf2[conf2.anno == 2025]
print("  scarto Europop2023 vs reale sulla pop 15-64 nel 2025:")
for _, r in conf2.iterrows():
    print(f"    {PAESI[r.geo]:<9} reale {r.valore_st/1e6:.2f}M  Europop {r.valore_pr/1e6:.2f}M "
          f"({100*(r.valore_pr/r.valore_st - 1):+.1f}%)")

pop1564 = pd.concat([
    st_1564[st_1564.anno <= 2025],
    pr_1564[(pr_1564.anno >= 2026) & (pr_1564.anno <= 2070)],
], ignore_index=True)
pop1564 = paese(pop1564)
salva(pop1564, "demo_pop_1564_proiezioni.csv",
      ["geo", "paese", "anno", "tipo", "valore"])

pp = pop1564.set_index(["geo", "anno"])["valore"]
for g in ["ES", "IT", "FR", "DE", "EU27_2020"]:
    s = pp.loc[g]
    picco_anno = int(s.idxmax())
    picco = s.max()
    v25, v50, v70 = s.get(2025), s.get(2050), s.get(2070)
    ris[f"pop1564_{g}"] = dict(picco_anno=picco_anno, picco=picco, y2025=v25,
                               y2050=v50, y2070=v70)
    print(f"  {PAESI[g]:<9} picco {picco/1e6:.2f}M nel {picco_anno} | 2025 {v25/1e6:.2f}M | "
          f"2050 {v50/1e6:.2f}M ({100*(v50/v25-1):+.1f}% vs 2025) | "
          f"2070 {v70/1e6:.2f}M ({100*(v70/v25-1):+.1f}%)")

# primo anno di calo persistente (>=5 anni consecutivi di calo) dal 2025 in poi
for g in ["ES", "IT"]:
    s = pp.loc[g].loc[2025:2070]
    dif = s.diff()
    inizio = None
    for y in range(2026, 2066):
        if all(dif.get(y + k, 0) < 0 for k in range(5)):
            inizio = y
            break
    ris[f"pop1564_calo_da_{g}"] = inizio
    print(f"  {PAESI[g]}: inizio del calo persistente della pop 15-64 (>=5 anni) = {inizio}")

# popolazione totale proiettata (per contesto)
tot_proj = pw[["geo", "anno", "TOTAL", "Y_GE65"]].copy()
for g in ["ES", "IT"]:
    d = tot_proj[tot_proj.geo == g].set_index("anno")
    print(f"  {PAESI[g]} popolazione totale Europop: 2025 {d.loc[2025,'TOTAL']/1e6:.2f}M | "
          f"2050 {d.loc[2050,'TOTAL']/1e6:.2f}M | 2070 {d.loc[2070,'TOTAL']/1e6:.2f}M")
    ris[f"poptot_{g}"] = {y: d.loc[y, "TOTAL"] for y in [2025, 2040, 2050, 2070]}
    ris[f"pop65_{g}"] = {y: d.loc[y, "Y_GE65"] for y in [2025, 2040, 2050, 2070]}

# stima indicativa del saldo migratorio cumulato implicito nel baseline
# (variazione totale proiettata meno saldo naturale tenuto costante al livello 2025)
for g in ["ES", "IT"]:
    nat25 = gw[(gw.geo == g) & (gw.anno == 2025)]["NATGROW"].iloc[0]
    d = tot_proj[tot_proj.geo == g].set_index("anno")
    for y1 in [2050, 2070]:
        var = d.loc[y1, "TOTAL"] - d.loc[2025, "TOTAL"]
        anni = y1 - 2025
        mig_impl = var - nat25 * anni
        ris[f"mig_implicita_{g}_{y1}"] = mig_impl
        print(f"  [indicativo] {PAESI[g]} 2025-{y1}: variazione pop {var/1e6:+.2f}M, "
              f"saldo naturale a livello 2025 ({nat25/1e3:+.0f}k/anno) = {nat25*anni/1e6:+.2f}M "
              f"-> migrazione netta cumulata implicita >= {mig_impl/1e6:+.2f}M "
              f"({mig_impl/anni/1e3:.0f}k/anno)")

# =====================================================================
# d) Spesa pensionistica in % Pil
# =====================================================================
print("\n[d] Spesa pensionistica in % Pil (ESSPROS, SPR_EXP_PENS)")

exp = load("eurostat_spr_exp_pens_pensioni.csv")
sp = exp[(exp.unit == "PC_GDP") & (exp.spdepb.isin(["TOTAL", "OLD", "SRV", "DIS"]))]
sp = sp[["geo", "anno", "spdepb", "OBS_VALUE"]].rename(
    columns={"spdepb": "tipo_pensione", "OBS_VALUE": "valore"})
sp["tipo_pensione"] = sp["tipo_pensione"].map({
    "TOTAL": "totale", "OLD": "vecchiaia", "SRV": "superstiti", "DIS": "invalidita"})
sp = paese(sp)
salva(sp, "pens_spesa_pil.csv", ["geo", "paese", "anno", "tipo_pensione", "valore"])

spt = sp[sp.tipo_pensione == "totale"].set_index(["geo", "anno"])["valore"]
for y in [1995, 2000, 2007, 2013, 2019, 2023, 2024]:
    riga = []
    for g in ORDINE:
        v = spt.get((g, y))
        if pd.notna(v):
            riga.append(f"{PAESI[g]} {v:.1f}")
    print(f"  {y}: " + " | ".join(riga))
for g in ["ES", "IT"]:
    s = spt.loc[g]
    ris[f"spesa_{g}"] = {y: s.get(y) for y in [1995, 2007, 2013, 2019, 2023, 2024]}
    print(f"  {PAESI[g]}: min {s.min():.1f}% ({int(s.idxmin())}), max {s.max():.1f}% ({int(s.idxmax())}), "
          f"ultimo {s.iloc[-1]:.1f}% ({int(s.index[-1])})")
gap = {y: spt.get(("IT", y)) - spt.get(("ES", y)) for y in [1995, 2007, 2013, 2019, 2023]}
ris["gap_IT_ES"] = gap
print("  divario IT-ES (punti di Pil): " + " | ".join(f"{y}: {v:+.1f}" for y, v in gap.items()))

# =====================================================================
# e) Beneficiari di pensione / occupati 20-64
# =====================================================================
print("\n[e] Beneficiari di pensione per occupato (proxy)")

ben = load("eurostat_spr_pns_ben_pensioni.csv")
ben = ben[(ben.sex == "T") & (ben.unit == "PER")]
ben_t = ben[ben.spdepb == "TOTAL"][["geo", "anno", "OBS_VALUE"]].rename(
    columns={"OBS_VALUE": "beneficiari_totale"})
ben_o = ben[ben.spdepb == "OLD_SRV"][["geo", "anno", "OBS_VALUE"]].rename(
    columns={"OBS_VALUE": "beneficiari_vecchiaia_superstiti"})

l1 = load("eurostat_lfsi_emp_a_lavoro.csv")
l2 = load("eurostat_lfsi_emp_a_h_lavoro.csv")
cols = ["geo", "anno", "unit", "sex", "OBS_VALUE"]
lf = pd.concat([l1[cols].assign(src=1), l2[cols].assign(src=2)])
lf = lf[lf.sex == "T"].sort_values("src").drop_duplicates(["geo", "anno", "unit"], keep="first")
occ = lf[lf.unit == "THS_PER"][["geo", "anno", "OBS_VALUE"]].rename(
    columns={"OBS_VALUE": "occupati_migliaia"})
tasso = lf[lf.unit == "PC_POP"][["geo", "anno", "OBS_VALUE"]].rename(
    columns={"OBS_VALUE": "tasso_occupazione_2064"})

bo = (ben_t.merge(ben_o, on=["geo", "anno"], how="left")
           .merge(occ, on=["geo", "anno"], how="inner")
           .merge(tasso, on=["geo", "anno"], how="left"))
bo["occupati"] = bo["occupati_migliaia"] * 1000
bo["ben_per_occupato"] = bo["beneficiari_totale"] / bo["occupati"]
bo["ben_vs_per_occupato"] = bo["beneficiari_vecchiaia_superstiti"] / bo["occupati"]
bo = paese(bo)
salva(bo, "pens_beneficiari_occupati.csv",
      ["geo", "paese", "anno", "beneficiari_totale", "beneficiari_vecchiaia_superstiti",
       "occupati", "tasso_occupazione_2064", "ben_per_occupato", "ben_vs_per_occupato"])

bi = bo.set_index(["geo", "anno"])
for g in ["ES", "IT"]:
    for y in [2006, 2013, 2019, 2023, 2024]:
        if (g, y) in bi.index:
            r = bi.loc[(g, y)]
            print(f"  {PAESI[g]} {y}: beneficiari {r.beneficiari_totale/1e6:.2f}M, "
                  f"occupati {r.occupati/1e6:.2f}M, rapporto {r.ben_per_occupato:.3f} "
                  f"(vecchiaia+superstiti {r.ben_vs_per_occupato:.3f})")
    ris[f"ben_occ_{g}"] = {y: bi.loc[(g, y), "ben_per_occupato"]
                           for y in [2006, 2013, 2019, 2023, 2024] if (g, y) in bi.index}

# =====================================================================
# f) Scenario meccanico: tasso di occupazione 2025 costante
# =====================================================================
print("\n[f] Scenario meccanico 'aritmetica pensionistica' (tasso occupazione 2025 costante)")

# tasso implicito = occupati 20-64 (LFS) / popolazione 15-64 (anagrafe), anno base 2025
base = []
for g in ORDINE:
    occ25 = occ[(occ.geo == g) & (occ.anno == 2025)]["occupati_migliaia"]
    p1564_25 = st_1564[(st_1564.geo == g) & (st_1564.anno == 2025)]["valore"]
    tasso25 = tasso[(tasso.geo == g) & (tasso.anno == 2025)]["tasso_occupazione_2064"]
    if occ25.empty or p1564_25.empty:
        continue
    base.append({
        "geo": g,
        "occupati_2025": occ25.iloc[0] * 1000,
        "pop1564_2025": p1564_25.iloc[0],
        "tasso_implicito": occ25.iloc[0] * 1000 / p1564_25.iloc[0],
        "tasso_occ_2064_2025": tasso25.iloc[0] if not tasso25.empty else float("nan"),
    })
base = pd.DataFrame(base).set_index("geo")
print(base.assign(tasso_implicito=lambda d: (100 * d.tasso_implicito).round(1)).to_string())

righe = []
for g in ORDINE:
    if g not in base.index:
        continue
    t = base.loc[g, "tasso_implicito"]
    d = pw[pw.geo == g].set_index("anno")
    for y in range(2025, 2071):
        p1564 = d.loc[y, "Y15-64"]
        p65 = d.loc[y, "Y_GE65"]
        occ_s = t * p1564
        righe.append({
            "geo": g, "anno": y,
            "pop_1564_proiettata": p1564,
            "pop_65p_proiettata": p65,
            "tasso_occupazione_implicito": t,
            "occupati_stimati": occ_s,
            "occupati_per_anziano": occ_s / p65,
            "anziani_per_100_occupati": 100 * p65 / occ_s,
        })
scen = paese(pd.DataFrame(righe))
salva(scen, "pens_scenario_aritmetica.csv",
      ["geo", "paese", "anno", "pop_1564_proiettata", "pop_65p_proiettata",
       "tasso_occupazione_implicito", "occupati_stimati", "occupati_per_anziano",
       "anziani_per_100_occupati"])

si = scen.set_index(["geo", "anno"])
for g in ["ES", "IT", "FR", "DE", "EU27_2020"]:
    vals = {y: si.loc[(g, y), "occupati_per_anziano"] for y in [2025, 2040, 2050, 2070]}
    ris[f"scen_{g}"] = vals
    print(f"  {PAESI[g]:<9} occupati stimati per anziano 65+: " +
          " | ".join(f"{y}: {v:.2f}" for y, v in vals.items()) +
          f"  (calo 2025-2050 {100*(vals[2050]/vals[2025]-1):+.0f}%, "
          f"2025-2070 {100*(vals[2070]/vals[2025]-1):+.0f}%)")

# quanti occupati servirebbero nel 2050 per tenere il rapporto del 2025
# e quale tasso di occupazione sulla popolazione 15-64 implicherebbero
for g in ["ES", "IT", "FR", "DE"]:
    r25 = si.loc[(g, 2025), "occupati_per_anziano"]
    p65_50 = si.loc[(g, 2050), "pop_65p_proiettata"]
    p1564_50 = si.loc[(g, 2050), "pop_1564_proiettata"]
    serv = r25 * p65_50
    ott = si.loc[(g, 2050), "occupati_stimati"]
    tasso_serv = 100 * serv / p1564_50
    ris[f"gap_occ_2050_{g}"] = (serv, ott, serv - ott, tasso_serv)
    print(f"  {PAESI[g]}: per tenere il rapporto 2025 servirebbero {serv/1e6:.2f}M occupati nel 2050 "
          f"(scenario: {ott/1e6:.2f}M, mancano {(serv-ott)/1e6:.2f}M) = un tasso di occupazione "
          f"del {tasso_serv:.0f}% della popolazione 15-64 (oggi {100*base.loc[g,'tasso_implicito']:.0f}%)")

# =====================================================================
# g) Fecondita' e reddito disponibile reale pro capite
# =====================================================================
print("\n[g] TFR e reddito disponibile reale pro capite")

frate = load("eurostat_demo_frate_fecondita.csv")
tfr = frate[["geo", "anno", "OBS_VALUE"]].rename(columns={"OBS_VALUE": "tfr"})
tfr = paese(tfr)
salva(tfr, "demo_fecondita_tfr.csv", ["geo", "paese", "anno", "tfr"])
ti = tfr.set_index(["geo", "anno"])["tfr"]
for g in ["ES", "IT", "FR", "DE"]:
    vals = {y: ti.get((g, y)) for y in [1995, 2008, 2015, 2024]}
    ris[f"tfr_{g}"] = vals
    print(f"  {PAESI[g]:<9} " + " | ".join(f"{y}: {v:.2f}" for y, v in vals.items() if pd.notna(v)))

red = load("eurostat_tepsr_wc310_lavoro.csv")
red = red[["geo", "anno", "OBS_VALUE"]].rename(columns={"OBS_VALUE": "indice_2008_100"})
red = paese(red)
salva(red, "demo_reddito_reale_procapite.csv",
      ["geo", "paese", "anno", "indice_2008_100"])
ri = red.set_index(["geo", "anno"])["indice_2008_100"]
for g in ["ES", "IT", "FR", "DE", "EU27_2020"]:
    vals = {y: ri.get((g, y)) for y in [2008, 2013, 2019, 2024]}
    ris[f"reddito_{g}"] = vals
    print(f"  {PAESI[g]:<9} " + " | ".join(f"{y}: {v:.1f}" for y, v in vals.items() if pd.notna(v)))

# extra di contesto: cuneo fiscale sui bassi salari
cun = load("eurostat_earn_nt_taxwedge_lavoro.csv")
cun = cun[["geo", "anno", "OBS_VALUE"]].rename(columns={"OBS_VALUE": "cuneo_pct"})
cun = paese(cun)
salva(cun, "pens_cuneo_bassi_salari.csv", ["geo", "paese", "anno", "cuneo_pct"])
ci = cun.set_index(["geo", "anno"])["cuneo_pct"]
print("  cuneo su bassi salari 2025: " +
      " | ".join(f"{PAESI[g]} {ci.get((g,2025)):.1f}" for g in ORDINE if pd.notna(ci.get((g, 2025)))))

print("\nFatto. File in", OUT)
