#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Modello Spagnolo — decomposizione contabile della crescita (ES vs IT, con FR/DE/EU27).

Input: CSV Eurostat SDMX gia' scaricati in ../input/ (nessun accesso a internet).
Output: CSV tidy (long, decimale con punto) in ../output/dati/ con prefisso `crescita_`.

Identita' contabile usata:
    PIL = POP  x  (OCC / POP)  x  (ORE / OCC)  x  (PIL / ORE)
        = popolazione x tasso di occupazione x ore per occupato x produttivita' oraria

Decomposizione in logaritmi: ln(PIL_T/PIL_0) = somma dei ln dei quattro fattori.
I contributi in log sono poi riscalati in modo che sommino esattamente alla crescita
cumulata in percentuale del PIL reale (colonna `contributo_pp`).

Concetti e scelte (documentate anche in output/findings_crescita.md):
- PIL reale: B1GQ, volumi concatenati 2015, milioni di euro (CLV15_MEUR), NAMA_10_GDP.
- Popolazione: POP_NC (popolazione totale dei conti nazionali, media annua), NAMA_10_PE.
- Occupati: EMP_DC (concetto interno/domestico). E' il concetto coerente con il PIL e con
  le ore: si verifica numericamente che ore totali (NAMA_10R_2EMHRW) = EMP_DC x HW_EMP.
  EMP_NC (concetto nazionale) e' usato solo come variante di robustezza; non esiste per EU27.
- Ore per occupato: HW_EMP in ore (NAMA_10_LP_ULC), disponibile 1995-2025 per tutti i paesi
  (il file regionale delle ore si ferma al 2023 per IT/DE, quindi non e' usato per il calcolo).
- Produttivita' oraria: ricavata come residuo, PIL reale / ore totali (euro 2015 per ora).
  Coerenza verificata contro l'indice ufficiale RLPR_HW (2015=100).
"""

from pathlib import Path
import numpy as np
import pandas as pd

BASE = Path(__file__).resolve().parent.parent
INPUT = BASE / "input"
OUT = BASE / "output" / "dati"
OUT.mkdir(parents=True, exist_ok=True)

PAESI = ["ES", "IT", "FR", "DE", "EU27_2020"]
NOMI = {
    "ES": "Spagna",
    "IT": "Italia",
    "FR": "Francia",
    "DE": "Germania",
    "EU27_2020": "Unione europea",
}

PERIODI = [
    ("1995-2007", 1995, 2007),
    ("2007-2013", 2007, 2013),
    ("2013-2019", 2013, 2019),
    ("2019-2025", 2019, 2025),
    ("1995-2025", 1995, 2025),
    ("2000-2025", 2000, 2025),
]

COMPONENTI = {
    "popolazione": "Popolazione",
    "tasso_occupazione": "Occupati su popolazione",
    "ore_per_occupato": "Ore per occupato",
    "produttivita_oraria": "Produttività oraria",
}


# ---------------------------------------------------------------- caricamento
def carica(nome):
    d = pd.read_csv(INPUT / nome)
    d["anno"] = pd.to_datetime(d["TIME_PERIOD"]).dt.year
    return d


def serie(d, **filtri):
    """Estrae una serie (geo x anno) come DataFrame wide: righe=anno, colonne=geo."""
    m = pd.Series(True, index=d.index)
    for k, v in filtri.items():
        m &= d[k] == v
    s = d[m]
    piv = s.pivot_table(index="anno", columns="geo", values="OBS_VALUE")
    return piv.reindex(columns=[c for c in PAESI if c in piv.columns])


gdp = carica("eurostat_nama_10_gdp_macro.csv")
pc = carica("eurostat_nama_10_pc_macro.csv")
lp = carica("eurostat_nama_10_lp_ulc_macro.csv")
pe = carica("eurostat_nama_10_pe_macro.csv")
ore_reg = carica("eurostat_nama_10r_2emhrw_ore.csv")

PIL = serie(gdp, na_item="B1GQ", unit="CLV15_MEUR")          # mln euro cat. 2015
PIL_CP = serie(gdp, na_item="B1GQ", unit="CP_MEUR")          # mln euro correnti
PIL_PC = serie(pc, na_item="B1GQ", unit="CLV15_EUR_HAB")     # euro cat. 2015 per abitante
PIL_PC_PPS = serie(pc, na_item="B1GQ", unit="CP_PPS_EU27_2020_HAB")
POP = serie(pe, na_item="POP_NC", unit="THS_PER")            # migliaia
OCC_DC = serie(pe, na_item="EMP_DC", unit="THS_PER")         # migliaia (concetto interno)
OCC_NC = serie(pe, na_item="EMP_NC", unit="THS_PER")         # migliaia (concetto nazionale)
ORE_OCC = serie(lp, na_item="HW_EMP", unit="HW")             # ore annue per occupato
ORE_HAB = serie(lp, na_item="HW_HAB", unit="HW")             # ore annue per abitante
RLPR_HW = serie(lp, na_item="RLPR_HW", unit="I15")           # indice 2015=100
RLPR_PER = serie(lp, na_item="RLPR_PER", unit="I15")         # indice 2015=100

ore_tot_reg = ore_reg[(ore_reg["wstatus"] == "EMP") & (ore_reg["nace_r2"] == "TOTAL")]
ORE_TOT_REG = ore_tot_reg.pivot_table(index="anno", columns="geo", values="OBS_VALUE")
ORE_TOT_REG = ORE_TOT_REG.reindex(columns=[c for c in PAESI if c in ORE_TOT_REG.columns])


# ------------------------------------------------------- grandezze derivate
# ore totali (migliaia di ore) = occupati (migliaia) x ore per occupato
ORE_TOT = OCC_DC * ORE_OCC
# produttivita' oraria reale come residuo: PIL (mln euro 2015) / ore (migliaia) -> euro/ora
PROD_H = PIL * 1_000_000 / (ORE_TOT * 1_000)
# produttivita' per occupato come residuo: euro 2015 per occupato
PROD_OCC = PIL * 1_000_000 / (OCC_DC * 1_000)
# tasso di occupazione "grezzo": occupati su popolazione totale (%)
OCC_SU_POP = OCC_DC / POP * 100
OCC_SU_POP_NC = OCC_NC / POP * 100


def indice(df, base=1995):
    return df / df.loc[base] * 100


# ---------------------------------------------------- controlli di coerenza
def controlli():
    righe = []
    # 1) ore totali calcolate vs file regionale
    comuni = ORE_TOT_REG.index.intersection(ORE_TOT.index)
    for g in ORE_TOT_REG.columns:
        a = ORE_TOT.loc[comuni, g]
        b = ORE_TOT_REG.loc[comuni, g].dropna()
        idx = a.index.intersection(b.index)
        scarto = ((a[idx] / b[idx] - 1) * 100).abs()
        righe.append(
            {
                "controllo": "ore_totali_calcolate_vs_file_regionale",
                "paese": NOMI[g],
                "anni": f"{idx.min()}-{idx.max()}",
                "scarto_max_pc": round(float(scarto.max()), 4),
            }
        )
    # 2) produttivita' oraria residuo vs indice ufficiale RLPR_HW
    for g in PIL.columns:
        a = PROD_H[g] / PROD_H[g].loc[2015] * 100
        b = RLPR_HW[g]
        scarto = (a - b).abs()
        righe.append(
            {
                "controllo": "prod_oraria_residuo_vs_RLPR_HW_indice2015",
                "paese": NOMI[g],
                "anni": f"{a.index.min()}-{a.index.max()}",
                "scarto_max_pc": round(float(scarto.max()), 4),
            }
        )
    # 3) PIL pro capite ricalcolato vs serie ufficiale
    for g in PIL.columns:
        a = PIL[g] * 1_000_000 / (POP[g] * 1_000)
        scarto = ((a / PIL_PC[g] - 1) * 100).abs()
        righe.append(
            {
                "controllo": "pil_procapite_ricalcolato_vs_ufficiale",
                "paese": NOMI[g],
                "anni": f"{a.index.min()}-{a.index.max()}",
                "scarto_max_pc": round(float(scarto.max()), 4),
            }
        )
    return pd.DataFrame(righe)


# ----------------------------------------------------------- decomposizione
def decomponi(occ, etichetta_concetto):
    """Decomposizione log della crescita del PIL reale nei quattro fattori."""
    pop = POP
    ore_occ = ORE_OCC
    ore_tot = occ * ore_occ
    prod_h = PIL * 1_000_000 / (ore_tot * 1_000)
    tasso = occ / pop

    fattori = {
        "popolazione": pop,
        "tasso_occupazione": tasso,
        "ore_per_occupato": ore_occ,
        "produttivita_oraria": prod_h,
    }

    righe = []
    for g in PIL.columns:
        if g not in occ.columns or occ[g].isna().all():
            continue
        for nome_p, a0, a1 in PERIODI:
            n = a1 - a0
            try:
                pil0, pil1 = PIL[g].loc[a0], PIL[g].loc[a1]
            except KeyError:
                continue
            if np.isnan(pil0) or np.isnan(pil1):
                continue
            log_tot = np.log(pil1 / pil0)
            cresc_tot_pp = (pil1 / pil0 - 1) * 100
            cagr_pp = ((pil1 / pil0) ** (1 / n) - 1) * 100
            logs = {}
            saltare = False
            for k, df in fattori.items():
                v0, v1 = df[g].loc[a0], df[g].loc[a1]
                if np.isnan(v0) or np.isnan(v1):
                    saltare = True
                    break
                logs[k] = np.log(v1 / v0)
            if saltare:
                continue
            somma_log = sum(logs.values())
            # riscalatura: i contributi sommano alla crescita cumulata in %
            k_scala = cresc_tot_pp / (somma_log * 100) if abs(somma_log) > 1e-12 else np.nan
            for k, lg in logs.items():
                righe.append(
                    {
                        "paese": NOMI[g],
                        "codice_paese": g,
                        "periodo": nome_p,
                        "anno_inizio": a0,
                        "anno_fine": a1,
                        "n_anni": n,
                        "componente": k,
                        "etichetta": COMPONENTI[k],
                        "contributo_pp": round(lg * 100 * k_scala, 3),
                        "contributo_log_pp": round(lg * 100, 3),
                        "contributo_annuo_pp": round(lg * 100 / n, 3),
                        "quota_su_crescita": (
                            round(lg / somma_log, 4) if abs(somma_log) > 1e-9 else np.nan
                        ),
                        "crescita_pil_pp": round(cresc_tot_pp, 3),
                        "crescita_pil_annua_pp": round(cagr_pp, 3),
                        "concetto_occupazione": etichetta_concetto,
                    }
                )
    col = [
        "paese", "codice_paese", "periodo", "anno_inizio", "anno_fine", "n_anni",
        "componente", "etichetta", "contributo_pp", "contributo_log_pp",
        "contributo_annuo_pp", "quota_su_crescita", "crescita_pil_pp",
        "crescita_pil_annua_pp", "concetto_occupazione",
    ]
    return pd.DataFrame(righe)[col]


def decomponi_procapite():
    """Stessa identita' al netto della popolazione: PIL/POP = (OCC/POP) x (ORE/OCC) x (PIL/ORE)."""
    tasso = OCC_DC / POP
    prod_h = PROD_H
    pil_pc = PIL * 1_000_000 / (POP * 1_000)  # euro 2015 per abitante (ricalcolato)
    fattori = {
        "tasso_occupazione": tasso,
        "ore_per_occupato": ORE_OCC,
        "produttivita_oraria": prod_h,
    }
    righe = []
    for g in PIL.columns:
        for nome_p, a0, a1 in PERIODI:
            n = a1 - a0
            v0, v1 = pil_pc[g].loc[a0], pil_pc[g].loc[a1]
            if np.isnan(v0) or np.isnan(v1):
                continue
            cresc_tot_pp = (v1 / v0 - 1) * 100
            logs = {k: np.log(df[g].loc[a1] / df[g].loc[a0]) for k, df in fattori.items()}
            somma_log = sum(logs.values())
            k_scala = cresc_tot_pp / (somma_log * 100) if abs(somma_log) > 1e-12 else np.nan
            for k, lg in logs.items():
                righe.append(
                    {
                        "paese": NOMI[g],
                        "codice_paese": g,
                        "periodo": nome_p,
                        "anno_inizio": a0,
                        "anno_fine": a1,
                        "n_anni": n,
                        "componente": k,
                        "etichetta": COMPONENTI[k],
                        "contributo_pp": round(lg * 100 * k_scala, 3),
                        "contributo_log_pp": round(lg * 100, 3),
                        "contributo_annuo_pp": round(lg * 100 / n, 3),
                        "quota_su_crescita": (
                            round(lg / somma_log, 4) if abs(somma_log) > 1e-9 else np.nan
                        ),
                        "crescita_pil_pc_pp": round(cresc_tot_pp, 3),
                        "crescita_pil_pc_annua_pp": round(((v1 / v0) ** (1 / n) - 1) * 100, 3),
                    }
                )
    return pd.DataFrame(righe)


# ------------------------------------------------- crescite per periodo (a,b)
def crescite_periodi():
    misure = {
        "pil_reale": PIL,
        "pil_pro_capite_reale": PIL_PC,
        "pil_pro_capite_pps": PIL_PC_PPS,
        "popolazione": POP,
        "occupati": OCC_DC,
        "ore_per_occupato": ORE_OCC,
        "ore_totali": ORE_TOT,
        "occupati_su_popolazione": OCC_SU_POP,
        "produttivita_oraria_residuo": PROD_H,
        "produttivita_per_occupato_residuo": PROD_OCC,
        "produttivita_oraria_rlpr_hw": RLPR_HW,
        "produttivita_per_occupato_rlpr_per": RLPR_PER,
    }
    etich = {
        "pil_reale": "PIL reale (volumi 2015)",
        "pil_pro_capite_reale": "PIL pro capite reale (volumi 2015)",
        "pil_pro_capite_pps": "PIL pro capite in PPS",
        "popolazione": "Popolazione totale",
        "occupati": "Occupati",
        "ore_per_occupato": "Ore per occupato",
        "ore_totali": "Ore totali lavorate",
        "occupati_su_popolazione": "Occupati su popolazione",
        "produttivita_oraria_residuo": "Produttività oraria (PIL/ore)",
        "produttivita_per_occupato_residuo": "Produttività per occupato (PIL/occupati)",
        "produttivita_oraria_rlpr_hw": "Produttività oraria (indice Eurostat)",
        "produttivita_per_occupato_rlpr_per": "Produttività per occupato (indice Eurostat)",
    }
    righe = []
    for chiave, df in misure.items():
        for g in df.columns:
            if df[g].isna().all():
                continue
            for nome_p, a0, a1 in PERIODI:
                if a0 not in df.index or a1 not in df.index:
                    continue
                v0, v1 = df[g].loc[a0], df[g].loc[a1]
                if np.isnan(v0) or np.isnan(v1) or v0 == 0:
                    continue
                n = a1 - a0
                righe.append(
                    {
                        "paese": NOMI[g],
                        "codice_paese": g,
                        "indicatore": chiave,
                        "etichetta": etich[chiave],
                        "periodo": nome_p,
                        "anno_inizio": a0,
                        "anno_fine": a1,
                        "valore_inizio": round(float(v0), 3),
                        "valore_fine": round(float(v1), 3),
                        "crescita_pp": round((v1 / v0 - 1) * 100, 3),
                        "crescita_annua_pp": round(((v1 / v0) ** (1 / n) - 1) * 100, 3),
                    }
                )
    return pd.DataFrame(righe)


# ------------------------------------------------------------- serie annuali
def lunga(df, indicatore, etichetta, decimali=3):
    d = df.reset_index().melt(id_vars="anno", var_name="codice_paese", value_name="valore")
    d = d.dropna(subset=["valore"])
    d["paese"] = d["codice_paese"].map(NOMI)
    d["indicatore"] = indicatore
    d["etichetta"] = etichetta
    d["valore"] = d["valore"].round(decimali)
    return d[["paese", "codice_paese", "anno", "indicatore", "etichetta", "valore"]]


def serie_pil_procapite():
    parti = [
        lunga(PIL_PC, "pil_pc_eur_2015", "PIL pro capite, euro a valori 2015", 1),
        lunga(indice(PIL_PC), "pil_pc_indice_1995", "PIL pro capite, indice 1995=100", 2),
        lunga(indice(PIL_PC, 2007), "pil_pc_indice_2007", "PIL pro capite, indice 2007=100", 2),
        lunga(PIL_PC_PPS, "pil_pc_pps_eu27", "PIL pro capite in PPS (UE27=100 nell'anno)", 1),
        lunga(PIL, "pil_mln_eur_2015", "PIL reale, milioni di euro a valori 2015", 1),
        lunga(indice(PIL), "pil_indice_1995", "PIL reale, indice 1995=100", 2),
        lunga(indice(PIL, 2007), "pil_indice_2007", "PIL reale, indice 2007=100", 2),
    ]
    return pd.concat(parti, ignore_index=True)


def serie_produttivita():
    parti = [
        lunga(RLPR_HW, "rlpr_hw_i15", "Produttività oraria reale, indice 2015=100", 2),
        lunga(RLPR_PER, "rlpr_per_i15", "Produttività per occupato, indice 2015=100", 2),
        lunga(indice(RLPR_HW), "rlpr_hw_indice_1995", "Produttività oraria, indice 1995=100", 2),
        lunga(indice(RLPR_PER), "rlpr_per_indice_1995", "Produttività per occupato, indice 1995=100", 2),
        lunga(PROD_H, "prod_oraria_eur", "Produttività oraria (PIL/ore), euro 2015", 2),
        lunga(indice(PROD_H), "prod_oraria_indice_1995", "Produttività oraria (PIL/ore), indice 1995=100", 2),
        lunga(PROD_OCC, "prod_per_occupato_eur", "Produttività per occupato (PIL/occupati), euro 2015", 0),
    ]
    return pd.concat(parti, ignore_index=True)


def serie_occupati_pop():
    parti = [
        lunga(OCC_SU_POP, "occupati_su_pop_pc", "Occupati su popolazione totale (%)", 3),
        lunga(OCC_SU_POP_NC, "occupati_su_pop_pc_nc", "Occupati (concetto nazionale) su popolazione (%)", 3),
        lunga(POP, "popolazione_migliaia", "Popolazione totale (migliaia)", 1),
        lunga(indice(POP), "popolazione_indice_1995", "Popolazione, indice 1995=100", 2),
        lunga(OCC_DC, "occupati_migliaia", "Occupati, concetto interno (migliaia)", 1),
        lunga(indice(OCC_DC), "occupati_indice_1995", "Occupati, indice 1995=100", 2),
        lunga(ORE_OCC, "ore_per_occupato", "Ore annue lavorate per occupato", 1),
        lunga(ORE_HAB, "ore_per_abitante", "Ore annue lavorate per abitante", 1),
        lunga(ORE_TOT / 1000, "ore_totali_mln", "Ore totali lavorate (milioni)", 1),
        lunga(indice(ORE_TOT), "ore_totali_indice_1995", "Ore totali lavorate, indice 1995=100", 2),
    ]
    return pd.concat(parti, ignore_index=True)


# --------------------------------------------------------------------- main
def main():
    dec = decomponi(OCC_DC, "EMP_DC (concetto interno)")
    dec_nc = decomponi(OCC_NC, "EMP_NC (concetto nazionale)")

    dec_pc = decomponi_procapite()

    dec.to_csv(OUT / "crescita_decomposizione_periodi.csv", index=False)
    dec_nc.to_csv(OUT / "crescita_decomposizione_periodi_empnc.csv", index=False)
    dec_pc.to_csv(OUT / "crescita_decomposizione_procapite.csv", index=False)
    crescite_periodi().to_csv(OUT / "crescita_indicatori_periodi.csv", index=False)
    serie_pil_procapite().to_csv(OUT / "crescita_pil_procapite_serie.csv", index=False)
    serie_produttivita().to_csv(OUT / "crescita_produttivita_serie.csv", index=False)
    serie_occupati_pop().to_csv(OUT / "crescita_occupati_pop_serie.csv", index=False)
    ctrl = controlli()
    ctrl.to_csv(OUT / "crescita_controlli_coerenza.csv", index=False)

    print("File scritti in", OUT)
    for f in sorted(OUT.glob("crescita_*.csv")):
        print(" -", f.name, sum(1 for _ in f.open()) - 1, "righe")

    print("\n--- Controlli di coerenza (scarto massimo in %) ---")
    print(ctrl.to_string(index=False))

    print("\n--- Decomposizione, contributi in punti percentuali (EMP_DC) ---")
    piv = dec.pivot_table(
        index=["codice_paese", "periodo"], columns="componente",
        values="contributo_pp", aggfunc="first",
    )
    tot = dec.groupby(["codice_paese", "periodo"])["crescita_pil_pp"].first()
    piv = piv[["popolazione", "tasso_occupazione", "ore_per_occupato", "produttivita_oraria"]]
    piv["TOTALE_PIL"] = tot
    print(piv.round(1).to_string())

    print("\n--- Contributi annui in punti percentuali (EMP_DC) ---")
    piva = dec.pivot_table(
        index=["codice_paese", "periodo"], columns="componente",
        values="contributo_annuo_pp", aggfunc="first",
    )[["popolazione", "tasso_occupazione", "ore_per_occupato", "produttivita_oraria"]]
    piva["TOTALE_annuo"] = dec.groupby(["codice_paese", "periodo"])["crescita_pil_annua_pp"].first()
    print(piva.round(2).to_string())

    print("\n--- Variante EMP_NC (robustezza) ---")
    pivn = dec_nc.pivot_table(
        index=["codice_paese", "periodo"], columns="componente",
        values="contributo_pp", aggfunc="first",
    )[["popolazione", "tasso_occupazione", "ore_per_occupato", "produttivita_oraria"]]
    print(pivn.round(1).to_string())

    print("\n--- Decomposizione del PIL PRO CAPITE, contributi in pp ---")
    pivp = dec_pc.pivot_table(
        index=["codice_paese", "periodo"], columns="componente",
        values="contributo_pp", aggfunc="first",
    )[["tasso_occupazione", "ore_per_occupato", "produttivita_oraria"]]
    pivp["TOTALE_PIL_PC"] = dec_pc.groupby(["codice_paese", "periodo"])["crescita_pil_pc_pp"].first()
    print(pivp.round(1).to_string())

    print("\n--- Crescite per periodo, indicatori chiave (%) ---")
    cp = crescite_periodi()
    chiave = [
        "pil_reale", "pil_pro_capite_reale", "popolazione", "occupati",
        "ore_per_occupato", "produttivita_oraria_rlpr_hw",
        "produttivita_per_occupato_rlpr_per",
    ]
    print(
        cp[cp.indicatore.isin(chiave)]
        .pivot_table(index=["codice_paese", "periodo"], columns="indicatore", values="crescita_pp")
        [chiave].round(1).to_string()
    )

    print("\n--- Livelli chiave ---")
    anni = [1995, 2000, 2007, 2013, 2019, 2024, 2025]
    for nome, df, dec_n in [
        ("PIL pro capite reale (euro 2015)", PIL_PC, 0),
        ("PIL pro capite PPS", PIL_PC_PPS, 0),
        ("Produttività oraria RLPR_HW (2015=100)", RLPR_HW, 1),
        ("Produttività per occupato RLPR_PER (2015=100)", RLPR_PER, 1),
        ("Occupati su popolazione (%)", OCC_SU_POP, 1),
        ("Ore per occupato", ORE_OCC, 0),
        ("Popolazione (migliaia)", POP, 0),
        ("Produttività oraria residuo (euro 2015/ora)", PROD_H, 1),
    ]:
        print(f"\n{nome}")
        print(df.loc[anni].round(dec_n).to_string())

    print("\n--- Rapporto Spagna/Italia ---")
    rap = pd.DataFrame(
        {
            "pil_pc_reale_ES_su_IT": (PIL_PC["ES"] / PIL_PC["IT"] * 100),
            "pil_pc_pps_ES_su_IT": (PIL_PC_PPS["ES"] / PIL_PC_PPS["IT"] * 100),
            "pil_pc_pps_ES_su_UE": (PIL_PC_PPS["ES"] / PIL_PC_PPS["EU27_2020"] * 100),
            "pil_pc_pps_IT_su_UE": (PIL_PC_PPS["IT"] / PIL_PC_PPS["EU27_2020"] * 100),
        }
    )
    print(rap.loc[anni].round(1).to_string())


if __name__ == "__main__":
    main()
