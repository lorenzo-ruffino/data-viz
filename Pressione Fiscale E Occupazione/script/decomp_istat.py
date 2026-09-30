"""Scomposizione della variazione della pressione fiscale 2023->2024 e 2024->2025.

Fonti
- Istat, Conti economici nazionali 22 settembre 2026 (tavole xls): PIL, redditi, ULA,
  consumi, conto AP, prelievo fiscale totale  -> edizione settembre 2026
- Istat SDMX 95_815_DF_DCCN_FPA_5 edizione 2026M4: gettito per singola imposta
  (ultima edizione disponibile), riconciliato con i totali di settembre
- Eurostat nama_10_gdp (D2X3) per ricavare il risultato lordo di gestione + reddito misto
- MEF, statistiche dichiarazioni: quote IRPEF netta dipendenti/pensionati/altri
- Misure: MEF (riforma IRPEF 2024 4,3 mld), UPB (cuneo 2025: IRPEF 8,44 mld, bonus 4,41 mld)
- Elasticita' IRPEF ~1,7 ricavata da UPB (2 punti di inflazione -> ~2,85 mld nel regime 2025)
"""
import os, json
import pandas as pd

IN = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "input", "")
YRS = ["2023", "2024", "2025"]

# ---------- Istat settembre 2026 (tavole comunicato 22/9/2026), milioni -> miliardi
SEPT = {
    "PIL":      {"2023": 2141760.8, "2024": 2210594.7, "2025": 2265002.5},
    "PILr":     {"2023": 1927858.6, "2024": 1948385.6, "2025": 1960421.0},
    "CONS_TER": {"2023": 1249798.3, "2024": 1289137.8, "2025": 1324328.8},
    "D1":       {"2023": 823557.5, "2024": 868445.0, "2025": 902086.4},
    "D11":      {"2023": 604040.3, "2024": 636122.2, "2025": 659246.5},
    "ULA_DIP":  {"2023": 17583.3, "2024": 18142.6, "2025": 18326.2},
    "ULA_TOT":  {"2023": 24608.7, "2024": 25092.9, "2025": 25402.5},
    "D62":      {"2023": 424290, "2024": 445640, "2025": 458798},
    "PRELIEVO": {"2023": 882974, "2024": 933129, "2025": 971650},
    # revisioni settembre vs aprile (Prospetto 9): imposte dirette, indirette, contributi
    "REV_D5":   {"2023": 21, "2024": 54, "2025": -90},
    "REV_D2":   {"2023": 20, "2024": 27, "2025": -704},
    "REV_D61":  {"2023": -55, "2024": -60, "2025": -47},
}
S = {k: {y: v / 1000 for y, v in d.items()} for k, d in SEPT.items()}
BONUS_2025 = 4.41  # bonus cuneo registrato come prestazione sociale (UPB): tolto dalla base pensioni

# ---------- Eurostat D2X3 (imposte nette su produzione, economia totale), edizione aprile
def ej(fn):
    d = json.load(open(IN + fn)); dims = d["id"]; sz = d["size"]
    labs = [sorted(d["dimension"][k]["category"]["index"], key=lambda c: d["dimension"][k]["category"]["index"][c]) for k in dims]
    out = {}
    for key, val in d["value"].items():
        i = int(key); co = []
        for s in reversed(sz): co.append(i % s); i //= s
        out[tuple(labs[j][c] for j, c in enumerate(co[::-1]))] = val
    return out
gd = ej("eurostat_nama_10_gdp_IT_CP.json")
D2X3 = {y: gd[("A", "CP_MEUR", "D2X3", "IT", y)] / 1000 + S["REV_D2"][y] for y in YRS}
S["B2A3G"] = {y: S["PIL"][y] - S["D1"][y] - D2X3[y] for y in YRS}
S["PENS"] = {y: S["D62"][y] - (BONUS_2025 if y == "2025" else 0) for y in YRS}

# ---------- Istat dettaglio imposte, edizione 2026M4
d = pd.read_csv(IN + "istat_95_815_DF_DCCN_FPA_5_2026M4.csv")
d = d[d.TIME_PERIOD.astype(str).isin(YRS)]
X = d.pivot_table(index="DATA_TYPE_AGGR", columns=d.TIME_PERIOD.astype(str), values="OBS_VALUE", aggfunc="sum") / 1000
def x(code, y): return float(X.loc[code, y]) if code in X.index else 0.0

# quote IRPEF netta (MEF, dichiarazioni): anno d'imposta 2023 e 2024; 2025 = 2024
QUOTE = {"2023": (0.533, 0.302), "2024": (0.539, 0.309), "2025": (0.539, 0.309)}

def items(y):
    irpef = x("D51A_C01_C_W0", y) + x("D51A_C02_C_W0", y) + x("D51A_C03_C_W0", y)
    qd, qp = QUOTE[y]
    fin = sum(x(c, y) for c in ["D51A_C04_C_W0", "D51A_C10_C_W0", "D51A_C12_C_W0", "D51A_C13_C_W0",
                                 "D51A_C15_C_W0", "D51B_C01_C_W0", "D51C_T_C_W0", "D214B_C04_C_W0",
                                 "D214C_T_C_W0", "D91C_C10_C_W0"])
    energia = sum(x(c, y) for c in ["D214A_C01_C_W0", "D214A_C02_C_W0", "D214A_C03_C_W0",
                                     "D214A_C05_C_W0", "D214A_C06_C_W0"]) + S["REV_D2"][y]
    imprese = sum(x(c, y) for c in ["D51B_C02_C_W0", "D51B_C08_C_W0", "D51B_C09_C_W0", "D51B_C10_C_W0",
                                     "D29H_C06_C_W0", "D29H_C13_C_W0"])
    autonomi_sost = x("D51A_C16_C_W0", y)
    immobili = x("D29A_T_C_W0", y) + x("D51A_C14_C_W0", y) + x("D59A_C02_C_W0", y) + \
        x("D214B_C05_C_W0", y) + x("D214B_C06_C_W0", y) + x("D214B_C07_C_W0", y)
    iva = x("D211_T_C_W0", y)
    tot_istat = S["PRELIEVO"][y]
    it = {
        # (valore, base, gruppo)
        "Contributi a carico dei dipendenti": (x("D613CE_T_C_W0", y), S["D11"][y], "L"),
        "Contributi a carico dei datori":     (x("D611_T_C_W0", y), S["D11"][y], "L"),
        "IRPEF+addiz. su lavoro dipendente":  (qd * irpef, S["D11"][y], "L"),
        "IRPEF+addiz. su pensioni":           (qp * irpef, S["PENS"][y], "P"),
        "IRPEF+addiz. su altri redditi":      ((1 - qd - qp) * irpef, S["B2A3G"][y], "A"),
        "Contributi autonomi + concordato":   (x("D613CS_T_C_W0", y) + autonomi_sost, S["B2A3G"][y], "A"),
        "IRES, IRAP, sostitutive imprese":    (imprese, S["B2A3G"][y], "K"),
        "Redditi e rendite finanziarie":      (fin, S["PIL"][y], "F"),
        "Energia (accise, oneri di sistema)": (energia, S["CONS_TER"][y], "E"),
        "IVA":                                (iva, S["CONS_TER"][y], "C"),
        "Immobili (IMU, cedolare, registro)": (immobili, S["PIL"][y], "O"),
    }
    it["Altro (tabacchi, giochi, bollo auto, canone, ETS, ...)"] = (
        tot_istat - sum(v[0] for v in it.values()), S["PIL"][y], "O")
    return it, irpef

def step(y0, y1):
    Y0, Y1 = S["PIL"][y0], S["PIL"][y1]; gY = Y1 / Y0
    i0, irp0 = items(y0); i1, irp1 = items(y1)
    rows = []
    for k in i0:
        a0, b0, g = i0[k]; a1, b1, _ = i1[k]
        dlt = 100 * (a1 / Y1 - a0 / Y0)
        comp = 100 * (a0 / Y0) * ((b1 / b0) / gY - 1)
        rows.append(dict(voce=k, gruppo=g, livello=100 * a0 / Y0, delta=dlt, compos=comp, intens=dlt - comp))
    R = pd.DataFrame(rows)
    # quota lavoro: occupazione (ULA dip vs PIL reale) e salario reale per ULA (vs deflatore)
    L = R[R.gruppo == "L"].livello.sum()
    gN = S["ULA_DIP"][y1] / S["ULA_DIP"][y0]; gYr = S["PILr"][y1] / S["PILr"][y0]
    occ = L * (gN / gYr - 1)
    lab_comp = R[R.gruppo == "L"].compos.sum()
    return R, dict(pressione0=100 * S["PRELIEVO"][y0] / Y0, pressione1=100 * S["PRELIEVO"][y1] / Y1,
                   L=L, occ=occ, sal=lab_comp - occ, irp0=irp0, irp1=irp1,
                   gN=gN, gYr=gYr, gW=(S["D11"][y1] / S["ULA_DIP"][y1]) / (S["D11"][y0] / S["ULA_DIP"][y0]),
                   gP=gY / gYr, gPens=S["PENS"][y1] / S["PENS"][y0])

if __name__ == "__main__":
    pd.set_option("display.width", 200)
    for y in YRS:
        print(y, "pressione %.2f  quota D1 %.2f  quota B2A3G %.2f" % (
            100 * S["PRELIEVO"][y] / S["PIL"][y], 100 * S["D1"][y] / S["PIL"][y], 100 * S["B2A3G"][y] / S["PIL"][y]))
    for y0, y1 in [("2023", "2024"), ("2024", "2025")]:
        R, m = step(y0, y1)
        print(f"\n===== {y0}->{y1}: pressione {m['pressione0']:.2f} -> {m['pressione1']:.2f} (Δ {m['pressione1']-m['pressione0']:+.2f})")
        print(R.round(3).to_string(index=False))
        print(f"somma Δ {R.delta.sum():+.3f}  composizione {R.compos.sum():+.3f}  intensità {R.intens.sum():+.3f}")
        print(f"lavoro dip: prelievo {m['L']:.2f}% PIL; compos lavoro {R[R.gruppo=='L'].compos.sum():+.3f} = occupazione {m['occ']:+.3f} + salari {m['sal']:+.3f}")
        print(f"ULA dip {100*(m['gN']-1):+.2f}%  PIL reale {100*(m['gYr']-1):+.2f}%  retrib/ULA {100*(m['gW']-1):+.2f}%  deflatore {100*(m['gP']-1):+.2f}%  pensioni {100*(m['gPens']-1):+.2f}%")
        print(f"IRPEF+addizionali: {m['irp0']:.1f} -> {m['irp1']:.1f}")
