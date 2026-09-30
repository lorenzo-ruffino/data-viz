"""Varianti di robustezza emerse dalla revisione:
(a) base ritardata di un anno per le imposte in autoliquidazione (IRES/IRAP, autonomi, IRPEF altri redditi)
(b) effetto occupazione misurato con ULA totali invece che dipendenti
"""
import decomp_istat as D
S = D.S
# B2A3G 2022 (Istat set. 2026 PIL e D1; D2X3 Eurostat aprile)
S["PIL"]["2022"] = 1998.2691; S["D1"]["2022"] = 783.5666
S["B2A3G"]["2022"] = S["PIL"]["2022"] - S["D1"]["2022"] - D.gd[("A","CP_MEUR","D2X3","IT","2022")]/1000
LAGGED = ["IRPEF+addiz. su altri redditi", "Contributi autonomi + concordato", "IRES, IRAP, sostitutive imprese"]
prev = {"2023": "2022", "2024": "2023", "2025": "2024"}
def run(y0, y1, lag):
    R, m = D.step(y0, y1)
    if lag:
        gY = S["PIL"][y1] / S["PIL"][y0]
        gB = S["B2A3G"][y0] / S["B2A3G"][prev[y0]]      # crescita base dell'anno prima
        for i, r in R.iterrows():
            if r.voce in LAGGED:
                R.at[i, "compos"] = r.livello * (gB / gY - 1); R.at[i, "intens"] = r.delta - R.at[i, "compos"]
    return R, m
for y0, y1 in [("2023", "2024"), ("2024", "2025")]:
    for lag in (False, True):
        R, m = run(y0, y1, lag)
        g = R.groupby("gruppo")[["delta", "compos", "intens"]].sum()
        L = R[R.gruppo == "L"].compos.sum()
        occ_tot = m["L"] * ((S["ULA_TOT"][y1] / S["ULA_TOT"][y0]) / m["gYr"] - 1)
        print(f"{y0}->{y1} lag={lag}: Δ {R.delta.sum():+.3f} compos {R.compos.sum():+.3f} intens {R.intens.sum():+.3f} | "
              f"lav {L:+.3f} (occ dip {m['occ']:+.3f}, occ ULA tot {occ_tot:+.3f}, salari {m['sal']:+.3f}) "
              f"pens {g.compos.get('P',0):+.3f} prof/aut {g.compos.get('A',0)+g.compos.get('K',0):+.3f} "
              f"cons/ener {g.compos.get('C',0)+g.compos.get('E',0):+.3f} || intens K+A {g.intens.get('A',0)+g.intens.get('K',0):+.3f}")
print("B2A3G crescita:", {y: round(100*(S['B2A3G'][y]/S['B2A3G'][prev[y]]-1),2) for y in ['2023','2024','2025']})
