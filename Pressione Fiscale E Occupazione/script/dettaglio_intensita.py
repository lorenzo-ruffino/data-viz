"""Dettaglio ("di cui") dell'effetto intensità: voci elementari Istat in punti di PIL
e scomposizione del blocco IRPEF in misure, fiscal drag e residuo."""
from decomp_istat import X, x, S, step, items
YP = {"2023": S["PIL"]["2023"], "2024": S["PIL"]["2024"], "2025": S["PIL"]["2025"]}
def dpp(codes, y0, y1, extra0=0, extra1=0):
    a0 = sum(x(c, y0) for c in codes) + extra0; a1 = sum(x(c, y1) for c in codes) + extra1
    return a1 - a0, 100 * (a1 / YP[y1] - a0 / YP[y0])
GRUPPI = {
 "Finanza": [("Ritenute su interessi (famiglie)", ["D51A_C04_C_W0"]), ("Ritenute su dividendi (famiglie)", ["D51A_C10_C_W0"]),
             ("Plusvalenze e risparmio gestito", ["D51C_T_C_W0"]), ("Imposta di bollo", ["D214B_C04_C_W0"]),
             ("Assicurazioni vita/prev. compl. (incl. straord. 2025)", ["D51A_C12_C_W0", "D51A_C13_C_W0", "D91C_C10_C_W0"]),
             ("Ritenute interessi imprese, Tobin tax, cripto", ["D51B_C01_C_W0", "D214C_T_C_W0", "D51A_C15_C_W0"])],
 "Energia": [("Energia elettrica + oneri di sistema", ["D214A_C05_C_W0", "D214A_C06_C_W0"]),
             ("Gas metano + oneri gas", ["D214A_C03_C_W0"]), ("Oli minerali e gas incondensabili", ["D214A_C01_C_W0", "D214A_C02_C_W0"])],
 "Imprese": [("IRES", ["D51B_C02_C_W0"]), ("IRAP", ["D29H_C06_C_W0"]), ("Extraprofitti energia e rinnovabili", ["D51B_C09_C_W0", "D29H_C13_C_W0"]),
             ("Rivalutazione beni aziendali", ["D51B_C08_C_W0"]), ("Global minimum tax", ["D51B_C10_C_W0"])],
}
EPS, MARG = 1.7, 0.28   # elasticità IRPEF (UPB) e aliquota marginale media sull'effetto riflesso
MIS = {"2024": {"Riforma IRPEF a tre aliquote (MEF)": -4.3}, "2025": {"Nuova detrazione cuneo (UPB)": -8.44}}
for y0, y1 in [("2023", "2024"), ("2024", "2025")]:
    Y1 = YP[y1]; R, m = step(y0, y1)
    print(f"\n######## {y0}->{y1}")
    for g, lst in GRUPPI.items():
        for lab, codes in lst:
            b, p = dpp(codes, y0, y1); print(f"  {g:8s} {lab:52s} {b:+6.2f} mld  {p:+.3f} pp")
    if y1 == "2025": print(f"  Energia  revisione settembre (accise energia)                        {S['REV_D2'][y1]-S['REV_D2'][y0]:+6.2f} mld")
    # --- cuneo: contributi dipendenti / retribuzioni
    r0 = x("D613CE_T_C_W0", y0) / S["D11"][y0]; r1 = x("D613CE_T_C_W0", y1) / S["D11"][y1]
    dssc = (r1 - r0) * S["D11"][y1]
    refl = -MARG * dssc
    # --- IRPEF block
    ir = R[R.voce.str.startswith("IRPEF")]; irp_int = ir.intens.sum(); irp_comp = ir.compos.sum()
    qd, qp = {"2024": (0.533, 0.302), "2025": (0.539, 0.309)}[y1]
    drag_d = qd * m["irp0"] * (EPS - 1) * (m["gW"] - 1)
    drag_p = qp * m["irp0"] * (EPS - 1) * (m["gPens"] - 1 - 0.003)
    mis = sum(MIS[y1].values())
    resid = irp_int - 100 * (refl + drag_d + drag_p + mis) / Y1
    print(f"  contributi dip./retribuzioni {100*r0:.2f}% -> {100*r1:.2f}%  => decontribuzione {dssc:+.2f} mld ({100*dssc/Y1:+.3f} pp)")
    print(f"  IRPEF+addiz: Δ {ir.delta.sum():+.3f} pp = composizione {irp_comp:+.3f} + intensità {irp_int:+.3f}")
    for k, v in MIS[y1].items(): print(f"     misura {k}: {v:+.2f} mld ({100*v/Y1:+.3f} pp)")
    print(f"     effetto riflesso decontribuzione su IRPEF: {refl:+.2f} mld ({100*refl/Y1:+.3f} pp)")
    print(f"     fiscal drag dipendenti: {drag_d:+.2f} mld ({100*drag_d/Y1:+.3f})  pensionati {drag_p:+.2f} mld ({100*drag_p/Y1:+.3f})")
    print(f"     residuo IRPEF: {resid:+.3f} pp ({resid*Y1/100:+.2f} mld)")
    print(f"  CUNEO netto (contributi + detrazione/riforma cuneo + riflesso): {100*(dssc+refl+(MIS[y1].get('Nuova detrazione cuneo (UPB)',0)))/Y1:+.3f} pp")
