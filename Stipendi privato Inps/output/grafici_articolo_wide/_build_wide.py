"""
Genera 5 CSV in formato wide a partire dagli aggregati, uno per ciascun grafico
dello script 7_grafici_articolo.R.

- 01: anno, stipendio_annuo_fte_reale (totale)
- 02: anno + colonne per fascia di eta' decennale (stipendio_annuo_fte_reale)
- 03: anno + colonne per macro-settore (stipendio_annuo_fte_reale)
- 04: anno + colonne per macro-settore (lavoratori)
- 05: branca, valore_aggiunto_per_occupato_2024_keur, variazione_occupati_pct_2014_2024
"""
import pandas as pd
from pathlib import Path

PROJ = Path("/Users/lorenzoruffino/Documents/Progetti/data-viz/Stipendi privato Inps")
AGG = PROJ / "output" / "aggregati"
OUT = PROJ / "output" / "grafici_articolo_wide"
OUT.mkdir(parents=True, exist_ok=True)

# -- 1. Stipendio reale totale -------------------------------------------------
d1 = (
    pd.read_csv(AGG / "headline.csv")
    .query("vista == 'complessivo'")
    .sort_values("anno")
    [["anno", "stipendio_annuo_fte_reale"]]
    .round(2)
)
d1.to_csv(OUT / "01_stipendio_totale.csv", index=False)

# -- 2. Stipendio per fascia di eta' decennale (base 100 nel 2014) ------------
ord_eta = ["20-29", "30-39", "40-49", "50-59", "60+"]
d2 = (
    pd.read_csv(AGG / "eta_decennale.csv")
    .query("vista == 'complessivo' and eta_decennale != '<20'")
    .pivot(index="anno", columns="eta_decennale", values="stipendio_annuo_fte_reale")
    [ord_eta]
)
d2 = (d2.div(d2.loc[2014]) * 100).round(2).reset_index()
d2.columns.name = None
d2.to_csv(OUT / "02_stipendio_per_eta.csv", index=False)

# -- 3 e 4. Settori macro ------------------------------------------------------
RIN_SET = {
    "Manifattura": "Manifattura",
    "Turismo e ristorazione": "Turismo",
    "Servizi alle imprese": "Servizi alle imprese",
    "Sanita' e assistenza sociale (privata)": "Sanità privata",
    "Finanza e assicurazioni": "Finanza",
    "Istruzione (privata)": "Istruzione privata",
    "Commercio": "Commercio",
    "Costruzioni": "Costruzioni",
    "ICT": "Settore informatico",
    "Trasporti e magazzinaggio": "Trasporti",
}
ord_set = [
    "Settore informatico", "Finanza", "Manifattura", "Trasporti",
    "Commercio", "Costruzioni", "Servizi alle imprese",
    "Sanità privata", "Istruzione privata", "Turismo",
]

set_raw = (
    pd.read_csv(AGG / "settore_macro.csv")
    .query("vista == 'complessivo'")
    .assign(settore=lambda d: d["macro_settore"].map(RIN_SET))
    .dropna(subset=["settore"])
)

d3 = (
    set_raw
    .pivot(index="anno", columns="settore", values="stipendio_annuo_fte_reale")
    [ord_set]
)
d3 = (d3.div(d3.loc[2014]) * 100).round(2).reset_index()
d3.columns.name = None
d3.to_csv(OUT / "03_stipendio_per_settore.csv", index=False)

d4 = (
    set_raw
    .pivot(index="anno", columns="settore", values="lavoratori")
    [ord_set]
)
d4 = (d4.div(d4.loc[2014]) * 100).round(2).reset_index()
d4.columns.name = None
d4.to_csv(OUT / "04_occupati_per_settore.csv", index=False)

# -- 5. Valore aggiunto vs occupati per branca ---------------------------------
RIN_BRANCA = {
    "A": "Agricoltura",
    "B": "Industria estrattiva",
    "C": "Manifattura",
    "D": "Energia",
    "E": "Acqua e rifiuti",
    "F": "Costruzioni",
    "G": "Commercio",
    "H": "Trasporti",
    "I": "Turismo",
    "J": "Settore informatico",
    "K": "Finanza",
    "M": "Servizi professionali",
    "N": "Servizi alle imprese",
    "P": "Istruzione",
    "Q": "Sanità",
    "R": "Cultura",
    "S": "Altri servizi",
    "T": "Lavoro domestico",
}
d5 = (
    pd.read_csv(AGG / "produttivita_branche_istat.csv")
    .query("codice not in ['L', 'O', '__TOT__']")
    .assign(branca=lambda d: d["codice"].map(RIN_BRANCA).fillna(d["branca"]))
    .dropna(subset=["produttivita_kEUR_per_occupato_2024", "occupati_var_pct"])
    .assign(
        valore_aggiunto_per_occupato_2024_keur=lambda d: d["produttivita_kEUR_per_occupato_2024"].round(2),
        variazione_occupati_pct_2014_2024=lambda d: d["occupati_var_pct"].round(2),
    )
    [["branca", "valore_aggiunto_per_occupato_2024_keur", "variazione_occupati_pct_2014_2024"]]
    .sort_values("branca")
)
d5.to_csv(OUT / "05_valore_aggiunto_vs_occupati.csv", index=False)

print("Scritti 5 CSV in", OUT)
