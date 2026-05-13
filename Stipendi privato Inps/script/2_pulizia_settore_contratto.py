"""Pulisce i dati Inps Osservatorio sulla terna anno × settore × tipologia
contrattuale e li unisce in un unico CSV pulito.

Per il 2014-2018 l'estrazione Inps non permette il filtro 'Stagionale': sono
disponibili solo Tempo determinato (file (3)), Tempo indeterminato (file (4))
e Totale senza filtro (file (5)). Lo stagionale viene ricavato come
Totale - Determinato - Indeterminato.

Per 2019-2023 e 2024 le tre tipologie compaiono direttamente nel breakdown
"Tipologia contrattuale".

Output: lavoratori_settore_tipologia_2014_2024.csv
"""

from __future__ import annotations

import os
import re
import unicodedata
from pathlib import Path

import pandas as pd

PROJ = Path("/Users/lorenzoruffino/Documents/Progetti/data-viz/Stipendi privato Inps")
SRC  = PROJ / "dati" / "inps" / "settore_contratto"
OUT  = PROJ / "output" / "dati_puliti" / "lavoratori_settore_tipologia_2014_2024.csv"


# -----------------------------------------------------------------------------
# Helper di parsing (riusati dallo script principale)
# -----------------------------------------------------------------------------
def _safe(value) -> str:
    if value is None:
        return ""
    if isinstance(value, float) and pd.isna(value):
        return ""
    return str(value).strip()


def parse_number(value):
    if value is None:
        return None
    s = str(value).strip()
    if s == "":
        return None
    s = s.replace(".", "").replace(",", ".")
    try:
        f = float(s)
        return int(f) if f.is_integer() else f
    except ValueError:
        return None


def snake_case(name: str) -> str:
    nfkd = unicodedata.normalize("NFKD", name)
    ascii_only = "".join(c for c in nfkd if not unicodedata.combining(c))
    return re.sub(r"[^a-z0-9]+", "_", ascii_only.lower()).strip("_")


DIM_RENAME = {
    "Attivita' economica ATECO 2007": "Attività economica ATECO 2007",
}

MEASURES = [
    "Numero lavoratori con almeno una giornata retribuita nell'anno",
    "Numero settimane retribuite nell'anno",
    "Numero settimane utili nell'anno",
    "Numero giornate retribuite nell'anno",
    "Somma retribuzioni nell'anno",
]


def _normalize_measure(name: str) -> str:
    """Normalizza il nome della misura (a volte ha trailing spaces)."""
    return name.strip()


def parse_inps_table(path: Path, expected_dims: list[str]) -> pd.DataFrame:
    """Parsa una tabella Inps con anni in colonne e {expected_dims} in righe.

    Restituisce un DataFrame long con: anno, {dim cols}, e una colonna per
    ciascuna misura.
    """
    raw = pd.read_csv(path, sep=";", header=None, skiprows=5,
                      dtype=str, quotechar='"')

    period_row = raw.iloc[0].tolist()
    measure_row = raw.iloc[1].tolist()
    body = raw.iloc[2:].reset_index(drop=True)

    # Le dim sono tutte le colonne con header non vuoto, fino al primo "" separatore
    dim_names: list[str] = []
    for cell in measure_row:
        name = _safe(cell)
        if name == "":
            break
        dim_names.append(DIM_RENAME.get(name, name))
    n_dim = len(dim_names)

    if set(dim_names) != set(expected_dims):
        raise ValueError(
            f"Dim attese {expected_dims} ma trovate {dim_names} in {path.name}"
        )

    # Trova gli anni nella riga period_row, posizioni n_dim+1, n_dim+1+5, ...
    anni: list[str] = []
    i = n_dim + 1
    while i < len(period_row):
        cell = _safe(period_row[i])
        if cell.isdigit():
            anni.append(cell)
            i += 5
        else:
            break

    # Costruisci elenco (anno, misura) per ogni colonna misura
    measure_columns: list[tuple[str, str]] = []
    for k, anno in enumerate(anni):
        for j in range(5):
            pos = n_dim + 1 + k * 5 + j
            misura = _safe(measure_row[pos])
            measure_columns.append((anno, _normalize_measure(misura)))

    expected_cols = n_dim + 1 + len(anni) * 5
    body = body.iloc[:, :expected_cols].copy()
    new_cols = list(dim_names) + ["__sep__"] + [f"_m{i}" for i in range(len(measure_columns))]
    body.columns = new_cols
    body = body.drop(columns=["__sep__"])

    # Forward-fill sulle dim e dropna
    for col in dim_names:
        body[col] = body[col].apply(_safe).replace("", pd.NA).ffill()
    body = body.dropna(subset=dim_names)

    # Filtra subtotali (tipologia "Totale", settore "Totale")
    for col in dim_names:
        body = body[body[col] != "Totale"]

    # Melt + parse number
    melted = body.melt(
        id_vars=dim_names,
        value_vars=[f"_m{i}" for i in range(len(measure_columns))],
        var_name="_idx", value_name="_value",
    )
    melted["_i"] = melted["_idx"].str[2:].astype(int)
    melted["anno"] = melted["_i"].map(lambda i: int(measure_columns[i][0]))
    melted["__measure"] = melted["_i"].map(lambda i: measure_columns[i][1])
    melted["_value"] = melted["_value"].map(parse_number)

    pivoted = melted.pivot_table(
        index=["anno"] + dim_names,
        columns="__measure",
        values="_value",
        aggfunc="first",
    ).reset_index()
    pivoted.columns.name = None
    return pivoted, dim_names


# -----------------------------------------------------------------------------
# 1. 2014-2018: tre file da combinare (det, indet, totale -> stagionale)
# -----------------------------------------------------------------------------
ATECO = "Attività economica ATECO 2007"
TIPOL = "Tipologia contrattuale"

det_2014, _   = parse_inps_table(SRC / "2014-2018 tempo determinato.csv",   [ATECO])
indet_2014, _ = parse_inps_table(SRC / "2014-2018 tempo indeterminato.csv", [ATECO])
tot_2014, _   = parse_inps_table(SRC / "2014-2018 totale.csv",              [ATECO])

det_2014[TIPOL]   = "Tempo determinato"
indet_2014[TIPOL] = "Tempo indeterminato"

# Stagionale = Totale - Det - Indet, calcolato per ogni (anno, settore)
KEY = ["anno", ATECO]
merged = (
    tot_2014.merge(det_2014, on=KEY, suffixes=("", "_det"))
            .merge(indet_2014, on=KEY, suffixes=("", "_indet"))
)

stag_2014_rows = []
for _, row in merged.iterrows():
    out = {"anno": row["anno"], ATECO: row[ATECO], TIPOL: "Stagionale"}
    for m in MEASURES:
        tot = row[m]
        det = row[f"{m}_det"]
        ind = row[f"{m}_indet"]
        if pd.notna(tot) and pd.notna(det) and pd.notna(ind):
            out[m] = tot - det - ind
        else:
            out[m] = None
    stag_2014_rows.append(out)
stag_2014 = pd.DataFrame(stag_2014_rows)

# Drop tot_2014 (subtotale), tieni det/indet/stag
df_2014 = pd.concat(
    [det_2014, indet_2014, stag_2014],
    ignore_index=True
)

# -----------------------------------------------------------------------------
# 2. 2019-2023: un solo file con tutte le tipologie
# -----------------------------------------------------------------------------
df_2019, _ = parse_inps_table(SRC / "2019-2023.csv", [ATECO, TIPOL])

# -----------------------------------------------------------------------------
# 3. 2024
# -----------------------------------------------------------------------------
df_2024, _ = parse_inps_table(SRC / "2024.csv", [ATECO, TIPOL])

# -----------------------------------------------------------------------------
# 4. Concatenazione finale
# -----------------------------------------------------------------------------
df = pd.concat([df_2014, df_2019, df_2024], ignore_index=True)
df = df.sort_values(by=["anno", ATECO, TIPOL]).reset_index(drop=True)

# Riordina colonne
ordered = ["anno", ATECO, TIPOL] + [m for m in MEASURES if m in df.columns]
df = df[ordered]

# Snake case headers
df.columns = [snake_case(c) for c in df.columns]

OUT.parent.mkdir(parents=True, exist_ok=True)
df.to_csv(OUT, index=False, encoding="utf-8")

print(f"Scritto {OUT}: {len(df)} righe")
print(f"Anni: {sorted(df['anno'].unique())}")
print(f"Tipologie: {sorted(df['tipologia_contrattuale'].unique())}")
print(f"Settori: {df['attivita_economica_ateco_2007'].nunique()}")
print()

# Spot check: somma per anno del totale lavoratori, confrontata col file
# precedente (lavoratori_attivita_economica_ateco_2014_2024.csv totale).
print("Lavoratori totali (privato) per anno:")
tot = df.groupby("anno")["numero_lavoratori_con_almeno_una_giornata_retribuita_nell_anno"].sum()
for anno, val in tot.items():
    print(f"  {anno}: {val:,.0f}")
