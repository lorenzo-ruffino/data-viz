"""Pulisce ed unisce le estrazioni dell'Osservatorio Inps sui lavoratori dipendenti.

Per ogni breakdown produce un singolo CSV con anni 2014-2024, senza subtotali,
con il "Periodo retribuito dal datore di lavoro" pivotato su righe e le 5
misure mantenute come colonne.
"""

from __future__ import annotations

import os
import re
import unicodedata

import pandas as pd

DATI_DIR = "/Users/lorenzoruffino/Documents/Progetti/data-viz/Stipendi privato Inps/dati"
OUT_DIR = "/Users/lorenzoruffino/Documents/Progetti/data-viz/Stipendi privato Inps"

YEAR_FILES = [
    "Anni 2014-2018 (classificazione dell'attività economica ISTAT ATECO 2007)",
    "Anni 2019-2023 (classificazione dell'attività economica ISTAT ATECO 2007)",
    "Anno 2024 (classificazione dell'attività economica ISTAT ATECO 2007)",
]

GROUPS = [
    {
        "suffix": "",
        "out_name": "lavoratori_eta_sesso_2014_2024.csv",
        "dim_cols": ["Anno", "Classe di età", "Sesso"],
    },
    {
        "suffix": "(1)",
        "out_name": "lavoratori_attivita_economica_ateco_2014_2024.csv",
        "dim_cols": ["Anno", "Attività economica ATECO 2007"],
    },
    {
        "suffix": "(2)",
        "out_name": "lavoratori_tempo_parziale_sesso_2014_2024.csv",
        "dim_cols": ["Anno", "Presenza tempo parziale nell'anno", "Sesso"],
    },
    {
        "suffix": "(3)",
        "out_name": "lavoratori_eta_tipologia_contrattuale_2014_2024.csv",
        "dim_cols": ["Anno", "Classe di età", "Tipologia contrattuale"],
    },
]

DIM_RENAME = {
    "Attivita' economica ATECO 2007": "Attività economica ATECO 2007",
}

PERIOD_NORMALIZE = {
    "Finoa 3 mesi": "Fino a 3 mesi",
    "Oltre3 e fino a 6 mesi": "Oltre 3 e fino a 6 mesi",
    "Oltre6 e meno di 12 mesi": "Oltre 6 e meno di 12 mesi",
    "Annointero": "Anno intero",
    "Totale": "Totale",
}

PERIOD_ORDER = [
    "Fino a 3 mesi",
    "Oltre 3 e fino a 6 mesi",
    "Oltre 6 e meno di 12 mesi",
    "Anno intero",
]

MEASURES_ORDER = [
    "Numero lavoratori con almeno una giornata retribuita nell'anno",
    "Numero settimane retribuite nell'anno",
    "Numero settimane utili nell'anno",
    "Numero giornate retribuite nell'anno",
    "Somma retribuzioni nell'anno",
]


def snake_case(name: str) -> str:
    """Header in snake_case: minuscolo, accenti rimossi, parole separate da `_`."""
    nfkd = unicodedata.normalize("NFKD", name)
    ascii_only = "".join(c for c in nfkd if not unicodedata.combining(c))
    lowered = ascii_only.lower()
    cleaned = re.sub(r"[^a-z0-9]+", "_", lowered)
    return cleaned.strip("_")


def _safe_str(value) -> str:
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


def clean_text(value):
    if value is None:
        return None
    s = str(value).strip()
    return s if s else None


def process_file(path: str, expected_dim_cols: list[str]) -> pd.DataFrame:
    # I primi 5 record sono intestazione/garbage (un record multi-riga porta
    # le righe sorgenti 5-34 dentro un'unica row CSV).
    raw = pd.read_csv(path, sep=";", header=None, skiprows=5, dtype=str, quotechar='"')

    period_row = raw.iloc[0].tolist()
    measure_row = raw.iloc[1].tolist()
    body = raw.iloc[2:].reset_index(drop=True)

    # Le dim sono tutte le colonne header prima della prima cella vuota.
    # Inps a volte permuta l'ordine delle dim tra anni — leggiamo dall'header.
    dim_names: list[str] = []
    for cell in measure_row:
        name = _safe_str(cell)
        if name == "":
            break
        dim_names.append(name)
    n_dim = len(dim_names)

    # Normalizza nomi delle dim (Inps a volte usa "Attivita'" senza accento).
    dim_names = [DIM_RENAME.get(d, d) for d in dim_names]

    if set(dim_names) != set(expected_dim_cols):
        raise ValueError(
            f"Dim attese {expected_dim_cols} ma trovate {dim_names} in {path}"
        )

    expected_cols = n_dim + 1 + 25  # dims + separator + 5 periodi × 5 misure

    measure_columns: list[tuple[str, str]] = []
    for i in range(25):
        period_idx = i // 5
        period_label_pos = n_dim + 1 + period_idx * 5
        raw_period = _safe_str(period_row[period_label_pos])
        period_norm = PERIOD_NORMALIZE.get(raw_period, raw_period)

        measure_pos = n_dim + 1 + i
        raw_measure = _safe_str(measure_row[measure_pos])
        measure_columns.append((period_norm, raw_measure))

    body = body.iloc[:, :expected_cols].copy()
    new_cols = list(dim_names) + ["__sep__"] + [f"_m{i}" for i in range(25)]
    body.columns = new_cols
    body = body.drop(columns=["__sep__"])

    for col in dim_names:
        body[col] = body[col].apply(clean_text)
        body[col] = body[col].ffill()

    # Drop subtotali: una qualunque dim non-Anno = "Totale".
    for col in dim_names:
        if col == "Anno":
            continue
        body = body[body[col] != "Totale"]

    body = body.dropna(subset=dim_names)

    melted = body.melt(
        id_vars=dim_names,
        value_vars=[f"_m{i}" for i in range(25)],
        var_name="_idx",
        value_name="_value",
    )
    melted["_i"] = melted["_idx"].str[2:].astype(int)
    melted["Periodo retribuito dal datore di lavoro"] = melted["_i"].map(
        lambda i: measure_columns[i][0]
    )
    melted["__measure"] = melted["_i"].map(lambda i: measure_columns[i][1])
    melted["_value"] = melted["_value"].map(parse_number)

    pivoted = melted.pivot_table(
        index=dim_names + ["Periodo retribuito dal datore di lavoro"],
        columns="__measure",
        values="_value",
        aggfunc="first",
    ).reset_index()
    pivoted.columns.name = None

    # Rimuovi il subtotale di periodo "Totale".
    pivoted = pivoted[pivoted["Periodo retribuito dal datore di lavoro"] != "Totale"]

    return pivoted


def main() -> None:
    for group in GROUPS:
        frames = []
        for year_file in YEAR_FILES:
            path = os.path.join(DATI_DIR, f"{year_file}{group['suffix']}.csv")
            frames.append(process_file(path, group["dim_cols"]))

        combined = pd.concat(frames, ignore_index=True)

        combined["Anno"] = combined["Anno"].astype(int)
        combined["Periodo retribuito dal datore di lavoro"] = pd.Categorical(
            combined["Periodo retribuito dal datore di lavoro"],
            categories=PERIOD_ORDER,
            ordered=True,
        )
        combined = combined.sort_values(
            by=group["dim_cols"] + ["Periodo retribuito dal datore di lavoro"]
        ).reset_index(drop=True)

        ordered_cols = (
            group["dim_cols"]
            + ["Periodo retribuito dal datore di lavoro"]
            + [m for m in MEASURES_ORDER if m in combined.columns]
        )
        combined = combined[ordered_cols]
        combined.columns = [snake_case(c) for c in combined.columns]

        out_path = os.path.join(OUT_DIR, group["out_name"])
        combined.to_csv(out_path, index=False, encoding="utf-8")
        print(f"{group['out_name']}: {len(combined)} righe")


if __name__ == "__main__":
    main()
