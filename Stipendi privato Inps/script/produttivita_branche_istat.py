"""Calcola la produttività del lavoro (valore aggiunto a prezzi concatenati
2020 per occupato) per branca di attività ATECO, dai Conti Nazionali Istat.

Input:
  - "Produzione e valore aggiunto per branca di attività (...)": VA in milioni
    di euro a prezzi concatenati anno di riferimento 2020 (al netto
    dell'inflazione)
  - "Occupati, unità di lavoro, posizioni lavorative e ore lavorate (...)":
    occupati totali (dipendenti + indipendenti) in migliaia

Calcolo: VA[mln €] / Occupati[k persone] = € × 10^6 / persone × 10^3
        = € × 10^3 / persona = MIGLIAIA di euro per occupato.

Output: output/produttivita_branche_istat.csv con livello 2014, livello 2024
e variazione %.

Nota: il file Occupati include la branca aggregata D_E (energia + acqua), che
nel file VA e' invece divisa in D ed E. Per evitare il doppio conteggio nel
totale economia, calcolo la produttivita' aggregata sommando le branche
non sovrapposte e usando come denominatore gli occupati totali.
"""

import csv
import os

PROJ = "/Users/lorenzoruffino/Documents/Progetti/data-viz/Stipendi privato Inps"
SRC_VA = os.path.join(
    PROJ,
    "Produzione e valore aggiunto per branca di attività (IT1,92_1225_DF_DCCN_ANA1_1,1.0).csv",
)
SRC_OCC = os.path.join(
    PROJ,
    "Occupati, unità di lavoro, posizioni lavorative e ore lavorate (IT1,92_507_DF_DCCN_OCCNSEC2010_1,1.0).csv",
)
OUT = os.path.join(PROJ, "output", "produttivita_branche_istat.csv")

LABELS = {
    "A": "Agricoltura, silvicoltura e pesca",
    "B": "Industria estrattiva",
    "C": "Industria manifatturiera",
    "D": "Energia (gas, elettricita')",
    "E": "Acqua, rifiuti, risanamento",
    "D_E": "Energia, acqua, rifiuti",
    "F": "Costruzioni",
    "G": "Commercio all'ingrosso e al dettaglio",
    "H": "Trasporti e magazzinaggio",
    "I": "Servizi di alloggio e di ristorazione",
    "J": "Servizi di informazione e comunicazione (ICT)",
    "K": "Attivita' finanziarie e assicurative",
    "L": "Attivita' immobiliari",
    "M": "Attivita' professionali, scientifiche e tecniche",
    "N": "Attivita' amministrative e di supporto",
    "O": "Pubblica amministrazione e difesa",
    "P": "Istruzione",
    "Q": "Sanita' e assistenza sociale",
    "R": "Attivita' artistiche e di intrattenimento",
    "S": "Altre attivita' di servizi",
    "T": "Lavoro domestico (famiglie come datori di lavoro)",
    "_T": "Totale attivita' economiche",
}


def parse_istat_csv(path: str):
    """Istat usa ' come quotechar e " come escapechar (per ' interni)."""
    with open(path, encoding="utf-8") as f:
        reader = csv.reader(f, delimiter=",", quotechar="'", escapechar='"')
        next(reader)  # header
        return list(reader)


# Valore aggiunto: BRKDW=col 6, TIME=col 16, VAL=col 17
va = {}  # va[(branca, anno)] = valore in mln € prezzi 2020
for r in parse_istat_csv(SRC_VA):
    if len(r) < 18:
        continue
    branca = r[6]
    anno = r[16]
    val = r[17]
    if not anno.isdigit() or not val:
        continue
    try:
        va[(branca, int(anno))] = float(val)
    except ValueError:
        pass

# Occupati: stessa struttura
occ = {}  # occ[(branca, anno)] = persone (migliaia)
for r in parse_istat_csv(SRC_OCC):
    if len(r) < 18:
        continue
    branca = r[6]
    tipologia = r[8]
    posizione = r[10]
    anno = r[16]
    val = r[17]
    if tipologia != "TOT" or posizione != "9":
        continue
    if not anno.isdigit() or not val:
        continue
    try:
        occ[(branca, int(anno))] = float(val)
    except ValueError:
        pass

# Branche presenti in entrambi i file
branche_va = sorted({b for b, _ in va.keys()})
branche_occ = sorted({b for b, _ in occ.keys()})
common = sorted(set(branche_va) & set(branche_occ))
solo_occ = sorted(set(branche_occ) - set(branche_va))

print("Branche con VA:", branche_va)
print("Solo occupati (no VA disponibile):", solo_occ)

# Calcolo produttivita' aggregata totale economia: somma VA di tutte le branche
# disponibili / occupati totali (_T). Usato come riga finale "totale economia".
for anno in (2014, 2024):
    va_tot = sum(v for (b, a), v in va.items() if a == anno)
    occ_tot = occ.get(("_T", anno))
    if va_tot and occ_tot:
        va.setdefault(("__TOT__", anno), va_tot)
        occ.setdefault(("__TOT__", anno), occ_tot)
LABELS["__TOT__"] = "TOTALE economia (somma branche disponibili)"

# Calcolo produttivita': VA [mln €] / Occupati [k persone] = k€ per occupato
out_rows = []
for branca in common + ["__TOT__"]:
    if branca != "__TOT__" and branca not in branche_occ:
        continue
    row = {
        "codice": branca,
        "branca": LABELS.get(branca, branca),
    }
    for anno in (2014, 2024):
        v = va.get((branca, anno))
        o = occ.get((branca, anno))
        row[f"valore_aggiunto_mln_eur_2020_{anno}"] = v
        row[f"occupati_migliaia_{anno}"] = o
        if v and o:
            row[f"produttivita_kEUR_per_occupato_{anno}"] = v / o
    p2014 = row.get("produttivita_kEUR_per_occupato_2014")
    p2024 = row.get("produttivita_kEUR_per_occupato_2024")
    if p2014 and p2024:
        row["produttivita_var_pct"] = (p2024 / p2014 - 1) * 100
    if (
        row.get("occupati_migliaia_2014")
        and row.get("occupati_migliaia_2024")
    ):
        row["occupati_var_pct"] = (
            row["occupati_migliaia_2024"] / row["occupati_migliaia_2014"] - 1
        ) * 100
    out_rows.append(row)

# Ordina per produttivita' 2024 decrescente, totale in fondo
out_rows.sort(
    key=lambda r: (
        r["codice"] == "__TOT__",
        -(r.get("produttivita_kEUR_per_occupato_2024") or 0),
    )
)

cols = [
    "codice",
    "branca",
    "valore_aggiunto_mln_eur_2020_2014",
    "valore_aggiunto_mln_eur_2020_2024",
    "occupati_migliaia_2014",
    "occupati_migliaia_2024",
    "produttivita_kEUR_per_occupato_2014",
    "produttivita_kEUR_per_occupato_2024",
    "produttivita_var_pct",
    "occupati_var_pct",
]
with open(OUT, "w", encoding="utf-8", newline="") as f:
    w = csv.DictWriter(f, fieldnames=cols)
    w.writeheader()
    for r in out_rows:
        w.writerow({c: r.get(c, "") for c in cols})

print(f"\nScritto {OUT}\n")

# Print riepilogo
print(
    f"{'Branca':<48} {'P 2014':>8} {'P 2024':>8} {'Var P %':>8} {'Var Occ %':>10}"
)
print("-" * 90)
for r in out_rows:
    print(
        f"{r['branca'][:46]:<48} "
        f"{r.get('produttivita_kEUR_per_occupato_2014', 0):>8,.1f} "
        f"{r.get('produttivita_kEUR_per_occupato_2024', 0):>8,.1f} "
        f"{r.get('produttivita_var_pct', 0):>+8.1f} "
        f"{r.get('occupati_var_pct', 0):>+10.1f}"
    )
