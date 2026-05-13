"""Estrae i dati Istat SBS (Structural Business Statistics) sui settori
commercio e servizi e calcola la produttività (valore aggiunto per occupato
e per dipendente) per ATECO a 1 lettera, anno 2023.

Salva un CSV pulito in output/produttivita_settori_2023.csv da incrociare
con i dati salariali Inps (output/settore_macro.csv) per confrontare il
valore aggiunto per dipendente con il salario settoriale.

Limitazioni: il dataset Istat SBS contiene solo le sezioni G-S (commercio
e servizi). Mancano:
  - C (manifattura)
  - F (costruzioni)
  - B (estrazione minerali), D (energia), E (acqua/rifiuti)
  - K (finanza e assicurazioni)
Il messaggio "settori a bassa produttività trainano l'occupazione"
rimane comunque visibile sui servizi, che sono il motore dell'occupazione
del decennio.
"""

import csv
import os

PROJ    = "/Users/lorenzoruffino/Documents/Progetti/data-viz/Stipendi privato Inps"
SRC     = os.path.join(PROJ, "dati", "istat", "commercio_servizi_sbs.csv")
OUT_DIR = os.path.join(PROJ, "output", "aggregati")
OUT     = os.path.join(OUT_DIR, "produttivita_settori_2023.csv")
os.makedirs(OUT_DIR, exist_ok=True)

# Mapping sezione ATECO -> nome breve coerente con la nostra classificazione macro
ATECO_LABEL = {
    "H": "Trasporti e magazzinaggio",
    "I": "Turismo e ristorazione",
    "J": "ICT",
    "L": "Immobiliari",
    "M": "Servizi professionali",
    "N": "Servizi alle imprese",
    "P": "Istruzione (privata)",
    "Q": "Sanita' e assistenza sociale (privata)",
    "R": "Cultura e intrattenimento",
    "S": "Altri servizi",
}

INDICATORI = {
    "Valore aggiunto al costo dei fattori (migliaia di euro)": "valore_aggiunto_kEUR",
    "Occupati": "occupati",
    "Lavoratori dipendenti": "dipendenti",
    "Salari e stipendi (migliaia di euro)": "salari_stipendi_kEUR",
    "Costi del personale (migliaia di euro)": "costi_personale_kEUR",
    "Ore lavorate dai dipendenti (migliaia)": "ore_dipendenti_k",
}

data: dict[str, dict] = {}
with open(SRC, encoding="utf-8") as f:
    reader = csv.reader(f, delimiter=",", quotechar="'")
    next(reader)  # header
    for r in reader:
        if len(r) < 12:
            continue
        indicatore = r[5]
        ateco = r[6]
        classe = r[8]
        obs = r[11]
        if classe != "TOTAL" or len(ateco) > 4:
            continue
        if indicatore not in INDICATORI:
            continue
        try:
            val = float(obs) if obs else None
        except ValueError:
            val = None
        data.setdefault(ateco, {"sezione_ateco": ateco}) [INDICATORI[indicatore]] = val

# Calcola produttività e cuneo
out_rows = []
for ateco, d in data.items():
    if ateco == "0010":
        d["macro_settore"] = "Totale commercio e servizi (G-S)"
    else:
        d["macro_settore"] = ATECO_LABEL.get(ateco, ateco)
    va = d.get("valore_aggiunto_kEUR")
    occ = d.get("occupati")
    dip = d.get("dipendenti")
    salari = d.get("salari_stipendi_kEUR")
    if va and occ:
        d["va_per_occupato_eur"] = va * 1000 / occ
    if va and dip:
        d["va_per_dipendente_eur"] = va * 1000 / dip
    if salari and dip:
        d["salario_lordo_medio_eur"] = salari * 1000 / dip
    if va and salari:
        d["quota_salari_su_va"] = salari / va
    out_rows.append(d)

# Ordina con totale in fondo
out_rows.sort(key=lambda r: (r["sezione_ateco"] == "0010", r["sezione_ateco"]))

cols = [
    "sezione_ateco", "macro_settore",
    "valore_aggiunto_kEUR", "occupati", "dipendenti",
    "salari_stipendi_kEUR", "costi_personale_kEUR", "ore_dipendenti_k",
    "va_per_occupato_eur", "va_per_dipendente_eur",
    "salario_lordo_medio_eur", "quota_salari_su_va",
]

with open(OUT, "w", encoding="utf-8", newline="") as f:
    w = csv.DictWriter(f, fieldnames=cols)
    w.writeheader()
    for r in out_rows:
        w.writerow({c: r.get(c, "") for c in cols})

print(f"Scritto {OUT}: {len(out_rows)} righe")
print()
print(f"{'Settore':<45} {'VA/occ €':>10} {'VA/dip €':>10} {'Sal lordo':>10}")
print("-" * 80)
for r in out_rows:
    print(f"{r['macro_settore'][:43]:<45} "
          f"{r.get('va_per_occupato_eur', 0):>10,.0f} "
          f"{r.get('va_per_dipendente_eur', 0):>10,.0f} "
          f"{r.get('salario_lordo_medio_eur', 0):>10,.0f}")
