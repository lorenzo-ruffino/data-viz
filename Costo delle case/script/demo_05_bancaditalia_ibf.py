"""
Titolo di godimento dell'abitazione di residenza: serie storica 1977-2022 e dettaglio 2022
dall'Indagine sui bilanci delle famiglie italiane (IBF) della Banca d'Italia.

Fonte: Banca d'Italia, Indagine sui bilanci delle famiglie italiane nell'anno 2022
https://www.bancaditalia.it/pubblicazioni/indagine-famiglie/bil-fam2022/index.html
- tavole_storiche.xlsx: Tav. S35 (titolo di godimento) e S37 (affittuari per caratteristiche del capofamiglia)
- tavole_cross_section.xlsx: Tav. T26 (titolo di godimento per caratteristiche, 2022)
Percentuali di FAMIGLIE; il capofamiglia è il maggior percettore di reddito.
Per il 2022 la tavola storica riporta due valori: pesi per il confronto storico (1) e pesi del nuovo disegno (2).

Output in ../input/:
- demo_ibf_titolo_godimento_1977_2022.csv
- demo_ibf_affittuari_caratteristiche_1977_2022.csv
- demo_ibf_titolo_godimento_2022_dettaglio.csv
"""

import csv
import io
import os
import urllib.request

import openpyxl

CARTELLA = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "input")
BASE = "https://www.bancaditalia.it/pubblicazioni/indagine-famiglie/bil-fam2022/"


def libro(nome):
    req = urllib.request.Request(BASE + nome, headers={"User-Agent": "Mozilla/5.0"})
    return openpyxl.load_workbook(io.BytesIO(urllib.request.urlopen(req, timeout=120).read()), data_only=True)


def righe(ws):
    return [list(r) for r in ws.iter_rows(values_only=True) if any(c not in (None, "") for c in r)]


def anni_da(riga):
    # "2022 (1)" = pesi per il confronto storico, "2022 (2)" = pesi del nuovo disegno
    out = []
    for c in riga[1:]:
        if c is None:
            continue
        s = str(c)
        out.append((int(s[:4]), "nuovo disegno" if "(2)" in s else "confronto storico"))
    return out


storiche = libro("tavole_storiche.xlsx")

# Tav. S35: titolo di godimento
r35 = righe(storiche["tav.S35"])
anni = anni_da(r35[3])
etichette = {"Titolo di godimento": "proprieta", "affitto": "affitto", "riscatto": "riscatto",
             "usufrutto o uso gratuito": "usufrutto o uso gratuito"}
with open(os.path.join(CARTELLA, "demo_ibf_titolo_godimento_1977_2022.csv"), "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["anno", "pesi", "titolo", "quota_famiglie_pct"])
    for r in r35[4:]:
        if r[0] in etichette:
            for (a, pesi), v in zip(anni, r[1:]):
                w.writerow([a, pesi, etichette[r[0]], v])

# Tav. S37: quota di affittuari per caratteristiche del capofamiglia
r37 = righe(storiche["tav.S37"])
anni = anni_da(r37[3])
gruppo = None
with open(os.path.join(CARTELLA, "demo_ibf_affittuari_caratteristiche_1977_2022.csv"), "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["anno", "pesi", "variabile", "modalita", "quota_famiglie_in_affitto_pct"])
    for r in r37[4:]:
        if r[0] is None or str(r[0]).startswith("("):
            continue
        if all(c in (None, "") for c in r[1:]):
            gruppo = r[0]
            continue
        for (a, pesi), v in zip(anni, r[1:]):
            w.writerow([a, pesi, "Totale" if r[0] == "Totale" else gruppo, r[0], "" if v == "." else v])

# Tav. T26: titolo di godimento per caratteristiche, 2022
cross = libro("tavole_cross_section.xlsx")
r26 = righe(cross["tav.T26"])
gruppo = None
with open(os.path.join(CARTELLA, "demo_ibf_titolo_godimento_2022_dettaglio.csv"), "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["variabile", "modalita", "proprieta", "affitto", "riscatto", "altro_titolo", "totale"])
    for r in r26[3:]:
        if r[0] is None or str(r[0]).startswith("("):
            continue
        if all(c in (None, "") for c in r[1:]):
            gruppo = r[0]
            continue
        w.writerow(["Totale" if r[0] == "Totale" else gruppo, r[0]] + ["" if v == "." else v for v in r[1:6]])
