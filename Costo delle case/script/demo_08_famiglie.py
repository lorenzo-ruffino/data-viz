"""
Famiglie: numero, componenti medi e famiglie unipersonali, Italia e grandi comuni.

Fonti (ISTAT):
- Censimenti 1951-2001: "L'Italia in 150 anni. Sommario di statistiche storiche", cap. 3, Tavola 3.1
  https://www.istat.it/wp-content/uploads/2019/03/cap_3.pdf
- Censimenti 2011 e 2019 (censimento permanente), Italia e 26 comuni con più di 150mila abitanti:
  "Censimento permanente della popolazione: le famiglie in Italia. Anni 2018 e 2019", 7 marzo 2022,
  Prospetti 3 e 4  https://www.istat.it/it/files/2022/03/Censimento-permanente-della-popolazione_le-famiglie-in-Italia.pdf
- Censimento permanente 2021: "I nuclei familiari nei censimenti della popolazione", novembre 2024
  https://www.istat.it/wp-content/uploads/2024/11/Statistica-report-Nuclei-familiari-1.pdf (26.206.246 famiglie)
- Indagine Aspetti della vita quotidiana, media 2024-2025: "Indicatori demografici - Anno 2025", 31 marzo 2026
  https://www.istat.it/comunicato-stampa/indicatori-demografici-anno-2025/ (26,6 milioni di famiglie, 37,1% unipersonali)
- Previsioni base 1/1/2025, scenario mediano: comunicato del 31 agosto 2026
  https://www.istat.it/comunicato-stampa/previsioni-della-popolazione-residente-delle-famiglie-e-delle-forze-di-lavoro-base-1-1-2025/

Attenzione: le fonti non sono perfettamente omogenee (censimenti tradizionali, censimento permanente
con nuova metodologia dal 2018, indagine campionaria AVQ, previsioni). Richiede pdftotext.

Output in ../input/:
- demo_famiglie_italia_1951_2050.csv
- demo_famiglie_grandi_comuni_2011_2019.csv
"""

import csv
import os
import re
import subprocess
import tempfile
import urllib.request

CARTELLA = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "input")


def pdf_testo(url):
    with tempfile.TemporaryDirectory() as tmp:
        f = os.path.join(tmp, "doc.pdf")
        req = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0"})
        with urllib.request.urlopen(req, timeout=180) as r, open(f, "wb") as out:
            out.write(r.read())
        return subprocess.run(["pdftotext", "-layout", f, "-"], capture_output=True, text=True).stdout


def numero(s):
    return float(s.replace(".", "").replace(",", "."))


# --- 1. Tavola 3.1: famiglie per ampiezza ai censimenti (1951-2001)
t = pdf_testo("https://www.istat.it/wp-content/uploads/2019/03/cap_3.pdf")
blocco = t[t.index("Tavola 3.1 - Famiglie residenti"):t.index("Tavola 3.2")]
assoluti = blocco[blocco.index("VALORI ASSOLUTI"):blocco.index("COMPOSIZIONI PERCENTUALI")]
italia = []
for riga in assoluti.splitlines():
    m = re.match(r"\s*(1951|1961|1971|1981|1991|2001)\s+(.*)", riga)
    if m:
        v = m.group(2).split()
        uno, tot, medio = numero(v[0]), numero(v[6]), numero(v[7])
        italia.append([int(m.group(1)), "censimento", int(tot), medio, round(100 * uno / tot, 1), "",
                       "ISTAT, Sommario di statistiche storiche, Tav. 3.1"])

# --- 2. Censimenti 2011 e 2019 (Prospetti 3 e 4)
t = pdf_testo("https://www.istat.it/it/files/2022/03/Censimento-permanente-della-popolazione_le-famiglie-in-Italia.pdf")
p3 = t[t.index("PROSPETTO 3. NUMERO DI FAMIGLIE"):t.index("PROSPETTO 4.")]
p4 = t[t.index("PROSPETTO 4. FAMIGLIE PER NUMERO DI COMPONENTI"):]
p4 = p4[:p4.index("Il fenomeno dello spopolamento")]


def righe_prospetto(testo, n_valori):
    # restituisce {comune: valori}; i nomi spezzati su più righe nel PDF ("Reggio" / valori / "nell'Emilia",
    # "TOTALE" / "GRANDI" valori / "COMUNI") vengono ricomposti
    SUFFISSI = {"nell'Emilia", "Calabria", "COMUNI"}
    out, prefisso = {}, ""
    for riga in testo.splitlines():
        s = riga.strip()
        if not s:
            continue
        num = re.findall(r"-?\d{1,3}(?:\.\d{3})+(?:,\d+)?|-?\d+,\d+", s)
        nome = re.sub(r"-?\d{1,3}(?:\.\d{3})+(?:,\d+)?|-?\d+,\d+", "", s).strip()
        if len(num) >= n_valori:
            chiave = " ".join(x for x in (prefisso, nome) if x)
            if chiave:  # le righe senza nome sono etichette dei grafici
                out[chiave] = num[:n_valori]
            prefisso = ""
        elif not num and s in SUFFISSI and out:
            ultimo = list(out)[-1]
            out[ultimo + " " + s] = out.pop(ultimo)
        elif not num and len(s) < 15:
            prefisso = s
        else:
            prefisso = ""
    return out


r3 = righe_prospetto(p3, 8)
r4 = righe_prospetto(p4, 10)

with open(os.path.join(CARTELLA, "demo_famiglie_grandi_comuni_2011_2019.csv"), "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["comune", "famiglie_2019", "pop_in_famiglia_2019", "famiglie_2011", "pop_in_famiglia_2011",
                "var_pct_famiglie", "var_pct_pop_in_famiglia", "componenti_medi_2019", "componenti_medi_2011",
                "quota_unipersonali_2019", "quota_unipersonali_2011"])
    for nome, v in r3.items():
        u = r4.get(nome)
        w.writerow([nome, int(numero(v[0])), int(numero(v[1])), int(numero(v[2])), int(numero(v[3])),
                    numero(v[4]), numero(v[5]), numero(v[6]), numero(v[7]),
                    numero(u[0]) if u else "", numero(u[5]) if u else ""])

it = r3["ITALIA"]
u_it = r4["ITALIA"]
italia.append([2011, "censimento", int(numero(it[2])), numero(it[7]), numero(u_it[5]), "",
               "ISTAT, Le famiglie in Italia 2018-2019 (7/3/2022), Prospetti 3-4"])
italia.append([2019, "censimento permanente", int(numero(it[0])), numero(it[6]), numero(u_it[0]), "",
               "ISTAT, Le famiglie in Italia 2018-2019 (7/3/2022), Prospetti 3-4"])
# --- 3. Dati successivi (valori riportati nei comunicati ISTAT citati in testa)
italia += [
    [2021, "censimento permanente", 26206246, "", "", "", "ISTAT, I nuclei familiari nei censimenti (11/2024)"],
    [2025, "indagine AVQ, media 2024-2025", 26600000, "", 37.1, "", "ISTAT, Indicatori demografici 2025 (31/3/2026)"],
    [2025, "previsioni base 2025 (scenario mediano)", 26700000, 2.20, 37.5, "", "ISTAT, Previsioni base 1/1/2025 (31/8/2026)"],
    [2043, "previsioni base 2025 (scenario mediano)", 27900000, "", "", "massimo previsto", "ISTAT, Previsioni base 1/1/2025 (31/8/2026)"],
    [2050, "previsioni base 2025 (scenario mediano)", 27600000, 1.98, 42.0, "", "ISTAT, Previsioni base 1/1/2025 (31/8/2026)"],
]
with open(os.path.join(CARTELLA, "demo_famiglie_italia_1951_2050.csv"), "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["anno", "fonte_tipo", "famiglie", "componenti_medi", "quota_unipersonali_pct", "nota", "fonte"])
    w.writerows(italia)
for r in italia:
    print(r)
print(len(r3), "comuni nel prospetto 3;", len(r4), "nel prospetto 4")
