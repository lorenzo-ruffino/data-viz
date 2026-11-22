"""
Sfratti in Italia (Ministero dell'Interno) e famiglie in affitto / abitazioni non occupate (ISTAT).

Fonti:
- Ministero dell'Interno, Ufficio centrale di statistica, rilevazione INT 00004
  "Procedure di rilascio di immobili ad uso abitativo" (dati annuali per regione e provincia)
  https://ucs.interno.gov.it/it/le-statistiche/le-indagini/territorio/procedure-di-rilascio-di-immobili-ad-uso-abitativo
  - edizione 2025 (dati 2024): serie storica nazionale 2004-2024 e dati provinciali 2024
  - edizioni 2022-2024 (dati 2021-2023) e file provinciale dati 2019
    (i file dei dati 2020 linkati dal sito restituiscono errore 404)
  - pubblicazione "Procedure di rilascio di immobili ad uso abitativo - anno 2004"
    (Tab. 6, serie storica 1983-2003)
- ISTAT, Censimento permanente della popolazione e delle abitazioni (esploradati.istat.it)
  - DF_DCSS_HUDW_1_COM: famiglie per titolo di godimento dell'abitazione, 2021
  - DF_DCSS_ABITAZIONI_TV_1: abitazioni, occupate e non occupate, 2019-2021-2023

Avvertenze:
- "Sfratti eseguiti" = eseguiti con l'intervento dell'ufficiale giudiziario (non distingue
  quelli con la forza pubblica). "Morosità" comprende anche "altra causa".
- La serie viene rivista: il 2004 vale 43.892 provvedimenti nell'edizione 2004 e 46.193
  nell'edizione 2025. Per il 1983-2003 si usa l'edizione 2004, dal 2004 l'edizione 2025.
- A livello provinciale le richieste di esecuzione oscillano in modo anomalo (es. Milano
  309 nel 2021, 2.311 nel 2022, 402 nel 2023): vanno usate con cautela.
- Il dato del comune capoluogo esiste solo per i provvedimenti emessi.
- Le famiglie in affitto (censimento 2021) includono gli inquilini delle case popolari.

Output in ../input/:
- sfratti_serie_nazionale_1983_2024.csv
- sfratti_province_2019_2024.csv
- sfratti_grandi_comuni_2024.csv
- sfratti_abitazioni_non_occupate_grandi_comuni.csv
"""

import csv
import io
import os
import re
import subprocess
import tempfile
import time
import urllib.request

import openpyxl

CARTELLA = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "input")
TMP = tempfile.mkdtemp()

BASE_UCS = "https://ucs.interno.gov.it/sites/default/files/"
FILE_UCS = {
    2024: "2026-03/INT00004-PROCEDURE_RILASCIO_IMMOBILI_USO_ABITATIVO_dati_2024.xlsx",
    2023: "2025-01/INT00004-PROCEDURE_RILASCIO_IMMOBILI_USO_ABITATIVO_dati_2023.xlsx",
    2022: "2025-02/INT00004-PROCEDURE_RILASCIO_IMMOBILI_USO_ABITATIVO_dati_2022.xlsx",
    2021: "2025-02/INT00004-PROCEDURE_RILASCIO_IMMOBILI_USO_ABITATIVO_dati_2021%20%281%29.xlsx",
    2019: "2025-01/Dati_Sfratti__al_31-12-2019_-_Provinciali.xlsx",
}
PDF_2004 = "2025-02/sfratti_anno_2004.pdf"

SDMX = "https://esploradati.istat.it/SDMXWS/rest/data/IT1,{df},1.0/all?detail=dataonly"

# Grandi comuni: nome come nei file del Viminale (provincia) -> codice ISTAT del comune capoluogo
GRANDI_COMUNI = {
    "Roma": "058091", "Milano": "015146", "Napoli": "063049", "Torino": "001272",
    "Palermo": "082053", "Genova": "010025", "Bologna": "037006", "Firenze": "048017",
    "Bari": "072006", "Catania": "087015", "Venezia": "027042", "Verona": "023091",
    "Messina": "083048", "Padova": "028060", "Trieste": "032006", "Brescia": "017029",
    "Parma": "034027", "Prato": "100005", "Modena": "036023", "Reggio Calabria": "080063",
}


def scarica(url, nome):
    percorso = os.path.join(TMP, nome)
    req = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0"})
    with urllib.request.urlopen(req, timeout=300) as r, open(percorso, "wb") as f:
        f.write(r.read())
    return percorso


def num(x):
    if x is None or x == "":
        return None
    if isinstance(x, (int, float)):
        return x
    x = str(x).strip().replace(".", "").replace(",", ".")
    try:
        return float(x)
    except ValueError:
        return None


def pulisci_nome(s):
    return re.sub(r"\s+", " ", str(s)).strip()


# ---------------------------------------------------------------- serie nazionale
def serie_nazionale():
    righe = []
    # 1983-2003 dalla pubblicazione 2004 (Tab. 6)
    pdf = scarica(BASE_UCS + PDF_2004, "sfratti_2004.pdf")
    testo = subprocess.run(["pdftotext", "-layout", pdf, "-"], capture_output=True, text=True).stdout
    inizio = testo.find("ANNI 1983 - 2003")
    blocco = testo[inizio:inizio + 6000]
    for riga in blocco.splitlines():
        m = re.match(r"\s*(19[89]\d|200[0-3])\s+([\d.]+)\s+([\d.]+)\s+([\d.]+)\s+([\d.]+)\s+(?:[-\d,]+\s+)?([\d.]+)\s+(?:[-\d,]+\s+)?([\d.]+)", riga)
        if m:
            v = [int(g.replace(".", "")) for g in m.groups()]
            if v[0] in {r["anno"] for r in righe}:
                break  # finita la tabella nazionale, iniziano le tabelle regionali
            righe.append({
                "anno": v[0], "necessita_locatore": v[1], "finita_locazione": v[2],
                "morosita_altra_causa": v[3], "provvedimenti_totale": v[4],
                "richieste_esecuzione": v[5], "sfratti_eseguiti": v[6],
                "fonte": "Viminale, pubblicazione anno 2004, Tab. 6",
            })
    anni_pdf = {r["anno"] for r in righe}
    assert anni_pdf == set(range(1983, 2004)), sorted(anni_pdf)

    # 2004-2024 dall'edizione 2025
    wb = openpyxl.load_workbook(scarica(BASE_UCS + FILE_UCS[2024], "dati_2024.xlsx"), data_only=True)
    ws = [w for w in wb if w.title.lower().startswith("serie storica")][0]
    for r in ws.iter_rows(values_only=True):
        if isinstance(r[0], (int, float)) and 2000 < r[0] < 2100:
            v = [c for c in r if c is not None]
            righe.append({
                "anno": int(v[0]), "necessita_locatore": int(v[1]), "finita_locazione": int(v[2]),
                "morosita_altra_causa": int(v[3]), "provvedimenti_totale": int(v[4]),
                "richieste_esecuzione": int(v[6]), "sfratti_eseguiti": int(v[8]),
                "fonte": "Viminale, INT 00004 edizione 2025 (dati 2024), serie storica",
            })
    for r in righe:
        assert r["necessita_locatore"] + r["finita_locazione"] + r["morosita_altra_causa"] == r["provvedimenti_totale"], r
        r["quota_morosita"] = round(r["morosita_altra_causa"] / r["provvedimenti_totale"] * 100, 1)
    righe.sort(key=lambda r: r["anno"])
    return righe


# ---------------------------------------------------------------- province
def leggi_province(anno, percorso):
    wb = openpyxl.load_workbook(percorso, data_only=True)
    if anno == 2019:
        ws = wb.worksheets[0]
    else:
        ws = [w for w in wb if "provincial" in w.title.lower()][0]
    out = []
    for r in ws.iter_rows(values_only=True):
        if not r[0] or not isinstance(r[0], str):
            continue
        valori = [c for c in r[1:] if c is not None and c != ""]
        if len(valori) < 12 or not all(isinstance(x, (int, float)) for x in valori[:12]):
            continue
        nome = pulisci_nome(r[0])
        k = chiave(nome)
        livello = "italia" if k == "totaleitalia" else ("regione" if k in REGIONI else "provincia")
        out.append({
            "anno": anno, "livello": livello, "provincia": nome,
            "necessita_capoluogo": int(valori[0]), "necessita_resto": int(valori[1]),
            "finita_capoluogo": int(valori[2]), "finita_resto": int(valori[3]),
            "morosita_capoluogo": int(valori[4]), "morosita_resto": int(valori[5]),
            "provvedimenti_totale": int(valori[6]),
            "richieste_esecuzione": int(valori[8]),
            "sfratti_eseguiti": int(valori[10]),
        })
    return out


def province():
    righe = []
    for anno in sorted(FILE_UCS):
        righe += leggi_province(anno, scarica(BASE_UCS + FILE_UCS[anno], f"prov_{anno}.xlsx"))
    for r in righe:
        r["provvedimenti_capoluogo"] = r["necessita_capoluogo"] + r["finita_capoluogo"] + r["morosita_capoluogo"]
        r["morosita_totale"] = r["morosita_capoluogo"] + r["morosita_resto"]
        somma = r["provvedimenti_capoluogo"] + r["necessita_resto"] + r["finita_resto"] + r["morosita_resto"]
        if somma != r["provvedimenti_totale"]:
            print("Somma non coerente:", r["anno"], r["provincia"], somma, r["provvedimenti_totale"])
    return righe


# ---------------------------------------------------------------- ISTAT
def sdmx(df, tentativi=4):
    req = urllib.request.Request(SDMX.format(df=df), headers={
        "Accept": "application/vnd.sdmx.data+csv;version=1.0.0", "User-Agent": "Mozilla/5.0"})
    for i in range(tentativi):
        try:
            with urllib.request.urlopen(req, timeout=600) as r:
                return list(csv.DictReader(io.StringIO(r.read().decode("utf-8"))))
        except OSError:
            if i == tentativi - 1:
                raise
            time.sleep(30)


# Nomi ISTAT delle province (NUTS3 / codici ISTAT) -> nome nei file del Viminale
NUTS3 = {
    "ITC11": "Torino", "ITC12": "Vercelli", "ITC13": "Biella", "ITC14": "Verbania", "ITC15": "Novara",
    "ITC16": "Cuneo", "ITC17": "Asti", "ITC18": "Alessandria", "ITC20": "Aosta", "ITC31": "Imperia",
    "ITC32": "Savona", "ITC33": "Genova", "ITC34": "La Spezia", "ITC41": "Varese", "ITC42": "Como",
    "ITC43": "Lecco", "ITC44": "Sondrio", "ITC45": "Milano", "ITC46": "Bergamo", "ITC47": "Brescia",
    "ITC48": "Pavia", "ITC49": "Lodi", "ITC4A": "Cremona", "ITC4B": "Mantova", "IT108": "Monza e della Brianza",
    "ITD10": "Bolzano", "ITD20": "Trento", "ITD31": "Verona", "ITD32": "Vicenza", "ITD33": "Belluno",
    "ITD34": "Treviso", "ITD35": "Venezia", "ITD36": "Padova", "ITD37": "Rovigo", "ITD41": "Pordenone",
    "ITD42": "Udine", "ITD43": "Gorizia", "ITD44": "Trieste", "ITD51": "Piacenza", "ITD52": "Parma",
    "ITD53": "Reggio Emilia", "ITD54": "Modena", "ITD55": "Bologna", "ITD56": "Ferrara", "ITD57": "Ravenna",
    "ITD58": "Forli'-Cesena", "ITD59": "Rimini", "ITE11": "Massa Carrara", "ITE12": "Lucca", "ITE13": "Pistoia",
    "ITE14": "Firenze", "ITE15": "Prato", "ITE16": "Livorno", "ITE17": "Pisa", "ITE18": "Arezzo",
    "ITE19": "Siena", "ITE1A": "Grosseto", "ITE21": "Perugia", "ITE22": "Terni", "ITE31": "Pesaro e Urbino",
    "ITE32": "Ancona", "ITE33": "Macerata", "ITE34": "Ascoli Piceno", "IT109": "Fermo", "ITE41": "Viterbo",
    "ITE42": "Rieti", "ITE43": "Roma", "ITE44": "Latina", "ITE45": "Frosinone", "ITF11": "L'Aquila",
    "ITF12": "Teramo", "ITF13": "Pescara", "ITF14": "Chieti", "ITF21": "Isernia", "ITF22": "Campobasso",
    "ITF31": "Caserta", "ITF32": "Benevento", "ITF33": "Napoli", "ITF34": "Avellino", "ITF35": "Salerno",
    "ITF41": "Foggia", "ITF42": "Bari", "ITF43": "Taranto", "ITF44": "Brindisi", "ITF45": "Lecce",
    "IT110": "Barletta-Andria-Trani", "ITF51": "Potenza", "ITF52": "Matera", "ITF61": "Cosenza",
    "ITF62": "Crotone", "ITF63": "Catanzaro", "ITF64": "Vibo Valentia", "ITF65": "Reggio Calabria",
    "ITG11": "Trapani", "ITG12": "Palermo", "ITG13": "Messina", "ITG14": "Agrigento", "ITG15": "Caltanissetta",
    "ITG16": "Enna", "ITG17": "Catania", "ITG18": "Ragusa", "ITG19": "Siracusa", "ITG25": "Sassari",
    "ITG26": "Nuoro", "ITG27": "Cagliari", "ITG28": "Oristano", "IT111": "Sud Sardegna", "IT": "Totale Italia",
}


REGIONI = {"abruzzo", "basilicata", "calabria", "campania", "emiliaromagna", "friuliveneziagiulia", "lazio",
           "liguria", "lombardia", "marche", "molise", "piemonte", "puglia", "sardegna", "sicilia", "toscana",
           "trentinoaltoadige", "umbria", "valledaosta", "veneto"}


def chiave(nome):
    n = nome.lower().replace("'", "").replace("’", "")
    n = re.sub(r"[^a-z]", "", n)
    alias = {"forlecesena": "forlicesena", "forliecesena": "forlicesena", "massacarrara": "massacarrara", "verbanocusioossola": "verbania",
             "reggionellemilia": "reggioemilia", "reggiodicalabria": "reggiocalabria",
             "monzabrianza": "monzaedellabrianza", "monzaebrianza": "monzaedellabrianza",
             "barlettaandriatrani": "barlettaandriatrani", "aostavalledaosta": "aosta"}
    return alias.get(n, n)


def main():
    os.makedirs(CARTELLA, exist_ok=True)

    # ---- serie nazionale
    serie = serie_nazionale()
    campi = ["anno", "necessita_locatore", "finita_locazione", "morosita_altra_causa", "provvedimenti_totale",
             "quota_morosita", "richieste_esecuzione", "sfratti_eseguiti", "fonte"]
    with open(os.path.join(CARTELLA, "sfratti_serie_nazionale_1983_2024.csv"), "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=campi)
        w.writeheader()
        w.writerows(serie)

    # ---- famiglie in affitto (censimento 2021)
    fam = {}
    for r in sdmx("DF_DCSS_HUDW_1_COM"):
        fam[(r["REF_AREA"], r["TENURE_STATUS_HOU"])] = float(r["OBS_VALUE"])
    affitto_prov = {chiave(NUTS3[c]): fam.get((c, "RENT")) for c in NUTS3}
    totale_prov = {chiave(NUTS3[c]): fam.get((c, "ALL")) for c in NUTS3}

    # ---- province
    prov = province()
    non_trovate = set()
    for r in prov:
        k = chiave(r["provincia"])
        fa = affitto_prov.get(k)
        r["famiglie_in_affitto_2021"] = round(fa) if fa else ""
        r["famiglie_totali_2021"] = round(totale_prov[k]) if totale_prov.get(k) else ""
        if fa:
            r["provvedimenti_per_1000_famiglie_affitto"] = round(r["provvedimenti_totale"] / fa * 1000, 2)
            r["morosita_per_1000_famiglie_affitto"] = round(r["morosita_totale"] / fa * 1000, 2)
            r["eseguiti_per_1000_famiglie_affitto"] = round(r["sfratti_eseguiti"] / fa * 1000, 2)
        else:
            if r["livello"] == "provincia":
                non_trovate.add(r["provincia"])
            r["provvedimenti_per_1000_famiglie_affitto"] = ""
            r["morosita_per_1000_famiglie_affitto"] = ""
            r["eseguiti_per_1000_famiglie_affitto"] = ""
    # Segnala le province con dati sui provvedimenti palesemente incompleti o classificati male
    for r in prov:
        note = []
        if r["livello"] == "provincia" and r["provvedimenti_totale"] < r["sfratti_eseguiti"]:
            note.append("provvedimenti inferiori agli sfratti eseguiti: possibile rilevazione incompleta dei provvedimenti")
        if r["livello"] == "provincia" and r["sfratti_eseguiti"] == 0 and r["richieste_esecuzione"] == 0:
            note.append("richieste di esecuzione e sfratti eseguiti pari a zero: dato mancante")
        if r["livello"] == "provincia" and r["provvedimenti_totale"] >= 200 and \
                r["necessita_capoluogo"] + r["necessita_resto"] > 0.4 * r["provvedimenti_totale"]:
            note.append("quota anomala di 'necessità del locatore': probabile errore di classificazione del motivo")
        r["nota_qualita"] = "; ".join(note)
    print("Province senza famiglie ISTAT 2021 (confini diversi o nomi):", sorted(non_trovate))
    campi = ["anno", "livello", "provincia", "necessita_capoluogo", "necessita_resto", "finita_capoluogo", "finita_resto",
             "morosita_capoluogo", "morosita_resto", "morosita_totale", "provvedimenti_capoluogo",
             "provvedimenti_totale", "richieste_esecuzione", "sfratti_eseguiti", "famiglie_in_affitto_2021",
             "famiglie_totali_2021", "provvedimenti_per_1000_famiglie_affitto", "morosita_per_1000_famiglie_affitto",
             "eseguiti_per_1000_famiglie_affitto", "nota_qualita"]
    with open(os.path.join(CARTELLA, "sfratti_province_2019_2024.csv"), "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=campi)
        w.writeheader()
        w.writerows(prov)

    # ---- grandi comuni 2024: provvedimenti nel capoluogo / famiglie in affitto nel comune
    righe = []
    for r in prov:
        if r["anno"] != 2024:
            continue
        nome = r["provincia"]
        if nome not in GRANDI_COMUNI and nome != "Totale Italia":
            continue
        codice = GRANDI_COMUNI.get(nome, "IT")
        fa = fam.get((codice, "RENT"))
        ft = fam.get((codice, "ALL"))
        if nome == "Totale Italia":
            prov_cap = r["provvedimenti_totale"]
            mor_cap = r["morosita_totale"]
        else:
            prov_cap = r["provvedimenti_capoluogo"]
            mor_cap = r["morosita_capoluogo"]
        righe.append({
            "comune": "Italia" if nome == "Totale Italia" else nome,
            "codice_istat": codice,
            "provvedimenti_comune_2024": prov_cap,
            "di_cui_morosita_2024": mor_cap,
            "famiglie_2021": round(ft),
            "famiglie_in_affitto_2021": round(fa),
            "quota_famiglie_in_affitto": round(fa / ft * 100, 1),
            "provvedimenti_per_1000_famiglie_affitto": round(prov_cap / fa * 1000, 2),
            "morosita_per_1000_famiglie_affitto": round(mor_cap / fa * 1000, 2),
            "richieste_esecuzione_provincia_2024": r["richieste_esecuzione"],
            "sfratti_eseguiti_provincia_2024": r["sfratti_eseguiti"],
            "nota_qualita": "; ".join(x for x in [
                r.get("nota_qualita", ""),
                "nessun provvedimento attribuito al capoluogo: dato mancante" if prov_cap == 0 else ""] if x),
        })
    righe.sort(key=lambda x: -x["famiglie_2021"])
    with open(os.path.join(CARTELLA, "sfratti_grandi_comuni_2024.csv"), "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=list(righe[0]))
        w.writeheader()
        w.writerows(righe)

    # ---- abitazioni non occupate (censimento permanente) nei grandi comuni
    ab = {}
    for r in sdmx("DF_DCSS_ABITAZIONI_TV_1"):
        ab[(r["REF_AREA"], r["INDICATOR"], r["TIME_PERIOD"])] = float(r["OBS_VALUE"])
    righe = []
    for nome, codice in [("Italia", "IT")] + list(GRANDI_COMUNI.items()):
        for anno in ("2019", "2021", "2023"):
            tot = ab.get((codice, "NUM_DW_AV", anno))
            non = ab.get((codice, "NUM_UNOCC_DW_AV", anno))
            if tot is None:
                continue
            righe.append({
                "comune": nome, "codice_istat": codice, "anno": anno,
                "abitazioni": round(tot), "abitazioni_occupate": round(ab[(codice, "NUM_OCC_DW_AV", anno)]),
                "abitazioni_non_occupate": round(non), "quota_non_occupate": round(non / tot * 100, 1),
            })
    with open(os.path.join(CARTELLA, "sfratti_abitazioni_non_occupate_grandi_comuni.csv"), "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=list(righe[0]))
        w.writeheader()
        w.writerows(righe)

    ultimo = serie[-1]
    fa_it = fam[("IT", "RENT")]
    print(f"{ultimo['anno']}: {ultimo['provvedimenti_totale']} provvedimenti, "
          f"{ultimo['provvedimenti_totale'] / fa_it * 1000:.1f} ogni 1000 famiglie in affitto (censimento 2021)")


if __name__ == "__main__":
    main()
