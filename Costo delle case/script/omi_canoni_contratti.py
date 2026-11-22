# Canoni medi dei nuovi contratti di locazione registrati (Agenzia delle Entrate - OMI)
# Fonte: Rapporti Immobiliari residenziali OMI-ABI (capitolo "Le locazioni"), un rapporto
# per anno con i dati dell'anno precedente. Si estraggono:
#  - la tabella "Mercato ordinario lungo periodo - Dati nelle principali città" (8 città)
#  - la riga ITALIA della tabella "Mercato ordinario lungo periodo - Dati per area territoriale"
#  - la stessa coppia per il "Mercato ordinario transitorio"
# Condizioni d'uso: le informazioni possono essere usate ed elaborate citando la fonte
# ("Agenzia delle Entrate - OMI, Rapporto Immobiliare residenziale <anno>").
# Richiede pdftotext (poppler).
import re, subprocess, tempfile, os
import requests
import pandas as pd

CARTELLA = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(CARTELLA, "..", "input", "prezzi_omi_canoni_contratti.csv")

RAPPORTI = {  # anno del rapporto: url (i dati si riferiscono all'anno precedente)
    2016: "https://www.agenziaentrate.gov.it/portale/documents/20143/263212/rapporto+immobiliare+2016_RI_2016_QuadroGenerale_12052016.pdf/c9f4b956-1ea4-809b-72e4-fe819e76d383",
    2017: "https://www.agenziaentrate.gov.it/portale/documents/20143/263178/rapporto+immobiliare2017_RI_2017_QuadroGenerale_15052017.pdf/a5d10a1d-b106-4f0b-44d2-8010c5c40a13",
    2018: "https://www.agenziaentrate.gov.it/portale/documents/20143/263144/rapporto+immobiliare2018_RI2018_Residenziale_20180522_.pdf/3be4874a-4f84-2f3a-7ad1-02ade4e0a5d4",
    2019: "https://www.agenziaentrate.gov.it/portale/documents/20143/263076/rapporto+immobiliare2019+ri_RI2019_Residenziale_20190523.pdf/a175f856-2363-dda7-da64-b1c0e544eb12",
    2020: "https://www.agenziaentrate.gov.it/portale/documents/20143/263076/RI2020_20200521_Residenziale.pdf/40fee96c-49ad-6edb-e36b-1f54ff623b12",
    2021: "https://www.agenziaentrate.gov.it/portale/documents/20143/263076/RI2021_Residenziale_20210520_.pdf/21bc5a24-7353-b961-dbc9-2a2b98cd86b9",
    2022: "https://www.agenziaentrate.gov.it/portale/documents/20143/263076/RI2022_Residenziale_20220519.pdf/3934f1ef-9aac-7e84-172a-7cbf82745322",
    2023: "https://www.agenziaentrate.gov.it/portale/documents/20143/263076/RI2023_Residenziale_20230518.pdf/5936e221-db99-a971-1bc5-8cf2d70f3d58",
    2024: "https://www.agenziaentrate.gov.it/portale/documents/20143/263076/RI2024_Residenziale_20240521.pdf/2ac96c07-26a6-6eeb-de79-c7d6428054f3",
    2025: "https://www.agenziaentrate.gov.it/portale/documents/20143/263076/RI2025_Residenziale20250520.pdf/8aa493d0-d389-5d34-b7ba-2815dadf3c14",
    2026: "https://www.agenziaentrate.gov.it/portale/documents/20143/263076/RI2026_Residenziale20260521.pdf/892125df-86a3-c21a-f8a1-86323f45ae93",
}
CITTA = ["ROMA", "MILANO", "NAPOLI", "TORINO", "PALERMO", "GENOVA", "BOLOGNA", "FIRENZE", "TOTALE"]
SEGMENTI = {"ordinario_lungo": "Mercato ordinario lungo periodo", "ordinario_transitorio": "Mercato ordinario transitorio"}


def numero(s):
    return float(s.replace(".", "").replace(",", ".").replace("%", ""))


def testo_pdf(url):
    with tempfile.TemporaryDirectory() as d:
        f = os.path.join(d, "r.pdf")
        open(f, "wb").write(requests.get(url, headers={"User-Agent": "Mozilla/5.0"}, timeout=120).content)
        subprocess.run(["pdftotext", "-layout", f, f + ".txt"], check=True)
        return open(f + ".txt", encoding="utf-8").read().splitlines()


def righe_tabella(righe, titolo, nomi):
    """Righe numeriche (nome + 6 colonne) dopo il primo titolo di tabella che non sia nell'indice."""
    for i, r in enumerate(righe):
        if titolo in r and "....." not in r:
            out = {}
            for r2 in righe[i + 1:i + 40]:
                m = re.match(r"^\s*([A-Z][A-Za-z' ]+?)\s{2,}([\d.]+)\s+([\d,]+%)\s+([\d.]+)\s+([\d,]+)\s+([\d.,]+)\s+([\d.,]+)", r2)
                if m and m.group(1).strip().upper() in nomi:
                    out.setdefault(m.group(1).strip().upper(), m.groups()[1:])  # la prima occorrenza è il totale
            if out:
                return out
    return {}


righe_out = []
for anno_r, url in RAPPORTI.items():
    righe = testo_pdf(url)
    anno = anno_r - 1
    for seg, tit in SEGMENTI.items():
        citta = righe_tabella(righe, f"{tit} - Dati nelle principali città", CITTA)
        ita = righe_tabella(righe, f"{tit} - Dati per area territoriale", ["ITALIA"])
        for nome, v in list(citta.items()) + list(ita.items()):
            righe_out.append({
                "anno": anno, "segmento": seg,
                "area": "Totale 8 città" if nome == "TOTALE" else nome.title(),
                "abitazioni_locate": numero(v[0]), "iml_pct": numero(v[1]),
                "superficie_media_m2": numero(v[3]), "canone_annuo_complessivo_mln": numero(v[4]),
                "canone_annuo_medio_euro_m2": numero(v[5]),
                "fonte": f"Agenzia delle Entrate - OMI, Rapporto Immobiliare residenziale {anno_r}",
                "url": url,
            })
        print(anno, seg, sorted(citta), "Italia" if ita else "-")

df = pd.DataFrame(righe_out).sort_values(["segmento", "area", "anno"])
df["canone_mensile_medio_euro_m2"] = (df["canone_annuo_medio_euro_m2"] / 12).round(2)
df.to_csv(OUT, index=False)
for seg in SEGMENTI:
    print(seg)
    print(df[df.segmento == seg].pivot(index="area", columns="anno", values="canone_annuo_medio_euro_m2").to_string())
    print(df[df.segmento == seg].pivot(index="area", columns="anno", values="superficie_media_m2").to_string())
