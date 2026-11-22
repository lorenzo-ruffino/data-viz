# Studenti universitari fuori sede nelle città italiane
#
# Stima per difetto dei fuori sede a partire dall'Anagrafe Nazionale Studenti
# (open data USTAT-MUR), confrontata con popolazione residente, famiglie in
# affitto (Censimento permanente 2021) e posti letto in residenze pubbliche.
#
# Fonti lette direttamente dagli URL (nessun file grezzo salvato):
# - USTAT-MUR, dataset "iscritti" (https://dati-ustat.mur.gov.it/dataset/iscritti):
#   14a = iscritti per provincia di residenza e provincia della sede didattica
#   (telematiche escluse, celle < 5 soppresse); 14c = iscritti per provincia
#   della scuola superiore di diploma e provincia della sede didattica
# - USTAT-MUR, posti alloggio DSU per comune al 1/11/2025, posti nei collegi
#   di merito per comune, posti in residenze gestite dagli atenei
# - ISTAT, popolazione residente per comune al 1/1/2026 (stima, demo.istat.it)
# - ISTAT, famiglie per titolo di godimento dell'abitazione, comuni, 2021
#   (dataflow DF_DCSS_HUDW_1_COM, esploradati.istat.it)
#
# Uscite in ../input/:
#   studenti_fuori_sede_province.csv     tutte le province sede di corsi, a.a. 2024/25
#   studenti_fuori_sede_citta.csv        città universitarie selezionate con rapporti
#   studenti_fuori_sede_serie.csv        serie 2010/11-2024/25 per le città selezionate
#   studenti_fuori_sede_italia_serie.csv serie nazionale 2010/11-2024/25
#   studenti_posti_letto_comune.csv      posti letto pubblici per comune (1/11/2025)
#   studenti_famiglie_affitto_comuni_2021.csv  copia dei dati ISTAT sulle famiglie in affitto

import io
import zipfile
from pathlib import Path

import pandas as pd
import requests

CARTELLA = Path(__file__).resolve().parent.parent
INPUT = CARTELLA / "input"

ANNO = "2024/2025"

URL = {
    "14a": "https://dati-ustat.mur.gov.it/dataset/3dd9ca7f-9cc9-4a1a-915c-e569b181dbd5/resource/b270ef1a-c219-48b1-8399-b1458e225d39/download/14a_iscrittixresidenzasedecorsogruppo.csv",
    "14c": "https://dati-ustat.mur.gov.it/dataset/3dd9ca7f-9cc9-4a1a-915c-e569b181dbd5/resource/349e58d7-6ea6-4b99-99cb-b8748fccbaf6/download/14c_iscrittixsedescuolasecondariasedecorso.csv",
    "07": "https://dati-ustat.mur.gov.it/dataset/3dd9ca7f-9cc9-4a1a-915c-e569b181dbd5/resource/eae4ee94-0797-41d2-b007-bc6dad3ef3e2/download/07_iscrittixresidenza.csv",
    "dsu": "https://dati-ustat.mur.gov.it/dataset/b89d46ba-1b59-4a45-9165-5b09df42a884/resource/bb4c7a89-ba45-4c1d-9a1d-f21613057760/download/2025_dsu_alloggi_comune.csv",
    "collegi": "https://dati-ustat.mur.gov.it/dataset/0a78be58-c096-4d59-a659-e8a610b49207/resource/eaa8d54b-f626-46cb-8e1e-8d0aebc37096/download/2025_collegi_alloggi_comune.csv",
    "atenei": "https://dati-ustat.mur.gov.it/dataset/efb04926-7ff7-43ed-900c-11738f4f7231/resource/030c57ae-47bb-4bf5-bf3f-ea5ce24bc15e/download/2025_atenei_strutture.csv",
    "pop": "https://demo.istat.it/data/posas/POSAS_2026_it_Comuni.zip",
}

# Città universitarie: nome, comune (codice ISTAT a 6 cifre), provincia della
# sede didattica (codice ISTAT), nome del comune nei file dei posti letto.
CITTA = [
    ("Milano", "015146", 15, "MILANO"), ("Roma", "058091", 58, "ROMA"),
    ("Bologna", "037006", 37, "BOLOGNA"), ("Torino", "001272", 1, "TORINO"),
    ("Napoli", "063049", 63, "NAPOLI"), ("Padova", "028060", 28, "PADOVA"),
    ("Firenze", "048017", 48, "FIRENZE"), ("Pisa", "050026", 50, "PISA"),
    ("Pavia", "018110", 18, "PAVIA"), ("Siena", "052032", 52, "SIENA"),
    ("Perugia", "054039", 54, "PERUGIA"), ("Urbino", "041067", 41, "URBINO"),
    ("Trento", "022205", 22, "TRENTO"), ("Venezia", "027042", 27, "VENEZIA"),
    ("Genova", "010025", 10, "GENOVA"), ("Bari", "072006", 72, "BARI"),
    ("Palermo", "082053", 82, "PALERMO"), ("Catania", "087015", 87, "CATANIA"),
    ("Parma", "034027", 34, "PARMA"), ("Modena", "036023", 36, "MODENA"),
    ("Ferrara", "038008", 38, "FERRARA"), ("Bergamo", "016024", 16, "BERGAMO"),
    ("Macerata", "043023", 43, "MACERATA"), ("Trieste", "032006", 32, "TRIESTE"),
    ("Verona", "023091", 23, "VERONA"), ("Cagliari", "092009", 92, "CAGLIARI"),
    ("Messina", "083048", 83, "MESSINA"), ("Salerno/Fisciano", "065116", 65, "FISCIANO"),
    ("Rende (Unical)", "078102", 78, "RENDE"),
]


def leggi_csv(chiave, **kw):
    return pd.read_csv(URL[chiave], sep=";", encoding="latin-1", **kw)


def regione_unica(r):
    # Trento e Bolzano formano un'unica regione (Trentino-Alto Adige)
    if isinstance(r, str) and r.startswith("Provincia autonoma"):
        return "TRENTINO-ALTO ADIGE"
    return r


# --- Anagrafica province -> regione (dal file per comune di residenza) ---------
res = leggi_csv("07")
prov = (res[["CODIstatProv", "ProvinciaRES", "RegioneRES"]]
        .drop_duplicates("CODIstatProv")
        .rename(columns={"CODIstatProv": "cod_prov", "ProvinciaRES": "provincia",
                         "RegioneRES": "regione"}))
prov["regione"] = prov["regione"].map(regione_unica)
# vecchie province sarde che compaiono ancora tra le province del diploma
extra = pd.DataFrame({"cod_prov": [104, 105, 106, 107],
                      "provincia": ["OLBIA-TEMPIO", "OGLIASTRA", "MEDIO CAMPIDANO", "CARBONIA-IGLESIAS"],
                      "regione": ["SARDEGNA"] * 4})
prov = pd.concat([prov, extra], ignore_index=True)
nome_prov = dict(zip(prov.cod_prov, prov.provincia))
reg_prov = dict(zip(prov.cod_prov, prov.regione))

# --- Misura 1: provincia di residenza vs provincia della sede didattica ----------
a = leggi_csv("14a")
a = a[a.SedeP.notna()].copy()
a["SedeP"] = a.SedeP.astype(int)
a["regione_sede"] = a.SedeP.map(reg_prov)
a["regione_res"] = a.ResidenzaR.map(regione_unica)
non_def = a.ResidenzaP.eq("PROVINCIA NON DEFINITA")
a["Isc_fp"] = a.Isc.where((a.ResidenzaP != a.SedeP.map(nome_prov)) & ~non_def, 0)
a["Isc_fr"] = a.Isc.where((a.regione_res != a.regione_sede) & ~non_def, 0)
a["Isc_est"] = a.Isc.where(a.ResidenzaP.eq("PROVINCIA ESTERA"), 0)
serie_res = (a.groupby(["AnnoA", "SedeP"])[["Isc", "Isc_fp", "Isc_fr", "Isc_est"]].sum()
             .rename(columns={"Isc": "iscritti", "Isc_fp": "fuori_provincia",
                              "Isc_fr": "fuori_regione", "Isc_est": "residenti_estero"})
             .reset_index())

# --- Misura 2: provincia della scuola di diploma vs provincia della sede ----------
c = leggi_csv("14c")
c = c[c.IstatProvD != 999].copy()  # provincia del diploma non definita
c["Isc_fpd"] = c.Isc.where(c.IstatProvD != c.IstatProvC, 0)
c["Isc_frd"] = c.Isc.where(c.IstatProvD.map(reg_prov) != c.IstatProvC.map(reg_prov), 0)
dipl = (c.groupby(["AnnoA", "IstatProvC"])[["Isc", "Isc_fpd", "Isc_frd"]].sum()
        .rename(columns={"Isc": "iscritti_diploma_noto", "Isc_fpd": "fuori_provincia_diploma",
                         "Isc_frd": "fuori_regione_diploma"})
        .reset_index().rename(columns={"IstatProvC": "SedeP"}))

province = serie_res.merge(dipl, on=["AnnoA", "SedeP"], how="left")
province.insert(2, "provincia_sede", province.SedeP.map(nome_prov))
province.insert(3, "regione_sede", province.SedeP.map(reg_prov))
province["quota_fuori_provincia"] = (province.fuori_provincia / province.iscritti).round(3)
province["quota_fuori_regione"] = (province.fuori_regione / province.iscritti).round(3)
province["quota_fuori_provincia_diploma"] = (province.fuori_provincia_diploma / province.iscritti_diploma_noto).round(3)
province["quota_fuori_regione_diploma"] = (province.fuori_regione_diploma / province.iscritti_diploma_noto).round(3)
province = province.rename(columns={"AnnoA": "anno_accademico", "SedeP": "cod_prov_sede"})
province["fonte"] = ("USTAT-MUR, Anagrafe Nazionale Studenti, open data iscritti 14a e 14c "
                     "(telematiche escluse) https://dati-ustat.mur.gov.it/dataset/iscritti")

# serie nazionale (somma delle province sede)
naz = province.groupby("anno_accademico")[["iscritti", "fuori_provincia", "fuori_regione", "residenti_estero"]].sum()
naz["quota_fuori_provincia"] = (naz.fuori_provincia / naz.iscritti).round(3)
naz["quota_fuori_regione"] = (naz.fuori_regione / naz.iscritti).round(3)
naz["fonte"] = province.fonte.iloc[0]
naz.to_csv(INPUT / "studenti_fuori_sede_italia_serie.csv")

ultimo = province[province.anno_accademico == ANNO].sort_values("fuori_provincia", ascending=False)
ultimo.to_csv(INPUT / "studenti_fuori_sede_province.csv", index=False)

# --- Popolazione residente al 1/1/2026 (stima) ----------------------------------
z = zipfile.ZipFile(io.BytesIO(requests.get(URL["pop"], timeout=300).content))
nome = [n for n in z.namelist() if n.endswith(".csv")][0]
pop = pd.read_csv(io.BytesIO(z.read(nome)), sep=";", skiprows=1, dtype={"Codice comune": str})
pop = pop[pop["Età"] == 999]
pop = dict(zip(pop["Codice comune"], pop["Totale"]))

# --- Famiglie in affitto, Censimento permanente 2021 -----------------------------
# Se l'API ISTAT non risponde si usa la copia già scaricata in input/.
codici = "+".join(x[1] for x in CITTA)
url_aff = ("https://esploradati.istat.it/SDMXWS/rest/data/DF_DCSS_HUDW_1_COM/"
           f".{codici}..ALL+RENT....../ALL/?startPeriod=2021&endPeriod=2021&format=csv")
copia_aff = INPUT / "studenti_famiglie_affitto_comuni_2021.csv"
try:
    aff = pd.read_csv(io.StringIO(requests.get(url_aff, timeout=120).text), dtype={"REF_AREA": str})
    aff = aff.pivot_table(index="REF_AREA", columns="TENURE_STATUS_HOU", values="OBS_VALUE")
    aff = aff.rename(columns={"ALL": "famiglie", "RENT": "famiglie_affitto"})[["famiglie", "famiglie_affitto"]]
    aff.reset_index().to_csv(copia_aff, index=False)
except (requests.RequestException, KeyError, pd.errors.ParserError):
    aff = pd.read_csv(copia_aff, dtype={"REF_AREA": str}).set_index("REF_AREA")
famiglie = aff["famiglie"].to_dict()
famiglie_affitto = aff["famiglie_affitto"].round().to_dict()

# --- Posti letto pubblici per comune (1/11/2025) --------------------------------
dsu = leggi_csv("dsu", dtype=str)
dsu["posti"] = pd.to_numeric(dsu.NUMERO, errors="coerce")
dsu = dsu.groupby("NOME_COMUNE").posti.sum()

col = leggi_csv("collegi", dtype=str)
col["posti"] = pd.to_numeric(col.NUMERO, errors="coerce")
col = col.groupby("NOME_COMUNE").posti.sum()

ate = leggi_csv("atenei", dtype=str)
ate["posti"] = pd.to_numeric(ate.POSTI_DISPONIBILI, errors="coerce").clip(lower=0)
# solo i totali (02 gestione diretta, 03 convenzionate o affidate a terzi), non i "di cui"
ate = ate[ate.CODICE_STRUTTURA.str.strip().isin(["02", "03"])]
# il comune dell'ateneo è il primo elemento del nome ("Milano - Politecnico")
ate["comune"] = ate.NOME_ATENEO.str.split(" - ").str[0].str.strip().str.upper()
ate["comune"] = ate.comune.replace({"ARCAVACATA DI RENDE": "RENDE"})
ate = ate.groupby("comune").posti.sum()

posti = pd.DataFrame({"posti_dsu": dsu, "posti_collegi_merito": col, "posti_atenei": ate}).fillna(0).astype(int)
posti["posti_totali"] = posti.sum(axis=1)
posti.index.name = "comune"
posti = posti.sort_values("posti_totali", ascending=False)
posti["data_riferimento"] = "2025-11-01"
posti["fonte"] = ("USTAT-MUR, open data DSU regionale 2025 (alloggi per comune), collegi universitari 2025, "
                  "contribuzione e interventi atenei 2025 (strutture) https://dati-ustat.mur.gov.it")
posti.to_csv(INPUT / "studenti_posti_letto_comune.csv")

# --- Tabella città ---------------------------------------------------------------
righe = []
for citta, cod_com, cod_prov, comune_posti in CITTA:
    p = ultimo[ultimo.cod_prov_sede == cod_prov].iloc[0]
    pl = int(posti.loc[comune_posti, "posti_totali"]) if comune_posti in posti.index else 0
    righe.append({
        "citta": citta,
        "provincia_sede": p.provincia_sede,
        "anno_accademico": ANNO,
        "iscritti_sedi_provincia": int(p.iscritti),
        "fuori_provincia_residenza": int(p.fuori_provincia),
        "fuori_regione_residenza": int(p.fuori_regione),
        "residenti_estero": int(p.residenti_estero),
        "quota_fuori_provincia": p.quota_fuori_provincia,
        "quota_fuori_regione": p.quota_fuori_regione,
        "fuori_provincia_diploma": int(p.fuori_provincia_diploma),
        "quota_fuori_provincia_diploma": p.quota_fuori_provincia_diploma,
        "popolazione_comune_2026": pop.get(cod_com),
        "famiglie_comune_2021": famiglie.get(cod_com),
        "famiglie_affitto_comune_2021": famiglie_affitto.get(cod_com),
        "posti_letto_pubblici_comune_2025": pl,
    })
citta = pd.DataFrame(righe)
citta["fuori_provincia_per_100_residenti"] = (100 * citta.fuori_provincia_residenza / citta.popolazione_comune_2026).round(1)
citta["fuori_provincia_per_100_famiglie_affitto"] = (100 * citta.fuori_provincia_residenza / citta.famiglie_affitto_comune_2021).round(1)
citta["fuori_provincia_per_posto_letto"] = (citta.fuori_provincia_residenza / citta.posti_letto_pubblici_comune_2025).round(1)
citta["posti_letto_per_100_fuori_provincia"] = (100 * citta.posti_letto_pubblici_comune_2025 / citta.fuori_provincia_residenza).round(1)
citta["fonte"] = ("USTAT-MUR ANS open data 14a/14c (a.a. 2024/25); ISTAT POSAS 1/1/2026 (stima); "
                  "ISTAT Censimento permanente 2021 DF_DCSS_HUDW_1_COM; USTAT-MUR posti alloggio 1/11/2025")
citta = citta.sort_values("fuori_provincia_residenza", ascending=False)
citta.to_csv(INPUT / "studenti_fuori_sede_citta.csv", index=False)

# --- Serie storica per le città selezionate --------------------------------------
cod_sel = {x[2]: x[0] for x in CITTA}
serie = province[province.cod_prov_sede.isin(cod_sel)].copy()
serie.insert(0, "citta", serie.cod_prov_sede.map(cod_sel))
serie = serie[["citta", "anno_accademico", "iscritti", "fuori_provincia", "fuori_regione",
               "residenti_estero", "quota_fuori_provincia", "quota_fuori_regione", "fonte"]]
serie.sort_values(["citta", "anno_accademico"]).to_csv(INPUT / "studenti_fuori_sede_serie.csv", index=False)

print(citta.drop(columns="fonte").to_string(index=False))
