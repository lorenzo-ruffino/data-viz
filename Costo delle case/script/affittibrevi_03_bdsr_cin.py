"""
Strutture ricettive e codici CIN per comune dalla Banca dati nazionale delle strutture
ricettive (BDSR) del Ministero del Turismo, rapportate alle abitazioni ISTAT 2021.

Fonte: export CSV della dashboard pubblica https://bdsr.ministeroturismo.gov.it/
(endpoint api-bdsr.ministeroturismo.gov.it/ui/user/downloadCsvDashboard, che restituisce
il CSV codificato in base64; la data di aggiornamento è nel nome del file).
Le categorie sono quelle ISTAT della rilevazione sulla capacità degli esercizi ricettivi:
- "Altri alloggi privati" (C2): locazioni brevi e turistiche non imprenditoriali
- "Alloggi in affitto gestiti in forma imprenditoriale" (B4): case vacanza, affittacamere,
  locazioni turistiche in forma d'impresa
- "Bed and breakfast" (C1), "Esercizi alberghieri" (A), resto (campeggi, agriturismi...)

Le abitazioni per comune vengono dal dataflow ISTAT DF_DCSS_ABITAZIONI_TV_1 (scaricato da
affittibrevi_02_indicatori.R nella cartella dei dati privati).
Da lanciare dalla radice del repo data-viz.
"""
import base64
import io
import json
import urllib.request
from pathlib import Path

import pandas as pd

DATI_PRIVATI = Path.home() / "Desktop" / "Costo delle case" / "dati_privati"
OUT = Path("Costo delle case/input")

f_raw = DATI_PRIVATI / "bdsr" / "nazionale.json"
f_raw.parent.mkdir(parents=True, exist_ok=True)
if not f_raw.exists():
    url = "https://api-bdsr.ministeroturismo.gov.it/ui/user/downloadCsvDashboard"
    f_raw.write_bytes(urllib.request.urlopen(url, timeout=120).read())

j = json.loads(f_raw.read_text())
data_agg = j["nomeFile"].removeprefix("Italia").removesuffix(".csv")  # es. 01_10_2026
d = pd.read_csv(io.StringIO(base64.b64decode(j["csv"]).decode("utf-8")), dtype=str)
num = ["Totale strutture con CIN verificato", "Totale strutture con CIN non verificato",
       "Totale strutture senza CIN", "Totale strutture"]
d[num] = d[num].apply(pd.to_numeric)

d["pro_com"] = d["Codice ISTAT Provincia"].str.zfill(3) + d["Codice ISTAT Comune"].str.zfill(3)
gruppi = {
    "Altri alloggi privati": "alloggi_privati_c2",
    "Alloggi in affitto gestiti in forma imprenditoriale": "alloggi_imprenditoriali_b4",
    "Bed and breakfast": "bed_and_breakfast",
    "Esercizi alberghieri": "alberghi",
}
d["gruppo"] = d["Categoria ISTAT"].map(gruppi).fillna("altre_strutture")

chiavi = ["pro_com", "Comune", "Provincia", "Regione"]
per_gruppo = d.pivot_table(index=chiavi, columns="gruppo", values="Totale strutture",
                           aggfunc="sum", fill_value=0)
totali = d.groupby(chiavi)[num].sum()
totali.columns = ["cin_verificato", "cin_non_verificato", "senza_cin", "strutture_totali"]
out = per_gruppo.join(totali).reset_index()
out = out.rename(columns={"Comune": "comune", "Provincia": "provincia", "Regione": "regione"})

# abitazioni ISTAT al 31/12/2021
abi = pd.read_csv(DATI_PRIVATI / "istat_abitazioni_comuni.csv", dtype={"REF_AREA": str})
abi = abi[(abi.INDICATOR == "NUM_DW_AV") & (abi.TIME_PERIOD == 2021)]
abi = abi[["REF_AREA", "OBS_VALUE"]].rename(columns={"REF_AREA": "pro_com", "OBS_VALUE": "abitazioni_2021"})
out = out.merge(abi, on="pro_com", how="left")

out["alloggi_residenziali_turistici"] = out["alloggi_privati_c2"] + out["alloggi_imprenditoriali_b4"]
out["alloggi_turistici_per_100_abitazioni"] = 100 * out["alloggi_residenziali_turistici"] / out["abitazioni_2021"]
out["quota_con_cin"] = (out["cin_verificato"] + out["cin_non_verificato"]) / out["strutture_totali"]
out["data_aggiornamento_bdsr"] = data_agg.replace("_", "/")
out = out.sort_values("strutture_totali", ascending=False)
out.to_csv(OUT / "affittibrevi_bdsr_comuni.csv", index=False)

citta = ["058091", "015146", "048017", "063049", "027042", "037006", "016024", "001272", "082053", "072006"]
cols = ["comune", "strutture_totali", "alloggi_privati_c2", "alloggi_imprenditoriali_b4", "bed_and_breakfast",
        "alberghi", "senza_cin", "abitazioni_2021", "alloggi_turistici_per_100_abitazioni"]
print("Aggiornamento BDSR:", data_agg)
print(out.set_index("pro_com").loc[citta, cols].round(2).to_string())
print(out[num_cols := ["strutture_totali", "alloggi_privati_c2", "alloggi_imprenditoriali_b4",
                      "bed_and_breakfast", "alberghi", "senza_cin"]].sum())
grandi = out[out.abitazioni_2021 >= 5000].sort_values("alloggi_turistici_per_100_abitazioni", ascending=False)
print(grandi[cols].head(20).round(1).to_string())
