# Indici ufficiali dei prezzi delle abitazioni e deflatore
#  - ISTAT IPAB (indice dei prezzi delle abitazioni, base 2025=100): Italia, ripartizioni
#    e i tre comuni per cui ISTAT pubblica l'indice (Torino, Milano, Roma), trimestrale e annuale
#  - ISTAT NIC (prezzi al consumo per l'intera collettività): indice generale mensile e annuale
#    raccordato in base 2025=100, usato come deflatore
#  - ISTAT NIC per capoluogo di provincia, voce 041 "Canoni di affitto effettivamente pagati"
#  - Eurostat prc_hpi_q (Italia, area euro, Ue) e OCSE (prezzi reali Italia dal 2000, per il confronto con il 2007-2008)
# Output in ../input/ con prefisso prezzi_
import io, os
import requests
import pandas as pd

CARTELLA = os.path.dirname(os.path.abspath(__file__))
INPUT = os.path.join(CARTELLA, "..", "input")
ISTAT = "https://esploradati.istat.it/SDMXWS/rest/data/"


def istat(query):
    url = ISTAT + query + ("&" if "?" in query else "?") + "format=csv"
    r = requests.get(url, timeout=300)
    r.raise_for_status()
    return pd.read_csv(io.StringIO(r.text))


NOMI = {"IT": "Italia", "ITC": "Nord-ovest", "ITD": "Nord-est", "ITE": "Centro", "ITFG": "Mezzogiorno",
        "ITC11": "Torino", "ITC45": "Milano", "ITE43": "Roma"}
TIPO = {"ALL": "totale", "NEW_DW": "nuove", "EXST_DW": "esistenti"}

# ---------------------------------------------------------------- IPAB trimestrale e annuale
ipab_q = istat("143_497_DF_DCSP_IPAB_6/Q...4./ALL/?startPeriod=2010")
ipab_q = ipab_q.assign(area=ipab_q.REF_AREA.map(NOMI), abitazioni=ipab_q.PURCHASES_DWELLINGS.map(TIPO))
ipab_q = ipab_q.rename(columns={"TIME_PERIOD": "trimestre", "OBS_VALUE": "ipab_2025_100"})[["area", "abitazioni", "trimestre", "ipab_2025_100"]]
ipab_a = istat("143_497_DF_DCSP_IPAB_7/A...4./ALL/?startPeriod=2010")
ipab_a = ipab_a.assign(area=ipab_a.REF_AREA.map(NOMI), abitazioni=ipab_a.PURCHASES_DWELLINGS.map(TIPO))
ipab_a = ipab_a.rename(columns={"TIME_PERIOD": "anno", "OBS_VALUE": "ipab_2025_100"})[["area", "abitazioni", "anno", "ipab_2025_100"]]

# ---------------------------------------------------------------- NIC indice generale, raccordo in base 2025
# Dataflow con tutte le basi (1995, 2010, 2015, 2025). Si raccorda con i coefficienti ufficiali:
# media 2010 in base 1995 = 139,8; media 2015 in base 2010 = 107,5; media 2025 in base 2015 = 122,6.
nic_m = istat("167_745_DF_DCSP_NIC1B2025_6/M.IT..4.00/ALL/?startPeriod=2005-01")
basi = {1: "1995", 9: "2010", 39: "2015", 85: "2025"}
nic_m["base"] = nic_m.DATA_TYPE.map(lambda x: basi.get(x, str(x)))
print("tipi dato NIC mensile:", nic_m.groupby("DATA_TYPE").TIME_PERIOD.agg(["min", "max"]).to_string())
nic_a = istat("167_747_DCSP_NIC2B2025_1/A.IT..4.00/ALL/?startPeriod=2005")
media = {t: d.set_index("TIME_PERIOD").OBS_VALUE.to_dict() for t, d in nic_a.groupby("DATA_TYPE")}
# indici annuali per base (DATA_TYPE 2 = base 1995, 10 = base 2010, 40 = base 2015)
k_95_10 = 100 / 139.8
k_10_15 = 100 / 107.5
k_15_25 = 100 / 122.6
assert abs(media[2][2010] - 139.8) < 0.01 and abs(media[10][2015] - 107.5) < 0.01 and abs(media[40][2025] - 122.6) < 0.01


def in_base_2025(valore, data_type):
    if data_type in (1, 2):
        return valore * k_95_10 * k_10_15 * k_15_25
    if data_type in (9, 10):
        return valore * k_10_15 * k_15_25
    if data_type in (39, 40):
        return valore * k_15_25
    return valore  # base 2025


nic_m["nic_2025_100"] = [in_base_2025(v, t) for v, t in zip(nic_m.OBS_VALUE, nic_m.DATA_TYPE)]
nic_m = nic_m.rename(columns={"TIME_PERIOD": "mese"})
# per i mesi presenti in più basi si tiene la base più recente
nic_m = nic_m.sort_values(["mese", "DATA_TYPE"]).groupby("mese").tail(1)[["mese", "nic_2025_100"]].reset_index(drop=True)
nic_m["anno"] = nic_m.mese.str[:4].astype(int)
nic_m["trimestre"] = nic_m.anno.astype(str) + "-Q" + ((nic_m.mese.str[5:7].astype(int) - 1) // 3 + 1).astype(str)
nic_annuo = nic_m.groupby("anno").agg(nic_2025_100=("nic_2025_100", "mean"), mesi=("mese", "count")).reset_index()
nic_trim = nic_m.groupby("trimestre").agg(nic_2025_100=("nic_2025_100", "mean"), mesi=("mese", "count")).reset_index()
nic_m.to_csv(os.path.join(INPUT, "prezzi_istat_nic_mensile.csv"), index=False)
nic_annuo.to_csv(os.path.join(INPUT, "prezzi_istat_nic_annuale.csv"), index=False)

# ---------------------------------------------------------------- IPAB reale (deflazionato con NIC)
ipab_q = ipab_q.merge(nic_trim[["trimestre", "nic_2025_100"]], on="trimestre", how="left")
ipab_q["ipab_reale_2025_100"] = ipab_q.ipab_2025_100 / ipab_q.nic_2025_100 * 100
ipab_a = ipab_a.merge(nic_annuo[["anno", "nic_2025_100"]], on="anno", how="left")
ipab_a["ipab_reale_2025_100"] = ipab_a.ipab_2025_100 / ipab_a.nic_2025_100 * 100
ipab_q.to_csv(os.path.join(INPUT, "prezzi_istat_ipab_trimestrale.csv"), index=False)
ipab_a.to_csv(os.path.join(INPUT, "prezzi_istat_ipab_annuale.csv"), index=False)

# ---------------------------------------------------------------- NIC affitti per capoluogo (voce 041)
aff = istat("167_742_DF_DCSP_NIC2B2015_3/A...4.041+00/ALL/?startPeriod=2016")
terr = requests.get("https://esploradati.istat.it/SDMXWS/rest/codelist/IT1/CL_ITTER107", timeout=120, headers={"Accept": "application/vnd.sdmx.structure+json"})
try:
    codici = {c["id"]: c["names"].get("it", c["name"]) for c in terr.json()["data"]["codelists"][0]["codes"]}
except Exception:
    codici = {}
aff["area"] = aff.REF_AREA.map(codici).fillna(aff.REF_AREA)
aff = aff.rename(columns={"TIME_PERIOD": "anno", "OBS_VALUE": "indice_2015_100", "E_COICOP_REV_ISTAT": "ecoicop"})
aff["voce"] = aff.ecoicop.astype(int).map({41: "canoni di affitto effettivi", 0: "indice generale"})  # il csv legge i codici come numeri
aff[["REF_AREA", "area", "voce", "anno", "indice_2015_100"]].to_csv(os.path.join(INPUT, "prezzi_istat_nic_affitti_capoluoghi.csv"), index=False)

# ---------------------------------------------------------------- Eurostat e OCSE
es = pd.read_csv(io.StringIO(requests.get(
    "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/prc_hpi_q/Q.TOTAL+DW_NEW+DW_EXST.I15_Q.IT+EA20+EU27_2020?format=SDMX-CSV&startPeriod=2010",
    timeout=120).text))
es = es.rename(columns={"TIME_PERIOD": "trimestre", "OBS_VALUE": "hpi_2015_100", "OBS_FLAG": "flag"})[["geo", "purchase", "trimestre", "hpi_2015_100", "flag"]]
es.to_csv(os.path.join(INPUT, "prezzi_eurostat_hpi_trimestrale.csv"), index=False)

oc = pd.read_csv(io.StringIO(requests.get(
    "https://sdmx.oecd.org/public/rest/data/OECD.ECO.MPD,DSD_AN_HOUSE_PRICES@DF_HOUSE_PRICES,1.0/ITA.A..?startPeriod=2000&dimensionAtObservation=AllDimensions",
    headers={"Accept": "application/vnd.sdmx.data+csv; charset=utf-8"}, timeout=120).text))
oc = oc.pivot_table(index="TIME_PERIOD", columns="MEASURE", values="OBS_VALUE").reset_index().rename(columns={"TIME_PERIOD": "anno"})
oc.columns.name = None
oc = oc.rename(columns={"HPI": "prezzi_nominali_2015_100", "RHP": "prezzi_reali_2015_100", "RPI": "affitti_2015_100",
                        "HPI_RPI": "prezzi_su_affitti_2015_100", "HPI_YDH": "prezzi_su_reddito_disp_pc_2015_100"})
oc[[c for c in oc.columns if not c.endswith("_AVG")]].to_csv(os.path.join(INPUT, "prezzi_ocse_italia_annuale.csv"), index=False)

print(ipab_a[ipab_a.abitazioni == "totale"].pivot(index="anno", columns="area", values="ipab_reale_2025_100").round(1).to_string())
print(ipab_q[(ipab_q.abitazioni == "totale") & (ipab_q.trimestre >= "2025-Q1")].pivot(index="trimestre", columns="area", values="ipab_2025_100").to_string())
print(nic_annuo.round(2).to_string())
