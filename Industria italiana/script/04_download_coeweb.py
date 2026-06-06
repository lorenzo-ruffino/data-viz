#!/usr/bin/env python3
# Scarica dati di commercio estero (export/import) dal databrowser Coeweb di ISTAT.
# Coeweb e' un nodo SDMX separato non raggiungibile dal server MCP istat: si usa
# l'API del databrowserhub.  Endpoint dati:
#   POST .../Coeweb/databrowserhub/api/core/nodes/1/datasets/{IT1,DATAFLOW,1.0}/data
#   body = lista di filtri  [{"id":"DIM","filterValues":["code",...]}, ...]
# La risposta e' in formato JSON-stat 2.0.
import json, urllib.request, csv, os

BASE = "https://esploradati.istat.it/Coeweb/databrowserhub/api/core/nodes/1/datasets"
INP = os.path.join(os.path.dirname(__file__), "..", "input")

def fetch(dataflow, filters):
    url = f"{BASE}/{dataflow}/data"
    body = json.dumps(filters).encode()
    req = urllib.request.Request(url, data=body, method="POST",
                                 headers={"Content-Type": "application/json"})
    with urllib.request.urlopen(req, timeout=120) as r:
        return json.load(r)

def jsonstat_rows(d):
    """Decodifica un JSON-stat 2.0: restituisce lista di dict {dim: codice, ..., value}."""
    dims = d["id"]; size = d["size"]
    strides = [1]*len(size)
    for k in range(len(size)-2, -1, -1):
        strides[k] = strides[k+1]*size[k+1]
    cats = []  # per ogni dim: lista ordinata di codici
    for dim in dims:
        cats.append(d["dimension"][dim]["category"]["index"])
    rows = []
    for flat, val in d["value"].items():
        flat = int(flat)
        rec = {}
        for k, dim in enumerate(dims):
            idx = (flat // strides[k]) % size[k]
            rec[dim] = cats[k][idx]
        rec["value"] = val
        rows.append(rec)
    return rows

CPA = ["C","CA","CB","CC","CD","CE","CF","CG","CH","CI","CJ","CK","CL","CM"]
CPA_LAB = {"C":"Totale manifattura","CA":"Alimentari bevande tabacco",
 "CB":"Tessile abbigliamento pelle","CC":"Legno carta stampa",
 "CD":"Coke e raffinazione petrolio","CE":"Chimica",
 "CF":"Farmaceutica","CG":"Gomma plastica minerali non met.",
 "CH":"Metallurgia e prodotti in metallo","CI":"Elettronica e ottica",
 "CJ":"Apparecchiature elettriche","CK":"Macchinari","CL":"Mezzi di trasporto",
 "CM":"Altri prodotti manifatturieri"}

# --- 1) Export e import nazionali per settore manifatturiero, 1991-2025 ----
print("1) Commercio estero nazionale per settore manifatturiero (CPA)")
d = fetch("IT1,DF_CPA_AT2007_COE_OPT_A_ALL,1.0", [
    {"id":"FREQ","filterValues":["A"]},
    {"id":"REF_AREA","filterValues":["ITTOT"]},
    {"id":"PARTNER_COUNTRY","filterValues":["WORLD"]},
    {"id":"DATA_TYPE","filterValues":["EV","IV"]},
    {"id":"CPA_ATECO2007_COE","filterValues":CPA},
])
dtlab = {"EV":"esportazioni","IV":"importazioni"}
rows = jsonstat_rows(d)
out = [("settore","codice_cpa","flusso","anno","valore_euro")]
for r in rows:
    out.append((CPA_LAB.get(r["CPA_ATECO2007_COE"], r["CPA_ATECO2007_COE"]),
                r["CPA_ATECO2007_COE"], dtlab.get(r["DATA_TYPE"], r["DATA_TYPE"]),
                r["TIME_PERIOD"], r["value"]))
out[1:] = sorted(out[1:], key=lambda x:(x[1],x[2],x[3]))
with open(os.path.join(INP,"coeweb_commercio_estero_per_settore.csv"),"w",newline="") as f:
    csv.writer(f).writerows(out)
print(f"   righe: {len(out)-1}  -> coeweb_commercio_estero_per_settore.csv")

# --- 2) Export manifatturiero per regione, 1991-2025 -----------------------
print("2) Export manifatturiero (CPA=C) per regione")
d = fetch("IT1,DF_CPA_AT2007_COE_OPT_A_REG_ALL,1.0", [
    {"id":"FREQ","filterValues":["A"]},
    {"id":"PARTNER_COUNTRY","filterValues":["WORLD"]},
    {"id":"DATA_TYPE","filterValues":["EV"]},
    {"id":"CPA_ATECO2007_COE","filterValues":["C"]},
])
rows = jsonstat_rows(d)
# etichette REF_AREA dalla risposta
arealab = d["dimension"]["REF_AREA"]["category"]["label"]
out = [("territorio","codice","anno","export_manifattura_euro")]
for r in rows:
    out.append((arealab.get(r["REF_AREA"], r["REF_AREA"]), r["REF_AREA"],
                r["TIME_PERIOD"], r["value"]))
out[1:] = sorted(out[1:], key=lambda x:(x[1],x[2]))
with open(os.path.join(INP,"coeweb_export_manifattura_per_regione.csv"),"w",newline="") as f:
    csv.writer(f).writerows(out)
print(f"   righe: {len(out)-1}  -> coeweb_export_manifattura_per_regione.csv")

# --- 3) Export manifatturiero per paese/area di destinazione, 1991-2025 ----
print("3) Export manifatturiero (CPA=C) per destinazione")
PART = {"WORLD":"Mondo","EU27":"Unione Europea (27)","EXT_EU":"Extra-UE",
        "DE":"Germania","FR":"Francia","US":"Stati Uniti","GB":"Regno Unito",
        "CH":"Svizzera","CN":"Cina","ES":"Spagna"}
d = fetch("IT1,DF_CPA_AT2007_COE_OPT_A_ALL,1.0", [
    {"id":"FREQ","filterValues":["A"]},
    {"id":"REF_AREA","filterValues":["ITTOT"]},
    {"id":"DATA_TYPE","filterValues":["EV"]},
    {"id":"CPA_ATECO2007_COE","filterValues":["C"]},
    {"id":"PARTNER_COUNTRY","filterValues":list(PART.keys())},
])
rows = jsonstat_rows(d)
out = [("destinazione","codice","anno","export_manifattura_euro")]
for r in rows:
    out.append((PART.get(r["PARTNER_COUNTRY"], r["PARTNER_COUNTRY"]),
                r["PARTNER_COUNTRY"], r["TIME_PERIOD"], r["value"]))
out[1:] = sorted(out[1:], key=lambda x:(x[1],x[2]))
with open(os.path.join(INP,"coeweb_export_manifattura_per_destinazione.csv"),"w",newline="") as f:
    csv.writer(f).writerows(out)
print(f"   righe: {len(out)-1}  -> coeweb_export_manifattura_per_destinazione.csv")
print("FATTO.")
