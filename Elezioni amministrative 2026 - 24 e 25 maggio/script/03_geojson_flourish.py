"""Unisce le geometrie dei comuni Istat con i dati elettorali del 24-25 maggio
2026 e produce un GeoJSON unico da caricare su Flourish."""
import csv
import json

GEO_IN = "/Users/lorenzoruffino/Downloads/Com01012025_g_WGS84(1).json"
CSV_IN = ("/Users/lorenzoruffino/Documents/Progetti/data-viz/"
          "Elezioni amministrative 2026 - 24 e 25 maggio/input/comuni_al_voto_2026.csv")
GEO_OUT = ("/Users/lorenzoruffino/Documents/Progetti/data-viz/"
           "Elezioni amministrative 2026 - 24 e 25 maggio/output/comuni_al_voto_flourish.geojson")

COLORI = {
    "Centrosinistra":        "#F12938",
    "Civico centrosinistra": "#F5A6B5",
    "Movimento 5 Stelle":    "#F2A900",
    "Altro":                 "#1B9E77",
    "Civico centrodestra":   "#5C9CDE",
    "Centrodestra":          "#0E5BAD",
    "Comuni minori":         "#4D4D4D",
    "Non al voto":           "#F7F7F7",
}

COAL_CAT = {
    "centrosinistra":        "Centrosinistra",
    "civico_centrosinistra": "Civico centrosinistra",
    "M5S":                   "Movimento 5 Stelle",
    "M5S_CSX":               "Movimento 5 Stelle",
    "civico_altro":          "Altro",
    "civico_centrodestra":   "Civico centrodestra",
    "centrodestra":          "Centrodestra",
}


def categoria(tipologia, coalizione):
    if tipologia == "INF":
        return "Comuni minori"
    return COAL_CAT[coalizione]


# Dati elettorali: solo i 743 comuni al voto il 24-25 maggio.
voto = {}
with open(CSV_IN, encoding="utf-8") as f:
    for r in csv.DictReader(f):
        if r["data_elezione"] != "24-25 maggio 2026":
            continue
        code = r["codice_istat"].zfill(6)
        cat = categoria(r["tipologia"], r["coalizione"])
        sindaco = " ".join(p.title() for p in
                           (r["sindaco_nome"], r["sindaco_cognome"]) if p)
        voto[code] = {
            "al_voto": "Sì",
            "categoria": cat,
            "colore": COLORI[cat],
            "coalizione": r["coalizione"],
            "tipologia": r["tipologia"],
            "popolazione": int(r["popolazione_31_12_2021"]),
            "data_elezione": r["data_elezione"],
            "sindaco_uscente": sindaco,
        }

print(f"Comuni al voto 24-25 maggio nel CSV: {len(voto)}")

with open(GEO_IN, encoding="utf-8") as f:
    geo = json.load(f)

VUOTO = {
    "al_voto": "No",
    "categoria": "Non al voto",
    "colore": COLORI["Non al voto"],
    "coalizione": "",
    "tipologia": "",
    "popolazione": "",
    "data_elezione": "",
    "sindaco_uscente": "",
}

n_match = 0
for feat in geo["features"]:
    code = feat["properties"].get("PRO_COM_T", "")
    dati = voto.get(code)
    if dati:
        n_match += 1
        feat["properties"].update(dati)
    else:
        feat["properties"].update(VUOTO)

print(f"Comuni con dati elettorali agganciati: {n_match} / {len(voto)}")
mancanti = sorted(set(voto) - {f["properties"].get("PRO_COM_T") for f in geo["features"]})
if mancanti:
    print(f"Senza geometria (assenti in Com01012025): {mancanti}")

with open(GEO_OUT, "w", encoding="utf-8") as f:
    json.dump(geo, f, ensure_ascii=False)

print(f"Salvato: {GEO_OUT}")
