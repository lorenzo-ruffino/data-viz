#!/usr/bin/env python3
"""
Ripiego per l'Europa: scarica le MEDIE MENSILI ERA5-Land (dataset
reanalysis-era5-land-monthly-means, precalcolato e quindi senza le code di ore
del dataset "derived" giornaliero) di temperatura a 2 m sull'area Europa, per
i mesi e gli anni indicati, in UN file per mese-del-calendario (tutti gli anni).

La media mensile ERA5-Land coincide con la media delle medie giornaliere del
mese (differenza trascurabile dovuta al fuso UTC+1 delle statistiche derivate),
quindi serve da baseline 1991-2020 per la mappa e la classifica dei paesi.

Uso:
  python3 01b_scarica_mensili_europa.py --anni 1991 2025 --mesi 7,8
Output: input/nc/europa_mensili/era5land_t2m_monthly_m<MM>_<A1>-<A2>.nc
"""
import argparse, sys, time
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent
PROGETTO = SCRIPT_DIR.parent
ENV_FILE = PROGETTO.parent / ".env"
AREA_EUROPA = [72, -25, 34, 35]


def leggi_env():
    v = {}
    for riga in ENV_FILE.read_text().splitlines():
        if riga.strip() and not riga.startswith("#") and "=" in riga:
            k, val = riga.split("=", 1); v[k.strip()] = val.strip()
    return v["CDSAPI_URL"], v["CDSAPI_KEY"]


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--anni", nargs=2, type=int, required=True)
    ap.add_argument("--mesi", default="7,8")
    ap.add_argument("--force", action="store_true")
    a = ap.parse_args()
    url, key = leggi_env()
    import cdsapi
    out = PROGETTO / "input" / "nc" / "europa_mensili"
    out.mkdir(parents=True, exist_ok=True)
    c = cdsapi.Client(url=url, key=key, quiet=True, progress=False)
    anni = list(range(a.anni[0], a.anni[1] + 1))
    esito = 0
    for m in [int(x) for x in a.mesi.split(",")]:
        dest = out / f"era5land_t2m_monthly_m{m:02d}_{anni[0]}-{anni[-1]}.nc"
        if dest.exists() and dest.stat().st_size > 10_000 and not a.force:
            print(f"SALTO {dest.name}", flush=True); continue
        req = {
            "product_type": ["monthly_averaged_reanalysis"],
            "variable": ["2m_temperature"],
            "year": [str(y) for y in anni],
            "month": [f"{m:02d}"],
            "time": ["00:00"],
            "data_format": "netcdf",
            "download_format": "unarchived",
            "area": AREA_EUROPA,
        }
        t0 = time.time()
        try:
            c.retrieve("reanalysis-era5-land-monthly-means", req, str(dest))
            print(f"[{time.strftime('%H:%M:%S')}] OK {dest.name} ({dest.stat().st_size/1e6:.1f} MB, {time.time()-t0:.0f}s)", flush=True)
        except Exception as e:
            print(f"[{time.strftime('%H:%M:%S')}] ERRORE {dest.name}: {str(e)[:300]}", flush=True)
            if dest.exists(): dest.unlink()
            esito = 1
    sys.exit(esito)


if __name__ == "__main__":
    main()
