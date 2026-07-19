#!/usr/bin/env python3
"""
Scarica da Copernicus CDS i dati ORARI ERA5-Land di temperatura a 2 metri
sull'Italia, per lo split giorno/notte. Un file per mese, ~40-60 MB l'uno.

Default (senza argomenti): maggio e giugno di 1991-2020 (baseline, include il
2003) e del 2026 — 62 file, ~2,8 GB.

I file già presenti vengono saltati; --force per riscaricare.

Uso:
  python3 02_scarica_orari.py
  python3 02_scarica_orari.py --mensile 2026-06 2026-06 --force
  python3 02_scarica_orari.py --anni 1991 2020 --mesi 7,8   # estendere all'estate
"""

import argparse
import calendar
import sys
import threading
import time
from datetime import date, timedelta
from pathlib import Path
from queue import Queue

SCRIPT_DIR = Path(__file__).resolve().parent
PROGETTO = SCRIPT_DIR.parent
ENV_FILE = PROGETTO.parent / ".env"

DATASET = "reanalysis-era5-land"
AREA_ITALIA = [47.2, 6.6, 35.4, 18.6]
RITARDO_GIORNI = 6

log_lock = threading.Lock()


def leggi_env():
    valori = {}
    if not ENV_FILE.exists():
        sys.exit(f"ERRORE: manca {ENV_FILE}")
    for riga in ENV_FILE.read_text().splitlines():
        riga = riga.strip()
        if riga and not riga.startswith("#") and "=" in riga:
            k, v = riga.split("=", 1)
            valori[k.strip()] = v.strip()
    return valori["CDSAPI_URL"], valori["CDSAPI_KEY"]


def log(msg, logfile):
    riga = f"[{time.strftime('%Y-%m-%d %H:%M:%S')}] {msg}"
    with log_lock:
        print(riga, flush=True)
        with open(logfile, "a") as f:
            f.write(riga + "\n")


def giorni_del_mese(anno, mese):
    cutoff = date.today() - timedelta(days=RITARDO_GIORNI)
    n = calendar.monthrange(anno, mese)[1]
    if (anno, mese) > (cutoff.year, cutoff.month):
        return []
    if (anno, mese) == (cutoff.year, cutoff.month):
        n = cutoff.day
    return [f"{d:02d}" for d in range(1, n + 1)]


def main():
    ap = argparse.ArgumentParser()
    gruppo = ap.add_mutually_exclusive_group()
    gruppo.add_argument("--mensile", nargs=2, metavar=("DA", "A"),
                        help="range AAAA-MM AAAA-MM, un file per mese")
    gruppo.add_argument("--anni", nargs=2, type=int, metavar=("ANNO1", "ANNO2"),
                        help="range di anni, per i mesi in --mesi")
    ap.add_argument("--mesi", default="5,6", help="mesi (1-12) separati da virgola")
    ap.add_argument("--force", action="store_true")
    ap.add_argument("--workers", type=int, default=3)
    args = ap.parse_args()

    mesi_filtro = [int(m) for m in args.mesi.split(",")]
    if args.mensile:
        (a1, m1), (a2, m2) = [map(int, x.split("-")) for x in args.mensile]
        mesi = []
        anno, mese = a1, m1
        while (anno, mese) <= (a2, m2):
            mesi.append((anno, mese))
            mese += 1
            if mese > 12:
                anno, mese = anno + 1, 1
    elif args.anni:
        mesi = [(a, m) for a in range(args.anni[0], args.anni[1] + 1) for m in mesi_filtro]
    else:
        # 2026 e 2003 per primi (i confronti più richiesti), poi il resto della baseline
        anni = [2026, 2003] + [a for a in range(1991, 2021) if a != 2003]
        mesi = [(a, m) for a in anni for m in (5, 6)]

    url, key = leggi_env()
    import cdsapi

    out_dir = PROGETTO / "input" / "nc" / "italia_orari"
    out_dir.mkdir(parents=True, exist_ok=True)
    logfile = PROGETTO / "input" / "nc" / "download_orari_log.txt"

    log(f"Piano: {len(mesi)} mesi orari, {args.workers} in parallelo", logfile)

    coda = Queue()
    for x in mesi:
        coda.put(x)
    falliti = []

    def worker():
        client = cdsapi.Client(url=url, key=key, quiet=True, progress=False)
        while True:
            try:
                anno, mese = coda.get_nowait()
            except Exception:
                return
            dest = out_dir / f"era5land_t2m_orario_{anno}-{mese:02d}.nc"
            try:
                if dest.exists() and dest.stat().st_size > 1_000_000 and not args.force:
                    log(f"SALTO   {dest.name} (già presente)", logfile)
                    continue
                giorni = giorni_del_mese(anno, mese)
                if not giorni:
                    log(f"SALTO   {dest.name} (mese non ancora disponibile)", logfile)
                    continue
                richiesta = {
                    "variable": ["2m_temperature"],
                    "year": [str(anno)],
                    "month": [f"{mese:02d}"],
                    "day": giorni,
                    "time": [f"{h:02d}:00" for h in range(24)],
                    "data_format": "netcdf",
                    "download_format": "unarchived",
                    "area": AREA_ITALIA,
                }
                ok = False
                for tentativo in (1, 2, 3):
                    try:
                        t0 = time.time()
                        client.retrieve(DATASET, richiesta, str(dest))
                        log(f"OK      {dest.name} ({dest.stat().st_size/1e6:.0f} MB, "
                            f"{time.time()-t0:.0f}s)", logfile)
                        ok = True
                        break
                    except Exception as e:
                        log(f"ERRORE  {dest.name} tentativo {tentativo}/3: "
                            f"{str(e)[:250]}", logfile)
                        if dest.exists():
                            dest.unlink()
                        time.sleep(30 * tentativo)
                if not ok:
                    falliti.append(dest.name)
            finally:
                coda.task_done()

    threads = [threading.Thread(target=worker, daemon=True) for _ in range(args.workers)]
    for th in threads:
        th.start()
    for th in threads:
        th.join()

    log(f"FINE: {len(falliti)} falliti", logfile)
    if falliti:
        log("Falliti: " + ", ".join(falliti), logfile)
        sys.exit(1)


if __name__ == "__main__":
    main()
