#!/usr/bin/env python3
"""
Scarica da Copernicus CDS le statistiche giornaliere ERA5-Land di temperatura a 2 metri
(minima, media e massima giornaliera) su griglia 0,1° (~9 km).

Dataset: derived-era5-land-daily-statistics (dal 1950, aggiornato con ~5-6 giorni di ritardo).
Credenziali: lette dal file .env nella root del repo data-viz (CDSAPI_URL, CDSAPI_KEY).

I file finiscono in "Temperature Copernicus/input/nc/<area>/", sempre UN FILE
PER MESE: era5land_t2m_<stat>_<AAAA>-<MM>.nc. (Niente richieste multi-anno:
sono "economiche" ma il servizio CDS le lascia in coda per ore.)

I file già presenti vengono saltati: rilanciare lo stesso comando riprende da dove
si era rimasti. Con --force si riscarica (es. per rigirare il 2026 quando i dati
ERA5T provvisori diventano definitivi).

Uso tipico:
  # piano standard Italia: mag+giu 2026, baseline mag+giu 1961-2019 a blocchi,
  # poi tutti i mesi da gennaio 2020 all'ultimo mese disponibile
  python3 01_scarica_era5land.py --preset completo

  # un range di mesi, un file per mese (estremi inclusi)
  python3 01_scarica_era5land.py --mensile 2020-01 2026-06
  python3 01_scarica_era5land.py --mensile 2026-06 2026-06 --force   # rigira giugno 2026

  # baseline a blocchi di 5 anni, solo per i mesi indicati
  python3 01_scarica_era5land.py --baseline 1961 2019 --mesi 5,6

  # Europa per la mappa continentale (solo media giornaliera di giugno)
  python3 01_scarica_era5land.py --preset europa-giugno
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
PROGETTO = SCRIPT_DIR.parent          # Temperature Copernicus/
REPO = PROGETTO.parent                # data-viz/
ENV_FILE = REPO / ".env"

DATASET = "derived-era5-land-daily-statistics"

# Bounding box [Nord, Ovest, Sud, Est]
AREE = {
    "italia": [47.2, 6.6, 35.4, 18.6],   # include Alpi, Lampedusa e Otranto
    "europa": [72, -25, 34, 35],         # include Islanda e Cipro
}

STAT_CDS = {
    "mean": "daily_mean",
    "min": "daily_minimum",
    "max": "daily_maximum",
}

# ERA5T-Land arriva con ~5 giorni di ritardo: margine prudente di 6
RITARDO_GIORNI = 6

log_lock = threading.Lock()


def leggi_env():
    """Legge CDSAPI_URL e CDSAPI_KEY dal .env della root del repo."""
    valori = {}
    if not ENV_FILE.exists():
        sys.exit(f"ERRORE: manca {ENV_FILE} con CDSAPI_URL e CDSAPI_KEY")
    for riga in ENV_FILE.read_text().splitlines():
        riga = riga.strip()
        if riga and not riga.startswith("#") and "=" in riga:
            k, v = riga.split("=", 1)
            valori[k.strip()] = v.strip()
    if "CDSAPI_URL" not in valori or "CDSAPI_KEY" not in valori:
        sys.exit(f"ERRORE: {ENV_FILE} deve contenere CDSAPI_URL e CDSAPI_KEY")
    return valori["CDSAPI_URL"], valori["CDSAPI_KEY"]


def log(msg, logfile):
    riga = f"[{time.strftime('%Y-%m-%d %H:%M:%S')}] {msg}"
    with log_lock:
        print(riga, flush=True)
        with open(logfile, "a") as f:
            f.write(riga + "\n")


def ultimo_giorno_disponibile():
    return date.today() - timedelta(days=RITARDO_GIORNI)


def giorni_del_mese(anno, mese):
    """Lista dei giorni scaricabili per (anno, mese); vuota se il mese non è ancora disponibile."""
    cutoff = ultimo_giorno_disponibile()
    n = calendar.monthrange(anno, mese)[1]
    if (anno, mese) > (cutoff.year, cutoff.month):
        return []
    if (anno, mese) == (cutoff.year, cutoff.month):
        n = cutoff.day
    return [f"{d:02d}" for d in range(1, n + 1)]


def task_mensile(area, stat, anno, mese):
    return {
        "area": area,
        "stat": stat,
        "anni": [anno],
        "mese": mese,
        "target": f"era5land_t2m_{stat}_{anno}-{mese:02d}.nc",
    }


def task_blocco(area, stat, anni, mese):
    return {
        "area": area,
        "stat": stat,
        "anni": anni,
        "mese": mese,
        "target": f"era5land_t2m_{stat}_m{mese:02d}_{anni[0]}-{anni[-1]}.nc",
    }


def costruisci_richiesta(task):
    anni = task["anni"]
    mese = task["mese"]
    if len(anni) == 1:
        giorni = giorni_del_mese(anni[0], mese)
    else:
        giorni = [f"{d:02d}" for d in range(1, calendar.monthrange(anni[0], mese)[1] + 1)]
    if not giorni:
        return None
    return {
        "variable": ["2m_temperature"],
        "year": [str(a) for a in anni],
        "month": [f"{mese:02d}"],
        "day": giorni,
        "daily_statistic": STAT_CDS[task["stat"]],
        "time_zone": "utc+01:00",
        "frequency": "1_hourly",
        "area": AREE[task["area"]],
    }


def scarica(client_factory, task, out_dir, logfile, force):
    dest = out_dir / task["target"]
    if dest.exists() and dest.stat().st_size > 10_000 and not force:
        log(f"SALTO   {task['target']} (già presente)", logfile)
        return "saltato"
    richiesta = costruisci_richiesta(task)
    if richiesta is None:
        log(f"SALTO   {task['target']} (mese non ancora disponibile)", logfile)
        return "non_disponibile"

    for tentativo in (1, 2, 3):
        try:
            t0 = time.time()
            client = client_factory()
            client.retrieve(DATASET, richiesta, str(dest))
            mb = dest.stat().st_size / 1e6
            log(f"OK      {task['target']} ({mb:.1f} MB, {time.time()-t0:.0f}s)", logfile)
            return "ok"
        except Exception as e:
            msg = str(e).replace("\n", " ")[:300]
            log(f"ERRORE  {task['target']} tentativo {tentativo}/3: {msg}", logfile)
            if dest.exists():
                dest.unlink()
            time.sleep(30 * tentativo)
    return "fallito"


def piano_preset_completo(oggi_cutoff):
    """Tutto a file mensili (le richieste multi-anno restano ferme in coda sul CDS).

    Ordine di priorità: 2026 mag-giu, poi GIUGNO 1961-2025 (mappa e analisi),
    poi MAGGIO 1961-2025, poi tutti gli altri mesi da gennaio 2020 in avanti.
    """
    tasks = []
    fine = (oggi_cutoff.year, oggi_cutoff.month)

    # 1) i mesi dell'analisi corrente
    for mese in (5, 6):
        if (2026, mese) <= fine:
            for stat in STAT_CDS:
                tasks.append(task_mensile("italia", stat, 2026, mese))

    # 2) giugno 1961-2025, poi maggio 1961-2025 (baseline 61-90 e 91-20 + serie)
    for mese in (6, 5):
        for anno in range(2025, 1960, -1):
            for stat in STAT_CDS:
                tasks.append(task_mensile("italia", stat, anno, mese))

    # 3) tutti gli altri mesi da gennaio 2020 all'ultimo disponibile
    anno, mese = 2020, 1
    while (anno, mese) <= fine:
        if mese not in (5, 6):
            for stat in STAT_CDS:
                tasks.append(task_mensile("italia", stat, anno, mese))
        mese += 1
        if mese > 12:
            anno, mese = anno + 1, 1
    return tasks


def piano_preset_europa_giugno(oggi_cutoff):
    """Solo media giornaliera di giugno, file mensili 1991-2026."""
    tasks = []
    fine = (oggi_cutoff.year, oggi_cutoff.month)
    if (2026, 6) <= fine:
        tasks.append(task_mensile("europa", "mean", 2026, 6))
    for anno in range(2025, 1990, -1):
        tasks.append(task_mensile("europa", "mean", anno, 6))
    return tasks


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    gruppo = ap.add_mutually_exclusive_group(required=True)
    gruppo.add_argument("--preset", choices=["completo", "europa-giugno"])
    gruppo.add_argument("--mensile", nargs=2, metavar=("DA", "A"),
                        help="range di mesi AAAA-MM AAAA-MM, un file per mese")
    gruppo.add_argument("--baseline", nargs=2, type=int, metavar=("ANNO1", "ANNO2"),
                        help="un file al mese per gli anni indicati, mesi in --mesi")
    ap.add_argument("--mesi", default="5,6", help="mesi (1-12) separati da virgola, default 5,6")
    ap.add_argument("--stats", default="mean,min,max", help="statistiche: mean,min,max")
    ap.add_argument("--area", default="italia", choices=list(AREE))
    ap.add_argument("--force", action="store_true", help="riscarica anche i file già presenti")
    ap.add_argument("--workers", type=int, default=3)
    args = ap.parse_args()

    url, key = leggi_env()
    import cdsapi

    def client_factory():
        # retry_max basso: se una richiesta resta appesa conviene fallire e
        # riprovarla da zero piuttosto che pollarla per ore
        return cdsapi.Client(url=url, key=key, quiet=True, progress=False,
                             retry_max=20)

    cutoff = ultimo_giorno_disponibile()
    stats = [s.strip() for s in args.stats.split(",") if s.strip() in STAT_CDS]
    mesi = [int(m) for m in args.mesi.split(",")]

    if args.preset == "completo":
        tasks = piano_preset_completo(cutoff)
    elif args.preset == "europa-giugno":
        tasks = piano_preset_europa_giugno(cutoff)
    elif args.mensile:
        (a1, m1), (a2, m2) = [map(int, x.split("-")) for x in args.mensile]
        tasks = []
        anno, mese = a1, m1
        while (anno, mese) <= (a2, m2):
            for stat in stats:
                tasks.append(task_mensile(args.area, stat, anno, mese))
            mese += 1
            if mese > 12:
                anno, mese = anno + 1, 1
    else:
        a1, a2 = args.baseline
        tasks = []
        for mese in mesi:
            for anno in range(a2, a1 - 1, -1):
                for stat in stats:
                    tasks.append(task_mensile(args.area, stat, anno, mese))

    out_dirs = {t["area"] for t in tasks}
    for a in out_dirs:
        (PROGETTO / "input" / "nc" / a).mkdir(parents=True, exist_ok=True)
    logfile = PROGETTO / "input" / "nc" / "download_log.txt"

    log(f"Piano: {len(tasks)} richieste, {args.workers} in parallelo, cutoff dati {cutoff}", logfile)

    coda = Queue()
    for t in tasks:
        coda.put(t)
    esiti = {"ok": 0, "saltato": 0, "fallito": 0, "non_disponibile": 0}
    falliti = []
    esiti_lock = threading.Lock()

    def worker():
        while True:
            try:
                t = coda.get_nowait()
            except Exception:
                return
            out_dir = PROGETTO / "input" / "nc" / t["area"]
            esito = scarica(client_factory, t, out_dir, logfile, args.force)
            with esiti_lock:
                esiti[esito] += 1
                if esito == "fallito":
                    falliti.append(t["target"])
            coda.task_done()

    threads = [threading.Thread(target=worker, daemon=True) for _ in range(args.workers)]
    for th in threads:
        th.start()
    for th in threads:
        th.join()

    log(f"FINE: {esiti['ok']} scaricati, {esiti['saltato']} saltati, "
        f"{esiti['non_disponibile']} non disponibili, {esiti['fallito']} falliti", logfile)
    if falliti:
        log("Falliti: " + ", ".join(falliti), logfile)
        sys.exit(1)


if __name__ == "__main__":
    main()
