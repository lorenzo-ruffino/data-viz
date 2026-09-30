#!/bin/sh
# Orchestratore notturno (5-6 settembre 2026): lavora da solo finché non ci sono
# tutti i dati per l'articolo sull'estate.
#   1. aspetta le medie mensili Europa (01b, lug+ago 1991-2025) e scarica anche giugno
#   2. ferma il download lento delle medie giornaliere Europa (coda CDS di ore)
#   3. ogni 10 minuti controlla se il CDS ha pubblicato il 31 agosto 2026;
#      appena c'è riscarica agosto 2026 (orari Italia + media giornaliera Europa)
#      con --force, verifica che il 31 ci sia davvero, e rilancia tutta la pipeline
# Log: output/log_notte.txt. A fine lavoro crea output/.notte_done
set -u
cd "$(dirname "$0")"
LOG="../output/log_notte.txt"
say() { echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*" | tee -a "$LOG"; }
rm -f ../output/.notte_done
say "avvio orchestratore notturno"

# ---- 1. medie mensili Europa ------------------------------------------------
while pgrep -f "01b_scarica_mensili_europa.py --anni 1991 2025 --mesi 7,8" >/dev/null; do sleep 30; done
say "download mensili lug/ago terminato: $(ls ../input/nc/europa_mensili 2>/dev/null | tr '\n' ' ')"
python3 01b_scarica_mensili_europa.py --anni 1991 2025 --mesi 6 >> "$LOG" 2>&1
for m in 06 07 08; do
  [ -s "../input/nc/europa_mensili/era5land_t2m_monthly_m${m}_1991-2025.nc" ] || say "ATTENZIONE: manca il mensile Europa m$m, riprovo"
  [ -s "../input/nc/europa_mensili/era5land_t2m_monthly_m${m}_1991-2025.nc" ] || python3 01b_scarica_mensili_europa.py --anni 1991 2025 --mesi ${m#0} >> "$LOG" 2>&1
done
say "mensili Europa presenti: $(ls ../input/nc/europa_mensili | tr '\n' ' ')"

# ---- 2. stop del download giornaliero lento ---------------------------------
pkill -f "01_scarica_era5land.py --baseline 1991 2025 --mesi 8,7" && say "fermato il download giornaliero lento Europa (coda CDS)"

# ---- 3. attesa del 31 agosto -------------------------------------------------
copertura() {
  curl -sS --max-time 40 "https://cds.climate.copernicus.eu/api/catalogue/v1/collections/reanalysis-era5-land" \
    | python3 -c "import sys,json; print(json.load(sys.stdin)['extent']['temporal']['interval'][0][1][:10])" 2>/dev/null
}
while :; do
  c="$(copertura)"
  say "copertura CDS ERA5-Land: ${c:-n.d.}"
  if [ -n "$c" ] && [ "$c" \> "2026-08-30" ]; then break; fi
  sleep 600
done
say "il 31 agosto e' disponibile: riscarico agosto 2026"

verifica31() {
  Rscript -e 'suppressMessages(library(ncdf4)); nc <- nc_open("../input/nc/italia_orari/era5land_t2m_orario_2026-08.nc"); tm <- ncvar_get(nc,"valid_time"); un <- ncatt_get(nc,"valid_time","units")$value; t <- as.POSIXct(sub("seconds since ","",un), tz="UTC")+tm; cat(format(max(t)), "\n")' 2>/dev/null
}
tent=0
while :; do
  tent=$((tent+1))
  python3 02_scarica_orari.py --mensile 2026-08 2026-08 --force --workers 1 >> "$LOG" 2>&1
  u="$(verifica31)"; say "agosto 2026 orari Italia: ultimo istante $u (tentativo $tent)"
  case "$u" in 2026-08-31*) break;; esac
  [ $tent -ge 6 ] && { say "ERRORE: dopo 6 tentativi il 31 agosto non c'e' nel file orario"; break; }
  sleep 900
done
python3 01_scarica_era5land.py --mensile 2026-08 2026-08 --area europa --stats mean --force --workers 1 >> "$LOG" 2>&1
say "agosto 2026 Europa giornaliero riscaricato"

# ---- 4. pipeline completa ---------------------------------------------------
say "rilancio la pipeline (00_aggiorna_estate.sh --no-download)"
sh 00_aggiorna_estate.sh --no-download >> "$LOG" 2>&1
say "FINITO: pipeline rigenerata"
touch ../output/.notte_done
