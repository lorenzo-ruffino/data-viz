#!/bin/sh
# Aggiornamento completo per l'articolo sull'estate 2026.
#
#   1. riscarica agosto 2026 (orari Italia + medie giornaliere Europa) con --force,
#      così entrano gli ultimi giorni pubblicati dal CDS (ritardo ERA5T ~5-6 giorni)
#   2. rilancia l'elaborazione base (04, 07) e tutti gli script "estate" (20-29)
#
# Uso:  sh script/00_aggiorna_estate.sh            # scarica + elabora
#       sh script/00_aggiorna_estate.sh --no-download   # solo elaborazione
# Log completo in output/log_aggiorna_estate.txt
set -e
cd "$(dirname "$0")"
LOG="../output/log_aggiorna_estate.txt"
: > "$LOG"
run() { echo "\n=== $(date '+%H:%M:%S') $* ===" | tee -a "$LOG"; "$@" 2>&1 | tee -a "$LOG"; }

if [ "$1" != "--no-download" ]; then
  run python3 02_scarica_orari.py --mensile 2026-08 2026-08 --force --workers 1
  run python3 01_scarica_era5land.py --mensile 2026-08 2026-08 --area europa --stats mean --force --workers 1
  # eventuali file della serie storica ancora mancanti (riprende da dove si era fermato)
  run python3 02_scarica_orari.py --anni 1961 2019 --mesi 8 --workers 3
  run python3 01_scarica_era5land.py --baseline 1991 2025 --mesi 7,8 --area europa --stats mean --workers 2
fi

echo "\nAgosto 2026 orario copre:" | tee -a "$LOG"
Rscript -e 'suppressMessages(library(ncdf4)); nc <- nc_open("../input/nc/italia_orari/era5land_t2m_orario_2026-08.nc"); tm <- ncvar_get(nc,"valid_time"); un <- ncatt_get(nc,"valid_time","units")$value; t <- as.POSIXct(sub("seconds since ","",un), tz="UTC")+tm; cat(" ", format(min(t)), "->", format(max(t)), "\n")' | tee -a "$LOG"

for s in 04_elabora_dati.R 07_giorno_notte.R \
         20a_analisi_estate.R 20b_soglie_estate.R \
         21_grafico_estate_anni.R 22_grafico_giornaliero_maggio_agosto.R \
         23_mappa_estate_italia.R 24_grafico_notti_tropicali_estate.R \
         25_mappa_estate_europa.R 26_analisi_europa_paesi_estate.R \
         27_grafico_ondate_calore_estate.R 28_export_dati_wide_estate.R \
         29_export_interattivi_estate.R; do
  if [ -f "$s" ]; then run Rscript "$s"; else echo "MANCA $s" | tee -a "$LOG"; fi
done
# compressione lossless dei PNG dell'articolo (oxipng, installato con brew)
if command -v oxipng >/dev/null; then
  echo "\n=== compressione PNG (oxipng) ===" | tee -a "$LOG"
  for f in grafico_estate_annuale grafico_giornaliero_maggio_agosto grafico_notti_tropicali_estate \
           grafico_ondate_calore_estate mappa_estate_2026_italia mappa_estate_2026_europa; do
    [ -f "../output/$f.png" ] && oxipng -o max --strip safe -q "../output/$f.png" && echo "  $f.png $(stat -f%z "../output/$f.png") byte" | tee -a "$LOG"
  done
fi
echo "\nFINITO $(date '+%H:%M:%S'). Findings: output/findings_estate_2026.md" | tee -a "$LOG"
