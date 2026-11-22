"""
Costo delle case - Pendolari che entrano ogni giorno nelle grandi città.

Fonte: ISTAT, matrice del pendolarismo del 15° Censimento della popolazione
(9 ottobre 2011). Spostamenti quotidiani per studio o lavoro con rientro in
giornata all'alloggio di residenza.
https://www.istat.it/storage/cartografia/matrici_pendolarismo/matrici_pendolarismo_2011.zip

Il file ha due tipi di record:
  S = conteggi completi per origine, destinazione, sesso e motivo
  L = stime (campionarie nei comuni con almeno 20.000 abitanti) con in più
      mezzo, orario di uscita e tempo impiegato
I conteggi in entrata/uscita usano i record S; le quote per mezzo e tempo
usano i record L (solo residenti in famiglia).

La matrice 2021 (uscita nel 2025-2026 sul databrowser ISTAT, dataflow
MATRICE_PEND) è una stima da registri e modelli e non riporta mezzo e tempo:
per questo le quote per mezzo e tempo sono disponibili solo per il 2011.

Output in input/:
  trasporti_pendolari_citta_2011.csv
  trasporti_pendolari_mezzo_tempo_2011.csv
"""

import csv
import io
import tempfile
import urllib.request
import zipfile
from collections import defaultdict
from pathlib import Path

URL = ("https://www.istat.it/storage/cartografia/matrici_pendolarismo/"
       "matrici_pendolarismo_2011.zip")
BASE = Path("/Users/lorenzoruffino/Documents/Progetti/data-viz/Costo delle case")
OUT = BASE / "input"

CITTA = {"015146": "Milano", "058091": "Roma", "001272": "Torino",
         "063049": "Napoli", "037006": "Bologna", "048017": "Firenze"}
MOTIVO = {"1": "studio", "2": "lavoro"}
MEZZO = {"01": "treno", "02": "tram", "03": "metropolitana",
         "04": "autobus urbano, filobus", "05": "corriera, autobus extraurbano",
         "06": "autobus aziendale o scolastico", "07": "auto (conducente)",
         "08": "auto (passeggero)", "09": "moto, motorino, scooter",
         "10": "bicicletta", "11": "altro mezzo", "12": "a piedi"}
TEMPO = {"1": "fino a 15 minuti", "2": "16-30 minuti",
         "3": "31-60 minuti", "4": "oltre 60 minuti"}


def leggi_matrice(percorso_zip):
    """Restituisce conteggi S (entrate, uscite, interni) e stime L per città."""
    s = defaultdict(int)
    l_mezzo = defaultdict(float)
    l_tempo = defaultdict(float)
    with zipfile.ZipFile(percorso_zip) as z:
        nome = next(n for n in z.namelist() if n.endswith(".txt"))
        with z.open(nome) as f:
            for riga in io.TextIOWrapper(f, encoding="latin-1"):
                tipo = riga[0]
                res = riga[4:7] + riga[8:11]
                motivo = riga[15]
                luogo = riga[17]
                dest = riga[19:22] + riga[23:26]
                if tipo == "S":
                    n = int(riga[50:60])
                    for cod, citta in CITTA.items():
                        if luogo == "2" and dest == cod and res != cod:
                            s[(citta, "entrano", motivo)] += n
                        if res == cod and luogo in "23":
                            s[(citta, "escono", motivo)] += n
                        if res == cod and luogo == "1":
                            s[(citta, "interni", motivo)] += n
                elif tipo == "L":
                    stima = float(riga[38:50])
                    mezzo = riga[31:33]
                    tempo = riga[36]
                    for cod, citta in CITTA.items():
                        if luogo == "2" and dest == cod and res != cod:
                            l_mezzo[(citta, motivo, mezzo)] += stima
                            l_tempo[(citta, motivo, tempo)] += stima
    return s, l_mezzo, l_tempo


def main():
    with tempfile.TemporaryDirectory() as tmp:
        percorso = Path(tmp) / "matrice2011.zip"
        urllib.request.urlretrieve(URL, percorso)
        s, l_mezzo, l_tempo = leggi_matrice(percorso)

    # Conteggi per città
    with open(OUT / "trasporti_pendolari_citta_2011.csv", "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["citta", "direzione", "motivo", "pendolari"])
        for (citta, direzione, motivo), n in sorted(s.items()):
            w.writerow([citta, direzione, MOTIVO[motivo], n])
        for citta in CITTA.values():
            for direzione in ("entrano", "escono", "interni"):
                tot = s[(citta, direzione, "1")] + s[(citta, direzione, "2")]
                w.writerow([citta, direzione, "totale", tot])

    # Quote per mezzo e tempo di chi entra in città da un altro comune
    with open(OUT / "trasporti_pendolari_mezzo_tempo_2011.csv", "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["citta", "motivo", "variabile", "modalita",
                    "stima_pendolari", "quota_perc"])
        for citta in CITTA.values():
            for motivo in ("1", "2"):
                tot = sum(v for (c, m, _), v in l_tempo.items()
                          if c == citta and m == motivo)
                for cod, etichetta in MEZZO.items():
                    v = l_mezzo.get((citta, motivo, cod), 0.0)
                    w.writerow([citta, MOTIVO[motivo], "mezzo", etichetta,
                                round(v), round(100 * v / tot, 1)])
                for cod, etichetta in TEMPO.items():
                    v = l_tempo.get((citta, motivo, cod), 0.0)
                    w.writerow([citta, MOTIVO[motivo], "tempo", etichetta,
                                round(v), round(100 * v / tot, 1)])


if __name__ == "__main__":
    main()
