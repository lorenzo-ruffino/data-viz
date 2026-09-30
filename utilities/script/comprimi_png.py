#!/usr/bin/env python3
"""Comprime i PNG dei grafici riducendoli a palette indicizzata.

I grafici di ggplot hanno poche tinte piatte più le sfumature dell'antialiasing
del testo: una palette a 256 colori li rappresenta senza perdita visibile e
taglia il peso del file di due terzi o più, utile per le immagini che finiscono
nella newsletter (Gmail tronca le email pesanti).

Uso:
    python3 comprimi_png.py <file.png | cartella> [...]
    python3 comprimi_png.py --dry-run <cartella>      # solo report, non scrive

Sostituisce il file solo se il risultato è più leggero dell'originale.
"""

import sys
from pathlib import Path

from PIL import Image

COLORI = 256


def comprimi(path: Path, dry_run: bool = False) -> tuple[int, int]:
    """Ritorna (peso_prima, peso_dopo) in byte."""
    prima = path.stat().st_size

    with Image.open(path) as im:
        im = im.convert("RGBA") if im.mode not in ("RGB", "RGBA", "P") else im
        if im.mode == "RGBA":
            # lo sfondo dei grafici è bianco pieno: appiattire l'alpha evita
            # che la palette sprechi slot per la trasparenza
            fondo = Image.new("RGB", im.size, (255, 255, 255))
            fondo.paste(im, mask=im.split()[3])
            im = fondo
        else:
            im = im.convert("RGB")

        unici = len(im.getcolors(maxcolors=1 << 24) or [])
        pal = im.quantize(colors=COLORI, method=Image.Quantize.MEDIANCUT,
                          dither=Image.Dither.NONE)

        tmp = path.with_suffix(".png.tmp")
        pal.save(tmp, format="PNG", optimize=True)

    dopo = tmp.stat().st_size
    if dry_run or dopo >= prima:
        tmp.unlink()
        dopo = prima if not dry_run else dopo
    else:
        tmp.replace(path)

    print(f"  {path.name:42s} {prima/1024:6.0f} KB → {dopo/1024:6.0f} KB "
          f"({(1 - dopo/prima)*100:4.0f}% in meno, {unici} colori unici)")
    return prima, dopo


def main(argv: list[str]) -> int:
    dry_run = "--dry-run" in argv
    target = [a for a in argv if not a.startswith("--")]
    if not target:
        print(__doc__)
        return 1

    files: list[Path] = []
    for t in target:
        p = Path(t)
        if p.is_dir():
            files.extend(sorted(p.glob("*.png")))
        elif p.is_file():
            files.append(p)
        else:
            print(f"non trovato: {t}", file=sys.stderr)
            return 1

    if not files:
        print("nessun PNG da comprimere")
        return 0

    tot_prima = tot_dopo = 0
    for f in files:
        a, b = comprimi(f, dry_run)
        tot_prima += a
        tot_dopo += b

    print(f"\n  {'TOTALE':42s} {tot_prima/1024:6.0f} KB → {tot_dopo/1024:6.0f} KB "
          f"({(1 - tot_dopo/tot_prima)*100:4.0f}% in meno)")
    if dry_run:
        print("  (dry-run: nessun file è stato sovrascritto)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
