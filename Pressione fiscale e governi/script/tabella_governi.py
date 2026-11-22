"""Tabella dei governi (PNG) dalla serie Istat: Chrome headless + ritaglio."""
import os, subprocess, html
import pandas as pd
from PIL import Image, ImageChops
B = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..")
g = pd.read_csv(f"{B}/input/governi_pressione.csv")
nomi = {"Prodi I / D'Alema / Amato": "Prodi I, D’Alema e Amato", "Berlusconi II-III": "Berlusconi II e III",
        "Prodi II": "Prodi II", "Berlusconi IV": "Berlusconi IV", "Monti": "Monti", "Letta / Renzi": "Letta e Renzi",
        "Gentiloni": "Gentiloni", "Conte I-II": "Conte I e II", "Draghi": "Draghi", "Meloni": "Meloni"}
it = lambda v, d=1: f"{v:.{d}f}".replace(".", ",")
sg = lambda v: ("+" if v > 0 else "−") + it(abs(v))
rows = []
for _, r in g.iterrows():
    cls = " class='hl'" if r.governo == "Meloni" else ""
    a, b = r.anni.split("-")
    rows.append(f"<tr{cls}><td>{html.escape(nomi[r.governo])}</td><td>{r.anni.replace('-', '–')}</td>"
                f"<td>{it(r.inizio)}%</td><td>{it(r.fine)}%</td><td class='v'>{sg(r.delta)}</td><td>{it(r.media)}%</td></tr>")
CSS = """@import url('https://fonts.googleapis.com/css2?family=Source+Sans+3:wght@400;600;700&display=swap');
body{margin:0;background:#fff;font-family:'Source Sans 3',Arial,sans-serif;color:#111827}
.card{display:inline-block;padding:28px 32px 22px 32px}
h1{font-size:25px;margin:0 0 4px 0;font-weight:700}.sub{font-size:16px;color:#4b5563;margin:0 0 16px 0}
table{border-collapse:collapse;font-size:16px}
th{background:#1e3a8a;color:#fff;font-weight:600;padding:9px 14px;text-align:center;line-height:1.2}
th:first-child,td:first-child{text-align:left}
td{padding:7px 14px;border-bottom:1px solid #e5e7eb;text-align:center;white-space:nowrap}
td.v{font-weight:700}tr.hl td{background:#fde3e5;font-weight:700}
caption{caption-side:bottom;text-align:left}.cred{margin-top:14px;font-size:14px;font-weight:600;color:#374151}
.note{font-size:13px;color:#6b7280;margin-top:3px}"""
body = ("<div class='card'><h1>La pressione fiscale con i diversi governi</h1>"
        "<div class='sub'>Imposte e contributi sociali in percentuale del PIL, Italia, 1996-2025</div><table>"
        "<tr><th>Governo</th><th>Anni<br>attribuiti</th><th>Anno prima<br>del governo</th><th>Ultimo anno<br>del governo</th>"
        "<th>Variazione<br>(punti)</th><th>Media<br>del periodo</th></tr>" + "".join(rows) +
        "<caption><div class='cred'>Elaborazione di Lorenzo Ruffino su dati Istat (edizione ottobre 2026).</div>"
        "<div class='note'>Nota: a ogni governo sono attribuiti gli anni a partire dal primo bilancio che ha scritto.</div></caption></table></div>")
hf, pf = f"{B}/output/04_tabella_governi.html", f"{B}/output/04_tabella_governi.png"
open(hf, "w").write(f"<!doctype html><html><head><meta charset='utf-8'><style>{CSS}</style></head><body>{body}</body></html>")
subprocess.run(["/Applications/Google Chrome.app/Contents/MacOS/Google Chrome", "--headless=new", "--disable-gpu",
                "--hide-scrollbars", "--force-device-scale-factor=2", "--window-size=1200,1000", "--virtual-time-budget=4000",
                f"--screenshot={pf}", f"file://{hf}"], check=True, capture_output=True)
im = Image.open(pf).convert("RGB"); box = ImageChops.difference(im, Image.new("RGB", im.size, "white")).getbbox()
im.crop((max(box[0]-40, 0), max(box[1]-40, 0), min(box[2]+40, im.width), min(box[3]+40, im.height))).save(pf)
os.remove(hf); print(pf)
