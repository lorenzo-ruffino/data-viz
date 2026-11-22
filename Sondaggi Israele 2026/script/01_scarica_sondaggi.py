#!/usr/bin/env python3
# ============================================================================
# Sondaggi per le elezioni della Knesset del 27 ottobre 2026 dalla voce inglese
# di Wikipedia "Opinion polling for the 2026 Israeli legislative election"
# (sezione "2026": tutti i sondaggi da gennaio all'ultimo giorno pubblicabile,
# venerdi' 23 ottobre).
#
# I sondaggi israeliani si pubblicano in SEGGI (su 120), non in percentuali.
# Le liste sotto la soglia del 3,25 per cento sono riportate con la loro
# percentuale fra parentesi, es. "(2,4%)", o con i seggi che avrebbero senza
# soglia, es. "(3)": per loro si salvano 0 seggi e la percentuale (i seggi
# teorici divisi per 1,2).
#
# La sezione e' spezzata in sette tabelle, una per ogni fase della campagna
# (fusioni e ritiri di liste cambiano le colonne). Le intestazioni hanno fino
# a quattro righe con rowspan/colspan: si ricostruisce la griglia e per ogni
# colonna si prende l'etichetta piu' in basso. Nei dati, una cella con colspan
# e' il valore di una lista congiunta (es. RZP-Zehut) e prende il nome delle
# colonne che copre unite da "+".
#
# Un sondaggio puo' occupare piu' righe (rowspan), una per scenario: si tiene
# la prima, che sulla voce e' lo scenario principale.
#
# Output: input/sondaggi_grezzi.csv (formato lungo: una riga per sondaggio
#         e lista)
# ============================================================================

import csv
import os
import re
import urllib.request
from datetime import date, timedelta

BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
INPUT = os.path.join(BASE, "input")
GREZZO = os.path.join(INPUT, "il2026_en.wiki")
URL = ("https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_"
       "2026_Israeli_legislative_election&action=raw")

MESI = {m: i + 1 for i, m in enumerate(
    ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
     "Nov", "Dec"])}
FISSE = ["data", "istituto", "editore", "n"]   # prime quattro colonne


def scarica():
    if not os.path.exists(GREZZO):
        req = urllib.request.Request(URL, headers={"User-Agent": "data-viz"})
        with urllib.request.urlopen(req) as r, open(GREZZO, "wb") as f:
            f.write(r.read())
    with open(GREZZO, encoding="utf-8") as f:
        return f.read()


def togli_template(s):
    out, depth, i = [], 0, 0
    while i < len(s):
        if s.startswith("{{", i):
            depth += 1; i += 2
        elif s.startswith("}}", i) and depth:
            depth -= 1; i += 2
        else:
            if not depth:
                out.append(s[i])
            i += 1
    return "".join(out)


def pulisci(cella):
    """Toglie note e template, tenendo date, percentuali e N/A."""
    c = re.sub(r"<ref[^>]*/>", "", cella)
    c = re.sub(r"<ref.*?</ref>", "", c, flags=re.S)
    c = re.sub(r"\{\{\s*Opdrts\s*\|([^}]*)\}\}",
               lambda m: "OPD:" + m.group(1).replace("|", "/"), c, flags=re.I)
    c = re.sub(r"\{\{\s*small\s*\|([^{}]*)\}\}", r"\1", c, flags=re.I)
    c = re.sub(r"\{\{\s*N/?A\s*\}\}", "NA", c, flags=re.I)
    c = re.sub(r"\{\{\s*party colou?r\s*\|[^}]*\}\}", "", c, flags=re.I)
    c = re.sub(r"\[\[[^\]|]*\|([^\]]*)\]\]", r"\1", c)
    c = re.sub(r"\[\[([^\]]*)\]\]", r"\1", c)
    c = togli_template(c)
    return c


def separa(cella):
    """-> (rowspan, colspan, valore)."""
    c = pulisci(cella)
    rs = re.search(r'rowspan\s*=\s*"?(\d+)', c)
    cs = re.search(r'colspan\s*=\s*"?(\d+)', c)
    if "|" in c:
        attr, val = c.split("|", 1)
        if attr.strip() and not re.search(r"(rowspan|colspan|style|class|align|data-sort-value)\s*=", attr):
            val = c          # il "|" non separava attributi
    else:
        val = re.sub(r'(rowspan|colspan)\s*=\s*"?\d+"?', "", c)
    val = re.sub(r"<br\s*/?>", " ", val)
    val = re.sub(r"<[^>]+>", "", val).replace("'''", "").replace("''", "")
    return (int(rs.group(1)) if rs else 1, int(cs.group(1)) if cs else 1,
            re.sub(r"\s+", " ", val).strip())


def spezza(riga, sep):
    """Divide sulle "||" o "!!" fuori da template e link."""
    parti, depth, i, ini = [], 0, 0, 0
    while i < len(riga):
        if riga.startswith("{{", i) or riga.startswith("[[", i):
            depth += 1; i += 2
        elif (riga.startswith("}}", i) or riga.startswith("]]", i)) and depth:
            depth -= 1; i += 2
        elif not depth and riga.startswith(sep, i):
            parti.append(riga[ini:i]); i += 2; ini = i
        else:
            i += 1
    parti.append(riga[ini:])
    return parti


def blocchi(tab):
    """Righe della tabella come liste di celle (header e dati)."""
    out = []
    for bl in re.split(r"\n\|-[^\n]*", tab):
        celle, tipo = [], None
        # riunisce le righe spezzate dentro una nota o un template
        logiche, buf = [], ""
        for riga in bl.split("\n"):
            buf = riga if not buf else buf + " " + riga
            if (buf.count("{{") <= buf.count("}}") and
                    len(re.findall(r"<ref[^>/]*>", buf)) <= buf.count("</ref>")):
                logiche.append(buf); buf = ""
        if buf:
            logiche.append(buf)
        for riga in logiche:
            if riga.startswith("{|") or riga.startswith("|}") or riga.startswith("|+"):
                continue
            if riga.startswith("!"):
                tipo = "h"; celle.extend(spezza(riga[1:], "!!"))
            elif riga.startswith("|"):
                tipo = tipo or "d"; celle.extend(spezza(riga[1:], "||"))
            elif celle:
                celle[-1] += "\n" + riga      # cella su piu' righe
        if celle:
            out.append((tipo, celle))
    return out


def intestazione(righe_h):
    """Etichetta di ogni colonna foglia dalla griglia delle intestazioni."""
    griglia = {}                       # (riga, col) -> (etichetta, id cella, pos)
    for r, celle in enumerate(righe_h):
        col, idc = 0, 0
        for cella in celle:
            rs, cs, val = separa(cella)
            while (r, col) in griglia:
                col += 1
            idc += 1
            for dr in range(rs):
                for dc in range(cs):
                    griglia[(r + dr, col + dc)] = (val, (r, idc), dc, cs)
            col += cs
    ncol = max(c for _, c in griglia) + 1
    nomi = []
    for c in range(ncol):
        nome = None
        for r in range(len(righe_h)):
            v = griglia.get((r, c))
            if v and v[0]:
                nome = v[0] if v[3] == 1 else f"{v[0]}#{v[2] + 1}"
        nomi.append(nome or f"col{c}")
    return nomi


def leggi_data(s):
    """'OPD:22/23/Oct/2026', 'OPD:/23/Oct/2026', 'OPD:/7/5/2026'."""
    p = s.split("OPD:", 1)[1].split("/")
    g1, g2, mese, anno = (p + ["", "", "", ""])[:4]
    mese = mese.strip()
    m = int(mese) if mese.isdigit() else MESI[mese[:3]]
    fine = date(int(anno), m, int(g2))
    if g1.strip():
        g1 = int(g1)
        if g1 <= fine.day:
            ini = date(int(anno), m, g1)
        else:                          # campo a cavallo di due mesi
            ini = date(int(anno), m - 1, g1) if m > 1 else date(int(anno) - 1, 12, g1)
    else:
        ini = fine
    return ini, fine


def numero(s):
    m = re.match(r"^\(?\s*(\d+(?:\.\d+)?)\s*%?\)?", s.replace(",", ""))
    return float(m.group(1)) if m else None


def main():
    testo = scarica()
    a = testo.index("=== 2026 ===")
    b = testo.index("=== 2025 ===")
    sezione = testo[a:b]

    out, visti = [], set()
    for nt, tab in enumerate(re.findall(r"\{\|.*?\n\|\}", sezione, flags=re.S)):
        bl = blocchi(tab)
        righe_h = [c for t, c in bl if t == "h"]
        nomi = intestazione(righe_h)
        pendenti = {}                  # colonna -> [rs restanti, valore, nome]
        for tipo, celle in bl:
            if tipo == "h":
                continue
            prime = [separa(c) for c in celle]
            if any(cs >= 15 for _, cs, _ in prime):
                continue               # riga-evento
            valori, col, k = {}, 0, 0
            while col < len(nomi):
                if col in pendenti:
                    rest, v, nome = pendenti[col]
                    valori[col] = (v, nome, True)
                    pendenti[col][0] -= 1
                    if pendenti[col][0] == 0:
                        del pendenti[col]
                    col += 1
                    continue
                if k >= len(prime):
                    break
                rs, cs, v = prime[k]; k += 1
                nome = "+".join(dict.fromkeys(
                    re.sub(r"#\d+$", "", n) if cs > 1 else n
                    for n in nomi[col:col + cs]))
                for dc in range(cs):
                    valori[col + dc] = (v if dc == 0 else None, nome, False)
                    if rs > 1:
                        pendenti[col + dc] = [rs - 1, v if dc == 0 else None, nome]
                col += cs
            if 0 not in valori or "OPD:" not in (valori[0][0] or ""):
                continue
            # le righe successive di un sondaggio a scenari ereditano la data
            if valori[0][2]:
                continue
            ini, fin = leggi_data(valori[0][0])
            if fin.year < 2026:        # refusi sull'anno nella voce
                ini, fin = ini.replace(year=2026), fin.replace(year=2026)
            istituto, editore = valori[1][0], valori[2][0]
            n = numero(valori[3][0] or "")
            chiave = (istituto, editore, fin, n)
            if chiave in visti:
                continue
            visti.add(chiave)
            for c in range(4, len(nomi)):
                if c not in valori or valori[c][0] is None:
                    continue
                v, nome, _ = valori[c]
                if nome in ("Lead",):
                    continue
                # sotto soglia: "(2.4%)", o "(3)" = seggi teorici senza soglia
                sotto = v.startswith("(")
                x = numero(v)
                if sotto and "%" not in v and x is not None:
                    x = round(x / 1.2, 1)      # seggi teorici -> percentuale
                out.append({
                    "tabella": nt + 1, "istituto": istituto, "editore": editore,
                    "data_inizio": ini, "data_fine": fin,
                    "data_media": ini + timedelta(days=(fin - ini).days // 2),
                    "n": int(n) if n else "", "lista": nome, "testo": v,
                    "seggi": 0 if sotto else ("" if x is None else x),
                    "pct_sotto_soglia": x if sotto else ""})

    campi = ["tabella", "istituto", "editore", "data_inizio", "data_fine",
             "data_media", "n", "lista", "testo", "seggi", "pct_sotto_soglia"]
    with open(os.path.join(INPUT, "sondaggi_grezzi.csv"), "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=campi)
        w.writeheader()
        w.writerows(out)
    print(f"{len(visti)} sondaggi, {len(out)} righe, "
          f"dal {min(r['data_media'] for r in out)} al {max(r['data_media'] for r in out)}")


if __name__ == "__main__":
    main()
