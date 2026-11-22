#!/usr/bin/env python3
# ============================================================================
# Sondaggi del primo turno delle presidenziali brasiliane (voto: 4 ottobre 2026)
# dalla voce inglese di Wikipedia "Opinion polling for the 2026 Brazilian
# presidential election".
#
# Si usano le due tabelle del primo turno dal 4 aprile 2026 in poi
# ("Aug–Oct (Campaign period)" e "Apr–Aug"): dalla scadenza per le dimissioni
# il campo e' quello definitivo (Lula, Flavio Bolsonaro, Caiado, Zema, Santos,
# Cury) e le colonne sono le stesse. Le tabelle di gennaio-marzo hanno un altro
# campo (Tarcisio, Ratinho, Leite) e non sono confrontabili.
#
# Un sondaggio puo' occupare piu' righe (rowspan), una per scenario. Si tiene
# lo scenario con piu' candidati del campo definitivo presenti; a parita', il
# primo (sulla voce e' sempre lo scenario principale).
#
# Output: input/sondaggi_grezzi.csv (una riga per sondaggio)
# ============================================================================

import csv
import os
import re
import urllib.request
from datetime import date, timedelta

BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
INPUT = os.path.join(BASE, "input")
GREZZO = os.path.join(INPUT, "br2026_en.wiki")
URL = ("https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_"
       "2026_Brazilian_presidential_election&action=raw")

SEZIONI = ["==== Aug–Oct (Campaign period) ====", "==== Apr–Aug ===="]
CAND = ["lula", "flavio", "caiado", "zema", "santos", "cury"]
COLONNE = ["istituto", "periodo"] + CAND + ["altri", "bianche_indecisi",
                                           "moe", "n", "vantaggio", "link"]
MESI = {m: i + 1 for i, m in enumerate(
    ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
     "Nov", "Dec"])}


def scarica():
    if not os.path.exists(GREZZO):
        req = urllib.request.Request(URL, headers={"User-Agent": "data-viz"})
        with urllib.request.urlopen(req) as r, open(GREZZO, "wb") as f:
            f.write(r.read())
    with open(GREZZO, encoding="utf-8") as f:
        return f.read()


def togli_template(s):
    s = re.sub(r"<ref[^>]*/>", "", s)
    s = re.sub(r"<ref.*?</ref>", "", s, flags=re.S)
    s = re.sub(r"\{\{NA\}\}", "NA", s, flags=re.I)
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


def separa_attributi(cella):
    """`rowspan="2" style=... |valore` -> (attributi, valore)."""
    c = re.sub(r"\[\[[^\]|]*\|([^\]]*)\]\]", r"\1", cella)
    c = re.sub(r"\[\[([^\]]*)\]\]", r"\1", c)
    c = togli_template(c)
    attr, val = "", c
    if "|" in c:
        attr, val = c.split("|", 1)
    val = re.sub(r"<[^>]+>", " ", val).replace("'''", "").replace("''", "")
    return attr, val.strip()


def tabella(testo, titolo):
    i = testo.index(titolo)
    a = testo.index("{|", i)
    b = testo.index("\n|}", a)
    return testo[a:b]


def righe(tab):
    """Righe di dati con le celle gia' espanse dai rowspan."""
    blocchi = re.split(r"\n\|-[^\n]*", tab)
    pendenti = {}            # colonna -> [valore, righe restanti]
    fuori = []
    for bl in blocchi:
        celle = []
        for riga in bl.split("\n"):
            if riga.startswith("|") and not riga.startswith("|}"):
                celle.extend(riga[1:].split("||"))
        if not celle or any("colspan" in c for c in celle):
            continue         # intestazioni o righe-evento
        valori, col, k = [], 0, 0
        while col < len(COLONNE):
            if col in pendenti:
                v, rest = pendenti[col]
                valori.append(v)
                pendenti[col][1] -= 1
                if pendenti[col][1] == 0:
                    del pendenti[col]
            elif k < len(celle):
                attr, v = separa_attributi(celle[k]); k += 1
                m = re.search(r'rowspan="?(\d+)', attr)
                if m and int(m.group(1)) > 1:
                    pendenti[col] = [v, int(m.group(1)) - 1]
                valori.append(v)
            else:
                valori.append("")
            col += 1
        fuori.append(dict(zip(COLONNE, valori)))
    return fuori


def numero(s):
    s = s.replace(",", "").replace("%", "").strip()
    m = re.match(r"^-?\d+(\.\d+)?", s)
    return float(m.group(0)) if m else None


def date_campo(periodo):
    """'29 Sep–1 Oct', '28–30 Sep', '20 Jul' -> (inizio, fine)."""
    p = periodo.replace("–", "-").replace("—", "-").strip()
    parti = [x.strip() for x in p.split("-")]
    def leggi(x, mese_def=None):
        t = x.split()
        g = int(t[0])
        mese = MESI[t[1][:3]] if len(t) > 1 else mese_def
        return g, mese
    if len(parti) == 1:
        g, m = leggi(parti[0]); d = date(2026, m, g); return d, d
    g2, m2 = leggi(parti[1])
    g1, m1 = leggi(parti[0], m2)
    return date(2026, m1, g1), date(2026, m2, g2)


def main():
    testo = scarica()
    tutte = []
    for s in SEZIONI:
        for r in righe(tabella(testo, s)):
            if not r["istituto"] or not r["periodo"]:
                continue
            r["presenti"] = sum(numero(r[c]) is not None for c in CAND)
            tutte.append(r)

    # uno scenario per sondaggio
    scelti = {}
    for ordine, r in enumerate(tutte):
        chiave = (r["istituto"], r["periodo"], r["n"])
        if chiave not in scelti or r["presenti"] > scelti[chiave][1]["presenti"]:
            scelti[chiave] = (ordine, r)

    out = []
    for _, r in sorted(scelti.values(), key=lambda x: x[0]):
        ini, fin = date_campo(r["periodo"])
        riga = {"istituto": r["istituto"], "data_inizio": ini, "data_fine": fin,
                "data_media": ini + timedelta(days=(fin - ini).days // 2),
                "n": int(numero(r["n"])) if numero(r["n"]) else "",
                "scenario_candidati": r["presenti"]}
        for c in CAND + ["altri", "bianche_indecisi"]:
            v = numero(r[c])
            riga[c] = "" if v is None else v
        out.append(riga)

    campi = ["istituto", "data_inizio", "data_fine", "data_media", "n",
             "scenario_candidati"] + CAND + ["altri", "bianche_indecisi"]
    with open(os.path.join(INPUT, "sondaggi_grezzi.csv"), "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=campi)
        w.writeheader()
        w.writerows(out)
    print(f"{len(tutte)} righe, {len(out)} sondaggi, "
          f"dal {min(r['data_media'] for r in out)} al {max(r['data_media'] for r in out)}")


if __name__ == "__main__":
    main()
