#!/usr/bin/env python3
# ============================================================================
# Sondaggi del ballottaggio Lula - Flavio Bolsonaro (voto: 25 ottobre 2026)
# dalla voce inglese di Wikipedia "Opinion polling for the 2026 Brazilian
# presidential election", sezione "Second round".
#
# Tre tabelle, dal 4 aprile 2026 in poi (come per il primo turno):
#   - "October (after 1st round)": solo Lula e Flavio, 9 colonne;
#   - "Aug–Oct (Campaign period)" e "Apr–Aug": tutti gli scenari di
#     ballottaggio (Lula o Flavio contro Caiado, Zema, Santos, Cury) in 13
#     colonne. Si tiene solo lo scenario Lula - Flavio.
#
# Un sondaggio puo' occupare piu' righe (rowspan), una per scenario: dopo il
# filtro ne resta al massimo una.
#
# Output: input/sondaggi_grezzi.csv (una riga per sondaggio)
# ============================================================================

import csv
import os
import re
import unicodedata
import urllib.request
from datetime import date, timedelta

BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
INPUT = os.path.join(BASE, "input")
GREZZO = os.path.join(INPUT, "br2026_en.wiki")
URL = ("https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_"
       "2026_Brazilian_presidential_election&action=raw")

ALTRI = ["caiado", "zema", "santos", "cury"]
COL_DOPO = ["istituto", "periodo", "lula", "flavio", "bianche_indecisi",
            "moe", "n", "vantaggio", "link"]
COL_PRIMA = (["istituto", "periodo", "lula", "flavio"] + ALTRI +
             ["bianche_indecisi", "moe", "n", "vantaggio", "link"])
# (titolo della sezione, colonne, fase)
SEZIONI = [("==== October (after 1st round) ====", COL_DOPO, "dopo"),
           ("==== Aug–Oct (Campaign period) ====", COL_PRIMA, "prima"),
           ("==== Apr–Aug ====", COL_PRIMA, "prima")]
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


def tabella(testo, titolo, dopo=0):
    i = testo.index(titolo, dopo)
    a = testo.index("{|", i)
    b = testo.index("\n|}", a)
    return testo[a:b]


def righe(tab, colonne):
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
        while col < len(colonne):
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
        fuori.append(dict(zip(colonne, valori)))
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
    inizio_ballottaggio = testo.index("==Second round==")
    out, scartate = [], 0
    for titolo, colonne, fase in SEZIONI:
        for r in righe(tabella(testo, titolo, inizio_ballottaggio), colonne):
            if not r["istituto"] or not r["periodo"]:
                continue
            lula, flavio = numero(r["lula"]), numero(r["flavio"])
            if lula is None or flavio is None or any(
                    numero(r.get(c, "")) is not None for c in ALTRI):
                scartate += 1
                continue
            ini, fin = date_campo(r["periodo"])
            out.append({
                "istituto": r["istituto"], "fase": fase,
                "data_inizio": ini, "data_fine": fin,
                "data_media": ini + timedelta(days=(fin - ini).days // 2),
                "n": int(numero(r["n"])) if numero(r["n"]) else "",
                "lula": lula, "flavio": flavio,
                "bianche_indecisi": (numero(r["bianche_indecisi"])
                                     if numero(r["bianche_indecisi"]) is not None
                                     else round(100 - lula - flavio, 1)),
            })

    # stesso istituto scritto in modi diversi (Verita/Veritá, DataTrends/Datatrends):
    # vale il primo nome incontrato
    nomi = {}
    for r in out:
        k = unicodedata.normalize("NFKD", r["istituto"]).encode("ascii", "ignore").decode().lower()
        r["istituto"] = nomi.setdefault(k, r["istituto"])

    # doppioni (stesso istituto, stesso periodo, stesso campione)
    visti, unici = set(), []
    for r in out:
        k = (r["istituto"], r["data_inizio"], r["data_fine"], r["n"])
        if k not in visti:
            visti.add(k); unici.append(r)

    campi = ["istituto", "fase", "data_inizio", "data_fine", "data_media", "n",
             "lula", "flavio", "bianche_indecisi"]
    with open(os.path.join(INPUT, "sondaggi_grezzi.csv"), "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=campi)
        w.writeheader()
        w.writerows(unici)
    for fase in ("prima", "dopo"):
        d = [r["data_media"] for r in unici if r["fase"] == fase]
        print(f"{fase}: {len(d)} sondaggi, dal {min(d)} al {max(d)}")
    print(f"scartate (altri scenari): {scartate}, doppioni: {len(out) - len(unici)}")


if __name__ == "__main__":
    main()
