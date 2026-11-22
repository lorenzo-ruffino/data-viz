#!/usr/bin/env python3
# ============================================================================
# Sondaggi per le elezioni generali spagnole del 29 novembre 2026 dalla voce
# inglese di Wikipedia "Opinion polling for the 2026 Spanish general election"
# (sezione "Voting intention estimates": tabelle 2025, 2026 gennaio-ottobre e
# 2026 ottobre-novembre).
#
# Ogni riga e' un sondaggio: istituto/committente, giorni di rilevazione
# (senza anno: lo da' il titolo della sezione), campione, affluenza stimata e
# una colonna per lista con la percentuale e, sotto, i seggi stimati
# dall'istituto. Si tengono tutti e due; "?" e "–" sono valori mancanti, e una
# forchetta di seggi ("127/139") diventa il suo punto medio.
#
# Una cella con colspan e' il valore di una lista congiunta (per esempio
# Frente Amplio + Front Valencia') e prende il nome delle colonne che copre
# unite da "+". Le righe grigie dei risultati elettorali e le righe-evento si
# saltano.
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
GREZZO = os.path.join(INPUT, "es2026_en.wiki")
URL = ("https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_"
       "2026_Spanish_general_election&action=raw")

MESI = {m: i + 1 for i, m in enumerate(
    ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
     "Nov", "Dec"])}
SEZIONI = [("=====2026 (Oct–Nov)=====", 2026), ("=====2026 (Jan–Oct)=====", 2026),
           ("=====2025=====", 2025)]
FINE = "=====2023–2024====="


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
    """Toglie note, commenti e template; dei seggi sotto la percentuale non
    resta niente (stanno in un template font)."""
    c = re.sub(r"<!--.*?-->", "", cella, flags=re.S)
    c = re.sub(r"<ref[^>]*/>", "", c)
    c = re.sub(r"<ref.*?</ref>", "", c, flags=re.S)
    c = re.sub(r"\{\{\s*smaller\s*\|([^{}]*)\}\}", r"\1", c, flags=re.I)
    c = re.sub(r"\[\[File:[^\]]*\|link=[^\]|]*\|([^\]|]*)\]\]", r"\1", c)
    c = re.sub(r"\[\[File:[^\]]*link=([^\]|]*)\]\]", r"\1", c)
    c = re.sub(r"\[\[[^\]|]*\|([^\]]*)\]\]", r"\1", c)
    c = re.sub(r"\[\[([^\]]*)\]\]", r"\1", c)
    c = re.sub(r"\[https?://\S+\s+([^\]]*)\]", r"\1", c)
    c = togli_template(c)
    return c


def seggi_di(cella):
    """Seggi sotto la percentuale: {{font|size=75%|font=Verdana|text=127/139}}."""
    c = re.sub(r"<!--.*?-->", "", cella, flags=re.S)
    m = re.search(r"\{\{\s*font\s*\|[^{}]*?text\s*=\s*([0-9]+)(?:\s*[/–-]\s*([0-9]+))?\s*\}\}", c)
    if not m:
        return None
    a = int(m.group(1)); b = int(m.group(2)) if m.group(2) else a
    return (a + b) / 2


def separa(cella):
    """-> (rowspan, colspan, valore)."""
    c = pulisci(cella)
    rs = re.search(r'rowspan\s*=\s*"?(\d+)', c)
    cs = re.search(r'colspan\s*=\s*"?(\d+)', c)
    if "|" in c:
        attr, val = c.split("|", 1)
        if attr.strip() and not re.search(
                r"(rowspan|colspan|style|class|align|data-sort-value)\s*=", attr):
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
    """Righe della tabella come (tipo, attributi della riga, celle)."""
    out = []
    pezzi = re.split(r"\n\|-([^\n]*)", tab)
    attributi = [""] + pezzi[1::2]
    for attr, bl in zip(attributi, pezzi[0::2]):
        celle, tipo = [], None
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
                celle[-1] += "\n" + riga
        if celle:
            out.append((tipo, attr, celle))
    return out


def intestazione(righe_h):
    """Etichetta di ogni colonna dalla prima riga di intestazione."""
    nomi = []
    for cella in righe_h[0]:
        _, cs, val = separa(cella)
        nomi.extend([val] * cs)
    return nomi


def leggi_data(s, anno):
    """'16–19 Nov', '19 Nov', '30 Oct–2 Nov', '29 Dec–5 Jan' -> (inizio, fine).
    L'anno e' quello della sezione, cioe' quello del giorno di fine campo."""
    s = s.replace("&ndash;", "–").replace("-", "–").strip()
    s = re.sub(r"^\?\s*–\s*", "", s)          # inizio campo ignoto: solo la fine
    m = re.match(r"^(\d{1,2})(?:\s+([A-Z][a-z]{2}))?(?:\s*–\s*(\d{1,2}))?\s+([A-Z][a-z]{2})\w*$", s)
    if not m:
        m2 = re.match(r"^(\d{1,2})\s+([A-Z][a-z]{2})\w*\s*–\s*(\d{1,2})\s+([A-Z][a-z]{2})\w*$", s)
        if not m2:
            return None
        g1, me1, g2, me2 = m2.groups()
    else:
        g1, me1, g2, me2 = m.groups()
        if g2 is None:
            g2, me1 = g1, me2
        me1 = me1 or me2
    fine = date(anno, MESI[me2], int(g2))
    a1 = anno - 1 if MESI[me1] > MESI[me2] else anno
    return date(a1, MESI[me1], int(g1)), fine


def numero(s):
    m = re.match(r"^\s*(\d+(?:\.\d+)?)", s.replace(",", ""))
    return float(m.group(1)) if m else None


def main():
    testo = scarica()
    out, n_sond, scartate = [], 0, []
    for k, (titolo, anno) in enumerate(SEZIONI):
        a = testo.index(titolo)
        b = testo.index(SEZIONI[k + 1][0]) if k + 1 < len(SEZIONI) else testo.index(FINE)
        tab = re.search(r"\{\|.*?\n\|\}", testo[a:b], flags=re.S).group(0)
        bl = blocchi(tab)
        nomi = intestazione([c for t, _, c in bl if t == "h"])
        pendenti = {}                  # colonna -> [righe restanti, nome, valore]
        for tipo, attr, celle in bl:
            if tipo == "h" or "background" in attr:      # intestazioni, risultati veri
                continue
            prime = [separa(c) + (seggi_di(c),) for c in celle]
            if any(cs >= 10 for _, cs, _, _ in prime):   # riga-evento
                continue
            # le celle con rowspan (giorni e campione del CIS, condivisi dalle
            # stime alternative sugli stessi microdati) valgono anche sotto
            valori, col, k = [], 0, 0
            while col < len(nomi):
                if col in pendenti:
                    valori.append(pendenti[col][1:])
                    pendenti[col][0] -= 1
                    if pendenti[col][0] == 0:
                        del pendenti[col]
                    col += 1
                    continue
                if k >= len(prime):
                    break
                rs, cs, v, sg = prime[k]; k += 1
                nome = "+".join(dict.fromkeys(nomi[col:col + cs]))
                valori.append([nome, v, sg])
                if rs > 1:
                    pendenti[col] = [rs - 1, nome, v, sg]
                col += cs
            d = {v[0]: v[1] for v in valori}
            date_ = leggi_data(d.get("Fieldwork date", ""), anno)
            if date_ is None:
                scartate.append(d.get("Fieldwork date", ""))
                continue
            ini, fin = date_
            firma = d["Polling firm/Commissioner"]
            # "CIS (Target Point)": ristima di altri sui microdati del CIS
            ristima = bool(re.match(r"^CIS\s*\(", firma))
            istituto, _, committente = firma.partition("/")
            n = numero(d.get("Sample size", ""))
            n_sond += 1
            for nome, v, sg in valori[4:]:
                if nome == "Lead":
                    continue
                x = numero(v)
                if x is None and sg is None:
                    continue
                out.append({
                    "id": n_sond, "sezione": titolo.strip("="),
                    "istituto": istituto.strip(), "committente": committente.strip(),
                    "ristima_cis": int(ristima),
                    "data_inizio": ini, "data_fine": fin,
                    "data_media": ini + timedelta(days=(fin - ini).days // 2),
                    "n": int(n) if n else "", "lista": nome,
                    "pct": "" if x is None else x,
                    "seggi": "" if sg is None else sg})

    campi = ["id", "sezione", "istituto", "committente", "ristima_cis", "data_inizio", "data_fine",
             "data_media", "n", "lista", "pct", "seggi"]
    with open(os.path.join(INPUT, "sondaggi_grezzi.csv"), "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=campi)
        w.writeheader()
        w.writerows(out)
    print(f"{n_sond} sondaggi, {len(out)} righe, "
          f"dal {min(r['data_fine'] for r in out)} al {max(r['data_fine'] for r in out)}")
    if scartate:
        print("Date non lette:", scartate)


if __name__ == "__main__":
    main()
