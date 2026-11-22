#!/usr/bin/env python3
# ============================================================================
# Errori storici dei sondaggi sul primo turno delle presidenziali francesi:
# ultimi sondaggi prima del voto contro il risultato, per il 2007, il 2012, il
# 2017 e il 2022. Serve ad allargare la fascia di incertezza della media dei
# sondaggi per il 2027.
#
# Fonte: le voci inglesi "Opinion polling for the <anno> French presidential
# election" di Wikipedia (input/storici/*.wiki, scaricate solo se mancano).
# Si legge la prima tabella della sezione "Official campaign" del primo turno:
# una riga per sondaggio (istituto, date, campione, [astensione], una colonna
# per candidato) e in cima la riga evidenziata del risultato vero. Le
# intestazioni hanno formati diversi da un anno all'altro (nomi con link e
# sigle di partito su due righe, file immagine nel 2022): il nome di ogni
# colonna e' il cognome che precede il <br> nel link, e il numero di colonne
# attese si controlla contro la riga del risultato.
#
# Sondaggi finali: quelli con fine rilevazione negli ultimi 15 giorni prima del
# primo turno, uno solo per istituto (il piu' recente: per i rolling quotidiani
# l'ultima ondata). Le righe del giorno del voto (exit poll, stime) sono
# escluse. Errore = risultato - media semplice dei sondaggi finali, in punti
# percentuali: positivo vuol dire che i sondaggi sottostimavano il candidato.
# Solo i candidati con una colonna nella tabella (cioe' quelli provati dagli
# istituti) e con almeno un valore numerico fra i sondaggi finali.
#
# Output: output/errori_storici.csv; a schermo la tabella e lo scarto
#         quadratico medio complessivo e per fascia di consenso
# ============================================================================

import csv
import math
import os
import re
import urllib.request
from collections import OrderedDict
from datetime import date, timedelta
from statistics import mean

BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
STORICI = os.path.join(BASE, "input", "storici")
GIORNI = 15

ELEZIONI = OrderedDict([
    (2007, date(2007, 4, 22)),
    (2012, date(2012, 4, 22)),
    (2017, date(2017, 4, 23)),
    (2022, date(2022, 4, 10)),
])

# Risultati veri, per controllo e come ripiego se la riga sulla voce non si legge
RISULTATI_NOTI = {
    2007: {"Sarkozy": 31.18, "Royal": 25.87, "Bayrou": 18.57, "Le Pen": 10.44},
    2012: {"Hollande": 28.63, "Sarkozy": 27.18, "Le Pen": 17.90,
           "Mélenchon": 11.10, "Bayrou": 9.13},
    2017: {"Macron": 24.01, "Le Pen": 21.30, "Fillon": 20.01,
           "Mélenchon": 19.58, "Hamon": 6.36},
    2022: {"Macron": 27.85, "Le Pen": 23.15, "Mélenchon": 21.95,
           "Zemmour": 7.07, "Pécresse": 4.78, "Jadot": 4.63},
}

MESI = {m: i + 1 for i, m in enumerate(
    ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
     "Nov", "Dec"])}


def pagina(anno):
    return f"Opinion_polling_for_the_{anno}_French_presidential_election"


def scarica(anno):
    os.makedirs(STORICI, exist_ok=True)
    percorso = os.path.join(STORICI, pagina(anno) + ".wiki")
    if not os.path.exists(percorso):
        url = ("https://en.wikipedia.org/w/index.php?title=" + pagina(anno)
               + "&action=raw")
        req = urllib.request.Request(url, headers={"User-Agent": "data-viz"})
        with urllib.request.urlopen(req) as r, open(percorso, "wb") as f:
            f.write(r.read())
    with open(percorso, encoding="utf-8") as f:
        return f.read()


# ---- parser delle tabelle (da 01_scarica_sondaggi.py) ----------------------

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
    c = re.sub(r"<!--.*?-->", "", cella, flags=re.S)
    c = re.sub(r"<ref[^>]*/>", "", c)
    c = re.sub(r"<ref.*?</ref>", "", c, flags=re.S)
    c = re.sub(r"\[\[[^\]|]*\|([^\]]*)\]\]", r"\1", c)
    c = re.sub(r"\[\[([^\]]*)\]\]", r"\1", c)
    c = re.sub(r"\[https?://\S+\s+([^\]]*)\]", r"\1", c)
    c = togli_template(c)
    return c


def separa(cella):
    """-> (rowspan, colspan, valore)."""
    c = pulisci(cella)
    rs = re.search(r'rowspan\s*=\s*"?(\d+)', c)
    cs = re.search(r'colspan\s*=\s*"?(\d+)', c)
    if "|" in c:
        attr, val = c.split("|", 1)
        if attr.strip() and not re.search(
                r"(rowspan|colspan|style|class|align|data-sort-value)\s*=", attr):
            val = c
    else:
        val = re.sub(r'(rowspan|colspan)\s*=\s*"?\d+"?', "", c)
    val = re.sub(r"<br\s*/?>", " ", val)
    val = re.sub(r"<[^>]+>", "", val).replace("'''", "").replace("''", "")
    return (int(rs.group(1)) if rs else 1, int(cs.group(1)) if cs else 1,
            re.sub(r"\s+", " ", val).strip())


def spezza(riga, sep):
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
    out = []
    for bl in re.split(r"\n\|-[^\n]*", tab):
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
            out.append((tipo, celle, bl))
    return out


# ---- lettura di una tabella -------------------------------------------------

def numero(s):
    m = re.match(r"^\s*(\d+(?:[.,]\d+)?)\s*%", s)
    return float(m.group(1).replace(",", ".")) if m else None


def data_fine(cella_grezza, testo, anno):
    m = re.search(r'data-sort-value\s*=\s*"(\d{4})-(\d{2})-(\d{2})"', cella_grezza)
    if m:
        return date(int(m.group(1)), int(m.group(2)), int(m.group(3)))
    m = re.findall(r"(\d{1,2})\s*([A-Z][a-z]{2})[a-z]*\.?\s*(\d{4})?", testo)
    if m:
        g, mm, a = m[-1]
        return date(int(a) if a else anno, MESI[mm], int(g))
    return None


def istituto(s):
    s = re.sub(r"\{\{.*?\}\}", "", s)
    s = s.replace("*", "").replace("†", "").replace("‡", "")
    return re.sub(r"\s+", " ", s).strip()


def nomi_candidati(testo_header):
    """Cognomi nell'ordine delle colonne: il testo del link seguito da <br>."""
    nomi = []
    for m in re.finditer(r"\[\[([^\]]*?)\]\]\s*<br\s*/?>", testo_header):
        link = m.group(1)
        nome = link.split("|", 1)[1] if "|" in link else link
        nome = re.sub(r"<br\s*/?>", "", nome).strip()
        nomi.append(nome)
    return nomi


def leggi(anno):
    testo = scarica(anno)
    ini = testo.index("== First round ==")
    ini = testo.index("Official campaign", ini)
    tab = re.search(r"\{\|.*?\n\|\}", testo[ini:], flags=re.S).group(0)
    bl = blocchi(tab)
    header = "\n".join(g for t, _, g in bl if t == "h")
    nomi = nomi_candidati(header)
    con_abs = bool(re.search(r"Abs\.", header))
    fissi = ["istituto", "data", "n"] + (["abs"] if con_abs else [])
    colonne = fissi + nomi
    pendenti, righe = {}, []
    for tipo, celle, _ in bl:
        if tipo == "h":
            continue
        prime = [separa(c) for c in celle]
        if any(cs >= 10 for _, cs, _ in prime):          # riga-evento
            continue
        valori, grezzi, col, k = [], [], 0, 0
        while col < len(colonne):
            if col in pendenti:
                valori.append(pendenti[col][1]); grezzi.append(pendenti[col][2])
                pendenti[col][0] -= 1
                if pendenti[col][0] == 0:
                    del pendenti[col]
                col += 1
                continue
            if k >= len(prime):
                break
            rs, cs, v = prime[k]; g = celle[k]; k += 1
            valori.append(v); grezzi.append(g)
            if rs > 1:
                pendenti[col] = [rs - 1, v, g]
            col += cs
        if len(valori) != len(colonne):
            print(f"  [{anno}] riga scartata ({len(valori)} celle su "
                  f"{len(colonne)}): {valori[:2]}")
            continue
        d = dict(zip(colonne, valori))
        d["fine"] = data_fine(grezzi[1], valori[1], anno)
        d["voti"] = OrderedDict((n, numero(v)) for n, v in
                                zip(nomi, valori[len(fissi):])
                                if numero(v) is not None)
        righe.append(d)
    return nomi, righe


def main():
    out, per_el = [], OrderedDict()
    for anno, voto in ELEZIONI.items():
        nomi, righe = leggi(anno)
        ris = next(r for r in righe if "election" in r["istituto"].lower())
        veri = RISULTATI_NOTI[anno]
        # controllo contro i risultati noti
        for c, x in veri.items():
            letto = ris["voti"].get(c)
            esito = "ok" if letto is not None and abs(letto - x) < 0.005 else "DIVERSO"
            if esito != "ok":
                print(f"  [{anno}] risultato {c}: letto {letto}, noto {x}  <-- {esito}")
        polls = [r for r in righe if r is not ris and r["fine"]]
        finestra = [r for r in polls
                    if voto - timedelta(days=GIORNI) <= r["fine"] < voto]
        scartate = [r for r in polls if r["fine"] >= voto]
        # tabella in ordine cronologico inverso: il primo per istituto e' l'ultimo
        # (Ifop e Ifop-Fiducial, Ipsos e Ipsos-Sopra Steria, OpinionWay e
        # OpinionWay-Kea sono lo stesso istituto: si confronta il primo
        # termine del nome; a parita' di data vale l'ordine della tabella)
        finali, visti = [], {}
        for r in sorted(finestra, key=lambda r: r["fine"], reverse=True):
            ist = istituto(r["istituto"])
            chiave = re.split(r"[-\s]", ist.lower())[0]
            if chiave not in visti:
                visti[chiave] = r; finali.append((ist, r))
        per_el[anno] = [(i, r["fine"]) for i, r in finali]
        for nome in nomi:
            vals = [r["voti"][nome] for _, r in finali if nome in r["voti"]]
            if not vals or nome not in ris["voti"]:
                continue
            m = mean(vals)
            out.append({"elezione": anno, "data_voto": voto.isoformat(),
                        "candidato": nome, "n_sondaggi": len(vals),
                        "media_sondaggi": round(m, 2),
                        "risultato": ris["voti"][nome],
                        "errore": round(ris["voti"][nome] - m, 2)})
        print(f"{anno}: {len(finali)} sondaggi finali, {len(scartate)} del giorno "
              f"del voto o dopo scartati")
        for ist, r in finali:
            print(f"    {r['fine']}  {ist}")

    os.makedirs(os.path.join(BASE, "output"), exist_ok=True)
    campi = ["elezione", "data_voto", "candidato", "n_sondaggi",
             "media_sondaggi", "risultato", "errore"]
    with open(os.path.join(BASE, "output", "errori_storici.csv"), "w",
              newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=campi)
        w.writeheader(); w.writerows(out)

    print("\nelezione candidato        n  media  risultato  errore")
    for r in out:
        print(f"{r['elezione']}     {r['candidato']:<14s} {r['n_sondaggi']:2d} "
              f"{r['media_sondaggi']:6.2f} {r['risultato']:9.2f} {r['errore']:+7.2f}")

    def rmse(lista):
        return math.sqrt(mean(r["errore"] ** 2 for r in lista)) if lista else float("nan")

    print("\nScarto quadratico medio (punti percentuali)")
    print(f"  complessivo            {rmse(out):.2f}  (n={len(out)})")
    for nome, filtro in [("media sondaggi >= 15%", lambda r: r["media_sondaggi"] >= 15),
                         ("media fra 5 e 15%", lambda r: 5 <= r["media_sondaggi"] < 15),
                         ("media sotto il 5%", lambda r: r["media_sondaggi"] < 5)]:
        s = [r for r in out if filtro(r)]
        print(f"  {nome:<22s} {rmse(s):.2f}  (n={len(s)})")


if __name__ == "__main__":
    main()
