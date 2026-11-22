#!/usr/bin/env python3
# ============================================================================
# Sondaggi sul primo turno delle presidenziali francesi del 18 aprile 2027
# dalla voce inglese di Wikipedia "Opinion polling for the 2027 French
# presidential election".
#
# Due fasi, divise dal 7 luglio 2026, quando la Corte d'appello di Parigi ha
# riaperto la strada alla candidatura di Marine Le Pen:
#   - fase "lepen" (sezione "First round", tabelle da luglio 2026): il
#     candidato del Rassemblement National e' Marine Le Pen;
#   - fase "bardella" (sezione "Scenario polling", tabella gennaio-giugno
#     2026): Le Pen era ineleggibile e gli istituti provavano Jordan Bardella.
#
# Ogni istituto prova piu' scenari nello stesso sondaggio, uno per riga: con o
# senza Attal, con Hollande al posto di Glucksmann, con Lagarde candidata e
# cosi' via. Qui serve lo scenario con il campo di oggi, non le ipotesi. Per
# ogni sondaggio si sceglie quindi la riga che:
#   - prova insieme i sei candidati principali (il candidato del RN della
#     fase, Melenchon, Glucksmann, Philippe, Attal, Retailleau);
#   - non contiene nomi che non sono candidati (Hollande, Faure, Lagarde, Le
#     Maire...). Eccezione: nella fase "bardella" Sarah Knafo, che allora gli
#     istituti provavano per Reconquete al posto di Zemmour;
#   - fra quelle rimaste, ha piu' candidati dichiarati (la lista piu'
#     completa), poi viene prima nella tabella.
# Villepin si e' candidato il 20 novembre 2026: da allora conta fra i
# dichiarati, anche per i sondaggi precedenti che lo provavano gia'.
# Ruffin si e' ritirato il 21 novembre 2026: le righe che lo contengono
# restano valide (quando il sondaggio e' stato fatto era in campo), ma non
# conta piu' fra i dichiarati quando si sceglie la lista piu' completa.
# I sondaggi che non hanno una riga cosi' restano fuori, e cosi' le righe la
# cui somma si allontana da 100 di piu' di due punti (refusi sulla voce).
#
# Output: input/sondaggi_grezzi.csv (formato lungo: una riga per sondaggio,
#         scenario e candidato; la colonna "scelto" marca lo scenario tenuto)
# ============================================================================

import csv
import os
import re
import urllib.request
from datetime import date, timedelta

BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
INPUT = os.path.join(BASE, "input")
GREZZO = os.path.join(INPUT, "fr2027_en.wiki")
URL = ("https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_"
       "2027_French_presidential_election&action=raw")

MESI = {m: i + 1 for i, m in enumerate(
    ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
     "Nov", "Dec"])}
SVOLTA = date(2026, 7, 7)
# (inizio sezione, fine sezione, fase)
SEZIONI = [("== First round ==", "=== January 2026 – June 2026 ===", "lepen"),
           ("==== January – June 2026 ====", "==== March – December 2025 ====",
            "bardella")]
RN = {"lepen": "Marine Le Pen", "bardella": "Jordan Bardella"}

# Candidati dichiarati al 22 novembre 2026 (voce "2027 French presidential
# election", sezione "Declared candidates")
PRINCIPALI = ["Jean-Luc Mélenchon", "Raphaël Glucksmann",
              "Édouard Philippe", "Gabriel Attal", "Bruno Retailleau"]
DICHIARATI = PRINCIPALI + ["Marine Le Pen",
    "David Lisnard", "Marine Tondelier", "Fabien Roussel",
    "Éric Zemmour", "Nathalie Arthaud", "François Asselineau", "Delphine Batho",
    "Xavier Bertrand", "Karim Bouamrane", "Nicolas Dupont-Aignan",
    "Jean-Michel Fauvergue", "Anasse Kazib", "Selma Labib", "Francis Lalanne",
    "Florian Philippot", "Natacha Polony", "Teddy Riner",
    "Dominique de Villepin"]
RITIRATI = ["François Ruffin"]
AMMESSI = {"lepen": DICHIARATI + RITIRATI,
           "bardella": DICHIARATI + RITIRATI + ["Jordan Bardella", "Sarah Knafo"]}
# nomi completi, per riportare a uno stesso nome le intestazioni col solo
# cognome ("Arthaud (LO)", "de Villepin (LFH)")
ALTRI_NOMI = ["Jordan Bardella", "Sarah Knafo",
              "François Hollande", "Olivier Faure", "Laurent Wauquiez",
              "Philippe Poutou", "Gérald Darmanin", "Sébastien Lecornu",
              "Christine Lagarde", "Bruno Le Maire", "Ségolène Royal",
              "Emmanuel Maurel", "Jérôme Guedj"]


def nome_completo(n):
    n = re.sub(r"\s*\(.*$", "", n).strip()
    tutti = DICHIARATI + RITIRATI + ALTRI_NOMI
    if n in tutti:
        return n
    for f in tutti:
        if f.endswith(" " + n):
            return f
    return n


def vert_header(m):
    """{{vert header|mw=35px|stp=1|[[Nome|Cognome]] [[Partito|(XX)]]}} -> ultimo
    parametro, cioe' il testo dell'intestazione."""
    corpo, parti, depth, ini, i = m.group(1), [], 0, 0, 0
    while i < len(corpo):
        if corpo.startswith("[[", i):
            depth += 1; i += 2
        elif corpo.startswith("]]", i) and depth:
            depth -= 1; i += 2
        elif corpo[i] == "|" and not depth:
            parti.append(corpo[ini:i]); i += 1; ini = i
        else:
            i += 1
    parti.append(corpo[ini:])
    return parti[-1]


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
    c = re.sub(r"<!--.*?-->", "", cella, flags=re.S)
    c = re.sub(r"<ref[^>]*/>", "", c)
    c = re.sub(r"<ref.*?</ref>", "", c, flags=re.S)
    c = re.sub(r"\{\{\s*Opdrts\s*\|([^}]*)\}\}",
               lambda m: "OPD:" + m.group(1).replace("|", "/"), c, flags=re.I)
    c = re.sub(r"\{\{\s*vert header\s*\|((?:[^{}])*)\}\}", vert_header, c, flags=re.I)
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
            out.append((tipo, celle))
    return out


def leggi_data(s):
    """'OPD:12/17/Nov/2026/year', 'OPD:/17/Nov/2026', 'OPD:30/2/Nov/2026'."""
    p = s.split("OPD:", 1)[1].split("/")
    g1, g2, mese, anno = (p + ["", "", "", ""])[:4]
    m = MESI[mese.strip()[:3]]
    fine = date(int(anno), m, int(g2))
    if g1.strip():
        g1 = int(g1)
        if g1 <= fine.day:
            ini = date(int(anno), m, g1)
        else:
            ini = date(int(anno), m - 1, g1) if m > 1 else date(int(anno) - 1, 12, g1)
    else:
        ini = fine
    return ini, fine


def numero(s):
    m = re.match(r"^\s*(\d+(?:[.,]\d+)?)\s*%?", s)
    return float(m.group(1).replace(",", ".")) if m else None


def main():
    testo = scarica()
    righe, n_sond = [], 0
    for inizio, fine, fase in SEZIONI:
        sezione = testo[testo.index(inizio):testo.index(fine)]
        for tab in re.findall(r"\{\|.*?\n\|\}", sezione, flags=re.S):
            bl = blocchi(tab)
            nomi = []
            for cella in [c for t, c in bl if t == "h"][0]:
                _, cs, val = separa(cella)
                nomi.extend([nome_completo(val)] * cs)
            pendenti, scenario = {}, 0
            for tipo, celle in bl:
                if tipo == "h":
                    continue
                prime = [separa(c) for c in celle]
                if any(cs >= 10 for _, cs, _ in prime):      # riga-evento
                    continue
                valori, col, k = [], 0, 0
                nuovo = 0 not in pendenti
                while col < len(nomi):
                    if col in pendenti:
                        valori.append(pendenti[col][1])
                        pendenti[col][0] -= 1
                        if pendenti[col][0] == 0:
                            del pendenti[col]
                        col += 1
                        continue
                    if k >= len(prime):
                        break
                    rs, cs, v = prime[k]; k += 1
                    valori.append(v)
                    if rs > 1:
                        pendenti[col] = [rs - 1, v]
                    col += cs
                if len(valori) < 4 or "OPD:" not in valori[1]:
                    continue
                if nuovo:
                    n_sond += 1; scenario = 1
                else:
                    scenario += 1
                ini, fin = leggi_data(valori[1])
                # ogni fase tiene solo i sondaggi dal suo lato della svolta
                if (fin >= SVOLTA) != (fase == "lepen"):
                    continue
                # campione: "1,598", "1 598" e anche "1.598"
                n = numero(re.sub(r"[.,\s\u00a0\u202f]|&nbsp;", "", valori[2]))
                for nome, v in zip(nomi[3:], valori[3:]):
                    x = numero(v)
                    if x is None:
                        continue
                    righe.append({
                        "id": n_sond, "scenario": scenario, "fase": fase,
                        "istituto": valori[0],
                        "data_inizio": ini, "data_fine": fin,
                        "data_media": ini + timedelta(days=(fin - ini).days // 2),
                        "n": int(n) if n else "", "candidato": nome, "pct": x})

    # scenario con il campo di oggi
    scen, fase_di = {}, {}
    for r in righe:
        scen.setdefault((r["id"], r["scenario"]), {})[r["candidato"]] = r["pct"]
        fase_di[r["id"]] = r["fase"]
    migliore, storte = {}, []
    for (i, s), voti in scen.items():
        fase = fase_di[i]
        if not all(p in voti for p in PRINCIPALI + [RN[fase]]):
            continue
        if any(c not in AMMESSI[fase] for c in voti):
            continue
        if abs(sum(voti.values()) - 100) > 2:
            storte.append((i, s, sum(voti.values())))
            continue
        voto = (-len([c for c in voti if c in DICHIARATI]), s)
        if i not in migliore or voto < migliore[i][0]:
            migliore[i] = (voto, s)
    for r in righe:
        r["scelto"] = int(r["id"] in migliore and migliore[r["id"]][1] == r["scenario"])

    campi = ["id", "scenario", "scelto", "fase", "istituto", "data_inizio", "data_fine",
             "data_media", "n", "candidato", "pct"]
    with open(os.path.join(INPUT, "sondaggi_grezzi.csv"), "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=campi)
        w.writeheader()
        w.writerows(righe)
    for fase in ("bardella", "lepen"):
        ids = {r["id"] for r in righe if r["fase"] == fase}
        date_ = [r["data_fine"] for r in righe if r["fase"] == fase]
        print(f"fase {fase}: {len(ids)} sondaggi, {len(ids & set(migliore))} con il "
              f"campo di oggi, dal {min(date_)} al {max(date_)}")
    for i, s, tot in storte:
        print(f"  somma {tot:g}: sondaggio {i}, scenario {s} scartato")


if __name__ == "__main__":
    main()
