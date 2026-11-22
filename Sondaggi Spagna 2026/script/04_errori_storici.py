#!/usr/bin/env python3
# ============================================================================
# Errori storici dei sondaggi spagnoli: ultimi sondaggi prima del voto contro
# il risultato, per le cinque elezioni generali dal 2015 al 2023 (20 dic 2015,
# 26 giu 2016, 28 apr 2019, 10 nov 2019, 23 lug 2023). Serve ad allargare la
# fascia di incertezza della media dei sondaggi per il 2026.
#
# Fonte: le voci inglesi "Opinion polling for the <elezione> Spanish general
# election" di Wikipedia (wikitext grezzo in input/storici/<nome>.wiki, scaricato
# solo se manca). Si legge la prima tabella nazionale "Voting intention
# estimates" (quella con le ultime settimane prima del voto). In ogni riga la
# cella di un partito ha la percentuale e, sotto, in un template font, i seggi
# (a volte un intervallo "127/139" o "127-139": si prende il punto medio). La
# riga grigia in cima alla tabella e' il risultato vero.
#
# Sondaggi finali: fine rilevazione nei 15 giorni prima del voto (voto escluso),
# uno per istituto (il piu' recente; l'istituto e' la parte prima della "/"),
# senza exit poll (righe verdi, "RTVE-FORTA", "exit"), senza ristime del CIS
# fatte da altri ("CIS (Target Point)"), senza medie di sondaggi (KeyData) e
# senza i sondaggi interni dei partiti (istituto = "PP", "PSOE", "Podemos"...).
# Compromis entra nel blocco di sinistra solo nell'aprile 2019 (negli altri
# anni e' dentro Podemos/Mas Pais); chi non ne riporta i seggi e' escluso dal blocco.
# Restano i sondaggi con rilevazione in periodo di divieto di pubblicazione
# (righe rosa su Wikipedia): sono sondaggi veri, usciti dopo il voto.
#
# Errore = risultato - media dei sondaggi finali: positivo vuol dire che i
# sondaggi sottovalutavano il partito. Nella tabella dei partiti Podemos-Sumar
# e' la colonna di Podemos / Unidos Podemos / Unidas Podemos / Sumar: dove il
# risultato dice "dentro Podemos" (confluenze, Compromis) la colonna e' gia'
# comprensiva, e non c'e' nulla da sommare.
#
# Blocchi in seggi (un sondaggio entra solo se riporta i seggi di TUTTI i
# partiti del blocco che nella tabella hanno una colonna):
#   destra   = PP + Vox + Ciudadanos
#   sinistra = PSOE + Podemos-Sumar (+ IU nel 2015, + Mas Pais nel nov 2019,
#              + Compromis nell'apr 2019)
#
# Output: output/errori_storici.csv (percentuali), output/errori_storici_seggi.csv
# ============================================================================

import csv
import os
import re
import urllib.request
from datetime import date
from math import sqrt
from statistics import mean

BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
STORICI = os.path.join(BASE, "input", "storici")
OUTPUT = os.path.join(BASE, "output")
os.makedirs(STORICI, exist_ok=True)

# nome file: (titolo della voce, data del voto)
ELEZIONI = {
    "2015":    ("Opinion_polling_for_the_2015_Spanish_general_election", date(2015, 12, 20)),
    "2016":    ("Opinion_polling_for_the_2016_Spanish_general_election", date(2016, 6, 26)),
    "2019-04": ("Opinion_polling_for_the_April_2019_Spanish_general_election", date(2019, 4, 28)),
    "2019-11": ("Opinion_polling_for_the_November_2019_Spanish_general_election", date(2019, 11, 10)),
    "2023":    ("Opinion_polling_for_the_2023_Spanish_general_election", date(2023, 7, 23)),
}
GIORNI_FINALI = 15

MESI = {m: i + 1 for i, m in enumerate(
    ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
     "Nov", "Dec"])}

# etichetta della colonna -> partito uniforme
PARTITI = {
    "PP": "PP", "PSOE": "PSOE", "Vox": "Vox", "VOX": "Vox",
    "Cs": "Ciudadanos", "C's": "Ciudadanos", "Ciudadanos": "Ciudadanos",
    "Podemos": "Podemos-Sumar", "UP": "Podemos-Sumar", "Sumar": "Podemos-Sumar",
    "Unidos Podemos": "Podemos-Sumar", "Unidas Podemos": "Podemos-Sumar",
    "IU": "IU", "IU–UPeC": "IU", "IU-UPeC": "IU",
    "MP": "Más País", "Más País": "Más País",
    "Compromís": "Compromís",
}
# partiti da tenere per elezione (IU solo 2015, Mas Pais solo nov 2019)
TENUTI = {
    "2015":    ["PP", "PSOE", "Podemos-Sumar", "Ciudadanos", "IU"],
    "2016":    ["PP", "PSOE", "Podemos-Sumar", "Ciudadanos"],
    "2019-04": ["PP", "PSOE", "Vox", "Podemos-Sumar", "Ciudadanos"],
    "2019-11": ["PP", "PSOE", "Vox", "Podemos-Sumar", "Ciudadanos", "Más País"],
    "2023":    ["PP", "PSOE", "Vox", "Podemos-Sumar"],
}
BLOCCHI = {"destra": ["PP", "Vox", "Ciudadanos"],
           "sinistra": ["PSOE", "Podemos-Sumar", "IU", "Más País", "Compromís"]}


def scarica(nome, titolo):
    percorso = os.path.join(STORICI, nome + ".wiki")
    if not os.path.exists(percorso):
        url = f"https://en.wikipedia.org/w/index.php?title={titolo}&action=raw"
        req = urllib.request.Request(url, headers={"User-Agent": "data-viz"})
        with urllib.request.urlopen(req) as r, open(percorso, "wb") as f:
            f.write(r.read())
    with open(percorso, encoding="utf-8") as f:
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
    """Toglie note, commenti e template (i seggi vanno estratti prima)."""
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


def intestazione(riga_h):
    nomi = []
    for cella in riga_h:
        _, cs, val = separa(cella)
        nomi.extend([val] * cs)
    return nomi


def leggi_data(s, anno):
    """'16–19 Nov', '19 Nov', '30 Oct–2 Nov', '26 Jun 2016' -> data di fine."""
    s = s.replace("&ndash;", "–").replace("-", "–").strip()
    s = re.sub(r"\s+\d{4}$", "", s)
    s = re.sub(r"^\?\s*–\s*", "", s)
    m = re.match(r"^(\d{1,2})(?:\s+([A-Z][a-z]{2}))?(?:\s*–\s*(\d{1,2}))?\s+([A-Z][a-z]{2})\w*$", s)
    if m:
        g1, me1, g2, me2 = m.groups()
        g2 = g2 or g1
    else:
        m2 = re.match(r"^(\d{1,2})\s+([A-Z][a-z]{2})\w*\s*–\s*(\d{1,2})\s+([A-Z][a-z]{2})\w*$", s)
        if not m2:
            return None
        g1, me1, g2, me2 = m2.groups()
    return date(anno, MESI[me2], int(g2))


def numero(s):
    m = re.match(r"^\s*(\d+(?:\.\d+)?)", s.replace(",", ""))
    return float(m.group(1)) if m else None


def seggi(raw):
    """Seggi dal template font (text=123 o text=114/118, 127–139): punto medio."""
    m = re.search(r"text\s*=\s*([^{}|]+)", raw)
    if not m:
        return None
    t = m.group(1).replace("&ndash;", "–").replace("–", "/").replace("-", "/")
    nums = [float(x) for x in re.findall(r"\d+", t)]
    return mean(nums) if nums else None


def leggi_tabella(testo, nome, anno):
    """Prima tabella di 'Voting intention estimates': lista di dict con
    istituto, fine, grigia (risultato), exit, pct{partito}, seggi{partito}."""
    a = testo.index("Voting intention estimates")
    tab = re.search(r"\{\|.*?\n\|\}", testo[a:], flags=re.S).group(0)
    bl = blocchi(tab)
    nomi = intestazione([c for t, _, c in bl if t == "h"][0])
    pendenti = {}
    righe = []
    for tipo, attr, celle in bl:
        if tipo == "h":
            continue
        prime = [(separa(c), c) for c in celle]
        if any(cs >= 10 for (_, cs, _), _ in prime):
            continue
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
            (rs, cs, v), raw = prime[k]; k += 1
            etichetta = "+".join(dict.fromkeys(nomi[col:col + cs]))
            valori.append([etichetta, v, raw])
            if rs > 1:
                pendenti[col] = [rs - 1, etichetta, v, raw]
            col += cs
        d = {v[0]: v for v in valori}
        firma = d["Polling firm/Commissioner"][1]
        fine = leggi_data(d["Fieldwork date"][1], anno)
        pct, seg = {}, {}
        for et, v, raw in valori[4:]:
            if et not in PARTITI:
                continue
            p = PARTITI[et]
            x = numero(v)
            s = seggi(raw)               # ci sono anche sondaggi con i soli seggi
            if x is not None:
                if p in pct:
                    raise ValueError(f"{nome}: colonna doppia {p} ({et}) in {firma}")
                pct[p] = x
            if s is not None:
                if p in seg:
                    raise ValueError(f"{nome}: seggi doppi {p} ({et}) in {firma}")
                seg[p] = s
        righe.append({"firma": firma, "fine": fine, "attr": attr,
                      "pct": pct, "seggi": seg})
    return righe


def sondaggi_finali(righe, voto):
    """Un sondaggio per istituto negli ultimi giorni prima del voto."""
    scelti = {}
    for r in righe:
        if r["fine"] is None or "background:#EFEFEF" in r["attr"].replace(" ", ""):
            continue
        firma = r["firma"]
        gg = (voto - r["fine"]).days
        if not 0 < gg <= GIORNI_FINALI:                    # il giorno del voto e' exit poll
            continue
        if ("EAFFEA" in r["attr"].upper() or re.search(r"exit|FORTA", firma, re.I)
                or re.match(r"^CIS\s*\(", firma) or re.match(r"^KeyData", firma)):
            continue
        istituto = firma.split("/")[0].strip()
        if istituto in PARTITI:                            # sondaggi interni dei partiti
            continue
        if istituto not in scelti or r["fine"] > scelti[istituto]["fine"]:
            r["istituto"] = istituto
            r["divieto"] = "FFEAEA" in r["attr"].upper()
            scelti[istituto] = r
    return list(scelti.values())


def main():
    errori, errori_seggi, elenco = [], [], []
    for nome, (titolo, voto) in ELEZIONI.items():
        testo = scarica(nome, titolo)
        righe = leggi_tabella(testo, nome, voto.year)
        vero = next(r for r in righe if "EFEFEF" in r["attr"].upper())
        finali = sondaggi_finali(righe, voto)
        elenco.append((nome, vero, finali))
        for p in TENUTI[nome]:
            vals = [r["pct"][p] for r in finali if p in r["pct"]]
            if p not in vero["pct"] or not vals:
                print(f"  {nome} {p}: manca risultato o sondaggi")
                continue
            m = mean(vals)
            errori.append({"elezione": nome, "data_voto": voto.isoformat(), "partito": p,
                           "n_sondaggi": len(vals), "media_sondaggi": round(m, 2),
                           "risultato": vero["pct"][p],
                           "errore": round(vero["pct"][p] - m, 2)})
        # blocchi: partiti del blocco con colonna e risultato in questa elezione
        for b, membri in BLOCCHI.items():
            membri = [p for p in membri if p in vero["seggi"]]   # solo i partiti con una colonna propria
            vals = [sum(r["seggi"][p] for p in membri) for r in finali
                    if all(p in r["seggi"] for p in membri)]
            if not vals:
                continue
            m = mean(vals)
            ris = sum(vero["seggi"][p] for p in membri)
            errori_seggi.append({"elezione": nome, "data_voto": voto.isoformat(), "blocco": b,
                                 "n_sondaggi": len(vals), "media_sondaggi": round(m, 1),
                                 "risultato": ris, "errore": round(ris - m, 1)})

    for percorso, righe, campi in [
            ("errori_storici.csv", errori,
             ["elezione", "data_voto", "partito", "n_sondaggi", "media_sondaggi", "risultato", "errore"]),
            ("errori_storici_seggi.csv", errori_seggi,
             ["elezione", "data_voto", "blocco", "n_sondaggi", "media_sondaggi", "risultato", "errore"])]:
        with open(os.path.join(OUTPUT, percorso), "w", newline="") as f:
            w = csv.DictWriter(f, fieldnames=campi)
            w.writeheader(); w.writerows(righe)

    print("Sondaggi finali per elezione")
    for nome, vero, finali in elenco:
        print(f"\n{nome}: risultato {vero['pct']} seggi {vero['seggi']}")
        for r in sorted(finali, key=lambda r: r["fine"]):
            print(f"  {r['fine']} {r['istituto']:<28}{'[divieto]' if r['divieto'] else '':10}"
                  f"{r['pct']}  seggi {r['seggi']}")

    def rmse(vs):
        return sqrt(mean(v * v for v in vs))

    print("\nErrori in percentuale (risultato - media sondaggi)")
    for r in errori:
        print(f"  {r['elezione']:8}{r['partito']:15}n={r['n_sondaggi']:2} media {r['media_sondaggi']:6.2f} "
              f"risultato {r['risultato']:5.1f} errore {r['errore']:+.2f}")
    print("Scarto quadratico medio per partito (punti)")
    for p in ["PP", "PSOE", "Vox", "Podemos-Sumar", "Ciudadanos", "IU", "Más País"]:
        v = [r["errore"] for r in errori if r["partito"] == p]
        if v:
            print(f"  {p:15}{rmse(v):.2f}  (n={len(v)})")
    print(f"  tutti          {rmse([r['errore'] for r in errori]):.2f}")
    print("\nErrori in seggi")
    for r in errori_seggi:
        print(f"  {r['elezione']:8}{r['blocco']:10}n={r['n_sondaggi']:2} media {r['media_sondaggi']:6.1f} "
              f"risultato {r['risultato']:5.0f} errore {r['errore']:+.1f}")
    print("Scarto quadratico medio per blocco (seggi)")
    for b in BLOCCHI:
        print(f"  {b:10}{rmse([r['errore'] for r in errori_seggi if r['blocco'] == b]):.1f}")


if __name__ == "__main__":
    main()
