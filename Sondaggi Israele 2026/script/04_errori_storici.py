#!/usr/bin/env python3
# ============================================================================
# Errori storici dei sondaggi israeliani: ultimi sondaggi prima del silenzio
# elettorale contro il risultato, per le sei elezioni dal 2015 al 2022.
#
# Fonte: le voci inglesi "Opinion polling for the <elezione>" di Wikipedia
# (input/storici/*.wiki). In ognuna la prima tabella ha in testa la riga del
# risultato, poi gli exit poll, poi la riga del silenzio elettorale e sotto i
# sondaggi in ordine cronologico inverso.
#
# Sondaggi finali: quelli pubblicati negli ultimi 7 giorni prima del silenzio,
# l'ultimo per ogni istituto. Errore = risultato - media dei sondaggi finali,
# in seggi: positivo vuol dire che i sondaggi sottovalutavano la lista.
#
# Le liste cambiano nome e composizione a ogni elezione: si confrontano per
# FAMIGLIA, la stessa ripartizione usata per i sondaggi del 2026:
#   likud, shas, utj, destra_rel (sionismo religioso e destra radicale),
#   yb (Yisrael Beiteinu), arabi, centro_sin (tutto il resto: Yesh Atid,
#   Blu e Bianco, Labour, Meretz, Kulanu, New Hope, Yamina 2021...)
# e per BLOCCO: blocco Netanyahu = likud + shas + utj + destra_rel (le liste
# che lo sostenevano a ogni elezione: nel 2021 Yamina di Bennett non c'e').
#
# Output: output/storici_sondaggi.csv (una riga per sondaggio finale/risultato
#         e famiglia), output/errori_storici.csv, output/errori_liste.csv
#         (errore per singola lista), output/errori_istituti.csv
# ============================================================================

import csv
import importlib.util
import os
import re
from collections import defaultdict
from datetime import date, timedelta
from statistics import mean

BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
spec = importlib.util.spec_from_file_location(
    "scarica", os.path.join(BASE, "script", "01_scarica_sondaggi.py"))
w = importlib.util.module_from_spec(spec); spec.loader.exec_module(w)

ELEZIONI = {
    "2015":       ("2015_Israeli_legislative_election", date(2015, 3, 17)),
    "2019 (apr)": ("April_2019_Israeli_legislative_election", date(2019, 4, 9)),
    "2019 (set)": ("September_2019_Israeli_legislative_election", date(2019, 9, 17)),
    "2020":       ("2020_Israeli_legislative_election", date(2020, 3, 2)),
    "2021":       ("2021_Israeli_legislative_election", date(2021, 3, 23)),
    "2022":       ("2022_Israeli_legislative_election", date(2022, 11, 1)),
}

# lista (come nell'intestazione, ripulita) -> famiglia, per elezione
FAMIGLIA_BASE = {
    "Likud": "likud", "Shas": "shas", "UTJ": "utj",
    "Yisrael Beiteinu": "yb",
    "Joint List": "arabi", "Ra'am": "arabi", "Hadash–Ta'al": "arabi",
    "Balad": "arabi", "Hadash": "arabi", "UAL-Ta'al": "arabi",
    "Ra'am–Balad": "arabi",
    "Jewish Home": "destra_rel", "Yachad": "destra_rel", "Otzma": "destra_rel",
    "Otzma Yehudit": "destra_rel", "URWP": "destra_rel",
    "New Right": "destra_rel", "Zehut": "destra_rel", "Yamina": "destra_rel",
    "Religious Zionist": "destra_rel", "RZP-OY": "destra_rel",
}
ECCEZIONI = {("2021", "Yamina"): "centro_sin"}   # Bennett fuori dal blocco
FAMIGLIE = ["likud", "shas", "utj", "destra_rel", "yb", "arabi", "centro_sin"]
BLOCCO_N = ["likud", "shas", "utj", "destra_rel"]

MESI = {m: i + 1 for i, m in enumerate(
    ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
     "Nov", "Dec"])}


def nome_lista(s):
    s = s.replace("&nbsp;", " ").replace(" –", "–").replace("- ", "-")
    return re.sub(r"\s+", " ", s).strip()


def famiglia(el, lista):
    if (el, lista) in ECCEZIONI:
        return ECCEZIONI[(el, lista)]
    return FAMIGLIA_BASE.get(lista, "centro_sin")


def istituto(s):
    """Nome dell'istituto, uniformato fra le elezioni."""
    t = s.lower()
    for chiave, nome in [("midgam", "Midgam"), ("kantar", "Kantar"),
                         ("tns", "Kantar"), ("maagar", "Maagar Mochot"),
                         ("panels", "Panels Politics (Lazar)"),
                         ("lazar", "Panels Politics (Lazar)"),
                         ("direct", "Direct Polls"), ("smith", "Smith"),
                         ("fuchs", "Camil Fuchs / Panel Project"),
                         ("panel project", "Camil Fuchs / Panel Project"),
                         ("channel 13", "Camil Fuchs / Panel Project"),
                         ("teleseker", "Teleseker"), ("dialog", "Dialog"),
                         ("geocartography", "Geocartography"),
                         ("shvakim", "Shvakim Panorama"),
                         ("channel 2", "Midgam"), ("channel 10", "Dialog"),
                         ("channel 1", "Kantar"), ("reshet bet", "Reshet Bet")]:
        if chiave in t:
            return nome
    return s.split("/")[0].strip()


def data_riga(attr_val, testo, anno):
    m = re.search(r"(\d{4})-(\d{2})-(\d{2})", attr_val)
    if m:
        return date(int(m.group(1)), int(m.group(2)), int(m.group(3)))
    m = re.findall(r"(\d{1,2})\s*([A-Z][a-z]{2})", testo)
    if m:
        g, mm = m[-1]
        return date(anno, MESI[mm], int(g))
    return None


def leggi(el):
    pagina, giorno = ELEZIONI[el]
    testo = open(os.path.join(BASE, "input", "storici", pagina + ".wiki"),
                 encoding="utf-8").read()
    tab = re.findall(r"\{\|.*?\n\|\}", testo, flags=re.S)[0]
    bl = w.blocchi(tab)
    nomi = [nome_lista(n) for n in w.intestazione([c for t, c in bl if t == "h"])]
    # prima colonna di partito: subito dopo data/istituto/editore
    p0 = next(i for i, n in enumerate(nomi) if n == "Likud" or "Likud" in n
              or n.startswith("Blue"))
    righe, silenzio = [], False
    for tipo, celle in bl:
        if tipo == "h":
            continue
        grezze = [w.pulisci(c) for c in celle]
        prime = [w.separa(c) for c in celle]
        etich = " ".join(v for _, _, v in prime[:3]).lower()
        if any(cs >= 10 for _, cs, _ in prime):
            if "silence" in etich or "deadline" in etich or "silen" in grezze[-1].lower():
                silenzio = True
            continue
        # espande i colspan sulle colonne
        valori, col = {}, 0
        for (rs, cs, v), g in zip(prime, grezze):
            nome = "+".join(dict.fromkeys(re.sub(r"#\d+$", "", n) if cs > 1 else n
                                          for n in nomi[col:col + cs]))
            valori[col] = (v, nome, g)
            col += cs
        risultato = (("election resul" in etich or "final results" in etich)
                     and "2013 election" not in etich
                     and not any(r["istituto"] == "Risultato" for r in righe))
        if not risultato and not silenzio:
            continue                    # exit poll, legislatura uscente
        if risultato:
            d, ist = giorno, "Risultato"
        else:
            d = data_riga(valori[0][2], valori[0][0], giorno.year)
            if d and d > giorno:           # date senza anno dell'anno prima
                d = d.replace(year=d.year - 1)
            ist = istituto(" ".join(valori[c][0] for c in (1, 2)
                                    if c in valori and c < p0))
        liste = {}
        for c, (v, nome, _) in valori.items():
            if c < p0 or nome in ("Gov.", "Opp.", "L", "R", "Others") \
                    or nome.startswith("2013"):
                continue
            x = w.numero(v)
            sotto = v.startswith("(")
            liste[nome] = 0.0 if (sotto or x is None) else x
        righe.append({"data": d, "istituto": ist, "liste": liste})
    return righe


def main():
    sond_out, err_out, ist_out, liste_out = [], [], [], []
    per_ist = defaultdict(list)          # istituto -> errori sul blocco N
    for el in ELEZIONI:
        righe = leggi(el)
        ris = next(r for r in righe if r["istituto"] == "Risultato")
        polls = [r for r in righe if r["istituto"] != "Risultato" and r["data"]]
        ultimo = max(r["data"] for r in polls)
        finali, visti = [], set()
        for r in polls:                    # ordine inverso: il primo e' l'ultimo
            if r["data"] >= ultimo - timedelta(days=7) and r["istituto"] not in visti:
                visti.add(r["istituto"]); finali.append(r)

        def per_famiglia(liste):
            out = defaultdict(float)
            for nome, s in liste.items():
                # liste congiunte di colonne di famiglie diverse non ci sono:
                # si usa la famiglia della prima
                out[famiglia(el, nome.split("+")[0])] += s
            out["blocco_netanyahu"] = sum(out[f] for f in BLOCCO_N)
            return out

        f_ris = per_famiglia(ris["liste"])
        f_pol = [per_famiglia(r["liste"]) for r in finali]
        for r, fp in [(ris, f_ris)] + list(zip(finali, f_pol)):
            for f in FAMIGLIE + ["blocco_netanyahu"]:
                sond_out.append({"elezione": el, "istituto": r["istituto"],
                                 "data": r["data"], "famiglia": f,
                                 "seggi": round(fp[f], 1)})
        for f in FAMIGLIE + ["blocco_netanyahu"]:
            m = mean(fp[f] for fp in f_pol)
            err_out.append({"elezione": el, "famiglia": f, "sondaggi": len(f_pol),
                            "media_sondaggi": round(m, 2),
                            "risultato": f_ris[f], "errore": round(f_ris[f] - m, 2)})
        # errore per singola lista, solo dove risultato e sondaggi la riportano
        # con la stessa colonna (le liste congiunte restano tali)
        for nome, seggi_ris in ris["liste"].items():
            valori = [r["liste"][nome] for r in finali if nome in r["liste"]]
            if not valori or (seggi_ris == 0 and mean(valori) < 1):
                continue
            m = mean(valori)
            liste_out.append({"elezione": el, "lista": nome,
                              "famiglia": famiglia(el, nome.split("+")[0]),
                              "sondaggi": len(valori), "media_sondaggi": round(m, 2),
                              "risultato": seggi_ris, "errore": round(seggi_ris - m, 2)})
        for r, fp in zip(finali, f_pol):
            per_ist[r["istituto"]].append(
                (el, f_ris["blocco_netanyahu"] - fp["blocco_netanyahu"],
                 f_ris["likud"] - fp["likud"], f_ris["arabi"] - fp["arabi"]))
        print(f"{el}: {len(finali)} sondaggi finali ({', '.join(r['istituto'] for r in finali)})")

    for ist, e in sorted(per_ist.items(), key=lambda x: -len(x[1])):
        ist_out.append({"istituto": ist, "elezioni": len(e),
                        "errore_blocco_netanyahu": round(mean(x[1] for x in e), 2),
                        "errore_assoluto_blocco": round(mean(abs(x[1]) for x in e), 2),
                        "errore_likud": round(mean(x[2] for x in e), 2),
                        "errore_arabi": round(mean(x[3] for x in e), 2),
                        "dettaglio": "; ".join(f"{x[0]} {x[1]:+.0f}" for x in e)})

    for nome, righe, campi in [
            ("storici_sondaggi.csv", sond_out, ["elezione", "istituto", "data", "famiglia", "seggi"]),
            ("errori_storici.csv", err_out, ["elezione", "famiglia", "sondaggi", "media_sondaggi", "risultato", "errore"]),
            ("errori_liste.csv", liste_out, ["elezione", "lista", "famiglia", "sondaggi", "media_sondaggi", "risultato", "errore"]),
            ("errori_istituti.csv", ist_out, ["istituto", "elezioni", "errore_blocco_netanyahu", "errore_assoluto_blocco", "errore_likud", "errore_arabi", "dettaglio"])]:
        with open(os.path.join(BASE, "output", nome), "w", newline="") as f:
            wr = csv.DictWriter(f, fieldnames=campi); wr.writeheader(); wr.writerows(righe)

    print("\nErrore medio per famiglia (risultato - sondaggi, seggi):")
    for f in FAMIGLIE + ["blocco_netanyahu"]:
        e = [r["errore"] for r in err_out if r["famiglia"] == f]
        print(f"  {f:17s} media {mean(e):+5.2f}  " + "  ".join(f"{x:+5.1f}" for x in e))


if __name__ == "__main__":
    main()
