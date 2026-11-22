"""
Redditi IRPEF per comune (MEF - Dipartimento delle Finanze, open data).

Scarica i file "Redditi e principali variabili IRPEF su base comunale" per tutti
gli anni d'imposta dal 2012 all'ultimo pubblicato, estrae le grandi città e il
totale Italia e calcola:
  - reddito complessivo medio (ammontare / frequenza del reddito complessivo,
    la stessa definizione dei comunicati MEF) e, in alternativa, per contribuente;
  - reddito imponibile medio (ammontare / frequenza del reddito imponibile);
  - reddito da lavoro dipendente e da pensione medi (ammontare / frequenza);
  - reddito netto stimato medio = (reddito complessivo - imposta netta
    - addizionale regionale - addizionale comunale) / frequenza del reddito
    complessivo; una variante aggiunge il bonus 80-100 euro / trattamento
    integrativo (colonna presente nei file dall'anno d'imposta 2015);
  - reddito mediano stimato per interpolazione lineare entro la classe di
    reddito complessivo che contiene la metà dei contribuenti.

Fonti per il totale Italia:
  - contribuenti, imponibile, imposta netta, addizionali, lavoro dipendente,
    pensione, bonus: somma di tutte le righe del file comunale (compresa la
    riga "Mancante/errata"), che coincide con il dato nazionale ufficiale;
  - reddito complessivo (totale e classi):
      * dal 2016: file nazionale per classi di reddito e sesso
        (sesso_calcolo_irpef_<anno dichiarazione>.csv), senza oscuramenti;
      * 2012-2015: somma delle classi dei comuni (le celle oscurate per
        segreto statistico nei comuni piccoli mancano: scarto di circa -0,1%
        sul reddito medio rispetto al dato ufficiale MEF).
    Il mediano "classi fini" (solo Italia, dal 2016) usa le 33 classi
    nazionali e serve a misurare l'errore dell'interpolazione sulle 8 classi.

Uscite (cartella ../input):
  - redditi_comuni_irpef.csv  : una riga per città e anno
  - redditi_comuni_classi.csv : distribuzione per classi di reddito complessivo
"""

import csv
import io
import re
import urllib.request
import zipfile
from pathlib import Path

import pandas as pd

BASE = "https://www1.finanze.gov.it/finanze/analisi_stat/public/"
PAGINA_COMUNALE = BASE + "index.php?search_class%5B0%5D=cCOMUNE&opendata=yes"
URL_COMUNALE = (BASE + "v_4_0_0/contenuti/"
                "Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_{anno}.zip?d=1615465800")
# Il file nazionale è indicizzato per anno di dichiarazione (= anno d'imposta + 1)
URL_NAZIONALE = BASE + "v_4_0_0/contenuti/sesso_calcolo_irpef_{anno_dich}.csv?d=1615465800"

ANNO_MIN = 2012
OUT_DIR = Path(__file__).resolve().parent.parent / "input"

# Codice catastale (stabile nel tempo) -> nome usato nei CSV
CITTA = {
    "F205": "Milano", "H501": "Roma", "L219": "Torino", "F839": "Napoli",
    "A944": "Bologna", "D612": "Firenze", "L736": "Venezia", "G224": "Padova",
    "D969": "Genova", "A662": "Bari", "G273": "Palermo", "L781": "Verona",
    "L378": "Trento",
}

# Classi di reddito complessivo dei file comunali: chiave, etichetta, limiti
CLASSI = [
    ("cl_le0", "minore o uguale a 0", None, 0),
    ("cl_0_10", "da 0 a 10000", 0, 10000),
    ("cl_10_15", "da 10000 a 15000", 10000, 15000),
    ("cl_15_26", "da 15000 a 26000", 15000, 26000),
    ("cl_26_55", "da 26000 a 55000", 26000, 55000),
    ("cl_55_75", "da 55000 a 75000", 55000, 75000),
    ("cl_75_120", "da 75000 a 120000", 75000, 120000),
    ("cl_120", "oltre 120000", 120000, None),
]

# Valori ufficiali per il controllo finale (reddito complessivo medio, euro)
# Italia: "Analisi dei dati IRPEF" del MEF per anno d'imposta
# (v_4_0_0/contenuti/analisi_dati_<anno>_irpef.pdf); Milano: portale open data
# del Comune di Milano (indicatori economia e finanze, fonte MEF).
CONTROLLI = {
    ("Italia", 2012): 19747, ("Italia", 2013): 20070, ("Italia", 2014): 20320,
    ("Italia", 2015): 20690, ("Italia", 2016): 20940, ("Italia", 2024): 25820,
    ("Milano", 2023): 40521, ("Milano", 2024): 41865,
}


def scarica(url):
    """Scarica un file in memoria."""
    req = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0"})
    with urllib.request.urlopen(req, timeout=120) as r:
        return r.read()


def anni_disponibili():
    """Anni d'imposta dei file comunali CSV elencati nella pagina open data."""
    try:
        html = scarica(PAGINA_COMUNALE).decode("utf-8", errors="ignore")
        anni = sorted({int(a) for a in re.findall(
            r"su_base_comunale_CSV_(\d{4})\.zip", html)})
    except Exception as e:  # pagina non raggiungibile: si prova per tentativi
        print("Pagina open data non leggibile:", e)
        anni = list(range(ANNO_MIN, 2031))
    return [a for a in anni if a >= ANNO_MIN]


def chiave_colonna(nome):
    """Riconduce le intestazioni (cambiate negli anni) a nomi standard."""
    n = re.sub(r"\s+", " ", nome.strip())
    if n == "Anno di imposta":
        return "anno"
    if n == "Codice catastale":
        return "codice_catastale"
    if n == "Codice Istat Comune":
        return "codice_istat"
    if n == "Denominazione Comune":
        return "nome_file"
    if n == "Numero contribuenti":
        return "contribuenti"
    m = re.match(r"(.+?) - (Frequenza|Ammontare)", n)
    if not m:
        return None
    var, tipo = m.group(1), "f" if m.group(2) == "Frequenza" else "a"
    mappa = [
        (r"^Reddito da lavoro dipendente", "lav_dip"),
        (r"^Reddito da pensione", "pensione"),
        (r"^Reddito imponibile addizionale", "imponibile_add"),
        (r"^Reddito imponibile$", "imponibile"),
        (r"^Imposta netta", "imposta_netta"),
        (r"^(Bonus|Trattamento) spettante", "bonus"),
        (r"^Addizionale regionale", "add_reg"),
        (r"^Addizionale comunale", "add_com"),
        (r"^Reddito complessivo$", "rc"),
        (r"^Reddito complessivo minore (o uguale )?(di|a) zero", "cl_le0"),
        (r"^Reddito complessivo da 0 a 10000", "cl_0_10"),
        (r"^Reddito complessivo da 10000 a 15000", "cl_10_15"),
        (r"^Reddito complessivo da 15000 a 26000", "cl_15_26"),
        (r"^Reddito complessivo da 26000 a 55000", "cl_26_55"),
        (r"^Reddito complessivo da 55000 a 75000", "cl_55_75"),
        (r"^Reddito complessivo da 75000 a 120000", "cl_75_120"),
        (r"^Reddito complessivo oltre 120000", "cl_120"),
    ]
    for pat, k in mappa:
        if re.search(pat, var):
            return f"{k}_{tipo}"
    return None


def leggi_comunale(anno):
    """Legge il CSV comunale di un anno (dentro lo zip) in un DataFrame."""
    z = zipfile.ZipFile(io.BytesIO(scarica(URL_COMUNALE.format(anno=anno))))
    nome = [n for n in z.namelist() if n.lower().endswith(".csv")][0]
    testo = z.read(nome).decode("utf-8-sig", errors="replace")
    righe = list(csv.reader(io.StringIO(testo), delimiter=";"))
    intest = righe[0]
    chiavi = [chiave_colonna(c) for c in intest]
    dati = []
    for r in righe[1:]:
        if not r or not any(x.strip() for x in r):
            continue
        r = (r + [""] * len(intest))[:len(intest)]  # il ";" finale non sposta le colonne
        dati.append({k: v.strip() for k, v in zip(chiavi, r) if k})
    df = pd.DataFrame(dati)
    num = [c for c in df.columns if c not in ("codice_catastale", "codice_istat", "nome_file")]
    for c in num:
        df[c] = pd.to_numeric(df[c].replace("", None), errors="coerce")
    df["codice_istat"] = df["codice_istat"].str.strip('"').str.zfill(6)
    return df


def leggi_nazionale(anno):
    """File nazionale per classi fini di reddito complessivo (somma dei due sessi)."""
    raw = scarica(URL_NAZIONALE.format(anno_dich=anno + 1)).decode("latin-1")
    df = pd.read_csv(io.StringIO(raw), sep=";", dtype=str)
    df.columns = [c.strip() for c in df.columns]
    classe = df.columns[0]
    for c in ["Reddito complessivo - Frequenza", "Reddito complessivo - Ammontare in euro"]:
        df[c] = pd.to_numeric(df[c].str.replace(".", "", regex=False), errors="coerce")
    g = (df.groupby(classe, sort=False)[["Reddito complessivo - Frequenza",
                                          "Reddito complessivo - Ammontare in euro"]]
         .sum().reset_index())
    g.columns = ["classe", "f", "a"]

    def limiti(lbl):
        x = lbl.replace(".", "")
        if x.startswith("minore di"):
            return None, float(x.split()[-1])
        if x == "zero":
            return 0.0, 0.0
        if x.startswith("oltre"):
            return float(x.split()[-1]), None
        lo, hi = re.findall(r"-?\d+", x)
        return float(lo), float(hi)

    g[["lo", "hi"]] = g["classe"].apply(lambda s: pd.Series(limiti(s)))
    return g


def mediano(classi):
    """
    Mediano per interpolazione lineare: si individua la classe in cui la
    frequenza cumulata supera la metà del totale e si assume che i contribuenti
    siano distribuiti uniformemente al suo interno:
        mediano = L + (N/2 - F_prec) / f_classe * (U - L)
    classi: lista di (lo, hi, frequenza) ordinate.
    """
    tot = sum(f for _, _, f in classi if f == f)
    meta, cum = tot / 2, 0.0
    for lo, hi, f in classi:
        if f != f or f <= 0:
            continue
        if cum + f >= meta:
            if lo is None or hi is None:
                return float("nan")  # la metà cade in una classe aperta
            return lo + (meta - cum) / f * (hi - lo)
        cum += f
    return float("nan")


def indicatori(r):
    """Calcola le medie a partire dagli ammontari e dalle frequenze."""
    out = dict(r)
    div = lambda a, b: a / b if b and b == b and a == a else float("nan")
    out["reddito_complessivo_medio"] = div(r["ammontare_reddito_complessivo"], r["freq_reddito_complessivo"])
    out["reddito_complessivo_medio_per_contribuente"] = div(r["ammontare_reddito_complessivo"], r["contribuenti"])
    out["reddito_imponibile_medio"] = div(r["ammontare_reddito_imponibile"], r["freq_reddito_imponibile"])
    netto = (r["ammontare_reddito_complessivo"] - r["ammontare_imposta_netta"]
             - r["ammontare_addizionale_regionale"] - r["ammontare_addizionale_comunale"])
    out["reddito_netto_stimato_medio"] = div(netto, r["freq_reddito_complessivo"])
    out["reddito_netto_stimato_medio_con_bonus"] = div(netto + r["ammontare_bonus_trattamento"],
                                                       r["freq_reddito_complessivo"])
    out["incidenza_imposte_pct"] = 100 * (1 - div(netto, r["ammontare_reddito_complessivo"]))
    out["reddito_lav_dip_medio"] = div(r["ammontare_lavoro_dipendente"], r["freq_lavoro_dipendente"])
    out["reddito_pensione_medio"] = div(r["ammontare_pensione"], r["freq_pensione"])
    return out


def riga_base(df_sel, anno):
    """Somma delle variabili 'semplici' sulle righe selezionate."""
    s = lambda c: df_sel[c].sum(min_count=1) if c in df_sel else float("nan")
    return {
        "anno_imposta": anno,
        "contribuenti": s("contribuenti"),
        "freq_reddito_imponibile": s("imponibile_f"),
        "ammontare_reddito_imponibile": s("imponibile_a"),
        "freq_imposta_netta": s("imposta_netta_f"),
        "ammontare_imposta_netta": s("imposta_netta_a"),
        "ammontare_addizionale_regionale": s("add_reg_a"),
        "ammontare_addizionale_comunale": s("add_com_a"),
        "freq_bonus_trattamento": s("bonus_f"),
        "ammontare_bonus_trattamento": s("bonus_a"),
        "freq_lavoro_dipendente": s("lav_dip_f"),
        "ammontare_lavoro_dipendente": s("lav_dip_a"),
        "freq_pensione": s("pensione_f"),
        "ammontare_pensione": s("pensione_a"),
    }


def main():
    anni = anni_disponibili()
    righe, classi_out = [], []
    for anno in anni:
        try:
            df = leggi_comunale(anno)
        except Exception as e:
            print(f"{anno}: file comunale non disponibile ({e})")
            continue
        print(f"{anno}: {len(df)} righe comunali")

        # --- città ---
        for cat, nome in CITTA.items():
            sel = df[df["codice_catastale"] == cat]
            assert len(sel) == 1, (anno, nome, len(sel))
            x = sel.iloc[0]
            cl = [(k, lbl, lo, hi, x[f"{k}_f"], x[f"{k}_a"]) for k, lbl, lo, hi in CLASSI]
            assert all(f == f for *_, f, _ in cl), f"classe oscurata: {nome} {anno}"
            f_cl = sum(c[4] for c in cl)
            a_cl = sum(c[5] for c in cl)
            if "rc_f" in df and x["rc_f"] == x["rc_f"]:
                # dal 2023 il totale è nel file: deve coincidere con la somma delle classi
                assert abs(x["rc_f"] - f_cl) < 1 and abs(x["rc_a"] - a_cl) < 1, (nome, anno)
            r = riga_base(sel, anno)
            r.update(comune=nome, codice_istat=x["codice_istat"], codice_catastale=cat,
                     freq_reddito_complessivo=f_cl, ammontare_reddito_complessivo=a_cl,
                     reddito_mediano_stimato=mediano([(lo, hi, f) for *_, lo, hi, f, _ in cl]),
                     reddito_mediano_stimato_classi_fini=float("nan"),
                     fonte_reddito_complessivo="file comunale MEF")
            righe.append(indicatori(r))
            for k, lbl, lo, hi, f, a in cl:
                classi_out.append(dict(anno_imposta=anno, comune=nome, codice_istat=x["codice_istat"],
                                       classe=lbl, classe_min=lo, classe_max=hi,
                                       frequenza=f, ammontare=a, quota_pct=100 * f / f_cl,
                                       reddito_medio_classe=a / f if f else float("nan"),
                                       fonte="file comunale MEF"))

        # --- Italia ---
        r = riga_base(df, anno)
        cl_com = [(k, lbl, lo, hi, df[f"{k}_f"].sum(), df[f"{k}_a"].sum()) for k, lbl, lo, hi in CLASSI]
        naz = None
        try:
            naz = leggi_nazionale(anno)
        except Exception:
            pass
        if naz is not None:
            # classi fini nazionali riaggregate nelle 8 classi comunali
            cl = []
            for k, lbl, lo, hi in CLASSI:
                if lo is None:
                    m = naz["hi"].notna() & (naz["hi"] <= 0)
                elif hi is None:
                    m = naz["lo"] >= lo
                else:
                    m = (naz["lo"] >= lo) & naz["hi"].notna() & (naz["hi"] <= hi) & (naz["hi"] > 0)
                cl.append((k, lbl, lo, hi, naz.loc[m, "f"].sum(), naz.loc[m, "a"].sum()))
            assert abs(sum(c[4] for c in cl) - naz["f"].sum()) < 1
            fonte = f"file nazionale MEF sesso_calcolo_irpef_{anno + 1}"
            fini = [(lo if lo == lo else None, hi if hi == hi else None, f)
                    for lo, hi, f in naz[["lo", "hi", "f"]].itertuples(index=False)]
            med_fini = mediano(fini)
            if "rc_f" in df:  # controllo incrociato con il totale del file comunale
                assert abs(df["rc_f"].sum() - naz["f"].sum()) < 1
        else:
            cl = cl_com
            fonte = "somma delle classi dei comuni (celle oscurate escluse)"
            med_fini = float("nan")
        f_cl = sum(c[4] for c in cl)
        a_cl = sum(c[5] for c in cl)
        r.update(comune="Italia", codice_istat="", codice_catastale="",
                 freq_reddito_complessivo=f_cl, ammontare_reddito_complessivo=a_cl,
                 reddito_mediano_stimato=mediano([(lo, hi, f) for *_, lo, hi, f, _ in cl]),
                 reddito_mediano_stimato_classi_fini=med_fini,
                 fonte_reddito_complessivo=fonte)
        righe.append(indicatori(r))
        for k, lbl, lo, hi, f, a in cl:
            classi_out.append(dict(anno_imposta=anno, comune="Italia", codice_istat="",
                                   classe=lbl, classe_min=lo, classe_max=hi,
                                   frequenza=f, ammontare=a, quota_pct=100 * f / f_cl,
                                   reddito_medio_classe=a / f if f else float("nan"),
                                   fonte=fonte))

    # --- salvataggio ---
    colonne = [
        "anno_imposta", "comune", "codice_istat", "codice_catastale", "contribuenti",
        "freq_reddito_complessivo", "ammontare_reddito_complessivo",
        "reddito_complessivo_medio", "reddito_complessivo_medio_per_contribuente",
        "reddito_mediano_stimato", "reddito_mediano_stimato_classi_fini",
        "freq_reddito_imponibile", "ammontare_reddito_imponibile", "reddito_imponibile_medio",
        "freq_imposta_netta", "ammontare_imposta_netta",
        "ammontare_addizionale_regionale", "ammontare_addizionale_comunale",
        "reddito_netto_stimato_medio", "incidenza_imposte_pct",
        "freq_bonus_trattamento", "ammontare_bonus_trattamento",
        "reddito_netto_stimato_medio_con_bonus",
        "freq_lavoro_dipendente", "ammontare_lavoro_dipendente", "reddito_lav_dip_medio",
        "freq_pensione", "ammontare_pensione", "reddito_pensione_medio",
        "fonte_reddito_complessivo",
    ]
    out = pd.DataFrame(righe)[colonne].sort_values(["comune", "anno_imposta"])
    # il bonus non è nei file 2012-2014: la variante "con bonus" resta vuota
    out.loc[out["freq_bonus_trattamento"].isna(), "reddito_netto_stimato_medio_con_bonus"] = float("nan")
    medie = [c for c in colonne if c.endswith(("_medio", "_stimato", "_fini", "_contribuente", "_bonus"))
             and not c.startswith(("freq", "ammontare"))]
    out[medie] = out[medie].round(0)
    out["incidenza_imposte_pct"] = out["incidenza_imposte_pct"].round(2)
    interi = [c for c in colonne if c.startswith(("freq_", "ammontare_")) or c == "contribuenti"]
    out[interi] = out[interi].round(0).astype("Int64")
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    out.to_csv(OUT_DIR / "redditi_comuni_irpef.csv", index=False)

    cls = pd.DataFrame(classi_out).sort_values(["comune", "anno_imposta"], kind="stable")
    cls["classe_min"] = cls["classe_min"].astype("Int64")
    cls["classe_max"] = cls["classe_max"].astype("Int64")
    cls[["frequenza", "ammontare"]] = cls[["frequenza", "ammontare"]].round(0).astype("Int64")
    cls["quota_pct"] = cls["quota_pct"].round(2)
    cls["reddito_medio_classe"] = cls["reddito_medio_classe"].round(0)
    cls.to_csv(OUT_DIR / "redditi_comuni_classi.csv", index=False)
    print(f"Salvati {len(out)} righe e {len(cls)} righe di classi in {OUT_DIR}")

    # --- controlli contro i valori ufficiali ---
    print("\nControllo reddito complessivo medio (calcolato vs ufficiale):")
    for (citta, anno), uff in CONTROLLI.items():
        v = out.loc[(out["comune"] == citta) & (out["anno_imposta"] == anno), "reddito_complessivo_medio"]
        if len(v):
            print(f"  {citta} {anno}: {v.iloc[0]:,.0f} vs {uff:,} ({100 * (v.iloc[0] / uff - 1):+.2f}%)")


if __name__ == "__main__":
    main()
