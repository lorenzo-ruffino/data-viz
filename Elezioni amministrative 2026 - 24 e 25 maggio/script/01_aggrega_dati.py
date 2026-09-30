"""Aggrega politiche 2022 e referendum 2026 per comune,
combina con l'elenco dei comuni al voto del 24-25 maggio 2026
e produce CSV + markdown di analisi."""

import csv
import re
import unicodedata
from collections import defaultdict, Counter
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
INP = ROOT / "input"
OUT = ROOT / "output"

# Politiche 2022: definizione coalizioni
CDX = {
    "FRATELLI D'ITALIA CON GIORGIA MELONI",
    "LEGA PER SALVINI PREMIER",
    "FORZA ITALIA",
    "NOI MODERATI/LUPI - TOTI - BRUGNARO - UDC",
}
CSX = {
    "PARTITO DEMOCRATICO - ITALIA DEMOCRATICA E PROGRESSISTA",
    "ALLEANZA VERDI E SINISTRA",
    "+EUROPA",
    "IMPEGNO CIVICO LUIGI DI MAIO - CENTRO DEMOCRATICO",
}
M5S = {"MOVIMENTO 5 STELLE"}

# Regioni → macro-area
NORD = {"PIEMONTE","VALLE D'AOSTA","LOMBARDIA","TRENTINO-ALTO ADIGE",
        "VENETO","FRIULI-VENEZIA GIULIA","LIGURIA","EMILIA ROMAGNA"}
CENTRO_REG = {"TOSCANA","UMBRIA","MARCHE","LAZIO"}
MEZZOGIORNO = {"ABRUZZO","MOLISE","CAMPANIA","PUGLIA","BASILICATA",
               "CALABRIA","SICILIA","SARDEGNA"}

def macro_area(regione):
    r = regione.upper().replace("EMILIA-ROMAGNA","EMILIA ROMAGNA")
    if r in NORD: return "Nord"
    if r in CENTRO_REG: return "Centro"
    if r in MEZZOGIORNO: return "Mezzogiorno"
    return "?"

def norm_istat(s):
    return re.sub(r"\D", "", str(s)).zfill(6) if s else ""

# 1) Carica anagrafica politiche 2022: codice → ISTAT
codice_to_istat = {}
with open(INP / "politiche2022_anagrafica.csv", encoding="utf-8") as f:
    r = csv.DictReader(f)
    for row in r:
        codice_to_istat[row["codice"]] = norm_istat(row["CODICE ISTAT"])

# 2) Aggrega voti politiche 2022 per ISTAT (CDX/CSX/M5S/altri/totale)
politiche = defaultdict(lambda: {"cdx":0,"csx":0,"m5s":0,"altri":0,"totale":0})
with open(INP / "politiche2022_camera.csv", encoding="utf-8") as f:
    r = csv.DictReader(f)
    for row in r:
        istat = codice_to_istat.get(row["codice"])
        if not istat: continue
        voti = int(row["voti"])
        lista = row["desc_lis"]
        # Stesso candidato può comparire con più liste collegate: i voti di lista
        # sono per la singola lista. Quindi sommiamo.
        p = politiche[istat]
        if lista in CDX:
            p["cdx"] += voti
        elif lista in CSX:
            p["csx"] += voti
        elif lista in M5S:
            p["m5s"] += voti
        else:
            p["altri"] += voti
        p["totale"] += voti

# NOTE: il file politiche è strutturato per candidato uninominale, e i voti
# delle liste possono ripetersi (stessa lista, stessa colonna del candidato).
# In realtà ogni riga è una coppia (candidato, lista) UNICA, ma per i collegi
# uninominali plurinominali può ripetersi tra candidati. Verifichiamo:
# Il valore "voti" è il voto della LISTA per quel candidato uninominale.
# Sommarli per comune è giusto (somma di tutti i candidati = totale lista nel comune).
# Verifica: nell'anagrafica abbiamo tot_vot_prop (proporzionali) per comune.

# 3) Carica referendum 2026
referendum = {}
with open(INP / "referendum2026.csv", encoding="utf-8") as f:
    r = csv.DictReader(f)
    for row in r:
        istat = norm_istat(row["cod_istat"])
        try:
            si = int(row["voti_si"]) if row["voti_si"] else 0
            no = int(row["voti_no"]) if row["voti_no"] else 0
        except ValueError:
            continue
        referendum[istat] = {
            "si": si, "no": no,
            "bianche": int(row["sk_bianche"] or 0),
            "nulle": int(row["sk_nulle"] or 0),
            "elettori": int(row["ele_t"] or 0),
            "votanti": int(row["vot_t"] or 0),
        }

# 4) Carica elenco comuni al voto 24-25 maggio 2026
voto_2026 = {}  # istat -> row
with open(INP / "comuni_al_voto_2026.csv", encoding="utf-8") as f:
    r = csv.DictReader(f)
    for row in r:
        if "24-25 maggio" in (row.get("data_elezione") or ""):
            voto_2026[norm_istat(row["codice_istat"])] = row

print(f"Politiche aggregate: {len(politiche)} comuni")
print(f"Referendum disponibili: {len(referendum)} comuni")
print(f"Comuni al voto 24-25 maggio: {len(voto_2026)}")

# 5) Costruisci anagrafica completa (istat → regione, comune, sigla)
anagrafica = {}
with open(INP / "politiche2022_anagrafica.csv", encoding="utf-8") as f:
    r = csv.DictReader(f)
    for row in r:
        istat = norm_istat(row["CODICE ISTAT"])
        anagrafica[istat] = {
            "regione": row["desc_circ"].split(" - ")[0].split(" ")[0],  # placeholder
            "comune": row["desc_com"],
            "provincia": row["desc_prov"],
            "codice": row["codice"],
            "circ": row["desc_circ"],
        }
# Better: leverage referendum CSV which has desc_regione
with open(INP / "referendum2026.csv", encoding="utf-8") as f:
    r = csv.DictReader(f)
    for row in r:
        istat = norm_istat(row["cod_istat"])
        if istat in anagrafica:
            anagrafica[istat]["regione"] = row["desc_regione"]
        else:
            anagrafica[istat] = {"regione": row["desc_regione"],
                                 "comune": row["desc_comune"],
                                 "provincia": row["desc_provincia"]}

# 6) Italia totals (somme assolute). 'n_pol' e 'n_ref' contano i comuni
# effettivamente presenti in ciascun dataset (i due possono divergere a
# causa di nuovi codici ISTAT della Sardegna, Valle d'Aosta esclusa
# dalle politiche 2022, fusioni post-2022).
def somma(istats, data=None):
    s = {"cdx":0,"csx":0,"m5s":0,"altri":0,"totale":0,
         "si":0,"no":0,"bianche":0,"nulle":0,
         "elettori_ref":0,"votanti_ref":0,
         "n":len(istats), "n_pol":0, "n_ref":0}
    for istat in istats:
        if istat in politiche:
            for k in ("cdx","csx","m5s","altri","totale"):
                s[k] += politiche[istat][k]
            s["n_pol"] += 1
        if istat in referendum:
            ref = referendum[istat]
            s["si"] += ref["si"]; s["no"] += ref["no"]
            s["bianche"] += ref["bianche"]; s["nulle"] += ref["nulle"]
            s["elettori_ref"] += ref["elettori"]; s["votanti_ref"] += ref["votanti"]
            s["n_ref"] += 1
    return s

def perc(num, den):
    return 100.0 * num / den if den else 0.0

# 7) Output CSV (tutti i comuni italiani con flag voto)
out_csv = OUT / "comuni_voti_2022_referendum_2026.csv"
out_csv.parent.mkdir(parents=True, exist_ok=True)
all_istats = sorted(set(list(politiche.keys()) + list(referendum.keys())))
with open(out_csv, "w", encoding="utf-8", newline="") as f:
    w = csv.writer(f)
    w.writerow(["codice_istat","regione","provincia","comune",
                "voti_cdx_2022","voti_csx_2022","voti_m5s_2022","voti_altri_2022","voti_totali_2022",
                "voti_si_ref2026","voti_no_ref2026","schede_bianche_ref","schede_nulle_ref",
                "elettori_ref","votanti_ref",
                "voto_24_25_maggio_2026"])
    for istat in all_istats:
        a = anagrafica.get(istat, {})
        p = politiche.get(istat, {})
        ref = referendum.get(istat, {})
        flag = "SI" if istat in voto_2026 else "NO"
        w.writerow([istat, a.get("regione",""), a.get("provincia",""), a.get("comune",""),
                    p.get("cdx",""), p.get("csx",""), p.get("m5s",""), p.get("altri",""), p.get("totale",""),
                    ref.get("si",""), ref.get("no",""), ref.get("bianche",""), ref.get("nulle",""),
                    ref.get("elettori",""), ref.get("votanti",""),
                    flag])
print(f"CSV scritto: {out_csv}")

# 8) Aggregazioni per il markdown

# (a) Comuni al voto 24-25 maggio vs Italia totale
istats_voto = set(voto_2026.keys())
istats_italia = set(all_istats)
istats_no_voto = istats_italia - istats_voto

tot_voto = somma(istats_voto, None)
tot_italia = somma(istats_italia, None)
tot_no_voto = somma(istats_no_voto, None)

# (b) SUP vs INF (solo tra i comuni al voto)
sup_istats = {istat for istat, row in voto_2026.items() if row["tipologia"]=="SUP"}
inf_istats = {istat for istat, row in voto_2026.items() if row["tipologia"]=="INF"}
tot_sup = somma(sup_istats, None)
tot_inf = somma(inf_istats, None)

# (c) Per coalizione del sindaco uscente (solo SUP)
coal_groups = defaultdict(set)
for istat, row in voto_2026.items():
    if row["tipologia"]=="SUP":
        coal_groups[row["coalizione"]].add(istat)
coal_totali = {c: somma(s, None) for c, s in coal_groups.items()}

# (d) Per macro-area (tutti i comuni al voto)
area_groups = defaultdict(set)
for istat, row in voto_2026.items():
    area_groups[macro_area(row["regione"])].add(istat)
area_totali = {a: somma(s, None) for a, s in area_groups.items()}

# Per il confronto, anche Italia per macro-area
area_italia = defaultdict(set)
for istat in istats_italia:
    a = anagrafica.get(istat, {})
    area_italia[macro_area(a.get("regione",""))].add(istat)
area_italia_totali = {a: somma(s, None) for a, s in area_italia.items()}

# 9) Genera markdown
def fmt_pct(num, den):
    return f"{perc(num,den):.1f}%" if den else "–"
def fmt_int(n):
    return f"{n:,}".replace(",",".")

md = []
md.append("# Comuni al voto del 24-25 maggio 2026 — risultati elettorali precedenti\n")
md.append("Confronto tra i risultati delle politiche 2022 (Camera) e del referendum giustizia 2026 nei comuni che andranno al voto il 24-25 maggio 2026, rispetto al complesso dell'Italia.\n")
md.append(f"Comuni al voto 24-25 maggio: **{len(voto_2026)}**.\n")

md.append("## 1. Comuni al voto 24-25 maggio vs Italia\n")
md.append("### Politiche 2022 (% sui voti validi alle liste)\n")
md.append("| Gruppo | n. comuni | CDX | CSX | M5S | Altri |")
md.append("|---|---:|---:|---:|---:|---:|")
for label, t in [("Comuni al voto 24-25 maggio", tot_voto),
                 ("Italia (esclusi comuni al voto)", tot_no_voto),
                 ("Italia (totale)", tot_italia)]:
    md.append(f"| {label} | {fmt_int(t['n_pol'])} | {fmt_pct(t['cdx'],t['totale'])} | {fmt_pct(t['csx'],t['totale'])} | {fmt_pct(t['m5s'],t['totale'])} | {fmt_pct(t['altri'],t['totale'])} |")

md.append("\n### Referendum giustizia 2026 (% sui voti validi)\n")
md.append("| Gruppo | n. comuni | SI | NO | Affluenza |")
md.append("|---|---:|---:|---:|---:|")
for label, t in [("Comuni al voto 24-25 maggio", tot_voto),
                 ("Italia (esclusi comuni al voto)", tot_no_voto),
                 ("Italia (totale)", tot_italia)]:
    validi = t["si"] + t["no"]
    md.append(f"| {label} | {fmt_int(t['n_ref'])} | {fmt_pct(t['si'],validi)} | {fmt_pct(t['no'],validi)} | {fmt_pct(t['votanti_ref'],t['elettori_ref'])} |")

md.append("\n## 2. Comuni superiori (SUP, >15.000 ab.) vs comuni inferiori (INF)\n")
md.append("### Politiche 2022\n")
md.append("| Tipologia | n. comuni | CDX | CSX | M5S | Altri |")
md.append("|---|---:|---:|---:|---:|---:|")
for label, t in [("SUP", tot_sup), ("INF", tot_inf), ("Italia (totale)", tot_italia)]:
    md.append(f"| {label} | {fmt_int(t['n_pol'])} | {fmt_pct(t['cdx'],t['totale'])} | {fmt_pct(t['csx'],t['totale'])} | {fmt_pct(t['m5s'],t['totale'])} | {fmt_pct(t['altri'],t['totale'])} |")

md.append("\n### Referendum giustizia 2026\n")
md.append("| Tipologia | n. comuni | SI | NO | Affluenza |")
md.append("|---|---:|---:|---:|---:|")
for label, t in [("SUP", tot_sup), ("INF", tot_inf), ("Italia (totale)", tot_italia)]:
    validi = t["si"] + t["no"]
    md.append(f"| {label} | {fmt_int(t['n_ref'])} | {fmt_pct(t['si'],validi)} | {fmt_pct(t['no'],validi)} | {fmt_pct(t['votanti_ref'],t['elettori_ref'])} |")

md.append("\n## 3. Comuni SUP per coalizione del sindaco uscente\n")
md.append("### Politiche 2022\n")
md.append("| Coalizione sindaco uscente | n. comuni | CDX | CSX | M5S | Altri |")
md.append("|---|---:|---:|---:|---:|---:|")
order = ["centrodestra","civico_centrodestra","centrosinistra","civico_centrosinistra",
         "M5S_CSX","M5S","civico_altro"]
for c in order:
    if c not in coal_totali: continue
    t = coal_totali[c]
    md.append(f"| {c} | {fmt_int(t['n_pol'])} | {fmt_pct(t['cdx'],t['totale'])} | {fmt_pct(t['csx'],t['totale'])} | {fmt_pct(t['m5s'],t['totale'])} | {fmt_pct(t['altri'],t['totale'])} |")

md.append("\n### Referendum giustizia 2026\n")
md.append("| Coalizione sindaco uscente | n. comuni | SI | NO | Affluenza |")
md.append("|---|---:|---:|---:|---:|")
for c in order:
    if c not in coal_totali: continue
    t = coal_totali[c]
    validi = t["si"] + t["no"]
    md.append(f"| {c} | {fmt_int(t['n_ref'])} | {fmt_pct(t['si'],validi)} | {fmt_pct(t['no'],validi)} | {fmt_pct(t['votanti_ref'],t['elettori_ref'])} |")

md.append("\n## 4. Comuni al voto per macro-area\n")
md.append("### Politiche 2022\n")
md.append("| Area | n. comuni al voto | CDX | CSX | M5S | Altri | (Italia stessa area) CDX | CSX | M5S |")
md.append("|---|---:|---:|---:|---:|---:|---:|---:|---:|")
for a in ("Nord","Centro","Mezzogiorno"):
    t = area_totali.get(a, somma(set(),None))
    it = area_italia_totali.get(a, somma(set(),None))
    md.append(f"| {a} | {fmt_int(t['n_pol'])} | {fmt_pct(t['cdx'],t['totale'])} | {fmt_pct(t['csx'],t['totale'])} | {fmt_pct(t['m5s'],t['totale'])} | {fmt_pct(t['altri'],t['totale'])} | {fmt_pct(it['cdx'],it['totale'])} | {fmt_pct(it['csx'],it['totale'])} | {fmt_pct(it['m5s'],it['totale'])} |")

md.append("\n### Referendum giustizia 2026\n")
md.append("| Area | n. comuni al voto | SI | NO | (Italia stessa area) SI | NO |")
md.append("|---|---:|---:|---:|---:|---:|")
for a in ("Nord","Centro","Mezzogiorno"):
    t = area_totali.get(a, somma(set(),None))
    it = area_italia_totali.get(a, somma(set(),None))
    validi = t["si"] + t["no"]
    validi_it = it["si"] + it["no"]
    md.append(f"| {a} | {fmt_int(t['n_ref'])} | {fmt_pct(t['si'],validi)} | {fmt_pct(t['no'],validi)} | {fmt_pct(it['si'],validi_it)} | {fmt_pct(it['no'],validi_it)} |")

md.append("\n---")
md.append("\n**Note metodologiche**")
md.append("- Politiche 2022 (Camera): voti per lista al proporzionale, aggregati per comune. Coalizioni: ")
md.append("  - **CDX**: Fratelli d'Italia, Lega, Forza Italia, Noi Moderati")
md.append("  - **CSX**: Partito Democratico, Alleanza Verdi-Sinistra, +Europa, Impegno Civico")
md.append("  - **M5S**: Movimento 5 Stelle")
md.append("  - **Altri**: Azione-Italia Viva, ItalExit, Italia Sovrana e Popolare, Sud chiama Nord, SVP, ecc.")
md.append("- Referendum giustizia 2026: voti SI/NO sul quesito unico (separazione carriere magistrati).")
md.append("- Macro-aree: Nord (Piemonte, VdA, Lombardia, TAA, Veneto, FVG, Liguria, Emilia Romagna); Centro (Toscana, Umbria, Marche, Lazio); Mezzogiorno (Abruzzo, Molise, Campania, Puglia, Basilicata, Calabria, Sicilia, Sardegna).")
md.append("- Coalizione sindaco uscente: classificazione manuale a partire da liste appoggianti il sindaco, integrata da Wikipedia/stampa per i casi civici. Vedi `input/comuni_al_voto_2026.csv`.")
md.append("")

out_md = OUT / "analisi_voti_precedenti.md"
out_md.write_text("\n".join(md))
print(f"Markdown scritto: {out_md}")
