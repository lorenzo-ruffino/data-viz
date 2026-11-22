# Inflazione di due panieri personali stilizzati, costruiti ripesando le classi del NIC 2026.
# Input: output/nic_voci_variazioni_cumulate.csv (variazioni tendenziali settembre 2026 e cumulate set 2020-set 2026, pesi NIC 2026).
import pandas as pd

d = pd.read_csv("output/nic_voci_variazioni_cumulate.csv", dtype={"codice": str})
c = d[d.codice.str.fullmatch(r"\d{4}")].set_index("codice")
c["cum"] = c.cum_set2020_set2026.fillna(c.cum_set2020_set2026.mean())

AUTO = ["0711", "0712", "0721", "0722", "0723", "0724", "1214"]

# Moltiplicatori dei pesi NIC (prefisso di codice ECOICOP v2 -> fattore)
PROFILI = {
    "Paniere medio NIC": {},
    "Benestante: esce spesso, viaggia, usa l'auto": {
        "1111": 3, "1120": 2.5, "0733": 3, "0980": 2, "0946": 2, "0961": 3,
        "0411": 0, "01": 0.7, "0452": 0.6, "0451": 0.7,
    },
    "Meno abbiente: cucina a casa, mezzi pubblici, gas": {
        "1111": 0.25, "1120": 0.2, "0733": 0.1, "0980": 0.3, "0946": 0.3, "0961": 0.3,
        "01": 1.6, "0452": 2, "0451": 1.5, "0731": 4, "0732": 4, "0735": 4, "0411": 1.5,
        **{k: 0 for k in AUTO},
    },
}

righe = []
for nome, molt in PROFILI.items():
    w = c.peso_2026_per_mille.copy()
    for pref, m in molt.items():
        w[[x for x in c.index if x.startswith(pref)]] *= m
    quota = lambda p: w[[x for x in c.index if x.startswith(p)]].sum() / w.sum() * 100
    righe.append({
        "profilo": nome,
        "inflazione_set2026": (w * c.tend_set2026).sum() / w.sum(),
        "cumulata_set2020_set2026": (w * c["cum"]).sum() / w.sum(),
        "quota_alimentari": quota("01"),
        "quota_energia_casa": quota("045"),
        "quota_carburanti": quota("0722"),
        "quota_ristoranti_bar": quota("1111"),
    })

out = pd.DataFrame(righe).round(1)
out.to_csv("output/panieri_personali_set2026.csv", index=False)
print(out.to_string(index=False))
