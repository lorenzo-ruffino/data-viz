# API Risultati Elettorali — Ministero dell'Interno

Documentazione non ufficiale dell'API usata dal sito [elezioni.interno.gov.it](https://elezioni.interno.gov.it) per esporre in tempo reale i dati elettorali italiani.

> **Nota:** questa documentazione è stata ricavata per reverse engineering in occasione del **referendum costituzionale del 22–23 marzo 2026** (riforma della giustizia). I parametri specifici (date, codici elezione, numero di schede) variano da elezione a elezione, ma la struttura degli endpoint e la logica dei codici territoriali sono le stesse per tutte le elezioni disponibili sul portale.

---

## Base URL

```
https://eleapi.interno.gov.it/siel/PX/
```

L'API non richiede autenticazione ma impone un **rate limiting** (circa 40 richieste/secondo). Superata la soglia risponde con una pagina HTML "Access Denied" invece di JSON. È necessario aggiungere gli header `origin` e `referer` con il valore `https://elezioni.interno.gov.it/` altrimenti alcune richieste vengono rifiutate.

---

## Parametri comuni

| Parametro | Posizione | Esempio | Significato |
|---|---|---|---|
| `DE` | path | `20260322` | Data elezione in formato `YYYYMMDD` |
| `TE` | path | `09` | Tipo elezione (09 = referendum) |
| `SK` | path | `01`–`05` | Numero scheda referendaria |
| `RE` | path | `01`–`20` | Codice regione (2 cifre, zero-padded) |
| `PR` | path | `081` | Codice provincia (3 cifre, zero-padded) |
| `CM` | path | `2620` | Codice comune (4 cifre, zero-padded) |
| `SZ` | path | `0001` | Codice sezione (4 cifre, zero-padded) |

> **Attenzione al padding**: tutti i codici territoriali richiedono il padding a zero esplicito. `/PR/2` restituisce "Errore di sintassi"; `/PR/002` funziona correttamente.

---

## Codici territoriali

### Voto Italia (`getentiFI`)

I comuni sono identificati da un codice a **9 cifre** restituito dall'endpoint `getentiFI`:

```
0 1 0 8 1 2 6 2 0
└─┘└───┘└───────┘
 RE   PR     CM
 01  081   2620  → TORINO
```

Le prime 2 cifre identificano la regione, le successive 3 la provincia, le ultime 4 il comune. Questi segmenti vanno usati separatamente come parametri di path.

Il campo `tipo` può essere:
- `RE` — regione (20 voci)
- `PR` — provincia (110 voci)
- `CM` — comune (7895 voci)

### Voto estero (`getentiFE`)

I paesi sono identificati da un codice a **4 cifre**:

```
1 2 2 0
└┘└───┘
ER  NA
01  220  → ALBANIA (ripartizione EUROPA)
```

La prima cifra identifica la ripartizione geografica (`ER`), le ultime 3 il paese (`NA`). Il codice ripartizione va zero-padded a 2 cifre nelle URL.

Il campo `tipo` può essere:
- `ER` — ripartizione estera (4 voci: Europa, America Meridionale, America Settentrionale e Centrale, Africa/Asia/Oceania/Antartide)
- `NA` — paese (181 voci)

---

## Endpoint

### 1. `getentiFI` — Lista degli enti territoriali (Italia)

```
GET /getentiFI/DE/{data}/TE/{tipo}
```

Restituisce la lista completa di tutti gli enti territoriali dell'elezione (regioni, province, comuni).

**Risposta:**
```json
{
  "int": { "file": "GEOPOLITICA REFERENDUM", ... },
  "enti": [
    { "cod": "010000000", "desc": "PIEMONTE", "tipo": "RE", "dt_agg": 20260323160000 },
    { "cod": "010810000", "desc": "TORINO",   "tipo": "PR", "dt_agg": 20260323160000 },
    { "cod": "010812620", "desc": "TORINO",   "tipo": "CM", "dt_agg": 20260323160000 }
  ]
}
```

---

### 2. `getentiFE` — Lista degli enti esteri

```
GET /getentiFE/DE/{data}/TE/{tipo}
```

Restituisce la lista delle ripartizioni e dei paesi per il voto estero.

**Risposta:**
```json
{
  "int": { "file": "GEOPOLITICA REFERENDUM ESTERO", "area": "E", ... },
  "enti": [
    { "cod": "1000", "desc": "EUROPA",  "tipo": "ER", "dt_agg": 20260323210434 },
    { "cod": "1220", "desc": "ALBANIA", "tipo": "NA", "dt_agg": 20260323180141 }
  ]
}
```

---

### 3. `votantiFI` — Affluenza (Italia)

```
GET /votantiFI/DE/{data}/TE/{tipo}/SK/{scheda}/PR/{provincia}
```

Restituisce l'affluenza per tutti i comuni di una provincia. Usare `SK/01`.

**Struttura `com_vot`** — per ogni comune, array con una riga per comunicazione (4 comunicazioni per il referendum 2026):

| Campo | Tipo | Significato |
|---|---|---|
| `com` | int | Numero comunicazione (1–4) |
| `dt_com` | num | Timestamp comunicazione (`20260322120000`) |
| `ele_t` / `ele_m` / `ele_f` | int | Elettori totali / maschi / femmine |
| `vot_t` / `vot_m` / `vot_f` | int | Votanti totali / maschi / femmine |
| `perc` | chr | Percentuale affluenza (stringa con virgola decimale, es. `"43,52"`) |

Le **4 comunicazioni** del referendum 2026 corrispondono a:

| `com` | Orario |
|---|---|
| 1 | Domenica ore 12:00 |
| 2 | Domenica ore 19:00 |
| 3 | Domenica ore 23:00 |
| 4 | Lunedì ore 15:00 (chiusura urne) |

---

### 4. `scrutiniFI` — Risultati scrutinio (Italia)

```
GET /scrutiniFI/DE/{data}/TE/{tipo}/SK/{scheda}[/RE/{reg}][/PR/{prov}][/CM/{comune}][/SZ/{sezione}]
```

Disponibile ai livelli: **nazionale**, **regionale**, **provinciale**, **comunale**, **sezione**.

```
/SK/01                                    → nazionale
/SK/01/RE/01                              → regione
/SK/01/RE/01/PR/081                       → provincia
/SK/01/RE/01/PR/081/CM/2620               → comune
/SK/01/PR/081/CM/2620/SZ/0001             → sezione (senza /RE/)
```

> **Comportamento speciale per le sezioni**: a differenza degli altri livelli, l'endpoint per sezione **non vuole `/RE/`** nel path. Includere la regione restituisce "Dati non trovati".

> Per tutti gli altri livelli la gerarchia deve essere completa: per interrogare una provincia occorre includere anche la regione, per un comune occorre includere regione e provincia.

**Risposta:**
```json
{
  "int": {
    "l_terr": "SEZIONE",
    "cod_com": 62, "desc_com": "ALLUVIONI PIOVERA",
    "cod_prov": 2, "desc_prov": "ALESSANDRIA",
    "cod_reg": 1, "desc_reg": "PIEMONTE",
    "ele_t": 683, "ele_m": 0, "ele_f": 0,
    "sz_tot": 1, "osp": "N"
  },
  "scheda": [
    {
      "cod": 1,
      "sz_perv": 1,
      "vot_t": 343, "vot_m": 0, "vot_f": 0,
      "perc_vot": "50,22",
      "sk_bianche": 1, "sk_nulle": 4, "sk_contestate": 0,
      "voti_si": 198, "voti_no": 140,
      "perc_si": "58,58", "perc_no": "41,42",
      "dt_agg": 20260323160854
    }
  ]
}
```

Il campo `scheda` è un array con una voce per ogni quesito referendario. Per il referendum 2026 c'è **1 quesito** (SK/01). Il referendum 2025 aveva **5 quesiti** (SK/01–SK/05).

> **Nota**: a livello di sezione, `ele_m`, `ele_f`, `vot_m`, `vot_f` sono restituiti come `0` per ragioni di privacy.

Se i dati non sono ancora disponibili per un ente:
```json
{ "Error": { "id": "200", "desc": "Dati non trovati", "value": "..." } }
```

Se il path è malformato (es. padding mancante):
```json
{ "Error": { "id": "310", "desc": "Errore di sintassi", "value": "..." } }
```

---

### 5. `scrutiniFE` — Risultati scrutinio (estero)

```
GET /scrutiniFE/DE/{data}/TE/{tipo}/SK/{scheda}[/ER/{ripartizione}][/NA/{paese}]
```

Disponibile ai livelli: **totale estero**, **ripartizione**, **paese**.

```
/SK/01                    → totale estero
/SK/01/ER/01              → ripartizione (es. Europa)
/SK/01/ER/01/NA/220       → paese (es. Albania)
```

Il codice ripartizione (`ER`) va zero-padded a 2 cifre. Il codice paese (`NA`) è a 3 cifre (ultime 3 del codice a 4 cifre restituito da `getentiFE`).

**Risposta:**
```json
{
  "int": {
    "l_terr": "NAZIONE",
    "cod_rip": 1, "desc_rip": "EUROPA",
    "cod_naz": 220, "desc_naz": "ALBANIA",
    "ele_t": 3063, "sz_tot": 2
  },
  "scheda": [
    {
      "cod": 1,
      "sz_perv": 2,
      "vot_t": 1333, "perc_vot": "43,52",
      "sk_bianche": 7, "sk_nulle": 75, "sk_contestate": 0,
      "voti_si": 784, "voti_no": 467,
      "perc_si": "62,67", "perc_no": "37,33",
      "dt_agg": 20260323210724
    }
  ]
}
```

---

### 6. Mappe GeoJSON

```
GET https://elezioni.interno.gov.it/mappe/nazioni_{n}.geojson   (n = 1–4)
GET https://elezioni.interno.gov.it/mappe/comuni.geojson
```

Le mappe dei paesi esteri sono divise in 4 file (255 feature totali). Ogni feature ha le proprietà `NAME`, `NAME_IT`, `ISO_A3`, `ISO_N3`, `CONTINENT`, `tipo`. Il join con i dati dell'API va fatto via `toupper(NAME_IT)` = `toupper(desc_naz)` (corrispondenza al 100% per tutti i 181 paesi con dati).

> **Formato non standard**: i file `nazioni_*.geojson` sono array JSON di feature, non `FeatureCollection`. Vanno wrappati prima di essere letti con `sf::st_read()`.

---

## Note operative

- **Decimali con virgola**: `perc_vot`, `perc_si`, `perc_no` sono stringhe con virgola come separatore decimale (es. `"43,52"`). Convertire con `as.numeric(gsub(",", ".", x))` — `as.numeric()` diretto produce `NA`.
- **Rate limiting**: con più di ~40 richieste/secondo l'API risponde con HTML "Access Denied". Con R è sicuro usare 4 worker paralleli con `Sys.sleep(0.1)` per worker. Con questo setup: ~7900 comuni in ~9 minuti, ~181 paesi in ~10 secondi, ~61500 sezioni in ~70 minuti.
- **Aggregazioni**: le percentuali aggregate vanno ricalcolate dai totali grezzi (`voti_si / (voti_si + voti_no)`), non facendo la media delle percentuali per ente.
- **Scrutini vs affluenza**: `scrutiniFI` restituisce i dati solo a scrutinio concluso per quell'ente; durante lo spoglio molti comuni hanno `scheda: null` o `sz_perv = 0`. Usare `votantiFI` per l'affluenza in tempo reale durante la giornata di voto.
