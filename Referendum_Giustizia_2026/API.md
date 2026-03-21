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
| `CM` | path | `2620` | Codice comune (4 cifre) |

### Codice comune univoco

I comuni sono identificati da un codice a **9 cifre** restituito dall'endpoint `getentiFI`:

```
0 1 0 8 1 2 6 2 0
└-─┘└───┘└────-─┘
 RE  PR     CM
 01  081   2620  → TORINO
```

Le prime 2 cifre identificano la regione, le successive 3 la provincia, le ultime 4 il comune. Questi segmenti vanno usati separatamente come parametri di path negli altri endpoint.

---

## Endpoint

### 1. `getentiFI` — Lista degli enti territoriali

```
GET /getentiFI/DE/{data}/TE/{tipo}
```

Restituisce la lista completa di tutti gli enti territoriali dell'elezione.

**Risposta:**
```json
{
  "int": { "file": "GEOPOLITICA REFERENDUM", ... },
  "enti": [
    { "cod": "010000000", "desc": "PIEMONTE",   "tipo": "RE", ... },
    { "cod": "010810000", "desc": "TORINO",      "tipo": "PR", ... },
    { "cod": "010812620", "desc": "TORINO",      "tipo": "CM", ... },
    ...
  ]
}
```

Il campo `tipo` può essere:
- `RE` — regione (20 voci)
- `PR` — provincia (110 voci)
- `CM` — comune (7895 voci)

---

### 2. `votantiFI` — Affluenza

```
GET /votantiFI/DE/{data}/TE/{tipo}/SK/{scheda}/PR/{provincia}
```

Restituisce l'affluenza per tutti i comuni di una provincia. Il parametro `SK` distingue due varianti di risposta:

- **`SK/01`** — risposta strutturata con `enti_f` (lista comuni) e campo `com_vot` per comune
- **`SK/02`** — risposta alternativa con struttura diversa

Usare `SK/01`.

**Struttura `com_vot`:**

Per ogni comune, `com_vot` è un **dataframe** con una riga per comunicazione (4 comunicazioni totali per il referendum 2026):

| Campo | Tipo | Significato |
|---|---|---|
| `com` | int | Numero comunicazione (1–4) |
| `dt_com` | num | Timestamp comunicazione (`20260322120000`) |
| `ele_t` / `ele_m` / `ele_f` | int | Elettori totali / maschi / femmine |
| `vot_t` / `vot_m` / `vot_f` | int | Votanti totali / maschi / femmine |
| `perc` | chr | Percentuale affluenza (stringa, da convertire) |

Le **4 comunicazioni** del referendum 2026 corrispondono a:

| `com` | Orario |
|---|---|
| 1 | Domenica ore 12:00 |
| 2 | Domenica ore 19:00 |
| 3 | Domenica ore 23:00 |
| 4 | Lunedì ore 15:00 (chiusura urne) |

---

### 3. `scrutiniFI` — Risultati scrutinio

```
GET /scrutiniFI/DE/{data}/TE/{tipo}/SK/{scheda}[/RE/{reg}[/PR/{prov}[/CM/{comune}]]]
```

Disponibile ai livelli: **nazionale**, **regionale**, **provinciale**, **comunale**. La gerarchia nel path deve sempre essere completa: per interrogare una provincia occorre includere anche la regione, per un comune occorre includere regione e provincia.

```
/SK/01                              → nazionale
/SK/01/RE/01                        → regione
/SK/01/RE/01/PR/081                 → provincia
/SK/01/RE/01/PR/081/CM/2620         → comune
```

Nota: il path `/SK/01/PR/081` (provincia senza regione) restituisce "Dati non trovati".

**Risposta:**
```json
{
  "int": {
    "cod_com": 2620, "desc_com": "TORINO",
    "ele_t": 631560, "sz_tot": 919,
    ...
  },
  "scheda": [
    {
      "cod": 1,
      "sz_perv": 0,
      "vot_t": 0, "vot_m": 0, "vot_f": 0,
      "perc_vot": "0",
      "sk_bianche": 0, "sk_nulle": 0, "sk_contestate": 0,
      "voti_si": 0, "voti_no": 0,
      "perc_si": "0", "perc_no": "0",
      "dt_agg": null
    }
  ]
}
```

Il campo `scheda` è un array con una voce per ogni quesito referendario. Per il referendum 2026 c'è **1 quesito** (SK/01); il referendum 2025 aveva **5 quesiti** (SK/01–SK/05). Il parametro `SK` nell'URL seleziona quale quesito interrogare.

Se i dati non sono ancora disponibili per un ente, la risposta è:
```json
{ "Error": { "id": "200", "desc": "Dati non trovati", "value": "..." } }
```

---

## Esempio di utilizzo completo (R)

```r
library(httr)
library(jsonlite)

headers <- c(
  'accept'  = 'application/json, text/plain, */*',
  'origin'  = 'https://elezioni.interno.gov.it',
  'referer' = 'https://elezioni.interno.gov.it/'
)

# Risultati per Torino
r <- GET(
  'https://eleapi.interno.gov.it/siel/PX/scrutiniFI/DE/20260322/TE/09/SK/01/RE/01/PR/081/CM/2620',
  add_headers(.headers = headers)
)
d <- fromJSON(rawToChar(r$content))
```

---

## Note operative

- **Rate limiting**: con più di ~40 richieste/secondo l'API risponde con HTML "Access Denied". Con R è sicuro usare 4 worker paralleli con `Sys.sleep(0.1)` tra una richiesta e l'altra per ogni worker (~7900 comuni in ~8 minuti).
- **Valori stringa**: `perc_vot`, `perc_si`, `perc_no` sono restituiti come stringhe — convertire con `as.numeric()`.
- **Aggregazioni**: le percentuali aggregate vanno ricalcolate dai totali grezzi (`voti_si / (voti_si + voti_no)`), non facendo la media delle percentuali per comune.