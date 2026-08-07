# Dati complementari ISTAT SDMX — Demografia imprese

Dati scaricati dal sistema **ISTAT SDMX** (esploradati.istat.it) per integrare le
tavole Excel ISTAT sulla demografia d'impresa 2004-2024, dando i **livelli assoluti**
(stock di imprese attive e addetti) accanto ai tassi di natalità/mortalità.

Fonte: registro statistico **ASIA-Imprese** (Archivio Statistico delle Imprese Attive).
Dataflow **`183_277_DF_DICA_ASIAUE1P_1`** — "Imprese e addetti - Principali dati"
(DSD `DICA_ASIAUE1P`, agenzia IT1, versione 1.0).

Scaricati con la CLI `opensdmx` (provider `istat`) il 2026-07-18. Il server MCP `istat`
rispondeva su discovery/codelist/structure ma andava **in timeout (408, >180s)** sulle
chiamate dati `get_data`/`get_constraints`: usato quindi il fallback `opensdmx`, che
interroga direttamente l'endpoint REST `https://esploradati.istat.it/SDMXWS/rest/`.

## Dimensioni del dataflow (ordine chiave SDMX)
`FREQ . REF_AREA . DATA_TYPE . ECON_ACTIVITY_NACE_2007 . PERS_EMPL_SIZE_CLASS . LEGAL_FORM . Y_ENTER_WITH_EMPLOYEES . Y_CRAFTMEN`

Codici usati come filtro (dai rispettivi codelist):
- `FREQ = A` (annuale)
- `DATA_TYPE` (CL_TIPO_DATO_CIS): **`AENTN`** = numero imprese attive · **`AENTEMPDAA`** = numero addetti delle imprese attive (valori medi annui). NB: il codice `AENTEMPD` (senza AA) non ha dati in questo dataflow.
- `ECON_ACTIVITY_NACE_2007` (CL_ATECO_2007): **`0010`** = totale attività economiche; sezioni Ateco come lettere `A`…`S`.
- `PERS_EMPL_SIZE_CLASS` (CL_CLLVT): **`TOTAL`** = totale; classi effettivamente presenti nel registro: `W0_9` (0-9 addetti), `W10_49`, `W50_249`, `W_GE250` (250 e più).
- `LEGAL_FORM` (CL_FORMGIUR): **`TOT`** = totale forme giuridiche.
- `Y_ENTER_WITH_EMPLOYEES` / `Y_CRAFTMEN` (CL_SI_NO): **`9`** = totale.
- `REF_AREA` (CL_ITTER107): `IT` = Italia; 21 regioni/province autonome `ITC1`…`ITG2`.

## Colonne dei CSV
Output SDMX-CSV grezzo. Colonne utili: `FREQ, REF_AREA, DATA_TYPE, ECON_ACTIVITY_NACE_2007,
PERS_EMPL_SIZE_CLASS, LEGAL_FORM, Y_ENTER_WITH_EMPLOYEES, Y_CRAFTMEN, TIME_PERIOD, OBS_VALUE`.
`TIME_PERIOD` è nel formato `AAAA-01-01` (dato annuale). Le colonne `NOTE_*`, `BASE_PER`,
`UNIT_MEAS`, `UNIT_MULT` sono per lo più vuote. Copertura temporale di tutti i file: **2012-2024**
(la serie ASIA in SDMX parte dal 2012; per il 2004-2011 non esiste in SDMX una serie omogenea
di stock).

---

## 1. `asia_imprese_addetti_nazionale.csv` — STOCK + ADDETTI, Italia
Imprese attive e addetti, totale nazionale, 2012-2024 (26 righe = 13 anni × 2 misure).
Filtro: `REF_AREA=IT · DATA_TYPE=AENTN,AENTEMPDAA · ATECO=0010 · SIZE=TOTAL · LEGAL=TOT · WEMPL=9 · CRAFT=9`.
- Imprese attive: **4.442.452 (2012)** → **4.751.988 (2024)**, +7,0%.
- Addetti: **16.722.210 (2012)** → **18.845.824 (2024)**, +12,7%.
- Addetti medi per impresa 2024: **3,97**.
- URL REST: `https://esploradati.istat.it/SDMXWS/rest/data/IT1,183_277_DF_DICA_ASIAUE1P_1,1.0/A.IT.AENTN+AENTEMPDAA.0010.TOTAL.TOT.9.9?startPeriod=2012&endPeriod=2024`

## 2. `asia_imprese_addetti_regioni.csv` — STOCK + ADDETTI per regione
Come sopra ma per le 21 regioni/PA (546 righe = 21 × 13 × 2).
Filtro: `REF_AREA=ITC1…ITG2 · DATA_TYPE=AENTN,AENTEMPDAA · ATECO=0010 · SIZE=TOTAL · LEGAL=TOT · WEMPL=9 · CRAFT=9`.
- Imprese attive 2024, prime regioni: **Lombardia 898.069 · Lazio 496.138 · Veneto 414.060 · Campania 392.261 · Emilia-Romagna 383.230**.
- URL REST: `.../A.ITC1+ITC2+ITC3+ITC4+ITD1+ITD2+ITD3+ITD4+ITD5+ITE1+ITE2+ITE3+ITE4+ITF1+ITF2+ITF3+ITF4+ITF5+ITF6+ITG1+ITG2.AENTN+AENTEMPDAA.0010.TOTAL.TOT.9.9?startPeriod=2012&endPeriod=2024`

## 3. `asia_per_classe_addetti_nazionale.csv` — distribuzione per classe di addetti
Imprese e addetti per classe di addetti, Italia, 2012-2024 (130 righe = 5 classi × 13 × 2).
Filtro: `REF_AREA=IT · DATA_TYPE=AENTN,AENTEMPDAA · ATECO=0010 · SIZE=(tutte) · LEGAL=TOT · WEMPL=9 · CRAFT=9`.
"Nascono e restano micro" — quota 2024 sul totale imprese:
- **0-9 addetti: 4.501.198 (94,7% delle imprese, ma solo 7,6 mln di addetti)**
- 10-49: 218.126 (4,6%) · 50-249: 27.845 (0,6%) · **250+: 4.819 (0,1% delle imprese ma 4,55 mln di addetti, ~24%)**.
- URL REST: `.../A.IT.AENTN+AENTEMPDAA.0010..TOT.9.9?startPeriod=2012&endPeriod=2024` (posizione SIZE vuota = tutte le classi)

## 4. `asia_per_settore_nazionale.csv` — distribuzione per macrosettore (sezione Ateco)
Imprese e addetti per sezione Ateco 2007, Italia, 2012-2024 (442 righe = 17 sezioni × 13 × 2).
Filtro: `REF_AREA=IT · DATA_TYPE=AENTN,AENTEMPDAA · ATECO=A…S · SIZE=TOTAL · LEGAL=TOT · WEMPL=9 · CRAFT=9`.
Presenti le sezioni B-S (escluse A agricoltura e O PA, fuori campo ASIA). Imprese vs addetti 2024:
- **G Commercio: 984.743 imprese / 3.437.688 addetti** · **M Att. professionali: 934.227 / 1.555.782**
- **F Costruzioni: 544.886 / 1.627.909** · **C Manifattura: 355.908 imprese ma 3.891.097 addetti** (settore a più alta intensità occupazionale).
- URL REST: `.../A.IT.AENTN+AENTEMPDAA.A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S.TOTAL.TOT.9.9?startPeriod=2012&endPeriod=2024`

---

## Obiettivo NON coperto dal SDMX
**Demografia d'impresa (natalità/mortalità/sopravvivenza delle imprese)**: nessun dataflow
disponibile su ISTAT SDMX. Le ricerche `natalità,mortalità,sopravvivenza`, `nate,cessate,iscrizioni`
e `demografia,impresa` restituiscono solo mortalità/tavole di vita della popolazione e statistiche
agricole/strutturali; i dataflow `DICA_*` con "età impresa" riguardano la struttura per età anagrafica
delle imprese (indagine SBS), non i tassi di natalità/mortalità. La demografia d'impresa ISTAT resta
pubblicata solo nelle **tavole Excel** (e in Eurostat `bd_*`), non in SDMX. Lo stock e gli addetti
qui scaricati servono a dare i livelli assoluti accanto ai tassi di quelle tavole.
