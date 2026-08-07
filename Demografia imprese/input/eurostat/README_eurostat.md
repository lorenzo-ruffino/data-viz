# Demografia d'impresa — confronto europeo (Eurostat Business Demography)

Dati scaricati il **2026-07-18** con R (pacchetto `eurostat` 4.0.0, che usa il
bulk download / SDMX-CSV di Eurostat). Script: `../../script/download_eurostat_bd.R`.

Paesi: **Italia (IT), Germania (DE), Francia (FR), Spagna (ES)** + aggregato
**UE27 (EU27_2020)**.

---

## AVVERTENZA METODOLOGICA IMPORTANTE (rottura di serie 2021)

La business demography Eurostat è stata **ristrutturata** con il passaggio dal
vecchio regolamento SBS (Structural Business Statistics) al nuovo **EBS**
(European Business Statistics). Ci sono quindi **due mondi non pienamente
confrontabili**:

- **Serie storica classica** → dataset `bd_9bd_sz_cl_r2` (NACE Rev. 2),
  **2004–2020**, ultimo aggiornamento 10/01/2024. È la serie lunga, coerente e
  con copertura piena dei quattro paesi. **File 01, 02, 03.**
- **Serie nuova (EBS)** → dataset `bd_size`, **2009–2023** ma con i tassi
  popolati solo dal **2021**, ultimo aggiornamento 13/03/2026. È il dato più
  recente ma **non è confrontabile** con la serie classica per un salto di
  perimetro e definizioni. **File 04 (solo snapshot 2021–2023).**

Per i confronti "ultimi 10 anni" la serie di riferimento è la classica
(2011–2020, con UE27 disponibile dal 2012). Il 2021–2023 va usato solo come
aggiornamento più recente, segnalando la rottura.

---

## Definizioni — quale popolazione copre ogni indicatore

- **Tutte le imprese ("all enterprises")**: include anche le imprese **senza
  dipendenti** (autonomi/ditte individuali). È la popolazione dei file 01–04.
- **Imprese con dipendenti ("employer enterprises", almeno 1 dipendente)**:
  popolazione diversa, contenuta in dataset separati (vedi sotto "Companion").
  **Non** usata nei file principali; utile se serve escludere l'effetto degli
  autonomi.
- **Business economy**:
  - serie classica (file 01–03), aggregato **`B-N_X_K642`** = industria,
    costruzioni e servizi **esclusi** amministrazione pubblica/difesa,
    previdenza obbligatoria e **attività delle holding (NACE 642)**. È la
    definizione "business economy **senza holding**".
  - serie nuova (file 04), aggregato **`B-S_X_O_S94`** = industria, costruzioni
    e servizi di mercato, esclusi PA/difesa/previdenza e organizzazioni
    associative (S94). Perimetro leggermente più ampio (arriva fino a S).

### Caveat da tenere presenti nell'articolo
- **Francia — regime auto-entrepreneur/micro-entrepreneur**: gonfia il numero
  di nascite e la quota di nate senza dipendenti, e tiene il **tasso di
  mortalità strutturalmente basso** (~4–5% ogni anno, non solo negli ultimi
  anni). I confronti su mortalità e crescita netta con la Francia vanno presi
  con cautela.
- **Germania — bassa quota di nate senza dipendenti** (52% nel 2020, 67% nel
  2023): riflette il minor peso/registrazione dei non-employer nella demografia
  tedesca, non solo un fenomeno economico.
- **Ritardo di conferma dei decessi**: una "morte d'impresa" è confermata solo
  dopo ~2 anni di inattività, quindi il **tasso di mortalità dell'ultimo/i
  anno/i è provvisorio e sottostimato**. Per confronti di mortalità/crescita
  netta preferire un anno con decessi consolidati (≤ 2018).
- **2020 = anno Covid**: cali di natalità diffusi; leggere il 2020 con il 2019
  a fianco.

---

## FILE

### 01 — Tasso di natalità e mortalità (tutte le imprese)
- **File**: `01_tasso_natalita_mortalita_all_2004_2020.csv`
- **Dataset**: `bd_9bd_sz_cl_r2` — Business demography by size class and NACE
  Rev. 2 activity (2004-2020)
- **Popolazione**: tutte le imprese · **NACE**: `B-N_X_K642` (business economy
  senza holding) · **sizeclas**: TOTAL
- **Indicatori**: `V97020` = Birth rate (%), `V97030` = Death rate (%)
- **Periodo**: IT/DE/FR/ES 2008–2020; UE27 2012–2020
- **URL**: https://ec.europa.eu/eurostat/databrowser/view/bd_9bd_sz_cl_r2
- **Numeri chiave (tasso di natalità, %)**:
  - Italia 2019 = **7,4** · 2020 = **6,5**
  - UE27 2019 = **10,0** · 2020 = **8,8** → *l'Italia nasce MENO della media UE
    (e meno di FR, DE, ES)*
  - Francia 2020 = 11,3 · Germania 2020 = 7,2 · Spagna 2020 = 7,4
  - Tasso di mortalità 2018 (decessi consolidati): Italia 5,8 · UE27 7,1

### 02 — Sopravvivenza a 3 e 5 anni (tutte le imprese)
- **File**: `02_sopravvivenza_3_5_anni_all_2004_2020.csv`
- **Dataset**: `bd_9bd_sz_cl_r2` · stesso perimetro del file 01
- **Indicatori**: `V97043` = Survival rate 3 anni (%), `V97045` = Survival rate
  5 anni (%). Definizione: imprese nate in t-n ancora vive in t / imprese nate
  in t-n.
- **Periodo**: 2008–2020 (IT/DE/FR/ES); UE27 dal 2012
- **URL**: https://ec.europa.eu/eurostat/databrowser/view/bd_9bd_sz_cl_r2
- **Numeri chiave (2020)**:
  - Sopravvivenza a 3 anni: Italia **56,9%** · UE27 **58,5%** · Germania 48,4% ·
    Francia 62,5% · Spagna 55,2%
  - Sopravvivenza a 5 anni: Italia **45,8%** · UE27 **46,1%** · Germania 37,3% ·
    Francia 50,8%
  - → *l'Italia sopravvive circa in linea con la media UE, meglio della Germania,
    peggio della Francia.*

### 03 — Imprese nate per classe dimensionale (per la quota "nascono micro")
- **File**: `03_nate_per_classe_dimensionale_all_2004_2020.csv`
- **Dataset**: `bd_9bd_sz_cl_r2` · NACE `B-N_X_K642`
- **Indicatore**: `V11920` = numero di imprese nate, per **sizeclas**
  (TOTAL, 0, 1-4, 5-9, GE10). Il numero di dipendenti è quello **alla nascita**.
- **Quota nate senza dipendenti** = valore sizeclas `0` / sizeclas `TOTAL`.
- **Periodo**: IT/DE/FR/ES 2004–2020; UE27 (classe 0) dal 2012
- **URL**: https://ec.europa.eu/eurostat/databrowser/view/bd_9bd_sz_cl_r2
- **Numeri chiave (quota nate con 0 dipendenti, 2020)**:
  - Italia **81,1%** · UE27 **83,1%** · Germania 52,5% · Francia 95,8% ·
    Spagna 82,3%
  - → *in Italia oltre 4 imprese nate su 5 nascono senza dipendenti, in linea
    con la media UE.*

### 04 — Snapshot recente EBS (2021–2023) — NON confrontabile con 01–03
- **File**: `04_snapshot_recente_bd_size_2021_2023.csv`
- **Dataset**: `bd_size` — Business demography by size class and NACE Rev. 2
  activity (2009-2023, tassi dal 2021)
- **Popolazione**: tutte le imprese · **NACE**: `B-S_X_O_S94` (business economy)
- **Indicatori**:
  - `ENT_BRTHR_PC` = tasso di natalità (%), `ENT_DTHR_PC` = tasso di mortalità (%)
  - `ENT_SRVLR_BRTH_PC` = sopravvivenza di coorte (%), disponibile solo a
    **1 e 2 anni** (age Y1, Y2): la serie EBS parte dal 2021, quindi al 2023 le
    coorti non hanno ancora 3/5 anni → per 3 e 5 anni usare il **file 02**.
  - `ENT_BRTH_NR` = numero di nate (sizeclas TOTAL e 0) per la quota senza
    dipendenti.
- **Periodo**: 2021–2023 (tutti i paesi + UE27)
- **URL**: https://ec.europa.eu/eurostat/databrowser/view/bd_size
- **Numeri chiave (2023)**:
  - Natalità: Italia **7,8%** · UE27 **10,5%** · Germania 8,4% · Francia 14,0% ·
    Spagna 9,1% → *Italia ancora sotto la media UE.*
  - Sopravvivenza a 1 anno: Italia 82,0% · UE27 81,0%
  - Quota nate 0 dipendenti: Italia 83,2% · UE27 83,3%

---

## Companion / dataset alternativi (non scaricati, per riferimento)

- **Imprese con dipendenti (employer), serie classica**: `bd_9fh_sz_cl_r2`
  (by size), `bd_9eg_l_form_r2` (by legal form). Nota: **niente aggregato UE**;
  Germania solo dal 2012. Da usare solo se serve escludere i non-employer.
  https://ec.europa.eu/eurostat/databrowser/view/bd_9fh_sz_cl_r2
- **Imprese con dipendenti, serie nuova (EBS)**: `bd_salge1_size`,
  `bd_salge1_l_form` (2009–2023).
  https://ec.europa.eu/eurostat/databrowser/view/bd_salge1_size
- **Imprese ad alta crescita**: `bd_9pm_r2` (2008–2021) / `bd_hgnace2_r3`.

## Schema colonne dei CSV
`dataset, popolazione, indic_code, indicatore, geo, geo_label, nace_r2,
sizeclas, [age solo file 04], anno, valore, unita`.
Valori mancanti = celle vuote (l'anno esiste ma il dato non è pubblicato).

## Come rigenerare
```
Rscript "Demografia imprese/script/download_eurostat_bd.R"
```
In alternativa via API REST SDMX-CSV. **La chiave va nel path** (ordine
dimensioni: `freq.indic_sb.sizeclas.nace_r2.geo`), non come query param — coi
query param i filtri vengono ignorati e l'estrazione va in errore
(EXTRACTION_TOO_BIG). Esempio, natalità Italia serie classica:
```
https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/bd_9bd_sz_cl_r2/A.V97020.TOTAL.B-N_X_K642.IT?format=SDMX-CSV&startPeriod=2018
```
Per `bd_size` l'ordine dimensioni è `freq.age.sizeclas.indic_sbs.nace_r2.geo`.
