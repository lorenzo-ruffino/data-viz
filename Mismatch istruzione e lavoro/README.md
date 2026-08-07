# Mismatch istruzione e lavoro

Analisi del disallineamento tra studio e lavoro in Italia: **verticale** (sovraistruzione)
e **orizzontale** (campo di studio vs professione), con confronto europeo.

## Fonti

| Fonte | Cosa | Dove |
|---|---|---|
| **ISTAT RCFL** microdati | sovraistruzione calcolata a mano dai microdati, 2015-2025 (44 trimestri) | letti da `../Lavoro da remoto/input/` (non duplicati) |
| **Eurostat** `lfsa_eoqgan`, `lfsa_eoqgan2` | over-qualification rate, confronto UE, per cittadinanza/sesso/settore | scaricati via pacchetto R `eurostat` |
| **ISTAT** esploradati (SDMX `150_915_DF_DCCV_TAXOCCU1_2`) | tasso di occupazione per titolo di studio (contesto) | scaricato via MCP `istat` |

> Nota: esploradati ISTAT **non** ha un dataflow SDMX dedicato alla sovraistruzione (è
> pubblicata solo in report/BES). Per questo l'indicatore verticale è ricostruito dai
> microdati, ed Eurostat fa da validazione.

## Definizioni

- **Sovraistruzione (verticale)**: laureato (ISCED 5-8) occupato in professione ISCO 4-8.
  Tasso = sovraistruiti / laureati occupati in ISCO 1-8 (escluse Forze armate). È la
  definizione standard Eurostat/ISTAT.
- **Mismatch orizzontale**: campo di studio ≠ campo della professione. *Proxy* perché i
  microdati public-use hanno la professione solo a **ISCO 1-digit** → vedi limite sotto.

## Variabili RCFL usate

`COND3` (occupati), `COEF_CCP`/`COEFMI` (peso, /10), `PROF1` (ISCO 1-digit), `HATLEV3MOD`
(ISCED 3 livelli, dal 2021), `TISTUD` (titolo dettaglio, per la serie storica), `HATFIELD_D`
(area disciplinare, 14 cat., dal 2021), `RIP5`, `SESSO`, `CLETAS`, `CITTAD`.

Codifica `HATFIELD_D`: 002 Insegnamento · 003 Arte e design · 004 Letterario-umanistico ·
005 Scienze sociali/comunicazione · 006 Economico · 007 Giuridico · 008 Scientifico ·
009 Informatica/ICT · 010 Ingegneria industriale · 011 Architettura/Ing. civile ·
012 Agrario-forestale-veterinario · 013 Medico-sanitario-farmaceutico · 014 Servizi
(001 Programmi generici = residuale).

## Limiti

1. **Professione solo a ISCO 1-digit** nei microdati public-use → l'orizzontale è un proxy
   (non distingue dentro "professioni intellettuali" un ingegnere da un medico). Per
   l'orizzontale rigoroso per campo: **AlmaLaurea** (coerenza/efficacia del titolo) o ISCO
   2-3 digit.
2. **Break LFS 2021**: terziario via `TISTUD` (10 classi, codici 6-10) fino al 2020,
   `HATLEV3MOD==3` dal 2021. La serie non mostra salti artificiali sul 2020→2021.
3. **`HATFIELD_D` solo dal 2021**: l'analisi per campo è 2021-2025 (fotografia 2025); la
   serie storica del tasso complessivo è 2015-2025.
4. **Indicatore oggettivo (ISCO)**, diverso dalla sovraistruzione *percepita* del rapporto
   ISTAT "Giovani 2024" (modulo ad hoc, non ricostruibile dai microdati standard).

## Script (`script/`)

1. `01_sovraistruzione_2025.R` — verticale, fotografia 2025: totale, campo, territorio, sesso/età/cittadinanza, cross.
2. `02_serie_storica_sovraistruzione.R` — serie 2015-2025 (trimestrale + annuale).
3. `03_orizzontale_2025.R` — proxy orizzontale: distribuzione ISCO per campo + coerenza.
4. `04_eurostat_overqualification.R` — confronto UE (ranking, sesso, cittadinanza, settore).

## Output (`output/`)

`v*` = verticale · `h*` = orizzontale · `e*` = Eurostat · `i*` = ISTAT contesto ·
**`FINDINGS.md`** = sintesi dei risultati per l'articolo.
