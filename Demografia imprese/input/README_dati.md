# Demografia d'impresa ISTAT 2004-2024 — dati consolidati

Dataset costruito unendo le **4 edizioni** del report ISTAT *Demografia d'impresa*
(ognuna copre 6 anni con 1 anno di sovrapposizione con la successiva). I CSV MASTER
puliti sono in `input/`; i CSV grezzi per edizione sono in `input/raw/`.

Prodotto da `script/consolida.py` (rieseguibile/idempotente, interprete
`/opt/homebrew/bin/python3`, pandas). Formato di tutti i master: UTF-8, separatore
virgola, decimale punto, precisione piena (float non arrotondati; i conteggi restano
interi). Ogni master ha la colonna `edizione` (o `fonte_anno` per la serie nazionale).

## Edizioni e file sorgente (cartella `/Users/lorenzoruffino/Desktop/IMPRESE/`)

| edizione   | prefisso | anni      | file Excel sorgente                          |
|------------|----------|-----------|----------------------------------------------|
| 2004-2009  | ed0409   | 2004-2009 | `tavole_SB.xls`                              |
| 2009-2014  | ed0914   | 2009-2014 | `Appendice report demografia d'impresa.xlsx` |
| 2014-2019  | ed1419   | 2014-2019 | `TAVOLE_DEMOGRAFIA2021.xlsx`                 |
| 2019-2024  | ed1924   | 2019-2024 | `TAVOLE-STATISTICHE.xlsx`                    |

Anni di **sovrapposizione**: 2009 (ed0409/ed0914), 2014 (ed0914/ed1419), 2019
(ed1419/ed1924). Nelle serie "preferite" (`*_serie.csv` e `serie_nazionale`) per gli
anni doppi si tiene sempre l'**edizione più recente** (2009←ed0914, 2014←ed1419,
2019←ed1924).

## Armonizzazione applicata ai master

- **Macrosettori**: `Industria in s.s.` e `INDUSTRIA in senso stretto` → `Industria in senso stretto`; `Altri Servizi` → `Altri servizi`; `Totale ` → `Totale`.
- **Settori tecnologia**: `Atra Industria (B,D,E)` → `Altra industria (B,D,E)` (refuso ISTAT). *NB: le etichette maiuscole `COSTRUZIONI` e `SERVIZI` della tavola per intensità tecnologica sono conservate verbatim (nessuna regola di rinomina le riguardava); solo `INDUSTRIA in senso stretto` è stata normalizzata come da specifica.*
- **Territori**: `Friuli-V.G.` → `Friuli-Venezia Giulia`; `Sud-Isole`/`Sud-isole` → `Sud e Isole`; `Nord-ovest` → `Nord-Ovest`; `Nord-est` → `Nord-Est`. Aggiunta colonna `tipo_territorio`: `regione` (19) | `provincia_autonoma` (Trento, Bolzano) | `ripartizione` (Nord-Ovest, Nord-Est, Centro, Sud e Isole) | `paese` (Italia) = 26 territori.
- Trim degli spazi iniziali/finali su tutte le colonne stringa.

---

## MASTER FILE

### 1. `demografia_macrosettori.csv` (120 righe)
- **Descrizione**: unione delle 4 edizioni — tassi di natalità/mortalità, imprese nate/cessate e turnover netto per i 5 macrosettori (Industria in senso stretto, Costruzioni, Commercio, Altri servizi, Totale). 5 × 6 anni × 4 edizioni.
- **Colonne**: `edizione, macrosettore, anno, provvisorio, tasso_natalita, imprese_nate, tasso_mortalita, imprese_cessate, turnover_netto`.
- **Fonte**: Tavola 1 di ogni edizione (per ed1419/ed1924 è la Tavola 2 "macrosettori").
- **Copertura**: 2004-2024 con gli anni doppi 2009/2014/2019 presenti in due edizioni.
- **Caveat**: l'ultimo anno di ogni edizione (2009, 2014, 2019, 2024) ha `provvisorio=True` (stima delle cessate/mortalità).

### 2. `demografia_macrosettori_serie.csv` (105 righe)
- **Descrizione**: serie preferita 2004-2024 senza duplicati (overlap → edizione più recente), 5 macrosettori × 21 anni. Aggiunta colonna `saldo = imprese_nate − imprese_cessate`.
- **Colonne**: come sopra + `saldo`.
- **Fonte**: derivata da `demografia_macrosettori.csv`.
- **Copertura**: 2004-2024, continua senza buchi per tutti i macrosettori.
- **Caveat**: 2024 provvisorio. Gli anni-cerniera prendono il valore finale (non provvisorio) dell'edizione nuova.

### 3. `demografia_regioni.csv` (624 righe)
- **Descrizione**: unione delle 4 edizioni — natalità/mortalità/turnover per 26 territori × 6 anni × 4 edizioni.
- **Colonne**: `edizione, territorio, tipo_territorio, anno, provvisorio, tasso_natalita, tasso_mortalita, turnover_netto, turnover_calcolato`.
- **Fonte**: Tavola 3 (ed0409) / Tavola 4 (ed0914, ed1419, ed1924) "regioni e ripartizioni".
- **Copertura**: 2004-2024 con overlap.
- **Caveat**: per l'edizione **2004-2009 il turnover netto non è nel file** ed è calcolato come `tasso_natalita − tasso_mortalita` → colonna `turnover_calcolato=True` (solo per quelle 156 righe; `False` per le altre edizioni, dove il turnover è di fonte). Ultimo anno di ogni edizione provvisorio. **Per regione esistono SOLO i tassi: i conteggi assoluti (nate/cessate) per territorio non sono MAI pubblicati.**

### 4. `demografia_regioni_serie.csv` (546 righe)
- **Descrizione**: serie preferita 2004-2024 senza duplicati, 26 territori × 21 anni.
- **Colonne**: come `demografia_regioni.csv`.
- **Fonte**: derivata da `demografia_regioni.csv`.
- **Copertura**: 2004-2024, continua per tutti i territori.
- **Caveat**: idem punto 3 (turnover calcolato per gli anni 2004-2008 che provengono da ed0409).

### 5. `settori_nace_2008_2014.csv` (240 righe)
- **Descrizione**: natalità/mortalità (e turnover netto dove disponibile) per settore NACE Rev.2 (30 voci, incl. `Totale`), unione ed0409 (2008-2009) + ed0914 (2009-2014). Overlap 2009 mantenuto (due edizioni).
- **Colonne**: `edizione, settore_codice, settore_nome, anno, provvisorio, tasso_natalita, tasso_mortalita, turnover_netto` (turnover **vuoto** per tutte le 60 righe ed0409; la riga `Totale` ha `settore_codice` vuoto).
- **Fonte**: Tavola 2 (ed0409) + Tavola 3 (ed0914).
- **Copertura**: 2008-2014. **Il dettaglio settoriale NACE NON esiste per il 2004-2007.**
- **Caveat**: i livelli di aggregazione NACE non coincidono perfettamente tra le due edizioni per alcuni settori; nomi settore troncati con ellissi nella fonte ed0914; per il settore 35 (energia) 2009 la Tavola 2 ed0409 riporta un valore arrotondato (17.3) diverso dalla Figura 3 (17.255…).

### 6. `settori_tecnologia_2014_2024.csv` (156 righe)
- **Descrizione**: tassi + imprese nate/cessate per i 13 settori classificati per **intensità tecnologica e di conoscenza** (INDUSTRIA in s.s., HT, MHT, MLT, LOT, Altra industria B/D/E, COSTRUZIONI, SERVIZI, HITS, KWNMS, Servizi finanziari, Altri servizi, Totale). ed1419 + ed1924, 13 × 6 × 2. Overlap 2019 mantenuto.
- **Colonne**: `edizione, settore, anno, provvisorio, tasso_natalita, imprese_nate, tasso_mortalita, imprese_cessate` (nessun turnover in questa tavola).
- **Fonte**: Tavola 3 di ed1419 e ed1924.
- **Copertura**: 2014-2024 con overlap.
- **Caveat**: **la classificazione settoriale cambia nel 2014**: dal 2014 si passa dai gruppi NACE (file 5) alla classificazione per intensità tecnologica. **Le due serie settoriali NON sono direttamente confrontabili.** Doppio spazio interno in `Manifatture ad  Alta tecnologia (HT)` conservato dalla fonte.

### 7. `settori_tecnologia_serie.csv` (143 righe)
- **Descrizione**: serie preferita 2014-2024 senza overlap, 13 settori × 11 anni (2019 da ed1924).
- **Colonne**: come file 6.
- **Fonte**: derivata da `settori_tecnologia_2014_2024.csv`.
- **Copertura**: 2014-2024 continua.

### 8. `sopravvivenza_coorti.csv` (300 righe)
- **Descrizione**: tassi di sopravvivenza per macrosettore, coorte di nascita e anni dalla nascita (matrice triangolare). 5 macrosettori × 15 osservazioni (5+4+3+2+1) per edizione × 4 edizioni.
- **Colonne**: `edizione, macrosettore, coorte, anno_osservazione, anni_dalla_nascita, tasso_sopravvivenza`.
- **Fonte**: Tavola 4 (ed0409) / Tavola 5 (ed0914, ed1419, ed1924).
- **Copertura**: coorti **2004-2023** (ed0409: 2004-2008; ed0914: 2009-2013; ed1419: 2014-2018; ed1924: 2019-2023).
- **Caveat**: struttura triangolare — la sopravvivenza **a 5 anni** è osservabile solo per le coorti a inizio edizione (2004, 2009, 2014, 2019); le coorti successive hanno finestre più corte (coorte 2013 → solo 1 anno, ecc.). `anni_dalla_nascita = anno_osservazione − coorte`.

### 9. `addetti_coorti.csv` (20 righe)
- **Descrizione**: addetti delle coorti 2004/2010/2014/2019 per macrosettore: addetti al t0 delle nate (a), addetti al t0 delle sopravviventi (b), addetti a fine finestra delle sopravviventi (c) + le 3 variazioni percentuali. 5 macrosettori × 4 coorti.
- **Colonne**: `edizione, coorte, orizzonte_anni, macrosettore, addetti_t0_nate, addetti_t0_sopravviventi, addetti_tfin_sopravviventi, perdita_pct_da_cessazioni, crescita_pct_sopravviventi, variazione_pct_netta`.
- **Fonte**: Tavola 7 (ed0409) / Tavola 6 (ed0914, ed1419, ed1924).
- **Copertura**: solo 4 coorti (2004, 2010, 2014, 2019).
- **Caveat**: **orizzonte 5/4/5/5 anni** — la coorte 2010 (ed0914) ha finestra 2010→2014 = **4 anni** (`orizzonte_anni=4`), non 5 come le altre. `addetti_t5_sopravviventi` di ed0409/ed1419/ed1924 e `addetti_tfin_sopravviventi` di ed0914 sono unificati nella colonna `addetti_tfin_sopravviventi`. Refuso nel titolo del foglio ed0914 ("tre anni").

### 10. `classi_dipendenti_2009_2014.csv` (30 righe)
- **Descrizione**: tassi di natalità e mortalità per classe di dipendenti (0, 1-4, 5-9, 10+, Totale) × 6 anni.
- **Colonne**: `edizione, classe_dipendenti, anno, tasso_natalita, tasso_mortalita`.
- **Fonte**: Tavola 2 di ed0914.
- **Copertura**: **solo 2009-2014** (dettaglio per classe di dipendenti disponibile solo in questa edizione).

### 11. `dimensione_media_2004_2009.csv` (60 righe)
- **Descrizione**: dimensione media (addetti per impresa) delle imprese della coorte 2004 sopravviventi, per macrosettore e per ripartizione geografica × 6 anni.
- **Colonne**: `edizione, gruppo, tipo_gruppo, anno, addetti_medi` (`tipo_gruppo` = `macrosettore` | `ripartizione`).
- **Fonte**: Tavole 5 e 6 di ed0409.
- **Copertura**: **solo 2004-2009**. Il `Totale` compare due volte per anno (un macrosettore + una ripartizione), distinto da `tipo_gruppo`.

### 12. `stock_settori_2009.csv` (30 righe)
- **Descrizione**: demografia imprese 2009 per settore NACE Rev.2: stock, nate, morte (stima), tassi, turnover.
- **Colonne**: `edizione, settore_codice, stock_2009, nate_2009, morte_2009_stima, tasso_natalita, tasso_mortalita, turnover`.
- **Fonte**: Figura 3 (blocco IMPRESE) di ed0409.
- **Copertura**: **solo 2009** (foto istantanea).
- **Caveat**: le morti 2009 sono una stima.

### 13. `serie_nazionale_2004_2024.csv` (21 righe)
- **Descrizione**: serie nazionale annuale costruita dalle righe `Totale` della serie preferita (file 2).
- **Colonne**: `anno, tasso_natalita, tasso_mortalita, turnover_netto, imprese_nate, imprese_cessate, saldo, provvisorio, fonte_anno` (`fonte_anno` = edizione da cui proviene l'anno).
- **Fonte**: `demografia_macrosettori_serie.csv` (righe `Totale`).
- **Copertura**: 2004-2024 continua.
- **Verifica di coerenza**: confrontata con `ed1924_serie_tassi_2006_2024` (grafico web ISTAT) per gli anni 2006-2024 — **nessuno scostamento > 0.05 punti percentuali** su natalità/mortalità.
- **Caveat**: 2024 provvisorio (stima cessate).

---

## File grezzi NON confluiti nei master (restano in `input/raw/`)

Estratti "extra" tenuti per completezza ma non usati nei master (citati come da specifica):

- `ed0409_addetti_coorte2004_percorso.csv` — Figura 6: percorso addetti coorte 2004 (quota alla nascita / creati dopo), 4 macrosettori × 6 anni.
- `ed0409_sopravvivenza_ripartizioni_coorte2004.csv` — Figura 4: quota sopravviventi coorte 2004 per ripartizione (indice base 2004=1).
- `ed0409_sopravvivenza_macrosettori_coorte2004_indice.csv` — Figura 5: indice sopravvivenza coorte 2004 per macrosettore (derivato dalla Tavola 4).
- `ed0409_addetti_settori_2009.csv` — Figura 3 (r43-72): tassi di turnover 2009 di addetti e imprese per settore (denominazioni estese).
- `ed0409_addetti_demografia_settori_2009.csv` — Figura 3 (blocco ADDETTI): stessa struttura di `stock_settori_2009` ma riferita agli addetti (dato unico).
- `ed0409_figura1e2.csv` — Figure 1 e 2: tassi natalità/mortalità per classe di addetti (0, 1-4, 5-9, 10+, Totale). **ATTENZIONE: valori come FRAZIONI (tasso/100), non in percentuale.**
- `ed1419_serie_tassi_2006_2019.csv` — serie nazionale 2006-2019 (grafico web ed1419), in percentuale.
- `ed1419_totale_annuale.csv`, `ed1924_totale_annuale.csv` — Tavola 1 "totale economia" (ridondante con le righe `Totale` dei macrosettori).
- `ed1924_serie_tassi_2006_2024.csv` — serie nazionale lunga 2006-2024 (grafico web ed1924); usata per la verifica di coerenza di `serie_nazionale`.

---

## GAP noti del dataset

- Dettaglio settoriale **NACE assente per il 2004-2007** (parte solo dal 2008).
- **La classificazione settoriale cambia nel 2014**: da gruppi NACE (file 5) a intensità tecnologica (file 6). Le due serie NON sono confrontabili.
- **Sopravvivenza a 5 anni** osservabile solo per le coorti 2004, 2009, 2014, 2019; le coorti successive hanno finestre più corte (2010→4 anni, 2013→1 anno, ecc.).
- **Addetti per coorte** solo per 4 coorti, con orizzonte di 4 anni (non 5) per la coorte 2010.
- **Classi di dipendenti** solo 2009-2014; **dimensione media** solo 2004-2009.
- **Conteggi assoluti per regione MAI presenti** (solo tassi): niente nate/cessate territoriali.
- Discordanze di arrotondamento **già presenti nella fonte** tra tavole diverse per lo stesso aggregato (es. natalità del Totale a 1 vs più decimali) e agli anni-cerniera (la mortalità dell'ultimo anno di ogni edizione è stima provvisoria, poi rivista nell'edizione successiva).

## Caveat metodologici generali (ISTAT)

- **Definizioni**: tasso di natalità = imprese nate / imprese attive nell'anno × 100 (analogo per la mortalità). La **popolazione** di riferimento sono le imprese attive dell'**industria e dei servizi di mercato**.
- Si contano le **"vere" nascite e cessazioni**, al netto di trasformazioni, fusioni, scorpori e cambi di attività/forma giuridica.
- **ATECO 2007 / NACE Rev.2** in vigore dai dati **2007** in poi.
- La **mortalità dell'ultimo anno** di ogni edizione è una **stima provvisoria** (`provvisorio=True`), rivista nell'edizione successiva (per questo agli anni-cerniera le cessate cambiano mentre le nate no).
- **NON confrontabile** con i dati camerali **Movimprese** (Unioncamere/InfoCamere): universo, definizioni e criteri sono diversi.
