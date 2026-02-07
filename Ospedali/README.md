# Accessibilita' ospedaliera in Europa

Analisi della popolazione europea in base al tempo di percorrenza in auto verso l'ospedale piu' vicino, con dettaglio per stato, regione e provincia italiana.

## Fonti dati

### Tempi di accesso agli ospedali
- **Dataset:** Accessibility to healthcare services (euro_access_healthcare_2023_100m_v2026_01)
- **Fonte:** Eurostat/GISCO — [Geographic accessibility](https://ec.europa.eu/eurostat/web/gisco/geodata/geographic-accessibility#Accessibility%20to%20healthcare%20services)
- **Documentazione:** [metadata.pdf](https://gisco-services.ec.europa.eu/pub/accessibility/services/metadata.pdf)
- **Anno di riferimento:** 2023
- **Risoluzione originale:** 100m
- **Proiezione:** ETRS89-LAEA (EPSG:3035)
- **Indicatore utilizzato:** Banda 1 (n1) — tempo di guida minimo verso il servizio sanitario piu' vicino, in secondi
- **Rete stradale:** TomTom Multinet 2023
- **Copertura:** Paesi UE (escluse regioni francesi d'oltremare), Norvegia, Svizzera, Albania

### Popolazione
- **Dataset:** GHS-POP R2023A (GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0)
- **Fonte:** European Commission, Joint Research Centre (JRC) — [Global Human Settlement Layer](https://human-settlement.emergency.copernicus.eu/ghs_pop2023.php)
- **Anno di riferimento:** 2020
- **Risoluzione originale:** 100m
- **Proiezione originale:** World Mollweide (EPSG:54009)
- **Citazione:** Schiavina M., Freire S., Carioli A., MacManus K. (2023): GHS-POP R2023A - GHS population grid multitemporal (1975-2030), European Commission, Joint Research Centre (JRC). DOI: 10.2905/2FF68A52-5B5B-4A22-8F40-C41DA8332CFE

### Confini amministrativi
- **Dataset:** NUTS 2021 (risoluzione 1:3M)
- **Fonte:** Eurostat/GISCO tramite pacchetto R `giscoR`
- **Livelli utilizzati:** NUTS0 (stati), NUTS2 (regioni italiane), NUTS3 (province italiane)

## Metodologia

1. **Caricamento raster accessibilita'** — il GeoTIFF Eurostat contiene il tempo di guida verso l'ospedale piu' vicino per ogni cella 100m. I valori originali in secondi sono convertiti in minuti.
2. **Caricamento raster popolazione** — 21 tile GHS-POP a 100m in proiezione Mollweide vengono uniti in un mosaico.
3. **Aggregazione a 1000m** — entrambi i raster vengono aggregati da 100m a 1000m: la popolazione con somma (per preservare i conteggi), l'accessibilita' con media.
4. **Riproiezione** — il raster popolazione viene riproiettato da Mollweide a EPSG:3035 e allineato alla griglia dell'accessibilita'.
5. **Estrazione zonale** — per ogni zona amministrativa (stato/regione/provincia), i pixel di tempo e popolazione vengono estratti usando `exactextractr`. La popolazione di ogni pixel viene pesata per la frazione di copertura del poligono.
6. **Calcolo statistiche:**
   - Popolazione classificata in fasce di 5 minuti (0-5, 5-10, ..., 55-60, 60+)
   - Media ponderata del tempo di accesso (pesata per popolazione)
   - Mediana ponderata del tempo di accesso (pesata per popolazione)

## Output

Tutti i file si trovano nella cartella `output/`.

| File | Contenuto |
|------|-----------|
| `stati_eu_fasce_5min_wide.csv` | 30 stati europei, una riga per stato |
| `stati_eu_fasce_5min_long.csv` | Stesso dato in formato lungo |
| `italia_regioni_fasce_5min_wide.csv` | 21 regioni italiane (NUTS2) |
| `italia_regioni_fasce_5min_long.csv` | Stesso dato in formato lungo |
| `italia_province_fasce_5min_wide.csv` | 107 province italiane (NUTS3) |
| `italia_province_fasce_5min_long.csv` | Stesso dato in formato lungo |

### Colonne dei file wide

| Colonna | Descrizione |
|---------|-------------|
| `id` | Codice NUTS |
| `nome` | Nome della zona (in italiano per gli stati) |
| `media_min` | Tempo medio di accesso ponderato per popolazione (minuti) |
| `mediana_min` | Tempo mediano di accesso ponderato per popolazione (minuti) |
| `pop_totale` | Popolazione totale della zona |
| `0-5 min` ... `60+ min` | Popolazione in ciascuna fascia di tempo |

### Paesi esclusi
- **Microstati:** Liechtenstein, Monaco, Andorra, San Marino, Vaticano
- **Fuori copertura:** Islanda
- **Dati parziali nel raster:** Montenegro, Macedonia del Nord, Serbia, Turchia, Regno Unito

## Requisiti

Script R (`analisi_accessibilita.R`) con le seguenti librerie: `terra`, `sf`, `giscoR`, `dplyr`, `tidyr`, `readr`, `exactextractr`.

## Licenze

- Dati Eurostat/GISCO: soggetti al [copyright e licenza generale Eurostat](https://ec.europa.eu/eurostat/web/main/help/copyright-notice)
- GHS-POP JRC: dati aperti, riutilizzo autorizzato con citazione della fonte
