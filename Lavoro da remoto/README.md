# Lavoro da remoto in Italia

Serie storica trimestrale della quota di occupati che lavorano da casa, costruita dai microdati MICRO.STAT della Rilevazione sulle forze di lavoro (RCFL) ISTAT.

## Fonte dati

Microdati MICRO.STAT RCFL — dati trasversali trimestrali:

https://www.istat.it/microdati/rilevazione-sulle-forze-di-lavoro-dati-trasversali-trimestrali/

Per ogni trimestre si scarica un archivio zip con i microdati (`MICRODATI/RCFL_Microdati_*.txt`) e i metadati (tracciato record, classificazioni, questionario, nota metodologica). In `input/` è conservato solo il file `.txt` dei microdati di ciascun trimestre dal 2015 Q1 al 2025 Q4 (44 trimestri, ~3,7 GB complessivi). La cartella è esclusa dal versionamento tramite `.gitignore`.

I file 2016 Q2 e 2017 Q2 hanno suffisso `AD-HOC` perché contengono il modulo ad-hoc UE accodato alle variabili standard.

## Variabile di interesse — attenzione al break di serie nel 2021

La domanda sul lavoro da casa nel questionario RCFL è cambiata con il regolamento UE 2019/1700, che ha riprogettato la LFS europea a partire dal 2021. Per costruire una serie storica continua bisogna tener presente questo break.

| Periodo | Variabile | Domanda | Modalità |
|---|---|---|---|
| 2015 Q1 → 2020 Q4 | `C48` | "Per accordo con il datore di lavoro, nelle 4 settimane … NOME ha effettuato a casa ore di lavoro retribuite o che possono essere recuperate?" | 001 = Sì, 2 o più volte a settimana<br>002 = Sì, meno di 2 volte a settimana<br>003 = No<br>997 = Non sa |
| 2021 Q1 → 2025 Q4 | `QC52` | "Sempre facendo riferimento al suo lavoro principale, nelle 4 settimane … NOME ha lavorato da casa? (compreso telelavoro e smart work)" | 1 = Sì, per la maggior parte del tempo (almeno la metà)<br>2 = Sì, qualche volta (meno della metà)<br>3 = No, mai<br>4 = Sì, è il mio esclusivo luogo di lavoro<br>997 = Non sa |

La nuova formulazione (QC52) è più ampia: include esplicitamente telelavoro e smart working e non richiede che le ore siano retribuite o "in accordo col datore". Le due variabili non sono quindi pienamente confrontabili. Un proxy ragionevole è:

- **C48** "almeno qualche volta lavora da casa" = (`001` + `002`)
- **QC52** "almeno qualche volta lavora da casa" = (`1` + `2` + `4`)

Vanno comunque commentati assieme con avvertenza esplicita.

## Pesi

Il peso campionario è `COEF_CCP` (con 1 decimale virtuale: dividere per 10). La condizione è `COND3` (1 = occupati, 2 = in cerca, 3 = inattivi). Sezione comune a tutti gli anni dal 2015.

Universo di riferimento per il lavoro da remoto: occupati (`COND3 == 1`).

## Struttura cartella

```
Lavoro da remoto/
├── README.md
├── input/    # microdati .txt 2015 Q1 → 2025 Q4 (gitignored)
├── script/   # script R
└── output/   # grafici e CSV
```
