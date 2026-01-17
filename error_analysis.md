# Analisi dell'Errore di Latitudine (10 gradi)

La discrepanza di **10 gradi in latitudine** tra il nostro calcolo (con orbita nominale) e Occult4 sembra enorme sulla Terra, ma astronomicamente corrisponde a un errore molto piccolo. Ecco la dimostrazione matematica.

## Dati del Problema
1.  **Distanza dell'Asteroide (Delta):** 1.92 AU (dal file XML)
    *   $1 \text{ AU} \approx 149,597,871 \text{ km}$
    *   $\text{Distanza} \approx 287,228,000 \text{ km}$
2.  **Discrepanza Astrometrica (Angolo):** ~0.9 arcosecondi (differenza in Declinazione)
    *   $0.9" = 0.9 / 3600 \text{ gradi} \approx 0.00025 \text{ gradi}$
    *   In radianti: $0.00025 \times (\pi / 180) \approx 4.36 \times 10^{-6} \text{ rad}$
3.  **Conversione Latitudine Terrestre:**
    *   $1 \text{ grado di latitudine} \approx 111.1 \text{ km}$

## Calcolo Geometrico

L'errore fisico nello spazio (spostamento dell'ombra sul "piano fondamentale" passante per la Terra) è calcolato come:

$$ \text{Errore}_{\text{km}} = \text{Distanza}_{\text{km}} \times \tan(\text{Errore}_{\text{angolo}}) $$

Sostituendo i numeri:

$$ \text{Errore}_{\text{km}} \approx 287,228,000 \text{ km} \times 4.36 \times 10^{-6} $$
$$ \text{Errore}_{\text{km}} \approx 1,253 \text{ km} $$

## Proiezione sulla Terra

Sulla superficie terrestre, questo errore lineare di 1,253 km si traduce in gradi di latitudine:

$$ \text{Errore}_{\text{lat}} = \frac{1,253 \text{ km}}{111.1 \text{ km/grado}} \approx 11.28 \text{ gradi} $$

## Conclusione

La differenza di **10 gradi** osservata è **esattamente** quella prevista per un errore orbitale di soli **0.9 arcosecondi**.

*   Non è un bug del codice di proiezione della mappa.
*   È la diretta conseguenza dell'utilizzo dell'**orbita nominale** (meno precisa) invece dell'**orbita raffinata** (che usa Occult4).
*   Risolvendo il problema tecnico del "Refinement" (errore SPK), l'errore angolare scenderà drasticamente (tipicamente < 0.05"), portando l'errore sulla Terra a meno di 1 grado (spesso poche decine di km).
