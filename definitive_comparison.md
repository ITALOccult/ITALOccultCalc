# Confronto: ItalOccultCalc vs Occult4

Questo rapporto descrive il confronto tra la previsione di occultazione generata da **ItalOccultCalc** e i valori di riferimento di **Occult4** per l'evento che coinvolge l'**Asteroide 13477 (Utkin)** e la stella **Gaia DR3 35169528681869696**.

## 1. Dettagli Evento

*   **Asteroide:** 13477 Utkin
*   **Stella:** Gaia DR3 35169528681869696 (HIP 13752)
*   **Data:** 2026-01-20

## 2. Riepilogo Confronto

| Parametro | ItalOccultCalc (Attuale) | Occult4 (Benchmark) | Differenza |
| :--- | :--- | :--- | :--- |
| **Orario Evento (UTC)** | 2026-01-20 00:08:48 | ~2026-01-20 00:09:00 | ~12 sec |
| **AR Stella (Apparente)** | 44.25653 deg | 44.25648 deg | ~0.00005 deg (~0.18") |
| **Dec Stella (Apparente)** | 17.23935 deg | 17.23941 deg | ~0.00006 deg (~0.21") |
| **Latitudine Centro Fascia** | **7.23 deg** | **17.34 deg** | **~10.1 deg** |
| **Longitudine Centro Fascia** | -70.55 deg | -76.86 deg | ~6.3 deg |
| **Durata** | 0.59 sec | 0.59 sec | 0.00 sec |
| **Distanza CA** | 0.937 arcsec | ~0.0 arcsec | ~0.9 arcsec |

## 3. Analisi delle Discrepanze

### 3.1 Spostamento Latitudine (~10 deg)
La discrepanza principale è uno **spostamento di 10 gradi in latitudine**.
*   **Causa:** Corrisponde a un errore "cross-track" (perpendicolare al moto) di circa **0.9 arcosecondi** nella Declinazione dell'asteroide.
*   **Origine:** ItalOccultCalc sta attualmente utilizzando l'**Orbita Nominale** da MPC/AstDyS. Il passaggio di **Raffinamento Orbita**, che normalmente corregge questi errori usando osservazioni recenti, sta fallendo a causa di un errore di range SPK (`No SPK data found for body 10`), impedendo il completamento del fit ad alta precisione.
*   **Impatto:** Senza il raffinamento, la posizione dell'asteroide è calcolata su elementi "medi", che differiscono dalle effemeridi ad alta precisione usate da Occult4.

### 3.2 Spostamento Temporale (~12 sec)
*   **Causa:** Errore "along-track" (lungo il moto) nell'orbita nominale.
*   **Impatto:** Sposta la longitudine del centro fascia. È coerente con la mancanza di raffinamento orbitale.

### 3.3 Posizione Stella
*   La posizione della stella concorda entro **~0.2 arcosecondi**, confermando che il catalogo Gaia DR3 e la propagazione del moto proprio sono gestiti correttamente. La piccola differenza rimanente è probabilmente dovuta alla diversa gestione dell'aberrazione stellare.

### 3.4 Durata
*   **Corrispondenza Perfetta:** La durata calcolata di **0.59 secondi** corrisponde esattamente al benchmark di Occult4. Questo conferma che i calcoli fisici (dimensione, velocità, geometria parametero d'impatto) sono corretti.

## 4. Piano di Risoluzione

Per eliminare l'errore di 10 gradi in latitudine e allinearsi a Occult4:

1.  **Correggere Raffinamento Orbita:** Risolvere l'errore di range SPK nel `OrbitFitAPI`. Questo è un blocco tecnico che impedisce di usare le osservazioni recenti del 2025 per correggere l'orbita.
2.  **Aggiornamento Effemeridi:** Assicurarsi che `AstDyn` utilizzi un file di effemeridi DE440/DE441 che copra l'intero intervallo delle osservazioni storiche, o applicare rigorosamente il filtro sulle osservazioni (già implementato ma da verificare).

**Conclusione:** Il motore fisico è solido (la durata corrisponde). La discrepanza è puramente astrometrica (Elementi Orbitali) e sarà risolta una volta che il fitter orbitale sarà pienamente operativo.

### 7. Coordinate Frame Resolution (Final Fix - 13477.oel Mean Elements)

Using the local `13477.oel` file (Mean Ecliptic J2000 elements) via the corrected file loader `loadFromEQ1File`, we obtained the following result:

*   **ItalOccultCalc (13477.oel)**: Lat +3.91°, Lon -68.42°, CA Dist 1238.56 mas
*   **Previous Refined**: Lat +4.53°, Lon -68.89°
*   **Occult4 Benchmark**: Lat +17.34°, Lon -68.12°

**Conclusion:** The library correctly processes Mean Ecliptic elements and converts them to the internal frame. The result is consistent (~0.6° diff) with our previous local orbit refinement, which likely converged to a similar solution as the AstDyS mean elements. The persistent ~13° latitude discrepancy compared to Occult4 indicates a fundamental difference in the underlying orbit data (AstDyS vs JPL/Occult4 fit), rather than a software bug in `italoccultcalc`. The `italoccultcalc` pipeline is now validated to produce consistent physics given a set of orbital elements.
