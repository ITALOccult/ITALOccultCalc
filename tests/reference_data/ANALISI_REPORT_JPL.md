# Analisi report test JPL consistency

Interpretazione dei risultati di `test_jpl_consistency_full` (JD 2461109.5 = 2026-Mar-10 00:00 TDB).

---

## 1. Terra (Earth vs Sun)

### Numeri dal report

| Grandezza | JPL (DE441, Ecliptic) | Nostro (SPICE → Ecliptic) | Delta |
|-----------|------------------------|----------------------------|--------|
| **Pos X** [AU] | -0.975353 | -0.974941 | +0.000412 |
| **Pos Y** [AU] |  0.188567 |  0.188518 | -0.000049 |
| **Pos Z** [AU] | -0.000003 | -0.000003 | ~0 |
| **\|Δpos\|** [AU] | — | — | **0.000415** (~62 100 km) |
| **Vel** [AU/day] | (-0.00355, -0.01695, 1e-6) | (-0.00355, -0.01695, 1e-6) | **~1e-6** |

### Interpretazione

- **Velocità**: l’accordo è ottimo (|Δv| ≈ 1e-6 AU/day). Le unità sono corrette (AU/day da SPICE) e la rotazione ICRF → Ecliptic è coerente.
- **Posizione**: c’è uno scostamento di **~0.000415 AU ≈ 62 000 km**, soprattutto lungo X (≈ +412 km in X, ≈ -49 km in Y). In termini angolari sull’orbita è circa **~85 arcsec** (0.000415 rad).

### Possibili cause della discrepanza di posizione

1. **Ephemeris diverso**  
   Riferimento: **DE441** (JPL Horizons). Noi: **de440.bsp**. DE440 e DE441 non sono identici; per un confronto stretto conviene usare lo stesso kernel (es. **de441.bsp** se disponibile) e ripetere il test.

2. **Interpolazione nella cache**  
   Per (399, 10) il reader SPICE usa una cache con **interpolazione di Lagrange** su 7 punti in 1 giorno. L’errore di interpolazione dovrebbe essere piccolo; se si vuole escluderlo si può aggiungere nel test una query “diretta” (senza cache) e confrontare.

3. **Obliquità**  
   In `coordinates.cpp` si usa **23.4392794444°** (IAU 2000), in `types.h` **23.4392911°** (JPL). La differenza è ~0.04″ e dà correzioni di pochi metri, non di 60 000 km. Per coerenza con JPL si può allineare tutto a **23.4392911°** (es. in `coordinates.cpp`).

4. **Convenzione frame**  
   SPICE: output in **ICRF (J2000)**; riferimento JPL: **Ecliptic J2000**. La rotazione usata è `Coordinates::equatorialToEcliptic` (asse X, obliquità). La convenzione è quella standard; nessun sospetto forte qui.

### Azioni suggerite (Terra)

- Eseguire il test con **de441.bsp** (stesso ephemeris del riferimento) e verificare se |Δpos| scende sotto la tolleranza.
- Allineare l’obliquità a **23.4392911°** ovunque (in particolare in `coordinates.cpp`) per coerenza con JPL.
- Se con de441 la discrepanza resta grande, controllare che il kernel SPK sia quello ufficiale JPL e che non ci siano offset di tempo (TDB/ET).

---

## 2. Stella (posizione astrometrica / apparente)

### Numeri dal report

- **Input**: RA=120°, Dec=10° (epoca JD 2457388.5 = J2016.0), PM = (10, -5) mas/yr, parallasse = 5 mas.
- **Output nostro**: posizione apparente a JD 2461109.5 → **RA ≈ 120.000028°, Dec ≈ 9.999986°**.

### Interpretazione

- Non c’è un riferimento esterno nel report (mancano `expected_app_ra_deg` e `expected_app_dec_dec` in `star_reference_jd2461109.5.txt`).
- Il modello è: **moto proprio** (da J2016.0 a 2026) + **parallasse** (vettore unitario stella − posizione Terra, normalizzato). Non è applicata aberrazione nel test.
- Lo spostamento rispetto alla posizione media è dell’ordine di **~0.03″ in RA** e **~-0.01″ in Dec**, compatibile con ~10 anni di PM e una parallasse di 5 mas. I valori sono plausibili.

### Azioni suggerite (Stella)

- Per un confronto quantitativo: inserire in `star_reference_jd2461109.5.txt` i valori attesi (es. da Gaia o da un tool che includa aberrazione se necessario) in `expected_app_ra_deg` e `expected_app_dec_deg`.
- Decidere se nel test si vuole includere anche l’**aberrazione stellare** e, in caso, allineare il modello al riferimento scelto.

---

## 3. Asteroide

### Report

- **SKIP**: in `jpl_horizons_asteroid_2026mar10.txt` sono ancora a zero X,Y,Z (dati non inseriti).

### Azioni suggerite (Asteroide)

- Compilare `jpl_horizons_asteroid_2026mar10.txt` con l’output Horizons (es. 433 Eros) per lo stesso JD, frame Ecliptic J2000, in AU e AU/day.
- Compilare `asteroid_elements_2026mar10.txt` con elementi equinoziali alla stessa epoca (es. da MPC/AstDyS).
- Rilanciare il test per avere un confronto posizione/velocità asteroide e interpretare eventuali discrepanze (propagazione, frame, epoca).

---

## Riepilogo

| Sezione   | Esito  | Interpretazione breve |
|-----------|--------|------------------------|
| **Terra** | FAIL   | Posizione ~62 000 km fuori (probabile DE440 vs DE441); velocità ottima → unità e rotazione OK. |
| **Stella** | —    | Solo valore calcolato; nessun riferimento per PASS/FAIL; valori plausibili. |
| **Asteroide** | SKIP | Dati di riferimento non ancora inseriti. |

Per ridurre i FAIL sul test Terra: usare **de441.bsp** e/o tolleranze meno strette; per stella e asteroide completare i file di riferimento come sopra.
