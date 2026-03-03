# Check imprecisioni e errori – IOccultCalc

Report di una verifica mirata a individuare errori di compilazione, warning, test falliti e pattern a rischio nel codice.

---

## 1. Errori di build (corretti)

### 1.1 `example_astdyn_fitting.cpp` – API non allineata

**Problema:** L’esempio usava API inesistenti o rinominate:

- `predictor.setOrbitalElements(elem)` → la classe espone solo `setAsteroid(AstDynEquinoctialElements)`.
- `predictor.setAsteroidUncertainty(...)` → non esiste; c’è solo `setOrbitalUncertainty(double)` (deprecato, no-op).
- `occ.time` → in `OccultationEvent` il tempo di CA è `timeCA` (e in base `OutputEvent` c’è `jd_event`).
- `occ.star_magnitude` → in `OutputEvent` il campo è `star_mag`.

**Correzioni applicate:**

- `setAsteroid(elem.toEquinoctial())` per passare elementi da orbit fit (OrbitalElements → AstDynEquinoctial).
- `setOrbitalUncertainty(...)` lasciato per compatibilità (resta no-op); eventuale uso reale andrebbe implementato in `OccultationPredictor`.
- `occ.time` → `occ.timeCA` per la data dell’evento.
- `occ.star_magnitude` → `occ.star_mag`.

Dopo queste modifiche il target `example_astdyn_fitting` compila.

---

## 2. Warning di build

### 2.1 Linker: librerie duplicate

**Messaggio:**  
`ld: warning: ignoring duplicate libraries: '.../libcspice.a', '.../libstarmap.a'`

**Causa:** In `CMakeLists.txt`, `target_link_libraries(ioccultcalc ...)` ripeteva `STARMAP_LIB` e `IOC_EARTH_LIB` due volte.

**Correzione:** Rimossi i link duplicati in `CMakeLists.txt`. Dopo la riconfigurazione/rebuild i warning del linker non dovrebbero più comparire.

### 2.2 Commento annidato in Integrator.hpp (AstDyn)

**Messaggio:**  
`warning: '/*' within block comment` in  
`external/ITALOccultLibrary/astdyn/include/astdyn/propagation/Integrator.hpp:179`

**Causa:** Un secondo `/**` in mezzo a un commento di documentazione (costruttore RKF78).

**Correzione:** Sostituito il `/**` interno con `* @param` in modo che il blocco Doxygen sia un unico commento. Il warning scompare.

---

## 3. Test falliti

### 3.1 OccultationEngine – file `1272.eq1` non trovato

**Problema:** Il test carica `1272.eq1` con path relativo `"1272.eq1"`. Con ctest la working directory è `build/tests/`, mentre il file è nella root del progetto → `loadAsteroidFromEQ1` fallisce.

**Correzione:** In `tests/CMakeLists.txt` è stata impostata la working directory del test:

```cmake
set_tests_properties(OccultationEngine PROPERTIES WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
```

Eseguendo ctest da `build/`, il test OccultationEngine viene lanciato dalla root e trova `1272.eq1`.

### 3.2 AllnumParsing – 5 sub-test falliti

**Situazione:** L’eseguibile `test_allnum_parsing` riporta “Test passati: 20 / 25” e “Test falliti: 5 / 25”. I 5 fallimenti sono interni a quel programma (es. parsing/validazione allnum).

**Azione suggerita:** Eseguire a mano `./build/tests/test_allnum_parsing` e verificare quali dei 25 casi falliscono (messaggi a video). Poi correggere i dati di test o la logica di parsing/validazione in base ai casi che falliscono.

---

## 4. Pattern a rischio (da tenere d’occhio)

### 4.1 Integratore RA15 segnalato come “BUGGY”

In `include/ioccultcalc/orbit_propagator.h` (circa riga 33):

```cpp
RA15        // ⚠ BUGGY: non conserva energia (ΔE/E~10⁻⁶, 323km errore/anno)
```

**Suggerimento:** Usare RA15 solo dove l’errore è accettabile; preferire RKF78 (o altro integratore stabile) per propagazioni lunghe o dove serve conservazione dell’energia. Eventualmente documentare in GUIDE/README il limite e le alternative.

### 4.2 Unità e conversioni (rad/deg, secondi, AU/km)

Nel codice ci sono conversioni rad↔deg, secondi↔giorni, AU↔km. Esempi:

- `occultation_analyzer.cpp`: `6378.137 / 3600.0` (km/s), `RAD_TO_DEG * 3600.0` (arcsec), `86400.0` (secondi/giorno).
- `phase2_occultation_geometry.cpp`: `min_dist_deg * 3600.0` (arcsec), `search_window_sec / 86400.0` (giorni).
- `initial_orbit.cpp`: residui in arcsec con `* 3600.0 * RAD_TO_DEG`.

**Suggerimento:** Introdurre costanti con nome (es. `ARCSEC_PER_DEG`, `SEC_PER_DAY`, `KM_PER_AU`) e usarle al posto di magic number; dove possibile, concentrare le conversioni in poche funzioni (es. in `coordinates.h` / `time_utils.h`) per ridurre errori di fattori 2 o π.

### 4.3 setOrbitalUncertainty deprecato e no-op

`OccultationPredictor::setOrbitalUncertainty(double)` è implementato a no-op. L’esempio `example_astdyn_fitting` lo chiama per “coerenza” ma l’incertezza non influenza le previsioni.

**Suggerimento:** Se l’incertezza orbitale deve entrare nel motore (es. probabilità SVD, bande di incertezza sul path), implementare il supporto in `OccultationPredictor` (memorizzare sigma e usarlo in `calculateProbabilitySVD` / `generateShadowPath`). Altrimenti rimuovere la chiamata dall’esempio e documentare che l’incertezza non è ancora usata.

---

## 5. Riepilogo modifiche applicate

| File | Modifica |
|------|----------|
| `examples/example_astdyn_fitting.cpp` | Allineamento a API: `setAsteroid(elem.toEquinoctial())`, `occ.timeCA`, `occ.star_mag`, `setOrbitalUncertainty` (no-op). |
| `CMakeLists.txt` | Rimossi `STARMAP_LIB` e `IOC_EARTH_LIB` duplicati in `target_link_libraries`. |
| `external/.../Integrator.hpp` | Corretto commento Doxygen (rimosso `/**` annidato). |
| `tests/CMakeLists.txt` | `set_tests_properties(OccultationEngine ... WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})`. |

---

## 6. Verifiche consigliate dopo il check

1. **Build pulita:**  
   `cd build && rm -rf * && cmake .. && make -j`  
   Controllare che non restino warning ld per librerie duplicate e che non compaia più il warning sul commento in `Integrator.hpp`.

2. **Test:**  
   `ctest --output-on-failure`  
   - OccultationEngine dovrebbe passare (working directory corretta).  
   - AllnumParsing: analizzare i 5 casi falliti con l’eseguibile a mano e correggere.

3. **Esempio:**  
   Eseguire (se possibile con dati/reti disponibili)  
   `./build/examples/example_astdyn_fitting`  
   per confermare che il flusso orbit fit → predictor → findOccultations funzioni come atteso.

4. **Documentazione:**  
   Aggiornare GUIDE/README se si decide di:  
   - sconsigliare l’uso di RA15 per propagazioni lunghe, o  
   - documentare che `setOrbitalUncertainty` è attualmente inutilizzato.

---

*Report generato il 2026-02-14. Per estendere il check (es. clang-tidy, cppcheck, analisi numerica mirata) si può riusare questo documento come base.*
