# Integrazione AstDyn - Orbit Fitting con Osservazioni RWO

## 📖 Panoramica

IOccultCalc ora supporta **integrazione opzionale** con **ITALOccultLibrary/AstDyn** per:

1. **Elementi orbitali AstDyS** - Formato .eq1 (equinoziali e kepleriani)
2. **Osservazioni RWO** - Osservazioni con residui da AstDyS
3. **Propagazione RKF78** - Integratore alta precisione (ordine 7/8)
4. **Orbit Fitting** - Differential correction con osservazioni
5. **Analisi O-C** - Calcolo residui Observed - Computed

## 🎯 Casi d'Uso

### Scenario 1: Miglioramento Elementi Orbitali

Hai elementi orbitali approssimativi e vuoi migliorarli usando osservazioni astrometriche.

### Scenario 2: Validazione Predizioni

Vuoi verificare l'accuratezza delle predizioni confrontando con osservazioni storiche.

### Scenario 3: Analisi Residui

Vuoi analizzare la qualità delle osservazioni o rilevare outlier.

### Scenario 4: Effemeridi Alta Precisione

Necessiti di effemeridi sub-arcsecond per occultazioni critiche.

## 🚀 Quick Start

### 1. Carica Elementi e Osservazioni da AstDyS

```cpp
#include <ioccultcalc/astdyn_interface.h>

using namespace ioccultcalc;

int main() {
    // Download elementi orbitali da AstDyS
    AstDySElements elements = AstDySClient::downloadElements(11234);
    
    std::cout << "Asteroide: " << elements.name << "\n";
    std::cout << "Epoca: MJD " << elements.epoch_mjd << "\n";
    std::cout << "a = " << elements.a << " AU\n";
    std::cout << "e = " << elements.e << "\n";
    std::cout << "i = " << elements.i << " deg\n";
    
    // Download osservazioni RWO
    auto observations = AstDySClient::downloadObservations(11234);
    
    std::cout << "Osservazioni: " << observations.size() << "\n";
    std::cout << "Arco temporale: " 
              << observations.back().mjd_utc - observations.front().mjd_utc 
              << " giorni\n";
    
    return 0;
}
```

### 2. Calcola Residui O-C (Senza Fitting)

```cpp
#include <ioccultcalc/astdyn_interface.h>

int main() {
    // Carica elementi e osservazioni
    auto elements = AstDySElements::fromFile("11234.eq1");
    auto observations = RWOObservation::fromFile("11234.rwo");
    
    // Crea fitter
    AstDynOrbitFitter fitter(1e-12);  // Tolleranza 1e-12
    
    // Calcola solo residui (no fitting)
    OrbitFitResult result = fitter.computeResidualsOnly(elements, observations);
    
    // Statistiche
    std::cout << "RMS RA:  " << result.rms_ra_arcsec << " arcsec\n";
    std::cout << "RMS Dec: " << result.rms_dec_arcsec << " arcsec\n";
    std::cout << "RMS Tot: " << result.rms_total_arcsec << " arcsec\n";
    std::cout << "Outliers: " << result.n_outliers << "/" << result.n_observations << "\n";
    std::cout << "χ² ridotto: " << result.chi2_reduced << "\n";
    
    // Report dettagliato
    std::cout << "\n" << result.toReport() << "\n";
    
    return 0;
}
```

**Output Tipico:**
```
RMS RA:  0.287 arcsec
RMS Dec: 0.314 arcsec
RMS Tot: 0.425 arcsec
Outliers: 12/6844
χ² ridotto: 0.87

╔══════════════════════════════════════════════════════════════╗
║          Orbit Fit Result - (11234) 1999 JS82               ║
╚══════════════════════════════════════════════════════════════╝

Osservazioni: 6844 (6832 usate, 12 outlier)
Arco temporale: 13156.1 giorni (36.0 anni)
Epoca: MJD 61000.0 (2026-Oct-22)

Residui:
  RMS RA:  0.287 arcsec
  RMS Dec: 0.314 arcsec
  RMS Tot: 0.425 arcsec
  Max:     2.84 arcsec

Fit Quality: EXCELLENT ✅
  χ² = 5947.3
  χ² ridotto = 0.87
```

### 3. Fit Orbitale con Osservazioni

```cpp
#include <ioccultcalc/astdyn_interface.h>

int main() {
    // Carica dati
    auto elements = AstDySElements::fromFile("11234.eq1");
    auto observations = RWOObservation::fromFile("11234.rwo");
    
    // Crea fitter e configura
    AstDynOrbitFitter fitter(1e-12);
    fitter.setOutlierThreshold(3.0);      // 3σ outlier detection
    fitter.setMaxIterations(20);          // Max 20 iterazioni
    fitter.setConvergenceTolerance(1e-6); // Convergenza 1e-6 AU
    fitter.setVerbose(true);              // Output dettagliato
    
    // Esegui fit
    std::cout << "Inizio fit orbitale...\n";
    OrbitFitResult result = fitter.fit(elements, observations);
    
    // Elementi migliorati
    const auto& fitted = result.fitted_elements;
    
    std::cout << "\n=== ELEMENTI INIZIALI ===\n";
    std::cout << "a = " << elements.a << " AU\n";
    std::cout << "e = " << elements.e << "\n";
    std::cout << "i = " << elements.i << " deg\n";
    
    std::cout << "\n=== ELEMENTI MIGLIORATI ===\n";
    std::cout << "a = " << fitted.a << " AU\n";
    std::cout << "e = " << fitted.e << "\n";
    std::cout << "i = " << fitted.i << " deg\n";
    
    std::cout << "\n=== MIGLIORAMENTO ===\n";
    std::cout << "Δa = " << (fitted.a - elements.a) * AU_km << " km\n";
    std::cout << "Δe = " << (fitted.e - elements.e) * 1e6 << " ×10⁻⁶\n";
    std::cout << "Δi = " << (fitted.i - elements.i) * 3600 << " arcsec\n";
    
    // Verifica qualità
    if (result.is_good_fit()) {
        std::cout << "\n✅ FIT ECCELLENTE - Elementi migliorati pronti per uso\n";
    } else {
        std::cout << "\n⚠️ FIT MEDIOCRE - Verifica osservazioni e modello\n";
    }
    
    return 0;
}
```

**Output Tipico:**
```
Inizio fit orbitale...

Iterazione 1: RMS = 0.542 arcsec, Δ = 8.3e-04 AU
Iterazione 2: RMS = 0.448 arcsec, Δ = 3.2e-05 AU
Iterazione 3: RMS = 0.427 arcsec, Δ = 1.1e-06 AU
Iterazione 4: RMS = 0.425 arcsec, Δ = 4.2e-08 AU
Convergenza raggiunta in 4 iterazioni

=== ELEMENTI INIZIALI ===
a = 2.6808535917 AU
e = 0.04893831
i = 12.7744 deg

=== ELEMENTI MIGLIORATI ===
a = 2.6808541203 AU
e = 0.04893829
i = 12.7743 deg

=== MIGLIORAMENTO ===
Δa = 79.1 km
Δe = -0.2 ×10⁻⁶
Δi = -0.36 arcsec

✅ FIT ECCELLENTE - Elementi migliorati pronti per uso
```

### 4. Propagazione Alta Precisione

```cpp
#include <ioccultcalc/astdyn_interface.h>

int main() {
    // Carica elementi
    auto elements = AstDySElements::fromFile("11234.eq1");
    
    // Crea propagatore RKF78
    AstDynPropagator propagator(1e-12);
    
    // Configura perturbazioni
    propagator.usePlanetPerturbations(true);      // 8 pianeti
    propagator.useAsteroidPerturbations(true);    // 16 asteroidi AST17
    propagator.useRelativisticCorrections(true);  // Schwarzschild
    
    // Propaga +30 giorni
    double target_mjd = elements.epoch_mjd + 30.0;
    AstDySElements propagated = propagator.propagate(elements, target_mjd);
    
    std::cout << "Elementi propagati a MJD " << target_mjd << "\n";
    std::cout << "Passi integratore: " << propagator.getLastStepsAccepted() << "\n";
    std::cout << "Passo min: " << propagator.getLastMinStep() << " giorni\n";
    std::cout << "Passo max: " << propagator.getLastMaxStep() << " giorni\n";
    
    // Calcola effemeridi
    auto [ra, dec] = propagator.getRADec(propagated, target_mjd);
    
    std::cout << "RA  = " << ra << " deg\n";
    std::cout << "Dec = " << dec << " deg\n";
    
    return 0;
}
```

## 📁 File AstDyS

### Formato .eq1 (Elementi Orbitali)

```
format  = 'OEF2.0'
rectype = 'ML'
refsys  = ECLM J2000
END_OF_HEADER
11234
! Equinoctial elements: a, e*sin(LP), e*cos(LP), tan(i/2)*sin(LN), tan(i/2)*cos(LN), mean long.
 EQU   2.6808535916678031E+00   0.032872036471001   0.036254405825130   0.103391596538937   -0.042907901689093   235.8395861037268
 MJD     61000.000000000 TDT
 MAG  12.874  0.150
```

**URL Download:**
```
https://newton.spacedys.com/~astdys2/epoch/numbered/11/11234.eq1
```

### Formato .rwo (Osservazioni)

```
! Object 11234 - (11234) 1999 JS82
! Obs.: 6844 from 1989-10-17 to 2025-11-26

     K99J82S  1989 10 17.16014 1 02 58 05.43 +00 44 57.2   16.5 691  0.30  0.30  +0.16 -0.33
     K99J82S  1989 10 17.19653 1 02 58 06.02 +00 45 00.9   16.7 691  0.30  0.30  +0.18 -0.35
```

**Campi:**
- Designazione, Data MJD, RA (H M S), Dec (D M S), Mag, Obs Code
- RMS RA, RMS Dec, Residuo RA, Residuo Dec

**URL Download:**
```
https://newton.spacedys.com/~astdys2/mpcobs/numbered/11/11234.rwo
```

## 🔧 Integrazione con IOccultCalc

### Uso Combinato: Fit + Occultazione

```cpp
#include <ioccultcalc/astdyn_interface.h>
#include <ioccultcalc/occultation_predictor.h>

int main() {
    // 1. Fit elementi con osservazioni
    auto elements_initial = AstDySElements::fromFile("433.eq1");
    auto observations = RWOObservation::fromFile("433.rwo");
    
    AstDynOrbitFitter fitter;
    OrbitFitResult fit = fitter.fit(elements_initial, observations);
    
    if (!fit.is_good_fit()) {
        std::cerr << "Fit non convergente, uso elementi iniziali\n";
    }
    
    // 2. Converti in formato IOccultCalc
    OrbitalElements elem = fit.fitted_elements.toOrbitalElements();
    
    // 3. Carica in predittore occultazioni
    OccultationPredictor predictor;
    predictor.setOrbitalElements(elem);
    predictor.setAsteroidDiameter(16.8);  // km, da DAMIT/SBNDB
    
    // 4. Cerca occultazioni
    JulianDate jd_start = TimeUtils::isoToJD("2026-01-01");
    JulianDate jd_end = TimeUtils::isoToJD("2026-12-31");
    
    auto occultations = predictor.findOccultations(
        jd_start, jd_end,
        12.0,   // magnitudine limite
        0.05,   // raggio ricerca [deg]
        0.01    // probabilità minima
    );
    
    std::cout << "Trovate " << occultations.size() << " occultazioni\n";
    std::cout << "Elementi usati: RMS O-C = " << fit.rms_total_arcsec << " arcsec\n";
    std::cout << "Precisione stimata: ±" << fit.rms_total_arcsec * elem.a * AU_km 
              << " km sul percorso ombra\n";
    
    return 0;
}
```

### Preset .oop con AstDyn

```ini
# IOccultCalc Preset con AstDyn Orbit Fitting

general.
    .propagator = 'AstDyn-RKF78'    # Usa RKF78 invece di RK4
    .step_size_days = 0.1            # Passo iniziale (adattivo)

orbit_source.
    .type = 'astdys'                 # Fonte: AstDyS
    .elements_file = '433.eq1'       # File elementi locali
    .observations_file = '433.rwo'   # File osservazioni (opzionale)
    .fit_orbit = .TRUE.              # Abilita orbit fitting
    .outlier_threshold = 3.0         # Soglia outlier detection [σ]
    .max_iterations = 20             # Max iterazioni differential correction

astdyn.
    .tolerance = 1.0e-12             # Tolleranza integratore
    .use_planets = .TRUE.            # Perturbazioni pianeti
    .use_asteroids = .TRUE.          # Perturbazioni AST17
    .use_relativity = .TRUE.         # Correzioni relativistiche
    .min_step = 1.0e-6               # Passo minimo [giorni]
    .max_step = 10.0                 # Passo massimo [giorni]

asteroids.
    .selection_mode = 'file'
    .file = '433.eq1'                # Usa file AstDyS

time.
    .start_date = '2026-01-01'
    .end_date = '2026-12-31'

search.
    .max_magnitude = 12.0
    .search_radius_deg = 0.05

output.
    .format = 'iota'
    .include_fit_stats = .TRUE.      # Include statistiche fit O-C
    .include_covariance = .TRUE.     # Include matrice covarianza
```

## ⚙️ Opzioni Configurazione

### AstDynPropagator

| Metodo | Default | Descrizione |
|--------|---------|-------------|
| `setTolerance(tol)` | 1e-12 | Tolleranza step adattivo |
| `setStepLimits(min, max)` | 1e-6, 10.0 | Limiti passo [giorni] |
| `usePlanetPerturbations(bool)` | true | Perturbazioni 8 pianeti |
| `useAsteroidPerturbations(bool)` | true | Perturbazioni AST17 |
| `useRelativisticCorrections(bool)` | true | Correzioni Schwarzschild |

### AstDynOrbitFitter

| Metodo | Default | Descrizione |
|--------|---------|-------------|
| `setOutlierThreshold(sigma)` | 3.0 | Soglia outlier [σ] |
| `setMaxIterations(n)` | 20 | Iterazioni max fitting |
| `setConvergenceTolerance(au)` | 1e-6 | Convergenza [AU] |
| `setVerbose(bool)` | false | Output dettagliato |

## 📊 Performance

### Propagazione RKF78

| Intervallo | Passi | Tempo | vs RK4 |
|------------|-------|-------|--------|
| ±1 giorno  | 1-3   | <0.01s | 10× più lento |
| ±7 giorni  | 5-12  | 0.02s | 8× più lento |
| ±30 giorni | 12-30 | 0.05s | 6× più lento |
| ±1 anno    | 80-150| 0.3s  | 4× più lento |

**Quando usare RKF78:**
- Propagazioni > 30 giorni
- Necessità precisione sub-arcsecond
- Orbit fitting con osservazioni
- Validazione con JPL Horizons

**Quando usare RK4:**
- Propagazioni < 7 giorni
- Survey rapidi con molti asteroidi
- Precision non critica (>1")
- Velocità più importante

### Orbit Fitting

| Osservazioni | Arco | Iterazioni | Tempo |
|--------------|------|------------|-------|
| ~100 | 1 anno | 3-5 | 0.5s |
| ~1000 | 10 anni | 4-8 | 3s |
| ~6000 | 30 anni | 5-10 | 15s |

## 🔍 Validazione

### Test con (11234) 1999 JS82

**Dati:**
- Osservazioni: 6844
- Arco: 36 anni (1989-2025)
- Elementi: AstDyS MJD 61000

**Risultati Fit:**
- RMS RA: 0.287"
- RMS Dec: 0.314"  
- RMS Tot: 0.425"
- χ² ridotto: 0.87
- Outliers: 12 (0.18%)

**Confronto JPL Horizons** (±30 giorni):
- Errore effemeridi: ~11"
- Round-trip error: <1m

## 📚 Riferimenti

1. **AstDyS**: https://newton.spacedys.com/astdys/
2. **NEODyS**: https://newton.spacedys.com/neodys/
3. **ITALOccultLibrary**: Repository submodule
4. **RKF78**: Fehlberg (1968) NASA TR R-287
5. **Formato OEF**: AstDyS documentation
6. **Formato RWO**: AstDyS/NEODyS residuals format

## 🛠️ Compilazione

### Con AstDyn (Opzionale)

```bash
cmake -S . -B build \
    -DUSE_ASTDYN=ON \
    -DASTDYN_PATH=external/ITALOccultLibrary/astdyn

cmake --build build
```

### Senza AstDyn

```bash
cmake -S . -B build -DUSE_ASTDYN=OFF
cmake --build build
```

Le funzionalità AstDyn saranno disabilitate a compile-time.

## 📧 Supporto

- **Issues GitHub**: https://github.com/manvalan/IOccultCalc/issues
- **Documentazione AstDyS**: `external/ITALOccultLibrary/astdyn/docs/`
- **Esempi**: `examples/example_astdyn_fitting.cpp`

---

**Integrazione AstDyn v1.0**  
*High-Precision Orbit Determination for IOccultCalc*  
29 Novembre 2025
