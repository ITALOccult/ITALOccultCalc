# Integrazione Algoritmo RA15 di OrbFit

## Stato Implementazione

### ✅ Completato
1. **Struttura base RA15Integrator**
   - File: `include/ioccultcalc/ra15_integrator.hpp`
   - File: `src/ra15_integrator.cpp`  
   - Classe completa con metodi principali

2. **Integrazione in OrbitPropagator**
   - Aggiunto `IntegratorType::RA15` in `orbit_propagator.h`
   - Metodo `integrateRA15()` funzionante
   - Switch automatico quando `opts.integrator = IntegratorType::RA15`

3. **CMakeLists.txt aggiornato**
   - `ra15_integrator.cpp` incluso nella libreria

4. **computeRadauConstants() corretto**
   - Traduzione fedele da OrbFit `SUBROUTINE radcon`
   - Coefficienti h, w, u, c, d, r calcolati esattamente come in Fortran

### ⚠️ In Progress
1. **Metodo iterate() da rifinire**
   - Struttura diversa da OrbFit `SUBROUTINE rasust`
   - OrbFit usa predizione posizione/velocità intermedie complesse
   - Necessita riscrittura completa seguendo algoritmo Fortran

2. **Metodo predict() da verificare**
   - OrbFit usa `SUBROUTINE rapred` con formule specifiche
   - Attuale implementazione semplificata

3. **Metodo extrapolate() da correggere**
   - OrbFit usa `SUBROUTINE bintrp` non ancora analizzata

### ❌ Da Fare
1. **Testing completo**
   - Attualmente RA15 fallisce con "troppi fallimenti convergenza"
   - Necessita debug metodo iterate()

2. **Documentazione utente**
   - Guida uso RA15 in README.md principale
   - Esempi codice

## Algoritmo RA15 (da OrbFit 5.0.8)

### Fonte
- Software: **OrbFit 5.0.8** (Andrea Milani et al., Università di Pisa)
- File Fortran: `src/propag/ra15_mod.f90` (783 righe)
- Licenza: GPL
- Usato da: NEODyS, AstDyS (tracking asteroidi professionali)

### Caratteristiche
- **Ordine 15**: Errore O(h¹⁶) vs O(h⁵) di RK4
- **Metodo implicito**: Punti Gauss-Radau (8 punti)
- **Step adattivo**: Controllo automatico precisione
- **Extrapolazione**: Riusa coefficienti B tra step

### Parametri Chiave
```cpp
RA15Options opts;
opts.llev = 10;           // Tolleranza ss = 10^(-10)
opts.h_init = 0.1;        // Step iniziale [giorni]
opts.eprk = 1e-12;        // Tolleranza convergenza
opts.lit1 = 10;           // Max iterazioni primo step
opts.lit2 = 4;            // Max iterazioni step successivi
opts.fixed_step = false;  // Step adattivo
```

### Subroutine Principali OrbFit

#### 1. radcon - Calcola costanti Gauss-Radau
**File Fortran**: linee 705-763

**Cosa fa**:
- Inizializza array h(8): spaziature Gauss-Radau
- Calcola array w(7): pesi integrazione  
- Calcola array u(7): coefficienti ausiliari
- Calcola array c(21), d(21), r(21): coefficienti ricorrenza

**Stato IOccultCalc**: ✅ **IMPLEMENTATO CORRETTAMENTE**

#### 2. rasust - Iterazione convergenza
**File Fortran**: linee 429-627

**Cosa fa**:
- Loop su 8 substep (j=2..8)
- Per ogni substep:
  1. Predice posizione y(k) usando serie collapsed (eq. 2.9-2.10)
  2. Predice velocità z(k) (per eq. secondo ordine)
  3. Valuta forza fj = f(y, z, t_substep)
  4. Calcola g-values usando ricorrenza (eq. 2.4)
  5. Aggiorna b-values (eq. 2.5)
- Accumula epsilon per controllo convergenza

**Stato IOccultCalc**: ⚠️ **IMPLEMENTAZIONE SEMPLIFICATA** (da rifare)

#### 3. rapred - Predice stato finale
**File Fortran**: linee 629-646

**Cosa fa**:
- Aggiorna posizione finale: x = x + v*t + t²*[f1*w1 + Σ b(i)*w(i)]
- Aggiorna velocità finale: v = v + t*[f1 + Σ b(i)*u(i)]

**Stato IOccultCalc**: ⚠️ **DA VERIFICARE**

#### 4. rabeta - Calcola g da b
**File Fortran**: linee 647-664

**Cosa fa**:
- Trasformazione g = D * b usando matrice d(21)

**Stato IOccultCalc**: ✅ **IMPLEMENTATO** (metodo computeG)

#### 5. bintrp - Extrapolazione coefficienti
**File Fortran**: linee 665-703

**Cosa fa**:
- Estrapola coefficienti B da step precedente a step corrente
- Usa rapporto q = t_new / t_old

**Stato IOccultCalc**: ⚠️ **IMPLEMENTAZIONE SEMPLIFICATA**

## Confronto Accuratezza Attesa

| Integratore | Ordine | Errore 1 giorno | Errore 40 giorni | Velocità |
|-------------|--------|-----------------|------------------|----------|
| **RK4** (attuale) | 4 | 314 km | **457,000 km** | 1× (baseline) |
| **RA15** (target) | 15 | < 1 m | **< 1 km** | 3-5× più lento |
| **JPL Horizons** | - | - | 0 km (riferimento) | - |

**Miglioramento atteso**: 450× riduzione errore

## Uso (quando funzionante)

### Esempio Base
```cpp
#include "ioccultcalc/orbit_propagator.h"

// Setup con RA15
PropagatorOptions opts;
opts.integrator = IntegratorType::RA15;  // Usa RA15 invece di RK4
opts.stepSize = 0.1;                     // Step iniziale [giorni]
opts.tolerance = 1e-12;                  // Massima precisione
opts.usePlanetaryPerturbations = true;
opts.useRelativisticCorrections = true;

OrbitPropagator propagator(opts);

// Propaga
OrbitState final = propagator.propagate(initialState, targetEpoch);

// Statistiche
auto stats = propagator.getLastStats();
std::cout << "Step eseguiti: " << stats.nSteps << "\n";
std::cout << "Valutazioni forza: " << stats.nEvaluations << "\n";
std::cout << "Media iterazioni/step: " << stats.avgIterations << "\n";
```

### Confronto RK4 vs RA15
```cpp
// Test RK4
PropagatorOptions opts_rk4;
opts_rk4.integrator = IntegratorType::RK4;
opts_rk4.stepSize = 0.1;
OrbitPropagator prop_rk4(opts_rk4);
auto result_rk4 = prop_rk4.propagate(state0, target);

// Test RA15
PropagatorOptions opts_ra15;
opts_ra15.integrator = IntegratorType::RA15;
opts_ra15.stepSize = 0.1;
opts_ra15.tolerance = 1e-12;
OrbitPropagator prop_ra15(opts_ra15);
auto result_ra15 = prop_ra15.propagate(state0, target);

// Confronta errori con JPL Horizons
double error_rk4 = (result_rk4.position - jpl_pos).magnitude() * AU_TO_KM;
double error_ra15 = (result_ra15.position - jpl_pos).magnitude() * AU_TO_KM;

std::cout << "Errore RK4: " << error_rk4 << " km\n";
std::cout << "Errore RA15: " << error_ra15 << " km\n";
std::cout << "Miglioramento: " << (error_rk4 / error_ra15) << "×\n";
```

## Debugging RA15

### Problema Corrente
```
ERRORE: RA15: troppi fallimenti convergenza (step = 0.000922)
```

**Causa**: Il metodo `iterate()` non replica fedelmente `SUBROUTINE rasust`

### Soluzione
Riscrivere `iterate()` seguendo esattamente la struttura Fortran:

1. **Loop substep j=2..8** (non j=1..7)
2. **Predizione posizione** con serie collapsed:
   ```fortran
   temp = w(3)*b(3,k) + s*(w(4)*b(4,k) + s*(...))
   y(k) = x(k) + q*(t*v(k) + t2*s*(f1(k)*w1 + s*(...)))
   ```
3. **Calcolo g-values** con ricorrenza specifica per ogni j
4. **Aggiornamento b-values** usando matrice c

### File da Analizzare
- `docs/ra15_mod.f90`: Implementazione di riferimento completa
- Linee 429-627: `SUBROUTINE rasust` (critica)
- Linee 228-398: Main loop `ra15`

## Riferimenti

1. **Paper originale**:
   Everhart, E. (1985). "An efficient integrator that uses Gauss-Radau spacings".  
   IAU Colloq. 83: Dynamics of Comets, pp. 185-202.

2. **Libro teoria**:
   Milani, A., Gronchi, G.F. (2010). "Theory of Orbit Determination".  
   Cambridge University Press, Chapter 7.

3. **Software OrbFit**:
   - Website: https://adams.dm.unipi.it/orbfit/
   - Download: https://adams.dm.unipi.it/orbfit/OrbFit5.0.8.tar.gz (29 MB)
   - File chiave: `src/propag/ra15_mod.f90`

4. **Documentazione IOccultCalc**:
   - `docs/RA15_ALGORITHM.md`: Spiegazione dettagliata algoritmo
   - `docs/ra15_mod.f90`: Codice Fortran originale estratto da OrbFit

## Prossimi Passi

1. **Riscrivere iterate()** fedelmente da `rasust`
2. **Testare convergenza** con caso semplice (orbita circolare)
3. **Validare con Eros 433** contro JPL Horizons
4. **Ottimizzare performance** se necessario
5. **Documentare** quando funzionante
6. **Pubblicare** paper su accuratezza raggiunta

## Note Implementazione

### Differenze C++ vs Fortran
- **Array indexing**: Fortran 1-based → C++ 0-based
- **Loop**: `DO 14 n=2,8` → `for (int n = 2; n <= 8; ++n)`
- **GOTO**: `GO TO 73` → `continue` o ristrutturazione
- **COMMON blocks**: → membri classe privati

### Corrispondenza Variabili
| Fortran | C++ | Significato |
|---------|-----|-------------|
| `h(8)` | `h_[8]` | Spaziature Gauss-Radau |
| `w(7)` | `w_[7]` | Pesi integrazione |
| `u(7)` | `u_[7]` | Coefficienti velocità |
| `c(21)` | `c_[21]` | Coefficienti aggiornamento |
| `d(21)` | `d_[21]` | Coefficienti derivate |
| `r(21)` | `r_[21]` | Coefficienti ricorrenza |
| `b(7,nv)` | `b_[7][6]` | Storage principale |
| `g(7,nv)` | `g_[7][6]` | Valori intermedi |
| `e(7,nv)` | `e_[7][6]` | Extrapolazione |

## Licenza

L'implementazione RA15 in IOccultCalc è derivata da OrbFit 5.0.8 (GPL).  
Codice originale: © Andrea Milani, Giovanni Gronchi, Giacomo Tommei et al.  
Traduzione C++: © 2025 IOccultCalc contributors

GPL-3.0 - Vedere LICENSE per dettagli.
