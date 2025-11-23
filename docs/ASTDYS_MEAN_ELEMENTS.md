# AstDyS Mean Elements vs Osculating Elements

## Scoperta

Gli elementi orbitali nei file `.eq0` e `.eq1` di AstDyS sono **MEAN ELEMENTS** (elementi medi), non osculating elements (elementi osculating).

## Differenza

- **Mean Elements**: Elementi orbitali medi, ottenuti da fit delle osservazioni con un modello che rimuove le perturbazioni periodiche. Rappresentano l'orbita "media" dell'asteroide.

- **Osculating Elements**: Elementi orbitali istantanei che rappresentano la posizione effettiva dell'asteroide all'epoca, includendo tutte le perturbazioni.

## Esempio: (704) Interamnia

### Mean Elements (AstDyS .eq1, MJD 61000 = 2025-11-21)
```
a = 3.0562 AU
e = 0.1552
i = 17.32°
Ω = 280.17°
ω = 94.11°
M = 184.21°
```

### Osculating Elements (JPL Horizons, 2025-11-21)
```
a = 11.644 AU  ← MOLTO DIVERSO!
e = 0.684      ← MOLTO DIVERSO!
i = 8.89°
Ω = 237.87°
ω = 5.36°
M = 163.96°
```

### Posizioni Risultanti

**Da Mean Elements** (calcolo diretto, SBAGLIATO):
- r = (-3.21, -0.53, -1.36) AU
- |r| = 3.53 AU
- RA = 189.32°, Dec = -22.74°
- **ERRORE**: ~132° in RA!

**Da Osculating Elements** (calcolo diretto, CORRETTO):
- r = (10.02, 15.37, 6.59) AU
- |r| = 19.50 AU
- RA = 56.90°, Dec = 19.77°
- **CORRETTO**: Match JPL Horizons!

## Implicazioni per IOccultCalc

### ❌ NON FUNZIONA
Calcolare posizioni direttamente da mean elements AstDyS:
```cpp
auto elements = astdys.getElements("704");
auto state = propagator.elementsToState(elements);  // SBAGLIATO!
```

### ✅ SOLUZIONI

#### Opzione 1: Propagare con Perturbazioni Complete
Mean elements devono essere propagati con:
- Perturbazioni planetarie (JPL DE441)
- Perturbazioni degli asteroidi massivi (AST17)
- Effetti relativistici
- (Opzionale) Forze non-gravitazionali

```cpp
auto elements = astdys.getElements("704");
auto state0 = propagator.elementsToState(elements);
auto state = propagator.propagate(state0, targetDate);  // Con perturbazioni!
```

**PROBLEMA**: Propagazione da mean elements può accumulare errori.

#### Opzione 2: Usare JPL Horizons (RACCOMANDATO)
Per posizioni precise, usare direttamente JPL Horizons:
```cpp
auto position = horizons.getPosition("704", targetDate);
```

#### Opzione 3: Usare SPK Files
SPK files da JPL contengono posizioni pre-calcolate:
```cpp
auto position = spk.getPosition(NAIF_ID_704, targetDate);
```

## Workflow Corretto per Occultazioni

1. **Filtro iniziale**: Usa database AstDyS
   - Magnitudine
   - Dimensione stimata
   - Orbita approssimativa

2. **Calcolo posizione precisa**: Usa JPL Horizons o SPK
   - Per ogni candidato che passa il filtro
   - Query JPL Horizons API
   - Oppure leggi da SPK file

3. **Calcolo shadow path**: Con posizioni precise
   - Geocentrico
   - Topocentric corrections

## Riferimenti

- OrbFit documentation: Mean elements are designed for long-term propagation
- AstDyS `.eq1` format: OEF2.0, ECLM J2000, fitted mean elements
- JPL Horizons: Provides both osculating elements and vectors

## Stato Attuale IOccultCalc

- ✅ Database integration (AstDyS)
- ✅ GAIA cache
- ✅ Propagation framework
- ✅ Coordinate transformations (ECLM→EQUATORIAL)
- ❌ Mean elements→position conversion (non necessaria!)
- 🔄 TODO: Integrate JPL Horizons API for precise positions
