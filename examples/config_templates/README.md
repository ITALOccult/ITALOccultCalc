# Template di Configurazione IOccultCalc

Questa directory contiene template di configurazione predefiniti per IOccultCalc, sia in formato `.oop` (compatibile OrbFit) che JSON.

## File Disponibili

### high_precision.oop / high_precision.json
Configurazione ad alta precisione per predizioni accurate:
- Integrator: RK4 con step 0.05 giorni
- Perturbazioni: 8 pianeti + 17 asteroidi massivi (AST17)
- Correzioni relativistiche abilitate
- JPL DE441 ephemerides
- Ricerca occultazioni con stelle fino a mag 14.0

### Uso

#### Formato .oop (OrbFit-style)
```bash
# Copia e modifica il template
cp high_precision.oop my_asteroid.oop

# Modifica i parametri (oggetto, date, etc.)
nano my_asteroid.oop

# Usa con IOccultCalc
./ioccultcalc_search --config my_asteroid.oop
```

#### Formato JSON
```bash
# Copia e modifica il template
cp high_precision.json my_config.json

# Modifica i parametri
nano my_config.json

# Usa con IOccultCalc
./ioccultcalc_search --config my_config.json
```

## Personalizzazione

### Parametri Principali da Modificare

#### Object (Oggetto Target)
```
object.
        .name = '433 Eros'      # Nome dell'asteroide
        .id = '433'             # Numero o designazione
        .diameter = 16.84       # Diametro in km (opzionale)
```

#### Time Span (Intervallo Temporale)
```
propag.
        .start_jd = 2461041.0   # 2026-01-01
        .end_jd = 2461405.0     # 2026-12-31
```

#### Search Parameters (Ricerca)
```
search.
        .start_jd = 2461041.0   # Inizio ricerca
        .end_jd = 2461405.0     # Fine ricerca
        .step_days = 0.5        # Campionamento (giorni)
        .mag_limit = 14.0       # Magnitudine limite
```

#### Precision (Precisione)
```
propag.
        .step_size = 0.05       # Passo integrazione (giorni)
        .tolerance = 1.0e-12    # Tolleranza

perturbations.
        .asteroid_count = 17    # 0=nessuno, 17=AST17
        .relativity = .TRUE.    # Correzioni relativistiche
```

## Conversione tra Formati

### Da .oop a JSON
```cpp
#include <ioccultcalc/config_manager.h>

ConfigManager config;
config.loadFromOop("my_config.oop");
config.saveToJson("my_config.json");
```

### Da JSON a .oop
```cpp
ConfigManager config;
config.loadFromJson("my_config.json");
config.saveToOop("my_config.oop");
```

## Creazione Programmatica

```cpp
#include <ioccultcalc/config_manager.h>

// Builder pattern
ConfigManager config = ConfigBuilder()
    .setObject("4 Vesta", "4")
    .setPropagator("RK4")
    .setStepSize(0.05)
    .setTimeSpan(2461041.0, 2461405.0)
    .enableAsteroids(17)
    .setMagnitudeLimit(12.0)
    .build();

config.saveToJson("vesta_config.json");
```

## Preset Disponibili

Il sistema fornisce 3 configurazioni preset:

### Default
```cpp
auto config = ConfigManager::createDefault();
```
- Bilanciamento precisione/velocità
- RK4, step 0.05 giorni
- AST17 abilitato

### High Precision
```cpp
auto config = ConfigManager::createHighPrecision();
```
- Massima precisione
- RA15, step 0.01 giorni
- AST17 + relatività

### Fast Search
```cpp
auto config = ConfigManager::createFastSearch();
```
- Ricerca veloce
- RK4, step 0.1 giorni
- Solo pianeti (no asteroidi)

## Validazione

Tutti i template possono essere validati:
```cpp
ConfigManager config;
config.loadFromJson("my_config.json");

std::vector<std::string> errors;
if (!config.validate(errors)) {
    for (const auto& error : errors) {
        std::cerr << "Error: " << error << "\n";
    }
}
```

## Advanced Asteroid Filtering

### massive_search.oop
Massive survey template with WHERE/WHERENOT filtering:
- Range: asteroids 1-100,000
- WHERE conditions (all must be true):
  - `diameter > 50 km`
  - `H < 10` (bright)
  - `orbit_class in ["MBA"]` (Main Belt)
  - `e < 0.3` (low eccentricity)
- WHERENOT conditions (any excludes):
  - `i > 30` (high inclination)
  - `a < 2.0` or `a > 3.5` (outside core MBA)
- Parallel processing with 4 threads

### nea_survey.json
Near-Earth Asteroid survey:
- WHERE: diameter > 1 km, H < 22, NEA classes, a < 1.3 AU
- WHERENOT: extreme e or i
- Parallel processing: 8 threads, batch size 100

### trojans.json
Jupiter Trojans survey:
- WHERE: L4/L5/Trojan classes, diameter > 30 km, 4.5 < a < 5.5 AU
- Starting from 588 (Achilles)

### creative_filters.oop
Collection of 10 creative filter examples:
1. **Giants Survey** - Monsters >200 km
2. **Dark Matter** - Low albedo objects
3. **Earth Crossers** - NEAs with crossing potential
4. **Flora Family** - Flora family members
5. **Orbital Oddities** - Unusual orbital characteristics
6. **Binary Candidates** - Potential binary systems
7. **Historic First 100** - Classic discoveries
8. **Goldilocks Zone** - Optimal observation targets
9. **Fast Spinners** - Rapid rotators <3h
10. **The Extremes** - Record holders (explicit list)

## Filter Syntax Quick Reference

### Operators
- Numeric: `>`, `<`, `>=`, `<=`, `==`, `!=`, `between`
- String: `in`, `not in`, `=`, `!=`, `like`

### Fields
- **Physical**: diameter, H, albedo, rotation_period, spectral_type
- **Orbital**: a, e, i, orbit_class
- **IDs**: number, name, designation

### Logic
- **WHERE**: All conditions must be true (AND)
- **WHERENOT**: Any condition excludes object (OR)

### Example
```fortran
asteroid_range.
    .from = 1
    .to = 100000
    .where = ['diameter > 50', 'H < 10']
    .wherenot = ['i > 30']
```

## Note

- Le date sono in formato Julian Date (JD)
- I path con `~` vengono espansi automaticamente
- I file `.oop` usano sintassi Fortran per booleani (`.TRUE.` / `.FALSE.`)
- JSON usa booleani standard (`true` / `false`)
- La conversione tra formati è bidirezionale e lossless
- Vedere [ASTEROID_FILTERING.md](../../docs/ASTEROID_FILTERING.md) per documentazione completa filtri

