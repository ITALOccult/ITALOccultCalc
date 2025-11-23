# Asteroid Filtering System

## Overview

Il sistema di filtraggio asteroidi di IOccultCalc fornisce un potente linguaggio SQL-like per selezionare asteroidi in base a proprietà fisiche ed orbitali. È progettato per gestire ricerche massive su decine di migliaia di oggetti con filtri sofisticati.

## Quick Start

```cpp
#include "ioccultcalc/asteroid_filter.h"

// Crea un range con filtri
AsteroidRange range = AsteroidRangeBuilder()
    .from(1).to(100000)
    .where("diameter > 50")
    .where("H < 10")
    .whereNot("i > 30")
    .build();

// Testa un asteroide
AsteroidProperties props;
props.number = 1;
props.diameter = 939.0;
props.H = 3.3;
props.i = 10.6;

if (range.matches(props)) {
    std::cout << "Ceres matches the filter!" << std::endl;
}
```

## Filter Syntax

### Campo Operatore Valore

Le condizioni di filtro seguono la sintassi: `campo operatore valore`

### Operatori Supportati

#### Operatori Numerici
- `>` - Greater than
- `<` - Less than
- `>=` - Greater than or equal
- `<=` - Less than or equal
- `==` o `=` - Equal
- `!=` - Not equal
- `between` - Range (es: `a between 2.2 and 3.2`)

#### Operatori Stringa
- `in` - Membership (es: `orbit_class in [MBA, NEA]`)
- `not in` - Non-membership
- `=` - Exact match (case-insensitive)
- `!=` - Not equal
- `like` - Pattern matching con wildcard `*`

### Campi Disponibili

#### Proprietà Fisiche
| Campo | Tipo | Unità | Descrizione |
|-------|------|-------|-------------|
| `number` | int | - | Numero IAU dell'asteroide |
| `diameter` | double | km | Diametro stimato |
| `H` | double | mag | Magnitudine assoluta |
| `albedo` | double | - | Albedo geometrico (0-1) |
| `rotation_period` | double | ore | Periodo di rotazione |
| `spectral_type` | string | - | Tipo spettrale (C, S, M, etc.) |

#### Elementi Orbitali
| Campo | Tipo | Unità | Descrizione |
|-------|------|-------|-------------|
| `a` | double | AU | Semiasse maggiore |
| `e` | double | - | Eccentricità (0-1) |
| `i` | double | deg | Inclinazione orbitale |
| `orbit_class` | string | - | Classe dinamica |

#### Identificatori
| Campo | Tipo | Descrizione |
|-------|------|-------------|
| `name` | string | Nome ufficiale |
| `designation` | string | Designazione provvisoria |

### Classi Orbitali Comuni

- **MBA** - Main Belt Asteroid
- **IMB** - Inner Main Belt
- **MMB** - Middle Main Belt
- **OMB** - Outer Main Belt
- **NEA** - Near-Earth Asteroid
- **Aten** - Aten-type NEA
- **Apollo** - Apollo-type NEA
- **Amor** - Amor-type NEA
- **PHA** - Potentially Hazardous Asteroid
- **L4, L5, Trojan** - Jupiter Trojans
- **Centaur** - Centaur object
- **TNO** - Trans-Neptunian Object

### Tipi Spettrali Comuni

- **C** - Carbonaceous (dark, primitive)
- **S** - Silicaceous (stony)
- **M** - Metallic
- **X** - X-complex (metal-rich)
- **P** - Primitive (low albedo)
- **D** - D-type (very red, low albedo)
- **E** - Enstatite achondrite
- **V** - Vesta-type (basaltic)
- **A** - A-type (olivine-rich)

## Logical Operations

### WHERE Conditions (AND Logic)

Tutte le condizioni WHERE devono essere soddisfatte (AND logico):

```cpp
.where("diameter > 50")    // AND
.where("H < 10")           // AND  
.where("e < 0.3")          // Tutte devono essere vere
```

### WHERENOT Conditions (OR Exclusion)

Qualsiasi condizione WHERENOT che si verifica esclude l'oggetto (OR esclusivo):

```cpp
.whereNot("i > 30")   // Escludi se i > 30
.whereNot("e > 0.5")  // OR escludi se e > 0.5
```

## Range Types

### Range Continuo

```cpp
AsteroidRange range = AsteroidRangeBuilder()
    .from(1)
    .to(100000)
    .build();
```

### Lista Esplicita

```cpp
std::vector<int> list = {1, 4, 10, 433, 951};
AsteroidRange range = AsteroidRangeBuilder()
    .explicitList(list)
    .build();
```

## Preset Filters

IOccultCalc fornisce filtri predefiniti per scenari comuni:

### Large Main Belt Asteroids
```cpp
AsteroidRange range = AsteroidFilterPresets::largeMBA();
// diameter > 50 km, H < 10, in main belt (2.2-3.2 AU)
```

### Near-Earth Asteroids
```cpp
AsteroidRange range = AsteroidFilterPresets::nearEarth();
// NEA, Aten, Apollo, Amor classes, a < 1.3 AU
```

### Potentially Hazardous Asteroids
```cpp
AsteroidRange range = AsteroidFilterPresets::potentiallyHazardous();
// PHA designation, diameter > 140m, H < 22
```

### Jupiter Trojans
```cpp
AsteroidRange range = AsteroidFilterPresets::trojans();
// L4/L5 Trojans, 4.5 < a < 5.5 AU, diameter > 20 km
```

### Centaurs
```cpp
AsteroidRange range = AsteroidFilterPresets::centaurs();
// Centaur class, 5.5 < a < 30 AU, diameter > 50 km
```

### Dark Asteroids
```cpp
AsteroidRange range = AsteroidFilterPresets::darkAsteroids();
// albedo < 0.07, C/P/D/B types, diameter > 30 km
```

### Metallic Asteroids
```cpp
AsteroidRange range = AsteroidFilterPresets::metallic();
// M/E/X types, albedo > 0.15, diameter > 20 km
```

### Historic First 100
```cpp
AsteroidRange range = AsteroidFilterPresets::historic();
// First 100 numbered asteroids (1-100)
```

### Fast Rotators
```cpp
AsteroidRange range = AsteroidFilterPresets::fastRotators();
// rotation_period < 3 hours, 5 < diameter < 50 km
```

### Binary Candidates
```cpp
AsteroidRange range = AsteroidFilterPresets::binaryCandidates();
// diameter > 50, M/E/X types, 2 < rotation_period < 5
```

## Examples

### Example 1: Large Dark MBA Survey

```cpp
AsteroidRange range = AsteroidRangeBuilder()
    .from(1).to(100000)
    .where("diameter > 100")
    .where("albedo < 0.06")
    .where("orbit_class in [MBA, IMB, OMB]")
    .where("a > 2.5")
    .where("a < 3.0")
    .whereNot("i > 20")
    .build();
```

**Targets**: Large, dark Main Belt asteroids in low-inclination orbits

### Example 2: NEA Occultation Candidates

```cpp
AsteroidRange range = AsteroidRangeBuilder()
    .from(1).to(100000)
    .where("orbit_class in [NEA, Aten, Apollo, Amor]")
    .where("diameter > 1")
    .where("H < 20")
    .whereNot("e > 0.9")
    .whereNot("i > 40")
    .build();
```

**Targets**: Bright NEAs with reasonable eccentricity and inclination

### Example 3: Flora Family Members

```cpp
AsteroidRange range = AsteroidRangeBuilder()
    .from(8).to(100000)
    .where("a > 2.15")
    .where("a < 2.35")
    .where("e > 0.12")
    .where("e < 0.18")
    .where("i > 2")
    .where("i < 8")
    .where("spectral_type in [S, Sr]")
    .where("diameter > 10")
    .build();
```

**Targets**: Flora asteroid family members

### Example 4: "Goldilocks" Optimal Targets

```cpp
AsteroidRange range = AsteroidRangeBuilder()
    .from(1).to(100000)
    .where("diameter > 30")
    .where("diameter < 150")
    .where("H > 8")
    .where("H < 12")
    .where("e > 0.05")
    .where("e < 0.15")
    .where("i > 5")
    .where("i < 15")
    .where("a > 2.3")
    .where("a < 2.9")
    .where("albedo > 0.10")
    .build();
```

**Targets**: "Just right" asteroids - not too big, not too small, not too extreme

### Example 5: Unusual Orbital Characteristics

```cpp
AsteroidRange range = AsteroidRangeBuilder()
    .from(1).to(100000)
    .where("diameter > 20")
    .whereNot("e > 0.05")
    .whereNot("e < 0.25")
    .whereNot("i > 5")
    .whereNot("i < 20")
    .whereNot("a > 2.2")
    .whereNot("a < 3.2")
    .build();
```

**Targets**: Asteroids with unusual orbits (excluding normal ranges)

## Configuration File Integration

### OrbFit Format (.oop)

```fortran
asteroid_range.
    .from = 1
    .to = 100000
    .where = ['diameter > 50', 'H < 10', 'orbit_class in ["MBA"]']
    .wherenot = ['i > 30', 'e > 0.5']
```

### JSON Format

```json
{
  "asteroid_range": {
    "from": 1,
    "to": 100000,
    "where": [
      "diameter > 50",
      "H < 10",
      "orbit_class in [\"MBA\"]"
    ],
    "wherenot": [
      "i > 30",
      "e > 0.5"
    ]
  }
}
```

## Performance Considerations

### Filtering Strategy

1. **Range First**: Define narrow ranges when possible
2. **Fast Filters First**: Put simple numeric comparisons before complex string operations
3. **Exclusion Last**: Use WHERENOT for final filtering after inclusion
4. **Batch Processing**: Process asteroids in batches for massive surveys

### Optimal Filter Order

```cpp
// ✓ GOOD: Fast filters first
.where("H < 12")                    // Fast numeric
.where("diameter > 30")             // Fast numeric
.where("orbit_class in [MBA]")      // String comparison
.whereNot("spectral_type like *V*") // Complex pattern

// ✗ BAD: Slow filters first
.where("spectral_type like *V*")    // Complex operation
.where("diameter > 30")             // Should be earlier
```

### Estimated Performance

- **Simple range (no filters)**: ~1M asteroids/second
- **Numeric filters only**: ~500K asteroids/second  
- **String filters included**: ~200K asteroids/second
- **Complex pattern matching**: ~50K asteroids/second

## API Reference

### AsteroidRangeBuilder

```cpp
AsteroidRangeBuilder()
```
**Constructor**: Inizializza builder con range default 1-100000

```cpp
AsteroidRangeBuilder& from(int start)
```
**Sets**: Inizio range (inclusive)

```cpp
AsteroidRangeBuilder& to(int end)
```
**Sets**: Fine range (inclusive)

```cpp
AsteroidRangeBuilder& explicitList(const std::vector<int>& list)
```
**Sets**: Lista esplicita di numeri asteroidi

```cpp
AsteroidRangeBuilder& where(const std::string& condition)
```
**Adds**: Condizione inclusiva (AND logic)

```cpp
AsteroidRangeBuilder& whereNot(const std::string& condition)
```
**Adds**: Condizione esclusiva (OR logic)

```cpp
AsteroidRange build()
```
**Returns**: Range configurato

### AsteroidRange

```cpp
bool matches(const AsteroidProperties& props) const
```
**Returns**: true se l'asteroide soddisfa tutti i filtri

```cpp
std::vector<int> getAsteroidList() const
```
**Returns**: Lista di numeri asteroidi nel range (senza applicare filtri)

```cpp
std::string toString() const
```
**Returns**: Rappresentazione testuale del range e filtri

### AsteroidProperties

```cpp
struct AsteroidProperties {
    int number;
    std::string designation;
    std::string name;
    double diameter;
    double H;
    double albedo;
    double a, e, i;
    double rotation_period;
    std::string orbit_class;
    std::string spectral_type;
    bool has_diameter;
    bool has_albedo;
};
```

## Error Handling

### Invalid Field Names

```cpp
try {
    AsteroidRange range = AsteroidRangeBuilder()
        .where("invalid_field > 10")
        .build();
} catch (const std::invalid_argument& e) {
    std::cerr << "Error: " << e.what() << std::endl;
    // Output: "Unknown field: invalid_field"
}
```

### Invalid Operators

```cpp
try {
    FilterCondition cond = FilterCondition::parse("diameter ?? 50");
} catch (const std::invalid_argument& e) {
    std::cerr << "Error: " << e.what() << std::endl;
    // Output: "No operator found in condition: diameter ?? 50"
}
```

### Missing Data

Il sistema gestisce gracefully dati mancanti:

```cpp
AsteroidProperties props;
props.number = 12345;
props.diameter = 0.0;      // Unknown diameter
props.albedo = 0.0;        // Unknown albedo
props.orbit_class = "";    // Unknown class

// Filters involving unknown data will typically fail gracefully
// diameter == 0 will be treated as "no data" not "zero diameter"
```

## Best Practices

### 1. Start Broad, Narrow Down

```cpp
// ✓ GOOD: Progressive refinement
auto range1 = AsteroidRangeBuilder()
    .from(1).to(100000)
    .where("diameter > 10")
    .build();
// Test a few asteroids...

auto range2 = AsteroidRangeBuilder()
    .from(1).to(100000)
    .where("diameter > 10")
    .where("H < 12")
    .where("orbit_class in [MBA]")
    .build();
// Now more specific
```

### 2. Use Presets as Templates

```cpp
// ✓ GOOD: Customize from preset
AsteroidRange range = AsteroidFilterPresets::largeMBA();
// Examine the preset logic, then create custom variant
```

### 3. Document Complex Filters

```cpp
// ✓ GOOD: Clear intent
// Survey for binary system candidates:
// - Large enough for meaningful gravity (>50 km)
// - Metallic (M/E/X types tend to be binary)
// - Moderate rotation (2-5 hours suggests tidal locking)
AsteroidRange range = AsteroidRangeBuilder()
    .from(1).to(100000)
    .where("diameter > 50")
    .where("spectral_type in [M, E, X]")
    .where("rotation_period > 2")
    .where("rotation_period < 5")
    .build();
```

### 4. Test with Known Objects

```cpp
// ✓ GOOD: Validate filter with known asteroids
AsteroidRange range = /* your complex filter */;

// Test with known examples
AsteroidProperties ceres;  // Should match
AsteroidProperties eros;   // Should not match
// etc.
```

## Integration with IOccultCalc

Il sistema di filtraggio si integra con il programma principale per ricerche massive:

```bash
# Search with config file containing asteroid_range
./ioccultcalc_search --config survey_config.json 88.793 7.407

# The config specifies which asteroids to process
```

Configuration example:
```json
{
  "asteroid_range": {
    "from": 1,
    "to": 50000,
    "where": ["diameter > 50", "H < 10"],
    "wherenot": ["i > 30"]
  },
  "operations": {
    "parallel_processing": true,
    "max_threads": 8
  }
}
```

## Future Enhancements

Planned features for future versions:

- [ ] Logical OR between WHERE conditions
- [ ] Nested conditions with parentheses
- [ ] Regular expression support for string fields
- [ ] Statistical analysis of filtered populations
- [ ] Visual filter builder/debugger
- [ ] Filter performance profiling
- [ ] Cached filter results for repeated queries

## See Also

- [Configuration System Documentation](CONFIG_SYSTEM.md)
- [IOccultCalc Quick Start](../QUICKSTART.md)
- [Configuration Templates](../examples/config_templates/)
