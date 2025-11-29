# Asteroid Database System - Quick Start

## Panoramica

IOccultCalc utilizza un database locale in JSON per memorizzare proprietà fisiche e orbitali degli asteroidi. Questo permette di:

1. **Filtrare** grandi popolazioni di asteroidi con criteri WHERE/WHERENOT
2. **Query rapide** senza scaricare dati ogni volta
3. **Lavorare offline** dopo il download iniziale
4. **Survey massive** su decine di migliaia di oggetti

## Setup Iniziale

### Opzione 1: Download Completo (Raccomandato)

Scarica il database completo MPC (~300 MB, 750K+ asteroidi):

```bash
./build_asteroid_database --source MPC
```

**Tempo**: ~5-10 minuti (dipende dalla connessione)  
**Spazio disco**: ~100 MB  
**RAM**: ~500 MB quando caricato

### Opzione 2: Subset Filtrato (Per Test)

Scarica solo asteroidi che soddisfano criteri specifici:

```bash
# Solo asteroidi grandi
./build_asteroid_database --source MPC \
  --filter "diameter > 50" \
  --output large_asteroids.json
```

**Tempo**: ~5-10 minuti download + 1-2 minuti filtraggio  
**Risultato**: ~2,000 asteroidi, ~500 KB

### Opzione 3: Range Specifico da AstDyS

Scarica un range specifico di asteroidi numerati:

```bash
./build_asteroid_database --source ASTDYS \
  --from 1 --to 10000
```

**Tempo**: ~20-30 minuti (query individuali)  
**Risultato**: 10,000 asteroidi

## File Database

**Posizione default**: `~/.ioccultcalc/asteroid_db.json`

**Struttura**:
```json
{
  "metadata": {
    "version": "1.0",
    "source": "MPC_EXTENDED_JSON",
    "last_update": "2025-11-22T18:30:00Z",
    "total_asteroids": 750000
  },
  "asteroids": {
    "1": {
      "number": 1,
      "name": "Ceres",
      "diameter": 939.4,
      "H": 3.53,
      "albedo": 0.090,
      "a": 2.769,
      "e": 0.0755,
      "i": 10.59,
      "orbit_class": "MBA",
      "spectral_type": "C"
    }
  }
}
```

## Uso nel Programma

### Esempio 1: Massive Survey con Filtri

```cpp
#include "ioccultcalc/asteroid_database.h"

int main() {
    // 1. Carica database
    AsteroidDatabase db;
    db.loadFromFile();
    
    // 2. Definisci filtri
    AsteroidRange range = AsteroidRangeBuilder()
        .from(1).to(100000)
        .where("diameter > 50")
        .where("H < 10")
        .where("orbit_class in [MBA]")
        .whereNot("i > 30")
        .build();
    
    // 3. Ottieni lista asteroidi
    auto asteroids = db.queryNumbers(range);
    
    std::cout << "Found " << asteroids.size() << " asteroids\n";
    
    // 4. Per ogni asteroide, calcola occultazioni
    for (int num : asteroids) {
        auto props = db.getProperties(num);
        std::cout << "Processing " << props.name << "...\n";
        
        // ... calcola occultazioni ...
    }
    
    return 0;
}
```

### Esempio 2: Query con Builder

```cpp
DatabaseQueryBuilder query(db);

auto results = query
    .from(1).to(50000)
    .whereDiameter(30, 200)           // 30-200 km
    .whereOrbitClass({"MBA", "IMB"})  // Main Belt
    .orderByDiameter(true)            // Largest first
    .limit(100)                       // Top 100
    .execute();

for (const auto& ast : results) {
    std::cout << ast.number << ". " << ast.name 
              << " - " << ast.diameter << " km\n";
}
```

### Esempio 3: Integration con Config File

**Config JSON**:
```json
{
  "asteroid_range": {
    "database": "~/.ioccultcalc/asteroid_db.json",
    "from": 1,
    "to": 100000,
    "where": [
      "diameter > 50",
      "H < 10",
      "orbit_class in [\"MBA\"]"
    ],
    "wherenot": [
      "i > 30"
    ]
  },
  "operations": {
    "parallel_processing": true,
    "max_threads": 8
  }
}
```

**Comando**:
```bash
./ioccultcalc_search --config survey.json 88.793 7.407
```

Il programma:
1. Carica `asteroid_db.json`
2. Applica filtri WHERE/WHERENOT
3. Ottiene lista ~200 asteroidi
4. Calcola occultazioni per ognuno (parallelo su 8 thread)

## Aggiornamento Database

### Update Automatico

```bash
# Aggiorna se più vecchio di 30 giorni
./build_asteroid_database --update --max-age 30
```

### Force Rebuild

```bash
# Riscarica tutto
./build_asteroid_database --source MPC
```

### Update Range Specifico

```bash
# Aggiorna solo asteroidi 1-1000
./build_asteroid_database --source ASTDYS \
  --from 1 --to 1000
```

## Statistiche Database

```bash
./build_asteroid_database --stats
```

Output:
```
╔═══════════════════════════════════════════════════════════╗
║              Database Statistics                          ║
╚═══════════════════════════════════════════════════════════╝

  Total asteroids:       750342
  Numbered:              750342
  Unnumbered:            0

  With diameter:         125847 (16%)
  With albedo:           94528 (12%)
  With spectral type:    58234 (7%)
  With rotation period:  12456 (1%)

  Last update:           2025-11-22 18:30:00
  Source:                MPC_EXTENDED_JSON

Database path: /Users/user/.ioccultcalc/asteroid_db.json
Memory usage:  487 MB
```

## Query Examples

### Large Dark Main Belt Asteroids

```cpp
AsteroidRange range = AsteroidRangeBuilder()
    .from(1).to(100000)
    .where("diameter > 100")
    .where("albedo < 0.06")
    .where("orbit_class in [MBA]")
    .where("a > 2.5")
    .where("a < 3.0")
    .whereNot("i > 20")
    .build();

int count = db.count(range);
std::cout << "Found " << count << " large dark MBAs\n";
// Result: ~50 asteroids
```

### Near-Earth Asteroids for Next Survey

```cpp
AsteroidRange range = AsteroidFilterPresets::nearEarth();
auto neas = db.query(range);

std::cout << "NEA Survey Candidates:\n";
for (const auto& ast : neas) {
    if (ast.diameter > 1.0) {  // > 1 km
        std::cout << "  " << ast.number << " " << ast.name 
                  << " - " << ast.diameter << " km\n";
    }
}
```

### Historic First 100

```cpp
AsteroidRange range = AsteroidFilterPresets::historic();
auto historic = db.query(range);

for (const auto& ast : historic) {
    std::cout << ast.number << ". " << ast.name << "\n";
}
```

## Performance Tips

### 1. Load Once, Query Many

```cpp
// ✓ GOOD: Load once
AsteroidDatabase db;
db.loadFromFile();

for (auto date : dates) {
    auto results = db.query(range);  // Fast!
}
```

```cpp
// ✗ BAD: Load every time
for (auto date : dates) {
    AsteroidDatabase db;
    db.loadFromFile();  // Slow! 2-3 seconds each time
    auto results = db.query(range);
}
```

### 2. Query Numbers First

```cpp
// ✓ GOOD: Get only numbers (fast)
auto numbers = db.queryNumbers(range);  // ~0.5 sec
for (int num : numbers) {
    auto props = db.getProperties(num);  // <0.001 sec each
    // ... process ...
}
```

```cpp
// ✗ BAD: Load all properties (slower)
auto asteroids = db.query(range);  // ~1 sec
for (const auto& ast : asteroids) {
    // ... process ...
}
```

### 3. Use Subsets for Interactive Work

```bash
# Create small fast-loading database for testing
./build_asteroid_database --source MPC \
  --filter "diameter > 100" \
  --output test_db.json
```

```cpp
// Loads in <0.1 sec instead of 2-3 sec
AsteroidDatabase db("test_db.json");
db.loadFromFile();
```

## Integration with IOccultCalc

### Configuration File

Aggiungi sezione `asteroid_range` al config:

```json
{
  "object": {
    "database_mode": true
  },
  "asteroid_range": {
    "database": "~/.ioccultcalc/asteroid_db.json",
    "from": 1,
    "to": 100000,
    "where": ["diameter > 50", "H < 10"],
    "wherenot": ["i > 30"]
  }
}
```

### Command Line

```bash
# Single asteroid (normal mode)
./ioccultcalc_search 433 88.793 7.407

# Database mode with filters
./ioccultcalc_search --config survey.json 88.793 7.407

# Override filter from command line
./ioccultcalc_search --config survey.json \
  --add-filter "diameter > 100" \
  88.793 7.407
```

## Troubleshooting

### Database file not found

```
Error: Cannot load database from ~/.ioccultcalc/asteroid_db.json
```

**Solution**: Run initial download
```bash
./build_asteroid_database --source MPC
```

### Out of memory

```
Error: Cannot allocate memory for database
```

**Solution**: Use filtered subset
```bash
./build_asteroid_database --source MPC \
  --filter "diameter > 30" \
  --output smaller_db.json
```

### Slow queries

**Problem**: Query takes >5 seconds

**Solution**: 
1. Reduce database size with filters
2. Use `queryNumbers()` instead of `query()`
3. Consider SQLite backend (future)

### Corrupted database

```
Error: JSON parse error
```

**Solution**: Delete and rebuild
```bash
rm ~/.ioccultcalc/asteroid_db.json
./build_asteroid_database --source MPC
```

## Advanced Usage

### Custom Data Source

```cpp
// Load from custom JSON
AsteroidDatabase db;
db.importFromJson("my_custom_data.json", true);
db.saveToFile();
```

### Merge Multiple Sources

```cpp
AsteroidDatabase db;

// Start with MPC
db.downloadDatabase(DataSource::MPC_EXTENDED_JSON);

// Add AstDyS for better precision on range
db.updateRange(1, 1000, DataSource::ASTDYS);

// Add JPL for specific asteroids
db.updateAsteroid(433, DataSource::JPL_SBDB);

db.saveToFile();
```

### Export Subset

```cpp
// Create specialized database for NEAs
AsteroidDatabase fullDb;
fullDb.loadFromFile();

AsteroidRange neaFilter = AsteroidFilterPresets::nearEarth();
fullDb.exportToJson(neaFilter, "nea_database.json");

// Now you have a small NEA-only database
```

## Best Practices

1. **Download once, use many times**
2. **Update weekly/monthly** with `--update`
3. **Use filtered subsets** for development/testing
4. **Query numbers first**, load properties as needed
5. **Keep backup** of database file
6. **Check stats** periodically to monitor coverage
7. **Version control** your config files, not the database

## Future Enhancements

- [ ] SQLite backend for faster queries
- [ ] Automatic weekly updates (cron job)
- [ ] Cloud sync for teams
- [ ] Incremental updates (only changed records)
- [ ] Compression in-memory
- [ ] Indexing for range queries
- [ ] Web interface for database exploration

## See Also

- [Asteroid Filtering Documentation](../docs/ASTEROID_FILTERING.md)
- [Database Sources](../docs/ASTEROID_DATABASE_SOURCES.md)
- [Configuration System](../docs/CONFIG_SYSTEM.md)
- [IOccultCalc Quick Start](../QUICKSTART.md)
