# Database Setup Guide

## Overview

IOccultCalc utilizza un database locale degli asteroidi per permettere survey efficienti su grandi quantità di oggetti. Il database è basato su JSON e integrato con il sistema di filtri avanzato.

## Directory Structure

Tutti i dati di IOccultCalc sono organizzati in `~/.ioccultcalc/`:

```
~/.ioccultcalc/
├── database/
│   └── asteroid_db.json          # Database principale
├── ephemerides/
│   ├── de441.bsp                 # Effemeridi planetarie JPL
│   └── *.bsp                     # Altri file SPK
├── iers/
│   └── finals2000A.all           # Dati orientamento Terra
├── cache/
│   └── *.json.gz                 # Download temporanei
├── logs/
│   └── *.log                     # Log programma
└── config/
    └── *.json                    # Configurazioni salvate
```

## Quick Start

### 1. Download Database MPC

Il modo più semplice è usare lo script Python fornito:

```bash
cd IOccultCalc
python3 tools/download_mpc_database.py
```

Questo scaricherà automaticamente il database MPC Extended (~ 300 MB) e lo convertirà nel formato IOccultCalc (~ 100 MB).

**Tempo stimato**: 5-10 minuti (dipende dalla connessione)

**Requisiti disco**: ~ 500 MB (300 MB per file compresso + 100 MB database)

### 2. Verifica Installazione

```bash
./build/examples/test_database
```

Se tutto è ok, vedrai:
```
✓ ALL TESTS COMPLETED SUCCESSFULLY
```

### 3. Usa con ioccultcalc_search

```bash
./ioccultcalc_search --database-mode --config survey.json <ra> <dec>
```

## Download Options

### Opzione 1: Database Completo (Raccomandato)

Tutti gli asteroidi numerati con proprietà fisiche:

```bash
python3 tools/download_mpc_database.py
```

**Pro**: Massima completezza, nessuna limitazione
**Contro**: File grande (300 MB download, 100 MB database)
**Tempo query**: 2-3 secondi caricamento iniziale

### Opzione 2: Subset Filtrato

Solo asteroidi che soddisfano criteri specifici:

```bash
# Solo asteroidi grandi
python3 tools/download_mpc_database.py --filter "diameter > 50"

# Solo NEA
python3 tools/download_mpc_database.py --filter "orbit_class == NEA"

# Combo multipli (modifica script)
```

**Pro**: File piccolo, caricamento rapido
**Contro**: Limitato ai criteri scelti
**Tempo query**: < 0.5 secondi

### Opzione 3: Download Manuale

Se hai già il file MPC:

```bash
# 1. Scarica manualmente
wget https://minorplanetcenter.net/Extended_Files/mpcorb_extended.json.gz

# 2. Posiziona in cache
mkdir -p ~/.ioccultcalc/cache
mv mpcorb_extended.json.gz ~/.ioccultcalc/cache/

# 3. Converti
python3 tools/download_mpc_database.py --skip-download
```

## Database Format

Il database è in formato JSON con questa struttura:

```json
{
  "metadata": {
    "version": "1.0",
    "source": "MPC_EXTENDED_JSON",
    "last_update": "2025-11-22T18:30:00",
    "total_asteroids": 750000,
    "with_diameter": 150000,
    "with_albedo": 120000
  },
  "asteroids": {
    "1": {
      "number": 1,
      "name": "Ceres",
      "designation": "",
      "diameter": 939.4,
      "H": 3.53,
      "albedo": 0.090,
      "a": 2.769,
      "e": 0.0755,
      "i": 10.59,
      "rotation_period": 0.0,
      "orbit_class": "MBA",
      "spectral_type": "C"
    }
  }
}
```

## Usage Examples

### Python API

```python
from ioccultcalc import AsteroidDatabase, AsteroidRangeBuilder

# Load database
db = AsteroidDatabase()
db.loadFromFile()  # Default path

# Query large asteroids
range = AsteroidRangeBuilder() \
    .where("diameter > 100") \
    .where("H < 10") \
    .build()

asteroids = db.query(range)
print(f"Found {len(asteroids)} large asteroids")

# Fast query (only numbers)
numbers = db.queryNumbers(range)
for num in numbers:
    props = db.getProperties(num)
    # ... calcola occultazioni ...
```

### C++ API

```cpp
#include "ioccultcalc/asteroid_database.h"
#include "ioccultcalc/asteroid_filter.h"

// Load database
ioccultcalc::AsteroidDatabase db;
db.loadFromFile();

// Query with filters
auto range = ioccultcalc::AsteroidRangeBuilder()
    .where("diameter > 100")
    .where("orbit_class == MBA")
    .build();

auto asteroids = db.query(range);

// Process results
for (const auto& ast : asteroids) {
    std::cout << ast.number << " " << ast.name 
              << " D=" << ast.diameter << " km\n";
}
```

### Command Line

```bash
# Search with filters
./ioccultcalc_search \
    --database-mode \
    --filter "diameter > 50" \
    --filter "H < 12" \
    <ra> <dec> <date>

# Use preset filter
./ioccultcalc_search \
    --database-mode \
    --preset largeMBA \
    <ra> <dec> <date>

# Specific asteroids
./ioccultcalc_search \
    --asteroids 1,2,4,10 \
    <ra> <dec> <date>
```

## Maintenance

### Update Database

Il database MPC viene aggiornato quotidianamente. Per aggiornare:

```bash
python3 tools/download_mpc_database.py
```

Il vecchio database sarà sovrascritto.

### Check Database Stats

```bash
# Info sistema
./build/examples/test_database

# Oppure programmaticamente
```cpp
auto stats = db.getStats();
std::cout << "Total: " << stats.total_asteroids << "\n";
std::cout << "With diameter: " << stats.with_diameter << "\n";
std::cout << "Last update: " << stats.last_update << "\n";
```

### Clean Cache

La cache può accumulare file temporanei:

```bash
# Manualmente
rm -rf ~/.ioccultcalc/cache/*

# O programmaticamente
DataManager::instance().cleanCache(30);  // Rimuovi file > 30 giorni
```

### Backup

Per backup completo:

```bash
tar czf ioccultcalc_backup.tar.gz ~/.ioccultcalc/
```

Per restore:

```bash
tar xzf ioccultcalc_backup.tar.gz -C ~/
```

## Performance

### Database Loading

| Database Size | Asteroids | Load Time | Memory |
|--------------|-----------|-----------|--------|
| Full MPC | 750,000 | 2-3 sec | ~500 MB |
| Subset (D>50) | ~15,000 | 0.5 sec | ~50 MB |
| Subset (NEA) | ~30,000 | 0.8 sec | ~80 MB |

### Query Performance

| Query Type | Time | Notes |
|-----------|------|-------|
| Simple filter | 0.5-1 sec | es: diameter > 100 |
| Complex filter | 1-2 sec | es: 3 condizioni AND |
| Fast query (numbers only) | 0.1-0.3 sec | Ottimizzato |

### Optimization Tips

1. **Use Fast Query**: Se serve solo lista numeri, usa `queryNumbers()` invece di `query()`
2. **Filter Early**: Applica filtri restrittivi prima (es: orbit_class)
3. **Subset Database**: Per survey ripetute, crea subset filtrato una volta
4. **Cache Results**: Per ricerche identiche, salva risultati

## Troubleshooting

### Download Fails

```bash
# Verifica connessione
ping minorplanetcenter.net

# Scarica manualmente e skippa download
wget https://minorplanetcenter.net/Extended_Files/mpcorb_extended.json.gz
python3 tools/download_mpc_database.py --skip-download
```

### Database Corrupted

```bash
# Rimuovi e scarica di nuovo
rm ~/.ioccultcalc/database/asteroid_db.json
python3 tools/download_mpc_database.py
```

### Out of Memory

Se il database completo è troppo grande:

```bash
# Crea subset
python3 tools/download_mpc_database.py --filter "diameter > 50"

# O aumenta memoria virtuale (swap)
```

### Slow Queries

```bash
# Verifica stats
./build/examples/test_database

# Controlla filtri
# "orbit_class == NEA" è veloce (pochi match)
# "H < 20" è lento (molti match)

# Usa fast query
auto numbers = db.queryNumbers(range);  // Molto più veloce
```

## Advanced Topics

### Custom Data Sources

Oltre a MPC, puoi importare da:

1. **AstDyS**: Orbite alta precisione
2. **JPL SBDB**: Dati individuali dettagliati
3. **File custom**: Tuo formato JSON

Vedi `docs/ASTEROID_DATABASE_SOURCES.md` per dettagli.

### Database Merging

```cpp
// Importa dati aggiuntivi
db.importFromJson("astdys_subset.json", true);  // merge=true

// Verifica no duplicati
auto stats = db.getStats();
```

### Parallel Processing

Per survey massive:

```cpp
#include <thread>
#include <vector>

// Split query in chunks
std::vector<std::thread> threads;
for (int chunk = 0; chunk < 8; ++chunk) {
    threads.emplace_back([chunk, &numbers]() {
        int start = chunk * numbers.size() / 8;
        int end = (chunk + 1) * numbers.size() / 8;
        for (int i = start; i < end; ++i) {
            // Calcola occultazioni per numbers[i]
        }
    });
}

for (auto& t : threads) {
    t.join();
}
```

## Next Steps

1. **Test Database**: `./build/examples/test_database`
2. **Download MPC**: `python3 tools/download_mpc_database.py`
3. **Run Survey**: `./ioccultcalc_search --database-mode ...`
4. **Read Filtering Guide**: `docs/ASTEROID_FILTERING.md`
5. **Explore Presets**: `examples/config_templates/*.oop`

## Support

Per problemi o domande:
- Vedi documentazione: `docs/DATABASE_QUICKSTART.md`
- Test suite: `examples/test_database.cpp`
- Esempi filtri: `examples/config_templates/creative_filters.oop`
