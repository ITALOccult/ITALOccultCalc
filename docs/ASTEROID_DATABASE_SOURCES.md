# Asteroid Database Sources

## Panoramica Sorgenti Dati

IOccultCalc può utilizzare diverse sorgenti per costruire un database locale di proprietà degli asteroidi:

## 1. Minor Planet Center (MPC)

### MPCORB Extended JSON ⭐ **RACCOMANDATO**

**URL**: https://minorplanetcenter.net/Extended_Files/mpcorb_extended.json.gz

**Contenuto**:
- **Tutti gli asteroidi numerati** (~750,000+)
- Elementi orbitali (a, e, i, Ω, ω, M)
- Magnitudine assoluta H
- Diametro stimato (quando disponibile)
- Albedo geometrico (quando disponibile)
- Tipo spettrale (quando disponibile)
- Classificazione orbitale
- Aggiornamento: **giornaliero**

**Formato JSON**:
```json
{
  "number": "1",
  "name": "Ceres",
  "designation": "",
  "H": 3.53,
  "G": 0.12,
  "epoch": "2024-01-01",
  "a": 2.7691651,
  "e": 0.0755347,
  "i": 10.59351,
  "om": 80.3932,
  "w": 73.5976,
  "ma": 77.372,
  "diameter": 939.4,
  "albedo": 0.090,
  "spec_T": "C",
  "orbit_type": "MBA"
}
```

**Dimensione**: ~200-300 MB compresso, ~2-3 GB decompresso

**Vantaggi**:
- ✅ Formato JSON nativo (facile parsing)
- ✅ Include proprietà fisiche
- ✅ Aggiornato giornalmente
- ✅ Fonte ufficiale IAU
- ✅ Completo

**Svantaggi**:
- ❌ File molto grande
- ❌ Non tutti hanno diametro/albedo

### MPCORB.DAT (Formato testo)

**URL**: https://minorplanetcenter.net/iau/MPCORB/MPCORB.DAT.gz

**Contenuto**: Solo elementi orbitali, più compatto ma senza proprietà fisiche

## 2. AstDyS (Asteroids Dynamic Site)

**URL Base**: https://newton.spacedys.com/astdys/

**Files Disponibili**:
- `allnum.cat`: Catalogo completo asteroidi numerati
- `allnum.cat.gz`: Versione compressa
- Query individuale: `https://newton.spacedys.com/astdys/index.php?pc=1.1.0&n=NUMERO`

**Contenuto**:
- Elementi orbitali alta precisione
- Parametri fisici (H, G, diametro, albedo)
- Classificazione tassonomica
- Parametri non-gravitazionali (per comete)
- Incertezze orbitali

**Formato testo proprietario** (richiede parser custom)

**Vantaggi**:
- ✅ Alta precisione orbitale
- ✅ Query singoli asteroidi veloce
- ✅ Include incertezze

**Svantaggi**:
- ❌ Formato non standard
- ❌ Richiede parsing complesso
- ❌ Solo asteroidi numerati

## 3. JPL Small-Body Database (SBDB)

**URL Query**: https://ssd-api.jpl.nasa.gov/sbdb.api

**Contenuto**:
- Elementi orbitali JPL
- Parametri fisici
- Dati scoperta
- Close approaches
- Parametri Yarkovsky

**Formato**: JSON API

**Esempio Query**:
```bash
curl "https://ssd-api.jpl.nasa.gov/sbdb.api?sstr=433"
```

**Risposta**:
```json
{
  "object": {
    "fullname": "433 Eros (1898 DQ)",
    "kind": "an",
    "orbit_class": {
      "name": "Amor",
      "code": "AMO"
    }
  },
  "phys_par": {
    "H": "10.4",
    "diameter": "16.8",
    "albedo": "0.25",
    "rot_per": "5.270"
  },
  "orbit": {
    "elements": {
      "e": "0.2229512519469658",
      "a": "1.458045729081037",
      "q": "1.132881108214988",
      "i": "10.82763824002722",
      "om": "304.2646456444987",
      "w": "178.9232177962222",
      "ma": "271.5643858941762"
    }
  }
}
```

**Vantaggi**:
- ✅ API moderna JSON
- ✅ Dati dettagliati
- ✅ Include rotation period
- ✅ Affidabile (NASA/JPL)

**Svantaggi**:
- ❌ Rate limit (1 richiesta/secondo)
- ❌ Deve fare query per singolo asteroide
- ❌ Lento per download massivo

## 4. Asteroid Lightcurve Database (LCDB)

**URL**: http://www.minorplanet.info/lightcurvedatabase.html

**Contenuto**:
- Periodi di rotazione
- Ampiezza curve di luce
- Qualità dati fotometrici
- Riferimenti pubblicazioni

**Formato**: CSV/Excel

**Ideale per**: Fast rotators filter

## Strategia Raccomandata per IOccultCalc

### Download Iniziale (Setup)

**Opzione A: Full Database (raccomandato per uso professionale)**

```bash
# 1. Scarica MPC Extended JSON (~300 MB)
curl -o mpcorb_extended.json.gz \
  https://minorplanetcenter.net/Extended_Files/mpcorb_extended.json.gz

# 2. Decomprimi
gunzip mpcorb_extended.json.gz

# 3. Importa in IOccultCalc
./ioccultcalc_search --import-database mpcorb_extended.json
```

**Opzione B: Subset (per test o uso limitato)**

```bash
# Crea database solo asteroidi grandi (< 10 MB)
./ioccultcalc_search --build-database \
  --source MPC \
  --filter "diameter > 50" \
  --output asteroid_db_large.json
```

**Opzione C: On-Demand (per singole predizioni)**

```bash
# Scarica dati solo quando serve, da AstDyS
./ioccultcalc_search 433 88.793 7.407 --download-on-demand
```

### Aggiornamento Periodico

**Settimanale/Mensile**:
```bash
# Aggiorna solo asteroidi già nel database
./ioccultcalc_search --update-database --max-age 30

# O ricarica completamente
./ioccultcalc_search --rebuild-database
```

### Query Selective

**Per massive survey** con filtri:

```cpp
// 1. Carica database locale
AsteroidDatabase db;
db.loadFromFile("~/.ioccultcalc/asteroid_db.json");

// 2. Query con filtri
AsteroidRange range = AsteroidRangeBuilder()
    .from(1).to(100000)
    .where("diameter > 50")
    .where("H < 10")
    .build();

// 3. Ottieni solo numeri (veloce)
std::vector<int> asteroids = db.queryNumbers(range);
// Result: [1, 2, 4, 10, 15, 16, ...] (~200 asteroidi)

// 4. Per ognuno, calcola occultazioni
for (int num : asteroids) {
    auto props = db.getProperties(num);
    // ... calcola occultazioni ...
}
```

## Formato Database Locale IOccultCalc

**File**: `~/.ioccultcalc/asteroid_db.json`

**Struttura**:
```json
{
  "metadata": {
    "version": "1.0",
    "source": "MPC_EXTENDED_JSON",
    "last_update": "2025-11-22T18:30:00Z",
    "total_asteroids": 750000,
    "with_diameter": 125000,
    "with_albedo": 95000
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
      "rotation_period": 9.074,
      "orbit_class": "MBA",
      "spectral_type": "C"
    },
    "2": { ... },
    "4": { ... }
  }
}
```

## Performance Considerations

### Memory Usage

| Database Size | Asteroids | RAM Used | Disk Space |
|---------------|-----------|----------|------------|
| Full | 750,000 | ~500 MB | ~100 MB (JSON) |
| Large (>50km) | ~2,000 | ~5 MB | ~500 KB |
| NEAs only | ~30,000 | ~20 MB | ~5 MB |
| Custom filter | Varies | Varies | Varies |

### Load Times

| Operation | Time (Full DB) | Time (Subset) |
|-----------|----------------|---------------|
| Load from disk | ~2-3 sec | <0.1 sec |
| Query filter | ~0.5-1 sec | <0.01 sec |
| Single lookup | <0.001 sec | <0.001 sec |

### Recommendations

1. **Per uso interattivo**: Usa subset filtrato (<10,000 asteroidi)
2. **Per survey massivi**: Carica full database in memoria una volta
3. **Per applicazioni web**: Usa database SQL (SQLite) invece di JSON
4. **Per cluster/HPC**: Distribuisci subset per nodo

## Esempio: Build Custom Database

```cpp
#include "ioccultcalc/asteroid_database.h"

int main() {
    // 1. Scarica da MPC
    AsteroidDatabase db;
    std::cout << "Downloading MPC Extended JSON...\n";
    db.downloadDatabase(DataSource::MPC_EXTENDED_JSON);
    
    // 2. Applica filtro per ridurre dimensione
    AsteroidRange filter = AsteroidRangeBuilder()
        .from(1).to(100000)
        .where("diameter > 30")  // Solo >30 km
        .where("H < 12")         // Ragionevolmente brillanti
        .build();
    
    // 3. Esporta subset
    db.exportToJson(filter, "asteroid_db_medium.json");
    
    // 4. Statistiche
    auto stats = db.getStats();
    std::cout << "Total: " << stats.total_asteroids << "\n";
    std::cout << "With diameter: " << stats.with_diameter << "\n";
    std::cout << "Filtered: " << db.count(filter) << "\n";
    
    return 0;
}
```

## API Endpoints Utili

### MPC API (non documentata pubblicamente)
```
# Non esiste API query diretta, solo file bulk
```

### JPL SBDB API
```bash
# Single asteroid
curl "https://ssd-api.jpl.nasa.gov/sbdb.api?sstr=433"

# Query close approaches
curl "https://ssd-api.jpl.nasa.gov/cad.api?des=433&date-min=2025-01-01"
```

### AstDyS Query
```bash
# Single numbered asteroid
curl "https://newton.spacedys.com/astdys/index.php?pc=1.1.0&n=433"
```

## Licensing & Attribution

### MPC Data
- **License**: Public domain (IAU data)
- **Citation**: "This research has made use of data and/or services provided by the International Astronomical Union's Minor Planet Center."

### JPL SBDB
- **License**: NASA Open Data (public domain)
- **Citation**: "Data from the JPL Small-Body Database"

### AstDyS
- **License**: Free for scientific use
- **Citation**: Appropriate paper depending on data used

## Future Enhancements

Planned for IOccultCalc database system:

- [ ] SQLite backend for faster queries
- [ ] Automatic weekly updates
- [ ] Incremental updates (solo changed/new)
- [ ] Compression in-memory for large datasets
- [ ] Indexing per fast range queries
- [ ] Integration with Gaia catalog for star data
- [ ] Cloud sync option
- [ ] Shared database for teams/observatories

## See Also

- [Asteroid Filtering Documentation](ASTEROID_FILTERING.md)
- [Configuration System](CONFIG_SYSTEM.md)
- [MPC Data Format Manual](https://minorplanetcenter.net/iau/info/MPOrbitFormat.html)
