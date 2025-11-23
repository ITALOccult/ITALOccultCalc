# GAIA DR3 Cache System

## Overview

Il sistema di cache GAIA DR3 permette di scaricare e mantenere localmente le stelle nella zona dove passa un asteroide, evitando query ripetute al server TAP di GAIA. La cache è organizzata in tiles HEALPix per efficienza.

## Features

- **Cache Persistente**: Le stelle scaricate vengono salvate localmente in `~/.ioccultcalc/gaia/`
- **Organizzazione HEALPix**: Sky diviso in 12,288 tiles (NSIDE=32, ~13.4° per tile)
- **Auto-Download**: Scarica automaticamente tiles mancanti quando necessario
- **Path-Based Query**: Ottimizzato per query lungo il percorso di un asteroide
- **Filtro Magnitudine**: Scarica solo stelle fino alla magnitudine limite specificata
- **Gestione Intelligente**: Riusa tiles esistenti, aggiorna solo se necessario

## Directory Structure

```
~/.ioccultcalc/gaia/
├── cache_index.json      # Indice tiles scaricate
├── tiles/
│   ├── tile_000001.json  # Tile HEALPix 1
│   ├── tile_000002.json  # Tile HEALPix 2
│   ├── tile_006336.json  # Tile all'equatore
│   └── ...
└── stats.json            # Statistiche cache (futuro)
```

## Quick Start

### 1. Setup Base

```cpp
#include "ioccultcalc/gaia_cache.h"
#include "ioccultcalc/gaia_client.h"

using namespace ioccultcalc;

// Create GAIA client
auto gaiaClient = std::make_shared<GaiaClient>();
gaiaClient->setTAPURL("https://gea.esac.esa.int/tap-server/tap");
gaiaClient->setTimeout(300);  // 5 minuti per query grandi

// Create cache
auto cache = GaiaCacheBuilder()
    .gaiaClient(gaiaClient)
    .autoDownload(true)
    .verbose(true)
    .build();
```

### 2. Download Pre-Survey (Opzionale)

Prima di una survey, puoi pre-scaricare la regione:

```cpp
// Download regione per survey
double ra_center = 83.8;    // Centro regione (gradi)
double dec_center = -5.4;
double radius = 10.0;        // Raggio (gradi)
double maxMag = 16.0;        // Magnitudine limite

// Download con callback progresso
cache->downloadRegion(ra_center, dec_center, radius, maxMag,
    [](int current, int total) {
        std::cout << "Progress: " << current << "/" << total 
                  << " tiles downloaded\r" << std::flush;
    }
);

std::cout << "\nDownload complete!" << std::endl;
```

**Tempo stimato**: ~2-5 minuti per regione 10° (dipende da GAIA server)

### 3. Query Durante Search

Durante la ricerca occultazioni, la cache scarica automaticamente:

```cpp
// Query stars lungo percorso asteroide
std::vector<EquatorialCoordinates> path = calculateAsteroidPath(...);

double width = 0.01;  // 36 arcsec (tipico per occultazioni)
double maxMag = 16.0;

// Auto-download se necessario
auto stars = cache->queryPath(path, width, maxMag, true);

std::cout << "Found " << stars.size() << " candidate stars" << std::endl;

// Usa stelle per predizione
for (const auto& star : stars) {
    auto event = predictor.predictOccultation(star, asteroid, ...);
    // ...
}
```

## Integration with ioccultcalc_search

### Config File Setup

Aggiungi sezione GAIA alla configurazione:

```json
{
  "asteroid_range": {
    "from": 1,
    "to": 10000,
    "where": ["diameter > 50", "H < 12"]
  },
  "search_parameters": {
    "start_date": "2026-01-01",
    "end_date": "2026-12-31",
    "max_magnitude": 16.0,
    "path_width_deg": 0.01,
    "time_step_hours": 2.4
  },
  "gaia_cache": {
    "enabled": true,
    "auto_download": true,
    "cache_dir": "~/.ioccultcalc/gaia",
    "tap_url": "https://gea.esac.esa.int/tap-server/tap",
    "timeout_seconds": 300,
    "pre_download_region": {
      "ra": 83.8,
      "dec": -5.4,
      "radius": 10.0
    }
  }
}
```

### Code Integration

```cpp
// In ioccultcalc_search main program
if (config["gaia_cache"]["enabled"]) {
    // Setup cache
    auto gaiaClient = std::make_shared<GaiaClient>();
    gaiaClient->setTAPURL(config["gaia_cache"]["tap_url"]);
    
    auto cache = GaiaCacheBuilder()
        .gaiaClient(gaiaClient)
        .autoDownload(config["gaia_cache"]["auto_download"])
        .verbose(verbose)
        .build();
    
    // Optional: Pre-download
    if (config["gaia_cache"].contains("pre_download_region")) {
        auto region = config["gaia_cache"]["pre_download_region"];
        cache->downloadRegion(
            region["ra"], region["dec"], region["radius"],
            config["search_parameters"]["max_magnitude"]
        );
    }
    
    // Per ogni asteroide
    for (int astNum : asteroidNumbers) {
        // Calcola path
        auto path = calculatePath(astNum, startDate, endDate);
        
        // Query stelle (usa cache)
        auto stars = cache->queryPath(
            path, 
            config["search_parameters"]["path_width_deg"],
            config["search_parameters"]["max_magnitude"],
            true  // auto-download
        );
        
        // Predici occultazioni
        for (const auto& star : stars) {
            // ...
        }
    }
}
```

## Performance

### Tile Size & Coverage

| NSIDE | Tiles | Tile Size | Angular Size |
|-------|-------|-----------|--------------|
| 16 | 3,072 | ~53° | ~26° dia |
| 32 | 12,288 | ~13.4° | ~13° dia |
| 64 | 49,152 | ~3.4° | ~6.6° dia |

**Scelta attuale**: NSIDE=32 (buon compromesso cache size / granularità)

### Tile Content

Per tile tipica (regione equatoriale, mag 16):
- **Stars**: ~5,000-10,000 stelle
- **File size**: ~500-1,000 KB
- **Download time**: 30-60 secondi per tile

### Cache Size Estimates

| Sky Coverage | Magnitude | Tiles | Total Stars | Cache Size |
|--------------|-----------|-------|-------------|-----------|
| 10° region | 16.0 | ~30 | ~200K | ~20 MB |
| Ecliptic band (±20°) | 16.0 | ~1,500 | ~10M | ~1 GB |
| Full sky | 16.0 | 12,288 | ~80M | ~8 GB |

**Raccomandazione**: Pre-download solo regioni di interesse (survey specifiche)

### Query Performance

| Operation | Cache Hit | Cache Miss (auto-download) |
|-----------|-----------|---------------------------|
| Point query | < 0.1 sec | 30-60 sec |
| Path query (10 points) | < 0.5 sec | 2-5 min |
| Region query (5°) | < 1 sec | 5-10 min |

**Ottimizzazione**: Prima run più lenta (download), runs successive immediate

## Advanced Usage

### Check Coverage

Prima di iniziare, verifica copertura:

```cpp
bool covered = cache->isCovered(ra, dec, radius, maxMag);
if (!covered) {
    std::cout << "Region not fully cached, will download..." << std::endl;
}
```

### Custom Cache Location

```cpp
auto cache = GaiaCacheBuilder()
    .cacheDir("/path/to/custom/cache")
    .build();
```

### Disable Auto-Download

Per controllo manuale:

```cpp
auto cache = GaiaCacheBuilder()
    .autoDownload(false)
    .build();

// Query (ritorna solo da cache)
auto stars = cache->queryRegion(ra, dec, radius, maxMag, false);

// Download esplicito se necessario
if (stars.empty()) {
    cache->downloadRegion(ra, dec, radius, maxMag);
    stars = cache->queryRegion(ra, dec, radius, maxMag, false);
}
```

### Cache Statistics

```cpp
auto stats = cache->getStats();
std::cout << "Cache Statistics:\n";
std::cout << "  Tiles: " << stats.total_tiles << "\n";
std::cout << "  Stars: " << stats.total_stars << "\n";
std::cout << "  Coverage: " << stats.sky_coverage << "%\n";
std::cout << "  Size: " << stats.total_size_mb << " MB\n";
std::cout << "  Mag range: " << stats.min_magnitude 
          << " to " << stats.max_magnitude << "\n";
```

### Cache Maintenance

```cpp
// Pulisci tiles vecchie (es. > 365 giorni)
int removed = cache->cleanOldTiles(365);
std::cout << "Removed " << removed << " old tiles\n";

// Clear completo
cache->clearCache();
```

## HEALPix System

### What is HEALPix?

HEALPix (Hierarchical Equal Area isoLatitude Pixelization) divide la sfera in tiles di area uguale. Vantaggi:

- **Equal Area**: Ogni tile copre la stessa area di cielo
- **Hierarchical**: Supporta multi-resolution (NSIDE)
- **Efficient**: Query region veloci
- **Standard**: Usato in astronomia (Planck, WMAP, etc.)

### Tile Numbering

RING scheme (usato qui):
- Tiles 0-N: Polo nord → equatore → polo sud
- Numerazione sequenziale per latitudine
- Tiles adiacenti hanno numeri vicini (buon per cache)

### Utility Functions

```cpp
// Convert coords → tile
int tileId = cache->coordsToHealpix(ra, dec, 32);

// Convert tile → coords (center)
double ra, dec;
cache->healpixToCoords(tileId, 32, ra, dec);

// Find tiles in region
auto tiles = cache->queryDisc(ra, dec, radius, 32);
std::cout << "Region covers " << tiles.size() << " tiles\n";
```

## Troubleshooting

### Slow Downloads

```bash
# 1. Check GAIA server status
curl -I https://gea.esac.esa.int/tap-server/tap

# 2. Increase timeout
client->setTimeout(600);  // 10 minutes

# 3. Download smaller regions
cache->downloadRegion(ra, dec, 5.0, maxMag);  // 5° invece di 10°
```

### Out of Disk Space

```bash
# Check cache size
du -sh ~/.ioccultcalc/gaia/

# Clean old tiles
./manage_gaia_cache --clean --older-than 180

# Or manually
rm -rf ~/.ioccultcalc/gaia/tiles/*
```

### Query Returns Empty

```bash
# Verify coverage
auto covered = cache->isCovered(ra, dec, radius, maxMag);

# Check magnitude limit
// Se tile ha maxMag=15 ma query chiede 16, tile va ri-scaricata
auto stats = cache->getStats();
if (stats.max_magnitude < requested_mag) {
    cache->clearCache();  // Force re-download con nuova mag
}
```

### Corrupted Tiles

```cpp
// Rimuovi singola tile
std::string tilePath = cache->getTilePath(tileId);
std::remove(tilePath.c_str());

// Reload index
cache->loadIndex();
```

## API Reference

### GaiaCache Class

```cpp
class GaiaCache {
public:
    // Query interface
    std::vector<GaiaStar> queryRegion(
        double raDeg, double decDeg, double radiusDeg,
        double maxMagnitude, bool autoDownload = true);
    
    std::vector<GaiaStar> queryPath(
        const std::vector<EquatorialCoordinates>& path,
        double widthDeg, double maxMagnitude, 
        bool autoDownload = true);
    
    bool isCovered(double raDeg, double decDeg, 
                   double radiusDeg, double maxMagnitude) const;
    
    // Cache management
    int downloadRegion(double raDeg, double decDeg, double radiusDeg,
                      double maxMagnitude,
                      std::function<void(int, int)> progressCallback = nullptr);
    
    int downloadPath(const std::vector<EquatorialCoordinates>& path,
                    double widthDeg, double maxMagnitude,
                    std::function<void(int, int)> progressCallback = nullptr);
    
    bool loadIndex();
    bool saveIndex();
    GaiaCacheStats getStats() const;
    int cleanOldTiles(int maxAgeDays);
    void clearCache();
    
    // Configuration
    void setGaiaClient(std::shared_ptr<GaiaClient> client);
    void setAutoDownload(bool enable);
    void setVerbose(bool verbose);
    
    // HEALPix utilities (public for testing)
    int coordsToHealpix(double raDeg, double decDeg, int nside = 32) const;
    void healpixToCoords(int healpix, int nside, double& raDeg, double& decDeg) const;
    std::vector<int> queryDisc(double raDeg, double decDeg, double radiusDeg, int nside = 32) const;
};
```

### GaiaCacheBuilder

```cpp
GaiaCacheBuilder builder;
auto cache = builder
    .cacheDir("/custom/path")
    .gaiaClient(client)
    .autoDownload(true)
    .verbose(true)
    .build();
```

### GaiaTile Structure

```cpp
struct GaiaTile {
    int healpix_id;            // HEALPix tile ID
    double ra_center;          // Center RA (deg)
    double dec_center;         // Center Dec (deg)
    double radius;             // Radius (deg)
    int star_count;            // Number of stars
    double max_magnitude;      // Magnitude limit
    std::string last_update;   // Download timestamp
    std::vector<GaiaStar> stars;  // Star data
};
```

## Best Practices

1. **Pre-Download for Surveys**: Se sai la regione, pre-scarica prima della survey
2. **Appropriate Magnitude**: Non scaricare mag 20 se serve solo mag 16
3. **Monitor Cache Size**: Controlla periodicamente con `getStats()`
4. **Persistent Cache**: Riusa cache tra runs - non ricaricare ogni volta
5. **Backup**: Backup cache prima di clearing (è costoso ri-scaricare)

## Future Enhancements

- [ ] Compressed tiles (gzip JSON)
- [ ] Multi-resolution support (adaptive NSIDE)
- [ ] Parallel downloads
- [ ] FITS format support
- [ ] Automatic cache updates (GAIA DR4)
- [ ] Query optimization (spatial index)
- [ ] Cache sharing (network mount)

## References

- **GAIA DR3**: https://gea.esac.esa.int/archive/
- **TAP Service**: https://gea.esac.esa.int/tap-server/tap
- **HEALPix**: https://healpix.sourceforge.io/
- **GAIA Data Model**: https://gea.esac.esa.int/archive/documentation/

## Support

- Test program: `./build/examples/test_gaia_cache`
- Documentation: `docs/GAIA_CACHE.md` (this file)
- Examples: `examples/test_gaia_cache.cpp`
