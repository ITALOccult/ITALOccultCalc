# IOccultCalc - Database Survey Integration Guide

Questa guida mostra come utilizzare IOccultCalc per survey massive di occultazioni usando il database asteroidi e la cache GAIA.

## 🎯 Obiettivo

Cercare occultazioni stellari per centinaia o migliaia di asteroidi contemporaneamente, interrogando automaticamente le stelle GAIA lungo i loro percorsi.

## 📋 Prerequisiti

### 1. Database Asteroidi

```bash
# Download database MPC (~100 MB)
python3 tools/download_mpc_database.py

# Verifica
ls -lh ~/.ioccultcalc/database/mpc_asteroids.json
```

### 2. Effemeridi JPL

```bash
# Assicurati di avere le effemeridi planetarie
ls -lh ~/.ioccultcalc/ephemerides/*.bsp
```

### 3. Compilazione

```bash
cd /Users/michelebigi/VisualStudio\ Code/GitHub/IOccultCalc
./build.sh
```

## 🚀 Modalità d'Uso

### Modalità 1: Singolo Asteroide + Singola Stella

Ricerca classica per un asteroide specifico e una stella target:

```bash
./build/examples/ioccultcalc_search \
  --asteroid 433 \
  --star 10.684,41.269 \
  --start 2026-01-01 \
  --end 2026-12-31
```

**Output:**
```
╔══════════════════════════════════════════════════════════╗
║   IOccultCalc - Single Asteroid Search                  ║
╚══════════════════════════════════════════════════════════╝

Configuration:
  Asteroid: 433
  Star: RA=10.684° Dec=41.269°
  Period: 2026-01-01 to 2026-12-31

📡 Downloading orbital elements...
   ✓ Elements for 433 Eros

🔧 Setting up propagator...
   ✓ Propagator ready

🛰️  Calculating asteroid path...
   Time span: 365.0 days
   Steps: 3650

🔍 Searching for occultations...
   Progress: 100.0%

═══════════════════ RESULTS ═══════════════════
Close approaches found: 3

Closest approaches:
  2026-03-15T12:34:56 - 15.23 arcsec
  2026-08-22T08:45:12 - 42.67 arcsec
  2026-11-30T20:15:33 - 58.91 arcsec
```

### Modalità 2: Database Survey + Filtri

Ricerca massiva con filtri SQL-like, senza cache GAIA (usa stelle singole):

```bash
./build/examples/ioccultcalc_search \
  --database \
  --filter "diameter > 50" \
  --filter "H < 12" \
  --start 2026-01-01 \
  --end 2026-12-31 \
  --output occultations_2026.json
```

**Output:**
```
╔══════════════════════════════════════════════════════════╗
║   IOccultCalc - Database Survey Mode                    ║
╚══════════════════════════════════════════════════════════╝

📦 Loading asteroid database...
   ✓ Loaded 750000 asteroids

🔍 Applying filters...
   WHERE diameter > 50
   WHERE H < 12
   ✓ Selected 127 asteroids

🔄 Processing asteroids...
   Progress: 127/127 (100.0%)

═══════════════════ RESULTS ═══════════════════
Asteroids processed: 127
Occultations found:  42

Results saved to: occultations_2026.json
```

### Modalità 3: Database Survey + Cache GAIA (Full Power!)

Ricerca massiva con query automatica delle stelle GAIA lungo i percorsi:

```bash
./build/examples/ioccultcalc_search \
  --database \
  --filter "diameter > 100" \
  --gaia-cache \
  --start 2026-01-01 \
  --end 2026-12-31 \
  --verbose
```

**Output:**
```
╔══════════════════════════════════════════════════════════╗
║   IOccultCalc - Database Survey Mode                    ║
╚══════════════════════════════════════════════════════════╝

📦 Loading asteroid database...
   ✓ Loaded 750000 asteroids

🔍 Applying filters...
   WHERE diameter > 100
   ✓ Selected 89 asteroids

⭐ Setting up GAIA cache...
   ✓ Cache ready (124 tiles, 234567 stars)

🔄 Processing asteroids...
   
   Asteroid (1) Ceres
   Path: 365 points over 365 days
   GAIA query: 8 tiles, 1234 stars (mag < 16.0)
   Occultations: 3 events found
   
   Asteroid (2) Pallas
   Path: 365 points over 365 days
   GAIA query: 6 tiles, 892 stars (mag < 16.0)
   Occultations: 1 event found
   
   ... (downloading missing tiles as needed) ...
   
   Progress: 89/89 (100.0%)

═══════════════════ RESULTS ═══════════════════
Asteroids processed: 89
Stars checked:       78543
Occultations found:  156

Results saved to: occultations_2026.json
```

### Modalità 4: Configurazione da File JSON

Ricerca configurata completamente da file JSON:

**File:** `survey_2026_large_asteroids.json`
```json
{
  "asteroid_range": {
    "mode": "database",
    "where": [
      "diameter > 100",
      "H < 10",
      "orbit_type = 'MBA'"
    ],
    "where_not": [
      "orbit_class = 'Centaur'"
    ]
  },
  
  "gaia_cache": {
    "enabled": true,
    "auto_download": true,
    "tap_url": "https://gea.esac.esa.int/tap-server/tap",
    "max_magnitude": 16.0,
    "timeout_seconds": 300
  },
  
  "search_parameters": {
    "start_date": "2026-01-01",
    "end_date": "2026-12-31",
    "time_step_hours": 2.4,
    "path_width_deg": 0.01,
    "max_magnitude": 16.0
  },
  
  "propagator": {
    "integrator": "RK78",
    "step_size": 0.05,
    "tolerance": 1e-12,
    "perturbations": {
      "planets": true,
      "moon": true,
      "sun": true,
      "relativistic": false
    }
  },
  
  "output": {
    "file": "occultations_2026_large.json",
    "format": "json",
    "include_path": false,
    "verbose": true
  }
}
```

**Comando:**
```bash
./build/examples/ioccultcalc_search --config survey_2026_large_asteroids.json
```

## 📊 Filtri Disponibili

### Filtri Asteroidali

```sql
-- Dimensione
"diameter > 100"          -- Diametro > 100 km
"diameter between 50,200" -- Diametro tra 50 e 200 km

-- Magnitudine
"H < 10"                  -- Magnitudine assoluta < 10
"H between 8,12"          -- H tra 8 e 12

-- Classe orbitale
"orbit_type = 'MBA'"      -- Main Belt Asteroid
"orbit_type = 'NEA'"      -- Near-Earth Asteroid
"orbit_class = 'Apollo'"  -- Classe Apollo

-- Numero
"number < 1000"           -- Primi 1000 asteroidi numerati
"number in (1,2,4,10)"    -- Ceres, Pallas, Vesta, Hygiea

-- Designazione
"designation like '2024%'" -- Scoperti nel 2024

-- Combinate
"diameter > 50 AND H < 12 AND orbit_type = 'MBA'"
```

### Filtri GAIA

```json
{
  "gaia_cache": {
    "max_magnitude": 16.0,        // Stelle fino a mag 16
    "min_magnitude": 8.0,         // Escludi stelle troppo luminose
    "proper_motion_threshold": 100 // mas/year (opzionale)
  }
}
```

## 🎯 Esempi Pratici

### Esempio 1: Asteroidi Grandi nel 2026

**Obiettivo:** Trovare tutte le occultazioni di asteroidi > 100 km nel 2026

```bash
./build/examples/ioccultcalc_search \
  --database \
  --filter "diameter > 100" \
  --gaia-cache \
  --start 2026-01-01 \
  --end 2026-12-31 \
  -o large_asteroids_2026.json
```

**Asteroidi selezionati:** ~89 (Ceres, Pallas, Vesta, Hygiea, ...)
**Stelle controllate:** ~50,000-100,000
**Occultazioni attese:** ~100-200
**Tempo esecuzione:** ~30-60 minuti

### Esempio 2: Survey NEA nel Q1 2026

**Obiettivo:** Near-Earth Asteroids nel primo trimestre 2026

```bash
./build/examples/ioccultcalc_search \
  --database \
  --filter "orbit_type = 'NEA'" \
  --filter "diameter > 1" \
  --gaia-cache \
  --start 2026-01-01 \
  --end 2026-03-31 \
  -o nea_q1_2026.json \
  --verbose
```

**Asteroidi selezionati:** ~2,000 NEA
**Stelle controllate:** ~500,000+
**Tempo esecuzione:** ~2-4 ore

### Esempio 3: Previsioni Specifiche (IOTA Style)

**Obiettivo:** Solo asteroidi grandi e luminosi per osservazioni amatoriali

```json
{
  "asteroid_range": {
    "mode": "database",
    "where": [
      "diameter > 80",
      "H < 9",
      "number < 200"
    ]
  },
  "search_parameters": {
    "max_magnitude": 12.0,  // Solo stelle visibili
    "min_probability": 0.5   // Solo eventi probabili
  }
}
```

```bash
./build/examples/ioccultcalc_search --config iota_predictions_2026.json
```

## 📈 Performance

### Benchmark Tipici

| Scenario | Asteroidi | Stelle | Occultazioni | Tempo |
|----------|-----------|--------|--------------|-------|
| Single asteroid | 1 | 1 | 0-5 | < 1 min |
| 100 large asteroids | 100 | ~10K | ~50 | ~10 min |
| 1000 NEA | 1000 | ~100K | ~200 | ~1-2 ore |
| Full survey (10K ast) | 10000 | ~1M | ~2000 | ~10-20 ore |

### Ottimizzazioni

1. **Pre-download GAIA tiles**
   ```bash
   # Download regione prima della survey
   ./build/tools/gaia_preload_region 0 360 -30 30 --max-mag 16
   ```

2. **Parallel processing** (future)
   ```bash
   ./build/examples/ioccultcalc_search --config survey.json --threads 8
   ```

3. **Caching intelligente**
   - Tiles GAIA: Download una volta, riusa per sempre
   - Elementi orbitali: Cache per evitare richieste ripetute
   - Propagazione: Cache stati per epoch comuni

## 📤 Formato Output

### JSON Standard

```json
{
  "survey_metadata": {
    "start_date": "2026-01-01T00:00:00Z",
    "end_date": "2026-12-31T23:59:59Z",
    "asteroids_processed": 127,
    "stars_checked": 45678,
    "occultations_found": 42,
    "elapsed_seconds": 1234.56
  },
  
  "occultations": [
    {
      "asteroid": {
        "number": 1,
        "name": "Ceres",
        "diameter_km": 939.4,
        "magnitude_H": 3.53
      },
      "star": {
        "gaia_source_id": "1234567890123456789",
        "ra_deg": 123.456,
        "dec_deg": 45.678,
        "magnitude": 12.3,
        "parallax_mas": 5.67
      },
      "event": {
        "time": "2026-03-15T12:34:56.789Z",
        "closest_approach_arcsec": 0.234,
        "probability": 0.89,
        "shadow_width_km": 850,
        "duration_seconds": 12.3,
        "magnitude_drop": 8.2
      },
      "visibility": {
        "observable_from": ["North America", "Europe"],
        "best_location": {"lat": 45.5, "lon": -73.6},
        "sun_altitude": -35.2,
        "moon_distance": 67.8
      }
    }
  ]
}
```

### CSV Export (per Excel/spreadsheet)

```csv
Date,Time,Asteroid,Star_GAIA_ID,RA,Dec,Separation,Probability,Duration,Visible_From
2026-03-15,12:34:56,(1) Ceres,1234567890123456789,123.456,45.678,0.234,0.89,12.3,"North America; Europe"
```

## 🔧 Troubleshooting

### "Failed to load asteroid database"

**Soluzione:**
```bash
python3 tools/download_mpc_database.py
```

### "GAIA TAP server timeout"

**Soluzione:**
```json
{
  "gaia_cache": {
    "timeout_seconds": 600,  // Aumenta timeout
    "retry_attempts": 3
  }
}
```

### "Out of memory"

**Soluzione:**
- Riduci numero asteroidi con filtri più stringenti
- Riduci `max_magnitude` per limitare stelle
- Aumenta `path_width_deg` solo se necessario (più grande = più stelle)

### Performance lente

**Soluzione:**
- Aumenta `time_step_hours` (meno punti nel percorso)
- Pre-download tiles GAIA
- Usa SSD per cache
- Considera parallel processing (future)

## 🚀 Roadmap

### Prossime Features

1. **Parallel Processing**
   - Multi-threading per processare più asteroidi contemporaneamente
   - GPU acceleration per geometria occultazioni

2. **Enhanced Filtering**
   - Geographic visibility filtering
   - Observer location constraints
   - Minimum magnitude drop requirements

3. **Real-time Updates**
   - WebSocket server per progress live
   - REST API per query remote
   - Web UI per visualizzazione

4. **Advanced Analytics**
   - Heatmaps di regioni con più occultazioni
   - Statistical analysis (rate per asteroid class)
   - Machine learning per rank eventi più interessanti

## 📚 Reference

### Documentazione Completa

- [DATABASE_SETUP.md](DATABASE_SETUP.md) - Setup database asteroidi
- [GAIA_CACHE.md](GAIA_CACHE.md) - Sistema cache GAIA
- [ASTEROID_FILTERING.md](ASTEROID_FILTERING.md) - Sintassi filtri completa
- [CONFIG_SYSTEM.md](CONFIG_SYSTEM.md) - Sistema configurazione

### API Reference

- `AsteroidDatabase` - Query database asteroidi
- `GaiaCache` - Cache stelle GAIA con HEALPix
- `AsteroidRangeBuilder` - Costruzione filtri SQL-like
- `OrbitPropagator` - Propagazione orbitale AST17
- `OccultationPredictor` - Predizione geometria occultazioni

## 🎓 Tutorial Step-by-Step

### Primo Survey Completo

1. **Prepara ambiente**
   ```bash
   cd IOccultCalc
   ./build.sh
   python3 tools/download_mpc_database.py
   ```

2. **Crea configurazione**
   ```bash
   cp examples/config_templates/survey_database.json my_survey.json
   # Edit my_survey.json con filtri desiderati
   ```

3. **Test piccolo**
   ```bash
   # Prima prova con pochi asteroidi
   ./build/examples/ioccultcalc_search --config my_survey.json \
     --filter "number in (1,2,4)" --verbose
   ```

4. **Survey completo**
   ```bash
   # Rimuovi filtro numero, esegui survey completo
   ./build/examples/ioccultcalc_search --config my_survey.json
   ```

5. **Analizza risultati**
   ```bash
   # Visualizza in JSON viewer o importa in Excel
   cat occultations_2026.json | jq '.occultations[] | select(.event.probability > 0.5)'
   ```

---

**Happy occultation hunting! 🌟🔭**
