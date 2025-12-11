# IOccultCalc Command-Line Tools

Utility a riga di comando per il calcolo delle occultazioni asteroidali.

## Asteroid Database Management

### build_allnum_database

C++ tool per creare/aggiornare database SQLite da AstDyS `allnum.cat`:
- Scarica `allnum.cat` da AstDyS (formato OEF2.0)
- Parsa elementi orbitali kepleriani con conversioni corrette gradi/radianti
- Inserisce in SQLite con tracciamento data download
- Verifica automaticamente se database è vecchio (> 30 giorni) e aggiorna

**Usage:**
```bash
# Build/update database (updates if older than 30 days)
./build/tools/build_allnum_database

# Force update regardless of age
./build/tools/build_allnum_database --force

# Custom database path
./build/tools/build_allnum_database --db-path /path/to/allnum.db

# Custom max age (update if older than N days)
./build/tools/build_allnum_database --max-age 15
```

**Monthly Auto-Update:**
```bash
# Manual update
./tools/update_allnum_database.sh

# Update if older than 15 days
./tools/update_allnum_database.sh 15

# Add to crontab (runs on 1st of each month at 2 AM)
0 2 1 * * /path/to/IOccultCalc/tools/update_allnum_database.sh
```

**Database Schema:**
- `allnum_asteroids`: elementi orbitali kepleriani (a, e, i, Omega, omega, M in radianti)
- `allnum_metadata`: tracciamento data download, totale record, epoca dati

**Data Source:**
- AstDyS allnum.cat: https://newton.spacedys.com/~astdys2/catalogs/allnum.cat

### build_asteroid_database.py

Builds a unified asteroid database combining:
- **Orbital elements** from AstDyS `allnum.cat` (high precision)
- **Physical properties** from MPC Extended JSON (diameter, albedo, spectral type)

**Features:**
- Downloads latest data from AstDyS and MPC
- Merges orbital elements with physical properties
- Tracks parsing date for periodic updates
- Auto-checks if update is needed (default: 30 days)

**Usage:**
```bash
# Build/update database (updates if older than 30 days)
python3 tools/build_asteroid_database.py

# Force update regardless of age
python3 tools/build_asteroid_database.py --force

# Custom output location
python3 tools/build_asteroid_database.py --output /path/to/asteroids.json

# Custom max age (update if older than N days)
python3 tools/build_asteroid_database.py --max-age 15
```

**Monthly Auto-Update:**

Add to crontab for monthly updates:
```bash
# Edit crontab
crontab -e

# Add this line (runs on 1st of each month at 2 AM)
0 2 1 * * /path/to/IOccultCalc/tools/update_asteroid_database.sh
```

Or use the wrapper script:
```bash
# Manual update
./tools/update_asteroid_database.sh

# Update if older than 15 days
./tools/update_asteroid_database.sh 15
```

**Data Sources:**
- AstDyS allnum.cat: https://newton.spacedys.com/astdys2/index.php?pc=4
- MPC Extended JSON: https://minorplanetcenter.net/Extended_Files/mpcorb_extended.json.gz

## occult_calc

Calcola le occultazioni stellari per un asteroide in una data specifica.

### Compilazione

```bash
cd IOccultCalc
mkdir -p build && cd build
cmake ..
make
```

L'eseguibile sarà in `build/tools/occult_calc`.

### Uso Base

```bash
./occult_calc <numero_asteroide> <data> [opzioni]
```

**Argomenti richiesti:**
- `numero_asteroide`: Numero dell'asteroide (es. 433 per Eros)
- `data`: Data dell'evento in formato YYYY-MM-DD

### Opzioni

| Opzione | Descrizione | Default |
|---------|-------------|---------|
| `--format=FORMAT` | Formato output: `iota`, `preston`, `xml`, `json` | `iota` |
| `--method=METHOD` | Metodo calcolo: `fast`, `standard`, `precise` | `standard` |
| `--output=FILE` | Salva output su file invece che stdout | - |
| `--all-formats` | Genera tutti i formati disponibili | - |
| `--observer=NAME` | Nome dell'osservatore | IOccultCalc User |
| `--location=LAT,LON` | Coordinate osservatore (gradi decimali) | - |
| `--search-window=DAYS` | Finestra di ricerca in giorni | ±15 |
| `--min-duration=SEC` | Durata minima evento in secondi | - |
| `--max-magnitude=MAG` | Magnitudine massima stella | 20.0 |
| `--verbose` | Output dettagliato | - |
| `--help` | Mostra l'aiuto | - |

### Formati Output

#### IOTA (Default)
Formato standard dell'International Occultation Timing Association:
```
================================================================================
                  ASTEROID OCCULTATION PREDICTION
================================================================================

Asteroid: (433) Eros
Star: Gaia DR3 1234567890123456
Event time: 2461116.205000
Close approach: 0.035 arcsec
Position angle: 125.5 deg
Max duration: 0.9 sec
Probability: 95%

Shadow Path:
  Latitude    Longitude   Time (UTC)        Duration
  -----------------------------------------------------------------
   40.0000   -5.0000  2461116.2050  0.9000s
   41.0000   -4.4000  2461116.2050  0.8000s
```

#### Preston
Formato compatto utilizzato da Steve Preston:
```
(433) Eros  2461116.205000 UTC
Star: Gaia DR3 1234567890123456  Mag 11.2
C/A: 0.035"  PA: 125.5°
Duration: 0.9 sec  Prob: 95%

Center Line:
  40.0000  -5.0000
  41.0000  -4.4000
```

#### XML (Occult4)
Formato XML compatibile con Occult4:
```xml
<?xml version="1.0" encoding="UTF-8"?>
<occultation>
  <asteroid>(433) Eros</asteroid>
  <star>1234567890123456</star>
  <time>2461116.205000</time>
  <ca>0.035</ca>
  <pa>125.5</pa>
  <duration>0.9</duration>
  <path>
    <point lat="40" lon="-5"/>
    <point lat="41" lon="-4.4"/>
  </path>
</occultation>
```

#### JSON
Formato JSON strutturato:
```json
{
  "asteroid": "(433) Eros",
  "star": {
    "gaia_id": "1234567890123456",
    "ra": 187.5,
    "dec": 15.3,
    "magnitude": 11.2
  },
  "event": {
    "time_jd": 2461116.205000,
    "close_approach_arcsec": 0.035,
    "position_angle_deg": 125.5,
    "max_duration_sec": 0.9,
    "probability": 0.95
  },
  "shadow_path": [
    {"lat": 40.0, "lon": -5.0, "jd": 2461116.2050, "duration": 0.9}
  ]
}
```

### Esempi

**Calcolo base per Eros:**
```bash
./occult_calc 433 2026-03-15
```

**Output in formato Preston salvato su file:**
```bash
./occult_calc 15 2026-05-08 --format=preston --output=eunomia.txt
```

**Calcolo ad alta precisione con output verbose:**
```bash
./occult_calc 16 2026-07-20 --method=precise --verbose
```

**Genera tutti i formati contemporaneamente:**
```bash
./occult_calc 704 2025-12-15 --all-formats
```

**Calcolo specifico per una località:**
```bash
./occult_calc 433 2026-03-15 --location=40.0,-5.0 --observer="Bologna Observatory"
```

**Output JSON con filtri:**
```bash
./occult_calc 10 2026-06-01 --format=json --min-duration=5.0 --max-magnitude=14.0
```

### Metodi di Calcolo

- **fast**: Integrazione veloce con passi più grandi. Per survey preliminari.
- **standard**: Bilanciamento tra velocità e precisione. Uso generale.
- **precise**: Alta precisione con integrazione a piccoli passi. Per eventi critici.

### Note Implementative

**Versione attuale (v1.0):**
L'utility utilizza dati di esempio per scopi dimostrativi. Le coordinate del percorso d'ombra sono basate sui dati delle predizioni IOTA/Preston reali.

**Sviluppi futuri:**
- Integrazione catalogo AstDyS per elementi orbitali aggiornati
- Query automatiche a Gaia DR3 per dati stellari
- Calcolo ephemeris con JPL DE441
- Modello di forze gravitazionali completo (perturbazioni planetarie)
- Correzione per parallasse e moto proprio stellare
- Calcolo specifico per sito osservativo

### Formato Date

Le date devono essere nel formato `YYYY-MM-DD`:
- `2026-03-15` ✓
- `2026-3-15` ✗ (mese deve essere 2 cifre)
- `15/03/2026` ✗ (formato non supportato)
- `2026-03-15T12:30:00` ✗ (orario non supportato)

### Troubleshooting

**Errore: "Invalid date format"**
```bash
# Errato:
./occult_calc 433 15-03-2026

# Corretto:
./occult_calc 433 2026-03-15
```

**Errore: "Invalid location format"**
```bash
# Errato:
./occult_calc 433 2026-03-15 --location=40.0 -5.0

# Corretto:
./occult_calc 433 2026-03-15 --location=40.0,-5.0
```

**Output troppo verbose**
Ometti il flag `--verbose` per output standard.

**Formato non riconosciuto**
```bash
# Formati supportati: iota, preston, xml, json
./occult_calc 433 2026-03-15 --format=iota
```

## Licenza

Parte del progetto IOccultCalc - vedi LICENSE nel root del repository.
