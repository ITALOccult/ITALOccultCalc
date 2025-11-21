# IOccultCalc Command-Line Tools

Utility a riga di comando per il calcolo delle occultazioni asteroidali.

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
