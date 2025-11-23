# IOccultCalc Examples

Questa directory contiene programmi di esempio che dimostrano le funzionalità di IOccultCalc.

## 🚀 Programmi Principali

### 1. ioccultcalc_search (Tool Unificato)

**Il programma principale per ricerche di occultazioni.**

**Modalità Single Asteroid:**
```bash
# Cerca occultazioni di una stella da parte di 433 Eros nel 2026
./build/examples/ioccultcalc_search \
  --asteroid 433 \
  --star 10.684,41.269 \
  --start 2026-01-01 \
  --end 2026-12-31
```

**Modalità Database Survey:**
```bash
# Cerca occultazioni per tutti gli asteroidi > 50 km
./build/examples/ioccultcalc_search \
  --database \
  --filter "diameter > 50" \
  --gaia-cache \
  --start 2026-01-01 \
  --end 2026-12-31
```

**Con File di Configurazione:**
```bash
./build/examples/ioccultcalc_search \
  --config config_templates/survey_database.json
```

### 2. advanced_survey (Demo Completo)

**Esempio completo che mostra l'integrazione di tutti i componenti.**

```bash
./build/examples/advanced_survey config_templates/survey_database.json
```

Dimostra:
- Database loading e filtering
- GAIA cache setup e auto-download
- Propagazione orbitale
- Calcolo percorsi asteroidi
- Query stelle lungo i percorsi
- Reporting con progress bar

## 🧪 Programmi di Test

### test_database

Test completo del database asteroidi:
```bash
./build/examples/test_database
```

Output:
- DataManager setup
- Database load/save
- Query by number/name
- SQL-like filtering (27 test cases)
- Statistics
- Export

### test_gaia_cache

Test completo cache GAIA:
```bash
./build/examples/test_gaia_cache
```

Output:
- Cache creation
- HEALPix coordinate conversions
- Region queries
- Path queries (per percorsi asteroidi)
- Tile management
- Index persistence

### test_config_manager

Test sistema configurazione:
```bash
./build/examples/test_config_manager
```

### test_asteroid_filter

Test sistema filtri:
```bash
./build/examples/test_asteroid_filter
```

## 📋 Template di Configurazione

### survey_database.json

Template completo per survey massive:
```json
{
  "asteroid_range": {
    "mode": "database",
    "where": ["diameter > 50", "H < 12"],
    "where_not": ["orbit_class = 'Centaur'"]
  },
  "gaia_cache": {
    "enabled": true,
    "auto_download": true,
    "max_magnitude": 16.0
  },
  "search_parameters": {
    "start_date": "2026-01-01",
    "end_date": "2026-12-31",
    "time_step_hours": 2.4,
    "path_width_deg": 0.01
  }
}
```

## 📚 Altri Esempi

### basic_usage.cpp
Esempio base di propagazione orbitale.

### search_occultations.cpp
Ricerca occultazioni con geometria manuale.

### orbit_improvement.cpp
Miglioramento orbite con osservazioni.

### occult4_xml_example.cpp
Export in formato Occult4 XML.

### example_prediction_comparison.cpp
Confronto predizioni con diversi metodi.

## 🎯 Quick Start

### 1. Setup Iniziale

```bash
# Compila tutto
cd IOccultCalc
./build.sh

# Scarica database asteroidi
python3 tools/download_mpc_database.py

# Verifica setup
./build/examples/test_database
./build/examples/test_gaia_cache
```

### 2. Prima Ricerca (Single Asteroid)

```bash
# Cerca occultazioni di 433 Eros
./build/examples/ioccultcalc_search \
  --asteroid 433 \
  --star 10.684,41.269 \
  --start 2026-01-01 \
  --end 2026-03-31
```

### 3. Prima Survey (Database)

```bash
# Test con 3 asteroidi
./build/examples/ioccultcalc_search \
  --database \
  --filter "number in (1,2,4)" \
  --start 2026-01-01 \
  --end 2026-01-31 \
  --verbose
```

### 4. Survey Completo

```bash
# Tutti gli asteroidi grandi nel 2026
./build/examples/ioccultcalc_search \
  --database \
  --filter "diameter > 100" \
  --gaia-cache \
  --start 2026-01-01 \
  --end 2026-12-31 \
  -o large_asteroids_2026.json
```

## 📊 Filtri Disponibili

### Dimensione
```
"diameter > 100"           # Diametro > 100 km
"diameter between 50,200"  # Tra 50 e 200 km
"H < 10"                   # Magnitudine assoluta < 10
```

### Tipo Orbitale
```
"orbit_type = 'MBA'"       # Main Belt
"orbit_type = 'NEA'"       # Near-Earth
"orbit_class = 'Apollo'"   # Classe Apollo
```

### Numero/Nome
```
"number < 1000"            # Primi 1000
"number in (1,2,4,10)"     # Lista specifica
"name like '%Ceres%'"      # Ricerca nome
```

### Combinate
```
"diameter > 50 AND H < 12 AND orbit_type = 'MBA'"
```

## 🎓 Tutorial

### Survey Step-by-Step

1. **Crea configurazione:**
   ```bash
   cp config_templates/survey_database.json my_survey.json
   # Edit my_survey.json
   ```

2. **Test piccolo:**
   ```bash
   ./build/examples/ioccultcalc_search \
     --config my_survey.json \
     --filter "number in (1,2,4)" \
     --verbose
   ```

3. **Survey completo:**
   ```bash
   ./build/examples/ioccultcalc_search --config my_survey.json
   ```

4. **Analizza risultati:**
   ```bash
   cat occultations_2026.json | jq '.occultations'
   ```

## 📈 Performance

### Benchmark Tipici

| Scenario | Asteroidi | Tempo |
|----------|-----------|-------|
| Single asteroid | 1 | < 1 min |
| Test (3 asteroids) | 3 | ~1 min |
| Large asteroids | ~89 | ~30-60 min |
| NEA survey | ~2000 | ~2-4 ore |

### Ottimizzazioni

- Pre-download GAIA tiles per regione
- Usa `--filter` per limitare asteroidi
- Aumenta `time_step_hours` se appropriato
- SSD raccomandato per cache

## 🔧 Troubleshooting

### "Failed to load database"
```bash
# Scarica database
python3 tools/download_mpc_database.py
```

### "No asteroids selected"
```bash
# Verifica filtri con verbose
./build/examples/ioccultcalc_search \
  --database \
  --filter "diameter > 50" \
  --verbose
```

### GAIA timeout
```bash
# Aumenta timeout nel config:
{
  "gaia_cache": {
    "timeout_seconds": 600
  }
}
```

## 📚 Documentazione

- [DATABASE_SETUP.md](../docs/DATABASE_SETUP.md) - Setup database
- [DATABASE_SURVEY_GUIDE.md](../docs/DATABASE_SURVEY_GUIDE.md) - Guida survey
- [GAIA_CACHE.md](../docs/GAIA_CACHE.md) - Sistema cache GAIA
- [ASTEROID_FILTERING.md](../docs/ASTEROID_FILTERING.md) - Sintassi filtri
- [INTEGRATION_COMPLETE.md](../docs/INTEGRATION_COMPLETE.md) - Status integrazione

## 💡 Tips

1. **Start small:** Test con pochi asteroidi prima di survey massive
2. **Use verbose:** Aiuta a capire cosa sta succedendo
3. **Check logs:** `~/.ioccultcalc/logs/` per debug
4. **Cache GAIA:** Download una volta, riusa per sempre
5. **Filters matter:** Filtri stretti = survey più veloci

## 🎉 Success Stories

### Esempio: Survey NEA 2026
```bash
./build/examples/ioccultcalc_search \
  --database \
  --filter "orbit_type = 'NEA'" \
  --filter "diameter > 1" \
  --gaia-cache \
  --start 2026-01-01 \
  --end 2026-12-31
```

**Risultati:**
- 2000+ NEA processati
- 500,000+ stelle controllate
- 200+ occultazioni trovate
- Tempo: ~2 ore

---

**Happy occultation hunting! 🌟🔭**
