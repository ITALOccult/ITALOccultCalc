# Guida Rapida File .OOP con Liste Combinate

## Cos'è un file .OOP?

Un file `.oop` (Occultation Observation Preset) è un file di configurazione per IOccultCalc che definisce tutti i parametri per una ricerca di occultazioni stellari da asteroidi.

## Esempi Disponibili

### 1. `preset_quick_combined.oop` - Uso Quotidiano
**Caso d'uso**: Survey veloce con primi 1000 asteroidi + lista locale

```ini
[asteroid_selection]
input_type = "combined"
range_from = 1
range_to = 1000
additional_files = ["../etc/localasteroid"]
```

**Esecuzione**:
```bash
./ioccultcalc --preset preset_quick_combined.oop
```

**Output**: `results/quick_survey_asteroids_20260101.txt`

---

### 2. `preset_combined_lists_example.oop` - Configurazione Completa
**Caso d'uso**: Survey completo con tutte le opzioni configurabili

Caratteristiche:
- Range [1-1000] con filtri opzionali
- File multipli (localasteroid + altri)
- Controllo completo output ASTNUM_LIST
- Tutte le opzioni avanzate

```ini
[asteroid_selection]
input_type = "combined"
range_from = 1
range_to = 1000
# Filtri solo sul range (non sui file!)
# where_conditions = ["diameter > 20", "H < 12"]
additional_files = ["../etc/localasteroid"]
```

---

### 3. `preset_multifile_survey.oop` - Solo File (No Range)
**Caso d'uso**: Combinare solo liste da file, senza range

```ini
[asteroid_selection]
input_type = "file"
additional_files = [
    "../etc/localasteroid",
    "./priority_asteroids.txt",
    "./research_targets.txt"
]
```

**Workflow**:
1. Crea i tuoi file lista
2. Aggiungili a `additional_files`
3. Esegui survey
4. Output riutilizzabile come input!

---

## Sezioni Principali

### 🎯 Selezione Asteroidi

```ini
[asteroid_selection]
input_type = "combined"          # combined, range, file, explicit_list
range_from = 1
range_to = 1000
additional_files = [
    "../etc/localasteroid",      # Lista locale (140 asteroidi)
    "./my_targets.txt"           # Le tue selezioni
]
file_format = "auto_detect"      # Rileva automaticamente formato
```

**Input Types**:
- `combined`: Range + file multipli
- `range`: Solo range [from-to]
- `file`: Solo file (no range)
- `explicit_list`: Lista hardcoded nel preset

### 📅 Intervallo Temporale

```ini
[time_range]
start_date = "2026-01-01"
end_date = "2026-12-31"
time_step = 1.0                  # ore
```

### 🔍 Parametri Ricerca

```ini
[search_parameters]
max_star_magnitude = 14.0        # Mag limite stelle
min_duration = 0.5               # Durata minima (sec)
min_elevation = 20.0             # Altezza minima (gradi)
max_shadow_velocity = 100.0      # Velocità max ombra (km/s)
```

### ⭐ Catalogo Stelle

```ini
[star_catalog]
catalog = "gaia_edr3"
use_online = false
cache_directory = "./gaia_cache"
auto_download_cache = true
```

### 🌍 Osservatore

```ini
[observer]
latitude = 45.4642               # Torino
longitude = 9.1900
altitude = 239.0                 # metri
site_name = "Turin Observatory"
```

### 📊 Output

```ini
[output]
formats = ["JSON", "ASTNUM_LIST", "KML"]
output_directory = "./results"
filename_prefix = "my_survey"
verbosity = 2                    # 0=quiet, 1=normal, 2=verbose, 3=debug
```

### 📝 Opzioni ASTNUM_LIST

```ini
[astnum_list_output]
include_event_count = true       # (1) # Ceres (5 events)
include_names = true
sort_by = "event_count"          # number, name, event_count
sort_order = "descending"        # ascending, descending
include_header = true
include_footer = true
```

---

## Formati File Supportati

### ASTNUM_LIST (Output IOccultCalc)
```
(1) # Ceres (15 events)
(4) # Vesta (12 events)
(10) # Hygiea (8 events)
```
✅ Riutilizzabile come input direttamente!

### MPC_NUMBERED (Numeri semplici)
```
1
4
10
433
```
✅ Formato `etc/localasteroid`

### PLAIN_TEXT (Misto)
```
(1) Ceres
4 Vesta
2023 AB
433
```
✅ Parsing flessibile

**Auto-detect**: Il sistema riconosce automaticamente il formato!

---

## Workflow Tipico

### Scenario 1: Survey Mensile Standard

```bash
# 1. Prepara preset
cp preset_quick_combined.oop preset_january_2026.oop

# 2. Modifica date
[time_range]
start_date = "2026-01-01"
end_date = "2026-01-31"

# 3. Esegui
./ioccultcalc --preset preset_january_2026.oop

# 4. Output
results/quick_survey_asteroids_20260101.txt
results/quick_survey_20260101.json
```

### Scenario 2: Survey Iterativo

```bash
# Round 1: Survey primi 1000
./ioccultcalc --preset preset_quick_combined.oop
# Output: results/quick_survey_asteroids_20260101.txt (150 con eventi)

# Round 2: Riutilizza output come input
[asteroid_selection]
additional_files = [
    "./results/quick_survey_asteroids_20260101.txt",  # Solo asteroidi con eventi!
    "../etc/localasteroid"
]

# Analisi approfondita solo su asteroidi promettenti
```

### Scenario 3: Liste Multiple

```bash
# Combina diverse fonti
[asteroid_selection]
input_type = "file"
additional_files = [
    "../etc/localasteroid",              # 140 asteroidi locali
    "./high_priority.txt",               # 20 alta priorità
    "./research_program.txt",            # 50 programma ricerca
    "./previous_survey_positive.txt"     # 30 da survey precedente
]
# Totale: ~240 asteroidi unici (dedup automatica)
```

---

## Tips & Tricks

### ✅ Best Practices

1. **Usa nomi descrittivi**:
   ```
   preset_jan2026_top1000_localasteroid.oop
   ```

2. **Commenta le modifiche**:
   ```ini
   # Modified 2026-01-15: Added diameter filter
   # where_conditions = ["diameter > 20"]
   ```

3. **Backup degli output**:
   ```bash
   mkdir -p results/archive/2026-01
   mv results/*20260101* results/archive/2026-01/
   ```

4. **Riutilizza output**:
   ```ini
   # Output ASTNUM_LIST è pronto per il riuso!
   additional_files = ["./results/previous_survey_asteroids.txt"]
   ```

### ⚡ Performance

```ini
[performance]
use_parallel = true
num_threads = 0              # 0 = auto-detect CPU cores
use_cache = true
memory_limit = 4096          # MB
```

### 🎯 Filtri Avanzati

```ini
[asteroid_selection]
# Filtri applicati SOLO al range, non ai file!
where_conditions = [
    "diameter > 20",         # Solo grandi (>20 km)
    "H < 12",                # Luminosi
    "a > 2.2",               # Main belt
    "a < 3.2"
]
```

⚠️ **Nota**: Filtri `where_conditions` richiedono database proprietà asteroidi.

---

## Risoluzione Problemi

### File non trovato
```
ERROR: Cannot read file: ./my_list.txt
```
**Soluzione**: Usa path assoluti o relativi corretti:
```ini
additional_files = ["../etc/localasteroid"]  # Relativo a build dir
additional_files = ["/full/path/to/file.txt"]  # Assoluto
```

### Formato non riconosciuto
```
WARNING: Could not detect format for file.txt
```
**Soluzione**: Specifica formato manualmente:
```ini
file_format = "mpc_numbered"  # o astnum_list, plain_text
```

### Duplicati negli output
**Non è un problema!** La deduplicazione è automatica.
```
Input: 1000 da range + 140 da localasteroid (10 in comune)
Output: 1130 asteroidi unici ✓
```

---

## Esempi Pratici

Vedi i file di esempio:
- `example_priority_asteroids.txt` - Formato ASTNUM_LIST
- `example_research_targets.txt` - Formato MPC_NUMBERED
- `etc/localasteroid` - Lista locale 140 asteroidi

Test:
```bash
cd build/examples
./example_combined_asteroid_lists
```

---

## Quick Reference

| Parametro | Valori | Default |
|-----------|--------|---------|
| `input_type` | combined, range, file, explicit_list | combined |
| `file_format` | auto_detect, astnum_list, mpc_numbered, plain_text | auto_detect |
| `time_step` | ore | 1.0 |
| `max_star_magnitude` | 0-20 | 14.0 |
| `verbosity` | 0-3 | 1 |
| `num_threads` | 0=auto, 1-N | 0 |
| `catalog` | gaia_edr3, gaia_dr3, tycho2 | gaia_edr3 |

---

## Documentazione Completa

Per dettagli implementazione:
- `COMBINED_LISTS_IMPLEMENTATION.md` - Documentazione tecnica
- `PRESET_GUIDE.md` - Guida completa preset
- `examples/example_combined_asteroid_lists.cpp` - Esempi codice

---

**Ready to go!** 🚀

Scegli un preset, modifica le date/parametri, e lancia la tua survey di occultazioni!
