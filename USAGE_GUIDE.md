# IOccultCalc - Guida Rapida Utilizzo

## Installazione

Gli eseguibili sono già installati in `/usr/local/bin` e disponibili nel PATH:

```bash
# Verifica installazione
which italoccultcalc
which ioccultcalc_search
which occult_calc
```

## Comandi Disponibili

### 1. italoccultcalc - Pipeline Completa

Ricerca automatica occultazioni asteroidali con pipeline completa a 7 fasi.

**Uso con file di configurazione JSON:**
```bash
italoccultcalc config.json
```

**Uso con file di configurazione OOP (formato OrbFit):**
```bash
italoccultcalc config.oop
```

**Opzioni disponibili:**
```bash
-v, --verbose    Mostra progresso dettagliato con % e nomi asteroidi
-h, --help       Mostra aiuto
```

**Esempio con output verboso:**
```bash
italoccultcalc -v preset_large_asteroids_jan2026.oop
```

Output verboso mostra:
```
[100%] (324) Bamberga
[100%] Analisi (10) Hygiea vs 1000 stelle
```

### 2. ioccultcalc_search - Ricerca Occultazioni

Ricerca occultazioni per periodo specifico.

```bash
ioccultcalc_search asteroids.txt 2026-01-01 2026-12-31 14.0
```

Parametri:
- `asteroids.txt`: File con lista numeri asteroidi
- Data inizio: YYYY-MM-DD
- Data fine: YYYY-MM-DD  
- Magnitudine limite: valore decimale

### 3. occult_calc - Calcolo Base

Calcolo singola occultazione.

```bash
occult_calc 433 2026-03-15
```

## File di Configurazione

### Formato JSON

Esempio: `preset_default.json`

```json
{
  "sections": [
    {
      "type": "object",
      "parameters": [
        {"name": "id", "value": "433", "type": "string"},
        {"name": "name", "value": "Eros", "type": "string"}
      ]
    },
    {
      "type": "propag",
      "parameters": [
        {"name": "step_size", "value": "0.05", "type": "double"},
        {"name": "type", "value": "RK4", "type": "string"}
      ]
    },
    {
      "type": "ephemeris",
      "parameters": [
        {"name": "jpl_version", "value": "DE441", "type": "string"}
      ]
    },
    {
      "type": "output",
      "parameters": [
        {"name": "format", "value": "JSON", "type": "string"},
        {"name": "file", "value": "results.json", "type": "string"}
      ]
    }
  ]
}
```

### Formato OOP (OrbFit-style)

Esempio: `config.oop`

```plaintext
! IOccultCalc Configuration File

object.
        .id = '433'
        .name = '433 Eros'

propag.
        .start_jd = 2461041.000000
        .end_jd = 2461405.000000
        .step_size = 0.050000
        .type = 'RK4'

ephemeris.
        .jpl_version = 'DE441'
        .ast17_file = '~/.ioccultcalc/ephemerides/codes_300ast_20100725.bsp'

output.
        .file = 'results.json'
        .format = 'JSON'
        .verbosity = 1

perturbations.
        .planets = .TRUE.
        .relativity = .TRUE.
        .asteroid_count = 17
```

## Template di Configurazione

Template predefiniti disponibili in `examples/config_templates/`:

```bash
# Survey asteroidi NEA
italoccultcalc examples/config_templates/nea_survey.json

# Survey Troiani di Giove
italoccultcalc examples/config_templates/trojans.json

# Ricerca con database completo
italoccultcalc examples/config_templates/survey_database.json
```

## Formati Output

ITALOccultCalc supporta 5 formati di output:

1. **TEXT**: Report leggibile
2. **JSON**: Dati strutturati
3. **LATEX/PDF**: Documentazione scientifica
4. **XML_OCCULT4**: Import OccultWatcher Cloud
5. **IOTA_CARD**: Schede osservative JPG

Specificare nel file di configurazione:
```json
{
  "type": "output",
  "parameters": [
    {"name": "format", "value": "JSON", "type": "string"}
  ]
}
```

Oppure in OOP:
```plaintext
output.
        .format = 'JSON'
```

## Esempi Completi

### Esempio 1: Ricerca Gennaio 2026

```bash
# Usa configurazione predefinita
italoccultcalc preset_jan2026_test.json
```

Output: `occultations_jan2026_test.json`

### Esempio 2: Singolo Asteroide

```bash
# Calcolo per (433) Eros
occult_calc 433 2026-03-15
```

### Esempio 3: Survey Completo

```bash
# Lista asteroidi in file
echo "324" > asteroids.txt    # Bamberga
echo "10" >> asteroids.txt     # Hygiea
echo "704" >> asteroids.txt    # Interamnia

# Ricerca anno completo
ioccultcalc_search asteroids.txt 2026-01-01 2026-12-31 14.0
```

## File e Directory

### Directory dati utente
```
~/.ioccultcalc/
├── ephemerides/          # Effemeridi JPL (DE441, AST17)
├── cache/                # Cache Gaia e elementi orbitali
└── output/               # Output generati
```

### File di configurazione disponibili
```
IOccultCalc/
├── preset_default.json
├── preset_fast_search.json
├── preset_high_precision.json
├── preset_jan2026_test.json
├── preset_output_config.json
└── test_config.oop
```

## Opzioni Avanzate

### Propagatore
Scegli tra:
- **RK4**: Runge-Kutta 4° ordine (veloce)
- **RKF78**: Runge-Kutta-Fehlberg 7(8) (alta precisione)
- **ORBFIT**: Integrazione stile OrbFit

### Perturbazioni
```json
{
  "type": "perturbations",
  "parameters": [
    {"name": "planets", "value": "true", "type": "bool"},
    {"name": "relativity", "value": "true", "type": "bool"},
    {"name": "asteroid_count", "value": "17", "type": "int"}
  ]
}
```

### Output Multipli
```json
{
  "type": "output",
  "parameters": [
    {"name": "format", "value": "JSON,PDF,XML", "type": "string"},
    {"name": "verbosity", "value": "2", "type": "int"}
  ]
}
```

## Troubleshooting

### Problema: "Formato configurazione non supportato"
**Soluzione**: Verifica che il file sia JSON o OOP valido
```bash
# Verifica JSON
python3 -m json.tool config.json

# Test con configurazione default
italoccultcalc preset_default.json
```

### Problema: "Database asteroidi non trovato"
**Soluzione**: Scarica database JPL
```bash
# Il database verrà scaricato automaticamente
# oppure manualmente da:
# https://ssd.jpl.nasa.gov/
```

### Problema: "Effemeridi DE441 non trovate"
**Soluzione**: Le effemeridi vengono scaricate automaticamente al primo uso
Directory: `~/.ioccultcalc/ephemerides/`

## Performance

### Ricerca veloce (1000 asteroidi)
```bash
# Usa preset ottimizzato
italoccultcalc preset_fast_search.json
```
Tempo: ~8 minuti (8 thread)

### Alta precisione
```bash
# Usa preset alta precisione
italoccultcalc preset_high_precision.json
```
Tempo: ~45 minuti (1 thread)

## Documentazione Completa

- **Manuale Scientifico**: `docs/manual/main.pdf` (202 pagine)
- **Riferimento OOP**: `docs/OOP_CONFIG_REFERENCE.md` (tutte le opzioni supportate)
- **Quick Start**: `QUICKSTART.md`
- **GitHub**: https://github.com/manvalan/IOccultCalc
- **Issues**: https://github.com/manvalan/IOccultCalc/issues

## Opzioni Avanzate OOP

Per l'elenco completo di tutte le sezioni e parametri supportati nei file OOP, consulta:
```bash
cat docs/OOP_CONFIG_REFERENCE.md
```

Sezioni disponibili:
- `object` - Configurazione asteroide target
- `propag` - Impostazioni propagazione orbita
- `ephemeris` - Configurazione effemeridi JPL
- `search` - Parametri ricerca occultazioni
- `database` - Filtri database asteroidi
- `filtering` - Filtri qualità e osservabilità
- `scoring` - Sistema priorità
- `performance` - Ottimizzazioni prestazioni
- `perturbations` - Modello perturbazioni
- `output` - Configurazione output
- `observer` - Vincoli località osservatore
- `gaia` - Impostazioni catalogo Gaia
- `validation` - Controllo qualità

---

**IOccultCalc v2.0** - Asteroid Occultation Prediction Software  
Gruppo Astrofili Massesi - IOTA
