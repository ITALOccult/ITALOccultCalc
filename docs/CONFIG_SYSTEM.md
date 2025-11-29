# Sistema di Gestione Configurazione IOccultCalc

## Panoramica

Il sistema di configurazione di IOccultCalc è ispirato ai file `.oop` di OrbFit e fornisce un modo flessibile per gestire tutti i parametri del software. Supporta sia il formato testuale in stile OrbFit che JSON per import/export.

## Caratteristiche Principali

- **Struttura a Sezioni**: Organizzazione gerarchica dei parametri (object, propagation, ephemeris, etc.)
- **Type-Safe**: Conversione automatica tra tipi (string, int, double, bool, array)
- **Validazione**: Controllo automatico della validità della configurazione
- **Interoperabilità**: Conversione bidirezionale tra formato .oop (OrbFit) e JSON
- **Builder Pattern**: Creazione programmatica di configurazioni con API fluente
- **Preset**: Configurazioni predefinite (default, high_precision, fast_search)
- **Metadata**: Supporto per metadati (autore, versione, data, etc.)

## Sezioni Disponibili

### OBJECT
Configurazione dell'oggetto target (asteroide):
```
object.
        .name = '433 Eros'
        .id = '433'
        .diameter = 16.84
        .H = 10.43
        .source = 'AstDyS'
```

### PROPAGATION
Impostazioni di propagazione orbitale:
```
propag.
        .type = 'RK4'              ! Integratore: RK4, RA15, ORBFIT
        .step_size = 0.05          ! Passo di integrazione (giorni)
        .start_jd = 2461041.0
        .end_jd = 2461405.0
        .tolerance = 1.0e-12
```

### EPHEMERIS
Configurazione effemeridi JPL:
```
ephemeris.
        .jpl_version = 'DE441'
        .de_file = '~/.ioccultcalc/ephemerides/linux_p1550p2650.441'
        .ast17_file = '~/.ioccultcalc/ephemerides/codes_300ast_20100725.bsp'
        .auto_download = .TRUE.
```

### PERTURBATIONS
Modello di perturbazioni (compatibile con OrbFit):
```
perturbations.
        .planets = .TRUE.          ! 8 pianeti maggiori
        .asteroid_count = 17       ! AST17: 17 asteroidi massivi
        .relativity = .TRUE.       ! Correzioni post-Newtoniane
        .earth_moon = .TRUE.       ! Sistema Terra-Luna separato
```

### SEARCH
Parametri di ricerca occultazioni:
```
search.
        .start_jd = 2461041.0
        .end_jd = 2461405.0
        .step_days = 0.5           ! Intervallo di campionamento
        .max_separation = 0.1      ! Separazione angolare massima (gradi)
        .mag_limit = 14.0          ! Limite di magnitudine
```

### STAR
Configurazione catalogo stellare:
```
star.
        .catalog = 'Gaia'
        .gaia_release = 'DR3'
        .proper_motion = .TRUE.
        .parallax = .TRUE.
```

### OUTPUT
Opzioni di output:
```
output.
        .format = 'JSON'           ! JSON, KML, TEXT, CSV
        .file = 'predictions.json'
        .verbosity = 1             ! 0=quiet, 1=normal, 2=verbose
        .coordinate_frame = 'J2000'
```

### IERS
Parametri di orientamento terrestre:
```
IERS.
        .use_iers = .TRUE.
        .extrapolation = .TRUE.
        .update_interval = 7
```

### OPERATIONS
Operazioni da eseguire:
```
operations.
        .download_elements = .TRUE.
        .propagate = .TRUE.
        .search_occultations = .TRUE.
        .export_json = .TRUE.
```

## Uso Programmatico

### Builder Pattern (Consigliato)

```cpp
#include <ioccultcalc/config_manager.h>

using namespace ioccultcalc;

// Crea configurazione con il builder
ConfigManager config = ConfigBuilder()
    .setObject("433 Eros", "433")
    .setPropagator("RK4")
    .setStepSize(0.05)
    .setTimeSpan(2461041.0, 2461405.0)
    .enablePlanets(true)
    .enableAsteroids(17)
    .enableRelativity(true)
    .setJplEphemeris("DE441")
    .setSearchInterval(2461041.0, 2461405.0, 0.5)
    .setMagnitudeLimit(14.0)
    .setOutputFormat("JSON")
    .setOutputFile("results.json")
    .setVerbosity(1)
    .build();

// Aggiungi metadata
config.setMetadata("author", "Michele Bigi");
config.setMetadata("version", "1.0");

// Salva in JSON
config.saveToJson("my_config.json");

// Salva in formato OrbFit .oop
config.saveToOop("my_config.oop");
```

### Accesso ai Parametri

```cpp
// Accesso diretto tramite sezione
auto propagSection = config.getSection(ConfigSection::PROPAGATION);
if (propagSection) {
    auto stepSize = propagSection->getParameter("step_size");
    if (stepSize) {
        double step = stepSize->asDouble();
        std::cout << "Step size: " << step << " days\n";
    }
}

// Ricerca parametro in tutte le sezioni
auto param = config.findParameter("step_size");
if (param) {
    std::cout << "Found: " << param->asDouble() << "\n";
}
```

### Modifica Parametri

```cpp
// Ottieni sezione
auto section = config.getSection(ConfigSection::PROPAGATION);
if (section) {
    ConfigSectionData updated = *section;
    
    // Modifica parametro
    updated.setParameter("step_size", 0.01, "Higher precision");
    
    // Aggiorna nella configurazione
    config.addSection(updated);
}
```

### Validazione

```cpp
std::vector<std::string> errors;
if (!config.validate(errors)) {
    std::cerr << "Configuration errors:\n";
    for (const auto& error : errors) {
        std::cerr << "  - " << error << "\n";
    }
}
```

## Caricamento Configurazioni

### Da File JSON

```cpp
ConfigManager config;
config.loadFromJson("config.json");
```

### Da File .oop (OrbFit)

```cpp
ConfigManager config;
config.loadFromOop("config.oop");
```

### Preset Predefiniti

```cpp
// Configurazione standard
auto config1 = ConfigManager::createDefault();

// Alta precisione (RA15, step piccolo, relatività)
auto config2 = ConfigManager::createHighPrecision();

// Ricerca veloce (step grande, no asteroidi)
auto config3 = ConfigManager::createFastSearch();
```

## Formato JSON

Esempio di file JSON:

```json
{
  "metadata": {
    "created_by": "IOccultCalc",
    "version": "1.0",
    "date": "2025-11-22"
  },
  "sections": [
    {
      "type": "object",
      "parameters": [
        {
          "name": "name",
          "value": "433 Eros",
          "type": "string",
          "comment": "Target asteroid name"
        }
      ]
    }
  ]
}
```

## Formato .oop (OrbFit)

Esempio di file .oop:

```
! IOccultCalc Configuration File

object.
        .name = '433 Eros'      ! Target asteroid name
        .id = '433'             ! Asteroid number

propag.
        .type = 'RK4'           ! Integrator type
        .step_size = 0.05       ! Step size (days)
```

## Esempi Completi

Vedi:
- `examples/test_config_manager.cpp` - Test completo del sistema
- `examples/config_templates/high_precision.oop` - Template .oop
- `examples/config_templates/high_precision.json` - Template JSON

## Integrazione con ioccultcalc_search

Il programma principale può essere modificato per accettare file di configurazione:

```bash
# Usa configurazione JSON
./ioccultcalc_search --config my_config.json

# Usa configurazione .oop
./ioccultcalc_search --config my_config.oop

# Override parametri specifici
./ioccultcalc_search --config my_config.json --step-size 0.01
```

## Conversione tra Formati

```cpp
// Carica da .oop, salva in JSON
ConfigManager config;
config.loadFromOop("orbfit_config.oop");
config.saveToJson("ioccultcalc_config.json");

// Carica da JSON, salva in .oop
config.loadFromJson("config.json");
config.saveToOop("config.oop");
```

## Vantaggi

1. **Compatibilità OrbFit**: Usa gli stessi file .oop di OrbFit
2. **JSON Moderno**: Formato standard per interoperabilità
3. **Type Safety**: Conversione automatica dei tipi con validazione
4. **Riutilizzabilità**: Salva configurazioni per run futuri
5. **Documentazione**: Commenti inline nei file .oop
6. **Validazione**: Controllo automatico di correttezza
7. **Preset**: Configurazioni ottimizzate pronte all'uso

## Testing

Compila ed esegui il test:

```bash
cd /path/to/IOccultCalc
./build.sh
./build/examples/test_config_manager
```

Output atteso:
- Creazione configurazione con builder
- Export in JSON e .oop
- Import da JSON
- Validazione
- Test preset
- Conversioni di tipo
