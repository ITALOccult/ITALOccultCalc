# Quick Start Guide

## Installazione Rapida (macOS)

```bash
# 1. Installa dipendenze
brew install cmake curl libxml2

# 2. Compila
cd IOccultCalc
mkdir build && cd build
cmake ..
make -j4

# 3. Esegui esempio
cd ..
./build/examples/example_basic 433
```

## Installazione Rapida (Linux)

```bash
# 1. Installa dipendenze
sudo apt-get install cmake g++ libcurl4-openssl-dev libxml2-dev

# 2. Compila
cd IOccultCalc
mkdir build && cd build
cmake ..
make -j4

# 3. Esegui esempio
cd ..
./build/examples/example_basic 433
```

## Primo Utilizzo

### Esempio Minimo

```cpp
#include <ioccultcalc/occultation_predictor.h>
#include <ioccultcalc/kml_exporter.h>
#include <ioccultcalc/time_utils.h>

using namespace ioccultcalc;

int main() {
    // Crea predittore
    OccultationPredictor predictor;
    
    // Carica asteroide (es. 433 Eros)
    predictor.loadAsteroidFromAstDyS("433");
    predictor.setAsteroidDiameter(16.8); // km
    
    // Cerca occultazioni
    JulianDate start = TimeUtils::isoToJD("2026-01-01");
    JulianDate end = TimeUtils::isoToJD("2026-12-31");
    
    auto occultations = predictor.findOccultations(
        start, end,
        12.0,  // mag limite
        0.05,  // raggio ricerca
        0.01   // probabilità minima
    );
    
    // Esporta in KML
    KMLExporter exporter;
    for (size_t i = 0; i < occultations.size(); ++i) {
        exporter.exportToKML(occultations[i], 
            "occultation_" + std::to_string(i) + ".kml");
    }
    
    return 0;
}
```

## Programmi Esempio

### example_basic
Ricerca occultazioni per un singolo asteroide:
```bash
./build/examples/example_basic [numero_asteroide]
```

### example_search
Ricerca occultazioni per multipli asteroidi:
```bash
./build/examples/example_search [file_lista] [data_inizio] [data_fine] [mag_limite]
```

Esempio:
```bash
./build/examples/example_search examples/asteroids.txt 2026-01-01 2026-12-31 14.0
```

## Output

I programmi generano file KML che possono essere aperti in:
- **Google Earth**: File → Apri → seleziona file .kml
- **Google Earth Web**: https://earth.google.com/web/ → Importa KML

### Cosa Visualizzare nel KML

- **Linea centrale (rossa)**: Traccia centrale dell'occultazione
- **Bande blu**: Limiti di incertezza (1-sigma)
- **Marker temporali**: Timestamp lungo il percorso
- **Informazioni**: Click su linea per vedere dettagli evento

## Parametri Comuni

### Magnitudine Limite
- **10-12**: Stelle molto brillanti (poche occultazioni)
- **12-14**: Buon compromesso
- **14-16**: Molte occultazioni (query più lente)

### Raggio Ricerca
- **0.01-0.05°**: Ricerca vicina (eventi molto probabili)
- **0.05-0.1°**: Standard
- **0.1-0.5°**: Ampia (trova più eventi)

### Probabilità Minima
- **0.1-1.0**: Solo eventi molto probabili
- **0.01-0.1**: Compromesso
- **0.001-0.01**: Tutti i possibili eventi

## Risoluzione Problemi

### Errore: "curl not found"
```bash
# macOS
brew install curl

# Linux
sudo apt-get install libcurl4-openssl-dev
```

### Errore: "xml2 not found"
```bash
# macOS
brew install libxml2

# Linux
sudo apt-get install libxml2-dev
```

### Query Gaia molto lente
- Riduci magnitudine limite
- Riduci raggio di ricerca
- Riduci intervallo temporale

### Nessuna occultazione trovata
- Aumenta raggio ricerca a 0.1-0.2°
- Aumenta magnitudine limite a 14-16
- Verifica che l'asteroide esista su AstDyS

## Prossimi Passi

1. Leggi la documentazione completa: `docs/GUIDE.md`
2. Esplora gli esempi in `examples/`
3. Personalizza i parametri per le tue necessità
4. Contribuisci su GitHub!

## Link Utili

- **AstDyS**: https://newton.spacedys.com/astdys2/
- **Gaia Archive**: https://gea.esac.esa.int/archive/
- **Google Earth**: https://earth.google.com/
- **Repository**: https://github.com/manvalan/IOccultCalc

## Supporto

Per problemi o domande, apri una issue su GitHub.
