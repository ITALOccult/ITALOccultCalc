# IOccultCalc - Guida Dettagliata

## Indice

1. [Installazione](#installazione)
2. [Architettura](#architettura)
3. [Utilizzo Avanzato](#utilizzo-avanzato)
4. [API Reference](#api-reference)
5. [Algoritmi](#algoritmi)

## Installazione

### macOS

```bash
# Installa dipendenze
brew install cmake curl libxml2

# Clone repository
git clone https://github.com/manvalan/IOccultCalc.git
cd IOccultCalc

# Build
mkdir build && cd build
cmake ..
make -j4

# Test
make test

# Installa (opzionale)
sudo make install
```

### Linux (Ubuntu/Debian)

```bash
sudo apt-get update
sudo apt-get install cmake g++ libcurl4-openssl-dev libxml2-dev

git clone https://github.com/manvalan/IOccultCalc.git
cd IOccultCalc

mkdir build && cd build
cmake ..
make -j4
```

## Architettura

La libreria è organizzata in moduli:

```
IOccultCalc/
├── include/ioccultcalc/
│   ├── types.h              # Tipi base (vettori, coordinate, JD)
│   ├── time_utils.h         # Utilità temporali
│   ├── coordinates.h        # Trasformazioni coordinate
│   ├── orbital_elements.h   # Elementi orbitali equinoziali
│   ├── astdys_client.h      # Client AstDyS2
│   ├── gaia_client.h        # Client Gaia DR3
│   ├── ephemeris.h          # Calcolo effemeridi
│   ├── occultation_predictor.h  # Predittore occultazioni
│   └── kml_exporter.h       # Export KML/GoogleEarth
└── src/                     # Implementazioni
```

### Flusso Tipico

1. **Caricamento Dati Asteroide**: `AstDysClient` scarica elementi orbitali
2. **Calcolo Effemeridi**: `Ephemeris` propaga l'orbita
3. **Query Stelle**: `GaiaClient` scarica stelle nella regione
4. **Predizione**: `OccultationPredictor` calcola geometria e shadow path
5. **Export**: `KMLExporter` genera file per GoogleEarth

## Utilizzo Avanzato

### Configurazione Personalizzata

```cpp
#include <ioccultcalc/occultation_predictor.h>

// Crea predittore
OccultationPredictor predictor;

// Carica elementi da AstDyS
predictor.loadAsteroidFromAstDyS("433");

// Configura parametri fisici
predictor.setAsteroidDiameter(16.8);  // km
predictor.setOrbitalUncertainty(50.0); // 1-sigma in km

// Ricerca con parametri customizzati
auto events = predictor.findOccultations(
    startDate,
    endDate,
    15.0,   // mag limite (stelle più deboli)
    0.2,    // raggio ricerca più ampio
    0.001   // probabilità minima più bassa
);
```

### Query Gaia Personalizzate

```cpp
#include <ioccultcalc/gaia_client.h>

GaiaClient gaia;

// Query cono
auto stars = gaia.queryCone(
    180.0,  // RA in gradi
    30.0,   // Dec in gradi
    1.0,    // raggio in gradi
    16.0    // mag limite
);

// Query lungo un path
std::vector<EquatorialCoordinates> path;
// ... riempi path ...
auto starsOnPath = gaia.queryAlongPath(path, 0.5, 14.0);

// Query stella singola
GaiaStar star = gaia.queryStar("4295806720");
```

### Export KML Personalizzato

```cpp
#include <ioccultcalc/kml_exporter.h>

KMLExporter exporter;

// Configura opzioni
KMLExporter::ExportOptions opts;
opts.showUncertaintyBands = true;
opts.showTimestamps = true;
opts.showCenterline = true;
opts.pathWidthKm = 200.0;
opts.centerlineColor = "ffff0000";  // Blu in AABBGGRR
opts.pathColor = "7f00ff00";        // Verde semi-trasparente
opts.uncertaintyColor = "7f0000ff"; // Rosso semi-trasparente

exporter.setExportOptions(opts);

// Export
exporter.exportToKML(event, "output.kml");
```

## API Reference

### TimeUtils

```cpp
// Conversioni temporali
JulianDate isoToJD(const std::string& isoDate);
std::string jdToISO(const JulianDate& jd);
JulianDate now();

// Tempo siderale
double gmst(const JulianDate& jd);  // Greenwich Mean Sidereal Time
double lst(const JulianDate& jd, double longitude);  // Local Sidereal Time
```

### Coordinates

```cpp
// Conversioni coordinate
Vector3D equatorialToCartesian(const EquatorialCoordinates& eq);
EquatorialCoordinates cartesianToEquatorial(const Vector3D& vec);

Vector3D geographicToECEF(const GeographicCoordinates& geo);
GeographicCoordinates ecefToGeographic(const Vector3D& ecef);

// Calcoli sferici
double angularSeparation(const EquatorialCoordinates& pos1,
                        const EquatorialCoordinates& pos2);
double positionAngle(const EquatorialCoordinates& from,
                    const EquatorialCoordinates& to);

// Correzioni astrometriche
EquatorialCoordinates applyPrecession(...);
EquatorialCoordinates applyAberration(...);
```

### Ephemeris

```cpp
// Setup
Ephemeris eph(elements);
eph.setElements(elements);

// Calcolo
EphemerisData compute(const JulianDate& jd);
std::vector<EphemerisData> computeRange(startJD, endJD, stepDays);

// Utilità statiche
static Vector3D getEarthPosition(const JulianDate& jd);
static Vector3D getSunPosition(const JulianDate& jd);
```

## Algoritmi

### Elementi Orbitali Equinoziali

La libreria usa elementi equinoziali perché:
- Non hanno singolarità per orbite circolari/equatoriali
- Migliori per integrazione numerica
- Diretta compatibilità con AstDyS

Conversione Kepleriano → Equinoziale:
```
h = e·sin(ω+Ω)
k = e·cos(ω+Ω)
p = tan(i/2)·sin(Ω)
q = tan(i/2)·cos(Ω)
λ = M + ω + Ω
```

### Propagazione Orbitale

1. Calcola mean motion: `n = k/√(a³)`
2. Propaga mean longitude: `λ(t) = λ₀ + n·Δt`
3. Risolvi equazione di Keplero per E
4. Calcola posizione nel piano orbitale
5. Ruota al sistema equatoriale usando elementi equinoziali

### Calcolo Shadow Path

1. Per ogni istante temporale:
   - Calcola posizione asteroide (geocentrica)
   - Calcola direzione stella
2. Proietta ombra asteroide sulla superficie terrestre
3. Trasforma in coordinate geografiche
4. Calcola durata occultazione da geometria e velocità relativa

### Probabilità Occultazione

Usa distribuzione gaussiana dell'errore orbitale:

```
P = 0.5·(1 + erf((r_asteroid - d_separation)/(σ·√2)))
```

dove:
- `r_asteroid`: raggio angolare asteroide
- `d_separation`: separazione angolare al closest approach
- `σ`: incertezza orbitale proiettata

## Performance

### Ottimizzazioni Consigliate

1. **Cache Gaia**: Salva stelle già scaricate
2. **Intervalli brevi**: Ricerche di 1-2 mesi più veloci
3. **Magnitudine limite**: Stelle più brillanti = meno dati
4. **Raggio ricerca**: Ridurre per ricerche preliminari

### Tempi Tipici

- Download elementi orbitali: 1-2 secondi
- Query Gaia (1°, mag<12): 5-10 secondi
- Calcolo 1 mese effemeridi: <1 secondo
- Predizione 1 occultazione completa: 2-5 secondi

## Troubleshooting

### Errori Comuni

**"HTTP request failed"**: Verifica connessione internet e firewall

**"Star not found"**: Source ID Gaia non valido

**"Failed to parse VOTable"**: Timeout query Gaia (aumenta timeout)

**Nessuna occultazione trovata**: 
- Aumenta raggio ricerca
- Aumenta magnitudine limite
- Verifica intervallo temporale

## Contribuire

Pull requests benvenute! Aree di miglioramento:
- Integrazione JPL Horizons per effemeridi precise
- Cache database locale Gaia
- Supporto VSOP87 completo
- Parallasse e aberrazione complete
- GUI

## Licenza

MIT License - vedi LICENSE file
