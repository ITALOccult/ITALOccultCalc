# IOccultCalc - Project Structure

## Overview
Libreria C++ completa per il calcolo delle previsioni di occultazioni asteroidali.

## Struttura Directory

```
IOccultCalc/
│
├── CMakeLists.txt              # Build configuration principale
├── LICENSE                     # MIT License
├── README.md                   # Documentazione principale
├── QUICKSTART.md              # Guida rapida
├── .gitignore                 # File git ignore
│
├── include/ioccultcalc/       # Header files pubblici
│   ├── types.h                # Tipi base (Vector3D, Coordinate, JulianDate)
│   ├── time_utils.h           # Utilità temporali e conversioni
│   ├── coordinates.h          # Trasformazioni coordinate
│   ├── orbital_elements.h     # Elementi orbitali equinoziali
│   ├── astdys_client.h        # Client per AstDyS2
│   ├── gaia_client.h          # Client per Gaia DR3 TAP
│   ├── ephemeris.h            # Calcolo effemeridi asteroidi
│   ├── occultation_predictor.h # Predittore occultazioni
│   └── kml_exporter.h         # Export KML per GoogleEarth
│
├── src/                       # Implementazioni
│   ├── time_utils.cpp
│   ├── coordinates.cpp
│   ├── orbital_elements.cpp
│   ├── astdys_client.cpp
│   ├── gaia_client.cpp
│   ├── ephemeris.cpp
│   ├── occultation_predictor.cpp
│   └── kml_exporter.cpp
│
├── examples/                  # Programmi di esempio
│   ├── CMakeLists.txt
│   ├── basic_usage.cpp        # Esempio base: singolo asteroide
│   ├── search_occultations.cpp # Ricerca multipli asteroidi
│   └── asteroids.txt          # Lista asteroidi esempio
│
├── tests/                     # Unit tests
│   ├── CMakeLists.txt
│   └── test_time_utils.cpp    # Test utilità temporali
│
└── docs/                      # Documentazione
    └── GUIDE.md               # Guida dettagliata

```

## Moduli

### 1. Core Types (`types.h`)
- `Vector3D`: Vettore 3D con operazioni
- `EquatorialCoordinates`: RA, Dec, distanza
- `GeographicCoordinates`: Lon, Lat, Alt
- `JulianDate`: Data giuliana

### 2. Time Utils (`time_utils.h/cpp`)
- Conversioni ISO ↔ JD
- GMST, LST
- Calendario ↔ JD

### 3. Coordinates (`coordinates.h/cpp`)
- Equatoriali ↔ Cartesiane
- Geografiche ↔ ECEF
- Separazione angolare
- Precessione, aberrazione
- Angolo di posizione

### 4. Orbital Elements (`orbital_elements.h/cpp`)
- Elementi equinoziali
- Conversione Kepleriano ↔ Equinoziale

### 5. AstDyS Client (`astdys_client.h/cpp`)
- Download elementi orbitali da AstDyS2
- Parsing formato .eq
- Batch download
- Search by name

### 6. Gaia Client (`gaia_client.h/cpp`)
- Query TAP/ADQL su Gaia DR3
- Query cono, regione, path
- Parsing VOTable
- Proper motion propagation

### 7. Ephemeris (`ephemeris.h/cpp`)
- Propagazione orbitale elementi equinoziali
- Risoluzione equazione di Keplero
- Posizione Terra/Sole
- Calcolo magnitudine

### 8. Occultation Predictor (`occultation_predictor.h/cpp`)
- Ricerca occultazioni in intervallo temporale
- Calcolo closest approach
- Geometria occultazione
- Shadow path sulla Terra
- Calcolo probabilità

### 9. KML Exporter (`kml_exporter.h/cpp`)
- Generazione file KML/KMZ
- Tracce occultazione
- Bande incertezza
- Timestamp markers
- Stili personalizzabili

## Dipendenze

### Required
- **CMake** ≥ 3.15
- **C++17** compiler (GCC, Clang, MSVC)
- **libcurl**: HTTP client per AstDyS e Gaia
- **libxml2**: Parsing VOTable da Gaia

### Optional
- **Google Test**: Per unit tests estesi
- **zlib**: Per compressione KMZ

## Build System

### CMake Targets
- `ioccultcalc`: Libreria statica principale
- `example_basic`: Esempio uso base
- `example_search`: Ricerca multipli asteroidi
- `test_time_utils`: Test suite

### Build Options
- `BUILD_TESTS`: Compila tests (default: ON)
- `BUILD_EXAMPLES`: Compila esempi (default: ON)

## Algoritmi Implementati

### Propagazione Orbitale
- Elementi equinoziali (non singolari)
- Newton-Raphson per equazione Keplero
- Trasformazioni orbitale → equatoriale

### Effemeridi Terra
- VSOP87 ridotta per posizione Terra
- Velocità da differenze finite

### Ricerca Occultazioni
- Grid search con step temporale
- Golden section per closest approach
- Distribuzione gaussiana probabilità

### Shadow Path
- Proiezione ombra su superficie terrestre
- Coordinate ECEF → geografiche WGS84
- Durata da geometria e velocità relativa

## Output Files

### KML Format
```xml
<kml>
  <Document>
    <Folder>
      <Placemark>  <!-- Central path -->
        <LineString>
          <coordinates>lon,lat,alt ...</coordinates>
        </LineString>
      </Placemark>
      <Placemark>  <!-- Uncertainty bands -->
        ...
      </Placemark>
    </Folder>
  </Document>
</kml>
```

## Performance Notes

### Typical Times (macOS M1)
- Download orbital elements: ~1-2s
- Gaia query (1°, mag<12): ~5-10s
- Compute 1 month ephemeris: <1s
- Full occultation prediction: ~2-5s
- KML export: <0.1s

### Memory Usage
- Base library: ~1MB
- 1000 stars cached: ~100KB
- Ephemeris table (30 days): ~10KB

## Future Enhancements

### Planned
- [ ] Local Gaia cache database
- [ ] JPL Horizons integration
- [ ] Full VSOP87 implementation
- [ ] Parallel query processing
- [ ] GUI application

### Consider
- [ ] Python bindings (pybind11)
- [ ] Web service API
- [ ] Real-time event alerts
- [ ] Mobile app support

## References

### Data Sources
- **AstDyS**: https://newton.spacedys.com/astdys2/
- **Gaia Archive**: https://gea.esac.esa.int/
- **JPL Horizons**: https://ssd.jpl.nasa.gov/horizons/

### Algorithms
- Meeus, J. "Astronomical Algorithms" (1998)
- Broucke & Cefola, "On the equinoctial orbit elements" (1972)
- IAU SOFA library documentation

### Standards
- VOTable 1.4: https://www.ivoa.net/documents/VOTable/
- KML 2.2: https://developers.google.com/kml/documentation/

## Contact & Contribution

- **Repository**: https://github.com/manvalan/IOccultCalc
- **Issues**: https://github.com/manvalan/IOccultCalc/issues
- **License**: MIT

Contributions welcome! See GitHub for issues and feature requests.
