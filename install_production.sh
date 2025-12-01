#!/bin/bash
# IOccultCalc - Production Installation Script
# Installa e configura IOccultCalc per uso operativo

set -e

echo "╔══════════════════════════════════════════════════════════╗"
echo "║     IOccultCalc - Production Installation v1.0           ║"
echo "╚══════════════════════════════════════════════════════════╝"
echo ""

# Colori
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Variabili
INSTALL_DIR="${HOME}/.ioccultcalc"
CATALOG_DIR="${HOME}/catalogs"
BUILD_DIR="$(pwd)/build"

# 1. Verifica dipendenze
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "1. Verifica Dipendenze"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

# Check CMake
if ! command -v cmake &> /dev/null; then
    echo -e "${RED}✗ CMake non trovato${NC}"
    echo "  Installa con: brew install cmake"
    exit 1
fi
echo -e "${GREEN}✓ CMake: $(cmake --version | head -1)${NC}"

# Check OpenMP
if [ ! -f "/opt/homebrew/opt/libomp/lib/libomp.dylib" ]; then
    echo -e "${YELLOW}⚠ OpenMP non trovato - installo libomp...${NC}"
    brew install libomp
fi
echo -e "${GREEN}✓ OpenMP: libomp installato${NC}"

# Check gfortran
if ! command -v gfortran &> /dev/null; then
    echo -e "${YELLOW}⚠ gfortran non trovato (opzionale per OrbFit)${NC}"
else
    echo -e "${GREEN}✓ gfortran: $(gfortran --version | head -1)${NC}"
fi

# 2. Crea directory
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "2. Creazione Directory"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

mkdir -p "${INSTALL_DIR}"/{data,ephemerides,output,presets}
mkdir -p "${CATALOG_DIR}"
mkdir -p output

echo -e "${GREEN}✓ Directory create:${NC}"
echo "  - ${INSTALL_DIR}"
echo "  - ${CATALOG_DIR}"

# 3. Build IOccultCalc
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "3. Compilazione IOccultCalc"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

# Update submodules
echo "Aggiornamento submodules..."
git submodule update --init --recursive --remote

# Build
mkdir -p "${BUILD_DIR}"
cd "${BUILD_DIR}"

echo "Configurazione CMake..."
cmake .. -DCMAKE_BUILD_TYPE=Release

echo "Compilazione (usando tutti i core)..."
make -j$(sysctl -n hw.ncpu) italoccultcalc

if [ -f "${BUILD_DIR}/examples/italoccultcalc" ]; then
    echo -e "${GREEN}✓ Compilazione completata${NC}"
else
    echo -e "${RED}✗ Errore compilazione${NC}"
    exit 1
fi

cd ..

# 4. Copia preset operativi
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "4. Configurazione Preset"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

# Copia preset
cp preset_1000ast_jan2026.oop "${INSTALL_DIR}/presets/standard.oop"
echo -e "${GREEN}✓ Preset standard copiato${NC}"

# 5. Download dati
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "5. Download Dati Essenziali"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

# Elementi orbitali MPC (dati reali)
if [ ! -f "${INSTALL_DIR}/data/all_numbered_asteroids.json" ]; then
    echo "Download elementi orbitali MPC (172 MB → ~1.5 GB)..."
    ./download_mpc_data.sh
    if [ -f "${INSTALL_DIR}/data/all_numbered_asteroids.json" ]; then
        echo -e "${GREEN}✓ Elementi orbitali MPC scaricati${NC}"
    else
        echo -e "${YELLOW}⚠ Esegui manualmente: ./download_mpc_data.sh${NC}"
    fi
else
    echo -e "${GREEN}✓ Elementi orbitali MPC già presenti${NC}"
fi

# Ephemeris DE440s (leggero, 32 MB)
if [ ! -f "${INSTALL_DIR}/ephemerides/de440s.bsp" ]; then
    echo "Download ephemeris DE440s (32 MB)..."
    curl -L "https://naif.jpl.nasa.gov/pub/naif/generic_kernels/spk/planets/de440s.bsp" \
         -o "${INSTALL_DIR}/ephemerides/de440s.bsp"
    echo -e "${GREEN}✓ DE440s scaricato${NC}"
else
    echo -e "${GREEN}✓ DE440s già presente${NC}"
fi

# Asteroid ephemeris (opzionale)
echo ""
echo -e "${YELLOW}Ephemeris asteroidi (opzionale):${NC}"
echo "  codes_300ast_20100725.bsp (9 MB)"
echo "  Download: https://naif.jpl.nasa.gov/pub/naif/generic_kernels/spk/asteroids/"

# 6. Catalogo Gaia
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "6. Catalogo Gaia (CRITICO)"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

if [ -f "${CATALOG_DIR}/gaia_mag18.cat.gz" ]; then
    SIZE=$(du -h "${CATALOG_DIR}/gaia_mag18.cat.gz" | cut -f1)
    echo -e "${GREEN}✓ Catalogo Mag18 trovato (${SIZE})${NC}"
else
    echo -e "${RED}✗ Catalogo Mag18 NON trovato${NC}"
    echo ""
    echo "Il catalogo Gaia Mag18 (9 GB) è ESSENZIALE per IOccultCalc."
    echo ""
    echo "Download automatico:"
    echo "  ./download_gaia_cache.sh"
    echo ""
    echo "Oppure manualmente da:"
    echo "  https://github.com/manvalan/IOC_GaiaLib/releases"
    echo ""
    echo -e "${YELLOW}Posizione: ${CATALOG_DIR}/gaia_mag18.cat.gz${NC}"
fi

# 7. Crea script wrapper
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "7. Creazione Script Launcher"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

cat > italoccultcalc.sh << 'LAUNCHER'
#!/bin/bash
# IOccultCalc Launcher Script

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EXEC="${SCRIPT_DIR}/build/examples/italoccultcalc"

if [ ! -f "$EXEC" ]; then
    echo "✗ Errore: italoccultcalc non trovato"
    echo "  Esegui: ./install_production.sh"
    exit 1
fi

# Check catalogo
if [ ! -f "${HOME}/catalogs/gaia_mag18.cat.gz" ]; then
    echo "⚠ WARNING: Catalogo Gaia Mag18 non trovato"
    echo "  Download: ./download_gaia_cache.sh"
fi

# Esegui
"$EXEC" "$@"
LAUNCHER

chmod +x italoccultcalc.sh
echo -e "${GREEN}✓ Launcher creato: ./italoccultcalc.sh${NC}"

# 8. Test veloce
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "8. Test Installazione"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

if "${BUILD_DIR}/examples/italoccultcalc" --help > /dev/null 2>&1; then
    echo -e "${GREEN}✓ Test esecuzione: OK${NC}"
else
    echo -e "${RED}✗ Test esecuzione: FAILED${NC}"
fi

# 9. Summary
echo ""
echo "╔══════════════════════════════════════════════════════════╗"
echo "║           ✓ INSTALLAZIONE COMPLETATA                    ║"
echo "╚══════════════════════════════════════════════════════════╝"
echo ""
echo "📁 Directory:"
echo "   Config:     ${INSTALL_DIR}"
echo "   Catalogo:   ${CATALOG_DIR}"
echo "   Output:     $(pwd)/output"
echo ""
echo "🚀 Esecuzione:"
echo "   ./italoccultcalc.sh <preset.oop>"
echo "   ./italoccultcalc.sh ${INSTALL_DIR}/presets/standard.oop"
echo ""
echo "📚 Documentazione:"
echo "   README.md"
echo "   QUICKSTART.md"
echo ""

# Check completezza
MISSING=0
[ ! -f "${CATALOG_DIR}/gaia_mag18.cat.gz" ] && MISSING=$((MISSING+1))
[ ! -f "${INSTALL_DIR}/data/all_numbered_asteroids.json" ] && MISSING=$((MISSING+1))

if [ $MISSING -gt 0 ]; then
    echo -e "${YELLOW}⚠ Completare setup:${NC}"
    [ ! -f "${CATALOG_DIR}/gaia_mag18.cat.gz" ] && echo "   - Download catalogo Gaia Mag18 (9 GB)"
    [ ! -f "${INSTALL_DIR}/data/all_numbered_asteroids.json" ] && echo "   - Preparare elementi orbitali asteroidi"
    echo ""
fi

echo "Per assistenza: https://github.com/manvalan/IOccultCalc"
echo ""
