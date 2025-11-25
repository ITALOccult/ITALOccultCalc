#!/bin/bash
# IOccultCalc Installation Script
# Installa gli eseguibili principali in /usr/local/bin

set -e

echo "================================================"
echo "IOccultCalc v2.0 - Installation Script"
echo "================================================"
echo ""

# Check if build directory exists
if [ ! -d "build" ]; then
    echo "❌ Error: build directory not found"
    echo "Please run: cmake -S . -B build && cmake --build build"
    exit 1
fi

# Check if executables exist
EXECUTABLES=(
    "build/tools/occult_calc"
    "build/examples/italoccultcalc"
    "build/examples/ioccultcalc_search"
)

for exe in "${EXECUTABLES[@]}"; do
    if [ ! -f "$exe" ]; then
        echo "❌ Error: $exe not found"
        echo "Please build the project first"
        exit 1
    fi
done

# Install executables
echo "Installing executables to /usr/local/bin..."
echo ""

sudo cp build/tools/occult_calc /usr/local/bin/
sudo cp build/examples/italoccultcalc /usr/local/bin/
sudo cp build/examples/ioccultcalc_search /usr/local/bin/

# Make executable
sudo chmod +x /usr/local/bin/occult_calc
sudo chmod +x /usr/local/bin/italoccultcalc
sudo chmod +x /usr/local/bin/ioccultcalc_search

echo "✅ Installation complete!"
echo ""
echo "Installed executables:"
echo "  - occult_calc          (Basic occultation calculator)"
echo "  - italoccultcalc       (ITALOccultCalc complete pipeline)"
echo "  - ioccultcalc_search   (Occultation search tool)"
echo ""
echo "Verification:"
ls -lh /usr/local/bin/*occult* 2>/dev/null
echo ""
echo "You can now run these commands from anywhere:"
echo "  $ occult_calc --help"
echo "  $ italoccultcalc --help"
echo "  $ ioccultcalc_search --help"
echo ""
echo "================================================"
