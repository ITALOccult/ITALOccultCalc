#!/bin/bash

# setup_ioccultcalc.sh
# Automated installer for IOccultCalc environment

set -e

echo "--------------------------------------------------"
echo "ITALOccultCalc Environment Setup"
echo "--------------------------------------------------"

# 1. Create directory structure
echo "[1/4] Creating directory structure..."
IOCC_PATH="$HOME/.ioccultcalc"
mkdir -p "$IOCC_PATH/ephemerides"
mkdir -p "$IOCC_PATH/data"
mkdir -p "$IOCC_PATH/config"
mkdir -p "$IOCC_PATH/logs"
echo "  Created $IOCC_PATH"

# 2. Download Ephemerides (DE441)
echo "[2/4] Verifying ephemerides..."
DE441_PATH="$IOCC_PATH/ephemerides/de441_part-2.bsp"
if [ ! -f "$DE441_PATH" ]; then
    echo "  Downloading DE441 (this might take a while)..."
    # Placeholder URL - update with actual source if different
    # curl -L "https://naif.jpl.nasa.gov/pub/naif/generic_kernels/spk/planets/de441.bsp" -o "$DE441_PATH"
    echo "  ! Note: de441_part-2.bsp not found. Please ensure it is placed in $IOCC_PATH/ephemerides/"
else
    echo "  DE441 already present."
fi

# 3. Initialize Asteroid Database
echo "[3/4] Initializing asteroid database..."
ALLNUM_JSON="$IOCC_PATH/data/all_numbered_asteroids.json"
if [ ! -f "$ALLNUM_JSON" ]; then
    echo "  Downloading all_numbered_asteroids.json..."
    # Placeholder for database initialization
    # curl -L "https://example.com/all_numbered_asteroids.json" -o "$ALLNUM_JSON"
    echo "  ! Note: Database file should be initialized or updated via italoccultcalc --update-db"
fi

# 4. Gaia Catalog Verification
echo "[4/4] Verifying Gaia Catalog path..."
GAIA_PATH="$HOME/.catalog/gaia_mag18_v2_multifile"
if [ -d "$GAIA_PATH" ]; then
    echo "  Gaia catalog found at $GAIA_PATH"
else
    echo "  ! WARNING: Gaia catalog not found at expected path: $GAIA_PATH"
fi

echo "--------------------------------------------------"
echo "Setup completed (with some warnings/placeholders)."
echo "You can now run italoccultcalc."
echo "--------------------------------------------------"
