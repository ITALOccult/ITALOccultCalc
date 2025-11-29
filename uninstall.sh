#!/bin/bash

# ============================================================================
# IOccultCalc - Uninstallation Script
# ============================================================================
# Remove IOccultCalc from the system
# Usage: ./uninstall.sh [--prefix PATH]
# ============================================================================

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Default installation prefix
INSTALL_PREFIX="/usr/local"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --prefix)
            INSTALL_PREFIX="$2"
            shift 2
            ;;
        --help)
            echo "Usage: ./uninstall.sh [--prefix PATH]"
            echo "Remove IOccultCalc from the system"
            echo ""
            echo "Options:"
            echo "  --prefix PATH    Installation prefix (default: /usr/local)"
            echo "  --help           Show this help"
            exit 0
            ;;
        *)
            echo -e "${RED}Unknown option: $1${NC}"
            exit 1
            ;;
    esac
done

echo -e "${BLUE}"
echo "╔═══════════════════════════════════════════════════════════════╗"
echo "║        IOccultCalc - Uninstallation Script                    ║"
echo "╚═══════════════════════════════════════════════════════════════╝"
echo -e "${NC}"

echo -e "${YELLOW}Uninstalling from: $INSTALL_PREFIX${NC}"
echo ""

# Check if we need sudo
SUDO=""
if [ ! -w "$INSTALL_PREFIX" ]; then
    echo "Administrator privileges required"
    SUDO="sudo"
fi

# Confirm
read -p "Are you sure you want to uninstall IOccultCalc? (y/N) " -n 1 -r
echo
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "Uninstallation cancelled"
    exit 0
fi

echo ""
echo "Removing IOccultCalc files..."

removed_count=0

# Remove executables
echo -n "  Removing executables... "
for exe in occult_calc italoccultcalc ioccultcalc_search; do
    if [ -f "$INSTALL_PREFIX/bin/$exe" ]; then
        $SUDO rm -f "$INSTALL_PREFIX/bin/$exe"
        ((removed_count++))
    fi
done
echo "✓"

# Remove library
echo -n "  Removing library... "
if [ -f "$INSTALL_PREFIX/lib/libioccultcalc.a" ]; then
    $SUDO rm -f "$INSTALL_PREFIX/lib/libioccultcalc.a"
    ((removed_count++))
fi
echo "✓"

# Remove headers
echo -n "  Removing headers... "
if [ -d "$INSTALL_PREFIX/include/ioccultcalc" ]; then
    $SUDO rm -rf "$INSTALL_PREFIX/include/ioccultcalc"
    ((removed_count++))
fi
echo "✓"

# Remove data and documentation
echo -n "  Removing data and documentation... "
if [ -d "$INSTALL_PREFIX/share/ioccultcalc" ]; then
    $SUDO rm -rf "$INSTALL_PREFIX/share/ioccultcalc"
    ((removed_count++))
fi
if [ -d "$INSTALL_PREFIX/share/doc/ioccultcalc" ]; then
    $SUDO rm -rf "$INSTALL_PREFIX/share/doc/ioccultcalc"
    ((removed_count++))
fi
echo "✓"

echo ""
echo -e "${GREEN}╔═══════════════════════════════════════════════════════════════╗${NC}"
echo -e "${GREEN}║        IOccultCalc successfully uninstalled!                   ║${NC}"
echo -e "${GREEN}╚═══════════════════════════════════════════════════════════════╝${NC}"
echo ""
echo "Removed $removed_count items from $INSTALL_PREFIX"
echo ""
