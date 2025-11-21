#!/bin/bash

# Script di build e test per IOccultCalc
# Uso: ./build.sh [clean|test|install]

set -e  # Exit on error

PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BUILD_DIR="${PROJECT_DIR}/build"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "================================================"
echo "IOccultCalc Build Script"
echo "================================================"
echo ""

# Parse arguments
CLEAN=false
TEST=false
INSTALL=false

for arg in "$@"; do
    case $arg in
        clean)
            CLEAN=true
            ;;
        test)
            TEST=true
            ;;
        install)
            INSTALL=true
            ;;
        *)
            echo -e "${RED}Unknown argument: $arg${NC}"
            echo "Usage: ./build.sh [clean|test|install]"
            exit 1
            ;;
    esac
done

# Clean build directory
if [ "$CLEAN" = true ]; then
    echo -e "${YELLOW}Cleaning build directory...${NC}"
    rm -rf "${BUILD_DIR}"
    echo -e "${GREEN}Clean complete${NC}"
    echo ""
fi

# Check dependencies
echo -e "${YELLOW}Checking dependencies...${NC}"

if ! command -v cmake &> /dev/null; then
    echo -e "${RED}ERROR: cmake not found${NC}"
    echo "Install with: brew install cmake (macOS) or apt-get install cmake (Linux)"
    exit 1
fi

if ! command -v curl-config &> /dev/null; then
    echo -e "${RED}ERROR: libcurl development files not found${NC}"
    echo "Install with: brew install curl (macOS) or apt-get install libcurl4-openssl-dev (Linux)"
    exit 1
fi

if ! command -v xml2-config &> /dev/null; then
    echo -e "${RED}ERROR: libxml2 development files not found${NC}"
    echo "Install with: brew install libxml2 (macOS) or apt-get install libxml2-dev (Linux)"
    exit 1
fi

echo -e "${GREEN}All dependencies found${NC}"
echo ""

# Create build directory
mkdir -p "${BUILD_DIR}"
cd "${BUILD_DIR}"

# Configure
echo -e "${YELLOW}Configuring CMake...${NC}"
cmake .. -DCMAKE_BUILD_TYPE=Release

# Build
echo ""
echo -e "${YELLOW}Building...${NC}"
make -j$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)

echo ""
echo -e "${GREEN}Build complete!${NC}"
echo ""

# Test
if [ "$TEST" = true ]; then
    echo -e "${YELLOW}Running tests...${NC}"
    ctest --output-on-failure
    echo ""
fi

# Install
if [ "$INSTALL" = true ]; then
    echo -e "${YELLOW}Installing...${NC}"
    sudo make install
    echo -e "${GREEN}Installation complete${NC}"
    echo ""
fi

# Summary
echo "================================================"
echo -e "${GREEN}BUILD SUCCESSFUL${NC}"
echo "================================================"
echo ""
echo "Executables:"
echo "  Basic example:  ./build/examples/example_basic"
echo "  Search example: ./build/examples/example_search"
echo "  Tests:          ./build/tests/test_time_utils"
echo ""
echo "Try it:"
echo "  cd ${PROJECT_DIR}"
echo "  ./build/examples/example_basic 433"
echo ""
echo "For more info, see README.md and docs/GUIDE.md"
echo ""
