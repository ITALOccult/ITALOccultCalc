#!/bin/bash

# ============================================================================
# IOccultCalc - Complete Installation Script
# ============================================================================
# Automatic installation and setup for IOccultCalc
# Usage: ./install.sh [options]
# Options:
#   --prefix PATH    Install to custom location (default: /usr/local)
#   --user           Install in user home directory (~/.local)
#   --dev            Development mode (no optimization)
#   --clean          Clean build before installation
#   --build-only     Only build, don't install
#   --skip-tests     Skip running tests
#   --help           Show this help
# ============================================================================

set -e  # Exit on error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Default configuration
INSTALL_PREFIX="/usr/local"
BUILD_TYPE="Release"
CLEAN_BUILD=false
USER_INSTALL=false
BUILD_ONLY=false
SKIP_TESTS=false
NUM_JOBS=$(sysctl -n hw.ncpu 2>/dev/null || nproc 2>/dev/null || echo 4)

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --prefix)
            INSTALL_PREFIX="$2"
            shift 2
            ;;
        --user)
            USER_INSTALL=true
            INSTALL_PREFIX="$HOME/.local"
            shift
            ;;
        --dev)
            BUILD_TYPE="Debug"
            shift
            ;;
        --clean)
            CLEAN_BUILD=true
            shift
            ;;
        --build-only)
            BUILD_ONLY=true
            shift
            ;;
        --skip-tests)
            SKIP_TESTS=true
            shift
            ;;
        --help)
            head -n 15 "$0" | tail -n 10
            exit 0
            ;;
        *)
            echo -e "${RED}Unknown option: $1${NC}"
            echo "Use --help for usage information"
            exit 1
            ;;
    esac
done

echo -e "${BLUE}"
echo "╔═══════════════════════════════════════════════════════════════╗"
echo "║          IOccultCalc v2.0 - Installation Script              ║"
echo "║    High-Precision Asteroid Occultation Prediction System      ║"
echo "╚═══════════════════════════════════════════════════════════════╝"
echo -e "${NC}"

# Print configuration
echo -e "${YELLOW}Configuration:${NC}"
echo "  Install prefix: $INSTALL_PREFIX"
echo "  Build type: $BUILD_TYPE"
echo "  Parallel jobs: $NUM_JOBS"
echo "  Clean build: $CLEAN_BUILD"
echo "  Build only: $BUILD_ONLY"
echo "  Skip tests: $SKIP_TESTS"
echo ""

# Check system
echo -e "${CYAN}[1/8] Checking system...${NC}"

# Detect OS
if [[ "$OSTYPE" == "darwin"* ]]; then
    OS="macOS"
    echo "  ✓ Operating System: macOS $(sw_vers -productVersion 2>/dev/null || echo 'Unknown')"
elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
    OS="Linux"
    echo "  ✓ Operating System: Linux"
else
    echo -e "${RED}  ✗ Unsupported OS: $OSTYPE${NC}"
    exit 1
fi

# Check dependencies
echo -e "${CYAN}[2/8] Checking dependencies...${NC}"

missing_deps=()

# Check CMake
if ! command -v cmake &> /dev/null; then
    missing_deps+=("cmake")
else
    CMAKE_VERSION=$(cmake --version | head -n1 | awk '{print $3}')
    echo "  ✓ CMake $CMAKE_VERSION"
fi

# Check C++ compiler
if ! command -v g++ &> /dev/null && ! command -v clang++ &> /dev/null; then
    missing_deps+=("c++ compiler (g++ or clang++)")
else
    if command -v clang++ &> /dev/null; then
        CXX_VERSION=$(clang++ --version | head -n1)
        echo "  ✓ C++ Compiler: $CXX_VERSION"
    else
        CXX_VERSION=$(g++ --version | head -n1)
        echo "  ✓ C++ Compiler: $CXX_VERSION"
    fi
fi

# Check Git (optional)
if ! command -v git &> /dev/null; then
    echo "  ⚠ Git not found (optional)"
else
    GIT_VERSION=$(git --version | awk '{print $3}')
    echo "  ✓ Git $GIT_VERSION"
fi

# Check OpenMP
if [[ "$OS" == "macOS" ]]; then
    if brew list libomp &> /dev/null 2>&1; then
        echo "  ✓ OpenMP (libomp) - parallel processing enabled"
    else
        echo "  ⚠ OpenMP not found - parallel processing disabled"
        echo "    Recommended: brew install libomp"
    fi
fi

# Report missing dependencies
if [ ${#missing_deps[@]} -gt 0 ]; then
    echo -e "${RED}  ✗ Missing required dependencies:${NC}"
    for dep in "${missing_deps[@]}"; do
        echo "    - $dep"
    done
    echo ""
    if [[ "$OS" == "macOS" ]]; then
        echo "Install with Homebrew:"
        echo "  brew install cmake"
    elif [[ "$OS" == "Linux" ]]; then
        echo "Install with apt (Ubuntu/Debian):"
        echo "  sudo apt-get install cmake g++ git"
        echo "Or with yum (RHEL/CentOS):"
        echo "  sudo yum install cmake gcc-c++ git"
    fi
    exit 1
fi

# Clean build directory
if [ "$CLEAN_BUILD" = true ]; then
    echo -e "${CYAN}[3/8] Cleaning build directory...${NC}"
    if [ -d "build" ]; then
        rm -rf build
        echo "  ✓ Build directory cleaned"
    else
        echo "  ⚠ No build directory to clean"
    fi
else
    echo -e "${CYAN}[3/8] Reusing existing build directory${NC}"
fi

# Create build directory
echo -e "${CYAN}[4/8] Configuring build...${NC}"
mkdir -p build
cd build

# Configure with CMake
CMAKE_ARGS=(
    "-DCMAKE_BUILD_TYPE=$BUILD_TYPE"
    "-DCMAKE_INSTALL_PREFIX=$INSTALL_PREFIX"
    "-DBUILD_EXAMPLES=ON"
    "-DBUILD_TESTS=ON"
)

# Add OpenMP support for macOS if available
if [[ "$OS" == "macOS" ]] && brew list libomp &> /dev/null 2>&1; then
    LIBOMP_PREFIX=$(brew --prefix libomp)
    CMAKE_ARGS+=(
        "-DOpenMP_C_FLAGS=-Xpreprocessor -fopenmp -I$LIBOMP_PREFIX/include"
        "-DOpenMP_CXX_FLAGS=-Xpreprocessor -fopenmp -I$LIBOMP_PREFIX/include"
        "-DOpenMP_C_LIB_NAMES=omp"
        "-DOpenMP_CXX_LIB_NAMES=omp"
        "-DOpenMP_omp_LIBRARY=$LIBOMP_PREFIX/lib/libomp.dylib"
    )
fi

echo "  Running CMake configuration..."
cmake "${CMAKE_ARGS[@]}" .. || {
    echo -e "${RED}  ✗ CMake configuration failed${NC}"
    exit 1
}
echo "  ✓ Configuration complete"

# Build
echo -e "${CYAN}[5/8] Building IOccultCalc...${NC}"
echo "  Using $NUM_JOBS parallel jobs"
cmake --build . --config $BUILD_TYPE -j $NUM_JOBS || {
    echo -e "${RED}  ✗ Build failed${NC}"
    exit 1
}
echo "  ✓ Build complete"

# Run tests
if [ "$SKIP_TESTS" = false ]; then
    echo -e "${CYAN}[6/8] Running tests...${NC}"
    
    test_passed=0
    test_failed=0
    
    # List of test executables to run
    TESTS=(
        "tests/test_combined_lists"
        "tests/test_astnum_output"
        "tests/test_asteroid_list_reader"
        "tests/test_localasteroid_real"
    )
    
    for test in "${TESTS[@]}"; do
        if [ -f "$test" ]; then
            test_name=$(basename "$test")
            echo -n "  Testing $test_name... "
            if ./"$test" > /dev/null 2>&1; then
                echo -e "${GREEN}PASSED${NC}"
                ((test_passed++))
            else
                echo -e "${YELLOW}FAILED (non-critical)${NC}"
                ((test_failed++))
            fi
        fi
    done
    
    echo "  ✓ Tests completed: $test_passed passed, $test_failed failed"
else
    echo -e "${CYAN}[6/8] Skipping tests${NC}"
fi

# Check executables
echo -e "${CYAN}[7/8] Checking executables...${NC}"
EXECUTABLES=(
    "tools/occult_calc"
    "examples/italoccultcalc"
    "examples/ioccultcalc_search"
)

for exe in "${EXECUTABLES[@]}"; do
    if [ -f "$exe" ]; then
        echo "  ✓ Built: $exe"
    else
        echo "  ⚠ Missing: $exe"
    fi
done

# Install (if not build-only mode)
if [ "$BUILD_ONLY" = false ]; then
    echo -e "${CYAN}[8/8] Installing IOccultCalc...${NC}"
    
    # Check if we need sudo
    SUDO=""
    if [ "$USER_INSTALL" = false ] && [ ! -w "$INSTALL_PREFIX" ]; then
        echo "  Administrator privileges required for system installation"
        SUDO="sudo"
    fi
    
    # Create directories
    echo "  Creating installation directories..."
    $SUDO mkdir -p "$INSTALL_PREFIX/bin"
    $SUDO mkdir -p "$INSTALL_PREFIX/lib"
    $SUDO mkdir -p "$INSTALL_PREFIX/include/ioccultcalc"
    $SUDO mkdir -p "$INSTALL_PREFIX/share/ioccultcalc/presets"
    $SUDO mkdir -p "$INSTALL_PREFIX/share/ioccultcalc/data"
    $SUDO mkdir -p "$INSTALL_PREFIX/share/doc/ioccultcalc"
    
    # Install executables
    echo "  Installing executables..."
    for exe in "${EXECUTABLES[@]}"; do
        if [ -f "$exe" ]; then
            exe_name=$(basename "$exe")
            $SUDO cp "$exe" "$INSTALL_PREFIX/bin/"
            $SUDO chmod +x "$INSTALL_PREFIX/bin/$exe_name"
            echo "    ✓ $exe_name"
        fi
    done
    
    # Install library
    if [ -f "lib/libioccultcalc.a" ]; then
        echo "  Installing library..."
        $SUDO cp lib/libioccultcalc.a "$INSTALL_PREFIX/lib/"
        echo "    ✓ libioccultcalc.a"
    fi
    
    # Install headers
    cd ..
    if [ -d "include/ioccultcalc" ]; then
        echo "  Installing headers..."
        $SUDO cp -r include/ioccultcalc/* "$INSTALL_PREFIX/include/ioccultcalc/"
        echo "    ✓ Header files"
    fi
    
    # Install preset files
    echo "  Installing preset files..."
    preset_count=0
    for preset in preset_*.oop preset_*.json; do
        if [ -f "$preset" ]; then
            $SUDO cp "$preset" "$INSTALL_PREFIX/share/ioccultcalc/presets/"
            ((preset_count++))
        fi
    done
    echo "    ✓ $preset_count preset files"
    
    # Install example data
    echo "  Installing example data..."
    data_count=0
    for example in example_*.txt; do
        if [ -f "$example" ]; then
            $SUDO cp "$example" "$INSTALL_PREFIX/share/ioccultcalc/data/"
            ((data_count++))
        fi
    done
    # Also copy localasteroid if exists
    if [ -f "etc/localasteroid" ]; then
        $SUDO cp etc/localasteroid "$INSTALL_PREFIX/share/ioccultcalc/data/"
        ((data_count++))
    fi
    echo "    ✓ $data_count data files"
    
    # Install documentation
    echo "  Installing documentation..."
    doc_count=0
    for doc in *.md; do
        if [ -f "$doc" ]; then
            $SUDO cp "$doc" "$INSTALL_PREFIX/share/doc/ioccultcalc/"
            ((doc_count++))
        fi
    done
    # Copy LICENSE if exists
    if [ -f "LICENSE" ]; then
        $SUDO cp LICENSE "$INSTALL_PREFIX/share/doc/ioccultcalc/"
        ((doc_count++))
    fi
    echo "    ✓ $doc_count documentation files"
    
    echo "  ✓ Installation complete"
    cd build
else
    echo -e "${CYAN}[8/8] Skipping installation (build-only mode)${NC}"
fi

# Installation summary
echo ""
echo -e "${GREEN}╔═══════════════════════════════════════════════════════════════╗${NC}"
echo -e "${GREEN}║           Installation completed successfully!                ║${NC}"
echo -e "${GREEN}╚═══════════════════════════════════════════════════════════════╝${NC}"
echo ""

if [ "$BUILD_ONLY" = false ]; then
    echo -e "${YELLOW}Installation Summary:${NC}"
    echo "  Installation prefix: $INSTALL_PREFIX"
    echo "  Executables: $INSTALL_PREFIX/bin/"
    echo "  Library: $INSTALL_PREFIX/lib/libioccultcalc.a"
    echo "  Headers: $INSTALL_PREFIX/include/ioccultcalc/"
    echo "  Presets: $INSTALL_PREFIX/share/ioccultcalc/presets/"
    echo "  Examples: $INSTALL_PREFIX/share/ioccultcalc/data/"
    echo "  Documentation: $INSTALL_PREFIX/share/doc/ioccultcalc/"
    echo ""
    
    # Verification
    echo -e "${YELLOW}Installed Executables:${NC}"
    for exe in "${EXECUTABLES[@]}"; do
        exe_name=$(basename "$exe")
        if [ -f "$INSTALL_PREFIX/bin/$exe_name" ]; then
            size=$(ls -lh "$INSTALL_PREFIX/bin/$exe_name" | awk '{print $5}')
            echo "  ✓ $exe_name ($size)"
        fi
    done
    echo ""
    
    # Check if install prefix is in PATH
    if [[ ":$PATH:" != *":$INSTALL_PREFIX/bin:"* ]]; then
        echo -e "${YELLOW}⚠ Note: $INSTALL_PREFIX/bin is not in your PATH${NC}"
        echo "Add to your shell profile (~/.bashrc or ~/.zshrc):"
        echo "  export PATH=\"$INSTALL_PREFIX/bin:\$PATH\""
        echo ""
    fi
    
    # Usage instructions
    echo -e "${YELLOW}Quick Start:${NC}"
    echo "  1. Calculate a single occultation:"
    echo "     occult_calc 433 2026-06-15"
    echo ""
    echo "  2. Run a monthly survey:"
    echo "     italoccultcalc --preset $INSTALL_PREFIX/share/ioccultcalc/presets/preset_production_monthly.oop"
    echo ""
    echo "  3. Search with combined asteroid lists:"
    echo "     italoccultcalc --preset $INSTALL_PREFIX/share/ioccultcalc/presets/preset_quick_combined.oop"
    echo ""
    
    echo -e "${YELLOW}Development:${NC}"
    echo "  Include in your C++ project:"
    echo "    #include <ioccultcalc/ioccultcalc.h>"
    echo "  Compile and link:"
    echo "    g++ -I$INSTALL_PREFIX/include myapp.cpp -L$INSTALL_PREFIX/lib -lioccultcalc"
    echo ""
    
    echo -e "${YELLOW}Documentation:${NC}"
    echo "  View installed guides:"
    echo "    ls $INSTALL_PREFIX/share/doc/ioccultcalc/"
    echo "  Italian OOP guide:"
    echo "    cat $INSTALL_PREFIX/share/doc/ioccultcalc/GUIDA_FILE_OOP.md"
    echo "  Combined lists guide:"
    echo "    cat $INSTALL_PREFIX/share/doc/ioccultcalc/COMBINED_LISTS_IMPLEMENTATION.md"
    echo ""
else
    echo -e "${YELLOW}Build Summary:${NC}"
    echo "  Build directory: $(pwd)"
    echo "  Build type: $BUILD_TYPE"
    echo "  Executables ready in: build/examples/ and build/tools/"
    echo ""
    echo "  To install, run:"
    echo "    ./install.sh"
    echo "  Or install to custom location:"
    echo "    ./install.sh --prefix /path/to/install"
    echo ""
fi

echo -e "${GREEN}IOccultCalc is ready to use! 🚀${NC}"
echo ""
