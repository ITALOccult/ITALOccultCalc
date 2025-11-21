# IOccultCalc Scientific Manual

Comprehensive scientific manual for the IOccultCalc library, documenting high-precision asteroid occultation prediction methods.

## Contents

- **16 Chapters** covering complete prediction workflow:
  1. Introduction
  2. Coordinate Systems (ICRS, ITRS, transformations)
  3. Time Systems (TAI, UTC, UT1, TT, TDB)
  4. Planetary Ephemerides (VSOP87D, ELP2000)
  5. Orbital Mechanics (classical & equinoctial elements)
  6. Numerical Integration (RKF78, DOPRI853)
  7. Perturbations (N-body dynamics)
  8. Relativistic Corrections (light-time, aberration, deflection)
  9. Precession & Nutation (IAU 2000A)
  10. Stellar Astrometry (Gaia DR3)
  11. Orbit Determination (differential correction)
  12. Asteroid Shape Models (triaxial ellipsoids)
  13. Besselian Method (fundamental plane geometry)
  14. Uncertainty Propagation (STM, Monte Carlo)
  15. Implementation (architecture, API, optimization)
  16. Validation (accuracy tests, benchmarks)

- **3 Appendices** with reference data:
  - A: Physical & Astronomical Constants (CODATA 2018, IAU 2015, leap seconds)
  - B: Complete Algorithms (5 detailed pseudocode implementations)
  - C: VSOP87 Coefficient Tables (Earth L0/L1 series)

- **46 Scientific References** (Milani, Vallado, IERS, Gaia, etc.)

## Features

- **15+ TikZ diagrams**: coordinate systems, time scales, orbital geometry, Besselian method, architecture
- **40+ tables**: error budgets, validation results, comparisons, constants
- **25+ examples**: numerical calculations demonstrating concepts
- **Comprehensive citations**: referencing authoritative sources throughout
- **Mathematical rigor**: complete equations, algorithms, validation results
- **Professional formatting**: book class with proper typography

## Requirements

### macOS Installation

Install MacTeX (complete LaTeX distribution):

```bash
# Option 1: Homebrew (recommended)
brew install --cask mactex

# Option 2: Download from https://www.tug.org/mactex/
# (~4 GB download, includes all packages)
```

After installation, add to PATH:

```bash
export PATH="/Library/TeX/texbin:$PATH"
```

### Required LaTeX Packages

All included in MacTeX. If using BasicTeX or custom installation:

- `amsmath`, `amssymb`, `amsfonts` - mathematical typesetting
- `natbib` - bibliography management (author-year citations)
- `hyperref` - PDF hyperlinks and cross-references
- `tikz` - diagrams and illustrations
- `algorithm`, `algorithmic` - pseudocode formatting
- `geometry` - page layout
- `fancyhdr` - headers and footers
- `graphicx` - figure handling
- `booktabs` - professional tables
- `xcolor` - colored links

## Compilation

### Full Build Process

```bash
cd docs/manual

# Step 1: First LaTeX pass (generates .aux, .toc files)
pdflatex main.tex

# Step 2: Process bibliography
bibtex main

# Step 3: Second pass (resolves citations)
pdflatex main.tex

# Step 4: Third pass (resolves cross-references)
pdflatex main.tex
```

### Quick Rebuild

After modifying content (no bibliography changes):

```bash
pdflatex main.tex
```

### Clean Build

Remove auxiliary files and rebuild:

```bash
rm -f *.aux *.bbl *.blg *.log *.out *.toc *.lof *.lot *.loa chapters/*.aux appendices/*.aux
pdflatex main.tex && bibtex main && pdflatex main.tex && pdflatex main.tex
```

## Output

Compilation generates `main.pdf` (~100-150 pages) containing:

- Front matter (title page, copyright, abstract, preface, table of contents, lists)
- 16 chapters with complete scientific content
- 3 appendices with reference data
- Bibliography with 46 references
- Hyperlinked cross-references and citations

## Structure

```
docs/manual/
├── main.tex                    # Main document (preamble, structure)
├── references.bib              # Bibliography (46 entries)
├── chapters/
│   ├── 01_introduction.tex
│   ├── 02_coordinate_systems.tex
│   ├── 03_time_systems.tex
│   ├── 04_planetary_ephemerides.tex
│   ├── 05_orbital_mechanics.tex
│   ├── 06_numerical_integration.tex
│   ├── 07_perturbations.tex
│   ├── 08_relativistic_corrections.tex
│   ├── 09_precession_nutation.tex
│   ├── 10_stellar_astrometry.tex
│   ├── 11_orbit_determination.tex
│   ├── 12_asteroid_shape.tex
│   ├── 13_besselian_method.tex
│   ├── 14_uncertainty_propagation.tex
│   ├── 15_implementation.tex
│   └── 16_validation.tex
├── appendices/
│   ├── appendix_a_constants.tex
│   ├── appendix_b_algorithms.tex
│   └── appendix_c_vsop_tables.tex
└── README.md                   # This file
```

## Customization

### Precision Levels

The manual documents 4 precision modes (Chapter 15):

- **FAST**: 5-10 km accuracy, 0.1 ms computation
- **STANDARD**: 1-2 km accuracy, 10 ms computation  
- **HIGH**: 0.5-1 km accuracy, 50 ms computation
- **REFERENCE**: 0.3-0.5 km accuracy, 10 s computation

### Custom Commands

Defined in `main.tex` preamble:

- `\ioccultcalc` - formatted library name
- `\vsop` - VSOP87 with proper spacing
- `\gaia` - Gaia mission name
- `\vect{x}` - bold vector notation
- `\mat{A}` - bold matrix notation
- `\deriv{f}{x}` - derivative df/dx
- `\pderiv{f}{x}` - partial derivative ∂f/∂x

## Troubleshooting

### Missing Packages

If compilation fails with "package not found":

```bash
# Install missing package (macOS with MacTeX)
sudo tlmgr install <package-name>

# Update all packages
sudo tlmgr update --all
```

### TikZ Errors

If diagrams fail to compile, ensure `tikz` and libraries are installed:

```bash
sudo tlmgr install pgf tikz-3dplot
```

### Bibliography Issues

If citations show as `[?]`:

1. Ensure `references.bib` exists and is valid
2. Run the full build process (pdflatex → bibtex → pdflatex × 2)
3. Check `.blg` file for BibTeX errors

### Long Compilation Times

Expected compile time: 30-60 seconds for full build

- First pass: ~15 s (processes all chapters and TikZ)
- BibTeX: ~2 s (processes 46 references)
- Second/third pass: ~10 s each (faster, aux files cached)

Use `pdflatex -interaction=nonstopmode` to skip error prompts.

## Validation

After compilation, verify:

- [ ] PDF opens without errors
- [ ] All 16 chapters present with correct numbering
- [ ] All 3 appendices included
- [ ] Table of contents accurate (check page numbers)
- [ ] List of figures complete (15+ entries)
- [ ] List of tables complete (40+ entries)
- [ ] Bibliography renders correctly (46 references)
- [ ] All citations resolved (no `[?]` markers)
- [ ] TikZ diagrams display correctly
- [ ] Hyperlinks work (click chapter/figure/equation references)
- [ ] Equations numbered consistently
- [ ] Algorithms formatted properly

## License

Manual content licensed under **Creative Commons Attribution-ShareAlike 4.0 International (CC-BY-SA 4.0)**.

You are free to:
- **Share** — copy and redistribute in any medium or format
- **Adapt** — remix, transform, and build upon the material

Under the terms:
- **Attribution** — cite the IOccultCalc project
- **ShareAlike** — distribute adaptations under the same license

See: https://creativecommons.org/licenses/by-sa/4.0/

## Citation

If using this manual in academic work:

```bibtex
@manual{IOccultCalc2025,
  title = {IOccultCalc: High-Precision Asteroid Occultation Prediction},
  author = {{IOccultCalc Development Team}},
  year = {2025},
  note = {Version 2.0},
  url = {https://github.com/yourusername/IOccultCalc}
}
```

## Contributing

To improve the manual:

1. Edit relevant `.tex` files in `chapters/` or `appendices/`
2. Add new references to `references.bib` (BibTeX format)
3. Test compilation locally
4. Ensure all cross-references resolve
5. Submit changes

## Contact

For questions about the manual or IOccultCalc library:

- GitHub Issues: [project repository]
- Documentation: `docs/` directory
- Technical details: `HIGH_PRECISION_ALGORITHMS.md`

## Acknowledgments

This manual documents methods from:
- Andrea Milani (orbit determination theory)
- David Vallado (astrodynamics fundamentals)
- IERS Conventions (time scales and transformations)
- JPL HORIZONS (ephemeris validation)
- Gaia mission (stellar astrometry)
- International Astronomical Union (standards and constants)

References cited throughout the text acknowledge all sources.
