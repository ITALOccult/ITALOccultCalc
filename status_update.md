# Situational Analysis: Asteroid 34713 Orbital Fit

## Current Status

We are currently investigating why the orbital fit for asteroid **34713 Yesiltas** is converging to a very high RMS residual of **~171 arcseconds** (with individual residuals up to **~800 arcseconds**), despite using high-precision models (DE440, IAU 2006).

### Key Issues Identified & Resolved:
1.  **[FIXED] GMST Double-Counting:** The `ReferenceFrame::gmst` function was using an outdated/redundant formula. It has been replaced with the modern **IAU 2006 ERA-based formula**.
2.  **[FIXED] RWO Parser Alphanumeric Codes:** The parser was failing to recognize alphanumeric observatory codes (e.g., `M22`, `G96`), causing observations to default to geocentric coordinates.
3.  **[FIXED] Missing Observatory Data:** The test harness `test_mpc_34713` was running with an empty observatory database. I have manually added the required station coordinates.
4.  **[FIXED] Codebase Cleanup:** Removed debug instrumentation and resolved several "unused variable" warnings to ensure a clean build.

### Remaining Blocker:
*   **Systemic Offset (~800 arcsec):** The large residuals suggest a fundamental frame misalignment or a serious bug in the light-time/aberration correction logic. Specifically, the RA discrepancy is consistently around **0.22 degrees**.

---

## Proposed Next Steps

### 1. Frame Transformation Audit (Ecliptic ↔ J2000)
I suspect the transformation between the Ecliptic frame (used in `.eq1` files) and the Equatorial J2000 frame (used internally) might have a sign error or be using an inconsistent definition.
- **Action:** Verify the rotation matrix direction in `ReferenceFrame.hpp` and compare with the standard IAU definition for Mean Ecliptic and Equinox of J2000.0.

### 2. Heliocentric vs. Barycentric Consistency
AstDyS (OrbFit) primarily uses **Heliocentric** coordinates for its element files, but the propagator often works in **Barycentric** or Heliocentric depending on settings. 
- **Action:** Verify that the `earth_center` calculation in `Residuals.cpp` correctly accounts for the Sun's position if the state is expected to be heliocentric.

### 3. Light-Time and Aberration Deep Dive
Verify that light-time is being applied at the correct stage (state propagation) and that stellar aberration is correctly computed for the observer's velocity.
- **Action:** Inspect `Residuals.cpp` to ensure the light-time subtraction is performed on the *observation epoch* to find the *emission epoch*.

### 4. Convergence Verification
Once the systematic error is identified, re-run the `test_mpc_34713` benchmark to confirm the RMS drops below **0.1 arcseconds**.

---

## Progress Tracking
- [x] Fix GMST formula
- [x] Fix RWO parser for obs codes
- [x] Populate test observatory database
- [/] Audit Ecliptic coordinate transformation
- [ ] Verify light-time correction logic
- [ ] Achieve < 0.1" RMS
