# IOccultCalc - OOP Configuration File Reference

## Supported Sections and Parameters

### 1. object - Target Asteroid Configuration

```plaintext
object.
        .id = '433'              ! Asteroid number or designation
        .name = 'Eros'           ! Asteroid name
        .min_diameter = 5.0      ! Minimum diameter (km) for database queries
        .max_diameter = 1000.0   ! Maximum diameter (km) for database queries
```

**Supported parameters:**
- `id` (string): Asteroid designation (number or provisional)
- `name` (string): Asteroid name
- `min_diameter` (double): Minimum diameter filter for multi-asteroid surveys
- `max_diameter` (double): Maximum diameter filter for multi-asteroid surveys

---

### 2. propag - Orbit Propagation Settings

```plaintext
propag.
        .start_jd = 2460676.5    ! Start epoch (Julian Date)
        .end_jd = 2460707.5      ! End epoch (Julian Date)
        .step_size = 0.05        ! Integration step (days)
        .type = 'RK4'            ! Propagator type
        .tolerance = 1.0E-10     ! Integration tolerance
```

**Supported parameters:**
- `start_jd` (double): Starting Julian Date for propagation
- `end_jd` (double): Ending Julian Date for propagation
- `step_size` (double): Integration step size in days
- `type` (string): Propagator type - 'RK4', 'RKF78', 'RA15', 'ORBFIT'
- `tolerance` (double): Integration tolerance (for adaptive methods)

---

### 3. ephemeris - JPL Ephemerides Configuration

```plaintext
ephemeris.
        .jpl_version = 'DE441'   ! JPL ephemeris version
        .ast17_file = '~/.ioccultcalc/ephemerides/codes_300ast_20100725.bsp'
        .use_spice = .TRUE.      ! Use SPICE toolkit
        .cache_enabled = .TRUE.  ! Enable caching
```

**Supported parameters:**
- `jpl_version` (string): JPL ephemeris version - 'DE440', 'DE441', 'DE430'
- `ast17_file` (string): Path to AST17 SPK file (17 massive asteroids)
- `use_spice` (bool): Enable SPICE/NAIF toolkit
- `cache_enabled` (bool): Enable ephemeris caching

---

### 4. output - Output Configuration

```plaintext
output.
        .file = 'results.json'   ! Output file path
        .format = 'JSON'         ! Output format
        .verbosity = 2           ! Verbosity level
        .include_path_coordinates = .TRUE.
        .include_uncertainty = .TRUE.
        .max_results = 1000      ! Maximum results to output
```

**Supported parameters:**
- `file` (string): Output file path
- `format` (string): Output format - 'JSON', 'TEXT', 'LATEX', 'XML_OCCULT4', 'IOTA_CARD'
- `verbosity` (int): Verbosity level (0=quiet, 1=normal, 2=verbose, 3=debug)
- `include_path_coordinates` (bool): Include detailed path coordinates
- `include_uncertainty` (bool): Include uncertainty estimates
- `max_results` (int): Maximum number of results to save

---

### 5. perturbations - Perturbation Model Settings

```plaintext
perturbations.
        .planets = .TRUE.            ! Include planetary perturbations
        .relativity = .TRUE.         ! Include relativistic corrections
        .asteroid_count = 17         ! Number of massive asteroids (AST17)
        .use_jpl_planets = .TRUE.    ! Use JPL planetary ephemerides
        .planetary_aberration = .TRUE. ! Include light-time correction
```

**Supported parameters:**
- `planets` (bool): Enable planetary perturbations (8 planets)
- `relativity` (bool): Enable relativistic corrections (Schwarzschild)
- `asteroid_count` (int): Number of massive asteroids to include (0-17)
- `use_jpl_planets` (bool): Use JPL ephemerides for planets
- `planetary_aberration` (bool): Include planetary aberration (light-time)

---

### 6. search - Occultation Search Parameters

```plaintext
search.
        .start_jd = 2460676.5        ! Search start epoch
        .end_jd = 2460707.5          ! Search end epoch
        .mag_limit = 15.0            ! Magnitude limit for stars
        .step_days = 0.5             ! Search sampling interval (days)
        .min_duration = 0.5          ! Minimum event duration (seconds)
        .max_duration = 300.0        ! Maximum event duration (seconds)
```

**Supported parameters:**
- `start_jd` (double): Search start Julian Date
- `end_jd` (double): Search end Julian Date
- `mag_limit` (double): Stellar magnitude limit for Gaia query
- `step_days` (double): Search sampling interval in days
- `min_duration` (double): Minimum acceptable event duration (seconds)
- `max_duration` (double): Maximum acceptable event duration (seconds)

---

### 7. database - Asteroid Database Filters

```plaintext
database.
        .min_perihelion = 1.0        ! Minimum perihelion distance (AU)
        .max_aphelion = 5.0          ! Maximum aphelion distance (AU)
        .min_observations = 50       ! Minimum number of observations
        .max_uncertainty = 10.0      ! Maximum orbital uncertainty (arcsec)
        .require_diameter = .TRUE.   ! Require known diameter
        .orbital_quality_min = 2     ! Minimum orbital quality (U parameter)
```

**Supported parameters:**
- `min_perihelion` (double): Minimum perihelion distance in AU
- `max_aphelion` (double): Maximum aphelion distance in AU
- `min_observations` (int): Minimum number of astrometric observations
- `max_uncertainty` (double): Maximum orbital uncertainty in arcseconds
- `require_diameter` (bool): Require diameter measurement available
- `orbital_quality_min` (int): Minimum orbital quality (JPL U parameter 0-9, lower=better)

---

### 8. filtering - Quality and Observability Filters

```plaintext
filtering.
        .min_magnitude_drop = 0.5    ! Minimum magnitude drop
        .min_altitude = 20.0         ! Minimum altitude (degrees)
        .sun_elevation_limit = -12.0 ! Sun altitude limit (degrees)
        .moon_separation_min = 30.0  ! Minimum Moon separation (degrees)
        .max_solar_elongation = 180.0
        .min_solar_elongation = 40.0
        .require_dark_sky = .TRUE.
        .exclude_twilight = .TRUE.
```

**Supported parameters:**
- `min_magnitude_drop` (double): Minimum stellar magnitude drop
- `min_altitude` (double): Minimum target altitude above horizon (degrees)
- `sun_elevation_limit` (double): Maximum Sun altitude (negative = below horizon)
- `moon_separation_min` (double): Minimum angular separation from Moon (degrees)
- `max_solar_elongation` (double): Maximum solar elongation (degrees)
- `min_solar_elongation` (double): Minimum solar elongation (degrees)
- `require_dark_sky` (bool): Require dark sky conditions
- `exclude_twilight` (bool): Exclude twilight events

---

### 9. scoring - Priority Scoring System

```plaintext
scoring.
        .weight_magnitude = 0.25     ! Weight for magnitude in scoring
        .weight_duration = 0.20      ! Weight for duration in scoring
        .weight_diameter = 0.30      ! Weight for asteroid size in scoring
        .weight_path_width = 0.15    ! Weight for path width
        .weight_orbital_quality = 0.10
        .min_score = 5.0             ! Minimum score for output
```

**Supported parameters:**
- `weight_magnitude` (double): Weight for stellar magnitude (0-1)
- `weight_duration` (double): Weight for event duration (0-1)
- `weight_diameter` (double): Weight for asteroid diameter (0-1)
- `weight_path_width` (double): Weight for shadow path width (0-1)
- `weight_orbital_quality` (double): Weight for orbital precision (0-1)
- `min_score` (double): Minimum priority score for output (0-10)

**Note:** Weights should sum to ~1.0 for normalized scoring.

---

### 10. performance - Performance and Optimization

```plaintext
performance.
        .parallel_threads = 8        ! Number of parallel threads
        .use_openmp = .TRUE.         ! Enable OpenMP parallelization
        .cache_gaia_stars = .TRUE.   ! Cache Gaia star catalog
        .optimize_earth_position = .TRUE.
```

**Supported parameters:**
- `parallel_threads` (int): Number of parallel threads (1-32)
- `use_openmp` (bool): Enable OpenMP multi-threading
- `cache_gaia_stars` (bool): Enable Gaia star catalog caching
- `optimize_earth_position` (bool): Use optimized Earth position calculation

---

### 11. observer - Observer Location Constraints

```plaintext
observer.
        .latitude_min = -90.0        ! Minimum latitude (degrees)
        .latitude_max = 90.0         ! Maximum latitude (degrees)
        .longitude_min = -180.0      ! Minimum longitude (degrees)
        .longitude_max = 180.0       ! Maximum longitude (degrees)
        .altitude = 0.0              ! Observer altitude (meters)
```

**Supported parameters:**
- `latitude_min` (double): Minimum observer latitude (-90 to 90)
- `latitude_max` (double): Maximum observer latitude (-90 to 90)
- `longitude_min` (double): Minimum observer longitude (-180 to 180)
- `longitude_max` (double): Maximum observer longitude (-180 to 180)
- `altitude` (double): Observer altitude above sea level (meters)

---

### 12. gaia - Gaia Catalog Settings

```plaintext
gaia.
        .release = 'DR3'             ! Gaia data release
        .magnitude_limit = 18.0      ! Magnitude limit
        .proper_motion = .TRUE.      ! Include proper motion
        .parallax_correction = .TRUE.
        .use_cache = .TRUE.
```

**Supported parameters:**
- `release` (string): Gaia data release - 'DR2', 'DR3', 'EDR3'
- `magnitude_limit` (double): Maximum G magnitude to query
- `proper_motion` (bool): Apply proper motion corrections
- `parallax_correction` (bool): Apply parallax corrections
- `use_cache` (bool): Use local cache for Gaia data

---

### 13. validation - Validation and Quality Control

```plaintext
validation.
        .compare_with_horizons = .FALSE.
        .check_path_consistency = .TRUE.
        .verify_star_positions = .TRUE.
        .log_warnings = .TRUE.
```

**Supported parameters:**
- `compare_with_horizons` (bool): Compare results with JPL Horizons
- `check_path_consistency` (bool): Validate path consistency
- `verify_star_positions` (bool): Verify star positions with catalog
- `log_warnings` (bool): Log validation warnings

---

## Complete Example: Large Asteroids Survey

```plaintext
! IOccultCalc Configuration - Large Asteroids January 2026

object.
        .id = '0'
        .name = 'Survey Large Asteroids >5km'
        .min_diameter = 5.0
        .max_diameter = 1000.0

propag.
        .start_jd = 2460676.500000
        .end_jd = 2460707.500000
        .step_size = 0.050000
        .type = 'RK4'

ephemeris.
        .jpl_version = 'DE441'
        .ast17_file = '~/.ioccultcalc/ephemerides/codes_300ast_20100725.bsp'

output.
        .file = 'occultations_large_asteroids_jan2026.json'
        .format = 'JSON'
        .verbosity = 2

perturbations.
        .planets = .TRUE.
        .relativity = .TRUE.
        .asteroid_count = 17

search.
        .start_jd = 2460676.500000
        .end_jd = 2460707.500000
        .mag_limit = 15.0
        .step_days = 0.5
        .min_duration = 0.5
        .max_duration = 300.0

database.
        .min_perihelion = 1.0
        .max_aphelion = 5.0
        .min_observations = 50
        .max_uncertainty = 10.0
        .require_diameter = .TRUE.

filtering.
        .min_magnitude_drop = 0.5
        .min_altitude = 20.0
        .sun_elevation_limit = -12.0
        .moon_separation_min = 30.0

scoring.
        .weight_magnitude = 0.25
        .weight_duration = 0.20
        .weight_diameter = 0.30
        .min_score = 5.0

performance.
        .parallel_threads = 8
        .use_openmp = .TRUE.
```

---

## Usage

```bash
# Run with OOP configuration file
italoccultcalc config.oop

# Run with JSON configuration file
italoccultcalc config.json

# View help
italoccultcalc --help
```

---

## Notes

1. **Boolean values**: Use `.TRUE.` or `.FALSE.` (Fortran style)
2. **String values**: Enclose in single quotes `'value'`
3. **Comments**: Start with `!` character
4. **Section headers**: End with a period (e.g., `object.`)
5. **Parameters**: Start with a period (e.g., `.id = '433'`)

---

## Data Types

- **string**: Text values in single quotes
- **double**: Floating-point numbers
- **int**: Integer numbers
- **bool**: `.TRUE.` or `.FALSE.`

---

## Default Values

If parameters are omitted, the following defaults are used:

- Propagator: RK4
- Step size: 0.05 days
- Magnitude limit: 14.0
- Diameter range: 50-1000 km
- Perihelion range: 1.5-4.5 AU
- Threads: 8
- Output format: JSON

---

**Last updated:** 2025-11-24  
**IOccultCalc version:** 2.0
