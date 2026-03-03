# Dati di riferimento per test avanzati (shadow path, occultazioni)

## File

- **eros_occultation_YYYYMMDD.json** – Dati occultazione storica 433 Eros (da IOTA/asteroidoccultation.com). Formato: coordinate stella Gaia DR3, path osservato, tempo centrale, durata.
- **jpl_horizons_eros_jd2459000.5.txt** – Posizione Eros da JPL Horizons per JD 2459000.5 (confronto heliocentrico/geocentrico).
- **earth_position_jd2459000.5.txt** / **earth_velocity_jd2459000.5.txt** – Terra vs Sole, frame Ecliptic J2000, JD 2459000.5 (AU, AU/day).
- **jpl_horizons_earth_2026mar10.txt** – Terra vs Sole da JPL Horizons, frame Ecliptic J2000, JD 2461109.5 (2026-Mar-10 00:00 TDB); valori in AU e AU/day (+ km/km/s in commento).
- **star_reference_jd2461109.5.txt** – (opzionale) Stella per test posizione astrometrica: ra_deg, dec_deg, ref_epoch_jd, pmra_mas, pmdec_mas, parallax_mas; opzionale expected_app_ra_deg, expected_app_dec_deg per confronto.
- **jpl_horizons_asteroid_2026mar10.txt** – (opzionale) Asteroide vs Sole, Ecliptic J2000, JD 2461109.5: JD=, X=, Y=, Z=, VX=, VY=, VZ= (AU, AU/day).
- **asteroid_elements_2026mar10.txt** – (opzionale) Elementi equinoziali per propagazione: epoch_jd=, a_au=, h=, k=, p=, q=, lambda_rad=, number=.
- **earth_rotation_test_points.csv** – Punti test per rotazione ITRF (lat, lon, alt_m).

## Test completo JPL (test_jpl_consistency_full)

Eseguire da `build/`: `./tests/test_jpl_consistency_full [reference_data_dir] [spk_path]`.
Default: `tests/reference_data`, `de440.bsp`.
Il test confronta: (1) posizione/velocità Terra vs JPL; (2) posizione apparente stella (media + PM + parallasse) vs attesa se fornita; (3) posizione/velocità asteroide (propagata) vs JPL se presenti riferimenti e elementi.
Scrive report in `reference_data/jpl_consistency_report.txt`. Exit 0 solo se tutti i confronti eseguiti passano.

## Costanti Terra (JPL / IERS, allineate al codice)

Per confronto con `types.h` e `coordinates.cpp`:

| Grandezza        | Valore JPL/geodetico   | Uso in codice        |
|------------------|------------------------|------------------------|
| Raggio equatoriale | 6378.137 km          | `EARTH_RADIUS_WGS84_KM` |
| Raggio polare    | 6356.752 km            | occultation_analyzer  |
| Schiacciamento   | 1/298.257223563        | `WGS84_FLATTENING`    |
| Raggio medio     | 6371.01 km             | raggio volumetrico    |
| Obliquità J2000  | 23.4392911°            | `OBLIQUITY_J2000`     |
| GM Terra         | 398600.435436 km³/s²   | (force_model / SPICE) |

## Come aggiornare

1. JPL Horizons: https://ssd.jpl.nasa.gov/horizons/ → Ephemeris Type: Vectors, Target: 399 (Earth) o 433 (Eros), Time: JD desiderato; Reference frame: Ecliptic J2000.0; Output units: KM-S (poi convertire in AU, AU/day se serve).
2. IOTA: cercare occultazioni Eros documentate e salvare JSON con campi: star_ra_deg, star_dec_deg, central_time_utc, path_observed_km, duration_sec, path_width_km.
