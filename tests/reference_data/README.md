# Dati di riferimento per test avanzati (shadow path, occultazioni)

## File

- **eros_occultation_YYYYMMDD.json** – Dati occultazione storica 433 Eros (da IOTA/asteroidoccultation.com). Formato: coordinate stella Gaia DR3, path osservato, tempo centrale, durata.
- **jpl_horizons_eros_jd2459000.5.txt** – Posizione Eros da JPL Horizons per JD 2459000.5 (confronto heliocentrico/geocentrico).
- **earth_rotation_test_points.csv** – Punti test per rotazione ITRF (lat, lon, alt_m).

## Come aggiornare

1. JPL Horizons: https://ssd.jpl.nasa.gov/horizons/ → Ephemeris Type: Vectors, Target: 433, Time: JD 2459000.5.
2. IOTA: cercare occultazioni Eros documentate e salvare JSON con campi: star_ra_deg, star_dec_deg, central_time_utc, path_observed_km, duration_sec, path_width_km.
