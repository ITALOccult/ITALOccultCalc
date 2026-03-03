#!/usr/bin/env python3
"""
Scarica stato 433 Eros (o altro asteroide) da JPL Horizons per JD 2461109.5
e scrive jpl_horizons_asteroid_2026mar10.txt per test_jpl_consistency_full.

Uso: python3 fetch_asteroid_jpl.py [numero_asteroide]
Default: 433 (Eros).

Richiede: requests (pip install requests) oppure urllib standard.
"""

import sys
import re
import urllib.request
import urllib.parse

TARGET_JD = 2461109.5  # 2026-Mar-10 00:00 TDB
OUTPUT_FILE = "jpl_horizons_asteroid_2026mar10.txt"

def main():
    body = sys.argv[1] if len(sys.argv) > 1 else "433"
    # Horizons API (GET)
    params = {
        "format": "text",
        "COMMAND": f"'{body}'",
        "OBJ_DATA": "NO",
        "MAKE_EPHEM": "YES",
        "EPHEM_TYPE": "VECTORS",
        "CENTER": "'10'",
        "START_TIME": "'2026-03-10 00:00'",
        "STOP_TIME": "'2026-03-11'",
        "STEP_SIZE": "'1 d'",
        "OUT_UNITS": "'AU-D'",
        "VEC_TABLE": "'2'",
        "REF_SYSTEM": "'ECLIPJ2000'",
        "REF_PLANE": "'ECLIPTIC'",
        "VEC_CORR": "'NONE'",
        "CSV_FORMAT": "NO",
    }
    url = "https://ssd.jpl.nasa.gov/api/horizons.api?" + urllib.parse.urlencode(params)
    try:
        with urllib.request.urlopen(url, timeout=30) as r:
            text = r.read().decode()
    except Exception as e:
        print("Errore richiesta Horizons:", e, file=sys.stderr)
        print("Generare manualmente:", OUTPUT_FILE, file=sys.stderr)
        print("  Horizons → Target", body, "→ Center 10 (Sun) → 2026-Mar-10 → Vectors, Ecliptic J2000, AU-D", file=sys.stderr)
        return 1

    # Parse: cerca blocco $$SOE ... $$EOE e righe X=, Y=, Z=, VX=, ...
    soe = text.find("$$SOE")
    eoe = text.find("$$EOE")
    if soe == -1 or eoe == -1:
        print("Risposta Horizons senza $$SOE/$$EOE", file=sys.stderr)
        return 1
    block = text[soe:eoe]
    # Formato tipico: "2461109.500000000 = ... \n X = ... Y = ... Z = ... \n VX = ..."
    jd_match = re.search(r"(\d+\.\d+)\s*=", block)
    x_match = re.search(r"X\s*=\s*([-\d.E+]+)", block)
    y_match = re.search(r"Y\s*=\s*([-\d.E+]+)", block)
    z_match = re.search(r"Z\s*=\s*([-\d.E+]+)", block)
    vx_match = re.search(r"VX\s*=\s*([-\d.E+]+)", block)
    vy_match = re.search(r"VY\s*=\s*([-\d.E+]+)", block)
    vz_match = re.search(r"VZ\s*=\s*([-\d.E+]+)", block)
    if not all([jd_match, x_match, y_match, z_match, vx_match, vy_match, vz_match]):
        print("Impossibile parsare X,Y,Z,VX,VY,VZ dalla risposta", file=sys.stderr)
        return 1
    jd = float(jd_match.group(1))
    x, y, z = float(x_match.group(1)), float(y_match.group(1)), float(z_match.group(1))
    vx, vy, vz = float(vx_match.group(1)), float(vy_match.group(1)), float(vz_match.group(1))
    # Se OUTPUT è in km e km/s, converti in AU e AU/day
    if abs(x) > 1e3:
        au_km = 149597870.7
        x, y, z = x / au_km, y / au_km, z / au_km
        kms_aud = 86400.0 / au_km
        vx, vy, vz = vx * kms_aud, vy * kms_aud, vz * kms_aud

    with open(OUTPUT_FILE, "w") as f:
        f.write("# JPL Horizons: asteroide " + body + " vs Sole, Ecliptic J2000\n")
        f.write("# JD " + str(jd) + " (2026-Mar-10 00:00 TDB), generato da fetch_asteroid_jpl.py\n\n")
        f.write("JD = " + str(jd) + "\n")
        f.write("X = " + str(x) + "\n")
        f.write("Y = " + str(y) + "\n")
        f.write("Z = " + str(z) + "\n")
        f.write("VX = " + str(vx) + "\n")
        f.write("VY = " + str(vy) + "\n")
        f.write("VZ = " + str(vz) + "\n")
    print("Scritto", OUTPUT_FILE)
    return 0

if __name__ == "__main__":
    sys.exit(main())
