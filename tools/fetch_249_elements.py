#!/usr/bin/env python3
import requests

def query_horizons_elements(obj_id, epoch):
    """Query JPL Horizons for Orbital Elements"""
    base_url = "https://ssd.jpl.nasa.gov/api/horizons.api"
    
    # Epoch example: '2026-01-11'
    
    params = {
        'format': 'text',
        'COMMAND': f"'{obj_id}'",
        'EPHEM_TYPE': 'ELEMENTS',
        'CENTER': "'@sun'", # Heliocentric
        'START_TIME': f"'{epoch}'",
        'STOP_TIME': f"'{epoch} 00:01'", # Just one point
        'STEP_SIZE': "'1 d'",
        'OUT_UNITS': "'AU-D'",
        'REF_PLANE': "'ECLIPTIC'",
        'REF_SYSTEM': "'ICRF'", # J2000
        'CSV_FORMAT': "'YES'"
    }
    
    print(f"Querying Horizons ELEMENTS for {obj_id} at {epoch}...")
    response = requests.get(base_url, params=params)
    text = response.text
    
    lines = text.split('\n')
    in_data = False
    
    for line in lines:
        if '$$SOE' in line:
            in_data = True
            continue
        if '$$EOE' in line:
            break
        if in_data and ',' in line:
            parts = line.split(',')
            # Format depends on EPHEM_TYPE=ELEMENTS
            # Usually: JD, ..., EC, QR, IN, OM, W, Tp, N, MA, TA, A, AD, PR, ....
            # Check Horizons manual or output header.
            # But let's print raw line to be sure.
            print(f"RAW: {line}")

if __name__ == "__main__":
    # Check 249 Ilse
    query_horizons_elements('249', '2026-01-11')
