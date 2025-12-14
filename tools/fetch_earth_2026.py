#!/usr/bin/env python3
import requests

def query_horizons_vectors(obj_id, epoch):
    """Query JPL Horizons for State Vectors"""
    base_url = "https://ssd.jpl.nasa.gov/api/horizons.api"
    
    params = {
        'format': 'text',
        'COMMAND': f"'{obj_id}'",
        'EPHEM_TYPE': 'VECTORS',
        'CENTER': "'@sun'", # Heliocentric
        'START_TIME': f"'{epoch}'",
        'STOP_TIME': f"'{epoch} 00:01'", 
        'STEP_SIZE': "'1 d'",
        'REF_PLANE': "'ECLIPTIC'", # To match my 'Ecliptic' output
        'REF_SYSTEM': "'ICRF'", # J2000
        'CSV_FORMAT': "'YES'",
        'VEC_TABLE': "'2'" # Position and Velocity
    }
    
    print(f"Querying Horizons VECTORS for {obj_id} at {epoch}...")
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
            print(f"RAW: {line}")

if __name__ == "__main__":
    # Earth (399)
    query_horizons_vectors('399', '2026-01-11 14:27') # Approx test time
