#!/usr/bin/env python3
"""
Query Horizons per posizione osservatore (geocentrico) di (704) Interamnia
"""
import urllib.parse
import urllib.request
import json
import ssl

# Disabilita SSL verification
ctx = ssl.create_default_context()
ctx.check_hostname = False
ctx.verify_mode = ssl.CERT_NONE

# Query per ephemeris osservatore
params = {
    'format': 'json',
    'COMMAND': '704',  # Interamnia
    'EPHEM_TYPE': 'OBSERVER',
    'CENTER': 'coord@399',  # Geocentrico
    'COORD_TYPE': 'GEODETIC',
    'SITE_COORD': '0,0,0',  # Centro Terra
    'START_TIME': '2024-12-10 02:30',
    'STOP_TIME': '2024-12-10 02:31',
    'STEP_SIZE': '1m',
    'QUANTITIES': '1,3',  # RA, Dec, distanza
    'CAL_FORMAT': 'CAL',
    'ANG_FORMAT': 'DEG',
}

url = 'https://ssd.jpl.nasa.gov/api/horizons.api?' + urllib.parse.urlencode(params)

try:
    print("Querying Horizons OBSERVER ephemeris...")
    with urllib.request.urlopen(url, timeout=30, context=ctx) as response:
        data = json.loads(response.read())
        result = data.get('result', '')
        
        # Trova sezione dati
        in_data = False
        for line in result.split('\n'):
            if '$$SOE' in line:
                in_data = True
                continue
            if '$$EOE' in line:
                break
            if in_data and line.strip():
                print(line)
                
except Exception as e:
    print(f"Error: {e}")
