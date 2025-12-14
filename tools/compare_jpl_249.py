import requests
import sys
import math

# Calculated Values from italoccultcalc (Phase 1 Astrometric Check)
# RA: 21h 58m 30.136s
# Dec: -20d 10' 29.86"
CALC_RA_DEG = 329.62557
CALC_DEC_DEG = -20.17496

# JPL Horizons API Parameters
# Target: 249 (Ilse)
# Observer: 500 (Geocentric)
# Time: JD 2461051.602604
url = "https://ssd.jpl.nasa.gov/api/horizons.api"
params = {
    "format": "text",
    "COMMAND": "249;",
    "OBJ_DATA": "NO",
    "MAKE_EPHEM": "YES",
    "EPHEM_TYPE": "OBSERVER",
    "CENTER": "500@399",  # Geocentric
    "START_TIME": "JD2461051.602604",
    "STOP_TIME": "JD2461051.603",
    "STEP_SIZE": "1m",
    "QUANTITIES": "1",    # 1 = Astrometric RA/DEC
    "CSV_FORMAT": "YES"
}

def dms_to_deg(d, m, s):
    sign = -1 if d < 0 else 1
    return sign * (abs(d) + m/60.0 + s/3600.0)

def hms_to_deg(h, m, s):
    return (h + m/60.0 + s/3600.0) * 15.0

print(f"Fetching JPL Horizons Data for JD {params['START_TIME']}...")
try:
    response = requests.get(url, params=params)
    response.raise_for_status()
    data = response.text
    
    # Simple parsing of CSV output
    # $$SOE
    # 2461051.602604000, A.D. 2026-Jan-11 02:27:44.9856, 329.6255628, -20.1749537, ...
    # $$EOE
    
    start_tag = "$$SOE"
    end_tag = "$$EOE"
    
    start_idx = data.find(start_tag)
    end_idx = data.find(end_tag)
    print(data) # DEBUG: Print full data
    
    if start_idx != -1 and end_idx != -1:
        line = data[start_idx + len(start_tag):end_idx].strip()
        print(f"DEBUG: Parsed line: '{line}'")
        parts = line.split(',')
        if len(parts) >= 6: # Date, Empty, Empty, RA, Dec, ...
            try:
                jpl_date = parts[0].strip() # First column is date/JD usually, checking headers
                # Based on raw output: '2026-Jan-11 ..., , , 08 05 ...'
                # Indices: 0=Date, 1=Empty, 2=Empty, 3=RA, 4=Dec
                
                # Check if parts[0] is just date or if JD is separate.
                # Actually usage indicates: 2461051.6026... is sometimes first.
                # Let's inspect parts again from debug log.
                # DEBUG: Parsed line: '2026-Jan-11 02:27:44.986, , , 08 05 59.41, +31 19 39.4,'
                
                if 'Jan' in parts[0]:
                    jpl_date = parts[0]
                    ra_str = parts[3].strip()
                    dec_str = parts[4].strip()
                else:
                    jpl_date = parts[1] # Assuming standard CSV
                    ra_str = parts[3].strip() # Adjust indices as needed based on observation
                    dec_str = parts[4].strip()

                # Parse RA (HH MM SS.ss)
                ra_p = ra_str.split()
                jpl_ra = hms_to_deg(float(ra_p[0]), float(ra_p[1]), float(ra_p[2]))
                
                # Parse Dec (DD MM SS.ss)
                dec_p = dec_str.split()
                jpl_dec = dms_to_deg(float(dec_p[0]), float(dec_p[1]), float(dec_p[2]))
                
                print("\n=== COMPARISON RESULTS ===")
                print(f"JPL Date: {jpl_date}")
                print(f"{'':<15} {'RA (deg)':<15} {'DEC (deg)':<15}")
                print("-" * 45)
                print(f"{'Calculated':<15} {CALC_RA_DEG:<15.6f} {CALC_DEC_DEG:<15.6f}")
                print(f"{'JPL Horizons':<15} {jpl_ra:<15.6f} {jpl_dec:<15.6f}")
                
                diff_ra_deg = CALC_RA_DEG - jpl_ra
                # Handle 360 wraparound
                if diff_ra_deg > 180: diff_ra_deg -= 360
                if diff_ra_deg < -180: diff_ra_deg += 360
                
                diff_dec_deg = CALC_DEC_DEG - jpl_dec
                
                diff_ra_arcsec = diff_ra_deg * 3600.0 * math.cos(math.radians(jpl_dec))
                diff_dec_arcsec = diff_dec_deg * 3600.0
                
                total_error = math.sqrt(diff_ra_arcsec**2 + diff_dec_arcsec**2)
                
                print("-" * 45)
                print(f"Diff RA  : {diff_ra_deg*3600:.4f}'' (raw) -> {diff_ra_arcsec:.4f}'' (cos(delta))")
                print(f"Diff Dec : {diff_dec_arcsec:.4f}''")
                print(f"Total Err: {total_error:.4f}''")
                
                if total_error < 0.1:
                    print("\n[SUCCESS] Excellent agreement with JPL (< 0.1'')")
                elif total_error < 1.0:
                    print("\n[PASS] Good agreement with JPL (< 1.0'')")
                else:
                    print("\n[FAIL] Discrepancy > 1.0''")
            except Exception as ve:
                print(f"Error converting values: {ve}")
                print(f"Parts: {parts}")
                
        else:
            print(f"Error parsing JPL data line. Not enough parts: {len(parts)}")
            print(line)
    else:
        print("Could not find data in JPL response.")
        # print(data) # Already printed
        
except Exception as e:
    print(f"Error: {e}")
