#!/usr/bin/env python3
"""
Parse MPCORB.DAT from Minor Planet Center
Creates comprehensive JSON database of ALL numbered asteroids
"""

import gzip
import json
import sys
from pathlib import Path
from datetime import datetime

INPUT_FILE = Path.home() / ".ioccultcalc" / "data" / "MPCORB.DAT.gz"
OUTPUT_JSON = Path.home() / ".ioccultcalc" / "data" / "all_numbered_asteroids.json"

def parse_mpcorb_line(line):
    """
    Parse MPCORB.DAT format (MPC database)
    
    Format documentation: https://minorplanetcenter.net/iau/info/MPOrbitFormat.html
    
    Fixed-width columns:
    Col 1-7: Designation/Number
    Col 9-13: H (absolute magnitude)
    Col 15-19: G (slope parameter)
    Col 21-25: Epoch (packed format)
    Col 27-35: M (mean anomaly, deg)
    Col 38-46: Argument of perihelion (deg)
    Col 49-57: Long. of ascending node (deg)
    Col 60-68: Inclination (deg)
    Col 71-79: Eccentricity
    Col 81-91: Semimajor axis (AU)
    ...
    Col 167-194: Readable designation
    """
    
    if len(line) < 194:
        return None
    
    try:
        asteroid = {}
        
        # Designation (first 7 chars, might be number or provisional designation)
        desig = line[0:7].strip()
        
        # Check if it's a numbered asteroid
        if not desig or not desig[0].isdigit():
            return None  # Skip unnumbered asteroids
        
        # Extract number
        try:
            asteroid['number'] = int(desig)
            asteroid['designation'] = desig
        except ValueError:
            return None  # Not a pure number
        
        # H magnitude
        h_str = line[8:13].strip()
        asteroid['H'] = float(h_str) if h_str else 15.0
        
        # G slope parameter
        g_str = line[14:19].strip()
        asteroid['G'] = float(g_str) if g_str else 0.15
        
        # Epoch (packed MPC format - e.g., "K257B" = 2025-11-02)
        epoch_packed = line[20:25].strip()
        asteroid['epoch_packed'] = epoch_packed
        # Convert to JD (simplified - use packed date decoder if needed)
        asteroid['epoch'] = 2460000.0  # Approximate for recent epochs
        
        # Mean anomaly (M) - col 27-35
        m_str = line[26:35].strip()
        asteroid['M'] = float(m_str) if m_str else 0.0
        
        # Argument of perihelion (omega) - col 38-46
        omega_str = line[37:46].strip()
        asteroid['omega'] = float(omega_str) if omega_str else 0.0
        
        # Longitude of ascending node (Omega) - col 49-57
        Omega_str = line[48:57].strip()
        asteroid['Omega'] = float(Omega_str) if Omega_str else 0.0
        
        # Inclination (i) - col 60-68
        i_str = line[59:68].strip()
        asteroid['i'] = float(i_str) if i_str else 0.0
        
        # Eccentricity (e) - col 71-79
        e_str = line[70:79].strip()
        asteroid['e'] = float(e_str) if e_str else 0.0
        
        # Semimajor axis (a) - col 93-103 (11 chars with spaces)
        a_str = line[92:103].strip()
        asteroid['a'] = float(a_str) if a_str else 0.0
        
        # Calculate perihelion and aphelion
        if asteroid['a'] > 0 and asteroid['e'] < 1.0:
            asteroid['q'] = asteroid['a'] * (1.0 - asteroid['e'])
            asteroid['Q'] = asteroid['a'] * (1.0 + asteroid['e'])
        else:
            asteroid['q'] = 0.0
            asteroid['Q'] = 0.0
        
        # Name (readable designation, columns 167-194)
        if len(line) >= 194:
            name = line[166:194].strip()
            if name:
                asteroid['name'] = name
            else:
                asteroid['name'] = f"Asteroid {asteroid['number']}"
        else:
            asteroid['name'] = f"Asteroid {asteroid['number']}"
        
        return asteroid
        
    except (ValueError, IndexError) as e:
        # Skip malformed lines
        return None

def parse_mpcorb_file(input_file):
    """Parse entire MPCORB.DAT file"""
    print(f"Parsing {input_file}...")
    
    try:
        with gzip.open(input_file, 'rt', encoding='latin-1') as f:
            lines = f.readlines()
    except Exception as e:
        print(f"✗ Errore lettura file: {e}", file=sys.stderr)
        sys.exit(1)
    
    print(f"✓ Lette {len(lines)} righe")
    
    asteroids = []
    header_lines = 0
    
    for i, line in enumerate(lines):
        # Skip header
        if line.startswith('-') or line.startswith('Des') or not line.strip():
            header_lines += 1
            continue
        
        asteroid = parse_mpcorb_line(line)
        if asteroid:
            asteroids.append(asteroid)
        
        if (i + 1) % 50000 == 0:
            print(f"  Processate {i+1:,} righe ({len(asteroids):,} asteroidi numerati)...", end="\r")
            sys.stdout.flush()
    
    print(f"\n✓ Parsing completato")
    print(f"  Righe header: {header_lines}")
    print(f"  Asteroidi numerati: {len(asteroids):,}")
    
    return asteroids

def save_database(asteroids, output_file):
    """Save JSON database"""
    output_file.parent.mkdir(parents=True, exist_ok=True)
    
    # Sort by number
    asteroids.sort(key=lambda a: a['number'])
    
    database = {
        "version": "1.0",
        "source": "Minor Planet Center (MPC)",
        "url": "https://minorplanetcenter.net/iau/MPCORB/MPCORB.DAT",
        "download_date": datetime.now().isoformat(),
        "count": len(asteroids),
        "description": "Complete catalog of all numbered asteroids with orbital elements",
        "asteroids": asteroids
    }
    
    print(f"\nSalvataggio: {output_file}")
    with open(output_file, 'w') as f:
        json.dump(database, f, indent=2)
    
    file_size_mb = output_file.stat().st_size / (1024 * 1024)
    print(f"✓ Salvato: {file_size_mb:.1f} MB")

def print_statistics(asteroids):
    """Print catalog statistics"""
    print("\n" + "=" * 70)
    print("STATISTICHE CATALOGO COMPLETO")
    print("=" * 70)
    
    numbers = [a['number'] for a in asteroids]
    print(f"Asteroidi numerati totali: {len(numbers):,}")
    print(f"Range numeri: {min(numbers):,} - {max(numbers):,}")
    
    # Orbital distribution
    main_belt = sum(1 for a in asteroids if 2.0 <= a['a'] <= 3.5)
    inner = sum(1 for a in asteroids if 0 < a['a'] < 2.0)
    outer = sum(1 for a in asteroids if a['a'] > 3.5)
    near_earth = sum(1 for a in asteroids if 0 < a['q'] < 1.3)
    trojans = sum(1 for a in asteroids if 4.6 <= a['a'] <= 5.5)
    
    print(f"\nDistribuzione orbitale:")
    print(f"  Main Belt (2.0-3.5 AU): {main_belt:,} ({main_belt*100/len(asteroids):.1f}%)")
    print(f"  Inner (<2.0 AU): {inner:,} ({inner*100/len(asteroids):.1f}%)")
    print(f"  Outer (>3.5 AU): {outer:,} ({outer*100/len(asteroids):.1f}%)")
    print(f"  Near-Earth (q<1.3 AU): {near_earth:,} ({near_earth*100/len(asteroids):.1f}%)")
    print(f"  Trojani Giove (4.6-5.5 AU): {trojans:,} ({trojans*100/len(asteroids):.1f}%)")
    
    # Magnitude
    h_values = [a['H'] for a in asteroids if 'H' in a and a['H'] < 90]
    if h_values:
        print(f"\nMagnitudine H:")
        print(f"  Range: {min(h_values):.1f} - {max(h_values):.1f}")
        print(f"  Media: {sum(h_values)/len(h_values):.1f}")
        very_bright = sum(1 for h in h_values if h < 10.0)
        bright = sum(1 for h in h_values if 10.0 <= h < 12.0)
        medium = sum(1 for h in h_values if 12.0 <= h < 14.0)
        print(f"  Molto luminosi (H<10): {very_bright:,} ({very_bright*100/len(h_values):.1f}%)")
        print(f"  Luminosi (10≤H<12): {bright:,} ({bright*100/len(h_values):.1f}%)")
        print(f"  Medi (12≤H<14): {medium:,} ({medium*100/len(h_values):.1f}%)")
    
    # Top 30 largest (estimated from H)
    sorted_by_h = sorted(asteroids, key=lambda a: a.get('H', 99))[:30]
    print(f"\nTop 30 più grandi (stimati da H magnitude):")
    for i, ast in enumerate(sorted_by_h, 1):
        name = ast.get('name', f"#{ast['number']}")
        a = ast['a']
        e = ast['e']
        print(f"  {i:2d}. ({ast['number']:6d}) {name:20s}: H={ast['H']:5.2f}  a={a:.3f} AU  e={e:.3f}")
    
    print("=" * 70)

def main():
    print("=" * 70)
    print("PARSING MPCORB.DAT - CATALOGO COMPLETO ASTEROIDI")
    print("=" * 70)
    print()
    
    if not INPUT_FILE.exists():
        print(f"✗ File non trovato: {INPUT_FILE}", file=sys.stderr)
        print("Scaricalo con:", file=sys.stderr)
        print("  curl -L -o ~/.ioccultcalc/data/MPCORB.DAT.gz https://minorplanetcenter.net/iau/MPCORB/MPCORB.DAT.gz")
        sys.exit(1)
    
    # Parse
    asteroids = parse_mpcorb_file(INPUT_FILE)
    
    if not asteroids:
        print("✗ Nessun asteroide numerato trovato!", file=sys.stderr)
        sys.exit(1)
    
    # Save
    save_database(asteroids, OUTPUT_JSON)
    
    # Statistics
    print_statistics(asteroids)
    
    print(f"\n✓ Catalogo completo pronto: {OUTPUT_JSON}")
    print(f"✓ Pronto per l'uso con italoccultcalc")

if __name__ == "__main__":
    main()
