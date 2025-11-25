#!/usr/bin/env python3
"""
Download ALL numbered asteroids from AstDyS
Creates comprehensive catalog with orbital elements for occultation predictions
Uses AstDyS bulk download (MOLTO più veloce del MPC API)
"""

import urllib.request
import gzip
import json
import sys
from pathlib import Path
from datetime import datetime

# Configurazione
ASTDYS_URL = "https://newton.spacedys.com/astdys/catalogs/allnum.cat.gz"
OUTPUT_JSON = Path.home() / ".ioccultcalc" / "data" / "all_numbered_asteroids.json"

def download_astdys_catalog():
    """Scarica il catalogo completo AstDyS (tutti gli asteroidi numerati)"""
    print(f"Download da AstDyS: {ASTDYS_URL}")
    print("(File ~60 MB compresso, ~300 MB decompresso)")
    
    try:
        # Download con progress
        response = urllib.request.urlopen(ASTDYS_URL)
        total_size = int(response.headers.get('content-length', 0))
        
        data = b''
        downloaded = 0
        chunk_size = 8192
        
        while True:
            chunk = response.read(chunk_size)
            if not chunk:
                break
            data += chunk
            downloaded += len(chunk)
            
            if total_size > 0:
                progress = downloaded * 100 / total_size
                print(f"\rDownload: {progress:.1f}% ({downloaded}/{total_size} bytes)", end="")
                sys.stdout.flush()
        
        print("\n✓ Download completato")
        return data
    
    except Exception as e:
        print(f"\n✗ Errore download: {e}", file=sys.stderr)
        sys.exit(1)

def parse_astdys_line(line):
    """
    Parsing del formato AstDyS allnum.cat
    
    Formato (fixed-width):
    Col 1-7: Number
    Col 9-26: Designation/Name
    Col 28-41: Epoch (MJD)
    Col 43-55: a (AU)
    Col 57-68: e
    Col 70-81: i (deg)
    Col 83-95: Omega (deg)
    Col 97-109: omega (deg)
    Col 111-123: M (deg)
    Col 125-130: H
    Col 132-136: G
    """
    
    if len(line) < 140:
        return None
    
    try:
        asteroid = {}
        
        # Number
        number_str = line[0:7].strip()
        if number_str:
            asteroid['number'] = int(number_str)
        else:
            return None
        
        # Name/Designation
        name = line[8:26].strip()
        if name:
            asteroid['name'] = name
            asteroid['designation'] = str(asteroid['number'])
        
        # Epoch (MJD to JD)
        epoch_mjd = float(line[27:41].strip())
        asteroid['epoch'] = epoch_mjd + 2400000.5
        
        # Orbital elements
        asteroid['a'] = float(line[42:55].strip())
        asteroid['e'] = float(line[56:68].strip())
        asteroid['i'] = float(line[69:81].strip())
        asteroid['Omega'] = float(line[82:95].strip())
        asteroid['omega'] = float(line[96:109].strip())
        asteroid['M'] = float(line[110:123].strip())
        
        # Absolute magnitude H
        h_str = line[124:130].strip()
        if h_str:
            asteroid['H'] = float(h_str)
        else:
            asteroid['H'] = 15.0  # default
        
        # Slope parameter G
        g_str = line[131:136].strip()
        if g_str:
            asteroid['G'] = float(g_str)
        else:
            asteroid['G'] = 0.15  # default
        
        # Calcola q e Q
        asteroid['q'] = asteroid['a'] * (1.0 - asteroid['e'])  # perihelion
        asteroid['Q'] = asteroid['a'] * (1.0 + asteroid['e'])  # aphelion
        
        return asteroid
    
    except (ValueError, IndexError) as e:
        # Riga malformata - skippa
        return None

def parse_astdys_catalog(data):
    """Parse del catalogo AstDyS completo"""
    print("\nParsing catalogo AstDyS...")
    
    # Decomprimi gzip
    try:
        catalog_text = gzip.decompress(data).decode('utf-8')
        print(f"✓ Decompresso: {len(catalog_text)} caratteri")
    except Exception as e:
        print(f"✗ Errore decompressione: {e}", file=sys.stderr)
        sys.exit(1)
    
    # Parse righe
    asteroids = []
    lines = catalog_text.split('\n')
    
    for i, line in enumerate(lines):
        if not line.strip() or line.startswith('#'):
            continue
        
        asteroid = parse_astdys_line(line)
        if asteroid:
            asteroids.append(asteroid)
        
        if (i + 1) % 10000 == 0:
            print(f"  Processate {i+1} righe ({len(asteroids)} asteroidi)...", end="\r")
            sys.stdout.flush()
    
    print(f"\n✓ Parsing completato: {len(asteroids)} asteroidi")
    return asteroids

def save_database(asteroids, output_file):
    """Salva database JSON con metadati"""
    output_file.parent.mkdir(parents=True, exist_ok=True)
    
    database = {
        "version": "1.0",
        "source": "AstDyS (University of Pisa)",
        "url": ASTDYS_URL,
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
    """Stampa statistiche catalogo"""
    print("\n" + "=" * 70)
    print("STATISTICHE CATALOGO COMPLETO")
    print("=" * 70)
    
    numbers = [a['number'] for a in asteroids]
    print(f"Asteroidi numerati totali: {len(numbers):,}")
    print(f"Range numeri: {min(numbers)} - {max(numbers)}")
    
    # Distribuzione per fascia orbitale
    main_belt = sum(1 for a in asteroids if 2.0 <= a['a'] <= 3.5)
    inner = sum(1 for a in asteroids if a['a'] < 2.0)
    outer = sum(1 for a in asteroids if a['a'] > 3.5)
    near_earth = sum(1 for a in asteroids if a['q'] < 1.3)
    trojans = sum(1 for a in asteroids if 4.6 <= a['a'] <= 5.5)
    
    print(f"\nDistribuzione orbitale:")
    print(f"  Main Belt (2.0-3.5 AU): {main_belt:,} ({main_belt*100/len(asteroids):.1f}%)")
    print(f"  Inner (<2.0 AU): {inner:,} ({inner*100/len(asteroids):.1f}%)")
    print(f"  Outer (>3.5 AU): {outer:,} ({outer*100/len(asteroids):.1f}%)")
    print(f"  Near-Earth (q<1.3 AU): {near_earth:,} ({near_earth*100/len(asteroids):.1f}%)")
    print(f"  Trojani (4.6-5.5 AU): {trojans:,} ({trojans*100/len(asteroids):.1f}%)")
    
    # Magnitudine
    h_values = [a['H'] for a in asteroids if 'H' in a]
    if h_values:
        print(f"\nMagnitudine H:")
        print(f"  Range: {min(h_values):.1f} - {max(h_values):.1f}")
        print(f"  Media: {sum(h_values)/len(h_values):.1f}")
        bright = sum(1 for h in h_values if h < 12.0)
        print(f"  Luminosi (H<12): {bright:,} ({bright*100/len(h_values):.1f}%)")
    
    # Top 20 più grandi (stimati da H)
    sorted_by_h = sorted(asteroids, key=lambda a: a.get('H', 99))[:20]
    print(f"\nTop 20 più grandi (H magnitude):")
    for i, ast in enumerate(sorted_by_h, 1):
        name = ast.get('name', f"Asteroid {ast['number']}")
        print(f"  {i:2d}. ({ast['number']}) {name}: H={ast['H']:.2f}")
    
    print("=" * 70)

def main():
    print("=" * 70)
    print("DOWNLOAD CATALOGO COMPLETO ASTEROIDI NUMERATI")
    print("Fonte: AstDyS (University of Pisa)")
    print("=" * 70)
    print()
    
    # Download
    compressed_data = download_astdys_catalog()
    
    # Parse
    asteroids = parse_astdys_catalog(compressed_data)
    
    if not asteroids:
        print("✗ Nessun asteroide trovato!", file=sys.stderr)
        sys.exit(1)
    
    # Salva
    save_database(asteroids, OUTPUT_JSON)
    
    # Statistiche
    print_statistics(asteroids)
    
    print(f"\n✓ Catalogo completo pronto: {OUTPUT_JSON}")
    print(f"✓ Usalo con: italoccultcalc --database {OUTPUT_JSON}")

if __name__ == "__main__":
    main()
