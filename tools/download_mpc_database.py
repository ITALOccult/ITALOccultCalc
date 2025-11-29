#!/usr/bin/env python3
"""
Download and convert MPC Extended JSON to IOccultCalc database format

Usage:
    python3 download_mpc_database.py [--output PATH] [--filter CONDITION]
    
Examples:
    # Download full database
    python3 download_mpc_database.py
    
    # Filter large asteroids only
    python3 download_mpc_database.py --filter "diameter > 50"
    
    # Custom output path
    python3 download_mpc_database.py --output ~/my_asteroid_db.json
"""

import json
import gzip
import urllib.request
import sys
import os
from pathlib import Path
import argparse
from datetime import datetime

MPC_URL = "https://minorplanetcenter.net/Extended_Files/mpcorb_extended.json.gz"

def download_with_progress(url, filename):
    """Download file with progress bar"""
    print(f"Downloading from {url}")
    print(f"Saving to {filename}")
    
    def reporthook(count, block_size, total_size):
        percent = min(100, int(count * block_size * 100 / total_size))
        sys.stdout.write(f"\r[{'=' * (percent // 2)}{' ' * (50 - percent // 2)}] {percent}%")
        sys.stdout.flush()
    
    try:
        urllib.request.urlretrieve(url, filename, reporthook)
        sys.stdout.write("\n")
        return True
    except Exception as e:
        print(f"\nError downloading: {e}")
        return False

def parse_filter(filter_str):
    """Parse simple filter condition"""
    # Simple parser for basic filters like "diameter > 50"
    parts = filter_str.split()
    if len(parts) != 3:
        return None
    
    field, op, value = parts
    try:
        value = float(value)
    except:
        pass  # Keep as string for orbit_class, etc.
    
    return (field, op, value)

def apply_filter(asteroid, filter_condition):
    """Apply filter to asteroid properties"""
    if not filter_condition:
        return True
    
    field, op, value = filter_condition
    
    if field not in asteroid:
        return False
    
    ast_value = asteroid[field]
    
    if op == '>':
        return ast_value > value
    elif op == '<':
        return ast_value < value
    elif op == '>=':
        return ast_value >= value
    elif op == '<=':
        return ast_value <= value
    elif op == '==':
        return ast_value == value
    elif op == '!=':
        return ast_value != value
    
    return True

def convert_mpc_to_ioccultcalc(mpc_file, output_file, filter_str=None):
    """Convert MPC extended JSON to IOccultCalc format"""
    print(f"Reading MPC data from {mpc_file}")
    
    filter_condition = parse_filter(filter_str) if filter_str else None
    
    # Read MPC JSON (may be very large!)
    try:
        with gzip.open(mpc_file, 'rt') as f:
            mpc_data = json.load(f)
    except:
        # Try uncompressed
        with open(mpc_file, 'r') as f:
            mpc_data = json.load(f)
    
    print(f"Processing {len(mpc_data)} asteroids...")
    
    # Convert to IOccultCalc format
    asteroids = {}
    total = 0
    filtered = 0
    
    for item in mpc_data:
        total += 1
        
        # Extract relevant fields
        ast = {
            "number": item.get("Number", 0),
            "name": item.get("Name", ""),
            "designation": item.get("Principal_desig", ""),
            "H": item.get("H", 99.0),
            "diameter": item.get("Diameter", 0.0),
            "albedo": item.get("Albedo", 0.0),
            "a": item.get("a", 0.0),
            "e": item.get("e", 0.0),
            "i": item.get("i", 0.0),
            "orbit_class": item.get("Orbit_type", ""),
            "spectral_type": item.get("Spec_T", ""),
            "rotation_period": 0.0  # Not in MPC data
        }
        
        # Apply filter
        if filter_condition and not apply_filter(ast, filter_condition):
            filtered += 1
            continue
        
        # Add to database
        number = ast["number"]
        if number > 0:  # Only numbered asteroids
            asteroids[str(number)] = ast
        
        # Progress
        if total % 10000 == 0:
            sys.stdout.write(f"\rProcessed {total} asteroids, kept {len(asteroids)}")
            sys.stdout.flush()
    
    sys.stdout.write(f"\rProcessed {total} asteroids, kept {len(asteroids)}\n")
    
    # Create IOccultCalc database structure
    db = {
        "metadata": {
            "version": "1.0",
            "source": "MPC_EXTENDED_JSON",
            "last_update": datetime.now().isoformat(),
            "total_asteroids": len(asteroids),
            "with_diameter": sum(1 for a in asteroids.values() if a["diameter"] > 0),
            "with_albedo": sum(1 for a in asteroids.values() if a["albedo"] > 0)
        },
        "asteroids": asteroids
    }
    
    # Write output
    print(f"Writing database to {output_file}")
    with open(output_file, 'w') as f:
        json.dump(db, f, indent=2)
    
    # Summary
    size_mb = os.path.getsize(output_file) / (1024 * 1024)
    print(f"\n✓ Database created successfully!")
    print(f"  Total asteroids: {len(asteroids)}")
    print(f"  File size: {size_mb:.1f} MB")
    if filter_condition:
        print(f"  Filtered out: {filtered}")
    
    return True

def main():
    parser = argparse.ArgumentParser(description='Download and convert MPC asteroid database')
    parser.add_argument('--output', '-o', 
                       help='Output path (default: ~/.ioccultcalc/database/asteroid_db.json)')
    parser.add_argument('--filter', '-f', 
                       help='Filter condition (e.g., "diameter > 50")')
    parser.add_argument('--skip-download', action='store_true',
                       help='Skip download, use existing file')
    
    args = parser.parse_args()
    
    # Determine paths
    home = Path.home()
    ioccultcalc_dir = home / '.ioccultcalc'
    database_dir = ioccultcalc_dir / 'database'
    cache_dir = ioccultcalc_dir / 'cache'
    
    # Create directories
    database_dir.mkdir(parents=True, exist_ok=True)
    cache_dir.mkdir(parents=True, exist_ok=True)
    
    output_path = args.output if args.output else str(database_dir / 'asteroid_db.json')
    mpc_file = str(cache_dir / 'mpcorb_extended.json.gz')
    
    # Download MPC data
    if not args.skip_download:
        if not download_with_progress(MPC_URL, mpc_file):
            print("Download failed!")
            return 1
    else:
        if not os.path.exists(mpc_file):
            print(f"Error: {mpc_file} not found. Remove --skip-download to download it.")
            return 1
    
    # Convert to IOccultCalc format
    if convert_mpc_to_ioccultcalc(mpc_file, output_path, args.filter):
        print(f"\nDatabase ready at: {output_path}")
        print("\nYou can now use IOccultCalc with:")
        print(f"  ./ioccultcalc_search --config survey.json <ra> <dec>")
        return 0
    else:
        print("Conversion failed!")
        return 1

if __name__ == '__main__':
    sys.exit(main())
