#!/usr/bin/env python3
"""
Download ALL numbered asteroids from MPC and build complete database
Creates a comprehensive asteroid catalog for occultation predictions
"""

import requests
import json
import sys
import time
from pathlib import Path

# Configurazione
OUTPUT_FILE = Path.home() / ".ioccultcalc" / "data" / "all_numbered_asteroids.json"
MPC_API_BASE = "https://minorplanetcenter.net/web_service"
BATCH_SIZE = 100  # Asteroidi per batch
MAX_NUMBERED = 700000  # Circa 620,000 asteroidi numerati attualmente

def download_asteroid_data(number):
    """Scarica dati di un singolo asteroide dal MPC"""
    try:
        url = f"{MPC_API_BASE}/object/{number}/"
        response = requests.get(url, timeout=10)
        
        if response.status_code == 200:
            return response.json()
        elif response.status_code == 404:
            return None  # Asteroide non esiste
        else:
            print(f"  Errore {response.status_code} per asteroide {number}", file=sys.stderr)
            return None
    except Exception as e:
        print(f"  Errore download {number}: {e}", file=sys.stderr)
        return None

def download_batch(start_number, count):
    """Scarica un batch di asteroidi"""
    results = []
    for i in range(count):
        number = start_number + i
        if number > MAX_NUMBERED:
            break
            
        data = download_asteroid_data(number)
        if data:
            results.append(data)
        
        # Rate limiting: 1 richiesta al secondo
        time.sleep(1.0)
    
    return results

def main():
    print("=" * 70)
    print("DOWNLOAD CATALOGO COMPLETO ASTEROIDI NUMERATI")
    print("=" * 70)
    print(f"\nTarget: ~620,000 asteroidi numerati")
    print(f"Output: {OUTPUT_FILE}")
    print(f"Batch size: {BATCH_SIZE}")
    print("\nQUESTO RICHIEDERÀ MOLTE ORE (rate limit: 1 req/sec)")
    print("\nPremi Ctrl+C per interrompere e salvare il progresso")
    print("=" * 70)
    
    # Crea directory output
    OUTPUT_FILE.parent.mkdir(parents=True, exist_ok=True)
    
    # Carica eventuali dati esistenti
    all_asteroids = []
    start_from = 1
    
    if OUTPUT_FILE.exists():
        print(f"\n✓ Trovato file esistente: {OUTPUT_FILE}")
        with open(OUTPUT_FILE, 'r') as f:
            existing_data = json.load(f)
            if isinstance(existing_data, dict) and 'asteroids' in existing_data:
                all_asteroids = existing_data['asteroids']
            else:
                all_asteroids = existing_data
        
        if all_asteroids:
            # Trova ultimo numero scaricato
            numbers = [a.get('number', 0) for a in all_asteroids if 'number' in a]
            start_from = max(numbers) + 1 if numbers else 1
            print(f"✓ Già presenti {len(all_asteroids)} asteroidi")
            print(f"✓ Ripresa dal numero {start_from}")
    
    try:
        current = start_from
        total_downloaded = len(all_asteroids)
        
        while current <= MAX_NUMBERED:
            print(f"\n[{current:6d}] Scaricando batch {current}-{current+BATCH_SIZE-1}...", end="")
            sys.stdout.flush()
            
            batch = download_batch(current, BATCH_SIZE)
            all_asteroids.extend(batch)
            total_downloaded += len(batch)
            
            print(f" OK ({len(batch)} trovati)")
            print(f"  Totale: {total_downloaded} asteroidi")
            
            # Salva progresso ogni 10 batch
            if (current // BATCH_SIZE) % 10 == 0:
                print(f"  Salvataggio progresso...")
                save_database(all_asteroids, OUTPUT_FILE)
            
            current += BATCH_SIZE
            
    except KeyboardInterrupt:
        print("\n\n⚠️  Interruzione utente - salvataggio progresso...")
    
    # Salvataggio finale
    print(f"\n✓ Download completato!")
    print(f"✓ Totale asteroidi: {len(all_asteroids)}")
    save_database(all_asteroids, OUTPUT_FILE)
    
    # Statistiche finali
    print_statistics(all_asteroids)

def save_database(asteroids, output_file):
    """Salva database con metadati"""
    database = {
        "version": "1.0",
        "source": "Minor Planet Center (MPC)",
        "download_date": time.strftime("%Y-%m-%d %H:%M:%S"),
        "count": len(asteroids),
        "asteroids": asteroids
    }
    
    with open(output_file, 'w') as f:
        json.dump(database, f, indent=2)
    
    print(f"✓ Salvato: {output_file} ({len(asteroids)} asteroidi)")

def print_statistics(asteroids):
    """Stampa statistiche del catalogo"""
    print("\n" + "=" * 70)
    print("STATISTICHE CATALOGO")
    print("=" * 70)
    
    numbers = [a.get('number', 0) for a in asteroids if 'number' in a]
    if numbers:
        print(f"Asteroidi numerati: {len(numbers)}")
        print(f"Range: {min(numbers)} - {max(numbers)}")
    
    # Conteggio per fascia orbitale
    main_belt = sum(1 for a in asteroids if 2.0 <= a.get('a', 0) <= 3.5)
    near_earth = sum(1 for a in asteroids if a.get('q', 999) < 1.3)
    trojans = sum(1 for a in asteroids if 5.0 <= a.get('a', 0) <= 5.5)
    
    print(f"\nDistribuzione orbitale:")
    print(f"  Main Belt (2.0-3.5 AU): {main_belt}")
    print(f"  Near-Earth (q < 1.3 AU): {near_earth}")
    print(f"  Trojani (5.0-5.5 AU): {trojans}")
    
    # Diametri conosciuti
    with_diameter = sum(1 for a in asteroids if 'diameter' in a and a['diameter'] > 0)
    print(f"\nDiametri conosciuti: {with_diameter} ({with_diameter*100/len(asteroids):.1f}%)")
    
    print("=" * 70)

if __name__ == "__main__":
    main()
