#!/usr/bin/env python3
"""
Script per scaricare file .eq1 e .rwo da AstDyS per asteroidi in lista IOccultCalc

Uso:
    python3 download_astdys_data.py asteroids_list.txt
    python3 download_astdys_data.py asteroids_list.txt --output-dir astdys_data
    python3 download_astdys_data.py --help

Input supportati:
    - Lista semplice (un numero per riga): 433, 11234, 203
    - Output IOccultCalc con occultazioni
    - File con designazioni MPC
"""

import sys
import os
import argparse
import time
import re
import ssl
from urllib.request import urlopen, Request
from urllib.error import HTTPError, URLError
from pathlib import Path

# Crea contesto SSL che accetta certificati
ssl_context = ssl.create_default_context()
ssl_context.check_hostname = False
ssl_context.verify_mode = ssl.CERT_NONE

# URL base AstDyS
ASTDYS_BASE_URL = "https://newton.spacedys.com/~astdys2"

def extract_asteroid_numbers(filepath):
    """
    Estrae numeri asteroidi da vari formati di file
    
    Supporta:
    - Lista semplice: un numero per riga
    - Output IOccultCalc: cerca pattern (NUM) o NUM
    - Designazioni provvisorie: 1999 JS82 → cerca database
    """
    asteroids = set()
    
    with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith('#') or line.startswith('!'):
                continue
            
            # Pattern 1: (NUM) Nome - formato IOccultCalc
            match = re.search(r'\((\d+)\)', line)
            if match:
                asteroids.add(int(match.group(1)))
                continue
            
            # Pattern 2: NUM all'inizio linea
            match = re.match(r'^(\d+)\s', line)
            if match:
                asteroids.add(int(match.group(1)))
                continue
            
            # Pattern 3: Solo numero
            if line.isdigit():
                asteroids.add(int(line))
                continue
            
            # Pattern 4: Designazione provvisoria (es: 1999 JS82)
            # TODO: richiederebbe lookup su database MPC
    
    return sorted(list(asteroids))

def download_file(url, output_path, timeout=30):
    """
    Scarica file da URL con retry e gestione errori
    
    Returns:
        bool: True se successo, False altrimenti
    """
    try:
        # User agent per evitare blocchi
        headers = {
            'User-Agent': 'IOccultCalc/1.0 (Asteroid occultation prediction software)'
        }
        req = Request(url, headers=headers)
        
        with urlopen(req, timeout=timeout, context=ssl_context) as response:
            content = response.read()
            
            # Verifica che non sia pagina errore HTML
            if content.startswith(b'<!DOCTYPE') or content.startswith(b'<html'):
                return False
            
            # Salva file
            output_path.parent.mkdir(parents=True, exist_ok=True)
            with open(output_path, 'wb') as f:
                f.write(content)
            
            return True
            
    except HTTPError as e:
        if e.code == 404:
            print(f"    ✗ Non trovato (404)")
        else:
            print(f"    ✗ Errore HTTP {e.code}: {e.reason}")
        return False
    except URLError as e:
        print(f"    ✗ Errore URL: {e.reason}")
        return False
    except Exception as e:
        print(f"    ✗ Errore: {e}")
        return False

def download_eq1(asteroid_num, output_dir, force=False):
    """
    Scarica file .eq1 (elementi orbitali equinoziali)
    
    URL: https://newton.spacedys.com/~astdys2/epoch/numbered/XX/NUM.eq1
    dove XX = NUM // 1000
    
    Salva come: output_dir/NUM.eq1 (senza subdirectory)
    """
    output_path = output_dir / f"{asteroid_num}.eq1"
    
    if output_path.exists() and not force:
        print(f"  .eq1: già presente, skip")
        return True
    
    # Calcola subdirectory per URL (numero diviso per 1000)
    subdir = asteroid_num // 1000
    url = f"{ASTDYS_BASE_URL}/epoch/numbered/{subdir}/{asteroid_num}.eq1"
    print(f"  .eq1: downloading da {url}...")
    
    success = download_file(url, output_path)
    if success:
        # Verifica validità file (deve contenere "EQU")
        try:
            with open(output_path, 'r') as f:
                content = f.read()
                if 'EQU' not in content:
                    print(f"    ✗ File invalido (no EQU)")
                    output_path.unlink()
                    return False
            print(f"    ✓ Salvato: {output_path.name}")
            return True
        except Exception as e:
            print(f"    ✗ Errore validazione: {e}")
            return False
    
    return False

def download_rwo(asteroid_num, output_dir, force=False):
    """
    Scarica file .rwo (osservazioni ottiche)
    
    URL: https://newton.spacedys.com/~astdys2/mpcobs/numbered/XX/NUM.rwo
    dove XX = NUM // 1000
    
    Salva come: output_dir/NUM.rwo (senza subdirectory, formato originale completo)
    """
    output_path = output_dir / f"{asteroid_num}.rwo"
    
    if output_path.exists() and not force:
        print(f"  .rwo: già presente, skip")
        return True
    
    # Calcola subdirectory per URL (numero diviso per 1000)
    subdir = asteroid_num // 1000
    url = f"{ASTDYS_BASE_URL}/mpcobs/numbered/{subdir}/{asteroid_num}.rwo"
    print(f"  .rwo: downloading da {url}...")
    
    success = download_file(url, output_path)
    if success:
        # Verifica validità file (deve avere osservazioni)
        try:
            with open(output_path, 'r') as f:
                lines = f.readlines()
                # Conta righe totali (include header)
                obs_count = sum(1 for line in lines 
                              if line.strip() and not line.startswith('!'))
                if obs_count < 1:
                    print(f"    ✗ File invalido (nessuna osservazione)")
                    output_path.unlink()
                    return False
            file_size_kb = output_path.stat().st_size / 1024
            print(f"    ✓ Salvato: {output_path.name} ({file_size_kb:.1f} KB, ~{obs_count} linee)")
            return True
        except Exception as e:
            print(f"    ✗ Errore validazione: {e}")
            return False
    
    return False

def download_orbital_info(asteroid_num, output_dir, force=False):
    """
    DISABILITATO - Non più necessario
    Il file .cat non viene più scaricato per evitare file extra
    """
    return False

def generate_summary(asteroid_num, output_dir, results):
    """
    Genera file riassuntivo SOLO se richiesto (opzione --generate-summaries)
    Default: NON generare summary per mantenere solo .eq1 e .rwo originali
    """
    # DISABILITATO di default - verrà chiamato solo se args.generate_summaries=True
    return

def main():
    parser = argparse.ArgumentParser(
        description='Scarica file .eq1 e .rwo da AstDyS per lista asteroidi',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Esempi:
  %(prog)s asteroids.txt
  %(prog)s asteroids.txt -o test_astdys_download
  %(prog)s asteroids.txt --only-eq1
  %(prog)s asteroids.txt --force --delay 2

Formati input supportati:
  - Lista numeri (uno per riga): 433, 11234, 203
  - Output IOccultCalc con formato: (NUM) Nome
  - File testo con pattern (NUM) in qualsiasi posizione

URL AstDyS:
  .eq1: https://newton.spacedys.com/~astdys2/epoch/numbered/XX/NUM.eq1
  .rwo: https://newton.spacedys.com/~astdys2/mpcobs/numbered/XX/NUM.rwo
  dove XX = NUM // 1000 (es: 11234 → subdir 11)

Struttura output:
  test_astdys_download/
    ├─ 433.eq1       (elementi orbitali equinoziali, formato originale)
    ├─ 433.rwo       (osservazioni ottiche complete con header)
    ├─ 11234.eq1
    └─ 11234.rwo

Note:
  - Salva file .eq1 e .rwo ESATTAMENTE come su server AstDyS
  - NO file aggiuntivi (summary, .cat, ecc.) - solo .eq1 e .rwo
  - File già presenti sono skippati (usa --force per riscaricare)
  - Rispetta il server: usa --delay per liste grandi (>100 asteroidi)
        """
    )
    
    parser.add_argument('input_file', 
                       help='File con lista asteroidi (numeri o output IOccultCalc)')
    parser.add_argument('--output-dir', '-o', default='test_astdys_download',
                       help='Directory output (default: test_astdys_download)')
    parser.add_argument('--only-eq1', action='store_true',
                       help='Scarica solo file .eq1 (elementi orbitali)')
    parser.add_argument('--only-rwo', action='store_true',
                       help='Scarica solo file .rwo (osservazioni)')
    parser.add_argument('--force', '-f', action='store_true',
                       help='Forza download anche se file esiste')
    parser.add_argument('--delay', type=float, default=1.0,
                       help='Ritardo tra download (secondi, default: 1.0)')
    parser.add_argument('--max-asteroids', type=int,
                       help='Limita numero asteroidi processati (per test)')
    parser.add_argument('--verbose', '-v', action='store_true',
                       help='Output verboso')
    parser.add_argument('--keep-structure', action='store_true',
                       help='Mantieni struttura originale AstDyS (no summary files)')
    
    args = parser.parse_args()
    
    # Verifica file input
    if not os.path.exists(args.input_file):
        print(f"Errore: file {args.input_file} non trovato")
        return 1
    
    # Crea directory output
    output_dir = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    
    print("╔═══════════════════════════════════════════════════════════╗")
    print("║        AstDyS Data Downloader - IOccultCalc             ║")
    print("╚═══════════════════════════════════════════════════════════╝\n")
    
    # Estrai lista asteroidi
    print(f"📄 Parsing {args.input_file}...")
    asteroids = extract_asteroid_numbers(args.input_file)
    
    if not asteroids:
        print("✗ Nessun asteroide trovato nel file")
        return 1
    
    if args.max_asteroids:
        asteroids = asteroids[:args.max_asteroids]
    
    print(f"✓ Trovati {len(asteroids)} asteroidi: {asteroids[:10]}{'...' if len(asteroids) > 10 else ''}\n")
    
    # Statistiche
    stats = {
        'total': len(asteroids),
        'eq1_success': 0,
        'eq1_failed': 0,
        'rwo_success': 0,
        'rwo_failed': 0,
        'both_success': 0
    }
    
    # Download
    print("📥 Inizio download...\n")
    
    for i, asteroid_num in enumerate(asteroids, 1):
        print(f"[{i}/{len(asteroids)}] Asteroide ({asteroid_num})")
        
        results = {'eq1': False, 'rwo': False}
        
        # Download .eq1 (elementi orbitali)
        if not args.only_rwo:
            results['eq1'] = download_eq1(asteroid_num, output_dir, args.force)
            if results['eq1']:
                stats['eq1_success'] += 1
            else:
                stats['eq1_failed'] += 1
            
            # Delay tra richieste
            if not args.only_eq1:
                time.sleep(args.delay / 2)
        
        # Download .rwo (osservazioni)
        if not args.only_eq1:
            results['rwo'] = download_rwo(asteroid_num, output_dir, args.force)
            if results['rwo']:
                stats['rwo_success'] += 1
            else:
                stats['rwo_failed'] += 1
        
        # Summary files disabilitati di default (mantieni solo .eq1 e .rwo originali)
        # if args.generate_summaries and (results['eq1'] or results['rwo']):
        #     generate_summary(asteroid_num, output_dir, results)
        
        if results['eq1'] and results['rwo']:
            stats['both_success'] += 1
        
        print()
        
        # Delay tra asteroidi (rispetta server)
        if i < len(asteroids):
            time.sleep(args.delay)
    
    # Riepilogo finale
    print("╔═══════════════════════════════════════════════════════════╗")
    print("║                    RIEPILOGO FINALE                      ║")
    print("╚═══════════════════════════════════════════════════════════╝\n")
    
    print(f"Asteroidi processati: {stats['total']}")
    print(f"\nFile .eq1 (elementi orbitali):")
    print(f"  ✓ Successo: {stats['eq1_success']}")
    print(f"  ✗ Falliti:  {stats['eq1_failed']}")
    
    print(f"\nFile .rwo (osservazioni):")
    print(f"  ✓ Successo: {stats['rwo_success']}")
    print(f"  ✗ Falliti:  {stats['rwo_failed']}")
    
    print(f"\nAsteroidi con entrambi i file: {stats['both_success']}")
    
    print(f"\n📁 File salvati in: {output_dir.absolute()}")
    
    # Verifica file scaricati
    eq1_files = list(output_dir.glob("*.eq1"))
    rwo_files = list(output_dir.glob("*.rwo"))
    
    print(f"\n✓ File .eq1 scaricati: {len(eq1_files)}")
    print(f"✓ File .rwo scaricati: {len(rwo_files)}")
    
    if eq1_files or rwo_files:
        print(f"\n💡 I file sono pronti per essere usati da AstDysClient:")
        print(f"   client.setLocalEQ1Directory(\"{output_dir.absolute()}\")")
        print(f"   client.setLocalRWODirectory(\"{output_dir.absolute()}\")")
    
    return 0

if __name__ == '__main__':
    sys.exit(main())
