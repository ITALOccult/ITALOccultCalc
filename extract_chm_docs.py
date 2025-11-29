#!/usr/bin/env python3
"""
Estrae la documentazione dal file CHM di Occult4
"""

import subprocess
import sys
import os

def extract_with_strings(chm_file, output_dir):
    """Estrae il contenuto usando il comando strings"""
    print(f"Estraendo contenuto da {chm_file}...")
    
    # Crea directory output
    os.makedirs(output_dir, exist_ok=True)
    
    # Estrai tutto il testo
    cmd = f"strings '{chm_file}'"
    result = subprocess.run(cmd, shell=True, capture_output=True, text=True)
    
    if result.returncode != 0:
        print(f"Errore nell'esecuzione di strings: {result.stderr}")
        return False
    
    content = result.stdout
    
    # Salva tutto il contenuto
    all_content_file = os.path.join(output_dir, "all_content.txt")
    with open(all_content_file, 'w', encoding='utf-8', errors='ignore') as f:
        f.write(content)
    print(f"✓ Salvato contenuto completo in: {all_content_file}")
    
    # Cerca riferimenti a Occelmnt
    occelmnt_lines = []
    lines = content.split('\n')
    
    for i, line in enumerate(lines):
        if 'occelmnt' in line.lower() or 'xml' in line.lower():
            # Prendi contesto (10 righe prima e dopo)
            start = max(0, i - 10)
            end = min(len(lines), i + 11)
            context = lines[start:end]
            occelmnt_lines.extend(context)
            occelmnt_lines.append("-" * 80)
    
    if occelmnt_lines:
        occelmnt_file = os.path.join(output_dir, "occelmnt_references.txt")
        with open(occelmnt_file, 'w', encoding='utf-8', errors='ignore') as f:
            f.write('\n'.join(occelmnt_lines))
        print(f"✓ Salvati riferimenti Occelmnt in: {occelmnt_file}")
    
    # Cerca pattern XML
    xml_patterns = []
    in_xml = False
    xml_block = []
    
    for line in lines:
        if line.strip().startswith('<') and not line.strip().startswith('</html'):
            in_xml = True
            xml_block = [line]
        elif in_xml:
            xml_block.append(line)
            if line.strip().startswith('</') or len(xml_block) > 50:
                if len(xml_block) > 5:  # Solo blocchi significativi
                    xml_patterns.append('\n'.join(xml_block))
                    xml_patterns.append("-" * 80)
                xml_block = []
                in_xml = False
    
    if xml_patterns:
        xml_file = os.path.join(output_dir, "xml_patterns.txt")
        with open(xml_file, 'w', encoding='utf-8', errors='ignore') as f:
            f.write('\n'.join(xml_patterns))
        print(f"✓ Salvati pattern XML in: {xml_file}")
    
    return True

def try_7z_extraction(chm_file, output_dir):
    """Prova ad estrarre con 7z se disponibile"""
    try:
        # Verifica se 7z è disponibile
        result = subprocess.run(['which', '7z'], capture_output=True)
        if result.returncode != 0:
            print("7z non disponibile, uso metodo alternativo...")
            return False
        
        print("Estraendo con 7z...")
        cmd = f"7z x -o'{output_dir}' '{chm_file}'"
        result = subprocess.run(cmd, shell=True, capture_output=True, text=True)
        
        if result.returncode == 0:
            print(f"✓ Estratto contenuto in: {output_dir}")
            return True
        else:
            print(f"Errore 7z: {result.stderr}")
            return False
    except Exception as e:
        print(f"Errore: {e}")
        return False

def search_occelmnt_docs(output_dir):
    """Cerca specificamente la documentazione Occelmnt"""
    print("\nCercando documentazione Occelmnt...")
    
    # Cerca file HTML
    html_files = []
    for root, dirs, files in os.walk(output_dir):
        for file in files:
            if 'occelmnt' in file.lower():
                html_files.append(os.path.join(root, file))
    
    if html_files:
        print(f"\n✓ Trovati {len(html_files)} file Occelmnt:")
        for f in html_files:
            print(f"  - {f}")
            # Leggi e stampa contenuto
            try:
                with open(f, 'r', encoding='utf-8', errors='ignore') as file:
                    content = file.read()
                    print(f"\n{'='*80}")
                    print(f"Contenuto di {os.path.basename(f)}:")
                    print('='*80)
                    print(content[:2000])  # Primi 2000 caratteri
                    print('...\n')
            except Exception as e:
                print(f"  Errore lettura: {e}")

def main():
    chm_file = os.path.expanduser("~/Downloads/OccultInstaller/Occult.chm")
    output_dir = os.path.expanduser("~/VisualStudio Code/GitHub/IOccultCalc/occult4_docs")
    
    if not os.path.exists(chm_file):
        print(f"Errore: File CHM non trovato in {chm_file}")
        sys.exit(1)
    
    print("="*80)
    print("ESTRAZIONE DOCUMENTAZIONE OCCULT4 CHM")
    print("="*80)
    print(f"File: {chm_file}")
    print(f"Output: {output_dir}\n")
    
    # Prova prima con 7z
    success = try_7z_extraction(chm_file, output_dir)
    
    if not success:
        # Fallback a strings
        success = extract_with_strings(chm_file, output_dir)
    
    if success:
        search_occelmnt_docs(output_dir)
        print("\n" + "="*80)
        print("✓ ESTRAZIONE COMPLETATA")
        print("="*80)
        print(f"\nVerifica i file in: {output_dir}")
        print("\nFile chiave:")
        print(f"  - all_content.txt (tutto il testo estratto)")
        print(f"  - occelmnt_references.txt (riferimenti Occelmnt)")
        print(f"  - xml_patterns.txt (pattern XML trovati)")
    else:
        print("\n✗ Estrazione fallita")
        sys.exit(1)

if __name__ == "__main__":
    main()
