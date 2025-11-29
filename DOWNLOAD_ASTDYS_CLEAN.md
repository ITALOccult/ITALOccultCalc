# Download AstDyS Data - Script Aggiornato

Script Python per scaricare file `.eq1` (elementi orbitali) e `.rwo` (osservazioni) da AstDyS, salvandoli **esattamente come sul server** senza modifiche.

## 🎯 Caratteristiche

- ✅ Salva file `.eq1` e `.rwo` nel formato originale AstDyS
- ✅ Mantiene header completi (`END_OF_HEADER`)
- ✅ NO file aggiuntivi (summary, .cat, ecc.)
- ✅ Directory flat (no subdirectory, tutti i file in una cartella)
- ✅ Skip automatico file già presenti
- ✅ Validazione formato file

## 📥 Uso Base

```bash
# Download singolo asteroide
echo "433" > asteroids.txt
python3 download_astdys_data.py asteroids.txt

# Download lista asteroidi
python3 download_astdys_data.py asteroids_list.txt

# Specifica directory output
python3 download_astdys_data.py asteroids.txt -o test_astdys_download

# Forza re-download file esistenti
python3 download_astdys_data.py asteroids.txt --force

# Solo elementi orbitali (no osservazioni)
python3 download_astdys_data.py asteroids.txt --only-eq1

# Solo osservazioni (no elementi)
python3 download_astdys_data.py asteroids.txt --only-rwo
```

## 📁 Struttura Output

```
test_astdys_download/
  ├─ 433.eq1        ← Elementi orbitali equinoziali (formato OEF2.0)
  ├─ 433.rwo        ← Osservazioni ottiche complete con header
  ├─ 1.eq1
  ├─ 1.rwo
  ├─ 11234.eq1
  └─ 11234.rwo
```

**Caratteristiche file salvati:**

### File `.eq1` (Elementi Orbitali)
- Formato: OEF2.0 (Orbit Element Format 2.0)
- Contiene: Elementi equinoziali, epoca, covarianza, RMS
- Header: `format = 'OEF2.0'`, `END_OF_HEADER`
- Dimensione: ~1-2 KB

### File `.rwo` (Osservazioni Ottiche)
- Formato: Fixed-width Fortran (198 caratteri/linea)
- Contiene: Header metadata + osservazioni complete
- Header: `version = 2`, `errmod = 'fcct14'`, `END_OF_HEADER`
- Dimensione: variabile (da KB a MB a seconda del numero di osservazioni)

## 🔧 Integrazione con IOccultCalc

I file scaricati sono pronti per essere usati direttamente da `AstDysClient`:

```cpp
AstDysClient client;
client.setLocalEQ1Directory("/path/to/test_astdys_download");
client.setLocalRWODirectory("/path/to/test_astdys_download");

// Ora getElements() e getObservations() useranno i file locali
auto elements = client.getElements("433");
auto observations = client.getObservations("433");
```

Il `MPCClient::loadFromRWOFile()` parsa correttamente questi file:

```cpp
MPCClient mpcClient;
ObservationSet obsSet = mpcClient.loadFromRWOFile("test_astdys_download/433.rwo");
// Ritorna 16.103+ osservazioni parsate
```

## 📝 Formati Input Supportati

### Lista Semplice
```
433
1
11234
203
```

### Output IOccultCalc
```
(433) Eros - Occultazione il 2026-01-15
(11234) Asteroid - ...
```

### Pattern Generici
Qualsiasi file con pattern `(NUM)` o `NUM` all'inizio riga.

## ⚙️ Opzioni Avanzate

```bash
# Ritardo tra download (rispetta il server)
python3 download_astdys_data.py asteroids.txt --delay 2.0

# Limita numero asteroidi (test)
python3 download_astdys_data.py asteroids.txt --max-asteroids 5

# Output verboso
python3 download_astdys_data.py asteroids.txt --verbose

# Help completo
python3 download_astdys_data.py --help
```

## 🌐 URL AstDyS

Il download avviene da:

- **Elementi**: `https://newton.spacedys.com/~astdys2/epoch/numbered/XX/NUM.eq1`
- **Osservazioni**: `https://newton.spacedys.com/~astdys2/mpcobs/numbered/XX/NUM.rwo`

dove `XX = NUM // 1000` (es: 11234 → subdirectory 11)

## ✅ Validazione File

Lo script verifica automaticamente:

### File `.eq1`
- Deve contenere `EQU` (elementi equinoziali)
- Formato OEF2.0 valido

### File `.rwo`
- Almeno 1 linea di osservazione
- Formato fixed-width corretto

File invalidi vengono **eliminati automaticamente**.

## 📊 Output Esempio

```
╔═══════════════════════════════════════════════════════════╗
║        AstDyS Data Downloader - IOccultCalc             ║
╚═══════════════════════════════════════════════════════════╝

📄 Parsing test_single_asteroid.txt...
✓ Trovati 1 asteroidi: [433]

📥 Inizio download...

[1/1] Asteroide (433)
  .eq1: downloading da https://newton.spacedys.com/~astdys2/epoch/numbered/0/433.eq1...
    ✓ Salvato: 433.eq1
  .rwo: downloading da https://newton.spacedys.com/~astdys2/mpcobs/numbered/0/433.rwo...
    ✓ Salvato: 433.rwo (3315.0 KB, ~17946 linee)

╔═══════════════════════════════════════════════════════════╗
║                    RIEPILOGO FINALE                      ║
╚═══════════════════════════════════════════════════════════╝

Asteroidi processati: 1

File .eq1 (elementi orbitali):
  ✓ Successo: 1
  ✗ Falliti:  0

File .rwo (osservazioni):
  ✓ Successo: 1
  ✗ Falliti:  0

Asteroidi con entrambi i file: 1

📁 File salvati in: /path/to/test_download_clean

✓ File .eq1 scaricati: 1
✓ File .rwo scaricati: 1

💡 I file sono pronti per essere usati da AstDysClient:
   client.setLocalEQ1Directory("/path/to/test_download_clean")
   client.setLocalRWODirectory("/path/to/test_download_clean")
```

## 🚀 Test Completato

```bash
# Test con (433) Eros
python3 download_astdys_data.py test_single_asteroid.txt -o test_download_clean

# Verifica file
ls -lh test_download_clean/
# -rw-r--r--  1.8K  433.eq1
# -rw-r--r--  3.2M  433.rwo

# Verifica header .rwo
head -5 test_download_clean/433.rwo
# version =   2
# errmod  = 'fcct14'
# RMSast  =   4.70870E-01
# RMSmag  =   4.79230E-01
# END_OF_HEADER

# Verifica formato .eq1
head -5 test_download_clean/433.eq1
# format  = 'OEF2.0'
# rectype = 'ML'
# refsys  = ECLM J2000
# END_OF_HEADER
# 433
```

## 📌 Note Importanti

1. **NO file extra**: Solo `.eq1` e `.rwo`, nessun summary o file aggiuntivo
2. **Formato originale**: File identici a quelli sul server AstDyS
3. **Header completi**: Tutti i file includono `END_OF_HEADER` e metadata
4. **Directory flat**: Tutti i file in una cartella, no subdirectory
5. **Compatibilità**: 100% compatibile con `MPCClient::loadFromRWOFile()` e `AstDysClient`

## 🐛 Modifiche Precedenti

**Vecchia versione** generava:
- ❌ File `NUM_summary.txt` per ogni asteroide
- ❌ File `asteroids_with_eq1.txt` con lista
- ❌ Suggerimenti di conversione con `eq1_to_preset.py`

**Nuova versione** (pulita):
- ✅ Solo file `.eq1` e `.rwo` originali
- ✅ Nessun file aggiuntivo
- ✅ Output minimalista e chiaro
- ✅ Pronto per uso diretto in C++

## 📖 Riferimenti

- **AstDyS**: https://newton.spacedys.com/~astdys2/
- **Formato OEF2.0**: Orbit Element Format 2.0
- **Parser C++**: `MPCClient::loadFromRWOFile()` in `src/mpc_client.cpp`
- **Test**: `examples/test_orbit_fitting_integration.cpp`
