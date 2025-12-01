# IOccultCalc - Guida all'Installazione

## 📦 Installazione Rapida

```bash
# Clone del repository
git clone https://github.com/manvalan/IOccultCalc.git
cd IOccultCalc

# Installazione standard in /usr/local
./install.sh

# Installazione utente in ~/.local (senza sudo)
./install.sh --user

# Installazione personalizzata
./install.sh --prefix /path/to/install
```

## 🔧 Opzioni di Installazione

### Opzioni Disponibili

```bash
./install.sh [options]
```

| Opzione | Descrizione |
|---------|-------------|
| `--prefix PATH` | Installa in un percorso personalizzato (default: `/usr/local`) |
| `--user` | Installa in `~/.local` (non richiede sudo) |
| `--dev` | Build di sviluppo con simboli di debug |
| `--clean` | Pulisce la directory build prima di compilare |
| `--build-only` | Solo compilazione, senza installazione |
| `--skip-tests` | Salta l'esecuzione dei test |
| `--help` | Mostra l'aiuto |

### Esempi

```bash
# Installazione standard di sistema
./install.sh

# Installazione solo per l'utente corrente
./install.sh --user

# Installazione in /opt/ioccultcalc
./install.sh --prefix /opt/ioccultcalc

# Build pulita in modalità sviluppo
./install.sh --clean --dev

# Solo compilazione senza installare
./install.sh --build-only
```

## 📋 Requisiti di Sistema

### Dipendenze Obbligatorie

- **CMake** ≥ 3.12
- **Compilatore C++17** (GCC, Clang, MSVC)
- **Git** (opzionale, per clonare il repository)

### Dipendenze Opzionali

- **OpenMP** - per il calcolo parallelo (raccomandato)
- **CSPICE** - scaricato automaticamente se non presente

### Installazione Dipendenze

#### macOS (Homebrew)

```bash
brew install cmake
brew install libomp  # OpenMP per calcolo parallelo
```

#### Ubuntu/Debian

```bash
sudo apt-get update
sudo apt-get install cmake g++ git
```

#### RHEL/CentOS/Fedora

```bash
sudo yum install cmake gcc-c++ git
```

## 🚀 Verifica Installazione

Dopo l'installazione, verifica che gli eseguibili siano disponibili:

```bash
# Verifica versione e percorso
which italoccultcalc
which occult_calc
which ioccultcalc_search

# Mostra l'aiuto
italoccultcalc --help
occult_calc --help
```

## 📁 Struttura Installazione

### Installazione Standard (`/usr/local`)

```
/usr/local/
├── bin/
│   ├── italoccultcalc          # Pipeline completa
│   ├── occult_calc             # Calcolatore base
│   └── ioccultcalc_search      # Tool di ricerca
├── lib/
│   └── libioccultcalc.a        # Libreria statica
├── include/
│   └── ioccultcalc/            # Header files
│       ├── ioccultcalc.h
│       ├── output_manager.h
│       ├── asteroid_filter.h
│       └── ...
└── share/
    ├── ioccultcalc/
    │   ├── presets/            # File .oop di configurazione
    │   │   ├── preset_quick_combined.oop
    │   │   ├── preset_production_monthly.oop
    │   │   └── ...
    │   └── data/               # Dati di esempio
    │       ├── example_priority_asteroids.txt
    │       ├── localasteroid
    │       └── ...
    └── doc/ioccultcalc/        # Documentazione
        ├── README.md
        ├── GUIDA_FILE_OOP.md
        ├── COMBINED_LISTS_IMPLEMENTATION.md
        └── ...
```

### Installazione Utente (`~/.local`)

Stessa struttura ma in `~/.local` invece di `/usr/local`.

## 🎯 Quick Start

### 1. Calcolo Singola Occultazione

```bash
# Eros il 15 giugno 2026
occult_calc 433 2026-06-15
```

### 2. Survey Mensile Standard

```bash
# Survey di gennaio 2026 con configurazione ottimizzata
italoccultcalc --preset /usr/local/share/ioccultcalc/presets/preset_production_monthly.oop
```

### 3. Liste Combinate di Asteroidi

```bash
# Range [1-1000] + file locale
italoccultcalc --preset /usr/local/share/ioccultcalc/presets/preset_quick_combined.oop
```

## 🔄 Aggiornamento

Per aggiornare IOccultCalc:

```bash
cd IOccultCalc
git pull
./install.sh --clean
```

## 🗑️ Disinstallazione

```bash
# Disinstallazione standard
./uninstall.sh

# Disinstallazione da percorso personalizzato
./uninstall.sh --prefix /path/to/install
```

Lo script di disinstallazione rimuove:
- Tutti gli eseguibili da `bin/`
- La libreria da `lib/`
- Gli header da `include/`
- I preset e dati da `share/`
- La documentazione da `share/doc/`

## 💻 Sviluppo

### Build per Sviluppo

```bash
# Build di debug senza installare
./install.sh --dev --build-only

# Esegui i test
cd build
ctest --verbose

# Esegui test specifici
./tests/test_combined_lists
./tests/test_astnum_output
```

### Uso della Libreria

Per usare IOccultCalc nei tuoi progetti C++:

```cpp
#include <ioccultcalc/ioccultcalc.h>
#include <ioccultcalc/output_manager.h>

int main() {
    // Il tuo codice qui
    return 0;
}
```

Compilazione:

```bash
g++ -std=c++17 \
    -I/usr/local/include \
    myapp.cpp \
    -L/usr/local/lib \
    -lioccultcalc \
    -o myapp
```

## 🐛 Risoluzione Problemi

### CMake non trova il compilatore

```bash
# Specifica manualmente il compilatore
export CXX=/usr/bin/clang++
./install.sh
```

### Errori di permessi

```bash
# Usa installazione utente invece di sistema
./install.sh --user
```

### OpenMP non trovato (macOS)

```bash
brew install libomp
./install.sh --clean
```

### Path non configurato

Aggiungi al tuo `~/.bashrc` o `~/.zshrc`:

```bash
export PATH="/usr/local/bin:$PATH"
# oppure per installazione utente
export PATH="$HOME/.local/bin:$PATH"
```

Poi ricarica:

```bash
source ~/.bashrc  # o ~/.zshrc
```

## 📚 Documentazione Completa

Dopo l'installazione, consulta la documentazione completa in:

```bash
/usr/local/share/doc/ioccultcalc/
```

Guide principali:
- `GUIDA_FILE_OOP.md` - Guida ai file .oop in italiano
- `COMBINED_LISTS_IMPLEMENTATION.md` - Liste combinate di asteroidi
- `PRESET_GUIDE.md` - Guida ai preset
- `CONFIGURATION_GUIDE.md` - Configurazione completa

## 🤝 Supporto

- **Issues**: https://github.com/manvalan/IOccultCalc/issues
- **Documentazione**: Nel repository e dopo l'installazione
- **Esempi**: In `share/ioccultcalc/data/` e `share/ioccultcalc/presets/`
