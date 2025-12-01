# IOccultCalc - Installation System

## 📦 Sistema di Installazione Completo

IOccultCalc dispone di **3 script di installazione** per diverse esigenze:

### 1. `install.sh` - Installazione Automatica Completa ⭐

**Uso consigliato**: Installazione sistema o utente con massima flessibilità

```bash
# Installazione standard in /usr/local
./install.sh

# Installazione utente (senza sudo)
./install.sh --user

# Installazione personalizzata
./install.sh --prefix /opt/ioccultcalc

# Solo compilazione
./install.sh --build-only
```

**Caratteristiche**:
- ✅ Installazione completa automatica
- ✅ Supporto multi-piattaforma (macOS, Linux)
- ✅ Verifica dipendenze automatica
- ✅ Build pulita con test
- ✅ Installazione eseguibili, libreria, header, preset, documentazione
- ✅ Configurazione PATH automatica
- ✅ 8 opzioni configurabili

**Output**:
```
/usr/local/
├── bin/                      # Eseguibili
│   ├── italoccultcalc
│   ├── occult_calc
│   └── ioccultcalc_search
├── lib/                      # Libreria
│   └── libioccultcalc.a
├── include/ioccultcalc/      # Header per sviluppo
└── share/
    ├── ioccultcalc/
    │   ├── presets/         # File .oop configurazione
    │   └── data/            # Dati esempio
    └── doc/ioccultcalc/     # Documentazione
```

### 2. `install_production.sh` - Setup Produzione

**Uso consigliato**: Prima installazione completa con cataloghi

```bash
./install_production.sh
```

**Caratteristiche**:
- ✅ Installazione utente in ~/.ioccultcalc
- ✅ Download catalogo Gaia Mag18 (9 GB)
- ✅ Download ephemeris DE440s
- ✅ Setup directory operative
- ✅ Crea launcher script
- ✅ Verifica completezza installazione

**Output**:
```
~/.ioccultcalc/
├── data/                     # Elementi orbitali
├── ephemerides/              # DE440s.bsp
├── output/                   # Risultati
└── presets/                  # Configurazioni

~/catalogs/
└── gaia_mag18.cat.gz        # Catalogo Gaia 9 GB
```

### 3. `build.sh` - Build Veloce

**Uso consigliato**: Sviluppo e test rapidi

```bash
./build.sh              # Build normale
./build.sh clean        # Clean + build
./build.sh test         # Build + test
```

**Caratteristiche**:
- ✅ Build rapida senza installazione
- ✅ Eseguibili in build/
- ✅ Ideale per sviluppo
- ✅ Test unitari integrati

## 🚀 Quick Start - Scegli il Tuo Scenario

### Scenario 1: Primo Utilizzo (Consigliato)

```bash
# 1. Clone repository
git clone https://github.com/manvalan/IOccultCalc.git
cd IOccultCalc

# 2. Installazione produzione completa
./install_production.sh

# 3. Download catalogo Gaia (una tantum)
./download_gaia_cache.sh

# 4. Prima survey
./italoccultcalc.sh preset_production_monthly.oop
```

### Scenario 2: Installazione Sistema

```bash
# Installazione globale con sudo
./install.sh

# Verifica
italoccultcalc --help

# Usa da qualsiasi directory
italoccultcalc --preset /usr/local/share/ioccultcalc/presets/preset_quick_combined.oop
```

### Scenario 3: Installazione Utente (No Sudo)

```bash
# Installazione in ~/.local
./install.sh --user

# Aggiungi al PATH (~/.bashrc o ~/.zshrc)
export PATH="$HOME/.local/bin:$PATH"

# Verifica
source ~/.zshrc
italoccultcalc --help
```

### Scenario 4: Sviluppo

```bash
# Build veloce
./build.sh

# Test
./build.sh test

# Esegui esempi
./build/examples/example_basic 433
./build/examples/example_combined_asteroid_lists
```

## 📋 Opzioni Dettagliate

### `install.sh` - Opzioni Complete

| Opzione | Descrizione | Esempio |
|---------|-------------|---------|
| *(nessuna)* | Standard in /usr/local | `./install.sh` |
| `--prefix PATH` | Percorso personalizzato | `./install.sh --prefix /opt/ioc` |
| `--user` | Home utente ~/.local | `./install.sh --user` |
| `--dev` | Build debug | `./install.sh --dev` |
| `--clean` | Pulisci prima di build | `./install.sh --clean` |
| `--build-only` | Solo compilazione | `./install.sh --build-only` |
| `--skip-tests` | Salta test | `./install.sh --skip-tests` |
| `--help` | Mostra aiuto | `./install.sh --help` |

### Combinazioni Utili

```bash
# Installazione pulita in modalità sviluppo
./install.sh --clean --dev

# Solo build senza installare
./install.sh --build-only --skip-tests

# Installazione utente pulita
./install.sh --user --clean

# Installazione custom ottimizzata
./install.sh --prefix /opt/ioccultcalc --clean
```

## 🔄 Aggiornamento

### Metodo 1: Pull + Reinstall

```bash
cd IOccultCalc
git pull
./install.sh --clean
```

### Metodo 2: Solo Rebuild

```bash
cd IOccultCalc
git pull
./build.sh clean
sudo cp build/examples/* /usr/local/bin/
```

## 🗑️ Disinstallazione

### Script Automatico

```bash
# Disinstallazione da /usr/local
./uninstall.sh

# Da percorso personalizzato
./uninstall.sh --prefix /opt/ioccultcalc
```

### Manuale

```bash
# Sistema
sudo rm -rf /usr/local/bin/{italoccultcalc,occult_calc,ioccultcalc_search}
sudo rm -rf /usr/local/lib/libioccultcalc.a
sudo rm -rf /usr/local/include/ioccultcalc
sudo rm -rf /usr/local/share/ioccultcalc
sudo rm -rf /usr/local/share/doc/ioccultcalc

# Utente
rm -rf ~/.local/bin/{italoccultcalc,occult_calc,ioccultcalc_search}
rm -rf ~/.local/lib/libioccultcalc.a
rm -rf ~/.local/include/ioccultcalc
rm -rf ~/.local/share/ioccultcalc

# Dati produzione
rm -rf ~/.ioccultcalc
rm -rf ~/catalogs/gaia_mag18.cat.gz
```

## 🔧 Risoluzione Problemi

### Problema: "CMake not found"

```bash
# macOS
brew install cmake

# Ubuntu/Debian
sudo apt-get install cmake

# RHEL/CentOS
sudo yum install cmake
```

### Problema: "Permission denied"

```bash
# Opzione 1: Usa installazione utente
./install.sh --user

# Opzione 2: Installa in directory scrivibile
./install.sh --prefix ~/ioccultcalc
```

### Problema: "OpenMP not found" (macOS)

```bash
# Installa libomp
brew install libomp

# Rebuild
./install.sh --clean
```

### Problema: "Command not found" dopo installazione

```bash
# Verifica percorso
which italoccultcalc

# Se vuoto, aggiungi al PATH
echo 'export PATH="/usr/local/bin:$PATH"' >> ~/.zshrc
source ~/.zshrc

# Oppure per installazione utente
echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.zshrc
source ~/.zshrc
```

### Problema: Build fallita

```bash
# Pulisci tutto e ricompila
rm -rf build
./install.sh --clean

# Verifica dipendenze
cmake --version
g++ --version  # o clang++ --version
```

## 📊 Confronto Script

| Caratteristica | install.sh | install_production.sh | build.sh |
|----------------|------------|-----------------------|----------|
| Installazione automatica | ✅ | ✅ | ❌ |
| Verifica dipendenze | ✅ | ✅ | ❌ |
| Test automatici | ✅ | ✅ | ⚠️ opzionale |
| Installa eseguibili | ✅ | ❌ | ❌ |
| Installa libreria | ✅ | ❌ | ❌ |
| Installa header | ✅ | ❌ | ❌ |
| Installa preset | ✅ | ✅ | ❌ |
| Installa docs | ✅ | ⚠️ parziale | ❌ |
| Download cataloghi | ❌ | ✅ | ❌ |
| Setup directory | ⚠️ minimale | ✅ completo | ❌ |
| Launcher script | ❌ | ✅ | ❌ |
| Multi-piattaforma | ✅ | ⚠️ macOS focus | ✅ |
| Personalizzabile | ✅✅ 8 opzioni | ❌ | ⚠️ 3 modi |
| Tempo esecuzione | 2-5 min | 5-15 min* | 1-3 min |

*dipende da download cataloghi

## 🎯 Raccomandazioni

### Per Utenti Finali

1. **Prima installazione**: `install_production.sh`
2. **Aggiornamenti**: `install.sh --clean`
3. **Uso quotidiano**: launcher script

### Per Sviluppatori

1. **Setup iniziale**: `install.sh --dev --user`
2. **Development**: `build.sh`
3. **Test**: `build.sh test`
4. **Deploy**: `install.sh --clean`

### Per Amministratori Sistema

1. **Installazione globale**: `install.sh`
2. **Posizione custom**: `install.sh --prefix /opt/ioccultcalc`
3. **Documentazione**: `/usr/local/share/doc/ioccultcalc/`

## 📚 Documentazione Completa

Dopo l'installazione con `install.sh`:

```bash
# Lista documenti installati
ls /usr/local/share/doc/ioccultcalc/

# Guide principali
cat /usr/local/share/doc/ioccultcalc/INSTALLATION_GUIDE.md
cat /usr/local/share/doc/ioccultcalc/GUIDA_FILE_OOP.md
cat /usr/local/share/doc/ioccultcalc/COMBINED_LISTS_IMPLEMENTATION.md
cat /usr/local/share/doc/ioccultcalc/PRESET_GUIDE.md
```

## 🤝 Supporto

- **GitHub Issues**: https://github.com/manvalan/IOccultCalc/issues
- **Discussioni**: https://github.com/manvalan/IOccultCalc/discussions
- **Documentazione**: Repository e post-installazione
- **Esempi**: `share/ioccultcalc/presets/` e `share/ioccultcalc/data/`

---

**Sistema di installazione IOccultCalc v2.0**
Flessibile, automatico, multi-piattaforma 🚀
