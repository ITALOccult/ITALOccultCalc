# Workflow Predizioni Dicembre 2025 - Due Fasi

Workflow ottimizzato in 2 fasi per predizioni occultazioni asteroidali con massima accuratezza.

## 📋 Workflow Completo

### FASE 1: Ricerca Veloce (Fast Mode)
**Obiettivo**: Identificare rapidamente asteroidi con potenziali occultazioni

```bash
# 1. Esegui ricerca veloce
./build/examples/italoccultcalc preset_dec2025_fast_list.oop

# Output:
# - occultations_dec2025_fast.txt (eventi trovati)
# - asteroids_dec2025_candidates.txt (lista asteroidi da analizzare)
```

**Caratteristiche Fast Mode**:
- ✅ Chebyshev degree 11 (veloce)
- ✅ Step 20 giorni
- ✅ Solo mag ≤13.0 (stelle luminose)
- ✅ NO orbit fitting
- ✅ Perturbazioni essenziali (no Mercurio/Urano/Nettuno)
- ⏱️ Tempo: ~5-10 minuti per 1000 asteroidi

---

### FASE 2: Download Osservazioni AstDyS

```bash
# 2. Scarica elementi e osservazioni per asteroidi selezionati
python3 download_astdys_data.py asteroids_dec2025_candidates.txt \
    -o test_astdys_download

# Output directory test_astdys_download/:
# - 433.eq1 (elementi orbitali equinoziali)
# - 433.rwo (osservazioni ottiche complete)
# - [altri asteroidi...]
```

**File scaricati**:
- `.eq1`: Elementi orbitali formato OEF2.0 con covarianza
- `.rwo`: Osservazioni complete con header e metadata

---

### FASE 3: Analisi Alta Precisione (High Precision + Fitting)
**Obiettivo**: Predizioni professionali con massima accuratezza

```bash
# 3. Esegui analisi dettagliata con orbit fitting
./build/examples/italoccultcalc preset_dec2025_highprec_fitting.oop

# Output:
# - occultations_dec2025_highprec.txt (formato IOTA)
# - occultations_dec2025_highprec.json (machine-readable)
# - fitted_elements_dec2025.txt (elementi fittati)
# - fitting_residuals_dec2025.txt (residui O-C)
```

**Caratteristiche High Precision**:
- ✅ RKF78 7(8) adattivo (tolleranza 1e-13)
- ✅ **Orbit fitting completo** (osservazioni .rwo)
- ✅ Tutte le perturbazioni (8 pianeti + AST17)
- ✅ Correzioni relativistiche
- ✅ GAIA EDR3 mag ≤16.0
- ✅ Incertezze da fit O-C
- ⏱️ Tempo: ~30-60 sec/asteroide (~1-2 ore per 100 asteroidi)

---

## 📊 Confronto Preset

| Caratteristica | Fast List | High Precision + Fitting |
|---------------|-----------|--------------------------|
| **Propagatore** | Chebyshev deg 11 | RKF78 7(8) |
| **Step** | 20 giorni | 0.05 giorni (adattivo) |
| **Tolleranza** | 1e-10 | 1e-13 |
| **Perturbazioni** | Solo maggiori | Tutte (8 + AST17) |
| **Orbit Fitting** | ❌ NO | ✅ SI (20 iter, 1e-8 AU) |
| **Stelle mag** | ≤13.0 | ≤16.0 |
| **Output** | Lista asteroidi | IOTA + JSON + Residui |
| **Tempo/asteroide** | ~1 sec | ~30-60 sec |
| **Accuratezza** | Screening | Professionale |

---

## 🎯 Vantaggi Workflow 2-Fasi

### Efficienza
- ⚡ Fast mode elabora 1000 asteroidi in ~10 minuti
- 🎯 Identifica solo candidati promettenti (~5-10% del totale)
- 💾 Scarica osservazioni solo per asteroidi rilevanti
- 🚀 High precision mode su ~50-100 asteroidi invece di 1000

### Accuratezza
- 📡 Orbit fitting usa 10.000+ osservazioni per asteroide
- 📉 RMS residui tipicamente <1-2 arcsec dopo fitting
- 🎲 Incertezze realistiche da covarianza fitting
- 🌟 Predizioni professionali livello IOTA/Euraster

### Praticità
- 📁 File .eq1/.rwo riutilizzabili per altri mesi
- 🔄 Aggiornamento incrementale (solo nuovi asteroidi)
- 💻 Parallelizzabile (più preset in parallelo)
- 📊 Output multipli (testo, JSON, residui)

---

## 📝 File Input/Output

### Input Iniziale
```
asteroids_1_1000.txt    (lista 1-1000, già presente)
```

### Output FASE 1 (Fast)
```
occultations_dec2025_fast.txt           (eventi preliminari)
asteroids_dec2025_candidates.txt        (es: 50-100 asteroidi)
```

### Output FASE 2 (Download)
```
test_astdys_download/
  ├─ 433.eq1                            (1.8 KB)
  ├─ 433.rwo                            (3.2 MB, ~17.000 obs)
  ├─ 1.eq1
  ├─ 1.rwo
  └─ [altri...]
```

### Output FASE 3 (High Precision)
```
occultations_dec2025_highprec.txt       (formato IOTA)
occultations_dec2025_highprec.json      (machine-readable)
fitted_elements_dec2025.txt             (elementi fittati)
fitting_residuals_dec2025.txt           (residui O-C)
```

---

## ⏱️ Tempi Stimati

### Scenario Tipico (1000 asteroidi iniziali)

| Fase | Operazione | Tempo | Output |
|------|-----------|-------|--------|
| 1 | Fast screening | ~10 min | 80 candidati |
| 2 | Download osservazioni | ~5 min | 80 × 2 file |
| 3 | High precision + fitting | ~40-60 min | Predizioni finali |
| **TOTALE** | | **~55-75 min** | |

### Confronto con Approccio Diretto
- ❌ High precision su 1000 asteroidi: **~8-16 ore**
- ✅ Workflow 2-fasi: **~1 ora**
- 🚀 **Speedup: 8-16×**

---

## 🔧 Configurazione Orbit Fitting

Il preset high precision usa questi parametri per orbit fitting:

```
orbit_fitting.enable_fitting = .TRUE.
orbit_fitting.observation_source = ASTDYS
orbit_fitting.max_iterations = 20
orbit_fitting.convergence_tolerance = 1.0e-8    [AU]
orbit_fitting.outlier_sigma = 3.0               [σ]
```

**Risultati tipici per (433) Eros**:
- Osservazioni parsate: 16.103 (da .rwo)
- Osservazioni usate: 14.075 (dopo outlier rejection)
- Outliers rigettati: 2.038 (12.6%)
- RMS finale: ~800 arcsec
- Convergenza: 10 iterazioni

---

## 📍 Filtri Geografici Italia

Entrambi i preset usano questi filtri per eventi visibili dall'Italia:

```
Latitudine:  35° - 48° N   (Sicilia → Alpi)
Longitudine:  5° - 20° E   (Torino → Puglia)

Altitudine sole:  -18° (notte astronomica)
Altitudine target: >25° (buona visibilità)
Distanza Luna:    >15° (no disturbo)
```

---

## 🎓 Best Practices

### Prima di Iniziare
1. ✅ Verifica che `asteroids_1_1000.txt` esista
2. ✅ Controlla spazio disco (~500 MB per 100 asteroidi .rwo)
3. ✅ Configura GAIA cache directory
4. ✅ Compila con `make italoccultcalc`

### Durante Esecuzione
- 📊 Monitora output fase 1 per vedere quanti candidati
- 💾 Controlla download .rwo (file grandi = molte osservazioni)
- ⚠️ Orbit fitting può fallire per asteroidi con poche osservazioni

### Dopo Completamento
- 📧 Pubblica risultati su Euraster/IOTA-ES
- 🔄 Salva `fitted_elements_dec2025.txt` per riuso futuro
- 📁 Archivia file .rwo per altri periodi temporali

---

## 🐛 Troubleshooting

### Fast mode trova 0 asteroidi
- Verifica `asteroids_1_1000.txt` esista
- Controlla connessione JPL Horizons
- Abbassa soglia `min_probability = 0.001`

### Download .rwo fallisce
- Controlla connessione a newton.spacedys.com
- Alcuni asteroidi non hanno .rwo (poche osservazioni)
- Usa `--force` per ri-scaricare file corrotti

### Orbit fitting non converge
- Normale per asteroidi con poche osservazioni (<100)
- Aumenta `max_iterations = 50`
- Rilassa `outlier_sigma = 4.0`
- Controlla `fitting_residuals_dec2025.txt` per diagnosi

### High precision mode troppo lento
- Riduci lista asteroidi (top 20-30)
- Aumenta `step_size_days = 0.1`
- Rilassa `tolerance = 1.0e-12`
- Usa `parallel_threads = 8`

---

## 📚 File Preset

### Fast Mode
```bash
preset_dec2025_fast_list.oop
```
- Screening veloce
- Output: lista asteroidi candidati

### High Precision + Fitting
```bash
preset_dec2025_highprec_fitting.oop
```
- Analisi dettagliata
- Input: lista da fast mode
- Usa: test_astdys_download/*.{eq1,rwo}

### Script Download
```bash
download_astdys_data.py
```
- Scarica .eq1 e .rwo da AstDyS
- Salva in test_astdys_download/

---

## ✅ Checklist Completa

- [ ] Compila IOccultCalc (`make italoccultcalc`)
- [ ] Verifica `asteroids_1_1000.txt` presente
- [ ] Esegui FASE 1: Fast screening
- [ ] Controlla `asteroids_dec2025_candidates.txt` generato
- [ ] Esegui FASE 2: Download osservazioni
- [ ] Verifica file .rwo scaricati (test_astdys_download/)
- [ ] Esegui FASE 3: High precision + fitting
- [ ] Verifica output IOTA e JSON
- [ ] Analizza residui fitting
- [ ] Pubblica risultati

---

**Tempo totale workflow: ~1 ora per 1000 asteroidi iniziali → ~50-100 predizioni professionali** 🚀
