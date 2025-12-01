# IOccultCalc - Preset Configurations Guide

## Preset Veloci vs Precisione Massima

Questa guida descrive i due preset principali per l'utilizzo di IOccultCalc v2.1.0-rkf78.

---

## 1. Fast Survey (preset_fast_survey_asteroids.json)

### 🎯 Obiettivo
Generare rapidamente una lista di asteroidi candidati per occultazioni stellari, privilegiando **performance** su precisione assoluta.

### ⚡ Caratteristiche Chiave

**Integrazione Orbitale:**
- Integratore: RKF78 adaptive
- Step size: 0.5 giorni (ottimizzato per velocità)
- Tolleranza: 1e-10 (buona precisione)
- Perturbazioni planetarie: **SÌ**
- Perturbazioni asteroidali: **NO** (risparmio 70% tempo)
- Correzioni relativistiche: **NO**

**Filtri Candidati:**
- Diametro: 50-1000 km
- Magnitudine: 8.0-18.0
- Altitudine minima: 15°
- Drop magnitudine minima: 0.3
- Limite risultati: 1000 asteroidi

**Performance:**
- Parallelizzazione: **ATTIVA**
- Posizioni approssimate: **ATTIVA**
- Salto eventi bassa probabilità: **ATTIVA**
- Soglia probabilità: 1%

**Output:**
- Formato: **JSON + ASTNUM_LIST** (dati + lista asteroidi)
- Verbosità: 1 (essenziale)
- Compressione: NO
- Pretty print: NO
- File lista: `asteroids_with_occultations.txt`

### 📊 Metriche Attese
- Tempo elaborazione: ~5-10 minuti per 1000 asteroidi/mese
- Precisione: ±5-10 km sul percorso ombra
- Accuratezza temporale: ±1-2 secondi
- RAM: ~2-4 GB

### 🚀 Utilizzo

```bash
# Esegui survey veloce
./ioccultcalc --preset preset_fast_survey_asteroids.json

# Output: results/fast_survey/candidates_YYYY-MM-DD_HHMMSS.json
```

### 💡 Quando Usare
- Survey iniziali su ampie popolazioni
- Identificazione rapida candidati promettenti
- Pianificazione osservativa preliminare
- Risorse computazionali limitate
- Necessità di aggiornamenti frequenti

---

## 2. Maximum Precision (preset_maximum_precision_asteroids.json)

### 🎯 Obiettivo
Analisi di **massima precisione** per asteroidi selezionati, con output completo in tutti i formati disponibili.

### 🔬 Caratteristiche Chiave

**Integrazione Orbitale:**
- Integratore: RKF78 adaptive
- Step size: 0.05 giorni (10x più fine)
- Tolleranza: 1e-13 (precisione estrema)
- Perturbazioni planetarie: **SÌ**
- Perturbazioni asteroidali: **SÌ** (12 major perturbers)
- Correzioni relativistiche: **SÌ**

**Modello Completo:**
- Proper motion stellare: **ATTIVA**
- Parallasse: **ATTIVA**
- Rifrazione atmosferica: **ATTIVA**
- Diffrazione Fresnel: **ATTIVA**
- Ellisse incertezza: **CALCOLATA**
- Percorso ombra: **DETTAGLIATO** (1 km risoluzione)

**Analisi Avanzata:**
- Iterazioni refinement: 5
- Salto eventi: **DISATTIVATO**
- Soglia probabilità: 0.1%
- Validazione qualità orbita: **ATTIVA**

**Output:**
- Formati: **JSON + XML + KML + JPG + ASTNUM_LIST** (completo)
- Verbosità: 3 (massimo dettaglio)
- Compressione: SÌ
- Tutti metadati inclusi
- File lista: `selected_asteroids_results.txt`

**Configurazione Output Dettagliata:**

### JSON
- Pretty print, metadata completi
- Ephemeris, incertezze, shadow path inclusi

### XML
- Schema incluso, UTF-8, validazione

### KML (Google Earth)
- Ground track completo
- Ellisse incertezza (30% opacità)
- Shadow path dettagliato
- Descrizioni complete

### JPG (Mappe)
- Risoluzione: 1920x1080, 300 DPI
- Qualità: 95%
- Griglia, scala, titolo inclusi
- Proiezione: Equirettangolare
- Star field, percorsi, bande incertezza visualizzate

### ASTNUM_LIST (Lista Asteroidi) ⭐ NUOVO!
- Lista numeri asteroidi che hanno prodotto occultazioni
- Un numero per riga, formato compatibile input IOccultCalc
- Include nome asteroide e conteggio eventi
- Perfetto per workflow iterativo: survey → lista → analisi precision

### 📊 Metriche Attese
- Tempo elaborazione: ~30-60 minuti per 10 asteroidi/mese
- Precisione: ±100-500 metri sul percorso ombra
- Accuratezza temporale: ±0.1-0.5 secondi
- RAM: ~4-8 GB

### 🚀 Utilizzo

```bash
# 1. Crea file lista asteroidi (o usa asteroids_selected.txt)
echo "1
433
704" > my_asteroids.txt

# 2. Modifica preset se necessario (campo "inputFile")
# preset_maximum_precision_asteroids.json, line 72:
# "inputFile": "./my_asteroids.txt"

# 3. Esegui analisi ad alta precisione
./ioccultcalc --preset preset_maximum_precision_asteroids.json

# 4. Risultati in: results/high_precision/
# - occultation_TIMESTAMP.json (dati completi)
# - occultation_TIMESTAMP.xml (formato standard)
# - occultation_TIMESTAMP.kml (visualizzazione Google Earth)
# - occultation_TIMESTAMP.jpg (mappe per stampa/presentazione)
```

### 💡 Quando Usare
- Predizioni finali per campagne osservative
- Pubblicazioni scientifiche
- Coordinamento reti osservative internazionali
- Analisi eventi critici (asteroidi rari, stelle brillanti)
- Validazione modelli orbitali
- Presentazioni e divulgazione (output JPG/KML)

---

## 3. Confronto Diretto

| Caratteristica | Fast Survey | Maximum Precision |
|----------------|-------------|-------------------|
| **Tempo elaborazione** | 5-10 min | 30-60 min |
| **Step size** | 0.5 giorni | 0.05 giorni |
| **Tolleranza** | 1e-10 | 1e-13 |
| **Pert. asteroidali** | NO | SÌ (12 major) |
| **Relatività** | NO | SÌ |
| **Precisione path** | ±5-10 km | ±100-500 m |
| **Precisione tempo** | ±1-2 s | ±0.1-0.5 s |
| **RAM** | 2-4 GB | 4-8 GB |
| **Output formati** | JSON+ASTNUM | JSON+XML+KML+JPG+ASTNUM |
| **Dettaglio output** | Base | Completo |
| **Parallelizzazione** | Sì | Sì |
| **Caso d'uso** | Survey, screening | Predizioni finali |

---

## 4. Workflow Consigliato

### Pipeline Completa Occultazioni

```
1. FASE SURVEY (Fast Survey)
   ├─ Input: Database completo MPC (100k+ asteroidi)
   ├─ Filtri: Diametro, mag, geometria
   ├─ Output: Lista ~50-200 candidati promettenti
   └─ Tempo: ~10 minuti
   
2. SELEZIONE MANUALE
   ├─ Review candidati
   ├─ Priorità: Drop mag, accessibilità, interesse scientifico
   └─ Output: Lista 5-20 target selezionati
   
3. FASE PRECISION (Maximum Precision)
   ├─ Input: Lista target selezionati
   ├─ Analisi completa con tutti i modelli
   ├─ Output: JSON+XML+KML+JPG per ogni evento
   └─ Tempo: ~5 minuti per target
   
4. DISTRIBUZIONE
   ├─ KML → Invio osservatori (Google Earth)
   ├─ JPG → Mappe stampabili/presentazioni
   ├─ XML → Integrazione database IOTA/EAON
   └─ JSON → Analisi software custom
```

### Esempio Pratico

```bash
# FASE 1: Survey veloce Gennaio 2026
./ioccultcalc --preset preset_fast_survey_asteroids.json \
  --start-date 2026-01-01 --end-date 2026-01-31

# Output: 
# - results/fast_survey/candidates_20260101.json (dati completi)
# - results/fast_survey/asteroids_with_occultations.txt (LISTA ASTEROIDI!)

# FASE 2: Automatica - usa lista asteroidi dal survey!
# Il file ASTNUM_LIST è già pronto come input per analisi precision

# FASE 3: Usa direttamente la lista dal survey! (WORKFLOW AUTOMATICO)
# Copia la lista generata dal fast survey
cp results/fast_survey/asteroids_with_occultations.txt selected_asteroids.txt

# Opzionale: filtra manualmente (es: solo primi 10)
head -20 selected_asteroids.txt | grep -E '^[0-9]' > top10_targets.txt

# FASE 4: Analisi alta precisione sulla lista filtrata
sed -i '' 's|asteroids_selected.txt|top10_targets.txt|' \
  preset_maximum_precision_asteroids.json

./ioccultcalc --preset preset_maximum_precision_asteroids.json

# FASE 5: Risultati
ls results/high_precision/
# occultation_20260115_183022_ceres.json
# occultation_20260115_183022_ceres.xml  
# occultation_20260115_183022_ceres.kml
# occultation_20260115_183022_ceres.jpg
# ... (per ogni evento trovato)
```

---

## 5. Personalizzazione Preset

### Modificare Fast Survey

```json
// Per survey più rigoroso ma comunque veloce:
{
  "propagator": {
    "stepSize": 0.2,              // da 0.5 → migliora precisione
    "tolerance": 1e-11,           // da 1e-10 → più accurato
    "useAsteroidalPerturbations": true  // solo major
  },
  "predictor": {
    "constraints": {
      "minMagnitudeDrop": 0.5     // da 0.3 → eventi più evidenti
    }
  }
}
```

### Modificare Maximum Precision

```json
// Per ultra-precision (pubblicazioni):
{
  "propagator": {
    "stepSize": 0.01,             // da 0.05 → massimo dettaglio
    "tolerance": 1e-14            // da 1e-13 → limite numerico
  },
  "predictor": {
    "occultationAnalysis": {
      "shadowPathResolution": 0.5,  // da 1.0 → path super-dettagliato
      "refinementIterations": 10    // da 5 → convergenza estrema
    }
  },
  "output": {
    "options": {
      "jpg": {
        "width": 3840, "height": 2160,  // 4K per stampa poster
        "dpi": 600                      // qualità tipografica
      }
    }
  }
}
```

---

## 6. File di Input Asteroidi

### Formato asteroids_selected.txt

```
# Formato: un asteroide per riga
# Supportati:
# - Numero MPC: 1, 433, 704
# - Designazione: 2010 AB123, 1999 RQ36
# - Nome: Ceres, Eros (case-insensitive)

# Esempio lista (file incluso):
1       # Ceres
2       # Pallas
4       # Vesta
10      # Hygiea
16      # Psyche
52      # Europa
704     # Interamnia
433     # Eros
```

### Generazione Lista Dinamica

```bash
# Top 20 asterodi per dimensione
./ioccultcalc --list-asteroids --sort-by diameter --limit 20 > top20_size.txt

# Asterodi Main Belt mag < 12
./ioccultcalc --list-asteroids --filter "orbit=main_belt,mag<12" > bright_mainbelt.txt

# Near-Earth Asteroids
./ioccultcalc --list-asteroids --filter "orbit=nea" > nea_list.txt
```

---

## 7. Formato ASTNUM_LIST - Lista Asteroidi con Occultazioni

### 🎯 Scopo
Il formato **ASTNUM_LIST** salva una lista testuale semplice contenente **solo i numeri degli asteroidi che hanno prodotto almeno un'occultazione**. Questo file può essere riutilizzato direttamente come input per analisi successive.

### 📄 Formato File

```
# ITALOccultCalc - Asteroid List with Occultations
# Generated: 2026-01-01 12:00:00
# Software: IOccultCalc v2.1.0-rkf78
# Total asteroids with occultations: 47
# Total events: 152
#
# Format: (asteroid_number) # [asteroid_name] (event_count)
# =====================================================

(1) # Ceres                (12 events)
(2) # Pallas               (8 events)
(4) # Vesta                (15 events)
(10) # Hygiea              (6 events)
(433) # Eros                (3 events)
(704) # Interamnia          (5 events)
(1036) # Ganymed            (2 events)
...

# End of list
```

### 🔄 Workflow Automatico

**Input → Processing → Output → Input ciclico:**

```bash
# 1. Survey iniziale (1000 asteroidi)
./ioccultcalc --preset preset_fast_survey_asteroids.json
# Output: asteroids_with_occultations.txt (47 asteroidi con eventi)

# 2. Analisi precision su lista filtrata
cp results/fast_survey/asteroids_with_occultations.txt input_precision.txt
./ioccultcalc --preset preset_maximum_precision_asteroids.json \
  --asteroid-list input_precision.txt
# Output: selected_asteroids_results.txt (47 asteroidi analizzati)

# 3. Iterazione mensile automatica
for month in {01..12}; do
  ./ioccultcalc --preset preset_fast_survey_asteroids.json \
    --start-date 2026-${month}-01 --end-date 2026-${month}-31
  
  # Accumula lista
  cat results/fast_survey/asteroids_with_occultations.txt >> yearly_list.txt
done

# 4. Rimuovi duplicati e analizza lista annuale
sort -u yearly_list.txt > unique_asteroids_2026.txt
./ioccultcalc --preset preset_maximum_precision_asteroids.json \
  --asteroid-list unique_asteroids_2026.txt --year 2026
```

### 💡 Casi d'Uso

1. **Pipeline Survey Mensile:**
   - Fast survey su 1000+ asteroidi
   - Filtra automaticamente i "hit" (con occultazioni)
   - Analisi precision solo su hit (~5-10% del totale)
   - **Risparmio tempo: 90%+**

2. **Monitoraggio Target Specifici:**
   - Survey su popolazione (NEA, MBA, Trojan)
   - Lista asteroidi attivi salvata
   - Re-analisi periodica stessa lista
   - Tracking evoluzione occultazioni

3. **Collaborazioni Internazionali:**
   - Condivisione liste standardizzate
   - Formato testo universale
   - Import/export tra software diversi
   - Compatibilità con OrbFit, Find_Orb, etc.

4. **Quality Control:**
   - Confronto liste diverse release
   - Validazione coverage survey
   - Audit asteroidi analizzati
   - Statistiche popolazioni

### 🔧 Configurazione Output

```json
{
  "output": {
    "formats": ["JSON", "ASTNUM_LIST"],
    "options": {
      "astnum_list": {
        "filename": "asteroids_with_occultations.txt",
        "includeEventCount": true,    // Mostra (N events)
        "includeNames": true,          // Mostra # Nome
        "sortBy": "number",            // "number", "priority", "events"
        "appendMode": false,           // true = accumula, false = sovrascrivi
        "includeHeader": true,         // Metadata header
        "minEvents": 1                 // Soglia minima eventi
      }
    }
  }
}
```

### 📊 Vantaggi vs Altri Formati

| Caratteristica | ASTNUM_LIST | JSON | XML |
|----------------|-------------|------|-----|
| **Dimensione file** | 1 KB | 500 KB | 800 KB |
| **Leggibilità** | ★★★★★ | ★★★ | ★★ |
| **Riuso come input** | ✅ Diretto | ⚠️ Parsing | ⚠️ Parsing |
| **Editabile manualmente** | ✅ Sì | ⚠️ Difficile | ❌ No |
| **Compatibilità tools** | ✅ Universale | ⚠️ Limitata | ⚠️ Limitata |
| **Performance scrittura** | ⚡ Istantaneo | 🐌 Lento | 🐌 Molto lento |
| **Dati completi evento** | ❌ No | ✅ Sì | ✅ Sì |

**Best Practice:** Usa **ASTNUM_LIST** per workflow e **JSON/XML** per archivio dati completi.

---

## 8. Output Files

### Fast Survey Output (JSON + ASTNUM_LIST)

```json
{
  "metadata": {
    "preset": "Fast Survey",
    "version": "2.1.0-rkf78",
    "timestamp": "2026-01-01T00:00:00Z",
    "processing_time_sec": 547.2
  },
  "candidates": [
    {
      "asteroid": {
        "number": 1,
        "name": "Ceres",
        "diameter_km": 939.4
      },
      "star": {
        "gaia_id": "DR3 123456789",
        "magnitude": 10.2
      },
      "event": {
        "date": "2026-01-15T18:30:22Z",
        "altitude_deg": 45.3,
        "magnitude_drop": 1.8,
        "duration_sec": 12.4,
        "probability": 0.87
      }
    }
  ],
  "statistics": {
    "total_asteroids": 1000,
    "total_events": 47,
    "processing_rate": 1.83
  }
}
```

### Maximum Precision Output

**JSON**: Dati completi + ephemeris + incertezze  
**XML**: Formato standard interoperabile  
**KML**: Visualizzazione 3D Google Earth  
**JPG**: Mappe pubblicabili alta risoluzione  

Vedi `CONFIGURATION_GUIDE.md` sezione Output per dettagli formati.

---

## 8. Best Practices

### 🏃 Fast Survey
1. ✅ Usa per survey mensili automatici
2. ✅ Parallelizzazione sempre attiva
3. ✅ Cache ephemeris attivata (risparmio 50% tempo)
4. ✅ Aggiorna database MPC settimanalmente
5. ⚠️ NON usare per predizioni finali distribuite

### 🔬 Maximum Precision
1. ✅ Usa SOLO per eventi confermati interessanti
2. ✅ Verifica qualità elementi orbitali input
3. ✅ Controlla log per warning accuratezza
4. ✅ Genera tutti i formati per distribuzione completa
5. ✅ Backup risultati (JSON compresso archivio)

### ⚡ Performance
- **Fast**: CPU multi-core, SSD, 8GB RAM minimo
- **Precision**: CPU high-end, 16GB RAM raccomandato
- Entrambi: Cache su SSD per Gaia DR3

---

## 9. Troubleshooting

### Fast Survey Lento
```bash
# Check: parallelizzazione attiva?
grep -A2 "parallel" preset_fast_survey_asteroids.json
# "enabled": true, "numThreads": 0  # 0 = auto detect

# Check: cache ephemeris abilitata?
grep -A3 "performance" preset_fast_survey_asteroids.json
# "cacheEphemerides": true

# Ottimizzazione: ridurre periodo ricerca
# "startDate": "2026-01-01", "endDate": "2026-01-07"  # 1 settimana
```

### Maximum Precision Errori Convergenza
```bash
# Aumenta iterazioni
# "maxIterations": 200000  # da 100000

# Rilassa tolleranza leggermente
# "tolerance": 1e-12  # da 1e-13

# Check log per dettagli
tail -f logs/high_precision.log
```

### Out of Memory
```bash
# Fast Survey: riduci cache
# "maxMemoryUsage": 2048  # da 4096 MB

# Precision: processa batch più piccoli
# "batchSize": 5  # da 10

# O processa asteroidi singolarmente
for ast in $(cat selected_targets.txt); do
  echo "$ast" | ./ioccultcalc --preset preset_maximum_precision_asteroids.json
done
```

---

## 10. Versioning e Aggiornamenti

**Current Version**: 2.1.0-rkf78

**Change Log**:
- v2.1.0-rkf78: Integratore RKF78 adaptive, ×273 performance vs RK4
- v2.0.0: Multi-formato output (JSON/XML/KML/JPG)
- v1.5.0: Gaia DR3 integration
- v1.0.0: Initial release

**Compatibilità Preset**:
- Questi preset richiedono **v2.1.0-rkf78** o superiore
- Per versioni precedenti, rimuovere sezioni non supportate

---

## 📞 Support

**Documentazione**: `CONFIGURATION_GUIDE.md`  
**Issues**: GitHub repository  
**Examples**: `examples/` directory  

---

**Created**: 2025-11-29  
**Author**: IOccultCalc Development Team  
**License**: MIT
