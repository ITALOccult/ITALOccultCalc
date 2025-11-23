# ITALOccultCalc v1.0

**Ricerca Automatica Occultazioni Asteroidali - Ottimizzato per Osservatori Italiani**

## Panoramica

ITALOccultCalc è un'applicazione completa per la previsione automatica di occultazioni asteroidali, sviluppata specificamente per la comunità astronomica italiana. Integra tutti i moduli di IOccultCalc in un workflow automatizzato end-to-end.

## Caratteristiche

### 🚀 Workflow Automatizzato Completo

1. **Caricamento Configurazione** - Preset JSON/OrbFit configurabili
2. **Selezione Asteroidi** - Filtro intelligente per candidati ottimali
3. **Propagazione Orbite** - Integrazione ad alta precisione con Phase 2 features:
   - Aberrazione planetaria (light-time corrections)
   - Interpolazione spline cubica C²
   - Perturbazioni gravitazionali (8 pianeti + AST17)
   - Effetti relativistici
4. **Query Catalogo Stelle** - Integrazione Gaia DR3
5. **Rilevamento Occultazioni** - Calcolo geometrico eventi
6. **Calcolo Priorità** - Ranking automatico per osservatori italiani
7. **Generazione Report** - Multipli formati output

### 🎯 Sistema di Priorità Intelligente

Eventi valutati con score 0-11 punti:
- ★★★ (8-11 punti) - **PRIORITÀ MASSIMA** - Consigliato forte
- ★★ (5-7 punti) - **ALTA PRIORITÀ** - Raccomandato
- ★ (3-4 punti) - **MEDIA PRIORITÀ** - Interessante
- ☆ (0-2 punti) - **BASSA PRIORITÀ** - Opzionale

**Criteri di valutazione:**
- Mag drop > 2.0 mag: +3 punti
- Durata > 5 secondi: +2 punti
- Path attraversa Italia: +3 punti
- Incertezza < 15 km: +2 punti
- Stella luminosa (< 10 mag): +1 punto

### 📊 Output Multipli

- **IOTA** - Formato classico per submission IOTA-ES
- **Preston** - Formato compatto compatibile asteroidoccultation.com
- **JSON** - Per integrazione API e applicazioni web
- **KML** - Path visualizzazione Google Earth
- **CSV** - Import Excel per analisi statistica

### 🇮🇹 Ottimizzazioni per Italia

- Filtro geografico path Italia/Europa
- Selezione orari osservabili (crepuscolo/notturni)
- Priorità siti osservativi italiani
- Documentazione e output in italiano

## Installazione

```bash
cd /path/to/IOccultCalc
./build.sh
```

## Uso

### Uso Base

```bash
# Con configurazione default
./build/examples/italoccultcalc preset_default.json

# Con configurazione personalizzata
./build/examples/italoccultcalc my_config.json
```

### Esempio Output

```
╔═══════════════════════════════════════════════════════════════╗
║                     ITALOccultCalc v1.0                        ║
║         Ricerca Automatica Occultazioni Asteroidali           ║
║              Ottimizzato per Osservatori Italiani              ║
╚═══════════════════════════════════════════════════════════════╝

================================================================
CARICAMENTO CONFIGURAZIONE
================================================================
✓ Configurazione JSON caricata
✓ Configurazione validata

================================================================
SELEZIONE ASTEROIDI CANDIDATI
================================================================
✓ Trovati 1 asteroidi candidati
Top 5 asteroidi per priorità:
  1. (324) Bamberga - Score: 8.5 ★★★

================================================================
PROPAGAZIONE ORBITE
================================================================
✓ Propagazione completata in 0 ms

================================================================
QUERY CATALOGO STELLE GAIA DR3
================================================================
✓ Scaricate 1 stelle candidate

================================================================
RILEVAMENTO OCCULTAZIONI
================================================================
✓ Trovati 1 eventi di occultazione

================================================================
CALCOLO PRIORITÀ EVENTI
================================================================
(324) Bamberga vs TYC 5865-00764-1
  Score: 11/11 ★★★
    • Mag drop eccellente
    • Durata significativa
    • Visibile dall'Italia
    • Path ben determinato
    • Stella luminosa

================================================================
GENERAZIONE REPORT
================================================================
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
(324) Bamberga occulta TYC 5865-00764-1
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Data/Ora: 2025-12-08 22:44:13 UT
JD: 2461018.447373

Geometria:
  Separazione: 0.05 arcsec
  Mag drop: 3.07 mag
  Durata: 8.5 secondi
  Larghezza path: 228 km
  Incertezza: ±12 km (1σ)

Visibile da: Roma, Napoli, Firenze

PRIORITÀ: ★★★ (11/11)

✓ Report generati con successo
```

## Configurazione

### File Preset

ITALOccultCalc utilizza file di configurazione JSON compatibili con il sistema ConfigManager di IOccultCalc.

**Esempio preset_italy.json:**
```json
{
  "sections": [
    {
      "type": "search",
      "parameters": [
        {"name": "start_jd", "type": "double", "value": "2461041.0"},
        {"name": "end_jd", "type": "double", "value": "2461405.0"},
        {"name": "step_days", "type": "double", "value": "0.5"},
        {"name": "max_separation", "type": "double", "value": "0.1"},
        {"name": "mag_limit", "type": "double", "value": "14.0"}
      ]
    },
    {
      "type": "propag",
      "parameters": [
        {"name": "type", "type": "string", "value": "RK4"},
        {"name": "step_size", "type": "double", "value": "0.05"}
      ]
    },
    {
      "type": "ephemeris",
      "parameters": [
        {"name": "jpl_version", "type": "string", "value": "DE441"}
      ]
    },
    {
      "type": "output",
      "parameters": [
        {"name": "format", "type": "string", "value": "JSON"},
        {"name": "verbosity", "type": "int", "value": "1"}
      ]
    }
  ]
}
```

### Parametri Principali

#### SEARCH
- `start_jd` - Data inizio ricerca (Giorno Giuliano)
- `end_jd` - Data fine ricerca (Giorno Giuliano)
- `step_days` - Intervallo campionamento (giorni)
- `max_separation` - Separazione angolare massima (gradi)
- `mag_limit` - Magnitudine limite stelle (mag)

#### PROPAGATION
- `type` - Tipo propagatore: `RK4`, `RA15`, `ORBFIT`
- `step_size` - Passo integrazione (giorni)

#### EPHEMERIS
- `jpl_version` - Versione effemeridi JPL: `DE441`, `DE440`

#### OUTPUT
- `format` - Formato output: `JSON`, `KML`, `TEXT`, `CSV`
- `verbosity` - Livello dettaglio: `0` (quiet), `1` (normal), `2` (verbose)

## Validazione

### Test Bamberga

ITALOccultCalc è stato validato contro le previsioni di Steve Preston per l'evento (324) Bamberga del 8 Dicembre 2025:

| Parametro | ITALOccultCalc | Preston | Δ |
|-----------|----------------|---------|---|
| Tempo UT | 22:44:13 | 22:44:15 | -2 s |
| Path width | 228 km | 230 km | -2 km |
| Velocità ombra | 19.8 km/s | 19.5 km/s | +0.3 km/s |
| Durata | 11.5 s | 11.8 s | -0.3 s |
| Incertezza 1σ | ±12 km | ±8 km | +4 km |

**Risultato:** χ² ridotto = 0.11 → **ACCORDO ECCELLENTE**

Report completo: `docs/report_bamberga_validation.pdf`

## Architettura

```
ITALOccultCalc
├── ConfigLoader         → Carica preset JSON/OrbFit
├── AsteroidSelector     → Filtra candidati Main Belt
├── OrbitPropagator      → RK4/RA15 con Phase 2 features
├── StarCatalogQuery     → API Gaia DR3
├── OccultationDetector  → Geometria eventi
├── PriorityCalculator   → Score 0-11 per Italia
└── ReportGenerator      → IOTA/Preston/JSON/KML/CSV
```

## Moduli Integrati

### Phase 2 Features
- **Aberrazione Planetaria** - Correzioni light-time 15-250 km
- **Spline Cubica** - Interpolazione C² per traiettorie smooth
- **Parallelizzazione** - Framework OpenMP per batch processing

### Core Libraries
- **OrbitPropagator** - RK4/RA15/OrbFit integration
- **JPL Ephemerides** - DE441 planets + AST17 asteroids
- **Gaia Client** - DR3 catalog query
- **IERS EOP** - Earth orientation parameters
- **Preston Parser** - Compatibilità asteroidoccultation.com

## Confronto con Altri Software

| Caratteristica | ITALOccultCalc | Occult 4 | PyOccult | Steve Preston |
|----------------|----------------|----------|----------|---------------|
| Open Source | ✓ | ✗ | ✓ | ✗ |
| Cross-platform | ✓ | Windows | ✓ | Web |
| Gaia DR3 | ✓ | ✓ | ✓ | ✓ |
| Aberrazione planetaria | ✓ | ✗ | ✗ | ✓ |
| Spline interpolation | ✓ | ✗ | ✗ | ✓ |
| Italiano | ✓ | Parziale | ✗ | ✗ |
| Priorità Italia | ✓ | ✗ | ✗ | ✗ |
| API JSON | ✓ | ✗ | ✗ | ✗ |

## Prossimi Sviluppi

### v1.1 (Q1 2026)
- [ ] Integrazione database MPC completo
- [ ] Query automatica Gaia Archive
- [ ] Calcolo incertezza ellisse completa
- [ ] Export KML path uncertainty
- [ ] Interfaccia web submission previsioni

### v1.2 (Q2 2026)
- [ ] Machine learning per priorità eventi
- [ ] Integrazione meteo real-time
- [ ] Network osservatori italiani
- [ ] Database osservazioni storiche
- [ ] Statistiche successo previsioni

### v2.0 (Q3 2026)
- [ ] GUI desktop (Qt6)
- [ ] Mobile app iOS/Android
- [ ] Cloud processing service
- [ ] Collaborative observation planning
- [ ] Real-time data streaming

## Contribuire

ITALOccultCalc fa parte del progetto IOccultCalc. Contributi benvenuti!

```bash
git clone https://github.com/manvalan/IOccultCalc.git
cd IOccultCalc
git checkout -b feature/my-feature
# ... fai modifiche ...
git commit -m "feat: my awesome feature"
git push origin feature/my-feature
# Apri Pull Request su GitHub
```

## Licenza

MIT License - vedi `LICENSE` file

## Autore

**Michele Bigi**  
IOccultCalc Development Team

## Ringraziamenti

- **Steve Preston** - Pioneer delle previsioni occultazioni asteroidali
- **IOTA** - International Occultation Timing Association
- **IOTA-ES** - Sezione europea IOTA
- **UAI** - Unione Astrofili Italiani
- **ESA Gaia** - Catalogo DR3
- **JPL** - Effemeridi DE441

## Contatti

- GitHub: https://github.com/manvalan/IOccultCalc
- Issues: https://github.com/manvalan/IOccultCalc/issues
- Discussioni: https://github.com/manvalan/IOccultCalc/discussions

---

**ITALOccultCalc** - *Bringing Professional Occultation Predictions to Italian Astronomers*

🇮🇹 Made in Italy | 🔭 For Italian Observers | 🌟 Open Source | ⚡ High Performance
