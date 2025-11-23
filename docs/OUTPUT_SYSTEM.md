# Sistema Output Multi-Formato ITALOccultCalc

## Panoramica

Il **OutputManager** è il sistema centralizzato per generare risultati di predizioni occultazioni in 5 formati diversi, tutti configurabili tramite file JSON o API programmatica.

### Formati Supportati

1. **TEXT** - File di testo leggibile (.txt)
2. **LATEX** - Documento LaTeX scientifico (.tex)
3. **PDF** - Report compilato da LaTeX (.pdf)
4. **XML** - Formato Occult4 per OccultWatcher Cloud (.xml)
5. **JSON** - Struttura dati per API/web (.json)
6. **IOTA_CARD** - Scheda grafica stile IOTA (.jpg)

---

## Architettura

```
┌─────────────────────────────────────────┐
│         OccultationEvent(s)             │
│  (dati evento da OrbitPropagator)       │
└──────────────┬──────────────────────────┘
               │
               ▼
┌─────────────────────────────────────────┐
│         OutputManager                    │
│  ┌────────────────────────────────────┐ │
│  │  OutputOptions (configurazione)    │ │
│  └────────────────────────────────────┘ │
│                                          │
│  ┌────────────────────────────────────┐ │
│  │  Format Handlers:                  │ │
│  │  • writeTextFormat()               │ │
│  │  • writeLatexFormat()              │ │
│  │  • compilePDF()                    │ │
│  │  • writeXmlOccult4()               │ │
│  │  • writeJsonFormat()               │ │
│  │  • writeIotaCard()                 │ │
│  └────────────────────────────────────┘ │
└──────────────┬──────────────────────────┘
               │
               ▼
┌─────────────────────────────────────────┐
│  Output Files:                          │
│  • *.txt, *.tex, *.pdf                  │
│  • *.xml (Occult4)                      │
│  • *.json                               │
│  • *.jpg (IOTA cards)                   │
└─────────────────────────────────────────┘
```

---

## Utilizzo Base

### C++ API

```cpp
#include "ioccultcalc/output_manager.h"

using namespace ioccultcalc;

// 1. Crea evento occultazione
OccultationEvent event;
event.asteroid_number = 324;
event.asteroid_name = "Bamberga";
event.jd_event = 2460653.44737;
event.mag_drop = 3.07;
event.duration_seconds = 11.5;
// ... altri parametri

// 2. Configura output
OutputManager manager;
OutputOptions opts;
opts.format = OutputFormat::PDF;
opts.output_file = "bamberga_occultation.pdf";
opts.latex_compile_pdf = true;
opts.latex_include_tikz_map = true;
manager.setOptions(opts);

// 3. Genera output
manager.writeEvent(event);
```

### Configurazione JSON

```json
{
  "output": {
    "format": "PDF",
    "file": "occultations.pdf",
    
    "latex": {
      "compile_pdf": true,
      "include_tikz_map": true,
      "document_class": "article"
    }
  }
}
```

```cpp
// Usa configurazione JSON
ConfigManager config;
config.loadFromJson("preset_output.json");

OutputManager manager(config);
manager.writeEvents(events);
```

---

## Formati Dettagliati

### 1. TEXT Format

**Caratteristiche:**
- Leggibile da umani
- ASCII map opzionale
- Larghezza colonne configurabile (default 80)
- Include header con metadata

**Esempio Output:**

```
================================================================
ITALOCCULTCALC - ASTEROID OCCULTATION PREDICTIONS
================================================================
Generated: 2025-11-23
Software: ITALOccultCalc v1.0.0
Total events: 3
================================================================

EVENT #1 - Priority: ★★★ (11/11)
----------------------------------------------------------------

ASTEROID:
  (324) Bamberga = 1892 A
  Diameter: 228.0 km
  H magnitude: 6.82
  Albedo: 0.063

STAR:
  Catalog: TYC 5865-00764-1
  RA: +56°49'24.24"
  Dec: +08°27'24.12"
  Magnitude: 7.50

OCCULTATION:
  Time (UTC): 2025-12-08 22:44:13
  JD: 2460653.447370
  Magnitude drop: 3.07 mag
  Duration: 11.5 seconds
  Shadow width: 228.0 km
  Shadow velocity: 19.80 km/s
  Closest approach: 0.120 arcsec

UNCERTAINTIES:
  Path: ±12.0 km
  Time: ±2.50 seconds

CENTRAL PATH:
  Start: +45°30'00.00", +09°12'00.00" (Milano)
  End:   +40°48'00.00", +14°18'00.00" (Napoli)

MAP:
  |-------------------------------------------------------------------------|
  |                                                                         |
  |                       *                                                 |
  |                         *                                               |
  |                           *                                             |
  |                             *                                           |
  |                               *                                         |
  |-------------------------------------------------------------------------|
  Lat: 40.5° to 45.8°
  Lon: 9.0° to 14.5°
```

**Configurazione:**

```json
"text": {
  "include_header": true,
  "include_map_ascii": true,
  "width_columns": 80
}
```

---

### 2. LATEX Format

**Caratteristiche:**
- Documento scientifico professionale
- TikZ maps con limiti 1-σ
- Tabelle formattate con booktabs
- Equazioni matematiche
- Multi-evento con TOC

**Struttura Documento:**

```latex
\documentclass[11pt,a4paper]{article}
\usepackage[utf8]{inputenc}
\usepackage{tikz,amsmath,booktabs}

\begin{document}

\maketitle

\section*{Summary}
[Tabella riepilogativa eventi]

\section{Event #1: (324) Bamberga}
\subsection{Event Parameters}
[Tabella parametri dettagliati]

\subsection{Ground Track}
[TikZ map con path centrale e limiti]

\end{document}
```

**Esempio TikZ Map:**

```latex
\begin{tikzpicture}[scale=0.8]
  % Griglia geografica
  \draw[gray,very thin] (0,0) rectangle (12,8);
  
  % Path centrale
  \draw[red,very thick] (1.2,6.5) -- (3.5,5.8) -- ... -- (8.3,2.1);
  
  % Limiti 1-sigma
  \draw[red,dotted] (1.3,6.6) -- ... -- (8.4,2.2);  % Nord
  \draw[red,dotted] (1.1,6.4) -- ... -- (8.2,2.0);  % Sud
  
  % Punti notevoli
  \filldraw[blue] (1.2,6.5) circle (2pt) node[above] {Start};
  \filldraw[blue] (8.3,2.1) circle (2pt) node[above] {End};
\end{tikzpicture}
```

**Configurazione:**

```json
"latex": {
  "compile_pdf": false,
  "include_tikz_map": true,
  "include_uncertainty_plot": true,
  "document_class": "article"
}
```

---

### 3. PDF Format

**Caratteristiche:**
- Compila automaticamente LaTeX → PDF
- Usa `pdflatex` (2 passaggi per references)
- Cerca pdflatex in percorsi standard
- Output pronto per distribuzione

**Requisiti:**
- MacTeX / TeX Live installato
- pdflatex in PATH o `/usr/local/texlive/*/bin/*/pdflatex`

**Workflow:**

```
1. Genera .tex da template
2. Esegue: pdflatex -interaction=nonstopmode file.tex
3. Esegue: pdflatex (secondo passaggio per refs)
4. Pulisce file ausiliari (.aux, .log)
5. Ritorna PDF finale
```

**Configurazione:**

```json
"latex": {
  "compile_pdf": true,
  "include_tikz_map": true
}
```

---

### 4. XML Format (Occult4)

**Caratteristiche:**
- Schema compatibile con **Occult4.2.0**
- Importabile in **OccultWatcher Cloud**
- Validazione XSD
- Include incertezze opzionali

**Struttura XML:**

```xml
<?xml version="1.0" encoding="UTF-8"?>
<OccultationPredictions version="Occult4.2.0">
  
  <Metadata>
    <Generator>ITALOccultCalc</Generator>
    <Software>v1.0.0</Software>
    <GenerationDate>2025-11-23</GenerationDate>
    <EventCount>3</EventCount>
  </Metadata>
  
  <Events>
    <Event>
      <Asteroid>
        <Number>324</Number>
        <Name>Bamberga</Name>
        <Diameter unit="km">228.00</Diameter>
        <AbsoluteMagnitude>6.82</AbsoluteMagnitude>
      </Asteroid>
      
      <Star>
        <Catalog>TYC</Catalog>
        <ID>5865-00764-1</ID>
        <RA unit="degrees">56.82340000</RA>
        <Dec unit="degrees">8.45670000</Dec>
        <Magnitude>7.50</Magnitude>
      </Star>
      
      <Occultation>
        <TimeUTC>2025-12-08 22:44:13</TimeUTC>
        <JulianDate>2460653.447370</JulianDate>
        <MagnitudeDrop>3.07</MagnitudeDrop>
        <Duration unit="seconds">11.50</Duration>
        <ShadowWidth unit="km">228.0</ShadowWidth>
        <ShadowVelocity unit="km/s">19.80</ShadowVelocity>
        <PathUncertainty unit="km">12.0</PathUncertainty>
        <TimeUncertainty unit="seconds">2.50</TimeUncertainty>
      </Occultation>
      
      <CentralPath>
        <Point lat="45.500000" lon="9.200000"/>
        <Point lat="44.500000" lon="11.300000"/>
        <Point lat="43.800000" lon="11.200000"/>
        <Point lat="41.900000" lon="12.500000"/>
        <Point lat="40.800000" lon="14.300000"/>
      </CentralPath>
      
      <Priority>
        <Score>11</Score>
        <Class>★★★</Class>
      </Priority>
    </Event>
  </Events>
  
</OccultationPredictions>
```

**Import in OccultWatcher:**

1. Genera XML con ITALOccultCalc
2. Login OccultWatcher Cloud
3. Import → Select XML file
4. Validate schema
5. Eventi disponibili per osservatori

**Configurazione:**

```json
"xml": {
  "schema_version": "Occult4.2.0",
  "include_uncertainties": true
}
```

---

### 5. JSON Format

**Caratteristiche:**
- Struttura dati per API REST
- Pretty-print opzionale
- Metadata separati
- Facile parsing

**Struttura JSON:**

```json
{
  "metadata": {
    "generator": "ITALOccultCalc",
    "software": "v1.0.0",
    "date_generated": "2025-11-23",
    "event_count": 3
  },
  
  "events": [
    {
      "asteroid": {
        "number": 324,
        "name": "Bamberga",
        "diameter_km": 228.00,
        "absolute_magnitude": 6.82
      },
      
      "star": {
        "catalog": "TYC",
        "id": "5865-00764-1",
        "ra_deg": 56.8234,
        "dec_deg": 8.4567,
        "magnitude": 7.5
      },
      
      "occultation": {
        "time_utc": "2025-12-08 22:44:13",
        "jd": 2460653.447370,
        "mag_drop": 3.07,
        "duration_sec": 11.50,
        "shadow_width_km": 228.0,
        "shadow_velocity_kms": 19.80,
        "path_uncertainty_km": 12.0,
        "time_uncertainty_sec": 2.50
      },
      
      "priority": {
        "score": 11,
        "class": "★★★"
      }
    }
  ]
}
```

**Utilizzo:**

```python
# Python
import json

with open('occultations.json') as f:
    data = json.load(f)
    
for event in data['events']:
    print(f"{event['asteroid']['name']}: {event['priority']['class']}")
```

```javascript
// JavaScript
fetch('occultations.json')
  .then(response => response.json())
  .then(data => {
    data.events.forEach(event => {
      console.log(`${event.asteroid.name}: ${event.priority.class}`);
    });
  });
```

**Configurazione:**

```json
"json": {
  "pretty_print": true,
  "indent_spaces": 2,
  "include_metadata": true
}
```

---

### 6. IOTA Card Format (JPG)

**Caratteristiche:**
- Scheda grafica stile **IOTA** (International Occultation Timing Association)
- Layout landscape 10"×7.5"
- Informazioni essenziali per osservatori
- Grande leggibilità (proiezione/stampa)
- Generata da LaTeX → PDF → JPG

**Layout Carta IOTA:**

```
┌─────────────────────────────────────────────────────────────────┐
│                   ASTEROID OCCULTATION                          │
│        (324) Bamberga occults TYC 5865-00764-1                  │
│                  2025-12-08 22:44:13 UT                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  EVENT DETAILS              │  STAR DATA                        │
│  ────────────────           │  ──────────                       │
│  Mag drop:  3.1 mag         │  Star mag:    7.5                 │
│  Duration:  11.5 sec        │  RA (J2000):  +56°49'24"          │
│  Shadow:    228 km          │  Dec (J2000): +08°27'24"          │
│  Speed:     19.8 km/s       │  Priority:    ★★★ (11/11)         │
│  Path unc:  ±12 km          │                                   │
│                             │                                   │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│                      [MAP: Ground track]                        │
│                                                                  │
│                                                                  │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

**Workflow Generazione:**

1. Crea .tex con layout IOTA
2. Compila con `pdflatex`
3. Converti PDF → JPG con `sips` (macOS) o ImageMagick
4. Risoluzione: 1920×1080 (Full HD)
5. Qualità JPEG: 90%

**Configurazione:**

```json
"iota_card": {
  "image_width": 1920,
  "image_height": 1080,
  "include_map": true,
  "include_finder_chart": true,
  "map_projection": "mercator"
}
```

**Utilizzo:**

- Stampa A4 landscape per osservazioni
- Proiezione su schermo sito osservativo
- Condivisione social media
- Submission IOTA per coordinamento

---

## Configurazione Completa

### File JSON Esempio: `preset_output_config.json`

```json
{
  "description": "Configurazione output multi-formato ITALOccultCalc",
  "version": "1.0",
  
  "output": {
    "format": "PDF",
    "file": "occultations_output.pdf",
    "append_mode": false,
    
    "text": {
      "include_header": true,
      "include_map_ascii": true,
      "width_columns": 80
    },
    
    "latex": {
      "compile_pdf": true,
      "include_tikz_map": true,
      "include_uncertainty_plot": true,
      "document_class": "article"
    },
    
    "xml": {
      "schema_version": "Occult4.2.0",
      "include_uncertainties": true
    },
    
    "json": {
      "pretty_print": true,
      "indent_spaces": 2,
      "include_metadata": true
    },
    
    "iota_card": {
      "image_width": 1920,
      "image_height": 1080,
      "include_map": true,
      "include_finder_chart": true,
      "map_projection": "mercator"
    },
    
    "path": {
      "resolution_km": 100.0,
      "include_italy_highlight": true,
      "highlight_countries": ["Italy", "France", "Switzerland", "Austria"]
    }
  }
}
```

---

## API Programmatica

### Esempi Utilizzo

#### 1. Singolo Evento, Formato TEXT

```cpp
OccultationEvent event = computeOccultation(asteroid, star);

OutputManager mgr;
OutputOptions opts;
opts.format = OutputFormat::TEXT;
opts.output_file = "event.txt";
mgr.setOptions(opts);

mgr.writeEvent(event);
```

#### 2. Multi-Evento, Formato PDF

```cpp
std::vector<OccultationEvent> events = searchOccultations(period);

OutputManager mgr;
OutputOptions opts;
opts.format = OutputFormat::PDF;
opts.output_file = "report.pdf";
opts.latex_compile_pdf = true;
opts.latex_include_tikz_map = true;
mgr.setOptions(opts);

mgr.writeEvents(events);
```

#### 3. Export XML per OccultWatcher

```cpp
OutputManager mgr;
OutputOptions opts;
opts.format = OutputFormat::XML_OCCULT4;
opts.output_file = "for_occultwatch.xml";
opts.xml_schema_version = "Occult4.2.0";
opts.xml_include_uncertainties = true;
mgr.setOptions(opts);

mgr.writeEvents(events);
```

#### 4. JSON per API Web

```cpp
OutputManager mgr;
OutputOptions opts;
opts.format = OutputFormat::JSON;
opts.output_file = "api_response.json";
opts.json_pretty_print = true;
opts.json_include_metadata = true;
mgr.setOptions(opts);

mgr.writeEvents(events);
```

#### 5. IOTA Cards per Campagna

```cpp
OutputManager mgr;
OutputOptions opts;
opts.format = OutputFormat::IOTA_CARD;
opts.iota_image_width = 1920;
opts.iota_image_height = 1080;
mgr.setOptions(opts);

// Genera una card per evento
for (const auto& evt : events) {
    std::string filename = "iota_card_" + evt.asteroid_name + ".jpg";
    mgr.writeEvent(evt, filename);
}
```

#### 6. Da Configurazione JSON

```cpp
ConfigManager config;
config.loadFromJson("preset_output_config.json");

OutputManager mgr(config);
mgr.writeEvents(events);  // Usa formato da config
```

---

## Test e Validazione

### Compilazione

```bash
cd IOccultCalc
cmake --build build --target test_output_formats
```

### Esecuzione Test

```bash
./build/examples/test_output_formats
```

**Output Atteso:**

```
╔═══════════════════════════════════════════════════════════════╗
║        TEST OUTPUT MANAGER - MULTI-FORMAT SYSTEM              ║
╚═══════════════════════════════════════════════════════════════╝

================================================================
TEST 1: TEXT FORMAT
================================================================
Output file: test_output_text.txt
Status: ✓ SUCCESS
Events: 3

================================================================
TEST 2: LATEX FORMAT
================================================================
Output file: test_output_latex.tex
Status: ✓ SUCCESS
Events: 3

================================================================
TEST 3: PDF FORMAT
================================================================
Output file: test_output_report.pdf
Status: ✓ SUCCESS
Events: 3

================================================================
TEST 4: XML FORMAT (Occult4 compatible)
================================================================
Output file: test_output_occult4.xml
Status: ✓ SUCCESS
Events: 3
Schema: Occult4.2.0
Import to: OccultWatcher Cloud

================================================================
TEST 5: JSON FORMAT
================================================================
Output file: test_output_structured.json
Status: ✓ SUCCESS
Events: 3
Format: Pretty-printed with metadata

================================================================
TEST 6: IOTA CARD FORMAT (JPG)
================================================================
Output file: test_output_iota_card.jpg
Status: ✓ SUCCESS
Resolution: 1920x1080
Style: IOTA observation card

✓ All tests completed!
```

---

## Casi d'Uso

### 1. Campagna Osservativa Nazionale

**Scenario:** Coordinamento osservatori italiani per Bamberga

```cpp
// Genera report PDF scientifico
OutputOptions pdf_opts;
pdf_opts.format = OutputFormat::PDF;
pdf_opts.output_file = "bamberga_campaign.pdf";
pdf_opts.latex_include_tikz_map = true;
manager.setOptions(pdf_opts);
manager.writeEvents({bamberga_event});

// Export XML per OccultWatcher
OutputOptions xml_opts;
xml_opts.format = OutputFormat::XML_OCCULT4;
xml_opts.output_file = "bamberga_for_ow.xml";
manager.setOptions(xml_opts);
manager.writeEvents({bamberga_event});

// Genera IOTA cards per osservatori
OutputOptions iota_opts;
iota_opts.format = OutputFormat::IOTA_CARD;
manager.setOptions(iota_opts);
manager.writeEvent(bamberga_event, "bamberga_iota.jpg");
```

### 2. Ricerca Automatica Mensile

**Scenario:** Scan automatico 1000 asteroidi, pubblicazione web

```cpp
auto events = searchMonth(2026, 1, 1000);  // Gennaio 2026

// JSON per API web
OutputOptions json_opts;
json_opts.format = OutputFormat::JSON;
json_opts.output_file = "jan2026_api.json";
json_opts.json_pretty_print = true;
manager.setOptions(json_opts);
manager.writeEvents(events);

// TEXT per email/newsletter
OutputOptions text_opts;
text_opts.format = OutputFormat::TEXT;
text_opts.output_file = "jan2026_newsletter.txt";
manager.setOptions(text_opts);
manager.writeEvents(events);
```

### 3. Paper Scientifico

**Scenario:** Pubblicazione risultati campagna 2025

```cpp
// LaTeX per paper submission
OutputOptions latex_opts;
latex_opts.format = OutputFormat::LATEX;
latex_opts.output_file = "paper_results.tex";
latex_opts.latex_document_class = "article";
latex_opts.latex_include_tikz_map = true;
latex_opts.latex_include_uncertainty_plot = true;
manager.setOptions(latex_opts);
manager.writeEvents(campaign_events);

// Include nel paper principale
// \input{paper_results.tex}
```

### 4. Coordinamento Internazionale

**Scenario:** Collaborazione IOTA/EAON/Japanese teams

```cpp
// Export XML per OccultWatcher (globale)
OutputOptions xml_global;
xml_global.format = OutputFormat::XML_OCCULT4;
xml_global.output_file = "global_predictions.xml";
manager.setOptions(xml_global);
manager.writeEvents(all_events);

// PDF report italiano
OutputOptions pdf_italy;
pdf_italy.format = OutputFormat::PDF;
pdf_italy.output_file = "previsioni_italia.pdf";
pdf_italy.include_italy_highlight = true;
manager.setOptions(pdf_italy);
manager.writeEvents(italy_events);
```

---

## Troubleshooting

### PDF Compilation Failed

**Problema:** `compilePDF()` ritorna `false`

**Soluzioni:**

1. Verifica pdflatex installato:
   ```bash
   which pdflatex
   # Output: /usr/local/texlive/2025/bin/universal-darwin/pdflatex
   ```

2. Installa MacTeX:
   ```bash
   brew install --cask mactex
   ```

3. Aggiungi a PATH:
   ```bash
   export PATH="/usr/local/texlive/2025/bin/universal-darwin:$PATH"
   ```

4. Compila manualmente per debug:
   ```bash
   pdflatex -interaction=nonstopmode file.tex
   cat file.log  # Vedi errori
   ```

### XML Import Failed in OccultWatcher

**Problema:** Schema validation error

**Soluzioni:**

1. Verifica schema version:
   ```json
   "xml": {
     "schema_version": "Occult4.2.0"  // Usa versione corretta
   }
   ```

2. Valida XML:
   ```bash
   xmllint --schema occult4_schema.xsd file.xml
   ```

3. Controlla encoding UTF-8:
   ```xml
   <?xml version="1.0" encoding="UTF-8"?>
   ```

### IOTA Card Generation Failed

**Problema:** JPG non generato

**Soluzioni:**

1. Verifica `sips` (macOS):
   ```bash
   sips --version
   ```

2. Alternativa ImageMagick:
   ```bash
   brew install imagemagick
   convert file.pdf file.jpg
   ```

3. Aggiorna `writeIotaCard()` per usare `convert` invece di `sips`

---

## Roadmap

### V1.1 (Q1 2026)

- [ ] Support KML export (Google Earth)
- [ ] Support CSV export (Excel)
- [ ] Finder charts automatici (star field)
- [ ] Email notification system

### V1.2 (Q2 2026)

- [ ] Web dashboard interattivo
- [ ] Real-time updates (WebSocket)
- [ ] Mobile app export (iOS/Android)
- [ ] Multi-language support

### V2.0 (Q3 2026)

- [ ] Cloud rendering (AWS Lambda)
- [ ] Batch processing parallelo
- [ ] ML-based priority tuning
- [ ] Integration Telegram bot

---

## Riferimenti

- **IOTA**: https://occultations.org
- **OccultWatcher**: https://www.asteroidoccultation.com
- **Occult4**: http://www.lunar-occultations.com/occult4
- **Gaia DR3**: https://gea.esac.esa.int/archive/
- **LaTeX TikZ**: https://tikz.dev

---

*Documentazione generata: 2025-11-23*  
*ITALOccultCalc v1.0*
