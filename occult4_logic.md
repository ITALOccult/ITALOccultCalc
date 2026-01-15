# Analisi di Occult4 e Differenze con IOccultCalc

Questo documento analizza il funzionamento interno di **Occult4** e lo confronta con l'attuale progetto **IOccultCalc** (italoccultcalc), basandosi sullo studio del codice sorgente C# e della documentazione estratta.

## 1. Architettura Generale

| Caratteristica | Occult4 | IOccultCalc |
| :--- | :--- | :--- |
| **Linguaggio** | C# (.NET Framework) | C++17 |
| **Interfaccia** | GUI (Windows Forms) | CLI / Libreria C++ |
| **Piattaforma** | Windows (Native) | Cross-platform (Linux, macOS) |
| **Parallelismo** | Limitato (BackgroundWorkers) | Elevato (OpenMP, Parallel STL) |

## 2. Logica di Calcolo (Occultazioni di Asteroidi)

### Occult4
II processo di ricerca in Occult4 è suddiviso in due fasi principali, simili a IOccultCalc, ma con alcune differenze tecniche:

1.  **Screening (Phase 1)**:
    *   Usa file di effemeridi giornaliere pre-calculate (`.bin` di ~1.2MB per ogni 10.000 giorni).
    *   Cerca nel catalogo stellare (Gaia, UCAC4) usando indici binari proprietari (`.inx`).
    *   Effettua un controllo di "vicinanza" grossolano basato sul moto giornaliero dell'asteroide.

### Gestione Effemeridi (.bin)
Occult4 ottiene le effemeridi planetarie in due modi:
- **JPL DE (High Precision)**: Se l'utente fornisce i file JPL Development Ephemeris (es. DE405, DE421), Occult4 li usa per generare file `.bin` giornalieri ad alta precisione tramite il metodo `AllPlanetsXYZ_Daily`.
- **Modelli Interni (Low Precision)**: In assenza di file JPL, Occult4 utilizza modelli matematici interni (espansioni in serie come **VSOP87** per i pianeti e modelli specifici per Plutone) i cui coefficienti sono memorizzati in file `.bin` fissi nella cartella `Resource Files`.

Questi file `.bin` intermedi (`[JD]_XYZ.bin`) vengono generati per velocizzare le ricerche massive, evitando di ricalcolare le posizioni planetarie ad ogni iterazione della Phase 1.

2.  **Raffinamento (Phase 2)**:
    *   Utilizza la formulazione del **Piano Fondamentale di Bessel**.
    *   Calcola le coordinate $X, Y$ (distanza proiettata) in tre punti temporali.
    *   Applica un **Fit Quadratico** per trovare l'istante di minima separazione ($T_0$).
    *   Può scaricare elementi orbitali direttamente da **JPL Horizons** in tempo reale per migliorare la precisione.

### IOccultCalc (italoccultcalc)
1.  **Phase 1 (Screening)**:
    *   Propagazione dinamica (Kepleriana o semplificata) durante la ricerca.
    *   Uso del catalogo Gaia compresso (`GaiaMag18Catalog`) o SQLite.
    *   Filtraggio tramite "corridor query" spaziale altamente ottimizzata.
2.  **Phase 2 (Geometry)**:
    *   Propagazione ad alta precisione (**RKF78** o **RA15**) con modelli di forza complessi (perturbazioni planetarie, relatività).
    *   Calcolo preciso del **Shadow Path** basato sulla geometria istantanea geocentrica/topocentrica.
    *   Integrazione con **AstDyS** per parametri di incertezza ed elementi medi.

## 3. Differenze Chiave

### Precisione e Propagazione
- **Occult4**: Utilizza un integratore numerico di tipo **Cowell multistep** (integrazione diretta delle equazioni del moto in coordinate cartesiane). Implementa un metodo "sum-of-differences" (tipicamente di ordine 8) con un passo fisso di 1 giorno. Include le perturbazioni dei 9 pianeti maggiori. Si affida molto a effemeridi pre-calculate o a JPL Horizons come "verità". L'integratore interno è meno avanzato di quello di IOccultCalc (es. manca lo step-size variabile).
- **IOccultCalc**: Implementa un motore di propagazione numerico completo (AstDyn) basato su algoritmi **RKF78** (Runge-Kutta-Fehlberg) o **RA15** (Radau) a passo variabile. Permette di simulare orbite partendo da dati grezzi (`.eq1`, `.rwo`) con modelli di forza che includono perturbazioni planetarie, relatività e parametri non gravitazionali (Yarkovsky).

### Gestione Dati Stellari
- **Occult4**: Progettato per l'uso interattivo da PC, usa file binari piatti ottimizzati per minimizzare l'input/output su disco.
- **IOccultCalc**: Pensato per processamenti massivi e automazione, usa strutture dati che facilitano la ricerca parallela e il caricamento selettivo.

### Output e Visualizzazione
- **Occult4**: Eccelle nella visualizzazione (mappe interattive, grafici di luce, reportistica pronta per la stampa).
- **IOccultCalc**: Genera dati tecnici (JSON, KML, XML per Occult4) destinati ad essere consumati da altri strumenti o per generare report PDF/A4 automatizzati.

## 4. Conclusione

**Occult4** è lo standard per l'astrofilo che desidera pianificare osservazioni individuali con una interfaccia amichevole. **IOccultCalc** è l'evoluzione "engine" ad alte prestazioni, che centralizza la logica di calcolo (tramite la nuova classe `OccultationEngine`) e garantisce una precisione scientifica superiore grazie all'integrazione di AstDyn.

L'implementazione di `OccultationEngine` mira proprio a colmare il gap tra le due fasi di IOccultCalc, rendendo il workflow fluido e robusto quanto quello di Occult4, ma con la potenza del C++.
