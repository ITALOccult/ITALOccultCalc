# ITALOccultCalc: Guida alla Configurazione

Questa guida descrive tutte le sezioni e i parametri disponibili per configurare `italoccultcalc`. È possibile utilizzare il formato **OOP** (compatibile con OrbFit), **YAML** o **TOML**.

---

## 1. Sezione [object]
Definisce l'oggetto o la lista di oggetti da analizzare.

| Opzione | Descrizione | Esempio |
| :--- | :--- | :--- |
| `asteroid_list` | Range o lista di asteroidi (numerati). | `"1..2000"`, `"1,2,5"` |
| `asteroid_list_file` | Percorso di un file di testo con nomi/numeri. | `"targets.txt"` |
| `asteroid_number` | Numero di un singolo asteroide. | `17030` |
| `id` | Identificativo stringa dell'oggetto. | `"Ceres"` |
| `min_diameter` | Diametro minimo (km) per il filtraggio. | `5.0` |
| `max_diameter` | Diametro massimo (km) per il filtraggio. | `500.0` |
| `max_asteroids` | Limite massimo di oggetti da processare in una sessione. | `100` |

---

## 2. Sezione [propagation] (o [propag])
Impostazioni per l'integratore numerico delle orbite.

| Opzione | Descrizione | Esempio |
| :--- | :--- | :--- |
| `type` | Algoritmo di integrazione (`RK4`, `RKF78`, `RA15`). | `"RKF78"` |
| `step_size` | Passo temporale in giorni. | `0.1` (2.4 ore) |

---

## 3. Sezione [search]
Parametri per la ricerca delle occultazioni.

| Opzione | Descrizione | Esempio |
| :--- | :--- | :--- |
| `start_mjd_tdb` | Data inizio ricerca in Modified Julian Day. | `61041.0` |
| `end_mjd_tdb` | Data fine ricerca in Modified Julian Day. | `61071.0` |
| `step_days` | Passo temporale per lo screening iniziale. | `0.04` (~1 ora) |
| `mag_limit` | Magnitudine massima delle stelle da cercare. | `14.0` |
| `max_separation` | Massima separazione angolare (deg) per i candidati. | `2.0` |
| `corridor_width_deg` | Larghezza del corridoio di ricerca lungo l'orbita. | `0.5` |

---

## 4. Sezione [perturbations] (o [pert])
Modello dinamico delle forze gravitazionali.

| Opzione | Descrizione | Esempio |
| :--- | :--- | :--- |
| `planets` | Abilita perturbazioni dei pianeti principali. | `true` |
| `asteroids` | Numero di asteroidi perturbatori massicci (0, 17, o 100). | `17` |
| `relativity` | Abilita correzioni relativistiche Post-Newtoniane. | `true` |

---

## 5. Sezione [output]
Configurazione dei report e delle esportazioni.

| Opzione | Descrizione | Esempio |
| :--- | :--- | :--- |
| `verbosity` | Livello di dettaglio console (0=min, 2=max). | `1` |
| `output_dir` | Sottodirectory dove salvare tutti i file generati. | `"risultati_gennaio"` |
| `output_file` | Nome del report testuale (.txt). | `"report.txt"` |
| `xml_file` | Esportazione XML compatibile con Occult4. | `"events.xml"` |
| `json_file` | Esportazione dati strutturati per il web. | `"data.json"` |
| `kml_file` | Tracciati ombra per Google Earth. | `"mappe.kml"` |

---

## 6. Sezione [orbit_fitting]
Parametri per il raffinamento orbitale tramite osservazioni.

| Opzione | Descrizione | Esempio |
| :--- | :--- | :--- |
| `enable_fitting` | Abilita fit iterativo se sono presenti osservazioni. | `true` |
| `observation_source` | Fonte dati (`AstDyS`, `MPC`). | `"AstDyS"` |
| `max_iterations` | Numero massimo di iterazioni del fit. | `10` |
| `outlier_threshold_sigma` | Soglia sigma per il rigetto di dati anomali. | `3.0` |

---

## 7. Sezione [gaia]
Configurazione dell'accesso al catalogo stellare.

| Opzione | Descrizione | Esempio |
| :--- | :--- | :--- |
| `version` | Versione del catalogo (es. `DR3`). | `"DR3"` |
| `use_local_cache` | Utilizza cache locale file .bin per le stelle. | `true` |
| `cache_directory` | Directory dove risiedono i file delle stelle. | `"data/gaia_dr3"` |

---

## 8. Sezione [observer]
Vincoli geografici legati al sito osservativo.

| Opzione | Descrizione | Esempio |
| :--- | :--- | :--- |
| `latitude` | Latitudine del sito (Gradi decimali). | `45.42` |
| `longitude` | Longitudine del sito (Gradi decimali). | `9.41` |
| `max_distance_km` | Massima distanza desiderata dal centro scia. | `500.0` |
| `min_mag_drop` | Calo di luce minimo richiesto (Magnitudini). | `0.1` |

---

## 9. Sezione [debug] (Solo per sviluppatori)
Strumenti di diagnostica.

| Opzione | Descrizione | Esempio |
| :--- | :--- | :--- |
| `check_epoch` | MJD preciso per verificare la posizione dell'asteroide. | `61000.5` |
| `target_star` | ID Gaia di una stella specifica da monitorare. | `877859914797936896` |
| `force_target_star`| Calcola l'occultazione *solo* per la stella specificata. | `true` |

---

**Nota Informativa**: 
Per convertire questa guida in PDF, è possibile aprirla con un visualizzatore Markdown (come quello incluso in VS Code o Obsidian) e selezionare "Esporta come PDF".
