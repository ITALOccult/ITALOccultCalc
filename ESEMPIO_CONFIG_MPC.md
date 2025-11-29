# ============================================================================
# ESEMPIO: Configurazione MPC (Minor Planet Center)
# ============================================================================
# Esempi di configurazione per usare elementi orbitali da MPC invece di AstDyS
# ============================================================================

# ════════════════════════════════════════════════════════════════════════
# OPZIONE 1: MPC - Download Automatico (Consigliato)
# ════════════════════════════════════════════════════════════════════════

orbit_source.
    .type = 'mpc'
    .auto_download = .TRUE.
    
    # MPC scaricherà automaticamente MPCORB.DAT (elementi kepleriani)
    # File: ~50 MB, contiene tutti gli asteroidi numerati + non numerati
    # URL: https://minorplanetcenter.net/iau/MPCORB/MPCORB.DAT

# ════════════════════════════════════════════════════════════════════════
# OPZIONE 2: MPC - File Locale (se già scaricato)
# ════════════════════════════════════════════════════════════════════════

orbit_source.
    .type = 'mpc'
    .auto_download = .FALSE.
    .local_file = '/path/to/MPCORB.DAT'
    
    # Vantaggi file locale:
    # - Più veloce (no download)
    # - Offline capability
    # - Controllo versione
    
    # Svantaggi:
    # - Richiede aggiornamento manuale
    # - File grande (~50 MB)

# ════════════════════════════════════════════════════════════════════════
# OPZIONE 3: MPC con Cache Locale
# ════════════════════════════════════════════════════════════════════════

orbit_source.
    .type = 'mpc'
    .auto_download = .TRUE.
    .cache_directory = '/Users/michelebigi/.ioccultcalc/mpc_cache'
    .cache_max_age_days = 7             # Ri-scarica dopo 7 giorni
    
    # Sistema ibrido:
    # - Prima esecuzione: download automatico in cache
    # - Esecuzioni successive: usa cache (se < 7 giorni)
    # - Dopo 7 giorni: ri-download automatico

# ════════════════════════════════════════════════════════════════════════
# OPZIONE 4: MPC con Orbit Fitting (Alta Precisione)
# ════════════════════════════════════════════════════════════════════════

orbit_source.
    .type = 'mpc'
    .auto_download = .TRUE.
    
    # Abilita orbit fitting con osservazioni MPC
    .fit_orbit = .TRUE.
    .outlier_threshold = 3.0
    .max_iterations = 20
    .convergence_tolerance = 1.0e-6
    
    # Osservazioni MPC (formato MPC 80-col)
    .observations_source = 'mpc'        # Usa osservazioni da MPC web service
    # oppure
    # .observations_file = 'observations.txt'  # File locale con osservazioni

# ════════════════════════════════════════════════════════════════════════
# CONFRONTO: MPC vs AstDyS
# ════════════════════════════════════════════════════════════════════════

# MPC (Minor Planet Center):
# ✅ Elementi Kepleriani standard
# ✅ Tutti gli asteroidi (numerati + non numerati)
# ✅ Aggiornamenti frequenti
# ✅ Formato standardizzato (MPCORB.DAT)
# ✅ Osservazioni in formato MPC 80-col
# ⚠️  Epoca elementi fisso (di solito 2000.0)
# ⚠️  NO covarianza (incertezza semplificata)

# AstDyS (Asteroids Dynamic Site):
# ✅ Elementi Equinoziali (migliori per propagazione)
# ✅ Epoca recente (aggiornata)
# ✅ Matrice covarianza completa
# ✅ Osservazioni formato .rwo (ricche di info)
# ✅ Fitting orbit già eseguito
# ⚠️  Solo asteroidi numerati (non provvisori)
# ⚠️  Download singolo per asteroide

# ════════════════════════════════════════════════════════════════════════
# QUANDO USARE MPC
# ════════════════════════════════════════════════════════════════════════

# Usa MPC quando:
# 1. Servono asteroidi NON numerati (designazioni provvisorie)
# 2. Vuoi un unico file con tutti gli elementi
# 3. Non serve massima precisione (screening veloce)
# 4. Compatibilità con altri software (formato standard)
# 5. AstDyS non disponibile o troppo lento

# ════════════════════════════════════════════════════════════════════════
# QUANDO USARE AstDyS
# ════════════════════════════════════════════════════════════════════════

# Usa AstDyS quando:
# 1. Serve MASSIMA PRECISIONE (predizioni professionali)
# 2. Vuoi orbit fitting con 10.000+ osservazioni
# 3. Serve matrice covarianza per incertezze realistiche
# 4. Solo asteroidi numerati (i più importanti)
# 5. Epoca recente per migliore accuratezza

# ════════════════════════════════════════════════════════════════════════
# WORKFLOW IBRIDO CONSIGLIATO
# ════════════════════════════════════════════════════════════════════════

# FASE 1: Screening con MPC (veloce)
orbit_source.
    .type = 'mpc'
    .auto_download = .TRUE.
    .fit_orbit = .FALSE.

# → Output: lista asteroidi candidati

# FASE 2: Predizioni professionali con AstDyS (preciso)
orbit_source.
    .type = 'astdys'
    .local_eq1_directory = 'test_astdys_download'
    .local_rwo_directory = 'test_astdys_download'
    .fit_orbit = .TRUE.
    .max_iterations = 20
    .convergence_tolerance = 1.0e-8

# → Output: predizioni IOTA/Euraster professionali

# ════════════════════════════════════════════════════════════════════════
# ESEMPIO COMPLETO: Preset MPC Veloce
# ════════════════════════════════════════════════════════════════════════

general.
    .propagator = 'Chebyshev'
    .step_size_days = 1.0
    .output_format = 'text'
    .verbose = .FALSE.

orbit_source.
    .type = 'mpc'                       # Minor Planet Center
    .auto_download = .TRUE.
    .cache_directory = '/Users/michelebigi/.ioccultcalc/mpc_cache'
    .cache_max_age_days = 7
    .fit_orbit = .FALSE.                # Screening veloce

time.
    .start_date = '2025-12-01'
    .end_date = '2025-12-31'
    .interval_days = 1.0

asteroids.
    .selection_mode = 'number_range'
    .number_range_start = 1
    .number_range_end = 1000

search.
    .max_magnitude = 13.0
    .search_radius_deg = 0.1
    .min_probability = 0.005

output.
    .output_file = 'occultations_mpc_screening.txt'
    .export_asteroid_list = .TRUE.
    .asteroid_list_file = 'asteroids_mpc_candidates.txt'

# ════════════════════════════════════════════════════════════════════════
# DOWNLOAD MANUALE MPCORB.DAT
# ════════════════════════════════════════════════════════════════════════

# Se preferisci scaricare manualmente:
# 
# 1. Vai a: https://minorplanetcenter.net/iau/MPCORB/MPCORB.DAT
# 2. Salva in: /Users/michelebigi/.ioccultcalc/mpc_cache/MPCORB.DAT
# 3. Configura preset:
#    orbit_source.
#        .type = 'mpc'
#        .local_file = '/Users/michelebigi/.ioccultcalc/mpc_cache/MPCORB.DAT'
#        .auto_download = .FALSE.

# ════════════════════════════════════════════════════════════════════════
# FORMATO MPCORB.DAT
# ════════════════════════════════════════════════════════════════════════

# MPCORB.DAT contiene (fixed-width columns):
# - Numero/designazione
# - Magnitude assoluta H
# - Slope parameter G
# - Epoca (Julian Date)
# - Anomalia media
# - Argomento perihelio
# - Longitudine nodo ascendente
# - Inclinazione
# - Eccentricità
# - Semiasse maggiore
# - Incertezza (U parameter)
# - Riferimento
# - Numero osservazioni
# - Numero opposizioni
# 
# Esempio linea:
# 00433    10.4  0.15 K252N  88.27649  178.66970  304.32276   10.82925  0.2227044  1.4581035  0 MPO681028  7866  75 1893-2024 0.48 M-v 3Eh MPCW      5088          (433) Eros

# ============================================================================

