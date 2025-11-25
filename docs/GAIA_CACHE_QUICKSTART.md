# Guida Rapida: Sistema Cache Gaia DR3

## Setup Iniziale (Una Volta Sola)

### 1. Download della Cache

Scarica la fascia principale degli asteroidi (±25° eclittica, mag < 15):

```bash
./gaia_cache_downloader --mainbelt 15.0 --verbose
```

**Output:**
```
╔═══════════════════════════════════════════════════════════════╗
║ GAIA Cache Downloader v1.0                                    ║
╚═══════════════════════════════════════════════════════════════╝

Download Main Belt Region:
  Fascia eclittica: ±25°
  RA: 0° - 360°
  Dec: -25° - +25°
  Magnitudine: < 15.0

Inizio download...
[████████████████░░░░░░░░░░░░░░░░░░░░░░░░░░] 32% (189/586 tiles)
```

**Tempo:** ~10-15 minuti per download completo  
**Spazio disco:** ~80-120 MB  
**Stelle:** ~250,000-300,000

### 2. Verifica Cache

Controlla che il download sia completato:

```bash
./gaia_cache_downloader --stats
```

**Output atteso:**
```
Statistiche Cache:
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  Tiles totali:      586
  Stelle totali:     284523
  Dimensione:        98.4 MB
  Copertura cielo:   12600.0 deg²
  Magnitudine:       3.50 - 15.00
  Ultimo aggiorn.:   2025-01-24 15:30:45
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

## Utilizzo Quotidiano

### Esecuzione con Cache (Raccomandata)

```bash
./italoccultcalc -v preset_large_asteroids_jan2026.oop
```

Il file `preset_large_asteroids_jan2026.oop` ha già configurato:
```
gaia.
    .use_local_cache = .TRUE.
    .cache_directory = '~/.ioccultcalc/catalogs'
```

**Output:**
```
================================================================
QUERY CATALOGO STELLE GAIA DR3
================================================================

Modalità: Cache locale
Cache dir: ~/.ioccultcalc/catalogs

Caricamento cache locale...
✓ Cache caricata: 284523 stelle in 586 tiles HEALPix
  Copertura: 12600.0 deg²
  Magnitudine: 3.5 - 15.0

Query cache locale...
✓ Trovate 9945 stelle in cache
```

**Tempo:** ~2-5 secondi (NO network)  
**Bandwidth:** 0 MB

### Esecuzione Online (Senza Cache)

Se non hai la cache o vuoi usare query online:

```bash
# Modifica temporaneamente il config
sed -i.bak 's/use_local_cache = .TRUE./use_local_cache = .FALSE./' config.oop
./italoccultcalc -v config.oop
```

**Oppure** crea un config separato per online:

```bash
cp preset_large_asteroids_jan2026.oop preset_online.oop
# Edita preset_online.oop: .use_local_cache = .FALSE.
./italoccultcalc -v preset_online.oop
```

**Tempo:** ~15-20 secondi (richiede network)  
**Bandwidth:** ~1-2 MB per ricerca

## Opzioni di Download Avanzate

### Download Banda Eclittica Personalizzata

Scarica banda più ampia (±30°, mag < 16):

```bash
./gaia_cache_downloader --ecliptic 30.0 16.0
```

**Parametri:**
- `30.0` = larghezza banda in gradi (±30° = 60° totali)
- `16.0` = magnitudine limite

**Risultato:** ~150 MB, ~500K stelle

### Download Regione Personalizzata

Scarica solo una regione specifica del cielo:

```bash
# Esempio: Regione Virgo-Libra (RA 170-200°, Dec -20 a +20°)
./gaia_cache_downloader --region 170 200 -20 20 15.0
```

**Parametri:**
- `170` = RA minima (gradi)
- `200` = RA massima (gradi)
- `-20` = Dec minima (gradi)
- `20` = Dec massima (gradi)
- `15.0` = magnitudine limite

**Risultato:** ~20 MB, ~40K stelle

## Gestione Cache

### Vedere Statistiche

```bash
./gaia_cache_downloader --stats
```

Mostra:
- Numero tiles e stelle
- Dimensione cache su disco
- Copertura cielo
- Range magnitudine
- Data ultimo aggiornamento

### Pulire Cache (Liberare Spazio)

```bash
./gaia_cache_downloader --clear
```

**ATTENZIONE:** Elimina TUTTA la cache. Conferma richiesta.

Per ricominciare:
```bash
./gaia_cache_downloader --mainbelt 15.0
```

## Confronto Performance

### Con Cache Locale

```
Operazione              Tempo    Network
──────────────────────────────────────────
Ricerca 31 giorni       ~3s      0 MB
Ricerca 365 giorni      ~8s      0 MB
100 ricerche            ~5min    0 MB
```

**Vantaggi:**
✅ Veloce (3-5x più rapido)  
✅ Funziona offline  
✅ Nessun rate limiting  
✅ Risultati deterministici  

### Con Query Online

```
Operazione              Tempo    Network
──────────────────────────────────────────
Ricerca 31 giorni       ~18s     ~2 MB
Ricerca 365 giorni      ~45s     ~15 MB
100 ricerche            ~30min   ~200 MB
```

**Vantaggi:**
✅ Nessun setup iniziale  
✅ Sempre aggiornata (proper motions recentissime)  
✅ Non occupa spazio disco  

## Workflow Consigliato

### Per Uso Occasionale (< 10 ricerche/mese)
```bash
# Usa query online, più semplice
./italoccultcalc config_online.oop
```

### Per Uso Regolare (> 10 ricerche/mese)
```bash
# Setup una volta
./gaia_cache_downloader --mainbelt 15.0

# Usa sempre con cache
./italoccultcalc config_cached.oop
```

### Per Produzioni Batch
```bash
# Download cache completa
./gaia_cache_downloader --ecliptic 30.0 16.0

# Loop su molti target
for asteroid in hygiea bamberga davida; do
    ./italoccultcalc config_${asteroid}.oop
done
# Totale: ~1 min invece di ~20 min
```

### Per Osservatori Remoti (Senza Internet)
```bash
# A casa, con internet:
./gaia_cache_downloader --mainbelt 15.0

# Copia cache su laptop:
rsync -av ~/.ioccultcalc/catalogs/ laptop:~/.ioccultcalc/catalogs/

# In osservatorio, offline:
laptop$ ./italoccultcalc config.oop  # Funziona!
```

## Directory e File

### Struttura Directory Cache

```
~/.ioccultcalc/catalogs/
├── index.json              # Indice tiles (8 KB)
├── tiles/
│   ├── tile_3705.json      # Tile HEALPix 3705 (~170 KB)
│   ├── tile_3706.json
│   ├── ...
│   └── tile_4290.json      # 586 tiles totali
└── metadata.json           # Statistiche (~2 KB)
```

**Totale:** ~80-120 MB per main belt (mag 15)

### File di Configurazione

**preset_large_asteroids_jan2026.oop** (cache attiva):
```
gaia.
    .use_local_cache = .TRUE.
    .cache_directory = '~/.ioccultcalc/catalogs'
    .auto_download = .FALSE.
    .max_magnitude = 15.0
    .healpix_level = 6
```

Per disabilitare cache:
```
gaia.
    .use_local_cache = .FALSE.
```

## Troubleshooting

### Cache non trovata

```
⚠️  Cache non trovata. Usa:
   gaia_cache_downloader --mainbelt 15.0
```

**Soluzione:**
```bash
./gaia_cache_downloader --mainbelt 15.0
```

### Download interrotto

Se il download si interrompe:
```bash
# Riprendi download (scarica solo tiles mancanti)
./gaia_cache_downloader --mainbelt 15.0
```

Il sistema è intelligente: scarica solo le tiles che mancano.

### Cache corrotta

```bash
# Elimina e ricrea
./gaia_cache_downloader --clear
./gaia_cache_downloader --mainbelt 15.0
```

### Spazio disco insufficiente

```bash
# Verifica spazio
df -h ~/.ioccultcalc/

# Opzioni:
# 1. Pulisci altre cache
rm -rf ~/.cache/thumbnails

# 2. Usa magnitudine più bassa (meno stelle)
./gaia_cache_downloader --clear
./gaia_cache_downloader --mainbelt 14.0  # Invece di 15.0

# 3. Scarica solo regione necessaria
./gaia_cache_downloader --region 150 210 -30 30 15.0
```

### Stelle mancanti in cache

Se una regione non è coperta:
```
⚠️  Tile 1234 missing (auto-download disabled)
Fallback a query online...
```

**Soluzione:** Scarica regione più ampia:
```bash
./gaia_cache_downloader --ecliptic 30.0 15.0  # Invece di 25.0
```

## FAQ

**Q: Quante stelle scarica --mainbelt 15.0?**  
A: ~280K stelle, ~100 MB

**Q: Quanto tempo ci vuole?**  
A: ~10-15 minuti (dipende da connessione)

**Q: Posso interrompere il download?**  
A: Sì (Ctrl+C). Riprendilo con lo stesso comando.

**Q: La cache scade?**  
A: No. Resta valida indefinitamente (proper motion applicati automaticamente)

**Q: Devo aggiornare la cache?**  
A: No per occultazioni. Le coordinate sono corrette per decenni.

**Q: Cache vs Online per precisione?**  
A: Identica precisione. Stesso catalogo Gaia DR3.

**Q: Posso usare cache su più computer?**  
A: Sì. Copia ~/.ioccultcalc/catalogs/ su tutti i computer.

**Q: Che succede se cambio magnitudine?**  
A: Se in cache hai mag 15 e cerchi mag 14, funziona (subset). Se cerchi mag 16, serve nuovo download.

---

**Ultima modifica:** 2025-01-24  
**Versione:** IOccultCalc 1.0  
**Tool:** gaia_cache_downloader 1.0
