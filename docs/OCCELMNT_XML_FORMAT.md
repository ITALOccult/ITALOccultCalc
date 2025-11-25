# Formato XML Occelmnt - Documentazione

## Struttura File XML

Basato sull'analisi del codice IOccultCalc e italoccult2, il formato XML Occelmnt per occultazioni asteroidali ha la seguente struttura:

### Root Element
```xml
<?xml version="1.0" encoding="UTF-8"?>
<Occelmnt generator="IOccultCalc" version="1.0">
```

### Metadata Section
```xml
  <Metadata>
    <Source>ITALOccultCalc</Source>
    <Created>2025-11-24 09:32:38</Created>
    <Count>5</Count>
  </Metadata>
```

**Campi:**
- `<Source>`: Nome dell'organizzazione/software che genera il file
- `<Created>`: Data e ora di creazione in formato `YYYY-MM-DD HH:MM:SS`  
- `<Count>`: Numero totale di eventi nel file

### Event Section (ripetuto per ogni occultazione)

```xml
  <Event>
    <EventID></EventID>
    
    <Asteroid>
      <Name>Hygiea</Name>
      <Designation>10</Designation>
      <Number>10</Number>  <!-- Opzionale -->
      <Magnitude>5.43</Magnitude>  <!-- Opzionale -->
    </Asteroid>
    
    <Star>
      <Catalog>Gaia DR3</Catalog>
      <ID>Gaia DR3 3557567320183657472</ID>
      <RA unit="degrees">162.40663106</RA>
      <Dec unit="degrees">-16.19275976</Dec>
      <RAFormatted>10:49:37.591</RAFormatted>
      <DecFormatted>-16:11:33.94</DecFormatted>
      <Magnitude>2.74</Magnitude>
      <Distance unit="parsec">125.5</Distance>  <!-- Opzionale -->
    </Star>
    
    <Time>
      <JulianDate>2461018.44737300</JulianDate>
      <UTC>2025-12-08 22:44:13</UTC>
    </Time>
    
    <Geometry>
      <CloseApproach unit="arcsec">0.0500</CloseApproach>
      <PositionAngle unit="degrees">45.23</PositionAngle>
      <PathWidth unit="km">250.00</PathWidth>
      <MaxDuration unit="seconds">20.35</MaxDuration>
      <Uncertainty unit="km">12.00</Uncertainty>  <!-- Opzionale -->
      <Probability>0.8000</Probability>
      <MagnitudeDrop>1.5</MagnitudeDrop>  <!-- Opzionale -->
    </Geometry>
    
    <CenterLine>
      <Point>
        <Latitude>41.902800</Latitude>
        <Longitude>12.496400</Longitude>
        <JD>2461018.44737300</JD>
        <DateTime>2025-12-08 22:44:13</DateTime>
        <StarAltitude>45.50</StarAltitude>
        <SunAltitude>-15.20</SunAltitude>
      </Point>
      <!-- Altri punti del percorso -->
    </CenterLine>
    
    <!-- Opzionali: limiti di incertezza -->
    <NorthLimit>
      <Point>
        <Latitude>42.102800</Latitude>
        <Longitude>12.496400</Longitude>
      </Point>
    </NorthLimit>
    
    <SouthLimit>
      <Point>
        <Latitude>41.702800</Latitude>
        <Longitude>12.496400</Longitude>
      </Point>
    </SouthLimit>
    
  </Event>
```

### Chiusura Root
```xml
</Occelmnt>
```

## Dettagli Campi Obbligatori vs Opzionali

### Obbligatori
- `<Occelmnt>` - Root element
- `<Metadata>/<Source>` - Identificazione sorgente
- `<Metadata>/<Created>` - Timestamp creazione
- `<Metadata>/<Count>` - Numero eventi
- `<Event>` - Contenitore evento
- `<Asteroid>/<Name>` O `<Asteroid>/<Designation>` - Identificazione asteroide
- `<Star>/<Catalog>` - Catalogo stella
- `<Star>/<ID>` - Identificativo stella
- `<Star>/<RA>` - Ascensione retta (gradi)
- `<Star>/<Dec>` - Declinazione (gradi)
- `<Star>/<Magnitude>` - Magnitudine stella
- `<Time>/<JulianDate>` - Data giuliana evento
- `<Time>/<UTC>` - Data/ora UTC
- `<Geometry>/<CloseApproach>` - Distanza minima (arcsec)
- `<Geometry>/<MaxDuration>` - Durata massima (sec)
- `<CenterLine>/<Point>` - Almeno un punto del percorso

### Opzionali
- `<EventID>` - Identificativo univoco evento
- `<Asteroid>/<Number>` - Numero MPC
- `<Asteroid>/<Magnitude>` - Magnitudine asteroide
- `<Star>/<RAFormatted>` - RA formattata (HH:MM:SS.sss)
- `<Star>/<DecFormatted>` - Dec formattata (±DD:MM:SS.ss)
- `<Star>/<Distance>` - Distanza stella (parsec)
- `<Geometry>/<PositionAngle>` - Angolo di posizione
- `<Geometry>/<PathWidth>` - Larghezza percorso
- `<Geometry>/<Uncertainty>` - Incertezza predizione
- `<Geometry>/<Probability>` - Probabilità occultazione
- `<Geometry>/<MagnitudeDrop>` - Calo magnitudine
- `<CenterLine>/<Point>/<JD>` - JD del punto
- `<CenterLine>/<Point>/<DateTime>` - Data/ora del punto
- `<CenterLine>/<Point>/<StarAltitude>` - Altezza stella
- `<CenterLine>/<Point>/<SunAltitude>` - Altezza sole
- `<NorthLimit>` - Limite nord incertezza
- `<SouthLimit>` - Limite sud incertezza

## Note Implementazione

### Coordinate Geografiche
Le coordinate devono essere in **gradi decimali**:
- Latitudine: -90° to +90° (positivo = Nord)
- Longitudine: -180° to +180° (positivo = Est)

### Date e Orari
- **Created**: Formato `YYYY-MM-DD HH:MM:SS`
- **UTC**: Formato `YYYY-MM-DD HH:MM:SS`
- **JulianDate**: Formato decimale con 8 cifre decimali

### Unità di Misura
Sempre specificare l'attributo `unit`:
- `<CloseApproach unit="arcsec">` - secondi d'arco
- `<RA unit="degrees">` - gradi decimali
- `<Dec unit="degrees">` - gradi decimali
- `<PathWidth unit="km">` - chilometri
- `<MaxDuration unit="seconds">` - secondi
- `<Uncertainty unit="km">` - chilometri
- `<Distance unit="parsec">` - parsec

### Escape XML
Caratteri speciali devono essere escaped:
- `&` → `&amp;`
- `<` → `&lt;`
- `>` → `&gt;`
- `"` → `&quot;`
- `'` → `&apos;`

## File Generato da IOccultCalc

Il file `test_output_occult4.xml` generato da IOccultCalc segue questo formato e include:

✓ Root element `<Occelmnt>`
✓ Metadata con timestamp corretto (2025-11-24)
✓ Coordinate geografiche corrette (41.9028°, 12.4964°)
✓ Dati stella da Gaia DR3
✓ Geometria evento (durata, distanza, incertezza)
✓ Punti percorso centrale con lat/lon

## Compatibilità

Questo formato è compatibile con:
- **Occult4** di Dave Herald
- **OccultWatcher Cloud** (OWCloud)
- **IOTA** standard XML format

## Riferimenti

- Repository: https://github.com/manvalan/IOccultCalc
- Codice sorgente: `src/occult4_xml.cpp`
- Repository italoccult2: https://github.com/manvalan/italoccult2
