# Algoritmo RA15 di Everhart

## Fonte
OrbFit 5.0.8 (Andrea Milani et al., Università di Pisa)  
File: `src/propag/ra15_mod.f90`

## Descrizione
**RA15** = **Radau spacing, 15° ordine**  
Integratore di Everhart specificamente progettato per problemi orbitali a lungo termine.

## Caratteristiche Chiave

### 1. Ordine Alto
- **Ordine 15**: molto superiore a RK4 (ordine 4)
- Errore locale: O(h^16)
- Errore globale: O(h^15)

### 2. Metodo Implicito
- **Punti di Gauss-Radau**: 8 punti interni con spaziatura non uniforme
- Richiede iterazioni per convergenza (3-20 iterazioni)
- Stabilità superiore per orbite planetarie

### 3. Step Size Adattivo
- Controllo automatico del passo basato su tolleranza
- Formula: `ss = 10^(-ll)` dove `ll` tipicamente 6-12
- Riduzione automatica se convergenza fallisce

### 4. Extrapolazione
- I coefficienti B vengono estrapolati da uno step al successivo
- Accelera la convergenza delle iterazioni

## Parametri di Controllo

### Parametri Pubblici (da file .opt in OrbFit)
```fortran
INTEGER llev           ! Sequence level: ss = 10^(-llev)
                       ! Tipico: 10 (ss=1e-10), range 6-12
                       
DOUBLE PRECISION hev   ! Initial stepsize (giorni)
                       ! Tipico: 0.01-0.1 giorni per asteroidi NEA
                       
DOUBLE PRECISION eprk_r ! Convergence tolerance
                        ! Tipico: 1e-12 (massima precisione)
                        
INTEGER lit1_r         ! Max iterations, first step
                       ! Tipico: 10
                       
INTEGER lit2_r         ! Max iterations, subsequent steps
                       ! Tipico: 4 (meno iterazioni con extrapolation)
```

### Valori Raccomandati per IOccultCalc
Per ottenere precisione < 10 km in 40 giorni:
- **llev = 10** (ss = 1e-10)
- **hev = 0.1** giorni (2.4 ore, come RK4 attuale)
- **eprk_r = 1e-12** (tolleranza massima)
- **lit1_r = 10** (primo step)
- **lit2_r = 4** (step successivi con extrapolation)

## Struttura dell'Algoritmo

```fortran
SUBROUTINE ra15(x, v, tini, tfin, tcur, nv, nclass, idcend)
  ! x, v: posizione e velocità (stato iniziale/finale)
  ! tini, tfin: tempi iniziale e finale
  ! nv: numero variabili (3 per x, 3 per v = 6 totale)
  ! nclass = -2: equazioni del secondo ordine y"=f(y,t)
  
  ! Setup costanti Gauss-Radau
  CALL radcon(ncl)  ! Calcola h(8), w(7), u(7), c(21), d(21), r(21)
  
  ! Loop principale sugli step
  DO WHILE (tm < tfin)
    ! Passo precedente: b(7,nv), g(7,nv), e(7,nv)
    
    ! Iterazioni per convergenza (lit1_r o lit2_r)
    DO m = 1, ni
      ! Calcola accelerazioni ai punti Gauss-Radau
      CALL rasust(m, t, t2, tm, tini, x, v, b, f1, nv, ...)
      
      ! Controllo convergenza: ep(m)/ep(1) < eprk_r
      IF (converged) EXIT
    ENDDO
    
    ! Se non converge: riduce stepsize e riprova
    IF (.NOT. converged .AND. .NOT. fixed_step) THEN
      tp = 0.8 * tp
      GOTO restart_step
    ENDIF
    
    ! Predizione stato finale step
    CALL rapred(ncl, x, v, t, t2, f1, b, nv)
    
    ! Aggiorna tempo
    tm = tm + t
    
    ! Calcola nuovo stepsize (se adattivo)
    IF (.NOT. fixed_step) THEN
      hv = max(abs(b(7,:))) * w(7) / t^7
      tp = (ss / hv)^(1/9) * dir
      ! Limita crescita/decrescita
      IF (tp/t > 1.4) tp = t * 1.4
    ENDIF
    
    ! Extrapolazione coefficienti B per prossimo step
    q = tp / t
    CALL bintrp(q, b, e, bd, nv, ns)
  ENDDO
END SUBROUTINE
```

## Confronto con RK4

| Caratteristica | RK4 | RA15 |
|----------------|-----|------|
| Ordine | 4 | 15 |
| Errore locale | O(h^5) | O(h^16) |
| Errore globale | O(h^4) | O(h^15) |
| Step size | Fisso | Adattivo |
| Valutazioni forza/step | 4 | 8 × (3-10 iterazioni) = 24-80 |
| Tipo | Esplicito | Implicito |
| Stabilità | Media | Alta |
| Extrapolazione | No | Sì (coefficienti B) |
| **Velocità** | Alta | Media (3-10× più lento) |
| **Accuratezza 40 giorni** | 457 km | < 1 km (atteso) |

## Vantaggi per Orbite Asteroidali

1. **Ordine alto**: Errore cresce ~1000× più lentamente di RK4
2. **Stabilità**: Metodo implicito previene divergenze su lunghi periodi
3. **Adattivo**: Riduce automaticamente step nelle zone critiche (close approaches)
4. **Extrapolazione**: Riusa informazioni da step precedenti

## Svantaggi

1. **Complessità**: Implementazione molto più complessa di RK4
2. **Costo computazionale**: 10-20× più lento di RK4 per step
3. **Overhead iniziale**: Setup costanti Gauss-Radau
4. **Convergenza**: Può fallire se forza troppo irregolare

## Stima Prestazioni

### RA15 con IOccultCalc
- **Step iniziale**: 0.1 giorni = 2.4 ore
- **Iterazioni/step**: ~4 (con extrapolation)
- **Valutazioni forza/step**: 8 × 4 = 32
- **Confronto RK4**: 32 vs 4 = **8× più lento per step**
- **Step ridotti**: Precisione superiore permette step più grandi
- **Netto**: **2-3× più lento di RK4** per stessa integrazione

### Accuratezza Attesa
Con llev=10, eprk_r=1e-12:
- 1 giorno: < 1 m (vs 314 km RK4)
- 10 giorni: < 100 m (vs 30,829 km RK4)
- 40 giorni: **< 1 km** (vs 457,349 km RK4)
- **Miglioramento: 400-500×**

## Riferimenti

1. Everhart, E. (1985). "An efficient integrator that uses Gauss-Radau spacings".  
   In: IAU Colloq. 83: Dynamics of Comets: Their Origin and Evolution, pp. 185-202.

2. Milani, A., Gronchi, G.F. (2010). "Theory of Orbit Determination".  
   Cambridge University Press, Chapter 7.

3. OrbFit 5.0.8 source code:  
   https://adams.dm.unipi.it/orbfit/OrbFit5.0.8.tar.gz

## Note Implementazione C++

L'implementazione richiede:
1. **Classe RA15Integrator** con metodi:
   - `computeRadauConstants()`: calcola h, w, u, c, d, r
   - `iterate()`: loop iterazioni convergenza
   - `extrapolate()`: estrapolazione coefficienti B
   - `adaptiveStepSize()`: controllo adattivo passo

2. **Storage arrays**: b(7,6), g(7,6), e(7,6), bd(7,6)

3. **Convergenza**: monitorare `ep(m)/ep(1) < eprk_r`

4. **Interfaccia**: compatibile con `OrbitPropagator` esistente
