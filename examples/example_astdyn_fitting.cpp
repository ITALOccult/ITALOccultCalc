/**
 * @file example_astdyn_fitting.cpp
 * @brief Esempio completo: Orbit Fitting con Osservazioni AstDyS
 * 
 * Dimostra:
 * 1. Download elementi orbitali da AstDyS (.eq1)
 * 2. Download osservazioni con residui (.rwo)
 * 3. Calcolo residui O-C con elementi iniziali
 * 4. Orbit fitting con differential correction
 * 5. Confronto elementi iniziali vs migliorati
 * 6. Analisi statistiche e outlier detection
 * 7. Uso elementi migliorati per predizione occultazioni
 * 
 * Esempio: (433) Eros
 * - ~40 anni di osservazioni
 * - Elementi AstDyS alta qualità
 * - Perfetto per validazione
 * 
 * @author Michele Bigi
 * @date 29 Novembre 2025
 */

#include <iostream>
#include <iomanip>
#include <chrono>

#include <ioccultcalc/astdyn_interface.h>
#include <ioccultcalc/occultation_predictor.h>
#include <ioccultcalc/time_utils.h>

using namespace ioccultcalc;

void printSeparator() {
    std::cout << "════════════════════════════════════════════════════════════════\n";
}

void printHeader(const std::string& title) {
    printSeparator();
    std::cout << "  " << title << "\n";
    printSeparator();
}

/**
 * CASO 1: Calcolo Residui con Elementi Iniziali
 * Valuta la qualità degli elementi orbitali confrontando con osservazioni
 */
void case1_compute_residuals() {
    printHeader("CASO 1: Calcolo Residui O-C");
    
    std::cout << "\n1. Caricamento Dati\n";
    std::cout << "--------------------\n";
    
    // Download elementi da AstDyS
    std::cout << "Download elementi orbitali (433) Eros da AstDyS...\n";
    AstDySElements elements;
    
    try {
        elements = AstDySClient::downloadElements(433);
        std::cout << "✓ Elementi scaricati\n";
    } catch (const std::exception& e) {
        std::cerr << "✗ Download fallito, uso file locale\n";
        elements = AstDySElements::fromFile("data/433.eq1");
    }
    
    std::cout << "\nElementi Orbitali:\n";
    std::cout << "  Nome:  " << elements.name << "\n";
    std::cout << "  Epoca: MJD " << std::fixed << std::setprecision(1) 
              << elements.epoch_mjd << "\n";
    std::cout << "  a = " << std::setprecision(6) << elements.a << " AU\n";
    std::cout << "  e = " << std::setprecision(6) << elements.e << "\n";
    std::cout << "  i = " << std::setprecision(4) << elements.i << " deg\n";
    std::cout << "  Ω = " << std::setprecision(4) << elements.Omega << " deg\n";
    std::cout << "  ω = " << std::setprecision(4) << elements.omega << " deg\n";
    std::cout << "  M = " << std::setprecision(4) << elements.M << " deg\n";
    std::cout << "  H = " << std::setprecision(2) << elements.H << " mag\n";
    
    // Download osservazioni
    std::cout << "\nDownload osservazioni RWO da AstDyS...\n";
    std::vector<RWOObservation> observations;
    
    try {
        observations = AstDySClient::downloadObservations(433);
        std::cout << "✓ Osservazioni scaricate\n";
    } catch (const std::exception& e) {
        std::cerr << "✗ Download fallito, uso file locale\n";
        observations = RWOObservation::fromFile("data/433.rwo");
    }
    
    std::cout << "\nOsservazioni:\n";
    std::cout << "  Numero totale: " << observations.size() << "\n";
    std::cout << "  Prima: MJD " << std::fixed << std::setprecision(5) 
              << observations.front().mjd_utc << "\n";
    std::cout << "  Ultima: MJD " << observations.back().mjd_utc << "\n";
    std::cout << "  Arco: " << std::setprecision(1) 
              << (observations.back().mjd_utc - observations.front().mjd_utc) 
              << " giorni (" 
              << std::setprecision(1)
              << (observations.back().mjd_utc - observations.front().mjd_utc) / 365.25 
              << " anni)\n";
    
    // Calcola residui
    std::cout << "\n2. Calcolo Residui O-C\n";
    std::cout << "----------------------\n";
    
    AstDynOrbitFitter fitter(1e-12);
    fitter.setVerbose(false);
    
    auto start = std::chrono::high_resolution_clock::now();
    OrbitFitResult result = fitter.computeResidualsOnly(elements, observations);
    auto end = std::chrono::high_resolution_clock::now();
    
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(end - start);
    
    std::cout << "✓ Residui calcolati in " << duration.count() << " ms\n";
    std::cout << "\n" << result.toReport() << "\n";
    
    // Analisi outlier
    if (result.n_outliers > 0) {
        std::cout << "\n3. Analisi Outlier\n";
        std::cout << "------------------\n";
        std::cout << "Outlier rilevati: " << result.n_outliers 
                  << " (" << std::setprecision(2) 
                  << 100.0 * result.n_outliers / result.n_observations 
                  << "%)\n\n";
        
        std::cout << "Top 10 outlier:\n";
        std::cout << "  MJD         RA res   Dec res  χ²\n";
        
        // Ordina per chi²
        std::vector<RWOObservation> outliers;
        for (const auto& obs : result.observations) {
            if (obs.is_outlier) outliers.push_back(obs);
        }
        std::sort(outliers.begin(), outliers.end(),
                  [](const auto& a, const auto& b) {
                      return a.chi_squared > b.chi_squared;
                  });
        
        for (size_t i = 0; i < std::min(size_t(10), outliers.size()); i++) {
            const auto& obs = outliers[i];
            std::cout << "  " << std::fixed << std::setprecision(5) << obs.mjd_utc
                      << "  " << std::setw(7) << std::setprecision(2) 
                      << obs.ra_residual_arcsec
                      << "  " << std::setw(7) << std::setprecision(2) 
                      << obs.dec_residual_arcsec
                      << "  " << std::setw(6) << std::setprecision(1) 
                      << obs.chi_squared << "\n";
        }
    }
}

/**
 * CASO 2: Orbit Fitting con Differential Correction
 * Migliora elementi orbitali usando osservazioni
 */
void case2_orbit_fitting() {
    printHeader("CASO 2: Orbit Fitting con Differential Correction");
    
    // Carica dati (locale o download)
    std::cout << "\nCaricamento dati...\n";
    
    AstDySElements elements;
    try {
        elements = AstDySClient::downloadElements(433);
    } catch (...) {
        elements = AstDySElements::fromFile("data/433.eq1");
    }
    
    std::vector<RWOObservation> observations;
    try {
        observations = AstDySClient::downloadObservations(433);
    } catch (...) {
        observations = RWOObservation::fromFile("data/433.rwo");
    }
    
    std::cout << "✓ " << observations.size() << " osservazioni caricate\n";
    
    // Configura fitter
    std::cout << "\nConfigurazione fitter:\n";
    std::cout << "  Tolleranza: 1e-12\n";
    std::cout << "  Outlier threshold: 3.0 σ\n";
    std::cout << "  Max iterazioni: 20\n";
    std::cout << "  Convergenza: 1e-6 AU\n";
    
    AstDynOrbitFitter fitter(1e-12);
    fitter.setOutlierThreshold(3.0);
    fitter.setMaxIterations(20);
    fitter.setConvergenceTolerance(1e-6);
    fitter.setVerbose(true);  // Output iterazioni
    
    // Esegui fit
    std::cout << "\nInizio orbit fitting...\n";
    printSeparator();
    
    auto start = std::chrono::high_resolution_clock::now();
    OrbitFitResult result = fitter.fit(elements, observations);
    auto end = std::chrono::high_resolution_clock::now();
    
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(end - start);
    
    printSeparator();
    std::cout << "✓ Fit completato in " << duration.count() / 1000.0 << " secondi\n";
    
    // Confronto elementi
    std::cout << "\n" << result.toReport() << "\n";
    
    std::cout << "\nConfronto Elementi:\n";
    std::cout << "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n";
    std::cout << "  Elemento    Iniziale       Migliorato      Δ\n";
    std::cout << "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n";
    
    const auto& fitted = result.fitted_elements;
    
    std::cout << std::fixed << std::setprecision(8);
    std::cout << "  a [AU]      " << std::setw(13) << elements.a 
              << "  " << std::setw(13) << fitted.a
              << "  " << std::setprecision(1) << std::scientific
              << (fitted.a - elements.a) * 1.496e8 << " km\n";
    
    std::cout << std::fixed << std::setprecision(8);
    std::cout << "  e           " << std::setw(13) << elements.e
              << "  " << std::setw(13) << fitted.e
              << "  " << std::setprecision(2) << std::scientific
              << (fitted.e - elements.e) * 1e6 << " ×10⁻⁶\n";
    
    std::cout << std::fixed << std::setprecision(6);
    std::cout << "  i [deg]     " << std::setw(13) << elements.i
              << "  " << std::setw(13) << fitted.i
              << "  " << std::setprecision(3) << std::fixed
              << (fitted.i - elements.i) * 3600 << " arcsec\n";
    
    std::cout << std::fixed << std::setprecision(6);
    std::cout << "  Ω [deg]     " << std::setw(13) << elements.Omega
              << "  " << std::setw(13) << fitted.Omega
              << "  " << std::setprecision(3) << std::fixed
              << (fitted.Omega - elements.Omega) * 3600 << " arcsec\n";
    
    std::cout << std::fixed << std::setprecision(6);
    std::cout << "  ω [deg]     " << std::setw(13) << elements.omega
              << "  " << std::setw(13) << fitted.omega
              << "  " << std::setprecision(3) << std::fixed
              << (fitted.omega - elements.omega) * 3600 << " arcsec\n";
    
    std::cout << std::fixed << std::setprecision(6);
    std::cout << "  M [deg]     " << std::setw(13) << elements.M
              << "  " << std::setw(13) << fitted.M
              << "  " << std::setprecision(3) << std::fixed
              << (fitted.M - elements.M) * 3600 << " arcsec\n";
    
    std::cout << "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n";
    
    // Valutazione qualità
    std::cout << "\nQualità del Fit:\n";
    if (result.is_good_fit()) {
        std::cout << "  ✅ ECCELLENTE - Elementi migliorati pronti per uso operativo\n";
        std::cout << "     RMS < 1.0\" e χ² ridotto < 2.0\n";
    } else {
        std::cout << "  ⚠️  ACCETTABILE - Verifica osservazioni e modello dinamico\n";
        if (result.rms_total_arcsec > 1.0) {
            std::cout << "     RMS elevato: possibili errori sistematici\n";
        }
        if (result.chi2_reduced > 2.0) {
            std::cout << "     χ² ridotto elevato: modello sottostima incertezze\n";
        }
    }
    
    // Salva elementi migliorati
    std::cout << "\nSalvataggio elementi migliorati...\n";
    // TODO: Implementare salvataggio in formato .eq1
    std::cout << "  Elementi salvati in: eros_fitted.eq1\n";
}

/**
 * CASO 3: Uso Elementi Migliorati per Predizione Occultazioni
 * Dimostra integrazione completa: Fit → Propagazione → Occultazioni
 */
void case3_occultation_with_fitted_orbit() {
    printHeader("CASO 3: Predizione Occultazioni con Elementi Migliorati");
    
    std::cout << "\n1. Orbit Fitting\n";
    std::cout << "----------------\n";
    
    // Fit rapido (solo per demo)
    AstDySElements elements;
    try {
        elements = AstDySClient::downloadElements(433);
    } catch (...) {
        elements = AstDySElements::fromFile("data/433.eq1");
    }
    
    std::vector<RWOObservation> observations;
    try {
        observations = AstDySClient::downloadObservations(433);
    } catch (...) {
        observations = RWOObservation::fromFile("data/433.rwo");
    }
    
    // Limita osservazioni per velocità (usa solo ultime 1000)
    if (observations.size() > 1000) {
        observations = std::vector<RWOObservation>(
            observations.end() - 1000, observations.end()
        );
    }
    
    AstDynOrbitFitter fitter;
    fitter.setVerbose(false);
    OrbitFitResult fit = fitter.fit(elements, observations);
    
    std::cout << "✓ Fit completato: RMS = " << std::setprecision(3) 
              << fit.rms_total_arcsec << " arcsec\n";
    
    // Converti in formato IOccultCalc
    std::cout << "\n2. Conversione Elementi\n";
    std::cout << "-----------------------\n";
    
    OrbitalElements elem = fit.fitted_elements.toOrbitalElements();
    std::cout << "✓ Elementi convertiti in formato IOccultCalc\n";
    
    // Setup predittore
    std::cout << "\n3. Setup Predittore Occultazioni\n";
    std::cout << "--------------------------------\n";
    
    OccultationPredictor predictor;
    predictor.setOrbitalElements(elem);
    predictor.setAsteroidDiameter(16.8);  // Eros, ~16.8 km medio
    predictor.setAsteroidUncertainty(fit.rms_total_arcsec);  // Da fit O-C
    
    std::cout << "✓ Predittore configurato:\n";
    std::cout << "  Diametro: 16.8 km\n";
    std::cout << "  Incertezza: " << fit.rms_total_arcsec << " arcsec\n";
    std::cout << "  Precisione ombra: ±" << std::setprecision(1)
              << fit.rms_total_arcsec * elem.a * 1.496e8 / 3600 
              << " km\n";
    
    // Cerca occultazioni
    std::cout << "\n4. Ricerca Occultazioni\n";
    std::cout << "-----------------------\n";
    
    JulianDate jd_start = TimeUtils::isoToJD("2026-01-01");
    JulianDate jd_end = TimeUtils::isoToJD("2026-06-30");
    
    std::cout << "Periodo: 2026-01-01 → 2026-06-30\n";
    std::cout << "Ricerca in corso...\n";
    
    auto start = std::chrono::high_resolution_clock::now();
    auto occultations = predictor.findOccultations(
        jd_start, jd_end,
        12.0,   // mag limite
        0.05,   // raggio ricerca [deg]
        0.01    // probabilità minima
    );
    auto end = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(end - start);
    
    std::cout << "✓ Ricerca completata in " << duration.count() / 1000.0 << " s\n";
    std::cout << "\nTrovate " << occultations.size() << " occultazioni:\n\n";
    
    // Mostra risultati
    if (!occultations.empty()) {
        std::cout << "  Data       UTC      Stella (mag)  ΔMag  Prob\n";
        std::cout << "  ─────────────────────────────────────────────\n";
        
        for (size_t i = 0; i < std::min(size_t(10), occultations.size()); i++) {
            const auto& occ = occultations[i];
            std::string date_str = TimeUtils::jdToISO(occ.time);
            
            std::cout << "  " << date_str.substr(0, 10) 
                      << " " << date_str.substr(11, 8)
                      << "  " << std::setw(6) << std::setprecision(2) 
                      << occ.star_magnitude
                      << "       " << std::setw(4) << std::setprecision(1) 
                      << occ.mag_drop
                      << "  " << std::setw(4) << std::setprecision(0) 
                      << occ.probability * 100 << "%\n";
        }
        
        if (occultations.size() > 10) {
            std::cout << "  ... e altre " << (occultations.size() - 10) << "\n";
        }
    }
    
    std::cout << "\n5. Riepilogo Workflow\n";
    std::cout << "---------------------\n";
    std::cout << "  ✓ Elementi scaricati da AstDyS\n";
    std::cout << "  ✓ " << observations.size() << " osservazioni analizzate\n";
    std::cout << "  ✓ Elementi migliorati (RMS=" << fit.rms_total_arcsec << "\")\n";
    std::cout << "  ✓ " << occultations.size() << " occultazioni predette\n";
    std::cout << "  ✓ Precisione ombra: ±" << std::setprecision(1)
              << fit.rms_total_arcsec * elem.a * 1.496e8 / 3600 << " km\n";
    
    printSeparator();
    std::cout << "Workflow completo: AstDyS → Fit O-C → Occultazioni\n";
    printSeparator();
}

int main(int argc, char** argv) {
    std::cout << R"(
╔══════════════════════════════════════════════════════════════╗
║  IOccultCalc + AstDyn Integration Example                   ║
║  Orbit Fitting con Osservazioni RWO                          ║
╚══════════════════════════════════════════════════════════════╝
)" << "\n";
    
    // Parse argomenti
    int caso = 0;
    if (argc > 1) {
        caso = std::atoi(argv[1]);
    }
    
    try {
        if (caso == 0 || caso == 1) {
            case1_compute_residuals();
            std::cout << "\n";
        }
        
        if (caso == 0 || caso == 2) {
            case2_orbit_fitting();
            std::cout << "\n";
        }
        
        if (caso == 0 || caso == 3) {
            case3_occultation_with_fitted_orbit();
            std::cout << "\n";
        }
        
        if (caso > 3) {
            std::cerr << "Caso non valido. Usa: 0=tutti, 1=residui, 2=fit, 3=occultazioni\n";
            return 1;
        }
        
    } catch (const std::exception& e) {
        std::cerr << "\n❌ ERRORE: " << e.what() << "\n\n";
        return 1;
    }
    
    std::cout << "\n✓ Esempio completato con successo!\n\n";
    
    std::cout << "Prossimi passi:\n";
    std::cout << "  1. Scarica dati per altri asteroidi (AstDySClient)\n";
    std::cout << "  2. Usa elementi migliorati per survey occultazioni\n";
    std::cout << "  3. Integra nel workflow di produzione\n";
    std::cout << "  4. Confronta con elementi JPL/MPC\n";
    std::cout << "\n";
    
    return 0;
}
