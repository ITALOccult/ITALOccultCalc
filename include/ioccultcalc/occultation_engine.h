#ifndef IOCCULTCALC_OCCULTATION_ENGINE_H
#define IOCCULTCALC_OCCULTATION_ENGINE_H

#include "ioccultcalc/orbital_elements.h"
#include "phase1_candidate_screening.h"
#include "phase2_occultation_geometry.h"
#include <memory>
#include <string>
#include <vector>

namespace ioccultcalc {

/**
 * @class OccultationEngine
 * @brief Classe di alto livello per gestire la pipeline di ricerca occultazioni.
 * 
 * Questa classe funge da punto di ingresso principale per i calcoli di occultazione,
 * gestendo le conversioni degli elementi orbitali e coordinando la Fase 1 e la Fase 2.
 */
class OccultationEngine {
public:
    OccultationEngine();
    ~OccultationEngine();

    /**
     * @brief Imposta gli elementi orbitali dell'asteroide.
     * Gestisce le conversioni interne per preparare i propagatori.
     */
    bool setAsteroidElements(const AstDynEquinoctialElements& elements);
    
    /**
     * @brief Carica l'asteroide da un file .eq1 (Mean Ecliptic).
     */
    bool loadAsteroidFromEQ1(int number, const std::string& eq1_path);
    bool loadAsteroidFromEQ1(const std::string& eq1_path);
    
    /**
     * @brief Carica l'asteroide dal database JSON locale.
     */
    bool loadAsteroidFromJSON(int number, const std::string& json_path = "");

    /**
     * @brief Carica l'asteroide dal nuovo database SQLite locale asteroids.db.
     */
    bool loadAsteroidFromDB(int number);

    /**
     * @brief Esegue la Fase 1: Screening stelle candidate.
     */
    Phase1Results runPhase1(const Phase1Config& config);
    
    /**
     * @brief Esegue la Fase 2: Geometria precisa e calcolo ombra.
     */
    std::vector<Phase2OccultationEvent> runPhase2(const Phase2Config& config, const std::vector<CandidateStar>& candidates);

    // Accesso ai componenti interni
    Phase1CandidateScreening& getPhase1() { return phase1_; }
    Phase2OccultationGeometry& getPhase2() { return phase2_; }
    const AstDynEquinoctialElements& getCurrentElements() const { return current_elements_; }

private:
    Phase1CandidateScreening phase1_;
    Phase2OccultationGeometry phase2_;
    AstDynEquinoctialElements current_elements_;
    bool elements_loaded_ = false;

    void syncElements();
};

} // namespace ioccultcalc

#endif // IOCCULTCALC_OCCULTATION_ENGINE_H
