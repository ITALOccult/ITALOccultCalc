/**
 * @file asteroid_filter.h
 * @brief Sistema di filtri per selezione asteroidi con range e condizioni
 * 
 * Sintassi ispirata a SQL ma semplificata per configurazioni:
 * 
 * range:
 *   from: 1
 *   to: 100000
 *   where:
 *     - "diameter > 5"
 *     - "H < 14"
 *     - "class in ['MBA', 'NEA']"
 *   wherenot:
 *     - "e > 0.9"  # Escludi orbite troppo eccentriche
 */

#ifndef IOCCULTCALC_ASTEROID_FILTER_H
#define IOCCULTCALC_ASTEROID_FILTER_H

#include <string>
#include <vector>
#include <memory>
#include <functional>
#include <nlohmann/json.hpp>

namespace ioccultcalc {

// Forward declaration
struct AsteroidProperties;

/**
 * @brief Tipo di operatore di confronto
 */
enum class ComparisonOp {
    EQUAL,              // ==, =
    NOT_EQUAL,          // !=, <>
    LESS_THAN,          // <
    LESS_EQUAL,         // <=
    GREATER_THAN,       // >
    GREATER_EQUAL,      // >=
    IN,                 // in [...]
    NOT_IN,             // not in [...]
    LIKE,               // like "pattern"
    BETWEEN             // between x and y
};

/**
 * @brief Proprietà di un asteroide per il filtraggio
 */
struct AsteroidProperties {
    int number;                      // Numero asteroide
    std::string designation;         // Designazione (es: "2024 AA")
    std::string name;                // Nome (es: "433 Eros")
    double diameter;                 // Diametro (km)
    double H;                        // Magnitudine assoluta
    double albedo;                   // Albedo geometrico
    double a;                        // Semiasse maggiore (AU)
    double e;                        // Eccentricità
    double i;                        // Inclinazione (gradi)
    double rotation_period;          // Periodo di rotazione (ore)
    std::string orbit_class;         // Classe orbitale (MBA, NEA, TNO, etc.)
    std::string spectral_type;       // Tipo spettrale (C, S, M, etc.)
    bool has_diameter;               // Diametro noto
    bool has_albedo;                 // Albedo noto
    
    // Costruttore default
    AsteroidProperties() : number(0), diameter(0), H(99), albedo(0.15),
                          a(0), e(0), i(0), rotation_period(0), 
                          has_diameter(false), has_albedo(false) {}
};

/**
 * @brief Singola condizione di filtro
 */
class FilterCondition {
public:
    FilterCondition(const std::string& field, ComparisonOp op, const std::string& value);
    
    // Valuta la condizione su un asteroide
    bool evaluate(const AsteroidProperties& props) const;
    
    // Serializzazione
    std::string toString() const;
    
    // Parsing
    static FilterCondition parse(const std::string& condition);
    
private:
    std::string field_;
    ComparisonOp operator_;
    std::string value_;
    
    bool evaluateNumeric(double fieldValue) const;
    bool evaluateString(const std::string& fieldValue) const;
};

/**
 * @brief Range di asteroidi con filtri WHERE e WHERENOT
 */
class AsteroidRange {
public:
    AsteroidRange();
    AsteroidRange(int from, int to);
    
    // Configurazione range
    void setRange(int from, int to);
    void setExplicitList(const std::vector<int>& asteroids);
    
    // Aggiungi condizioni WHERE (inclusione)
    void addWhereCondition(const std::string& condition);
    void addWhereCondition(const FilterCondition& condition);
    
    // Aggiungi condizioni WHERENOT (esclusione)
    void addWhereNotCondition(const std::string& condition);
    void addWhereNotCondition(const FilterCondition& condition);
    
    // Valutazione
    bool matches(const AsteroidProperties& props) const;
    std::vector<int> getAsteroidList() const;
    
    // Rappresentazione
    std::string toString() const;
    
    // Accessors
    int getFrom() const { return from_; }
    int getTo() const { return to_; }
    bool isExplicitList() const { return useExplicitList_; }
    const std::vector<int>& getExplicitList() const { return explicitList_; }
    const std::vector<FilterCondition>& getWhereConditions() const { return whereConditions_; }
    const std::vector<FilterCondition>& getWhereNotConditions() const { return whereNotConditions_; }
    
private:
    int from_;
    int to_;
    bool useExplicitList_;
    std::vector<int> explicitList_;
    std::vector<FilterCondition> whereConditions_;
    std::vector<FilterCondition> whereNotConditions_;
};

/**
 * @brief Builder per configurazione range con sintassi fluente
 */
class AsteroidRangeBuilder {
public:
    AsteroidRangeBuilder();
    
    // Range continuo
    AsteroidRangeBuilder& from(int start);
    AsteroidRangeBuilder& to(int end);
    
    // Lista esplicita
    AsteroidRangeBuilder& explicitList(const std::vector<int>& list);
    
    // Aggiungi lista da file (fluent interface)
    AsteroidRangeBuilder& addListFromFile(const std::string& filename);
    
    // Metodi helper per aggiungere liste
    void addToList(const std::vector<int>& additional);
    void addToListFromFile(const std::string& filename);
    
    // Condizioni WHERE (inclusione)
    AsteroidRangeBuilder& where(const std::string& condition);
    
    // Condizioni WHERENOT (esclusione)
    AsteroidRangeBuilder& whereNot(const std::string& condition);
    
    // Build
    AsteroidRange build();

private:
    AsteroidRange range_;
    int from_ = 1;
    int to_ = 100000;
    bool useExplicitList_ = false;
    std::vector<int> explicitList_;
    std::vector<std::string> whereConditions_;
    std::vector<std::string> whereNotConditions_;
};/**
 * @brief Preset comuni di filtri
 */
class AsteroidFilterPresets {
public:
    // Main Belt Asteroids > 50 km
    static AsteroidRange largeMBA();
    
    // Near-Earth Asteroids
    static AsteroidRange nearEarth();
    
    // Potentially Hazardous Asteroids
    static AsteroidRange potentiallyHazardous();
    
    // Jupiter Trojans
    static AsteroidRange trojans();
    
    // Centaurs
    static AsteroidRange centaurs();
    
    // Dark asteroids (low albedo)
    static AsteroidRange darkAsteroids();
    
    // Metallic asteroids
    static AsteroidRange metallic();
    
    // First 100 historic asteroids
    static AsteroidRange historic();
    
    // Fast rotators
    static AsteroidRange fastRotators();
    
    // Binary system candidates
    static AsteroidRange binaryCandidates();
};

} // namespace ioccultcalc

#endif // IOCCULTCALC_ASTEROID_FILTER_H
