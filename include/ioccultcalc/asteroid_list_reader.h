/**
 * @file asteroid_list_reader.h
 * @brief Utility per leggere liste di asteroidi da file in vari formati
 * 
 * Supporta:
 * - ASTNUM_LIST (formato output IOccultCalc)
 * - MPC_NUMBERED (numeri MPC semplici)
 * - PLAIN_TEXT (un asteroide per riga)
 */

#ifndef IOCCULTCALC_ASTEROID_LIST_READER_H
#define IOCCULTCALC_ASTEROID_LIST_READER_H

#include <string>
#include <vector>
#include <fstream>
#include <sstream>
#include <stdexcept>

namespace ioccultcalc {

/**
 * @brief Formati supportati per file input lista asteroidi
 */
enum class AsteroidListFormat {
    AUTO_DETECT,    ///< Rileva automaticamente il formato
    ASTNUM_LIST,    ///< Formato ASTNUM_LIST (output IOccultCalc)
    MPC_NUMBERED,   ///< Solo numeri MPC (un numero per riga)
    PLAIN_TEXT      ///< Testo semplice (numero o designazione per riga)
};

/**
 * @brief Entry singolo asteroide da file
 */
struct AsteroidListEntry {
    int number;                  ///< Numero MPC (0 se non numerico)
    std::string designation;     ///< Designazione (es: "2024 AA")
    std::string name;            ///< Nome se presente nei commenti
    int eventCount;              ///< Conteggio eventi se presente
    
    AsteroidListEntry() : number(0), eventCount(0) {}
    
    // Helper per identificazione
    bool hasNumber() const { return number > 0; }
    bool hasDesignation() const { return !designation.empty(); }
    bool hasName() const { return !name.empty(); }
    
    // Rappresentazione stringa per debug
    std::string toString() const {
        std::stringstream ss;
        if (hasNumber()) {
            ss << number;
            if (hasName()) ss << " (" << name << ")";
        } else if (hasDesignation()) {
            ss << designation;
        }
        return ss.str();
    }
};

/**
 * @brief Reader per file liste asteroidi
 */
class AsteroidListReader {
public:
    /**
     * @brief Legge lista asteroidi da file
     * @param filename Path al file
     * @param format Formato del file (AUTO_DETECT per rilevamento automatico)
     * @return Vettore di entry asteroidi
     */
    static std::vector<AsteroidListEntry> readFromFile(
        const std::string& filename,
        AsteroidListFormat format = AsteroidListFormat::AUTO_DETECT);
    
    /**
     * @brief Legge solo numeri asteroidi da file (ignora entry senza numero)
     * @param filename Path al file
     * @param format Formato del file
     * @return Vettore di numeri asteroidi
     */
    static std::vector<int> readNumbersFromFile(
        const std::string& filename,
        AsteroidListFormat format = AsteroidListFormat::AUTO_DETECT);
    
    /**
     * @brief Rileva automaticamente il formato del file
     * @param filename Path al file
     * @return Formato rilevato
     */
    static AsteroidListFormat detectFormat(const std::string& filename);
    
    /**
     * @brief Verifica se una linea è un commento
     */
    static bool isCommentLine(const std::string& line);
    
    /**
     * @brief Estrae numero asteroide da una linea
     * @return Numero asteroide (0 se non trovato)
     */
    static int extractAsteroidNumber(const std::string& line);
    
    /**
     * @brief Estrae nome asteroide dai commenti (se presente)
     * @return Nome (vuoto se non trovato)
     */
    static std::string extractAsteroidName(const std::string& line);
    
    /**
     * @brief Estrae conteggio eventi dai commenti (se presente)
     * @return Numero eventi (0 se non trovato)
     */
    static int extractEventCount(const std::string& line);
    
    /**
     * @brief Pulisce stringa da whitespace e commenti inline
     */
    static std::string cleanLine(const std::string& line);
    
private:
    // Parsing formato-specifico
    static std::vector<AsteroidListEntry> readAstnumList(std::ifstream& file);
    static std::vector<AsteroidListEntry> readMpcNumbered(std::ifstream& file);
    static std::vector<AsteroidListEntry> readPlainText(std::ifstream& file);
};

} // namespace ioccultcalc

#endif // IOCCULTCALC_ASTEROID_LIST_READER_H
