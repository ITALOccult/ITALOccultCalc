#ifndef IOCCULTCALC_TIME_UTILS_H
#define IOCCULTCALC_TIME_UTILS_H

#include "types.h"
#include <string>
#include <ctime>

namespace ioccultcalc {

class TimeUtils {
public:
    // Converte una data ISO (YYYY-MM-DD o YYYY-MM-DD HH:MM:SS) in Julian Date
    static JulianDate isoToJD(const std::string& isoDate);
    
    // Converte Julian Date in stringa ISO
    static std::string jdToISO(const JulianDate& jd);
    
    // Calcola Julian Date dal calendario gregoriano
    static JulianDate calendarToJD(int year, int month, int day, 
                                    int hour = 0, int minute = 0, double second = 0.0);
    
    // Converte Julian Date in calendario gregoriano
    static void jdToCalendar(const JulianDate& jd, int& year, int& month, int& day,
                            int& hour, int& minute, double& second);
    
    // Ottiene il Julian Date corrente
    static JulianDate now();
    
    // Calcola il Greenwich Mean Sidereal Time
    static double gmst(const JulianDate& jd);
    
    // Calcola il Local Sidereal Time per una data longitudine
    static double lst(const JulianDate& jd, double longitude);
};

} // namespace ioccultcalc

#endif // IOCCULTCALC_TIME_UTILS_H
