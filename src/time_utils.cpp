#include "ioccultcalc/time_utils.h"
#include <sstream>
#include <iomanip>
#include <cmath>
#include <chrono>
#include <stdexcept>

namespace ioccultcalc {

JulianDate TimeUtils::isoToJD(const std::string& isoDate) {
    int year, month, day, hour = 0, minute = 0;
    double second = 0.0;
    
    char dummy;
    std::istringstream iss(isoDate);
    
    iss >> year >> dummy >> month >> dummy >> day;
    
    if (iss >> hour >> dummy >> minute >> dummy >> second) {
        // Parsed with time
    }
    
    return calendarToJD(year, month, day, hour, minute, second);
}

std::string TimeUtils::jdToISO(const JulianDate& jd) {
    int year, month, day, hour, minute;
    double second;
    
    jdToCalendar(jd, year, month, day, hour, minute, second);
    
    std::ostringstream oss;
    oss << std::setfill('0') 
        << std::setw(4) << year << "-"
        << std::setw(2) << month << "-"
        << std::setw(2) << day << " "
        << std::setw(2) << hour << ":"
        << std::setw(2) << minute << ":"
        << std::setw(2) << static_cast<int>(second);
    
    return oss.str();
}

JulianDate TimeUtils::calendarToJD(int year, int month, int day,
                                    int hour, int minute, double second) {
    // Algoritmo da Meeus "Astronomical Algorithms"
    if (month <= 2) {
        year -= 1;
        month += 12;
    }
    
    int A = year / 100;
    int B = 2 - A + A / 4;
    
    double jd = static_cast<int>(365.25 * (year + 4716)) +
                static_cast<int>(30.6001 * (month + 1)) +
                day + B - 1524.5;
    
    // Aggiungi la frazione del giorno
    double dayFraction = (hour + minute / 60.0 + second / 3600.0) / 24.0;
    jd += dayFraction;
    
    return JulianDate(jd);
}

void TimeUtils::jdToCalendar(const JulianDate& jd, int& year, int& month, int& day,
                             int& hour, int& minute, double& second) {
    // Algoritmo da Meeus "Astronomical Algorithms"
    double jdVal = jd.jd + 0.5;
    int Z = static_cast<int>(jdVal);
    double F = jdVal - Z;
    
    int A = Z;
    if (Z >= 2299161) {
        int alpha = static_cast<int>((Z - 1867216.25) / 36524.25);
        A = Z + 1 + alpha - alpha / 4;
    }
    
    int B = A + 1524;
    int C = static_cast<int>((B - 122.1) / 365.25);
    int D = static_cast<int>(365.25 * C);
    int E = static_cast<int>((B - D) / 30.6001);
    
    day = B - D - static_cast<int>(30.6001 * E);
    month = (E < 14) ? E - 1 : E - 13;
    year = (month > 2) ? C - 4716 : C - 4715;
    
    // Calcola ore, minuti, secondi dalla frazione
    double dayFraction = F * 24.0;
    hour = static_cast<int>(dayFraction);
    dayFraction = (dayFraction - hour) * 60.0;
    minute = static_cast<int>(dayFraction);
    second = (dayFraction - minute) * 60.0;
}

JulianDate TimeUtils::now() {
    auto now = std::chrono::system_clock::now();
    auto now_time_t = std::chrono::system_clock::to_time_t(now);
    auto now_ms = std::chrono::duration_cast<std::chrono::milliseconds>(
        now.time_since_epoch()) % 1000;
    
    std::tm* tm = std::gmtime(&now_time_t);
    double second = tm->tm_sec + now_ms.count() / 1000.0;
    
    return calendarToJD(tm->tm_year + 1900, tm->tm_mon + 1, tm->tm_mday,
                       tm->tm_hour, tm->tm_min, second);
}

double TimeUtils::gmst(const JulianDate& jd) {
    // Greenwich Mean Sidereal Time
    // Formula da Meeus
    double T = (jd.jd - 2451545.0) / 36525.0;
    
    double gmst = 280.46061837 + 360.98564736629 * (jd.jd - 2451545.0) +
                  T * T * (0.000387933 - T / 38710000.0);
    
    // Normalizza a [0, 360)
    gmst = fmod(gmst, 360.0);
    if (gmst < 0) gmst += 360.0;
    
    return gmst * DEG_TO_RAD;
}

double TimeUtils::lst(const JulianDate& jd, double longitude) {
    // Local Sidereal Time
    double gmstRad = gmst(jd);
    double lstRad = gmstRad + longitude * DEG_TO_RAD;
    
    // Normalizza a [0, 2π)
    lstRad = fmod(lstRad, 2.0 * M_PI);
    if (lstRad < 0) lstRad += 2.0 * M_PI;
    
    return lstRad;
}

} // namespace ioccultcalc
