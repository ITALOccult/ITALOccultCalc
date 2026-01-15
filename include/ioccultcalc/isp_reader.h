#ifndef IOCCULTCALC_ISP_READER_H
#define IOCCULTCALC_ISP_READER_H

#include "types.h"
#include <utility>

namespace ioccultcalc {

/**
 * @struct SPKState
 * @brief Rappresentazione base di uno stato cartesiano per i lettori SPICE
 */
struct SPKState {
    Vector3D position;
    Vector3D velocity;
    double epoch_jd;
    SPKState() : epoch_jd(0) {}
    SPKState(const Vector3D& p, const Vector3D& v, double jd) 
        : position(p), velocity(v), epoch_jd(jd) {}
};

/**
 * @brief Interface for SPICE/Ephemeris readers
 * 
 * Allows decoupling the Ephemeris engine from the specific SPICE implementation.
 */
class ISPReader {
public:
    virtual ~ISPReader() = default;

    /**
     * @brief Get state (position and velocity) of a target body relative to a center
     * 
     * @param target NAIF ID of target body
     * @param jd Julian Date (TDB)
     * @param center NAIF ID of center body
     * @return std::pair<Vector3D, Vector3D> Position (AU) and Velocity (AU/day)
     */
    virtual std::pair<Vector3D, Vector3D> getState(int target, double jd, int center) = 0;

    /**
     * @brief Get position only (convenience)
     */
    virtual Vector3D getPosition(int target, double jd, int center) {
        return getState(target, jd, center).first;
    }

    /**
     * @brief Get velocity only (convenience)
     */
    virtual Vector3D getVelocity(int target, double jd, int center) {
        return getState(target, jd, center).second;
    }

    /**
     * @brief Check if any kernels are currently loaded
     */
    virtual bool isLoaded() const = 0;

    /**
     * @brief Get Earth state (Convenience for AstDynWrapper)
     */
    virtual SPKState getEarthState(double jd) {
        auto s = getState(399, jd, 10);
        SPKState cs;
        cs.position = s.first;
        cs.velocity = s.second;
        cs.epoch_jd = jd;
        return cs;
    }
};

} // namespace ioccultcalc

#endif // IOCCULTCALC_ISP_READER_H
