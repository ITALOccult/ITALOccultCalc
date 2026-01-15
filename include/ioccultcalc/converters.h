#ifndef IOCCULTCALC_CONVERTERS_H
#define IOCCULTCALC_CONVERTERS_H

#include "ioccultcalc/gaia_client.h" // For ioccultcalc::GaiaStar
#include "ioccultcalc/types.h"
#include <string>

namespace ioccultcalc {
namespace converters {

inline GaiaStar toGaiaStar(const CandidateStar& candidate) {
    GaiaStar star;
    star.sourceId = std::to_string(candidate.source_id); // sourceId is string in GaiaStar
    star.pos.ra = candidate.ra_deg;
    star.pos.dec = candidate.dec_deg;
    star.pmra = candidate.pmra;
    star.pmdec = candidate.pmdec;
    star.parallax = candidate.parallax;
    star.phot_g_mean_mag = candidate.phot_g_mean_mag;
    return star;
}

} // namespace converters
} // namespace ioccultcalc

#endif // IOCCULTCALC_CONVERTERS_H
