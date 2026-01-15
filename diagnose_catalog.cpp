#include <iostream>
#include <vector>
#include <nlohmann/json.hpp>
#include "ioc_gaialib/unified_gaia_catalog.h"
#include "ioc_gaialib/types.h"

int main() {
    const char* home = getenv("HOME");
    std::string homeDir = home ? home : "";
    std::string catalogPath = homeDir + "/.catalog/gaia_mag18_v2_multifile";
    
    nlohmann::json gaiaConfJson;
    gaiaConfJson["catalog_type"] = "multifile_v2";
    gaiaConfJson["multifile_directory"] = catalogPath;
    
    if (!ioc::gaia::UnifiedGaiaCatalog::initialize(gaiaConfJson.dump())) {
        std::cerr << "Failed to initialize catalog\n";
        return 1;
    }
    
    auto& catalog = ioc::gaia::UnifiedGaiaCatalog::getInstance();
    
    ioc::gaia::QueryParams params;
    params.ra_center = 122.837;
    params.dec_center = 25.137;
    params.radius = 0.5;
    params.max_magnitude = 18.0;
    
    std::cout << "Querying cone at RA=" << params.ra_center << " Dec=" << params.dec_center << " radius=" << params.radius << "...\n";
    auto stars = catalog.queryCone(params);
    
    std::cout << "Found " << stars.size() << " stars. Listing brightest ones:\n";
    
    std::sort(stars.begin(), stars.end(), [](const ioc::gaia::GaiaStar& a, const ioc::gaia::GaiaStar& b) {
        return a.phot_g_mean_mag < b.phot_g_mean_mag;
    });

    for (size_t i = 0; i < std::min(stars.size(), (size_t)20); ++i) {
        double d_ra = (stars[i].ra - 122.837) * std::cos(25.137 * M_PI / 180.0);
        double d_dec = (stars[i].dec - 25.137);
        double dist_arcsec = std::sqrt(d_ra*d_ra + d_dec*d_dec) * 3600.0;
        
        std::cout << "Star: ID=" << stars[i].source_id << " RA=" << stars[i].ra 
                  << " Dec=" << stars[i].dec << " Mag=" << stars[i].phot_g_mean_mag 
                  << " Dist=" << dist_arcsec << " arcsec\n";
    }
    
    // Also try to find by specific Source ID
    uint64_t target_id = 681067090275723392;
    std::cout << "\nQuerying specific Source ID: " << target_id << "..." << std::endl;
    auto target_star = catalog.queryBySourceId(target_id);
    if (target_star) {
        std::cout << "Found Star! ID=" << target_star->source_id 
                  << " RA=" << target_star->ra 
                  << " Dec=" << target_star->dec 
                  << " Mag=" << target_star->phot_g_mean_mag;
        if (!target_star->tycho2_designation.empty()) std::cout << " TYC=" << target_star->tycho2_designation;
        std::cout << std::endl;
    } else {
        std::cout << "Star ID " << target_id << " NOT found in catalog." << std::endl;
    }

    // Also try to find by Tycho-2 if possible
    std::cout << "\nTrying to find TYC 1931-01194-1...\n";
    auto star = catalog.queryByTycho2("1931-01194-1");
    if (star) {
        std::cout << "Found Tycho-2 star! ID=" << star->source_id << " RA=" << star->ra << " Dec=" << star->dec << "\n";
    } else {
        std::cout << "Tycho-2 star NOT found.\n";
    }
    
    return 0;
}
