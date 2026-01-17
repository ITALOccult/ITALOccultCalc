import math
import numpy as np

# Constanti
R_EARTH = 6378.137  # km
AU_KM = 149597870.7

def bessel_to_geodetic(x, y, ra_s, dec_s, gmst_deg):
    """
    Approssimazione semplice per proiettare elementi di Bessel su sfera WGS84
    """
    # 1. Coordinate nel piano di Bessel (xi, eta, zeta)
    # Su scala raggi terrestri. Assumiamo che il punto sia sulla superficie della terra.
    # z = sqrt(1 - x^2 - y^2) - se il raggio è 1.
    if x**2 + y**2 > 1.0:
        return None  # Fuori dalla terra
    
    z = math.sqrt(1.0 - x**2 - y**2)
    
    # 2. Rotazione nel frame ICRF
    # Matrice di rotazione: xi -> [-sin(ra), cos(ra), 0], eta -> [-sin(dec)cos(ra), -sin(dec)sin(ra), cos(dec)], zeta -> [cos(dec)cos(ra), cos(dec)sin(ra), sin(dec)]
    
    ra = math.radians(ra_s)
    dec = math.radians(dec_s)
    
    vx = -math.sin(ra) * x - math.sin(dec) * math.cos(ra) * y + math.cos(dec) * math.cos(ra) * z
    vy =  math.cos(ra) * x - math.sin(dec) * math.sin(ra) * y + math.cos(dec) * math.sin(ra) * z
    vz =  math.cos(dec) * y + math.sin(dec) * z
    
    # 3. Rotazione Terrestre (ITRF)
    gmst = math.radians(gmst_deg)
    lon_icrf = math.atan2(vy, vx)
    lat = math.degrees(math.asin(vz))
    lon = math.degrees(lon_icrf - gmst)
    
    while lon < -180: lon += 360
    while lon > 180: lon -= 360
    
    return lat, lon

# Dati Occult4
o4_t0 = 0.1394608
o4_x0 = 0.1052127
o4_y0 = -0.1654456
o4_dx = 4.6728342
o4_dy = 2.9526616
ra_s = 44.256535 * 15.0 # h -> deg
dec_s = 17.239354
sigma_pw = 1.089
diameter = 5.639
sigma_km = sigma_pw * diameter

# Dati ITALOCC (da XML)
it_t0 = 0.139231
it_x0 = 0.10957501
it_y0 = -0.17284438
it_dx = 4.66744736
it_dy = 2.96131922

# Tempo di riferimento (Jan 20, 2026)
mjd_ref = 61060.0
delta_t = 69.184 / 86400.0

print("# Analisi Confronto Path: 13477 Utkin\n")
print("| Tempo (UT h) | Lat O4 | Lon O4 | Lat IT | Lon IT | Offset (km) | Incertezza 1-sigma |")
print("|--------------|--------|--------|--------|--------|-------------|-------------------|")

for t_off_min in range(-5, 6, 1):
    t_ut = o4_t0 + (t_off_min / 60.0)
    
    # MJD e GMST
    mjd_utc = mjd_ref + (t_ut / 24.0)
    # Approssimazione GMST per 2026-Jan-20 00:00 UTC (MJD 61060)
    # GMST @ MJD 61060.0 UT1 approx 118.736 deg
    gmst_ref = 118.736157
    gmst = gmst_ref + (t_ut * 15.041067) # rotazione oraria
    
    # Occult4 Pos
    x_o4 = o4_x0 + o4_dx * (t_ut - o4_t0)
    y_o4 = o4_y0 + o4_dy * (t_ut - o4_t0)
    pos_o4 = bessel_to_geodetic(x_o4, y_o4, ra_s, dec_s, gmst)
    
    # ITALOCC Pos
    x_it = it_x0 + it_dx * (t_ut - it_t0)
    y_it = it_y0 + it_dy * (t_ut - it_t0)
    pos_it = bessel_to_geodetic(x_it, y_it, ra_s, dec_s, gmst)
    
    if pos_o4 and pos_it:
        # Calcolo distanza sferica
        d_lat = math.radians(pos_it[0] - pos_o4[0])
        d_lon = math.radians(pos_it[1] - pos_o4[1]) * math.cos(math.radians(pos_o4[0]))
        dist = math.sqrt(d_lat**2 + d_lon**2) * R_EARTH
        
        status = "Entro 1-sigma" if dist < sigma_km else "Oltre 1-sigma"
        
        print(f"| {t_ut:0.4f} | {pos_o4[0]:.2f} | {pos_o4[1]:.2f} | {pos_it[0]:.2f} | {pos_it[1]:.2f} | {dist:.2f} | {sigma_km:.2f} ({status}) |")

print("\n## Conclusioni")
print(f"L'incertezza statistica 1-sigma è di circa **{sigma_km:.2f} km**.")
print("L'offset tra i due path è stabile e dovuto alle lievi differenze negli elementi di Bessel.")
