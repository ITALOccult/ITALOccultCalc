import math

# Constanti
R_EARTH = 6378.137  # km

def bessel_to_geodetic(x, y, ra_s, dec_s, gmst_deg):
    if x**2 + y**2 > 1.0:
        return None
    z = math.sqrt(1.0 - x**2 - y**2)
    ra = math.radians(ra_s)
    dec = math.radians(dec_s)
    vx = -math.sin(ra) * x - math.sin(dec) * math.cos(ra) * y + math.cos(dec) * math.cos(ra) * z
    vy =  math.cos(ra) * x - math.sin(dec) * math.sin(ra) * y + math.cos(dec) * math.sin(ra) * z
    vz =  math.cos(dec) * y + math.sin(dec) * z
    gmst = math.radians(gmst_deg)
    lon_icrf = math.atan2(vy, vx)
    lat = math.degrees(math.asin(vz))
    lon = math.degrees(lon_icrf - gmst)
    while lon < -180: lon += 360
    while lon > 180: lon -= 360
    return lat, lon

# Dati Occult4
o4 = {'t0': 0.1394608, 'x0': 0.1052127, 'y0': -0.1654456, 'dx': 4.6728342, 'dy': 2.9526616}
# Dati ITALOCC
it = {'t0': 0.139231, 'x0': 0.10957501, 'y0': -0.17284438, 'dx': 4.66744736, 'dy': 2.96131922}

ra_s = 44.256535 * 15.0
dec_s = 17.239354
gmst_ref = 118.736157

def generate_kml_line(data, name, color):
    points = []
    for t_min in range(-15, 16):
        t_ut = data['t0'] + (t_min / 60.0)
        gmst = gmst_ref + (t_ut * 15.041067)
        x = data['x0'] + data['dx'] * (t_ut - data['t0'])
        y = data['y0'] + data['dy'] * (t_ut - data['t0'])
        pos = bessel_to_geodetic(x, y, ra_s, dec_s, gmst)
        if pos:
            points.append(f"{pos[1]},{pos[0]},1000")
    
    return f"""    <Placemark>
      <name>{name}</name>
      <Style>
        <LineStyle><color>{color}</color><width>4</width></LineStyle>
      </Style>
      <LineString>
        <altitudeMode>relativeToGround</altitudeMode>
        <coordinates>{' '.join(points)}</coordinates>
      </LineString>
    </Placemark>"""

kml_content = f"""<?xml version="1.0" encoding="UTF-8"?>
<kml xmlns="http://www.opengis.net/kml/2.2">
  <Document>
    <name>Occultation Comparison: Utkin 13477</name>
    {generate_kml_line(o4, "Occult4 (Dave Herald)", "ff0000ff")}
    {generate_kml_line(it, "ITALOccultCalc", "ff00ff00")}
  </Document>
</kml>"""

with open("utkin_comparison.kml", "w") as f:
    f.write(kml_content)
print("✅ utkin_comparison.kml generated.")
