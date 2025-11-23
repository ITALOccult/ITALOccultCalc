#!/usr/bin/env python3
"""
Verifica formule Terra usando pyephem/skyfield
"""
import math

# Data: 2024-12-10 02:30 UTC
jd = 2460656.604166667

# Secoli giuliani da J2000.0
T = (jd - 2451545.0) / 36525.0
print(f"T = {T:.10f} secoli")

# Longitudine media del Sole
L = 280.46646 + 36000.76983 * T + 0.0003032 * T * T
print(f"L (mean longitude) = {L:.6f}°")

# Anomalia media
M = 357.52911 + 35999.05029 * T - 0.0001537 * T * T
L = L % 360.0
M = M % 360.0
print(f"M (mean anomaly) = {M:.6f}°")

M_rad = math.radians(M)

# Eccentricità
e = 0.016708634 - 0.000042037 * T - 0.0000001267 * T * T
print(f"e (eccentricity) = {e:.10f}")

# Equazione del centro
C = (1.914602 - 0.004817 * T - 0.000014 * T * T) * math.sin(M_rad) + \
    (0.019993 - 0.000101 * T) * math.sin(2.0 * M_rad) + \
    0.000289 * math.sin(3.0 * M_rad)
print(f"C (equation of center) = {C:.6f}°")

# Longitudine vera del Sole
sunLon = L + C
print(f"Sun true longitude = {sunLon:.6f}°")

# Distanza
R = (1.000001018 * (1.0 - e * e)) / (1.0 + e * math.cos(M_rad + math.radians(C)))
print(f"R (distance) = {R:.10f} AU")

# Longitudine Terra
earthLon = (sunLon + 180.0) % 360.0
print(f"Earth longitude = {earthLon:.6f}°")

# Coordinate eclittiche
x_ecl = R * math.cos(math.radians(earthLon))
y_ecl = R * math.sin(math.radians(earthLon))
print(f"\nEclittica: x={x_ecl:.6f}, y={y_ecl:.6f}, z=0")

# Conversione equatoriale (eps J2000 = 23.4392911°)
eps = 23.4392911
eps_rad = math.radians(eps)

x_eq = x_ecl
y_eq = y_ecl * math.cos(eps_rad)
z_eq = y_ecl * math.sin(eps_rad)

print(f"Equatoriale: x={x_eq:.6f}, y={y_eq:.6f}, z={z_eq:.6f}")
print(f"\nHorizons:    x=0.203452, y=0.963555, z=-5.5633e-05")
