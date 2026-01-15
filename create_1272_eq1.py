import math

# Elements for 1272 Gefion (Epoch 2461000.5 = MJD 61000.0)
# Reference: JPL Small-Body Database (Mean Ecliptic J2000)
a = 2.78812814728145
e = 0.1508231407184913
i_deg = 8.423357131812779
node_deg = 321.2760489955581
w_deg = 4.986457687388398
M_deg = 130.2574665756102
epoch_mjd = 61000.0

# Convert to radians
d2r = math.pi / 180.0
i_rad = i_deg * d2r
node_rad = node_deg * d2r
w_rad = w_deg * d2r
M_rad = M_deg * d2r

# Derived angles (Longitude of Perihelion and Mean Longitude)
varpi = w_rad + node_rad 
lambda_mean = M_rad + varpi 

# Equinoctial Elements (Standard conversion)
h = e * math.sin(varpi)
k = e * math.cos(varpi)
p = math.tan(i_rad / 2.0) * math.sin(node_rad)
q = math.tan(i_rad / 2.0) * math.cos(node_rad)

# lambda_deg for EQ1 needs to be in degrees (per parser logic)
lambda_deg = (lambda_mean * 180.0 / math.pi) % 360.0

print(f"a={a}, h={h}, k={k}, p={p}, q={q}, lambda={lambda_deg}")

# Construct EQ1 content
header = """format  = 'OEF2.0'       ! file format
rectype = 'ML'           ! record type (1L/ML)
refsys  = ECLM J2000     ! default reference system
END_OF_HEADER
1272
! Equinoctial elements: a, e*sin(LP), e*cos(LP), tan(i/2)*sin(LN), tan(i/2)*cos(LN), mean long.
"""

line_eq = f" EQU   {a:.16E}  {h:.16E}   {k:.16E}   {p:.16E}    {q:.16E}  {lambda_deg:.16E}\n"
line_mjd = f" MJD     {epoch_mjd:.9f} TDT\n"
line_mag = " MAG  12.8  0.150\n" 
line_lsp = "! Non-grav parameters: model used, actual number in use, dimension\n LSP   0  0    6\n"

# Synthetic Covariance (Diagonal 1e-16 to avoid singular matrix during initialization)
vals = [0.0] * 21
std_dev = 1.0e-8 
vals[0] = std_dev**2 # aa
vals[2] = std_dev**2 # hh
vals[5] = std_dev**2 # kk
vals[9] = std_dev**2 # pp
vals[14] = std_dev**2 # qq
vals[20] = std_dev**2 # ll

cov_str = ""
for i in range(0, 21, 3):
    chunk = vals[i:i+3]
    line_str = " COV"
    for v in chunk:
        line_str += f"   {v:.16E}"
    cov_str += line_str + "\n"

content = header + line_eq + line_mjd + line_mag + line_lsp + cov_str

with open("1272.eq1", "w") as f:
    f.write(content)

print("1272.eq1 created for Gefion (Mean Ecliptic J2000).")
