import math

# Elements for 1272 Gefion from Occult4 XML (Epoch 2026-01-12)
# <Orbit>0,141.1827,2026,1,12,5.0507,321.2641,8.4266,0.15105,2.78796,2.36682,12.92,5.0,0.15</Orbit>
a = 2.78796
e = 0.15105
i_deg = 8.4266
node_deg = 321.2641
w_deg = 5.0507
M_deg = 141.1827
epoch_mjd = 61052.0 # 2026-01-12

# Convert to radians
d2r = math.pi / 180.0
i_rad = i_deg * d2r
node_rad = node_deg * d2r
w_rad = w_deg * d2r
M_rad = M_deg * d2r

# Derived angles
varpi = w_rad + node_rad 
lambda_mean = M_rad + varpi 

# Equinoctial Elements
h = e * math.sin(varpi)
k = e * math.cos(varpi)
p = math.tan(i_rad / 2.0) * math.sin(node_rad)
q = math.tan(i_rad / 2.0) * math.cos(node_rad)

lambda_deg = (lambda_mean * 180.0 / math.pi) % 360.0

# Construct EQ1 content
header = """format  = 'OEF2.0'       ! file format
rectype = 'ML'           ! record type (1L/ML)
refsys  = ECLM J2000     ! default reference system
END_OF_HEADER
1272
! Equinoctial elements from JPL Horizons / Occult4
"""

line_eq = f" EQU   {a:.16E}  {h:.16E}   {k:.16E}   {p:.16E}    {q:.16E}  {lambda_deg:.16E}\n"
line_mjd = f" MJD     {epoch_mjd:.9f} TDT\n"
line_mag = " MAG  12.8  0.150\n" 
line_lsp = "! Non-grav parameters\n LSP   0  0    6\n"

# Synthetic Covariance (Small value)
vals = [0.0] * 21
std_dev = 1.0e-9
vals[0] = std_dev**2
vals[2] = std_dev**2
vals[5] = std_dev**2
vals[9] = std_dev**2
vals[14] = std_dev**2
vals[20] = std_dev**2

cov_str = ""
for i in range(0, 21, 3):
    chunk = vals[i:i+3]
    line_str = " COV"
    for v in chunk:
        line_str += f"   {v:.16E}"
    cov_str += line_str + "\n"

content = header + line_eq + line_mjd + line_mag + line_lsp + cov_str

with open("1272_jpl.eq1", "w") as f:
    f.write(content)

print("1272_jpl.eq1 created.")
