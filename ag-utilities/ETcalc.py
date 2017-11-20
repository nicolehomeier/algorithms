# N. Homeier
# Feb 2017
#
#
# RTi station height = 8ft = 2.44m
# uz = RTi wind
#
# 1 kWh = 3.6MJ
# insolation units are kWh/(m^2*d)
# and here we need MJ/(m^2*d)
# so should take our daily insolation estimate in kWh/m^2 and multiply by 3.6 to get to daily MJ/m^2

import argparse
import math
import datetime
import numpy as np

parser = argparse.ArgumentParser()
parser.add_argument('-la','--latitude',default=21.629779,type=float,help="latitude at location of interest")
parser.add_argument('-d','--dayofyear',default=datetime.datetime.today().timetuple().tm_yday,type=float,help="day of year (1-366)")
parser.add_argument('-e','--elevation',default='0.01',type=float,help="elevation in km of location of interest")
parser.add_argument('-u','--wind',default='2',type=float,help="mean daily wind speed in mph")
parser.add_argument('-rs','--solarrad',default='5.56',type=float,help="estimated solar radiation in kWh m^-2 d^-1")
parser.add_argument('-tx','--Tmax',default='30',type=float,help="in C, maximum daily T")
parser.add_argument('-tn','--Tmin',default='20',type=float,help="in C, minimum daily T")
parser.add_argument('-td','--Td',default='18',type=float,help="average dew point btwn 7-8am local time")

args = parser.parse_args()

psi_little = math.radians(args.latitude)
d = args.dayofyear
h = args.elevation
uz = args.wind
R_s = args.solarrad
Tx = args.Tmax
Tn = args.Tmin
T_amdew = args.Td

G = 0   #this is only non-zero if calculating less than daily time periods
omega = 4.901e-9    #stefan-boltzmann constant,MJ K^-4 m^-2 d^-1
u_2 = uz*0.44704*(4.87/(np.log(67.8*2.44-5.42)))   #get windspeed at 2m in m/s
R_s = 3.6*R_s   #convert from kWh/m^2 to MJ m^-2 d^-1


d_r = 1+0.033*np.cos(2*3.14159/365*d)      # correction for orbit eccentricity, I mean come on now, but ok I'll do it
delta = 0.409*np.sin(2*3.14159/365*d-1.39)  #sun's declination above celestial equator
omega_s = np.arccos(-np.tan(psi_little)*np.tan(delta))  #sunrise HA
R_a = (24*60/3.14159)*0.082*d_r*(omega_s*np.sin(delta)*np.sin(psi_little) + np.cos(psi_little)*np.cos(delta)*np.sin(omega_s))

R_so = R_a*(0.75 + 0.00002*h*1000)
R_ns = (1-0.23)*R_s
f = 1.35*(R_s/R_so) - 0.35
e_a = 0.6108*math.exp((17.27*T_amdew)/(T_amdew+237.3))  #actual vapor pressure, use a 7:30am local time dew point measurement, this is T_amdew
epsilonp = 0.34 - 0.14*math.sqrt(e_a)
R_nl = -f*epsilonp*omega*(((Tx+273.15)**4 + (Tn+273.15)**4)/2)


z = 2.44 #height of RTi in m
T = (Tx + Tn)/2 #mean daily air temperature in C at 1.5-2.5m height
P = 101.3*((293-0.0065*z)/293)**5.26
delta = 2503*math.exp((17.27*T)/(T+237.3))/(T+237.3)**2  #slope of saturation vapor pressure curve
R_n = R_ns + R_nl  #net radiation at crop surface
gamma = 0.000665*P #kPa/C
C_n = 1600 # daily, tall  #constant, look up
e_sTx = 0.6108*math.exp(17.27*Tx/(Tx+237.3))
e_sTn = 0.6108*math.exp(17.27*Tn/(Tn+237.3))
e_s = (e_sTx + e_sTn)/2 #saturation vapor pressure
C_d = 0.38      # daily, tall   #constant, look up

print(R_n)

numer = 0.408*delta*(R_n - G) + gamma*(C_n/(T + 273))*u_2*(e_s - e_a)
denom = delta + gamma*(1+C_d*u_2)

ET_sz = numer/denom #mm per day
print(ET_sz)
