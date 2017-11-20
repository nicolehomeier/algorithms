# N. Homeier
# June 2015
#
# this code takes input and saves a csv of insolation values for each minute of a day

import argparse
import math
import datetime
import numpy as np

parser = argparse.ArgumentParser()
parser.add_argument('-la','--latitude',default=42.3867617,type=float,help="latitude at location of interest")
parser.add_argument('-lo','--longitude',default=-71.0974135,type=float,help="longitude at location of interest")
parser.add_argument('-d','--dayofyear',default=datetime.datetime.today().timetuple().tm_yday,type=float,help="day of year (1-366)")
parser.add_argument('-az','--azimuthmodule',default=180.,type=float,help="which direction is the panel facing, should be 180/due south")
parser.add_argument('-t','--tiltangle',default=60.,type=float,help="tilt angle of panel, ro maximize energy should be = latitude")
parser.add_argument('-e','--elevation',default='0.03',type=float,help="elevation in km of location of interest")
args = parser.parse_args()

psi_little = math.radians(args.latitude)
long = args.longitude
d = args.dayofyear
psi = math.radians(args.azimuthmodule)
beta = math.radians(args.tiltangle)
h = args.elevation


# declination angle
delta = math.radians(23.45)*math.sin(math.radians(360./365.*(d-81.)))

sunrise = 12. - 1./15.*math.degrees(np.arccos(-np.sin(psi_little)*np.sin(delta)/(np.cos(psi_little)*np.cos(delta))))
sunset = 12. + 1./15.*math.degrees(np.arccos(-np.sin(psi_little)*np.sin(delta)/(np.cos(psi_little)*np.cos(delta))))

LT = np.arange(0,24,0.0002777778) #each second
b = math.radians(360./365.*(d-81.))
eot = 9.87*np.sin(2*b) - 7.53*np.cos(b) - 1.5*np.sin(b) #equation of time

########DANGER#########
lstm = -4 #this needs to be calculated! currently hard-coded at 4h, this is the difference between local time and GMT in hours, currently hard-coded at -4h, this is put in to ensure the output is at each minute of the day in local time so it matches our slow data and a vector to vector difference can be easily calculated
########DANGER#########

tc = 4*(long - 15*lstm) # time correction in minutes
LST = LT + tc/60

HRA = 15.*(LST-12.)
HRA = np.radians(HRA)

S_mod = np.zeros([len(LST)])
I_d = np.zeros([len(LST)])

for i in range(len(LST)):
    if LST[i] > sunrise and LST[i] < sunset:
        # sun elevation angle
        alpha = np.arcsin(np.sin(delta)*np.sin(psi_little) + np.cos(delta)*np.cos(psi_little)*np.cos(HRA[i]))
        zenith_ang = np.radians(90 - np.degrees(alpha))
        # sun azimuth angle
        if HRA[i] < 0:
            big_theta = np.arccos((np.sin(delta)*np.cos(psi_little)-np.cos(delta)*np.sin(psi_little)*np.cos(HRA[i]))/np.cos(alpha))
        else:
            azi = np.arccos((np.sin(delta)*np.cos(psi_little)-np.cos(delta)*np.sin(psi_little)*np.cos(HRA[i]))/np.cos(alpha))
            big_theta = math.radians(360 - math.degrees(azi))
        
        AM = 1/(np.cos(zenith_ang) + 0.50572*(96.07995-zenith_ang)**-1.6364)
        
        amfac = AM**0.678
        I_d[i] = 1.353*((1-0.14*h)*0.7**amfac + 0.14*h)
        
        # I_d is the intensity perpendicular to the sun's rays, units are kW/m^2
        
        S_mod[i] = max(0,I_d[i]*(np.cos(alpha)*np.sin(beta)*np.cos(psi-big_theta) + np.sin(alpha)*np.cos(beta)))

np.savetxt("pyinsol.csv",S_mod,delimiter=',')

