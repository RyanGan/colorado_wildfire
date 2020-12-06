# ----------------------------------------------------------
# Title: Exploration and regrid of High Park WRF-Chem Models
# Author: Ryan Gan
# Date: 2018-10-16
# Python Version: 3.6
# ----------------------------------------------------------

# This python script evaluates Will's WRF-Chem estimates of 
# the High Park fire that effected the Ft. Collins area.
# It also aligns the grids of this WRF-Chem model with grids
# used in the Colorado smoke analysis.

# modules used ----
# numpy for arrays
import numpy as np
import pandas as pd 
# plotting packages
from matplotlib import pyplot as plt
import seaborn as sns
# netcdf module
import netCDF4 as nc 
# import dataset
from netCDF4 import Dataset
# import interpolate from scipy
from scipy import interpolate
import os 


# read nc files -----
wrf_pm = Dataset('/Users/ryangan/Documents/local_git_repo/' +
    'colorado_wildfire/data/smoke/HighParkPM25_15KM.nc')

# print out summary of wrf nc
print(wrf_pm)

# print out each variable detail
for v in wrf_pm.variables:
    print(wrf_pm.variables[v])
