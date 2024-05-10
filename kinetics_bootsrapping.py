# -*- coding: utf-8 -*-
"""
Created on Sat Jun 18 18:46:08 2022

@author: uthma
"""

import numpy as np
import pandas as pd
#import matplotlib.pyplot as plt
from scipy.optimize import minimize
import scipy.stats as st
#from scipy.interpolate import make_interp_spline, BSpline

# Import data file
# Column 1 = time (t)
# Column 2 = output (ymeas)
wd = "C:\\Users\\uthma\\Dropbox (UFL)\\PhD\\Lab analysis result\\Block9_15_Corrected\\blk9_15.csv"
pd.read_csv(wd)
data = pd.read_csv(wd)
Conc = [1.16864,2.97223,6.13536,7.71319,9.28853]

alpha = []
kd = []
R2 = []
np.random.seed(6917)
for i in range(5):
    dat = data[(data["C0"] == Conc[i])]

    for j in range(10):
        dats = dat.sample(n=12)
        t = np.array(dats['Time'])
        ymeas = np.array(dats['Ce_C0'])
        ## m/v is 1
        def model(p):
            a = p[0]
            k = p[1]
            #f = p[2]
            # predicted values
            f=0
            m=1
            v=1
            r=1+(m/v)*k
            b=(1+f*(m/v)*k)/r
            ypred=1/r+((1/(r*b)-1/r)*np.exp(-(a/b)*t))
            return ypred
        def objective(p):
            ypred = model(p)    
            sse = sum((ymeas-ypred)**2)
            return sse

        # initial guesses for a, f, and k
        p0 = [0.005,0.5]

        # show initial objective
        print('Initial SSE Objective: ' + str(objective(p0)))

        # optimize a,f, and k
        solution = minimize(objective,p0)
        p = solution.x
        alpha.append(p[0])
        kd.append(p[1])

        m = sum(ymeas)/float(len(ymeas))
        sst = sum((ymeas - m)**2)
        nse = 1 - solution.fun/sst
        R2.append(nse)
    #alpha    
    alpha_ci = st.t.interval(alpha=0.95,
                    df=len(alpha)-1,
                    loc=np.mean(alpha),
                    scale=st.sem(alpha))
    #kd
    kd_ci = st.t.interval(alpha=0.95,
			df=len(kd)-1,
			loc=np.mean(kd),
			scale=st.sem(kd))
    #nse
    nse_ci = st.t.interval(alpha=0.95,
			df=len(R2)-1,
			loc=np.mean(R2),
			scale=st.sem(R2))
    print('alpha confidence interval: ' + str(alpha_ci))
    print('kd confidence interval: ' + str(kd_ci))
    print('RSquared confidence interval: ' + str(nse_ci)) 
    alpha.clear()
    kd.clear()
    R2.clear()
    
    # show final objective
#    print('Final SSE Objective: ' + str(objective(p)))

#    print('a: ' + str(p[0]))
#    print('k: ' + str(p[1]))
    #print('f: ' + str(p[2]))
#    print('RSquared: ' + str(nse))

#import scipy.stats as st

# define sample data
# create 95% confidence interval
# alpha

