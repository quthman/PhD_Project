# -*- coding: utf-8 -*-
"""
Created on Sun Jun 12 10:01:34 2022

@author: uthma
"""

from numpy import exp, random, array

from lmfit import Parameters, fit_report, minimize

p_true = Parameters()
p_true.add('a', value=.48)
p_true.add('k', value=.83)
t=array([ 0,  2,  2,  2,  8,  8,  8, 16, 16, 16, 32, 32, 32, 64, 64, 64])

def residual(pars, x, data=None):
    """Model a decaying sine wave and subtract data."""
    vals = pars.valuesdict()
    a = vals['a']
    k = vals['k']
    f=0
    m=1
    v=1
    r=1+(m/v)*k
    b=(1+f*(m/v)*k)/r
    ypred=1/r+((1/(r*b)-1/r)*exp(-(a/b)*t))
    
    if data is None:
        return ypred
    return ypred - data


random.seed(0)
x = array([1.        , 0.60872542, 0.61263817, 0.65303727, 0.54573022,
       0.54289347, 0.59679155, 0.53898073, 0.55580554, 0.54905605,
       0.5010271 , 0.48283283, 0.5309596 , 0.53506798, 0.60872542,
       0.56940233])
#noise = random.normal(scale=0.7215, size=x.size)
data = residual(p_true, x)

fit_params = Parameters()
fit_params.add('a', value=.47)
fit_params.add('k', value=.82)
#fit_params.add('shift', value=0.0)
#fit_params.add('decay', value=0.02)

out = minimize(residual, fit_params, args=(x,), kws={'data': data})

print(fit_report(out))
# <end examples/doc_fitting_withreport.py>