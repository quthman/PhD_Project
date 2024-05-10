import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy.optimize import minimize
#from scipy.interpolate import make_interp_spline, BSpline

# Import data file
# Column 1 = time (t)
# Column 2 = output (ymeas)
wd = "C:\\Users\\uthma\\Dropbox (UFL)\\PhD\\Lab analysis result\\Blk9_CBlk_15-60\\cityblk9.csv"
pd.read_csv(wd)
data = pd.read_csv(wd)
## Block 9
data_blk9 =  data[(data["Soil Type"] == "B")]
depth = ["15-30","30-45","45-60"]

for i in range(3):
    dat = data_blk9[(data_blk9["Depth"] == depth[i])]
    t = np.array(dat['Time'])
    ymeas = np.array(dat['Ce_C0'])

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
    p0 = [0.05,0.5]

    # show initial objective
    print('Initial SSE Objective: ' + str(objective(p0)))

    # optimize a,f, and k
    solution = minimize(objective,p0)
    p = solution.x

    m = sum(ymeas)/float(len(ymeas))
    sst = sum((ymeas - m)**2)
    nse = 1 - solution.fun/sst

    # show final objective
    print('Final SSE Objective: ' + str(objective(p)))
    print('a: ' + str(p[0]))
    print('k: ' + str(p[1]))
    #print('f: ' + str(p[2]))
    print('RSquared: ' + str(nse))

    # calculate new ypred
    f0=0; m0=1; v0=1
    a0 = p[0]; k0 = p[1]
    t0 = np.linspace(t.min(),t.max(),300)
    r0=1+(m0/v0)*k0
    b0=(1+f0*(m0/v0)*k0)/r0
    ypred=1/r0+((1/(r0*b0)-1/r0)*np.exp(-(a0/b0)*t0))

    # plot results
    plt.figure()
    plt.plot(t0,ypred,'r-',linewidth=3,label='Predicted')
    plt.plot(t,ymeas,'ko',linewidth=2,label='Measured')
    plt.ylabel('Relative concentration')
    plt.xlabel('Time')
    plt.annotate('$R^2$ = ' + str(round(nse,3)), (20,0.95))
    plt.annotate('alpha = ' + str(round(a0,3)), (20,0.91))
    plt.annotate('$K_D$ = ' + str(round(k0,3)), (20,0.87))
    plt.annotate('$C_0$ = ' + str(round(9.448098,1)),  (20,0.82))
    plt.legend(loc='best')
    plt.savefig('optimized_blk9'+ str(depth[i]) +'.png',dpi=320)
    plt.show()

## City Block
data_cblk =  data[(data["Soil Type"] == "C")]
depth = ["15-30","30-45","45-60"]

for i in range(3):
    dat = data_cblk[(data_cblk["Depth"] == depth[i])]
    t = np.array(dat['Time'])
    ymeas = np.array(dat['Ce_C0'])

    ## m/v is 1
    # initial guesses for a, f, and k
    p0 = [0.05,0.5]

    # show initial objective
    print('Initial SSE Objective: ' + str(objective(p0)))

    # optimize a,f, and k
    solution = minimize(objective,p0)
    p = solution.x

    m = sum(ymeas)/float(len(ymeas))
    sst = sum((ymeas - m)**2)
    nse = 1 - solution.fun/sst

    # show final objective
    print('Final SSE Objective: ' + str(objective(p)))
    print('a: ' + str(p[0]))
    print('k: ' + str(p[1]))
    #print('f: ' + str(p[2]))
    print('RSquared: ' + str(nse))

    # calculate new ypred
    f0=0; m0=1; v0=1
    a0 = p[0]; k0 = p[1]
    t0 = np.linspace(t.min(),t.max(),300)
    r0=1+(m0/v0)*k0
    b0=(1+f0*(m0/v0)*k0)/r0
    ypred=1/r0+((1/(r0*b0)-1/r0)*np.exp(-(a0/b0)*t0))

    # plot results
    plt.figure()
    plt.plot(t0,ypred,'r-',linewidth=3,label='Predicted')
    plt.plot(t,ymeas,'ko',linewidth=2,label='Measured')
    plt.ylabel('Relative concentration')
    plt.xlabel('Time')
    plt.annotate('$R^2$ = ' + str(round(nse,3)), (20,0.95))
    plt.annotate('alpha = ' + str(round(a0,3)), (20,0.91))
    plt.annotate('$K_D$ = ' + str(round(k0,3)), (20,0.87))
    plt.annotate('$C_0$ = ' + str(round(9.448098,1)),  (20,0.82))
    plt.legend(loc='best')
    plt.savefig('optimized_cblk'+ str(depth[i]) +'.png',dpi=320)
    plt.show()