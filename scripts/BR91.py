# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import pandas as pd
import numpy as np
from matplotlib import pyplot as plt
# import seaborn as sns
import scipy.stats
import scipy.optimize
# sns.set()

pd.set_option('precision', 2)
pd.set_option('display.float_format', lambda x: '%.2f' % x)

filename = 'https://raw.githubusercontent.com/jmbejara/bfi-reu-2019/master/BresnahanAndReiss1991_DATA.csv'
df = pd.read_csv(filename)

df.head()

def V(df, Ni, alpha=np.ones(5), beta=np.ones(4)):
    """Per-capita variable profit
    
    df: data
    Ni: number of incumbents in market i
    alpha: alpha parameters
    beta: beta parameters
    """
    n = np.min([5, Ni])
#     n = Ni
    a = 0
    for i in range(1,n):
        a = a + alpha[i]
    
    
    Z = np.c_[df.ELD, df.PINC, df.LNHDD, df.FFRAC]
    # Don't use W in the V function for our replication
    # W = df.LANDV
    # X = np.c_[W, Z]

    # X is an I x 4 numpy array, where I is the number of observations
    X = np.c_[Z]
    
    
    profit = alpha[0] + X @ beta - a
    return profit

def F(df, Ni, gamma=np.ones(6)):
    n = np.min([5, Ni])
#     n = Ni
    g = 0
    # gamma 2 through gamma 5 (indices 1 through 4)
    for i in range(1,n):
        g = g + gamma[i]

    fixed_cost = gamma[0] + gamma[5] * df.LANDV + g
    return fixed_cost

def S_func(df, lam):
    S = (df.TPOP + lam[0] * df.OPOP + lam[1] * df.NGRW + 
         lam[2] * df.PGRW + lam[3] * df.OCTY)
    return S

def neg_log_lik(theta=np.ones(19), market='TIRE', df=None):
    lam = theta[0:4]
    beta = theta[4:8]
    alpha = theta[8:13]
    gamma = theta[13:19]
    
    Phi = scipy.stats.norm.cdf
    
    S = S_func(df, lam)
#     S = (df.TPOP + lam[0] * df.OPOP + lam[1] * df.NGRW + 
#          lam[2] * df.PGRW + lam[3] * df.OCTY)
    
    P = [0] * 6
    Pi_bar = lambda N: S * V(df, N, alpha=alpha, beta=beta) - F(df, N, gamma=gamma)
    P[0] = np.log( 1 - Phi(Pi_bar(0)) )
    P[5] = np.log( Phi(Pi_bar(5)) )
    for i in range(1,5):
        P[i] = np.log( Phi(Pi_bar(i)) - Phi(Pi_bar(i+1)) )

        
    for i in range(6):
        P[i][P[i] == -np.inf] = -100000000
    log_lik = 0 
    for i in range(5):
        log_lik = log_lik + np.sum(P[i] * (df[market] == i))
    log_lik = log_lik + np.sum(P[5] * (df[market] >= 5))
    return -log_lik


def theta_to_param_dict(theta):
    lam = theta[0:4]
    beta = theta[4:8]
    alpha = theta[8:13]
    gamma = theta[13:19]
    d = {'lam': lam, 'beta': beta, 'alpha': alpha, 'gamma': gamma, 'theta': theta}
    return d



theta0 = np.ones(19) * 0.1
neg_log_lik(df=df, theta=theta0)


ba = [-np.inf] * 8
bb = [0] * 10
lower_bounds = [*ba, *bb, -np.inf]
upper_bounds = [np.inf] * 19
bounds = list(zip(lower_bounds, upper_bounds))

theta0 = np.ones(19) * 0.1
nll = lambda theta: neg_log_lik(theta=theta, df=df)
out = scipy.optimize.minimize(nll, theta0, bounds=bounds, 
                              #options={'disp':True}
                             )

out
