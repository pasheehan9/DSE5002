# -*- coding: utf-8 -*-
"""
Created on Sat Oct  1 23:22:20 2022

@author: jlowh
"""

from statsmodels.datasets.fertility.data import load_pandas
import numpy as np
df = load_pandas().data
df.columns
np.round(np.mean(df['1963']),2)


from statsmodels.datasets.interest_inflation.data import load_pandas
df = load_pandas().data
df.head()

import statsmodels.api as sm
import numpy as np
df
Y = df['Dp']
X = df['R']
x = sm.add_constant(X)
model = sm.OLS(Y,X)
results = model.fit()
results.summary()

import statsmodels.api as sm
import statsmodels.formula.api as smf
from statsmodels.datasets.interest_inflation import load_pandas
df = load_pandas().data
model = smf.quantreg('df ~ R',df)
res = model.fit(q=.5)

res2_coefs = res.params

print(res2_coefs)

print(results.summary())
