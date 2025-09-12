# -*- coding: utf-8 -*-
"""
Created on Fri Sep 12 13:24:45 2025

@author: jhuba
"""


import pandas as pd
import numpy as np
from climate_indices import indices, compute
from scipy.stats import skew, kurtosis
import matplotlib.pyplot as plt

# === Load and prepare data ===
df = pd.read_csv(r"weather.csv")
df['date'] = pd.to_datetime(df['date'], format="%m/%d/%Y")
df['doy'] = df['date'].dt.dayofyear

# === climate statistics
clim_stats = df.groupby('doy')['prec'].agg(['mean', 'std']).reset_index().rename(columns={'mean':'mu', 'std':'sigma'})
df = pd.merge(df, clim_stats, on='doy', how='left')

# === Calculate SPA and SAPI ===
df['SPA'] = (df['prec'] - df['mu']) / df['sigma']
df['SAPI_raw'] = df['prec'].rolling(window=44, min_periods=1).sum()
SAPI_mean = df['SAPI_raw'].mean()
SAPI_sd = df['SAPI_raw'].std()
df['SAPI'] = (df['SAPI_raw'] - SAPI_mean) / SAPI_sd

# === DWAAI calculation function ===
def compute_dwaai(df, index, pre_days=44, pro_days=10, alpha=1.3):
    if index - pre_days < 0 or index + pro_days >= len(df):
        return np.nan

    pre_period = df.iloc[(index - pre_days):index]
    pro_period = df.iloc[(index + 1):(index + 1 + pro_days)]

    SPA_pre = pre_period['SPA'].mean()
    SPA_pro = pro_period['SPA'].mean()
    SAPI_0 = df.iloc[index]['SAPI']

    K = 0
    for i in range(1, pro_days + 1):
        SAPI_i = df.iloc[index + i]['SAPI']
        if pd.isna(SAPI_i):
            continue
        K += (SAPI_i - SAPI_0) / i

    term1 = (SPA_pro - SPA_pre) * (abs(SPA_pre) + abs(SPA_pro))
    weight = alpha ** (-abs(SPA_pre + SPA_pro))

    DWAAI = (K + term1) * weight
    return DWAAI