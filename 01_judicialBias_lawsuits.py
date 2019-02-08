### tj-sp wrangling cases
# developed by
# Andre Assumpcao
# andre.assumpcao@gmail.com

# import statements
import codecs
import feather
import glob
import importlib
import math
import numpy as np
import os
import pandas as pd
import re
import time
import tjsp
from bs4 import BeautifulSoup

# load data onto python session
district = pd.read_csv('comarcas_sp.csv')
ibgetse  = pd.read_csv('ibge_tse.csv')
lawsuits = feather.read_dataframe('lawsuits.feather')
candidateCPF = feather.read_dataframe('candidateCPF.feather')

# join ibge, tse, and judicial district information
# first, reduce ibge number from seven to six digits
district['muni_code'] = district['muni_code'].astype('str').str.slice(0, 6)
ibgetse['ibge'] = ibgetse['ibge'].astype('str')

# next, district, ibge, and tse individual identifiers
muncode = pd.merge(district, ibgetse, how = 'left',
                  left_on = 'muni_code', right_on = 'ibge')

# drop all useless vars (non-ids)
muncode = muncode.drop(list(muncode.columns)[0:5], axis = 1)
muncode = muncode.rename(columns = {'comarca_codetj': 'tj'})

# check rows containing null for judicial district and return rows
pos = muncode.index[muncode['tj'].isnull()].tolist()

# determine district values to replace the nulls
values = [77, 77, 73, 77, 529, 77]
muncode.loc[pos, 'tj'] = values

# replace values
muncode['tj'] = muncode['tj'].astype('int')
muncode['tj'] = muncode['tj'].astype('str').str.pad(4, fillchar = '0')

# 1. sort values by candidateID, 2. filter candidates with SCT cases,
# 3. drop duplicates, 4. join with CPF dataset, 5. drop scraperID
lawsuits = lawsuits.sort_values('candidateID')
lawsuits = lawsuits[lawsuits['caseID'] != 'N']
lawsuits = lawsuits.drop_duplicates()
lawsuits = pd.merge(lawsuits, candidateCPF, how = 'left', \
                    left_on = 'candidateID', right_on = 'scraperID')
lawsuits = lawsuits.drop('scraperID', axis = 1)



