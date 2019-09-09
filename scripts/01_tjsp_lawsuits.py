### tj-sp cases wrangling
# this script joins case, judicial district, and politician data toge-
#   the and the goal is to filter down the list of lawsuits to lawsuits
#   filed in the municipality in which politicians have been elected to
#   office.
# developed by:
# andre assumpcao
# andre.assumpcao@gmail.com

# import standard libraries
import codecs
import re
import time
import glob
import importlib
import math
import numpy as np
import os
import pandas as pd

# import third-party and local libraries
import feather
import tjsp
from bs4 import BeautifulSoup

# load data onto python session
district = pd.read_csv('data/comarcas_sp.csv')
ibgetse  = pd.read_csv('data/ibge_tse.csv')
lawsuits = feather.read_dataframe('data/lawsuits.feather')
candidateCPF = feather.read_dataframe('data/candidateCPF.feather')

# join ibge, tse, and judicial district information
# first, reduce ibge number from seven to six digits
district['muni_code'] = district['muni_code'].astype('str').str.slice(0, 6)
ibgetse['ibge'] = ibgetse['ibge'].astype('str')

# next, join district, ibge, and tse individual identifiers
muncode = pd.merge(district, ibgetse, how = 'left',
                   left_on = 'muni_code', right_on = 'ibge')

# drop all useless vars (non-ids)
muncode = muncode.drop(list(muncode.columns)[0:5], axis = 1)
muncode = muncode.rename(columns = {'comarca_codetj': 'tj'})

# check rows containing null for judicial district and return rows
pos = muncode.index[muncode['tj'].isnull()].tolist()

# determine district values to replace the nulls and replace them
values = [77, 77, 73, 77, 529, 77]
muncode.loc[pos, 'tj'] = values

# replace values
muncode['tj'] = muncode['tj'].astype('int')
muncode['tj'] = muncode['tj'].astype('str').str.pad(4, fillchar = '0')
muncode['tse']= muncode['tse'].astype('str')

# 1. sort values by candidateID, 2. filter candidates with SCT cases,
# 3. drop duplicates, 4. join with CPF dataset, 5. drop scraperID
lawsuits = lawsuits.sort_values('candidateID')
lawsuits = lawsuits[lawsuits['caseID'] != 'N']
lawsuits = lawsuits.drop_duplicates()
lawsuits = pd.merge(lawsuits, candidateCPF, how = 'left',
                    left_on = 'candidateID', right_on = 'scraperID')
lawsuits = lawsuits.drop('scraperID', 1)

# rename tse id and strip tj id for join on same columns in muncode
lawsuits = lawsuits.rename(columns = {'munID': 'tse'})
lawsuits['tj'] = lawsuits['caseID'].str[-4:]

# join on tse and tj id and return the dataset with filtered cases for
# politicians who were challenged in court at their electoral district
sct = pd.merge(lawsuits, muncode, 'left', ['tj', 'tse'], indicator = True)
sct = sct[sct['_merge'] == 'both'].drop('_merge', 1)

# save dataset to file
sct.reset_index(drop = True).to_csv('data/sct.csv')
