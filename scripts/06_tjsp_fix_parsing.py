### tjsp fixing parsing errors
#  this script fixes parsing errors from earlier scripts
# developed by:
# andre assumpcao
# andre.assumpcao@gmail.com

# import statements
import pandas as pd
import os, re

# define args and load csv
kwargs = {'dtype': str, 'index_col': 0}
summary = pd.read_csv('data/sctSummary_random.csv', **kwargs, header = None)

# pull column names out, reset index, and drop invalid column
columns = summary.iloc[0].to_list()[:10]
summary = summary.reset_index(drop = True)
summary.drop(0, axis = 0, inplace = True)

# filter sct cases only
summary = summary[summary.iloc[:,1]=='Procedimento do Juizado Especial Cível']
summary = summary.iloc[:,0:10].reset_index(drop = True)
summary.columns = columns

# save to disk
summary.to_csv('data/sctSummary_random.csv', index = False)

# drop invalidate lawsuits from litigants data
cnjs = summary['Processo'].to_list()
litigants = pd.read_csv('data/sctLitigants_random.csv', **kwargs)
litigants = litigants[litigants['Processo'].isin(cnjs)].reset_index(drop=True)

# save to disk
litigants.to_csv('data/sctLitigants_random.csv', index = False)
