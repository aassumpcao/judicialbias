### tjsp sct case decision parser
# this script parses the case information for all small claims cases
#   involving politicians in the state court of são paulo since 2008.
#   each case information is loaded onto python as an html file and
#   parsed using module 'tjsp' developed by me.
# developed by:
# andre assumpcao
# andre.assumpcao@gmail.com

# import standard libraries
import codecs
import glob
import importlib
import math
import numpy as np
import os
import pandas as pd
import re
import time
import random

# import third-party and local libraries
import tjsp
from bs4 import BeautifulSoup

# change directory for loading files
os.chdir('./html')

# extract list of cases from folder 'html'
files = glob.glob('*')

# import litigant problems
litigantProblems = [
    'sct00003571920178260346-7726.html',
    'sct00019782220158260346-3452.html',
    'sct00554541420118260346-17348.html',
    'sct00037947720138260356-11928.html',
    'sct01040081420108260346-19013.html',
    'sct00003477220178260346-7726.html',
    'sct00003607120178260346-7726.html',
    'sct00546804720128260346-3452.html'
]

# check for files that don't have litigant problems
files = list(set(files) - set(litigantProblems))

# set length of list
limit = len(files)

# extract case and person identified
cases  = [file[3:23] for file in files]
people = [re.search('(?<=-)(.)*(?=\\.html)', file)[0] for file in files]

# empty list for lawsuit summaries and errors
lawsuits = []
errors = []

# build case summary list
for i, file in zip(range(limit), files):
    # process cases
    try:
        # load each file
        lawsuit = tjsp.parser(file).parse_summary(transpose = True)
        # append to lawsuits list
        lawsuits.append(lawsuit)
    # create list of errors
    except:
        errors.append(file)
    # print warning every 100 iterations
    if (i + 1) % 100 == 0: print(str(i + 1) + ' / ' + str(limit))

# # errors
# len(errors)
# errors

# manual fix of problems
#   1. download:
#      the case number used to find case documentation was not unique.
#      i manually searched for cases and downloaded the right case html:
#          'sct10014244420168260505-10649.html'
#          'sct00083531520148260624-6442.html'
#          'sct00024131520158260372-11272.html'
#
#   2. confidential:
#      cases for which information is not fully available. i had to
#      manually input values for these cases:
#          'sct10021558720178260575-10236.html'
#          'sct10138212520168260477-4820.html'
#          'sct10000988820158260180-11695.html'
#
#   3. delete:
#      miscoded cases that should not be here
#          'sct00048942420108260372-11272.html'
#          'sct00037792620148260372-11272.html'
#          'sct00041193320158260372-11272.html'

# build dataset from list
lawsuits = pd.concat(lawsuits, ignore_index = True, sort = False)

# merge caseID and candidateID
lawsuits['caseID'] = cases
lawsuits['candidateID'] = people

# drop useless columns
droplist = list(lawsuits.columns)
droplist = [droplist[i] for i in [0, 1, 2, 4, 7, 9] + list(range(11, 17))]
sctinfo  = lawsuits.drop(droplist, axis = 1)

# rename columns
rename = ['subject', 'other', 'assignment', 'judge', 'claim', 'caseID',
          'candidateID']
sctinfo.columns = rename

# empty lists for litigants and errors
litigants = []
errors = []

# build case summary list
for i, file in zip(range(limit), files):
    # process litigants
    try:
        # load each file
        litigant = tjsp.parser(file).parse_litigants(transpose = True)
        # append cases and candidate information
        litigant['caseID'] = cases[i]
        litigant['candidateID'] = people[i]
        # append to litigants list
        litigants.append(litigant)
    # create list of errors
    except:
        errors.append(file)
    # print warning every 100 iterations
    if (i + 1) % 100 == 0: print(str(i + 1) + ' / ' + str(limit))

# check errors
len(errors)

# # write to file
# codecs.open('litigantsProblems.txt', 'w', 'utf-8').write(errors)

# build dataset from list
litigants = pd.concat(litigants, ignore_index = True, sort = False)
