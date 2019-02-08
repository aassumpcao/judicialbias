### tjsp sct cases scraper
# developed by:
# andre assumpcao
# andre.assumpcao@gmail.com

# import statements
from selenium                          import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.common.exceptions        import TimeoutException
from selenium.common.exceptions        import StaleElementReferenceException
from selenium.webdriver.common.by      import By
from selenium.webdriver.common.keys    import Keys
from selenium.webdriver.support.ui     import WebDriverWait
from selenium.webdriver.support        import expected_conditions as EC
import codecs
import feather
import glob
import math
import numpy as np
import os
import pandas as pd
import re
import time
import tjsp # import scraper module
import importlib

# # reload module if necessary
importlib.reload(tjsp)

# define chrome options
CHROME_PATH      ='/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'
CHROMEDRIVER_PATH='/usr/local/bin/chromedriver'
WINDOW_SIZE      ='1920,1080'

# set options
chrome_options = Options()
chrome_options.add_argument('--headless')
chrome_options.add_argument('--window-size=%s' % WINDOW_SIZE)
chrome_options.binary_location = CHROME_PATH

# open invisible browser
browser = webdriver.Chrome(executable_path = CHROMEDRIVER_PATH, \
                           options = chrome_options)

# set implicit wait for page load
browser.implicitly_wait(10)

# create empty list
lawsuit = []

# import test dataset with all elected politicians
candidates = feather.read_dataframe('candidatesUnique.feather')

# download case numbers
for x in range(len(candidates)):

    # define search
    name   = '\"' + candidates.iloc[x, 0] + '\"'
    candid = candidates.iloc[x, 1]

    # scrape numbers, save to object, and make candid of same length
    case   = tjsp.scraper(browser).case(name)
    candid = [candid] * len(case) if not isinstance(case, str) else [candid]

    # join cases and candidate ids together
    cases = list(zip(case, candid))

    # bind at the end of lawsuit dataset
    lawsuit.extend(cases)

    # print warning every 10 iterations
    if (x + 1) % 10 == 0: print(str(x + 1) + ' / ' + str(len(candidates)))

# quit loop at the end
browser.quit()

# create pandas dataset from list
lawsuits = pd.DataFrame(lawsuit)
lawsuits.columns = ['caseID', 'candidateID']

# sort values by candidateID
lawsuits = lawsuits.sort_values('candidateID')

# filter only candidates who have SCT cases
lawsuits = lawsuits[lawsuits['caseID'] != 'N']

# drop duplicates
lawsuits = lawsuits.drop_duplicates()

# save to disk
feather.write_dataframe(lawsuits, 'lawsuits1.feather')


