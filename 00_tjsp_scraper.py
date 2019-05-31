### tjsp sct cases scraper
# this script downloads case numbers by ssn. we feed a database of ssn
#   to module 'tjsp' and search for cases in small claims courts in the
#   state of São Paulo for which people with these ssn were litigants.
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
import datetime

# import third-party and local libraries
from selenium                          import webdriver
from selenium.common.exceptions        import StaleElementReferenceException
from selenium.common.exceptions        import TimeoutException
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.by      import By
from selenium.webdriver.common.keys    import Keys
from selenium.webdriver.support        import expected_conditions as EC
from selenium.webdriver.support.ui     import WebDriverWait
import tjsp

# # reload module if necessary
# importlib.reload(tjsp)

# define chrome options
CHROME_PATH = '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'
WINDOW_SIZE = '1920,1080'
CHROMEDRIVER_PATH = '/usr/local/bin/chromedriver'

# set options
chrome_options = Options()
chrome_options.add_argument('--headless')
chrome_options.add_argument('--window-size=%s' % WINDOW_SIZE)
chrome_options.binary_location = CHROME_PATH

# open invisible browser
browser = webdriver.Chrome(executable_path = CHROMEDRIVER_PATH, \
                           options = chrome_options)

# set implicit wait for page load
browser.implicitly_wait(2)

# create empty list
cases = {'casenumber': [], 'litigant': []}

# import test dataset with all elected politicians
kwargs = {'index_col': 0, 'dtype': str}
candidates = pd.read_csv('data/candidatesUnique.csv', **kwargs)
candidates = list(candidates['candidate.ssn'])
limit = len(candidates)

# download case numbers
for i in range(limit):
    # scrape numbers, save to object, and make candid of same length
    case = tjsp.scraper(browser).cpf(candidates[i])
    # bind cases at the end of lawsuit dataset
    cases['casenumber'] += case['casenumber']
    cases['litigant']   += case['litigant']
    # print warning every 10 iterations
    if (i + 1) % 2000 == 0:
        print(str(i + 1) + ' / ' + str(limit) + '; ', datetime.datetime.now())

# quit loop at the end
browser.quit()

# create pandas dataset from list
pd.DataFrame(cases).to_csv('data/lawsuitsByCPF.csv', index = False)

