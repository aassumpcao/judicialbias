### tjsp sct case decision scraper
# this script downloads the case information for all small claims cases
#   involving politicians in the state court of são paulo since 2008.
#   each case information is downloaded to disk as an html file, which
#   then needs to be parsed into quantitative data.
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

# import third-party and local libraries
from selenium                          import webdriver
from selenium.common.exceptions        import StaleElementReferenceException
from selenium.common.exceptions        import TimeoutException
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.by      import By
from selenium.webdriver.common.keys    import Keys
from selenium.webdriver.support        import expected_conditions as EC
from selenium.webdriver.support.ui     import WebDriverWait
import feather
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
browser = webdriver.Chrome(executable_path = CHROMEDRIVER_PATH,
                           options = chrome_options)

# set implicit wait for page load
browser.implicitly_wait(10)

# import test dataset with all elected politicians
sct = pd.read_csv('data/sct.csv', index_col = 0, dtype = str)

# format case number dropping '-' and '.' and making it a list
key = [re.sub('-|\\.', '', i) for i in list(sct['caseID'])]

# prepare values for dictionary and build dictionary
value = list(sct['candidateID'])
cases = dict(zip(key, value))
limit = len(cases)

# create folder for html files
if not os.path.exists('./html'):
  os.mkdir('./html')

# change directory for download
os.chdir('./html')

# download sct cases
for i, (number, person) in enumerate(cases.items()):

    # run scraper
    nothing = tjsp.scraper(browser).decision(number)

    # list files in directory, get the latest file, and create new name
    files   = glob.glob('./*')
    file    = max(files, key = os.path.getctime)
    newname = file[:-5] + '-' + person + '.html'

    # assign new name
    os.rename(file, newname)

    # print warning every 10 iterations
    if (i + 1) % 10 == 0: print(str(i + 1) + ' / ' + str(limit))

# quit browser
browser.quit()
