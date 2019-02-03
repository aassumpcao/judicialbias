python3.7
# import statements
import codecs
import glob
import pandas as pd
import re
import os
import numpy as np
import time
import re
import math
from bs4 import BeautifulSoup
from selenium                          import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.common.exceptions        import TimeoutException
from selenium.common.exceptions        import StaleElementReferenceException
from selenium.webdriver.common.by      import By
from selenium.webdriver.common.keys    import Keys
from selenium.webdriver.support.ui     import WebDriverWait
from selenium.webdriver.support        import expected_conditions as EC


# initial options
# set working dir
os.chdir('/Users/aassumpcao/OneDrive - University of North Carolina ' +
  'at Chapel Hill/Documents/Research/2020 Dissertation/2019 Judicial Bias')

# define chrome options
CHROME_PATH      ='/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'
CHROMEDRIVER_PATH='/usr/local/bin/chromedriver'
WINDOW_SIZE      ='1920,1080'

# set options
chrome_options = Options()
chrome_options.binary_location = CHROME_PATH
# open invisible browser
browser = webdriver.Chrome(executable_path = CHROMEDRIVER_PATH)
# set implicit wait for page load
browser.implicitly_wait(60)

# tests ok!
tjsp.scraper(browser).sct_case('Fernando Haddad')
tjsp.scraper(browser).sct_case('Nathan Jensen')
tjsp.scraper(browser).sct_case('"marcelo assumpção"')

# tests ok!
tjsp.scraper(browser).decision('10006570820188260320')
tjsp.scraper(browser).decision('00054902620128260505')
tjsp.scraper(browser).decision('00085892420138260002')
tjsp.scraper(browser).decision('00085892420138260002')

import tjsp
import imp
imp.reload(tjsp)

browser.quit()
exit()

file = 'sct00054902620128260505.html'

file = tjsp.parser(file)

soup = file.soup

# define static variables for finding tables
terms = re.compile('( )+(Dados do processo)|(Partes do processo)|' +
    '(Movimentações)|(Petições diversas)|(Incidentes, ações)|' + 
    '(Apensos, Entranhados)|(Audiências)|(Histórico de classes)')

# define regex for substituting weird characters in all tables
regex0 = re.compile(r'\n|\t')
regex1 = re.compile(r'\\n|\\t')
regex2 = re.compile(r'\\xa0')

### parse_summary(self, transpose = False):
"""method to wrangle summary information"""

### initial objects for parser
# find summary table
summary = soup.find_all('table', {'class': 'secaoFormBody'})[1]

# extract text from table
thead = [label.text for label in \
         summary.find_all('label', {'class': 'labelClass'})]

# extract index from table
ihead = [i for i in range(len(summary.find_all('tr', {'class': ''}))) \
         if not summary.find_all('tr', {'class': ''})[i].find('label', \
         {'class': 'labelClass'}) == None]

# next step is to reduce the dimensions of the variable values to match
# the number of names we have
###### See parse_details tse
summary.find_all('tr', {'class': ''})
