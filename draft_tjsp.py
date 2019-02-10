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
import importlib
import tjsp
import feather
from bs4 import BeautifulSoup
# from selenium                          import webdriver
# from selenium.webdriver.chrome.options import Options
# from selenium.common.exceptions        import TimeoutException
# from selenium.common.exceptions        import StaleElementReferenceException
# from selenium.webdriver.common.by      import By
# from selenium.webdriver.common.keys    import Keys
# from selenium.webdriver.support.ui     import WebDriverWait
# from selenium.webdriver.support        import expected_conditions as EC

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
chrome_options.add_argument('--headless')
chrome_options.add_argument('--window-size=%s' % WINDOW_SIZE)
chrome_options.binary_location = CHROME_PATH

# open invisible browser
browser = webdriver.Chrome(executable_path = CHROMEDRIVER_PATH, \
                           options = chrome_options)

# set implicit wait for page load
browser.implicitly_wait(10)

# tests ok!
tjsp.scraper(browser).case('"Fernando Holiday"')
tjsp.scraper(browser).case('Nathan Jensen')
tjsp.scraper(browser).case('"marcelo assumpção"')

# tests ok!
tjsp.scraper(browser).decision('10092683820178260011')
tjsp.scraper(browser).decision('00054902620128260505')
tjsp.scraper(browser).decision('00085892420138260002')
tjsp.scraper(browser).decision('00085892420138260002')

# tests ok!
tjsp.parser(file).parse_summary()
tjsp.parser(file).parse_litigants()
tjsp.parser(file).parse_updates()
tjsp.parser(file).parse_petitions()
tjsp.parser(file).parse_hearings()

# testing
importlib.reload(tjsp)

file = 'sct10092683820178260011.html'
file = 'sct00085892420138260002.html'
file = 'sct10006570820188260320.html'

file = tjsp.parser(file)

soup = file.soup
globals()

# find petitions table
table = soup.find_all('table', {'style': 'margin-left:15px; margin-top:1px;'})[5]

# find text in each row
head = [row.text for row in table.find_all('th')]
body = [td.text for tr in table.find_all('tr')[1:] for td in tr.find_all('td')]

# clean up string
text = [re.sub(regex0, '', i) for i in body]
text = list(filter(regex8.search, text))
text = [i.strip() for i in text]

# created nested list of hearings and their names
text = [text[i:i + 4] for i in range(0, len(text), 4)]

# transform to pd dataset
text = pd.DataFrame(text)
text.columns = ['dates', 'hearing', 'status', 'attendees']

# problem for summary parser
'1000570-26.2018.8.26.0361'

randomlist = random.sample(list(lawsuits['caseID']), 3)

# test random list
randomlist = [re.sub('-|\\.', '', i) for i in randomlist]

# actual random list
randomlist = ['10326328020158260602', '00691248420118260002', '00056093620158260002']

tjsp.scraper(browser).decision(randomlist[0])
tjsp.scraper(browser).decision(randomlist[1])
tjsp.scraper(browser).decision(randomlist[2])
tjsp.scraper(browser).decision('10005702620188260361')

files = ['sct' + i + '.html' for i in randomlist]

# tests ok!
tjsp.parser(files[0]).parse_summary()
tjsp.parser(files[1]).parse_summary()
tjsp.parser(files[2]).parse_summary()


# testing
tjsp.parser(files[0]).parse_litigants()
tjsp.parser(files[1]).parse_litigants()
tjsp.parser(files[2]).parse_litigants()

browser.quit()

### TO-DO: fix parser when litigants don't have lawyers. Example below:
file0 = 'sct10005702620188260361.html' # working
file1 = 'sct00691248420118260002.html' # not working

tjsp.parser(file0).parse_litigants()
tjsp.parser(file1).parse_litigants()

# load data from different iterations
# data = [feather.read_dataframe(i) for i in glob.glob('./lawsuits*')]

# concatenate elements in data
# data = pd.concat(data)

# # write to disk
# feather.write_dataframe(data, 'lawsuits.feather')

import feather

# load data onto python session
lawsuits = feather.read_dataframe('lawsuits.feather')
candidateCPF = feather.read_dataframe('candidateCPF.feather')

# sort values by candidateID
lawsuits = lawsuits.sort_values('candidateID')

# filter only candidates who have SCT cases
lawsuits = lawsuits[lawsuits['caseID'] != 'N']

# drop duplicates
lawsuits = lawsuits.drop_duplicates()

# join on scraperID and pull CPF
lawsuits = pd.merge(lawsuits, candidateCPF, how = 'left', \
                    left_on = 'candidateID', right_on = 'scraperID')
# drop scraperID
lawsuits = lawsuits.drop('scraperID', axis = 1)
lawsuits.dtypes


import tjsp

file = tjsp.parser(files[3567])
file.soup

# find litigants table
table = file.soup.find('table', {'id': 'tablePartesPrincipais'})

# find text in each row
text = [row.text for row in \
        table.find_all('tr', {'class': 'fundoClaro'})]

# clean up string
text = [re.sub(regex0,' ', i) for i in text]
text = [re.sub(regex2, '', i) for i in text]
text = [re.sub(regex4,' ', i) for i in text]

# split variable names and contents.
text = [re.split(regex5, i) for i in text]

# flatten list, trim whitespace, replace ':', and delete empty strings
flat = [i for j in text for i in j]
flat = [i.strip() for i in flat]
flat = [re.sub(':', '', i) for i in flat]
flat = list(filter(regex6.search, flat))

# slice claimant and plaintiff info
indices   = [i for i, word in enumerate(flat) if re.search(regex9, word)]
claimant  = [flat[i:i + 2] for i in range(indices[0], indices[1], 2)]
plaintiff = [flat[i:i + 2] for i in range(indices[1], len(flat), 2)]

# flatten lists
claimant  = [i for j in claimant for i in j]
plaintiff = [i for j in plaintiff for i in j]

litigants = [claimant, plaintiff]

pd.DataFrame([claimant, plaintiff])





# transform to pd dataset
text = pd.DataFrame(text)

# return outcome if transpose is not provided as argument
if transpose == False:
    text.columns = ['parts', 'values']
    return pd.DataFrame(text)
else:
    text = text.T
    text.columns = text.iloc[0]
    return pd.DataFrame(text[1:])
