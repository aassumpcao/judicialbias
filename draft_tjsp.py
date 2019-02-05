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



# body = []
# for tr in table.find_all('tr')[1:]:
#     for td in tr.find_all('td'):
#         body.append(td.text)

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
tjsp.parser('sct10005702620188260361.html').parse_litigants()

# testing
tjsp.parser(files[0]).parse_litigants()
tjsp.parser(files[1]).parse_litigants()
tjsp.parser(files[2]).parse_litigants()

browser.quit()
