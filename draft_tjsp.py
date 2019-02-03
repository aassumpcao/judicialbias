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
browser.implicitly_wait(60)

chrome_options = Options()
chrome_options.binary_location = CHROME_PATH
# open invisible browser
browser = webdriver.Chrome(executable_path = CHROMEDRIVER_PATH)
# set implicit wait for page load

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
tjsp.parser('sct10092683820178260011.html').parse_summary()
tjsp.parser('sct00085892420138260002.html').parse_summary()
tjsp.parser('sct10006570820188260320.html').parse_summary()

# testing
tjsp.parser(file).parse_summary()
tjsp.parser(file).parse_litigants()


import tjsp
import importlib
importlib.reload(tjsp)

browser.quit()
exit()

file = 'sct10092683820178260011.html'

file = tjsp.parser(file)

soup = file.soup

# find litigants table
table = soup.find('table', {'id': 'tablePartesPrincipais'})

# find text in reach row
text = [row.text for row in table.find_all('tr', {'class': 'fundoClaro'})]

# clean up string
text = [re.sub(regex0,' ', i) for i in text]
text = [re.sub(regex2, '', i) for i in text]
text = [re.sub(regex4,' ', i) for i in text]

# split variable names and contents. then, flatten list
text = [re.split(regex5, i) for i in text]

# flatten list, trim whitespace, replace ':', and delete empty strings
flat = [i for j in text for i in j]
flat = [i.strip() for i in flat]
flat = [re.sub(':', '', i) for i in flat]
flat = list(filter(regex6.search, flat))

# created nested list of litigant categories and their names
text = [flat[i:i+2] for i in range(0, len(flat), 2)]

# transform to pd dataset
text = pd.DataFrame(text)

# return outcome if transpose is not provided as argument
if transpose == False:
    text.columns = ['litigantType', 'values']
    return pd.DataFrame(text)
else:
    text = text.T
    text.columns = text.iloc[0]
    return pd.DataFrame(text[1:])




