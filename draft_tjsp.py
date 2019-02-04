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
tjsp.parser(file).parse_summary()
tjsp.parser(file).parse_litigants()
tjsp.parser(file).parse_updates()

# testing


import tjsp
import importlib
importlib.reload(tjsp)

browser.quit()
exit()

file = 'sct10092683820178260011.html'
file = 'sct00085892420138260002.html'
file = 'sct10006570820188260320.html'

file = tjsp.parser(file)

soup = file.soup



# find updates table
table = soup.find('tbody', {'id': 'tabelaTodasMovimentacoes'})

# find text in each row
text = [row.text for row in table.find_all('tr', {'style': ''})]

# clean up string
text = [re.sub(regex0, '', i) for i in text]
text = [re.sub(regex2, '', i) for i in text]
text = [re.sub(regex4,' ', i) for i in text]

# split variables, flatten list, trim whitespace, and extract unique values
text = [re.split(regex7, i, maxsplit = 1) for i in text]
flat = [i for j in text for i in j]
flat = [i.strip() for i in flat]
flat = list(filter(regex8.search, flat))

# created nested list of litigant categories and their names
text = [flat[i:i + 2] for i in range(0, len(flat), 2)]

# transform to pd dataset
text = pd.DataFrame(text)
text.columns = ['updates', 'values']

# return outcome 
return text






