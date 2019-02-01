# python
# import statements
import codecs
import glob
import pandas as pd
import re
import os
from bs4 import BeautifulSoup
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.common.exceptions        import TimeoutException
from selenium.common.exceptions        import StaleElementReferenceException
from selenium.webdriver.common.by      import By
from selenium.webdriver.common.keys    import Keys
from selenium.webdriver.support.ui     import WebDriverWait
from selenium.webdriver.support        import expected_conditions as EC
import numpy as np
import time
import re
import math

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
from tjsp import *

scraper(browser).sct_case('Fernando Haddad')
scraper(browser).sct_case('Nathan Jensen')

### methods code
numbers = '//*[contains(concat( " ", @class, " " ),' + \
            ' concat( " ", "fonteNegrito", " " ))]'

error = '//*[(@class = "aviso espacamentoCimaBaixo' + \
            ' centralizado fonteNegrito")]'

search = '//*[contains(@class, "espacamentoCimaBaixo")]'

searched = browser.find_element_by_xpath(search).text

total = re.search(regex0, searched)
error = re.search(regex1, searched)

a = re.search(regex1, browser.find_element_by_xpath(error).text)


name = 'Nathan Jensen'
regex0 = re.compile('(?<=de )[0-9]+', re.IGNORECASE)
regex1 = re.compile('[0-9]+', re.IGNORECASE)

total = browser.find_element_by_xpath(results).text

re.search(regex1, 'Não foi encontrado nenhum resultado correspondente à busca realizada.') == None

casenumbers = browser.find_elements_by_xpath(numbers)
casenumbers = [x.text for x in casenumbers[:10]]

if re.search(regex1, casenumbers[0]) == None:
    'There are no SCT cases for candidate ' + name + '.'

browser.quit()
exit()
