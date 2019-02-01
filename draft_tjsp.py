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
# chrome_options.add_argument('--headless')
# chrome_options.add_argument('--window-size=%s' % WINDOW_SIZE)
chrome_options.binary_location = CHROME_PATH

# open invisible browser
browser = webdriver.Chrome(executable_path = CHROMEDRIVER_PATH)

# # open invisible browser
# browser = webdriver.Chrome(executable_path = CHROMEDRIVER_PATH,
#                            chrome_options  = chrome_options)

# set implicit wait for page load
browser.implicitly_wait(60)

from tjsp import *

scraper(browser).sct_case('Paulo Maluf')

### methods code
################################# every url entry should become self.url
################################# every browser entry should become self.browser
url      = 'https://esaj.tjsp.jus.br/cjpg/'
classSCT = 'Procedimento do Juizado Especial Cível'


# store politician's name
name = 'Fernando Haddad'

# store politician's name in class object
name = 'Paulo Maluf'

# navigate to page
browser.get(url)

# search parameters
# find text box to write politicians' name in and case class
nameid  = 'iddadosConsulta.pesquisaLivre'
classid = 'classe_selectionText'

# click to search 
sbtpath = '//*[(@id = "pbSubmit")]'

# find total number of cases in page
numbers = '//*[contains(concat( " ", @class, " " ),' + \
    ' concat( " ", "fonteNegrito", " " ))]'

# fint total number of cases in all pages and extract it
results = '//*[@class = "espacamentoCimaBaixo"]' + \
    '//*[contains(text(), "Resultados")]'
regex0  = re.compile('(?<=de )[0-9]+', re.IGNORECASE)
regex1  = re.compile('[0-9]+(?= a)', re.IGNORECASE)

# find next page button
nextpage = '//*[@title = "Próxima página"]'

# find 'pesquisa livre' text box and send politician's name
browser.find_element_by_id(nameid).send_keys(name)

# find 'classe' text box and send special civil tribunals id
WebDriverWait(browser, .5)
sctclass = browser.find_element_by_id(classid)
sctclass.send_keys(classSCT)
sctclass.send_keys(Keys.TAB)

# find submit button and click it to search cases
browser.find_element_by_xpath(sbtpath).click()

# check if elements are visible in first page and wait if not
visible = EC.presence_of_element_located((By.XPATH, results))

# extract total number of case hits
total = browser.find_element_by_xpath(results).text
total = re.search(regex0, total)[0]

# total pages
pages = math.ceil(int(total) / 10)

# find all case numbers in first page and get them
casenumbers = browser.find_elements_by_xpath(numbers)
casenumbers = [x.text for x in casenumbers[:10]]

# run different loops if there are multiple pages
if pages > 1:
    # for loop to construct list of case numbers in other pages
    for i in range(1, pages):
        # click to advance pages
        browser.find_element_by_xpath(nextpage).click()
        time.sleep(1)
        # get additional case numbers
        extranumbers = browser.find_elements_by_xpath(numbers)
        extranumbers = [x.text for x in extranumbers[:10]]
        # extend case numbers list
        casenumbers.extend(extranumbers)
