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
browser.implicitly_wait(10)

### methods code
################################# every url entry should become self.url
################################# every browser entry should become self.browser

# provide e-saj url
url = 'https://esaj.tjsp.jus.br/cjpg/'

# navigate to page
browser.get(url)

# find box to write politicians' name down
nameid  = 'iddadosConsulta.pesquisaLivre'
sbtpath = '//*[(@id = "pbSubmit")]'

# find 'pesquisa livre' box element in the webpage
namebox = browser.find_element_by_id('iddadosConsulta.pesquisaLivre')

# send politician's name
namebox.send_keys('Fernando E Holiday')



namebox.send_keys('Fernando E Holiday')

