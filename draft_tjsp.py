python3.7
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

# tests ok!
scraper(browser).sct_case('Fernando Haddad')
scraper(browser).sct_case('Nathan Jensen')
scraper(browser).sct_case('"marcelo assumpção"')

# tests ok!
scraper(browser).decision('10006570820188260320')
scraper(browser).decision('00054902620128260505')
scraper(browser).decision('00085892420138260002')
scraper(browser).decision('00085892420138260002')


browser.quit()
exit()
