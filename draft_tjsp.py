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


#### replacements
#### self.number = number; self.browser = browser; self.urldec = urldec

# necessary objects for trial run but not for method
urldec = 'https://esaj.tjsp.jus.br/cpopg/open.do'
number = '03175302320098260100'
    
number = '10092683820178260011'

# search parameters
# store case number in class object
number = number

# navigate to page
browser.get(urldec)

# find text box to write case numbers
numberid = 'numeroDigitoAnoUnificado'

# click to search and check whether information has been loaded
sbtpath = '//*[(@id = "pbEnviar")]'
check   = '//*[(@class = "subtitle")]'

# try catch for cases that weren't found
try:
    # find 'case number' text box and send sct number
    numberbox = browser.find_element_by_id(numberid)
    numberbox.send_keys(str(number[0:13]))
    numberbox.send_keys(str(number[16:20]))
    numberbox.send_keys(Keys.TAB)

    # find submit button and click it to search cases
    browser.find_element_by_xpath(sbtpath).click()

    # count to wait for elements to be loaded
    counter = 1

    # while loop to test it for us
    while counter < 7:
        # check if elements are visible in first page and wait if not
        if len(browser.find_elements_by_xpath(check)) == 1:
            time.sleep(5)
            counter += 1
        else:
            break

    # save inner html to object
    html = browser.execute_script(java)

    # determine file names
    file = './sct' + str(number) + '.html'

    # save to file with correct encoding
    try:
        codecs.open(file, 'w', 'utf-8').write(html)
    except:
        codecs.open(file, 'w', 'cp1252').write(html)

    # return status
    return 'Your decision has been downloaded.'

# handle error
except:
    return 'Your decision search has failed.'

# quit
browser.quit()
exit()
