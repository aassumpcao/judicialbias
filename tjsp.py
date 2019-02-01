### tj-sp classes and methods
# developed by:
# Andre Assumpcao
# andre.assumpcao@gmail.com

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

# define scraper class
class scraper:
    """series of methods to download case documents from tj-sp
        
    attributes:

    methods:
        case:
        sct_case:
        decision:

    """

    # initial arguments (esaj website and browser)
    browser = []
    url     = 'https://esaj.tjsp.jus.br/cjpg/'

    # init method share by all class instances
    def __init__(self, browser):
        """load into class the browser"""
        # browser
        self.browser = browser

    # case number scraper function
    def case(self, name):
        """method to download case numbers by candidate information"""
        # search parameters
        # not available

        # return call
        return 'This method has not been developed yet'

    # sct case number scraper function (special civil tribunals)
    def sct_case(self, cases):
        """method to narrow in on sct cases from list of cases"""
        # search parameters
        

        # navigate to self.page
        self.browser.get(self.page)

        

    # sct case decisions scraper function
    def decision(self, number):

# define parser class
class parser:
    """series of methods to parse case documents from tj-sp


    """

    # init method share by all class instances
    def __init__(self, file):
