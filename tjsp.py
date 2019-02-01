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
import math

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
    browser  = []
    url      = 'https://esaj.tjsp.jus.br/cjpg/'
    classSCT = 'Procedimento do Juizado Especial Cível'
    
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
    def sct_case(self, name):
        """method to narrow in on sct cases from list of cases"""
        
        # search parameters
        # store politician's name in class object
        self.name = name

        # navigate to page
        self.browser.get(self.url)

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
        regex0  = re.compile('(?<=de )[0-9]+', re.IGNORECASE)
        results = '//*[@class = "espacamentoCimaBaixo"]' + \
            '//*[contains(text(), "Resultados")]'
        
        # find next page button
        nextpage = '//*[@title = "Próxima página"]'

        # try catch for candidates who weren't found
        try:
            # find 'pesquisa livre' text box and send politician's name
            namebox = self.browser.find_element_by_id(nameid)
            namebox.send_keys(self.name)
            namebox.send_keys(Keys.TAB)

            # find 'classe' text box and send special civil tribunals id
            classbox = self.browser.find_element_by_id(classid)
            classbox.send_keys(self.classSCT)
            classbox.send_keys(Keys.TAB)

            # find submit button and click it to search cases
            time.sleep(.5)
            self.browser.find_element_by_xpath(sbtpath).click()

            # check if elements are visible in first page and wait if not
            time.sleep(2)
            # visible = EC.presence_of_element_located((By.XPATH, results))

            # extract total number of case hits
            total = self.browser.find_element_by_xpath(results).text
            total = re.search(regex0, total)[0]

            # total pages
            pages = math.ceil(int(total) / 10)

            # find all case numbers in first page and get them
            casenumbers = self.browser.find_elements_by_xpath(numbers)
            casenumbers = [x.text for x in casenumbers[:10]]

            # run different loops if there are multiple pages
            if pages > 1:
                # for loop to construct list of case numbers in other pages
                for i in range(1, pages + 1):
                    # click to advance pages
                    self.browser.find_element_by_xpath(nextpage).click()
                    time.sleep(1)
                    # get additional case numbers
                    extranumbers = self.browser.find_elements_by_xpath(numbers)
                    extranumbers = [x.text for x in extranumbers[:10]]
                    # extend case numbers list
                    casenumbers.extend(extranumbers)

            # wrangle data
            casenumbers = pd.DataFrame(casenumbers)
            casenumbers.columns = ['sctnumber']

        # handle error
        except:
            return 'There are no SCT cases for candidate ' + self.name + '.'

    # sct case decisions scraper function
    def decision(self, number):
        
        # search parameters
        # not available

        # return call
        return 'This method has not been developed yet'

# define parser class
class parser:
    """series of methods to parse case documents from tj-sp
    """
    # init method share by all class instances
    def __init__(self, file):
        
        # search parameters
        # not available

        # return call
        return 'This method has not been developed yet'
