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
    urlcase  = 'https://esaj.tjsp.jus.br/cjpg/'
    urldec   = 'https://esaj.tjsp.jus.br/cpopg/open.do'
    classSCT = 'Procedimento do Juizado Especial Cível'
    java     = 'return document.getElementsByTagName("html")[0].innerHTML'
    
    # init method share by all class instances
    def __init__(self, browser):
        """load into class the browser"""
        # browser
        self.browser = browser

    # sct case number scraper function (special civil tribunals)
    def sct_case(self, name):
        """method to narrow in on sct cases from list of cases"""
        
        # search parameters
        # store politician's name in class object
        self.name = name

        # navigate to page
        self.browser.get(self.urlcase)

        # find text box to write politicians' name in and case class
        nameid  = 'iddadosConsulta.pesquisaLivre'
        classid = 'classe_selectionText'

        # click to search 
        sbtpath = '//*[(@id = "pbSubmit")]'

        # find search results
        search = '//*[contains(@class, "espacamentoCimaBaixo")]'

        # find total number of cases in page
        numbers = '//*[contains(@class, "fonteNegrito")]'

        # fint total number of cases in all pages and extract it
        regex0 = re.compile('(?<=de )[0-9]+', re.IGNORECASE)
        regex1 = re.compile('[0-9]+', re.IGNORECASE)
        
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
            time.sleep(1)
            self.browser.find_element_by_xpath(sbtpath).click()

            # check if elements are visible in first page and wait if not
            time.sleep(2)

            # check for error
            searched = self.browser.find_element_by_xpath(search).text

            # extract total number of case hits
            total = re.search(regex0, searched)

            # exit if no cases are found
            if total == None: return 'No cases found for ' + self.name + '.'       
            
            # total pages
            total = re.search(regex0, searched)[0]
            pages = math.ceil(int(total) / 10)

            # find all case numbers in first page and get them
            casenumbers = self.browser.find_elements_by_xpath(numbers)
            casenumbers = [x.text for x in casenumbers[:10]]
            
            # continue if there are matches
            # run different loops if there are multiple pages
            if pages > 1:
                # for loop to construct list of case numbers in other pages
                for i in range(pages - 1):
                    if not i == pages - 1:
                        # click to advance pages
                        self.browser.find_element_by_xpath(nextpage).click()
                    time.sleep(1)
                    # get additional case numbers
                    extranumbers = self.browser.find_elements_by_xpath(numbers)
                    extranumbers = [x.text for x in extranumbers[:10]]
                    # extend case numbers list
                    casenumbers.extend(extranumbers)
                    # break out in last iteration

            # return outcome
            return casenumbers

        # handle error
        except:
            return 'There were errors when finding SCT cases for candidate ' \
                + self.name + '.'

    # case number scraper function
    def case(self, name):
        """method to download any case number by candidate information"""
        
        # search parameters
        # not available

        # return call
        return 'This method has not been developed yet'

    # sct case decisions scraper function
    def decision(self, number):
        """method to download case decisions by candidate information"""
        
        # search parameters
        # store case number in class object
        self.number = number

        # navigate to page
        self.browser.get(self.urldec)

        # find text box to write case numbers
        numberid = 'numeroDigitoAnoUnificado'

        # click to search and check whether information has been loaded
        sbtpath = '//*[(@id = "pbEnviar")]'
        check   = '//*[(@class = "subtitle")]'

        # try catch for cases that weren't found
        try:
            # find 'case number' text box and send sct number
            numberbox = self.browser.find_element_by_id(numberid)
            numberbox.send_keys(str(self.number[0:13]))
            numberbox.send_keys(str(self.number[16:20]))
            numberbox.send_keys(Keys.TAB)

            # find submit button and click it to search cases
            self.browser.find_element_by_xpath(sbtpath).click()

            # count to wait for elements to be loaded
            counter = 1

            # while loop to test it for us
            while counter < 7:
                # check if elements are visible in first page and wait if not
                if len(self.browser.find_elements_by_xpath(check)) == 1:
                    time.sleep(5)
                    counter += 1
                else:
                    break

            # save inner html to object
            html = self.browser.execute_script(self.java)

            # determine file names
            file = './sct' + str(self.number) + '.html'

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
