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
        browser:    placeholder for selenium browser call
    
    methods:
        case:       download case number by name (only SCT class for now)
        decision:   use case number to download judicial decision
    
    """
    # initial arguments (for both esaj website and browser)
    browser  = []
    urlcase  = 'https://esaj.tjsp.jus.br/cjpg/'
    urldec   = 'https://esaj.tjsp.jus.br/cpopg/open.do'
    classSCT = 'Procedimento do Juizado Especial Cível'
    java     = 'return document.getElementsByTagName("html")[0].innerHTML'
    
    # init method shared by all class instances
    def __init__(self, browser):
        """load into class the browser"""
        
        # store browser info
        self.browser = browser

    # case number scraper function (only available for special civil tribunals)
    def case(self, name):
        """method to narrow in on sct cases from list of cases"""
        
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
            time.sleep(.5)
            self.browser.find_element_by_xpath(sbtpath).click()

            # force wait before information is loaded
            time.sleep(2)

            # return the text containing the results of the search
            searched = self.browser.find_element_by_xpath(search).text

            # confirm the number of cases found
            total = re.search(regex0, searched)

            # exit if no cases are found
            if total == None: return 'Nothing found'       
            
            # else determine the number of pages containing all cases
            total = re.search(regex0, searched)[0]
            pages = math.ceil(int(total) / 10)

            # find all individual case numbers in first page and extract them
            casenumbers = self.browser.find_elements_by_xpath(numbers)
            casenumbers = [x.text for x in casenumbers[:10]]
            
            # run loop if there are multiple pages
            if pages > 1:
                # for loop to construct list of case numbers in other pages
                for i in range(pages - 1):
                    if not i == pages - 1:
                        # click to advance pages except for last page
                        self.browser.find_element_by_xpath(nextpage).click()
                    time.sleep(1)
                    # get additional case numbers
                    extranumbers = self.browser.find_elements_by_xpath(numbers)
                    extranumbers = [x.text for x in extranumbers[:10]]
                    # extend case numbers list
                    casenumbers.extend(extranumbers)

            # return case numbers outcome as list
            return casenumbers

        # handle error
        except:
            return 'There were errors when finding cases for candidate ' \
                + self.name + '.'

    # # case number scraper function
    # def case(self, name):
    #     """method to download any case number by candidate information"""
        
    #     # search parameters
    #     # not available

    #     # return call
    #     return 'This method has not been developed yet'

    # sct case decisions scraper function
    def decision(self, number):
        """method to download case decisions by candidate information"""
        
        # store case number in class object
        self.number = number

        # navigate to page
        self.browser.get(self.urldec)

        # find text box to write case numbers
        numberid = 'numeroDigitoAnoUnificado'

        # click to search and check whether information has been loaded
        sbtpath = '//*[(@id = "pbEnviar")]'
        check   = '//*[(@class = "subtitle")]'

        # try catch for handling cases not available in online database
        try:
            # find 'case number' text box and send individual case number
            numberbox = self.browser.find_element_by_id(numberid)
            numberbox.send_keys(str(self.number[0:13]))
            numberbox.send_keys(str(self.number[16:20]))
            numberbox.send_keys(Keys.TAB)

            # find submit button and click it to search cases
            self.browser.find_element_by_xpath(sbtpath).click()

            # counter used in while loop when waiting for elements to be loaded
            counter = 1

            # while loop to test whether elements are loaded
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

            # save to disk with correct encoding
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

    attributes:
        file:               path to html containing the candidacy decision

    methods:
        parse_summary:      parse summary table
        parse_litigants:    parse litigants table
        parse_updates:      parse case updates
        parse_petitions:    parse petitions information
        parse_incidents:
        parse_attachments:
        parse_hearings:

    """

    # define static variables used for parsing all tables
    soup   = []
    tables = []

    # define regex for substituting weird characters in all tables
    regex0 = re.compile(r'\n|\t')
    regex1 = re.compile(r'\\n|\\t')
    regex2 = re.compile(r'\xa0')
    regex3 = re.compile(r':', re.IGNORECASE)
    regex4 = re.compile(' +')
    regex5 = re.compile('(?<= )([a-z]+:)', re.IGNORECASE)
    regex6 = re.compile('[a-z]+')
    regex7 = re.compile('([0-9]{2}/[0-9]{2}/[0-9]{4})')
    regex8 = re.compile('(.)+')
    
    # init method shared by all class instances
    def __init__(self, file):
        """load into class the file which will be parsed"""
        
        # try utf-8 encoding first or cp1252 if loading fails
        try:
            self.file = codecs.open(file, 'r', 'utf-8').read()
        except:
            self.file = codecs.open(file, 'r', 'cp1252').read()

        # call BeautifulSoup to read string as html
        self.soup = BeautifulSoup(self.file, 'lxml')

    #1 parse summary info table:
    def parse_summary(self, transpose = False):
        """method to parse summary information"""

        # find summary table
        table = self.soup.find_all('table', {'class': 'secaoFormBody'})[1]

        # find text in each row
        text = [row.text for row in table.find_all('tr', {'class': ''})]

        # subset list to meaningful variables
        text = list(filter(self.regex3.search, text))

        # clean up string
        text = [re.sub(self.regex0, '', i) for i in text]
        text = [re.sub(self.regex2, '', i) for i in text]
        text = [re.sub(self.regex4,' ', i) for i in text]

        # split variable names and content
        text = [re.split(self.regex3, i, maxsplit = 1) for i in text]

        # transform to pd dataset, drop duplicates, and reindex rows
        text = pd.DataFrame(text).drop_duplicates().reset_index(drop = True)

        # return outcome if transpose is not provided as argument
        if transpose == False:
            text.columns = ['variables', 'values']
            return pd.DataFrame(text)
        else:
            text = text.T
            text.columns = text.iloc[0]
            return pd.DataFrame(text[1:])

    #2 parse litigants
    def parse_litigants(self, transpose = False):
        """method to parse litigant information"""
        
        # find litigants table
        table = self.soup.find('table', {'id': 'tablePartesPrincipais'})

        # find text in each row
        text = [row.text for row in \
                table.find_all('tr', {'class': 'fundoClaro'})]

        # clean up string
        text = [re.sub(self.regex0,' ', i) for i in text]
        text = [re.sub(self.regex2, '', i) for i in text]
        text = [re.sub(self.regex4,' ', i) for i in text]

        # split variable names and contents.t
        text = [re.split(self.regex5, i) for i in text]

        # flatten list, trim whitespace, replace ':', and delete empty strings
        flat = [i for j in text for i in j]
        flat = [i.strip() for i in flat]
        flat = [re.sub(':', '', i) for i in flat]
        flat = list(filter(self.regex6.search, flat))

        # created nested list of litigant categories and their names
        text = [flat[i:i + 2] for i in range(0, len(flat), 2)]

        # transform to pd dataset
        text = pd.DataFrame(text)

        # return outcome if transpose is not provided as argument
        if transpose == False:
            text.columns = ['parts', 'values']
            return pd.DataFrame(text)
        else:
            text = text.T
            text.columns = text.iloc[0]
            return pd.DataFrame(text[1:])

    #3 parse updates
    def parse_updates(self):
        """method to parse update information"""

        # find updates table
        table = self.soup.find('tbody', {'id': 'tabelaTodasMovimentacoes'})

        # find text in each row
        text = [row.text for row in table.find_all('tr', {'style': ''})]

        # clean up string
        text = [re.sub(self.regex0, '', i) for i in text]
        text = [re.sub(self.regex2, '', i) for i in text]
        text = [re.sub(self.regex4,' ', i) for i in text]

        # split variables, flatten list, trim space, and extract unique values
        text = [re.split(self.regex7, i, maxsplit = 1) for i in text]
        flat = [i for j in text for i in j]
        flat = [i.strip() for i in flat]
        flat = list(filter(self.regex8.search, flat))

        # created nested list of litigant categories and their names
        text = [flat[i:i + 2] for i in range(0, len(flat), 2)]

        # transform to pd dataset
        text = pd.DataFrame(text)
        text.columns = ['updates', 'values']

        # return outcome 
        return text

    #4 parse petitions
    def parse_petitions(self):
        """method to parse petition information"""

        # find petitions table
        table = self.soup.find_all('table', \
                {'style': 'margin-left:15px; margin-top:1px;'})[2]

        # find text in each row
        text = [row.text for row in table.find_all('tbody')]

        # clean up string
        text = [re.sub(self.regex0, '', i) for i in text]
        text = [re.sub(self.regex2, '', i) for i in text]
        text = [re.sub(self.regex4,' ', i) for i in text]

        # split variables, flatten list, trim space, and extract unique values
        text = [re.split(self.regex7, i) for i in text]

        # flatten list, trim whitespace, replace ':', and delete empty strings
        flat = [i for j in text for i in j]
        flat = [i.strip() for i in flat]
        flat = list(filter(self.regex8.search, flat))

        # created nested list of litigant categories and their names
        text = [flat[i:i + 2] for i in range(0, len(flat), 2)]

        # transform to pd dataset
        text = pd.DataFrame(text)
        text.columns = ['dates', 'values']

        # return outcome 
        return text

    #5 parse incidental information
    def parse_incidents(self):
        pass

    #6 parse attached cases
    def parse_casesattached(self):
        pass

    #7 parse court hearings
    def parse_hearings(self):
        """method to parse case hearing information"""

        # find petitions table
        table = self.soup.find_all('table', \
                {'style': 'margin-left:15px; margin-top:1px;'})[5]

        # find text in each row
        # head = [row.text for row in table.find_all('th')]
        body = [td.text for tr in table.find_all('tr')[1:] \
                for td in tr.find_all('td')]

        # clean up string
        text = [re.sub(self.regex0, '', i) for i in body]
        text = list(filter(self.regex8.search, text))
        text = [i.strip() for i in text]

        # created nested list of hearings and their names
        text = [text[i:i + 4] for i in range(0, len(text), 4)]

        # transform to pd dataset
        text = pd.DataFrame(text)
        text.columns = ['dates', 'hearing', 'status', 'attendees']

        # return outcome 
        return text


