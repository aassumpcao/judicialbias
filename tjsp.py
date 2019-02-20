### tjsp classes and methods
# developed by:
# Andre Assumpcao
# andre.assumpcao@gmail.com

# import standard libraries
import codecs
import glob
import math
import numpy as np
import os
import pandas as pd
import re
import time

# import third-party libraries
from bs4 import BeautifulSoup
from selenium                          import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.common.exceptions        import TimeoutException
from selenium.common.exceptions        import StaleElementReferenceException
from selenium.webdriver.common.by      import By
from selenium.webdriver.common.keys    import Keys
from selenium.webdriver.support.ui     import Select
from selenium.webdriver.support.ui     import WebDriverWait
from selenium.webdriver.support        import expected_conditions as EC

# define scraper class
class scraper:

    """
        the scraper class contains methods used to download data from
        tjsp websites using multiple criteria, such as case number, SSN
        number, or an individual's name.

        attributes:
            browser:    placeholder for selenium browser call

        methods:
            name:       download case number by name
            cpf:        download case number by SSN
            decision:   use case number to download judicial decisions
    """

    # create variables used by all scraper methods
    browser = []

    # provide webpages used in scraper (broad and narrow search options)
    pageBroad = 'https://esaj.tjsp.jus.br/cjpg/'
    pageNarrw = 'https://esaj.tjsp.jus.br/cpopg/open.do'

    # provide lawsuit class identification according to tjsp's website
    caseClass = 'Procedimento do Juizado Especial Cível'

    # provide java command to save a page's inner html
    java = 'return document.getElementsByTagName("html")[0].innerHTML'

    ### define arguments used in scraper method
    # group 1: html ids/xpaths used for broad search by name
    searchBox  = 'iddadosConsulta.pesquisaLivre'
    classBox   = 'classe_selectionText'
    submitBox1 = 'pbSubmit'
    resultsBox = '//*[contains(@class, "espacamentoCimaBaixo")]'
    resultsTot = '//*[contains(@class, "fonteNegrito")]'
    nextPages  = '//*[@title = "Próxima página"]'

    # group 2: html ids/xpaths used for narrow search by SSN
    selectSSN  = '//*[(@name = "cbPesquisa")]' + \
                 '/option[text() = "Documento da Parte"]'
    searchSSN  = 'campo_DOCPARTE'
    submitBox2 = 'pbEnviar'
    checkClass = '//*[contains(@id, "divProcesso")]'
    caseNumber = '//*[contains(@class, "linkProcesso")]'
    resultSSN  = '//span[(@class = "resultadoPaginacao")]'

    # group 3: html ids/xpaths used for narrow search by case number
    selectByID = 'numeroDigitoAnoUnificado'
    checkCase  = '//*[(@class = "subtitle")]'

    # scraper regex arguments
    regex0 = re.compile('(?<=de )[0-9]+', re.IGNORECASE)
    regex1 = re.compile(r'\n')
    regex2 = re.compile('[^a-zA-Z]')
    regex3 = re.compile('CPF')
    regex4 = re.compile('Juizado Especial Cível')
    regex5 = re.compile('Não foi encontrado')

    # init method shared by all class instances
    def __init__(self, browser):
        """load browser into class"""

        # store browser info
        self.browser = browser

    # define inner function to search, filter, and collect cases
    # found by SSN to sct cases only
    def _numbers_sct(self, casenumbers):

        # get process summary
        for i in self.browser.find_elements_by_xpath(self.checkClass):
            case = [None, None, None]
            for x, j in enumerate(i.find_elements_by_tag_name('div')):
                if re.search(self.regex1, j.text): case[0] = j.text
                if re.search(self.regex2, j.text) and x < 2: case[1] = j.text
                if re.search(self.regex3, j.text): case[2] = j.text
            # append to cases
            casenumbers['title'].append(case[0])
            casenumbers['casenumber'].append(case[1])
            casenumbers['litigant'].append(case[2])

        # return cases
        return casenumbers

    # define inner function to collect case numbers regardless of
    # case type
    def _numbers_all(self):

        # find and extract all individual case numbers in first page
        casenumbers = self.browser.find_elements_by_xpath(self.resultsTot)
        casenumbers = [x.text for x in casenumbers[:10]]

        # return cases numbers
        return casenumbers

    # search method taking in name as argument (sct cases only)
    def name(self, name):

        """ the name method saves sct case numbers using a person's name
            as the search criterion. search patterns follow the same
            structure as that of the tj-sp website.
        """

        # store politician's name in class object
        self.name = name

        # navigate to page
        self.browser.get(self.pageBroad)

        # find 'pesquisa livre' box and send name
        namebox = self.browser.find_element_by_id(self.searchBox)
        namebox.send_keys(self.name)
        namebox.send_keys(Keys.TAB)

        # find 'classe' box and send sct id
        classbox = self.browser.find_element_by_id(self.classBox)
        classbox.send_keys(self.caseClass)
        classbox.send_keys(Keys.TAB)

        # wait for .5s before clicking 'consultar'
        time.sleep(.5)
        self.browser.find_element_by_id(self.submitBox1).click()

        # check number of results found
        searched = self.browser.find_element_by_xpath(self.resultsBox).text

        # check if anything is found and exit program if not
        if re.search(self.regex5, searched):
            return [self.name + ' has no cases.']

        # else determine the number of pages containing all cases
        total = re.search(self.regex0, searched)[0]
        pages = math.ceil(int(total) / 10)

        # call numbers_all()
        casenumbers = self._numbers_all()

        # run loop if there are multiple pages
        if pages > 1:
            # loop constructing list of case numbers in other pages
            for i in range(pages - 1):
                if not i == pages - 1:
                    # click to advance pages except for last page
                    self.browser.find_element_by_xpath(self.nextPages).click()
                time.sleep(.5)
                # get additional case numbers
                extranumbers = self._numbers_all()
                # extend case numbers list
                casenumbers.extend(extranumbers)

        # return case numbers outcomes as list
        return casenumbers


    # search method taking in ssn as argument
    def cpf(self, cpf):

        """ the cpf method saves sct case numbers using a person's ssn
            as the search criterion. there are no additional search
            criteria other than ssn
        """

        # store politician's cpf in class object
        self.cpf = cpf

        # navigate to page
        self.browser.get(self.pageNarrw)

        # find 'documento da parte' box and send ssn
        self.browser.find_element_by_xpath(self.selectSSN).click()
        self.browser.find_element_by_id(self.searchSSN).send_keys(self.cpf)

        # wait before clicking
        time.sleep(.5)
        self.browser.find_element_by_id(self.submitBox2).click()

        # wait before page is loaded
        time.sleep(1)

        # check number of results found
        try:
            searched = self.browser.find_element_by_xpath(self.resultSSN).text
        except:
            return {'title':[None], 'casenumber':[None], 'litigant':[self.cpf]}

        # else determine the number of pages containing all cases
        total = re.search(self.regex0, searched)[0]
        pages = math.ceil(int(total) / 25)

        # define empty dictionary for process
        casenumbers = {'title': [], 'casenumber': [], 'litigant': []}

        # call numbers_sct()
        casenumbers = self._numbers_sct(casenumbers)

        # run loop if there are multiple pages
        if pages > 1:
            # loop constructing list of case numbers in other pages
            for i in range(pages - 1):
                if not i == pages - 1:
                    # click to advance pages except for last page
                    self.browser.find_element_by_xpath(self.nextPages).click()
                time.sleep(1)
                # get additional case numbers
                casenumbers = self._numbers_sct(casenumbers)

        # check for sct cases
        cases = pd.DataFrame(casenumbers)
        cases = cases[cases['title'].str.contains(self.regex4)]

        # check if empty
        if cases.empty:
            return {'title': [None], 'casenumber': [None], 'litigant': [None]}
        else:
            # return case numbers outcome as dictionary
            return cases.to_dict('list')

    # search method taking in case number as argument
    def case(self, number):

        """ this case method downloads the case document using the case
            number provided by the user.
        """

        # store case number in class object
        self.number = number

        # navigate to page
        self.browser.get(self.pageNarrw)

        # find 'case number' box and send individual case number
        numberbox = self.browser.find_element_by_id(self.selectByID)
        numberbox.send_keys(str(self.number[0:13]))
        numberbox.send_keys(str(self.number[16:20]))

        # find 'pesquisar' and hit enter
        self.browser.find_element_by_id(self.submitBox2).click()

        # wait for elements to load
        time.sleep(.5)

        # check number of results found
        searched = self.browser.find_elements_by_xpath(self.checkCase)

        # check number of results found
        if not type(searched) == list: return 'Your download has failed.'

        # save inner html to object
        html = self.browser.execute_script(self.java)

        if len(html) == 0:
            time.sleep(2)
            html = self.browser.execute_script(self.java)

        # determine file names
        file = './sct' + str(self.number) + '.html'

        # save to disk with correct encoding
        try:
            codecs.open(file, 'w', 'utf-8').write(html)
        except:
            codecs.open(file, 'w', 'cp1252').write(html)

        return 'Your download was successful.'

# define parser class
class parser:

    """
        the parser class contains methods to parse judicial decisions
        downloaded using the scraper class.

        attributes:
            file:               path to html file containing decision

        methods:
            parse_summary:      parse summary table
            parse_litigants:    parse litigants table
            parse_updates:      parse case updates
            parse_petitions:    parse petitions information
            parse_incidents:    -
            parse_attachments:  -
            parse_hearings:     parse hearing information

    """

    # create variables used by all parser methods
    file = []
    soup = []
    tables = []

    # define claimant and plaintiffs
    claim = 'Reqte|Autor|Exeqte|Imptte|Embargte|Reclamante'
    plain = 'Reqd[ao]|Exectd[ao]|Imptd[ao]|Réu|Embargd[ao]|Reclamad[ao]'
    lawyer= 'Advogad[oa]'

    # parser regex arguments
    regex0  = re.compile(r'\n|\t')
    regex1  = re.compile(r'\\n|\\t')
    regex2  = re.compile(r'\xa0')
    regex3  = re.compile(r':', re.IGNORECASE)
    regex4  = re.compile(' +')
    regex5  = re.compile('(?<= )([a-zA-Zé\\.]+:)', re.IGNORECASE)
    regex6  = re.compile('[a-zA-Z]+')
    regex7  = re.compile('([0-9]{2}/[0-9]{2}/[0-9]{4})')
    regex8  = re.compile('(.)+')
    regex9  = re.compile(claim, re.IGNORECASE)
    regex10 = re.compile(plain, re.IGNORECASE)
    regex11 = re.compile(lawyer, re.IGNORECASE)

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

    def parse_summary(self, transpose = False):
        pass

    def parse_litigants(self, transpose = False):
        pass

    def parse_updates(self):
        pass

    def parse_petitions(self):
        pass

    def parse_incidents(self):
        pass

    def parse_casesattached(self):
        pass

    def parse_hearings(self):
        pass

