### tj-sp classes and methods
# developed by:
# Andre Assumpcao
# andre.assumpcao@gmail.com

# import statements
import codecs
import glob
import math
import numpy as np
import os
import pandas as pd
import re
import time
from bs4 import BeautifulSoup
from selenium                          import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.common.exceptions        import TimeoutException
from selenium.common.exceptions        import StaleElementReferenceException
from selenium.webdriver.common.by      import By
from selenium.webdriver.common.keys    import Keys
from selenium.webdriver.support.ui     import WebDriverWait
from selenium.webdriver.support        import expected_conditions as EC


# define scraper class
class scraper:
    """series of methods to download case documents from tj-sp

    attributes:
        browser:    placeholder for selenium browser call

    methods:
        case:       download case number by name (only SCTs for now)
        decision:   use case number to download judicial decision

    """
    # initial arguments (for both esaj website and browser)
    browser  = []
    urlcase  = 'https://esaj.tjsp.jus.br/cjpg/'
    urldec   = 'https://esaj.tjsp.jus.br/cpopg/open.do'
    classSCT = 'Procedimento do Juizado Especial Cível'
    java     = 'return document.getElementsByTagName("html")[0].innerHTML'

    # regex arguments
    regex0 = re.compile('(?<=de )[0-9]+', re.IGNORECASE)
    regex1 = re.compile(r'\n')
    regex2 = re.compile('[^a-zA-Z]')
    regex3 = re.compile('CPF')
    regex4 = re.compile('Juizado Especial Cível')

    # init method shared by all class instances
    def __init__(self, browser):
        """load into class the browser"""

        # store browser info
        self.browser = browser

    # case number scraper function (only for special civil tribunals)
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
        self.regex0 = re.compile('(?<=de )[0-9]+', re.IGNORECASE)

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
            total = re.search(self.regex0, searched)

            # exit if no cases are found
            if not total: return ['Nothing found']

            # else determine the number of pages containing all cases
            total = re.search(self.regex0, searched)[0]
            pages = math.ceil(int(total) / 10)

            # find and extract all individual case numbers in first page
            casenumbers = self.browser.find_elements_by_xpath(numbers)
            casenumbers = [x.text for x in casenumbers[:10]]

            # run loop if there are multiple pages
            if pages > 1:
                # loop constructing list of case numbers in other pages
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

    # case number scraper function
    def cpf(self, cpf, sct = False):
        """method to download any case number by cpf"""

        # store politician's cpf in class object
        self.cpf = cpf

        # navigate to page
        self.browser.get(self.urldec)

        # find text box to write politicians' name in and case class
        select  = '//*[(@name = "cbPesquisa")]' + \
                  '/option[text() = "Documento da Parte"]'
        cpfid   = '//*[(@id = "campo_DOCPARTE")]'
        cpfid   = 'campo_DOCPARTE'
        classid = 'classe_selectionText'

        # click to search
        sbtpath = '//*[(@id = "pbEnviar")]'

        # find search results
        search = '//span[(@class = "resultadoPaginacao")]'

        # find total number of cases in page
        numbers = '//*[contains(@class, "linkProcesso")]'

        # find next page button
        nextpage = '//*[@title = "Próxima página"]'

        # try catch for candidates who weren't found
        try:
            # find 'documento da parte' selection box, click and send
            # politician cpf
            self.browser.find_element_by_xpath(select).click()
            self.browser.find_element_by_id(cpfid).send_keys(cpf)

            # find submit button and click it to search cases
            time.sleep(.5)
            self.browser.find_element_by_xpath(sbtpath).click()

            # force wait before information is loaded
            time.sleep(.5)

            # return the text containing the results of the search
            searched = self.browser.find_element_by_xpath(search).text

            # confirm the number of cases found
            total = re.search(self.regex0, searched)

            # exit if no cases are found
            if not total: return 'No case found for cpf ' + self.cpf + '.'

            # else determine the number of pages containing all cases
            total = re.search(self.regex0, searched)[0]
            pages = math.ceil(int(total) / 25)

            # if sct
            if sct:
                # if summary info is requested
                caseclass = '//*[contains(@id, "divProcesso")]'

                # define empty list for process
                cases = {'title': [], 'casenumber': [], 'litigant': []}

                # get process summary
                for i in self.browser.find_elements_by_xpath(caseclass):
                    case = [None, None, None]
                    for x, j in enumerate(i.find_elements_by_tag_name('div')):
                        if re.search(self.regex1, j.text):
                            case[0] = j.text
                        if re.search(self.regex2, j.text) and x < 2:
                            case[1] = j.text
                        if re.search(self.regex3, j.text):
                            case[2] = j.text
                    # append to cases
                    cases['title'].append(case[0])
                    cases['casenumber'].append(case[1])
                    cases['litigant'].append(case[2])
            else:
                # find and extract all individual case numbers in first page
                casenumbers = self.browser.find_elements_by_xpath(numbers)
                casenumbers = [x.text for x in casenumbers]

            # run loop if there are multiple pages
            if pages > 1:
                # loop constructing list of case numbers in other pages
                for i in range(pages - 1):
                    if not i == pages - 1:
                        # click to advance pages except for last page
                        self.browser.find_element_by_xpath(nextpage).click()
                    time.sleep(.5)
                    # if sct
                    if sct:
                        time.sleep(.5)
                        # get process summary
                        for i in \
                            self.browser.find_elements_by_xpath(caseclass):
                            case = [None, None, None]
                            for x, j in \
                                enumerate(i.find_elements_by_tag_name('div')):
                                if re.search(self.regex1, j.text):
                                    case[0] = j.text
                                if re.search(self.regex2, j.text) and x < 2:
                                    case[1] = j.text
                                if re.search(self.regex3, j.text):
                                    case[2] = j.text
                            # append to cases
                            cases['title'].append(case[0])
                            cases['casenumber'].append(case[1])
                            cases['litigant'].append(case[2])
                    else:
                        # get additional case numbers
                        extranum = self.browser.find_elements_by_xpath(num)
                        extranum = [x.text for x in extranum]
                        # extend case numbers list
                        casenumbers.extend(extranum)

            # return cases summaries or case numbers as list
            if sct:
                cases = pd.DataFrame(cases)
                cases = cases[cases['title'].str.contains(self.regex4)]
                if len(cases) > 0:
                    cases = cases.drop(columns = 'title').to_dict('list')
                    return cases
                else:
                    return 'No sct cases found for cpf' + self.cpf + '.'
            else:
                return casenumbers

        # handle error
        except:
            return 'Error for cpf ' + self.cpf + '.'

    # sct case decisions scraper function
    def decision(self, number):
        """method to download case decisions by candidate information

            currently only working for SCT cases

        """

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
            # find 'case number' text box and type individual number
            numberbox = self.browser.find_element_by_id(numberid)
            numberbox.send_keys(str(self.number[0:13]))
            numberbox.send_keys(str(self.number[16:20]))
            numberbox.send_keys(Keys.TAB)

            # find submit button and click it to search cases
            self.browser.find_element_by_xpath(sbtpath).click()

            # counter for when waiting for elements to be loaded
            counter = 1

            # while loop to test whether elements are loaded
            while counter < 7:
                # check and wait if els are not visible in first page
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
    regex5 = re.compile('(?<= )([a-zA-Zé\\.]+:)', re.IGNORECASE)
    regex6 = re.compile('[a-zA-Z]+')
    regex7 = re.compile('([0-9]{2}/[0-9]{2}/[0-9]{4})')
    regex8 = re.compile('(.)+')
    regex9 = re.compile('Reqte|Autor|Exeqte|Imptte|Embargte|Reclamante',
                        re.IGNORECASE)
    regex10 = re.compile('Reqd[ao]|Exectd[ao]|Imptd[ao]|Réu|Embargd[ao]|' +
                         'Reclamad[ao]', re.IGNORECASE)
    regex11 = re.compile('Advogad[oa]', re.IGNORECASE)

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
        table = self.soup.find('table', {'id': 'tableTodasPartes'})

        # use other table if all litigants is empty
        if not table:
            table = self.soup.find('table', {'id': 'tablePartesPrincipais'})

        # find text in each row
        text = [row.text for row in \
                table.find_all('tr', {'class': 'fundoClaro'})]

        # clean up string
        text = [re.sub(self.regex0,' ', i) for i in text]
        text = [re.sub(self.regex2, '', i) for i in text]
        text = [re.sub(self.regex4,' ', i) for i in text]

        # split variable names and contents.
        text = [re.split(self.regex5, i) for i in text]

        # flatten list, trim whitespace, replace ':', and delete empty strings
        flat = [i for j in text for i in j]
        flat = [i.strip() for i in flat]
        flat = [re.sub(':', '', i) for i in flat]
        flat = list(filter(self.regex6.search, flat))

        # initiate dictionary with case litigants
        litigants = {'claimant': [], 'defendant': [],
                     'clawyers': [], 'dlawyers': []}

        # switch litigants
        switch = 0

        # define lists of keys and values
        keys = flat[::2]
        values = flat[1::2]

        # zip over keys and values and assign to dictionary entry
        for key, value in zip(keys, values):
            if re.search(self.regex9, key):
                litigants['claimant'].append(value)
                switch = 1
            if re.search(self.regex10, key):
                litigants['defendant'].append(value)
                switch = 2
            if re.search(self.regex11, key) and switch == 1:
                litigants['clawyers'].append(value)
            if re.search(self.regex11, key) and switch == 2:
                litigants['dlawyers'].append(value)

        # collapse lists
        litigants['claimant'] = [';'.join(litigants['claimant'])]
        litigants['defendant']= [';'.join(litigants['defendant'])]
        litigants['clawyers'] = [';'.join(litigants['clawyers'])]
        litigants['dlawyers'] = [';'.join(litigants['dlawyers'])]

        # make dictionary a pd dataframe
        text = pd.DataFrame.from_dict(litigants)

        # return outcome if transpose is not provided as argument
        if transpose == False:
            text = text.transpose().reset_index()
            text.columns = ['partKey', 'partValue']
            return text
        else:
            return text

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


