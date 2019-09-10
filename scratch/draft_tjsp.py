python3.7
# import statements
import codecs
import glob
import pandas as pd
import re
import os
import numpy as np
import time
import re
import random
import math
import importlib
import tjsp_tests
import feather
from bs4 import BeautifulSoup
from selenium                          import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.common.exceptions        import TimeoutException
from selenium.common.exceptions        import StaleElementReferenceException
from selenium.webdriver.common.by      import By
from selenium.webdriver.common.keys    import Keys
from selenium.webdriver.support.ui     import WebDriverWait
from selenium.webdriver.support.ui     import Select
from selenium.webdriver.support        import expected_conditions as EC

clear = lambda: os.system('clear')

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
chrome_options.add_argument('--headless')
chrome_options.add_argument('--window-size=%s' % WINDOW_SIZE)
chrome_options.binary_location = CHROME_PATH

# open invisible browser
browser = webdriver.Chrome(executable_path = CHROMEDRIVER_PATH, \
                           options = chrome_options)

# set implicit wait for page load
browser.implicitly_wait(10)

# tests ok!
tjsp.scraper(browser).case('"Fernando Holiday"')
tjsp.scraper(browser).case('Nathan Jensen')
tjsp.scraper(browser).case('"marcelo assumpção"')

# tests ok!
tjsp.scraper(browser).decision('10092683820178260011')
tjsp.scraper(browser).decision('00054902620128260505')
tjsp.scraper(browser).decision('00085892420138260002')
tjsp.scraper(browser).decision('00085892420138260002')

# tests ok!
tjsp.parser(file).parse_summary()
tjsp.parser(file).parse_litigants()
tjsp.parser(file).parse_updates()
tjsp.parser(file).parse_petitions()
tjsp.parser(file).parse_hearings()

# testing
os.chdir('..')
importlib.reload(tjsp)
os.chdir('./html')

file = 'sct10092683820178260011.html'
file = 'sct00085892420138260002.html'
file = 'sct10006570820188260320.html'

file = tjsp.parser(file).parse_litigants()

soup = file.soup
globals()

# find petitions table
table = soup.find_all('table', {'style': 'margin-left:15px; margin-top:1px;'})[5]

# find text in each row
head = [row.text for row in table.find_all('th')]
body = [td.text for tr in table.find_all('tr')[1:] for td in tr.find_all('td')]

# clean up string
text = [re.sub(regex0, '', i) for i in body]
text = list(filter(regex8.search, text))
text = [i.strip() for i in text]

# created nested list of hearings and their names
text = [text[i:i + 4] for i in range(0, len(text), 4)]

# transform to pd dataset
text = pd.DataFrame(text)
text.columns = ['dates', 'hearing', 'status', 'attendees']

# problem for summary parser
'1000570-26.2018.8.26.0361'

randomlist = random.sample(list(lawsuits['caseID']), 3)

# test random list
randomlist = [re.sub('-|\\.', '', i) for i in randomlist]

# actual random list
randomlist = ['10326328020158260602', '00691248420118260002', '00056093620158260002']

tjsp.scraper(browser).decision(randomlist[0])
tjsp.scraper(browser).decision(randomlist[1])
tjsp.scraper(browser).decision(randomlist[2])
tjsp.scraper(browser).decision('10005702620188260361')

files = ['sct' + i + '.html' for i in randomlist]

# tests ok!
tjsp.parser(files[0]).parse_summary()
tjsp.parser(files[1]).parse_summary()
tjsp.parser(files[2]).parse_summary()


# testing
tjsp.parser(files[0]).parse_litigants()
tjsp.parser(files[1]).parse_litigants()
tjsp.parser(files[2]).parse_litigants()

browser.quit()

### TO-DO: fix parser when litigants don't have lawyers. Example below:
file0 = 'sct10005702620188260361.html' # working
file1 = 'sct00691248420118260002.html' # not working

tjsp.parser(file0).parse_litigants()
tjsp.parser(file1).parse_litigants()

# load data from different iterations
# data = [feather.read_dataframe(i) for i in glob.glob('./lawsuits*')]

# concatenate elements in data
# data = pd.concat(data)

# # write to disk
# feather.write_dataframe(data, 'lawsuits.feather')

import feather

# load data onto python session
lawsuits = feather.read_dataframe('lawsuits.feather')
candidateCPF = feather.read_dataframe('candidateCPF.feather')

# sort values by candidateID
lawsuits = lawsuits.sort_values('candidateID')

# filter only candidates who have SCT cases
lawsuits = lawsuits[lawsuits['caseID'] != 'N']

# drop duplicates
lawsuits = lawsuits.drop_duplicates()

# join on scraperID and pull CPF
lawsuits = pd.merge(lawsuits, candidateCPF, how = 'left', \
                    left_on = 'candidateID', right_on = 'scraperID')
# drop scraperID
lawsuits = lawsuits.drop('scraperID', axis = 1)
lawsuits.dtypes

regex0 = re.compile(r'\n|\t')
regex1 = re.compile(r'\\n|\\t')
regex2 = re.compile(r'\xa0')
regex3 = re.compile(r':', re.IGNORECASE)
regex4 = re.compile(' +')
regex5 = re.compile('(?<= )([a-zA-Zé\\.]+:)', re.IGNORECASE)
regex6 = re.compile('[a-zA-Z]+')
regex7 = re.compile('([0-9]{2}/[0-9]{2}/[0-9]{4})')
regex8 = re.compile('(.)+')
regex9 = re.compile('Reqte|Autor|Exeqte|Imptte|Embargte|Reclamante', re.IGNORECASE)
regex10 = re.compile('Reqd[ao]|Exectd[ao]|Imptd[ao]|Réu|Embargd[ao]|Reclamad[ao]', re.IGNORECASE)
regex11 = re.compile('Advogad[oa]', re.IGNORECASE)

cpf = '00768782872'
cpf = '96926147868'

# store politician's cpf in class object
self.cpf = cpf

# navigate to page
browser.get(urldec)

# find text box to write politicians' name in and case class
select  = '//*[(@name = "cbPesquisa")]/option[text() = "Documento da Parte"]'
cpfid   = '//*[(@id = "campo_DOCPARTE")]'
cpfid   = 'campo_DOCPARTE'
classid = 'classe_selectionText'

# click to search
sbtpath = '//*[(@id = "pbEnviar")]'

# find search results
search = '//span[(@class = "resultadoPaginacao")]'

# find total number of cases in page
numbers = '//*[contains(@class, "linkProcesso")]'

# fint total number of cases in all pages and extract it
regex0 = re.compile('(?<=de )[0-9]+', re.IGNORECASE)
regex1 = re.compile('[0-9]+', re.IGNORECASE)

# find next page button
nextpage = '//*[@title = "Próxima página"]'

# try catch for candidates who weren't found

# find 'documento da parte' selection box, click and send politician
# cpf
browser.find_element_by_xpath(select).click()
browser.find_element_by_id(cpfid).send_keys(cpf)

# find submit button and click it to search cases
time.sleep(.5)
browser.find_element_by_xpath(sbtpath).click()

# force wait before information is loaded
time.sleep(1)

# return the text containing the results of the search
searched = browser.find_element_by_xpath(search).text

# confirm the number of cases found
total = re.search(regex0, searched)

# exit if no cases are found
if not total: return 'No case found for cpf ' + self.cpf + '.'

# else determine the number of pages containing all cases
total = re.search(regex0, searched)[0]
pages = math.ceil(int(total) / 25)

# find and extract all individual case numbers in first page
casenumbers = browser.find_elements_by_xpath(numbers)
casenumbers = [x.text for x in casenumbers]

# run loop if there are multiple pages
if pages > 1:
    # loop constructing list of case numbers in other pages
    for i in range(pages - 1):
        if not i == pages - 1:
            # click to advance pages except for last page
            browser.find_element_by_xpath(nextpage).click()
        time.sleep(1)
        # get additional case numbers
        extranumbers = browser.find_elements_by_xpath(numbers)
        extranumbers = [x.text for x in extranumbers]
        # extend case numbers list
        casenumbers.extend(extranumbers)

# return case numbers outcome as list
return casenumbers

# handle error
return 'No case found for cpf ' + self.cpf + '.'

regex1 = re.compile(r'\n')
regex2 = re.compile('[^a-zA-Z]')
regex3 = re.compile('CPF')

# if summary info is requested
caseclass = '//*[contains(@id, "divProcesso")]'

# define empty list for process
cases = {'title': [], 'casenumber': [], 'litigant': []}

# get process summary
for i in browser.find_elements_by_xpath(caseclass):
    case = [None, None, None]
    for x, j in enumerate(i.find_elements_by_tag_name('div')):
        if re.search(regex1, j.text): case[0] = j.text
        if re.search(regex2, j.text) and x < 2: case[1] = j.text
        if re.search(regex3, j.text): case[2] = j.text
    cases['title'].append(case[0])
    cases['casenumber'].append(case[1])
    cases['litigant'].append(case[2])

cases = pd.DataFrame(cases)




cpf = '00768782872'
cpf = '96926147868'

importlib.reload(tjsp)
tjsp.scraper(browser).cpf('96926147868', True)

len(test['title'])
len(test['casenumber'])
len(test['litigant'])

caseclass = '//*[contains(@id, "divProcesso")]'

cases = {'title': [], 'casenumber': [], 'litigant': []}

# get process summary
for i in browser.find_elements_by_xpath(caseclass):
    case = [None, None, None]
    for x, j in enumerate(i.find_elements_by_tag_name('div')):
        if re.search(regex1, j.text):
            case[0] = j.text
        if re.search(regex2, j.text) and x < 2:
            case[1] = j.text
        if re.search(regex3, j.text):
            case[2] = j.text
    cases['title'].append(case[0])
    cases['casenumber'].append(case[1])
    cases['litigant'].append(case[2])


tjsp.scraper(browser).case('Micheloni')


import pandas as pd
import os, re

# define args and load csv
kwargs = {'header': None, 'dtype': str, 'index_col': 0}
summary = pd.read_csv('data/sctSummary_random.csv', **kwargs)

# pull column names out, reset index, and drop invalid column
columns = summary.iloc[0].to_list()[:10]
summary = summary.reset_index(drop = True)
summary.drop(0, axis = 0, inplace = True)

# filter sct cases only
summary = summary[summary.iloc[:,1]=='Procedimento do Juizado Especial Cível']
summary = summary.iloc[:,0:10]
summary.columns = columns
