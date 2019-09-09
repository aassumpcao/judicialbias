### tjsp sct case decision scraper
# this script downloads the case information for all small claims cases
#  for random litigants in 2008. each case information is downloaded to
#  disk as an html file, which then needs to be parsed into quantitative
#  data.
# developed by:
# andre assumpcao
# andre.assumpcao@gmail.com

# import standard libraries
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.support.ui import WebDriverWait
import csv, os, pandas as pd

# import
import tjsp

# define chrome options
CHROME_PATH = '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'
CHROMEDRIVER_PATH = '/usr/local/bin/chromedriver'

# set options
chrome_options = Options()
chrome_options.add_argument('--headless')
chrome_options.add_argument('--window-size=1920,1080')
chrome_options.binary_location = CHROME_PATH

# open invisible browser
args = {'executable_path': CHROMEDRIVER_PATH, 'options': chrome_options}
browser = webdriver.Chrome(**args)

# set implicit wait for page load
browser.implicitly_wait(10)

# load data
candidates = pd.read_csv('data/tjspSentences.csv')
candidates = candidates.dropna(subset = ['claimant.win'])
candidates = candidates.reset_index(drop = True)
keep = list(candidates['class'] == 'Procedimento do Juizado Especial Cível')

# crate list of lawsuitIDs serving as reference to new scraper
candidates = candidates[keep]
cnjs = candidates['caseID'].to_list()

# create function to search lawsuits filed at the same time
def get_cnj(cnj):
    _dv = lambda x: 98 - (int(x) % 97)
    _dv_str = lambda x: '{:02d}'.format(_dv(x))
    plus, minus = str(int(cnj[:-13]) + 1), str(int(cnj[:-13]) - 1)
    n_plus, n_minus = plus + cnj[-11:] + '00', minus + cnj[-11:] + '00'
    plus = plus + _dv_str(n_plus) + cnj[-11:]
    minus = minus + _dv_str(n_minus) + cnj[-11:]
    plus, minus = '{:020d}'.format(int(plus)), '{:020d}'.format(int(minus))
    return [(cnj, plus), (cnj, minus)]

# create list of new lawsuits to download
random_cases = [get_cnj(cnj) for cnj in cnjs]
random_cases = [pd.DataFrame(random) for random in random_cases]

# create dataframe to store results
random_cases = pd.concat(random_cases, ignore_index = True)
random_cases.columns = ['cnj', 'cnj_adjacente']
random_cases.to_csv('data/tjspRandom.csv', index = False)

# create list of downloads
downloads = random_cases['cnj_adjacente'].to_list()

# create folder for html files
if not os.path.exists('html'):
  os.mkdir('html')

# change directory for download
os.chdir('html')

# execute loop
results = [tjsp.scraper(browser).case(download) for download in downloads]

# quit browser
browser.quit()
