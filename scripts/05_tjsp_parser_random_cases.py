### tjsp random sct case decision parser
#  this script parses the case information for random small claims cases
#  filed at the same time as politician's cases in the state court of
#  são paulo since 2008. each case information is loaded onto python as
#  an html file and parsed using module 'tjsp' developed by me.
# developed by:
# andre assumpcao
# andre.assumpcao@gmail.com

# import standard libraries
import codecs, os, re
import numpy as np, pandas as pd

# import own library
import tjsp

# list files in html folder
files = os.listdir('html')
regex = re.compile(r'html$')
files = ['html/' + file for file in list(filter(regex.search, files))]

# define function to parse different table parts
def parser(file, table):
    tjsp_parser = tjsp.parser(file)
    if table == 1:
        try:
            results = tjsp_parser.parse_summary(True)
            results['Processo'] = file[8:-5]
        except:
            pass
    elif table == 2:
        try:
            results = tjsp_parser.parse_litigants(True)
            results['Processo'] = file[8:-5]
        except:
            pass
    elif table == 3:
        try:
            results = tjsp_parser.parse_updates()
            results['Processo'] = [file[8:-5]] * len(results)
        except:
            pass
    try:
        return results
    except:
        pass

# create three lists to fill in
summary, litigants, details = [], [], []
kwargs = {'ignore_index': True, 'sort': False}

# execute loop
for i, file in enumerate(files):
    # tuple expansion filling in lists
    summary += [parser(file, 1)]
    details += [parser(file, 3)]
    litigants += [parser(file, 2)]
    # print progress
    if (i + 1) % 100 == 0:
        print('{} / {} parsed'.format(i + 1, len(files)))

# build dataset from list
summary = pd.concat([s for s in summary], **kwargs)
details = pd.concat([d for d in details], **kwargs)
litigants = pd.concat([l for l in litigants], **kwargs)

# save sets to file
summary.to_csv('data/sctSummary_random.csv')
details.to_csv('data/sctDetails_random.csv')
litigants.to_csv('data/sctLitigants_random.csv')
