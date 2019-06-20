### judicial favoritism of politicians
# master script
#   this is the master script for the reproduction of the entire work in chapter
#   two of my dissertation. it contains two large groups of scripts (r and
#   python): data wrangling (or munging) and analysis. i indicate below the
#   execution times for either group when scripts took longer than 15 minutes
#   to execute. if you have r, rstudio, and python installed on the computer,
#   you can source this script from the top. if you would like further
#   clarification on how to go about these scripts, please email me at the
#   address below.
# author: andre assumpcao
# by: andre.assumpcao@gmail.com
  
# import statements (== packages required to run all scripts in R)
if (!require(AER))       {install.packages('AER')}
if (!require(extrafont)) {install.packages('extrafont')}
if (!require(fuzzyjoin)) {install.packages('fuzzyjoin')}
if (!require(genderBR))  {install.packages('genderBR')}
if (!require(lfe))       {install.packages('lfe')}
if (!require(magrittr))  {install.packages('magrittr')}
if (!require(sandwich))  {install.packages('sandwich')}
if (!require(stargazer)) {install.packages('stargazer')}
if (!require(stopwords)) {install.packages('stopwords')}
if (!require(stringdist)){install.packages('stringdist')}
if (!require(tidyverse)) {install.packages('tidyverse')}
if (!require(xtable))    {install.packages('xtable')}

# load rproj (comment out if using another R IDE)
rstudioapi::openProject('2019 Judicial Bias.Rproj')

### wrangling scripts
# these scripts wrangle all data used in this paper. you should not run them as
# they will take a long time to process (> 44 hours if laptop; > 33 hours if
# cluster). you are better off using the final datasets than producing them;
# nonetheless, i include all files for replication and transparency purposes if
# you are interested in a particular step taken.

# wrangle candidate data
source('scripts/01_tjsp_candidates.R')

# wrangle judicial data
source('scripts/02_tjsp_sentences.R')

# wrangle judge data
source('scripts/03_tjsp_judges.R')

# prepare analysis for paper
source('scripts/04_tjsp_analysis_prep.R')

# python3.7: install packages from requirements.txt to run the next script.
system2('cat scripts/requirements.txt | xargs -n 1 pip install')

# python3.7: create judicial outcome  algorithm from all sentences. this
# script takes 25 hours to run on a big memory (500g) cluster. use with caution.
system2('python scripts/99_tjsp_sct_outcome.py &')

# produce paper analysis
source('scripts/05_tjsp_analysis.R')
