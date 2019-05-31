### judicial favoritism of politicians: analysis script
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
if (!require(here))      {install.packages('here')}
if (!require(lfe))       {install.packages('lfe')}
if (!require(magrittr))  {install.packages('magrittr')}
if (!require(pdftools))  {install.packages('pdftools')}
if (!require(sandwich))  {install.packages('sandwich')}
if (!require(stargazer)) {install.packages('stargazer')}
if (!require(stopwords)) {install.packages('stopwords')}
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
source('scripts/01_judicialBias_candidates.R')

# prepare dataset for python scraper
source('scripts/02_judicialBias_lawsuits.R')
