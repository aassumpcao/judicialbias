### judicial bias paper
# script for scraping case information from the web
# by andre.assumpcao@gmail.com

# import statements
library(tidyverse)
library(magrittr)
library(readr)

# load dataset
load('candidatesSP.Rda')

# subset data
candidatesSP %<>% select(candidate.name, candidate.ssn, scraper.id)

# subset to unique values
candidatesSP %<>% distinct(candidate.ssn, .keep_all = TRUE)

# change letter case
candidatesSP %<>% mutate(candidate.name = str_to_title(candidate.name))

# create test dataset
trialRun <- sample_n(candidatesSP, 500)

# save to disk
write_csv(trialRun, 'trialRun.csv')
save(trialRun, file = 'trialRun.Rda')

# create final dataset
write_csv(candidatesSP, 'candidatesUnique.csv')
save(candidatesSP, file = 'candidatesUnique.Rda')

# quit R
q('no')