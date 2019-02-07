### judicial bias paper
# script for scraping case information from the web
# by andre.assumpcao@gmail.com

# import statements
library(tidyverse)
library(magrittr)
library(feather)

# load dataset
load('candidatesSP.Rda')

# subset data
candidates %<>% select(candidate.name, scraper.id)

# subset to unique values
candidates %<>% distinct(candidate.name, .keep_all = TRUE)

# change letter case
candidates %<>% mutate(candidate.name = str_to_title(candidate.name))

# create test dataset
trialRun <- sample_n(candidates, 500)

# save to disk
write_feather(trialRun, 'trialRun.feather')
save(trialRun, file = 'trialRun.Rda')

# create final dataset
write_feather(candidates, 'candidatesUnique.feather')
save(candidates, file = 'candidatesUnique.Rda')

# quit R
q('no')