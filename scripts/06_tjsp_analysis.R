### judicial favoritism of politicians
# this script produces paper analysis
# author: andre assumpcao
# email:  andre.assumpcao@gmail.com

# for testing
rm(list = ls())

### import statements
# import packages
library(magrittr)
library(tidyverse)

# load data
load('data/candidatesSP.Rda')
load('data/sctSummary.Rda')
load('data/sctDetails.Rda')

### body
# drop useless variables
candidatesSP %<>% select(-candidate.plaintiff, -trial.outcome)

#
