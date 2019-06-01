### judicial favoritism of politicians
# candidates wrangling
#   this script wrangles the candidates in all local elections in São Paulo
#   between 2004 and 2016.
# author: andre assumpcao
# email:  andre.assumpcao@gmail.com

# for testing
rm(list = ls())

### import statements
# import packages
library(magrittr)
library(readr)
library(tidyverse)

# load data
load('data/candidatesSP.Rda')
load('data/sctSummary.Rda')
load('data/sctDetails.Rda')

### body
# drop useless variables
candidatesSP %<>% select(-candidate.plaintiff, -trial.outcome)

#
