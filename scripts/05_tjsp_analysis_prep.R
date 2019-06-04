### judicial favoritism of politicians
# this script prepares sentences for analysis
# author: andre assumpcao
# by andre.assumpcao@gmail.com

# testing
rm(list = ls())

### import statements
# import packages
library(tidyverse)
library(magrittr)

# load data
load('data/tseCandidates.Rda')
load('data/tjspSentences.Rda')

# join tjsp and tse data
tjspSentences %>%
  left_join(ungroup(tseCandidates), c('candidateID.x' = 'scraper.id')) %>%
  filter(!is.na(claimant.win)) %>%
  filter(class == 'Procedimento do Juizado Especial Cível')

