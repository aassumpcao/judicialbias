### judicial favoritism of politicians
# this script produces paper analysis
# author: andre assumpcao
# email:  andre.assumpcao@gmail.com

# for testing
rm(list = ls())

### data and library calls
# import libraries
library(AER)
library(extrafont)
library(lfe)
library(magrittr)
library(stargazer)
library(tidyverse)
library(xtable)

# load data
load('data/tjspFinal.Rda')

### function definitions
# (blank)

### define variable labels
# outcome label
outcomeLabel <- 'SCT Favorable Outcome'

# create list of covariates
covariates <- c(10, 6, 11:13, 24, 30:31, 35, 33, 28, 29)
covarLabel <- c('Case Duration (in days)', 'Amount Claimed (in R$)',
                'Pay (in R$)', 'Male', 'Tenure (in days)',  'Age', 'Male',
                'Political Experience', 'Campaign Expenditures (in R$)',
                'Elected', 'Level of Education', 'Marital Status')

# define variable types for analysis
integers <- c(6, 9, 11, 19, 24, 30:31, 33, 35)
factors  <- c(3, 5, 12, 14:16, 25:29, 34, 36, 37)

# change variable types
tjspAnalysis %<>%
  mutate(case.claim = str_replace_all(case.claim, '\\.|R\\$', '')) %>%
  mutate(case.claim = str_replace_all(case.claim, '\\,', '.') %>% trimws()) %>%
  mutate_at(vars(integers), as.integer) %>%
  mutate_at(vars(factors), as.factor) %>%
  mutate(judge.gender = judge.gender %>% {ifelse(. == 'Female', 0, 1)})

### tables and analysis
# produce summary statistics table
stargazer(

  # summmary table
  as.data.frame(tjspAnalysis[, covariates[1:3]]),

  # table cosmetics
  type = 'text',
  title = 'Descriptive Statistics',
  style = 'default',
  summary = TRUE,
  # out = 'tables/sumstats.tex',
  out.header = FALSE,
  covariate.labels = covarLabel,
  align = FALSE,
  digit.separate = 3,
  digits = 3,
  digits.extra = 0,
  font.size = 'scriptsize',
  header = FALSE,
  initial.zero = FALSE,
  model.names = FALSE,
  label = 'tab:sumstats',
  no.space = FALSE,
  table.placement = '!htbp',
  summary.logical = TRUE,
  summary.stat = c('n', 'mean', 'sd', 'min', 'max')
)
