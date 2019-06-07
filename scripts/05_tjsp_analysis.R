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

### wrangle variables one final time
# change variable types
tjspAnalysis %<>%
  mutate(case.claim = str_replace_all(case.claim, '\\.|R\\$', '')) %>%
  mutate(case.claim = str_replace_all(case.claim, '\\,', '.') %>% trimws()) %>%
  mutate_at(vars(integers), as.integer) %>%
  mutate_at(vars(factors), as.factor) %>%
  mutate(judge.gender = judge.gender %>% {ifelse(. == 'Female', 0, 1)})

# filling in missing values for analysis (always median)
missing.claims <- median(as.numeric(tjspAnalysis$case.claim), na.rm = TRUE)
tjspAnalysis[is.na(tjspAnalysis$case.claim), 'case.claim'] <- missing.claims

# fix claims with errors (> 40,000)
tjspAnalysis %<>%
  mutate(case.claim = case.claim %>% {ifelse(. > 40000, missing.claims, .)})

# fix median tenure time
missing.tenure <- median(as.numeric(tjspAnalysis$judge.tenure), na.rm = TRUE)
tjspAnalysis[is.na(tjspAnalysis$judge.tenure), 'judge.tenure'] <- missing.tenure

# create outcome variable for case ruling
tjspAnalysis %<>%
  mutate(sct.favorable = case_when(
    case.claimant.win == 1 & str_detect(candidate.litigant.type,'Claimant')~ 1,
    case.claimant.win == 0 & str_detect(candidate.litigant.type,'Defendant')~ 1,
  )) %>%
  replace_na(list(sct.favorable = 0))

### tables and analysis
# produce summary statistics table (case level)
stargazer(

  # summmary table
  as.data.frame(tjspAnalysis[, covariates[1:2]]),

  # table cosmetics
  type = 'latex',
  title = 'Descriptive Statistics',
  style = 'default',
  summary = TRUE,
  # out = 'tables/sumstats.tex',
  out.header = FALSE,
  covariate.labels = covarLabel[1:2],
  align = FALSE,
  digit.separate = 3,
  digits = 0,
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
# produce summary statistics table (judge level)
tjspAnalysis %>%
  select(5, covariates[3:5]) %>%
  group_by(case.judge) %>%
  filter(row_number() == 1) %>%
  {stargazer(

    # summmary table
    as.data.frame(.),

    # table cosmetics
    type = 'latex',
    title = 'Descriptive Statistics',
    style = 'default',
    summary = TRUE,
    # out = 'tables/sumstats.tex',
    out.header = FALSE,
    covariate.labels = covarLabel[3:5],
    align = FALSE,
    digit.separate = 3,
    digits = 3,
    digits.extra = 3,
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
}
# produce summary statistics table
tjspAnalysis %>%
  select(candidate.ssn, covariates[6:10]) %>%
  group_by(candidate.ssn) %>%
  filter(row_number() == 1) %>%
  {stargazer(

    # summmary table
    as.data.frame(.),

    # table cosmetics
    type = 'latex',
    title = 'Descriptive Statistics',
    style = 'default',
    summary = TRUE,
    # out = 'tables/sumstats.tex',
    out.header = FALSE,
    covariate.labels = covarLabel[6:10],
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
}

### regression tables
# abrams et al (2012) random assignment test. we create a random assignment
# dataset to use as a baseline distribution against which we compare the empi-
# rical realization of my own sample
# steps:
# 1. count cases by judge and court
# 2. group cases by court
# 3. draw random cases from 2 and fill in columns in 1
# 4. compute mean by judge and court
# 5. compute iqr across all observations
# 6. repeat 1,000 times

# 1. compute the number of cases by judge per court
cases <- count(tjspAnalysis, case.judge) %>%
         left_join(tjspAnalysis, 'case.judge') %>%
         select(1:2, tjsp.ID) %>%
         distinct(case.judge, .keep_all = TRUE)

# 2. group cases by court to create sample from which to pull observations
fromCourts <- function(court, n, var, ...){
  # create sliced dataset from court ID
  df <- filter(tjspAnalysis, tjsp.ID == as.character(court))
  # draw the number of observations from dataset
  sim <- sample_n(select(tjspAnalysis, var), as.integer(n), ...)
  # assign new name to variables
  sim <- unname(unlist(sim))
  # return call
  return(sim)
}

# draw random cases from 2, fill in columns in 1, and calculate iqr 1000 times.
# DISCLAIMER: 15 minutes running time
# create vector of simulated iqr
simulated.iqr <- c()

# execute loop creating monte carlo simulation
for (x in 1:1000) {
  # create vector containing simulation results
  simulations <- c()

  # execute loop to create simulations
  for (i in 1:nrow(cases)) {
    # create list of arguments
    args <- list(cases[i, 3], cases[i, 2], 'candidate.age', replace = TRUE)
    # call to fromCourts using args from cases
    row <- do.call(fromCourts, args)
    # bind onto vector
    simulations <- c(simulations, row)
  }
  # convert to numeric
  simulations <- as.integer(simulations)

  # extract iqr
  simulated.iqr <- c(simulated.iqr, IQR(simulations))

  # print progress
  if (x %% 100 == 0) {print(x)}
}
