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
load('data/tjspSimulation1.Rda')
load('data/tjspSimulation2.Rda')

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
    case.claimant.win == 1 & str_detect(candidate.litigant.type,'Claimant') ~ 1,
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

### analysis tables
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
         mutate(tjsp.ID = as.character(tjsp.ID)) %>%
         distinct(case.judge, .keep_all = TRUE)

# 2. group cases by court to create sample from which to pull observations
fromCourts <- function(court, n, var, ...) {
  # create sliced dataset from court ID
  df <- filter(tjspAnalysis, tjsp.ID == court) %>% select(var)
  # draw the number of observations from dataset
  sim <- sample_n(df, as.integer(n), ...)
  # assign new name to variables
  sim <- unname(unlist(sim))
  # return call
  return(sim)
}

# # draw random cases from 2, fill in columns in 1, and calculate moments 1000
# # times (steps 3, 4, and 5 at once). DISCLAIMER: 25 minutes running time.
# # create vector of simulated mean and iqr
# simulated.mean <- tibble(.rows = 5262)
# outcome.mean   <- tibble(.rows = 5262)

# # execute loop creating monte carlo simulation for covariate age
# for (x in 1:1000) {
#   # create vector containing simulation results
#   simulats <- c()
#   outcomes <- c()
#   # execute loop to create simulations
#   for (i in 1:nrow(cases)) {
#     # create list of arguments
#     args <- list(as.character(cases[i, 3]), cases[i, 2], 'candidate.age',
#                  replace = TRUE)
#     # call to fromCourts using args from cases
#     row <- do.call(fromCourts, args)
#     # replace list elements
#     args[[3]] <- 'sct.favorable'
#     # call to fromCourts to build outcome
#     outcome <- do.call(fromCourts, args)
#     # bind onto vector
#     simulats <- c(simulats, row)
#     outcomes <- c(outcomes, outcome)
#   }
#   # convert to numeric
#   simulats <- as.numeric(simulats)
#   outcomes <- as.integer(outcomes)
#   # extract mean and iqr
#   simulated.mean <- bind_cols(simulated.mean, sim = simulats)
#   outcome.mean   <- bind_cols(outcome.mean, sim = outcomes)
#   # print progress
#   if (x %% 100 == 0) {print(x)}
# }

# # save dataset
# save(simulated.mean, file = 'data/tjspSimulation1.Rda')
# save(outcome.mean,   file = 'data/tjspSimulation2.Rda')

# uncount variables
ids <- uncount(cases, n)

# bind to simulations and change order of variables
simulated.mean %<>% bind_cols(ids) %>% select(1001, 1:1000)
outcome.mean   %<>% bind_cols(ids) %>% select(1001, 1:1000)

# summarize covariate and outcome by mean following abrams et al (2012)
age.simulation <- simulated.mean %>%
  group_by(case.judge) %>%
  summarize_all(mean) %>%
  select(-1)
sct.simulation <- outcome.mean %>%
  group_by(case.judge) %>%
  summarize_all(mean) %>%
  select(-1)

# extract moment distributions from simulated and empirical datasets
age.mean <- age.simulation %>%
  t() %>%
  as_tibble(.name_repair = 'universal') %>%
  summarize_all(mean) %>%
  unlist() %>%
  unname()
sct.mean <- sct.simulation %>%
  t() %>%
  as_tibble(.name_repair = 'universal') %>%
  summarize_all(mean) %>%
  unlist() %>%
  unname()

# extract moment distributions from simulated datasets
age.ci <- quantile(age.mean, probs = c(.05, .95))
sct.mean.ci <- quantile(sct.mean, probs = c(.05, .95))

# compute the 1,000 iqr for both distributions
age.iqr <- age.simulation %>% summarize_all(IQR) %>% unlist() %>% unname()
sct.iqr <- sct.simulation %>% summarize_all(IQR) %>% unlist() %>% unname()

# narrow down dataset to age and sct outcome variables
empirical.moments <- tjspAnalysis %>%
  group_by(case.judge) %>%
  select(candidate.age, sct.favorable)

# extract moment distributions from empirical dataset (age)
empirical.age.iqr <- empirical.moments %>%
  group_by(case.judge) %>%
  summarize_all(mean) %>%
  summarize_all(IQR) %>%
  {unlist(.[,2])} %>%
  unname()
empirical.age.mean <- age.simulation %>%
  summarize_all(mean) %>%
  unlist() %>%
  mean()

# extract moment distributions from empirical dataset (sct favorable)
empirical.sct.iqr <- empirical.moments %>%
  group_by(case.judge) %>%
  summarize_all(mean) %>%
  summarize_all(IQR) %>%
  {unlist(.[,3])} %>%
  unname()
empirical.sct.mean <- sct.simulation %>%
  summarize_all(mean) %>%
  unlist() %>%
  mean()

# manually try out quantiles for age
iqr.significant <- quantile(age.iqr, probs = c(.05))
age.iqr.signif  <- quantile(age.iqr, probs = c(.0649002)) %>%
                   names() %>%
                   str_remove('\\%$') %>%
                   {as.numeric(.) / 100} %>%
                   round(digits = 3) %>%
                   {str_remove(as.character(.), '^0{1}')}

# manually try out quantiles for age
iqr.significant <- quantile(sct.iqr, probs = c(.05))
sct.iqr.signif  <- quantile(sct.iqr, probs = c(.01)) %>%
                   names() %>%
                   str_remove('\\%$') %>%
                   {as.numeric(.) / 100} %>%
                   round(digits = 3) %>%
                   {str_remove(as.character(.), '^0{1}')}

### produce random assignment graphs for age variable
# build mean plot
ggplot() +
  geom_histogram(aes(x = age.mean),
    bins = 25, fill = 'grey63', alpha = .5, color = 'black') +
  scale_x_continuous(breaks = seq(15, 75, 5)) +
  scale_y_continuous(breaks = seq(0, 80, 10)) +
  geom_col(aes(x = empirical.age.mean, y = 85), color = 'black') +
  geom_segment(
    aes(x = age.ci[1], xend = age.ci[1], y = -5, yend = 85), size = 1) +
  geom_segment(
    aes(x = age.ci[2], xend = age.ci[2], y = -5, yend = 85), size = 1) +
  labs(y = 'Density', x = 'Simulated Mean of Candidate Age') +
  theme_bw() +
  theme(axis.title = element_text(size = 10),
        axis.title.y = element_text(margin = margin(r = 12)),
        axis.title.x = element_text(margin = margin(t = 12)),
        axis.text.y = element_text(size = 10, lineheight = 1.1, face = 'bold'),
        axis.text.x = element_text(size = 10, lineheight = 1.1, face = 'bold'),
        text = element_text(family = 'LM Roman 10'),
        panel.border = element_rect(color = 'black', size = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = 'grey79')
  )

# save plot
ggsave('age-mean.pdf', device = cairo_pdf, path = 'plots', dpi = 100,
       width = 7, height = 5)

# build mean plot
ggplot() +
  geom_histogram(aes(x = age.iqr),
    bins = 25, fill = 'grey63', alpha = .5, color = 'black') +
  scale_x_continuous(breaks = seq(5, 15, .5)) +
  scale_y_continuous(breaks = seq(0, 135, 15)) +
  geom_col(aes(x = empirical.age.iqr, y = 129), color = 'black', width = .07) +
  geom_text(aes(y = 129, x = empirical.age.iqr),
    label = paste0('p-value = ', age.iqr.signif) ,
        family = 'LM Roman 10', position = position_udge(x = .25, y = 3)) +
      labs(y = 'Density', x = 'Simulated Interquartie Range of Candidate Age') +
      theme_bw() +      theme(axis.title = element_text(size = 10),
            axis.ttle.y = element_text(margin = margin(r = 12)),
        axis.titlex = element_text(margin = margin(t = 12)),
        axis.text.y = element_text(size = 10, lineheight = 1.1, face = 'bold'),
        axis.text.x = element_text(size = 10, lineheight = 1.1, face = 'bold'),
        text = element_text(family = 'LM Roman 10'),
        sct.panerderif = element_rect(color = 'black', size = 1),
                  panel.grid.major  .x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.y = element_line(color = 'grey79')
                  )

# save plot
ggsave('iqr-age.pdf', device = cairo_pdf, path = 'plots', dpi = 100,
       width = 7, height = 5)

### produce random outcome graphs for sct.favorable variable
# build mean plot
ggplot() +
  geom_histogram(aes(x = sct.mean),
    bins = 25, fill = 'grey63', alpha = .5, color = 'black') +
  scale_x_continuous(breaks = seq(0, 1, .1)) +
  scale_y_continuous(breaks = seq(0, 55, 5)) +
  geom_col(aes(x = empirical.sct.mean, y = 55), color = 'black') +
  geom_segment(
    aes(x = sct.ci[1], xend = sct.ci[1], y = -5, yend = 85), size = 1) +
  geom_segment(
    aes(x = sct.ci[2], xend = sct.ci[2], y = -5, yend = 85), size = 1) +
  labs(y = 'Density', x = 'Simulated Mean of Candidate Age') +
  theme_bw() +
  theme(axis.title = element_text(size = 10),
        axis.title.y = element_text(margin = margin(r = 12)),
        axis.title.x = element_text(margin = margin(t = 12)),
        axis.text.y = element_text(size = 10, lineheight = 1.1, face = 'bold'),
        axis.text.x = element_text(size = 10, lineheight = 1.1, face = 'bold'),
        text = element_text(family = 'LM Roman 10'),
        panel.border = element_rect(color = 'black', size = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = 'grey79')
  )

# save plot
ggsave('age-mean.pdf', device = cairo_pdf, path = 'plots', dpi = 100,
       width = 7, height = 5)
