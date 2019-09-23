### judicial favoritism of politicians
#  this script produces paper analysis
# andre assumpcao and julio trecenti
# email: andre.assumpcao@gmail.com
# email: julio.trecenti@gmail.com

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
load('data/tjspAnalysis.Rda')
load('data/tjspAnalysisRandom.Rda')
load('data/tjspSimulation1.Rda')
load('data/tjspSimulation2.Rda')
load('data/tjspSimulation1Random.Rda')
load('data/tjspSimulation2Random.Rda')
load('data/tjspSimulation1ClaimantWin.Rda')
load('data/tjspSimulation1DefendantWin.Rda')
load('data/tjspSimulation2DefendantWin.Rda')

# rename objects
# the distribution of covariates per judge for politicians and random cases
s.politician.age <- simulated.mean
s.politician.win <- outcome.mean
s.claimant.win01 <- outcome.mean.claimant.win

# the distribution of outcomes per judge (pro-politician ruling and claimant
# win in cases involving politicians and random cases)
s.claimant.sex00 <- simulated.mean.random
s.claimant.win02 <- outcome.mean.random

# remove old objects
rm(list = objects(pattern = '(simulated|outcome\\.)'))

### define variable labels
# outcome label
outcomeLabel <- 'SCT Favorable Outcome'

# create list of covariates
covariates <- c(
  'case.duration', 'case.claim', 'judge.gender', 'judge.pay', 'judge.tenure',
  'candidate.age', 'candidate.gender', 'candidate.experience',
  'candidate.expenditure', 'candidate.elected', 'candidate.education',
  'candidate.maritalstatus'
)
covarLabel <- c(
  'Case Duration (in days)', 'Amount Claimed (in R$)', 'Male', 'Pay (in R$)',
  'Tenure (in days)', 'Age', 'Male', 'Political Experience',
  'Campaign Expenditures (in R$)', 'Elected to Office', 'Level of Education',
  'Marital Status'
)

# define variable types for analysis
integers <- c(
  'case.duration', 'case.claim', 'case.claimant.win', 'judge.pay',
  'candidate.age', 'candidate.male', 'candidate.experience',
  'candidate.expenditure'
)
factors <- c(
  'case.subject', 'case.judge', 'tjsp.ID', 'ibge.ID', 'election.ID',
  'candidate.ethnicity', 'candidate.gender', 'candidate.occupation',
  'candidate.education', 'candidate.maritalstatus', 'candidacy.situation',
  'party.number', 'party.coalition'
)

### wrangle variables one final time
# change variable types
tjspAnalysis %<>%
  mutate(case.claim = str_replace_all(case.claim, '\\.|R\\$', '')) %>%
  mutate(case.claim = str_replace_all(case.claim, '\\,', '.') %>% trimws()) %>%
  mutate_at(vars(integers), as.integer) %>%
  mutate_at(vars(factors), as.factor) %>%
  mutate(judge.gender = as.integer(ifelse(judge.gender == 'Female', 0, 1))) %>%
  mutate(
    candidate.gender = as.integer(ifelse(candidate.gender == 'FEMININO', 0, 1)),
  )

# do the same for random cases
integers <- c('case.claim', 'judge.pay', 'judge.tenure')
factors  <- c('case.subject', 'case.judge', 'tjsp.ID', 'ibge.ID')

# wrangle random cases data
tjspAnalysisRandom %<>%
  mutate(case.claim = str_replace_all(case.claim, '\\.|R\\$', '')) %>%
  mutate(case.claim = str_replace_all(case.claim, '\\,', '.') %>% trimws()) %>%
  mutate_at(vars(integers), as.integer) %>%
  mutate_at(vars(factors), as.factor) %>%
  mutate_at(
    vars(claimant.sex, defendant.sex, clawyers.sex, dlawyers.sex, judge.gender),
    ~as.integer(ifelse(. == 'Female', 0, 1))
  )

# filling in missing values for analysis (always median)
missing.claims <- median(as.numeric(tjspAnalysis$case.claim), na.rm = TRUE)
tjspAnalysis[is.na(tjspAnalysis$case.claim), 'case.claim'] <- missing.claims

# fix claims with errors (> 40,000)
tjspAnalysis %<>%
  mutate(case.claim = case.claim %>% {ifelse(. > 40000, missing.claims, .)})

# filling in missing values for analysis (always median) for random cases
missing.claims <- median(as.numeric(tjspAnalysisRandom$case.claim), na.rm = T)
missing.claims -> tjspAnalysisRandom[
  is.na(tjspAnalysisRandom$case.claim), 'case.claim'
]

# fix claims with errors (> 40,000)
tjspAnalysisRandom %<>%
  mutate(case.claim = case.claim %>% {ifelse(. > 40000, missing.claims, .)}) %>%
  mutate(defendant.win = car::recode(claimant.win, '1=0;0=1'))

# fix median tenure time
missing.tenure <- median(as.numeric(tjspAnalysisRandom$judge.tenure), na.rm = T)
missing.tenure -> tjspAnalysisRandom[
  is.na(tjspAnalysisRandom$judge.tenure), 'judge.tenure'
]

# create outcome variable for case
tjspAnalysis %<>%
  mutate(sct.favorable = case_when(
    case.claimant.win == 1 & str_detect(candidate.litigant.type,'Claimant') ~ 1,
    case.claimant.win == 0 & str_detect(candidate.litigant.type,'Defendant')~ 1
  )) %>%
  replace_na(list(sct.favorable = 0)) %>%
  mutate(case.defendant.win = car::recode(case.claimant.win, '1=0;0=1'))

### tables and analysis
# produce summary statistics table (case level)
stargazer(

  # summmary table
  tjspAnalysis %>%
    select(case.duration, case.claim, sct.favorable) %>%
    as.data.frame(),

  # table cosmetics
  type = 'latex',
  title = 'Descriptive Statistics',
  style = 'default',
  summary = TRUE,
  # out = 'tables/sumstats.tex',
  out.header = FALSE,
  covariate.labels = c(covarLabel[1:2], 'Pro-Politician Ruling'),
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

# produce summary statistics table (judge level)
stargazer(

  # summary table
  tjspAnalysis %>%
    select(case.judge, judge.gender, judge.tenure, judge.pay) %>%
    group_by(case.judge) %>%
    filter(row_number() == 1) %>%
    replace_na(list(judge.tenure = 3382)) %>%
    as.data.frame(),

  # table cosmetics
  type = 'latex',
  title = 'Descriptive Statistics',
  style = 'default',
  summary = TRUE,
  # out = 'tables/sumstats.tex',
  out.header = FALSE,
  covariate.labels = covarLabel[c(3,5,4)],
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

# produce summary statistics table
stargazer(

  # summary table
  tjspAnalysis %>%
    select(
      candidate.ssn, candidate.age, candidate.gender, candidate.experience,
      candidate.elect, candidate.expenditure
    ) %>%
    group_by(candidate.ssn) %>%
    summarize(
      candidate.age = first(candidate.age),
      candidate.gender = first(candidate.gender),
      candidate.experience = max(candidate.experience),
      candidate.elect = mean(as.integer(candidate.elect)),
      candidate.expenditure = mean(candidate.expenditure)
    ) %>%
    as.data.frame(),

  # table cosmetics
  type = 'latex',
  title = 'Descriptive Statistics',
  style = 'default',
  summary = TRUE,
  # out = 'tables/sumstats.tex',
  out.header = FALSE,
  covariate.labels = covarLabel[c(6:8, 10, 9)],
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
countJudges <- function(dataset) {
  # 1. count the number of cases per judge, 2. merge on the same dataset to
  #  recover court ID, and 3. throw away repeated observations
  count(dataset, case.judge) %>%
  left_join(dataset, 'case.judge') %>%
  select(1:2, tjsp.ID) %>%
  mutate(tjsp.ID = as.character(tjsp.ID)) %>%
  distinct(case.judge, .keep_all = TRUE) %>%
  return()
}

# 2. group cases by court to create sample from which to pull observations
sampleCourts <- function(dataset, court, n, var, ...) {
  # create sliced dataset from court ID
  df <- filter(dataset, tjsp.ID == court) %>% select(var)
  # draw the number of observations from dataset
  sim <- sample_n(df, as.integer(n), ...)
  # assign new name to variables
  sim <- unname(unlist(sim))
  # return call
  return(sim)
}

# 3. create objects using the countJudges function
cases <- countJudges(tjspAnalysis)
casesRandom <- countJudges(tjspAnalysisRandom)

# # 4. create function to execute with monte carlo simulations for covariate age
# object1 <- tibble(.rows = nrow(tjspAnalysis))
# object2 <- tibble(.rows = nrow(tjspAnalysisRandom))

# # we have to run this loop three times to produce five datasets:
# # first run:
# #   tjspSimulation1.Rda
# #   tjspSimulation2.Rda
# # second run:
# #   tjspSimulation1Random.Rda
# #   tjspSimulation2Random.Rda
# # third run:
# #   tjspSimulation2ClaimantWin.Rda

# # execute loop creating monte carlo simulation for covariate age
# for (x in 1:1000) {
#   # create vector containing simulation results
#   simulats <- c()
#   outcomes <- c()
#   # execute loop to create simulations
#   for (i in 1:nrow(casesRandom)) {
#     # create list of arguments
#     args <- list(
#       tjspAnalysisRandom, as.character(casesRandom[i, 'tjsp.ID']),
#       casesRandom[i, 'n'], 'claimant.sex', replace = TRUE
#     )
#     # call to sampleCourts using args from cases
#     row <- do.call(sampleCourts, args)
#     # replace list elements
#     args[[4]] <- 'defendant.win'
#     # call to sampleCourts to build outcome
#     outcome <- do.call(sampleCourts, args)
#     # bind onto vector
#     simulats <- c(simulats, row)
#     outcomes <- c(outcomes, outcome)
#   }
#   # convert to numeric
#   simulats <- as.numeric(simulats)
#   outcomes <- as.integer(outcomes)
#   # extract mean and iqr
#   object1 <- bind_cols(object1, sim = simulats)
#   object2 <- bind_cols(object2, sim = outcomes)
#   # print progress
#   if (x %% 100 == 0) {print(x)}
# }

# # save dataset
# object1 -> simulated.mean
# object2 -> s.defendant.win02
# save(simulated.mean,      file = 'data/tjspSimulation1.Rda')
# save(s.defendant.win02,   file = 'data/tjspSimulation2DefendantWin.Rda')

# 3. calculate moments of simulated distribution and return them in a list
calculateMoments <- function(judges, simulation){
  # uncount variables
  ids <- uncount(judges, n)
  # bind to simulation
  simulation %<>% bind_cols(ids) %>% select(1001, 1:1000)
  # summarize simulation to judge means
  simulation %<>% group_by(case.judge) %>% summarize_all(mean) %>% select(-1)
  # extract moment distributions from simulated and empirical datasets
  simulation.mean <- simulation %>%
    t() %>%
    as_tibble(.name_repair = 'universal') %>%
    summarize_all(mean) %>%
    unlist() %>%
    unname()
  # extract moment distributions from simulated datasets
  simulation.ci <- quantile(simulation.mean, probs = c(.05, .95))
  # compute the 1,000 iqr for both distributions
  simulation.iqr <- summarize_all(simulation, IQR) %>% unlist() %>% unname()
  # return list with simulation moments
  obj <- list(ci = simulation.ci, iqr = simulation.iqr, mean = simulation.mean)
  # return iqr distribution
  return(obj)
}

# calculate moments for all five simulations
s.politician.age <- calculateMoments(cases,       s.politician.age)
s.politician.win <- calculateMoments(cases,       s.politician.win)
s.claimant.win01 <- calculateMoments(cases,       s.claimant.win01)
s.claimant.sex00 <- calculateMoments(casesRandom, s.claimant.sex00)
s.claimant.win02 <- calculateMoments(casesRandom, s.claimant.win02)

# calculate moments for two simulations of defendant outcomes
s.defendant.win01 <- calculateMoments(cases,       s.defendant.win01)
s.defendant.win02 <- calculateMoments(casesRandom, s.defendant.win02)

# 4. calculate moments for empirical distribution
calculateEmpiricalMoments <- function(dataset, variables){
  # narrow down dataset to age and sct outcome variables
  dataset %<>% group_by(case.judge) %>% select(case.judge, variables)
  # extract moment distributions from empirical dataset (age)
  empirical.iqr <- dataset %>%
    select(case.judge, variables) %>%
    group_by(case.judge) %>%
    summarize_all(mean) %>%
    summarize_all(IQR) %>%
    select(-case.judge) %>%
    unlist()
  # extract moment distributions from empirical dataset (sct favorable)
  empirical.mean <- dataset %>%
    select(case.judge, variables) %>%
    group_by(case.judge) %>%
    summarize_all(mean) %>%
    select(-case.judge) %>%
    lapply(mean) %>%
    unlist()
  # return call
  return(list(iqr = empirical.iqr, mean = empirical.mean))
}

# create arguments for calculateEmpiricalMomentsfunction call
a <- list(
  tjspAnalysis,
  c('candidate.age', 'case.claimant.win', 'case.defendant.win', 'sct.favorable')
)
b <- list(
  filter(tjspAnalysis, str_detect(candidate.litigant.type, 'Defendant')),
  'case.defendant.win'
)
c <- list(tjspAnalysisRandom, c('claimant.win', 'claimant.sex','defendant.win'))

# calculate moments for all five simulations
empirical.politicians <- do.call(calculateEmpiricalMoments, a)
empirical.pdefendants <- do.call(calculateEmpiricalMoments, b)
empirical.randomcases <- do.call(calculateEmpiricalMoments, c)

# 5. create function to plot iqr and mean graphs
graphDistr <- function(x, y, width = .07, bins = 25, legend = 'IQR',
  save = FALSE, name = NULL) {
  # initiate plot object
  p <- ggplot() +
    geom_histogram(
      aes(x = x, fill = 'grey79'), bins = bins, alpha = .5, color = 'black'
    )
  # define graphical parameters for y
  y_min  <- 0
  y_max  <- max(ggplot_build(p)$data[[1]]['y']) + 10
  y_incr <- round((y_max - y_min) / 10, 0)
  y_col  <- y_max
  # define graphical parameters for x
  x_max  <- max(ggplot_build(p)$data[[1]]['x'])
  x_min  <- min(ggplot_build(p)$data[[1]]['x'])
  x_incr <- round((x_max - x_min) / 5, 2)
  # define label parameters
  signif  <- quantile(x, probs = .05)
  pvalue  <- ecdf(x)
  x_value <- pvalue(y)
  label   <- paste0('p-value = ', round(x_value, 3))
  # finish graph
  p <- p +
    scale_x_continuous(breaks = round(seq(x_min, x_max, x_incr), 1)) +
    scale_y_continuous(breaks = round(seq(y_min, y_max, y_incr), 1)) +
    geom_col(
      aes(x = y, y = y_col, fill = 'grey25'), color = 'black', width = width
    ) +
    geom_text(
      aes(y = y_col, x = y), label = label, family = 'LM Roman 10',
      position = position_nudge(y = y_incr / 2)
    ) +
    scale_fill_manual(
      name = element_blank(), values = c('grey25', 'grey79'),
      labels = paste0(c('Empirical ', 'Simulated '), legend)
    ) +
    labs(y = 'Density', x = paste0('Simulated ', legend)) +
    theme_bw() +
    theme(
      axis.title = element_text(size = 10),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.title.x = element_blank(),
      axis.text.y = element_text(size = 10, lineheight = 1.1, face = 'bold'),
      axis.text.x = element_text(size = 10, lineheight = 1.1, face = 'bold'),
      text = element_text(family = 'LM Roman 10'),
      panel.border = element_rect(color = 'black', size = 1),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = 'grey79'),
      legend.position = 'bottom'
    )
  # save plot if requested
  if (save == TRUE & !is.null(name)) {
    ggsave(
      filename = paste0(name, '.pdf'), plot = p, device = cairo_pdf,
      path = 'plots', dpi = 100, width = 7, height = 5
    )
  }
  # return plot
  return(p)
}

# produce plots for comparison of politician wins
graphDistr(
  s.politician.age$iqr,
  empirical.politicians$iqr['candidate.age'],
  # save = TRUE, name = 'age-iqr-politicians'
)
graphDistr(
  s.politician.win$iqr,
  empirical.politicians$iqr['sct.favorable'],
   width = .007, # save = TRUE, name = 'win-iqr-politicians'
)
graphDistr(
  s.claimant.sex00$iqr,
  empirical.randomcases$iqr['claimant.sex'],
   width = .004, # save = TRUE, name = 'sex-iqr-randomcases'
)
graphDistr(
  s.claimant.win02$iqr,
  empirical.randomcases$iqr['claimant.win'],
   width = .004, # save = TRUE, name = 'win-iqr-randomcases'
)
# produce plots for showing the result is coming from defendants
graphDistr(
  s.defendant.win01$iqr, empirical.pdefendants$iqr['case.defendant.win'],
   width = .004, # save = TRUE, name = 'win-iqr-politicians-defendants'
)
graphDistr(
  s.defendant.win02$iqr, empirical.randomcases$iqr['defendant.win'],
   width = .004, # save = TRUE, name = 'win-iqr-random-defendants'
)

### regression discontinuity analysis
# create vector of election dates
electionDate <- c('2004-10-03', '2008-10-05', '2012-10-07', '2016-10-02')

# create election dates variable, which assigns treatment to observations
tjspAnalysis$rd.date <- tjspAnalysis %>%
  {case_when(.$election.year == 2008 ~ electionDate[2],
             .$election.year == 2012 ~ electionDate[3],
             .$election.year == 2016 ~ electionDate[4]
  )} %>%
  as.Date(format = '%Y-%m-%d')

# split dataset to elected-candidates only and create running var
tjspElected <- tjspAnalysis %>%
  mutate_at(vars(voto.secao, voto.total, office.vacancies), as.integer) %>%
  mutate(election.votes = voto.total / office.vacancies) %>%
  mutate(election.share = ifelse(office.ID == 11,
    (voto.secao / election.votes) - .5,
    (voto.secao / election.votes) - (election.votes / election.votes)
  ))

# save to disk
save(tjspElected, file = 'data/tjspElected.Rda')

# different bandwidths for which we want to test everything
bws <- c(.40, .35, .30, .25, .20, .15, .10, .0842, .05, .01)

# create dataset for rd regressions
rdData <- filter(tjspElected, office.ID == 11 & case.lastupdate > rd.date) %>%
          mutate(treatment = ifelse(election.share > 0, 1, 0))
rdData %<>% replace_na(list(
  election.share = median(rdData$election.share, na.rm = TRUE),
  treatment = median(rdData$treatment, na.rm = TRUE))
)

# run rd regressions for different bandwidths
rdEstimates <- bws %>%
  lapply(function(x){
    rdData %$%
      rdrobust::rdrobust(y = sct.favorable, x = election.share, h = x) %>%
      {c(estimate = unname(.$Estimate[1, 1]), pvalue = .$pv[1, 1],
        n = sum(.$Nh), .$ci[1,], bws = .$bws[1,1]
      )}
  })

# bind into different datasets
rdResults <- tibble()
rdResults <- lapply(rdEstimates, bind_rows, rdResults) %>%
             {bind_rows(rdResults, .)}

# build point estimate graphs
ggplot(data = rdResults) +
  geom_point(aes(y = estimate, x = 1:10)) +
  geom_point(aes(y = unlist(rdResults[8,1]), x = 8), color = 'dodgerblue2') +
  geom_errorbar(
    aes(ymax = `CI Upper`, ymin = `CI Lower`, x = 1:10), width = .5
  ) +
  geom_errorbar(
    aes(ymax = unlist(rdResults[8,4]), ymin = unlist(rdResults[8,5]), x = 8),
    width = .5, color = 'dodgerblue2'
  ) +
  geom_text(aes(y = estimate, x = 1:10,
    label = format(round(estimate, 2), nsmall = 2)), family = 'LM Roman 10',
    nudge_x = .4, nudge_y = .03
  ) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'gray33') +
  scale_y_continuous(breaks = seq(-.25, .75, .125)) +
  scale_x_continuous(
    breaks = 1:10, labels = round(rdResults$bws, 2) %>%
      format(nsmall = 2) %>% paste0(' \n (n = ', rdResults$n, ')')
  ) +
  labs(y = 'Point Estimate', x = element_blank()) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 10),
    axis.title.y = element_text(margin = margin(r = 12)),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 10, lineheight = 1.1, face = 'bold'),
    axis.text.x = element_text(size = 9, lineheight = 1.1, face = 'bold'),
    text = element_text(family = 'LM Roman 10'),
    panel.border = element_rect(color = 'black', size = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = 'bottom'
  )

# save plot
ggsave(
  'rd-bws.pdf', device = cairo_pdf, path = 'plots', dpi = 100, width = 7,
  height = 5
)

# build point estimate graphs
rdData$sct.predicted <- predict(
  loess(
    sct.favorable ~ election.share + treatment + election.share^2 +
    treatment * election.share, data = rdData, family = 'gaussian'
  )
)

# create rd plot
ggplot(data = rdData) +
  stat_summary_bin(
    aes(election.share, sct.favorable, color = election.share > 0),
    geom = 'point', bins = 30
  ) +
  geom_smooth(
    aes(election.share, sct.favorable, color = TRUE), linetype = 'dashed',
    se = FALSE, size = .5
  ) +
  geom_smooth(aes(election.share, sct.predicted, color = election.share > 0)) +
  geom_vline(xintercept = 0, linetype = 'longdash') +
  scale_color_manual(
    name = 'Politician Condition:', breaks = c('FALSE','TRUE'),
    values = c('FALSE' = 'grey15', 'TRUE' = 'grey60'),
    labels = c('Lost Election', 'Won Election')
  ) +
  labs(y = 'Pro-Politican Ruling', x = 'Vote Share Centered at 50 percent') +
  lims(y = c(NA, 1), x = c(-.2, .2)) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 10),
    axis.title.y = element_text(margin = margin(r = 12)),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.text.y = element_text(size = 10, lineheight = 1.1, face = 'bold'),
    axis.text.x = element_text(size = 10, lineheight = 1.1, face = 'bold'),
    text = element_text(family = 'LM Roman 10'),
    panel.border = element_rect(color = 'black', size = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = 'top'
  )

# save plot
ggsave(
  'rd-plot.pdf', device = cairo_pdf, path = 'plots', dpi = 100, width = 7,
  height = 5
)

# remove everything for serial sourcing
rm(list = ls())
