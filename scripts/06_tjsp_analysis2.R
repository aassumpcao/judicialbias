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
load('data/tjsp_simulation_age.Rda')
load('data/tjsp_simulation_out.Rda')
load('data/tjsp_simulation_net.Rda')
load('data/tjsp_simulation_out_autor1.Rda')
load('data/tjsp_simulation_out_autor2.Rda')
load('data/tjsp_simulation_out_reu1.Rda')
load('data/tjsp_simulation_out_reu2.Rda')
load('data/tjsp_randomcases_autor.Rda')
load('data/tjsp_randomcases_reu.Rda')

# set seed for prediction exercises
s <- 19910401
set.seed(s)

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
  ) %>%
  replace_na(list(candidate.elect = 1))

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

# create outcome variable for sct case
tjspAnalysis %<>%
  mutate(sct.favorable = case_when(
    case.claimant.win == 1 & str_detect(candidate.litigant.type,'Claimant') ~ 1,
    case.claimant.win == 0 & str_detect(candidate.litigant.type,'Defendant')~ 1
  )) %>%
  replace_na(list(sct.favorable = 0)) %>%
  mutate(
    case.defendant.win = car::recode(case.claimant.win, '1=0;0=1'),
    claimant.win.polit = ifelse(
      case.claimant.win == 1 & str_detect(candidate.litigant.type,'Claimant'),
      1, 0),
    defendant.win.polit = ifelse(
      case.defendant.win == 1 & str_detect(candidate.litigant.type,'Defendant'),
      1, 0)
  )

# create outcome for sct claim
tjspAnalysis$polit.net.claim <- tjspAnalysis %$%
  case_when(
    case.claimant.win == 1 &
      str_detect(candidate.litigant.type, 'Claimant')  ~ case.claim,
    case.claimant.win == 1 &
      str_detect(candidate.litigant.type, 'Defendant') ~ -1*case.claim,
    case.claimant.win == 0 &
      str_detect(candidate.litigant.type, 'Claimant')  ~ 0,
    case.claimant.win == 0 &
      str_detect(candidate.litigant.type, 'Defendant') ~ 0
  )

### tables and analysis
# produce summary statistics table (case level)
stargazer(

  # summmary table
  tjspAnalysis %>%
    select(case.duration, case.claim, sct.favorable) %>%
    as.data.frame(),

  # table cosmetics
  type = 'text',
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
  type = 'text',
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
  type = 'text',
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
# create the statistics on the number of judges per court and the number of
# cases per judge.
judges_per_court <- tjspAnalysis %>%
  group_by(tjsp.ID) %>%
  count(case.judge) %>%
  mutate(number.judges = row_number()) %>%
  summarize(number.judges = max(number.judges)) %>%
  select(number.judges) %>%
  unlist() %>%
  mean()
cases_per_judge <- tjspAnalysis %>%
  count(case.judge) %>%
  select(n) %>%
  unlist() %>%
  mean()

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

# create function to produce simulations
produceSimulations <- function(ds, var, return = 'simulation') {

  # check if variable requires change in the denominator
  check_var <- str_extract(var, '^(claimant|defendant)')
  # if variable requires change in denominator, filter dataset
  if (!is.na(check_var)) {check_var <- str_to_title(check_var)}
  # check if candidate.litigant.type exists
  if ('candidate.litigant.type' %in% names(ds) & !is.na(check_var)){
    df <- filter(ds, str_detect(candidate.litigant.type, check_var))
  } else {
    df <- ds
  }

  # 1. compute the number of cases by judge per court
  countJudges <- function(dataset = df){
    # 1. count the number of cases per judge, 2. merge on the same dataset to
    #  recover court ID, and 3. throw away repeated observations
    cases <- count(dataset, case.judge) %>%
             left_join(dataset, 'case.judge') %>%
             select(1:2, tjsp.ID) %>%
             mutate(tjsp.ID = as.character(tjsp.ID)) %>%
             distinct(case.judge, .keep_all = TRUE)
    return(cases)
  }

  # 2. group cases by court to create sample from which to pull observations
  sampleCourts <- function(df, court, n, var, ...){
    # create sliced dataset from court ID
    df <- filter(df, tjsp.ID == court) %>% select(var)
    # draw the number of observations from dataset
    sim <- sample_n(df, as.integer(n), ...)
    # assign new name to variables
    sim <- unname(unlist(sim))
    # return call
    return(sim)
  }

  # 4. create function to execute monte carlo simulations
  simulateVar <- function(dataset1 = df, dataset2, var, nsim = 1000){
    # return object
    object <- tibble(.rows = nrow(dataset1))
    # execute loop creating monte carlo simulation for covariate age
    for (x in 1:nsim) {
      # create vector containing simulation results
      simulations <- c()
      # execute loop to create simulations
      for (i in 1:nrow(dataset2)){
        # create list of arguments
        args <- list(
          dataset1, as.character(dataset2[i, 'tjsp.ID']),
          dataset2[i, 'n'], var, replace = TRUE
        )
        # call to sampleCourts using args from cases
        row <- do.call(sampleCourts, args)
        # bind onto vector
        simulations <- c(simulations, row)
      }
      # convert to numeric
      simulations <- as.numeric(simulations)
      # extract mean and iqr
      object <- bind_cols(object, sim = simulations)
      # print progress
      if (x %% 100 == 0) {print(x)}
    }
    return(object)
  }

  # execute internal functions to compute number of cases and simulations
  if (return == 'simulation') {
    object <- simulateVar(dataset2 = countJudges(), var = var)
  } else {
    object <- countJudges()
  }

  # return call
  return(object)
}

# run simulations for each variable:
# 1. candidate.age:    i want to prove random allocation of politician cases
# 2. sct.favorable:    i want to prove that outcomes for politicians are not
#                      random
# 3. polit.net.claim:  i want to prove that claim outcomes are not random
# 4. autor.favorable:  i want to prove that outcomes don't change if politician
#                      is claimant
# 5. reu.favorable:    i want to prove that outcomes don't change if politician
#                      is defendant
# 6. random.claimant:
# 7. random.defendant:

# # create simulated datasets
# # 1. simulation of random assignment using politician.age
# simulation_age <- produceSimulations(tjspAnalysis, 'candidate.age')

# # 2. simulation of politician outcome using sct decision favorable
# simulation_out <- produceSimulations(tjspAnalysis, 'sct.favorable')

# # 3. simulation of politician outcome using sct decision favorable
# simulation_net <- produceSimulations(tjspAnalysis, 'polit.net.claim')

# # 4. simulation of politician outcome when politician is claimant
# simulation_out_autor1 <- produceSimulations(tjspAnalysis, 'claimant.win.polit')
# simulation_out_autor2 <- produceSimulations(tjspAnalysis, 'case.claimant.win')

# # 5. simulation of politician outcome when politician is defendant
# simulation_out_reu1 <- produceSimulations(tjspAnalysis, 'defendant.win.polit')
# simulation_out_reu2 <- produceSimulations(tjspAnalysis, 'case.defendant.win')

# # 6. simulation of politician outcome when politician is claimant
# randomcases_autor <- produceSimulations(tjspAnalysisRandom, 'claimant.win')

# # 7. simulation of politician outcome when politician is defendant
# randomcases_reu <- produceSimulations(tjspAnalysisRandom, 'defendant.win')

# # save datasets
# save(simulation_age,        file = 'data/tjsp_simulation_age.Rda')
# save(simulation_out,        file = 'data/tjsp_simulation_out.Rda')
# save(simulation_net,        file = 'data/tjsp_simulation_net.Rda')
# save(simulation_out_autor1, file = 'data/tjsp_simulation_out_autor1.Rda')
# save(simulation_out_autor2, file = 'data/tjsp_simulation_out_autor2.Rda')
# save(simulation_out_reu1,    file = 'data/tjsp_simulation_out_reu1.Rda')
# save(simulation_out_reu2,    file = 'data/tjsp_simulation_out_reu2.Rda')
# save(randomcases_autor,     file = 'data/tjsp_randomcases_autor.Rda')
# save(randomcases_reu,       file = 'data/tjsp_randomcases_reu.Rda')

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

# create cases variables
cases1 <- produceSimulations(tjspAnalysis, 'candidate.age', 'judges')
cases2 <- produceSimulations(tjspAnalysis, 'sct.favorable', 'judges')
cases3 <- produceSimulations(tjspAnalysis, 'polit.net.claim', 'judges')
cases4 <- produceSimulations(tjspAnalysis, 'claimant.win.polit', 'judges')
cases5 <- produceSimulations(tjspAnalysis, 'case.claimant.win', 'judges')
cases6 <- produceSimulations(tjspAnalysis, 'defendant.win.polit', 'judges')
cases7 <- produceSimulations(tjspAnalysis, 'case.defendant.win', 'judges')
cases8 <- produceSimulations(tjspAnalysisRandom, 'claimant.win', 'judges')
cases9 <- produceSimulations(tjspAnalysisRandom, 'defendant.win', 'judges')

# calculate moments for all five simulations
s.politicians.age    <- calculateMoments(cases1, simulation_age)
s.politicians.win    <- calculateMoments(cases2, simulation_out)
s.politicians.net    <- calculateMoments(cases3, simulation_net)
s.politicians.autor1 <- calculateMoments(cases4, simulation_out_autor1)
s.politicians.autor2 <- calculateMoments(cases5, simulation_out_autor2)
s.politicians.reu1   <- calculateMoments(cases6, simulation_out_reu1)
s.politicians.reu2   <- calculateMoments(cases7, simulation_out_reu2)
s.randomcases.autor  <- calculateMoments(cases8, randomcases_autor)
s.randomcases.reu    <- calculateMoments(cases9, randomcases_reu)

# 4. calculate moments for empirical distribution
calculateEmpiricalMoments <- function(dataset, variable){

  # check if variable requires change in the denominator
  check_var <- str_extract(variable, '^(claimant|defendant)')
  # if variable requires change in denominator, filter dataset
  if (!is.na(check_var)) {check_var1 <- str_to_title(check_var)}
  # check if candidate.litigant.type exists
  if ('candidate.litigant.type' %in% names(dataset) & !is.na(check_var)){
    df <- filter(dataset, str_detect(candidate.litigant.type, check_var1))
  } else {
    df <- dataset
  }

  # narrow down dataset to age and sct outcome variables
  df %<>%
    group_by(case.judge) %>%
    select(case.judge, variable)

  # extract moment distributions from empirical dataset (age)
  empirical.iqr <- df %>%
    summarize_all(mean) %>%
    summarize_all(IQR) %>%
    select(-case.judge) %>%
    unlist()

  # extract moment distributions from empirical dataset (sct favorable)
  empirical.mean <- df %>%
    summarize_all(mean) %>%
    select(-case.judge) %>%
    lapply(mean) %>%
    unlist()

  # return call
  return(list(iqr = empirical.iqr, mean = empirical.mean))
}

# create arguments for calculateEmpiricalMomentsfunction call
randomcases_args <- c('claimant.win', 'defendant.win')
politician_args  <- c(
  'candidate.age', 'sct.favorable', 'claimant.win.polit', 'defendant.win.polit',
  'polit.net.claim', 'case.claimant.win', 'case.defendant.win'
)

# calculate moments for all five simulations
empirical.politicians <- mapply(
  calculateEmpiricalMoments, politician_args,
  MoreArgs = list(dataset = tjspAnalysis)
)
empirical.randomcases <- mapply(
  calculateEmpiricalMoments, randomcases_args,
  MoreArgs = list(dataset = tjspAnalysisRandom)
)

# 5. create function to plot simulation iqr and mean graphs
graphDistribution <- function(x, y, bins = 25, legend = 'IQR', save = FALSE,
  name = NULL, round = FALSE){

  # initiate plot object
  p <- ggplot() +
    geom_histogram(
      aes(x = x, fill = 'grey79'), bins = bins, alpha = .5, color = 'black'
    )

    # define break parameters
  if (round == TRUE) {round <- 3; prop <- 1.5}
  else               {round <- 1; prop <- 1.2}

  # define graphical parameters for y
  y_min  <- 0
  y_max  <- prop * max(ggplot_build(p)$data[[1]]['y'])
  y_incr <- round((y_max - y_min) / 10, 0)
  y_col  <- y_max - y_incr

  # define graphical parameters for x
  x_max  <- max(ggplot_build(p)$data[[1]]['x'])
  x_min  <- min(ggplot_build(p)$data[[1]]['x'])
  x_incr <- round((x_max - x_min) / 5, 3)
  width  <- ggplot_build(p)$data[[1]] %>%
            {.['xmax'] - .['xmin']} %>%
            unlist() %>%
            unname() %>%
            {.[1]}

  # define label parameters
  signif  <- quantile(x, probs = .05)
  pvalue  <- ecdf(x)
  x_value <- pvalue(y)
  IQR_value <- as.character(round(y, 3))
  IQR_value <- ifelse(nchar(IQR_value) > 7, str_sub(IQR_value, 1, 6), IQR_value)
  label <- paste0(legend, ' = ', IQR_value, '\n p-value = ', round(x_value, 3))
  label <- str_replace_all(label, '(\\b0\\.)', '\\.')
  x_label_pos <- ifelse(x_value < .4, .7 * x_incr, -.7 * x_incr)

  # set breaks based on round
  x_breaks <- round(seq(x_min, x_max, x_incr), round)
  y_breaks <- round(seq(y_min, y_max, y_incr), round)

  # finish graph
  p <- p +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(breaks = y_breaks) +
    geom_col(
      aes(x = y, y = y_col, fill = 'grey25'), color = 'black', width = width
    ) +
    geom_text(
      aes(x = y, y = y_col), label = label, family = 'LM Roman 10',
      position = position_nudge(x = x_label_pos, y = .25 * y_incr)
    ) +
    scale_fill_manual(
      name = element_blank(), values = c('grey25', 'grey79'),
      labels = paste0(c('Empirical ', 'Simulated '), legend)
    ) +
    labs(y = 'Frequency', x = paste0('Simulated ', legend)) +
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
p
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

# print matrices
empirical.politicians;empirical.randomcases

# produce plots for comparison of politician wins
# 1. random distribution of cases using candidate.age iqr
graphDistribution(
  s.politicians.age$iqr,
  empirical.politicians[['iqr', 'candidate.age']],
  name = 'age-iqr-politicians'
)

# 2. pro-politician bias using sct.favorable iqr
graphDistribution(
  s.politicians.win$iqr,
  empirical.politicians[['iqr', 'sct.favorable']],
  name = 'win-iqr-politicians'
)

# 3. pro-politician bias using sct.favorable mean
# plot graph
graphDistribution(
  simulation_out %>%
    summarize_all(mean) %>%
    unlist() %>%
    unname(),
  empirical.politicians[['mean', 'sct.favorable']], round = TRUE,
  legend = 'Mean', name = 'win-mean-politicians'
)

# # create boxplot
# p <- bind_rows(

#   # aggregate first dataset to only show results for claimant politicians
#   tjspAnalysis %>%
#     group_by(case.judge) %>%
#     summarize(sct.favorable = mean(sct.favorable)) %>%
#     transmute(group = 'SCT Favorable: Empirical', win = sct.favorable),

#   # aggregate second dataset to show results by judge
#   runif(514,0, 1) %>%
#     {tibble(group = 'SCT Favorable: Simulated', win = .)}
#   ) %>%
#   mutate(
#     group = factor(group, levels = c(
#       'SCT Favorable: Empirical', 'SCT Favorable: Simulated')
#     )
#   ) %>%
#   ggplot(aes(x = group, y = win, fill = group)) +
#   scale_y_continuous(breaks = seq(0, 1, .125)) +
#   scale_fill_manual(breaks = NULL, values = c('grey54', 'grey79')) +
#   geom_boxplot(width = .5) +
#   labs(y = element_blank(), x = element_blank()) +
#   theme_bw() +
#   theme(
#     axis.title = element_text(size = 10),
#     axis.title.y = element_text(margin = margin(r = 12)),
#     axis.title.x = element_blank(),
#     axis.text.y = element_text(size = 10, lineheight = 1.1, face = 'bold'),
#     axis.text.x = element_text(size = 10, lineheight = 1.1, face = 'bold'),
#     text = element_text(family = 'LM Roman 10'),
#     panel.border = element_rect(color = 'black', size = 1),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.y = element_line(color = 'grey79'),
#     panel.grid.minor.y = element_line(color = 'grey79')
#   )

# # save plot
# ggsave(
#   filename = 'sct-favorable-boxplot.pdf', plot = p, device = cairo_pdf,
#   path = 'plots', dpi = 100, width = 7, height = 5
# )

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
    (voto.secao / voto.total) - (election.votes / voto.total)
  ))

# save to disk
save(tjspElected, file = 'data/tjspElected.Rda')

# different bandwidths for which we want to test everything
bws <- c(.40, .35, .30, .25, .20, .15, .10, .0842, .05, .01)

# create dataset for rd regressions
rdData <- tjspElected %>%
  filter(office.ID == 11) %>%
  filter(case.lastupdate > rd.date) %>%
  mutate(treatment = ifelse(election.share > 0, 1, 0))
rdData %<>% replace_na(
  list(
    election.share = median(rdData$election.share, na.rm = TRUE),
    treatment = median(rdData$treatment, na.rm = TRUE)
  ))

# run rd regressions for different bandwidths
rdEstimates <- bws %>%
  lapply(function(x){
    rdData %$%
      rdrobust::rdrobust(y = sct.favorable, x = election.share, h = x) %>%
      {c(estimate = unname(.$Estimate[1, 1]), pvalue = .$pv[1, 1],
        n = sum(.$Nh), .$ci[1,], bws = .$bws[1, 1]
      )}
  })

# bind into different datasets
rdResults <- tibble()
rdResults <- lapply(rdEstimates, bind_rows, rdResults) %>%
             {bind_rows(rdResults, .)}

# build point estimate graphs
p <- rdResults %>%
  ggplot() +
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
  plot = p, 'rd-bws.pdf', device = cairo_pdf, path = 'plots', dpi = 100,
  width = 7, height = 5
)

# build point estimate graphs
rdData$sct.predicted <- predict(
  loess(
    sct.favorable ~ election.share + treatment + election.share^2 +
    treatment * election.share, data = rdData, family = 'gaussian'
  )
)

# create rd plot
p <- rdData %>%
  ggplot() +
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
    name = 'Politician Status:', breaks = c('FALSE','TRUE'),
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
  plot = p, 'rd-plot.pdf', device = cairo_pdf, path = 'plots', dpi = 100,
  width = 7, height = 5
)

### sample representativeness

### tables and analysis
# panel A
# produce summary statistics table (case level)
stargazer(

  # summmary table
  tjspAnalysisRandom %>%
    select(case.duration, case.claim, claimant.win) %>%
    as.data.frame(),

  # table cosmetics
  type = 'text',
  title = 'Descriptive Statistics',
  style = 'default',
  summary = TRUE,
  # out = 'tables/sumstats_random.tex',
  out.header = FALSE,
  covariate.labels = c(covarLabel[1:2], 'Claimant Win Rate'),
  align = FALSE,
  digit.separate = 3,
  digits = 3,
  digits.extra = 3,
  font.size = 'scriptsize',
  header = FALSE,
  initial.zero = FALSE,
  model.names = FALSE,
  label = 'tab:sumstats_random',
  no.space = FALSE,
  table.placement = '!htbp',
  summary.logical = TRUE,
  summary.stat = c('n', 'mean', 'sd', 'min', 'max')
)

# produce summary statistics table (judge level)
stargazer(

  # summary table
  tjspAnalysisRandom %>%
    select(case.judge, judge.gender, judge.tenure, judge.pay) %>%
    group_by(case.judge) %>%
    filter(row_number() == 1) %>%
    as.data.frame(),

  # table cosmetics
  type = 'text',
  title = 'Descriptive Statistics',
  style = 'default',
  summary = TRUE,
  # out = 'tables/sumstats_random.tex',
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
  label = 'tab:sumstats_random',
  no.space = FALSE,
  table.placement = '!htbp',
  summary.logical = TRUE,
  summary.stat = c('n', 'mean', 'sd', 'min', 'max')
)

# panel B
# case-level covariates
case.level.politician <- tjspAnalysis %>%
  select(
    case.duration, case.claim, claimant.win = case.claimant.win,
    defendant.win = case.defendant.win, candidate.litigant.type
  ) %>%
  select(-candidate.litigant.type) %>%
  as.list()
case.level.random <- tjspAnalysisRandom %>%
  select(case.duration, case.claim, claimant.win, defendant.win) %>%
  as.list()

# judge-level covariates
judge.level.politician <- tjspAnalysis %>%
  select(case.judge, judge.gender, judge.tenure, judge.pay) %>%
  group_by(case.judge) %>%
  filter(row_number() == 1) %>%
  replace_na(list(judge.tenure = 3382)) %>%
  ungroup() %>%
  select(-case.judge) %>%
  as.list()
judge.level.random <- tjspAnalysisRandom %>%
  select(case.judge, judge.gender, judge.tenure, judge.pay) %>%
  group_by(case.judge) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(-case.judge) %>%
  as.list()

# create function to produce table of balance tests
balanceTest <- function(list1, list2) {
  # loop to run t.tests per element in list
  for (i in seq(1, length(list1))) {
    var1 <- unlist(list1[i])
    var2 <- unlist(list2[i])
    output <- t.test(var1, var2)
    row <- output %>%
      {list(.$estimate[1], .$estimate[2], .$estimate[1] - .$estimate[2],
            .$statistic, .$p.value)} %>%
      unlist() %>%
      round(3) %>%
      as.character() %>%
      str_replace_all('(\\b0\\.)([0-9]{3,3}\\b)', '\\.\\2') %>%
      str_replace_all('(\\b[0-9]{3,5})(\\.)(.)*', '\\1')
    names(row) <- c('v1', 'v2', 'v3', 'v4', 'v5')
    if (i == 1) {table <- bind_rows(row)}
    else {table <- bind_rows(table, row)}
  }
  return(table)
}

# produce both tables
table1 <- balanceTest(case.level.politician, case.level.random)
table2 <- balanceTest(judge.level.politician, judge.level.random)

# merge tables and print them. insert numbers into final table
bind_rows(table1, table2) %>%
xtable::xtable()

# create function to compare all units of both samples
compareToRandom <- function(x, y) {
  # perform t-test on two distributions of court outcomes
  row <- t.test(x, y)
  # extract statistics from t-test
  output <- row %$%
    c(estimate[1], estimate[2], estimate[1] - estimate[2], statistic, p.value)
  # assign names to vector
  names(output) <- c('politicians', 'random', 'diff', 't-stat', 'p-value')
  return(output)
}

# compare all claimants in politician sample vs. all claimants in random sample
length(s.politicians.autor2$mean);length(s.randomcases.autor$mean);
length(s.politicians.autor1$mean);length(s.randomcases.autor$mean);
length(s.politicians.reu2$mean);length(s.randomcases.reu$mean);
length(s.politicians.reu1$mean);length(s.randomcases.reu$mea)

bind_rows(
  compareToRandom(s.politicians.autor2$mean, s.randomcases.autor$mean),
  compareToRandom(s.politicians.autor1$mean, s.randomcases.autor$mean),
  compareToRandom(s.politicians.reu2$mean, s.randomcases.reu$mean),
  compareToRandom(s.politicians.reu1$mean, s.randomcases.reu$mean)
) %>%
xtable::xtable(digits = 3)

# # 8. pro-politician bias using claimant.win.polit mean
# graphDistribution(
#   s.randomcases.autor$mean,
#   s.politicians.autor1$mean %>% mean(),
#   legend = 'Mean', round = TRUE,
#   name = 'win-mean-politicians-claimant'
# )

# # 9. pro-politician bias using defendant.win.polit iqr
# graphDistribution(
#   s.randomcases.reu$mean,
#   s.politicians.reu1$mean %>% mean(),
#   legend = 'Mean', round = TRUE,
#   name = 'win-mean-politicians-defendant'
# )
# ### difference-in-differences analysis
# # create time and treatment variables
# tjspElected$time  <- tjspElected %$% ifelse(case.lastupdate > rd.date, 1, 0)
# tjspElected$treat <- tjspElected %$% ifelse(candidate.elect == 1, 1, 0)

# # run test regression
# lfe::felm(sct.favorable ~ time + treat + time * treat + case.duration +
#   case.claim + judge.pay + candidate.age + candidate.experience +
#   candidate.expenditure + candidate.ethnicity + candidate.gender +
#   candidate.education + candidate.maritalstatus |
#   ibge.ID + election.ID + party.number + case.subject | 0 |
#   case.judge + tjsp.ID, data = tjspElected, exactDOF = TRUE
# ) %>%
# summary()

# # there is no effect of election on court outcomes

# remove everything for serial sourcing
rm(list = ls())
