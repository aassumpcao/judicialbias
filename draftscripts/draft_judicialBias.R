candidates.2010 %>% names()
candidates.2012 %>% names()
candidates.2016 %>% names()

candidates %>% names()

candidates %$% table(is.na(candidate.dob))


candidates %$% table(candidate.gender)

# join all elected candidates from 2012 and 2016 elections
candidates <- bind_rows(candidates.2012, candidates.2016) %>%
              filter(COD_SIT_TOT_TURNO %in% 1:3) %>%
              filter(SIGLA_UF == 'SP')

candidates %$% table(candidate.occupation)

candidatesSP2008 <- filter(candidates, election.year == 2008)
candidatesSP2012 <- filter(candidates, election.year == 2012)
candidatesSP2016 <- filter(candidates, election.year == 2016)

left_join(candidatesSP2012, candidatesSP2008, by = 'candidate.ssn')

# find repeated candidates
candidates.reelected2012 <- candidatesSP2012$candidate.ssn %>%
                            match(candidatesSP2008$candidate.ssn) %>%
                            subset(!is.na(.))

candidates.reelected2016 <- candidatesSP2016$candidate.ssn %>%
                            match(candidatesSP2012$candidate.ssn) %>%
                            subset(!is.na(.))

candidates2004 %>% names()
candidates2004 %$% table(DESC_SIT_TOT_TURNO, COD_SIT_TOT_TURNO)

# identify the reelected candidates in 2008.
candidates2004 <- filter(candidates.2010, ANO_ELEICAO == 2004) %>%
                  filter(COD_SIT_TOT_TURNO %in% c(1, 5))
candidates2008 <- filter(candidates.2010, ANO_ELEICAO == 2008) %>%
                  filter(COD_SIT_TOT_TURNO %in% c(1, 5))

candidates2012 <- filter(candidates.2012, ANO_ELEICAO == 2012)
candidates2016 <- filter(candidates.2016, ANO_ELEICAO == 2016)

candidates2008 %$% table(DESCRICAO_SEXO, CODIGO_SEXO)

candidates %$% table(candidacy.expenditures, election.year)

library(tidyverse)
library(magrittr)
library(feather)
library(readr)



# import packages
library(tidyverse)
library(magrittr)
library(feather)
library(readr)

# load data
load('data/sctSummary.Rda')
load('data/sctDetails.Rda')

sct <- read_csv('data/sct.csv')

# filter to sct cases
sctSummary %>%
  filter(class == 'Procedimento do Juizado Especial Cível') %>%
  View()

### electoral crime and performance paper
# main analysis script
#   this script produces all tables, plots, and analyses in the electoral crime
#   and performance paper

# author: andre assumpcao
# by andre.assumpcao@gmail.com

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
load('data/tseFinal.Rda')

### function definitions
# function to change names of instrumented variable in felm regression so that
# stargazer outputs everything in the same row
cbeta <- function(reg, name = 'candidacy.invalid.ontrial') {
  # Args:
  #   reg: regression object

  # Returns:
  #   matrix of robust standard errors

  # Body:
  #   compute standard errors in different ways if lm, ivreg or felm classes

  # function
  # check where the coefficient with the wrong name is
  i <- which(str_detect(row.names(reg$coefficients), fixed('(fit)')))
  j <- which(str_detect(row.names(reg$beta), fixed('(fit)')))
  w <- which(str_detect(names(reg$rse), fixed('(fit)')))

  # assign
  if (class(reg) == 'felm') {
    if (!is.null(name)) {
      row.names(reg$coefficients)[i] <- name
      row.names(reg$beta)[j]         <- name
      names(reg$rse)[w]              <- name
    }
  }

  # return call
  return(reg)
}

# function to measure the degree to which selection on unobservables would hurt
# coefficient stability
coefstab <- function(restricted, unrestricted, stat, b_star = 0, r2_max = 1) {
  # Args:
  #   restricted:   restricted regression object
  #   unrestricted: unrestricted regression object

  # Returns:
  #   degree of selection or max r.squared

  # Body:
  #   extract r.squared and coefficients

  # function
  # find variable for which we want to test coefficient stability
  try(silent = TRUE, expr = {
    i <- which(str_detect(names(restricted$coefficients), 'invalid'))
    j <- which(str_detect(names(unrestricted$coefficients), 'invalid'))
  })
  try(silent = TRUE, expr = {
    i <- which(str_detect(row.names(restricted$coefficients), 'invalid'))
    j <- which(str_detect(row.names(unrestricted$coefficients), 'invalid'))
  })

  # extract coefficients
  b_zero  <- restricted$coefficients[i]
  b_tilde <- unrestricted$coefficients[j]

  # extract rsquared
  r2_zero  <- summary(restricted)$r.squared
  r2_tilde <- summary(unrestricted)$r.squared

  # return calls
  if (stat == 'bias') {
    # calculate bias-adjusted coefficient
    bias <- (b_tilde - (b_zero - b_tilde)) *
            ((r2_max - r2_tilde) / (r2_tilde - r2_zero))
    # return
    return(bias)

  } else if (stat == 'delta') {
    # calculate delta
    delta <- ((b_tilde - b_star) * (r2_tilde - r2_zero)) /
             ((b_zero - b_tilde) * (r2_max - r2_tilde))
    # return
    return(delta)

  } else if (stat == 'r2_max') {
    # calculate max r2
    r2_max <- ((b_tilde / (b_zero - b_tilde)) * (r2_tilde - r2_zero)) + r2_tilde
    # return
    return(unname(r2_max))
  }
}

# define function to calculate corrected SEs for OLS, IV, and FELM regressions
cse <- function(reg, fs = FALSE, ...) {
  # Args:
  #   reg: regression object

  # Returns:
  #   matrix of robust standard errors

  # Body:
  #   compute standard errors in different ways if lm, ivreg or felm classes

  # function
  if (class(reg) == 'lm') {
    rob <- sqrt(diag(sandwich::vcovHC(reg, type = 'HC1')))
  } else if (class(reg) == 'felm') {
    if (fs == FALSE) {reg <- cbeta(reg, ...)}
    rob <- summary(reg, robust = TRUE)$coefficients[, 2]
  } else if (class(reg) == 'ivreg') {
    rob <- ivpack::robust.se(reg)[, 2]
  } else {
    message('not implemented yet')
  }

  # return matrix
  return(rob)
}

# function to simulate other correlation levels between trial and appeals
# rulings using the same judicial review data
simcorrel <- function(correl.shift = NULL, ...) {
  # Args:
  #   var: variable used to compute correlation
  #   ...: additional arguments passed to sample()

  # Returns:
  #   list with correlation coefficient, mean, and vector of simulated outcomes

  # Body:
  #   call to sample, correlation, mean, and store it to object

  # function
  # extract actual observed values from appeals distribution
  var <- tse.analysis$candidacy.invalid.onappeal

  # determine size of sampled observations
  if (is.null(correl.shift)) {samplesize <- 9470}
  else                       {samplesize <- ceiling(9470 * correl.shift)}

  # replace values in original variable
  if (samplesize < 9470) {
    # determine size of non-sampled observations
    sampled <- sample(9470, samplesize, replace = FALSE)
    var[sampled] <- sample(c(1, 0), size = samplesize, replace = TRUE)

  } else {
    var %<>% sample(size = samplesize, ...)
  }

  # produce object
  object <- list(correlation = cor(tse.analysis$candidacy.invalid.ontrial, var),
                 mean = mean(var), appeals.outcomes = var)
  # return call
  invisible(object)
}

# function to conduct t-tests across parameters in different regressions
t.test2 <- function(mean1, mean2, se1, se2) {
  # Args:
  #   mean1, mean2: means of each parameter
  #   se1, se2:     standard errors of each parameter

  # Returns:
  #   test statistics

  # Body:
  #   compute statistics and return results

  # function
  se <- se1 + se2
  df <- ((se1 + se2)^2) / ((se1)^2 / (9442 - 1) + (se2)^2 / (9442 - 1))
  t  <- (mean1 - mean2) / se
  result <- c(mean1, mean2, mean1 - mean2, se, t, 2 * pt(-abs(t), df))
  names(result) <- c('Trial', 'Appeals', 'Difference in beta', 'Std. Error',
                     't-stat', 'p-value')

  # return call
  return(result)
}

### define y's and x's used in analysis and their labels
# outcome labels
outcomes       <- c('outcome.elected', 'outcome.share', 'outcome.distance')
outcome.labels <- c('Probability of Election',
                    'Total Vote Share (in p.p.)',
                    'Vote Distance to Election Cutoff (in p.p.)')

# define instruments and their labels
instrument        <- 'candidacy.invalid.onappeal'
instrumented      <- 'candidacy.invalid.ontrial'
instrument.labels <- c('Convicted at Trial', 'Convicted on Appeal')

# define independent variables labels
covariates       <- c('candidate.age', 'candidate.male',
                      'candidate.maritalstatus', 'candidate.education',
                      'candidate.experience', 'candidacy.expenditures.actual')
covariate.labels <- c('Age', 'Male', 'Level of Education', 'Marital Status',
                      'Political Experience', 'Campaign Expenditures (in R$)')

### define matrices of fixed effects
# municipality and time
mun.label   <- 'Municipal Election'
time.label  <- 'Election Year'

# define variable types for analysis
integers <- c(6, 11, 17, 20, 26, 31:32, 36:43)
factors  <- c(2, 5, 9, 21, 23:24, 33:35)

# change variable types
tse.analysis %<>%
  mutate_at(vars(integers), as.integer) %>%
  mutate_at(vars(factors), as.factor)

# define levels for education and marital status variables
labels1 <- c('Illiterate', 'Completed ES/MS', 'Incomplete ES/MS',
             'Can Read and Write', 'Completed HS', 'Incomplete HS',
             'Completed College', 'Incomplete College')
labels2 <- c('Married', 'Divorced', 'Legally Divorced', 'Single', 'Widowed')

# assign factors
tse.analysis$candidate.education %<>% factor(labels = labels1)
tse.analysis$candidate.maritalstatus %<>% factor(labels = labels2)

# remove variable indexes
rm(integers, factors, labels1, labels2)

### tables and analyses
# produce summary statistics table
stargazer(

  # summmary table
  as.data.frame(
    tse.analysis[, c(covariates, instrumented, instrument, outcomes)]
  ),

  # table cosmetics
  type = 'text',
  title = 'Descriptive Statistics',
  style = 'default',
  summary = TRUE,
  # out = 'tables/sumstats.tex',
  out.header = FALSE,
  covariate.labels = c(covariate.labels[c(1:2, 5:6)],
                       instrument.labels,
                       outcome.labels),
  align = FALSE,
  digit.separate = 3,
  digits = 3,
  digits.extra = 2,
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

### reversals table
# run tabulation of convictions
reversals <- tse.analysis %$%
  table(candidacy.invalid.ontrial, candidacy.invalid.onappeal)

# do the math for table
reversals[1, 2] / (reversals[1, 1] + reversals[1, 2])
reversals[2, 1] / (reversals[2, 2] + reversals[2, 1])

# remove useless objects
rm(reversals)

# revert expenditures to log (and preserve original dataset for candidate
# disengagement tests at the end)
candidate.disengagement.analysis <- tse.analysis
tse.analysis$candidacy.expenditures.actual %<>% {log(. + 1)}

### first-stage tests
# produce graphs testing the first-stage strength
fs01 <- lm(candidacy.invalid.ontrial ~ candidacy.invalid.onappeal, tse.analysis)
fs02 <- felm(candidacy.invalid.ontrial ~ candidacy.invalid.onappeal +
  candidate.age + candidate.male + candidate.experience +
  candidacy.expenditures.actual + candidate.maritalstatus +
  candidate.education, data = tse.analysis, exactDOF = TRUE)
fs03 <- felm(candidacy.invalid.ontrial ~ candidacy.invalid.onappeal +
  candidate.age + candidate.male + candidate.experience +
  candidacy.expenditures.actual + candidate.maritalstatus +
  candidate.education | election.ID + election.year + party.number,
  data = tse.analysis, exactDOF = TRUE)

# extract point estimates and s.e.'s for graph and tables
point.estimates1 <- c(summary(fs01)$coefficients[2, 1], cse(fs01, fs = TRUE)[2])
point.estimates2 <- c(summary(fs02)$coefficients[2, 1], cse(fs02, fs = TRUE)[2])
point.estimates3 <- c(summary(fs03)$coefficients[1, 1], cse(fs03, fs = TRUE)[1])

# extract f-stat for graphs and tables
f.stat1 <- summary(fs01)$fstatistic[1]
f.stat2 <- summary(fs02)$fstat
f.stat3 <- summary(fs03)$F.fstat[1]

# build vectors with point estimates and 10%, 5%, and 1% CIs around estimates
fs.estimate1 <- point.estimates1 %>%
  {c(.[1], .[1] - qnorm(.05) * .[2], .[1] + qnorm(.05) * .[2],
     .[1] - qnorm(.025) * .[2], .[1] + qnorm(.025) * .[2],
     .[1] - qnorm(.005) * .[2], .[1] + qnorm(.005) * .[2])} %>%
  unname() %>%
  round(3)
fs.estimate2 <- point.estimates2 %>%
  {c(.[1], .[1] - qnorm(.05) * .[2], .[1] + qnorm(.05) * .[2],
     .[1] - qnorm(.025) * .[2], .[1] + qnorm(.025) * .[2],
     .[1] - qnorm(.005) * .[2], .[1] + qnorm(.005) * .[2])} %>%
  unname() %>%
  round(3)
fs.estimate3 <- point.estimates3 %>%
  {c(.[1], .[1] - qnorm(.05) * .[2], .[1] + qnorm(.05) * .[2],
     .[1] - qnorm(.025) * .[2], .[1] + qnorm(.025) * .[2],
     .[1] - qnorm(.005) * .[2], .[1] + qnorm(.005) * .[2])} %>%
  unname() %>%
  round(3)

# build dataset
models <- rep(c('model1', 'model2', 'model3'), 3)
ci_bound <- rep(c('90% CI', '95% CI', '99% CI'), each = 3)
estimate <- rep(c(fs.estimate1[1], fs.estimate2[1], fs.estimate3[1]), 3)
ci_upper <- c(fs.estimate1[2], fs.estimate2[2], fs.estimate3[2],
              fs.estimate1[4], fs.estimate2[4], fs.estimate3[4],
              fs.estimate1[6], fs.estimate2[6], fs.estimate3[6])
ci_lower <- c(fs.estimate1[3], fs.estimate2[3], fs.estimate3[3],
              fs.estimate1[5], fs.estimate2[5], fs.estimate3[5],
              fs.estimate1[7], fs.estimate2[7], fs.estimate3[7])
fs.estimates <- tibble(models, ci_bound, estimate, ci_upper, ci_lower)

# define x-axis labels for ggplot
labels <- c(f.stat1, f.stat2, f.stat3) %>%
  round(1) %>%
  format(big.mark = ',') %>%
  trimws() %>%
  {paste0('(F-stat = ', ., ')')} %>%
  {paste(c('No Covariates', 'Individual Covariates',
           'Individual Covariates \n and Fixed-Effects'), ., sep = '\n')}

# build plot
ggplot(fs.estimates, aes(y = estimate, x = models, group = ci_bound)) +
  geom_point(aes(color = ci_bound), position = position_dodge(width = .25),
    size = 3) +
  geom_text(aes(label = estimate), nudge_x = -.25, family = 'LM Roman 10',
    size = 4) +
  geom_errorbar(aes(ymax = ci_upper, ymin = ci_lower, color = ci_bound),
    width = .25, position = position_dodge(width = .25)) +
  scale_color_manual(values = c('grey74', 'yellow4', 'grey10'),
    name = 'Confidence Intervals:') +
  scale_x_discrete(labels = labels) +
  labs(y = 'Instrument Point Estimates', x = element_blank()) +
  ylim(min = .7, max = .8) +
  theme_bw() +
  theme(axis.title  = element_text(size = 10),
        axis.text.y = element_text(size = 10, lineheight = 1.1, face = 'bold'),
        axis.text.x = element_text(size = 10, lineheight = 1.1, face = 'bold'),
        text = element_text(family = 'LM Roman 10'),
        panel.border = element_rect(colour = 'black', size = 1),
        legend.text  = element_text(size = 10), legend.position = 'top',
        panel.grid.major = element_line(color = 'lightcyan4',
                                        linetype = 'dotted'),
        panel.grid.minor = element_line(color = 'lightcyan4',
                                        linetype = 'dotted')
  )

# # save plot
# ggsave('firststage.pdf', device = cairo_pdf, path = 'plots', dpi = 100,
#        width = 7, height = 5)

# remove unnecessary objects
rm(list = objects(pattern = 'f\\.stat|point\\.estimate|names|models|ci'))

# produce tables showing first-stage strength
stargazer(

  # first-stage regressions
  list(fs01, fs02, fs03),

  # table cosmetics
  type = 'text',
  title = 'First-Stage Regressions of Convictions at Trial and on Appeal',
  style = 'default',
  # out = 'tables/firststage.tex',
  out.header = FALSE,
  covariate.labels = instrument.labels[2],
  dep.var.caption = paste0('Outcome: ', instrument.labels[1]),
  dep.var.labels.include = FALSE,
  align = TRUE,
  se = list(cse(fs01, fs = TRUE), cse(fs02, fs = TRUE), cse(fs03, fs = TRUE)),
  p.auto = TRUE,
  column.sep.width = '4pt',
  digit.separate = 3,
  digits = 3,
  digits.extra = 0,
  font.size = 'scriptsize',
  header = FALSE,
  initial.zero = FALSE,
  model.names = FALSE,
  keep = c('invalid'),
  label = 'tab:firststage',
  no.space = FALSE,
  omit = c('constant|education|maritalstatus', 'party|electoral'),
  omit.labels = c('Individual Controls', 'Fixed-Effects'),
  omit.stat = c('ser', 'f', 'rsq'),
  omit.yes.no = c('Yes', '-'),
  table.placement = '!htbp'
)

### hausman tests of instrument strength
# produce graphs testing the first-stage strength
tse.analysis %>%
  {ivreg(outcome.elected ~
    candidacy.invalid.ontrial | candidacy.invalid.onappeal, data = .)} -> iv01
tse.analysis %>%
  {ivreg(outcome.share ~
    candidacy.invalid.ontrial | candidacy.invalid.onappeal, data = .)} -> iv02
tse.analysis %>%
  {ivreg(outcome.distance ~
    candidacy.invalid.ontrial | candidacy.invalid.onappeal, data = .)} -> iv03
filter(tse.analysis, office.ID == 13) %>%
  {ivreg(outcome.distance ~
    candidacy.invalid.ontrial | candidacy.invalid.onappeal, data = .)} -> iv04
filter(tse.analysis, office.ID == 11) %>%
  {ivreg(outcome.distance ~
    candidacy.invalid.ontrial | candidacy.invalid.onappeal, data = .)} -> iv05

# create hausman dataset
hausman <- objects(pattern = 'iv') %>%
           lapply(get) %>%
           lapply(summary, diagnostics = TRUE) %>%
           lapply(function(x){x$diagnostics[2, c(3, 4)]}) %>%
           unlist()

# print table
tibble(
  Outcome = str_remove_all(outcome.labels, '\\((.)*\\)') %>% trimws() %>%
    c('City Councilor', 'Mayor'),
  `Hausman Statistic` = hausman[seq(1, 10, 2)] %>% sprintf(fmt = '%.2f'),
  `p-value` = hausman[seq(2, 10, 2)] %>% sprintf(fmt = '%.3f')
) %>%
xtable(
  label  = 'tab:hausman',
  align  = c('r', 'l', 'D{.}{.}{-2}', 'D{.}{.}{-3}'),
  digits = c(0, 0, 2, -3)
) %>%
print.xtable(
  floating = FALSE,
  hline.after = c(-1, -1, 0, 5, 5),
  include.rownames = FALSE
)

# # remove unnecessary objects
# rm(iv01, iv02, iv03, iv04, iv05)

### ols results
# create regression objects using the three outcomes and two samples

# outcome 1: probability of election
ols01 <- lm(outcome.elected ~ candidacy.invalid.ontrial, data = tse.analysis)
ols02 <- lm(outcome.elected ~ candidacy.invalid.ontrial + candidate.age +
  candidate.male + candidate.experience + candidacy.expenditures.actual +
  candidate.maritalstatus + candidate.education, data = tse.analysis)
ols03 <- felm(outcome.elected ~ candidacy.invalid.ontrial + candidate.age +
  candidate.male + candidate.experience + candidacy.expenditures.actual +
  candidate.maritalstatus + candidate.education | election.ID + election.year +
  party.number, data = tse.analysis, exactDOF = TRUE)

# outcome 2: vote share
ols04 <- lm(outcome.share ~ candidacy.invalid.ontrial, data = tse.analysis)
ols05 <- lm(outcome.share ~ candidacy.invalid.ontrial + candidate.age +
  candidate.male + candidate.experience + candidacy.expenditures.actual +
  candidate.maritalstatus + candidate.education, data = tse.analysis)
ols06 <- felm(outcome.share ~ candidacy.invalid.ontrial + candidate.age +
  candidate.male + candidate.experience + candidacy.expenditures.actual +
  candidate.maritalstatus + candidate.education | election.ID + election.year +
  party.number, data = tse.analysis, exactDOF = TRUE)

# outcome 3: distance to election cutoff for city councilor candidates
ols07 <- filter(tse.analysis, office.ID == 13) %>%
  {lm(outcome.distance ~ candidacy.invalid.ontrial, data = .)}
ols08 <- filter(tse.analysis, office.ID == 13) %>%
  {lm(outcome.distance ~ candidacy.invalid.ontrial + candidate.age +
    candidate.male + candidate.experience + candidacy.expenditures.actual +
    candidate.maritalstatus + candidate.education, data = .)}
ols09 <- filter(tse.analysis, office.ID == 13) %>%
  {felm(outcome.distance ~ candidacy.invalid.ontrial + candidate.age +
    candidate.male + candidate.experience + candidacy.expenditures.actual +
    candidate.maritalstatus + candidate.education | election.ID + election.year+
    party.number, data = ., exactDOF = TRUE)}

# outcome 3: distance to election cutoff for mayor candidates
ols10 <- filter(tse.analysis, office.ID == 11) %>%
  {lm(outcome.distance ~ candidacy.invalid.ontrial, data = .)}
ols11 <- filter(tse.analysis, office.ID == 11) %>%
  {lm(outcome.distance ~ candidacy.invalid.ontrial + candidate.age +
    candidate.male + candidate.experience + candidacy.expenditures.actual +
    candidate.maritalstatus + candidate.education, data = .)}
ols12 <- filter(tse.analysis, office.ID == 11) %>%
  {felm(outcome.distance ~ candidacy.invalid.ontrial + candidate.age +
    candidate.male + candidate.experience + candidacy.expenditures.actual +
    candidate.maritalstatus + candidate.education | election.ID + election.year+
    party.number, data = ., exactDOF = TRUE)}

### instrumental variables results
# create regression objects using the three outcomes and two samples

# outcome 1: probability of election
ss01 <- tse.analysis %>%
  {felm(outcome.elected ~ 1 | 0 | (candidacy.invalid.ontrial ~
    candidacy.invalid.onappeal), data = ., exactDOF = TRUE)}
ss02 <- tse.analysis %>%
  {felm(outcome.elected ~ candidate.age + candidate.male +
    candidate.experience + candidacy.expenditures.actual +
    candidate.maritalstatus + candidate.education | 0 |
    (candidacy.invalid.ontrial ~ candidacy.invalid.onappeal), data = .,
    exactDOF = TRUE)}
ss03 <- tse.analysis %>%
  {felm(outcome.elected ~ candidate.age + candidate.male +
    candidate.experience + candidacy.expenditures.actual +
    candidate.maritalstatus + candidate.education | election.ID + election.year+
    party.number | (candidacy.invalid.ontrial ~ candidacy.invalid.onappeal),
    data = ., exactDOF = TRUE)}

# outcome 2: vote share
ss04 <- tse.analysis %>%
  {felm(outcome.share ~ 1 | 0 | (candidacy.invalid.ontrial ~
    candidacy.invalid.onappeal), data = ., exactDOF = TRUE)}
ss05 <- tse.analysis %>%
  {felm(outcome.share ~ candidate.age + candidate.male + candidate.experience +
    candidacy.expenditures.actual + candidate.maritalstatus +
    candidate.education | 0 | (candidacy.invalid.ontrial ~
    candidacy.invalid.onappeal), data = ., exactDOF = TRUE)}
ss06 <- tse.analysis %>%
  {felm(outcome.share ~ candidate.age + candidate.male + candidate.experience +
    candidacy.expenditures.actual + candidate.maritalstatus +
    candidate.education | election.ID + election.year + party.number |
    (candidacy.invalid.ontrial ~ candidacy.invalid.onappeal), data = .,
    exactDOF = TRUE)}

# outcome 3: distance to election cutoff for city councilor candidates
ss07 <- filter(tse.analysis, office.ID == 13) %>%
  {felm(outcome.distance ~ 1 | 0 | (candidacy.invalid.ontrial ~
    candidacy.invalid.onappeal), data = ., exactDOF = TRUE)}
ss08 <- filter(tse.analysis, office.ID == 13) %>%
  {felm(outcome.distance ~ candidate.age + candidate.male +
    candidate.experience + candidacy.expenditures.actual +
    candidate.maritalstatus + candidate.education | 0 |
    (candidacy.invalid.ontrial ~ candidacy.invalid.onappeal), data = .,
    exactDOF = TRUE)}
ss09 <- filter(tse.analysis, office.ID == 13) %>%
  {felm(outcome.distance ~ candidate.age + candidate.male +
    candidate.experience + candidacy.expenditures.actual +
    candidate.maritalstatus + candidate.education | election.ID + election.year+
    party.number | (candidacy.invalid.ontrial ~ candidacy.invalid.onappeal),
    data = ., exactDOF = TRUE)}

# outcome 3: distance to election cutoff for mayor candidates
ss10 <- filter(tse.analysis, office.ID == 11) %>%
  {felm(outcome.distance ~ 1 | 0 | (candidacy.invalid.ontrial ~
    candidacy.invalid.onappeal), data = ., exactDOF = TRUE)}
ss11 <- filter(tse.analysis, office.ID == 11) %>%
  {felm(outcome.distance ~ candidate.age + candidate.male +
    candidate.experience + candidacy.expenditures.actual +
    candidate.maritalstatus + candidate.education | 0 |
    (candidacy.invalid.ontrial ~ candidacy.invalid.onappeal), data = .,
    exactDOF = TRUE)}
ss12 <- filter(tse.analysis, office.ID == 11) %>%
  {felm(outcome.distance ~ candidate.age + candidate.male +
    candidate.experience + candidacy.expenditures.actual +
    candidate.maritalstatus + candidate.education | election.ID + election.year+
    party.number | (candidacy.invalid.ontrial ~ candidacy.invalid.onappeal),
    data = ., exactDOF = TRUE)}

# produce tables with outcome one
stargazer(

  # first-stage regressions
  list(ols01, ols02, ols03, cbeta(ss01), cbeta(ss02), cbeta(ss03)),

  # table cosmetics
  type = 'text',
  title = 'The Effect of Electoral Crimes on the Probability of Election',
  style = 'default',
  # out = 'tables/secondstageoutcome1.tex',
  out.header = FALSE,
  column.labels = rep(c('OLS', 'IV'), each = 3),
  column.separate = rep(1, 6),
  covariate.labels = instrument.labels[1],
  dep.var.caption = paste0('Outcome: ', outcome.labels[1]),
  dep.var.labels.include = FALSE,
  align = TRUE,
  se = list(cse(ols01), cse(ols02), cse(ols03),
            cse(ss01), cse(ss02), cse(ss03)),
  p.auto = TRUE,
  column.sep.width = '4pt',
  digit.separate = 3,
  digits = 3,
  digits.extra = 0,
  font.size = 'scriptsize',
  header = FALSE,
  initial.zero = FALSE,
  model.names = FALSE,
  keep = c('invalid'),
  label = 'tab:secondstageoutcome1',
  no.space = FALSE,
  omit = c('constant', 'party|electoral'),
  omit.labels = c('Individual Controls', 'Fixed-Effects'),
  omit.stat = c('ser', 'f', 'rsq'),
  omit.yes.no = c('Yes', '-'),
  table.placement = '!htbp'
)

# extract f-stat for graphs and tables
c('\textit{F}-stat ',
  summary(ols01)$fstatistic[1] %>% round(2),
  summary(ols02)$fstatistic[1] %>% round(2),
  summary(ols03)$F.fstat[1]    %>% round(2),
  summary(ss01)$F.fstat[1]     %>% round(2),
  summary(ss02)$F.fstat[1]     %>% round(2),
  summary(ss03)$F.fstat[1]     %>% round(2)
) %>%
paste0(collapse = ' & ') %>%
paste0(' \\')

# produce tables with outcome two
stargazer(

  # first-stage regressions
  list(ols04, ols05, ols06, cbeta(ss04), cbeta(ss05), cbeta(ss06)),

  # table cosmetics
  type = 'text',
  title = 'The Effect of Electoral Crimes on the Total Vote Share',
  style = 'default',
  # out = 'tables/secondstageoutcome2.tex',
  out.header = FALSE,
  column.labels = rep(c('OLS', 'IV'), each = 3),
  column.separate = rep(1, 6),
  covariate.labels = instrument.labels[2],
  dep.var.caption = paste0('Outcome: ', outcome.labels[2]),
  dep.var.labels.include = FALSE,
  align = TRUE,
  se = list(cse(ols04), cse(ols05), cse(ols06),
            cse(ss04), cse(ss05), cse(ss06)),
  p.auto = TRUE,
  column.sep.width = '4pt',
  digit.separate = 3,
  digits = 3,
  digits.extra = 0,
  font.size = 'scriptsize',
  header = FALSE,
  initial.zero = FALSE,
  model.names = FALSE,
  keep = c('invalid'),
  label = 'tab:secondstageoutcome2',
  no.space = FALSE,
  omit = c('constant', 'party|electoral'),
  omit.labels = c('Individual Controls', 'Fixed-Effects'),
  omit.stat = c('ser', 'f', 'rsq'),
  omit.yes.no = c('Yes', '-'),
  table.placement = '!htbp'
)

# extract f-stat for graphs and tables
c('\textit{F}-stat ',
  summary(ols04)$fstatistic[1] %>% round(2),
  summary(ols05)$fstatistic[1] %>% round(2),
  summary(ols06)$F.fstat[1]    %>% round(2),
  summary(ss04)$F.fstat[1]     %>% round(2),
  summary(ss05)$F.fstat[1]     %>% round(2),
  summary(ss06)$F.fstat[1]     %>% round(2)
) %>%
paste0(collapse = ' & ') %>%
paste0(' \\')

# produce cis for discussion in paper
cis <- list(
          c(summary(ols04)$coefficients[2], cse(ols04)[2]),
          c(summary(ols05)$coefficients[2], cse(ols05)[2]),
          c(summary(ols06)$coefficients[1], cse(ols06)[1]),
          summary(ss04, robust = TRUE)$coefficients[2, c(1, 2)],
          summary(ss05, robust = TRUE)$coefficients[17, c(1, 2)],
          summary(ss06, robust = TRUE)$coefficients[16, c(1, 2)]
        ) %>%
        lapply(unname) %>%
        lapply(function(x){c(x[1]-qnorm(.005)*x[2], x[1]+qnorm(.005)*x[2])})

# produce tables with outcome three for city councilor and mayor sample
stargazer(

  # first-stage regressions
  list(ols09, cbeta(ss09), ols12, cbeta(ss12)),

  # table cosmetics
  type = 'text',
  title = paste('The Effect of Electoral Crimes on the Vote Distance to',
                'Election Cutoff', sep = ' '),
  style = 'default',
  # out = 'tables/secondstageoutcome3.tex',
  out.header = FALSE,
  column.labels = rep(c('OLS', 'IV'), 2),
  column.separate = rep(1, 4),
  covariate.labels = instrument.labels[1],
  dep.var.caption = paste0('Outcome: ', outcome.labels[3]),
  dep.var.labels.include = FALSE,
  align = TRUE,
  se = list(cse(ols09), cse(ss09), cse(ols12), cse(ss12)),
  p.auto = TRUE,
  column.sep.width = '4pt',
  digit.separate = 3,
  digits = 3,
  digits.extra = 0,
  font.size = 'scriptsize',
  header = FALSE,
  initial.zero = FALSE,
  model.names = FALSE,
  keep = c('invalid'),
  label = 'tab:secondstageoutcome3',
  no.space = FALSE,
  omit = c('constant', 'party|electoral'),
  omit.labels = c('Individual Controls', 'Fixed-Effects'),
  omit.stat = c('ser', 'f', 'rsq'),
  omit.yes.no = c('Yes', '-'),
  table.placement = '!htbp'
)

# extract f-stat for graphs and tables
objects(pattern = 'ols|ss') %>%
{.[c(9, 12, 21, 24)]} %>%
lapply(get) %>%
lapply(function(x){summary(x)$F.fstat[1]}) %>%
unlist() %>%
round(2) %>%
paste0(collapse = ' & ') %>%
{paste0('\textit{F}-stat & ', .)} %>%
paste0(' \\')

### test for coefficient stability
# here i implement the tests in altonji el at. (2005), oster (2017). i estimate
# boundaries for beta_iv based on the y-variation use in each regression

# extract beta.ols from ols regressions
objects(pattern = 'ols') %>%
lapply(get) %>%
lapply(function(x){
 if (class(x) == 'lm') {x$coefficients %>% {.[str_detect(names(.), 'trial')]}}
 else {x$coefficients %>% {.[str_detect(row.names(.), 'trial')]}}
}) %>%
unlist() %>%
unname() -> betas

# extract beta.olsse from ols regressions
objects(pattern = 'ols') %>%
lapply(get) %>%
lapply(function(x){cse(x)[str_detect(names(cse(x)), 'trial')]}) %>%
unlist() %>%
unname() -> stder

# calculate the lower bound of confidence intervals to compare to iv parameters
betas.star <- betas + qnorm(.025) * stder

# extract beta.ols from ols regressions
objects(pattern = 'ss') %>%
lapply(get) %>%
lapply(function(x){x$coefficients %>% {.[str_detect(row.names(.), 'tri')]}}) %>%
unlist() -> betas.iv

# calculate set of maximum r-squared
rsqr <- objects(pattern = 'ss') %>%
        lapply(get) %>%
        lapply(function(x){summary(x)$r.squared}) %>%
        unlist()

# create vector for R2_ur + (R2_ur - R2_r)
rmax <- rsqr %>%
  {c(.[2] + (.[2] - .[1]), .[3] + (.[3] - .[1]), .[5] + (.[5] - .[4]),
     .[6] + (.[6] - .[4]), .[8] + (.[8] - .[7]), .[9] + (.[9] - .[7]),
     .[11]+ (.[11] - .[10]), .[12] + (.[12] - .[10])
  )} %>%
  lapply(function(x){ifelse(x < 1, x, 1)}) %>%
  unlist()

# produce statistics
coefstab01 <- c(coefstab(ss01, ss02, 'delta', betas[2], rmax[1]),
                coefstab(ss01, ss02, 'delta', betas[2], 2 * rsqr[2]),
                coefstab(ss01, ss02, 'r2_max'),
                coefstab(ss01, ss03, 'delta', betas[3], rmax[2]),
                coefstab(ss01, ss03, 'delta', betas[3], 2 * rsqr[3]),
                coefstab(ss01, ss03, 'r2_max'))
coefstab02 <- c(coefstab(ss04, ss05, 'delta', betas[5], rmax[3]),
                coefstab(ss04, ss05, 'delta', betas[5], 2 * rsqr[5]),
                coefstab(ss04, ss05, 'r2_max'),
                coefstab(ss04, ss06, 'delta', betas[6], 1),
                coefstab(ss04, ss06, 'delta', betas[6], 1),
                coefstab(ss04, ss06, 'r2_max'))
coefstab03 <- c(coefstab(ss07, ss08, 'delta', betas[8], rmax[5]),
                coefstab(ss07, ss08, 'delta', betas[8], 2 * rsqr[8]),
                coefstab(ss07, ss08, 'r2_max'),
                coefstab(ss07, ss09, 'delta', betas[9], 1),
                coefstab(ss07, ss09, 'delta', betas[9], 1),
                coefstab(ss07, ss09, 'r2_max'))
coefstab04 <- c(coefstab(ss10, ss11, 'delta', betas[11], rmax[7]),
                coefstab(ss10, ss11, 'delta', betas[11], 2 * rsqr[11]),
                coefstab(ss10, ss11, 'r2_max'),
                coefstab(ss10, ss12, 'delta', betas[12], 1),
                coefstab(ss10, ss12, 'delta', betas[12], 1),
                coefstab(ss10, ss12, 'r2_max'))

# create table
tibble(coefstab01, coefstab02, coefstab03, coefstab04) %>%
t() %>%
as_tibble(.name_repair = 'universal') %>%
mutate_all(~round(., 2)) %>%
mutate(
  outcome = c('Outcome 1: Probability of Election', 'Outcome 2: Vote Share',
    'Outcome 3: Vote Distance to Cutoff (City Councilor)',
    'Outcome 4: Vote Distance to Cutoff (Mayor)')
) %>%
select(7, 1:6) %>%
xtable() %>%
print.xtable(floating = FALSE, hline.after = c(-1, -1, 0, 4, 4),
  include.rownames = FALSE)

# create r-squares for table
c(rmax[1], 2 * rsqr[2], NA_real_, rmax[2], 2 * rsqr[3], NA_real_) %>%
round(2) %>%
paste0(collapse = ' & ')
c(rmax[3], 2 * rsqr[5], NA_real_, rmax[4], 1, NA_real_) %>%
round(2) %>%
paste0(collapse = ' & ')
c(rmax[5], 2 * rsqr[8], NA_real_, rmax[6], 1, NA_real_) %>%
round(2) %>%
paste0(collapse = ' & ')
c(rmax[7], 2 * rsqr[11], NA_real_, rmax[8], 1, NA_real_) %>%
round(2) %>%
paste0(collapse = ' & ')

# remove unnecessary objects
rm(list = objects(pattern = 'rsqr|rmax|coefstab'))

### test for heterogeneous judicial behavior between trial and appeals
# i am interested in knowing whether justices change change the factors
# affecting ruling when elections have passed.

# regression for factors affecting trial
covariate.balance.instrumented <- felm(candidacy.invalid.ontrial ~
  outcome.elected + candidate.age + candidate.male + candidate.maritalstatus +
  candidate.education + candidate.experience + candidacy.expenditures.actual |
  party.number | 0 | election.ID + election.year, data = tse.analysis,
  exactDOF = TRUE, psdef = FALSE)

# regression for factors affecting appeals
covariate.balance.instrument <- felm(candidacy.invalid.onappeal ~
  outcome.elected + candidate.age + candidate.male + candidate.maritalstatus +
  candidate.education + candidate.experience + candidacy.expenditures.actual |
  party.number | 0 | election.ID + election.year, data = tse.analysis,
  exactDOF = TRUE, psdef = FALSE)

# check point estimates and standard errors in each regression
summary(covariate.balance.instrumented, robust = TRUE)$coefficients[, c(1, 2)]
summary(covariate.balance.instrument,   robust = TRUE)$coefficients[, c(1, 2)]

# create table of judicial behavior
judicial.behavior <- tibble()

# loop over stats and create vector
for (i in 1:16) {
  # create data vector
  vector <- t.test2(
    summary(covariate.balance.instrumented,
      robust = TRUE)$coefficients[i, 1],
    summary(covariate.balance.instrument,
      robust = TRUE)$coefficients[i, 1],
    summary(covariate.balance.instrumented,
      robust = TRUE)$coefficients[i, 2],
    summary(covariate.balance.instrument,
      robust = TRUE)$coefficients[i, 2]
  )
  # bind to data
  judicial.behavior <- bind_rows(judicial.behavior, vector)
}

# format variable names to include in table
var.names <- summary(covariate.balance.instrument)$coefficients %>%
  {dimnames(.)[[1]]} %>%
  str_remove_all('candida(cy|te)\\.|education|maritalstatus|\\.actual')
var.names[c(2, 3)] %<>% str_to_sentence()
var.names[c(1, 15, 16)] <- c('Elected to Office', covariate.labels[c(5, 6)])

# format judicial behavior dataset
judicial.behavior %>%
  mutate(Variable = var.names) %>%
  select(Variable, everything()) %>%
  mutate(`Difference in beta` = Trial - Appeals) %>%
  mutate_at(vars(2:7), ~sprintf(., fmt = '%.3f')) %>%
  slice(1:3, 15:16, 4:14) %>%
  xtable(label = 'tab:heterogeneous_sentencing') %>%
  print.xtable(floating = FALSE, hline.after = c(-1, -1, 0, 16, 16),
    include.rownames = FALSE)

# remove useless objects
rm(list = objects(pattern = 'balance|var\\.names|judicial\\.behavior|vector'))

### test for the correlation between instrument and other covariates
# here i want to know whether the instrument might be significantly correlated
# with other covariates beyond the endogenous correlation between the
# instrumented variable and covariates. solution: run ols with instrument
# straight into second-stage

# outcome 1: probability of election
ols13 <- lm(outcome.elected ~ candidacy.invalid.onappeal, data = tse.analysis)
ols14 <- lm(outcome.elected ~ candidacy.invalid.onappeal + candidate.age +
  candidate.male + candidate.experience + candidacy.expenditures.actual +
  candidate.maritalstatus + candidate.education, data = tse.analysis)
ols15 <- felm(outcome.elected ~ candidacy.invalid.onappeal + candidate.age +
  candidate.male + candidate.experience + candidacy.expenditures.actual +
  candidate.maritalstatus + candidate.education | election.ID + election.year +
  party.number, data = tse.analysis, exactDOF = TRUE)

# outcome 2: vote share
ols16 <- lm(outcome.share ~ candidacy.invalid.onappeal, data = tse.analysis)
ols17 <- lm(outcome.share ~ candidacy.invalid.onappeal + candidate.age +
  candidate.male + candidate.experience + candidacy.expenditures.actual +
  candidate.maritalstatus + candidate.education, data = tse.analysis)
ols18 <- felm(outcome.share ~ candidacy.invalid.onappeal + candidate.age +
  candidate.male + candidate.experience + candidacy.expenditures.actual +
  candidate.maritalstatus + candidate.education | election.ID + election.year +
  party.number, data = tse.analysis, exactDOF = TRUE)

# outcome 3: distance to election cutoff for city councilor candidates
ols19 <- filter(tse.analysis, office.ID == 13) %>%
  {lm(outcome.distance ~ candidacy.invalid.onappeal, data = .)}
ols20 <- filter(tse.analysis, office.ID == 13) %>%
  {lm(outcome.distance ~ candidacy.invalid.onappeal + candidate.age +
    candidate.male + candidate.experience + candidacy.expenditures.actual +
    candidate.maritalstatus + candidate.education, data = .)}
ols21 <- filter(tse.analysis, office.ID == 13) %>%
  {felm(outcome.distance ~ candidacy.invalid.onappeal + candidate.age +
    candidate.male + candidate.experience + candidacy.expenditures.actual +
    candidate.maritalstatus + candidate.education | election.ID + election.year+
    party.number, data = ., exactDOF = TRUE)}

# outcome 3: distance to election cutoff for mayor candidates
ols22 <- filter(tse.analysis, office.ID == 11) %>%
  {lm(outcome.distance ~ candidacy.invalid.onappeal, data = .)}
ols23 <- filter(tse.analysis, office.ID == 11) %>%
  {lm(outcome.distance ~ candidacy.invalid.onappeal + candidate.age +
    candidate.male + candidate.experience + candidacy.expenditures.actual +
    candidate.maritalstatus + candidate.education, data = .)}
ols24 <- filter(tse.analysis, office.ID == 11) %>%
  {felm(outcome.distance ~ candidacy.invalid.onappeal + candidate.age +
    candidate.male + candidate.experience + candidacy.expenditures.actual +
    candidate.maritalstatus + candidate.education | election.ID + election.year+
    party.number, data = ., exactDOF = TRUE)}

# define list of models to extract betas and std errors
models <- objects(pattern = 'ols')

# recover betas
betas <- models %>%
  lapply(get) %>%
  lapply(summary) %>%
  lapply(function(x){x$coefficients[, 1]}) %>%
  unlist() %>%
  {.[str_detect(names(.), 'invalid')]}

# recover standard errors
stderr <- models %>%
  lapply(get) %>%
  lapply(cse) %>%
  unlist() %>%
  {.[str_detect(names(.), 'invalid')]}

# define vectors for dataset
depvar <- c('Probability of Election', 'Vote Share',
            'Vote Distance to Cutoff (City Councilor)',
            'Vote Distance to Cutoff (Mayor)')
models <- c('no.covariates', 'covariates', 'covariates.fe')
comparison <- rep(paste(rep(depvar, each = 3), models, sep = '.'), 2)
endogenous <- rep(c('Trial', 'Appeals'), each = 12)

# build dataset
tibble(outcomes = rep(rep(depvar, each = 3), 2), betas, models = rep(models,
  8), comparison, endogenous, stderr, ci_upper = betas + qnorm(0.005) *
  stderr, ci_lower = betas - qnorm(0.005) * stderr, group = paste0(models,
  endogenous)) %>%
mutate(outcomes = factor(outcomes, levels = unique(depvar)),
  models = factor(models, unique(models)), comparison = factor(comparison,
    levels = unique(unlist(comparison))), endogenous = factor(endogenous,
    levels = c("Trial", "Appeals"))) -> instrument.check

# build plot
ggplot(instrument.check, aes(y = betas, x = models, color = endogenous)) +
  geom_point(aes(color = endogenous), position = position_dodge(width = .25)) +
  geom_errorbar(aes(ymax = ci_upper, ymin = ci_lower, color = endogenous),
    width = .25, position = position_dodge(width = .25)) +
  scale_color_manual(values = c('grey56', 'grey10'), name = 'Coefficients:') +
  scale_x_discrete(
    labels = rep(c('No Covariates', 'Individual Covariates',
                   'Individual \n Covariates \n and Fixed Effects'), 4)) +
  labs(y = 'Point Estimates and 99% CIs', x = element_blank()) +
  facet_wrap(outcomes ~ ., scales = 'free_y') +
  theme_bw() +
  theme(axis.title  = element_text(size = 10),
        axis.text.y = element_text(size = 10, lineheight = 1.1, face = 'bold'),
        axis.text.x = element_text(size = 10, lineheight = 1.1, face = 'bold'),
        text = element_text(family = 'LM Roman 10'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = 'lightcyan4',
                                        linetype = 'dotted'),
        panel.border = element_rect(colour = 'black', size = 1),
        legend.text  = element_text(size = 10), legend.position = 'top',
        strip.text.x = element_text(size = 10, face = 'bold')
  )

# # save plot
# ggsave('instrumentcorrelation.pdf', device = cairo_pdf, path = 'plots',
#        dpi = 100, width = 10, height = 5)

# remove unnecessary objects
rm(depvar, models, comparison, endogenous, instrument.check, betas, stderr)

### test for voter disengagement
# there are two potential explanations for the effect here: (i) voters would be
# switching their choices when voting OR (ii) they would be disengaging from the
# political process altogether. this is what we test here.

# disengagement outcomes
voter.engagement <- c('Voter Turnout (percent)', 'Valid Votes (percent)',
                      'Invalid Votes (percent)')

# disengagement at the individual level
disengagement01 <- filter_at(tse.analysis, vars(37, 39, 41), ~!is.na(.)) %>%
  {felm(log(votes.turnout + 1) ~ candidate.age + candidate.male +
    candidate.experience + candidacy.expenditures.actual +
    candidate.maritalstatus + candidate.education | election.ID +
    election.year + party.number | (candidacy.invalid.ontrial ~
    candidacy.invalid.onappeal) | election.ID + election.year, data = .,
    exactDOF = TRUE, psdef = FALSE)}
disengagement02 <- filter_at(tse.analysis, vars(37, 39, 41), ~!is.na(.)) %>%
  {felm(log(votes.valid + 1) ~ candidate.age + candidate.male +
    candidate.experience + candidacy.expenditures.actual +
    candidate.maritalstatus + candidate.education | election.ID +
    election.year + party.number | (candidacy.invalid.ontrial ~
    candidacy.invalid.onappeal) | election.ID + election.year, data = .,
    exactDOF = TRUE, psdef = FALSE)}
disengagement03 <- filter_at(tse.analysis, vars(37, 39, 41), ~!is.na(.)) %>%
  {felm(log(votes.invalid + 1) ~ candidate.age + candidate.male +
    candidate.experience + candidacy.expenditures.actual +
    candidate.maritalstatus + candidate.education | election.ID +
    election.year + party.number | (candidacy.invalid.ontrial ~
    candidacy.invalid.onappeal) | election.ID + election.year, data = .,
    exactDOF = TRUE, psdef = FALSE)}

# aggregate dataset to party level
party.aggregation <- tse.analysis %>%
  group_by(election.ID, office.ID, election.year, party.number) %>%
  select(-matches('outcome|candidate')) %>%
  summarize(proportion.invalid.ontrial  = sum(candidacy.invalid.ontrial) /
    first(as.integer(office.vacancies)) * 100, proportion.invalid.onappeal =
    sum(candidacy.invalid.onappeal) / first(as.integer(office.vacancies)) * 100,
    votes.valid = first(votes.valid), votes.turnout = first(votes.turnout),
    votes.invalid = first(votes.invalid)
  ) %>%
  ungroup() %>%
  filter_at(vars(votes.turnout, votes.valid, votes.invalid), ~!is.na(.))

# disengagement at the party level
disengagement04 <- party.aggregation %>%
  {felm(log(votes.turnout + 1) ~ 1 | election.ID + election.year |
    (proportion.invalid.ontrial ~ proportion.invalid.onappeal) | election.ID +
    election.year, data = ., exactDOF = TRUE, psdef = FALSE)}
disengagement05 <- party.aggregation %>%
  {felm(log(votes.valid + 1) ~ 1 | election.ID + election.year |
    (proportion.invalid.ontrial ~ proportion.invalid.onappeal) | election.ID +
    election.year, data = ., exactDOF = TRUE, psdef = FALSE)}
disengagement06 <- party.aggregation %>%
  {felm(log(votes.invalid + 1) ~ 1 | election.ID + election.year |
    (proportion.invalid.ontrial ~ proportion.invalid.onappeal) | election.ID +
    election.year, data = ., exactDOF = TRUE, psdef = FALSE)}

# aggregate dataset to election level
election.aggregation <- tse.analysis %>%
  group_by(election.ID, office.ID, election.year) %>%
  select(-matches('outcome|candidate')) %>%
  summarize(proportion.invalid.ontrial = sum(candidacy.invalid.ontrial) /
    first(as.integer(office.vacancies)) * 100, proportion.invalid.onappeal =
    sum(candidacy.invalid.onappeal) / first(as.integer(office.vacancies)) * 100,
    votes.valid = first(votes.valid), votes.turnout = first(votes.turnout),
    votes.invalid = first(votes.invalid)
  ) %>%
  ungroup() %>%
  filter_at(vars(votes.turnout, votes.valid, votes.invalid), ~!is.na(.))

# disengagement at the election level
disengagement07 <- election.aggregation %>%
  {felm(log(votes.turnout + 1) ~ 1 | election.ID + election.year |
    (proportion.invalid.ontrial ~ proportion.invalid.onappeal) | election.ID +
    election.year, data = ., exactDOF = TRUE, psdef = FALSE)}
disengagement08 <- election.aggregation %>%
  {felm(log(votes.valid + 1) ~ 1 | election.ID + election.year |
    (proportion.invalid.ontrial ~ proportion.invalid.onappeal) | election.ID +
    election.year, data = ., exactDOF = TRUE, psdef = FALSE)}
disengagement09 <- election.aggregation %>%
  {felm(log(votes.invalid + 1) ~ 1 | election.ID + election.year |
    (proportion.invalid.ontrial ~ proportion.invalid.onappeal) | election.ID +
    election.year, data = ., exactDOF = TRUE, psdef = FALSE)}

# build table
# produce tables with outcome one
stargazer(

  # first-stage regressions
  list(disengagement04, disengagement06, disengagement07, disengagement09),

  # table cosmetics
  type = 'text',
  title = 'The Effect of Electoral Crimes on Voter Engagement',
  style = 'default',
  # out = 'tables/voterbehavior.tex',
  out.header = FALSE,
  column.labels = c('Party-Level', 'Election-Level'),
  column.separate = rep(2, 2),
  covariate.labels = 'Share of Candidacies Invalid at Trial',
  dep.var.labels = rep(paste0('Outcome: ', voter.engagement[c(2, 3)]), 2),
  align = FALSE,
  apply.coef = function(x){x * 100},
  apply.se = function(x){x * 100},
  se = list(
    cse(disengagement04, name = NULL), cse(disengagement06, name = NULL),
    cse(disengagement07, name = NULL), cse(disengagement09, name = NULL)
  ),
  p.auto = TRUE,
  column.sep.width = '4pt',
  digit.separate = 3,
  digits = 3,
  digits.extra = 0,
  font.size = 'scriptsize',
  header = FALSE,
  initial.zero = FALSE,
  model.names = FALSE,
  keep = c('invalid'),
  label = 'tab:voterbehavior',
  no.space = FALSE,
  omit = c('constant', 'party|electoral'),
  omit.labels = c('Individual Controls', 'Fixed-Effects'),
  omit.stat = c('ser', 'rsq'),
  omit.yes.no = c('Yes', '-'),
  table.placement = '!htbp'
)

# extract f-stat for graphs and tables and assign latex format to it
objects(pattern = 'disengagement') %>%
{.[c(4, 6, 7, 9)]} %>%
lapply(get) %>%
lapply(function(x){summary(x)$F.fstat[1]}) %>%
unlist() %>%
round(1) %>%
paste0(collapse = ' & ') %>%
{paste0('\textit{F}-stat & ', .)} %>%
paste0(' \\')

# remove unnecessary objects
rm(party.aggregation, election.aggregation)

### test for candidate disengagement
# what i am testing here is whether candidates' strategies change conditional on
# the type of (favorable or unfavorable) ruling they see at either stage.
# ideally, what we want to show is that candidates keep the same strategy
# regardless of whether they see favorable rulings or not.

# tests: campaign expenditures by judicial ruling and across the entire review
# process using a non-parametric bootstrapped sample of expenditures.

# standardize candidate expenditures to offset outlier problems
candidate.disengagement.analysis %<>%
  mutate(candidacy.expenditures.actual = candidacy.expenditures.actual)

# test 1: campaign expenditures by judicial ruling
trial.expenditures <- candidate.disengagement.analysis %$%
  t.test(candidacy.expenditures.actual ~ candidacy.invalid.ontrial)
appeals.expenditures <- candidate.disengagement.analysis %$%
  t.test(candidacy.expenditures.actual ~ candidacy.invalid.onappeal)

# test 2: campaign expendtireus across judicial review process
review.expenditures <- candidate.disengagement.analysis %>%
  filter(candidacy.invalid.ontrial == 1) %$%
  t.test(candidacy.expenditures.actual ~ candidacy.invalid.onappeal)

# convert vectors to datasets
trial.expenditures   %<>% unlist() %>% {tibble(., names = names(unlist(.)))}
appeals.expenditures %<>% unlist() %>% {tibble(., names = names(unlist(.)))}
review.expenditures  %<>% unlist() %>% {tibble(., names = names(unlist(.)))}

# build dataset
bind_cols(trial.expenditures, appeals.expenditures, review.expenditures) %>%
select(1, 2, 3, 5) %>%
rename_all(~paste0('var', 1:4)) %>%
select(var2, var1, var3, var4) %>%
slice(-c(2, 4, 5, 8:11)) %>%
slice(3, 4, 1, 2) %>%
t() %>%
as_tibble(.name_repair = 'unique') %>%
rename_all(~c('Favorable', 'Unfavorable', 't-stat', 'p-value')) %>%
slice(-1) %>%
mutate_all(as.numeric) %>%
mutate_at(vars(3, 4), ~round(., digits = 3)) %>%
mutate(`Ruling Stage` = c('Trial', 'Appeals', 'Trial')) %>%
select(`Ruling Stage`, everything()) %>%
xtable(label = 'tab:candidatebehavior') %>%
print.xtable(floating = FALSE, hline.after = c(-1, -1, 0, 3, 3),
  include.rownames = FALSE)

# remove unnecessary objects
rm(candidate.disengagement.analysis, trial.expenditures, appeals.expenditures,
   review.expenditures)

### heterogeneous treatment effects
# these are the tests of differential effect conditional on conviction reason.
# here i am testing two hypotheses: (i) whether voters do punish politicians for
# type of electoral violation and (ii) whether strategy is beneficial from the
# when politicians are not caught.

# # build new dataset containing only the politicians for which i can recover the
# # type of electoral crime
# hte.analysis <- filter(tse.analysis, !is.na(candidacy.ruling.class))

# # relevel ruling categories to procedural or substantial rule breaking
# hte.analysis$class <- hte.analysis$candidacy.ruling.class %>%
#   {ifelse(.  != 'Requisito Faltante', 'Substantial', 'Procedural')} %>%
#   factor() %>%
#   {relevel(., ref = 'Procedural')}

# # create one long ivreg regression formula for all problems
# treat <- paste0(instrumented, ' * class + ')
# instr <- paste0(instrument, ' * class + ')
# exgos <- paste0(covariates, collapse = ' + ')
# fe    <- ' + election.year + election.ID + party.number'
# equations <- paste0(outcomes, ' ~ ', treat, exgos, fe, ' | ', instr, exgos, fe)

# # run regressions (note: up to 4 minutes to execute)
# # outcome 1: probability of election
# hte01 <- ivreg(equations[1], data = hte.analysis)

# # outcome 2: vote share
# hte02 <- ivreg(equations[2], data = hte.analysis)

# # outcome 3: distance to election cutoff for city councilor candidates
# hte03 <- ivreg(equations[3], data = filter(hte.analysis, office.ID == 13))

# # outcome 3: distance to election cutoff for mayor candidates
# hte04 <- ivreg(equations[3], data = filter(hte.analysis, office.ID == 11))

# # compute standard errors (note: up to 5 minutes to execute)
# hte01.se <- cse(hte01)
# hte02.se <- cse(hte02)
# hte03.se <- cse(hte03)
# hte04.se <- cse(hte04)

# # produce table
# stargazer(

#   # first-stage regressions
#   list(hte01, hte02, hte03, hte04),

#   # table cosmetics
#   type = 'latex',
#   title = 'Heterogeneous Effect of Electoral Crime',
#   style = 'default',
#   # out = 'tables/hte.tex',
#   out.header = FALSE,
#   column.labels = c(rep('Full Sample', 2), 'City Councilor', 'Mayor'),
#   column.separate = rep(1, 4),
#   covariate.labels = c(instrument.labels[1], 'Substantial', 'Inter'),
#   dep.var.caption = '',
#   # dep.var.labels = paste0('Outcome: ', 1:4),
#   dep.var.labels.include = FALSE,
#   align = FALSE,
#   se = list(hte01.se, hte02.se, hte03.se, hte04.se),
#   p.auto = TRUE,
#   column.sep.width = '4pt',
#   digit.separate = 3,
#   digits = 3,
#   digits.extra = 0,
#   font.size = 'scriptsize',
#   header = FALSE,
#   initial.zero = FALSE,
#   model.names = FALSE,
#   keep = 'invalid|class',
#   label = 'tab:hte',
#   no.space = FALSE,
#   omit = c('education', 'party'),
#   omit.labels = c('Individual Controls', 'Fixed-Effects'),
#   omit.stat = c('ser', 'f', 'rsq'),
#   omit.yes.no = c('Yes', '-'),
#   table.placement = '!htbp'
# )

# # extract f-stats for table
# objects(pattern = '^hte0[1-4]{1}$') %>%
# lapply(get) %>%
# lapply(function(x){summary(x)$waldtest[1]}) %>%
# unlist() %>%
# round(2) %>%
# paste0(collapse = ' & ') %>%
# {paste0('\textit{F}-stat & ', .)} %>%
# paste0(' \\')

# # remove unnecessary objects
# rm(list = objects(pattern = 'hte'))

### placebo test
# here I want to estimate an entire set of correlations between trial and
# appeals decisions to map when exactly would the IV parameter become the same
# as the OLS parameter

# # create vectors of independent coefficients, standard errors, and correlations
# se <- c()
# betas <- c()
# fstat <- c()
# ucorrel <- c()
# ccorrel <- c()

# # set seed to break process down into 2
# set.seed(12345)

# # execute for loop (~ 45 minutes)
# for (i in 1:10000) {

#   # determine correlation deviation from main sample
#   x <- runif(1, .001)

#   # call to simulation and store to dataset
#   y <- simcorrel(x)
#   tse.analysis$appeals.simulation <- y$appeals.outcomes

#   # run regressions
#   regression <- tse.analysis %>%
#     {felm(outcome.elected ~ candidate.age + candidate.male +
#       candidate.experience + candidacy.expenditures.actual +
#       candidate.maritalstatus + candidate.education | election.ID +
#       election.year + party.number | (candidacy.invalid.ontrial ~
#       appeals.simulation), data = ., exactDOF = TRUE)}

#   # run regressions
#   firststage <- tse.analysis %>%
#     {felm(candidacy.invalid.ontrial ~ appeals.simulation + candidate.age +
#       candidate.male + candidate.experience + candidacy.expenditures.actual +
#       candidate.maritalstatus + candidate.education | election.ID +
#       election.year + party.number, data = ., exactDOF = TRUE)}

#   # store results
#   estimates <- summary(regression, robust = TRUE)$coefficients[16, c(1, 2)]
#   ucorrel <- c(ucorrel, unname(y$correlation))
#   ccorrel <- c(ccorrel, summary(firststage)$coefficients[1])
#   betas <- c(betas, unname(estimates[1]))
#   fstat <- c(fstat, summary(firststage)$fstat)
#   se <- c(se, unname(estimates[2]))
# }

# # create dataset
# simulation <- tibble(ccorrel, ucorrel, betas, se, fstat)

# # save to file
# save(simulation, file = 'data/tseSimulation.Rda')

# load from file
load('data/tseSimulation.Rda')

# determine which coefficients are significant at the 5% level
simulation %<>% mutate(significant = ifelse(abs(betas / se) >= 1.96, 1, 0))
weak.iv.simulation <- filter(simulation, fstat <= 10)
strg.iv.simulation <- filter(simulation, fstat  > 10)

# create mean and standard errors for simulation beta
simulation.mean <- mean(unlist(simulation[,'betas']))
simulation.ses  <- mean(unlist(simulation[,'se']))
simulation.corr <- mean(unlist(simulation[,'ccorrel']))

# create mean and standard errors for actual beta
iv.mean <- summary(ss03)$coefficients[16, 1]
iv.ses  <- summary(ss03, robust = TRUE)$coefficients[16, 2]
iv.corr <- summary(fs03)$coefficients[1, 1]

# create mean and standard errors for weak instrument beta
weak.iv.mean <- mean(unlist(weak.iv.simulation[, 'betas']))
weak.iv.ses  <- mean(unlist(weak.iv.simulation[, 'se']))
weak.iv.corr <- mean(unlist(weak.iv.simulation[, 'ccorrel']))

# create mean and standard errors for strh instrument beta
strg.iv.mean <- mean(unlist(strg.iv.simulation[, 'betas']))
strg.iv.ses  <- mean(unlist(strg.iv.simulation[, 'se']))
strg.iv.corr <- min(unlist(strg.iv.simulation[, 'ccorrel']))

# create mean and standard errors for ols beta
ols.mean <- summary(ols03)$coefficients[1, 1]
ols.ses  <- cse(ols03)[1]

# create labels for data
ylabel <- rep(.52, 2)
xlabel <- c(strg.iv.mean, ols.mean)

# build plot
ggplot() +
  scale_x_continuous(breaks = seq(-.3, -.15, .025), limits = c(-.30, -.15)) +
  scale_y_continuous(breaks = seq(.52, .76, .04), limits = c(.52, .76)) +
  geom_point(data = strg.iv.simulation, aes(y = ccorrel,  x = betas),
    color = 'grey4', alpha = .5) +
  geom_segment(aes(y = .52, yend = .52,
    x = strg.iv.mean - qnorm(.025) * strg.iv.ses,
    xend = strg.iv.mean + qnorm(.025) * strg.iv.ses), color = 'skyblue2') +
  geom_point(aes(y = .52, x = strg.iv.mean), color = 'blue',
    fill = 'skyblue2', shape = 21, size = 3) +
  geom_segment(aes(y = .52, yend = .52, x = -.16,
    xend = ols.mean + qnorm(.025) * ols.ses), color = 'skyblue2') +
  geom_segment(aes(y = .52, yend = .52, x = -.15,
    xend = -.16), color = 'skyblue2', linetype = 'dashed') +
  geom_point(aes(y = .52, x = ols.mean), color = 'blue', fill = 'skyblue2',
    shape = 21, size = 3) +
  geom_segment(aes(y = .52, yend = .76, x = ols.mean + qnorm(.025) * ols.ses,
    xend = ols.mean + qnorm(.025) * ols.ses), color = 'grey1',
    linetype = 'dashed') +
  geom_label(data = tibble(y = ylabel, x = xlabel), aes(y = y, x = x),
    label = paste0(c('IV: ', 'OLS: '), round(xlabel, 3)),
    family = 'LM Roman 10', position = position_nudge(x = 0, y = .01)) +
  labs(y = 'Correlation Coefficient',
       x = 'IV Coefficient Point Estimate Simulations') +
  theme_bw() +
  theme(axis.title  = element_text(size = 10),
        axis.title.y = element_text(margin = margin(r = 12)),
        axis.title.x = element_text(margin = margin(t = 12)),
        axis.text.y = element_text(size = 10, lineheight = 1.1, face = 'bold'),
        axis.text.x = element_text(size = 10, lineheight = 1.1, face = 'bold'),
        text = element_text(family = 'LM Roman 10'),
        panel.border = element_rect(color = 'black', size = 1),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(color = 'grey79')
  )

# # save plot
# ggsave('weakinstruments.pdf', device = cairo_pdf, path = 'plots', dpi = 100,
#        width = 7, height = 5)

# remove everything for serial sourcing
rm(list = ls())


### regression discontinuity analysis
# create vector of election dates
electionDate <- c('2004-10-03', '2008-10-05', '2012-10-07', '2016-10-02')

# create election dates variable, which assigns treatment to observations
tjspAnalysis$rd.assign <- tjspAnalysis %>%
  {case_when(.$election.year == 2008 ~ electionDate[2],
             .$election.year == 2012 ~ electionDate[3],
             .$election.year == 2016 ~ electionDate[4],
  )} %>%
  as.Date(format = '%Y-%m-%d')

# create variable distance to cutoff
tjspAnalysis$rd.distance <- tjspAnalysis %>% {.$case.lastupdate - .$rd.assign}

# baseline regression
library(rdrobust)

# not significant
tjspAnalysis %$%
 rdrobust(y = sct.favorable, x = rd.distance, p = 1, q = 2, level = 95,
          h = 100) %>%
 summary()

# not significant
lm(sct.favorable ~ rd.distance + candidate.elected + rd.distance * candidate.elected,
   data = tjspAnalysis) %>%
summary()
