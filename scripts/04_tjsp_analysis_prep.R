### judicial favoritism of politicians
# this script prepares all data for analysis
# author: andre assumpcao
# by andre.assumpcao@gmail.com
rm(list=ls())
### import statements
# import packages
library(tidyverse)
library(magrittr)

# load data
load('data/campaign.Rda')
load('data/tjspJudges.Rda')
load('data/tjspMun.Rda')
load('data/tjspSentences.Rda')
load('data/tseCandidates.Rda')

### wrangle datasets
# join tjsp and tse data
tjspAnalysis <- tjspSentences %>%
  left_join(ungroup(tseCandidates), c('candidateID.x' = 'scraper.id')) %>%
  filter(!is.na(claimant.win)) %>%
  filter(class == 'Procedimento do Juizado Especial Cível')

# remove unnecessary dataset
rm(tseCandidates)

# define joinkey for finding campaign expenditure
joinkey <- c('election.year', 'election.ID', 'office.ID', 'candidate.number')

# work on getting the right campaign expenditure variable
tjspAnalysis <- campaign %>%
  filter(SG_UE_SUPERIOR == 'SP') %>%
  filter(ANO_ELEICAO %in% seq(2008, 2016, 4) & DS_CARGO != 'Vice-prefeito') %>%
  mutate(office.ID = as.character(ifelse(DS_CARGO == 'Vereador', 13, 11))) %>%
  rename(election.year = ANO_ELEICAO, election.ID = SG_UE) %>%
  rename(candidate.number = NR_CANDIDATO) %>%
  inner_join(tjspAnalysis, joinkey) %>%
  group_by(election.year, candidate.ssn) %>%
  summarize(x = sum(TOTAL_DESPESA)) %>%
  {left_join(tjspAnalysis, ., c('election.year', 'candidate.ssn'))} %>%
  group_by(election.ID) %>%
  mutate(x = ifelse(is.na(x), mean(x, na.rm = TRUE), x)) %>%
  group_by(office.ID) %>%
  mutate(x = ifelse(is.na(x), mean(x, na.rm = TRUE), x)) %>%
  ungroup() %>%
  rename(candidacy.expenditures.actual = x)

# define joinkey for finding judges responsible for each case
joinkey <- c('judge' = 'judge.name')

# reformat judge spelling in tjsp database
tjspAnalysis %<>%
  mutate_at(vars(judge),  str_to_lower) %>%
  mutate_at(vars(judge), ~iconv(., 'UTF-8', 'ASCII//TRANSLIT')) %>%
  mutate_at(vars(judge), ~str_remove_all(., '\\^|~|\'|\\"')) %>%
  mutate_at(vars(judge), str_squish)

# reformat judge spelling in tjsp pay database
tjspJudges$judge.name %<>% str_to_lower()

# match judge individual information onto sentences and electoral data
tjspAnalysis %<>%
  fuzzyjoin::fuzzy_left_join(tjspJudges, joinkey, match_fun = str_detect) %>%
  mutate(judge.pay = judge.pay %>% {ifelse(is.na(.), '35023.25', .)}) %>%
  mutate_at(vars(4, 15, 55), ~as.Date(., '%d/%m/%Y')) %>%
  mutate(t = as.numeric(updates - judge.tenure.start)) %>%
  mutate(c = as.numeric(updates - assignment)) %>%
  mutate(judge.tenure  = t %>% {ifelse(. < 1, median(., na.rm = TRUE), .)}) %>%
  mutate(case.duration = c %>% {ifelse(. < 1, median(., na.rm = TRUE), .)}) %>%
  select(-t, -c)

# fill in missing gender of judges
names <- tjspAnalysis[is.na(tjspAnalysis$judge.gender), 'judge'] %>% unlist()
names %<>% {ifelse(str_detect(., 'carlos'), 'Male', 'Female')}
names[is.na(names)] <- 'Male'
tjspAnalysis[is.na(tjspAnalysis$judge.gender), 'judge.gender'] <- names

# anonymize judges
tjspAnalysis %<>% {mutate(., judge = group_indices(., judge))}

# match circuit and municipal information
rows <- match(tjspAnalysis$caseID, str_remove_all(tjspMun$caseID, '\\.|-'))

# create new variables containing circuit, electoral district, and municipal
# information
tjspAnalysis$tjsp.ID <- unlist(tjspMun[rows, 'tj'])
tjspAnalysis$ibge.ID <- unlist(tjspMun[rows, 'ibge'])

# create list of useless variables
vars <- c(8:13, 16, 17, 21, 26:28, 30, 33, 35, 37, 40, 46:47, 50, 51, 53, 55)

# drop useless vars
tjspAnalysis %<>% select(-vars)
varNames <- names(tjspAnalysis)

# rename remaining vars
varNames[1:7] <- c(paste0('case.', varNames[1:6]), 'case.ID')
varNames[8] <- 'candidate.litigant.type'
varNames[9:10] <- c('case.lastupdate', 'case.claimant.win')
varNames[31]  <- 'candidate.expenditure'
names(tjspAnalysis) <- varNames

# reorder variables
tjspAnalysis %<>%
  select(
    matches('^case'), matches('^judge'), tjsp.ID, ibge.ID, election.ID,
    matches('^elect'), matches('^off'), matches('^candida'), matches('^par')
  )

# save to file
save(tjspAnalysis, file = 'data/tjspFinal.Rda')

# remove everything for serial sourcing
rm(list = ls())

