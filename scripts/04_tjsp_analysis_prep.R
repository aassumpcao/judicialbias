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
library(fuzzyjoin)

# load data
load('data/tseCandidates.Rda')
load('data/tjspSentences.Rda')
load('data/tjspJudges.Rda')
load('data/campaign.Rda')
tjspMun <- read_csv('data/sct.csv', locale = locale(encoding = 'latin1'),
  trim_ws = TRUE, col_types = cols(.default = 'c')) %>% select(-1)

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

#
test0 <- fuzzyjoin::fuzzy_inner_join(
  tjspAnalysis, tjspJudges, joinkey, match_fun = str_detect
)

test1 <- fuzzyjoin::fuzzy_anti_join(
  tjspAnalysis, tjspJudges, joinkey, match_fun = str_detect
)

# cap judge names at a maximum of 53 characters
tjspJudges %>%
  mutate(judge = str_sub(judge.name, 1, 53)) %>%
  {fuzzy_inner_join(
    filter(tjspAnalysis, !is.na(judge)), ., 'judge', match_fun = str_detect
  )}




nchar(tjspAnalysis$judge) %>% min(na.rm = TRUE)
nchar(tjspAnalysis$judge) %>% max(na.rm = TRUE)

nchar(tjspJudges$judge.name) %>% min()
nchar(tjspJudges$judge.name) %>% max()
